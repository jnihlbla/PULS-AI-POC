000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2711100.                                                
000300 AUTHOR.         STEFAN KIHLBERG.                                         
000400 DATE-WRITTEN.   96/11/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄGGER UPP ELLER UPPDATERAR ORDER OCH ORDERFÖRSLAG PÅ            
000900*        WDE3 FRÅN FIL                                                    
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLORDL (WDE3)                              
001200*                              WLARTS (WDK7)                              
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- IN-FIL                                                     
002700     SELECT INFIL                      ASSIGN TO W27111D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  INFIL                                                                
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W27111      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000     SKIP2                                                                
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W2711100'.            
004400 01  CHKP-VAR.                                                            
004500 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004600 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004700 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004800 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004900 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005000 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300     SKIP2                                                                
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
005900     88  END-OF-INFIL                        VALUE 'J'.                   
006000     EJECT                                                                
006100                                                                          
006200 01  ARBETSAREOR.                                                         
006300     03  WS-IDDISTR          PIC 9(5)      VALUE ZERO.                    
006400     03 IX                   PIC S9(4)     VALUE ZERO COMP-3.             
006500     03 IX-2                 PIC S9(4)     VALUE ZERO COMP-3.             
006600     03 IX-3                 PIC S9(4)     VALUE ZERO COMP-3.             
006700     03 IX-BUY               PIC 9(2)      VALUE ZERO.                    
006710     03 ISRT-IX              PIC 9(2)      VALUE ZERO.                    
006800     03 WS-KDREFTXT          PIC S9(2)     VALUE ZERO COMP-3.             
006900     03 WS-IDARTNR           PIC S9(9)     VALUE ZERO COMP-3.             
007000*      --- VALID IDDC CODES                                               
007100*                                                                         
007200*01    -COPY WWDC99                                                       
007300       EJECT                                                              
007400 01  DAGENS-DATUM-AAMMDD         PIC 9(6)    VALUE ZERO.                  
007500                                                                          
007600     EJECT                                                                
007700                                                                          
007800                                                                          
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000*                                                                         
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900 01  IN-AREA-START               PIC X(24)   VALUE                        
009000                                             'IN-AREA-START'.             
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W27111     -PRE IN-                                       
009400*                                                                         
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009700     SKIP3                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
009900     03  W-IDARTNR-X.                                                     
010000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010100     03  W-IDDC-X.                                                        
010200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010300     03  W-IDDC-B6-X.                                                     
010400         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
010500     03  W-IDDC-B616-X.                                                   
010600         05  W-IDDC-B616     PIC X(2)   VALUE SPACE.                      
010700                                                                          
010800                                                                          
010900     03 W-WDE301KY-X.                                                     
011000         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
011100         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
011200         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
011300         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
011400         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
011500                                                                          
011600     03 W-WDE301KY-MIN-X.                                                 
011700         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
011800         05  W-IDPERSON-BUY-MIN  PIC S9(3) VALUE ZERO COMP-3.             
011900         05  W-KDREFTYP-MIN      PIC X     VALUE SPACE.                   
012000         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
012100         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
012200                                                                          
012300     03 W-WDE301KY-MAX-X.                                                 
012400         05  W-IDDC-MAX          PIC X(2)  VALUE SPACE.                   
012500         05  W-IDPERSON-BUY-MAX  PIC S9(3) VALUE ZERO COMP-3.             
012600         05  W-KDREFTYP-MAX      PIC X     VALUE SPACE.                   
012700         05  W-IDARTNR-MAX       PIC S9(9)                                
012800                                         VALUE +999999999 COMP-3.         
012900         05  W-IDDISTR-MAX       PIC S9(5) VALUE +99999 COMP-3.           
013000                                                                          
013100     SKIP2                                                                
013200*    --- STATUS-KOD FRÅN IMS                                              
013300 01  STATUS-WS                   PIC XX.                                  
013400     88  SEGMENT-FINNS                       VALUE '  '.                  
013410     88  SEGMENT-INSERTED                    VALUE '  '.                  
013500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013510     88  SEGMENT-INDEX-DUBBLET               VALUE 'NI'.                  
013600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013800     88  IMS-EJ-OK                           VALUE 'XD'.                  
013900     SKIP2                                                                
014000 01  GODK-STATUSKODER.                                                    
014100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200     SKIP3                                                                
014300 01  SSA1                        PIC X(128).                              
014400 01  SSA2                        PIC X(128).                              
014500     EJECT                                                                
014600*    --- IMS FUNKTIONSKODER                                               
014700*01  -COPY W0003                                                          
014800     EJECT                                                                
014900*    ---  DLI INPUT-OUTPUT AREA                                           
015000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ORDL01'.                      
015100 01  DLI-IO-ORDL01.                                                       
015200*    03  -COPY WDE301                                                     
015300     EJECT                                                                
015400                                                                          
015500 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS01'.                      
015600 01  DLI-IO-ARTS01.                                                       
015700*    03  -COPY WDK701                                                     
015800     EJECT                                                                
015900                                                                          
016000 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTS11'.                      
016100 01  DLI-IO-ARTS11.                                                       
016200*    03  -COPY WDK711                                                     
016300     EJECT                                                                
016400                                                                          
016500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
016600     SKIP3                                                                
016700 01  DLI-IO-AREA-WDK611.                                                  
016800*    03  -COPY WDK611                                                     
016900     EJECT                                                                
017000                                                                          
017100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
017200     SKIP3                                                                
017300 01  DLI-IO-AREA-WDK629.                                                  
017400*    03  -COPY WDK629                                                     
017500     EJECT                                                                
017600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017700 01   DLI-IO-AREA-B601.                                                   
017800*     03  -COPY WDB601                                                    
017900                                                                          
018000 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
018100 01   DLI-IO-AREA-B616.                                                   
018200*     03  -COPY WDB616 -PRE B6-                                           
018300                                                                          
018400     EJECT                                                                
018500 LINKAGE SECTION.                                                         
018600                                                                          
018700*01  -COPY W0009   -PRE MSG-                                              
018800     EJECT                                                                
018900*01  -COPY W0008   -PRE ORDL-                                             
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008   -PRE ARTS-                                             
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500*01  -COPY W0008   -PRE WDK6-                                             
019600     05  FILLER                  PIC X.                                   
019700     EJECT                                                                
019800*01  -COPY W0008      -PRE WDB6-                                          
019900     05  FILLER                  PIC X.                                   
020000     EJECT                                                                
020100 PROCEDURE DIVISION  USING MSG-PCB ORDL-PCB ARTS-PCB                      
020200                                   WDK6-PCB WDB6-PCB.                     
020300 MAIN SECTION.                                                            
020400     ENTRY 'DLITCBL' USING MSG-PCB ORDL-PCB ARTS-PCB                      
020500                                   WDK6-PCB WDB6-PCB.                     
020600                                                                          
020700     SKIP2                                                                
020800     PERFORM A-INIT                                                       
020900                                                                          
021000     PERFORM S01-LAES-INFIL                                               
021100                                                                          
021200     PERFORM UNTIL END-OF-INFIL                                           
021300                                                                          
021400       IF CHKP-ANT > CHKP-MAX                                             
021500         PERFORM X-TAG-CHECKPOINT                                         
021600       END-IF                                                             
021700                                                                          
021800       MOVE IN-IDDC    TO W-IDDC                                          
021900                          W-IDDC-B6                                       
022000       PERFORM IMS-GU-WDB601                                              
022100       IF IN-IDARTNR NOT = WS-IDARTNR                                     
022200         MOVE 1          TO IX-BUY                                        
022300       ELSE                                                               
022400         ADD +1          TO IX-BUY                                        
022500       END-IF                                                             
022600       MOVE IN-IDARTNR TO W-IDARTNR                                       
022700                          WS-IDARTNR                                      
022800       MOVE IN-IDDC           TO W-IDDC-301                               
022900                                 W-IDDC-MIN                               
023000                                 W-IDDC-MAX                               
023100       MOVE IN-IDPERSON-BUY   TO W-IDPERSON-BUY                           
023200                                 W-IDPERSON-BUY-MIN                       
023300                                 W-IDPERSON-BUY-MAX                       
023400       MOVE IN-IDARTNR        TO W-IDARTNR-301                            
023500                                 W-IDARTNR-MIN                            
023600                                 W-IDARTNR-MAX                            
023700       MOVE IN-IDDISTR        TO W-IDDISTR                                
023800                                 W-IDDISTR-MIN                            
023900                                 W-IDDISTR-MAX                            
024000**********                                                                
024100**     MOVE IN-KDREFTYP       TO W-KDREFTYP                               
024200**********                                                                
024300                                                                          
024400       MOVE ZERO TO WS-KDREFTXT                                           
024500                                                                          
024600       IF  (IN-KDREFTYP = 'A'                                             
024700       AND IN-KDREFORS = 'O')                                             
024800       OR  (IN-KDREFTYP = 'B'                                             
024900       AND IN-KDREFORS = 'O')                                             
025000           PERFORM E-ORDER                                                
025100                                                                          
025200       ELSE                                                               
025300         EVALUATE IN-KDREFTYP                                             
025400            WHEN 'A'                                                      
025500               PERFORM B-FLYGFORSLAG                                      
025600            WHEN 'B'                                                      
025700               PERFORM C-BATFORSLAG                                       
025800            WHEN 'C'                                                      
025900               PERFORM B-FLYGFORSLAG                                      
026000            WHEN 'L'                                                      
026100               PERFORM D-LOKAL-ARTIKEL                                    
026200            WHEN 'O'                                                      
026300               PERFORM E-ORDER                                            
026400            WHEN 'R'                                                      
026500               PERFORM G-RETUR                                            
026600            WHEN 'T'                                                      
026700               PERFORM I-TRANSFER                                         
026800         END-EVALUATE                                                     
026900       END-IF                                                             
027000                                                                          
027100       PERFORM S01-LAES-INFIL                                             
027200                                                                          
027300     END-PERFORM                                                          
027400                                                                          
027500     PERFORM Z-FINIT                                                      
027600                                                                          
027700     MOVE ZERO TO RETURN-CODE                                             
027800     GOBACK                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 A-INIT SECTION.                                                          
028200     SKIP2                                                                
028300                                                                          
028400     PERFORM IMS-RESTART                                                  
028500                                                                          
028600     OPEN INPUT INFIL                                                     
028700                                                                          
028800     ACCEPT DAGENS-DATUM-AAMMDD FROM DATE                                 
028900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029000     MOVE ZERO  TO WS-IDARTNR                                             
029100     .                                                                    
029200     EJECT                                                                
029300                                                                          
029400                                                                          
029500                                                                          
029600 B-FLYGFORSLAG SECTION.                                                   
029700                                                                          
029800     MOVE 'A'                    TO W-KDREFTYP-MIN                        
029900                                 W-KDREFTYP-MAX                           
030000     IF IN-IDDISTR = ZERO                                                 
030100        PERFORM S41-HAMTA-REFILLDISTRIKT                                  
030200        MOVE WS-IDDISTR          TO W-IDDISTR                             
030300                                    W-IDDISTR-MIN                         
030400                                    W-IDDISTR-MAX                         
030500     END-IF                                                               
030600                                                                          
030700     PERFORM IMS-GHU-ORDL01                                               
030800     IF SEGMENT-FINNS                                                     
030900        IF REF-KDREFORS = 'O'                                             
031000           CONTINUE                                                       
031100        ELSE                                                              
031200           IF IN-KVBEART > REF-KVBEART                                    
031300              MOVE IN-KVBEART      TO REF-KVBEART                         
031400           END-IF                                                         
031500           IF IN-KDREFTXT < REF-KDREFTXT                                  
031600              MOVE IN-KDREFTXT     TO REF-KDREFTXT                        
031700           END-IF                                                         
031800           PERFORM IMS-REPL-ORDL                                          
031900           ADD +1 TO CHKP-ANT                                             
032000        END-IF                                                            
032100     ELSE                                                                 
032200       MOVE 'C'                  TO W-KDREFTYP-MIN                        
032300                                 W-KDREFTYP-MAX                           
032400       PERFORM IMS-GHU-ORDL01                                             
032500       IF SEGMENT-FINNS                                                   
032600          IF REF-KDREFORS = 'O'                                           
032700             CONTINUE                                                     
032800          ELSE                                                            
032900             IF IN-KVBEART > REF-KVBEART                                  
033000                MOVE IN-KVBEART TO REF-KVBEART                            
033100             END-IF                                                       
033200             IF IN-KDREFTXT < REF-KDREFTXT                                
033300                MOVE IN-KDREFTXT TO REF-KDREFTXT                          
033400             END-IF                                                       
033500             PERFORM IMS-REPL-ORDL                                        
033600             ADD +1 TO CHKP-ANT                                           
033700          END-IF                                                          
033800       ELSE                                                               
033900          PERFORM S21-SKAPA-FLYGFORSLAG                                   
034000          PERFORM IMS-ISRT-ORDL01                                         
034100          ADD +1 TO CHKP-ANT                                              
034200       END-IF                                                             
034300     END-IF                                                               
034400     .                                                                    
034500     EJECT                                                                
034600                                                                          
034700                                                                          
034800                                                                          
034900 C-BATFORSLAG SECTION.                                                    
035000                                                                          
035100     MOVE 'A'                 TO W-KDREFTYP-MIN                           
035200                                 W-KDREFTYP-MAX                           
035300                                                                          
035400     IF IN-IDDISTR = ZERO                                                 
035500        PERFORM S41-HAMTA-REFILLDISTRIKT                                  
035600        MOVE WS-IDDISTR       TO W-IDDISTR                                
035700                                 W-IDDISTR-MIN                            
035800                                 W-IDDISTR-MAX                            
035900     END-IF                                                               
036000                                                                          
036100     PERFORM IMS-GHU-ORDL01                                               
036200     IF SEGMENT-FINNS                                                     
036300        IF REF-KDREFORS = 'O'                                             
036400           CONTINUE                                                       
036500        ELSE                                                              
036600           IF IN-KDREFTXT < REF-KDREFTXT                                  
036700              MOVE IN-KDREFTXT  TO REF-KDREFTXT                           
036800           END-IF                                                         
036900           PERFORM IMS-REPL-ORDL                                          
037000           ADD +1 TO CHKP-ANT                                             
037100        END-IF                                                            
037200     ELSE                                                                 
037300       MOVE 'C'               TO W-KDREFTYP-MIN                           
037400                                 W-KDREFTYP-MAX                           
037500                                                                          
037600       PERFORM IMS-GHU-ORDL01                                             
037700       IF SEGMENT-FINNS                                                   
037800          IF REF-KDREFORS = 'O'                                           
037900             CONTINUE                                                     
038000          ELSE                                                            
038100             IF IN-KDREFTXT < REF-KDREFTXT                                
038200                MOVE IN-KDREFTXT TO REF-KDREFTXT                          
038300             END-IF                                                       
038400             PERFORM IMS-REPL-ORDL                                        
038500             ADD +1 TO CHKP-ANT                                           
038600          END-IF                                                          
038700       ELSE                                                               
038800          MOVE 'B'               TO W-KDREFTYP-MIN                        
038900                                      W-KDREFTYP-MAX                      
039000          PERFORM IMS-GHU-ORDL01                                          
039100          IF SEGMENT-FINNS                                                
039200             IF REF-KDREFORS = 'O'                                        
039300                CONTINUE                                                  
039400             ELSE                                                         
039500                IF IN-KDREFTXT = 02                                       
039600                   MOVE IN-KDREFTXT   TO REF-KDREFTXT                     
039700                   MOVE IN-IDDC-REF   TO REF-IDDC-REF                     
039800                ELSE                                                      
039900                   IF IN-KVBEART > REF-KVBEART                            
040000                      MOVE IN-KVBEART TO REF-KVBEART                      
040100                   END-IF                                                 
040200                   IF IN-KDREFTXT < REF-KDREFTXT                          
040300                      MOVE IN-KDREFTXT TO REF-KDREFTXT                    
040400                   END-IF                                                 
040500                END-IF                                                    
040600                PERFORM IMS-REPL-ORDL                                     
040700                ADD +1 TO CHKP-ANT                                        
040800             END-IF                                                       
040900          ELSE                                                            
041000             PERFORM S22-SKAPA-BATFORSLAG                                 
041100             PERFORM IMS-ISRT-ORDL01                                      
041200             ADD +1 TO CHKP-ANT                                           
041300          END-IF                                                          
041400       END-IF                                                             
041500     END-IF                                                               
041600     .                                                                    
041700     EJECT                                                                
041800                                                                          
041900                                                                          
042000 D-LOKAL-ARTIKEL SECTION.                                                 
042100                                                                          
042200     MOVE 'L'                 TO W-KDREFTYP-MIN                           
042300                                 W-KDREFTYP-MAX                           
042400                                                                          
042500     IF IN-IDDISTR = ZERO                                                 
042600        PERFORM S41-HAMTA-REFILLDISTRIKT                                  
042700        MOVE WS-IDDISTR       TO W-IDDISTR                                
042800                                 W-IDDISTR-MIN                            
042900                                 W-IDDISTR-MAX                            
043000     END-IF                                                               
043100                                                                          
043200     PERFORM IMS-GHU-ORDL01                                               
043300     IF SEGMENT-FINNS                                                     
043400        IF REF-KDREFORS = 'O'                                             
043500           CONTINUE                                                       
043600        ELSE                                                              
043700           IF IN-KVBEART > REF-KVBEART                                    
043800              MOVE IN-KVBEART   TO REF-KVBEART                            
043900           END-IF                                                         
044000           IF IN-KDREFTXT < REF-KDREFTXT                                  
044100              MOVE IN-KDREFTXT  TO REF-KDREFTXT                           
044200           END-IF                                                         
044300           PERFORM IMS-REPL-ORDL                                          
044400           ADD +1 TO CHKP-ANT                                             
044500        END-IF                                                            
044600     ELSE                                                                 
044700        PERFORM S23-SKAPA-LOKAL-FORSLAG                                   
044800        PERFORM IMS-ISRT-ORDL01                                           
044900        ADD +1 TO CHKP-ANT                                                
045000     END-IF                                                               
045100     .                                                                    
045200     EJECT                                                                
045300                                                                          
045400                                                                          
045500 E-ORDER SECTION.                                                         
045600                                                                          
045700     PERFORM S24-SKAPA-ORDER                                              
045800     PERFORM IMS-ISRT-ORDL01                                              
045900     IF SEGMENT-FINNS-REDAN                                               
046000       MOVE IN-KDREFTYP       TO W-KDREFTYP-MIN                           
046100                                 W-KDREFTYP-MAX                           
046200       PERFORM IMS-GHU-ORDL01                                             
046300       ADD IN-KVBEART        TO REF-KVBEART                               
046400       ADD IN-KVBEART-CD     TO REF-KVBEART-CD                            
046500       IF SEGMENT-FINNS                                                   
046600         PERFORM IMS-REPL-ORDL                                            
046700         ADD +1 TO CHKP-ANT                                               
046800         IF IN-IDDC = '11'                                                
046900           PERFORM S38-UPPDATERA-CLAG-KVBEART                             
047000         ELSE                                                             
047100           PERFORM S36-UPPDATERA-SLAG-KVBEART                             
047200         END-IF                                                           
047300         IF IN-KVBEART-CD > ZERO                                          
047400           PERFORM S37-UPPDATERA-K611                                     
047500         END-IF                                                           
047600       END-IF                                                             
047700     ELSE                                                                 
047800       ADD +1 TO CHKP-ANT                                                 
047900       IF IN-IDDC = '11'                                                  
048000         PERFORM S38-UPPDATERA-CLAG-KVBEART                               
048100       ELSE                                                               
048200         PERFORM S36-UPPDATERA-SLAG-KVBEART                               
048300       END-IF                                                             
048400       IF IN-KVBEART-CD > ZERO                                            
048500         PERFORM S37-UPPDATERA-K611                                       
048600       END-IF                                                             
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
049100                                                                          
049200 G-RETUR SECTION.                                                         
049300                                                                          
049400     MOVE 'R'                 TO W-KDREFTYP-MIN                           
049500                                 W-KDREFTYP-MAX                           
049600     IF IN-IDDISTR = ZERO                                                 
049700        PERFORM S42-HAMTA-RETURDISTRIKT                                   
049800        MOVE WS-IDDISTR       TO W-IDDISTR                                
049900                                 W-IDDISTR-MIN                            
050000                                 W-IDDISTR-MAX                            
050100     END-IF                                                               
050200     PERFORM IMS-GHU-ORDL01                                               
050300     IF SEGMENT-FINNS                                                     
050400        PERFORM IMS-DLET-ORDL                                             
050500        ADD +1 TO CHKP-ANT                                                
050600     END-IF                                                               
050700     PERFORM S26-SKAPA-RETUR                                              
050800     PERFORM IMS-ISRT-ORDL01                                              
050900     ADD +1 TO CHKP-ANT                                                   
051000     PERFORM IMS-GHU-SLAG                                                 
051100     IF SEGMENT-FINNS                                                     
051200        IF SLAG-TIRETUR-BEORD = DAGENS-DATUM-AAMMDD                       
051300*          FÖR OMKÖRNING                                                  
051400           CONTINUE                                                       
051500        ELSE                                                              
051600           MOVE IN-KVBEART          TO SLAG-KVRETUR-BEORD                 
051700           MOVE DAGENS-DATUM-AAMMDD TO SLAG-TIRETUR-BEORD                 
051800           PERFORM IMS-REPL-SLAG                                          
051900           ADD +1 TO CHKP-ANT                                             
052000        END-IF                                                            
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400                                                                          
052500                                                                          
052600 I-TRANSFER SECTION.                                                      
052700                                                                          
052800     MOVE 'T'               TO W-KDREFTYP                                 
052900     PERFORM IMS-GHU-SLAG                                                 
053000     IF SEGMENT-FINNS                                                     
053100       PERFORM IMS-GHU-ORDL-WLORDL01                                      
053200                                                                          
053300       IF SEGMENT-FINNS                                                   
053400         IF IN-IDKUNDNR = REF-IDKUNDNR                                    
053500           COMPUTE SLAG-KVBEART = SLAG-KVBEART - REF-KVBEART              
053600           END-COMPUTE                                                    
053700                                                                          
053800           COMPUTE SLAG-KVBEART = SLAG-KVBEART + IN-KVBEART               
053900           END-COMPUTE                                                    
054000                                                                          
054100           MOVE IN-KVBEART TO REF-KVBEART                                 
054200                                                                          
054300           PERFORM IMS-REPL-ORDL                                          
054400           ADD +1 TO CHKP-ANT                                             
054500         ELSE                                                             
054600******************************************************************        
054700* DETTA ÄR FÖR ATT KUNNA SKICKA FLERA TRANSFER FRÅN OLIKA DC     *        
054800* TILL SAMMA DC I OCH MED ATT IDKUNDNR EJ ÄR NYCKEL PÅ WDE3      *        
054900******************************************************************        
055000           ADD IX-BUY           TO REF-IDPERSON-BUY                       
055100           MOVE IN-IDKUNDNR     TO REF-IDKUNDNR                           
055200           MOVE IN-KVBEART      TO REF-KVBEART                            
055300           PERFORM IMS-ISRT-ORDL01                                        
055400           ADD +1 TO CHKP-ANT                                             
055500           COMPUTE SLAG-KVBEART = SLAG-KVBEART + IN-KVBEART               
055600         END-IF                                                           
055700       ELSE                                                               
055800         PERFORM S27-SKAPA-TRANSFER                                       
055900         PERFORM IMS-ISRT-ORDL01                                          
055901         ADD +1 TO CHKP-ANT                                               
055902         MOVE +1    TO ISRT-IX                                            
055903         PERFORM UNTIL SEGMENT-INSERTED OR (ISRT-IX > 20)                 
055910           IF SEGMENT-FINNS-REDAN                                         
055920           OR SEGMENT-INDEX-DUBBLET                                       
055930             ADD IX-BUY         TO REF-IDPERSON-BUY                       
055940             PERFORM IMS-ISRT-ORDL01                                      
056000             ADD +1 TO CHKP-ANT                                           
056001                       ISRT-IX                                            
056002                       IX-BUY                                             
056010           END-IF                                                         
056020         END-PERFORM                                                      
056100                                                                          
056110         IF SEGMENT-INSERTED                                              
056200           COMPUTE SLAG-KVBEART = SLAG-KVBEART + IN-KVBEART               
056210         END-IF                                                           
056300                                                                          
056400       END-IF                                                             
056500       PERFORM IMS-REPL-SLAG                                              
056600       ADD +1 TO CHKP-ANT                                                 
056700     END-IF                                                               
056800     .                                                                    
056900     EJECT                                                                
057000                                                                          
057100 Z-FINIT SECTION.                                                         
057200                                                                          
057300                                                                          
057400     CLOSE INFIL                                                          
057500     SKIP2                                                                
057600     MOVE 'S' TO POSTSUM-OPKOD                                            
057700     CALL POSTSUM USING POSTSUM-PARM                                      
057800     .                                                                    
057900     EJECT                                                                
058000                                                                          
058100                                                                          
058200 S01-LAES-INFIL   SECTION.                                                
058300     SKIP2                                                                
058400     READ INFIL INTO IN-AREA                                              
058500     AT END                                                               
058600        SET END-OF-INFIL TO TRUE                                          
058700                                                                          
058800     NOT AT END                                                           
058900        MOVE 'INFIL' TO POSTSUM-FDNAMN                                    
059000        MOVE 'W27111D1' TO POSTSUM-DDNAMN2                                
059100        CALL POSTSUM USING POSTSUM-PARM                                   
059200                                                                          
059300     END-READ                                                             
059400     .                                                                    
059500     EJECT                                                                
059600                                                                          
059700                                                                          
059800 S21-SKAPA-FLYGFORSLAG SECTION.                                           
059900                                                                          
060000     MOVE IN-KDREFTYP     TO W-KDREFTYP                                   
060100     PERFORM S31-SKAPA-E3-POST                                            
060200     .                                                                    
060300     EJECT                                                                
060400                                                                          
060500 S22-SKAPA-BATFORSLAG SECTION.                                            
060600                                                                          
060700     MOVE 'B'             TO W-KDREFTYP                                   
060800     PERFORM S31-SKAPA-E3-POST                                            
060900     .                                                                    
061000     EJECT                                                                
061100                                                                          
061200 S23-SKAPA-LOKAL-FORSLAG SECTION.                                         
061300                                                                          
061400     MOVE 'L'             TO W-KDREFTYP                                   
061500     PERFORM S31-SKAPA-E3-POST                                            
061600     .                                                                    
061700     EJECT                                                                
061800                                                                          
061900                                                                          
062000 S24-SKAPA-ORDER SECTION.                                                 
062100                                                                          
062200     IF IN-IDDISTR = ZERO                                                 
062300        PERFORM S41-HAMTA-REFILLDISTRIKT                                  
062400        MOVE WS-IDDISTR   TO W-IDDISTR                                    
062500     END-IF                                                               
062600     MOVE 'O'             TO W-KDREFTYP                                   
062700     PERFORM S31-SKAPA-E3-POST                                            
062800     .                                                                    
062900     EJECT                                                                
063000                                                                          
063100                                                                          
063200 S26-SKAPA-RETUR SECTION.                                                 
063300                                                                          
063400     MOVE 'R'             TO W-KDREFTYP                                   
063500     PERFORM S31-SKAPA-E3-POST                                            
063600     .                                                                    
063700     EJECT                                                                
063800                                                                          
063900                                                                          
064000 S27-SKAPA-TRANSFER SECTION.                                              
064100                                                                          
064200     PERFORM S31-SKAPA-E3-POST                                            
064300     .                                                                    
064400     EJECT                                                                
064500                                                                          
064600                                                                          
064700 S31-SKAPA-E3-POST SECTION.                                               
064800                                                                          
064900     MOVE IN-IDDC         TO REF-IDDC                                     
065000     MOVE IN-IDPERSON-BUY TO REF-IDPERSON-BUY                             
065100     MOVE IN-KDREFTYP     TO REF-KDREFTYP                                 
065200     MOVE IN-IDARTNR      TO REF-IDARTNR                                  
065300     IF IN-IDKUNDNR = ZERO                                                
065400        MOVE ZERO         TO REF-IDKUNDNR                                 
065500     ELSE                                                                 
065600        MOVE IN-IDKUNDNR     TO REF-IDKUNDNR                              
065700     END-IF                                                               
065800     IF IN-IDDISTR = ZERO                                                 
065900        MOVE WS-IDDISTR   TO REF-IDDISTR                                  
066000     ELSE                                                                 
066100        MOVE IN-IDDISTR      TO REF-IDDISTR                               
066200     END-IF                                                               
066300     MOVE IN-KVBEART      TO REF-KVBEART                                  
066400     MOVE IN-KVBEART-CD   TO REF-KVBEART-CD                               
066500     MOVE IN-KDREFORS     TO REF-KDREFORS                                 
066600     MOVE IN-IDLEVNR      TO REF-IDLEVNR                                  
066700     MOVE IN-KDREFTXT     TO REF-KDREFTXT                                 
066800                                                                          
066900     MOVE IN-ADLAGOMR-CDC   TO REF-ADLAGOMR-CDC                           
067000     MOVE IN-ADGANG-CDC     TO REF-ADGANG-CDC                             
067100     MOVE IN-ADPLATS-CDC    TO REF-ADPLATS-CDC                            
067200     MOVE IN-ADLAGOMR-SDC   TO REF-ADLAGOMR-SDC                           
067300     MOVE IN-ADGANG-SDC     TO REF-ADGANG-SDC                             
067400     MOVE IN-ADPLATS-SDC    TO REF-ADPLATS-SDC                            
067500     MOVE IN-ADLAGOMR-CD    TO REF-ADLAGOMR-CD                            
067600     MOVE IN-ADGANG-CD      TO REF-ADGANG-CD                              
067700     MOVE IN-ADPLATS-CD     TO REF-ADPLATS-CD                             
067800     MOVE IN-KDFRAKT        TO REF-KDFRAKT                                
067900     MOVE IN-IDDC-REF       TO REF-IDDC-REF                               
068000     .                                                                    
068100     EJECT                                                                
068200                                                                          
068300                                                                          
068400 S36-UPPDATERA-SLAG-KVBEART SECTION.                                      
068500                                                                          
068600     PERFORM IMS-GHU-SLAG                                                 
068700     IF SEGMENT-FINNS                                                     
068800        COMPUTE SLAG-KVBEART = SLAG-KVBEART + IN-KVBEART                  
068900        MOVE DAGENS-DATUM-AAMMDD TO SLAG-TIORDREG                         
069000        PERFORM IMS-REPL-SLAG                                             
069100        ADD +1 TO CHKP-ANT                                                
069200     END-IF                                                               
069300     .                                                                    
069400     EJECT                                                                
069500 S37-UPPDATERA-K611 SECTION.                                              
069600                                                                          
069700     PERFORM IMS-GHU-K611                                                 
069800                                                                          
069900     MOVE 1                  TO IX                                        
070000     PERFORM UNTIL IX > 4                                                 
070100     OR SLAG-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX)                          
070200       ADD 1                 TO IX                                        
070300     END-PERFORM                                                          
070400                                                                          
070500     IF IX > 4                                                            
070600       CONTINUE                                                           
070700     ELSE                                                                 
070800                                                                          
070900       COMPUTE CLAG-KVRESS-CD (IX) =                                      
071000               CLAG-KVRESS-CD (IX) + IN-KVBEART-CD                        
071100     END-IF                                                               
071200     PERFORM IMS-REPL-K611                                                
071300     .                                                                    
071400     EJECT                                                                
071500                                                                          
071600 S38-UPPDATERA-CLAG-KVBEART SECTION.                                      
071700                                                                          
071800     PERFORM IMS-GHU-K611                                                 
071900     IF SEGMENT-FINNS                                                     
072000        COMPUTE CLAG-KVBEART = CLAG-KVBEART + IN-KVBEART                  
072100        PERFORM IMS-REPL-K611                                             
072200        ADD +1 TO CHKP-ANT                                                
072300        PERFORM IMS-GHNP-K629                                             
072400        IF SEGMENT-FINNS                                                  
072500          MOVE DAGENS-DATUM-AAMMDD TO CREF-TIORDREG                       
072600          PERFORM IMS-REPL-K629                                           
072700          ADD +1 TO CHKP-ANT                                              
072800        END-IF                                                            
072900     END-IF                                                               
073000     .                                                                    
073100     EJECT                                                                
073200                                                                          
073300                                                                          
073400 S41-HAMTA-REFILLDISTRIKT SECTION.                                        
073500                                                                          
073600     IF IN-IDDC = '11'                                                    
073700       PERFORM IMS-GU-K611                                                
073800       IF SEGMENT-FINNS                                                   
073900         MOVE CLAG-IDDC-REF  TO W-IDDC-B616                               
074000         PERFORM IMS-GU-WDB616                                            
074100         IF SEGMENT-FINNS                                                 
074200           MOVE B6-REF-IDDISTR-REFILL TO WS-IDDISTR                       
074300         END-IF                                                           
074400       END-IF                                                             
074500     ELSE                                                                 
074600       PERFORM IMS-GU-SLAG                                                
074700       IF SEGMENT-FINNS                                                   
074800         IF SLAG-IDDC-REF = '11'                                          
074900           MOVE DCS-IDDISTR-REFILL TO WS-IDDISTR                          
075000         ELSE                                                             
075100           MOVE SLAG-IDDC-REF TO W-IDDC-B616                              
075200           PERFORM IMS-GU-WDB616                                          
075300           IF SEGMENT-FINNS                                               
075400             MOVE B6-REF-IDDISTR-REFILL TO WS-IDDISTR                     
075500           END-IF                                                         
075600         END-IF                                                           
075700       END-IF                                                             
075800     END-IF                                                               
075900     .                                                                    
076000     EJECT                                                                
076100                                                                          
076200                                                                          
076300 S42-HAMTA-RETURDISTRIKT SECTION.                                         
076400                                                                          
076500     PERFORM IMS-GU-SLAG                                                  
076600     IF SEGMENT-FINNS                                                     
076700       IF SLAG-IDDC-REF = '11'                                            
076800         MOVE DCS-IDDISTR-RETUR  TO WS-IDDISTR                            
076900       ELSE                                                               
077000         MOVE SLAG-IDDC-REF TO W-IDDC-B616                                
077100         PERFORM IMS-GU-WDB616                                            
077200         IF SEGMENT-FINNS                                                 
077300           MOVE B6-REF-IDDISTR-RETUR  TO WS-IDDISTR                       
077400         END-IF                                                           
077500       END-IF                                                             
077600     END-IF                                                               
077700     .                                                                    
077800     EJECT                                                                
077900                                                                          
078000 X-TAG-CHECKPOINT   SECTION.                                              
078100                                                                          
078200* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
078300* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
078400     PERFORM IMS-CHECKPOINT                                               
078500     MOVE ZERO TO CHKP-ANT                                                
078600* --- LÄS OM DATABAS OM DET BEHÖVS                                        
078700     .                                                                    
078800     EJECT                                                                
078900* --- IMS SEKTIONER ---                                                   
079000                                                                          
079100                                                                          
079200 IMS-GHU-ORDL01 SECTION.                                                  
079300                                                                          
079400     STRING 'WLORDL01(WDE301KY=>' W-WDE301KY-MIN-X                        
079500                    '&WDE301KY<=' W-WDE301KY-MAX-X ')'                    
079600          DELIMITED BY SIZE INTO SSA1                                     
079700     MOVE '  GE' TO GODK-STATUSKODER                                      
079800     CALL CBLTDLI USING GHU ORDL-PCB DLI-IO-ORDL01 SSA1                   
079900     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
080000     PERFORM IMS-STATUSKONTROLL                                           
080100     .                                                                    
080200     SKIP3                                                                
080300                                                                          
080400 IMS-GHU-ORDL-WLORDL01 SECTION.                                           
080500                                                                          
080600     STRING 'WLORDL01(WDE301KY =' W-WDE301KY-X ')'                        
080700          DELIMITED BY SIZE INTO SSA1                                     
080800     MOVE '  GE' TO GODK-STATUSKODER                                      
080900     CALL CBLTDLI USING GHU ORDL-PCB DLI-IO-ORDL01 SSA1                   
081000     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
081100     PERFORM IMS-STATUSKONTROLL                                           
081200     .                                                                    
081300     EJECT                                                                
081400                                                                          
081500 IMS-ISRT-ORDL01 SECTION.                                                 
081600                                                                          
081700     MOVE 'WLORDL01 ' TO SSA1                                             
081800     MOVE '  IINI' TO GODK-STATUSKODER                                    
081900     CALL CBLTDLI USING ISRT ORDL-PCB DLI-IO-ORDL01 SSA1                  
082000     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
082100     PERFORM IMS-STATUSKONTROLL                                           
082200     .                                                                    
082300     SKIP3                                                                
082400 IMS-REPL-ORDL SECTION.                                                   
082500                                                                          
082600     MOVE '  ' TO GODK-STATUSKODER                                        
082700     CALL CBLTDLI USING REPL ORDL-PCB DLI-IO-ORDL01                       
082800     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
082900     PERFORM IMS-STATUSKONTROLL                                           
083000     .                                                                    
083100     SKIP3                                                                
083200 IMS-DLET-ORDL SECTION.                                                   
083300                                                                          
083400     MOVE '  ' TO GODK-STATUSKODER                                        
083500     CALL CBLTDLI USING DLET ORDL-PCB DLI-IO-ORDL01                       
083600     MOVE ORDL-STATUS-CODE TO STATUS-WS                                   
083700     PERFORM IMS-STATUSKONTROLL                                           
083800     .                                                                    
083900     EJECT                                                                
084000 IMS-RESTART SECTION.                                                     
084100     SKIP2                                                                
084200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
084300     MOVE '  ' TO GODK-STATUSKODER                                        
084400     CALL CBLTDLI USING XRST MSG-PCB                                      
084500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
084600                        CHKP-AREA-LENGTH CHKP-AREA                        
084700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084800     PERFORM IMS-STATUSKONTROLL                                           
084900     .                                                                    
085000     EJECT                                                                
085100                                                                          
085200                                                                          
085300 IMS-GHU-SLAG SECTION.                                                    
085400                                                                          
085500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
085600          DELIMITED BY SIZE INTO SSA1                                     
085700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
085800          DELIMITED BY SIZE INTO SSA2                                     
085900     MOVE '  ' TO GODK-STATUSKODER                                        
086000     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-ARTS11 SSA1 SSA2              
086100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
086200     PERFORM IMS-STATUSKONTROLL                                           
086300     .                                                                    
086400     EJECT                                                                
086500                                                                          
086600 IMS-GU-SLAG SECTION.                                                     
086700                                                                          
086800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
086900          DELIMITED BY SIZE INTO SSA1                                     
087000     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
087100          DELIMITED BY SIZE INTO SSA2                                     
087200     MOVE '  GE' TO GODK-STATUSKODER                                      
087300     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-ARTS11 SSA1 SSA2               
087400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     .                                                                    
087700     EJECT                                                                
087800                                                                          
087900                                                                          
088000 IMS-REPL-SLAG SECTION.                                                   
088100                                                                          
088200     MOVE '  ' TO GODK-STATUSKODER                                        
088300     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-ARTS11                       
088400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
088500     PERFORM IMS-STATUSKONTROLL                                           
088600     .                                                                    
088700     EJECT                                                                
088800                                                                          
088900 IMS-GU-K611 SECTION.                                                     
089000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
089100          DELIMITED BY SIZE INTO SSA1                                     
089200     MOVE 'WDK611  '       TO SSA2                                        
089300     MOVE '  GE' TO GODK-STATUSKODER                                      
089400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
089500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
089600     PERFORM IMS-STATUSKONTROLL                                           
089700     .                                                                    
089800     SKIP3                                                                
089900                                                                          
090000 IMS-GHU-K611 SECTION.                                                    
090100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
090200          DELIMITED BY SIZE INTO SSA1                                     
090300     MOVE 'WDK611  '       TO SSA2                                        
090400     MOVE '  GE' TO GODK-STATUSKODER                                      
090500     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
090600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     .                                                                    
090900     SKIP3                                                                
091000                                                                          
091100 IMS-REPL-K611 SECTION.                                                   
091200     SKIP2                                                                
091300     MOVE '  ' TO GODK-STATUSKODER                                        
091400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK611                  
091500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
091600     PERFORM IMS-STATUSKONTROLL                                           
091700     .                                                                    
091800     EJECT                                                                
091900                                                                          
092000 IMS-GHNP-K629 SECTION.                                                   
092100     MOVE 'WDK629  '       TO SSA1                                        
092200     MOVE '  GE' TO GODK-STATUSKODER                                      
092300     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-AREA-WDK629 SSA1             
092400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     .                                                                    
092700     SKIP3                                                                
092800                                                                          
092900 IMS-REPL-K629 SECTION.                                                   
093000     SKIP2                                                                
093100     MOVE '  ' TO GODK-STATUSKODER                                        
093200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK629                  
093300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
093400     PERFORM IMS-STATUSKONTROLL                                           
093500     .                                                                    
093600     EJECT                                                                
093700                                                                          
093800 IMS-GU-WDB601    SECTION.                                                
093900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
094000          DELIMITED BY SIZE INTO SSA1                                     
094100     MOVE '  ' TO GODK-STATUSKODER                                        
094200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
094300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
094400     PERFORM IMS-STATUSKONTROLL                                           
094500     .                                                                    
094600     EJECT                                                                
094700                                                                          
094800 IMS-GU-WDB616    SECTION.                                                
094900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
095000          DELIMITED BY SIZE INTO SSA1                                     
095100     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
095200          DELIMITED BY SIZE INTO SSA2                                     
095300     MOVE '  GE' TO GODK-STATUSKODER                                      
095400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
095500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
095600     PERFORM IMS-STATUSKONTROLL                                           
095700     .                                                                    
095800     EJECT                                                                
095900                                                                          
096000 IMS-CHECKPOINT SECTION.                                                  
096100     SKIP2                                                                
096200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
096300     MOVE '  XD' TO GODK-STATUSKODER                                      
096400     CALL CBLTDLI USING CHKP MSG-PCB                                      
096500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
096600                        CHKP-AREA-LENGTH CHKP-AREA                        
096700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096800     PERFORM IMS-STATUSKONTROLL                                           
096900                                                                          
097000     IF IMS-EJ-OK                                                         
097100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
097200       DISPLAY FELTEXT                                                    
097300       CALL FELLOG                                                        
097400     END-IF                                                               
097500     .                                                                    
097600     EJECT                                                                
097700 IMS-STATUSKONTROLL SECTION.                                              
097800     SKIP2                                                                
097900     SET STATUS-IX TO 1                                                   
098000     SEARCH GODK-STATUS                                                   
098100       AT END                                                             
098200         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
098300         DISPLAY FELTEXT                                                  
098400         CALL FELLOG                                                      
098500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
098600         CONTINUE                                                         
098700     END-SEARCH                                                           
098800     .                                                                    
098900     EJECT                                                                
