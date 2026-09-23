000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1167000.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/01/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        BEHANDLA FIL W116D7                                              
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDK7                                       
001200*        PROGRAMMET LÄSER      WDC9                                       
001300*        PROGRAMMET LÄSER      WDD9                                       
001400*        PROGRAMMET UPPDATERAR WDG3                                       
001500*                                                                         
001600                                                                          
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200                                                                          
002300*          --- ???                                                        
002400     SELECT W1167D                     ASSIGN TO W11670D1.                
002500*          --- ANNULLATIONSBEGÄRAN AV AVTAL                               
002600     SELECT W1167E                     ASSIGN TO W11670D2.                
002700                                                                          
002800                                                                          
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100                                                                          
003200 FD  W1167D                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  -COPY W1167D      -L.                                                
003700                                                                          
003800                                                                          
003900 FD  W1167E                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  W1167E-POST  -COPY W1145A      -L.                                   
004400                                                                          
004500                                                                          
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W1167000'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 01  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005300 01  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005400                                                                          
005500*01  -COPY WWDCKONS                                                       
005600                                                                          
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006000                                                                          
006100 77  W1167D-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W1167D                       VALUE 'J'.                   
006300                                                                          
006400 01  W-DAGENS-DATUM              PIC 9(6)    VALUE ZERO.                  
006500 01  W-DAGENS-AAAAVV             PIC 9(6)    VALUE 200000.                
006600 01  FILLER REDEFINES W-DAGENS-AAAAVV.                                    
006700     03 FILLER                   PIC 9(2).                                
006800     03 W-DAGENS-VECKA           PIC 9(4).                                
006900 01  W-DAAVROP-JFR               PIC 9(6).                                
007000                                                                          
007100                                                                          
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300                                                                          
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007800     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
007900                                                                          
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100                                                                          
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300                                                                          
008400**** VARIABLER TILL WDATKONV****                                          
008500*01      -COPY WDATAREA.                                                  
008600                                                                          
008700**** VARIABLER TILL W009VADD****                                          
008800 01  W009VADD-AAVV               PIC S9(5)    COMP-3.                     
008900 01  W009VADD-ANTAL              PIC S9(3)    COMP-3.                     
009000                                                                          
009100 01  IN-AREA-START               PIC X(24)   VALUE                        
009200                                             'IN-AREA-START'.             
009300*01  AREA -COPY W1167D     -PRE IN-                                       
009400                                                                          
009500                                                                          
009600 01  ABEG-AREA-START             PIC X(24)   VALUE                        
009700                                             'ABEG-AREA-START'.           
009800*01  AREA -COPY W1145A     -PRE ABEG-                                     
009900                                                                          
010000                                                                          
010100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010200                                                                          
010300 01  NYCKLAR-TILL-DLI.                                                    
010400     03  W-IDARTNR-X.                                                     
010500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010600     03  W-IDDC-X.                                                        
010700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010800     03  W-IDDC-MIN-X.                                                    
010900         05  W-IDDC-MIN          PIC X(2)    VALUE '70'.                  
011000     03  W-IDDC-MAX-X.                                                    
011100         05  W-IDDC-MAX          PIC X(2)    VALUE '7Z'.                  
011200     03  W-WDK723KY-X.                                                    
011300         05  W-WDK723KY          PIC X(12)    VALUE SPACE.                
011400     03  W-IDLAND-X.                                                      
011500         05  W-IDLAND            PIC X(2)    VALUE 'CN'.                  
011600     03  W-WDC901KY-X.                                                    
011700         05 W-IDDC-C9            PIC X(2).                                
011800         05 W-IDARTNR-C9         PIC S9(9)           COMP-3.              
011900     03  W-IDLEVNR-X.                                                     
012000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
012100     03  W-WDD901KY-X.                                                    
012200         05 W-IDARTNR-D9         PIC S9(9)           COMP-3.              
012300         05 W-IDDC-D9            PIC X(2).                                
012400     03  W-WDD905KY-X.                                                    
012500         05  W-WDD905KY          PIC X(7)    VALUE SPACE.                 
012600     03  W-WDG301KY-X.                                                    
012700         05  W-WDG3KEY           PIC X(30)    VALUE SPACE.                
012800                                                                          
012900                                                                          
013000*    --- STATUS-KOD FRÅN IMS                                              
013100 01  STATUS-WS                   PIC XX.                                  
013200     88  SEGMENT-FINNS                       VALUE '  '.                  
013300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013600     88  IMS-EJ-OK                           VALUE 'XD'.                  
013700                                                                          
013800 01  GODK-STATUSKODER.                                                    
013900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014000                                                                          
014100 01  SSA1                        PIC X(64).                               
014200 01  SSA2                        PIC X(64).                               
014300 01  SSA3                        PIC X(64).                               
014400                                                                          
014500                                                                          
014600*    --- IMS FUNKTIONSKODER                                               
014700*01  -COPY W0003                                                          
014800                                                                          
014900                                                                          
015000*    ---  DLI INPUT-OUTPUT AREA                                           
015100                                                                          
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
015300 01  DLI-IO-WDK701.                                                       
015400*    03  -COPY WDK701                                                     
015500                                                                          
015600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
015700 01  DLI-IO-WDK711.                                                       
015800*    03  -COPY WDK711                                                     
015900                                                                          
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
016100 01  DLI-IO-WDK722.                                                       
016200*    03  -COPY WDK722                                                     
016300                                                                          
016400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
016500 01  DLI-IO-WDK723.                                                       
016600*    03  -COPY WDK723                                                     
016700                                                                          
016800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
016900 01  DLI-IO-WDK712.                                                       
017000*    03  -COPY WDK712                                                     
017100                                                                          
017200                                                                          
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC901'.                      
017400 01  DLI-IO-WDC901.                                                       
017500*    03  -COPY WDC901                                                     
017600                                                                          
017700                                                                          
017800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
017900 01  DLI-IO-WDD901.                                                       
018000*    03  -COPY WDD901 -PRE D901-                                          
018100                                                                          
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
018300 01  DLI-IO-WDD902.                                                       
018400*    03  -COPY WDD902 -PRE D902-                                          
018500                                                                          
018600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
018700 01  DLI-IO-WDD905.                                                       
018800*    03  -COPY WDD905 -PRE D905-                                          
018900                                                                          
019000                                                                          
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG301'.                      
019200 01  DLI-IO-WDG301.                                                       
019300*    03  -COPY WDG301                                                     
019400                                                                          
019500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG302'.                      
019600 01  DLI-IO-WDGX2204.                                                     
019700*    03  -COPY WDGX2204                                                   
019800                                                                          
019900                                                                          
020000 LINKAGE SECTION.                                                         
020100                                                                          
020200*01  -COPY W0009  -PRE MSG-                                               
020300                                                                          
020400*01  -COPY W0008  -PRE WDK7-                                              
020500     05  FILLER                  PIC X.                                   
020600                                                                          
020700*01  -COPY W0008  -PRE WDK7-2-                                            
020800     05  FILLER                  PIC X.                                   
020900                                                                          
021000*01  -COPY W0008  -PRE WDC9-                                              
021100     05  FILLER                  PIC X.                                   
021200                                                                          
021300*01  -COPY W0008  -PRE WDD9-                                              
021400     05  FILLER                  PIC X.                                   
021500                                                                          
021600*01  -COPY W0008  -PRE WDG3-                                              
021700     05  FILLER                  PIC X.                                   
021800                                                                          
021900                                                                          
022000 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB WDK7-2-PCB                    
022100                                   WDC9-PCB WDD9-PCB WDG3-PCB.            
022200 MAIN SECTION.                                                            
022300     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB WDK7-2-PCB                    
022400                                   WDC9-PCB WDD9-PCB WDG3-PCB.            
022500                                                                          
022600     PERFORM A-INIT                                                       
022700     PERFORM S01-LAES-W1167D                                              
022800     PERFORM UNTIL END-OF-W1167D                                          
022900                                                                          
023000        IF IN-IDPTYP = 'BAC'                                              
023100           PERFORM B-BACKNING                                             
023200        ELSE                                                              
023300           PERFORM C-ERSATTNING                                           
023400        END-IF                                                            
023500                                                                          
023600       PERFORM S01-LAES-W1167D                                            
023700     END-PERFORM                                                          
023800                                                                          
023900                                                                          
024000     PERFORM Z-FINIT                                                      
024100                                                                          
024200     MOVE ZERO TO RETURN-CODE                                             
024300     GOBACK                                                               
024400     .                                                                    
024500                                                                          
024600                                                                          
024700 A-INIT SECTION.                                                          
024800     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
024900                                                                          
025000     MOVE 'IDAG'          TO DAT-KDDATFORM                                
025100     CALL WDATKONV USING     DAT-KDDATFORM                                
025200                             DAT-I-TIDATUM                                
025300                             DAT-O-TIDATUM                                
025400                             DAT-KDSVAR                                   
025500                                                                          
025600     MOVE DAT-TIAAMMDD    TO W-DAGENS-DATUM                               
025700     MOVE DAT-TIAAVV-GRP  TO W-DAGENS-VECKA                               
025800                                                                          
025900     OPEN INPUT  W1167D                                                   
026000     OPEN OUTPUT W1167E                                                   
026100                                                                          
026200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026300     .                                                                    
026400                                                                          
026500                                                                          
026600 B-BACKNING SECTION.                                                      
026700     MOVE 'B-BACKNING      ' TO CURRENT-SECTION                           
026800                                                                          
026900     MOVE IN-IDARTNR  TO W-IDARTNR                                        
027000     PERFORM IMS-GHU-WDK712                                               
027100     IF SEGMENT-FINNS                                                     
027200        MOVE ZERO     TO LART-TIERSDAT-VIPS                               
027300        PERFORM IMS-REPL-WDK712                                           
027400     END-IF                                                               
027500     .                                                                    
027600                                                                          
027700                                                                          
027800 C-ERSATTNING SECTION.                                                    
027900     MOVE 'C-ERSATTNING    ' TO CURRENT-SECTION                           
028000                                                                          
028100     MOVE IN-IDARTNR  TO W-IDARTNR                                        
028200                                                                          
028300     PERFORM IMS-GHU-WDK712                                               
028400     IF SEGMENT-FINNS                                                     
028500        MOVE W-DAGENS-DATUM TO LART-TIERSDAT-VIPS                         
028600        PERFORM IMS-REPL-WDK712                                           
028700     END-IF                                                               
028800                                                                          
028900     PERFORM IMS-GU-WDK701                                                
029000     IF SEGMENT-FINNS                                                     
029100        PERFORM IMS-GHNP-WDK711                                           
029200        PERFORM UNTIL SEGMENT-SAKNAS                                      
029300                                                                          
029400           MOVE SLAG-IDDC TO W-IDDC                                       
029500           PERFORM IMS-GU-WDK723                                          
029600           IF SEGMENT-FINNS                                               
029700              PERFORM CA-BEGAR-ANNULLATION                                
029800           END-IF                                                         
029900                                                                          
030000           MOVE ZERO TO SLAG-KVPB-REF                                     
030100           PERFORM IMS-REPL-WDK711                                        
030200                                                                          
030300           PERFORM IMS-GHNP-WDK711                                        
030400                                                                          
030500        END-PERFORM                                                       
030600     END-IF                                                               
030700                                                                          
030800                                                                          
030900     PERFORM CB-SKAPA-HANDELSE-2204                                       
031000                                                                          
031100     PERFORM CC-ANNULLERA-KOPANMODAN                                      
031200                                                                          
031300     PERFORM CD-RENSA-LEVERANSPLAN                                        
031400     .                                                                    
031500                                                                          
031600                                                                          
031700 CA-BEGAR-ANNULLATION SECTION.                                            
031800     MOVE 'CA-BEG-ANNULL  ' TO CURRENT-SECTION                            
031900                                                                          
032000     PERFORM IMS-GU-WDK722                                                
032100     IF SEGMENT-SAKNAS                                                    
032200        MOVE ZERO      TO XLAG-IDANSK                                     
032300     END-IF                                                               
032400                                                                          
032500     MOVE W-IDARTNR    TO ABEG-IDARTNR                                    
032600     MOVE W-IDDC       TO ABEG-IDDC                                       
032700     MOVE XLAG-IDANSK  TO ABEG-IDANSK                                     
032800                                                                          
032900     PERFORM S10-SKRIV-W1167E                                             
033000     .                                                                    
033100                                                                          
033200                                                                          
033300 CB-SKAPA-HANDELSE-2204 SECTION.                                          
033400     MOVE 'CB-SKAPA-2204  ' TO CURRENT-SECTION                            
033500                                                                          
033600     .                                                                    
033700                                                                          
033800                                                                          
033900 CC-ANNULLERA-KOPANMODAN SECTION.                                         
034000     MOVE 'CC-ANNULL-ANMOD' TO CURRENT-SECTION                            
034100                                                                          
034200     MOVE W-IDARTNR    TO W-IDARTNR-C9                                    
034300     MOVE WC-NDC-CN-71 TO W-IDDC-C9                                       
034400     PERFORM CCA-RENSA-ANMODAN                                            
034500                                                                          
034600     MOVE WC-NDC-CN-72 TO W-IDDC-C9                                       
034700     PERFORM CCA-RENSA-ANMODAN                                            
034800                                                                          
034900     MOVE WC-NDC-CN-73 TO W-IDDC-C9                                       
035000     PERFORM CCA-RENSA-ANMODAN                                            
035100                                                                          
035110     MOVE WC-NDC-CN-74 TO W-IDDC-C9                                       
035120     PERFORM CCA-RENSA-ANMODAN                                            
035200     .                                                                    
035300                                                                          
035400                                                                          
035500 CCA-RENSA-ANMODAN       SECTION.                                         
035600     MOVE 'CCA-RENSA-ANMOD' TO CURRENT-SECTION                            
035700                                                                          
035800     PERFORM IMS-GHU-WDC901                                               
035900     IF SEGMENT-FINNS                                                     
036000        IF  KART-KDANSKQ = '2'                                            
036100        AND KART-TIINKOP > ZERO                                           
036200           MOVE ZERO  TO KART-TILEVBEG                                    
036300                         KART-KVPROG                                      
036400                         KART-TIINKOP                                     
036500           PERFORM IMS-REPL-WDC901                                        
036600        ELSE                                                              
036700           PERFORM IMS-DLET-WDC901                                        
036800        END-IF                                                            
036900     END-IF                                                               
037000     .                                                                    
037100                                                                          
037200                                                                          
037300 CD-RENSA-LEVERANSPLAN  SECTION.                                          
037400     MOVE 'CD-RENSA-LEVPL ' TO CURRENT-SECTION                            
037500                                                                          
037600     MOVE W-IDARTNR    TO W-IDARTNR-D9                                    
037700     MOVE WC-NDC-CN-71 TO W-IDDC-D9                                       
037800     PERFORM CDA-RENSA-DC                                                 
037900                                                                          
038000     MOVE WC-NDC-CN-72 TO W-IDDC-D9                                       
038100     PERFORM CDA-RENSA-DC                                                 
038200                                                                          
038300     MOVE WC-NDC-CN-73 TO W-IDDC-D9                                       
038400     PERFORM CDA-RENSA-DC                                                 
038410                                                                          
038420     MOVE WC-NDC-CN-74 TO W-IDDC-D9                                       
038430     PERFORM CDA-RENSA-DC                                                 
038500     .                                                                    
038600                                                                          
038700                                                                          
038800 CDA-RENSA-DC           SECTION.                                          
038900     MOVE 'CDA-RENSA-DC   ' TO CURRENT-SECTION                            
039000                                                                          
039100     PERFORM IMS-GU-WDD901                                                
039200     IF SEGMENT-FINNS                                                     
039300        PERFORM IMS-GHNP-WDD905                                           
039400        IF SEGMENT-FINNS                                                  
039500           IF D905-KDAVROP = 1                                            
039600              PERFORM IMS-DLET-WDD905                                     
039700           ELSE                                                           
039800              IF D905-KDAVROP = 2                                         
039900                 MOVE D901-IDDC        TO W-IDDC                          
040000                 PERFORM IMS-GU-WDK722                                    
040100                 MOVE W-DAGENS-VECKA   TO W009VADD-AAVV                   
040200                 COMPUTE W009VADD-ANTAL = XLAG-KVVECKOR-LT + 1            
040300                 CALL W009VADD USING W009VADD-AAVV                        
040400                                     W009VADD-ANTAL                       
040500                 MOVE W009VADD-AAVV    TO W-DAAVROP-JFR                   
040600                 IF D905-DAAVROP-AVS > W-DAAVROP-JFR                      
040700                    PERFORM IMS-DLET-WDD905                               
040800                 END-IF                                                   
040900              END-IF                                                      
041000           END-IF                                                         
041100        END-IF                                                            
041200     END-IF                                                               
041300     .                                                                    
041400                                                                          
041500                                                                          
041600 Z-FINIT SECTION.                                                         
041700     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
041800                                                                          
041900                                                                          
042000     CLOSE W1167D                                                         
042100           W1167E                                                         
042200                                                                          
042300     MOVE 'S' TO POSTSUM-OPKOD                                            
042400     CALL POSTSUM USING POSTSUM-PARM                                      
042500     .                                                                    
042600                                                                          
042700                                                                          
042800 S01-LAES-W1167D  SECTION.                                                
042900     MOVE 'S01-LAES-W1167D ' TO CURRENT-SECTION                           
043000                                                                          
043100     READ W1167D INTO IN-AREA                                             
043200     AT END                                                               
043300        MOVE HIGH-VALUE TO IN-IDPTYP                                      
043400        SET END-OF-W1167D TO TRUE                                         
043500                                                                          
043600     NOT AT END                                                           
043700        MOVE 'W1167D'   TO POSTSUM-FDNAMN                                 
043800        MOVE 'W11670D1' TO POSTSUM-DDNAMN2                                
043900        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
044000        CALL POSTSUM USING POSTSUM-PARM                                   
044100     END-READ                                                             
044200     .                                                                    
044300                                                                          
044400                                                                          
044500  S10-SKRIV-W1167E SECTION.                                               
044600     MOVE 'S10-SKRIV-W1167E' TO CURRENT-SECTION                           
044700                                                                          
044800       WRITE W1167E-POST  FROM ABEG-AREA                                  
044900       MOVE 'W1167E'        TO POSTSUM-FDNAMN                             
045000       MOVE 'W11670D2'      TO POSTSUM-DDNAMN2                            
045100       MOVE SPACE           TO POSTSUM-TRANSTYP                           
045200       CALL POSTSUM USING POSTSUM-PARM                                    
045300       .                                                                  
045400                                                                          
045500                                                                          
045600* --- IMS SEKTIONER ---                                                   
045700                                                                          
045800 IMS-GU-WDK701 SECTION.                                                   
045900     MOVE 'IMS-GU-WDK701   ' TO CURRENT-IMS-SECTION                       
046000                                                                          
046100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
046200          DELIMITED BY SIZE INTO SSA1                                     
046300     MOVE '  GE'              TO GODK-STATUSKODER                         
046400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
046500     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
046600     PERFORM IMS-STATUSKONTROLL                                           
046700     .                                                                    
046800                                                                          
046900                                                                          
047000 IMS-GHNP-WDK711 SECTION.                                                 
047100     MOVE 'IMS-GHNP-WDK711 ' TO CURRENT-IMS-SECTION                       
047200                                                                          
047300     STRING 'WDK711  (IDDC    =>' W-IDDC-MIN                              
047400                    '&IDDC    =<' W-IDDC-MAX ')'                          
047500          DELIMITED BY SIZE INTO SSA1                                     
047600     MOVE '  GE'              TO GODK-STATUSKODER                         
047700     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
047800     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
047900     PERFORM IMS-STATUSKONTROLL                                           
048000     .                                                                    
048100           ,                                                              
048200           ,                                                              
048300 IMS-REPL-WDK711 SECTION.                                                 
048400     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
048500                                                                          
048600     MOVE '  '             TO GODK-STATUSKODER                            
048700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
048800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
048900     PERFORM IMS-STATUSKONTROLL                                           
049000     .                                                                    
049100                                                                          
049200                                                                          
049300 IMS-GU-WDK722 SECTION.                                                   
049400     MOVE 'IMS-GU-WDK722   ' TO CURRENT-IMS-SECTION                       
049500                                                                          
049600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
049700          DELIMITED BY SIZE INTO SSA1                                     
049800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
049900          DELIMITED BY SIZE INTO SSA2                                     
050000     MOVE 'WDK722 '           TO SSA3                                     
050100     MOVE '    '              TO GODK-STATUSKODER                         
050200     CALL CBLTDLI USING GHNP WDK7-2-PCB DLI-IO-WDK722                     
050300                             SSA1 SSA2 SSA3                               
050400     MOVE WDK7-2-STATUS-CODE  TO STATUS-WS                                
050500     PERFORM IMS-STATUSKONTROLL                                           
050600     .                                                                    
050700     SKIP3                                                                
050800 IMS-GU-WDK723 SECTION.                                                   
050900     MOVE 'IMS-GU-WDK723   ' TO CURRENT-IMS-SECTION                       
051000                                                                          
051100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
051200          DELIMITED BY SIZE INTO SSA1                                     
051300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
051400          DELIMITED BY SIZE INTO SSA2                                     
051500     MOVE   'WDK723 '         TO SSA3                                     
051600     MOVE '  GE' TO GODK-STATUSKODER                                      
051700     CALL CBLTDLI USING GHNP WDK7-2-PCB DLI-IO-WDK723                     
051800                                        SSA1 SSA2 SSA3                    
051900     MOVE WDK7-2-STATUS-CODE  TO STATUS-WS                                
052000     PERFORM IMS-STATUSKONTROLL                                           
052100     .                                                                    
052200     SKIP3                                                                
052300 IMS-GHU-WDK712 SECTION.                                                  
052400     MOVE 'IMS-GHU-WDK712  ' TO CURRENT-IMS-SECTION                       
052500                                                                          
052600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
052700          DELIMITED BY SIZE INTO SSA1                                     
052800     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
052900          DELIMITED BY SIZE INTO SSA2                                     
053000     MOVE '  GE'              TO GODK-STATUSKODER                         
053100     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
053200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
053300     PERFORM IMS-STATUSKONTROLL                                           
053400     .                                                                    
053500                                                                          
053600                                                                          
053700 IMS-REPL-WDK712 SECTION.                                                 
053800     MOVE 'IMS-REPL-WDK712 ' TO CURRENT-IMS-SECTION                       
053900                                                                          
054000     MOVE '  '             TO GODK-STATUSKODER                            
054100     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
054200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
054300     PERFORM IMS-STATUSKONTROLL                                           
054400     .                                                                    
054500                                                                          
054600                                                                          
054700 IMS-GHU-WDC901 SECTION.                                                  
054800     MOVE 'IMS-GHU-WDC901  ' TO CURRENT-IMS-SECTION                       
054900                                                                          
055000     STRING 'WDC901  (WDC901KY =' W-WDC901KY-X ')'                        
055100          DELIMITED BY SIZE INTO SSA1                                     
055200     MOVE '  GE'              TO GODK-STATUSKODER                         
055300     CALL CBLTDLI USING GHU WDC9-PCB DLI-IO-WDC901 SSA1                   
055400     MOVE WDC9-STATUS-CODE    TO STATUS-WS                                
055500     PERFORM IMS-STATUSKONTROLL                                           
055600     .                                                                    
055700                                                                          
055800                                                                          
055900 IMS-REPL-WDC901 SECTION.                                                 
056000     MOVE 'IMS-REPL-WDC901 ' TO CURRENT-IMS-SECTION                       
056100                                                                          
056200     MOVE '  '             TO GODK-STATUSKODER                            
056300     CALL CBLTDLI USING REPL WDC9-PCB DLI-IO-WDC901                       
056400     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
056500     PERFORM IMS-STATUSKONTROLL                                           
056600     .                                                                    
056700                                                                          
056800                                                                          
056900 IMS-DLET-WDC901 SECTION.                                                 
057000     MOVE 'IMS-DLET-WDC901 ' TO CURRENT-IMS-SECTION                       
057100                                                                          
057200     MOVE '  '             TO GODK-STATUSKODER                            
057300     CALL CBLTDLI USING DLET WDC9-PCB DLI-IO-WDC901                       
057400     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
057500     PERFORM IMS-STATUSKONTROLL                                           
057600     .                                                                    
057700                                                                          
057800                                                                          
057900 IMS-GU-WDD901 SECTION.                                                   
058000     MOVE 'IMS-GU-WDD901   ' TO CURRENT-IMS-SECTION                       
058100                                                                          
058200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
058300          DELIMITED BY SIZE INTO SSA1                                     
058400     MOVE '  GE'              TO GODK-STATUSKODER                         
058500     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
058600     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
058700     PERFORM IMS-STATUSKONTROLL                                           
058800     .                                                                    
058900                                                                          
059000                                                                          
059100 IMS-GHNP-WDD905 SECTION.                                                 
059200     MOVE 'IMS-GHNP-WDD905 ' TO CURRENT-IMS-SECTION                       
059300                                                                          
059400     MOVE 'WDD905 '        TO SSA1                                        
059500     MOVE '  GE' TO GODK-STATUSKODER                                      
059600     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1                  
059700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
059800     PERFORM IMS-STATUSKONTROLL                                           
059900     .                                                                    
060000                                                                          
060100                                                                          
060200 IMS-DLET-WDD905 SECTION.                                                 
060300     MOVE 'IMS-DLET-WDD905 ' TO CURRENT-IMS-SECTION                       
060400                                                                          
060500     MOVE '  '             TO GODK-STATUSKODER                            
060600     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
060700     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
060800     PERFORM IMS-STATUSKONTROLL                                           
060900     .                                                                    
061000                                                                          
061100                                                                          
061200 IMS-ISRT-WDG301 SECTION.                                                 
061300                                                                          
061400     MOVE 'WDG301 '        TO SSA1                                        
061500     MOVE '  II'           TO GODK-STATUSKODER                            
061600     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDG301 SSA1                  
061700     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
061800     PERFORM IMS-STATUSKONTROLL                                           
061900     .                                                                    
062000     EJECT                                                                
062100 IMS-ISRT-WDGX02 SECTION.                                                 
062200                                                                          
062300     STRING 'WDG301  (WDG301KY =' W-WDG301KY-X ')'                        
062400          DELIMITED BY SIZE INTO SSA1                                     
062500     MOVE 'WDG302 '           TO SSA2                                     
062600     MOVE '  II'              TO GODK-STATUSKODER                         
062700     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2204 SSA1 SSA2           
062800     MOVE WDG3-STATUS-CODE    TO STATUS-WS                                
062900     PERFORM IMS-STATUSKONTROLL                                           
063000     .                                                                    
063100     EJECT                                                                
063200 IMS-STATUSKONTROLL SECTION.                                              
063300     SKIP2                                                                
063400     SET STATUS-IX TO 1                                                   
063500     SEARCH GODK-STATUS                                                   
063600       AT END                                                             
063700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
063800           DELIMITED BY SIZE INTO FELTEXT                                 
063900         DISPLAY FELTEXT                                                  
064000         CALL FELLOG                                                      
064100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
064200         CONTINUE                                                         
064300     END-SEARCH                                                           
064400     .                                                                    
