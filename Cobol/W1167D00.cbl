000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1167D00.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/01/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        UPPDATERAR TIERSDAT-VIPS FÖR ALL NDC MARKNADER                   
001000*        DÄR ARTIKELN BLIVIT ERSATT.                                      
001100*                                                                         
001200*        FÖRE USA OCH KINA SOM HAR LOCAL SOURCING UPPDATERAS              
001300*        ÄVEN ANNAN INFO (AVTALS- OCH ANSKAFFAR-INFO)                     
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WDK7                                       
001600*        PROGRAMMET LÄSER      WDC9                                       
001700*        PROGRAMMET UPPDATERAR WDG3                                       
001800*        PROGRAMMET UPPDATERAR WDR3                                       
001900*                                                                         
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*    --- ERSÄTTNINGSMEDDELANDE KINA                                       
002800     SELECT W1167D                     ASSIGN TO W1167DD1.                
002900                                                                          
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200                                                                          
003300 FD  W1167D                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W1167D      -L.                                                
003800                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W1167D00'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004500 01  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004600 01  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004700                                                                          
004800*01  -COPY WWLNDKON                                                       
004900                                                                          
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005300                                                                          
005400 77  W-ANTAL-POSTER              PIC 9(7)    VALUE ZERO.                  
005500                                                                          
005600 77  W1167D-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W1167D                       VALUE 'J'.                   
005800                                                                          
005900 77  WDK711-TRAFF-SW             PIC X       VALUE 'N'.                   
006000     88  WDK711-TRAFF                        VALUE 'J'.                   
006100                                                                          
006200 01  W-DAGENS-DATUM              PIC 9(6)    VALUE ZERO.                  
006300 01  W-DAGENS-AAAAVV             PIC 9(6)    VALUE 200000.                
006400 01  FILLER REDEFINES W-DAGENS-AAAAVV.                                    
006500     03 FILLER                   PIC 9(2).                                
006600     03 W-DAGENS-VECKA           PIC 9(4).                                
006700                                                                          
006800                                                                          
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000                                                                          
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
007400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007500     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
007600                                                                          
007700*    --- PARAMETRAR TILL POSTSUM                                          
007800                                                                          
007900*01  -COPY W0005   -PRE  POSTSUM-                                         
008000                                                                          
008100**** VARIABLER TILL WDATKONV****                                          
008200*01      -COPY WDATAREA.                                                  
008300                                                                          
008400*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
008500*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'W005WDK7'.            
008700*01 -COPY W005WDK7                                                        
008800                                                                          
008900                                                                          
009000 01  IN-AREA-START               PIC X(24)   VALUE                        
009100                                             'IN-AREA-START'.             
009200*01  AREA -COPY W1167D     -PRE IN-                                       
009300                                                                          
009400                                                                          
009500 01  ABEG-AREA-START             PIC X(24)   VALUE                        
009600                                             'ABEG-AREA-START'.           
009700*01  AREA -COPY W1145A     -PRE ABEG-                                     
009800                                                                          
009900                                                                          
010000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010100                                                                          
010200 01  CHKP-VAR.                                                            
010300 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
010400 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
010500 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
010600 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
010700 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
010800 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
010900                                                                          
011000 01  NYCKLAR-TILL-DLI.                                                    
011100     03  W-IDARTNR-X.                                                     
011200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011300     03  W-IDDC-X.                                                        
011400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011500     03  W-IDDC-MIN-X.                                                    
011600         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
011700     03  W-IDDC-MAX-X.                                                    
011800         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
011900     03  W-WDK723KY-X.                                                    
012000         05  W-WDK723KY          PIC X(12)    VALUE SPACE.                
012100     03  W-IDLAND-X.                                                      
012200         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
012300     03  W-WDC901KY-X.                                                    
012400         05 W-IDDC-C9            PIC X(2)    VALUE SPACE.                 
012500         05 W-IDARTNR-C9         PIC S9(9)   VALUE ZERO COMP-3.           
012600     03  W-IDLEVNR-X.                                                     
012700         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
012800     03  W-WDG301-2203-X.                                                 
012900         05  W-IDHTYP            PIC X(04)   VALUE '2203'.                
013000         05  W-IDDC-2203         PIC X(02)   VALUE SPACE.                 
013100         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
013200     03  W-WDGXKEY-4579-X.                                                
013300          05 W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
013400          05 W-IDPGM             PIC X(8)    VALUE 'W1167D00'.            
013500          05 FILLER              PIC X(18)   VALUE LOW-VALUE.             
013600                                                                          
013700                                                                          
013800*    --- STATUS-KOD FRÅN IMS                                              
013900 01  STATUS-WS                   PIC XX.                                  
014000     88  SEGMENT-FINNS                       VALUE '  '.                  
014100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014400     88  IMS-EJ-OK                           VALUE 'XD'.                  
014500                                                                          
014600 01  GODK-STATUSKODER.                                                    
014700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014800                                                                          
014900 01  ALL-SSA.                                                             
015000     03 SSA1                     PIC X(64).                               
015100     03 SSA2                     PIC X(64).                               
015200     03 SSA3                     PIC X(64).                               
015300                                                                          
015400                                                                          
015500*    --- IMS FUNKTIONSKODER                                               
015600*01  -COPY W0003                                                          
015700                                                                          
015800                                                                          
015900*    ---  DLI INPUT-OUTPUT AREA                                           
016000                                                                          
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
016200 01  DLI-IO-WDK701.                                                       
016300*    03  -COPY WDK701                                                     
016400                                                                          
016500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
016600 01  DLI-IO-WDK711.                                                       
016700*    03  -COPY WDK711                                                     
016800                                                                          
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
017000 01  DLI-IO-WDK722.                                                       
017100*    03  -COPY WDK722                                                     
017200                                                                          
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
017400 01  DLI-IO-WDK723.                                                       
017500*    03  -COPY WDK723                                                     
017600                                                                          
017700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
017800 01  DLI-IO-WDK712.                                                       
017900*    03  -COPY WDK712                                                     
018000                                                                          
018100                                                                          
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC901'.                      
018300 01  DLI-IO-WDC901.                                                       
018400*    03  -COPY WDC901                                                     
018500                                                                          
018600                                                                          
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG302'.                      
018800 01  DLI-IO-WDGX2204.                                                     
018900*    03  -COPY WDGX2204                                                   
019000                                                                          
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR301'.                      
019200 01  DLI-IO-WDR301.                                                       
019300*    03  -COPY WDR301                                                     
019400       05   -COPY W1145A  -RED FIL-WDR301-DATA -PRE 5A-                   
019500                                                                          
019600                                                                          
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
019800 01  DLI-IO-WDGX4580.                                                     
019900*    03  -COPY WDGX4580                                                   
020000                                                                          
020100                                                                          
020200 LINKAGE SECTION.                                                         
020300                                                                          
020400*01  -COPY W0009  -PRE MSG-                                               
020500                                                                          
020600*01  -COPY W0008  -PRE WDK7-                                              
020700     05  FILLER                  PIC X.                                   
020800                                                                          
020900*01  -COPY W0008  -PRE WDC9-                                              
021000     05  FILLER                  PIC X.                                   
021100                                                                          
021200*01  -COPY W0008  -PRE WDG3-                                              
021300     05  FILLER                  PIC X.                                   
021400                                                                          
021500*01  -COPY W0008  -PRE WDR3-                                              
021600     05  FILLER                  PIC X.                                   
021700                                                                          
021800*01  -COPY W0008  -PRE 4579-                                              
021900     05  FILLER                  PIC X.                                   
022000                                                                          
022100 01  W005K7-WDB6-PCB             PIC X.                                   
022200                                                                          
022300 01  W005K7-WDK6-PCB             PIC X.                                   
022400                                                                          
022500 01  W005K7-WDK7-PCB             PIC X.                                   
022600                                                                          
022700                                                                          
022800 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB WDC9-PCB                      
022900                                   WDG3-PCB                               
023000                                   WDR3-PCB 4579-PCB                      
023100                                   W005K7-WDB6-PCB                        
023200                                   W005K7-WDK6-PCB                        
023300                                   W005K7-WDK7-PCB.                       
023400 MAIN SECTION.                                                            
023500     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB WDC9-PCB                      
023600                                   WDG3-PCB                               
023700                                   WDR3-PCB 4579-PCB                      
023800                                   W005K7-WDB6-PCB                        
023900                                   W005K7-WDK6-PCB                        
024000                                   W005K7-WDK7-PCB.                       
024100                                                                          
024200     PERFORM A-INIT                                                       
024300     PERFORM S01-LAES-W1167D                                              
024400     PERFORM UNTIL END-OF-W1167D                                          
024500                                                                          
024600        MOVE IN-IDARTNR TO W-IDARTNR                                      
024700        PERFORM IMS-GU-WDK701                                             
024800        IF SEGMENT-FINNS                                                  
024900          IF IN-IDLANDX2 = WC-LAND-CN OR WC-LAND-US                       
025000            IF IN-IDPTYP = 'BAC'                                          
025100               PERFORM B-BACKNING                                         
025200            ELSE                                                          
025300               PERFORM C-ERSATTNING                                       
025400            END-IF                                                        
025500          ELSE                                                            
025600            IF IN-IDPTYP = 'BAC'                                          
025700               PERFORM B-BACKNING                                         
025800            ELSE                                                          
025900               PERFORM D-ERSATTNING                                       
026000            END-IF                                                        
026100          END-IF                                                          
026200          IF CHKP-ANT > CHKP-MAX                                          
026300             PERFORM X-TAG-CHECKPOINT                                     
026400          END-IF                                                          
026500        END-IF                                                            
026600        PERFORM S01-LAES-W1167D                                           
026700     END-PERFORM                                                          
026800                                                                          
026900                                                                          
027000     PERFORM Z-FINIT                                                      
027100                                                                          
027200     MOVE ZERO TO RETURN-CODE                                             
027300     GOBACK                                                               
027400     .                                                                    
027500                                                                          
027600                                                                          
027700 A-INIT SECTION.                                                          
027800     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
027900                                                                          
028000     MOVE 'IDAG'          TO DAT-KDDATFORM                                
028100     CALL WDATKONV USING     DAT-KDDATFORM                                
028200                             DAT-I-TIDATUM                                
028300                             DAT-O-TIDATUM                                
028400                             DAT-KDSVAR                                   
028500                                                                          
028600     MOVE DAT-TIAAMMDD    TO W-DAGENS-DATUM                               
028700     MOVE DAT-TIAAVV-GRP  TO W-DAGENS-VECKA                               
028800                                                                          
028900     OPEN INPUT  W1167D                                                   
029000                                                                          
029100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029200                                                                          
029300     PERFORM IMS-RESTART                                                  
029400                                                                          
029500     PERFORM IMS-GHU-RESTART                                              
029600     IF 4580-KVPOST > ZERO                                                
029700        MOVE ZERO TO W-ANTAL-POSTER                                       
029800        PERFORM UNTIL W-ANTAL-POSTER = 4580-KVPOST                        
029900           PERFORM S01-LAES-W1167D                                        
030000           ADD 1  TO W-ANTAL-POSTER                                       
030100        END-PERFORM                                                       
030200     END-IF                                                               
030300     .                                                                    
030400                                                                          
030500                                                                          
030600 B-BACKNING SECTION.                                                      
030700     MOVE 'B-BACKNING      ' TO CURRENT-SECTION                           
030800                                                                          
030900     MOVE IN-IDLANDX2   TO W-IDLANDX2                                     
031000     PERFORM IMS-GHNP-WDK712                                              
031100     IF SEGMENT-FINNS                                                     
031200        MOVE ZERO     TO LART-TIERSDAT-VIPS                               
031300        PERFORM IMS-REPL-WDK712                                           
031400     END-IF                                                               
031500     .                                                                    
031600                                                                          
031700                                                                          
031800 C-ERSATTNING SECTION.                                                    
031900     MOVE 'C-ERSATTNING    ' TO CURRENT-SECTION                           
032000                                                                          
032100     MOVE LOW-VALUES        TO W-IDDC-MIN                                 
032200     MOVE HIGH-VALUES       TO W-IDDC-MAX                                 
032300     IF IN-IDLANDX2 = WC-LAND-CN                                          
032400       MOVE '71'            TO W-IDDC-MIN                                 
032500       MOVE '79'            TO W-IDDC-MAX                                 
032600     END-IF                                                               
032700     IF IN-IDLANDX2 = WC-LAND-US                                          
032800       MOVE '41'            TO W-IDDC-MIN                                 
032900       MOVE '49'            TO W-IDDC-MAX                                 
033000     END-IF                                                               
033100                                                                          
033200     PERFORM IMS-GHNP-WDK711                                              
033300     PERFORM UNTIL SEGMENT-SAKNAS                                         
033400                                                                          
033500        MOVE ZERO           TO SLAG-KVPB-REF                              
033600                               SLAG-KVPBREOI                              
033700        MOVE W-DAGENS-DATUM TO SLAG-TIREFSTA                              
033800        MOVE 'P'            TO SLAG-KDREFSTA                              
033900        PERFORM IMS-REPL-WDK711                                           
034000                                                                          
034100        MOVE SLAG-IDDC TO W-IDDC                                          
034200                                                                          
034300        IF SLAG-IDDC-REF = SPACE                                          
034400           PERFORM CA-SKAPA-HANDELSE-2204                                 
034500        END-IF                                                            
034600                                                                          
034700        PERFORM IMS-GNP-WDK722                                            
034800        IF SEGMENT-SAKNAS                                                 
034900           MOVE ZERO      TO XLAG-IDANSK                                  
035000        END-IF                                                            
035100                                                                          
035200        PERFORM CC-ANNULLERA-KOPANMODAN                                   
035300                                                                          
035400        PERFORM IMS-GNP-WDK723                                            
035500        IF SEGMENT-FINNS                                                  
035600           PERFORM CD-BEGAR-ANNULLATION                                   
035700        END-IF                                                            
035800                                                                          
035900        PERFORM IMS-GHNP-WDK711                                           
036000                                                                          
036100     END-PERFORM                                                          
036200                                                                          
036300     MOVE IN-IDLANDX2   TO W-IDLANDX2                                     
036400     PERFORM IMS-GHNP-WDK712                                              
036500     IF SEGMENT-FINNS                                                     
036600        MOVE DAT-TIAAVVD    TO LART-TIERSDAT-VIPS                         
036700        PERFORM IMS-REPL-WDK712                                           
036800     END-IF                                                               
036900     .                                                                    
037000                                                                          
037100                                                                          
037200 CA-SKAPA-HANDELSE-2204 SECTION.                                          
037300     MOVE 'CA-SKAPA-2204  ' TO CURRENT-SECTION                            
037400                                                                          
037500     MOVE SLAG-IDDC         TO W-IDDC-2203                                
037600     MOVE IN-IDARTNR        TO 2204-IDARTNR                               
037700     MOVE 03                TO 2204-KDLPORS                               
037800                                                                          
037900     PERFORM IMS-ISRT-WDG302                                              
038000     .                                                                    
038100                                                                          
038200                                                                          
038300 CC-ANNULLERA-KOPANMODAN SECTION.                                         
038400     MOVE 'CC-ANNULL-ANMOD' TO CURRENT-SECTION                            
038500                                                                          
038600     MOVE W-IDARTNR    TO W-IDARTNR-C9                                    
038700     MOVE SLAG-IDDC    TO W-IDDC-C9                                       
038800                                                                          
038900     PERFORM IMS-GHU-WDC901                                               
039000     IF SEGMENT-FINNS                                                     
039100        IF  KART-KDANSKQ = '2'                                            
039200        AND KART-TIINKOP > ZERO                                           
039300           MOVE ZERO  TO KART-TILEVBEG                                    
039400                         KART-KVPROG                                      
039500                         KART-TIINKOP                                     
039600           PERFORM IMS-REPL-WDC901                                        
039700        ELSE                                                              
039800           PERFORM IMS-DLET-WDC901                                        
039900        END-IF                                                            
040000     END-IF                                                               
040100     .                                                                    
040200                                                                          
040300                                                                          
040400 CD-BEGAR-ANNULLATION SECTION.                                            
040500     MOVE 'CD-BEG-ANNULL  ' TO CURRENT-SECTION                            
040600                                                                          
040700     MOVE IDPGM             TO FIL-IDPGM                                  
040800     ACCEPT FIL-TIREGDAT    FROM DATE                                     
040900     ACCEPT FIL-TIKLOCK     FROM TIME                                     
041000     MOVE 1                 TO FIL-IDSEKVNR                               
041100     MOVE 'W114'            TO FIL-CT-IDSYSTEM                            
041200     MOVE 'H02'             TO FIL-CT-IDPTYP                              
041300     MOVE 'A'               TO FIL-CT-IDVTYP                              
041400     MOVE W-IDDC            TO 5A-IDDC                                    
041500     MOVE W-IDARTNR         TO 5A-IDARTNR                                 
041600     MOVE XLAG-IDANSK       TO 5A-IDANSK                                  
041700                                                                          
041800     PERFORM IMS-ISRT-WDR301                                              
041900     IF SEGMENT-FINNS-REDAN                                               
042000        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
042100           ADD 1 TO FIL-IDSEKVNR                                          
042200           PERFORM IMS-ISRT-WDR301                                        
042300        END-PERFORM                                                       
042400     END-IF                                                               
042500     .                                                                    
042600                                                                          
042700                                                                          
042800 D-ERSATTNING SECTION.                                                    
042900     MOVE 'D-ERSATTNING    ' TO CURRENT-SECTION                           
043000                                                                          
043100     MOVE LOW-VALUES        TO W-IDDC-MIN                                 
043200     MOVE HIGH-VALUES       TO W-IDDC-MAX                                 
043300     IF IN-IDLANDX2 = WC-LAND-CA                                          
043400       MOVE '51'            TO W-IDDC-MIN                                 
043500       MOVE '51'            TO W-IDDC-MAX                                 
043600     END-IF                                                               
043700     IF IN-IDLANDX2 = WC-LAND-BR                                          
043800       MOVE '52'            TO W-IDDC-MIN                                 
043900       MOVE '52'            TO W-IDDC-MAX                                 
044000     END-IF                                                               
044100     IF IN-IDLANDX2 = WC-LAND-MX                                          
044200       MOVE '53'            TO W-IDDC-MIN                                 
044300       MOVE '53'            TO W-IDDC-MAX                                 
044400     END-IF                                                               
044500     IF IN-IDLANDX2 = WC-LAND-JP                                          
044600       MOVE '6A'            TO W-IDDC-MIN                                 
044700       MOVE '61'            TO W-IDDC-MAX                                 
044800     END-IF                                                               
044900     IF IN-IDLANDX2 = WC-LAND-AU                                          
045000       MOVE '62'            TO W-IDDC-MIN                                 
045100       MOVE '62'            TO W-IDDC-MAX                                 
045200     END-IF                                                               
044900     IF IN-IDLANDX2 = WC-LAND-TH                                          
045000       MOVE '63'            TO W-IDDC-MIN                                 
045100       MOVE '63'            TO W-IDDC-MAX                                 
045200     END-IF                                                               
044900     IF IN-IDLANDX2 = WC-LAND-TW                                          
045000       MOVE '64'            TO W-IDDC-MIN                                 
045100       MOVE '64'            TO W-IDDC-MAX                                 
045200     END-IF                                                               
045300     IF IN-IDLANDX2 = WC-LAND-IN                                          
045400       MOVE '67'            TO W-IDDC-MIN                                 
045500       MOVE '67'            TO W-IDDC-MAX                                 
045600     END-IF                                                               
045700     IF IN-IDLANDX2 = WC-LAND-KR                                          
045800       MOVE '65'            TO W-IDDC-MIN                                 
045900       MOVE '65'            TO W-IDDC-MAX                                 
046000     END-IF                                                               
046010     IF IN-IDLANDX2 = WC-LAND-MY                                          
046020       MOVE '66'            TO W-IDDC-MIN                                 
046030       MOVE '66'            TO W-IDDC-MAX                                 
046040     END-IF                                                               
046100     IF IN-IDLANDX2 = WC-LAND-ZA                                          
046200       MOVE '85'            TO W-IDDC-MIN                                 
046300       MOVE '85'            TO W-IDDC-MAX                                 
046400     END-IF                                                               
046500     IF IN-IDLANDX2 = WC-LAND-TR                                          
046600       MOVE '86'            TO W-IDDC-MIN                                 
046700       MOVE '86'            TO W-IDDC-MAX                                 
046800     END-IF                                                               
046900     IF IN-IDLANDX2 = WC-LAND-AE                                          
047000       MOVE '87'            TO W-IDDC-MIN                                 
047100       MOVE '87'            TO W-IDDC-MAX                                 
047200     END-IF                                                               
047300                                                                          
047400     MOVE NEJ TO WDK711-TRAFF-SW                                          
047500     PERFORM IMS-GHNP-WDK711                                              
047600     IF SEGMENT-FINNS                                                     
047700        MOVE JA TO WDK711-TRAFF-SW                                        
047800     END-IF                                                               
047900                                                                          
048000     MOVE IN-IDLANDX2   TO W-IDLANDX2                                     
048100     PERFORM IMS-GHNP-WDK712                                              
048200     IF SEGMENT-FINNS                                                     
048300        MOVE DAT-TIAAVVD    TO LART-TIERSDAT-VIPS                         
048400        PERFORM IMS-REPL-WDK712                                           
048500     ELSE                                                                 
048600        IF WDK711-TRAFF                                                   
048700           PERFORM E-NEW-WDK712                                           
048800        END-IF                                                            
048900     END-IF                                                               
049000     .                                                                    
049100                                                                          
049200                                                                          
049300 E-NEW-WDK712 SECTION.                                                    
049400     MOVE 'E-NEW-WDK712    '  TO CURRENT-SECTION                          
049500                                                                          
049600     MOVE ALL '+'       TO WDK7-W005WDK7                                  
049700     MOVE 'WDK712'      TO WDK7-IDSEGM                                    
049800     MOVE IN-IDARTNR    TO WDK7-IDARTNR-KFB                               
049900     MOVE IN-IDLANDX2   TO WDK7-IDLANDX2-KFB                              
050000                           WDK7-IDLANDX2                                  
050100     MOVE DAT-TIAAVVD   TO WDK7-TIERSDAT-VIPS                             
050200                                                                          
050300     CALL W005WDK7  USING WDK7-W005WDK7 W005K7-WDB6-PCB                   
050400                      W005K7-WDK6-PCB W005K7-WDK7-PCB                     
050500     ADD +1 TO CHKP-ANT                                                   
050600     .                                                                    
050700                                                                          
050800                                                                          
050900 Z-FINIT SECTION.                                                         
051000     MOVE 'Z-FINIT         '  TO CURRENT-SECTION                          
051100                                                                          
051200     CLOSE W1167D                                                         
051300                                                                          
051400     MOVE 'S' TO POSTSUM-OPKOD                                            
051500     CALL POSTSUM USING POSTSUM-PARM                                      
051600                                                                          
051700     PERFORM IMS-GHU-RESTART                                              
051800     MOVE ZERO       TO 4580-KVPOST                                       
051900     ACCEPT 4580-TIUPPDAT FROM DATE                                       
052000     ACCEPT 4580-TIUPPTID FROM TIME                                       
052100     PERFORM IMS-REPL-RESTART                                             
052200     .                                                                    
052300                                                                          
052400                                                                          
052500 S01-LAES-W1167D  SECTION.                                                
052600     MOVE 'S01-LAES-W1167D ' TO CURRENT-SECTION                           
052700                                                                          
052800     READ W1167D INTO IN-AREA                                             
052900     AT END                                                               
053000        MOVE HIGH-VALUE TO IN-IDPTYP                                      
053100        SET END-OF-W1167D TO TRUE                                         
053200                                                                          
053300     NOT AT END                                                           
053400        MOVE 'W1167D'   TO POSTSUM-FDNAMN                                 
053500        MOVE 'W1167DD1' TO POSTSUM-DDNAMN2                                
053600        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
053700        CALL POSTSUM USING POSTSUM-PARM                                   
053800                                                                          
053900        ADD 1 TO W-ANTAL-POSTER                                           
054000     END-READ                                                             
054100     .                                                                    
054200                                                                          
054300                                                                          
054400 X-TAG-CHECKPOINT   SECTION.                                              
054500                                                                          
054600     PERFORM IMS-GHU-RESTART                                              
054700     MOVE W-ANTAL-POSTER TO 4580-KVPOST                                   
054800     ACCEPT 4580-TIUPPDAT FROM DATE                                       
054900     ACCEPT 4580-TIUPPTID FROM TIME                                       
055000     PERFORM IMS-REPL-RESTART                                             
055100                                                                          
055200     PERFORM IMS-CHECKPOINT                                               
055300     MOVE ZERO TO CHKP-ANT                                                
055400     .                                                                    
055500                                                                          
055600                                                                          
055700* --- IMS SEKTIONER ---                                                   
055800                                                                          
055900                                                                          
056000 IMS-RESTART SECTION.                                                     
056100     MOVE 'IMS-RESTART     ' TO CURRENT-IMS-SECTION                       
056200                                                                          
056300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
056400     MOVE '  ' TO GODK-STATUSKODER                                        
056500     CALL CBLTDLI USING XRST MSG-PCB                                      
056600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
056700                        CHKP-AREA-LENGTH CHKP-AREA                        
056800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056900     PERFORM IMS-STATUSKONTROLL                                           
057000     .                                                                    
057100                                                                          
057200                                                                          
057300 IMS-CHECKPOINT SECTION.                                                  
057400     MOVE 'IMS-CHECKPOINT  ' TO CURRENT-IMS-SECTION                       
057500                                                                          
057600     MOVE SPACE  TO CHKP-MSG-IO-AREA                                      
057700     MOVE '  XD' TO GODK-STATUSKODER                                      
057800     CALL CBLTDLI USING CHKP MSG-PCB                                      
057900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
058000                        CHKP-AREA-LENGTH CHKP-AREA                        
058100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058200     PERFORM IMS-STATUSKONTROLL                                           
058300                                                                          
058400     IF IMS-EJ-OK                                                         
058500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
058600       DISPLAY FELTEXT                                                    
058700       CALL FELLOG                                                        
058800     END-IF                                                               
058900     .                                                                    
059000                                                                          
059100                                                                          
059200 IMS-GU-WDK701 SECTION.                                                   
059300     MOVE 'IMS-GU-WDK701   ' TO CURRENT-IMS-SECTION                       
059400                                                                          
059500     MOVE SPACE               TO ALL-SSA                                  
059600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
059700          DELIMITED BY SIZE INTO SSA1                                     
059800     MOVE '  GE'              TO GODK-STATUSKODER                         
059900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
060000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
060100     PERFORM IMS-STATUSKONTROLL                                           
060200     .                                                                    
060300                                                                          
060400                                                                          
060500 IMS-GHNP-WDK711 SECTION.                                                 
060600     MOVE 'IMS-GHNP-WDK711 ' TO CURRENT-IMS-SECTION                       
060700                                                                          
060800     MOVE SPACE               TO ALL-SSA                                  
060900     STRING 'WDK711  (IDDC    =>' W-IDDC-MIN                              
061000                    '&IDDC    =<' W-IDDC-MAX ')'                          
061100          DELIMITED BY SIZE INTO SSA1                                     
061200     MOVE '  GE'              TO GODK-STATUSKODER                         
061300     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
061400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
061500     PERFORM IMS-STATUSKONTROLL                                           
061600     .                                                                    
061700                                                                          
061800                                                                          
061900 IMS-REPL-WDK711 SECTION.                                                 
062000     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
062100                                                                          
062200     MOVE SPACE               TO ALL-SSA                                  
062300     MOVE '  '             TO GODK-STATUSKODER                            
062400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
062500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
062600     PERFORM IMS-STATUSKONTROLL                                           
062700     ADD +2 TO CHKP-ANT                                                   
062800     .                                                                    
062900                                                                          
063000                                                                          
063100 IMS-GNP-WDK722 SECTION.                                                  
063200     MOVE 'IMS-GNP-WDK722  ' TO CURRENT-IMS-SECTION                       
063300                                                                          
063400     MOVE SPACE               TO ALL-SSA                                  
063500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
063600          DELIMITED BY SIZE INTO SSA1                                     
063700     MOVE 'WDK722 '           TO SSA2                                     
063800     MOVE '  GE'              TO GODK-STATUSKODER                         
063900     CALL CBLTDLI USING GNP  WDK7-PCB DLI-IO-WDK722 SSA1 SSA2             
064000     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
064100     PERFORM IMS-STATUSKONTROLL                                           
064200     .                                                                    
064300                                                                          
064400                                                                          
064500 IMS-GNP-WDK723 SECTION.                                                  
064600     MOVE 'IMS-GNP-WDK723  ' TO CURRENT-IMS-SECTION                       
064700                                                                          
064800     MOVE SPACE               TO ALL-SSA                                  
064900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
065000          DELIMITED BY SIZE INTO SSA1                                     
065100     MOVE   'WDK723 '         TO SSA2                                     
065200     MOVE '  GE' TO GODK-STATUSKODER                                      
065300     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK723 SSA1 SSA2             
065400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
065500     PERFORM IMS-STATUSKONTROLL                                           
065600     .                                                                    
065700                                                                          
065800                                                                          
065900 IMS-GHNP-WDK712 SECTION.                                                 
066000     MOVE 'IMS-GHNP-WDK712 ' TO CURRENT-IMS-SECTION                       
066100                                                                          
066200     MOVE SPACE               TO ALL-SSA                                  
066300     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
066400          DELIMITED BY SIZE INTO SSA1                                     
066500     MOVE '  GE'              TO GODK-STATUSKODER                         
066600     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK712 SSA1                  
066700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
066800     PERFORM IMS-STATUSKONTROLL                                           
066900     .                                                                    
067000                                                                          
067100                                                                          
067200 IMS-REPL-WDK712 SECTION.                                                 
067300     MOVE 'IMS-REPL-WDK712 ' TO CURRENT-IMS-SECTION                       
067400                                                                          
067500     MOVE SPACE               TO ALL-SSA                                  
067600     MOVE '  '             TO GODK-STATUSKODER                            
067700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
067800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
067900     PERFORM IMS-STATUSKONTROLL                                           
068000     ADD +1 TO CHKP-ANT                                                   
068100     .                                                                    
068200                                                                          
068300                                                                          
068400 IMS-GHU-WDC901 SECTION.                                                  
068500     MOVE 'IMS-GHU-WDC901  ' TO CURRENT-IMS-SECTION                       
068600                                                                          
068700     MOVE SPACE               TO ALL-SSA                                  
068800     STRING 'WDC901  (WDC901KY =' W-WDC901KY-X ')'                        
068900          DELIMITED BY SIZE INTO SSA1                                     
069000     MOVE '  GE'              TO GODK-STATUSKODER                         
069100     CALL CBLTDLI USING GHU WDC9-PCB DLI-IO-WDC901 SSA1                   
069200     MOVE WDC9-STATUS-CODE    TO STATUS-WS                                
069300     PERFORM IMS-STATUSKONTROLL                                           
069400     .                                                                    
069500                                                                          
069600                                                                          
069700 IMS-REPL-WDC901 SECTION.                                                 
069800     MOVE 'IMS-REPL-WDC901 ' TO CURRENT-IMS-SECTION                       
069900                                                                          
070000     MOVE SPACE               TO ALL-SSA                                  
070100     MOVE '  '             TO GODK-STATUSKODER                            
070200     CALL CBLTDLI USING REPL WDC9-PCB DLI-IO-WDC901                       
070300     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
070400     PERFORM IMS-STATUSKONTROLL                                           
070500     ADD +2 TO CHKP-ANT                                                   
070600     .                                                                    
070700                                                                          
070800                                                                          
070900 IMS-DLET-WDC901 SECTION.                                                 
071000     MOVE 'IMS-DLET-WDC901 ' TO CURRENT-IMS-SECTION                       
071100                                                                          
071200     MOVE SPACE               TO ALL-SSA                                  
071300     MOVE '  '             TO GODK-STATUSKODER                            
071400     CALL CBLTDLI USING DLET WDC9-PCB DLI-IO-WDC901                       
071500     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
071600     PERFORM IMS-STATUSKONTROLL                                           
071700     ADD +2 TO CHKP-ANT                                                   
071800     .                                                                    
071900                                                                          
072000                                                                          
072100 IMS-ISRT-WDG302 SECTION.                                                 
072200     MOVE 'IMS-ISRT-WDG302 ' TO CURRENT-IMS-SECTION                       
072300                                                                          
072400     MOVE SPACE               TO ALL-SSA                                  
072500     STRING 'WDG301  (WDG3KEY  =' W-WDG301-2203-X ')'                     
072600          DELIMITED BY SIZE INTO SSA1                                     
072700     MOVE 'WDG302 '           TO SSA2                                     
072800     MOVE '  II'              TO GODK-STATUSKODER                         
072900     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2204 SSA1 SSA2           
073000     MOVE WDG3-STATUS-CODE    TO STATUS-WS                                
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     ADD +1 TO CHKP-ANT                                                   
073300     .                                                                    
073400                                                                          
073500                                                                          
073600 IMS-ISRT-WDR301   SECTION.                                               
073700     MOVE 'IMS-ISRT-WDR301 '  TO CURRENT-IMS-SECTION                      
073800                                                                          
073900     MOVE SPACE               TO ALL-SSA                                  
074000     MOVE 'WDR301 '           TO SSA1                                     
074100     MOVE '  II'              TO GODK-STATUSKODER                         
074200     CALL CBLTDLI USING ISRT WDR3-PCB DLI-IO-WDR301 SSA1                  
074300     MOVE WDR3-STATUS-CODE    TO STATUS-WS                                
074400     PERFORM IMS-STATUSKONTROLL                                           
074500     ADD +1 TO CHKP-ANT                                                   
074600     .                                                                    
074700                                                                          
074800                                                                          
074900 IMS-GHU-RESTART  SECTION.                                                
075000     MOVE 'IMS-GHU-RESTART '  TO CURRENT-IMS-SECTION                      
075100                                                                          
075200     MOVE SPACE          TO ALL-SSA                                       
075300     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
075400          DELIMITED BY SIZE INTO SSA1                                     
075500     MOVE 'WDR470   '    TO SSA2                                          
075600     MOVE '    '         TO GODK-STATUSKODER                              
075700     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
075800     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
075900     PERFORM IMS-STATUSKONTROLL                                           
076000     .                                                                    
076100                                                                          
076200                                                                          
076300 IMS-REPL-RESTART SECTION.                                                
076400     MOVE 'IMS-REPL-RESTART'  TO CURRENT-IMS-SECTION                      
076500                                                                          
076600     MOVE '  '             TO GODK-STATUSKODER                            
076700     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
076800     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
076900     PERFORM IMS-STATUSKONTROLL                                           
077000     .                                                                    
077100                                                                          
077200                                                                          
077300 IMS-STATUSKONTROLL SECTION.                                              
077400                                                                          
077500     SET STATUS-IX TO 1                                                   
077600     SEARCH GODK-STATUS                                                   
077700       AT END                                                             
077800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
077900           DELIMITED BY SIZE INTO FELTEXT                                 
078000         DISPLAY FELTEXT                                                  
078100         CALL FELLOG                                                      
078200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
078300         CONTINUE                                                         
078400     END-SEARCH                                                           
079000     .                                                                    
