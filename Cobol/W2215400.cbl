000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2215400.                                                
000400 AUTHOR.         P-A HELGEGREN (KOPIA BODIL)                              
000500 DATE-WRITTEN.   94/08/24.                                                
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        LÄSER FIL FÖR GODKÄNNANDE AV LEVPLANFÖRSLAG                      
001000*        - SKAPAR TRANS W2T163X TILL DISPATCHEN                           
001100*                                                                         
001200*    INDATA .                                                             
001300*                                                                         
001400*        FIL FRÅN PGM W22150 (W221P023)                                   
001500*                                                                         
001600*    UTDATA .                                                             
001700*                                                                         
001800*        W2I16301 + WMSGKOM                                               
001900                                                                          
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*                         ARTNR SOM HAR LEVERANSPLANFÖRSLAG               
002800*                               SOM SKALL GODKÄNNAS AUTOMATISKT           
002900     SELECT W22154                     ASSIGN TO W22154D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP2                                                                
003500 FD  W22154                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800     SKIP2                                                                
003900*01  -COPY W22154        -L.                                              
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W2215400'.            
004600 77  CURRENT-SECTION             PIC X(32) VALUE SPACE.                   
004610 77  DBS-SECTION                 PIC X(32) VALUE SPACE.                   
004620                                                                          
004700 77  W-CHKP-MAX                  PIC S9(3)   VALUE +100  COMP-3.          
004800 77  W-CHKP-RAEKNARE             PIC S9(3)   VALUE +0    COMP-3.          
004900 77  W-MSG-IO-AREA-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
005000 77  W-MSG-IO-AREA               PIC X(32)   VALUE SPACE.                 
005100 77  W-CHKP-AREA-1-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
005200 77  W-CHKP-AREA-1               PIC X(32)   VALUE SPACE.                 
005300 77  JA                          PIC X(1)    VALUE 'J'.                   
005400 77  NEJ                         PIC X(1)    VALUE 'N'.                   
005500                                                                          
005600*01  -COPY WWDCKONS                                                       
005700                                                                          
005800 77  W22154-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W22154                       VALUE 'J'.                   
006000                                                                          
006100 77  SW-FORSLAG                  PIC X       VALUE 'N'.                   
006200     88  FORSLAG-FINNS                       VALUE 'J'.                   
006300                                                                          
006400 77  WS-IDLEVNR                  PIC X(5)  VALUE SPACE.                   
006500 77  WS-IDARTNR                  PIC 9(9)  VALUE ZERO.                    
006600 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
006700 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
006710 77  SW-BLOC-FLORS-06            PIC X       VALUE 'N'.                   
006720 77  WS-DAGENS-DATUM             PIC 9(8)  VALUE ZERO.                    
006721 77  WS-YESTERDAY                PIC 9(8)  VALUE ZERO.                    
006900                                                                          
006910 01  WS-TIREGDAT-AAAAMMDD        PIC 9(8)    VALUE ZERO.                  
006920 01  FILLER REDEFINES WS-TIREGDAT-AAAAMMDD.                               
006930     03  WS-TIREGDAT-SEKEL       PIC 9(2).                                
006940     03  WS-TIREGDAT-AAMMDD      PIC 9(6).                                
007000     EJECT                                                                
007010                                                                          
007100 01  FELTEXT.                                                             
007200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007400     EJECT                                                                
007500 01  DYNAMISKA-SUBPROGRAM.                                                
007600*                                                                         
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008000     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
008010     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005 -PRE  POSTSUM-                                           
008500     EJECT                                                                
008501*                                                                         
008502*    -- SUBPROGRAM WZ20DAYS                                               
008503 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
008504*01 -COPY WZ20DAYS                                                        
008510     EJECT                                                                
008600 01  IN-AREA-START               PIC X(24)   VALUE                        
008700                                             'IN-AREA-START'.             
008800     SKIP2                                                                
008900*01  AREA -COPY W22154       -PRE IN-.                                    
009000*                                                                         
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  NYCKLAR-TILL-DLI.                                                    
009500     03  W-WDD901KY-X.                                                    
009600         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
009700         05  W-IDDC              PIC X(2)   VALUE SPACE.                  
009800     03  W-IDLEVNR-X.                                                     
009900         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
009910                                                                          
009920 01  W-WDGX2231-X.                                                        
009930     03  W-IDHTYP-2231           PIC X(4)   VALUE '2231'.                 
009931     03  FILLER                  PIC X(26)  VALUE LOW-VALUE.              
009932                                                                          
009933 01  W-WDGX2232-X.                                                        
009934     03  W-IDANSK-2232           PIC S9(3)  VALUE ZERO COMP-3.            
009935     03  FILLER                  PIC X(3)   VALUE LOW-VALUE.              
009940                                                                          
009950 01  W-WDGXKEY-2223-X.                                                    
009951     03  W-IDHTYP-2223           PIC X(4)   VALUE '2223'.                 
009952     03  W-IDANSK-2223           PIC S9(3)  COMP-3 VALUE ZERO.            
009953     03  FILLER                  PIC X(24)  VALUE LOW-VALUE.              
009954                                                                          
009955 01  W-IDARTNR-2224-X.                                                    
009956     03 W-IDARTNR-2224           PIC S9(9)  VALUE ZERO  COMP-3.           
009958                                                                          
009959 01  W-IDDC-2224-X.                                                       
009960     03  W-IDDC-2224             PIC X(2)   VALUE SPACE.                  
009961                                                                          
009962 01  W-KDLARM-X.                                                          
009970     03  W-KDLARM                PIC S9(3)  COMP-3 VALUE ZERO.            
009980                                                                          
010000     SKIP3                                                                
010100*    --- STATUS-KOD FRÅN IMS                                              
010200 01  STATUS-WS                   PIC XX.                                  
010300     88  SEGMENT-FINNS                       VALUE '  '.                  
010400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010600     88  IMS-EJ-OK                           VALUE 'XD'.                  
010700     SKIP2                                                                
010800 01  GODK-STATUSKODER.                                                    
010900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01  SSA1                        PIC X(64).                               
011200 01  SSA2                        PIC X(64).                               
011300 01  SSA3                        PIC X(64).                               
011400     EJECT                                                                
011500*    --- IMS FUNKTIONSKODER                                               
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011710*    ---  DLI INPUT-OUTPUT AREA                                           
011740                                                                          
012902 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDD901'.              
012903 01  DLI-IO-WDD901.                                                       
012904*    03 WLINLB01 -COPY WDD901    -PRE INLB01-                             
012905     EJECT                                                                
012907 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDD902'.              
012908 01  DLI-IO-WDD902.                                                       
012909*    03 WLINLB11 -COPY WDD902    -PRE INLB11-                             
012910     EJECT                                                                
012912 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDD905'.              
012913 01  DLI-IO-WDD905.                                                       
012914*    03 WLINLB23 -COPY WDD905    -PRE INLB23-                             
012915     EJECT                                                                
012916                                                                          
012919 01  FILLER               PIC X(16) VALUE 'DLI-IO-WDGX2232'.              
012920 01  DLI-IO-WDGX2232.                                                     
012921*    03  -COPY WDGX2232                                                   
012922     EJECT                                                                
012923                                                                          
012924 01  FILLER               PIC X(16) VALUE 'DLI-IO-WDGX2223'.              
012925 01  DLI-IO-WDGX2223.                                                     
012926*    03  -COPY WDGX2223                                                   
012927     EJECT                                                                
012928                                                                          
012929 01  FILLER               PIC X(16) VALUE 'DLI-IO-WDGX2224'.              
012930 01  DLI-IO-WDGX2224.                                                     
012931*    03  -COPY WDGX2224                                                   
012932                                                                          
012940     EJECT                                                                
013000*    ---  MSG INPUT-OUTPUT AREA                                           
013100 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
013200     SKIP3                                                                
013300*01  -COPY WMSGAREA                                                       
013400     EJECT                                                                
013500     05  FILLER REDEFINES MSG-MID-OUT.                                    
013600        07  -COPY W2I10301  -PRE MID-.                                    
013700     EJECT                                                                
013800*    ---  AREA FÖR W006KOM SUBMODUL                                       
013900 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
014000     SKIP3                                                                
014100 01  KOM-IO-AREA.                                                         
014200*    03  -COPY WMSGKOM                                                    
014300     EJECT                                                                
014400 LINKAGE SECTION.                                                         
014500                                                                          
014600*01  -COPY W0009 -PRE MSG-                                                
014700     EJECT                                                                
014800*01  -COPY W0009 -PRE ALT-                                                
014900     EJECT                                                                
015000*01  -COPY W0009 -PRE KOMA-                                               
015100     EJECT                                                                
015200*01  -COPY W0008 -PRE WDD9-                                               
015300     05  FILLER      PIC X.                                               
015400     EJECT                                                                
015403*01  -COPY W0008 -PRE WDR2-                                               
015404     05  FILLER      PIC X.                                               
015405     EJECT                                                                
015406*01  -COPY W0008 -PRE WDR5-                                               
015407     05  FILLER      PIC X.                                               
015410     EJECT                                                                
015500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB WDD9-PCB              
015510                           WDR2-PCB WDR5-PCB.                             
015600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB WDD9-PCB              
015610                           WDR2-PCB WDR5-PCB.                             
015700                                                                          
015800     PERFORM A-INIT                                                       
015900     PERFORM S01-LAES-W22154                                              
016000                                                                          
016100     PERFORM UNTIL END-OF-W22154                                          
016200        MOVE IN-IDARTNR TO W-IDARTNR                                      
016210                           W-IDARTNR-2224                                 
016300        MOVE IN-IDLEVNR TO W-IDLEVNR                                      
016400        PERFORM IMS-GU-WDD901                                             
016500        IF SEGMENT-FINNS                                                  
016501                                                                          
016502          PERFORM B-KOLLA-LARM-BLOCKAD-VECKA                              
016503                                                                          
016510          IF SW-BLOC-FLORS-06 = JA                                        
016512            DISPLAY '* ARTNR BLOCKAD-VECKA: ' IN-IDARTNR                  
016520          ELSE                                                            
016600            MOVE NEJ TO SW-FORSLAG                                        
016700            PERFORM IMS-GNP-WDD905                                        
016800            PERFORM UNTIL SEGMENT-SAKNAS OR FORSLAG-FINNS                 
016900               IF INLB23-KDAVROP = 1                                      
017000                  MOVE JA TO SW-FORSLAG                                   
017100               ELSE                                                       
017200                  PERFORM IMS-GNP-WDD905                                  
017300               END-IF                                                     
017400            END-PERFORM                                                   
017500            IF SW-FORSLAG = NEJ                                           
017600***           NOV 2005: VI GODKÄNNER, ÄVEN OM FÖRSLAG EJ FINNS            
017700               MOVE JA TO SW-FORSLAG                                      
017800               DISPLAY '* FÖRSLAG SAKNAS : ' IN-IDARTNR                   
017900                       ' * '                 IN-IDLEVNR                   
018000            END-IF                                                        
018100            IF FORSLAG-FINNS                                              
018200               PERFORM C-SKAPA-TRANS                                      
018300               PERFORM D-SKICKA-TRANS                                     
018400            ELSE                                                          
018500               DISPLAY '* FÖRSLAG SAKNAS : ' IN-IDARTNR                   
018600                       ' * '                 IN-IDLEVNR                   
018700            END-IF                                                        
018710          END-IF                                                          
018800        ELSE                                                              
018900           DISPLAY '* ARTNR SAKNAS : ' IN-IDARTNR                         
019000        END-IF                                                            
019100        PERFORM S01-LAES-W22154                                           
019200     END-PERFORM                                                          
019300                                                                          
019400     PERFORM Z-FINIT                                                      
019500                                                                          
019600     MOVE ZERO TO RETURN-CODE                                             
019700     GOBACK                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 A-INIT SECTION.                                                          
020010     MOVE 'A-INIT  '  TO CURRENT-SECTION                                  
020100                                                                          
020200     OPEN INPUT W22154                                                    
020300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020400     MOVE ZERO TO W-CHKP-RAEKNARE                                         
020500     PERFORM IMS-RESTART                                                  
020600     PERFORM AA-SKAPA-HEADER                                              
020700                                                                          
020710     MOVE WC-CDC-SE TO W-IDDC                                             
020711                       W-IDDC-2224                                        
020720                                                                          
020731                                                                          
020732     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-DAGENS-DATUM                  
020733     MOVE WS-DAGENS-DATUM  TO DAYS-TIDATE1                                
020734     MOVE 'YYYYMMDD'       TO DAYS-KDDATFMT1                              
020735     MOVE 'YYYYMMDD'       TO DAYS-KDDATFMT2                              
020736     MOVE SPACE            TO DAYS-TIDATE2                                
020737                              DAYS-IDCALEND                               
020738     MOVE -1               TO DAYS-KVDAYS                                 
020739                                                                          
020740     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
020741                                                                          
020742     MOVE DAYS-TIDATE2(1:8) TO WS-YESTERDAY                               
020743                                                                          
020800     .                                                                    
020900     EJECT                                                                
021000 AA-SKAPA-HEADER SECTION.                                                 
021010     MOVE 'AA-SKAPA-HEADER ' TO CURRENT-SECTION                           
021100                                                                          
021200     MOVE +54                   TO MSG-KOM-KVLL                           
021300     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
021400                                   MSG-KOM-KDZ2                           
021500     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
021600     MOVE 'W2I16301'            TO MSG-KOM-IDCPYTXT                       
021700     MOVE 'LEVPLAN'             TO MSG-KOM-IDSNDNOD                       
021800     MOVE IDPGM                 TO MSG-KOM-IDSNDJOB                       
021900                                                                          
022000     ACCEPT WS-TIUPPDAT FROM DATE                                         
022100     ACCEPT WS-TIUPPTID FROM TIME                                         
022200     MOVE WS-TIUPPDAT           TO MSG-KOM-TIREGDAT                       
022300     MOVE WS-TIUPPTID           TO MSG-KOM-TIKLOCK                        
022400                                                                          
022500     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
022600                                   MSG-KOM-KDSVAR                         
022700     .                                                                    
022800     EJECT                                                                
022810 B-KOLLA-LARM-BLOCKAD-VECKA SECTION.                                      
022820     MOVE 'B-KOLLA-LARM-BLOCKAD-VECKA' TO CURRENT-SECTION                 
022821*--------------------------------------------------------                 
022830*--  BLOCKADE AVROP I VECKOR SOM EJ GÅR ATT FLYTTA                        
022831*--  FÅR EJ AUTOMATGODKÄNNAS.KDLPORS=06                                   
022832*--------------------------------------------------------                 
022833                                                                          
022834     MOVE NEJ  TO SW-BLOC-FLORS-06                                        
022835                                                                          
022836     MOVE IN-IDANSK                TO W-IDANSK-2232                       
022837     PERFORM IMS-GU-WDR220                                                
022838     IF SEGMENT-FINNS                                                     
022839        MOVE 2232-IDANSK-LARM      TO W-IDANSK-2223                       
022840     ELSE                                                                 
022841        MOVE ZERO                  TO W-IDANSK-2223                       
022842     END-IF                                                               
022843     PERFORM IMS-GU-WDGX2223                                              
022844     IF SEGMENT-SAKNAS                                                    
022846       CONTINUE                                                           
022847     ELSE                                                                 
022848       MOVE '999'                    TO W-KDLARM                          
022850       PERFORM IMS-GNP-WDGX2224                                           
022851       PERFORM UNTIL SEGMENT-SAKNAS OR (SW-BLOC-FLORS-06 = JA)            
022852         IF 2224-IDLEVNR = IN-IDLEVNR                                     
022853           MOVE 2224-TIREGDAT TO WS-TIREGDAT-AAMMDD                       
022854           MOVE 20            TO WS-TIREGDAT-SEKEL                        
022855           IF WS-TIREGDAT-AAAAMMDD >= WS-YESTERDAY                        
022856             MOVE JA TO SW-BLOC-FLORS-06                                  
022857           END-IF                                                         
022858         END-IF                                                           
022859         PERFORM IMS-GNP-WDGX2224                                         
022860       END-PERFORM                                                        
022861     END-IF                                                               
022862                                                                          
022863                                                                          
022864     .                                                                    
022870     EJECT                                                                
022900 C-SKAPA-TRANS SECTION.                                                   
022910     MOVE 'C-SKAPA-TRANS ' TO CURRENT-SECTION                             
023000                                                                          
023100                                                                          
023210     MOVE +215                  TO MSG-KVLL                               
023300     MOVE LOW-VALUE             TO MSG-KDZ1                               
023400                                   MSG-KDZ2                               
023500     MOVE 'W2T163X'             TO MSG-KDTRANS-1                          
023600     MOVE '2103'                TO MSG-IDTRANS-1                          
023700     MOVE '1'                   TO MSG-KDMFSFOR-1                         
023800                                                                          
023900     MOVE SPACE                 TO MID-W2I10301                           
024000     MOVE '+++++++++'           TO MID-IDARTNR-IN                         
024100     MOVE '+++++'               TO MID-IDLEVNR-IN                         
024200     MOVE '+'                   TO MID-KDBEHX-PLAN-IN                     
024300     MOVE IN-IDARTNR            TO MID-IDARTNR-UT                         
024400     MOVE IN-IDLEVNR            TO MID-IDLEVNR-UT                         
024412     MOVE ALL '+'               TO MID-TEARTNOT1                          
024420     MOVE ALL '+'               TO MID-TEARTNOT2                          
024500     MOVE 'F'                   TO MID-KDBEHX-PLAN-UT                     
024600     MOVE '2'                   TO MID-KOMKOD                             
024700***  DISPLAY '******* OK ********* '                                      
024800***           IN-IDARTNR                                                  
024900     .                                                                    
025000     EJECT                                                                
025100 D-SKICKA-TRANS SECTION.                                                  
025110     MOVE 'D-SKICKA-TRANS ' TO CURRENT-SECTION                            
025200                                                                          
025300     CALL W006KOM USING MSG-PCB                                           
025400                        ALT-PCB                                           
025500                        KOMA-PCB                                          
025600                        MSG-KOM-WMSGKOM                                   
025700                        MSG-IO-AREA                                       
025800                                                                          
025900     ADD +1 TO W-CHKP-RAEKNARE                                            
026000                                                                          
026100     MOVE 'W22154'       TO POSTSUM-FDNAMN                                
026200     MOVE 'DISPATCH'     TO POSTSUM-DDNAMN2                               
026300     MOVE 'ANT'          TO POSTSUM-TRANSTYP                              
026400     CALL POSTSUM USING POSTSUM-PARM                                      
026500                                                                          
026600     IF W-CHKP-RAEKNARE > W-CHKP-MAX                                      
026700        PERFORM IMS-CHECKPOINT                                            
026800        MOVE +0 TO W-CHKP-RAEKNARE                                        
026900        ADD +1 TO MSG-KOM-TIKLOCK                                         
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 Z-FINIT SECTION.                                                         
027400                                                                          
027500     CLOSE W22154                                                         
027600                                                                          
027700     MOVE 'S' TO POSTSUM-OPKOD                                            
027800     CALL POSTSUM USING POSTSUM-PARM                                      
027900     .                                                                    
028000     EJECT                                                                
028100 S01-LAES-W22154  SECTION.                                                
028200                                                                          
028300     READ W22154 INTO IN-AREA                                             
028400     AT END                                                               
028500        SET END-OF-W22154 TO TRUE                                         
028600                                                                          
028700     NOT AT END                                                           
028800        MOVE 'W22154'       TO POSTSUM-FDNAMN                             
028900        MOVE 'W22154D1'     TO POSTSUM-DDNAMN2                            
029000        MOVE 'IN'           TO POSTSUM-TRANSTYP                           
029100        CALL POSTSUM USING POSTSUM-PARM                                   
029200     END-READ                                                             
029300     .                                                                    
029400     EJECT                                                                
029500* IMS SECTIONER                                                           
029600                                                                          
029700 IMS-GU-WDD901 SECTION.                                                   
029710     MOVE 'IMS-GU-WDD901 '  TO DBS-SECTION                                
029720                                                                          
029800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
029900          DELIMITED BY SIZE INTO SSA1                                     
030000     MOVE '  GE' TO GODK-STATUSKODER                                      
030100     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901  SSA1                   
030200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
030300     PERFORM IMS-STATUSKONTROLL                                           
030400     .                                                                    
030500                                                                          
030600 IMS-GNP-WDD905 SECTION.                                                  
030610     MOVE 'IMS-GNP-WDD905 ' TO DBS-SECTION                                
030630                                                                          
030700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
030800          DELIMITED BY SIZE INTO SSA1                                     
030810     MOVE 'WDD905 ' TO SSA2                                               
031100     MOVE '  GE' TO GODK-STATUSKODER                                      
031200     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
031300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
031400     PERFORM IMS-STATUSKONTROLL                                           
031500     .                                                                    
031600     EJECT                                                                
031601 IMS-GU-WDR220 SECTION.                                                   
031602     MOVE 'IMS-GU-WDR220 '  TO DBS-SECTION                                
031603                                                                          
031604     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
031605          DELIMITED BY SIZE INTO SSA1                                     
031606     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
031607          DELIMITED BY SIZE INTO SSA2                                     
031608     MOVE '  GE' TO GODK-STATUSKODER                                      
031609     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2232 SSA1 SSA2             
031610     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
031611     PERFORM IMS-STATUSKONTROLL                                           
031612     .                                                                    
031619     EJECT                                                                
031620 IMS-GU-WDGX2223 SECTION.                                                 
031621     MOVE 'IMS-GU-WDGX2223   '  TO DBS-SECTION                            
031630                                                                          
031640     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
031650          DELIMITED BY SIZE INTO SSA1                                     
031660     MOVE '  GE'              TO GODK-STATUSKODER                         
031670     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX2223 SSA1                  
031680     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
031690     PERFORM IMS-STATUSKONTROLL                                           
031691     .                                                                    
031700     EJECT                                                                
031710 IMS-GNP-WDGX2224 SECTION.                                                
031720     MOVE 'IMS-GNP-WDGX2224  '  TO DBS-SECTION                            
031730                                                                          
031740     STRING 'WDR550  (IDARTNR  =' W-IDARTNR-2224-X                        
031741                    '&IDDC     =' W-IDDC-2224-X                           
031742                    '&KDLARM   =' W-KDLARM-X ')'                          
031743          DELIMITED BY SIZE INTO SSA1                                     
031760     MOVE '  GE'              TO GODK-STATUSKODER                         
031770     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX2224 SSA1                 
031780     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
031790     PERFORM IMS-STATUSKONTROLL                                           
031791     .                                                                    
031792     EJECT                                                                
031800 IMS-RESTART SECTION.                                                     
031830                                                                          
031900     MOVE SPACE TO W-MSG-IO-AREA                                          
032000     MOVE '  ' TO GODK-STATUSKODER                                        
032100     CALL CBLTDLI USING XRST MSG-PCB                                      
032200                             W-MSG-IO-AREA-LENGTH                         
032300                             W-MSG-IO-AREA                                
032400                             W-CHKP-AREA-1-LENGTH                         
032500                             W-CHKP-AREA-1                                
032600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032700     PERFORM IMS-STATUSKONTROLL                                           
032800     .                                                                    
032900     SKIP2                                                                
033000 IMS-CHECKPOINT SECTION.                                                  
033100     SKIP2                                                                
033200     MOVE IDPGM TO W-MSG-IO-AREA                                          
033300     MOVE '  XD' TO GODK-STATUSKODER                                      
033400     CALL CBLTDLI USING CHKP MSG-PCB                                      
033500                             W-MSG-IO-AREA-LENGTH                         
033600                             W-MSG-IO-AREA                                
033700                             W-CHKP-AREA-1-LENGTH                         
033800                             W-CHKP-AREA-1                                
033900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034000     PERFORM IMS-STATUSKONTROLL                                           
034100     IF IMS-EJ-OK                                                         
034200       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
034300       CALL FELLOG                                                        
034400     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 IMS-STATUSKONTROLL SECTION.                                              
034800                                                                          
034900     SET STATUS-IX TO 1                                                   
035000     SEARCH GODK-STATUS                                                   
035100       AT END                                                             
035200         MOVE 'IMS RETURKOD : ' TO FELTEXT-STR                            
035300         DISPLAY FELTEXT STATUS-WS                                        
035400         CALL FELLOG                                                      
035500       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
035600         CONTINUE                                                         
035700     END-SEARCH                                                           
035800     .                                                                    
