000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W5532200.                                                
000400 AUTHOR.         GUN LÖFGREN.                                             
000500 DATE-WRITTEN.   96/10/01.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        LÄSER FIL INNEHÅLLANDE PRISUPPDATERINGAR.                        
001200*                                                                         
001300*        FILEN KOMMER FRÅN PROGRAM W55320.                                
001400*                                                                         
001500*    INDATA :                                                             
001600*                                                                         
001700*        FIL W55320                                                       
001800*     PROGRAMMET UPPDATERAR WDR9/SAPA BASEN                               
001900*                                                                         
002000*                                                                         
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700                                                                          
002800*          --- NYA PRISER ELLER PÅLÄGG FRÅN BILD 5111                     
002900*          --- ELLER BILD 5118.                                           
003000                                                                          
003100     SELECT W55320                     ASSIGN TO W55322D1.                
003200     SKIP3                                                                
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600     SKIP2                                                                
003700 FD  W55320                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100 01  IN-POST.                                                             
004200*    03  -COPY WDH801        -L.                                          
004300     SKIP3                                                                
004400     EJECT                                                                
004500                                                                          
004600 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W5532200'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005110 01  DAGENS-AAAAMMDD     PIC 9(8)    VALUE ZERO.                          
005120 01  WS-PRARTBES-PR      PIC S9(7)V9(2)  COMP-3.                          
005130 01  FL-PRARTBES         PIC X       VALUE 'N'.                           
005200                                                                          
005300 01  CHKP-VAR.                                                            
005400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
005500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
005600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
005700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
005800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005900     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
006000     SKIP2                                                                
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400 01  W-ARBETS-AREOR.                                                      
006500     03  W-NYTT-PRARTSTD     PIC S9(7)V99   VALUE ZERO COMP-3.            
006600     03  W-DIFF-PRARTSTD     PIC S9(7)V99   VALUE ZERO COMP-3.            
006700     03  W-GAM-PRARTSTD      PIC S9(7)V99   VALUE ZERO COMP-3.            
006800                                                                          
006900*                FÖLJANDE FÄLT ANVÄNDS VID BERÄKNING                      
007000*                AV DIFFERENS MELLAN NYTT OCH GAMMALT STANDARDPRIS        
007100                                                                          
007200     03  W-DIFF-LAGERVARDE   PIC S9(7)V99   VALUE ZERO COMP-3.            
007300                                                                          
007400 01  WS-ARBAREA.                                                          
007500     03  WS-HHMMSSTH             PIC 9(8)    VALUE ZERO.                  
007600 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
007700                                                                          
007800 01  SPAR-IDDC                   PIC X(2).                                
007900 01  W-IDSEKVNR                  PIC S9(3)   VALUE ZERO COMP-3.           
008000 01  W-KDPRODSL                  PIC S9(3)   VALUE ZERO COMP-3.           
008100 01  W-EKH-IDARTNR               PIC X(9)    VALUE ' '.                   
008200 01  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
008300     EJECT                                                                
008400                                                                          
008500 77  W55320-EOF-SW               PIC X       VALUE 'N'.                   
008600     88  END-OF-W55320                       VALUE 'J'.                   
008700                                                                          
008800*    --- VALID IDDC CODES                                                 
008900*                                                                         
009000*01  -COPY WWDC99                                                         
009100*01  -COPY WWDCKONS                                                       
009200                                                                          
009300 01  DYNAMISKA-SUBPROGRAM.                                                
009400*                                                                         
009500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009800     03  DATKONV                 PIC X(8)    VALUE 'WDATKONV'.            
009900     03  W553KOST                PIC X(8)    VALUE 'W553KOST'.            
010000     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL POSTSUM                                          
010300*                                                                         
010400*01  -COPY W0005 -PRE  POSTSUM-                                           
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
010700*01 -COPY W009CIA                                                         
010800                                                                          
010900     EJECT                                                                
011000 01  IN-AREA-START            PIC X(24)   VALUE 'IN-AREA-START'.          
011100     SKIP2                                                                
011200 01  IN-AREA.                                                             
011300*    03      -COPY WDH801 -PRE IN-                                        
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700                                                                          
011800 01  NYCKLAR-TILL-DLI.                                                    
011900     03  W-IDARTNR-X.                                                     
012000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012100     03  W-KDSEGKEY-X.                                                    
012200         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
012220     03  W-DAPRLIST-X.                                                    
012230         05  W-DAPRLIST          PIC   9(8)  VALUE ZERO.                  
012240                                                                          
012300     03  W-WDH801-X.                                                      
012400         05  W-IDARTNR-WDH       PIC S9(9)   VALUE ZERO COMP-3.           
012500         05  W-DAREGDAT          PIC 9(8)    VALUE ZERO.                  
012600         05  W-TIREGTID          PIC S9(7)   VALUE ZERO COMP-3.           
012700                                                                          
012800     03  W-IDDC-MIN-X.                                                    
012900         05  W-IDDC-MIN          PIC X(2)    VALUE LOW-VALUE.             
013000     03  W-IDDC-MAX-X.                                                    
013100         05  W-IDDC-MAX          PIC X(2)    VALUE HIGH-VALUE.            
013200     SKIP2                                                                
013300*    --- STATUS-KOD FRÅN IMS                                              
013400 01  STATUS-WS                   PIC XX.                                  
013500     88  SEGMENT-FINNS                       VALUE '  '.                  
013600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013800     88  IMS-EJ-OK                           VALUE 'XD'.                  
013900     SKIP2                                                                
014000 01  GODK-STATUSKODER.                                                    
014100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200     SKIP3                                                                
014300 01  SSA1                        PIC X(64).                               
014400*    --- IMS FUNKTIONSKODER                                               
014500*01  -COPY W0003                                                          
014600     EJECT                                                                
014700*    ---  DLI INPUT-OUTPUT AREA                                           
014800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014900     SKIP3                                                                
015000 01  DLI-IO-AREA.                                                         
015100     03  WLARTC01.                                                        
015200*        05  -COPY WDK601  -PRE ARTC-                                     
015300     EJECT                                                                
015400 01  DLI-IO-AREA1.                                                        
015500     03  WLARTC11.                                                        
015600*        05  -COPY WDK611  -PRE ARTC-                                     
015700     EJECT                                                                
015701 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K621'.         
015710 01  DLI-IO-K621.                                                         
015730*    03  -COPY WDK621                                                     
015740     EJECT                                                                
015800 01  DLI-IO-AREA2.                                                        
015900     03  WLPRIG01.                                                        
016000*        05  -COPY WDH801                                                 
016100     EJECT                                                                
016200 01  DLI-IO-AREA4.                                                        
016300     03  IO-AREA4                PIC X(500)  VALUE SPACE.                 
016400     SKIP3                                                                
016500     03  WLARTS01 REDEFINES IO-AREA4.                                     
016600*        05  -COPY WDK701  -PRE ARTS-                                     
016700     SKIP3                                                                
016800     03  WLARTS11 REDEFINES IO-AREA4.                                     
016900*        05  -COPY WDK711  -PRE ARTS-                                     
017000 01  FILLER                      PIC X(16)  VALUE 'WLSAPA01'.             
017100*01  WLSAPA01 -COPY WDR901                                                
017200*    05 -COPY W510EKHA -RED FIL-WDR901-DATA                               
017300     EJECT                                                                
017400                                                                          
017500     EJECT                                                                
017600 LINKAGE SECTION.                                                         
017700                                                                          
017800*01  -COPY W0009 -PRE MSG-                                                
017900     EJECT                                                                
018000*01  -COPY W0008 -PRE PRIG-                                               
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300*01  -COPY W0008 -PRE ARTC-                                               
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600*01  -COPY W0008 -PRE ARTS-                                               
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008 -PRE SAPA-                                               
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200 PROCEDURE DIVISION  USING MSG-PCB PRIG-PCB                               
019300                           ARTC-PCB ARTS-PCB SAPA-PCB.                    
019400 MAIN SECTION.                                                            
019500     ENTRY 'DLITCBL' USING MSG-PCB PRIG-PCB                               
019600                           ARTC-PCB ARTS-PCB SAPA-PCB.                    
019700                                                                          
019800     PERFORM A-INIT                                                       
019900     PERFORM S01-LAES-W55320                                              
020000                                                                          
020100     PERFORM UNTIL END-OF-W55320                                          
020200       IF CHKP-ANT > CHKP-MAX                                             
020300         PERFORM X-TAG-CHECKPOINT                                         
020400       END-IF                                                             
020500                                                                          
020600       IF IN-PRI-KDPRIBEH = 'X' OR 'U'                                    
020700         PERFORM B-BERAKNA-STANDARDPRIS-XTRA                              
020800       ELSE                                                               
020900         PERFORM C-BERAKNA-STANDARDPRIS                                   
021000       END-IF                                                             
021100       PERFORM S01-LAES-W55320                                            
021200     END-PERFORM                                                          
021300                                                                          
021400     PERFORM Z-FINIT                                                      
021500                                                                          
021600     MOVE ZERO TO RETURN-CODE                                             
021700     GOBACK                                                               
021800     .                                                                    
021900     SKIP3                                                                
022000 A-INIT SECTION.                                                          
022100                                                                          
022200     PERFORM IMS-RESTART                                                  
022300                                                                          
022400     OPEN INPUT W55320                                                    
022500                                                                          
022600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022700     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
022800     .                                                                    
022900     EJECT                                                                
023000 B-BERAKNA-STANDARDPRIS-XTRA SECTION.                                     
023100                                                                          
023200*** DENNA SECTION SKALL I STÄLLET FÖR VIA R05,                            
023300*** UPPDATERA STANDARPRIS, PRINK (O PÅLÄGG='U') PÅ ARTREG.                
023400*** OBS, WDH8-UPPDATERAS EJ MEN TRANS FÖR SAP-BOKNING SKAPAS              
023500                                                                          
023600     MOVE IN-PRI-IDARTNR            TO W-IDARTNR                          
023700     MOVE ZERO                      TO W-GAM-PRARTSTD                     
023800                                       W-DIFF-PRARTSTD                    
023900                                       W-DIFF-LAGERVARDE                  
024000     PERFORM IMS-GET-ARTC-WLART01                                         
024100     IF SEGMENT-FINNS                                                     
024200       MOVE ARTC-ART-KDPRODSL       TO W-KDPRODSL                         
024300       MOVE ARTC-ART-KDSORT         TO WS-KDSORT                          
024400       PERFORM IMS-GET-ARTC-WLARTC11                                      
024500       IF SEGMENT-FINNS                                                   
024510         IF IN-PRI-KDPRIBEH = 'U'                                         
024520*** DETTA ÄR EN SPECIAL NÄR MAN VILL ÄNDRA KALKYLPÅLÄGG  *****            
024530           PERFORM BA-HAEMTA-PRARTBES-PR                                  
024540           PERFORM IMS-GET-ARTC-WLART01                                   
024550           PERFORM IMS-GET-ARTC-WLARTC11                                  
024560           MOVE IN-PRI-N-PRDIRLON   TO ARTC-CLAG-PRDIRLON                 
024570           MOVE IN-PRI-N-PRDMTRL    TO ARTC-CLAG-PRDMTRL                  
024580           MOVE IN-PRI-N-PROVRPAL   TO ARTC-CLAG-PROVRPAL                 
024590           COMPUTE ARTC-CLAG-PRARTSJK ROUNDED =                           
024591                                     + WS-PRARTBES-PR                     
024592                                     + ARTC-CLAG-PRDIRLON                 
024593                                     + ARTC-CLAG-PRDMTRL                  
024594                                     + ARTC-CLAG-PROVRPAL                 
024595         END-IF                                                           
024596                                                                          
024600         IF IN-PRI-N-PRINK NOT = 0                                        
024610           IF WS-KDSORT = 'SW'                                            
024620             MOVE ZERO          TO ARTC-CLAG-PRHEMTAG                     
024630           ELSE                                                           
024700             IF ARTC-CLAG-PRHEMTAG = 0                                    
024800** SKAPA FÖRST NYTT HEMTAGNINGSPÅLÄGG                                     
024900               IF IN-PRI-N-RETULF = 0                                     
025000                 MOVE 1.0812    TO IN-PRI-N-RETULF                        
025100               END-IF                                                     
025200               COMPUTE ARTC-CLAG-PRHEMTAG ROUNDED =                       
025300               IN-PRI-N-PRINK * (IN-PRI-N-RETULF - 1) /                   
025400               IN-PRI-N-RETULF                                            
025500             ELSE                                                         
025600              IF ARTC-CLAG-PRINK NOT = 0                                  
025700** ELLER RÄKNA FÖRST FRAM NYTT HEMTAGNINGSPÅLÄGG                          
025800               COMPUTE ARTC-CLAG-PRHEMTAG ROUNDED =                       
025900                 ARTC-CLAG-PRHEMTAG *                                     
025910                 IN-PRI-N-PRINK / ARTC-CLAG-PRINK                         
026000              END-IF                                                      
026100             END-IF                                                       
026110           END-IF                                                         
026200           IF IN-PRI-N-TEARTNOT = 'RETULF=0'                              
026300             MOVE 0                 TO ARTC-CLAG-RETULF                   
026400           END-IF                                                         
026500           MOVE IN-PRI-N-PRINK      TO ARTC-CLAG-PRINK                    
026600         END-IF                                                           
026700                                                                          
027900         MOVE ARTC-CLAG-PRARTSTD    TO W-GAM-PRARTSTD                     
028000         COMPUTE ARTC-CLAG-PRARTSTD ROUNDED =                             
028100                                       + ARTC-CLAG-PRINK                  
028200                                       + ARTC-CLAG-PRDIRLON               
028300                                       + ARTC-CLAG-PRDMTRL                
028400                                       + ARTC-CLAG-PROVRPAL               
028500         MOVE ARTC-CLAG-PRARTSTD    TO W-NYTT-PRARTSTD                    
028693                                                                          
028700         PERFORM IMS-REPL-ARTC                                            
028800         PERFORM CAA-SKAPA-BOKFORING-TRANS                                
028900         ADD +1 TO CHKP-ANT                                               
029000       END-IF                                                             
029100     END-IF                                                               
029200     .                                                                    
029300     EJECT                                                                
029301 BA-HAEMTA-PRARTBES-PR  SECTION.                                          
029302                                                                          
029303     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
029304     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
029305     PERFORM IMS-GNP-WDK621                                               
029306     IF SEGMENT-SAKNAS                                                    
029307       MOVE ARTC-CLAG-PRARTSTD  TO WS-PRARTBES-PR                         
029308     ELSE                                                                 
029309       MOVE NEJ                 TO FL-PRARTBES                            
029310       PERFORM UNTIL  SEGMENT-SAKNAS                                      
029311         IF PRL-SUINLEV-PR > ZERO                                         
029312           MOVE PRL-PRARTBES-PR  TO WS-PRARTBES-PR                        
029313           SET SEGMENT-SAKNAS TO TRUE                                     
029314         ELSE                                                             
029315           IF FL-PRARTBES = NEJ                                           
029316             MOVE PRL-PRARTBES-PR TO WS-PRARTBES-PR                       
029317             MOVE JA              TO FL-PRARTBES                          
029318           END-IF                                                         
029319           PERFORM IMS-GNP-WDK621                                         
029320         END-IF                                                           
029321       END-PERFORM                                                        
029322     END-IF                                                               
029323     .                                                                    
029330     EJECT                                                                
029400 C-BERAKNA-STANDARDPRIS SECTION.                                          
029500                                                                          
029600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
029700* BERÄKNAR STANDARDPRIS OCH FÖRSÄLJNINGSPRIS FÖR WDK6 OCH       *         
029800* BERÄKNAR PÅLÄGG FÖR WDK6                                      *         
029900* SAMT SÄTTER JA TILL FLAGGA-KLAR I WDH8                        *         
030000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
030100                                                                          
030200     MOVE IN-PRI-IDARTNR      TO W-IDARTNR-WDH                            
030300     MOVE IN-PRI-DAREGDAT     TO W-DAREGDAT                               
030400     MOVE IN-PRI-TIREGTID     TO W-TIREGTID                               
030500                                                                          
030600     PERFORM IMS-GET-WDH801                                               
030700     IF SEGMENT-FINNS                                                     
030800       IF PRI-FLKLAR = JA                                                 
030900                                                                          
031000*** VID OMSTART ÄR DENNA ARTIKELN REDAN KLAR     ****                     
031100          CONTINUE                                                        
031200       ELSE                                                               
031300         IF DAGENS-DATUM(5:4) = 1230 OR 1231 OR 0101                      
031400***   VID ÅRSSLUT FÅR EJ STANDARDPRIS OCH PRINK UPPDATERAS                
031500***   DOCK SKER LOGGNING PÅ LISTA W55327-001 OCH EV. W55328-001           
031600           MOVE ZERO                TO PRI-O-PRARTSTD                     
031700                                       PRI-N-PRARTSTD                     
031800                                       PRI-O-PRINK                        
031900                                       PRI-N-PRINK                        
032000         ELSE                                                             
032100           PERFORM CA-UPPDAT-PRIS-STD                                     
032200         END-IF                                                           
032300         MOVE JA               TO PRI-FLKLAR                              
032400         PERFORM IMS-REPL-WDH801                                          
032500         ADD +1 TO CHKP-ANT                                               
032600       END-IF                                                             
032700     END-IF                                                               
032800     .                                                                    
032900     EJECT                                                                
033000 CA-UPPDAT-PRIS-STD SECTION.                                              
033100                                                                          
033200     MOVE IN-PRI-IDARTNR            TO W-IDARTNR                          
033300     MOVE ZERO                      TO W-GAM-PRARTSTD                     
033400                                       W-DIFF-PRARTSTD                    
033500                                       W-DIFF-LAGERVARDE                  
033600     PERFORM IMS-GET-ARTC-WLART01                                         
033700     IF SEGMENT-FINNS                                                     
033800       MOVE ARTC-ART-KDPRODSL       TO W-KDPRODSL                         
033900       MOVE ARTC-ART-KDSORT         TO WS-KDSORT                          
034000       PERFORM IMS-GET-ARTC-WLARTC11                                      
034100       IF SEGMENT-FINNS                                                   
034110         IF WS-KDSORT = 'SW'                                              
034120           MOVE ZERO        TO ARTC-CLAG-PRHEMTAG                         
034130         ELSE                                                             
034200           IF ARTC-CLAG-PRHEMTAG = 0                                      
034300             IF IN-PRI-N-RETULF = 0                                       
034400               MOVE 1.0812    TO IN-PRI-N-RETULF                          
034500             END-IF                                                       
034600             COMPUTE ARTC-CLAG-PRHEMTAG ROUNDED =                         
034700               IN-PRI-N-PRINK *                                           
034710               (IN-PRI-N-RETULF - 1) / IN-PRI-N-RETULF                    
034800           ELSE                                                           
034900             IF ARTC-CLAG-PRINK NOT = ZERO                                
035000               COMPUTE ARTC-CLAG-PRHEMTAG ROUNDED =                       
035100                 ARTC-CLAG-PRHEMTAG *                                     
035110                 IN-PRI-N-PRINK / ARTC-CLAG-PRINK                         
035200             END-IF                                                       
035300           END-IF                                                         
035310         END-IF                                                           
035400         MOVE IN-PRI-N-PRINK        TO ARTC-CLAG-PRINK                    
035500         MOVE ARTC-CLAG-PRARTSTD    TO W-GAM-PRARTSTD                     
035600         COMPUTE ARTC-CLAG-PRARTSTD ROUNDED =                             
035700                                  + ARTC-CLAG-PRINK                       
035800                                  + ARTC-CLAG-PRDIRLON                    
035900                                  + ARTC-CLAG-PRDMTRL                     
036000                                  + ARTC-CLAG-PROVRPAL                    
036100         MOVE ARTC-CLAG-PRARTSTD    TO W-NYTT-PRARTSTD                    
036200         PERFORM IMS-REPL-ARTC                                            
036300         PERFORM CAA-SKAPA-BOKFORING-TRANS                                
036400       END-IF                                                             
036500     END-IF                                                               
036600     .                                                                    
036700     EJECT                                                                
036800 CAA-SKAPA-BOKFORING-TRANS SECTION.                                       
036900                                                                          
037000     COMPUTE W-DIFF-PRARTSTD =                                            
037100       W-NYTT-PRARTSTD - W-GAM-PRARTSTD                                   
037200                                                                          
037300***  LAGERVÄRDESFÖRÄNDRING CDC (IDDC = 11)                                
037400     MOVE WC-CDC-SE          TO SPAR-IDDC                                 
037500     COMPUTE W-DIFF-LAGERVARDE = W-DIFF-PRARTSTD *                        
037600       (ARTC-CLAG-KVLS + ARTC-CLAG-KVEFRS +                               
037700        ARTC-CLAG-KVAKS-CDC + ARTC-CLAG-KVAKS-PAV)                        
037800     COMPUTE EKH-KVANTAL = ARTC-CLAG-KVLS + ARTC-CLAG-KVEFRS +            
037900                       ARTC-CLAG-KVAKS-CDC + ARTC-CLAG-KVAKS-PAV          
038000     PERFORM D-UPPDATERA-WDR9                                             
038100                                                                          
038200***  LAGERVÄRDESFÖRÄNDRING TERMINAL (IDDC = 12)                           
038300     MOVE WC-CDC-TR          TO SPAR-IDDC                                 
038400     COMPUTE W-DIFF-LAGERVARDE = W-DIFF-PRARTSTD *                        
038500                            ARTC-CLAG-KVAKS-T                             
038600     COMPUTE EKH-KVANTAL = ARTC-CLAG-KVAKS-T                              
038700     PERFORM D-UPPDATERA-WDR9                                             
038800                                                                          
038900***  LAGERVÄRDESFÖRÄNDRING SDC M.FL (IDFTG=57)                            
039000     PERFORM IMS-GU-SLAGERROT                                             
039100     IF SEGMENT-FINNS                                                     
039200       PERFORM IMS-GNP-SLAGERINFO                                         
039300       PERFORM UNTIL SEGMENT-SAKNAS                                       
039400                                                                          
039500         IF SEGMENT-FINNS                                                 
039600*-----------INGA TRANSAR FÖR DC:N DÄR IDFTG INTE ÄR 57------*             
039700*-----------SKALL BEARBETAS---------------------------------*             
039800           MOVE ARTS-SLAG-IDDC       TO WS-IDDC                           
039900           IF NDC-NA OR LDC-CN OR XDC-NON-VCC-OWNED                       
039910              CONTINUE                                                    
039920           ELSE                                                           
040000             MOVE ARTS-SLAG-IDDC       TO SPAR-IDDC                       
040100             COMPUTE W-DIFF-LAGERVARDE   = W-DIFF-PRARTSTD *              
040200                    (ARTS-SLAG-KVLS      + ARTS-SLAG-KVEFRS +             
040300                     ARTS-SLAG-KVAKS-SDC + ARTS-SLAG-KVAKS-PAV)           
040400             COMPUTE EKH-KVANTAL = ARTS-SLAG-KVLS +                       
040500                                   ARTS-SLAG-KVEFRS +                     
040600                                   ARTS-SLAG-KVAKS-SDC +                  
040700                                   ARTS-SLAG-KVAKS-PAV                    
040800             PERFORM D-UPPDATERA-WDR9                                     
040900           END-IF                                                         
041000           PERFORM IMS-GNP-SLAGERINFO                                     
041100         END-IF                                                           
041200       END-PERFORM                                                        
041300     END-IF                                                               
041400     .                                                                    
041500     EJECT                                                                
041600 D-UPPDATERA-WDR9   SECTION.                                              
041700                                                                          
041800     IF W-DIFF-LAGERVARDE NOT = 0                                         
041900       MOVE 'W5532200'       TO FIL-IDPGM                                 
042000       MOVE DAGENS-DATUM     TO FIL-DAREGDAT                              
042100       ACCEPT    FIL-TIKLOCK    FROM TIME                                 
042200       MOVE 1                TO FIL-IDSEKVNR                              
042300       MOVE 'W510EKHA'       TO FIL-IDCPYTXT                              
042400       MOVE SPACE            TO FIL-IDUSER                                
042500       MOVE IN-PRI-IDARTNR   TO EKH-IDARTNR                               
042600       MOVE '401'            TO EKH-KDEKHHT                               
042700       MOVE '401'            TO EKH-KDEKSHT                               
042800       MOVE 'DET'            TO EKH-KDEKNIVA                              
042900       MOVE SPAR-IDDC        TO EKH-IDDC-SEND                             
043000       MOVE SPACE            TO EKH-IDDC-REC                              
043100       MOVE +0               TO EKH-IDDISTR                               
043200       MOVE +0               TO EKH-IDKUNDNR                              
043300                                                                          
043400       MOVE 'VO'             TO CIA-IDARTPRE-IN                           
043500       MOVE IN-PRI-IDARTNR   TO CIA-IDARTBET-IN                           
043600       CALL W009CIA USING       CIA-W009CIA                               
043700       MOVE CIA-IDARTBET-UT TO EKH-IDVERGL                                
043800                                                                          
043900       MOVE DAGENS-DATUM     TO EKH-DAVERDAT                              
044000       MOVE W-KDPRODSL       TO EKH-KDPRODSL                              
044100       MOVE ZERO             TO EKH-KDPSLLOC                              
044200       MOVE SPACE            TO EKH-FLLSBOK                               
044300       MOVE 'SEK'            TO EKH-KDVALISO                              
044400       MOVE 1.00             TO EKH-PRKURS                                
044500       MOVE ZERO             TO EKH-PRARTNTO                              
044600       MOVE ZERO             TO EKH-PRARTSJK                              
044700       MOVE ZERO             TO EKH-PRHEMTAG                              
044800       MOVE W-DIFF-PRARTSTD  TO EKH-PRARTSTD                              
044900       MOVE ZERO             TO EKH-PRLANDCO                              
045000       MOVE ZERO             TO EKH-PRINK                                 
045100       MOVE ZERO             TO EKH-PRDIRLON                              
045200       MOVE ZERO             TO EKH-PRDMTRL                               
045300       MOVE ZERO             TO EKH-PROVRPAL                              
045400       MOVE ZERO             TO EKH-SUBEL                                 
045500       MOVE SPACE            TO EKH-IDTRANS                               
045600                                EKH-IDLEVNR                               
046000                                EKH-IDKST                                 
046001       MOVE ZERO             TO EKH-BEVAT                                 
046002                                EKH-IDANALYS                              
046003                                EKH-IDKONTO                               
046100                                EKH-KDANMORS                              
046200                                EKH-KDFRAKT                               
046300                                EKH-SUVAT                                 
046400                                EKH-DAAVIDAT                              
046500                                EKH-IDAVINR                               
046600                                EKH-KDAVVTYP                              
046700                                EKH-KDRT                                  
046800                                EKH-KVANTMOT                              
046900                                EKH-KVAVIS                                
047000       MOVE WS-KDSORT        TO EKH-KDSORT                                
047100       MOVE SPACE            TO EKH-KDTRADP                               
047101       MOVE SPACE            TO EKH-FLDCET                                
047110       MOVE SPACE            TO EKH-IDKUNDRF                              
047120       MOVE SPACE            TO EKH-IDFAKT-EXP                            
047200       PERFORM IMS-ISRT-WDR901                                            
047300       PERFORM UNTIL SEGMENT-FINNS                                        
047400         ADD +1  TO FIL-IDSEKVNR                                          
047500         PERFORM IMS-ISRT-WDR901                                          
047600       END-PERFORM                                                        
047700       MOVE 'POSTER'       TO POSTSUM-FDNAMN                              
047800       MOVE 'LADDATS '     TO POSTSUM-DDNAMN2                             
047900       MOVE 'WDR9'         TO POSTSUM-TRANSTYP                            
048000       CALL POSTSUM USING POSTSUM-PARM                                    
048100     END-IF                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 Z-FINIT SECTION.                                                         
048500                                                                          
048600     CLOSE W55320                                                         
048700                                                                          
048800     MOVE 'S' TO POSTSUM-OPKOD                                            
048900     CALL POSTSUM USING POSTSUM-PARM                                      
049000     .                                                                    
049100     EJECT                                                                
049200 S01-LAES-W55320  SECTION.                                                
049300                                                                          
049400     READ W55320 INTO IN-AREA                                             
049500     AT END                                                               
049600        SET END-OF-W55320 TO TRUE                                         
049700                                                                          
049800     NOT AT END                                                           
049900        MOVE 'W55320'       TO POSTSUM-FDNAMN                             
050000        MOVE 'W55322D1'     TO POSTSUM-DDNAMN2                            
050100        MOVE 'R25'          TO POSTSUM-TRANSTYP                           
050200        CALL POSTSUM USING POSTSUM-PARM                                   
050300     END-READ                                                             
050400     .                                                                    
050500* --- IMS SEKTIONER ---                                                   
050600                                                                          
050700 X-TAG-CHECKPOINT   SECTION.                                              
050800                                                                          
050900     PERFORM IMS-CHECKPOINT                                               
051000     MOVE ZERO TO CHKP-ANT                                                
051100     .                                                                    
051200     SKIP2                                                                
051300 IMS-GET-ARTC-WLART01 SECTION.                                            
051400                                                                          
051500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
051600          DELIMITED BY SIZE INTO SSA1                                     
051700     MOVE '  GE' TO GODK-STATUSKODER                                      
051800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
051900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
052000     PERFORM IMS-STATUSKONTROLL                                           
052100     .                                                                    
052200     EJECT                                                                
052300 IMS-GET-ARTC-WLARTC11 SECTION.                                           
052400                                                                          
052500     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
052600          DELIMITED BY SIZE INTO SSA1                                     
052700     MOVE '  GE' TO GODK-STATUSKODER                                      
052800     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA1 SSA1                   
052900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
053000     PERFORM IMS-STATUSKONTROLL                                           
053100     .                                                                    
053200     SKIP3                                                                
053300 IMS-REPL-ARTC SECTION.                                                   
053400                                                                          
053500     MOVE '  ' TO GODK-STATUSKODER                                        
053600     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA1                        
053700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
053800     PERFORM IMS-STATUSKONTROLL                                           
053900     .                                                                    
053901     SKIP3                                                                
053910 IMS-GNP-WDK621 SECTION.                                                  
053920                                                                          
053930     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
053940          DELIMITED BY SIZE INTO SSA1                                     
053950     MOVE '  GE' TO GODK-STATUSKODER                                      
053960     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-K621 SSA1                     
053970     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
053980     PERFORM IMS-STATUSKONTROLL                                           
053990     .                                                                    
054000     EJECT                                                                
054100 IMS-GET-WDH801 SECTION.                                                  
054200                                                                          
054300     STRING 'WLPRIG01(WDH801KY =' W-WDH801-X ')'                          
054400          DELIMITED BY SIZE INTO SSA1                                     
054500     MOVE '  GE' TO GODK-STATUSKODER                                      
054600     CALL CBLTDLI USING GHU  PRIG-PCB DLI-IO-AREA2 SSA1                   
054700     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     .                                                                    
055000     SKIP2                                                                
055100 IMS-REPL-WDH801 SECTION.                                                 
055200                                                                          
055300     MOVE '  ' TO GODK-STATUSKODER                                        
055400     CALL CBLTDLI USING REPL PRIG-PCB DLI-IO-AREA2                        
055500     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
055600     PERFORM IMS-STATUSKONTROLL                                           
055700     .                                                                    
055800     EJECT                                                                
055900 IMS-GU-SLAGERROT SECTION.                                                
056000                                                                          
056100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
056200            DELIMITED BY SIZE INTO SSA1                                   
056300     MOVE '  GE' TO GODK-STATUSKODER                                      
056400     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA4 SSA1                     
056500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
056600     PERFORM IMS-STATUSKONTROLL                                           
056700     .                                                                    
056800     SKIP2                                                                
056900 IMS-GNP-SLAGERINFO SECTION.                                              
057000     STRING 'WLARTS11(IDDC    >=' W-IDDC-MIN-X                            
057100                    '&IDDC    <=' W-IDDC-MAX-X ')'                        
057200            DELIMITED BY SIZE INTO SSA1                                   
057300     MOVE '  GE' TO GODK-STATUSKODER                                      
057400     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA4 SSA1                    
057500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
057600     PERFORM IMS-STATUSKONTROLL                                           
057700     .                                                                    
057800     EJECT                                                                
057900********** WDR9 PEDAL*********************************************        
058000 IMS-ISRT-WDR901 SECTION.                                                 
058100     SKIP2                                                                
058200     MOVE 'WLSAPA01 ' TO SSA1                                             
058300     MOVE '  II' TO GODK-STATUSKODER                                      
058400     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
058500     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
058600     PERFORM IMS-STATUSKONTROLL                                           
058700     .                                                                    
058800     EJECT                                                                
058900 IMS-RESTART SECTION.                                                     
059000                                                                          
059100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
059200     MOVE '  ' TO GODK-STATUSKODER                                        
059300     CALL CBLTDLI USING XRST MSG-PCB                                      
059400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
059500                        CHKP-AREA-LENGTH CHKP-AREA                        
059600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     .                                                                    
059900     EJECT                                                                
060000 IMS-CHECKPOINT SECTION.                                                  
060100                                                                          
060200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
060300     MOVE '  XD' TO GODK-STATUSKODER                                      
060400     CALL CBLTDLI USING CHKP MSG-PCB                                      
060500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
060600                        CHKP-AREA-LENGTH CHKP-AREA                        
060700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060800     PERFORM IMS-STATUSKONTROLL                                           
060900                                                                          
061000     IF IMS-EJ-OK                                                         
061100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
061200       DISPLAY FELTEXT                                                    
061300       CALL FELLOG                                                        
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700 IMS-STATUSKONTROLL SECTION.                                              
061800                                                                          
061900     SET STATUS-IX TO 1                                                   
062000     SEARCH GODK-STATUS                                                   
062100       AT END                                                             
062200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
062300           DELIMITED BY SIZE INTO FELTEXT                                 
062400         DISPLAY FELTEXT                                                  
062500         CALL FELLOG                                                      
062600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
062700         CONTINUE                                                         
062800     END-SEARCH                                                           
062900     .                                                                    
