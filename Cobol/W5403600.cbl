000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5403600.                                                
000300*AUTHOR.         KARL JOHAN HANSSON.                                      
000400*DATE-WRITTEN.   NOV 2003.                                                
000500*    REMARKS.                                                             
000600*        FUNKTION.                                                        
000700*            SAMTLIGA ARTIKLAR PÅ BASEN WDK6 LÄSES.                       
000800*            ALLA ARTIKLAR DÄR KDERS-UTG ÄR 0 BEHANDLAS.                  
000900*            OMRÄKNING AV BESTÄLLNINGSPRISER SKER I 21-                   
001000*            OCH 11-SEGMENTEN, ÄVEN SJÄLVKOST RÄKNAS OM.                  
001100*                                                                         
001200*        RETURKOD.                                                        
001300*                                                                         
001400*        INDATA.                                                          
001500*            LEVERANTÖRSREGISTRET WDF1                                    
001600*            ARTIKELREGISTRET     WDK6                                    
001700*            REGISTER FÖR ÅRSKURS WDG2 (HTYP 9305)                        
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200                                                                          
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600 77  IDPGM                    PIC X(8)          VALUE 'W5403600'.         
002700 77  JA                       PIC X                   VALUE 'J'.          
002800 77  NEJ                      PIC X                   VALUE 'N'.          
002900 77  FLOMRAKNAT-21-SEG        PIC X                   VALUE 'N'.          
003000 77  SEGM-RKN                 PIC S9      COMP-3      VALUE +0.           
003100 77  RETURKOD                 PIC S9(4)   COMP SYNC   VALUE +0.           
003110 77  W-CHKP-RAKNARE           PIC S9(5)   VALUE +0    COMP-3.             
003120 77  W-CHKP-MAX               PIC S9(5)   VALUE +900  COMP-3.             
003130 77  CHKP-ID                  PIC X(8)    VALUE 'W5403600'.               
003140 77  MSG-IO-AREA-LENGTH-1     PIC S9(9)   VALUE +32  COMP SYNC.           
003150 77  MSG-IO-AREA-1            PIC X(32)   VALUE SPACE.                    
003160 77  CHKP-AREA-1-LENGTH       PIC S9(9)   VALUE +32  COMP SYNC.           
003170 77  CHKP-AREA-1              PIC X(32)   VALUE SPACE.                    
003180 77  WS-COUNT                 PIC 9(5)    VALUE ZEROS.                    
003190 77  W-DATE-AAMM              PIC 9(4)    VALUE ZERO.                     
003200                                                                          
003300 01  ARBETS-AREA.                                                         
003400     03  SPAR-PRHANTK        PIC S9(5)V9(2)  COMP-3  VALUE +0.            
003500     03  NYTT-PRARTBES       PIC S9(7)V99    COMP-3  VALUE +0.            
003600     03  NYTT-PRARTSJK       PIC S9(7)V99    COMP-3  VALUE +0.            
003700     03  LEV-RETULF          PIC S9(3)V9(4)  COMP-3  VALUE +0.            
003800     03  W-RETULF            PIC S9(3)V9(4)  COMP-3  VALUE +0.            
003900     03  W-PRKURS            PIC S9(6)V9(5)  COMP-3  VALUE +0.            
004000     03  W-REVALUTA          PIC S9(3)       COMP-3  VALUE +0.            
004100     03  W-PRL-PRARTBES-PR   PIC S9(7)V99    COMP-3  VALUE +0.            
004200     03  ANT-HTAG            PIC S9(7)       COMP-3  VALUE +0.            
004300     03  ANT-USJK            PIC S9(7)       COMP-3  VALUE +0.            
004400     03  ANT21               PIC S9(7)       COMP-3  VALUE +0.            
004500     03  ANT22               PIC S9(7)       COMP-3  VALUE +0.            
004600     03  ANT23               PIC S9(7)       COMP-3  VALUE +0.            
004700     03  ANT27               PIC S9(7)       COMP-3  VALUE +0.            
004800     EJECT                                                                
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200                                                                          
005300 01  GEN-SUBPGM.                                                          
005400     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
005500     03  FELLOG              PIC X(8)    VALUE 'FELLOG '.                 
005600     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
005610     03  W510CURR            PIC X(8)    VALUE 'W510CURR'.                
005700     EJECT                                                                
005800 01  GODK-STATUSKODER.                                                    
005900     03  GODK-STATUS   PIC XX  OCCURS 5  INDEXED BY STATUS-IX.            
006000     SKIP3                                                                
006100 01  STATUS-WS         PIC XX.                                            
006200     88  SEGM-FINNS           VALUE '  '.                                 
006300     88  SEGM-SAKNAS          VALUE 'GE'.                                 
006400     88  BASEN-SLUT           VALUE 'GB'.                                 
006500                                                                          
006600 01  NYCKLAR-TILL-DLI.                                                    
006700     03  W-IDLEVNR-X.                                                     
006800         05  W-IDLEVNR        PIC X(5)    VALUE SPACE.                    
006810     03  W-IDLANDX2-X.                                                    
006820         05  W-IDLANDX2       PIC X(2)    VALUE SPACE.                    
007600     03  W-IDARTNR-X.                                                     
007700         05  W-IDARTNR        PIC S9(9)   COMP-3.                         
007800     SKIP3                                                                
007900 01  SSA-ER.                                                              
008000     03  SSA1        PIC X(128).                                          
008100     03  SSA2        PIC X(128).                                          
008200     EJECT                                                                
008300*01  -COPY W0003                                                          
008400     EJECT                                                                
008410*01  -COPY W510CURR                                                       
008420     EJECT                                                                
008500 01  FILLER                  PIC X(16) VALUE 'WDK601-AREA'.               
008600                                                                          
008700 01  DLI-IO-WDK601.                                                       
008800*    03 WDK611   -COPY WDK601                                             
008900     EJECT                                                                
009000 01  FILLER                  PIC X(16) VALUE 'WDK611-AREA'.               
009100                                                                          
009200 01  DLI-IO-WDK611.                                                       
009300*    03 WDK611   -COPY WDK611                                             
009400     EJECT                                                                
009500 01  FILLER                  PIC X(16) VALUE 'WDK621-AREA'.               
009600                                                                          
009700 01  DLI-IO-WDK621.                                                       
009800*    03 WDK621   -COPY WDK621                                             
009900     EJECT                                                                
010000 01  FILLER                  PIC X(16) VALUE 'WDK622-AREA'.               
010100                                                                          
010200 01  DLI-IO-WDK622.                                                       
010300*    03 WDK622   -COPY WDK622                                             
010400     EJECT                                                                
010500 01  FILLER                  PIC X(16) VALUE 'WDK623-AREA'.               
010600                                                                          
010700 01  DLI-IO-WDK623.                                                       
010800*    03 WDK623   -COPY WDK623                                             
010900     EJECT                                                                
011000 01  FILLER                  PIC X(16) VALUE 'WDK627-AREA'.               
011100                                                                          
011200 01  DLI-IO-WDK627.                                                       
011300*    03 WDK627   -COPY WDK627                                             
011400     EJECT                                                                
012000 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
012100                                                                          
012200*01  WDF101 -COPY WDF101                                                  
012300     EJECT                                                                
012400 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
012500*01  WDF102S   -COPY WDF102 -PRE LEV-                                     
012600     EJECT                                                                
013010 01  DLI-IO-WDK6-2-AREA.                                                  
013021*    03 WDK601   -COPY WDK601 -PRE K601-2                                 
013022*    03 WDK611   -COPY WDK611 -PRE K611-2                                 
013030     EJECT                                                                
013100 LINKAGE SECTION.                                                         
013110*01  -COPY W0009     -PRE MSG-                                            
013120     EJECT                                                                
013200*01  -COPY W0008     -PRE LEV-                                            
013300     05  FILLER                  PIC X.                                   
013400                                                                          
013500*01  -COPY W0008     -PRE WDK6-                                           
013600     05  FILLER                  PIC X.                                   
013700                                                                          
013800*01  -COPY W0008     -PRE WDK6-2-                                         
013900     05  FILLER                  PIC X.                                   
014000                                                                          
014100*01  -COPY W0008     -PRE WDG2-                                           
014200     05  FILLER                  PIC X.                                   
014300     EJECT                                                                
014400 PROCEDURE DIVISION USING   MSG-PCB LEV-PCB WDK6-PCB WDK6-2-PCB           
014410                            WDG2-PCB.                                     
014500 MAIN SECTION.                                                            
014600     ENTRY 'DLITCBL' USING MSG-PCB LEV-PCB WDK6-PCB WDK6-2-PCB            
014610                           WDG2-PCB.                                      
014700                                                                          
014710     PERFORM A-INIT                                                       
014900                                                                          
015000     PERFORM IMS-GN-WDK601                                                
015100     PERFORM UNTIL BASEN-SLUT                                             
015200       MOVE ART-IDARTNR              TO W-IDARTNR                         
015300       MOVE NEJ                      TO FLOMRAKNAT-21-SEG                 
015400       IF ART-KDERS-UTG = 0                                               
015500         MOVE 1.0812                 TO W-RETULF                          
015600                                        LEV-RETULF                        
015700******611***** LÄS PRIMÄR                                                 
015800         PERFORM IMS-GNP-WDK611                                           
015900         IF SEGM-FINNS                                                    
016000           MOVE CLAG-PRDIRLON        TO SPAR-PRHANTK                      
016100           ADD  CLAG-PRDMTRL         TO SPAR-PRHANTK                      
016200           ADD  CLAG-PROVRPAL        TO SPAR-PRHANTK                      
016300         ELSE                                                             
016400           MOVE ZERO                 TO SPAR-PRHANTK                      
016500         END-IF                                                           
016600         MOVE 0                      TO NYTT-PRARTBES                     
016610         MOVE 0                      TO NYTT-PRARTSJK                     
016700******621*****                                                            
016800         MOVE +0                     TO SEGM-RKN                          
016900         PERFORM IMS-GHNP-WDK621                                          
017000         PERFORM UNTIL NOT SEGM-FINNS                                     
017100           IF PRL-FLHUVLEV = JA                                           
017200              PERFORM E-UPPDAT-WDK621-SEGM                                
017300           END-IF                                                         
017400           PERFORM IMS-GHNP-WDK621                                        
017500         END-PERFORM                                                      
017600******611***** LÄS OCH EV UPPDATERA SEKUNDÄR                              
017710         PERFORM IMS-GHU-WDK611-MED-WDK6-2-PCB                            
017800         IF SEGM-FINNS                                                    
017810           IF K601-2ART-KDSORT = 'SW'                                     
017812             MOVE ZERO                 TO K611-2CLAG-PRHEMTAG             
017820           ELSE                                                           
017900             IF K611-2CLAG-PRINK NOT = +0                                 
018000               COMPUTE K611-2CLAG-PRHEMTAG ROUNDED =                      
018100                 K611-2CLAG-PRINK * (W-RETULF - 1) / W-RETULF             
018200               END-COMPUTE                                                
018300             ELSE                                                         
018400               MOVE ZERO               TO K611-2CLAG-PRHEMTAG             
018500             END-IF                                                       
018501             IF CLAG-PRHEMTAG NOT = K611-2CLAG-PRHEMTAG                   
018502               ADD +1                  TO ANT-HTAG                        
018503             END-IF                                                       
018510           END-IF                                                         
018600                                                                          
018700           IF NYTT-PRARTSJK > 0                                           
018800             IF NYTT-PRARTSJK NOT = K611-2CLAG-PRARTSJK                   
018900               ADD +1                TO ANT-USJK                          
019000             END-IF                                                       
019200             MOVE NYTT-PRARTSJK      TO K611-2CLAG-PRARTSJK               
019300           END-IF                                                         
019700           PERFORM IMS-REPL-WDK611-MED-WDK6-2-PCB                         
019710           ADD +1                    TO W-CHKP-RAKNARE                    
019800         END-IF                                                           
019900******622*****                                                            
020000         MOVE +0                     TO SEGM-RKN                          
020100         PERFORM IMS-GHNP-WDK622                                          
020200         PERFORM UNTIL NOT SEGM-FINNS                                     
020300           ADD +1                    TO SEGM-RKN                          
020400           IF SEGM-RKN > +5                                               
020500             PERFORM IMS-DLET-WDK622                                      
020510             ADD +1                  TO W-CHKP-RAKNARE                    
020600*    OM SEGMNR > 5 TAS SEGMENTET BORT PÅ BASEN.                           
020700             ADD +1                  TO ANT22                             
020800           END-IF                                                         
020900           PERFORM IMS-GHNP-WDK622                                        
021000         END-PERFORM                                                      
021100******623*****                                                            
021200         MOVE +0                     TO SEGM-RKN                          
021300         PERFORM IMS-GHNP-WDK623                                          
021400         PERFORM UNTIL NOT SEGM-FINNS                                     
021500           ADD +1                    TO SEGM-RKN                          
021600           IF SEGM-RKN > +5                                               
021700             PERFORM IMS-DLET-WDK623                                      
021710             ADD +1                  TO W-CHKP-RAKNARE                    
021800*    OM SEGMNR > 5 TAS SEGMENTET BORT PÅ BASEN.                           
021900             ADD +1                  TO ANT23                             
022000           END-IF                                                         
022100           PERFORM IMS-GHNP-WDK623                                        
022200         END-PERFORM                                                      
022300******627*****                                                            
022400         MOVE +0                     TO SEGM-RKN                          
022500         PERFORM IMS-GHNP-WDK627                                          
022600         PERFORM UNTIL SEGM-SAKNAS                                        
022700           ADD +1                    TO SEGM-RKN                          
022800           IF SEGM-RKN > +5                                               
022900             PERFORM IMS-DLET-WDK627                                      
022910             ADD +1                  TO W-CHKP-RAKNARE                    
023000*    OM SEGMNR > 5 TAS SEGMENTET BORT PÅ BASEN.                           
023100             ADD +1                  TO ANT27                             
023200           END-IF                                                         
023300           PERFORM IMS-GHNP-WDK627                                        
023400         END-PERFORM                                                      
023500       END-IF                                                             
023600                                                                          
023610       IF W-CHKP-RAKNARE       >  W-CHKP-MAX                              
023620         PERFORM IMS-CHECKPOINT                                           
023621         COMPUTE WS-COUNT = WS-COUNT + 1                                  
023630         MOVE ZERO                    TO W-CHKP-RAKNARE                   
023641         PERFORM IMS-GU-WDK601                                            
023650       END-IF                                                             
023700       PERFORM IMS-GN-WDK601                                              
023800     END-PERFORM                                                          
023900                                                                          
024000     DISPLAY '**************************************************'         
024100     DISPLAY 'ANTAL UPPDAT. ART MED NYTT BES O SJKPRIS ' ANT-USJK         
024200     DISPLAY 'ANTAL UPPDAT. ARTIKLAR MED NY HEMTAGPÅL  ' ANT-HTAG         
024300     DISPLAY 'ANTAL UPPDATERADE WDK621-SEGMENT         ' ANT21            
024400     DISPLAY 'ANTAL BORTTAGNA WDK622-SEGMENT           ' ANT22            
024500     DISPLAY 'ANTAL BORTTAGNA WDK623-SEGMENT           ' ANT23            
024600     DISPLAY 'ANTAL BORTTAGNA WDK627-SEGMENT           ' ANT27            
024610     DISPLAY 'NO OF TIMES CHECKPOINT MAX IS REACHED    ' WS-COUNT         
024700     DISPLAY '**************************************************'         
024800     MOVE +0 TO RETURN-CODE                                               
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025110 A-INIT SECTION.                                                          
025111                                                                          
025120     MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)                  
025121     MOVE 01                         TO W-DATE-AAMM(3:2)                  
025122     PERFORM IMS-RESTART                                                  
025123                                                                          
025124     MOVE ZERO         TO W-CHKP-RAKNARE                                  
025130     .                                                                    
025140     EJECT                                                                
025200 E-UPPDAT-WDK621-SEGM SECTION.                                            
025300                                                                          
025400******************************************************************        
025500*    ETT K621-SEGMENT HAR LÄSTS.                                          
025600*    OM SEGMNR = 1 TESTA  OCH UPPDATERA 21-SEGMENTET.                     
025700*    OM GIVNA VILLKOR ÄR UPPFYLLDA SKALL ETT NYTT BESTÄLLNINGS-           
025800*    PRIS BERÄKNAS OCH REPLACE AV 21-SEGMENTET SKER DÅ.                   
025900*    EVENTUELLT RÄKNAS BEST.PRIS I 11-SEGMENTET OCKSÅ OM.                 
026000******************************************************************        
026100                                                                          
026200     ADD +1                   TO SEGM-RKN                                 
026300                                                                          
026400     IF SEGM-RKN = +1                                                     
026500       PERFORM EA-BER-PRIS-I-K621-SEGM                                    
026600     END-IF                                                               
026700                                                                          
026800***** HÄMTA FÖRSTA INLEV.MÄRKTA RAD-PRIS                                  
026900     IF FLOMRAKNAT-21-SEG = NEJ AND PRL-SUINLEV-PR > 0                    
027000       MOVE JA                TO FLOMRAKNAT-21-SEG                        
027100       PERFORM EB-BER-INLEV-PRIS-SEG                                      
027200     END-IF                                                               
027300     .                                                                    
027400     EJECT                                                                
027500 EA-BER-PRIS-I-K621-SEGM SECTION.                                         
027600                                                                          
027700     PERFORM E01-LAS-RETULF-OCH-KURS                                      
027800     MOVE LEV-RETULF          TO W-RETULF                                 
027900                                                                          
028000     MOVE PRL-PRARTBES-PR     TO W-PRL-PRARTBES-PR                        
028100     COMPUTE PRL-PRARTBES-PR ROUNDED =                                    
028200            PRL-PRARTBEL-PR * LEV-RETULF *                                
028300              W-PRKURS / W-REVALUTA                                       
028400     IF PRL-PRARTBES-PR = 0 AND PRL-PRARTBEL-PR > 0                       
028500       MOVE 0.01              TO PRL-PRARTBES-PR                          
028600     END-IF                                                               
028700     PERFORM IMS-REPL-WDK621                                              
028710     ADD +1       TO W-CHKP-RAKNARE                                       
028800     IF PRL-PRARTBES-PR NOT = W-PRL-PRARTBES-PR                           
028900       ADD +1                 TO ANT21                                    
029000     END-IF                                                               
029100     .                                                                    
029200     EJECT                                                                
029300 EB-BER-INLEV-PRIS-SEG SECTION.                                           
029400                                                                          
029500     PERFORM E01-LAS-RETULF-OCH-KURS                                      
029600                                                                          
029700     IF PRL-PRARTBEL-PR > 0                                               
029800       COMPUTE NYTT-PRARTBES ROUNDED =                                    
029900                    PRL-PRARTBEL-PR * LEV-RETULF *                        
030000                    W-PRKURS / W-REVALUTA                                 
030100       IF NYTT-PRARTBES = ZERO                                            
030200         MOVE 0.01            TO NYTT-PRARTBES                            
030300       END-IF                                                             
030400       COMPUTE NYTT-PRARTSJK = NYTT-PRARTBES + SPAR-PRHANTK               
030500     END-IF                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 E01-LAS-RETULF-OCH-KURS SECTION.                                         
030900                                                                          
031000     MOVE PRL-IDLEVNR           TO W-IDLEVNR                              
031010     MOVE 'SE'                  TO W-IDLANDX2                             
031100     PERFORM IMS-GU-WDF102                                                
031200     IF SEGM-FINNS                                                        
031300       MOVE LEV-TULL-RETULF-1   TO LEV-RETULF                             
031400     END-IF                                                               
031500                                                                          
031600     MOVE PRL-KDVALISO          TO CURR-KDVALISO-ROW                      
031610     MOVE 'SEK'                 TO CURR-KDVALISO-HUV                      
031700     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
031710     MOVE 'A'                   TO CURR-KDVALTYP                          
031720     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
031730     IF CURR-KDSVAR = ' '                                                 
031900       MOVE CURR-PRKURS-NEW     TO W-PRKURS                               
032000       MOVE CURR-REVALUTA-TO    TO W-REVALUTA                             
032100     ELSE                                                                 
032200       MOVE +1                  TO W-PRKURS                               
032300                                   W-REVALUTA                             
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 IMS-GU-WDF102 SECTION.                                                   
032800                                                                          
032900     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
033000     DELIMITED BY SIZE INTO SSA1                                          
033100     STRING 'WDF102  (IDLAND   =' W-IDLANDX2-X ')'                        
033110     DELIMITED BY SIZE INTO SSA2                                          
033300     MOVE '  GE' TO GODK-STATUSKODER                                      
033400     CALL CBLTDLI USING GU LEV-PCB LEV-WDF102S SSA1 SSA2                  
033500     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
033600     PERFORM IMS-STATUSKONTROLL                                           
033700     .                                                                    
033800     EJECT                                                                
035100 IMS-GN-WDK601 SECTION.                                                   
035200                                                                          
035300     MOVE 'WDK601 ' TO SSA1                                               
035400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
035500     CALL CBLTDLI USING  GN  WDK6-PCB DLI-IO-WDK601 SSA1                  
035600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
035700     PERFORM IMS-STATUSKONTROLL                                           
035800     .                                                                    
035900     EJECT                                                                
035910 IMS-GU-WDK601 SECTION.                                                   
035920                                                                          
035931     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
035932          DELIMITED BY SIZE INTO SSA1                                     
035940     MOVE '  GEGB' TO GODK-STATUSKODER                                    
035950     CALL CBLTDLI USING  GU  WDK6-PCB DLI-IO-WDK601 SSA1                  
035960     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
035970     PERFORM IMS-STATUSKONTROLL                                           
035980     .                                                                    
035990     EJECT                                                                
036000 IMS-GNP-WDK611 SECTION.                                                  
036100                                                                          
036200     STRING 'WDK611  (KDSEGKEY =1)'                                       
036300                      DELIMITED BY SIZE INTO SSA1                         
036400     MOVE '  ' TO GODK-STATUSKODER                                        
036500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
036600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
036700     PERFORM IMS-STATUSKONTROLL                                           
036800     .                                                                    
036900     EJECT                                                                
037000 IMS-GHNP-WDK621 SECTION.                                                 
037100                                                                          
037200     STRING 'WDK611  (KDSEGKEY =1)'                                       
037300                      DELIMITED BY SIZE INTO SSA1                         
037400     MOVE 'WDK621 ' TO SSA2                                               
037500     MOVE '  GE' TO GODK-STATUSKODER                                      
037600     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK621 SSA1 SSA2             
037700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
037800     PERFORM IMS-STATUSKONTROLL                                           
037900     .                                                                    
038000     EJECT                                                                
038100 IMS-REPL-WDK621 SECTION.                                                 
038200                                                                          
038300     MOVE '  ' TO GODK-STATUSKODER                                        
038400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK621                       
038500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
038600     PERFORM IMS-STATUSKONTROLL                                           
038700     .                                                                    
038800     EJECT                                                                
038900 IMS-GHNP-WDK622 SECTION.                                                 
039000                                                                          
039100     STRING 'WDK611  (KDSEGKEY =1)'                                       
039200                      DELIMITED BY SIZE INTO SSA1                         
039300     MOVE 'WDK622 ' TO SSA2                                               
039400     MOVE '  GE' TO GODK-STATUSKODER                                      
039500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK622 SSA1 SSA2             
039600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
039700     PERFORM IMS-STATUSKONTROLL                                           
039800     .                                                                    
039900     EJECT                                                                
040000 IMS-DLET-WDK622 SECTION.                                                 
040100                                                                          
040200     MOVE '  ' TO GODK-STATUSKODER                                        
040300     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK622                       
040400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
040500     PERFORM IMS-STATUSKONTROLL                                           
040600     .                                                                    
040700     EJECT                                                                
040800 IMS-GHNP-WDK623 SECTION.                                                 
040900                                                                          
041000     STRING 'WDK611  (KDSEGKEY =1)'                                       
041100                      DELIMITED BY SIZE INTO SSA1                         
041200     MOVE 'WDK623 ' TO SSA2                                               
041300     MOVE '  GE' TO GODK-STATUSKODER                                      
041400     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK623 SSA1 SSA2             
041500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
041600     PERFORM IMS-STATUSKONTROLL                                           
041700     .                                                                    
041800     EJECT                                                                
041900 IMS-DLET-WDK623 SECTION.                                                 
042000                                                                          
042100     MOVE '  ' TO GODK-STATUSKODER                                        
042200     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK623                       
042300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
042400     PERFORM IMS-STATUSKONTROLL                                           
042500     .                                                                    
042600     EJECT                                                                
042700 IMS-GHNP-WDK627 SECTION.                                                 
042800                                                                          
042900     STRING 'WDK611  (KDSEGKEY =1)'                                       
043000                      DELIMITED BY SIZE INTO SSA1                         
043100     MOVE 'WDK627 ' TO SSA2                                               
043200     MOVE '  GE' TO GODK-STATUSKODER                                      
043300     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK627 SSA1 SSA2             
043400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     .                                                                    
043700     EJECT                                                                
043800 IMS-DLET-WDK627 SECTION.                                                 
043900                                                                          
044000     MOVE '  ' TO GODK-STATUSKODER                                        
044100     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK627                       
044200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
044300     PERFORM IMS-STATUSKONTROLL                                           
044400     .                                                                    
044500     EJECT                                                                
044600 IMS-GHU-WDK611-MED-WDK6-2-PCB SECTION.                                   
044700                                                                          
044800     STRING 'WDK601  *D(IDARTNR  =' W-IDARTNR-X ')'                       
044900                      DELIMITED BY SIZE INTO SSA1                         
045000     MOVE 'WDK611 ' TO SSA2                                               
045100     MOVE '  GE' TO GODK-STATUSKODER                                      
045200     CALL CBLTDLI USING GHU WDK6-2-PCB DLI-IO-WDK6-2-AREA                 
045210                            SSA1 SSA2                                     
045300     MOVE WDK6-2-STATUS-CODE TO STATUS-WS                                 
045400     PERFORM IMS-STATUSKONTROLL                                           
045500     .                                                                    
045600     EJECT                                                                
045700 IMS-REPL-WDK611-MED-WDK6-2-PCB SECTION.                                  
045800                                                                          
045900     MOVE '  ' TO GODK-STATUSKODER                                        
046000     CALL CBLTDLI USING REPL WDK6-2-PCB DLI-IO-WDK6-2-AREA                
046100     MOVE WDK6-2-STATUS-CODE TO STATUS-WS                                 
046200     PERFORM IMS-STATUSKONTROLL                                           
046300     .                                                                    
046400     EJECT                                                                
046410 IMS-RESTART  SECTION.                                                    
046420                                                                          
046430     MOVE SPACE TO MSG-IO-AREA-1                                          
046440     MOVE '  ' TO GODK-STATUSKODER                                        
046450     CALL CBLTDLI USING XRST MSG-PCB                                      
046460                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
046470                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
046480     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046490     PERFORM IMS-STATUSKONTROLL                                           
046491     .                                                                    
046492     EJECT                                                                
046494 IMS-CHECKPOINT  SECTION.                                                 
046495                                                                          
046496     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
046497     MOVE '  XD' TO GODK-STATUSKODER                                      
046498     CALL CBLTDLI USING CHKP MSG-PCB                                      
046499                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
046500                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
046501     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046502     PERFORM IMS-STATUSKONTROLL                                           
046503     .                                                                    
046504     EJECT                                                                
046510 IMS-STATUSKONTROLL SECTION.                                              
046600                                                                          
046700     SET STATUS-IX TO 1                                                   
046800     SEARCH GODK-STATUS                                                   
046900       AT END                                                             
047000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
047100           DELIMITED BY SIZE INTO FELTEXT                                 
047200         DISPLAY FELTEXT                                                  
047300         CALL FELLOG                                                      
047400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047500         CONTINUE                                                         
047600     END-SEARCH                                                           
047700     .                                                                    
