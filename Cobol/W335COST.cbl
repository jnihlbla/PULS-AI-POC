000100 ID DIVISION.                                                             
000200 PROGRAM-ID.       W335COST                                               
000300 AUTHOR.           ELEONOR ÖSTRÖM                                         
000400 DATE-WRITTEN.     DEC  2002.                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    PROGRAMMET ÄR ETT SUBPROGRAM SOM RÄKNAR UT SJÄLVKOST                 
000800*    MED MÅNADSKURS FÖR HUVUDLEVERANTÖRER OCH LOKALA                      
000900*    LEVERANTÖRER.                                                        
001000*                                                                         
001100*                                                                         
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 DATA DIVISION.                                                           
001600     SKIP2                                                                
001700 WORKING-STORAGE SECTION.                                                 
001800                                                                          
001900 77  JA                  PIC X(1)  VALUE 'J'.                             
002000 77  YES                 PIC X(1)  VALUE 'Y'.                             
002100 77  NEJ                 PIC X(1)  VALUE 'N'.                             
002200 77  FLSLUTA-LAS         PIC X     VALUE 'N'.                             
002300 77  INDX                PIC S9(9) VALUE +0   COMP SYNC.                  
002400                                                                          
002500 77  FORTSATT-SW         PIC X.                                           
002600     88  FORTSATT-OK               VALUE 'J'.                             
002700     88  FORTSATT-EJ-OK            VALUE 'N'.                             
002800*                                                                         
002900 77  FELTEXT             PIC X(80)   VALUE SPACE.                         
003000                                                                          
003100 01  W-TEST-IDARTNR      PIC S9(9) COMP-3 VALUE ZERO.                     
003200 01  W-TEST-IDLEVNR      PIC X(5)  VALUE SPACE.                           
003300 01  WS-IDLEVNR          PIC X(5)  VALUE SPACE.                           
003400 01  SPAR-PRARTBES       PIC S9(7)V9(2)      VALUE ZERO COMP-3.           
003500 01  SPAR-PRARTSJK       PIC S9(7)V9(2)      VALUE ZERO COMP-3.           
003600 01  SPAR-PRARTBEL-PR    PIC S9(7)V9(5)      VALUE ZERO COMP-3.           
003700 01  SPAR-PRDIRLON       PIC S9(4)V9(3)      VALUE ZERO COMP-3.           
003800 01  SPAR-PRDMTRL        PIC S9(6)V9(3)      VALUE ZERO COMP-3.           
003900 01  SPAR-PROVRPAL       PIC S9(4)V9(3)      VALUE ZERO COMP-3.           
004000 01  WS-RETULF           PIC S9(3)V9(4)      VALUE ZERO COMP-3.           
004100 01  WS-PRKURS           PIC S9(5)V9(5)      VALUE ZERO COMP-3.           
004200 01  WS-PRARTBES-ARB     PIC S9(7)V9(5)      VALUE ZERO COMP-3.           
004300 01  WS-PRARTBES-PR      PIC S9(7)V9(2)      VALUE ZERO COMP-3.           
004400 01  WS-PRARTSJK         PIC S9(7)V9(2)      VALUE ZERO COMP-3.           
004502 01  WS-DAGENS-DATUM     PIC 9(8)            VALUE ZERO.                  
004602 01  W-PRKURS            PIC S9(5)V9(2)      VALUE +0   COMP-3.           
004702 01  W-REVALUTA          PIC S9(5)           VALUE +0   COMP-3.           
004802 01  W-DATE-AAMM         PIC 9(4)            VALUE ZERO.                  
004902 01  WS-KDVALISO-HUV     PIC X(3)            VALUE 'SEK'.                 
005002     SKIP2                                                                
005102 01  DAGENS-DATUM        PIC 9(6)            VALUE ZERO.                  
005202 01  DAGENS-DATUM-724    PIC 9(8).                                        
005302     EJECT                                                                
005402                                                                          
005502*    --- VALID IDDC CODES                                                 
005602*01 -COPY WWDC99                                                          
005702*                                                                         
005802*01 -COPY W510CURR                                                        
005902                                                                          
006002 01  GENERELLA-SUBPROGRAM.                                                
006102     03  CBLTDLI           PIC X(8)    VALUE 'CBLTDLI '.                  
006202     03  FELLOG            PIC X(8)    VALUE 'FELLOG  '.                  
006302     03  W510CURR          PIC X(8)    VALUE 'W510CURR'.                  
006402                                                                          
006502*    ---- ARBETSAREOR FÖR IMS-SECTIONERNA                                 
006602 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006702     SKIP3                                                                
006802 01  NYCKLAR-TILL-DLI.                                                    
006902* TILL WDK6.                                                              
007002     03  W-WDK6-IDARTNR-X.                                                
007102         05  W-IDARTNR            PIC S9(9)    VALUE ZERO COMP-3.         
007202     03  W-KDSEGKEY-X.                                                    
007302         05  W-KDSEGKEY           PIC X(1)     VALUE '1'.                 
007402* TILL WDK7.                                                              
007502     03  W-WDK701-IDARTNR-X.                                              
007602         05  W-WDK701-IDARTNR     PIC S9(9)    VALUE ZERO COMP-3.         
007702     03  W-WDK711-IDDC-X.                                                 
007802         05  W-WDK711-IDDC        PIC X(2)     VALUE SPACE.               
007902     03  W-IDDC-K7-X.                                                     
008002         05  W-IDDC-K7            PIC X(2)     VALUE SPACE.               
008102     03  W-IDLEVNR-PR-X.                                                  
008202         05  W-IDLEVNR-PR         PIC X(5)     VALUE ZERO.                
008302     03  W-DAPRLIST-K7-N.                                                 
008402         05  W-DAPRLIST-K7        PIC 9(8)     VALUE ZERO.                
008502     03  W-IDLAND-K7-X.                                                   
008602         05  W-IDLAND-K7          PIC X(2)     VALUE SPACE.               
008702* TILL WDF1.                                                              
008802     03  W-WDF101-IDLEVNR-X.                                              
008902         05    W-WDF101-IDLEVNR   PIC X(5)     VALUE SPACE.               
009002     03  W-WDF102-IDLAND-X.                                               
009102         05    W-WDF102-IDLAND    PIC X(2)     VALUE SPACE.               
009202                                                                          
009900* TILL WDB6                                                               
010000     03  W-IDDC-B6-X.                                                     
010100         05 W-IDDC-B6            PIC X(2).                                
010200     03  W-IDLANDX2-X.                                                    
010300         05    W-IDLANDX2        PIC X(2)    VALUE SPACE.                 
010400* TILL ÅRSKURSER                                                          
010500     03  W-WDGX5117-X.                                                    
010600         05    W-WDGX5117    PIC X(4)    VALUE '5117'.                    
010700         05    W-WDGXTIAA    PIC S9(3)   VALUE ZERO COMP-3.               
010800         05    FILLER        PIC X(24)   VALUE LOW-VALUE.                 
010900     03  W-WDGX5118-X.                                                    
011000         05    W-KDVALISO-Y  PIC X(3)    VALUE SPACE.                     
011100         05    FILLER        PIC X(2)    VALUE LOW-VALUE.                 
011200*    --- IMS FUNKTIONSKODER                                               
011300*01  -COPY W0003.                                                         
011400     EJECT                                                                
011500*    ---- STATUSKOD FRÅN IMS                                              
011600 01  STATUS-WS             PIC XX.                                        
011700     88  SEGMENT-FINNS                 VALUE '  '.                        
011800     88  SEGMENT-SAKNAS                VALUE 'GE'.                        
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP2                                                                
012300 01  SSA1                  PIC X(64).                                     
012400 01  SSA2                  PIC X(64).                                     
012500 01  SSA3                  PIC X(64).                                     
012600     EJECT                                                                
012700*    ---  DLI INPUT-OUTPUT AREA                                           
012800 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK601'.            
012900 01  DLI-IO-WDK601.                                                       
013000*    03  -COPY WDK601                                                     
013100     EJECT                                                                
013200 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK611'.            
013300 01  DLI-IO-WDK611.                                                       
013400*    03  -COPY WDK611                                                     
013500     EJECT                                                                
013600 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK621'.            
013700 01  DLI-IO-WDK621.                                                       
013800*    03  -COPY WDK621                                                     
013900     EJECT                                                                
014400 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDF101'.            
014500 01  DLI-IO-WDF101.                                                       
014600*    03   -COPY WDF101                                                    
014700     EJECT                                                                
014800 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDF102'.            
014900 01  DLI-IO-WDF102.                                                       
015000    03   -COPY WDF102                                                     
015100     EJECT                                                                
015200 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK701'.            
015300 01  DLI-IO-WDK701.                                                       
015400*    03   -COPY WDK701                                                    
015500     EJECT                                                                
015600 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK711'.            
015700 01  DLI-IO-WDK711.                                                       
015800*    03   -COPY WDK711                                                    
015900     EJECT                                                                
016000                                                                          
016100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711-71'.          
016200     SKIP3                                                                
016300 01  DLI-IO-WDK711-71.                                                    
016400*        05  -COPY WDK711 -PRE BP-                                        
016500     SKIP3                                                                
016600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK724'.             
016700 01  DLI-IO-WDK724.                                                       
016800*        05  -COPY WDK724                                                 
016900                                                                          
017000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017100 01   DLI-IO-AREA-B601.                                                   
017200*     03  -COPY WDB601                                                    
017300     EJECT                                                                
017400                                                                          
017500 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
017600 01   DLI-IO-AREA-B617.                                                   
017700*     03  -COPY WDB617                                                    
017800     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300     SKIP2                                                                
018400*   -COPY W335COST                                                        
018500     EJECT                                                                
018600*01  -COPY W0008 -PRE  WDK6-.                                             
018700     05  FILLER        PIC X.                                             
018800     EJECT                                                                
018900                                                                          
019000*01  -COPY W0008 -PRE  WDK7-.                                             
019100     05  FILLER        PIC X.                                             
019200     EJECT                                                                
019300                                                                          
019400*01  -COPY W0008 -PRE  WDF1-.                                             
019500     05  FILLER        PIC X.                                             
019600     EJECT                                                                
019802                                                                          
019902*01  -COPY W0008 -PRE  9305-.                                             
020002     05  FILLER        PIC X.                                             
020102     EJECT                                                                
020202                                                                          
020400*01  -COPY W0008 -PRE  WDK72-.                                            
020500     05  FILLER        PIC X.                                             
020600     EJECT                                                                
020700*01  -COPY W0008     -PRE WDB6-                                           
020800     05 FILLER                   PIC X(18).                               
020900     EJECT                                                                
021300 PROCEDURE DIVISION USING COST-W335COST WDK6-PCB WDK7-PCB                 
021401                          WDF1-PCB 9305-PCB                               
021500                          WDK72-PCB WDB6-PCB.                             
021600                                                                          
021700     PERFORM A-INIT                                                       
021800                                                                          
021900     PERFORM B-LAS-ARTIKELREGISTER                                        
022000                                                                          
022100     MOVE ZERO TO RETURN-CODE                                             
022200     GOBACK                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 A-INIT SECTION.                                                          
022600                                                                          
022700     MOVE FUNCTION CURRENT-DATE(3:6) TO  DAGENS-DATUM                     
022800     MOVE FUNCTION CURRENT-DATE(1:8) TO  WS-DAGENS-DATUM                  
022900                                                                          
023000     MOVE ZERO                    TO WS-PRARTBES-ARB                      
023100                                     WS-PRARTBES-PR                       
023200                                     WS-PRARTSJK                          
023300                                     WS-RETULF                            
023400                                     WS-PRKURS                            
023500                                     COST-PRARTBES-MON                    
023600                                     COST-PRARTBES-MONLOC                 
023700                                     COST-PRARTSJK-MON                    
023800                                     COST-PRARTSJK-MONLOC                 
023900                                                                          
024000     .                                                                    
024100     EJECT                                                                
024200                                                                          
024300 B-LAS-ARTIKELREGISTER SECTION.                                           
024400                                                                          
024500     MOVE COST-IDARTNR            TO W-IDARTNR                            
024600     MOVE COST-IDDC               TO WS-IDDC                              
024700                                                                          
024800     PERFORM IMS-GU-WDK601                                                
024900     MOVE ART-IDLEVNR TO  W-WDF101-IDLEVNR                                
025000                          WS-IDLEVNR                                      
025100                                                                          
025200     PERFORM IMS-GNP-WDK611                                               
025300     MOVE CLAG-PRARTSJK        TO SPAR-PRARTSJK                           
025400     MOVE 0.1                  TO SPAR-PRARTBES                           
025500     MOVE CLAG-PRDIRLON        TO SPAR-PRDIRLON                           
025600     MOVE CLAG-PRDMTRL         TO SPAR-PRDMTRL                            
025700     MOVE CLAG-PROVRPAL        TO SPAR-PROVRPAL                           
025800                                                                          
025900**** ALLA DC O HUVUDLEVERANTÖR                                            
026000**** DENNA GÖRS ALLTID                                                    
026100     MOVE ART-IDLEVNR TO W-TEST-IDLEVNR                                   
026200     MOVE NEJ TO FLSLUTA-LAS                                              
026300                                                                          
026400     PERFORM UNTIL SEGMENT-SAKNAS OR FLSLUTA-LAS = JA                     
026500                                                                          
026600       PERFORM IMS-GNP-WDK621                                             
026700                                                                          
026800       IF SEGMENT-FINNS AND PRL-KDSTATUS-PR = 1 AND                       
026900              PRL-SUINLEV-PR > 0 AND PRL-FLHUVLEV = JA                    
027000         MOVE PRL-PRARTBEL-PR    TO SPAR-PRARTBEL-PR                      
027100         MOVE PRL-IDLEVNR        TO W-WDF101-IDLEVNR                      
027200         MOVE PRL-KDVALISO       TO CURR-KDVALISO-ROW                     
027300         PERFORM BA-BEHANDLA-LEVERANTOR                                   
027400         PERFORM BB-HAMTA-PRKURS                                          
027500         IF PRL-FLHUVLEV = JA                                             
027600           PERFORM BC-BERAKNA-PRARTBES-HLEV                               
027700         ELSE                                                             
027800           PERFORM BD-BERAKNA-PRARTBES-EJ-HLEV                            
027900         END-IF                                                           
028000         PERFORM BE-BERAKNA-PRARTSJK                                      
028100         MOVE JA TO FLSLUTA-LAS                                           
028200       END-IF                                                             
028300                                                                          
028400     END-PERFORM                                                          
028500     IF WS-PRARTBES-PR > 0                                                
028600       MOVE WS-PRARTBES-PR           TO COST-PRARTBES-MON                 
028700       MOVE WS-PRARTSJK              TO COST-PRARTSJK-MON                 
028800     ELSE                                                                 
028900       MOVE SPAR-PRARTBES            TO COST-PRARTBES-MON                 
029000       MOVE SPAR-PRARTSJK            TO COST-PRARTSJK-MON                 
029100     END-IF                                                               
029200                                                                          
029300     IF COST-IDDC NOT = '11'                                              
029400                                                                          
029500       MOVE COST-IDARTNR         TO W-WDK701-IDARTNR                      
029600                                    W-IDARTNR                             
029700       MOVE COST-IDDC            TO W-WDK711-IDDC                         
029800                                    W-IDDC-K7                             
029900                                    WS-IDDC                               
030000       MOVE NEJ TO FLSLUTA-LAS                                            
030100                                                                          
030200       PERFORM IMS-GU-WDK711                                              
030300                                                                          
030400*** NDC OCH LOKALLEVERANTÖR                                               
030500                                                                          
030600       IF NDC AND SLAG-IDLEVNR NOT = '1441 ' AND                          
030700          NOT NDC-CN AND NOT NDC-US                                       
030800         PERFORM IMS-GU-WDK601                                            
030900         PERFORM IMS-GNP-WDK611                                           
031000         MOVE +0                 TO WS-PRARTBES-PR                        
031100         MOVE SLAG-IDLEVNR       TO W-TEST-IDLEVNR                        
031200                                    W-WDF101-IDLEVNR                      
031300                                                                          
031400         PERFORM UNTIL SEGMENT-SAKNAS OR FLSLUTA-LAS = JA                 
031500                                                                          
031600           PERFORM IMS-GNP-WDK621                                         
031700           IF SEGMENT-FINNS AND PRL-KDSTATUS-PR = 1                       
031800             IF PRL-IDLEVNR = W-TEST-IDLEVNR                              
031900               MOVE PRL-PRARTBEL-PR    TO SPAR-PRARTBEL-PR                
032000               MOVE PRL-KDVALISO          TO CURR-KDVALISO-ROW            
032100               PERFORM BA-BEHANDLA-LEVERANTOR                             
032200               PERFORM BB-HAMTA-PRKURS                                    
032300               IF PRL-FLHUVLEV = JA                                       
032400                 PERFORM BC-BERAKNA-PRARTBES-HLEV                         
032500               ELSE                                                       
032600                 PERFORM BD-BERAKNA-PRARTBES-EJ-HLEV                      
032700               END-IF                                                     
032800               PERFORM BE-BERAKNA-PRARTSJK                                
032900               MOVE JA TO FLSLUTA-LAS                                     
033000             END-IF                                                       
033100           END-IF                                                         
033200         END-PERFORM                                                      
033300         IF WS-PRARTBES-PR > 0                                            
033400           MOVE WS-PRARTBES-PR       TO COST-PRARTBES-MONLOC              
033500           MOVE WS-PRARTSJK          TO COST-PRARTSJK-MONLOC              
033600         ELSE                                                             
033700           MOVE SPAR-PRARTBES        TO COST-PRARTBES-MONLOC              
033800           MOVE SPAR-PRARTSJK        TO COST-PRARTSJK-MONLOC              
033900         END-IF                                                           
034000       END-IF                                                             
034100                                                                          
034200*** KINA ELLER USA NDC OCH LOKALLEVERANTÖR                                
034300                                                                          
034400       IF (NDC-CN OR NDC-US) AND                                          
034500          SLAG-IDLEVNR NOT = '1441 '                                      
034600         MOVE +0                 TO WS-PRARTBES-PR                        
034700                                                                          
034800*    -- WDK711                                                            
034900         PERFORM IMS-GU-WDK711-BESPRIS                                    
035000         IF SEGMENT-FINNS                                                 
035100                                                                          
035200*    -- WDK724                                                            
035300           MOVE SLAG-IDLEVNR         TO W-IDLEVNR-PR                      
035400                                        W-WDF101-IDLEVNR                  
035500           COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAGENS-DATUM             
035600           PERFORM IMS-GNP-WDK724                                         
035700                                                                          
035800           IF SEGMENT-FINNS                                               
035900             MOVE SPRL-PRARTBEL-PR   TO SPAR-PRARTBEL-PR                  
036000             MOVE SPRL-KDVALISO      TO CURR-KDVALISO-ROW                 
036100             PERFORM BA-BEHANDLA-LEVERANTOR                               
036200             PERFORM BB-HAMTA-PRKURS                                      
036300             PERFORM BC-BERAKNA-PRARTBES-HLEV                             
036400             PERFORM BE-BERAKNA-PRARTSJK                                  
036500           END-IF                                                         
036600           IF WS-PRARTBES-PR > 0                                          
036700             MOVE WS-PRARTBES-PR     TO COST-PRARTBES-MONLOC              
036800             MOVE WS-PRARTSJK        TO COST-PRARTSJK-MONLOC              
036900           ELSE                                                           
037000             MOVE SPAR-PRARTBES      TO COST-PRARTBES-MONLOC              
037100             MOVE SPAR-PRARTSJK      TO COST-PRARTSJK-MONLOC              
037200           END-IF                                                         
037300         END-IF                                                           
037400       END-IF                                                             
037500     END-IF                                                               
037600     .                                                                    
037700     EJECT                                                                
037800 BA-BEHANDLA-LEVERANTOR SECTION.                                          
037900                                                                          
038000     MOVE +1    TO WS-RETULF                                              
038100     PERFORM IMS-GU-WDF101                                                
038200     IF SEGMENT-FINNS                                                     
038300       IF NDC-CN                                                          
038400         MOVE 'CN' TO W-WDF102-IDLAND                                     
038500       ELSE                                                               
038600         IF NDC-US                                                        
038700           MOVE 'US' TO W-WDF102-IDLAND                                   
038800         ELSE                                                             
038900           MOVE 'SE' TO W-WDF102-IDLAND                                   
039000         END-IF                                                           
039100       END-IF                                                             
039200       PERFORM IMS-GNP-WDF102                                             
039300       IF SEGMENT-FINNS                                                   
039400         IF TULL-TITULF < DAGENS-DATUM                                    
039500           MOVE TULL-RETULF-1   TO WS-RETULF                              
039600         ELSE                                                             
039700           MOVE TULL-RETULF-2   TO WS-RETULF                              
039800         END-IF                                                           
039900       END-IF                                                             
040000     END-IF                                                               
040100     .                                                                    
040200     EJECT                                                                
040300 BB-HAMTA-PRKURS SECTION.                                                 
040400                                                                          
040502     MOVE DAGENS-DATUM(1:2)     TO W-DATE-AAMM(1:2)                       
040602     MOVE COST-TIMM             TO W-DATE-AAMM(3:2)                       
040702     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
040802     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
040902     MOVE 'M'                   TO CURR-KDVALTYP                          
041002                                                                          
041102     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
041202     IF CURR-KDSVAR = ' '                                                 
041302       MOVE CURR-PRKURS-NEW     TO WS-PRKURS                              
041402     ELSE                                                                 
041502       MOVE 1                   TO WS-PRKURS                              
041602     END-IF                                                               
041700     .                                                                    
041800     EJECT                                                                
041900 BC-BERAKNA-PRARTBES-HLEV SECTION.                                        
042000                                                                          
042100     COMPUTE WS-PRARTBES-ARB ROUNDED = SPAR-PRARTBEL-PR *                 
042202             WS-RETULF * WS-PRKURS                                        
042300     COMPUTE WS-PRARTBES-PR ROUNDED = WS-PRARTBES-ARB                     
042400     .                                                                    
042500     EJECT                                                                
042600 BD-BERAKNA-PRARTBES-EJ-HLEV SECTION.                                     
042700                                                                          
042800     COMPUTE WS-PRARTBES-ARB ROUNDED = SPAR-PRARTBEL-PR                   
042902             * WS-PRKURS                                                  
043000     COMPUTE WS-PRARTBES-PR ROUNDED = WS-PRARTBES-ARB                     
043100     .                                                                    
043200     EJECT                                                                
043300 BE-BERAKNA-PRARTSJK SECTION.                                             
043400                                                                          
043500     IF NDC-CN OR NDC-US                                                  
043600       MOVE WS-IDDC       TO W-IDDC-B6                                    
043700       PERFORM IMS-GU-WDB601                                              
043800       MOVE DCS-IDLANDX2 TO W-IDLANDX2                                    
043900       IF SEGMENT-FINNS                                                   
044000         PERFORM IMS-GNP-WDB617                                           
044100         IF SEGMENT-FINNS                                                 
044200****   VALUTAKURS FÖR CNY/USD, PRISERNA ÄR I SEK I CLAG                   
044300           MOVE WS-DAGENS-DATUM(3:2) TO W-DATE-AAMM(1:2)                  
044310           MOVE 01                   TO W-DATE-AAMM(3:2)                  
044400           IF NDC-CN                                                      
044500              MOVE 'CNY'          TO CURR-KDVALISO-ROW                    
044600           ELSE                                                           
044700              IF NDC-US                                                   
044800                 MOVE 'USD'       TO CURR-KDVALISO-ROW                    
044900              END-IF                                                      
045000           END-IF                                                         
045010           MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                    
045050           MOVE W-DATE-AAMM       TO CURR-TIAAMM                          
045060           MOVE 'A'               TO CURR-KDVALTYP                        
045070           CALL W510CURR USING CURR-W510CURR 9305-PCB                     
045080           IF CURR-KDSVAR = ' '                                           
045300             MOVE CURR-PRKURS-NEW  TO W-PRKURS                            
045400             MOVE CURR-REVALUTA-TO TO W-REVALUTA                          
045500           ELSE                                                           
045600             MOVE 1                TO W-PRKURS                            
045700             MOVE 1                TO W-REVALUTA                          
045800           END-IF                                                         
045900           COMPUTE WS-PRARTSJK ROUNDED = WS-PRARTBES-PR +                 
046000                 (PROC-REDIRLON * SPAR-PRDIRLON                           
046100                 * W-REVALUTA / W-PRKURS) +                               
046200                 (PROC-REDMTRL  * SPAR-PRDMTRL                            
046300                 * W-REVALUTA / W-PRKURS)                                 
046400           END-COMPUTE                                                    
046500         END-IF                                                           
046600       END-IF                                                             
046700     ELSE                                                                 
046800       COMPUTE WS-PRARTSJK = WS-PRARTBES-PR +                             
046900                             SPAR-PRDIRLON +                              
047000                             SPAR-PRDMTRL +                               
047100                             SPAR-PROVRPAL                                
047200     END-IF                                                               
047300     .                                                                    
047400     EJECT                                                                
047500* IMS-SECTIONER                                                           
047600                                                                          
047700 IMS-GU-WDK601 SECTION.                                                   
047800                                                                          
047900     STRING 'WDK601  (IDARTNR  =' W-WDK6-IDARTNR-X ')'                    
048000             DELIMITED BY SIZE INTO SSA1                                  
048100     MOVE '  GE' TO GODK-STATUSKODER                                      
048200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
048300     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
048400     PERFORM IMS-STATUSKONTROLL                                           
048500     .                                                                    
048600     EJECT                                                                
048700 IMS-GNP-WDK611  SECTION.                                                 
048800                                                                          
048900     STRING 'WDK611  (KDSEGKEY =1)'                                       
049000             DELIMITED BY SIZE INTO SSA1                                  
049100     MOVE '  GE' TO GODK-STATUSKODER                                      
049200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
049300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
049400     PERFORM IMS-STATUSKONTROLL                                           
049500     .                                                                    
049600     SKIP2                                                                
049700 IMS-GNP-WDK621  SECTION.                                                 
049800     MOVE 'WDK621   ' TO SSA1                                             
049900     MOVE '  GE' TO GODK-STATUSKODER                                      
050000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
050100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
050200     PERFORM IMS-STATUSKONTROLL                                           
050300     .                                                                    
050400     SKIP2                                                                
050500 IMS-GU-WDF101 SECTION.                                                   
050600                                                                          
050700     STRING 'WDF101  (IDLEVNR  =' W-WDF101-IDLEVNR-X ')'                  
050800     DELIMITED BY SIZE INTO SSA1                                          
050900     MOVE '  GE' TO GODK-STATUSKODER                                      
051000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
051100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     .                                                                    
051400     SKIP3                                                                
051500 IMS-GNP-WDF102 SECTION.                                                  
051600                                                                          
051700     STRING 'WDF102  (IDLAND   =' W-WDF102-IDLAND-X ')'                   
051800     DELIMITED BY SIZE INTO SSA1                                          
051900     MOVE '  GE' TO GODK-STATUSKODER                                      
052000     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF102 SSA1                   
052100     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
052200     PERFORM IMS-STATUSKONTROLL                                           
052300     .                                                                    
052400     EJECT                                                                
052500 IMS-GU-WDK711 SECTION.                                                   
052600                                                                          
052700     STRING 'WDK701  (IDARTNR  =' W-WDK701-IDARTNR-X ')'                  
052800     DELIMITED BY SIZE INTO SSA1                                          
052900     STRING 'WDK711  (IDDC     =' W-WDK711-IDDC-X ')'                     
053000     DELIMITED BY SIZE INTO SSA2                                          
053100     MOVE '  GE' TO GODK-STATUSKODER                                      
053200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
053300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
053400     PERFORM IMS-STATUSKONTROLL                                           
053500     .                                                                    
053600     SKIP2                                                                
054000                                                                          
054100 IMS-GU-WDK711-BESPRIS SECTION.                                           
054200     STRING 'WDK701  (IDARTNR  =' W-WDK701-IDARTNR-X ')'                  
054300          DELIMITED BY SIZE INTO SSA1                                     
054400     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
054500          DELIMITED BY SIZE INTO SSA2                                     
054600     MOVE '  GE' TO GODK-STATUSKODER                                      
054700     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK711-71                     
054800          SSA1 SSA2                                                       
054900     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
055000     PERFORM IMS-STATUSKONTROLL                                           
055100     .                                                                    
055200                                                                          
055300 IMS-GNP-WDK724 SECTION.                                                  
055400     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
055500                    '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                      
055600          DELIMITED BY SIZE INTO SSA1                                     
055700     MOVE '  GE' TO GODK-STATUSKODER                                      
055800     CALL CBLTDLI USING GNP WDK72-PCB DLI-IO-WDK724 SSA1                  
055900     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
056000     PERFORM IMS-STATUSKONTROLL                                           
056100     .                                                                    
056200     EJECT                                                                
056300                                                                          
056400 IMS-GU-WDB601    SECTION.                                                
056500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
056600          DELIMITED BY SIZE INTO SSA1                                     
056700     MOVE '  GE' TO GODK-STATUSKODER                                      
056800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
056900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
057000     PERFORM IMS-STATUSKONTROLL                                           
057100     IF SEGMENT-SAKNAS                                                    
057200         MOVE SPACE TO DCS-KDDC                                           
057300     END-IF                                                               
057400     .                                                                    
057500                                                                          
057600 IMS-GNP-WDB617    SECTION.                                               
057700     MOVE 'WDB617   ' TO SSA1                                             
057800     MOVE '  GE' TO GODK-STATUSKODER                                      
057900     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
058000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
058100     PERFORM IMS-STATUSKONTROLL                                           
058200     .                                                                    
058300                                                                          
059500 IMS-STATUSKONTROLL SECTION.                                              
059600                                                                          
059700     SET STATUS-IX TO 1                                                   
059800     SEARCH GODK-STATUS                                                   
059900       AT END                                                             
060000         CALL FELLOG                                                      
060100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
060200         CONTINUE                                                         
060300     END-SEARCH                                                           
060400     .                                                                    
