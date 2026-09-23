000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3039300.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   02/01/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BACKGRUNDS MPP FÖR ATT TA EMOT TRANSAR UTAN PRIS AV WZ1.         
000900*        UPPDATERAR HÄNDBASE MED ARTIKELN.KOLLAR W335 PRIS FÖR ETT        
001000*        MARKNADSPRIS. OCH SÄNDER MAIL TILL PRISSÄTTNINGSANSVARIGA        
001100*        VIA WZ01 MODULEN TILL W0T541 USING WZ01MAIL.                     
001200*                                                                         
001300* FRÅGANDE PROGRAM.                                                       
001400*        PROGRAMMET W30392                                                
001500*        TRANSACTIONER TAS EMOT MED HJÄLP AV WZ01 M0ODULEN                
001600*    INDATA.                                                              
001700*        TRANSAKTION: W30393X                                             
001800*        MID:         W30393I1 (VIA WZ01)                                 
001900*                                                                         
002000*    UTDATA.                                                              
002100*        SÄNDNING VIA WZ01  TILL MAIL                                     
002200         MOD:         WZ01MAIL (VIA WZ01)                                 
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W3039300'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  WS-YYMMDDHHMM               PIC 9(14)   VALUE ZERO.                  
003900                                                                          
004000 77  BASPRIS-SW                  PIC X       VALUE 'J'.                   
004100     88  BASPRIS-OK                          VALUE 'J'.                   
004200     88  BASPRIS-SAKNAS                      VALUE 'N'.                   
004300                                                                          
004400 77  MAILSENT-SW                  PIC X       VALUE 'N'.                  
004500     88  MAILSENT-NOTYET                      VALUE 'N'.                  
004600     88  MAILSENT-ALREADY                     VALUE 'J'.                  
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100     EJECT                                                                
005200                                                                          
005300*01  -COPY WWPRODSL                                                       
005400                                                                          
005500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005600 01  GENERELLA-SUBPROGRAM.                                                
005700     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006200     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006900 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
007000     SKIP3                                                                
007100 01  MESSAGE-CODES.                                                       
007200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
007300     EJECT                                                                
007400 01  COUNTERS.                                                            
007500     03  TALLY-OK                PIC 9(4)    VALUE ZERO.                  
007600     03  TALLY-FEL               PIC 9(4)    VALUE ZERO.                  
007700 01  W-DADATTID                  PIC 9(14).                               
007800 01  W-MARKGROSS                 PIC Z(6)9.99 VALUE ZERO.                 
007900 01  W-IDKUNDNR                  PIC 9(6).                                
008000 01  W-KDFEL                     PIC 9(3).                                
008100 01  W-WDK6-KOLL                 PIC X      VALUE 'Y'.                    
008200*    --- AREOR FÖR KOMMUNIKTION                                           
008300 01  FILLER                      PIC X(16)   VALUE 'RECEIVE-AREA'.        
008400*01  -COPY WZ01RECV                                                       
008500     SKIP3                                                                
008600 01  RECV-DATA.                                                           
008700*03  -COPY  WZ01RESP  -PRE  MID-                                          
008800*03  -COPY  W30393I1                                                      
008900     EJECT                                                                
009000 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
009100 01  HDR-AREA.                                                            
009200*    03  -COPY WZ01REQU  -PRE HDR-                                        
009300*    03  -COPY WZ04HDR                                                    
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
009600 01  DOC-AREA.                                                            
009700     03  FILLER                  PIC X(80)   VALUE SPACE.                 
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'DOC-RUB '.            
010000 01  DOC-RUBRIK-AREA.                                                     
010100     03  FILLER                  PIC X(02)   VALUE SPACE.                 
010200     03  FILLER                  PIC X(10)                                
010300         VALUE ' DISTRICT '.                                              
010400     03  FILLER                  PIC X(10)                                
010500         VALUE ' CUSTOMER '.                                              
010600     03  FILLER                  PIC X(11)                                
010700         VALUE '    PARTNO.'.                                             
010800     03  FILLER                  PIC X(10)                                
010900         VALUE ' ERROR    '.                                              
011000     03  FILLER                  PIC X(19)                                
011100         VALUE ' SUGGESTED RETAIL= '.                                     
011200 01  FILLER                      PIC X(16)   VALUE 'DOC-LINE'.            
011300 01  DOC-LINE-AREA.                                                       
011400     03  FILLER                  PIC X(06)   VALUE SPACE.                 
011500     03  LINE-IDDISTR            PIC 9(5)    VALUE ZERO.                  
011600     03  FILLER                  PIC X(05)   VALUE SPACE.                 
011700     03  LINE-IDKUNDNR           PIC 9(5)    VALUE ZERO.                  
011800     03  FILLER                  PIC X(03)   VALUE SPACE.                 
011900     03  LINE-IDARTNR            PIC 9(9)    VALUE ZERO.                  
012000     03  FILLER                  PIC X(03)   VALUE SPACE.                 
012100     03  LINE-KDFEL              PIC 9(3)    VALUE ZERO.                  
012200     03  FILLER                  PIC X(11)   VALUE SPACE.                 
012300     03  LINE-MARKGROSS          PIC Z(6)9.99 VALUE ZERO.                 
012400                                                                          
012500 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
012600*01  -COPY WZ01SEND                                                       
012700     SKIP3                                                                
012800*************************************************                         
012900 01  SEND-DATA.                                                           
013000*03  -COPY WZ01MAIL                                                       
013100 01   KDRC-DISPLAY               PIC X(4).                                
013200     EJECT                                                                
013300*  AREA FÖR PRISRETUR VÄRDEN                                              
013400*01  -COPY W335PRIS                                                       
013500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013800     SKIP3                                                                
013900 01  NYCKLAR-TILL-DLI.                                                    
014000     03  WDR401-X.                                                        
014100         05  FILLER              PIC X(4)    VALUE '3101'.                
014200         05  W-IDDISTR           PIC 9(4)    VALUE ZERO.                  
014300         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
014400     03  W-DADATUM-X.                                                     
014500         05  W-DADATUM          PIC 9(8).                                 
014600     03  W-IDARTNR-X.                                                     
014700         05  W-IDARTNR          PIC S9(9) COMP-3 .                        
014800     03  W-KDSEGKEY-K6.                                                   
014900         05  W-KDSEGKEY-K6-X    PIC X(1)  VALUE '1'.                      
015000     SKIP2                                                                
015100*    --- STATUS-KOD FRÅN IMS                                              
015200 01  STATUS-WS                   PIC XX.                                  
015300     88  SEGMENT-FINNS                       VALUE '  '.                  
015400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015600     SKIP2                                                                
015700 01  GODK-STATUSKODER.                                                    
015800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015900     SKIP3                                                                
016000 01  SSA1                        PIC X(64).                               
016100 01  SSA2                        PIC X(64).                               
016200 01  SSA3                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800                                                                          
016900     EJECT                                                                
017000 01  FILLER         PIC X(16) VALUE 'DLI-PRISFEL'.                        
017100 01  DLI-IO-WDR401.                                                       
017200*    03  -COPY WDGX3101                                                   
017300 01  DLI-IO-WDGX3102.                                                     
017400*    03  -COPY WDGX3102                                                   
017500 01  DLI-IO-WDGX3104.                                                     
017600*    03  -COPY WDGX3104                                                   
017700 01  DLI-IO-WDGX3106.                                                     
017800*    03  -COPY WDGX3106                                                   
017900 01  DLI-IO-WDK601.                                                       
018000*    03  -COPY WDK601                                                     
018100 01  DLI-IO-WDK611.                                                       
018200*    03  -COPY WDK611                                                     
018300     EJECT                                                                
018400 LINKAGE SECTION.                                                         
018500*    -- ANVÄNDS EJ I DETTA PROGRAMMET                                     
018600 01  IO-PCB                      PIC X.                                   
018700 01  DAP-PCB                     PIC X.                                   
018800*01  -COPY W0009  -PRE SENDMAIL-                                          
018900     EJECT                                                                
019000*01  -COPY W0008  -PRE WDR4-                                              
019100     05  FILLER                  PIC X.                                   
019200     EJECT                                                                
019300*01  -COPY W0008  -PRE WDK6-                                              
019400     05  FILLER                  PIC X.                                   
019500 01  PRIS-ARTC-PCB               PIC X.                                   
019600 01  PRIS-WDK7-PCB               PIC X.                                   
019700 01  PRIS-GMTA-PCB               PIC X.                                   
019800 01  PRIS-BETA-PCB               PIC X.                                   
019900 01  PRIS-GPRIA-PCB              PIC X.                                   
020000 01  PRIS-GPRIB-PCB              PIC X.                                   
020100 01  PRIS-COST-WDK6-PCB          PIC X.                                   
020200 01  PRIS-COST-WDK7-PCB          PIC X.                                   
020300 01  PRIS-COST-WDF1-PCB          PIC X.                                   
020400 01  PRIS-COST-9305-PCB          PIC X.                                   
020500 01  PRIS-COST-WDK72-PCB         PIC X.                                   
020600 01  PRIS-COST-WDB6-PCB          PIC X.                                   
020800     EJECT                                                                
020900 PROCEDURE DIVISION  USING IO-PCB DAP-PCB SENDMAIL-PCB WDR4-PCB           
021000                                WDK6-PCB PRIS-ARTC-PCB                    
021100                                PRIS-WDK7-PCB                             
021200                                PRIS-GMTA-PCB PRIS-BETA-PCB               
021300                                PRIS-GPRIA-PCB PRIS-GPRIB-PCB             
021400                                PRIS-COST-WDK6-PCB                        
021500                                PRIS-COST-WDK7-PCB                        
021600                                PRIS-COST-WDF1-PCB                        
021700                                PRIS-COST-9305-PCB                        
021800                                PRIS-COST-WDK72-PCB                       
021900                                PRIS-COST-WDB6-PCB.                       
022100 MAIN SECTION.                                                            
022200     ENTRY 'DLITCBL' USING IO-PCB DAP-PCB SENDMAIL-PCB WDR4-PCB           
022300                                WDK6-PCB PRIS-ARTC-PCB                    
022400                                PRIS-WDK7-PCB                             
022500                                PRIS-GMTA-PCB PRIS-BETA-PCB               
022600                                PRIS-GPRIA-PCB PRIS-GPRIB-PCB             
022700                                PRIS-COST-WDK6-PCB                        
022800                                PRIS-COST-WDK7-PCB                        
022900                                PRIS-COST-WDF1-PCB                        
023000                                PRIS-COST-9305-PCB                        
023100                                PRIS-COST-WDK72-PCB                       
023200                                PRIS-COST-WDB6-PCB.                       
023400                                                                          
023500     PERFORM S01-RECV-OPEN                                                
023600     PERFORM S02-RECV-MESSAGE                                             
023700                                                                          
023800*FIX 050120                                                               
023900     IF MID-IDARTNR = 8636763                                             
024000       CONTINUE                                                           
024100     ELSE                                                                 
024200       PERFORM A-INIT                                                     
024300       PERFORM B-BEARBETA-RAD                                             
024400     END-IF                                                               
024500*FIX SLUT                                                                 
024600                                                                          
024700     PERFORM S03-RECV-CLOSE                                               
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200                                                                          
025300 A-INIT SECTION.                                                          
025400***** HÄMTA  INFO FRÅN MIDEN                                              
025500     MOVE MID-IDDISTR  TO  W-IDDISTR                                      
025600     MOVE MID-IDARTNR  TO  W-IDARTNR                                      
025700     MOVE MID-IDKUNDNR TO  W-IDKUNDNR                                     
025800     MOVE MID-KDFEL    TO  W-KDFEL                                        
025900*********************************************************                 
026000*******KOLLA MOT W335PRIS OM MARKNADSPRIS FINNS I PULS***                 
026100                                                                          
026200     MOVE 1           TO  PRIS-KDCALL                                     
026300     MOVE  'W3039300' TO  PRIS-IDPGM                                      
026400     MOVE W-IDARTNR   TO  PRIS-IDARTNR                                    
026500     MOVE W-IDDISTR   TO  PRIS-IDDISTR                                    
026600     MOVE W-IDKUNDNR  TO  PRIS-IDKUNDNR                                   
026700     MOVE '11'        TO  PRIS-IDDC                                       
026800     MOVE '4'         TO  PRIS-KDORDKL                                    
026900     MOVE 1           TO  PRIS-KVBEART                                    
027000     MOVE SPACE       TO  PRIS-FLINVEST                                   
027100     CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                      
027200                         PRIS-WDK7-PCB                                    
027300                         PRIS-GMTA-PCB PRIS-BETA-PCB                      
027400                         PRIS-GPRIA-PCB PRIS-GPRIB-PCB                    
027500                         PRIS-COST-WDK6-PCB                               
027600                         PRIS-COST-WDK7-PCB                               
027700                         PRIS-COST-WDF1-PCB                               
027800                         PRIS-COST-9305-PCB                               
027900                         PRIS-COST-WDK72-PCB                              
028000                         PRIS-COST-WDB6-PCB                               
028200                                                                          
028300     IF PRIS-PRBPRIS = 0                                                  
028400       MOVE NEJ TO BASPRIS-SW                                             
028500     END-IF                                                               
028600*****************************************************************         
028700     PERFORM IMS-GU-WDK601                                                
028800     IF SEGMENT-SAKNAS                                                    
028900      MOVE 'N' TO W-WDK6-KOLL                                             
029000     ELSE                                                                 
029100        IF ART-KDERS-UTG > 0                                              
029200           MOVE 'N' TO W-WDK6-KOLL                                        
029300        ELSE                                                              
029400           PERFORM IMS-GNP-WDK611                                         
029500           IF SEGMENT-SAKNAS                                              
029600              MOVE 'N' TO W-WDK6-KOLL                                     
029700           ELSE                                                           
029800              IF (CLAG-KDERS > 10)                                        
029900              OR CLAG-FLLSRDEL = 'N'                                      
030000                 MOVE 'N' TO W-WDK6-KOLL                                  
030100              END-IF                                                      
030200           END-IF                                                         
030300        END-IF                                                            
030400     END-IF                                                               
030500*****************************************************************         
030600                                                                          
030700     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DADATUM                         
030800     MOVE FUNCTION CURRENT-DATE(1:14) TO WS-YYMMDDHHMM                    
030900***********************************************                           
031000***GÖR KOLL AV DATUMSGMENT I HÄNDELSEBASEN* LÄGG UPP DAGSSEGMENT*         
031100     PERFORM IMS-GHN-WDGX3104                                             
031200     IF SEGMENT-SAKNAS                                                    
031300       MOVE W-DADATUM TO 3104-DADATUM                                     
031400       PERFORM IMS-ISRT-WDGX3104                                          
031500     ELSE                                                                 
031600       IF  3104-DADATUM < W-DADATUM                                       
031700         PERFORM  IMS-DLET-WDGX3104                                       
031800         MOVE W-DADATUM TO 3104-DADATUM                                   
031900         PERFORM IMS-ISRT-WDGX3104                                        
032000       END-IF                                                             
032100     END-IF                                                               
032200     .                                                                    
032300     EJECT                                                                
032400 B-BEARBETA-RAD SECTION.                                                  
032500**************                                                            
032600*KOLLA OM ARTIKELSEGMENT FINNS REDAN DENNA DAG****** OCH                  
032700*STÄLL FLAGGA DÄREFTER. (OBS EJ FÖR RENAULT OCH LAND-ROVER)               
032800     IF W-WDK6-KOLL = 'Y'                                                 
032900       MOVE ART-KDPRODSL         TO TEST-KDPRODSL                         
033000       IF KDPRODSL-VOLVO-ALL                                              
033100         IF KDPRODSL-EMB                                                  
033200           CONTINUE                                                       
033300         ELSE                                                             
033400          MOVE W-IDARTNR TO 3106-IDARTNR                                  
033500          MOVE W-KDFEL   TO 3106-KDFEL                                    
033600          PERFORM IMS-ISRT-WDGX3106                                       
033700          IF SEGMENT-FINNS-REDAN                                          
033800            MOVE JA TO MAILSENT-SW                                        
033900          END-IF                                                          
034000          IF MAILSENT-ALREADY                                             
034100            CONTINUE                                                      
034200          ELSE                                                            
034300            PERFORM IMS-GU-WDGX3102                                       
034400            PERFORM BA-SEND-MAIL                                          
034500          END-IF                                                          
034600         END-IF                                                           
034700       END-IF                                                             
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 BA-SEND-MAIL SECTION.                                                    
035200*****************************************************************         
035300*UPDATERA  PARAMETRAR*****  NY VERSION                                    
035400****************************************************                      
035500     MOVE PRIS-PRBPRIS TO W-MARKGROSS                                     
035600***********************                                                   
035700*SEND MAIL NY VERSION                                                     
035800***********************                                                   
035900     IF 3102-IDMAIL NOT = SPACE                                           
036000       MOVE SPACE TO MAIL-WZ01MAIL                                        
036100       MOVE 3102-IDMAIL          TO MAIL-IDMAIL                           
036200       STRING 'DIST '  W-IDDISTR ' CUSTNO ' MID-IDKUNDNR                  
036300              ' PARTNO ' MID-IDARTNR                                      
036400                 DELIMITED BY SIZE INTO MAIL-IDMAILTTL                    
036500       STRING 'DIST ' W-IDDISTR ' CUSTNO ' MID-IDKUNDNR                   
036600              ' PARTNO ' MID-IDARTNR ' ERROR= ' MID-KDFEL                 
036700                     ' SUGGESTED RETAIL= ' W-MARKGROSS                    
036800                        DELIMITED BY SIZE INTO MAIL-TEMAIL (1)            
036900       MOVE 1  TO                  MAIL-KVMAILLN                          
037000       PERFORM S04-SEND-OPEN                                              
037100       PERFORM S05-SEND-MESSAGE                                           
037200       PERFORM S06-SEND-CLOSE                                             
037300     END-IF                                                               
037400     MOVE SPACE TO MAIL-WZ01MAIL                                          
037500                                                                          
037600     IF BASPRIS-SAKNAS                                                    
037700       MOVE ART-KDPRODSL         TO TEST-KDPRODSL                         
037800       IF KDPRODSL-VOLVO-ALL                                              
037900         PERFORM S04-SKICKA-OPEN                                          
038000         PERFORM S04-SKAPA-HEADER                                         
038100         PERFORM S04-PUT-DAP-HEADER                                       
038200         MOVE DOC-RUBRIK-AREA TO DOC-AREA                                 
038300         PERFORM S04-PUT-DOC                                              
038400         MOVE W-IDDISTR     TO LINE-IDDISTR                               
038500         MOVE MID-IDKUNDNR  TO LINE-IDKUNDNR                              
038600         MOVE MID-IDARTNR   TO LINE-IDARTNR                               
038700         MOVE MID-KDFEL     TO LINE-KDFEL                                 
038800         MOVE W-MARKGROSS   TO LINE-MARKGROSS                             
038900         MOVE DOC-LINE-AREA     TO DOC-AREA                               
039000         PERFORM S04-PUT-DOC                                              
039100         PERFORM S04-SKICKA-CLOSE                                         
039200*                                                                         
039300       END-IF                                                             
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700*********************************                                         
039800 S01-RECV-OPEN SECTION.                                                   
039900     MOVE 'OPEN' TO RECV-KDFUNC                                           
040000     MOVE 'CARPARTS.PULS.PRMAIL' TO RECV-ADDISPABS                        
040100                                                                          
040200     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
040300                                   RECV-OPEN-AREA                         
040400********              ...FELHANTERING...                                  
040500     IF RECV-KDRC > 0                                                     
040600      MOVE RECV-KDRC TO KDRC-DISPLAY                                      
040700      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                      
040800      DELIMITED BY SIZE INTO FELTEXT                                      
040900      DISPLAY FELTEXT                                                     
041000      CALL FELLOG                                                         
041100     END-IF                                                               
041200     .                                                                    
041300     EJECT                                                                
041400 S02-RECV-MESSAGE SECTION.                                                
041500                                                                          
041600     MOVE 'GET' TO RECV-KDFUNC                                            
041700     MOVE LENGTH OF RECV-DATA TO RECV-KVDLEN                              
041800     CALL WZ01RECV USING RECV-CONTROL-AREA                                
041900                         RECV-KVDLEN                                      
042000                         RECV-DATA                                        
042100**FELHANTERING...                                                         
042200     IF RECV-KDRC > 1                                                     
042300       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
042400       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
042500       DELIMITED BY SIZE INTO FELTEXT                                     
042600       DISPLAY FELTEXT                                                    
042700       CALL FELLOG                                                        
042800     END-IF                                                               
042900     .                                                                    
043000     EJECT                                                                
043100 S03-RECV-CLOSE SECTION.                                                  
043200                                                                          
043300     MOVE 'CLOSE' TO RECV-KDFUNC                                          
043400     CALL WZ01RECV USING RECV-CONTROL-AREA                                
043500**FELHANTERING...                                                         
043600     IF RECV-KDRC > 0                                                     
043700      MOVE RECV-KDRC TO KDRC-DISPLAY                                      
043800      STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                     
043900       DELIMITED BY SIZE INTO FELTEXT                                     
044000       DISPLAY FELTEXT                                                    
044100       CALL FELLOG                                                        
044200     END-IF                                                               
044300     .                                                                    
044400     EJECT                                                                
044500 S04-SEND-OPEN SECTION.                                                   
044600     MOVE 'OPEN'                        TO SEND-KDFUNC                    
044700     MOVE 'CARPARTS.PULS.SENDMAIL'      TO SEND-ADDISPABS                 
044800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
044900                         SEND-OPEN-AREA                                   
045000     IF SEND-KDRC  > 0                                                    
045100        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
045200        STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                    
045300        DELIMITED BY SIZE INTO FELTEXT                                    
045400        DISPLAY FELTEXT                                                   
045500        CALL FELLOG                                                       
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900 S05-SEND-MESSAGE SECTION.                                                
046000                                                                          
046100     MOVE 'PUT'                           TO SEND-KDFUNC                  
046200     MOVE LENGTH OF SEND-DATA             TO SEND-KVDLEN                  
046300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
046400                         SEND-KVDLEN                                      
046500                         SEND-DATA                                        
046600                                                                          
046700     IF SEND-KDRC  > 0                                                    
046800        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
046900        STRING 'WZ01SEND PUT  ERROR RC= ' KDRC-DISPLAY                    
047000        DELIMITED BY SIZE INTO FELTEXT                                    
047100        DISPLAY FELTEXT                                                   
047200        CALL FELLOG                                                       
047300     END-IF                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 S06-SEND-CLOSE SECTION.                                                  
047700                                                                          
047800     MOVE 'CLOSE' TO SEND-KDFUNC                                          
047900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
048000                                                                          
048100     IF SEND-KDRC  > 0                                                    
048200        MOVE SEND-KDRC TO KDRC-DISPLAY                                    
048300        STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                   
048400        DELIMITED BY SIZE INTO FELTEXT                                    
048500        DISPLAY FELTEXT                                                   
048600        CALL FELLOG                                                       
048700     END-IF                                                               
048800     .                                                                    
048900     EJECT                                                                
049000 S04-SKICKA-OPEN SECTION.                                                 
049100                                                                          
049200     MOVE 'OPEN'                     TO SEND-KDFUNC                       
049300     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
049400     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
049500                                                                          
049600     IF SEND-KDRC > 0                                                     
049700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
049800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
049900       DELIMITED BY SIZE INTO FELTEXT                                     
050000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
050100     END-IF                                                               
050200     .                                                                    
050300     SKIP3                                                                
050400                                                                          
050500 S04-SKICKA-CLOSE SECTION.                                                
050600                                                                          
050700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
050800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
050900                                                                          
051000     IF SEND-KDRC > 0                                                     
051100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
051200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
051300       DELIMITED BY SIZE INTO FELTEXT                                     
051400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
051500     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051800 S04-SKAPA-HEADER   SECTION.                                              
051900     MOVE 001             TO HDR-REQU-IDMSGVER                            
052000     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
052100     MOVE IDPGM           TO HDR-REQU-IDUSER                              
052200                                                                          
052300     MOVE 'W3039300'      TO HDR-IDOUTTYPE                                
052400     MOVE SPACE           TO HDR-IDOUTREC                                 
052500     MOVE 'W3039300'      TO HDR-IDOUTREC(1:8)                            
052600     MOVE WS-YYMMDDHHMM   TO HDR-IDLIST                                   
052700     .                                                                    
052800 S04-PUT-DAP-HEADER SECTION.                                              
052900     MOVE 'PUT'                           TO SEND-KDFUNC                  
053000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
053100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
053200                         SEND-KVDLEN                                      
053300                         HDR-AREA                                         
053400     IF SEND-KDRC > ZERO                                                  
053500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
053600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
053700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
053800       CALL ABEND USING  RKOD-ABEND-MED-DUMP                              
053900     END-IF                                                               
054000     .                                                                    
054100 S04-PUT-DOC      SECTION.                                                
054200     MOVE 'PUT'                           TO SEND-KDFUNC                  
054300     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
054400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
054500                         SEND-KVDLEN                                      
054600                         DOC-AREA                                         
054700     IF SEND-KDRC > ZERO                                                  
054800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
054900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
055000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
055100       CALL ABEND USING  RKOD-ABEND-MED-DUMP                              
055200     END-IF                                                               
055300     .                                                                    
055400* --- IMS SEKTIONER ---                                                   
055500     SKIP3                                                                
055600 IMS-GHN-WDGX3104 SECTION.                                                
055700                                                                          
055800     STRING 'WDR401  (WDGXKEY  =' WDR401-X   ')'                          
055900          DELIMITED BY SIZE INTO SSA1                                     
056000     MOVE  'WDGX3104' TO SSA2                                             
056100     MOVE '  GE' TO GODK-STATUSKODER                                      
056200     CALL CBLTDLI USING GHN WDR4-PCB DLI-IO-WDGX3104 SSA1 SSA2            
056300     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
056400     PERFORM IMS-STATUSKONTROLL                                           
056500     .                                                                    
056600     SKIP3                                                                
056700 IMS-DLET-WDGX3104 SECTION.                                               
056800                                                                          
056900     MOVE '  ' TO GODK-STATUSKODER                                        
057000     CALL CBLTDLI USING DLET WDR4-PCB DLI-IO-WDGX3104                     
057100     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
057200     PERFORM IMS-STATUSKONTROLL                                           
057300     .                                                                    
057400     EJECT                                                                
057500 IMS-ISRT-WDGX3104 SECTION.                                               
057600                                                                          
057700     STRING 'WDR401  (WDGXKEY  =' WDR401-X   ')'                          
057800          DELIMITED BY SIZE INTO SSA1                                     
057900     MOVE   'WDGX3104 ' TO SSA2                                           
058000     MOVE '  ' TO GODK-STATUSKODER                                        
058100     CALL CBLTDLI USING ISRT WDR4-PCB DLI-IO-WDGX3104 SSA1 SSA2           
058200     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
058300     PERFORM IMS-STATUSKONTROLL                                           
058400     .                                                                    
058500     SKIP3                                                                
058600 IMS-ISRT-WDGX3106 SECTION.                                               
058700                                                                          
058800     STRING 'WDR401  (WDGXKEY  =' WDR401-X   ')'                          
058900          DELIMITED BY SIZE INTO SSA1                                     
059000     STRING 'WDGX3104(DADATUM  =' W-DADATUM-X ')'                         
059100          DELIMITED BY SIZE INTO SSA2                                     
059200     MOVE 'WDGX3106' TO   SSA3                                            
059300     MOVE '  II' TO GODK-STATUSKODER                                      
059400     CALL CBLTDLI USING ISRT WDR4-PCB DLI-IO-WDGX3106                     
059500                                     SSA1 SSA2 SSA3                       
059600     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     .                                                                    
059900     SKIP3                                                                
060000 IMS-GU-WDGX3102 SECTION.                                                 
060100                                                                          
060200     STRING 'WDR401  (WDGXKEY  =' WDR401-X   ')'                          
060300          DELIMITED BY SIZE INTO SSA1                                     
060400     MOVE   'WDGX3102' TO SSA2                                            
060500     MOVE '  ' TO GODK-STATUSKODER                                        
060600     CALL CBLTDLI USING GHU WDR4-PCB DLI-IO-WDGX3102                      
060700                                     SSA1 SSA2                            
060800     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
060900     PERFORM IMS-STATUSKONTROLL                                           
061000     .                                                                    
061100     SKIP3                                                                
061200 IMS-GU-WDK601 SECTION.                                                   
061300                                                                          
061400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
061500          DELIMITED BY SIZE INTO SSA1                                     
061600     MOVE '  GE' TO GODK-STATUSKODER                                      
061700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
061800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
061900     PERFORM IMS-STATUSKONTROLL                                           
062000     .                                                                    
062100     SKIP3                                                                
062200 IMS-GNP-WDK611 SECTION.                                                  
062300                                                                          
062400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-X ')'                     
062500          DELIMITED BY SIZE INTO SSA1                                     
062600     MOVE '  GE' TO GODK-STATUSKODER                                      
062700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1                    
062800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
062900     PERFORM IMS-STATUSKONTROLL                                           
063000     .                                                                    
063100     EJECT                                                                
063200 IMS-STATUSKONTROLL SECTION.                                              
063300                                                                          
063400     SET STATUS-IX TO 1                                                   
063500     SEARCH GODK-STATUS                                                   
063600       AT END                                                             
063700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
063800         DELIMITED BY SIZE INTO FELTEXT                                   
063900         CALL FELLOG                                                      
064000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
064100         CONTINUE                                                         
064200     END-SEARCH                                                           
064300     .                                                                    
