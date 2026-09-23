000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL013500.                                                
000300 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000400 DATE-WRITTEN.   2004/11/03.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        WL013500 PROGRAM IS A REPLICA OF W6021400 PROGRAM                
000900*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001000*                                                                         
001100*        UPPDATERING AV ARTIKELHISTORIK KVALITET                          
001200*        FÖR SDC/NDC.                                                     
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR W6D2                                       
001500*        PROGRAMMET LÄSER      WDK7                                       
001600*                                                                         
001700* ADDRESS: 'CARPARTS.LDC.PARTHISTQUALITY'                                 
001800*                                                                         
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: WL0135U                                             
002200*        REQUEST:     WZ01REQU                                            
002300*                     WL0135I1                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        RESPONSE:    WZ01RESP                                            
002700*                     WL0135O1                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'WL013500'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  WS-IMS                      PIC X(80) VALUE SPACE.                   
004300 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
004400                                                                          
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
004900 77  WS-IDKVAINF                 PIC X(2)    VALUE SPACE.                 
005000 77  WS-TIREGDAT                 PIC X(6)    VALUE SPACE.                 
005100 77  WS-TISTADAT                 PIC 9(6)    VALUE ZERO.                  
005200 77  WS-TISTODAT                 PIC 9(6)    VALUE ZERO.                  
005300 01  W1-DAREGDAT                 PIC 9(08).                               
005400 77  FL-INTERN                   PIC X       VALUE 'N'.                   
005500                                                                          
005600 77  WS-IDELMT-ERROR             PIC X(16) VALUE SPACE.                   
005700 77  WS-IDMSG-ERROR              PIC X(03) VALUE SPACE.                   
005800 77  WS-IDMSG-INFO               PIC X(03) VALUE SPACE.                   
005900 77  WS-SPAR-IDDC2               PIC X(2)  VALUE SPACE.                   
006000                                                                          
006100 01  DAGENS-DATUM                PIC 9(6) VALUE ZERO.                     
006200                                                                          
006300                                                                          
006400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006500                                                                          
006600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006700     88  INDATA-OK                           VALUE 'J'.                   
006800     88  INDATA-FEL                          VALUE 'N'.                   
006900                                                                          
007000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007100     88  NYCKLAR-OK                          VALUE 'J'.                   
007200     88  NYCKLAR-FEL                         VALUE 'N'.                   
007300                                                                          
007400 77  NYTT-ARTNR-SW               PIC X       VALUE 'N'.                   
007500     88  NYTT-ARTNR                          VALUE 'J'.                   
007600     88  GAMMALT-ARTNR                       VALUE 'N'.                   
007700                                                                          
007800 77  POST-SW                     PIC X       VALUE 'N'.                   
007900     88  POST-FINNS                          VALUE 'J'.                   
008000     88  POST-SAKNAS                         VALUE 'N'.                   
008100                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  EGEN-MID                            VALUE '6214'.                
008400     88  GODK-MID                            VALUE '6211' '6212'          
008500                                                   '6213' '6214'          
008600                                                   '6215' '6216'          
008700                                                   '6217' '6218'          
008800                                                   '6219'.                
008900     88  HELP-MID                            VALUE '0551'.                
009000 77  FL-LAES                     PIC X       VALUE 'J'.                   
009100                                                                          
009200     EJECT                                                                
009300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009600     EJECT                                                                
009700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009800 01  GENERELLA-SUBPROGRAM.                                                
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010200     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010400     EJECT                                                                
010500 01  MESSAGE-CODES.                                                       
010600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011100     03  ERR-MISSING-KEY         PIC X(3)    VALUE '005'.                 
011200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011400     03  UPDATE-NOT-ALLOWED      PIC X(3)    VALUE '007'.                 
011500     EJECT                                                                
011600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'WZ01SUB '.            
011900*01  -COPY WZ01SUB                                                        
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
012200*01  -COPY WZ01SEND                                                       
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012500 01  REQU-AREA.                                                           
012600*    03  -COPY WZ01REQU                                                   
012700*    03  -COPY WL0135I1                                                   
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
013000 01  RESP-AREA.                                                           
013100*    03  -COPY WZ01RESP                                                   
013200*    03  -COPY WL0135O1                                                   
013300     EJECT                                                                
013400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500*                                                                         
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013800     SKIP3                                                                
013900 01  NYCKLAR-TILL-DLI.                                                    
014000     03  W-IDARTNR-X.                                                     
014100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014200     03  W-IDDC-X.                                                        
014300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
014400     03  W-W6D211KY-X.                                                    
014500         05  W-DAREGDAT-9KOMPL   PIC 9(08)   VALUE ZERO.                  
014600         05  W-TIKLOCK-9KOMPL    PIC S9(09)  VALUE ZERO COMP-3.           
014700     03  W-IDKVAINF-X.                                                    
014800         05  W-IDKVAINF          PIC  9(02)  VALUE ZERO.                  
014900     03  W-IDKVAINF-MIN-X.                                                
015000         05  W-IDKVAINF-MIN      PIC  9(02)  VALUE ZERO.                  
015100     03  W-IDKVAINF-MAX-X.                                                
015200         05  W-IDKVAINF-MAX      PIC  9(02)  VALUE ZERO.                  
015300     03  W-KDARBTYP-X.                                                    
015400         05  W-KDARBTYP          PIC X(8)    VALUE 'QUAL    '.            
015500     03  W-IDPERSON-X.                                                    
015600         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
015700     03  W-IDDC-B6-X.                                                     
015800         05 W-IDDC-B6            PIC X(2).                                
015900     03  W-WDGXKEY-6331-X.                                                
016000         05  W-IDHTYP            PIC X(4)    VALUE '6331'.                
016100         05  W-IDDC-6331         PIC X(2)    VALUE '11'.                  
016200         05  W-6331-LOW          PIC X(24)   VALUE LOW-VALUE.             
016300                                                                          
016400     03  W-WDGXKEY-6332-X.                                                
016500         05  W-IDDC-6332         PIC X(2)    VALUE SPACE.                 
016600                                                                          
016700     03  W-WDGXKEY-6334-X.                                                
016800         05  W-IDDC-6334         PIC X(2)    VALUE SPACE.                 
016900                                                                          
017000     SKIP2                                                                
017100*    --- STATUS-KOD FRÅN IMS                                              
017200 01  STATUS-WS                   PIC XX.                                  
017300     88  SEGMENT-FINNS                       VALUE '  '.                  
017400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017600     SKIP2                                                                
017700 01  GODK-STATUSKODER.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900     SKIP3                                                                
018000 01  SSA1                        PIC X(64).                               
018100 01  SSA2                        PIC X(64).                               
018200 01  SSA3                        PIC X(64).                               
018300     EJECT                                                                
018400*    --- IMS FUNKTIONSKODER                                               
018500*01  -COPY W0003                                                          
018600     EJECT                                                                
018700*    ---  DLI INPUT-OUTPUT AREA                                           
018800                                                                          
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D2'.                        
019000 01  DLI-IO-W6D2-AREA.                                                    
019100     03  DLI-IO-W6D2 PIC X(1200).                                         
019200     03  IO-W6D201 REDEFINES DLI-IO-W6D2.                                 
019300*        05  -COPY W6D201  -PRE KVAH-                                     
019400     EJECT                                                                
019500     03  IO-W6D211 REDEFINES DLI-IO-W6D2.                                 
019600*          05  -COPY W6D211                                               
019700     EJECT                                                                
019800 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D221'.                      
019900 01  DLI-IO-W6D221.                                                       
020000*          05  -COPY W6D221                                               
020100     EJECT                                                                
020200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6'.                        
020300 01  DLI-IO-WDK6-AREA.                                                    
020400     03  DLI-IO-WDK6 PIC X(900).                                          
020500     03  IO-WDK601 REDEFINES DLI-IO-WDK6.                                 
020600*          05  -COPY WDK601                                               
020700     EJECT                                                                
020800     03  IO-WDK611 REDEFINES DLI-IO-WDK6.                                 
020900*          05  -COPY WDK611                                               
021000     EJECT                                                                
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7'.                        
021200 01  DLI-IO-WDK7-AREA.                                                    
021300     03  DLI-IO-WDK7 PIC X(300).                                          
021400     03  IO-WDK701 REDEFINES DLI-IO-WDK7.                                 
021500*          05  -COPY WDK701                                               
021600     EJECT                                                                
021700     03  IO-WDK711 REDEFINES DLI-IO-WDK7.                                 
021800*          05  -COPY WDK711                                               
021900     EJECT                                                                
022000 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-P311'.          
022100 01  DLI-IO-P311.                                                         
022200*    03  -COPY WDP311                                                     
022300     EJECT                                                                
022400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
022500 01   DLI-IO-AREA-B601.                                                   
022600*     03  -COPY WDB601                                                    
022700     EJECT                                                                
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG6331'.                     
022900 01  DLI-IO-WDGX6331.                                                     
023000*    03  -COPY WDGX6331.                                                  
023100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG6332'.                     
023200 01  DLI-IO-WDGX6332.                                                     
023300*    03  -COPY WDGX6332.                                                  
023400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG6334'.                     
023500 01  DLI-IO-WDGX6334.                                                     
023600*    03  -COPY WDGX6334.                                                  
023700     EJECT                                                                
023800 LINKAGE SECTION.                                                         
023900 01  MSG-PCB                     PIC X.                                   
024000*01  -COPY W0008   -PRE W6D2A-                                            
024100     05  FILLER                  PIC X.                                   
024200                                                                          
024300*01  -COPY W0008   -PRE W6D2B-                                            
024400     05  FILLER                  PIC X.                                   
024500                                                                          
024600*01  -COPY W0008   -PRE WDK6-                                             
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900*01  -COPY W0008   -PRE WDK7-                                             
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025200*01  -COPY W0008   -PRE WDP3-                                             
025300     05  FILLER                  PIC X.                                   
025400     EJECT                                                                
025500*01  -COPY W0008   -PRE WDB6-                                             
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025800*01  -COPY W0008   -PRE WDR2-                                             
025900     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026100*01  -COPY W0008   -PRE WDR2ALT-                                          
026200     05  FILLER                  PIC X.                                   
026300     EJECT                                                                
026400 PROCEDURE DIVISION  USING MSG-PCB  W6D2A-PCB W6D2B-PCB                   
026500                                    WDK6-PCB WDK7-PCB WDP3-PCB            
026600                                    WDB6-PCB WDR2-PCB WDR2ALT-PCB.        
026700     ENTRY 'DLITCBL' USING MSG-PCB  W6D2A-PCB W6D2B-PCB                   
026800                                    WDK6-PCB WDK7-PCB WDP3-PCB            
026900                                    WDB6-PCB WDR2-PCB WDR2ALT-PCB.        
027000                                                                          
027100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
027200     IF SUB-KDRC = 0                                                      
027300       PERFORM A-INIT                                                     
027400       PERFORM B-KOLLA-NYCKLAR                                            
027500       IF NYCKLAR-OK                                                      
027600         IF REQU-KDPGMACT = 'E'                                           
027700           PERFORM G-KOLLA-INPUT                                          
027800           IF INDATA-OK                                                   
027900             PERFORM H-UPPDATERA                                          
028000           END-IF                                                         
028100         END-IF                                                           
028200         IF INDATA-OK                                                     
028300           PERFORM F-LAES-VISA-INFO                                       
028400         END-IF                                                           
028500       END-IF                                                             
028600       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
028700       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
028800       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
028900       IF WS-IDMSG-INFO  NOT = SPACE                                      
029000          MOVE SPACE           TO RESP-IDMSG-ERROR                        
029100          MOVE SPACE           TO RESP-IDELMT-ERROR                       
029200       ELSE                                                               
029300         IF WS-IDMSG-ERROR NOT = SPACE                                    
029400           IF REQU-KDPGMACT = 'E' OR 'N' OR 'F'                           
029500            MOVE ALL '+' TO RESP-WL0135O1                                 
029600            MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                     
029700            MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                    
029800            MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                      
029900            MOVE  001             TO RESP-IDMSGVER                        
030000           ELSE                                                           
030100             IF REQU-KDPGMACT = 'S'                                       
030200               MOVE ALL '+'          TO RESP-WL0135O1                     
030300               MOVE SPACE            TO RESP-TABELLRAD                    
030400               MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                  
030500               MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                 
030600               MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                   
030700               MOVE  001             TO RESP-IDMSGVER                     
030800             END-IF                                                       
030900           END-IF                                                         
031000         END-IF                                                           
031100       END-IF                                                             
031200       PERFORM S02-RETURN-RESPONSE                                        
031300     END-IF                                                               
031400                                                                          
031500     MOVE ZERO TO RETURN-CODE                                             
031600     GOBACK                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 A-INIT SECTION.                                                          
032000                                                                          
032100     MOVE ALL '+' TO RESP-AREA                                            
032200     MOVE SPACE   TO RESP-IDMSG-INFO                                      
032300                     RESP-IDMSG-ERROR                                     
032400                     RESP-IDELMT-ERROR                                    
032500     MOVE SPACE   TO RESP-WL0135O1                                        
032600                                                                          
032700                                                                          
032800     MOVE 001     TO RESP-IDMSGVER                                        
032900                                                                          
033000     MOVE FUNCTION CURRENT-DATE(3:6) TO DAGENS-DATUM                      
033100     .                                                                    
033200     EJECT                                                                
033300 B-KOLLA-NYCKLAR SECTION.                                                 
033400                                                                          
033500     MOVE JA TO NYCKLAR-SW                                                
033600                                                                          
033700*    -- KONTROLL AV IDARTNR                                               
033800                                                                          
033900     IF REQU-IDARTNR-KEY NOT = ALL '+'                                    
034000       MOVE JA          TO NYTT-ARTNR-SW                                  
034100     END-IF                                                               
034200     INSPECT REQU-IDARTNR-KEY REPLACING LEADING SPACE BY ZERO             
034300     IF REQU-IDARTNR-KEY NUMERIC                                          
034400       MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                 
034500     ELSE                                                                 
034600       MOVE NEJ TO NYCKLAR-SW                                             
034700       MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR                           
034800       MOVE '024'          TO RESP-IDMSG-ERROR                            
034900     END-IF                                                               
035000                                                                          
035100*    -  KONTROLL AV IDKVAINF                                              
035200                                                                          
035300     IF (REQU-IDKVAINF-KEY = ALL '+' OR SPACE) OR                         
035400        (REQU-KDPGMACT      = 'F')                                        
035500       IF NYTT-ARTNR                                                      
035600*        MOVE ZERO            TO WS-IDKVAINF                              
035700         MOVE '99'            TO WS-IDKVAINF                              
035800       END-IF                                                             
035900     ELSE                                                                 
036000       MOVE REQU-IDKVAINF-KEY   TO WS-IDKVAINF                            
036100     END-IF                                                               
036200     IF REQU-IDKVAINF-UPPD = '99'                                         
036300       MOVE '99' TO WS-IDKVAINF                                           
036400     END-IF                                                               
036500     INSPECT WS-IDKVAINF REPLACING LEADING SPACE BY ZERO                  
036600                                                                          
036700*    -- KONTROLL AV TIREGDAT                                              
036800                                                                          
036900     IF (REQU-TIREGDAT-KEY = ALL '+' OR SPACE) OR                         
037000        (REQU-KDPGMACT      = 'F')                                        
037100       IF NYTT-ARTNR                                                      
037200*        MOVE ZERO            TO WS-TIREGDAT                              
037300         MOVE 999999          TO WS-TIREGDAT                              
037400       ELSE                                                               
037500         IF REQU-IDKVAINF-KEY = ALL '+'                                   
037600           CONTINUE                                                       
037700         ELSE                                                             
037800           MOVE ZERO            TO WS-TIREGDAT                            
037900         END-IF                                                           
038000       END-IF                                                             
038100     ELSE                                                                 
038200       MOVE REQU-TIREGDAT-KEY   TO WS-TIREGDAT                            
038300     END-IF                                                               
038400     INSPECT WS-TIREGDAT REPLACING LEADING SPACE BY ZERO                  
038500     IF WS-TIREGDAT NUMERIC                                               
038600       CONTINUE                                                           
038700     ELSE                                                                 
038800       MOVE NEJ TO NYCKLAR-SW                                             
038900       MOVE 'TIREGDAT'     TO RESP-IDELMT-ERROR                           
039000       MOVE '024'          TO RESP-IDMSG-ERROR                            
039100     END-IF                                                               
039200                                                                          
039300** KOLL AV IDDC OCH IDDC2                                                 
039400                                                                          
039500     IF REQU-IDDC2-KEY NOT = ALL '+'                                      
039600       MOVE REQU-IDDC2-KEY TO W-IDDC                                      
039700                              RESP-IDDC2-KEY                              
039800                              DCQ-IDDC                                    
039900                              WS-SPAR-IDDC2                               
040000                              W-IDDC-B6                                   
040100*** LÄA WDB6 KOLLA OM DC FINNS                                            
040200       PERFORM IMS-GU-WDB601                                              
040300       IF SEGMENT-SAKNAS                                                  
040400         MOVE NEJ          TO NYCKLAR-SW                                  
040500         MOVE 'IDDC'       TO RESP-IDELMT-ERROR                           
040600         MOVE '023'        TO RESP-IDMSG-ERROR                            
040700       ELSE                                                               
040800         IF REQU-IDDC2-KEY NOT = REQU-IDDC-KEY                            
040900           PERFORM BA-KOLLA-DC-TRAD                                       
041000         END-IF                                                           
041100       END-IF                                                             
041200     ELSE                                                                 
041300       MOVE REQU-IDDC-KEY  TO RESP-IDDC-KEY                               
041400                              W-IDDC                                      
041500                              DCQ-IDDC                                    
041600     END-IF                                                               
041700     IF NYCKLAR-OK                                                        
041800       MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                          
041900       MOVE WS-IDKVAINF  TO W-IDKVAINF                                    
042000                            RESP-IDKVAINF-KEY                             
042100       IF NYCKLAR-OK                                                      
042200         MOVE WS-TIREGDAT  TO W1-DAREGDAT                                 
042300                              RESP-TIREGDAT-KEY                           
042400         IF WS-TIREGDAT NOT = ZERO                                        
042500           IF WS-TIREGDAT < 500000                                        
042600             MOVE 20       TO W1-DAREGDAT (1:2)                           
042700           ELSE                                                           
042800             IF WS-TIREGDAT < 999999                                      
042900               MOVE 19     TO W1-DAREGDAT (1:2)                           
043000             ELSE                                                         
043100               MOVE 99999999 TO W1-DAREGDAT                               
043200             END-IF                                                       
043300           END-IF                                                         
043400         END-IF                                                           
043500         COMPUTE W-DAREGDAT-9KOMPL = 99999999 - W1-DAREGDAT               
043600       END-IF                                                             
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 BA-KOLLA-DC-TRAD SECTION.                                                
044100     MOVE NEJ TO NYCKLAR-SW                                               
044200     MOVE NEJ TO POST-SW                                                  
044300**   KOLLA OM PROF-IDDC OCH IDDC2 FINNS I SAMMA ROT                       
044400     MOVE REQU-IDDC-KEY            TO W-IDDC-6332                         
044500     MOVE REQU-IDDC2-KEY           TO W-IDDC-6334                         
044600     PERFORM IMS-GET-WDGX6334-UNIK                                        
044700     IF SEGMENT-SAKNAS                                                    
044800       MOVE NEJ                    TO NYCKLAR-SW                          
044900     ELSE                                                                 
045000       MOVE JA                     TO NYCKLAR-SW                          
045100     END-IF                                                               
045200                                                                          
045300     IF NYCKLAR-FEL                                                       
045400**     KOLLA OM IDDC2 ÄR EN LEVEL2                                        
045500       MOVE REQU-IDDC2-KEY         TO W-IDDC-6332                         
045600       PERFORM IMS-GHU-WDGX6332                                           
045700       IF SEGMENT-SAKNAS                                                  
045800         MOVE NEJ                  TO NYCKLAR-SW                          
045900       ELSE                                                               
046000         MOVE JA                   TO POST-SW                             
046100         IF REQU-IDUSER = 6332-IDUSER(1) OR                               
046200            REQU-IDUSER =  6332-IDUSER(2) OR                              
046300            REQU-IDUSER =  6332-IDUSER(3) OR                              
046400            REQU-IDUSER =  6332-IDUSER(4)                                 
046500           MOVE JA         TO NYCKLAR-SW                                  
046600         END-IF                                                           
046700       END-IF                                                             
046800     END-IF                                                               
046900                                                                          
047000     IF NYCKLAR-FEL AND POST-SAKNAS                                       
047100       MOVE REQU-IDDC2-KEY         TO W-IDDC-6334                         
047200**     IDDC2 ÄR LEVEL3 , HITTA LEVEL2 TILL IDDC2                          
047300       PERFORM IMS-GET-WDGX6331                                           
047400       IF SEGMENT-FINNS                                                   
047500         PERFORM IMS-GNP-WDGX6332                                         
047600         PERFORM UNTIL SEGMENT-SAKNAS OR POST-FINNS                       
047700           IF SEGMENT-FINNS                                               
047800             MOVE 6332-IDDC        TO W-IDDC-6332                         
047900             PERFORM IMS-GET-WDGX6334-UNIK                                
048000             IF SEGMENT-FINNS                                             
048100               MOVE JA         TO POST-SW                                 
048200**             KOLLA IDUSER                                               
048300               IF REQU-IDUSER = 6332-IDUSER(1) OR                         
048400                 REQU-IDUSER =  6332-IDUSER(2) OR                         
048500                 REQU-IDUSER =  6332-IDUSER(3) OR                         
048600                 REQU-IDUSER =  6332-IDUSER(4)                            
048700                   MOVE JA         TO NYCKLAR-SW                          
048800               END-IF                                                     
048900             ELSE                                                         
049000               PERFORM IMS-GNP-WDGX6332                                   
049100             END-IF                                                       
049200           END-IF                                                         
049300         END-PERFORM                                                      
049400       END-IF                                                             
049500     END-IF                                                               
049600     IF NYCKLAR-FEL                                                       
049700       MOVE 'IDDC'       TO RESP-IDELMT-ERROR                             
049800       MOVE '023'        TO RESP-IDMSG-ERROR                              
049900     END-IF                                                               
050000     .                                                                    
050100     EJECT                                                                
050200 F-LAES-VISA-INFO SECTION.                                                
050300                                                                          
050400     PERFORM IMS-GU-WDK611                                                
050500                                                                          
050600     IF SEGMENT-FINNS                                                     
050700        PERFORM IMS-GU-WDK711                                             
050800     END-IF                                                               
050900                                                                          
051000     IF SEGMENT-FINNS                                                     
051100       PERFORM IMS-GU-W6D201                                              
051200                                                                          
051300       IF SEGMENT-FINNS                                                   
051400         MOVE LOW-VALUE            TO W-IDKVAINF-MIN-X                    
051500         MOVE HIGH-VALUE           TO W-IDKVAINF-MAX-X                    
051600         IF WS-IDKVAINF > ZERO                                            
051700           MOVE W-IDKVAINF         TO W-IDKVAINF-MAX                      
051800           MOVE ZERO               TO W-DAREGDAT-9KOMPL                   
051900         END-IF                                                           
052000                                                                          
052100         PERFORM FA-LAES-GRUNDDATA                                        
052200       ELSE                                                               
052300         MOVE 'QUALITY'       TO RESP-IDELMT-ERROR                        
052400         MOVE '286'           TO RESP-IDMSG-ERROR                         
052500       END-IF                                                             
052600     ELSE                                                                 
052700       MOVE '025'           TO RESP-IDMSG-ERROR                           
052800       MOVE 'IDARTNR'       TO RESP-IDELMT-ERROR                          
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 FA-LAES-GRUNDDATA SECTION.                                               
053300                                                                          
053400     PERFORM IMS-GNP-W6D211                                               
053500                                                                          
053600     IF SEGMENT-FINNS                                                     
053700       IF REQU-KDPGMACT = 'N'                                             
053800         PERFORM IMS-GNP-W6D211-NEXT                                      
053900       END-IF                                                             
054000                                                                          
054100       IF SEGMENT-FINNS                                                   
054200         MOVE INFO-DAREGDAT-9KOMPL      TO W-DAREGDAT-9KOMPL              
054300         MOVE INFO-TIKLOCK-9KOMPL       TO W-TIKLOCK-9KOMPL               
054400                                                                          
054500         COMPUTE W1-DAREGDAT = 99999999 - INFO-DAREGDAT-9KOMPL            
054600*        MOVE W1-DAREGDAT (3:6)  TO RESP-TIREGDAT                         
054700*                                   RESP-TIREGDAT-KEY                     
054800         MOVE INFO-TIREGDAT      TO RESP-TIREGDAT                         
054900                                    RESP-TIREGDAT-KEY                     
055000         MOVE INFO-KDKVAINF      TO RESP-KDKVAINF                         
055100                                                                          
055200         MOVE INFO-KDPERSON      TO W-IDPERSON                            
055300         PERFORM IMS-GU-WDP311                                            
055400         IF SEGMENT-FINNS                                                 
055500           MOVE PERS-IDNAMN(1:26) TO RESP-IDNAMN                          
055600           MOVE PERS-IDMAIL       TO RESP-IDMAIL                          
055700         ELSE                                                             
055800           MOVE SPACE             TO RESP-IDNAMN                          
055900           MOVE 'QUAL '           TO RESP-IDNAMN(1:5)                     
056000           MOVE INFO-KDPERSON     TO RESP-IDNAMN(6:3)                     
056100         END-IF                                                           
056200                                                                          
056300         MOVE INFO-TEKVAINF-EXT(1) TO RESP-TEKVAINF-002-RAD1              
056400         MOVE INFO-TEKVAINF-EXT(2) TO RESP-TEKVAINF-002-RAD2              
056500         MOVE INFO-TEKVAINF-EXT(3) TO RESP-TEKVAINF-002-RAD3              
056600         MOVE INFO-TEKVAINF-EXT(4) TO RESP-TEKVAINF-002-RAD4              
056700         MOVE INFO-TEKVAINF-EXT(5) TO RESP-TEKVAINF-002-RAD5              
056800         MOVE INFO-TEKVAINF-EXT(6) TO RESP-TEKVAINF-002-RAD6              
056900         MOVE INFO-TEKVAINF-EXT(7) TO RESP-TEKVAINF-002-RAD7              
057000                                                                          
057100         MOVE INFO-IDKVAINF             TO RESP-IDKVAINF-UPPD             
057200                                           RESP-IDKVAINF-KEY              
057300                                                                          
057400         MOVE INFO-TIKLAR-LEV           TO RESP-TIKLAR-LEV                
057500         IF INFO-FLSTOCH = 'J'                                            
057600           MOVE 'Y'                     TO RESP-FLSTOCH                   
057700         ELSE                                                             
057800           MOVE INFO-FLSTOCH            TO RESP-FLSTOCH                   
057900         END-IF                                                           
058000                                                                          
058100         PERFORM FB-LAES-DCDATA                                           
058200                                                                          
058300         MOVE LOW-VALUE                 TO W-IDKVAINF-MIN-X               
058400         PERFORM IMS-GNP-W6D211                                           
058500                                                                          
058600       ELSE                                                               
058700         MOVE '286'          TO RESP-IDMSG-ERROR                          
058800       END-IF                                                             
058900     ELSE                                                                 
059000       MOVE '286'          TO RESP-IDMSG-ERROR                            
059100     END-IF                                                               
059200                                                                          
059300     IF SEGMENT-FINNS                                                     
059400       IF REQU-KDPGMACT = 'E'                                             
059500         CONTINUE                                                         
059600       END-IF                                                             
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000 FB-LAES-DCDATA SECTION.                                                  
060100                                                                          
060200     PERFORM IMS-GU-W6D221                                                
060300     IF SEGMENT-FINNS                                                     
060400       IF DCQ-TISTADAT = ZERO                                             
060500         MOVE SPACE               TO RESP-TISTADAT-UT                     
060600       ELSE                                                               
060700         MOVE DCQ-TISTADAT        TO WS-TISTADAT                          
060800         MOVE WS-TISTADAT         TO RESP-TISTADAT-UT                     
060900       END-IF                                                             
061000       IF DCQ-TISTODAT = ZERO                                             
061100         MOVE SPACE               TO RESP-TISTODAT-UT                     
061200       ELSE                                                               
061300         MOVE DCQ-TISTODAT        TO WS-TISTODAT                          
061400         MOVE WS-TISTODAT         TO RESP-TISTODAT-UT                     
061500       END-IF                                                             
061600       MOVE DCQ-KVANTAL           TO RESP-KVANTAL-UT                      
061700       MOVE DCQ-KVAVV-KVAL        TO RESP-KVAVV-KVAL-UT                   
061800       MOVE DCQ-KVART-SKROT       TO RESP-KVART-SKROT-UT                  
061900       MOVE DCQ-KVART-RET         TO RESP-KVART-RET-UT                    
062000       MOVE DCQ-KVART-KJUST       TO RESP-KVART-KJUST-UT                  
062100       IF REQU-KDPGMACT = 'E'                                             
062200         IF REQU-BEINIT = ALL '+'                                         
062300           MOVE DCQ-BEINIT            TO RESP-BEINIT                      
062400         ELSE                                                             
062500           MOVE REQU-BEINIT           TO RESP-BEINIT                      
062600         END-IF                                                           
062700                                                                          
062800         IF REQU-IDNAMN-DC-QUAL = ALL '+'                                 
062900           MOVE DCQ-IDNAMN            TO RESP-IDNAMN-DC-QUAL              
063000         ELSE                                                             
063100           MOVE REQU-IDNAMN-DC-QUAL   TO RESP-IDNAMN-DC-QUAL              
063200         END-IF                                                           
063300                                                                          
063400         IF REQU-TEKVAINF-DC-RAD1 = ALL '+'                               
063500           MOVE DCQ-TEKVAINF(1)(1:63) TO RESP-TEKVAINF-DC-RAD1            
063600         ELSE                                                             
063700           MOVE REQU-TEKVAINF-DC-RAD1 TO RESP-TEKVAINF-DC-RAD1            
063800         END-IF                                                           
063900                                                                          
064000         IF REQU-TEKVAINF-DC-RAD2 = ALL '+'                               
064100           MOVE DCQ-TEKVAINF(2)       TO RESP-TEKVAINF-DC-RAD2            
064200         ELSE                                                             
064300           MOVE REQU-TEKVAINF-DC-RAD2 TO RESP-TEKVAINF-DC-RAD2            
064400         END-IF                                                           
064500                                                                          
064600         IF REQU-TEKVAINF-DC-RAD3 = ALL '+'                               
064700           MOVE DCQ-TEKVAINF(3)       TO RESP-TEKVAINF-DC-RAD3            
064800         ELSE                                                             
064900           MOVE REQU-TEKVAINF-DC-RAD3 TO RESP-TEKVAINF-DC-RAD3            
065000         END-IF                                                           
065100                                                                          
065200         IF REQU-TEKVAINF-DC-RAD4 = ALL '+'                               
065300           MOVE DCQ-TEKVAINF(4)       TO RESP-TEKVAINF-DC-RAD4            
065400         ELSE                                                             
065500           MOVE REQU-TEKVAINF-DC-RAD4 TO RESP-TEKVAINF-DC-RAD4            
065600         END-IF                                                           
065700       ELSE                                                               
065800         MOVE DCQ-BEINIT            TO RESP-BEINIT                        
065900         MOVE DCQ-IDNAMN            TO RESP-IDNAMN-DC-QUAL                
066000         MOVE DCQ-TEKVAINF(1)(1:63) TO RESP-TEKVAINF-DC-RAD1              
066100         MOVE DCQ-TEKVAINF(2)       TO RESP-TEKVAINF-DC-RAD2              
066200         MOVE DCQ-TEKVAINF(3)       TO RESP-TEKVAINF-DC-RAD3              
066300         MOVE DCQ-TEKVAINF(4)       TO RESP-TEKVAINF-DC-RAD4              
066400       END-IF                                                             
066500     ELSE                                                                 
066600       IF INFO-IDKVAINF = 99                                              
066700         PERFORM FA-LAES-GRUNDDATA                                        
066800       END-IF                                                             
066900     END-IF                                                               
067000     .                                                                    
067100     EJECT                                                                
067200 G-KOLLA-INPUT SECTION.                                                   
067300                                                                          
067400     MOVE JA  TO INDATA-SW                                                
067500     IF REQU-INPUT = ALL '+'                                              
067600       MOVE '014'   TO RESP-IDMSG-ERROR                                   
067700       MOVE SPACE   TO RESP-IDELMT-ERROR                                  
067800       MOVE NEJ TO INDATA-SW                                              
067900     ELSE                                                                 
068000       PERFORM IMS-GU-WDK611                                              
068100       IF SEGMENT-FINNS                                                   
068200          PERFORM IMS-GU-WDK711                                           
068300       END-IF                                                             
068400                                                                          
068500       IF SEGMENT-SAKNAS                                                  
068600         MOVE NEJ                       TO INDATA-SW                      
068700         MOVE '025'                     TO RESP-IDMSG-ERROR               
068800         MOVE 'IDARTNR'                 TO RESP-IDELMT-ERROR              
068900       ELSE                                                               
069000         PERFORM IMS-GU-W6D201                                            
069100         IF SEGMENT-FINNS                                                 
069200           PERFORM IMS-GU-W6D211                                          
069300           IF SEGMENT-SAKNAS                                              
069400             IF REQU-IDKVAINF-UPPD = '99'                                 
069500               MOVE JA     TO FL-INTERN                                   
069600             ELSE                                                         
069700               MOVE NEJ    TO INDATA-SW                                   
069800               MOVE '023'       TO RESP-IDMSG-ERROR                       
069900               MOVE 'IDKVAINF'  TO RESP-IDELMT-ERROR                      
070000             END-IF                                                       
070100           END-IF                                                         
070200         ELSE                                                             
070300           IF REQU-IDKVAINF-UPPD = '99'                                   
070400              MOVE W-IDARTNR   TO KVAH-ART-IDARTNR                        
070500              MOVE SPACE       TO KVAH-ART-ADKVAULG                       
070600              MOVE '0000'      TO KVAH-ART-KDKVAKTL                       
070700              PERFORM IMS-ISRT-W6D201                                     
070800              MOVE JA     TO FL-INTERN                                    
070900           ELSE                                                           
071000              MOVE NEJ    TO INDATA-SW                                    
071100              MOVE '023'       TO RESP-IDMSG-ERROR                        
071200              MOVE 'IDKVAINF'  TO RESP-IDELMT-ERROR                       
071300           END-IF                                                         
071400         END-IF                                                           
071500                                                                          
071600         IF REQU-BEINIT NOT = ALL '+'                                     
071700           IF REQU-BEINIT = SPACE                                         
071800              MOVE '023'       TO RESP-IDMSG-ERROR                        
071900              MOVE 'BEINIT'    TO RESP-IDELMT-ERROR                       
072000             MOVE NEJ TO INDATA-SW                                        
072100           ELSE                                                           
072200             MOVE REQU-BEINIT    TO RESP-BEINIT                           
072300           END-IF                                                         
072400         END-IF                                                           
072500                                                                          
072600         IF REQU-TISTADAT-IN NOT = ALL '+'                                
072700           IF REQU-TISTADAT-IN NUMERIC                                    
072800             CONTINUE                                                     
072900           ELSE                                                           
073000             MOVE NEJ TO INDATA-SW                                        
073100             MOVE '024'       TO RESP-IDMSG-ERROR                         
073200             MOVE 'TISTADAT'  TO RESP-IDELMT-ERROR                        
073300           END-IF                                                         
073400         END-IF                                                           
073500                                                                          
073600         IF REQU-TISTODAT-IN NOT = ALL '+'                                
073700           IF REQU-TISTODAT-IN NUMERIC                                    
073800             CONTINUE                                                     
073900           ELSE                                                           
074000             MOVE NEJ TO INDATA-SW                                        
074100             MOVE '024'       TO RESP-IDMSG-ERROR                         
074200             MOVE 'TISTODAT'  TO RESP-IDELMT-ERROR                        
074300           END-IF                                                         
074400         END-IF                                                           
074500                                                                          
074600         IF REQU-KVANTAL-IN NOT = ALL '+'                                 
074700           IF REQU-KVANTAL-IN NUMERIC                                     
074800             CONTINUE                                                     
074900           ELSE                                                           
075000             MOVE NEJ TO INDATA-SW                                        
075100             MOVE '024'       TO RESP-IDMSG-ERROR                         
075200             MOVE 'KVANTAL'   TO RESP-IDELMT-ERROR                        
075300           END-IF                                                         
075400         END-IF                                                           
075500                                                                          
075600         IF REQU-KVAVV-KVAL-IN NOT = ALL '+'                              
075700           IF REQU-KVAVV-KVAL-IN NUMERIC                                  
075800             CONTINUE                                                     
075900           ELSE                                                           
076000             MOVE NEJ TO INDATA-SW                                        
076100             MOVE '024'       TO RESP-IDMSG-ERROR                         
076200             MOVE 'KVAVV-KVAL' TO RESP-IDELMT-ERROR                       
076300           END-IF                                                         
076400         END-IF                                                           
076500                                                                          
076600         IF REQU-KVART-SKROT-IN NOT = ALL '+'                             
076700           IF REQU-KVART-SKROT-IN NUMERIC                                 
076800             CONTINUE                                                     
076900           ELSE                                                           
077000             MOVE NEJ TO INDATA-SW                                        
077100             MOVE '024'       TO RESP-IDMSG-ERROR                         
077200             MOVE 'KVSKROT'   TO RESP-IDELMT-ERROR                        
077300           END-IF                                                         
077400         END-IF                                                           
077500                                                                          
077600         IF REQU-KVART-RET-IN NOT = ALL '+'                               
077700           IF REQU-KVART-RET-IN NUMERIC                                   
077800              CONTINUE                                                    
077900           ELSE                                                           
078000             MOVE NEJ TO INDATA-SW                                        
078100             MOVE '024'       TO RESP-IDMSG-ERROR                         
078200             MOVE 'KVART-RET' TO RESP-IDELMT-ERROR                        
078300           END-IF                                                         
078400         END-IF                                                           
078500                                                                          
078600         IF REQU-KVART-KJUST-IN NOT = ALL '+'                             
078700           IF REQU-KVART-KJUST-IN NUMERIC                                 
078800              CONTINUE                                                    
078900           ELSE                                                           
079000             MOVE NEJ TO INDATA-SW                                        
079100             MOVE '024'          TO RESP-IDMSG-ERROR                      
079200             MOVE 'KVART-KJUST'  TO RESP-IDELMT-ERROR                     
079300           END-IF                                                         
079400         END-IF                                                           
079500                                                                          
079600         IF REQU-FLTABORT NOT = ALL '+'                                   
079700            IF REQU-FLTABORT = JA OR YES                                  
079800               CONTINUE                                                   
079900            ELSE                                                          
080000               MOVE NEJ                  TO INDATA-SW                     
080100               MOVE '279'          TO RESP-IDMSG-ERROR                    
080200           END-IF                                                         
080300         END-IF                                                           
080400                                                                          
080500                                                                          
080600       END-IF                                                             
080700                                                                          
080800       IF INDATA-OK                                                       
080900         PERFORM IMS-GU-W6D221-2                                          
081000         IF SEGMENT-FINNS                                                 
081100           IF DCQ-FLSLUT = JA                                             
081200             MOVE NEJ TO INDATA-SW                                        
081300             MOVE '007'          TO RESP-IDMSG-ERROR                      
081400           END-IF                                                         
081500                                                                          
081600         ELSE                                                             
081700           IF REQU-BEINIT = ALL '+' OR SPACE                              
081800             MOVE NEJ TO INDATA-SW                                        
081900             MOVE 'BEINIT'       TO RESP-IDELMT-ERROR                     
082000             MOVE '026'          TO RESP-IDMSG-ERROR                      
082100             PERFORM GA-MID-INDATA-TILL-MOD                               
082200           END-IF                                                         
082210         END-IF                                                           
082300         IF WS-IDKVAINF = '99'                                            
082400           CONTINUE                                                       
082500         ELSE                                                             
082600           IF REQU-FLTABORT = JA OR YES                                   
082700             MOVE NEJ TO INDATA-SW                                        
082800             MOVE '279'          TO RESP-IDMSG-ERROR                      
082900           END-IF                                                         
083000         END-IF                                                           
083200       END-IF                                                             
083300     END-IF                                                               
083400     .                                                                    
083500     EJECT                                                                
083600 GA-MID-INDATA-TILL-MOD SECTION.                                          
083700                                                                          
083800     IF REQU-TISTADAT-IN NOT = ALL '+'                                    
083900        MOVE REQU-TISTADAT-IN       TO RESP-TISTADAT-IN                   
084000     END-IF                                                               
084100                                                                          
084200     IF REQU-TISTODAT-IN NOT = ALL '+'                                    
084300        MOVE REQU-TISTODAT-IN       TO RESP-TISTODAT-IN                   
084400     END-IF                                                               
084500                                                                          
084600     IF REQU-KVANTAL-IN NOT = ALL '+'                                     
084700        MOVE REQU-KVANTAL-IN        TO RESP-KVANTAL-IN                    
084800     END-IF                                                               
084900                                                                          
085000     IF REQU-KVAVV-KVAL-IN NOT = ALL '+'                                  
085100        MOVE REQU-KVAVV-KVAL-IN     TO RESP-KVAVV-KVAL-IN                 
085200     END-IF                                                               
085300                                                                          
085400     IF REQU-KVART-SKROT-IN NOT = ALL '+'                                 
085500        MOVE REQU-KVART-SKROT-IN    TO RESP-KVART-SKROT-IN                
085600     END-IF                                                               
085700                                                                          
085800     IF REQU-KVART-RET-IN NOT = ALL '+'                                   
085900        MOVE REQU-KVART-RET-IN      TO RESP-KVART-RET-IN                  
086000     END-IF                                                               
086100                                                                          
086200     IF REQU-KVART-KJUST-IN NOT = ALL '+'                                 
086300        MOVE REQU-KVART-KJUST-IN    TO RESP-KVART-KJUST-IN                
086400     END-IF                                                               
086500                                                                          
086600     IF REQU-BEINIT NOT = ALL '+'                                         
086700        MOVE REQU-BEINIT            TO RESP-BEINIT                        
086800     END-IF                                                               
086900                                                                          
087000     IF REQU-IDNAMN-DC-QUAL NOT = ALL '+'                                 
087100        MOVE REQU-IDNAMN-DC-QUAL    TO RESP-IDNAMN-DC-QUAL                
087200     END-IF                                                               
087300                                                                          
087400     IF REQU-TEKVAINF-DC-RAD1 NOT = ALL '+'                               
087500       MOVE REQU-TEKVAINF-DC-RAD1  TO RESP-TEKVAINF-DC-RAD1               
087600     END-IF                                                               
087700                                                                          
087800     IF REQU-TEKVAINF-DC-RAD2 NOT = ALL '+'                               
087900       MOVE REQU-TEKVAINF-DC-RAD2  TO RESP-TEKVAINF-DC-RAD2               
088000     END-IF                                                               
088100                                                                          
088200     IF REQU-TEKVAINF-DC-RAD3 NOT = ALL '+'                               
088300       MOVE REQU-TEKVAINF-DC-RAD3  TO RESP-TEKVAINF-DC-RAD3               
088400     END-IF                                                               
088500                                                                          
088600     IF REQU-TEKVAINF-DC-RAD4 NOT = ALL '+'                               
088700       MOVE REQU-TEKVAINF-DC-RAD4  TO RESP-TEKVAINF-DC-RAD4               
088800     END-IF                                                               
088900                                                                          
089000     IF REQU-FLTABORT NOT = ALL '+'                                       
089100       MOVE REQU-FLTABORT          TO RESP-FLTABORT                       
089200     END-IF                                                               
089300     .                                                                    
089400     EJECT                                                                
089500 H-UPPDATERA SECTION.                                                     
089600                                                                          
089700     IF FL-INTERN = JA                                                    
089800       MOVE DAGENS-DATUM TO INFO-TIREGDAT                                 
089900       MOVE 79000000     TO INFO-DAREGDAT-9KOMPL                          
090000       MOVE 999999999    TO INFO-TIKLOCK-9KOMPL                           
090100                                                                          
090200       MOVE REQU-IDKVAINF-UPPD        TO INFO-IDKVAINF                    
090300       MOVE ZERO  TO INFO-TIKLAR-QUAL                                     
090400                     INFO-TIKLAR-LEV                                      
090500                     INFO-KDPERSON                                        
090600       MOVE SPACE TO INFO-KDKVAINF                                        
090700                     INFO-TEKVAINP                                        
090800                     INFO-FLQPA                                           
090900                     INFO-FLSTOCH                                         
091000                     INFO-TEKVAINF-INT(1)                                 
091100                     INFO-TEKVAINF-INT(2)                                 
091200                     INFO-TEKVAINF-INT(3)                                 
091300                     INFO-TEKVAINF-INT(4)                                 
091400                     INFO-TEKVAINF-INT(5)                                 
091500                     INFO-TEKVAINF-INT(6)                                 
091600                     INFO-TEKVAINF-INT(7)                                 
091700                     INFO-TEKVAINF-EXT(1)                                 
091800                     INFO-TEKVAINF-EXT(2)                                 
091900                     INFO-TEKVAINF-EXT(3)                                 
092000                     INFO-TEKVAINF-EXT(4)                                 
092100                     INFO-TEKVAINF-EXT(5)                                 
092200                     INFO-TEKVAINF-EXT(6)                                 
092300                     INFO-TEKVAINF-EXT(7)                                 
092400       PERFORM IMS-ISRT-W6D211                                            
092500     END-IF                                                               
092600                                                                          
092700     PERFORM IMS-GHU-W6D221                                               
092800                                                                          
092900     IF SEGMENT-FINNS                                                     
093000       MOVE INFO-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                     
093100       MOVE INFO-TIKLOCK-9KOMPL  TO W-TIKLOCK-9KOMPL                      
093200                                                                          
093300       IF REQU-FLTABORT = JA OR YES                                       
093400         PERFORM IMS-DLET-W6D221                                          
094200       ELSE                                                               
094300         IF REQU-BEINIT NOT = ALL '+'                                     
094400           MOVE REQU-BEINIT TO DCQ-BEINIT                                 
094500         END-IF                                                           
094600                                                                          
094700         IF REQU-TISTADAT-IN NOT = ALL '+'                                
094800           MOVE REQU-TISTADAT-IN TO DCQ-TISTADAT                          
094900         END-IF                                                           
095000                                                                          
095100         IF REQU-TISTODAT-IN NOT = ALL '+'                                
095200           MOVE REQU-TISTODAT-IN TO DCQ-TISTODAT                          
095300         END-IF                                                           
095400                                                                          
095500         IF REQU-KVANTAL-IN NOT = ALL '+'                                 
095600           MOVE REQU-KVANTAL-IN TO DCQ-KVANTAL                            
095700         END-IF                                                           
095800                                                                          
095900         IF REQU-KVAVV-KVAL-IN NOT = ALL '+'                              
096000           MOVE REQU-KVAVV-KVAL-IN TO DCQ-KVAVV-KVAL                      
096100         END-IF                                                           
096200                                                                          
096300         IF REQU-KVART-SKROT-IN NOT = ALL '+'                             
096400           MOVE REQU-KVART-SKROT-IN TO DCQ-KVART-SKROT                    
096500         END-IF                                                           
096600                                                                          
096700         IF REQU-KVART-RET-IN NOT = ALL '+'                               
096800           MOVE REQU-KVART-RET-IN TO DCQ-KVART-RET                        
096900         END-IF                                                           
097000                                                                          
097100         IF REQU-KVART-KJUST-IN NOT = ALL '+'                             
097200           MOVE REQU-KVART-KJUST-IN TO DCQ-KVART-KJUST                    
097300         END-IF                                                           
097400                                                                          
097500         IF REQU-IDNAMN-DC-QUAL NOT = ALL '+'                             
097600           MOVE REQU-IDNAMN-DC-QUAL TO DCQ-IDNAMN                         
097700         END-IF                                                           
097800                                                                          
097900         IF REQU-TEKVAINF-DC-RAD1 NOT = ALL '+'                           
098000           MOVE REQU-TEKVAINF-DC-RAD1  TO DCQ-TEKVAINF(1)(1:63)           
098100         END-IF                                                           
098200                                                                          
098300         IF REQU-TEKVAINF-DC-RAD2 NOT = ALL '+'                           
098400           MOVE REQU-TEKVAINF-DC-RAD2  TO DCQ-TEKVAINF(2)                 
098500         END-IF                                                           
098600                                                                          
098700         IF REQU-TEKVAINF-DC-RAD3 NOT = ALL '+'                           
098800           MOVE REQU-TEKVAINF-DC-RAD3  TO DCQ-TEKVAINF(3)                 
098900         END-IF                                                           
099000                                                                          
099100         IF REQU-TEKVAINF-DC-RAD4 NOT = ALL '+'                           
099200           MOVE REQU-TEKVAINF-DC-RAD4  TO DCQ-TEKVAINF(4)                 
099300         END-IF                                                           
099400                                                                          
099500         PERFORM IMS-REPL-W6D221                                          
099600       END-IF                                                             
099700     ELSE                                                                 
099800       PERFORM IMS-GHU-W6D211                                             
099900       IF SEGMENT-FINNS                                                   
100000         MOVE REQU-BEINIT TO DCQ-BEINIT                                   
100100         MOVE NEJ        TO DCQ-FLSLUT                                    
100200                                                                          
100300         IF REQU-TISTADAT-IN NOT = ALL '+'                                
100400           MOVE REQU-TISTADAT-IN TO DCQ-TISTADAT                          
100500         ELSE                                                             
100600           MOVE ZERO TO DCQ-TISTADAT                                      
100700         END-IF                                                           
100800                                                                          
100900         IF REQU-TISTODAT-IN NOT = ALL '+'                                
101000           MOVE REQU-TISTODAT-IN TO DCQ-TISTODAT                          
101100         ELSE                                                             
101200           MOVE ZERO TO DCQ-TISTODAT                                      
101300         END-IF                                                           
101400                                                                          
101500         IF REQU-KVANTAL-IN NOT = ALL '+'                                 
101600           MOVE REQU-KVANTAL-IN TO DCQ-KVANTAL                            
101700         ELSE                                                             
101800           MOVE ZERO TO DCQ-KVANTAL                                       
101900         END-IF                                                           
102000                                                                          
102100         IF REQU-KVAVV-KVAL-IN NOT = ALL '+'                              
102200           MOVE REQU-KVAVV-KVAL-IN TO DCQ-KVAVV-KVAL                      
102300         ELSE                                                             
102400           MOVE ZERO TO DCQ-KVAVV-KVAL                                    
102500         END-IF                                                           
102600                                                                          
102700         IF REQU-KVART-SKROT-IN NOT = ALL '+'                             
102800           MOVE REQU-KVART-SKROT-IN TO DCQ-KVART-SKROT                    
102900         ELSE                                                             
103000           MOVE ZERO TO DCQ-KVART-SKROT                                   
103100         END-IF                                                           
103200                                                                          
103300         IF REQU-KVART-RET-IN NOT = ALL '+'                               
103400           MOVE REQU-KVART-RET-IN TO DCQ-KVART-RET                        
103500         ELSE                                                             
103600           MOVE ZERO TO DCQ-KVART-RET                                     
103700         END-IF                                                           
103800                                                                          
103900         IF REQU-KVART-KJUST-IN NOT = ALL '+'                             
104000           MOVE REQU-KVART-KJUST-IN TO DCQ-KVART-KJUST                    
104100         ELSE                                                             
104200           MOVE ZERO TO DCQ-KVART-KJUST                                   
104300         END-IF                                                           
104400                                                                          
104500         IF REQU-IDNAMN-DC-QUAL NOT = ALL '+'                             
104600           MOVE REQU-IDNAMN-DC-QUAL TO DCQ-IDNAMN                         
104700         ELSE                                                             
104800           MOVE SPACE TO DCQ-IDNAMN                                       
104900         END-IF                                                           
105000                                                                          
105100         MOVE SPACE TO DCQ-TEKVAINF(1)                                    
105200         IF REQU-TEKVAINF-DC-RAD1 NOT = ALL '+'                           
105300           MOVE REQU-TEKVAINF-DC-RAD1  TO DCQ-TEKVAINF(1)(1:63)           
105400         END-IF                                                           
105500                                                                          
105600         IF REQU-TEKVAINF-DC-RAD2 NOT = ALL '+'                           
105700           MOVE REQU-TEKVAINF-DC-RAD2  TO DCQ-TEKVAINF(2)                 
105800         ELSE                                                             
105900           MOVE SPACE TO DCQ-TEKVAINF(2)                                  
106000         END-IF                                                           
106100                                                                          
106200         IF REQU-TEKVAINF-DC-RAD3 NOT = ALL '+'                           
106300           MOVE REQU-TEKVAINF-DC-RAD3  TO DCQ-TEKVAINF(3)                 
106400         ELSE                                                             
106500           MOVE SPACE TO DCQ-TEKVAINF(3)                                  
106600         END-IF                                                           
106700                                                                          
106800         IF REQU-TEKVAINF-DC-RAD4 NOT = ALL '+'                           
106900           MOVE REQU-TEKVAINF-DC-RAD4  TO DCQ-TEKVAINF(4)                 
107000         ELSE                                                             
107100           MOVE SPACE TO DCQ-TEKVAINF(4)                                  
107200         END-IF                                                           
107300                                                                          
107400         MOVE INFO-DAREGDAT-9KOMPL TO W-DAREGDAT-9KOMPL                   
107500         MOVE INFO-TIKLOCK-9KOMPL  TO W-TIKLOCK-9KOMPL                    
107600         PERFORM IMS-ISRT-W6D221                                          
107700       END-IF                                                             
107800     END-IF                                                               
107900     MOVE '001'    TO RESP-IDMSG-INFO                                     
108000     .                                                                    
108100     EJECT                                                                
108200*    --- DISPATCHER SECTIONS                                              
108300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
108400                                                                          
108500     MOVE 'GETARG'               TO SUB-KDFUNC                            
108600     MOVE 'CARPARTS.LDC.PARTHISTQUALITY'      TO SUB-ADDISPABS            
108700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
108800                                                                          
108900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
109000                                                                          
109100     IF SUB-KDRC > 0                                                      
109200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
109300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
109400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
109500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
109600     END-IF                                                               
109700     .                                                                    
109800     SKIP3                                                                
109900 S02-RETURN-RESPONSE SECTION.                                             
110000                                                                          
110100     MOVE 'RETURN'                   TO SUB-KDFUNC                        
110200     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
110300                                                                          
110400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
110500                                                                          
110600     IF SUB-KDRC > 0                                                      
110700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
110800       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
110900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
111000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
111100     END-IF                                                               
111200     .                                                                    
111300     EJECT                                                                
111400                                                                          
111500 IMS-GU-W6D201 SECTION.                                                   
111600                                                                          
111700     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
111800          DELIMITED BY SIZE INTO SSA1                                     
111900     MOVE '  GE' TO GODK-STATUSKODER                                      
112000     CALL CBLTDLI USING GU W6D2A-PCB DLI-IO-W6D2 SSA1                     
112100     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
112200     PERFORM IMS-STATUSKONTROLL                                           
112300     .                                                                    
112400     EJECT                                                                
112500 IMS-ISRT-W6D201 SECTION.                                                 
112600                                                                          
112700     MOVE 'W6D201   ' TO SSA1                                             
112800     MOVE '  ' TO GODK-STATUSKODER                                        
112900     CALL CBLTDLI USING ISRT W6D2A-PCB DLI-IO-W6D2                        
113000                                                    SSA1                  
113100     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
113200     PERFORM IMS-STATUSKONTROLL                                           
113300     .                                                                    
113400     SKIP3                                                                
113500 IMS-GNP-W6D211 SECTION.                                                  
113600                                                                          
113700     STRING 'W6D211  (W6D211KY=>' W-W6D211KY-X                            
113800                    '&IDKVAINF=>' W-IDKVAINF-MIN-X                        
113900                    '&IDKVAINF=<' W-IDKVAINF-MAX-X ')'                    
114000          DELIMITED BY SIZE INTO SSA1                                     
114100     MOVE '  GE' TO GODK-STATUSKODER                                      
114200     CALL CBLTDLI USING GNP W6D2A-PCB DLI-IO-W6D2 SSA1                    
114300     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
114400     PERFORM IMS-STATUSKONTROLL                                           
114500     .                                                                    
114600     SKIP3                                                                
114700 IMS-GNP-W6D211-NEXT SECTION.                                             
114800     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
114900          DELIMITED BY SIZE INTO SSA1                                     
115000     MOVE 'W6D211    '        TO SSA2                                     
115100     MOVE '  GE' TO GODK-STATUSKODER                                      
115200     CALL CBLTDLI USING GNP W6D2A-PCB DLI-IO-W6D2 SSA1 SSA2               
115300     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
115400     PERFORM IMS-STATUSKONTROLL                                           
115500     .                                                                    
115600     SKIP3                                                                
115700 IMS-GU-W6D211 SECTION.                                                   
115800     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
115900          DELIMITED BY SIZE INTO SSA1                                     
116000     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
116100          DELIMITED BY SIZE INTO SSA2                                     
116200     MOVE '  GE' TO GODK-STATUSKODER                                      
116300     CALL CBLTDLI USING GU W6D2A-PCB DLI-IO-W6D2 SSA1 SSA2                
116400     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
116500     PERFORM IMS-STATUSKONTROLL                                           
116600     .                                                                    
116700     SKIP3                                                                
116800 IMS-GHU-W6D211 SECTION.                                                  
116900     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
117000          DELIMITED BY SIZE INTO SSA1                                     
117100     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
117200          DELIMITED BY SIZE INTO SSA2                                     
117300     MOVE '  GE' TO GODK-STATUSKODER                                      
117400     CALL CBLTDLI USING GHU W6D2A-PCB DLI-IO-W6D2 SSA1 SSA2               
117500     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
117600     PERFORM IMS-STATUSKONTROLL                                           
117700     .                                                                    
117800     SKIP3                                                                
117900 IMS-ISRT-W6D211 SECTION.                                                 
118000                                                                          
118100     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
118200          DELIMITED BY SIZE INTO SSA1                                     
118300     MOVE 'W6D211 ' TO SSA2                                               
118400     MOVE '  II' TO GODK-STATUSKODER                                      
118500     CALL CBLTDLI USING ISRT W6D2A-PCB DLI-IO-W6D2                        
118600                                                    SSA1 SSA2             
118700     MOVE W6D2A-STATUS-CODE TO STATUS-WS                                  
118800     PERFORM IMS-STATUSKONTROLL                                           
118900     .                                                                    
119000     SKIP3                                                                
119900 IMS-GU-W6D221 SECTION.                                                   
120000                                                                          
120100     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
120200          DELIMITED BY SIZE INTO SSA1                                     
120300     STRING 'W6D211  (W6D211KY =' W-W6D211KY-X ')'                        
120400          DELIMITED BY SIZE INTO SSA2                                     
120500     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
120600          DELIMITED BY SIZE INTO SSA3                                     
120700     MOVE '  GE' TO GODK-STATUSKODER                                      
120800     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
120900     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
121000     PERFORM IMS-STATUSKONTROLL                                           
121100     .                                                                    
121200     SKIP3                                                                
121300 IMS-GU-W6D221-2 SECTION.                                                 
121400                                                                          
121500     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
121600          DELIMITED BY SIZE INTO SSA1                                     
121700     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
121800          DELIMITED BY SIZE INTO SSA2                                     
121900     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
122000          DELIMITED BY SIZE INTO SSA3                                     
122100     MOVE '  GE' TO GODK-STATUSKODER                                      
122200     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
122300     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
122400     PERFORM IMS-STATUSKONTROLL                                           
122500     .                                                                    
122600     SKIP3                                                                
122700 IMS-GET-W6D221-FIRST SECTION.                                            
122800                                                                          
122900     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
123000          DELIMITED BY SIZE INTO SSA1                                     
123100     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
123200          DELIMITED BY SIZE INTO SSA2                                     
123300     MOVE 'W6D221  *F'        TO SSA3                                     
123400     MOVE '  GE' TO GODK-STATUSKODER                                      
123500     CALL CBLTDLI USING GU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3         
123600     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
123700     PERFORM IMS-STATUSKONTROLL                                           
123800     .                                                                    
123900     SKIP3                                                                
124000 IMS-GHU-W6D221 SECTION.                                                  
124100                                                                          
124200     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
124300          DELIMITED BY SIZE INTO SSA1                                     
124400     STRING 'W6D211  (IDKVAINF =' W-IDKVAINF-X ')'                        
124500          DELIMITED BY SIZE INTO SSA2                                     
124600     STRING 'W6D221  (IDDC     =' W-IDDC-X ')'                            
124700          DELIMITED BY SIZE INTO SSA3                                     
124800     MOVE '  GE' TO GODK-STATUSKODER                                      
124900     CALL CBLTDLI USING GHU W6D2B-PCB DLI-IO-W6D221 SSA1 SSA2 SSA3        
125000     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
125100     PERFORM IMS-STATUSKONTROLL                                           
125200     .                                                                    
125300     SKIP3                                                                
125400 IMS-ISRT-W6D221 SECTION.                                                 
125500                                                                          
125600     STRING 'W6D201  (IDARTNR  =' W-IDARTNR-X ')'                         
125700          DELIMITED BY SIZE INTO SSA1                                     
125800     STRING 'W6D211  (W6D211KY =' W-W6D211KY-X ')'                        
125900          DELIMITED BY SIZE INTO SSA2                                     
126000     MOVE 'W6D221 ' TO SSA3                                               
126100     MOVE '  II' TO GODK-STATUSKODER                                      
126200     CALL CBLTDLI USING ISRT W6D2B-PCB DLI-IO-W6D221                      
126300                                                    SSA1 SSA2 SSA3        
126400     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
126500     PERFORM IMS-STATUSKONTROLL                                           
126600     .                                                                    
126700     SKIP3                                                                
126800 IMS-REPL-W6D221 SECTION.                                                 
126900                                                                          
127000     MOVE '  ' TO GODK-STATUSKODER                                        
127100     CALL CBLTDLI USING REPL W6D2B-PCB DLI-IO-W6D221                      
127200     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
127300     PERFORM IMS-STATUSKONTROLL                                           
127400     .                                                                    
127500     SKIP3                                                                
127600 IMS-DLET-W6D221 SECTION.                                                 
127700                                                                          
127800     MOVE '  ' TO GODK-STATUSKODER                                        
127900     CALL CBLTDLI USING DLET W6D2B-PCB DLI-IO-W6D221                      
128000     MOVE W6D2B-STATUS-CODE TO STATUS-WS                                  
128100     PERFORM IMS-STATUSKONTROLL                                           
128200     .                                                                    
128300     EJECT                                                                
128400 IMS-GU-WDK611 SECTION.                                                   
128500                                                                          
128600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
128700          DELIMITED BY SIZE INTO SSA1                                     
128800     MOVE 'WDK611   ' TO SSA2                                             
128900     MOVE '  GE' TO GODK-STATUSKODER                                      
129000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK6 SSA1 SSA2                 
129100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
129200     PERFORM IMS-STATUSKONTROLL                                           
129300     .                                                                    
129400     EJECT                                                                
129500 IMS-GU-WDK711 SECTION.                                                   
129600                                                                          
129700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
129800          DELIMITED BY SIZE INTO SSA1                                     
129900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
130000          DELIMITED BY SIZE INTO SSA2                                     
130100     MOVE '  GE' TO GODK-STATUSKODER                                      
130200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK7 SSA1 SSA2                 
130300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
130400     PERFORM IMS-STATUSKONTROLL                                           
130500     .                                                                    
130600     EJECT                                                                
130700 IMS-GU-WDP311 SECTION.                                                   
130800     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
130900          DELIMITED BY SIZE INTO SSA1                                     
131000     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
131100          DELIMITED BY SIZE INTO SSA2                                     
131200     MOVE '  GE' TO GODK-STATUSKODER                                      
131300     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-P311 SSA1 SSA2                 
131400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
131500     PERFORM IMS-STATUSKONTROLL                                           
131600     .                                                                    
131700     EJECT                                                                
131800 IMS-GU-WDB601    SECTION.                                                
131900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
132000          DELIMITED BY SIZE INTO SSA1                                     
132100     MOVE '  GE' TO GODK-STATUSKODER                                      
132200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
132300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
132400     PERFORM IMS-STATUSKONTROLL                                           
132500     IF SEGMENT-SAKNAS                                                    
132600         MOVE SPACE TO DCS-KDDC                                           
132700     END-IF                                                               
132800     .                                                                    
132900 IMS-GET-WDGX6331 SECTION.                                                
133000     MOVE 'IMS-GET-WDGX6331        ' TO WS-IMS                            
133100*    DISPLAY 'IMS-GET-WDGX6331        '                                   
133200                                                                          
133300     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
133400          DELIMITED BY SIZE INTO SSA1                                     
133500     MOVE '  GE' TO GODK-STATUSKODER                                      
133600     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX6331 SSA1                  
133700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
133800     PERFORM IMS-STATUSKONTROLL                                           
133900     .                                                                    
134000     EJECT                                                                
134100 IMS-GHU-WDGX6332 SECTION.                                                
134200     MOVE 'IMS-GU-WDGX6332         ' TO WS-IMS                            
134300*    DISPLAY 'IMS-GU-WDGX6332         '                                   
134400                                                                          
134500     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
134600          DELIMITED BY SIZE INTO SSA1                                     
134700     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
134800          DELIMITED BY SIZE INTO SSA2                                     
134900     MOVE '  GE' TO GODK-STATUSKODER                                      
135000     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-WDGX6332 SSA1 SSA2            
135100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
135200     PERFORM IMS-STATUSKONTROLL                                           
135300     .                                                                    
135400     EJECT                                                                
135500 IMS-GNP-WDGX6332 SECTION.                                                
135600     MOVE 'IMS-GNP-WDGX6332 SECTION' TO WS-IMS                            
135700*    DISPLAY 'IMS-GNP-WDGX6332 SECTION'                                   
135800                                                                          
135900     MOVE 'WDGX6332   ' TO SSA1                                           
136000     MOVE '  GE' TO GODK-STATUSKODER                                      
136100     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-WDGX6332 SSA1                 
136200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
136300     PERFORM IMS-STATUSKONTROLL                                           
136400     .                                                                    
136500     EJECT                                                                
136600 IMS-GET-WDGX6334-UNIK SECTION.                                           
136700     MOVE 'IMS-GET-WDGX6334-UNIK   ' TO WS-IMS                            
136800*    DISPLAY 'IMS-GET-WDGX6334-UNIK   '                                   
136900                                                                          
137000     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-6331-X ')'                    
137100          DELIMITED BY SIZE INTO SSA1                                     
137200                                                                          
137300     STRING 'WDGX6332(IDDC     =' W-WDGXKEY-6332-X ')'                    
137400          DELIMITED BY SIZE INTO SSA2                                     
137500                                                                          
137600     STRING 'WDGX6334(IDDC     =' W-WDGXKEY-6334-X ')'                    
137700          DELIMITED BY SIZE INTO SSA3                                     
137800                                                                          
137900     MOVE '  GE' TO GODK-STATUSKODER                                      
138000     CALL CBLTDLI USING GU WDR2ALT-PCB                                    
138100     DLI-IO-WDGX6334 SSA1 SSA2 SSA3                                       
138200                                                                          
138300     MOVE WDR2ALT-STATUS-CODE TO STATUS-WS                                
138400     PERFORM IMS-STATUSKONTROLL                                           
138500     .                                                                    
138600     EJECT                                                                
138700 IMS-STATUSKONTROLL SECTION.                                              
138800                                                                          
138900     SET STATUS-IX TO 1                                                   
139000     SEARCH GODK-STATUS                                                   
139100       AT END                                                             
139200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
139300         DELIMITED BY SIZE INTO FELTEXT                                   
139400         CALL FELLOG                                                      
139500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
139600         CONTINUE                                                         
139700     END-SEARCH                                                           
139800     .                                                                    
