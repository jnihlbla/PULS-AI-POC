000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL014500.                                                
000400 AUTHOR.         TAPAS KUMAR GHOSH.                                       
000500 DATE-WRITTEN.   2004/06/30.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER FAKTURAHISTORIK FRÅN WDL5                                  
001000*                                                                         
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDL5                                       
001300*                                                                         
001400*        WL014500 PROGRAM IS A REPLICA OF W4072700 PROGRAM                
001500*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001600*                                                                         
001700*                                                                         
001800* ADDRESS:     'CARPARTS.LDC.INVOICEQUERY'                                
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: WL0145T                                             
002200*        REQUEST:     WZ01REQU                                            
002300*                     WL0145I1                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        RESPONSE:    WZ01RESP                                            
002700*                     WL0145O1                                            
002800*                                                                         
002900* ÄNDRINGAR:                                                              
003000* 2011-09-21 E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1                 
003100* 2017-01-26 E-TRACKER 10296404 RETURNS FROM CA TO US                     
003200* 2017-07-03 E-TRACKER 10302968 GENERIC SOLUTION IDFTG                    
003300*                                                                         
003400                                                                          
003500                                                                          
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000*    -- CHECKED BY WY2000                                                 
004100     SKIP3                                                                
004200 77  IDPGM                       PIC X(08)   VALUE 'WL014500'.            
004300 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004400 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004500 77  KDRC-DISPLAY                PIC Z(5).                                
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000 77  WS-COUNT                    PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500 77  WS-IDFAKT                   PIC X(7)    VALUE SPACE.                 
005600 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005700 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
005800 77  WS-IDKOLLI                  PIC X(5)    VALUE SPACE.                 
005900 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
006000 77  WS-IDARTNR                  PIC X(8)    VALUE SPACE.                 
006100                                                                          
006200 77  WS-IDELMT-ERROR             PIC X(16).                               
006300 77  WS-IDMSG-ERROR              PIC X(03).                               
006400 77  WS-IDMSG-INFO               PIC X(03).                               
006500                                                                          
006600                                                                          
006700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006800     88  NYCKLAR-OK                          VALUE 'J'.                   
006900     88  NYCKLAR-FEL                         VALUE 'N'.                   
007000                                                                          
007100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007200     88  EGEN-MID                            VALUE '4727'.                
007300     88  GODK-MID                            VALUE '4721' '4722'          
007400                                                   '4723' '4724'          
007500                                                   '4725' '4726'          
007600                                                   '4727' '4728'          
007700                                                   '4729'.                
007800     88  HELP-MID                            VALUE '0551'.                
007900     EJECT                                                                
008000*    --- PARAMETERS TO ABEND                                              
008100                                                                          
008200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008500                                                                          
008600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008700     88  INDATA-OK                           VALUE 'J'.                   
008800     88  INDATA-WRONG                        VALUE 'N'.                   
008900     SKIP3                                                                
009000                                                                          
009100 01  TEST-IDDISTR                PIC 9(5)    VALUE ZERO COMP-3.           
009200*01  FILLER  -COPY WWDIST34      -RED TEST-IDDISTR.                       
009300                                                                          
009400     EJECT                                                                
009500*01  -COPY WWIDFTG                                                        
009600                                                                          
009700     EJECT                                                                
009800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009900 01  GENERELLA-SUBPROGRAM.                                                
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010400     EJECT                                                                
010500                                                                          
010600 01  MESSAGE-CODES.                                                       
010700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010900     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
011000     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
011100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011200     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '00A'.                 
011300                                                                          
011400     EJECT                                                                
011500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011600*                                                                         
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011900     SKIP3                                                                
012000*01  -COPY WZ01SUB                                                        
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012300     SKIP3                                                                
012400 01  REQU-AREA.                                                           
012500*    03  -COPY WZ01REQU                                                   
012600*    03  -COPY WL0145I1                                                   
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012900     SKIP3                                                                
013000 01  RESP-AREA.                                                           
013100*    03  -COPY WZ01RESP                                                   
013200*    03  -COPY WL0145O1                                                   
013300     EJECT                                                                
013400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600                                                                          
013700 01  NYCKLAR-TILL-DLI.                                                    
013800                                                                          
013900     03  W-IDFAKT-X.                                                      
014000         05 W-IDFAKT             PIC S9(7)   COMP-3.                      
015000                                                                          
016000     03  W-WDL511KY-X.                                                    
017000         05  W-IDPRODNR          PIC S9(7)   COMP-3.                      
018000         05  W-IDKOLLI           PIC S9(5)   COMP-3.                      
018100                                                                          
018200     03  W-IDPRODNR-MIN-X.                                                
018300         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
018400                                                                          
018500     03  W-IDPRODNR-MAX-X.                                                
018600         05  W-IDPRODNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
018700                                                                          
018800     03  W-IDKOLLI-MIN-X.                                                 
018900         05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
019000                                                                          
019100     03  W-IDKOLLI-MAX-X.                                                 
019200         05  W-IDKOLLI-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
019300                                                                          
019400     03  W-IDKUNDNR-MIN-X.                                                
019500         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
019600                                                                          
019700     03  W-IDKUNDNR-MAX-X.                                                
019800         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
019900                                                                          
020000     03  W-IDORDNR-MIN-X.                                                 
020100         05  W-IDORDNR-MIN       PIC  9(7)   VALUE ZERO.                  
020200                                                                          
020300     03  W-IDORDNR-MAX-X.                                                 
020400         05  W-IDORDNR-MAX       PIC  9(7)   VALUE ZERO.                  
020500                                                                          
020600     03  W-WDL521KY-MIN-X.                                                
020700         05  W-IDARTNR-MIN       PIC S9(9)   COMP-3.                      
020800         05  W-IDPURAD-MIN       PIC S9(5)   COMP-3 VALUE +0.             
020900                                                                          
021000     03  W-WDL521KY-MAX-X.                                                
021100         05  W-IDARTNR-MAX       PIC S9(9)   COMP-3.                      
021200         05  W-IDPURAD-MAX       PIC S9(5)   COMP-3 VALUE +99999.         
021300     EJECT                                                                
021400*    --- STATUS-KOD FRÅN IMS                                              
021500 01  STATUS-WS                   PIC XX.                                  
021600     88  SEGMENT-FINNS                       VALUE '  '.                  
021700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021800     88  BASEN-SLUT                          VALUE 'GB'.                  
021900                                                                          
022000 01  GODK-STATUSKODER.                                                    
022100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022200                                                                          
022300 01  SSA1                        PIC X(256).                              
022400 01  SSA2                        PIC X(96).                               
022500     EJECT                                                                
022600*    --- IMS FUNKTIONSKODER                                               
022700*01  -COPY W0003                                                          
022800     EJECT                                                                
022900*    ---  DLI INPUT-OUTPUT AREA                                           
023000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
023100                                                                          
023200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L501'.         
023300 01  DLI-IO-AREA-WDL501.                                                  
023400*    03  -COPY WDL501                                                     
023500     EJECT                                                                
023600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L511'.         
023700 01  DLI-IO-AREA-WDL511.                                                  
023800*    03  -COPY WDL511                                                     
023900     EJECT                                                                
024000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L521'.         
024100 01  DLI-IO-AREA-WDL521.                                                  
024200*    03  -COPY WDL521                                                     
024300     EJECT                                                                
024400 LINKAGE SECTION.                                                         
024500 01  MSG-PCB                     PIC X.                                   
024600*01  -COPY W0008  -PRE WDL5-                                              
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900 PROCEDURE DIVISION  USING MSG-PCB WDL5-PCB.                              
025000     ENTRY 'DLITCBL' USING MSG-PCB WDL5-PCB.                              
025100                                                                          
025200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
025300     IF SUB-KDRC = 0                                                      
025400       PERFORM A-INIT                                                     
025500       PERFORM B-KOLLA-NYCKLAR                                            
025600       IF NYCKLAR-OK                                                      
025700         PERFORM F-LAES-VISA-INFO                                         
025800       END-IF                                                             
025900                                                                          
026000       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
026100       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
026200       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
026300       IF WS-IDMSG-ERROR NOT = SPACE                                      
026400         MOVE ALL '+' TO RESP-AREA                                        
026500         MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                        
026600         MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                       
026700         MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                         
026800         MOVE ZERO             TO RESP-KVRADER                            
026900         MOVE    1             TO RESP-IDMSGVER                           
027000       END-IF                                                             
027100       PERFORM S02-RETURN-RESPONSE                                        
027200     END-IF                                                               
027300                                                                          
027400     MOVE ZERO                  TO RETURN-CODE                            
027500     GOBACK                                                               
027600     .                                                                    
027700     SKIP2                                                                
027800 A-INIT                         SECTION.                                  
027900                                                                          
028000     MOVE ALL '+' TO  RESP-AREA                                           
028100     MOVE ZERO    TO RESP-KVRADER                                         
028200     MOVE 1       TO RESP-IDMSGVER                                        
028300     MOVE SPACE   TO RESP-IDMSG-INFO                                      
028400                     RESP-IDMSG-ERROR                                     
028500                     RESP-IDELMT-ERROR                                    
028600     .                                                                    
028700     EJECT                                                                
028800 B-KOLLA-NYCKLAR                SECTION.                                  
028900                                                                          
029000     MOVE JA                    TO NYCKLAR-SW                             
030000                                                                          
030100*    -- KONTROLL AV IDFAKT                                                
030200     IF REQU-IDFAKT-KEY = ALL '+'                                         
030300       CONTINUE                                                           
030400     ELSE                                                                 
030500       MOVE REQU-IDFAKT-KEY     TO WS-IDFAKT                              
030600     END-IF                                                               
030700     IF WS-IDFAKT NUMERIC AND WS-IDFAKT > ZERO                            
030800       MOVE WS-IDFAKT           TO W-IDFAKT                               
030900       MOVE REQU-IDFAKT-KEY     TO RESP-IDFAKT-KEY                        
031000     ELSE                                                                 
031100       MOVE NEJ                 TO NYCKLAR-SW                             
031200       IF REQU-IDFAKT-KEY  NOT NUMERIC                                    
031300         MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                               
031400         MOVE '024'    TO RESP-IDMSG-ERROR                                
031500       ELSE                                                               
031600         MOVE 'IDFAKT' TO RESP-IDELMT-ERROR                               
031700         MOVE '126'    TO RESP-IDMSG-ERROR                                
031800       END-IF                                                             
031900     END-IF                                                               
032000                                                                          
032100*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
032200*    RETURNS FROM CA (FTG=54) TO US (DC=44, FTG=53)                       
032300*    IF REQU-IDUSER = 'PHCA4G1'                                           
032400*       AND REQU-IDDC-KEY = '44'                                          
032500*      MOVE '54'           TO REQU-IDFTG-KEY                              
032600*    END-IF                                                               
032700*    END FIX                                                              
032800     IF REQU-IDFTG-KEY NOT NUMERIC                                        
032900       MOVE NEJ              TO NYCKLAR-SW                                
033000       MOVE 'IDFTG'          TO RESP-IDELMT-ERROR                         
033100       MOVE '023'            TO RESP-IDMSG-ERROR                          
033200     ELSE                                                                 
033300       MOVE REQU-IDFTG-KEY   TO WS-IDFTG                                  
033400     END-IF                                                               
033500                                                                          
033600*    -- KONTROLL AV IDKUNDNR                                              
033700                                                                          
033800     IF REQU-IDKUNDNR-KEY NOT = ALL '+'                                   
033900       IF REQU-IDKUNDNR-KEY NUMERIC                                       
034000         MOVE REQU-IDKUNDNR-KEY   TO RESP-IDKUNDNR-KEY                    
034100                                     WS-IDKUNDNR                          
034200       ELSE                                                               
034300         MOVE NEJ                 TO NYCKLAR-SW                           
034400         MOVE 'IDKUNDNR'          TO RESP-IDELMT-ERROR                    
034500         MOVE '024'               TO RESP-IDMSG-ERROR                     
034600       END-IF                                                             
034700     END-IF                                                               
034800                                                                          
034900*    -- KONTROLL AV IDORDNR                                               
035000     IF REQU-IDORDNR-KEY NOT = ALL '+'                                    
035100       IF REQU-IDORDNR-KEY   NUMERIC                                      
035200         MOVE REQU-IDORDNR-KEY    TO RESP-IDORDNR-KEY                     
035300                                     WS-IDORDNR                           
035400       ELSE                                                               
035500         MOVE NEJ                 TO NYCKLAR-SW                           
035600         MOVE 'IDORDNR'           TO RESP-IDELMT-ERROR                    
035700         MOVE '024'               TO RESP-IDMSG-ERROR                     
035800       END-IF                                                             
035900     END-IF                                                               
036000                                                                          
036100*    -- KONTROLL AV IDKOLLI                                               
036200     IF REQU-IDKOLLI-KEY NOT  = ALL '+'                                   
036300       IF REQU-IDKOLLI-KEY   NUMERIC                                      
036400         MOVE REQU-IDKOLLI-KEY    TO RESP-IDKOLLI-KEY                     
036500                                     WS-IDKOLLI                           
036600       ELSE                                                               
036700         MOVE NEJ                 TO NYCKLAR-SW                           
036800         MOVE 'IDKOLLI'           TO RESP-IDELMT-ERROR                    
036900         MOVE '024'               TO RESP-IDMSG-ERROR                     
037000       END-IF                                                             
037100     END-IF                                                               
037200                                                                          
037300*    -- KONTROLL AV IDPRODNR                                              
037400     IF REQU-IDPRODNR-KEY NOT = ALL '+'                                   
037500       IF REQU-IDPRODNR-KEY   NUMERIC                                     
037600         MOVE REQU-IDPRODNR-KEY   TO RESP-IDPRODNR-KEY                    
037700                                     WS-IDPRODNR                          
037800       ELSE                                                               
037900         MOVE NEJ                 TO NYCKLAR-SW                           
038000         MOVE 'IDPRODNR'          TO RESP-IDELMT-ERROR                    
038100         MOVE '024'               TO RESP-IDMSG-ERROR                     
038200       END-IF                                                             
038300     END-IF                                                               
038400                                                                          
038500*    -- KONTROLL AV IDARTNR                                               
038600                                                                          
038700     IF REQU-IDARTNR-KEY NOT = ALL '+'                                    
038800       IF REQU-IDARTNR-KEY   NUMERIC                                      
038900         MOVE REQU-IDARTNR-KEY    TO RESP-IDARTNR-KEY                     
039000                                     WS-IDARTNR                           
039100       ELSE                                                               
039200         MOVE NEJ                 TO NYCKLAR-SW                           
039300         MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                    
039400         MOVE '024'               TO RESP-IDMSG-ERROR                     
039500       END-IF                                                             
039600     END-IF                                                               
039700                                                                          
039800     IF REQU-IDDC-KEY = ALL '+'                                           
039900       MOVE NEJ                 TO NYCKLAR-SW                             
040000       MOVE 'IDDC'   TO RESP-IDELMT-ERROR                                 
040100       MOVE '023'    TO RESP-IDMSG-ERROR                                  
040200     ELSE                                                                 
040300       MOVE REQU-IDDC-KEY       TO RESP-IDDC-KEY                          
040400     END-IF                                                               
040500                                                                          
040600     IF NYCKLAR-OK                                                        
040700       PERFORM BA-KOLLA-NYCKEL-SAMBAND                                    
040800     END-IF                                                               
040900                                                                          
041000     .                                                                    
041100     EJECT                                                                
041200 BA-KOLLA-NYCKEL-SAMBAND        SECTION.                                  
041300                                                                          
041400     IF WS-IDPRODNR              > ZERO                                   
041500       MOVE WS-IDPRODNR         TO W-IDPRODNR-MIN                         
041600                                   W-IDPRODNR-MAX                         
041700     ELSE                                                                 
041800       MOVE ZERO                TO W-IDPRODNR-MIN                         
041900       MOVE +9999999            TO W-IDPRODNR-MAX                         
042000     END-IF                                                               
042100                                                                          
042200     IF WS-IDKOLLI               > ZERO                                   
042300       MOVE WS-IDKOLLI          TO W-IDKOLLI-MIN                          
042400                                   W-IDKOLLI-MAX                          
042500     ELSE                                                                 
042600       MOVE ZERO                TO W-IDKOLLI-MIN                          
042700       MOVE +99999              TO W-IDKOLLI-MAX                          
042800     END-IF                                                               
042900                                                                          
043000     IF WS-IDARTNR               > ZERO                                   
043100       MOVE WS-IDARTNR          TO W-IDARTNR-MIN                          
043200                                   W-IDARTNR-MAX                          
043300     ELSE                                                                 
043400       MOVE ZERO                TO W-IDARTNR-MIN                          
043500       MOVE +999999999          TO W-IDARTNR-MAX                          
043600     END-IF                                                               
043700                                                                          
043800     IF WS-IDORDNR               > ZERO                                   
043900       MOVE WS-IDORDNR          TO W-IDORDNR-MIN                          
044000                                   W-IDORDNR-MAX                          
044100     ELSE                                                                 
044200       MOVE ZERO                TO W-IDORDNR-MIN                          
044300       MOVE 9999999             TO W-IDORDNR-MAX                          
044400     END-IF                                                               
044500                                                                          
044600     IF WS-IDKUNDNR              > ZERO                                   
044700       MOVE WS-IDKUNDNR         TO W-IDKUNDNR-MIN                         
044800                                   W-IDKUNDNR-MAX                         
044900     ELSE                                                                 
045000       MOVE ZERO                TO W-IDKUNDNR-MIN                         
045100       MOVE +9999999            TO W-IDKUNDNR-MAX                         
045200     END-IF                                                               
045300     .                                                                    
045400     EJECT                                                                
045500 F-LAES-VISA-INFO               SECTION.                                  
045600                                                                          
045700     PERFORM IMS-GU-WDL501                                                
045800                                                                          
045900     IF SEGMENT-SAKNAS                                                    
046000       MOVE 'IDFAKT'            TO RESP-IDELMT-ERROR                      
046100       MOVE '041'               TO RESP-IDMSG-ERROR                       
046200     ELSE                                                                 
046300       PERFORM IMS-GNP-WDL511                                             
046400       IF SEGMENT-SAKNAS                                                  
046500         MOVE 'IDFAKT'          TO RESP-IDELMT-ERROR                      
046600         MOVE '041'             TO RESP-IDMSG-ERROR                       
046700       ELSE                                                               
046800         PERFORM FA-KOLLA-IDFTG-SHOW-OK                                   
046900         IF INDATA-OK                                                     
047000           MOVE +0              TO WS-COUNT                               
047100           PERFORM FB-INIT-RESP-LINES                                     
047200         END-IF                                                           
047300       END-IF                                                             
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 FA-KOLLA-IDFTG-SHOW-OK         SECTION.                                  
047800                                                                          
047900     MOVE JA                     TO INDATA-SW                             
048000     MOVE FAK-IDDISTR            TO TEST-IDDISTR                          
049000     EVALUATE     TRUE                                                    
050000       WHEN IDFTG-US                                                      
050100         IF DIST34-NDC-NA       OR                                        
050200            DIST34-USA-NDC      OR                                        
050300            DIST34-NDC-BYPASS                                             
050400                                                                          
050500            CONTINUE                                                      
050600         ELSE                                                             
050700            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
050800            MOVE NEJ             TO INDATA-SW                             
050900         END-IF                                                           
051000       WHEN IDFTG-CA                                                      
051100         IF DIST34-NDC-NA       OR                                        
051200            DIST34-KANADA-NDC   OR                                        
051300            DIST34-NDC-BYPASS                                             
051400                                                                          
051500            CONTINUE                                                      
051600         ELSE                                                             
051700            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
051800            MOVE NEJ             TO INDATA-SW                             
051900         END-IF                                                           
052000       WHEN IDFTG-CN                                                      
052100         IF DIST34-KINA-NDC   OR                                          
052200            DIST34-KINA-LDC                                               
052300                                                                          
052400            CONTINUE                                                      
052500         ELSE                                                             
052600            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
052700            MOVE NEJ             TO INDATA-SW                             
052800         END-IF                                                           
052900       WHEN IDFTG-IN                                                      
053000         IF DIST34-INDIA-NDC                                              
053100                                                                          
053200            CONTINUE                                                      
053300         ELSE                                                             
053400            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
053500            MOVE NEJ             TO INDATA-SW                             
053600         END-IF                                                           
053700       WHEN IDFTG-KR                                                      
053800         IF DIST34-KOREA-NDC                                              
053900                                                                          
054000            CONTINUE                                                      
054100         ELSE                                                             
054200            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
054300            MOVE NEJ             TO INDATA-SW                             
054400         END-IF                                                           
054500       WHEN IDFTG-MY                                                      
054600         IF DIST34-MALAYSIA-NDC                                           
054700                                                                          
054800            CONTINUE                                                      
054900         ELSE                                                             
055000            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
055100            MOVE NEJ             TO INDATA-SW                             
055200         END-IF                                                           
055300       WHEN IDFTG-TR                                                      
055400         IF DIST34-TURKEY-NDC                                             
055500                                                                          
055600            CONTINUE                                                      
055700         ELSE                                                             
055800            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
055900            MOVE NEJ             TO INDATA-SW                             
056000         END-IF                                                           
056100       WHEN IDFTG-MX                                                      
056200         IF DIST34-MEXICO-NDC                                             
056300                                                                          
056400            CONTINUE                                                      
056500         ELSE                                                             
056600            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
056700            MOVE NEJ             TO INDATA-SW                             
056800         END-IF                                                           
056801       WHEN IDFTG-BR                                                      
056802         IF DIST34-BRAZIL-NDC                                             
056803                                                                          
056804            CONTINUE                                                      
056805         ELSE                                                             
056806            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
056807            MOVE NEJ             TO INDATA-SW                             
056808         END-IF                                                           
056810       WHEN IDFTG-ZA                                                      
056820         IF DIST34-SOUTH-AFRICA-NDC                                       
056830                                                                          
056840            CONTINUE                                                      
056850         ELSE                                                             
056860            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
056870            MOVE NEJ             TO INDATA-SW                             
056880         END-IF                                                           
056900       WHEN IDFTG-PV                                                      
057000         IF DIST34-NDC-NA       OR                                        
057100            DIST34-KANADA-NDC   OR                                        
057200            DIST34-NDC-BYPASS   OR                                        
057300            DIST34-USA-NDC      OR                                        
057400            DIST34-KINA-NDC     OR                                        
057500            DIST34-KINA-LDC     OR                                        
057600            DIST34-INDIA-NDC    OR                                        
057700            DIST34-KOREA-NDC    OR                                        
057800            DIST34-TURKEY-NDC   OR                                        
057900            DIST34-MALAYSIA-NDC OR                                        
058000            DIST34-MEXICO-NDC   OR                                        
058001            DIST34-BRAZIL-NDC   OR                                        
058010            DIST34-SOUTH-AFRICA-NDC                                       
058100                                                                          
058200            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
058300            MOVE NEJ             TO INDATA-SW                             
058400         END-IF                                                           
058500       WHEN OTHER                                                         
058600            MOVE ERR-NOT-AUTHORIZED  TO RESP-IDMSG-ERROR                  
058700            MOVE NEJ             TO INDATA-SW                             
058800     END-EVALUATE                                                         
058900     .                                                                    
059000     EJECT                                                                
059100 FB-INIT-RESP-LINES SECTION.                                              
059200                                                                          
059300     MOVE +0                  TO INDX                                     
059400     PERFORM UNTIL INDX > MAX-INDX                                        
059500       MOVE FAKC-IDPRODNR TO W-IDPRODNR                                   
059600       MOVE FAKC-IDKOLLI  TO W-IDKOLLI                                    
059700       PERFORM IMS-GNP-WDL521                                             
059800       PERFORM UNTIL SEGMENT-SAKNAS                                       
059900                  OR INDX = MAX-INDX                                      
060000         ADD  +1              TO INDX                                     
061000         MOVE FAKC-IDDISTR    TO RESP-IDDISTR (INDX)                      
061100         MOVE FAKC-IDKUNDNR   TO RESP-IDKUNDNR(INDX)                      
061200         MOVE FAKC-IDORDNR7   TO RESP-IDORDNR7(INDX)                      
061300         MOVE FAKC-IDPRODNR   TO RESP-IDPRODNR(INDX)                      
061400         MOVE FAK-IDDC        TO RESP-IDDC   (INDX)                       
061500         MOVE FAKC-IDKOLLI    TO RESP-IDKOLLI (INDX)                      
061600         MOVE FAKL-IDARTNR    TO RESP-IDARTNR (INDX)                      
061700         MOVE FAKL-KVBEART-Q  TO RESP-KVBEART (INDX)                      
061800         MOVE FAKL-KVAVBART   TO RESP-KVAVBART(INDX)                      
061900         MOVE FAKL-KVLEVART   TO RESP-KVLEVART(INDX)                      
062000         MOVE FAKL-IDLEVNR    TO RESP-IDLEVNR (INDX)                      
062100                                                                          
062200         PERFORM IMS-GNP-WDL521                                           
062300         ADD 1            TO WS-COUNT                                     
062400       END-PERFORM                                                        
062500       IF SEGMENT-SAKNAS                                                  
062600         PERFORM IMS-GNP-WDL511                                           
062700         IF SEGMENT-SAKNAS                                                
062800           COMPUTE INDX = MAX-INDX + 1                                    
062900         END-IF                                                           
063000       ELSE                                                               
064000         ADD  +1              TO INDX                                     
065000       END-IF                                                             
065100     END-PERFORM                                                          
065200     MOVE WS-COUNT  TO RESP-KVRADER                                       
065300     .                                                                    
065400     EJECT                                                                
065500*    --- DISPATCHER SECTIONS                                              
065600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
065700                                                                          
065800     MOVE 'GETARG'               TO SUB-KDFUNC                            
065900     MOVE 'CARPARTS.LDC.INVOICEQUERY'  TO SUB-ADDISPABS                   
066000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
067000                                                                          
067100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
067200                                                                          
067300     IF SUB-KDRC > 0                                                      
067400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
067500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
067600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
067700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
067800     END-IF                                                               
067900     .                                                                    
068000                                                                          
068100 S02-RETURN-RESPONSE SECTION.                                             
068200                                                                          
068300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
068400     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
068500                                                                          
068600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
068700                                                                          
068800     IF SUB-KDRC > 0                                                      
068900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
069000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
069100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
069200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
069300     END-IF                                                               
069400     .                                                                    
069500     EJECT                                                                
069600* --- IMS SEKTIONER ---                                                   
069700                                                                          
069800 IMS-GU-WDL501                 SECTION.                                   
069900                                                                          
070000     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
071000          DELIMITED BY SIZE INTO SSA1                                     
071100     MOVE '  GE' TO GODK-STATUSKODER                                      
071200     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-AREA-WDL501 SSA1               
071300     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
071400     PERFORM IMS-STATUSKONTROLL                                           
071500     .                                                                    
071600     EJECT                                                                
071700 IMS-GNP-WDL511                 SECTION.                                  
071800                                                                          
071900     STRING 'WDL511  (IDPRODNR>=' W-IDPRODNR-MIN-X                        
072000                    '&IDPRODNR<=' W-IDPRODNR-MAX-X                        
072100                    '&IDKOLLI >=' W-IDKOLLI-MIN-X                         
072200                    '&IDKOLLI <=' W-IDKOLLI-MAX-X                         
072300                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
072400                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X                        
072500                    '&IDORDNR7>=' W-IDORDNR-MIN-X                         
072600                    '&IDORDNR7<=' W-IDORDNR-MAX-X ')'                     
072700          DELIMITED BY SIZE INTO SSA1                                     
072800     MOVE '  GE' TO GODK-STATUSKODER                                      
072900     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL511 SSA1              
073000     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     .                                                                    
073300     EJECT                                                                
073400 IMS-GNP-WDL521                 SECTION.                                  
073500                                                                          
073600     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
073700          DELIMITED BY SIZE INTO SSA1                                     
073800     STRING 'WDL521  (WDL521KY>=' W-WDL521KY-MIN-X                        
073900                    '&WDL521KY<=' W-WDL521KY-MAX-X ')'                    
074000          DELIMITED BY SIZE INTO SSA2                                     
074100     MOVE '  GE' TO GODK-STATUSKODER                                      
074200     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL521 SSA1 SSA2         
074300     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
074400     PERFORM IMS-STATUSKONTROLL                                           
074500     .                                                                    
074600     EJECT                                                                
074700                                                                          
074800 IMS-STATUSKONTROLL             SECTION.                                  
074900                                                                          
075000     SET STATUS-IX TO 1                                                   
075100     SEARCH GODK-STATUS                                                   
075200       AT END                                                             
075300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
075400         DELIMITED BY SIZE INTO FELTEXT                                   
075500         CALL FELLOG                                                      
075600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
075700         CONTINUE                                                         
075800     END-SEARCH                                                           
075900     .                                                                    
