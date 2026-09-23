000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1021310.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   APRIL 90.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*      - VISA STRUKTURRADER.                                              
001100*                                                                         
001200*      - VISAR INFORMATION OM STRUKTUREN SAMT ALLA ARTIKELRADER           
001300*        SOM INGÅR I STRUKTUREN OCH INTE ÄR HISTORIKRADER                 
001400*        - ALLTSÅ NU OCH FRAMÅT                                           
001500*                                                                         
001600*      - RAD SOM HAR STRUKTURTYP KAN MARKERAS FÖR ATT VISA RADENS         
001700*        STRUKTUR . RETURFUNKTION MED PF-TANGENT TILL URSPRUNGLIG         
001800*        STRUKTUR.                                                        
001900*                                                                         
002000*      - RAD KAN MARKERAS FÖR ATT VISA RADEN BILD 1212. RETUR MED         
002100*        MED PF-TANGENT .                                                 
002200*                                                                         
002300*      - LÄSER WDD1 BENREG (WDD3) OCH RASA                                
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W1T213                                              
002700*        REQU:        W10213I1                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        RESP:        W10213O1                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700*    -COPY WY2000W1                                                       
003800     SKIP3                                                                
003900 77  IDPGM                   PIC X(8)    VALUE 'W1021310'.                
004000 77  JA                      PIC X       VALUE 'J'.                       
004100 77  NEJ                     PIC X       VALUE 'N'.                       
004200 77  S-IX                    PIC S9(9)   VALUE +1   COMP SYNC.            
004300 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
004400 77  SPAR-INDX               PIC S9(9)   VALUE +0   COMP SYNC.            
004500 77  SPAR-IDRADNR            PIC S9(9)   VALUE ZERO COMP-3.               
004600 77  INDXNOT                 PIC S9(9)   VALUE +0   COMP SYNC.            
004700 77  MAX-TESTRNOT            PIC S9(9)   VALUE +2.                        
004800 77  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
004900 77  IDARTNR-KOLL-WS         PIC S9(9)   VALUE ZERO  COMP-3.              
005000 77  IDARTNR-KONV-WS         PIC S9(9)   VALUE ZERO  COMP-3.              
005100 77  IDRADNR-WS              PIC X(4)    VALUE SPACE.                     
005200 77  IDSKYLT-WS              PIC X(3)    VALUE SPACE.                     
005400 77  TEST-IDARTNR            PIC 9(9)    VALUE ZERO.                      
005500 77  SPAR-RADNR              PIC S9(5)   VALUE ZERO  COMP-3.              
005600 77  SPAR-KDBENHOM           PIC S9      VALUE ZERO  COMP-3.              
005700 77  SPAR-RAD-KDBENHOM       PIC S9      VALUE ZERO  COMP-3.              
005800 77  SATS-STR-RA             PIC X(3)    VALUE '222'.                     
005900 77  SATS-STR-RB             PIC X(3)    VALUE '223'.                     
006000 77  SATS-STR-BERPV          PIC X(3)    VALUE '226'.                     
006100 77  SATS-STR-CARP           PIC X(3)    VALUE '221'.                     
006110 77  C-PART-LINE             PIC X(1)    VALUE 'P'.                       
006120 77  C-SUPPL-PART-LINE       PIC X(1)    VALUE 'S'.                       
006130 77  C-NOTE-LINE             PIC X(1)    VALUE 'N'.                       
006140 01  W-ARB-SALDO             PIC S9(7)   VALUE ZERO COMP-3.               
006150 01  W-TIAAAAVV.                                                          
006160   03 W-TISEKEL              PIC  9(2)    VALUE ZERO.                     
006170   03 W-TIAA                 PIC  9(2)    VALUE ZERO.                     
006180   03 W-TIVV                 PIC  9(2)    VALUE ZERO.                     
006200                                                                          
006300 01  SPAR-AAVV               PIC 9(4).                                    
006400*                                                                         
006500 01  FILLER REDEFINES SPAR-AAVV.                                          
006600   03 SPAR-AA                PIC 9(2).                                    
006700   03 SPAR-VV                PIC 9(2).                                    
006800                                                                          
006900 01  DAGENS-DATUM            PIC 9(6).                                    
007000*                                                                         
007100 01  FILLER REDEFINES DAGENS-DATUM.                                       
007200   03  DAGENS-DATUM-AR       PIC 9(2).                                    
007300   03  DAGENS-DATUM-MANAD    PIC 9(2).                                    
007400   03  DAGENS-DATUM-DAG      PIC 9(2).                                    
007500                                                                          
007600 77  INDATA-SW               PIC X       VALUE 'J'.                       
007700   88  INDATA-OK                         VALUE 'J'.                       
007800   88  INDATA-FEL                        VALUE 'N'.                       
007900                                                                          
008000 77  LAES-SW                 PIC X       VALUE 'J'.                       
008100   88  LAES-UNIK                         VALUE 'J'.                       
008200   88  LAES-ALLA                         VALUE 'N'.                       
008300                                                                          
008400 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
008500   88  NYCKLAR-OK                        VALUE 'J'.                       
008600   88  NYCKLAR-FEL                       VALUE 'N'.                       
008700                                                                          
008800 77  INPUT-SW                PIC X       VALUE 'J'.                       
008900   88  INPUT-FINNS                       VALUE 'J'.                       
009000   88  INPUT-FINNS-EJ                    VALUE 'N'.                       
009100                                                                          
009200 77  BEN-SW                  PIC X       VALUE 'J'.                       
009300   88  BENAMNING-FINNS                   VALUE 'J'.                       
009400   88  BENAMNING-FINNS-EJ                VALUE 'N'.                       
009500                                                                          
009600 77  FLYTT-SW                PIC X       VALUE 'J'.                       
009700   88  FLYTT-OK                          VALUE 'J'.                       
009800   88  FLYTT-EJ-OK                       VALUE 'N'.                       
009900                                                                          
010000 77  ALLT-SW                 PIC X       VALUE 'J'.                       
010100   88  ALLT-OK                           VALUE 'J'.                       
010200                                                                          
010300 77  STATUS-SW               PIC X       VALUE 'J'.                       
010400   88  STATUS-OK                         VALUE 'J'.                       
010500   88  STATUS-EJ-OK                      VALUE 'N'.                       
010600                                                                          
010700 77  FORTSATTNING-SW         PIC X       VALUE 'J'.                       
010800   88  FORTSATTNING-OK                   VALUE 'J'.                       
010900   88  FORTSATTNING-EJ-OK                VALUE 'N'.                       
011000                                                                          
011100 01  ALL-SPACE.                                                           
011200   03  FILLER                PIC X(50)   VALUE SPACE.                     
011300 01  ALL-PLUS.                                                            
011400   03  FILLER                PIC X(50)   VALUE ALL '+'.                   
011500                                                                          
011600     EJECT                                                                
011700                                                                          
011800 01  UTRAD                   PIC X(72)   VALUE SPACE.                     
011900 01  FILLER REDEFINES UTRAD.                                              
012000   03 IDREANTPSA          PIC Z9(1).9(3).                                 
012100   03 FILLER              PIC X(1).                                       
012200   03 IDLEVNR             PIC X(5).                                       
012300   03 FILLER              PIC X(1).                                       
012400   03 BELEVART            PIC X(30).                                      
012500   03 FILLER              PIC X(1).                                       
012600   03 IBEART              PIC X(15).                                      
012700   03 FILLER              PIC X(1).                                       
012800   03 STRTYP              PIC X(1).                                       
012900   03 FILLER              PIC X(2).                                       
013000   03 STATUSKOD           PIC X(1).                                       
013100   03 FILLER              PIC X(1).                                       
013200   03 AARVECKA            PIC Z(4).                                       
013300   03 FILLER              PIC X(3).                                       
013400 01  FILLER REDEFINES UTRAD.                                              
013500   03 AIDREANTPSA         PIC Z9(1).9(3).                                 
013600   03 FILLER              PIC X(7).                                       
013700   03 IDARTNR             PIC Z(9).                                       
013800   03 FILLER              PIC X(22).                                      
013900   03 BEART               PIC X(15).                                      
014000   03 FILLER              PIC X(1).                                       
014100   03 ASTRTYP             PIC X(1).                                       
014200   03 FILLER              PIC X(2).                                       
014300   03 STATKOD             PIC X(1).                                       
014400   03 FILLER              PIC X(1).                                       
014500   03 AAVV                PIC Z(4).                                       
014600   03 FILLER              PIC X(2).                                       
014700   03 FARLIG              PIC X(1).                                       
014800 01  FILLER REDEFINES UTRAD.                                              
014900   03 INFORAD             PIC X(70).                                      
015000   03 FORTS               PIC X(1).                                       
015100   03 FILLER              PIC X(1).                                       
015200                                                                          
015300 01  MESSAGE-CODES.                                                       
015400   03  INF-MORE-INFO-EXISTS  PIC X(3)  VALUE '011'.                       
015500   03  ERR-WRONG-KEYS        PIC X(3)  VALUE '022'.                       
015600   03  ERR-PART-MISSING      PIC X(3)  VALUE '025'.                       
015700   03  ERR-LINE-MISSING      PIC X(3)  VALUE '025'.                       
015800   03  ERR-CORR-HILITE-FLDS  PIC X(3)  VALUE '020'.                       
015900   03  ERR-STRUCTURE-MISSING PIC X(3)  VALUE '025'.                       
016000   03  ERR-PF15-FOR-SPLIT-UP PIC X(3)  VALUE '416'.                       
016100   03  ERR-STRUCT-NOT-COMP   PIC X(3)  VALUE '417'.                       
016200*                                                                         
016300     EJECT                                                                
016400 01  GENERELLA-SUBPROGRAM.                                                
016500   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
016600   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
016700   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
016800   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
016900   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
017000   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
017100     EJECT                                                                
017200*01  -COPY WWLAND03                                                       
017300     EJECT                                                                
017400*01  -COPY WDECAREA                                                       
017500     EJECT                                                                
017600*01  -COPY WMEDAREA                                                       
017700     EJECT                                                                
017800*01  -COPY WDATAREA                                                       
017900     EJECT                                                                
018000*                    ****  PARAMETRAR TILL W005INIT                       
018100*01  -COPY WMSGINIT                                                       
018200     EJECT                                                                
018300*01  -COPY WMFSAREA                                                       
018400     EJECT                                                                
018500******************************************************************        
018600*                                                                         
018700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018800*                                                                         
018900 01  IMS-WS.                                                              
019000   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
019100     SKIP3                                                                
019200 01  NYCKLAR-TILL-DLI.                                                    
019300   03  W-IDDC-X.                                                          
019400     05  W-IDDC              PIC  X(2)   VALUE SPACE.                     
019500   03  W-IDARTNR-X.                                                       
019600     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
019700   03  W-IDSKYLT-X.                                                       
019800     05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                     
019900   03  W-KDSTRRAD-X.                                                      
020000     05  W-KDSTRRAD          PIC X       VALUE SPACE.                     
020100   03  W-IDRADNR-X.                                                       
020200     05  W-IDRADNR           PIC  S9(5)  VALUE ZERO  COMP-3.              
020300   03  W-BEART-X.                                                         
020400     05  W-BEART             PIC  X(25)  VALUE SPACE.                     
020410   03  W-DABEHOV-MIN-X.                                                   
020420     05  W-DABEHOV-MIN       PIC   9(6)  VALUE ZERO.                      
020430   03  W-DABEHOV-MAX-X.                                                   
020440     05  W-DABEHOV-MAX       PIC   9(6)  VALUE ZERO.                      
020500     SKIP3                                                                
020600*                        **** STATUS-KOD FRÅN IMS                         
020700   03  STATUS-WS             PIC XX.                                      
020800     88  SEGMENT-FINNS                   VALUE '  '.                      
020900     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
021000     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
021100     SKIP3                                                                
021200   03  GODK-STATUSKODER.                                                  
021300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021310   03    SW-ARTC11-SEGM-FINNS PIC X    VALUE 'N'.                         
021320     88  ARTC11-SEGMENT-FINNS          VALUE 'J'.                         
021400     SKIP3                                                                
021500 01    SSA1                  PIC X(64).                                   
021600 01    SSA2                  PIC X(64).                                   
021700 01    SSA3                  PIC X(64).                                   
021800     EJECT                                                                
021900*                            IMS FUNKTIONSKODER                           
022000*01    -COPY W0003                                                        
022100     EJECT                                                                
022200*                            DLI INPUT-OUTPUT AREA                        
022300 01  DLI-IO-AREA.                                                         
022400   03  IO-AREA               PIC X(250)  VALUE SPACE.                     
022500     SKIP3                                                                
022600*  03  WLSATB01  -COPY WDJ101  -PRE SATB-  -RED IO-AREA.                  
022700     EJECT                                                                
022800*  03  WLSATB11  -COPY WDJ111  -PRE SATB-  -RED IO-AREA.                  
022900     EJECT                                                                
023000*  03  WLSATB22  -COPY WDJ122  -PRE SATB-  -RED IO-AREA.                  
023100     EJECT                                                                
023200 01  DLI-IO-AREA-1.                                                       
023300   03  IO-AREA-1             PIC X(928)  VALUE SPACE.                     
023400*  03  WLARTC01  -COPY WDK601                -RED IO-AREA-1.              
023500     EJECT                                                                
023600*  03  WLARTC11  -COPY WDK611                -RED IO-AREA-1.              
023700     EJECT                                                                
023800     SKIP3                                                                
023900 01  DLI-IO-AREA-2.                                                       
024000   03  IO-AREA-2             PIC X(250)  VALUE SPACE.                     
024100     SKIP3                                                                
024200*  03  WLBENA01  -COPY WDD301  -PRE BEN-   -RED IO-AREA-2.                
024300     EJECT                                                                
024400*  03  WLBENA11  -COPY WDD311  -PRE BEN-   -RED IO-AREA-2.                
024500     EJECT                                                                
024600 01  DLI-IO-AREA-3.                                                       
024700   03  IO-AREA-3             PIC X(350)  VALUE SPACE.                     
024800     SKIP3                                                                
024900*  03  WDK701    -COPY WDK701              -RED IO-AREA-3.                
025000     EJECT                                                                
025100*  03  WDK711    -COPY WDK711              -RED IO-AREA-3.                
025200     EJECT                                                                
025210 01  DLI-IO-AREA-4.                                                       
025220   03  IO-AREA-4             PIC X(250)  VALUE SPACE.                     
025230     SKIP3                                                                
025240*  03  WDK901    -COPY WDK901              -RED IO-AREA-4.                
025250     EJECT                                                                
025260*  03  WDK911    -COPY WDK911              -RED IO-AREA-4.                
025270     EJECT                                                                
025300                                                                          
025400 LINKAGE SECTION.                                                         
025500 01  REQU-AREA.                                                           
025600*    03 -COPY WZ01REQU                                                    
025700*    03 -COPY W10213I1                                                    
025800     EJECT                                                                
025900 01  RESP-AREA.                                                           
026000*    03 -COPY WZ01RESP                                                    
026100*    03 -COPY W10213O1                                                    
026200     EJECT                                                                
026300 01  MAX-KVRADER                 PIC S9(4) COMP.                          
026400     EJECT                                                                
026500*01  -COPY W0008     -PRE SATB-                                           
026600     05  FILLER              PIC X.                                       
026700     EJECT                                                                
026800*01  -COPY W0008     -PRE BENA-                                           
026900     05  FILLER              PIC X.                                       
027000     EJECT                                                                
027100*01  -COPY W0008     -PRE BENA-A-                                         
027200     05  FILLER              PIC X.                                       
027300     EJECT                                                                
027400*01  -COPY W0008     -PRE ARTC-                                           
027500     05  FILLER              PIC X.                                       
027600     EJECT                                                                
027700*01  -COPY W0008     -PRE WDK7-                                           
027800     05  FILLER              PIC X.                                       
027900     EJECT                                                                
027910*01  -COPY W0008     -PRE WDK9-                                           
027920     05  FILLER              PIC X.                                       
027930     EJECT                                                                
028000 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
028100                           SATB-PCB  BENA-PCB  BENA-A-PCB                 
028200                           ARTC-PCB WDK7-PCB WDK9-PCB.                    
028300     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA MAX-KVRADER                
028400                           SATB-PCB  BENA-PCB  BENA-A-PCB                 
028500                           ARTC-PCB WDK7-PCB WDK9-PCB.                    
028600                                                                          
028700     PERFORM A-INIT                                                       
028800     PERFORM B-KOLLA-NYCKLAR                                              
028900     IF NYCKLAR-OK                                                        
029000       COMPUTE IDARTNR-KONV-WS = +999999999                               
029100                                 - IDARTNR-KOLL-WS                        
029200       MOVE IDARTNR-KONV-WS      TO W-IDARTNR                             
029300       PERFORM IMS-GU-SATB-ART                                            
029400       IF SEGMENT-FINNS                                                   
029500         IF SATB-STR-IDUSER = REQU-IDUSER                                 
029600           MOVE JA               TO FORTSATTNING-SW                       
029700           MOVE ERR-STRUCT-NOT-COMP                                       
029800                                 TO RESP-IDMSG-ERROR                      
029900         ELSE                                                             
030000           MOVE REQU-IDARTNR-KEY TO W-IDARTNR                             
030100           PERFORM IMS-GU-SATB-ART                                        
030200           IF SEGMENT-FINNS                                               
030300             MOVE JA             TO FORTSATTNING-SW                       
030400           ELSE                                                           
030500             MOVE NEJ            TO FORTSATTNING-SW                       
030600           END-IF                                                         
030700         END-IF                                                           
030800       ELSE                                                               
030900         MOVE REQU-IDARTNR-KEY   TO W-IDARTNR                             
031000         PERFORM IMS-GU-SATB-ART                                          
031100         IF SEGMENT-FINNS                                                 
031200           MOVE JA               TO FORTSATTNING-SW                       
031300         ELSE                                                             
031400           MOVE NEJ              TO FORTSATTNING-SW                       
031500         END-IF                                                           
031600       END-IF                                                             
031700                                                                          
031800       IF FORTSATTNING-OK                                                 
031900         IF REQU-FIRST OR                                                 
032000            (REQU-QUERY AND REQU-IDMSGVER = '001')                        
032100           PERFORM D-BEHANDLA-ARTIKEL                                     
032200         ELSE                                                             
032300           IF REQU-NEXT                                                   
032400             PERFORM E-NAESTA-SIDA                                        
032500           ELSE                                                           
032600             PERFORM F-SAMMA-SIDA                                         
032700           END-IF                                                         
032800         END-IF                                                           
032900         IF NYCKLAR-OK AND ALLT-OK                                        
033000           PERFORM G-LAES-VISA-INFO                                       
033100           PERFORM MFS-RENSA-FAELT-INMAT                                  
033200         END-IF                                                           
033300       ELSE                                                               
033400         MOVE ERR-PART-MISSING   TO RESP-IDMSG-ERROR                      
033500         MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                     
033600         PERFORM MFS-RENSA-FAELT-IN                                       
033700         PERFORM MFS-RENSA-FAELT-UT                                       
033800         PERFORM MFS-FORM-ATTR                                            
033900         MOVE ZERO               TO RESP-KVRADER                          
034000         MOVE NEJ                TO NYCKLAR-SW                            
034100       END-IF                                                             
034200     ELSE                                                                 
034300       PERFORM MFS-FORM-ATTR                                              
034400     END-IF                                                               
034500                                                                          
034600     MOVE ZERO                   TO RETURN-CODE                           
034700     GOBACK                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 A-INIT SECTION.                                                          
035100                                                                          
035200     MOVE ALL '+'                TO RESP-W10213O1                         
035300                                                                          
035400     PERFORM MFS-FORM-ATTR                                                
035500                                                                          
035600     MOVE 001                    TO RESP-IDMSGVER                         
035700     MOVE ALL-SPACE              TO RESP-IDMSG-ERROR                      
035800                                    RESP-IDMSG-INFO                       
035900                                    RESP-IDELMT-ERROR                     
036000                                                                          
036100     MOVE REQU-KVRADER           TO RESP-KVRADER                          
036200                                                                          
036300     ACCEPT DAGENS-DATUM       FROM DATE                                  
036400     .                                                                    
036500     EJECT                                                                
036600                                                                          
036700 B-KOLLA-NYCKLAR SECTION.                                                 
036800                                                                          
036900     MOVE JA                     TO NYCKLAR-SW                            
037000                                                                          
037100     MOVE REQU-IDDC              TO W-IDDC                                
037200                                                                          
037300     IF (REQU-IDARTNR-KEY NUMERIC) AND (REQU-IDARTNR-KEY > ZERO)          
037400       AND (REQU-IDARTNR-KEY < 99999999)                                  
037500       MOVE REQU-IDARTNR-KEY     TO W-IDARTNR                             
037600                                    IDARTNR-KOLL-WS                       
037700     ELSE                                                                 
037800       MOVE NEJ                  TO NYCKLAR-SW                            
037900       IF REQU-IDARTNR-KEY NOT NUMERIC                                    
038000         MOVE ERR-WRONG-KEYS     TO RESP-IDMSG-ERROR                      
038100       ELSE                                                               
038200         MOVE ERR-PART-MISSING   TO RESP-IDMSG-ERROR                      
038300         MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                     
038400       END-IF                                                             
038500     END-IF                                                               
038600                                                                          
038700     IF REQU-IDSKYLT-KEY = SPACE OR ALL '+'                               
038800       IF REQU-IDSPRAK = 'SV'                                             
038900         MOVE 'S '               TO IDSKYLT-WS                            
039000       ELSE                                                               
039100         MOVE 'GB'               TO IDSKYLT-WS                            
039200       END-IF                                                             
039300     ELSE                                                                 
039400       MOVE REQU-IDSKYLT-KEY     TO IDSKYLT-WS                            
039500     END-IF                                                               
039600                                                                          
039700     SET WWLAND03-IX             TO +1                                    
039800     SEARCH WWLAND03-IDSKYLT-RAD                                          
039900       AT END                                                             
040000         MOVE NEJ                TO NYCKLAR-SW                            
040100         MOVE ERR-WRONG-KEYS     TO RESP-IDMSG-ERROR                      
040200       WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = IDSKYLT-WS                    
040300         CONTINUE                                                         
040400     END-SEARCH                                                           
040500     MOVE IDSKYLT-WS             TO W-IDSKYLT                             
040600                                                                          
040700     IF REQU-IDRADNR-KEY = SPACE OR ALL '+'                               
040800       MOVE ZERO                 TO REQU-IDRADNR-KEY                      
040900     END-IF                                                               
041000                                                                          
041100     IF REQU-IDRADNR-KEY NUMERIC                                          
041200       MOVE REQU-IDRADNR-KEY     TO W-IDRADNR                             
041300       IF REQU-IDRADNR-KEY = ZERO                                         
041400         MOVE '00010'            TO SPAR-RADNR                            
041500       ELSE                                                               
041600         MOVE REQU-IDRADNR-KEY   TO SPAR-RADNR                            
041700       END-IF                                                             
041800     ELSE                                                                 
041900       MOVE NEJ                  TO NYCKLAR-SW                            
042000       MOVE ERR-WRONG-KEYS       TO RESP-IDMSG-ERROR                      
042100     END-IF                                                               
042200                                                                          
042300     IF NYCKLAR-FEL                                                       
042400       MOVE ZERO                 TO RESP-KVRADER                          
042500       PERFORM MFS-RENSA-FAELT-IN                                         
042600       PERFORM MFS-RENSA-FAELT-UT                                         
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000                                                                          
043100 D-BEHANDLA-ARTIKEL SECTION.                                              
043200                                                                          
043300     IF SEGMENT-FINNS                                                     
043400       MOVE SATB-STR-KDBENHOM    TO SPAR-KDBENHOM                         
043500       IF (W-IDSKYLT = 'S  ') AND                                         
043600         (SATB-STR-BEART-SVE = SPACE)                                     
043700         PERFORM IMS-GU-BEN-SEQ                                           
043800         IF SEGMENT-FINNS                                                 
043900           MOVE BEN-TEXT-BEART   TO RESP-BEART                            
044000         ELSE                                                             
044100           MOVE SPACE            TO RESP-BEART                            
044200         END-IF                                                           
044300       ELSE                                                               
044400         MOVE SATB-STR-BEART-SVE TO RESP-BEART                            
044500       END-IF                                                             
044600                                                                          
044700       IF W-IDSKYLT NOT = 'S  '                                           
044800         IF SATB-STR-BEART-SVE NOT = SPACE                                
044900           MOVE 'S'              TO W-IDSKYLT                             
045000           MOVE SATB-STR-BEART-SVE                                        
045100                                 TO W-BEART                               
045200           PERFORM IMS-GU-BENA01-ASEQ                                     
045300           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
045400                         (BEN-BEN-KDHOMONYM = SPAR-KDBENHOM)              
045500             IF SEGMENT-FINNS                                             
045600               IF BEN-BEN-KDHOMONYM = SPAR-KDBENHOM                       
045700                 MOVE JA         TO BEN-SW                                
045800               ELSE                                                       
045900                 PERFORM IMS-GN-BENA01-ASEQ-NEXT                          
046000               END-IF                                                     
046100             ELSE                                                         
046200               MOVE NEJ          TO BEN-SW                                
046300               MOVE SPACE        TO RESP-BEART                            
046400             END-IF                                                       
046500           END-PERFORM                                                    
046600                                                                          
046700           IF SEGMENT-FINNS                                               
046800             MOVE IDSKYLT-WS     TO W-IDSKYLT                             
046900             PERFORM IMS-GNP-BENA11-ASEQ                                  
047000             IF SEGMENT-FINNS                                             
047100               MOVE BEN-TEXT-BEART                                        
047200                                 TO RESP-BEART                            
047300             ELSE                                                         
047400               MOVE SPACE        TO RESP-BEART                            
047500             END-IF                                                       
047600           END-IF                                                         
047700         ELSE                                                             
047800           PERFORM IMS-GU-BEN-SEQ                                         
047900           IF SEGMENT-FINNS                                               
048000             MOVE BEN-TEXT-BEART TO RESP-BEART                            
048100           ELSE                                                           
048200             MOVE SPACE          TO RESP-BEART                            
048300           END-IF                                                         
048400         END-IF                                                           
048500       END-IF                                                             
048600                                                                          
048700       IF SATB-STR-KDPRODSL NOT = ZERO AND                                
048800          SATB-STR-IDFKNGRP NOT = ZERO                                    
048900         MOVE SATB-STR-KDPRODSL  TO RESP-KDPRODSL                         
049000         MOVE SATB-STR-IDFKNGRP  TO RESP-IDFKNGRP                         
049100         MOVE SATB-STR-IDSTRTYP  TO RESP-IDSTRTYP                         
049200       ELSE                                                               
049300         MOVE SATB-STR-IDSTRTYP  TO RESP-IDSTRTYP                         
049400         PERFORM IMS-GU-ARTC01                                            
049500         IF SEGMENT-FINNS                                                 
049600           MOVE ART-KDPRODSL     TO RESP-KDPRODSL                         
049700           MOVE ART-IDFKNGRP     TO RESP-IDFKNGRP                         
049800           PERFORM IMS-GU-ARTC11                                          
049900           IF SEGMENT-FINNS                                               
050000             MOVE CLAG-KDPSLLOC                                           
050100                                 TO RESP-KDPSLLOC                         
050200           END-IF                                                         
050300         END-IF                                                           
050400       END-IF                                                             
050500     ELSE                                                                 
050600       MOVE ERR-PART-MISSING     TO RESP-IDMSG-ERROR                      
050700       MOVE 'IDARTNR'            TO RESP-IDELMT-ERROR                     
050800       PERFORM MFS-RENSA-FAELT-IN                                         
050900       PERFORM MFS-RENSA-FAELT-UT                                         
051000       MOVE ZERO                 TO RESP-KVRADER                          
051100       MOVE NEJ                  TO NYCKLAR-SW                            
051200     END-IF                                                               
051300                                                                          
051400     .                                                                    
051500     EJECT                                                                
051600                                                                          
051700 E-NAESTA-SIDA SECTION.                                                   
051800                                                                          
051900     MOVE REQU-IDRADNR-START     TO W-IDRADNR                             
052000     .                                                                    
052100     EJECT                                                                
052200                                                                          
052300 F-SAMMA-SIDA SECTION.                                                    
052400                                                                          
052500     MOVE REQU-IDRADNR-START     TO W-IDRADNR                             
052600     PERFORM MFS-RENSA-FAELT-IN                                           
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000 G-LAES-VISA-INFO SECTION.                                                
053100                                                                          
053200     MOVE ZERO                   TO RESP-KVRADER                          
053300     MOVE NEJ                    TO FLYTT-SW                              
053400     MOVE +1                     TO INDX                                  
053500     PERFORM IMS-GNP-SATB-RAD-NEXT                                        
053600     IF SEGMENT-FINNS                                                     
053700       MOVE SATB-RAD-IDRADNR     TO RESP-IDRADNR-START                    
053800     ELSE                                                                 
053900       MOVE ZERO                 TO RESP-IDRADNR-START                    
054000     END-IF                                                               
054100                                                                          
054200     PERFORM UNTIL (INDX > MAX-KVRADER) OR (SEGMENT-SAKNAS)               
054300       IF SATB-RAD-IDRADNR >= SPAR-RADNR                                  
054400                                                                          
054500         PERFORM UNTIL INDX > MAX-KVRADER                                 
054600           IF SEGMENT-FINNS                                               
054700             PERFORM GA-KOLLA-RADSTATUS                                   
054800             MOVE SATB-RAD-KDSTRRAD                                       
054900                                 TO W-KDSTRRAD                            
055000             MOVE SATB-RAD-IDRADNR                                        
055100                                 TO RESP-IDRADNR-LINE(INDX)               
055200                                    W-IDRADNR                             
055300                                    SPAR-IDRADNR                          
055400             IF STATUS-OK                                                 
055500               PERFORM GB-BEHANDLA-RAD                                    
057000                                                                          
057010               IF REQU-IDMSGVER NOT = '001'                               
057100                 PERFORM IMS-GNP-SATB-NOTER                               
057200                 IF SEGMENT-FINNS                                         
057300                   PERFORM GC-HAMTA-NOTERINGSRAD                          
057400                   IF INDX > MAX-KVRADER                                  
057500                     MOVE JA     TO FLYTT-SW                              
057600                     ADD -1      TO INDX                                  
057700                                    RESP-KVRADER                          
057800                     MOVE ALL-SPACE                                       
057910                                 TO RESP-LINE-DETAILS(INDX)               
058000                                    RESP-IDRADNR-LINE(INDX)               
058100                     ADD +1      TO INDX                                  
058200                                    RESP-KVRADER                          
058300                   ELSE                                                   
058400                     MOVE NEJ    TO FLYTT-SW                              
058800                     MOVE ZERO   TO SPAR-AAVV                             
058900                     MOVE ALL-SPACE                                       
059000                                 TO RESP-IDRADNR-LINE(INDX)               
059100                     ADD +1      TO INDX                                  
059200                                    RESP-KVRADER                          
059300                   END-IF                                                 
059400                 END-IF                                                   
059500               END-IF                                                     
059600             END-IF                                                       
059700             IF FLYTT-EJ-OK                                               
059800               PERFORM IMS-GNP-SATB-RAD-NEXT                              
059900             END-IF                                                       
060000           ELSE                                                           
060100             COMPUTE INDX = MAX-KVRADER + 1                               
060200           END-IF                                                         
060300         END-PERFORM                                                      
060400       ELSE                                                               
060500         PERFORM IMS-GNP-SATB-RAD-NEXT                                    
060600         IF SEGMENT-SAKNAS                                                
060700           MOVE ERR-LINE-MISSING TO RESP-IDMSG-ERROR                      
060800           MOVE 'LINE'           TO RESP-IDELMT-ERROR                     
060900         END-IF                                                           
061000       END-IF                                                             
061100     END-PERFORM                                                          
061200                                                                          
061300     IF FLYTT-OK                                                          
061400       MOVE SPAR-IDRADNR         TO RESP-IDRADNR-NEXT                     
061500       MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                       
061600     ELSE                                                                 
061700       IF SEGMENT-FINNS                                                   
061800         PERFORM GA-KOLLA-RADSTATUS                                       
061900         IF STATUS-OK                                                     
062000           MOVE SATB-RAD-IDRADNR TO RESP-IDRADNR-NEXT                     
062100           MOVE INF-MORE-INFO-EXISTS                                      
062200                                 TO RESP-IDMSG-INFO                       
062300         ELSE                                                             
062400           PERFORM IMS-GNP-SATB-RAD-NEXT                                  
062500           IF SEGMENT-FINNS                                               
062600             PERFORM UNTIL (SEGMENT-SAKNAS) OR (STATUS-OK)                
062700               IF SEGMENT-FINNS                                           
062800                 PERFORM GA-KOLLA-RADSTATUS                               
062900                 IF STATUS-OK                                             
063000                   MOVE SATB-RAD-IDRADNR                                  
063100                                 TO RESP-IDRADNR-NEXT                     
063200                   MOVE INF-MORE-INFO-EXISTS                              
063300                                 TO RESP-IDMSG-INFO                       
063400                 ELSE                                                     
063500                   PERFORM IMS-GNP-SATB-RAD-NEXT                          
063600                 END-IF                                                   
063700               ELSE                                                       
063800                 MOVE ZERO       TO RESP-IDRADNR-NEXT                     
063900               END-IF                                                     
064000             END-PERFORM                                                  
064100           ELSE                                                           
064200             MOVE ZERO           TO RESP-IDRADNR-NEXT                     
064300           END-IF                                                         
064400         END-IF                                                           
064500       ELSE                                                               
064600         MOVE ZERO               TO RESP-IDRADNR-NEXT                     
064700       END-IF                                                             
064800     END-IF                                                               
064900     .                                                                    
065000     EJECT                                                                
065100                                                                          
065200 GA-KOLLA-RADSTATUS SECTION.                                              
065300                                                                          
065400     IF SATB-RAD-KDISATS = 'N' OR 'T'                                     
065500       MOVE SATB-RAD-TISTADAT    TO DAT-I-TIDATUM                         
065600       MOVE JA                   TO STATUS-SW                             
065700       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
065800       CALL WDATKONV          USING DAT-KDDATFORM                         
065900                                    DAT-I-TIDATUM                         
066000                                    DAT-O-TIDATUM                         
066100                                    DAT-KDSVAR                            
066200       MOVE DAT-TIAA-VECKA       TO SPAR-AA                               
066300       MOVE DAT-TIVV             TO SPAR-VV                               
066400     ELSE                                                                 
066500       IF SATB-RAD-KDISATS = 'E'                                          
066600         MOVE SATB-RAD-TISTODAT  TO DAT-I-TIDATUM                         
066700         MOVE JA                 TO STATUS-SW                             
066800         MOVE 'AAMMDD'           TO DAT-KDDATFORM                         
066900         CALL WDATKONV        USING DAT-KDDATFORM                         
067000                                    DAT-I-TIDATUM                         
067100                                    DAT-O-TIDATUM                         
067200                                    DAT-KDSVAR                            
067300         MOVE DAT-TIAA-VECKA     TO SPAR-AA                               
067400         MOVE DAT-TIVV           TO SPAR-VV                               
067500       ELSE                                                               
067600         MOVE SATB-RAD-TISTODAT  TO TMP1-YYMMDD                           
067700         MOVE DAGENS-DATUM       TO TMP2-YYMMDD                           
067800         PERFORM WY2000P1                                                 
067900         IF  SATB-RAD-KDISATS = 'U'                                       
068000         AND TMP1-YYMMDD > TMP2-YYMMDD                                    
068100           MOVE SATB-RAD-TISTODAT                                         
068200                                 TO DAT-I-TIDATUM                         
068300           MOVE JA               TO STATUS-SW                             
068400           MOVE 'AAMMDD'         TO DAT-KDDATFORM                         
068500           CALL WDATKONV      USING DAT-KDDATFORM                         
068600                                    DAT-I-TIDATUM                         
068700                                    DAT-O-TIDATUM                         
068800                                    DAT-KDSVAR                            
068900           MOVE DAT-TIAA-VECKA   TO SPAR-AA                               
069000           MOVE DAT-TIVV         TO SPAR-VV                               
069100         ELSE                                                             
069200           MOVE NEJ              TO STATUS-SW                             
069300                                                                          
069400           MOVE SATB-RAD-TISTODAT                                         
069500                                 TO TMP1-YYMMDD                           
069600           MOVE DAGENS-DATUM     TO TMP2-YYMMDD                           
069700           PERFORM WY2000P1                                               
069800           IF  SATB-RAD-KDISATS = ' '                                     
069900           AND TMP1-YYMMDD > TMP2-YYMMDD                                  
070000             MOVE JA             TO STATUS-SW                             
070100             MOVE SATB-RAD-TISTADAT                                       
070200                                 TO TMP1-YYMMDD                           
070300             MOVE DAGENS-DATUM   TO TMP2-YYMMDD                           
070400             PERFORM WY2000P1                                             
070500             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
070600               MOVE SATB-RAD-TISTADAT                                     
070700                                 TO DAT-I-TIDATUM                         
070800               MOVE 'AAMMDD'     TO DAT-KDDATFORM                         
070900               CALL WDATKONV  USING DAT-KDDATFORM                         
071000                                    DAT-I-TIDATUM                         
071100                                    DAT-O-TIDATUM                         
071200                                    DAT-KDSVAR                            
071300               MOVE DAT-TIAA-VECKA                                        
071400                                 TO SPAR-AA                               
071500               MOVE DAT-TIVV     TO SPAR-VV                               
071600             ELSE                                                         
071601               MOVE ZERO         TO SPAR-AAVV                             
071610             END-IF                                                       
071700           ELSE                                                           
071800             MOVE NEJ            TO STATUS-SW                             
071900           END-IF                                                         
072000         END-IF                                                           
072100       END-IF                                                             
072200     END-IF                                                               
072300     .                                                                    
072400     EJECT                                                                
072500                                                                          
072600 GB-BEHANDLA-RAD SECTION.                                                 
072700                                                                          
072800     MOVE NEJ                    TO BEN-SW                                
072810     MOVE SPACE                  TO RESP-LINE-DETAILS   (INDX)            
072900     IF SATB-RAD-IDARTNR NOT = ZERO                                       
072920       MOVE C-PART-LINE        TO RESP-LINE-TYPE-LINE (INDX)              
073010       MOVE SATB-RAD-IDARTNR   TO RESP-IDARTNR-LINE (INDX)                
073011                                  W-IDARTNR                               
073020       MOVE SATB-RAD-REANTPSA  TO RESP-REANTPSA-LINE (INDX)               
073030       MOVE SATB-RAD-KDISATS   TO RESP-KDISATS-LINE (INDX)                
073040       MOVE SPAR-AAVV          TO RESP-TIAAVV-LINE (INDX)                 
073050       MOVE SATB-RAD-IDSTRTYP  TO RESP-IDSTRTYP-LINE (INDX)               
073100       PERFORM GBA-GET-ADDRESS                                            
073110       PERFORM IMS-GU-ARTC11                                              
073120       IF SEGMENT-FINNS                                                   
073121         MOVE JA               TO SW-ARTC11-SEGM-FINNS                    
073130         MOVE CLAG-KDFARLIG    TO RESP-KDFARLIG-LINE    (INDX)            
073140         MOVE CLAG-IDANSK      TO RESP-IDANSK-LINE      (INDX)            
073150         MOVE CLAG-PRARTSTD    TO RESP-PRARTSTD-LINE    (INDX)            
073160         MOVE CLAG-KVVECKOR-LT TO RESP-KVVECKOR-LT-LINE (INDX)            
073161                                                                          
073162         IF REQU-IDMSGVER NOT = '001'                                     
073163           COMPUTE W-ARB-SALDO =  CLAG-KVLS                               
073165                                - CLAG-KVRESS                             
073171                                - CLAG-KVROS                              
073172                                + CLAG-KVAKS-CDC                          
073173                                + CLAG-KVAKS-PAV                          
073174                                + CLAG-KVAKS-T                            
073175                                                                          
073176           PERFORM IMS-GU-WDK901                                          
073177           IF SEGMENT-FINNS                                               
073178             COMPUTE W-ARB-SALDO   =  W-ARB-SALDO                         
073181                                    - ART-KVOKS-BULK                      
073182                                    - ART-KVOKS-DAG                       
073183                                    - ART-KVOKS-VOR                       
073191             IF CLAG-TIDISPIN = ZERO                                      
073192               COMPUTE W-ARB-SALDO =  W-ARB-SALDO                         
073193                                    - ART-SUTPO-TOT                       
073194             ELSE                                                         
073196               PERFORM GBB-SUBTRACT-WDK911-TPO                            
073198             END-IF                                                       
073199           END-IF                                                         
073200                                                                          
073201           MOVE W-ARB-SALDO      TO RESP-ARB-SALDO-LINE   (INDX)          
073202         END-IF                                                           
073203       END-IF                                                             
073210       IF SATB-RAD-BEART-SVE = SPACE                                      
073300         IF ARTC11-SEGMENT-FINNS                                          
074100           MOVE IDSKYLT-WS       TO W-IDSKYLT                             
074200           PERFORM IMS-GU-BEN-SEQ                                         
074300           IF SEGMENT-FINNS                                               
074400             MOVE BEN-TEXT-BEART TO RESP-BEART-LINE (INDX)                
074500           END-IF                                                         
074600         END-IF                                                           
074700       ELSE                                                               
075000         IF IDSKYLT-WS NOT = 'S  '                                        
075100           MOVE 'S'              TO W-IDSKYLT                             
075200           MOVE SATB-RAD-BEART-SVE                                        
075300                                 TO W-BEART                               
075400           MOVE SATB-RAD-KDBENHOM                                         
075500                                 TO SPAR-RAD-KDBENHOM                     
075600           PERFORM IMS-GU-BENA01-ASEQ                                     
075700           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
075800                      (BEN-BEN-KDHOMONYM = SPAR-RAD-KDBENHOM)             
075900             IF SEGMENT-FINNS                                             
076000               IF BEN-BEN-KDHOMONYM = SPAR-RAD-KDBENHOM                   
076100                 MOVE JA         TO BEN-SW                                
076200               ELSE                                                       
076300                 PERFORM IMS-GN-BENA01-ASEQ-NEXT                          
076400               END-IF                                                     
076500             ELSE                                                         
076600               MOVE SPACE        TO RESP-BEART-LINE (INDX)                
076700               MOVE NEJ          TO BEN-SW                                
076800             END-IF                                                       
076900           END-PERFORM                                                    
077000                                                                          
077100           IF SEGMENT-FINNS                                               
077200             MOVE IDSKYLT-WS     TO W-IDSKYLT                             
077300             PERFORM IMS-GNP-BENA11-ASEQ                                  
077400             IF SEGMENT-FINNS                                             
077500               MOVE BEN-TEXT-BEART                                        
077600                                 TO RESP-BEART-LINE (INDX)                
077700             ELSE                                                         
077800               MOVE SPACE        TO RESP-BEART-LINE (INDX)                
077900             END-IF                                                       
078000           END-IF                                                         
078100         ELSE                                                             
078200           MOVE SATB-RAD-BEART-SVE                                        
078300                                 TO RESP-BEART-LINE (INDX)                
078400         END-IF                                                           
078800       END-IF                                                             
078810       MOVE ZERO     TO SPAR-AAVV                                         
078820       ADD +1        TO INDX                                              
078830                        RESP-KVRADER                                      
078900     ELSE                                                                 
078901       IF REQU-IDMSGVER NOT = '001'                                       
078910         MOVE C-SUPPL-PART-LINE    TO RESP-LINE-TYPE-LINE (INDX)          
078911         MOVE SATB-RAD-IDLEVNR     TO RESP-IDLEVNR-LINE   (INDX)          
078912         MOVE SATB-RAD-BELEVART    TO RESP-BELEVART-LINE  (INDX)          
078920         MOVE SATB-RAD-REANTPSA    TO RESP-REANTPSA-LINE  (INDX)          
078930         MOVE SATB-RAD-KDISATS     TO RESP-KDISATS-LINE   (INDX)          
078940         MOVE SPAR-AAVV            TO RESP-TIAAVV-LINE    (INDX)          
078950         MOVE SATB-RAD-IDSTRTYP    TO RESP-IDSTRTYP-LINE  (INDX)          
079300         IF IDSKYLT-WS NOT = 'S  '                                        
079400           MOVE 'S'                TO W-IDSKYLT                           
079500           MOVE SATB-RAD-BEART-SVE TO W-BEART                             
079600           MOVE SATB-RAD-KDBENHOM  TO SPAR-RAD-KDBENHOM                   
079700           PERFORM IMS-GU-BENA01-ASEQ                                     
079800           PERFORM UNTIL (SEGMENT-SAKNAS) OR                              
079900                         (BEN-BEN-KDHOMONYM = SPAR-RAD-KDBENHOM)          
080000             IF SEGMENT-FINNS                                             
080100               IF BEN-BEN-KDHOMONYM = SPAR-RAD-KDBENHOM                   
080200                 MOVE JA           TO BEN-SW                              
080300               ELSE                                                       
080400                 PERFORM IMS-GN-BENA01-ASEQ-NEXT                          
080500               END-IF                                                     
080600             ELSE                                                         
080700               MOVE SPACE          TO RESP-BEART-LINE (INDX)              
080800               MOVE NEJ            TO BEN-SW                              
080900             END-IF                                                       
081000           END-PERFORM                                                    
081100                                                                          
081200           IF SEGMENT-FINNS                                               
081300             MOVE IDSKYLT-WS       TO W-IDSKYLT                           
081400             PERFORM IMS-GNP-BENA11-ASEQ                                  
081500             IF SEGMENT-FINNS                                             
081600               MOVE BEN-TEXT-BEART TO RESP-BEART-LINE (INDX)              
081700             ELSE                                                         
081800               MOVE SPACE          TO RESP-BEART-LINE (INDX)              
081900             END-IF                                                       
082000           END-IF                                                         
082100         ELSE                                                             
082200           MOVE SATB-RAD-BEART-SVE TO RESP-BEART-LINE (INDX)              
082300         END-IF                                                           
082900                                                                          
083000         IF SATB-RAD-IDARTNR = ZERO AND                                   
083100            SATB-RAD-IDLEVNR = SPACE AND                                  
083200            SATB-RAD-BELEVART = SPACE                                     
083300           MOVE SATB-RAD-BEART-SVE   TO RESP-BEART-LINE (INDX)            
083400         END-IF                                                           
083401                                                                          
083402         MOVE ZERO     TO SPAR-AAVV                                       
083403         ADD +1        TO INDX                                            
083404                          RESP-KVRADER                                    
083405         END-IF                                                           
083410     END-IF                                                               
083500     .                                                                    
083600     EJECT                                                                
083700                                                                          
083800 GBA-GET-ADDRESS SECTION.                                                 
083900                                                                          
084000     IF REQU-IDMSGVER = '001'                                             
084100       PERFORM IMS-GU-WDK711                                              
084200       IF SEGMENT-FINNS                                                   
084300         MOVE SLAG-ADLAGOMR      TO RESP-ADLAGOMR-LINE (INDX)             
084400         MOVE SLAG-ADGANG        TO RESP-ADGANG-LINE (INDX)               
084500         MOVE SLAG-ADPLATS       TO RESP-ADPLATS-LINE (INDX)              
084600       ELSE                                                               
084700         MOVE ZERO               TO RESP-ADLAGOMR-LINE (INDX)             
084800                                    RESP-ADGANG-LINE (INDX)               
084900                                    RESP-ADPLATS-LINE (INDX)              
085000       END-IF                                                             
085100     END-IF                                                               
085200     .                                                                    
085300     EJECT                                                                
085310 GBB-SUBTRACT-WDK911-TPO SECTION.                                         
085320                                                                          
085390     MOVE ZERO       TO W-DABEHOV-MIN                                     
085391                                                                          
085392     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
085393     MOVE CLAG-TIDISPIN TO DAT-I-TIDATUM                                  
085394     CALL WDATKONV USING DAT-KDDATFORM                                    
085395                         DAT-I-TIDATUM                                    
085396                         DAT-O-TIDATUM                                    
085397                         DAT-KDSVAR                                       
085398     IF DAT-KDSVAR-OK                                                     
085399       MOVE DAT-TIAA-VECKA TO W-TIAA                                      
085400       MOVE DAT-TIVV       TO W-TIVV                                      
085401       MOVE DAT-TISEKEL    TO W-TISEKEL                                   
085402       MOVE W-TIAAAAVV     TO W-DABEHOV-MAX                               
085403     ELSE                                                                 
085404       MOVE ZERO           TO W-DABEHOV-MAX                               
085405     END-IF                                                               
085407                                                                          
085408     PERFORM IMS-GNP-WDK911                                               
085409     PERFORM UNTIL SEGMENT-SAKNAS                                         
085410       COMPUTE W-ARB-SALDO =  W-ARB-SALDO                                 
085411                            - ANT-SUTPO-PB                                
085412                            - ANT-SUTPO-EJPB                              
085413       PERFORM IMS-GNP-WDK911                                             
085414     END-PERFORM                                                          
085417     .                                                                    
085418     EJECT                                                                
085419                                                                          
085420 GC-HAMTA-NOTERINGSRAD SECTION.                                           
085500                                                                          
085600     MOVE +1                     TO INDXNOT                               
085610     IF INDX NOT > MAX-KVRADER                                            
085620       MOVE C-NOTE-LINE          TO RESP-LINE-TYPE-LINE (INDX)            
085700       MOVE SATB-NOT-TESTRNOT(1) TO RESP-TESTRNOT-LINE  (INDX)            
085800       IF SATB-NOT-TESTRNOT(2) = SPACE                                    
085900         MOVE SPACE                TO RESP-CONTINUE-LINE (INDX)           
086000       ELSE                                                               
086100         MOVE '*'                  TO RESP-CONTINUE-LINE (INDX)           
086200       END-IF                                                             
086210     END-IF                                                               
086300     .                                                                    
086400     EJECT                                                                
086410 S01-KONV-TIDISPIN SECTION.                                               
086420                                                                          
086498     .                                                                    
086499     EJECT                                                                
086500 MFS-RENSA-FAELT-IN SECTION.                                              
086600                                                                          
086700     MOVE +1                     TO INDX                                  
086800     PERFORM UNTIL INDX > MAX-KVRADER                                     
086900       MOVE ALL-SPACE            TO RESP-SELECT-LINE(INDX)                
087000                                    RESP-IDRADNR-LINE(INDX)               
087100       ADD +1                    TO INDX                                  
087200     END-PERFORM                                                          
087300     .                                                                    
087400                                                                          
087500 MFS-RENSA-FAELT-INMAT SECTION.                                           
087600                                                                          
087700     MOVE +1                     TO INDX                                  
087800     PERFORM UNTIL INDX > MAX-KVRADER                                     
087900       MOVE ALL-SPACE            TO RESP-SELECT-LINE(INDX)                
088000       ADD +1                    TO INDX                                  
088100     END-PERFORM                                                          
088200     .                                                                    
088300                                                                          
088400 MFS-FORM-ATTR SECTION.                                                   
088500                                                                          
088600     MOVE +1                     TO INDX                                  
088700     PERFORM UNTIL INDX > MAX-KVRADER                                     
088800       MOVE MFS-FORMATETS-ATTR   TO RESP-SELECT-LINE-ATTR(INDX)           
088900       ADD +1                    TO INDX                                  
089000     END-PERFORM                                                          
089100     .                                                                    
089200                                                                          
089300 MFS-RENSA-FAELT-UT SECTION.                                              
089400                                                                          
089500     MOVE ALL-SPACE              TO RESP-BEART                            
089600                                    RESP-KDPRODSL                         
089700                                    RESP-IDFKNGRP                         
089800                                    RESP-IDSTRTYP                         
089900                                    RESP-IDRADNR-START                    
090000                                    RESP-IDRADNR-NEXT                     
090100                                    RESP-KDPSLLOC                         
090200     MOVE +1                     TO INDX                                  
090300     PERFORM UNTIL INDX > MAX-KVRADER                                     
090400       MOVE ALL-SPACE            TO RESP-SELECT-LINE(INDX)                
090500                                    RESP-IDRADNR-LINE(INDX)               
090600                                    RESP-LINE-DETAILS(INDX)               
090700       ADD +1                    TO INDX                                  
090800     END-PERFORM                                                          
090900     .                                                                    
091000     SKIP2                                                                
091100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
091200     MOVE ALL-PLUS               TO RESP-IDRADNR-START                    
091300                                    RESP-IDRADNR-NEXT                     
091400                                    RESP-BEART                            
091500                                    RESP-KDPRODSL                         
091600                                    RESP-IDFKNGRP                         
091700                                    RESP-IDSTRTYP                         
091800                                    RESP-KDPSLLOC                         
091900     MOVE +1                     TO INDX                                  
092000     PERFORM UNTIL INDX > MAX-KVRADER                                     
092100       MOVE ALL-PLUS             TO RESP-IDRADNR-LINE(INDX)               
092200                                    RESP-SELECT-LINE(INDX)                
092300                                    RESP-LINE-DETAILS(INDX)               
092400       ADD +1                    TO INDX                                  
092500     END-PERFORM                                                          
092600     .                                                                    
092700     SKIP2                                                                
092800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
092900                                                                          
093000     MOVE +1                     TO INDX                                  
093100     PERFORM UNTIL INDX > MAX-KVRADER                                     
093200       MOVE ALL-PLUS             TO RESP-SELECT-LINE(INDX)                
093300                                    RESP-IDRADNR-LINE(INDX)               
093400       ADD +1                    TO INDX                                  
093500     END-PERFORM                                                          
093600     .                                                                    
093700     EJECT                                                                
093800******************************************************                    
093900*                  IMS SEKTIONER                     *                    
094000******************************************************                    
094100 IMS-GU-ARTC01 SECTION.                                                   
094200     SKIP2                                                                
094300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
094400          DELIMITED BY SIZE INTO SSA1                                     
094500     MOVE '  GE' TO GODK-STATUSKODER                                      
094600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-1 SSA1                    
094700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
094800     PERFORM IMS-STATUSKONTROLL                                           
094900     .                                                                    
095000                                                                          
095100 IMS-GU-ARTC11 SECTION.                                                   
095200     SKIP2                                                                
095300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
095400          DELIMITED BY SIZE INTO SSA1                                     
095500     MOVE 'WLARTC11 ' TO SSA2                                             
095600     MOVE '  GE' TO GODK-STATUSKODER                                      
095700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-1 SSA1 SSA2               
095800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100                                                                          
096200 IMS-GU-WDK711 SECTION.                                                   
096300     SKIP2                                                                
096400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
096500          DELIMITED BY SIZE INTO SSA1                                     
096600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
096700          DELIMITED BY SIZE INTO SSA2                                     
096800     MOVE '  GE' TO GODK-STATUSKODER                                      
096900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-3 SSA1 SSA2               
097000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
097100     PERFORM IMS-STATUSKONTROLL                                           
097200     .                                                                    
097300                                                                          
097301 IMS-GU-WDK901         SECTION.                                           
097302     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
097304            DELIMITED BY SIZE INTO SSA1                                   
097305     MOVE '  GE' TO GODK-STATUSKODER                                      
097306     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-AREA-4 SSA1                    
097307     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
097308     PERFORM IMS-STATUSKONTROLL                                           
097309     .                                                                    
097310     SKIP3                                                                
097311 IMS-GNP-WDK911         SECTION.                                          
097312     STRING 'WDK911  (DABEHOV >=' W-DABEHOV-MIN-X                         
097314                    '&DABEHOV <=' W-DABEHOV-MAX-X ')'                     
097315            DELIMITED BY SIZE INTO SSA1                                   
097316     MOVE '  GE' TO GODK-STATUSKODER                                      
097317     CALL CBLTDLI USING GNP WDK9-PCB DLI-IO-AREA-4 SSA1                   
097318     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
097319     PERFORM IMS-STATUSKONTROLL                                           
097320     .                                                                    
097321     EJECT                                                                
097400 IMS-GU-SATB-ART SECTION.                                                 
097500     SKIP2                                                                
097600     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
097700          DELIMITED BY SIZE INTO SSA1                                     
097800     MOVE '  GE' TO GODK-STATUSKODER                                      
097900     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
098000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
098100     PERFORM IMS-STATUSKONTROLL                                           
098200     .                                                                    
098300                                                                          
098400                                                                          
098500 IMS-GNP-SATB-RAD-NEXT SECTION.                                           
098600     SKIP2                                                                
098700     STRING 'WLSATB11(IDRADNR =>' W-IDRADNR-X ')'                         
098800          DELIMITED BY SIZE INTO SSA1                                     
098900     MOVE '  GE' TO GODK-STATUSKODER                                      
099000     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
099100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
099200     PERFORM IMS-STATUSKONTROLL                                           
099300     .                                                                    
099400                                                                          
099500 IMS-GNP-SATB-NOTER SECTION.                                              
099600     SKIP2                                                                
099700     STRING 'WLSATB11(WDJ111KY =' W-KDSTRRAD-X                            
099800                                  W-IDRADNR-X ')'                         
099900          DELIMITED BY SIZE INTO SSA1                                     
100000     MOVE 'WLSATB22 ' TO SSA2                                             
100100     MOVE '  GE' TO GODK-STATUSKODER                                      
100200     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1 SSA2                
100300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
100400     PERFORM IMS-STATUSKONTROLL                                           
100500     .                                                                    
100600     EJECT                                                                
100700                                                                          
100800 IMS-GU-BEN-SEQ SECTION.                                                  
100900     SKIP2                                                                
101000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
101100          DELIMITED BY SIZE INTO SSA1                                     
101200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
101300          DELIMITED BY SIZE INTO SSA2                                     
101400     MOVE '  GE' TO GODK-STATUSKODER                                      
101500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-2 SSA1 SSA2               
101600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
101700     PERFORM IMS-STATUSKONTROLL                                           
101800     .                                                                    
101900                                                                          
102000 IMS-GU-BENA01-ASEQ SECTION.                                              
102100     SKIP2                                                                
102200     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
102300                                  W-BEART-X ')'                           
102400          DELIMITED BY SIZE INTO SSA1                                     
102500     MOVE '  GE' TO GODK-STATUSKODER                                      
102600     CALL CBLTDLI USING GU BENA-A-PCB DLI-IO-AREA-2 SSA1                  
102700     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
102800     PERFORM IMS-STATUSKONTROLL                                           
102900     .                                                                    
103000     EJECT                                                                
103100                                                                          
103200 IMS-GN-BENA01-ASEQ-NEXT SECTION.                                         
103300     SKIP2                                                                
103400     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
103500                                  W-BEART-X ')'                           
103600          DELIMITED BY SIZE INTO SSA1                                     
103700     MOVE '  GE' TO GODK-STATUSKODER                                      
103800     CALL CBLTDLI USING GN BENA-A-PCB DLI-IO-AREA-2 SSA1                  
103900     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
104000     PERFORM IMS-STATUSKONTROLL                                           
104100     .                                                                    
104200     EJECT                                                                
104300                                                                          
104400 IMS-GNP-BENA11-ASEQ SECTION.                                             
104500     SKIP2                                                                
104600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
104700          DELIMITED BY SIZE INTO SSA1                                     
104800     MOVE '  GE' TO GODK-STATUSKODER                                      
104900     CALL CBLTDLI USING GNP BENA-A-PCB DLI-IO-AREA-2 SSA1                 
105000     MOVE BENA-A-STATUS-CODE TO STATUS-WS                                 
105100     PERFORM IMS-STATUSKONTROLL                                           
105200     .                                                                    
105300                                                                          
105400 IMS-STATUSKONTROLL SECTION.                                              
105500     SKIP2                                                                
105600     SET STATUS-IX TO 1                                                   
105700     SEARCH GODK-STATUS                                                   
105800       AT END CALL FELLOG                                                 
105900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
106000     END-SEARCH                                                           
106100     .                                                                    
106200     EJECT                                                                
106300     EJECT                                                                
106400*    -COPY WY2000P1                                                       
