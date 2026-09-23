000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4028300.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   90/05/17.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        BILD 4283 - FRÅGA PÅ ORDERBEKRÄFTELSE ARTIKEL.                   
001100*        PROGRAMMET VISAR ALLA ORDERBEKRÄFTELSER FÖR                      
001200*        EN VISS ARTIKEL.                                                 
001300*                                                                         
001400*        PROGRAMMET ANROPAR W218ETA FÖR HÄMTNING                          
001500*        AV ETA-DATUM/NDC-LAGER.                                          
001600*       (ESTIMATED TIME AVAILABLE)                                        
001700*                                                                         
001800*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001900*        PROGRAMMET LÄSER      WLORQM                                     
002000*        PROGRAMMET LÄSER      WLORQN                                     
002100*        PROGRAMMET LÄSER      WLSATB                                     
002200*        PROGRAMMET LÄSER      WDB6                                       
002300*                                                                         
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W4T283                                              
002700*        MID:         W4I28301                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W4O28301                                            
003100                                                                          
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500*    -COPY WY2000W1                                                       
003600     SKIP3                                                                
003700 77  IDPGM                       PIC X(08)   VALUE 'W4028300'.            
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  ASTERIX                     PIC X       VALUE '*'.                   
004200 77  TEK                         PIC XX      VALUE ' ='.                  
004300                                                                          
004400*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004700                                                                          
004800 77  TABELL-IX                   PIC S9(3)  VALUE +0    COMP SYNC.        
004900 77  TAB-INDX                    PIC S9(3)  VALUE +0    COMP SYNC.        
005000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005100                                                                          
005200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005300 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005400 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005500 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005600 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
005700 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
005800 77  WS-IDORDER-SPAR             PIC S9(7)   VALUE +0 COMP-3.             
005900 77  WS-KDORDBEK-NUM             PIC 9(2)    VALUE ZERO.                  
006000 77  WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
006100                                                                          
006200 77  WS-IDORDER                  PIC X(7)    VALUE SPACE.                 
006300 77  WS-KDFRAKT                  PIC X(2)    VALUE SPACE.                 
006400 77  WS-KDORDKL                  PIC X(1)    VALUE SPACE.                 
006500 77  WS-IDORDER-NUM              PIC 9(7)    VALUE ZERO.                  
006600 77  WS-KDFRAKT-NUM              PIC 9(2)    VALUE ZERO.                  
006700 77  WS-KDORDKL-NUM              PIC 9(1)    VALUE ZERO.                  
006800 01  W-SPAR-IDKUNDRF.                                                     
006900     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
007000     03  FILLER                  PIC X(3)    VALUE '+++'.                 
007100                                                                          
007200                                                                          
007300                                                                          
007400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007500     88  NYCKLAR-OK                          VALUE 'J'.                   
007600     88  NYCKLAR-FEL                         VALUE 'N'.                   
007700                                                                          
007800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007900     88  ALLT-OK                             VALUE 'J'.                   
008000                                                                          
008100 77  IFYLLT-SW                   PIC X       VALUE 'J'.                   
008200     88  ALLT-IFYLLT                         VALUE 'J'.                   
008300                                                                          
008400 77  RAKNA-UPP-INDX-SW           PIC X       VALUE 'J'.                   
008500     88  RAKNA-UPP-INDX                      VALUE 'J'.                   
008600                                                                          
008700 77  RAD-SW                      PIC X       VALUE 'N'.                   
008800     88  RAD-IFYLLD                          VALUE 'J'.                   
008900                                                                          
009000 77  PROFORMA-SW                 PIC X       VALUE 'N'.                   
009100     88  PROFORMA                            VALUE 'J'.                   
009200                                                                          
009300                                                                          
009400 77  KEY-KOLL                    PIC X       VALUE '3'.                   
009500     88  BASNR-IFYLLT                        VALUE '3'.                   
009600     88  KDORDBEK-IFYLLT                     VALUE '4'.                   
009700     88  IDDC-IFYLLT                         VALUE '5'.                   
009800                                                                          
009900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010000     88  EGEN-MID                            VALUE '4283'.                
010100     88  GODK-MID                            VALUE '4281' '4282'          
010200                                                   '4283' '4284'.         
010300                                                                          
010400                                                                          
010500                                                                          
010600 77  GODK-KDORDBEK               PIC X(2)    VALUE SPACE.                 
010700     88 NOLL-KOD                             VALUE '0 ' '00' ' 0'         
010800                                                   '  '.                  
010900     88 GODK-KOD                            VALUE '10' '15' '16'          
011000                                                  '30' '31' '32'          
011100                                                  '33' '34'               
011200                                                  '40' '41' '42'          
011300                                                  '43' '44' '51'          
011400                                                  '52' '53' '54'          
011500                                                  '55' '56' '57'          
011600                                                  '58' '59'               
011700                                                  '61' '65' '66'          
011800                                                  '67' '70' '71'          
011900                                                  '72' '73' '74'          
012000                                                  '75' '76' '80'          
012100                                                  '81' '82' '83'          
012200                                                  '84'                    
012300                                                  '85' '87' '90'          
012400                                                  '91' '92' '95'          
012500                                                  '96' '97' '98'          
012600                                                  '99'.                   
012700     EJECT                                                                
012800*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
012900 01  WS-ETA-DATUM.                                                        
013000     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
013100     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
013200     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
013300                                                                          
013400     EJECT                                                                
013500 01  ARTIKEL-REKSIFFRA.                                                   
013600     03 WS-IDARTNR               PIC X(9)    VALUE SPACE.                 
013700     03 WS-STRECK                PIC X(1)    VALUE '-'.                   
013800     03 WS-REKNR                 PIC X(1)    VALUE SPACE.                 
013900                                                                          
014000 01  KOD.                                                                 
014100     03 WS-KDORDBEK              PIC X(2)    VALUE SPACE.                 
014200     03 WS-ASTERIX               PIC X       VALUE SPACE.                 
014300     EJECT                                                                
014400                                                                          
014500 01  SPAR-TABELL.                                                         
014600    03 TABELL OCCURS 6 TIMES.                                             
014700      05  TAB-IDARTNR        PIC S9(9)           COMP-3.                  
014800                                                                          
014900 01  OBKR-SEG-SPAR.                                                       
015000     03  FILLER                  PIC X(236).                              
015100                                                                          
015200 01  WS-IDKUNDRF                 PIC X(10).                               
015300 01  FILLER REDEFINES WS-IDKUNDRF.                                        
015400     03  WS-IDORDNR5             PIC 9(5).                                
015500     03  FILLER                  PIC X(5).                                
015600 01  FILLER REDEFINES WS-IDKUNDRF.                                        
015700     03  WS-IDORDNR7             PIC 9(7).                                
015800     03  FILLER                  PIC X(3).                                
015900 01  FILLER REDEFINES WS-IDKUNDRF.                                        
016000     03  FILLER                  PIC X(5).                                
016100     03  BLANK-TECKEN            PIC X(2).                                
016200     03  FILLER                  PIC X(3).                                
016300                                                                          
016400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016500 01  GENERELLA-SUBPROGRAM.                                                
016600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017000     EJECT                                                                
017100 01  GEMENSAMMA-SUBPROGRAM.                                               
017200     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
017300*        HÄMTA TIBERANK                                                   
017400     EJECT                                                                
017500*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
017600 01 FILLER                       PIC X(8)    VALUE 'W218LETA'.            
017700*   -COPY W218LETA -PRE ETA-.                                             
017800     EJECT                                                                
017900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
018000*01 -COPY WMSGINIT                                                        
018100     EJECT                                                                
018200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018300*   -COPY WMEDAREA                                                        
018400     SKIP3                                                                
018500 01  MESSAGE-CODES.                                                       
018600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
018700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
018800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
018900     EJECT                                                                
019000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019100*                                                                         
019200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019300     SKIP3                                                                
019400*01  MID -COPY W4I28301    -PRE MID-                                      
019500     EJECT                                                                
019600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019700     SKIP3                                                                
019800*01  -COPY WMSGAREA                                                       
019900     EJECT                                                                
020000     03  MOD REDEFINES MSG-AREA.                                          
020100*      05  -COPY W4O28301    -PRE MOD-                                    
020200     EJECT                                                                
020300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020400     SKIP3                                                                
020500*01  -COPY WMFSAREA                                                       
020600     EJECT                                                                
020700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020800*                                                                         
020900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021000     SKIP3                                                                
021100 01  NYCKLAR-TILL-DLI.                                                    
021200                                                                          
021300     03  W-IDARTNR-WDK6-X.                                                
021400         05  W-IDARTNR-WDK6          PIC S9(9) VALUE ZERO COMP-3.         
021500                                                                          
021600     03  W-KDORDBEK-MIN-X.                                                
021700         05  W-KDORDBEK-MIN          PIC  9(2) VALUE ZERO.                
021800                                                                          
021900     03  W-KDORDBEK-MAX-X.                                                
022000         05  W-KDORDBEK-MAX          PIC  9(2) VALUE ZERO.                
022100                                                                          
022200     03  W-IDSYSTEM-X.                                                    
022300         05  W-IDSYSTEM              PIC  X(4) VALUE 'PROF'.              
022400                                                                          
022500     03  W-IDDC-X.                                                        
022600         05  W-IDDC                  PIC  X(2) VALUE SPACE.               
022700                                                                          
022800     03  W-IDDC-MIN-X.                                                    
022900         05  W-IDDC-MIN              PIC  X(2) VALUE 'AA'.                
023000                                                                          
023100     03  W-IDDC-MAX-X.                                                    
023200         05  W-IDDC-MAX              PIC  X(2) VALUE '99'.                
023300                                                                          
023400     03  W-WDQ101KY-X.                                                    
023500         05  W-IDORDER-WDQ1          PIC S9(7) VALUE ZERO COMP-3.         
023600         05  W-IDARTNR-WDQ1          PIC S9(9) VALUE ZERO COMP-3.         
023700         05  W-IDLOPNR-WDQ1          PIC S9(3) VALUE ZERO COMP-3.         
023800         05  W-IDSEKVNR-WDQ1         PIC S9(3) VALUE ZERO COMP-3.         
023900         05  W-IDDC-WDQ1             PIC  X(2) VALUE SPACE.               
024000         05  W-KDORDBEK-WDQ1         PIC 9(2)  VALUE ZERO.                
024100                                                                          
024200     03  W-WDQ1A1KY-X.                                                    
024300         05  W-IDDISTR-SEQ           PIC S9(5) VALUE ZERO COMP-3.         
024400         05  W-IDKUNDNR-SEQ          PIC S9(7) VALUE ZERO COMP-3.         
024500         05  W-TIREGDAT-SEQ          PIC S9(9) VALUE ZERO COMP-3.         
024600         05  W-IDARTNR-SEQ-X.                                             
024700            07  W-IDARTNR-SEQ        PIC S9(9) VALUE ZERO COMP-3.         
024800         05  W-IDLOPNR-SEQ           PIC S9(3) VALUE ZERO COMP-3.         
024900         05  W-IDSEKVNR-SEQ          PIC S9(3) VALUE ZERO COMP-3.         
025000         05  W-IDDC-SEQ              PIC  X(2) VALUE SPACE.               
025100         05  W-KDORDBEK-SEQ          PIC 9(2)  VALUE ZERO.                
025200         05  W-IDKUNDRF-SEQ          PIC X(10) VALUE SPACE.               
025300         05  W-IDORDER-SEQ           PIC S9(7) VALUE ZERO COMP-3.         
025400                                                                          
025500     03  W-WDQ1A1KY-MIN-X.                                                
025600         05  W-IDDISTR-MIN-SEQ       PIC S9(5) VALUE ZERO COMP-3.         
025700         05  W-IDKUNDNR-MIN-SEQ      PIC S9(7) VALUE ZERO COMP-3.         
025800         05  W-TIREGDAT-MIN-SEQ      PIC S9(9) VALUE ZERO COMP-3.         
025900         05  W-IDARTNR-MIN-SEQ       PIC S9(9) VALUE ZERO COMP-3.         
026000         05  W-IDLOPNR-MIN-SEQ       PIC S9(3) VALUE ZERO COMP-3.         
026100         05  W-IDSEKVNR-MIN-SEQ      PIC S9(3) VALUE ZERO COMP-3.         
026200         05  W-IDDC-MIN-SEQ          PIC  X(2) VALUE SPACE.               
026300         05  W-KDORDBEK-MIN-SEQ      PIC 9(2)  VALUE ZERO.                
026400         05  W-IDKUNDRF-MIN-SEQ      PIC X(10) VALUE SPACE.               
026500         05  W-IDORDER-MIN-SEQ       PIC S9(7) VALUE ZERO COMP-3.         
026600                                                                          
026700     03  W-WDQ1A1KY-MAX-X.                                                
026800         05  W-IDDISTR-MAX-SEQ       PIC S9(5) VALUE ZERO COMP-3.         
026900         05  W-IDKUNDNR-MAX-SEQ      PIC S9(7) VALUE ZERO COMP-3.         
027000         05  W-TIREGDAT-MAX-SEQ      PIC S9(9) VALUE ZERO COMP-3.         
027100         05  W-IDARTNR-MAX-SEQ       PIC S9(9) VALUE ZERO COMP-3.         
027200         05  W-IDLOPNR-MAX-SEQ       PIC S9(3) VALUE ZERO COMP-3.         
027300         05  W-IDSEKVNR-MAX-SEQ      PIC S9(3) VALUE ZERO COMP-3.         
027400         05  W-IDDC-MAX-SEQ          PIC  X(2) VALUE SPACE.               
027500         05  W-KDORDBEK-MAX-SEQ      PIC 9(2)  VALUE ZERO.                
027600         05  W-IDKUNDRF-MAX-SEQ      PIC X(10) VALUE SPACE.               
027700         05  W-IDORDER-MAX-SEQ       PIC S9(7) VALUE ZERO COMP-3.         
027800                                                                          
027900                                                                          
028000     03  W-WDJ1CSEQ-X.                                                    
028100         05  W-IDLEVNR               PIC  X(5) VALUE SPACE.               
028200         05  W-BELEVART              PIC X(30) VALUE SPACE.               
028300         05  W-IDARTNR               PIC S9(9) VALUE ZERO COMP-3.         
028400                                                                          
028500     03  W-IDLEVNR-X                 PIC  X(5) VALUE '1002 '.             
028600                                                                          
028700     03  W-IDDC-B6-X.                                                     
028800         05 W-IDDC-B6                  PIC X(2).                          
028900                                                                          
029000*    --- STATUS-KOD FRÅN IMS                                              
029100 01  STATUS-WS                   PIC XX.                                  
029200     88  SEGMENT-FINNS                       VALUE '  '.                  
029300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
029400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029500     88  BASEN-SLUT                          VALUE 'GB'.                  
029600     SKIP2                                                                
029700 01  GODK-STATUSKODER.                                                    
029800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029900     SKIP3                                                                
030000 01  SSA1                        PIC X(192).                              
030100 01  SSA2                        PIC X(64).                               
030200     EJECT                                                                
030300*    --- IMS FUNKTIONSKODER                                               
030400*01  -COPY W0003                                                          
030500     EJECT                                                                
030600*    ---  DLI INPUT-OUTPUT AREA                                           
030700 01  FILLER                    PIC X(16)  VALUE 'IO-AREA-ORQM01'.         
030800 01  DLI-IO-ORQM01.                                                       
030900*    03  WLORQM01   -COPY WDQ101                                          
031000     EJECT                                                                
031100                                                                          
031200 01  FILLER                    PIC X(16)  VALUE 'IO-AREA-ORQN01'.         
031300 01  DLI-IO-ORQN01.                                                       
031400*    03  WLORQM11   -COPY WDQ1A1                                          
031500     EJECT                                                                
031600                                                                          
031700 01  FILLER                    PIC X(16)  VALUE 'IO-AREA-SATBXX'.         
031800 01  DLI-IO-SATBXX.                                                       
031900*    03  WLSATB01   -COPY WDJ111      -PRE SATB-                          
032000*    03  WLSATB01   -COPY WDJ101      -PRE SATB-                          
032100     EJECT                                                                
032200                                                                          
032300 01  FILLER                    PIC X(16)  VALUE 'IO-AREA-ARTC01'.         
032400 01  DLI-IO-ARTC11.                                                       
032500*    03  WLARTC11     -COPY WDK611                                        
032600     EJECT                                                                
032700 01  FILLER                      PIC X(12) VALUE 'DUMMY-ARTC'.            
032800 01  ETA-ARTC-PCB                PIC X.                                   
032900 01  FILLER                      PIC X(12) VALUE 'DUMMY-LEVA'.            
033000 01  ETA-LEVA-PCB                PIC X.                                   
033100                                                                          
033200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
033300 01   DLI-IO-AREA-B601.                                                   
033400*     03  -COPY WDB601                                                    
033500                                                                          
033600 LINKAGE SECTION.                                                         
033700                                                                          
033800*01  -COPY W0009      -PRE MSG-                                           
033900     EJECT                                                                
034000*01  -COPY W0008      -PRE USEA-                                          
034100     05  FILLER                  PIC X.                                   
034200     EJECT                                                                
034300*01  -COPY W0008      -PRE ORQM-                                          
034400     05  FILLER                  PIC X.                                   
034500*01  -COPY W0008      -PRE ORQN-                                          
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034800*01  -COPY W0008      -PRE SATB-                                          
034900     05  FILLER                  PIC X.                                   
035000*01  -COPY W0008      -PRE ARTC-                                          
035100     05  FILLER                  PIC X.                                   
035200     EJECT                                                                
035300*01  -COPY W0008      -PRE WDB6-                                          
035400     05  FILLER                  PIC X.                                   
035500     EJECT                                                                
035600 01  ETA-WDK7-PCB                PIC X.                                   
035700 01  ETA-INLC-PCB                PIC X.                                   
035800 01  ETA-WDB6-PCB                PIC X.                                   
035900 01  ETA-WDD9-PCB                PIC X.                                   
036000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ORQM-PCB ORQN-PCB             
036100                                   SATB-PCB ARTC-PCB WDB6-PCB             
036200                                   ETA-WDK7-PCB                           
036300                                   ETA-INLC-PCB                           
036400                                   ETA-WDB6-PCB ETA-WDD9-PCB.             
036500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORQM-PCB ORQN-PCB             
036600                                   SATB-PCB ARTC-PCB WDB6-PCB             
036700                                   ETA-WDK7-PCB                           
036800                                   ETA-INLC-PCB                           
036900                                   ETA-WDB6-PCB ETA-WDD9-PCB.             
037000                                                                          
037100     PERFORM IMS-GET-MSG                                                  
037200     IF SEGMENT-FINNS                                                     
037300       PERFORM A-INIT                                                     
037400       PERFORM B-KOLLA-NYCKLAR                                            
037500       IF NYCKLAR-OK                                                      
037600         IF MFS-FIRST                                                     
037700           PERFORM C-FOERSTA-SIDA                                         
037800         ELSE                                                             
037900           IF MFS-NEXT                                                    
038000             PERFORM D-NAESTA-SIDA                                        
038100           ELSE                                                           
038200             PERFORM E-SAMMA-SIDA                                         
038300           END-IF                                                         
038400         END-IF                                                           
038500         PERFORM F-LAES-VISA-INFO                                         
038600       END-IF                                                             
038700       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O28301 + 4                      
038800       PERFORM IMS-INSERT-MSG                                             
038900     END-IF                                                               
039000                                                                          
039100     MOVE ZERO TO RETURN-CODE                                             
039200     GOBACK                                                               
039300     .                                                                    
039400     EJECT                                                                
039500 A-INIT SECTION.                                                          
039600                                                                          
039700     IF MSG-DUBBLA-TRANSKODER                                             
039800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I28301                 
039900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
040000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
040100     ELSE                                                                 
040200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I28301                  
040300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
040400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
040500     END-IF                                                               
040600                                                                          
040700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
040800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
040900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
041000                                                                          
041100     MOVE LOW-VALUE TO MSG-AREA                                           
041200     MOVE 'W4O28301' TO MFS-IDMOD                                         
041300     MOVE '4283' TO MOD-IDTRANS                                           
041400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
041500                                                                          
041600     IF NOT EGEN-MID                                                      
041700       MOVE SPACE TO MFS-KDTRTYP                                          
041800       MOVE '7' TO MFS-IDPFK                                              
041900       MOVE NEJ TO MID-PROFORMA                                           
042000     END-IF                                                               
042100                                                                          
042200     IF ENGLISH-TEXT                                                      
042300       MOVE +2    TO SPRAK-IX                                             
042400       MOVE 'GB ' TO MED-IDSKYLT                                          
042500     ELSE                                                                 
042600       MOVE +1    TO SPRAK-IX                                             
042700       MOVE 'S  ' TO MED-IDSKYLT                                          
042800     END-IF                                                               
042900     MOVE +1 TO TABELL-IX                                                 
043000     PERFORM 6 TIMES                                                      
043100       MOVE ZERO TO TAB-IDARTNR(TABELL-IX)                                
043200       ADD +1 TO TABELL-IX                                                
043300     END-PERFORM                                                          
043400                                                                          
043500     MOVE LOW-VALUE    TO  W-WDQ1A1KY-MIN-X                               
043600                                                                          
043700     MOVE HIGH-VALUE TO    W-WDQ1A1KY-MAX-X                               
043800                                                                          
043900     ACCEPT DAGENS-DATUM  FROM DATE                                       
044000     .                                                                    
044100     EJECT                                                                
044200 B-KOLLA-NYCKLAR SECTION.                                                 
044300                                                                          
044400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
044500     MOVE '001'             TO MSGI-KDCALL                                
044600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
044700     MOVE '4283'            TO MSGI-IDTRANS                               
044800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
044900     IF EGEN-MID                                                          
045000        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
045100        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
045200        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
045300        MOVE MID-IDORDER-IN   TO W-SPAR-IDORDNR7                          
045400        MOVE W-SPAR-IDKUNDRF   TO MSGI-IDKUNDRF                           
045500        MOVE MID-KDFRAKT-IN    TO MSGI-KDFRAKT                            
045600     END-IF                                                               
045700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
045800     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
045900                                                                          
046000     MOVE JA TO ALLT-SW                                                   
046100     MOVE JA TO NYCKLAR-SW                                                
046200                                                                          
046300     PERFORM BA-KOLLA-DISTRIKT                                            
046400     PERFORM BB-KOLLA-KUNDNR                                              
046500     PERFORM BE-KOLLA-ARTNR                                               
046600     PERFORM BD-KOLLA-IDDC                                                
046700     MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK-IN                              
046800     IF ALLT-IFYLLT                                                       
046900       PERFORM BF-KOLLA-ORDBEK                                            
047000     END-IF                                                               
047100                                                                          
047200     PERFORM BG-KOLLA-ORDERNR                                             
047300     PERFORM BH-KOLLA-FRAKTKOD                                            
047400     PERFORM BI-KOLLA-ORDERKLASS                                          
047500     PERFORM BJ-KOLLA-PROFORMA                                            
047600                                                                          
047700     IF NYCKLAR-FEL                                                       
047800       IF ALLT-OK                                                         
047900         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
048000         CALL WMEDKONV USING MED-WMEDAREA                                 
048100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
048200         PERFORM MFS-RENSA-FAELT-UT                                       
048300       ELSE                                                               
048400         MOVE '001'      TO MED-IDMFSFEL                                  
048500         CALL WMEDKONV USING MED-WMEDAREA                                 
048600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
048700         PERFORM MFS-RENSA-FAELT-UT                                       
048800       END-IF                                                             
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 BA-KOLLA-DISTRIKT SECTION.                                               
049300                                                                          
049400     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
049500     IF MID-IDDISTR-IN     NOT = ALL '+'                                  
049600       INSPECT WS-IDDISTR  REPLACING LEADING SPACE BY ZERO                
049700       MOVE '7'         TO MFS-IDPFK                                      
049800       MOVE SPACE       TO MFS-KDTRTYP                                    
049900     END-IF                                                               
050000                                                                          
050100     MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                          
050200     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
050300       MOVE MSGI-IDDISTR       TO WS-IDDISTR-NUM                          
050400       MOVE WS-IDDISTR-NUM     TO W-IDDISTR-MIN-SEQ                       
050500                                  W-IDDISTR-MAX-SEQ                       
050600                                  W-IDDISTR-SEQ                           
050700     ELSE                                                                 
050800       MOVE NEJ                TO NYCKLAR-SW                              
050900     END-IF                                                               
051000                                                                          
051100     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
051200     .                                                                    
051300     EJECT                                                                
051400 BB-KOLLA-KUNDNR SECTION.                                                 
051500                                                                          
051600     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
051700     IF MID-IDKUNDNR-IN     NOT = ALL '+'                                 
051800       MOVE '7'         TO MFS-IDPFK                                      
051900       MOVE SPACE       TO MFS-KDTRTYP                                    
052000     END-IF                                                               
052100                                                                          
052200     IF MSGI-IDKUNDNR NUMERIC                                             
052300       MOVE MSGI-IDKUNDNR   TO WS-IDKUNDNR-NUM                            
052400       MOVE WS-IDKUNDNR-NUM TO W-IDKUNDNR-MIN-SEQ                         
052500                               W-IDKUNDNR-MAX-SEQ                         
052600                               W-IDKUNDNR-SEQ                             
052700     ELSE                                                                 
052800       MOVE NEJ TO NYCKLAR-SW                                             
052900     END-IF                                                               
053000                                                                          
053100     MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                                
053200     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
053300     IF MOD-IDKUNDNR-UT = SPACE                                           
053400       MOVE '     0' TO MOD-IDKUNDNR-UT                                   
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 BD-KOLLA-IDDC   SECTION.                                                 
053900                                                                          
054000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
054100     IF MID-IDDC-IN = ALL '+'                                             
054200         MOVE MID-IDDC-UT   TO W-IDDC-B6                                  
054300                               MOD-IDDC-UT                                
054400     ELSE                                                                 
054500       MOVE MID-IDDC-IN     TO W-IDDC-B6                                  
054600                               MOD-IDDC-UT                                
054700       MOVE '7'             TO MFS-IDPFK                                  
054800       MOVE SPACE           TO MFS-KDTRTYP                                
054900     END-IF                                                               
055000     PERFORM IMS-GU-WDB601                                                
055100                                                                          
055200                                                                          
055300     IF NOT DCS-CDC-TR                                                    
055400       IF DCS-KDDC      NOT =  SPACE                                      
055500         MOVE DCS-IDDC      TO W-IDDC-MIN                                 
055600                               W-IDDC-MAX                                 
055700       END-IF                                                             
055800     ELSE                                                                 
055900       MOVE MSGI-IDDC       TO W-IDDC-MIN                                 
056000                               W-IDDC-MAX                                 
056100     END-IF                                                               
056200     .                                                                    
056300     EJECT                                                                
056400 BE-KOLLA-ARTNR SECTION.                                                  
056500                                                                          
056600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
056700                                                                          
056800     IF MID-IDARTNR-IN       NOT = ALL '+'                                
056900       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
057000       MOVE '7'         TO MFS-IDPFK                                      
057100     END-IF                                                               
057200                                                                          
057300     IF MSGI-IDARTNR NUMERIC                                              
057400       IF MSGI-IDARTNR = ZERO                                             
057500         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
057600         MOVE NEJ TO IFYLLT-SW                                            
057700         MOVE NEJ TO NYCKLAR-SW                                           
057800       ELSE                                                               
057900         MOVE MSGI-IDARTNR   TO WS-IDARTNR-NUM                            
058000         MOVE WS-IDARTNR-NUM TO W-IDARTNR-SEQ                             
058100                                MOD-IDARTNR-UT                            
058200         MOVE JA TO ALLT-SW                                               
058300         MOVE JA  TO IFYLLT-SW                                            
058400       END-IF                                                             
058500     ELSE                                                                 
058600       MOVE NEJ TO NYCKLAR-SW                                             
058700     END-IF                                                               
058800     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
058900                                                                          
059000     .                                                                    
059100     EJECT                                                                
059200 BF-KOLLA-ORDBEK SECTION.                                                 
059300                                                                          
059400     IF GODK-MID                                                          
059500        IF MID-KDORDBEK-IN = ALL '+'                                      
059600          MOVE MID-KDORDBEK-UT TO GODK-KDORDBEK                           
059700        ELSE                                                              
059800          MOVE MID-KDORDBEK-IN TO GODK-KDORDBEK                           
059900          MOVE '7'      TO MFS-IDPFK                                      
060000        END-IF                                                            
060100                                                                          
060200        IF GODK-KOD AND GODK-MID                                          
060300          MOVE '4' TO KEY-KOLL                                            
060400          MOVE GODK-KDORDBEK TO W-KDORDBEK-MIN                            
060500                                W-KDORDBEK-MAX                            
060600                                MOD-KDORDBEK-UT                           
060700        ELSE                                                              
060800          IF NOLL-KOD                                                     
060900            MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK-UT                       
061000            MOVE LOW-VALUE  TO W-KDORDBEK-MIN-X                           
061100            MOVE HIGH-VALUE TO W-KDORDBEK-MAX-X                           
061200          ELSE                                                            
061300            MOVE GODK-KDORDBEK TO MOD-KDORDBEK-UT                         
061400            MOVE NEJ TO NYCKLAR-SW                                        
061500          END-IF                                                          
061600        END-IF                                                            
061700     ELSE                                                                 
061800        MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK-UT                           
061900        MOVE LOW-VALUE       TO W-KDORDBEK-MIN-X                          
062000        MOVE HIGH-VALUE      TO W-KDORDBEK-MAX-X                          
062100     END-IF                                                               
062200                                                                          
062300                                                                          
062400                                                                          
062500     .                                                                    
062600     EJECT                                                                
062700 BG-KOLLA-ORDERNR SECTION.                                                
062800                                                                          
062900     MOVE MFS-RENSA-FAELT TO MOD-IDORDER-IN                               
063000                                                                          
063100     MOVE MSGI-IDKUNDRF(1:7)   TO MOD-IDORDER-UT                          
063200                                                                          
063300     .                                                                    
063400     EJECT                                                                
063500 BH-KOLLA-FRAKTKOD SECTION.                                               
063600                                                                          
063700     MOVE MFS-RENSA-FAELT TO MOD-KDFRAKT-IN                               
063800                                                                          
063900     MOVE MSGI-KDFRAKT    TO MOD-KDFRAKT-UT                               
064000                                                                          
064100     .                                                                    
064200     EJECT                                                                
064300 BI-KOLLA-ORDERKLASS SECTION.                                             
064400                                                                          
064500     MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-IN                               
064600                                                                          
064700     IF MID-KDORDKL-IN = ALL '+'                                          
064800       MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-UT                             
064900     ELSE                                                                 
065000       MOVE MID-KDORDKL-IN TO MOD-KDORDKL-UT                              
065100       INSPECT MOD-KDORDKL-UT REPLACING LEADING ZERO BY SPACE             
065200     END-IF                                                               
065300                                                                          
065400     .                                                                    
065500     EJECT                                                                
065600                                                                          
065700 BJ-KOLLA-PROFORMA SECTION.                                               
065800                                                                          
065900     MOVE MFS-RENSA-FAELT TO MOD-PROFORMA                                 
066000     IF MID-PROFORMA = ALL '+'                                            
066100        IF MID-PROFORMA-SPAR = SPACE                                      
066200           MOVE NEJ TO MOD-PROFORMA                                       
066300                       MOD-PROFORMA-SPAR                                  
066400                       PROFORMA-SW                                        
066500        ELSE                                                              
066600           MOVE MID-PROFORMA-SPAR TO MOD-PROFORMA                         
066700                                     MOD-PROFORMA-SPAR                    
066800                                     PROFORMA-SW                          
066900        END-IF                                                            
067000     ELSE                                                                 
067100        MOVE '7' TO MFS-IDPFK                                             
067200        MOVE SPACE TO MFS-KDTRTYP                                         
067300        IF MID-PROFORMA = NEJ                                             
067400           MOVE NEJ TO MOD-PROFORMA                                       
067500                       MOD-PROFORMA-SPAR                                  
067600        ELSE                                                              
067700           IF MID-PROFORMA = 'J' OR 'Y'                                   
067800              MOVE JA           TO MOD-PROFORMA                           
067900                                   MOD-PROFORMA-SPAR                      
068000                                   PROFORMA-SW                            
068100           ELSE                                                           
068200              MOVE MFS-ALFA-FAELT-FEL TO MOD-PROFORMA-ATTR                
068300              MOVE MFS-ROER-EJ-FAELT  TO MOD-PROFORMA                     
068400              MOVE NEJ TO NYCKLAR-SW                                      
068500              MOVE NEJ TO ALLT-SW                                         
068600           END-IF                                                         
068700        END-IF                                                            
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 C-FOERSTA-SIDA SECTION.                                                  
069200                                                                          
069300     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
069400     CALL WMEDKONV USING MED-WMEDAREA                                     
069500     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
069600     .                                                                    
069700                                                                          
069800 D-NAESTA-SIDA SECTION.                                                   
069900                                                                          
070000     MOVE MID-IDORDER-NEXT    TO W-IDORDER-SEQ                            
070100     MOVE MID-IDKUNDRF-NEXT   TO W-IDKUNDRF-SEQ                           
070200     MOVE MID-IDARTNR-NEXT    TO W-IDARTNR-SEQ                            
070300     MOVE MID-IDLOPNR-NEXT    TO W-IDLOPNR-SEQ                            
070400     MOVE MID-IDSEKVNR-NEXT   TO W-IDSEKVNR-SEQ                           
070500     MOVE MID-TITIREGD-9KOMPL-NEXT TO W-TIREGDAT-SEQ                      
070600     MOVE MID-IDDC-NEXT       TO W-IDDC-SEQ                               
070700     MOVE MID-KDORDBEK-NEXT   TO W-KDORDBEK-SEQ                           
070800                                                                          
070900     MOVE JA TO ALLT-SW                                                   
071000     .                                                                    
071100     EJECT                                                                
071200 E-SAMMA-SIDA SECTION.                                                    
071300                                                                          
071400     MOVE MID-IDORDER-ENTER   TO W-IDORDER-SEQ                            
071500     MOVE MID-IDKUNDRF-ENTER  TO W-IDKUNDRF-SEQ                           
071600     MOVE MID-IDARTNR-ENTER   TO W-IDARTNR-SEQ                            
071700     MOVE MID-IDLOPNR-ENTER   TO W-IDLOPNR-SEQ                            
071800     MOVE MID-IDSEKVNR-ENTER  TO W-IDSEKVNR-SEQ                           
071900     MOVE MID-TITIREGD-9KOMPL-ENTER TO W-TIREGDAT-SEQ                     
072000     MOVE MID-IDDC-ENTER      TO W-IDDC-SEQ                               
072100     MOVE MID-KDORDBEK-ENTER  TO W-KDORDBEK-SEQ                           
072200                                                                          
072300     MOVE JA TO ALLT-SW                                                   
072400     .                                                                    
072500     EJECT                                                                
072600 F-LAES-VISA-INFO SECTION.                                                
072700                                                                          
072800     IF PROFORMA                                                          
072900        MOVE ' =' TO TEK                                                  
073000     ELSE                                                                 
073100        MOVE 'NE' TO TEK                                                  
073200     END-IF                                                               
073300     IF MFS-FIRST                                                         
073400        PERFORM IMS-GU-ORQN01-SEQ                                         
073500     ELSE                                                                 
073600        PERFORM IMS-GU-ORQN01                                             
073700     END-IF                                                               
073800     IF SEGMENT-FINNS                                                     
073900       MOVE SEQA-IDORDER        TO MOD-IDORDER-ENTER                      
074000       MOVE SEQA-IDARTNR        TO MOD-IDARTNR-ENTER                      
074100       MOVE SEQA-IDKUNDRF       TO MOD-IDKUNDRF-ENTER                     
074200       MOVE SEQA-IDLOPNR        TO MOD-IDLOPNR-ENTER                      
074300       MOVE SEQA-IDSEKVNR       TO MOD-IDSEKVNR-ENTER                     
074400       MOVE SEQA-IDDC           TO MOD-IDDC-ENTER                         
074500       MOVE SEQA-KDORDBEK       TO MOD-KDORDBEK-ENTER                     
074600       MOVE SEQA-TITIREGD-9KOMPL TO MOD-TITIREGD-9KOMPL-ENTER             
074700       MOVE SEQA-IDWDQ101       TO W-WDQ101KY-X                           
074800       PERFORM FA-KOLLA-VILKEN-LAESNING                                   
074900     ELSE                                                                 
075000       MOVE '058'     TO MED-IDMFSFEL                                     
075100       CALL WMEDKONV USING MED-WMEDAREA                                   
075200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
075300       PERFORM MFS-RENSA-FAELT-UT                                         
075400       MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                               
075500     END-IF                                                               
075600     .                                                                    
075700                                                                          
075800     EJECT                                                                
075900 FA-KOLLA-VILKEN-LAESNING SECTION.                                        
076000                                                                          
076100     PERFORM IMS-GU-ORQM01                                                
076200     MOVE +1 TO INDX                                                      
076300     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
076400                   BASEN-SLUT OR                                          
076500                   INDX > MAX-INDX                                        
076600       MOVE MFS-RENSA-FAELT TO WS-ASTERIX                                 
076700       IF SEGMENT-FINNS                                                   
076800         IF OBKR-BERADREF = 'W480      '                                  
076900           MOVE NEJ TO RAKNA-UPP-INDX-SW                                  
077000         ELSE                                                             
077100           IF OBKR-KDORDBEK = 40                                          
077200              IF OBKR-IDARTNR-TILLK > ZERO                                
077300                PERFORM FAB-KOLLA-ORDBEK-KOD                              
077400                MOVE JA TO RAKNA-UPP-INDX-SW                              
077500              ELSE                                                        
077600                MOVE NEJ TO RAKNA-UPP-INDX-SW                             
077700              END-IF                                                      
077800           ELSE                                                           
077900              IF OBKR-IDARTNR-TILLK = ZERO AND OBKR-BEERS = SPACE         
078000                PERFORM FAB-KOLLA-ORDBEK-KOD                              
078100                MOVE JA TO RAKNA-UPP-INDX-SW                              
078200              ELSE                                                        
078300                MOVE NEJ TO RAKNA-UPP-INDX-SW                             
078400              END-IF                                                      
078500           END-IF                                                         
078600         END-IF                                                           
078700         IF INDX NOT = 15                                                 
078800            PERFORM IMS-GN-ORQN01-SEQ                                     
078900            IF SEGMENT-FINNS                                              
079000               MOVE SEQA-IDWDQ101   TO W-WDQ101KY-X                       
079100               PERFORM IMS-GU-ORQM01                                      
079200            END-IF                                                        
079300         END-IF                                                           
079400         IF RAKNA-UPP-INDX                                                
079500           ADD +1 TO INDX                                                 
079600         END-IF                                                           
079700       END-IF                                                             
079800     END-PERFORM                                                          
079900     IF INDX > MAX-INDX AND SEGMENT-FINNS AND                             
080000                            INDX NOT = 15                                 
080100        PERFORM FAC-SPARA-NEXT-NYCKLAR                                    
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500 FAB-KOLLA-ORDBEK-KOD SECTION.                                            
080600                                                                          
080700     EVALUATE OBKR-KDORDBEK                                               
080800     WHEN 10                                                              
080900       PERFORM FABA-LAES-VISA-KOD-10                                      
081000     WHEN 15 THRU 16                                                      
081100       PERFORM FABB-LAES-VISA-KOD-15-TILL-16                              
081200     WHEN 20 THRU 22                                                      
081300       PERFORM FABF-LAES-VISA-KOD-52-TILL-55                              
081400     WHEN 30 THRU 34                                                      
081500       PERFORM FABE-LAES-VISA-KOD-81-83-85-87                             
081600     WHEN 43 THRU 44                                                      
081700       PERFORM FABB-LAES-VISA-KOD-15-TILL-16                              
081800     WHEN 95 THRU 97                                                      
081900       PERFORM FABB-LAES-VISA-KOD-15-TILL-16                              
082000     WHEN 40                                                              
082100       PERFORM FABC-LAES-VISA-KOD-40                                      
082200     WHEN 41 THRU 42                                                      
082300       MOVE ASTERIX TO WS-ASTERIX                                         
082400       PERFORM FABD-LAES-VISA-KOD-41-TILL-42                              
082500     WHEN 61 THRU 65                                                      
082600       MOVE ASTERIX TO WS-ASTERIX                                         
082700       PERFORM FABD-LAES-VISA-KOD-41-TILL-42                              
082800     WHEN 52 THRU 56                                                      
082900       PERFORM FABF-LAES-VISA-KOD-52-TILL-55                              
083000     WHEN 58                                                              
083100       PERFORM FABF-LAES-VISA-KOD-52-TILL-55                              
083200     WHEN 66  THRU 68                                                     
083300       PERFORM FABF-LAES-VISA-KOD-52-TILL-55                              
083400     WHEN 57                                                              
083500       PERFORM FABG-LAES-VISA-KOD-57                                      
083600     WHEN 59                                                              
083700       PERFORM FABG-LAES-VISA-KOD-57                                      
083800     WHEN 70 THRU 71                                                      
083900       PERFORM FABJ-LAES-VISA-KOD-70-TILL-71                              
084000     WHEN 72 THRU 76                                                      
084100       PERFORM FABK-LAES-VISA-KOD-72-TILL-76                              
084200     WHEN 77                                                              
084300       PERFORM FABJ-LAES-VISA-KOD-70-TILL-71                              
084400     WHEN 80                                                              
084500       PERFORM FABL-LAES-VISA-KOD-80-TILL-83                              
084600     WHEN 82                                                              
084700       PERFORM FABL-LAES-VISA-KOD-80-TILL-83                              
084800     WHEN 81                                                              
084900       PERFORM FABE-LAES-VISA-KOD-81-83-85-87                             
085000     WHEN 83                                                              
085100       PERFORM FABE-LAES-VISA-KOD-81-83-85-87                             
085200     WHEN 84                                                              
085300       PERFORM FABQ-LAES-VISA-KOD-84                                      
085400     WHEN 85                                                              
085500       PERFORM FABE-LAES-VISA-KOD-81-83-85-87                             
085600     WHEN 87                                                              
085700       PERFORM FABE-LAES-VISA-KOD-81-83-85-87                             
085800     WHEN 90 THRU 91                                                      
085900       PERFORM FABM-LAES-VISA-KOD-90-TILL-91                              
086000     WHEN 98                                                              
086100*      PERFORM FABO-LAES-VISA-KOD-98                                      
086200       PERFORM FABP-LAES-VISA-KOD-99                                      
086300     WHEN 92 THRU 93                                                      
086400       PERFORM FABP-LAES-VISA-KOD-99                                      
086500     WHEN 99                                                              
086600       PERFORM FABP-LAES-VISA-KOD-99                                      
086700     WHEN OTHER                                                           
086800       SUBTRACT +1 FROM INDX                                              
086900     END-EVALUATE                                                         
087000     .                                                                    
087100     EJECT                                                                
087200                                                                          
087300 FABA-LAES-VISA-KOD-10 SECTION.                                           
087400                                                                          
087500     MOVE SPACE                 TO WS-ASTERIX                             
087600     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
087700     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
087800     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
087900     MOVE OBKR-IDARTNR          TO WS-IDARTNR                             
088000     MOVE OBKR-REKSIFFR         TO WS-REKNR                               
088100     MOVE ARTIKEL-REKSIFFRA     TO MOD-IDARTNR(INDX)                      
088200     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
088300     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
088400     IF BLANK-TECKEN = SPACE                                              
088500       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
088600     ELSE                                                                 
088700       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
088800     END-IF                                                               
088900     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
089000     MOVE OBKR-KVBEART-Q        TO MOD-KVANTAL(INDX)                      
089100     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
089200     MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                            
089300     IF BLANK-TECKEN = SPACE                                              
089400       MOVE WS-IDORDNR5         TO MOD-IDORDNR7-REF(INDX)                 
089500     ELSE                                                                 
089600       MOVE WS-IDORDNR7         TO MOD-IDORDNR7-REF(INDX)                 
089700     END-IF                                                               
089800     IF OBKR-TITPO > +0                                                   
089900        MOVE OBKR-TITPO         TO MOD-TITPO(INDX)                        
090000     ELSE                                                                 
090100        MOVE MFS-RENSA-FAELT    TO MOD-TITPO(INDX)                        
090200     END-IF                                                               
090300     .                                                                    
090400                                                                          
090500     EJECT                                                                
090600 FABB-LAES-VISA-KOD-15-TILL-16 SECTION.                                   
090700                                                                          
090800     MOVE SPACE                 TO WS-ASTERIX                             
090900     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
091000     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
091100     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
091200     IF OBKR-IDARTNR-TILLK > ZERO                                         
091300       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
091400       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
091500       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
091600       INSPECT MOD-IDARTNR(INDX) REPLACING                                
091700                        LEADING ZERO BY SPACE                             
091800     ELSE                                                                 
091900       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
092000       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
092100       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
092200       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
092300     END-IF                                                               
092400     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
092500     IF BLANK-TECKEN = SPACE                                              
092600       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
092700     ELSE                                                                 
092800       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
092900     END-IF                                                               
093000                                                                          
093100     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
093200     MOVE OBKR-KVBEART-Q        TO MOD-KVANTAL(INDX)                      
093300     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
093400     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
093500       MOVE OBKR-IDKUNDRF-RO    TO WS-IDKUNDRF                            
093600     ELSE                                                                 
093700       MOVE OBKR-IDKUNDRF       TO WS-IDKUNDRF                            
093800     END-IF                                                               
093900     IF BLANK-TECKEN = SPACE                                              
094000       MOVE WS-IDORDNR5         TO MOD-IDORDNR7-REF(INDX)                 
094100     ELSE                                                                 
094200       MOVE WS-IDORDNR7         TO MOD-IDORDNR7-REF(INDX)                 
094300     END-IF                                                               
094400     EVALUATE OBKR-KDORDBEK                                               
094500       WHEN 96                                                            
094600         MOVE OBKR-TIDISPIN     TO MOD-TITPO(INDX)                        
094700       WHEN 97                                                            
094800         MOVE OBKR-TIREPDAT     TO MOD-TITPO(INDX)                        
094900       WHEN OTHER                                                         
095000         MOVE MFS-RENSA-FAELT   TO MOD-TITPO(INDX)                        
095100     END-EVALUATE                                                         
095200     .                                                                    
095300     EJECT                                                                
095400 FABC-LAES-VISA-KOD-40 SECTION.                                           
095500                                                                          
095600     MOVE SPACE                 TO WS-ASTERIX                             
095700     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
095800     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
095900     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
096000     MOVE OBKR-IDARTNR-TILLK    TO WS-IDARTNR                             
096100     MOVE OBKR-REKSIFFR         TO WS-REKNR                               
096200     MOVE ARTIKEL-REKSIFFRA     TO MOD-IDARTNR(INDX)                      
096300     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
096400     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
096500     IF BLANK-TECKEN = SPACE                                              
096600       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
096700     ELSE                                                                 
096800       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
096900     END-IF                                                               
097000     MOVE OBKR-KVBEART          TO MOD-KVANTAL(INDX)                      
097100     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
097200     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
097300       MOVE OBKR-IDKUNDRF-RO    TO WS-IDKUNDRF                            
097400     ELSE                                                                 
097500       MOVE OBKR-IDKUNDRF       TO WS-IDKUNDRF                            
097600     END-IF                                                               
097700     IF BLANK-TECKEN = SPACE                                              
097800       MOVE WS-IDORDNR5         TO MOD-IDORDNR7-REF(INDX)                 
097900     ELSE                                                                 
098000       MOVE WS-IDORDNR7         TO MOD-IDORDNR7-REF(INDX)                 
098100     END-IF                                                               
098200     MOVE MFS-RENSA-FAELT       TO MOD-TITPO(INDX)                        
098300     .                                                                    
098400                                                                          
098500     EJECT                                                                
098600 FABD-LAES-VISA-KOD-41-TILL-42 SECTION.                                   
098700                                                                          
098800     MOVE ASTERIX               TO WS-ASTERIX                             
098900     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
099000     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
099100     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
099200     MOVE OBKR-IDARTNR          TO WS-IDARTNR                             
099300     MOVE OBKR-REKSIFFR         TO WS-REKNR                               
099400     MOVE ARTIKEL-REKSIFFRA     TO MOD-IDARTNR(INDX)                      
099500     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
099600     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
099700     IF BLANK-TECKEN = SPACE                                              
099800       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
099900     ELSE                                                                 
100000       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
100100     END-IF                                                               
100200     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
100300     MOVE OBKR-KVBEART          TO MOD-KVANTAL(INDX)                      
100400     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
100500     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
100600       MOVE OBKR-IDKUNDRF-RO    TO WS-IDKUNDRF                            
100700     ELSE                                                                 
100800       MOVE OBKR-IDKUNDRF       TO WS-IDKUNDRF                            
100900     END-IF                                                               
101000     IF BLANK-TECKEN = SPACE                                              
101100       MOVE WS-IDORDNR5         TO MOD-IDORDNR7-REF(INDX)                 
101200     ELSE                                                                 
101300       MOVE WS-IDORDNR7         TO MOD-IDORDNR7-REF(INDX)                 
101400     END-IF                                                               
101500     MOVE MFS-RENSA-FAELT       TO MOD-TITPO(INDX)                        
101600     .                                                                    
101700                                                                          
101800     EJECT                                                                
101900                                                                          
102000 FABE-LAES-VISA-KOD-81-83-85-87 SECTION.                                  
102100                                                                          
102200     MOVE SPACE                 TO WS-ASTERIX                             
102300     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
102400     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
102500     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
102600     IF OBKR-IDARTNR-TILLK > ZERO                                         
102700       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
102800       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
102900       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
103000       INSPECT MOD-IDARTNR(INDX) REPLACING                                
103100                           LEADING ZERO BY SPACE                          
103200     ELSE                                                                 
103300       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
103400       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
103500       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
103600       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
103700     END-IF                                                               
103800     MOVE OBKR-IDKUNDRF     TO WS-IDKUNDRF                                
103900     IF BLANK-TECKEN = SPACE                                              
104000       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
104100     ELSE                                                                 
104200       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
104300     END-IF                                                               
104400     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
104500*    IF OBKR-KDORDBEK = 81 OR 83 OR 85 OR 87                              
104600        MOVE OBKR-KVANNANT      TO MOD-KVANTAL(INDX)                      
104700*    ELSE                                                                 
104800*       MOVE OBKR-KVBEART       TO MOD-KVANTAL(INDX)                      
104900*    END-IF                                                               
105000     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
105100     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
105200       MOVE OBKR-IDKUNDRF-RO    TO WS-IDKUNDRF                            
105300     ELSE                                                                 
105400       MOVE OBKR-IDKUNDRF       TO WS-IDKUNDRF                            
105500     END-IF                                                               
105600     IF BLANK-TECKEN = SPACE                                              
105700       MOVE WS-IDORDNR5         TO MOD-IDORDNR7-REF(INDX)                 
105800     ELSE                                                                 
105900       MOVE WS-IDORDNR7         TO MOD-IDORDNR7-REF(INDX)                 
106000     END-IF                                                               
106100     MOVE MFS-RENSA-FAELT       TO MOD-TITPO(INDX)                        
106200     .                                                                    
106300                                                                          
106400     EJECT                                                                
106500 FABF-LAES-VISA-KOD-52-TILL-55 SECTION.                                   
106600                                                                          
106700     MOVE SPACE                 TO WS-ASTERIX                             
106800     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
106900     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
107000     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
107100     IF OBKR-KDORDBEK = 58                                                
107200       MOVE OBKR-IDARTNR        TO MOD-IDARTNR(INDX)                      
107300     ELSE                                                                 
107400       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
107500       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
107600       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
107700     END-IF                                                               
107800     INSPECT MOD-IDARTNR(INDX) REPLACING                                  
107900                    LEADING ZERO BY SPACE                                 
108000     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
108100     IF BLANK-TECKEN = SPACE                                              
108200       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
108300     ELSE                                                                 
108400       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
108500     END-IF                                                               
108600     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
108700     IF OBKR-KDORDBEK = 22                                                
108800       MOVE OBKR-KVANNANT       TO MOD-KVANTAL(INDX)                      
108900     ELSE                                                                 
109000       MOVE OBKR-KVBEART        TO MOD-KVANTAL(INDX)                      
109100     END-IF                                                               
109200     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
109300     MOVE MFS-RENSA-FAELT       TO MOD-IDORDNR7-REF(INDX)                 
109400                                                                          
109500     .                                                                    
109600                                                                          
109700     EJECT                                                                
109800 FABG-LAES-VISA-KOD-57 SECTION.                                           
109900                                                                          
110000     MOVE SPACE                 TO WS-ASTERIX                             
110100     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
110200     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
110300     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
110400     MOVE OBKR-IDARTNR          TO WS-IDARTNR                             
110500     MOVE OBKR-REKSIFFR         TO WS-REKNR                               
110600     MOVE ARTIKEL-REKSIFFRA     TO MOD-IDARTNR(INDX)                      
110700     INSPECT MOD-IDARTNR(INDX) REPLACING                                  
110800                          LEADING ZERO BY SPACE                           
110900     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
111000     IF BLANK-TECKEN = SPACE                                              
111100       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
111200     ELSE                                                                 
111300       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
111400     END-IF                                                               
111500     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
111600     MOVE OBKR-KVBEART          TO MOD-KVANTAL(INDX)                      
111700     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
111800     MOVE MFS-RENSA-FAELT       TO MOD-IDORDNR7-REF(INDX)                 
111900     IF OBKR-KDORDBEK = 57                                                
112000        MOVE OBKR-WDQ101 TO OBKR-SEG-SPAR                                 
112100        MOVE OBKR-IDARTNR TO W-IDARTNR                                    
112200        PERFORM IMS-GET-GU-SATB01                                         
112300        MOVE +1 TO TABELL-IX                                              
112400        ADD +1 TO INDX                                                    
112500        PERFORM UNTIL SEGMENT-SAKNAS  OR                                  
112600                      TABELL-IX > 5                                       
112700          IF SATB-STR-IDARTNR < 100000000 AND                             
112800             SATB-STR-TIBORT = 0                                          
112900             MOVE SATB-RAD-TISTADAT      TO TMP1-YYMMDD                   
113000             MOVE DAGENS-DATUM           TO TMP2-YYMMDD                   
113100             PERFORM WY2000P1                                             
113200             IF TMP1-YYMMDD       NOT > TMP2-YYMMDD                       
113300                MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                   
113400                MOVE DAGENS-DATUM        TO TMP2-YYMMDD                   
113500                PERFORM WY2000P1                                          
113600                IF TMP1-YYMMDD    NOT < TMP2-YYMMDD                       
113700                   MOVE SATB-STR-IDARTNR TO TAB-IDARTNR(TABELL-IX)        
113800                   ADD +1 TO TABELL-IX                                    
113900                END-IF                                                    
114000             END-IF                                                       
114100          END-IF                                                          
114200          PERFORM IMS-GET-GN-SATB01                                       
114300        END-PERFORM                                                       
114400                                                                          
114500        COMPUTE TAB-INDX = MAX-INDX - INDX                                
114600        IF TAB-INDX < INDX                                                
114700          SUBTRACT 1 FROM INDX                                            
114800          PERFORM MFS-RENSA-FAELT-PA-RAD                                  
114900          MOVE +15 TO INDX                                                
115000        ELSE                                                              
115100          MOVE +1 TO TABELL-IX                                            
115200          PERFORM UNTIL TAB-IDARTNR(TABELL-IX) = ZERO OR                  
115300                                     TABELL-IX > 4                        
115400            MOVE TAB-IDARTNR(TABELL-IX) TO MOD-IDARTNR(INDX)              
115500           INSPECT MOD-IDARTNR(INDX) REPLACING                            
115600                              LEADING ZERO BY SPACE                       
115700            ADD +1 TO TABELL-IX                                           
115800            ADD +1 TO INDX                                                
115900          END-PERFORM                                                     
116000          SUBTRACT 1 FROM INDX                                            
116100        END-IF                                                            
116200     END-IF                                                               
116300     .                                                                    
116400                                                                          
116500     EJECT                                                                
116600 FABJ-LAES-VISA-KOD-70-TILL-71 SECTION.                                   
116700                                                                          
116800     MOVE SPACE                 TO WS-ASTERIX                             
116900     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
117000     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
117100     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
117200     MOVE OBKR-IDARTNR          TO WS-IDARTNR                             
117300     MOVE OBKR-REKSIFFR         TO WS-REKNR                               
117400     MOVE ARTIKEL-REKSIFFRA     TO MOD-IDARTNR(INDX)                      
117500     INSPECT MOD-IDARTNR(INDX) REPLACING                                  
117600                          LEADING ZERO BY SPACE                           
117700     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
117800     IF BLANK-TECKEN = SPACE                                              
117900       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
118000     ELSE                                                                 
118100       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
118200     END-IF                                                               
118300     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
118400     MOVE OBKR-KVBEART-Q        TO MOD-KVANTAL(INDX)                      
118500     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
118600     MOVE OBKR-IDKUNDRF-RO      TO WS-IDKUNDRF                            
118700     IF BLANK-TECKEN = SPACE                                              
118800       MOVE WS-IDORDNR5         TO MOD-IDORDNR7-REF(INDX)                 
118900     ELSE                                                                 
119000       MOVE WS-IDORDNR7         TO MOD-IDORDNR7-REF(INDX)                 
119100     END-IF                                                               
119200     IF OBKR-TITPO > +0                                                   
119300        MOVE OBKR-TITPO         TO MOD-TITPO(INDX)                        
119400        INSPECT MOD-TITPO(INDX) REPLACING LEADING ZERO BY SPACE           
119500     ELSE                                                                 
119600        MOVE MFS-RENSA-FAELT    TO MOD-TITPO(INDX)                        
119700     END-IF                                                               
119800     .                                                                    
119900                                                                          
120000     EJECT                                                                
120100 FABK-LAES-VISA-KOD-72-TILL-76 SECTION.                                   
120200                                                                          
120300     MOVE SPACE                 TO WS-ASTERIX                             
120400     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
120500     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
120600     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
120700     IF OBKR-IDARTNR-TILLK > ZERO                                         
120800       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
120900       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
121000       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
121100       INSPECT MOD-IDARTNR(INDX) REPLACING                                
121200                        LEADING ZERO BY SPACE                             
121300     ELSE                                                                 
121400       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
121500       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
121600       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
121700       INSPECT MOD-IDARTNR(INDX) REPLACING                                
121800                          LEADING ZERO BY SPACE                           
121900     END-IF                                                               
122000     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
122100     IF BLANK-TECKEN = SPACE                                              
122200       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
122300     ELSE                                                                 
122400       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
122500     END-IF                                                               
122600     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
122700     IF OBKR-KDORDBEK = 74                                                
122800        MOVE OBKR-KVBEART-Q     TO MOD-KVANTAL(INDX)                      
122900     ELSE                                                                 
123000        MOVE OBKR-KVBEART       TO MOD-KVANTAL(INDX)                      
123100     END-IF                                                               
123200     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
123300     MOVE MFS-RENSA-FAELT       TO MOD-IDORDNR7-REF(INDX)                 
123400     IF OBKR-TITPO > +0                                                   
123500        MOVE OBKR-TITPO         TO MOD-TITPO(INDX)                        
123600     ELSE                                                                 
123700        MOVE MFS-RENSA-FAELT    TO MOD-TITPO(INDX)                        
123800     END-IF                                                               
123900     .                                                                    
124000                                                                          
124100     EJECT                                                                
124200 FABL-LAES-VISA-KOD-80-TILL-83 SECTION.                                   
124300                                                                          
124400     MOVE SPACE                 TO WS-ASTERIX                             
124500     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
124600     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
124700     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
124800     IF OBKR-IDARTNR-TILLK > ZERO                                         
124900       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
125000       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
125100       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
125200     ELSE                                                                 
125300       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
125400       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
125500       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
125600     END-IF                                                               
125700     INSPECT MOD-IDARTNR(INDX) REPLACING                                  
125800                          LEADING ZERO BY SPACE                           
125900     MOVE OBKR-IDKUNDRF     TO WS-IDKUNDRF                                
126000     IF BLANK-TECKEN = SPACE                                              
126100       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
126200     ELSE                                                                 
126300       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
126400     END-IF                                                               
126500     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
126600     IF OBKR-KDORDBEK = 80                                                
126700        MOVE OBKR-KVANNANT      TO MOD-KVANTAL(INDX)                      
126800     ELSE                                                                 
126900        MOVE OBKR-KVBEART       TO MOD-KVANTAL(INDX)                      
127000     END-IF                                                               
127100     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
127200     MOVE MFS-RENSA-FAELT       TO MOD-IDORDNR7-REF(INDX)                 
127300     MOVE MFS-RENSA-FAELT       TO MOD-TITPO(INDX)                        
127400     .                                                                    
127500                                                                          
127600     EJECT                                                                
127700 FABM-LAES-VISA-KOD-90-TILL-91 SECTION.                                   
127800                                                                          
127900     MOVE SPACE                 TO WS-ASTERIX                             
128000     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
128100     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
128200     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
128300     IF OBKR-IDARTNR-TILLK > ZERO                                         
128400       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
128500                                  W-IDARTNR-WDK6                          
128600       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
128700       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
128800     ELSE                                                                 
128900       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
129000                                   W-IDARTNR-WDK6                         
129100       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
129200       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
129300     END-IF                                                               
129400     INSPECT MOD-IDARTNR(INDX) REPLACING                                  
129500                      LEADING ZERO BY SPACE                               
129600     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
129700     IF BLANK-TECKEN = SPACE                                              
129800       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
129900     ELSE                                                                 
130000       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
130100     END-IF                                                               
130200     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
130300     MOVE OBKR-KVRO             TO MOD-KVANTAL(INDX)                      
130400     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
130500     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
130600       MOVE OBKR-IDKUNDRF-RO TO WS-IDKUNDRF                               
130700     ELSE                                                                 
130800       MOVE SPACE            TO WS-IDKUNDRF                               
130900     END-IF                                                               
131000     IF BLANK-TECKEN = SPACE                                              
131100       MOVE WS-IDORDNR5         TO MOD-IDORDNR7-REF(INDX)                 
131200     ELSE                                                                 
131300       MOVE WS-IDORDNR7         TO MOD-IDORDNR7-REF(INDX)                 
131400     END-IF                                                               
131500                                                                          
131600     IF DCS-IDDC NOT = OBKR-IDDC                                          
131700        MOVE OBKR-IDDC            TO W-IDDC-B6                            
131800        PERFORM IMS-GU-WDB601                                             
131900     END-IF                                                               
132100     IF DCS-NDC                                                           
132200        PERFORM FABMA-HAEMTA-TIBERANK                                     
132300                                                                          
132400        IF ETA-SVAR-OK = JA                                               
132500          IF DCS-NDC-NA                                                   
132600            MOVE ETA-TIAAMMDD-SVAR TO MOD-TITPO(INDX)                     
132700          ELSE                                                            
132800            IF ETA-KVAVIS-ETA > +0                                        
132900              MOVE ETA-TIAAMMDD-SVAR TO MOD-TITPO(INDX)                   
133000            ELSE                                                          
133100              MOVE MFS-RENSA-FAELT TO MOD-TITPO(INDX)                     
133200            END-IF                                                        
133300          END-IF                                                          
133400        ELSE                                                              
133500           MOVE MFS-RENSA-FAELT   TO MOD-TITPO(INDX)                      
133600        END-IF                                                            
133700     ELSE                                                                 
133800        PERFORM IMS-GET-GU-ARTC11                                         
133900                                                                          
134000        IF SEGMENT-FINNS                                                  
134100           MOVE CLAG-TIDISPIN     TO MOD-TITPO(INDX)                      
134200        ELSE                                                              
134300           MOVE MFS-RENSA-FAELT   TO MOD-TITPO(INDX)                      
134400        END-IF                                                            
134500     END-IF                                                               
134600     .                                                                    
134700                                                                          
134800     EJECT                                                                
134900 FABMA-HAEMTA-TIBERANK SECTION.                                           
135000                                                                          
135100     MOVE '612'                TO ETA-KDCALL                              
135200     MOVE OBKR-IDDC            TO ETA-IDDC-REC                            
135300     MOVE OBKR-KDFRAKT         TO ETA-KDFRAKT                             
135400     MOVE W-IDARTNR-WDK6       TO ETA-IDARTNR                             
135500     MOVE SPACE                TO ETA-IDLEVNR                             
135600     MOVE OBKR-TIREGDAT        TO ETA-TIAAMMDD-ANROP                      
135700                                  WS-ETA-DATUM                            
135800     IF WS-ETA-DATUM-AAR > 50                                             
135900        MOVE 19                TO ETA-TISEKEL-ANROP                       
136000     ELSE                                                                 
136100        MOVE 20                TO ETA-TISEKEL-ANROP                       
136200     END-IF                                                               
136300                                                                          
136400     CALL W218ETA  USING ETA-W218LETA                                     
136500                         ETA-ARTC-PCB                                     
136600                         ETA-WDK7-PCB                                     
136700                         ETA-INLC-PCB                                     
136800                         ETA-LEVA-PCB                                     
136900                         ETA-WDB6-PCB                                     
137000                         ETA-WDD9-PCB                                     
137100     .                                                                    
137200     EJECT                                                                
137300 FABO-LAES-VISA-KOD-98 SECTION.                                           
137400                                                                          
137500     MOVE SPACE                 TO WS-ASTERIX                             
137600     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
137700     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
137800     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
137900     MOVE OBKR-IDARTNR          TO MOD-IDARTNR(INDX)                      
138000     INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE            
138100     MOVE SPACE                 TO ARTIKEL-REKSIFFRA                      
138200     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
138300     IF BLANK-TECKEN = SPACE                                              
138400       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
138500     ELSE                                                                 
138600       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
138700     END-IF                                                               
138800     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
138900     MOVE OBKR-KVBEART          TO MOD-KVANTAL(INDX)                      
139000     MOVE MFS-RENSA-FAELT       TO MOD-IDDC(INDX)                         
139100                                   MOD-TITPO(INDX)                        
139200                                   MOD-IDORDNR7-REF(INDX)                 
139300     .                                                                    
139400                                                                          
139500     EJECT                                                                
139600 FABP-LAES-VISA-KOD-99 SECTION.                                           
139700                                                                          
139800     MOVE SPACE                 TO WS-ASTERIX                             
139900     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
140000     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
140100     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
140200     IF OBKR-IDARTNR-TILLK > ZERO                                         
140300       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
140400       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
140500       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
140600       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
140700     ELSE                                                                 
140800       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
140900       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
141000       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
141100       INSPECT MOD-IDARTNR(INDX) REPLACING LEADING ZERO BY SPACE          
141200     END-IF                                                               
141300     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
141400     IF BLANK-TECKEN = SPACE                                              
141500       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
141600     ELSE                                                                 
141700       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
141800     END-IF                                                               
141900     MOVE OBKR-BEKUNDRF         TO MOD-BEKUNDRF(INDX)                     
142000     IF OBKR-KDORDBEK = 93                                                
142100        MOVE OBKR-KVANNANT      TO MOD-KVANTAL(INDX)                      
142200     ELSE                                                                 
142300        MOVE OBKR-KVPRERO       TO MOD-KVANTAL(INDX)                      
142400     END-IF                                                               
142500     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
142600     MOVE MFS-RENSA-FAELT       TO MOD-IDORDNR7-REF(INDX)                 
142700     .                                                                    
142800                                                                          
142900     EJECT                                                                
143000 FABQ-LAES-VISA-KOD-84 SECTION.                                           
143100                                                                          
143200     MOVE SPACE                 TO WS-ASTERIX                             
143300     MOVE OBKR-KDORDBEK         TO WS-KDORDBEK                            
143400     MOVE KOD                   TO MOD-KDORDBEK(INDX)                     
143500     MOVE OBKR-TIREGDAT         TO MOD-TIREGDAT(INDX)                     
143600     IF OBKR-IDARTNR-TILLK > ZERO                                         
143700       MOVE OBKR-IDARTNR-TILLK TO WS-IDARTNR                              
143800       MOVE OBKR-REKSIFFR-TILLK TO WS-REKNR                               
143900       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
144000     ELSE                                                                 
144100       MOVE OBKR-IDARTNR        TO WS-IDARTNR                             
144200       MOVE OBKR-REKSIFFR       TO WS-REKNR                               
144300       MOVE ARTIKEL-REKSIFFRA TO MOD-IDARTNR(INDX)                        
144400     END-IF                                                               
144500     INSPECT MOD-IDARTNR(INDX) REPLACING                                  
144600                          LEADING ZERO BY SPACE                           
144700     MOVE OBKR-IDKUNDRF         TO WS-IDKUNDRF                            
144800     IF BLANK-TECKEN = SPACE                                              
144900       MOVE WS-IDORDNR5         TO MOD-IDORDNR7(INDX)                     
145000     ELSE                                                                 
145100       MOVE WS-IDORDNR7         TO MOD-IDORDNR7(INDX)                     
145200     END-IF                                                               
145300     MOVE OBKR-KVBEART          TO MOD-KVANTAL(INDX)                      
145400     MOVE OBKR-IDDC             TO MOD-IDDC(INDX)                         
145500     IF OBKR-IDKUNDRF-RO NOT = '0000000   '                               
145600       MOVE OBKR-IDKUNDRF-RO    TO WS-IDKUNDRF                            
145700     ELSE                                                                 
145800       MOVE OBKR-IDKUNDRF       TO WS-IDKUNDRF                            
145900     END-IF                                                               
146000     IF BLANK-TECKEN = SPACE                                              
146100       MOVE WS-IDORDNR5         TO MOD-IDORDNR7-REF(INDX)                 
146200     ELSE                                                                 
146300       MOVE WS-IDORDNR7         TO MOD-IDORDNR7-REF(INDX)                 
146400     END-IF                                                               
146500     MOVE MFS-RENSA-FAELT       TO MOD-TITPO(INDX)                        
146600     .                                                                    
146700                                                                          
146800     EJECT                                                                
146900 FAC-SPARA-NEXT-NYCKLAR SECTION.                                          
147000                                                                          
147100     MOVE SEQA-IDORDER            TO MOD-IDORDER-NEXT                     
147200     MOVE SEQA-IDARTNR            TO MOD-IDARTNR-NEXT                     
147300     MOVE SEQA-IDLOPNR            TO MOD-IDLOPNR-NEXT                     
147400     MOVE SEQA-IDSEKVNR           TO MOD-IDSEKVNR-NEXT                    
147500     MOVE SEQA-IDDC               TO MOD-IDDC-NEXT                        
147600     MOVE SEQA-KDORDBEK           TO MOD-KDORDBEK-NEXT                    
147700     MOVE SEQA-IDKUNDRF           TO MOD-IDKUNDRF-NEXT                    
147800     MOVE SEQA-TITIREGD-9KOMPL    TO MOD-TITIREGD-9KOMPL-NEXT             
147900     MOVE INF-MORE-INFO-EXISTS    TO MED-IDMFSINF                         
148000     CALL WMEDKONV USING MED-WMEDAREA                                     
148100     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
148200     .                                                                    
148300                                                                          
148400     EJECT                                                                
148500 MFS-RENSA-FAELT-UT SECTION.                                              
148600                                                                          
148700*    --- ALLA UTDATA-FÄLT                                                 
148800     MOVE +1 TO INDX                                                      
148900     PERFORM UNTIL INDX > MAX-INDX                                        
149000       MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(INDX)                         
149100                               MOD-TIREGDAT(INDX)                         
149200                               MOD-IDARTNR(INDX)                          
149300                               MOD-IDORDNR7(INDX)                         
149400                               MOD-BEKUNDRF(INDX)                         
149500                               MOD-KVANTAL(INDX)                          
149600                               MOD-IDDC(INDX)                             
149700                               MOD-IDORDNR7-REF(INDX)                     
149800                               MOD-TITPO(INDX)                            
149900       ADD +1 TO INDX                                                     
150000     END-PERFORM                                                          
150100     .                                                                    
150200     SKIP2                                                                
150300 MFS-RENSA-FAELT-PA-RAD SECTION.                                          
150400                                                                          
150500*    --- UTDATA-FÄLT PÅ SKÄRMEN                                           
150600     MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(INDX)                           
150700                             MOD-TIREGDAT(INDX)                           
150800                             MOD-IDARTNR(INDX)                            
150900                             MOD-IDORDNR7(INDX)                           
151000                             MOD-BEKUNDRF(INDX)                           
151100                             MOD-KVANTAL(INDX)                            
151200                             MOD-IDDC(INDX)                               
151300                             MOD-IDORDNR7-REF(INDX)                       
151400                             MOD-TITPO(INDX)                              
151500     .                                                                    
151600     EJECT                                                                
151700 IMS-GET-MSG SECTION.                                                     
151800                                                                          
151900     MOVE '  QC' TO GODK-STATUSKODER                                      
152000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
152100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
152200     PERFORM IMS-STATUSKONTROLL                                           
152300     .                                                                    
152400     SKIP3                                                                
152500 IMS-INSERT-MSG SECTION.                                                  
152600                                                                          
152700     IF ENGLISH-TEXT                                                      
152800       MOVE 'N' TO MFS-KDHUVOMR                                           
152900     END-IF                                                               
153000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
153100     MOVE SPACE TO GODK-STATUSKODER                                       
153200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
153300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
153400     PERFORM IMS-STATUSKONTROLL                                           
153500     .                                                                    
153600     EJECT                                                                
153700 IMS-GU-ORQM01 SECTION.                                                   
153800                                                                          
153900     STRING 'WLORQM01(WDQ101KY =' W-WDQ101KY-X                            
154000                    '&IDSYSTEM' TEK W-IDSYSTEM-X ')'                      
154100          DELIMITED BY SIZE INTO SSA1                                     
154200     MOVE '  GE' TO GODK-STATUSKODER                                      
154300     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-ORQM01 SSA1                    
154400     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
154500     PERFORM IMS-STATUSKONTROLL                                           
154600     .                                                                    
154700                                                                          
154800 IMS-GU-ORQN01 SECTION.                                                   
154900                                                                          
155000     STRING 'WLORQN01(WDQ1A1KY =' W-WDQ1A1KY-X                            
155100                    '&IDSYSTEM' TEK W-IDSYSTEM-X ')'                      
155200          DELIMITED BY SIZE INTO SSA1                                     
155300     MOVE '  GE' TO GODK-STATUSKODER                                      
155400     CALL CBLTDLI USING GU ORQN-PCB DLI-IO-ORQN01 SSA1                    
155500     MOVE ORQN-STATUS-CODE TO STATUS-WS                                   
155600     PERFORM IMS-STATUSKONTROLL                                           
155700     .                                                                    
155800     EJECT                                                                
155900 IMS-GU-ORQN01-SEQ SECTION.                                               
156000                                                                          
156100       STRING 'WLORQN01(WDQ1A1KY>=' W-WDQ1A1KY-MIN-X                      
156200                      '&WDQ1A1KY<=' W-WDQ1A1KY-MAX-X                      
156300                      '&IDARTNR  =' W-IDARTNR-SEQ-X                       
156400                      '&IDDC    >=' W-IDDC-MIN-X                          
156500                      '&IDDC    <=' W-IDDC-MAX-X                          
156600                      '&KDORDBEK>=' W-KDORDBEK-MIN-X                      
156700                      '&KDORDBEK<=' W-KDORDBEK-MAX-X                      
156800                      '&IDSYSTEM' TEK W-IDSYSTEM-X ')'                    
156900            DELIMITED BY SIZE INTO SSA1                                   
157000     MOVE '  GE' TO GODK-STATUSKODER                                      
157100     CALL CBLTDLI USING GU ORQN-PCB DLI-IO-ORQN01 SSA1                    
157200     MOVE ORQN-STATUS-CODE TO STATUS-WS                                   
157300     PERFORM IMS-STATUSKONTROLL                                           
157400     .                                                                    
157500                                                                          
157600 IMS-GN-ORQN01-SEQ SECTION.                                               
157700                                                                          
157800       STRING 'WLORQN01(WDQ1A1KY>=' W-WDQ1A1KY-MIN-X                      
157900                      '&WDQ1A1KY<=' W-WDQ1A1KY-MAX-X                      
158000                      '&IDARTNR  =' W-IDARTNR-SEQ-X                       
158100                      '&IDDC    >=' W-IDDC-MIN-X                          
158200                      '&IDDC    <=' W-IDDC-MAX-X                          
158300                      '&KDORDBEK>=' W-KDORDBEK-MIN-X                      
158400                      '&KDORDBEK<=' W-KDORDBEK-MAX-X                      
158500                      '&IDSYSTEM' TEK W-IDSYSTEM-X ')'                    
158600            DELIMITED BY SIZE INTO SSA1                                   
158700     MOVE '  GE' TO GODK-STATUSKODER                                      
158800     CALL CBLTDLI USING GN ORQN-PCB DLI-IO-ORQN01 SSA1                    
158900     MOVE ORQN-STATUS-CODE TO STATUS-WS                                   
159000     PERFORM IMS-STATUSKONTROLL                                           
159100     .                                                                    
159200     EJECT                                                                
159300                                                                          
159400 IMS-GET-GU-SATB01 SECTION.                                               
159500                                                                          
159600     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
159700          DELIMITED BY SIZE INTO SSA1                                     
159800     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
159900          DELIMITED BY SIZE INTO SSA2                                     
160000     MOVE '  GE' TO GODK-STATUSKODER                                      
160100     CALL CBLTDLI USING GU SATB-PCB DLI-IO-SATBXX SSA1 SSA2               
160200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
160300     PERFORM IMS-STATUSKONTROLL                                           
160400     .                                                                    
160500                                                                          
160600 IMS-GET-GN-SATB01 SECTION.                                               
160700                                                                          
160800     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
160900          DELIMITED BY SIZE INTO SSA1                                     
161000     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
161100          DELIMITED BY SIZE INTO SSA2                                     
161200     MOVE '  GE' TO GODK-STATUSKODER                                      
161300     CALL CBLTDLI USING GN SATB-PCB DLI-IO-SATBXX SSA1 SSA2               
161400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
161500     PERFORM IMS-STATUSKONTROLL                                           
161600     .                                                                    
161700     EJECT                                                                
161800 IMS-GET-GU-ARTC11 SECTION.                                               
161900                                                                          
162000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDK6-X ')'                    
162100          DELIMITED BY SIZE INTO SSA1                                     
162200     MOVE 'WLARTC11 ' TO SSA2                                             
162300     MOVE '  GE' TO GODK-STATUSKODER                                      
162400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2               
162500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
162600     PERFORM IMS-STATUSKONTROLL                                           
162700     .                                                                    
162800                                                                          
162900 IMS-GU-WDB601    SECTION.                                                
163000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
163100          DELIMITED BY SIZE INTO SSA1                                     
163200     MOVE '  GE' TO GODK-STATUSKODER                                      
163300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
163400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
163500     PERFORM IMS-STATUSKONTROLL                                           
163600     IF SEGMENT-SAKNAS                                                    
163700         MOVE SPACE TO DCS-KDDC                                           
163800     END-IF                                                               
163900     .                                                                    
164000 IMS-STATUSKONTROLL SECTION.                                              
164100                                                                          
164200     SET STATUS-IX TO 1                                                   
164300     SEARCH GODK-STATUS                                                   
164400       AT END CALL FELLOG                                                 
164500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
164600     END-SEARCH                                                           
164700     .                                                                    
164800     EJECT                                                                
164900*    -COPY WY2000P1                                                       
