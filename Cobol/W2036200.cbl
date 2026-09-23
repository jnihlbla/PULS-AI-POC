000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2036200.                                                
000300 AUTHOR.         STEFAN KIHLBERG.                                         
000400 DATE-WRITTEN.   97/08/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700**   FUNKTION:                                                            
000800*        SKAPAR FÖRBI REFILL ORDER                                        
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDK6                                       
001100*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001200*        PROGRAMMET LÄSER      WDR410                                     
001300*        PROGRAMMET LÄSER      WDR420                                     
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W2T362                                              
001700*        MID:         W2I362N1                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O362N1                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W2036200'.            
003000 77  CURRENT-IMS-SECTION         PIC X(80)   VALUE SPACE.                 
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  YES                         PIC X       VALUE 'Y'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004000                                                                          
004100                                                                          
004200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004300     88  NYCKLAR-OK                          VALUE 'J'.                   
004400     88  NYCKLAR-FEL                         VALUE 'N'.                   
004500                                                                          
004600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004700     88  EGEN-MID                            VALUE '2362'.                
004800     88  HELP-MID                            VALUE '0551'.                
004900     EJECT                                                                
005000                                                                          
005100 01  FILLER                     PIC X(24)   VALUE 'ARBETSFALT'.           
005200                                                                          
005300 01  ARBETSFALT.                                                          
005400                                                                          
005500     03  IX                  PIC S9(3) VALUE ZERO COMP-3.                 
005600     03  ORAD-IX             PIC S9(3) VALUE ZERO COMP-3.                 
005700     03  ORAD-IX-MAX         PIC S9(3) VALUE +5   COMP-3.                 
005800     03  DISTR-IX            PIC S9(3) VALUE ZERO COMP-3.                 
005900     03  MAX-IX              PIC S9(3) VALUE +12 COMP-3.                  
006000                                                                          
006100     03  WS-IDDC2            PIC  X(2) VALUE SPACE.                       
006200     03  WS-IDDC-REF         PIC  X(2) VALUE SPACE.                       
006300     03  WS-IDDISTR          PIC S9(5) VALUE ZERO COMP-3.                 
006400     03  WS-IDDISTR-TRUNK    PIC  9(4) VALUE ZERO.                        
006500     03  WS-IDKAMPRF         PIC  X(7) VALUE SPACE.                       
006600     03  WS-IDKUNDNR         PIC  9(7) VALUE ZERO.                        
006700     03  WS-IDKUNDNR-TRUNK   PIC  9(6) VALUE ZERO.                        
006800     03  WS-KDFAKTYP         PIC  X(1) VALUE SPACE.                       
006900     03  WS-KDFRAKT          PIC  9(3) VALUE ZERO.                        
007000     03  WS-KDFRAKT-TRUNK    PIC  9(2) VALUE ZERO.                        
007100     03  WS-KDORDKL          PIC  9(1) VALUE ZERO.                        
007200     03  WS-KDORDKL-TRUNK    PIC  9(1) VALUE ZERO.                        
007300     03  WS-IDKUNDRF         PIC 9(7)  VALUE ZERO.                        
007400     03  WS-ANTAL-IFYLLDA    PIC S9(3) VALUE ZERO COMP-3.                 
007500     03  WS-KVBEART-TRUNK    PIC 9(6)  VALUE ZERO.                        
007600     03  WS-KVBEART          PIC S9(7) VALUE ZERO COMP-3.                 
007700     03  WS-START-DATUM      PIC 9(6)  VALUE ZERO.                        
007800     03  WS-STOPP-DATUM      PIC 9(6)  VALUE ZERO.                        
007900     03  WS-KVAR-KAMP        PIC 9(6)  VALUE ZERO.                        
008000     03  WS-CDC-11           PIC X(2)  VALUE '11'.                        
008100     03  WS-VAR              PIC X     VALUE SPACE.                       
008200                                                                          
008300                                                                          
008400 01  FILLER       PIC X(24)         VALUE  'WS-ORDERNUMMER-7POS'.         
008500 01  WS-ORDERNR-7POS.                                                     
008600     03  FILLER                  PIC 9(2)  VALUE ZERO.                    
008700     03  WS-ORDERNR-NUM          PIC 9(5)  VALUE ZERO.                    
008800                                                                          
008900 01  FILLER     PIC X(24)   VALUE  'MOTTAGANDE-LAGER-ADRESS'.             
009000 01  MOTTAGANDE-LAGER-ADRESS.                                             
009100     03 WS-ADART-X.                                                       
009200        05 WS-ADLAGOMR-X    PIC  9(2)     VALUE ZERO.                     
009300        05 WS-ADGANG-X      PIC  9(2)     VALUE ZERO.                     
009400        05 WS-ADPLATS-X     PIC  X(5)     VALUE SPACE.                    
009500        05 FILLER           PIC  X(1)     VALUE SPACE.                    
009600     EJECT                                                                
009700                                                                          
009800 01  FILLER     PIC X(24)   VALUE 'ADART-TABELL'.                         
009900 01  ADART-TABELL.                                                        
010000     03 TAB-ADART-GRUPP OCCURS 12.                                        
010100        05 TAB-ADART-X          PIC X(10).                                
010200                                                                          
010300 01  FILLER                     PIC X(24)   VALUE 'TIDFALT'.              
010400                                                                          
010500 01  TIDFALT.                                                             
010600     03 DAGENS-DATUM         PIC  9(6) VALUE ZERO.                        
010700     03 DAGENS-TID           PIC  9(8) VALUE ZERO.                        
010800     03 DAGENS-TID-DELAR REDEFINES DAGENS-TID.                            
010900        05 FILLER            PIC  9(1).                                   
011000        05 DAGENS-TIT        PIC  9(1).                                   
011100        05 DAGENS-TIMM       PIC  9(2).                                   
011200        05 FILLER            PIC  9(4).                                   
011300                                                                          
011400 01  KONTROLL-SIFFRA.                                                     
011500   03  REK-IDARTNR               PIC 9(9)    VALUE 0.                     
011600   03  REK-LNGD                  PIC 9(1)    VALUE 9.                     
011700   03  REK-REKSIFFR              PIC 9(1)    VALUE 0.                     
011800     EJECT                                                                
011900                                                                          
012000 01  FILLER                     PIC X(24)   VALUE 'SWITCHAR'.             
012100                                                                          
012200 77  INDATA-SW                      PIC X   VALUE 'J'.                    
012300     88  INDATA-FEL                         VALUE 'N'.                    
012400     88  INDATA-OK                          VALUE 'J'.                    
012500                                                                          
012600 77  DC-HITTAT-SW                   PIC X   VALUE '/'.                    
012700     88  DC-HITTAT                          VALUE 'J'.                    
012800     EJECT                                                                
012900                                                                          
013000 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
013100*01  -COPY WMSGKOM                                                        
013200     EJECT                                                                
013300                                                                          
013400*01    -COPY WWDC99 -PRE REF-                                             
013500     EJECT                                                                
013600 01  P-TO-P-SW.                                                           
013700   03  P-TO-P-KVLL           PIC S9(4)   COMP SYNC.                       
013800   03  P-TO-P-KDZ1           PIC X(1)    VALUE LOW-VALUE.                 
013900   03  P-TO-P-KDZ2           PIC X(1)    VALUE LOW-VALUE.                 
014000   03  P-TO-P-KDTRANS        PIC X(8).                                    
014100   03  P-TO-P-IDTRANS        PIC X(4).                                    
014200   03  P-TO-P-KDMFSFOR       PIC X(1).                                    
014300   03  P-TO-P-DATA           PIC X(1000).                                 
014400     EJECT                                                                
014500                                                                          
014600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014700 01  GENERELLA-SUBPROGRAM.                                                
014800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015200     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
015300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
015400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015500                                                                          
015600 01  GEMENSAMMA-SUBPROGRAM.                                               
015700     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
015800*        KONTROLL OCH UTTAG AV AUTOMATISKT ORDERNUMMER                    
015900     EJECT                                                                
016000 01 FILLER                       PIC X(8)    VALUE 'W411ORDN'.            
016100*   -COPY W411ORDN                                                        
016200     EJECT                                                                
016300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
016400*01 -COPY WMEDAREA                                                        
016500     SKIP3                                                                
016600 01  MESSAGE-CODES.                                                       
016700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
016800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
016900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
017000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
017100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
017200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
017300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
017400     03  ERR-WRONG-DC            PIC X(3)    VALUE '440'.                 
017500     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
017600     03  INF-PART-SUPERSEDED     PIC X(3)    VALUE '018'.                 
017700     EJECT                                                                
017800                                                                          
017900     03  MED-1                   PIC X(40)                                
018000         VALUE 'PART NOT AVAILABLE ON THIS DC          '.                 
018100     03  MED-2                   PIC X(40)                                
018200         VALUE 'LOCAL PART                             '.                 
018300     03  MED-5                   PIC X(40)                                
018400         VALUE 'ORDER READY  OR FULL PAGE              '.                 
018500     03  MED-6                   PIC X(40)                                
018600         VALUE 'UPPDATE DONE, ORDER COMPLETE           '.                 
018700     03  MED-7                   PIC X(40)                                
018800         VALUE 'UPPDATE DONE, ORDER NOT COMPLETE       '.                 
018900     03  MED-8                   PIC X(40)                                
019000         VALUE 'WRONG DISTRICT                         '.                 
019100*                                                                         
019200*    --- AREOR FÖR W006KOM SUBMODUL                                       
019300*                                                                         
019400 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
019500 01  KOM-IO-AREA.                                                         
019600   03  KOM-AREA                     PIC X(2500) VALUE SPACE.              
019700*03  FILLER  -COPY W4I25101 -PRE OHUV-  -RED KOM-AREA.                    
019800     EJECT                                                                
019900*03  FILLER  -COPY W4I25201 -PRE ORAD-  -RED KOM-AREA.                    
020000     EJECT                                                                
020100 01  RETURKOD-ABEND.                                                      
020200   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16 COMP SYNC.         
020300                                                                          
020400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
020500*                                                                         
020600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020700     SKIP3                                                                
020800*01  MID -COPY W2I36201                                                   
020900     EJECT                                                                
021000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021100     SKIP3                                                                
021200*01  -COPY WMSGAREA                                                       
021300     EJECT                                                                
021400     03  MOD REDEFINES MSG-AREA.                                          
021500*      05  -COPY W2O36201                                                 
021600     EJECT                                                                
021700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021800     SKIP3                                                                
021900*01  -COPY WMFSAREA                                                       
022000     EJECT                                                                
022100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022200*                                                                         
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022500     SKIP3                                                                
022600 01  NYCKLAR-TILL-DLI.                                                    
022700                                                                          
022800     03  W-IDARTNR-X.                                                     
022900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
023000     03  W-KDSEGKEY-X.                                                    
023100         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
023200     03  W-IDDC-X.                                                        
023300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
023400     03  W-IDDC-B601-X.                                                   
023500         05  W-IDDC-B601     PIC X(2)   VALUE SPACE.                      
023600     03  W-IDDC-B616-X.                                                   
023700         05  W-IDDC-B616     PIC X(2)   VALUE SPACE.                      
023800     03  W-WDGXKEY-X.                                                     
023900          05 W-IDHTYP            PIC X(4)    VALUE '2505'.                
024000          05 FILLER              PIC X(26)   VALUE LOW-VALUE.             
024100     03  W-IDUSER-X.                                                      
024200         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
024300     03  W-IDGMTREF-X.                                                    
024400         05  W-IDGMTREF          PIC X(17)   VALUE SPACE.                 
024500     03  W-IDDISTR-X.                                                     
024600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
024700     03  W-IDKUNDNR-X.                                                    
024800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
024900     03  W-IDDISTR-FOM-X.                                                 
025000         05  W-IDDISTR-FOM       PIC S9(5)   VALUE ZERO COMP-3.           
025100     03  W-IDDISTR-TOM-X.                                                 
025200         05  W-IDDISTR-TOM       PIC S9(5)   VALUE ZERO COMP-3.           
025300     03  W-IDKUNDNR-FOM-X.                                                
025400         05  W-IDKUNDNR-FOM      PIC S9(7)   VALUE ZERO COMP-3.           
025500     03  W-IDKUNDNR-TOM-X.                                                
025600         05  W-IDKUNDNR-TOM      PIC S9(7)   VALUE ZERO COMP-3.           
025700     03  W-IDKUNDNR-X.                                                    
025800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
025900     03  W-IDKUNDRF-X.                                                    
026000         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
026100                                                                          
026200     03  W-WDM201-X.                                                      
026300         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
026400         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
026500     03  W-WDM211-X.                                                      
026600         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
026700     03  W-WDM221-X.                                                      
026800         05  W-KMRK-IDDISTR-FOM  PIC S9(05) VALUE ZERO COMP-3.            
026900         05  W-KMRK-IDDISTR-TOM  PIC S9(05) VALUE ZERO COMP-3.            
027000         05  W-KMRK-IDKUNDNR-FOM PIC S9(07) VALUE ZERO COMP-3.            
027100         05  W-KMRK-IDKUNDNR-TOM PIC S9(07) VALUE ZERO COMP-3.            
027200                                                                          
027300*    --- STATUS-KOD FRÅN IMS                                              
027400 01  STATUS-WS                   PIC XX.                                  
027500     88  SEGMENT-FINNS                       VALUE '  '.                  
027600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027800     SKIP2                                                                
027900 01  GODK-STATUSKODER.                                                    
028000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028100     SKIP3                                                                
028200 01  SSA1                        PIC X(128).                              
028300 01  SSA2                        PIC X(144).                              
028400 01  SSA3                        PIC X(128).                              
028500     EJECT                                                                
028600*    --- IMS FUNKTIONSKODER                                               
028700*01  -COPY W0003                                                          
028800     EJECT                                                                
028900*    ---  DLI INPUT-OUTPUT AREA                                           
029000                                                                          
029100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
029200 01  DLI-IO-WDK601.                                                       
029300*    03  -COPY WDK601                                                     
029400     EJECT                                                                
029500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
029600 01  DLI-IO-WDK611.                                                       
029700*    03  -COPY WDK611                                                     
029800     EJECT                                                                
029900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
030000 01  DLI-IO-WDK629.                                                       
030100*    03  -COPY WDK629                                                     
030200     EJECT                                                                
030300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS01'.                    
030400 01  DLI-IO-WLARTS01.                                                     
030500*    03  -COPY WDK701                                                     
030600     EJECT                                                                
030700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
030800 01  DLI-IO-WLARTS11.                                                     
030900*    03  -COPY WDK711                                                     
031000     EJECT                                                                
031100 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WL250501'.            
031200     SKIP3                                                                
031300 01  DLI-IO-WL250501.                                                     
031400     03 FILLER                   PIC X(30).                               
031500     EJECT                                                                
031600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WL250511'.            
031700     SKIP3                                                                
031800 01  DLI-IO-WL250511.                                                     
031900     03  -COPY WDGX2506                                                   
032000     EJECT                                                                
032100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
032200 01   DLI-IO-AREA-B601.                                                   
032300*     03  -COPY WDB601                                                    
032400     EJECT                                                                
032500 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
032600 01   DLI-IO-AREA-B616.                                                   
032700*     03  -COPY WDB616 -PRE B616-                                         
032800     EJECT                                                                
032900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM201'.         
033000 01  DLI-IO-WDM201.                                                       
033100*    03 -COPY WDM201                                                      
033200     EJECT                                                                
033300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
033400 01  DLI-IO-WDM211.                                                       
033500*    03 -COPY WDM211                                                      
033600     EJECT                                                                
033700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
033800 01  DLI-IO-WDM221.                                                       
033900*    03 -COPY WDM221                                                      
034000     EJECT                                                                
034100                                                                          
034200 LINKAGE SECTION.                                                         
034300*01  -COPY W0009   -PRE MSG-                                              
034400*01  -COPY W0009   -PRE ALT-                                              
034500*01  -COPY W0009   -PRE KOMA-                                             
034600*01  -COPY W0008   -PRE USEA-                                             
034700     05  FILLER                  PIC X.                                   
034800     EJECT                                                                
034900*01  -COPY W0008  -PRE WDK6-                                              
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200*01  -COPY W0008  -PRE ARTS-                                              
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500*01  -COPY W0008  -PRE 2505-                                              
035600     05  FILLER                  PIC X.                                   
035700     EJECT                                                                
035800 01  ORDN-XXKP-PCB               PIC X.                                   
035900 01  ORDN-ORQL-PCB               PIC X.                                   
036000 01  ORDN-PROC-PCB               PIC X.                                   
036100 01  ORDN-ORQI-PCB               PIC X.                                   
036200     EJECT                                                                
036300*01  -COPY W0008      -PRE WDB6-                                          
036400     05  FILLER                  PIC X.                                   
036500*01  -COPY W0008      -PRE WDM2-                                          
036600     05  FILLER                  PIC X.                                   
036700     EJECT                                                                
036800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB                       
036900                           WDK6-PCB ARTS-PCB 2505-PCB                     
037000                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
037100                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
037200                           WDB6-PCB WDM2-PCB.                             
037300 MAIN SECTION.                                                            
037400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB                       
037500                           WDK6-PCB ARTS-PCB 2505-PCB                     
037600                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
037700                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
037800                           WDB6-PCB WDM2-PCB.                             
037900                                                                          
038000     PERFORM IMS-GET-MSG                                                  
038100     IF SEGMENT-FINNS                                                     
038200        PERFORM A-INIT                                                    
038300        IF NYCKLAR-OK                                                     
038400           IF MFS-UPDATE                                                  
038500              PERFORM G-KOLLA-INPUT                                       
038600              IF INDATA-OK                                                
038700                 PERFORM H-SKAPA-DISPATCHERTRANS                          
038800                 PERFORM I-VISA-SKICKAD-ORDERDEL                          
038900              END-IF                                                      
039000           ELSE                                                           
039100              IF MFS-FIRST                                                
039200                 PERFORM C-FOERSTA-SIDA                                   
039300              ELSE                                                        
039400                PERFORM E-SAMMA-SIDA                                      
039500              END-IF                                                      
039600           END-IF                                                         
039700        END-IF                                                            
039800*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
039900*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
040000        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O36201 + 4                     
040100        PERFORM IMS-INSERT-MSG                                            
040200     END-IF                                                               
040300                                                                          
040400     MOVE ZERO TO RETURN-CODE                                             
040500     GOBACK                                                               
040600     .                                                                    
040700     EJECT                                                                
040800 A-INIT SECTION.                                                          
040900                                                                          
041000     IF MSG-DUBBLA-TRANSKODER                                             
041100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I36201                 
041200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
041300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
041400     ELSE                                                                 
041500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I36201                  
041600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
041700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
041800     END-IF                                                               
041900                                                                          
042000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
042100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
042200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
042300                                                                          
042400     MOVE LOW-VALUE TO MSG-AREA                                           
042500     MOVE 'W2O362N1' TO MFS-IDMOD                                         
042600     MOVE '2362' TO MOD-IDTRANS                                           
042700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
042800                                                                          
042900     IF EGEN-MID OR HELP-MID                                              
043000       CONTINUE                                                           
043100     ELSE                                                                 
043200       MOVE SPACE TO MFS-KDTRTYP                                          
043300       MOVE '7' TO MFS-IDPFK                                              
043400     END-IF                                                               
043500     ACCEPT DAGENS-DATUM FROM DATE                                        
043600     ACCEPT DAGENS-TID FROM TIME                                          
043700     .                                                                    
043800     EJECT                                                                
043900                                                                          
044000 C-FOERSTA-SIDA SECTION.                                                  
044100                                                                          
044200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
044300     CALL WMEDKONV USING MED-WMEDAREA                                     
044400     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
044500                                                                          
044600     PERFORM MFS-RENSA-FAELT-IN                                           
044700     PERFORM MFS-RENSA-FAELT-UT                                           
044800     PERFORM MFS-FORM-ATTR                                                
044900     MOVE MFS-FORMATETS-ATTR TO MOD-IDORDNR7-ATTR                         
045000     .                                                                    
045100     EJECT                                                                
045200                                                                          
045300                                                                          
045400 E-SAMMA-SIDA SECTION.                                                    
045500                                                                          
045600     IF EGEN-MID OR HELP-MID                                              
045700       IF MID-INPUT = ALL '+'                                             
045800         PERFORM MFS-RENSA-FAELT-IN                                       
045900         MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-IN                            
046000       ELSE                                                               
046100         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
046200         CALL WMEDKONV USING MED-WMEDAREA                                 
046300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
046400         PERFORM EA-MID-INDATA-TILL-MOD                                   
046500       END-IF                                                             
046600     ELSE                                                                 
046700       PERFORM MFS-RENSA-FAELT-IN                                         
046800     END-IF                                                               
046900     .                                                                    
047000     EJECT                                                                
047100                                                                          
047200                                                                          
047300 EA-MID-INDATA-TILL-MOD SECTION.                                          
047400                                                                          
047500     IF MID-SEND-IDDC NOT = ALL '+'                                       
047600        MOVE MFS-ROER-EJ-FAELT TO                                         
047700                            MOD-SEND-IDDC-IN                              
047800        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
047900                            MOD-SEND-IDDC-IN-ATTR                         
048000     ELSE                                                                 
048100        MOVE MFS-RENSA-FAELT TO MOD-SEND-IDDC-IN                          
048200        MOVE MFS-NUM-FAELT-RAETT TO                                       
048300                            MOD-SEND-IDDC-IN-ATTR                         
048400     END-IF                                                               
048500                                                                          
048600     IF MID-RECV-IDDC NOT = ALL '+'                                       
048700        MOVE MFS-ROER-EJ-FAELT TO                                         
048800                            MOD-RECV-IDDC-IN                              
048900        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
049000                            MOD-RECV-IDDC-IN-ATTR                         
049100     ELSE                                                                 
049200        MOVE MFS-RENSA-FAELT TO MOD-RECV-IDDC-IN                          
049300        MOVE MFS-NUM-FAELT-RAETT TO                                       
049400                            MOD-RECV-IDDC-IN-ATTR                         
049500     END-IF                                                               
049600                                                                          
049700     IF MID-FLFLYG NOT = ALL '+'                                          
049800        MOVE MFS-ROER-EJ-FAELT TO                                         
049900                            MOD-FLFLYG-IN                                 
050000        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
050100                            MOD-FLFLYG-IN-ATTR                            
050200     ELSE                                                                 
050300        MOVE MFS-RENSA-FAELT TO MOD-FLFLYG-IN                             
050400        MOVE MFS-ALFA-FAELT-RAETT TO                                      
050500                            MOD-FLFLYG-IN-ATTR                            
050600     END-IF                                                               
050700                                                                          
050800     IF MID-IDKAMPRF NOT = ALL '+'                                        
050900        MOVE MFS-ROER-EJ-FAELT TO                                         
051000                            MOD-IDKAMPRF-IN                               
051100        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
051200                            MOD-IDKAMPRF-IN-ATTR                          
051300     ELSE                                                                 
051400        MOVE MFS-RENSA-FAELT TO MOD-IDKAMPRF-IN                           
051500        MOVE MFS-NUM-FAELT-RAETT TO                                       
051600                            MOD-IDKAMPRF-IN-ATTR                          
051700     END-IF                                                               
051800                                                                          
051900     IF MID-BELAGINS-DEL NOT = ALL '+'                                    
052000        MOVE MFS-ROER-EJ-FAELT TO                                         
052100                            MOD-BELAGINS-DEL-IN                           
052200        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
052300                            MOD-BELAGINS-DEL-IN-ATTR                      
052400     ELSE                                                                 
052500        MOVE MFS-RENSA-FAELT TO MOD-BELAGINS-DEL-IN                       
052600        MOVE MFS-ALFA-FAELT-RAETT TO                                      
052700                            MOD-BELAGINS-DEL-IN-ATTR                      
052800     END-IF                                                               
052900                                                                          
053000     MOVE +1 TO IX                                                        
053100     PERFORM UNTIL IX > MAX-IX                                            
053200        IF MID-IDARTNR-RAD(IX) NOT = ALL '+'                              
053300           MOVE MFS-ROER-EJ-FAELT TO                                      
053400                               MOD-IDARTNR-RAD-IN(IX)                     
053500           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
053600                               MOD-IDARTNR-RAD-IN-ATTR(IX)                
053700        ELSE                                                              
053800           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RAD-IN(IX)                 
053900           MOVE MFS-NUM-FAELT-RAETT TO                                    
054000                               MOD-IDARTNR-RAD-IN-ATTR(IX)                
054100        END-IF                                                            
054200                                                                          
054300        IF MID-KVBEART-RAD(IX) NOT = ALL '+'                              
054400           MOVE MFS-ROER-EJ-FAELT TO                                      
054500                               MOD-KVBEART-RAD-IN(IX)                     
054600           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
054700                               MOD-KVBEART-RAD-IN-ATTR(IX)                
054800        ELSE                                                              
054900           MOVE MFS-RENSA-FAELT TO MOD-KVBEART-RAD-IN(IX)                 
055000           MOVE MFS-NUM-FAELT-RAETT TO                                    
055100                               MOD-KVBEART-RAD-IN-ATTR(IX)                
055200        END-IF                                                            
055300                                                                          
055400        IF MID-BERADREF-RAD(IX) NOT = ALL '+'                             
055500           MOVE MFS-ROER-EJ-FAELT TO                                      
055600                               MOD-BERADREF-RAD-IN(IX)                    
055700           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
055800                               MOD-BERADREF-RAD-IN-ATTR(IX)               
055900        ELSE                                                              
056000           MOVE MFS-RENSA-FAELT TO MOD-BERADREF-RAD-IN(IX)                
056100           MOVE MFS-ALFA-FAELT-RAETT TO                                   
056200                               MOD-BERADREF-RAD-IN-ATTR(IX)               
056300        END-IF                                                            
056400                                                                          
056500        ADD +1 TO IX                                                      
056600     END-PERFORM                                                          
056700                                                                          
056800                                                                          
056900     IF MID-FLSLUT NOT = ALL '+'                                          
057000        MOVE MFS-ROER-EJ-FAELT TO                                         
057100                            MOD-FLSLUT-IN                                 
057200        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
057300                            MOD-FLSLUT-IN-ATTR                            
057400     ELSE                                                                 
057500        MOVE MFS-RENSA-FAELT TO MOD-FLSLUT-IN                             
057600        MOVE MFS-ALFA-FAELT-RAETT TO                                      
057700                            MOD-FLSLUT-IN-ATTR                            
057800     END-IF                                                               
057900                                                                          
058000     .                                                                    
058100     EJECT                                                                
058200                                                                          
058300                                                                          
058400 G-KOLLA-INPUT SECTION.                                                   
058500                                                                          
058600     MOVE SPACES        TO MOD-TEMFSFEL                                   
058700     MOVE JA            TO INDATA-SW                                      
058800     MOVE SPACE         TO WS-IDDC-REF                                    
058900     IF MID-INPUT = ALL '+'                                               
059000        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
059100        MOVE 'GB '         TO MED-IDSKYLT                                 
059200        CALL WMEDKONV USING MED-WMEDAREA                                  
059300        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
059400        PERFORM MFS-ROER-EJ-FAELT-UT                                      
059500        MOVE NEJ TO INDATA-SW                                             
059600     ELSE                                                                 
059700        PERFORM GA-KOLLA-INPUT-1                                          
059800        IF INDATA-FEL                                                     
059900          IF MOD-TEMFSFEL = SPACES                                        
060000           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
060100           MOVE 'GB '         TO MED-IDSKYLT                              
060200           CALL WMEDKONV USING MED-WMEDAREA                               
060300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
060400          END-IF                                                          
060500          PERFORM MFS-ROER-EJ-FAELT-UT                                    
060600          PERFORM MFS-ROER-EJ-FAELT-IN                                    
060700          MOVE MFS-FORMATETS-ATTR TO MOD-IDORDNR7-ATTR                    
060800        ELSE                                                              
060900           PERFORM GB-KOLLA-INPUT-2                                       
061000           IF INDATA-FEL                                                  
061100              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
061200              MOVE 'GB '         TO MED-IDSKYLT                           
061300              CALL WMEDKONV USING MED-WMEDAREA                            
061400              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
061500              PERFORM MFS-ROER-EJ-FAELT-UT                                
061600              PERFORM MFS-ROER-EJ-FAELT-IN                                
061700              MOVE MFS-FORMATETS-ATTR TO MOD-IDORDNR7-ATTR                
061800           ELSE                                                           
061900              PERFORM GC-KOLLA-INPUT-3                                    
062000              IF INDATA-FEL                                               
062100                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
062200                 MOVE 'GB '         TO MED-IDSKYLT                        
062300                 CALL WMEDKONV USING MED-WMEDAREA                         
062400                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
062500                 PERFORM MFS-ROER-EJ-FAELT-UT                             
062600                 PERFORM MFS-ROER-EJ-FAELT-IN                             
062700                 MOVE MFS-FORMATETS-ATTR TO MOD-IDORDNR7-ATTR             
062800              ELSE                                                        
062900                 IF MID-IDKAMPRF NOT = ALL '+'                            
063000                   PERFORM GD-KOLLA-INPUT-4                               
063100                   IF INDATA-FEL                                          
063200                    IF MOD-TEMFSFEL = SPACES                              
063300                      MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL           
063400                      MOVE 'GB '    TO MED-IDSKYLT                        
063500                      CALL WMEDKONV USING MED-WMEDAREA                    
063600                      MOVE MED-MFSFEL TO MOD-TEMFSFEL                     
063700                    END-IF                                                
063800                    PERFORM MFS-ROER-EJ-FAELT-UT                          
063900                    PERFORM MFS-ROER-EJ-FAELT-IN                          
064000                    MOVE MFS-FORMATETS-ATTR TO MOD-IDORDNR7-ATTR          
064100                   END-IF                                                 
064200                 END-IF                                                   
064300              END-IF                                                      
064400           END-IF                                                         
064500        END-IF                                                            
064600     END-IF                                                               
064700     .                                                                    
064800     EJECT                                                                
064900                                                                          
065000                                                                          
065100 GA-KOLLA-INPUT-1 SECTION.                                                
065200                                                                          
065300*FORMELLA KONTROLLER                                                      
065400*SENDING AND RECEIVING DC                                                 
065500     IF MID-SEND-IDDC NOT = ALL '+'                                       
065600     AND MID-RECV-IDDC NOT = ALL '+'                                      
065700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-SEND-IDDC-IN-ATTR               
065800                                      MOD-RECV-IDDC-IN-ATTR               
065900                                                                          
066000         MOVE MID-RECV-IDDC        TO W-IDDC-B601                         
066100                                      W-IDDC                              
066200                                                                          
066300         MOVE MID-SEND-IDDC        TO W-IDDC-B616                         
066310         PERFORM IMS-GU-WDB616                                            
066320         IF SEGMENT-SAKNAS                                                
066330           MOVE NEJ                TO INDATA-SW                           
066340           MOVE MFS-ALFA-FAELT-FEL TO MOD-SEND-IDDC-IN-ATTR               
066350                                        MOD-RECV-IDDC-IN-ATTR             
066360                                                                          
066370           MOVE ERR-WRONG-DC       TO MED-IDMFSFEL                        
066380           MOVE 'GB '              TO MED-IDSKYLT                         
066390           CALL WMEDKONV        USING MED-WMEDAREA                        
066391           MOVE MED-MFSFEL         TO MOD-TEMFSFEL                        
066392         END-IF                                                           
066400     ELSE                                                                 
066500         MOVE NEJ                  TO INDATA-SW                           
066600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-SEND-IDDC-IN-ATTR               
066700                                      MOD-RECV-IDDC-IN-ATTR               
066800                                                                          
066900         MOVE ERR-WRONG-DC         TO MED-IDMFSFEL                        
067000         MOVE 'GB '                TO MED-IDSKYLT                         
067100         CALL WMEDKONV          USING MED-WMEDAREA                        
067200         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
067300     END-IF                                                               
067400                                                                          
067500                                                                          
067600*FLFLYG                                                                   
067700     IF MID-FLFLYG NOT = ALL '+'                                          
067800        IF MID-FLFLYG = NEJ OR JA OR YES                                  
067900           MOVE MFS-ALFA-FAELT-RAETT TO                                   
068000                      MOD-FLFLYG-IN-ATTR                                  
068100        ELSE                                                              
068200            MOVE NEJ TO INDATA-SW                                         
068300            MOVE MFS-ALFA-FAELT-FEL TO                                    
068400                            MOD-FLFLYG-IN-ATTR                            
068500        END-IF                                                            
068600     ELSE                                                                 
068700         MOVE NEJ TO INDATA-SW                                            
068800         MOVE MFS-ALFA-FAELT-FEL TO                                       
068900                         MOD-FLFLYG-IN-ATTR                               
069000     END-IF                                                               
069100                                                                          
069200                                                                          
069300*IDKAMPRF                                                                 
069400     IF MID-IDKAMPRF NOT = ALL '+'                                        
069500        IF MID-IDKAMPRF NUMERIC                                           
069600           MOVE MID-IDKAMPRF          TO WS-IDKAMPRF                      
069700           INSPECT WS-IDKAMPRF REPLACING LEADING SPACE BY ZERO            
069800           IF WS-IDKAMPRF NUMERIC AND WS-IDKAMPRF > ZERO                  
069900             MOVE MID-SEND-IDDC       TO W-KAMP-IDDC                      
070000             MOVE WS-IDKAMPRF         TO W-KAMP-IDKAMPRF                  
070100             PERFORM IMS-GU-WDM201                                        
070200             IF SEGMENT-FINNS                                             
070300               MOVE KAMP-TISTADAT     TO WS-START-DATUM                   
070400               IF KAMP-TISTODAT > 0                                       
070500                 MOVE KAMP-TISTODAT   TO WS-STOPP-DATUM                   
070600               ELSE                                                       
070700                 MOVE 999999          TO WS-STOPP-DATUM                   
070800               END-IF                                                     
070900               MOVE MFS-NUM-FAELT-RAETT TO                                
071000                          MOD-IDKAMPRF-IN-ATTR                            
071100             ELSE                                                         
071200               MOVE NEJ TO INDATA-SW                                      
071300               MOVE MFS-NUM-FAELT-FEL TO                                  
071400                             MOD-IDKAMPRF-IN-ATTR                         
071500               MOVE 'CAMPAIGN REF DO NOT EXIST' TO MOD-TEMFSINF           
071600             END-IF                                                       
071700           ELSE                                                           
071800             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKAMPRF-IN-ATTR             
071900             MOVE NEJ                 TO NYCKLAR-SW                       
072000           END-IF                                                         
072100        ELSE                                                              
072200           MOVE NEJ                   TO INDATA-SW                        
072300           MOVE MFS-NUM-FAELT-FEL     TO                                  
072400                         MOD-IDKAMPRF-IN-ATTR                             
072500           MOVE 'CAMPAIGN REF NOT NUMERIC' TO MOD-TEMFSINF                
072600        END-IF                                                            
072700     END-IF                                                               
072800                                                                          
072900                                                                          
073000*BELAGINS-DEL                                                             
073100     IF MID-BELAGINS-DEL NOT = ALL '+'                                    
073200        MOVE MFS-ALFA-FAELT-RAETT TO                                      
073300                   MOD-BELAGINS-DEL-IN-ATTR                               
073400     ELSE                                                                 
073500         MOVE MFS-ALFA-FAELT-RAETT TO                                     
073600                         MOD-BELAGINS-DEL-IN-ATTR                         
073700     END-IF                                                               
073800                                                                          
073900                                                                          
074000*ORDERRAD                                                                 
074100     MOVE 1 TO IX                                                         
074200     PERFORM UNTIL IX > MAX-IX                                            
074300        IF MID-IDARTNR-RAD(IX) NOT = ALL '+'                              
074400           IF MID-IDARTNR-RAD(IX) NUMERIC                                 
074500              MOVE MFS-NUM-FAELT-RAETT TO                                 
074600                         MOD-IDARTNR-RAD-IN-ATTR(IX)                      
074700           ELSE                                                           
074800              MOVE MFS-NUM-FAELT-FEL   TO                                 
074900                            MOD-IDARTNR-RAD-IN-ATTR(IX)                   
075000              MOVE NEJ TO INDATA-SW                                       
075100           END-IF                                                         
075200        ELSE                                                              
075300            MOVE MFS-NUM-FAELT-RAETT TO                                   
075400                            MOD-IDARTNR-RAD-IN-ATTR(IX)                   
075500        END-IF                                                            
075600                                                                          
075700        IF MID-KVBEART-RAD(IX) NOT = ALL '+'                              
075800           IF MID-KVBEART-RAD(IX) NUMERIC                                 
075900              MOVE MFS-NUM-FAELT-RAETT TO                                 
076000                         MOD-KVBEART-RAD-IN-ATTR(IX)                      
076100           ELSE                                                           
076200              MOVE MFS-NUM-FAELT-FEL   TO                                 
076300                            MOD-KVBEART-RAD-IN-ATTR(IX)                   
076400              MOVE NEJ TO INDATA-SW                                       
076500           END-IF                                                         
076600        ELSE                                                              
076700            MOVE MFS-NUM-FAELT-RAETT TO                                   
076800                            MOD-KVBEART-RAD-IN-ATTR(IX)                   
076900        END-IF                                                            
077000                                                                          
077100        IF MID-BERADREF-RAD(IX) NOT = ALL '+'                             
077200           MOVE MFS-ALFA-FAELT-RAETT TO                                   
077300                      MOD-BERADREF-RAD-IN-ATTR(IX)                        
077400        ELSE                                                              
077500            MOVE MFS-ALFA-FAELT-RAETT TO                                  
077600                            MOD-BERADREF-RAD-IN-ATTR(IX)                  
077700        END-IF                                                            
077800        ADD +1 TO IX                                                      
077900     END-PERFORM                                                          
078000                                                                          
078100                                                                          
078200*FLSLUT                                                                   
078300     IF MID-FLSLUT NOT = ALL '+'                                          
078400        IF MID-FLSLUT =  JA OR YES OR NEJ                                 
078500           MOVE MFS-ALFA-FAELT-RAETT TO                                   
078600                      MOD-FLSLUT-IN-ATTR                                  
078700        ELSE                                                              
078800            MOVE NEJ TO INDATA-SW                                         
078900            MOVE MFS-ALFA-FAELT-FEL TO                                    
079000                            MOD-FLSLUT-IN-ATTR                            
079100        END-IF                                                            
079200     ELSE                                                                 
079300         MOVE NEJ TO INDATA-SW                                            
079400         MOVE MFS-ALFA-FAELT-FEL TO                                       
079500                         MOD-FLSLUT-IN-ATTR                               
079600     END-IF                                                               
079700     .                                                                    
079800     EJECT                                                                
079900                                                                          
080000                                                                          
080100 GB-KOLLA-INPUT-2 SECTION.                                                
080200                                                                          
080300*KOLLA OM IFYLLDA PÅ IFYLLDA RADER  HAR BÅDE ARTNR OCH BEART              
080400                                                                          
080500     MOVE 1 TO IX                                                         
080600     PERFORM UNTIL IX > MAX-IX                                            
080700        IF MID-IDARTNR-RAD(IX) = ALL '+' AND                              
080800           MID-KVBEART-RAD(IX) = ALL '+'                                  
080900        OR                                                                
081000           MID-IDARTNR-RAD(IX) NOT = ALL '+' AND                          
081100           MID-KVBEART-RAD(IX) NOT = ALL '+'                              
081200           MOVE MFS-NUM-FAELT-RAETT TO                                    
081300                      MOD-IDARTNR-RAD-IN-ATTR(IX)                         
081400                      MOD-KVBEART-RAD-IN-ATTR(IX)                         
081500        ELSE                                                              
081600           MOVE NEJ TO INDATA-SW                                          
081700           IF MID-IDARTNR-RAD(IX) = ALL '+'                               
081800              MOVE MFS-NUM-FAELT-FEL   TO                                 
081900                            MOD-IDARTNR-RAD-IN-ATTR(IX)                   
082000           END-IF                                                         
082100           IF MID-KVBEART-RAD(IX) = ALL '+'                               
082200              MOVE MFS-NUM-FAELT-FEL   TO                                 
082300                            MOD-KVBEART-RAD-IN-ATTR(IX)                   
082400           END-IF                                                         
082500                                                                          
082600        END-IF                                                            
082700        ADD +1 TO IX                                                      
082800     END-PERFORM                                                          
082900                                                                          
083000                                                                          
083100*KOLLA OM ALLA RADER ÄR IFYLLDA ELLER ORDERN ÄR AVSLUTAD                  
083200                                                                          
083300     IF INDATA-OK                                                         
083400        MOVE ZERO TO WS-ANTAL-IFYLLDA                                     
083500        MOVE 1 TO IX                                                      
083600        PERFORM UNTIL IX > MAX-IX                                         
083700                                                                          
083800           IF MID-IDARTNR-RAD(IX) NOT = ALL '+' AND                       
083900              MID-KVBEART-RAD(IX) NOT = ALL '+'                           
084000              COMPUTE WS-ANTAL-IFYLLDA =                                  
084100                      WS-ANTAL-IFYLLDA + 1                                
084200                                                                          
084300           END-IF                                                         
084400           ADD +1 TO IX                                                   
084500        END-PERFORM                                                       
084600                                                                          
084700        IF WS-ANTAL-IFYLLDA < MAX-IX AND MID-FLSLUT = NEJ                 
084800           MOVE NEJ TO INDATA-SW                                          
084900           MOVE MFS-ALFA-FAELT-FEL   TO                                   
085000                            MOD-FLSLUT-IN-ATTR                            
085100           MOVE MED-5 TO MOD-TEMFSINF                                     
085200        END-IF                                                            
085300     END-IF                                                               
085400     .                                                                    
085500     EJECT                                                                
085600                                                                          
085700                                                                          
085800 GC-KOLLA-INPUT-3 SECTION.                                                
085900                                                                          
086000*ORDERRAD                                                                 
086100*IDARTNR                                                                  
086200                                                                          
086300     IF INDATA-OK                                                         
086400        MOVE +1 TO IX                                                     
086500        PERFORM UNTIL IX > MAX-IX                                         
086600           IF MID-IDARTNR-RAD(IX) NOT = ALL '+'                           
086700              MOVE MID-IDARTNR-RAD(IX) TO W-IDARTNR                       
086800              PERFORM IMS-GU-WDK601                                       
086900              IF SEGMENT-FINNS                                            
087000                 IF ART-KDERS-UTG = ZERO                                  
087100********************W-IDDC ÄR MOTTAGANDE DC                               
087200                    IF W-IDDC = '11'                                      
087300                      PERFORM GCA-REFILL-TILL-CDC                         
087400                    ELSE                                                  
087500                      PERFORM IMS-GU-ARTS11                               
087600                      IF SEGMENT-FINNS                                    
087700                         MOVE SLAG-IDDC-REF TO REF-WS-IDDC                
087800                                               WS-IDDC-REF                
088100                         IF REF-CDC-SE OR REF-NDC                         
088200                            MOVE SLAG-ADLAGOMR TO WS-ADLAGOMR-X           
088300                            MOVE SLAG-ADGANG TO WS-ADGANG-X               
088400                            MOVE SLAG-ADPLATS TO WS-ADPLATS-X             
088500                            MOVE WS-ADART-X  TO TAB-ADART-X(IX)           
088600                            IF SLAG-IDDC-REF NOT = MID-SEND-IDDC          
088700                              MOVE NEJ TO INDATA-SW                       
088800                              MOVE MFS-NUM-FAELT-FEL TO                   
088900                                   MOD-IDARTNR-RAD-IN-ATTR(IX)            
089000                            MOVE 'PART NOT REFILLED FROM SEND DC'         
089100                                             TO MOD-TEMFSINF              
089110                            ELSE                                          
089120                              IF SLAG-FLFLYG = 'S' AND                    
089130                                 MID-FLFLYG = JA                          
089140                                MOVE NEJ TO INDATA-SW                     
089150                                MOVE MFS-NUM-FAELT-FEL TO                 
089160                                   MOD-IDARTNR-RAD-IN-ATTR(IX)            
089170                                MOVE 'NOT ALLOWED BY AIR'                 
089180                                             TO MOD-TEMFSINF              
089200                              END-IF                                      
089210                            END-IF                                        
089300                         ELSE                                             
089400                            MOVE NEJ TO INDATA-SW                         
089500                            MOVE MFS-NUM-FAELT-FEL TO                     
089600                                 MOD-IDARTNR-RAD-IN-ATTR(IX)              
089700                            MOVE MED-2 TO MOD-TEMFSINF                    
089800                         END-IF                                           
089900                      ELSE                                                
090000                         MOVE NEJ TO INDATA-SW                            
090100                         MOVE MFS-NUM-FAELT-FEL TO                        
090200                              MOD-IDARTNR-RAD-IN-ATTR(IX)                 
090300                         MOVE MED-1 TO MOD-TEMFSINF                       
090400                      END-IF                                              
090500                    END-IF                                                
090600                 ELSE                                                     
090700                    MOVE NEJ TO INDATA-SW                                 
090800                    MOVE MFS-NUM-FAELT-FEL TO                             
090900                         MOD-IDARTNR-RAD-IN-ATTR(IX)                      
091000                    MOVE INF-PART-SUPERSEDED TO MED-IDMFSFEL              
091100                    MOVE 'GB '         TO MED-IDSKYLT                     
091200                    CALL WMEDKONV USING MED-WMEDAREA                      
091300                    MOVE MED-MFSFEL TO MOD-TEMFSINF                       
091400                 END-IF                                                   
091500              ELSE                                                        
091600                 MOVE NEJ TO INDATA-SW                                    
091700                 MOVE MFS-NUM-FAELT-FEL TO                                
091800                      MOD-IDARTNR-RAD-IN-ATTR(IX)                         
091900                 MOVE INF-PART-MISSING TO MED-IDMFSFEL                    
092000                 MOVE 'GB '         TO MED-IDSKYLT                        
092100                 CALL WMEDKONV USING MED-WMEDAREA                         
092200                 MOVE MED-MFSFEL TO MOD-TEMFSINF                          
092300              END-IF                                                      
092400           END-IF                                                         
092500           ADD 1 TO IX                                                    
092600        END-PERFORM                                                       
092700     END-IF                                                               
092800     .                                                                    
092900     EJECT                                                                
093000 GCA-REFILL-TILL-CDC SECTION.                                             
093100                                                                          
093200     PERFORM IMS-GNP-WDK611                                               
093300     IF SEGMENT-FINNS                                                     
093400       PERFORM IMS-GNP-WDK629                                             
093500       IF SEGMENT-FINNS                                                   
093600          MOVE CLAG-IDDC-REF TO REF-WS-IDDC                               
093700                                WS-IDDC-REF                               
093900          IF REF-NDC                                                      
094000             MOVE CLAG-ADLAGOMR TO WS-ADLAGOMR-X                          
094100             MOVE CLAG-ADGANG TO WS-ADGANG-X                              
094200             MOVE CLAG-ADPLATS TO WS-ADPLATS-X                            
094300             MOVE WS-ADART-X                 TO TAB-ADART-X(IX)           
094400             IF CLAG-IDDC-REF NOT = MID-SEND-IDDC                         
094500               MOVE NEJ TO INDATA-SW                                      
094600               MOVE MFS-NUM-FAELT-FEL TO                                  
094700                    MOD-IDARTNR-RAD-IN-ATTR(IX)                           
094800               MOVE 'PART NOT REFILLED FROM SEND DC'                      
094900                              TO MOD-TEMFSINF                             
094910             ELSE                                                         
094911               IF CREF-FLFLYG = 'S' AND                                   
094912                  MID-FLFLYG = JA                                         
094913                 MOVE NEJ TO INDATA-SW                                    
094914                 MOVE MFS-NUM-FAELT-FEL TO                                
094915                    MOD-IDARTNR-RAD-IN-ATTR(IX)                           
094916                 MOVE 'NOT ALLOWED BY AIR'                                
094917                              TO MOD-TEMFSINF                             
094918               END-IF                                                     
095000             END-IF                                                       
095100          ELSE                                                            
095200             MOVE NEJ TO INDATA-SW                                        
095300             MOVE MFS-NUM-FAELT-FEL TO                                    
095400                  MOD-IDARTNR-RAD-IN-ATTR(IX)                             
095500             MOVE MED-2 TO MOD-TEMFSINF                                   
095600          END-IF                                                          
095700       ELSE                                                               
095800          MOVE NEJ TO INDATA-SW                                           
095900          MOVE MFS-NUM-FAELT-FEL TO                                       
096000               MOD-IDARTNR-RAD-IN-ATTR(IX)                                
096100          MOVE MED-1 TO MOD-TEMFSINF                                      
096200       END-IF                                                             
096300     ELSE                                                                 
096400       MOVE NEJ TO INDATA-SW                                              
096500       MOVE MFS-NUM-FAELT-FEL TO                                          
096600            MOD-IDARTNR-RAD-IN-ATTR(IX)                                   
096700       MOVE MED-1 TO MOD-TEMFSINF                                         
096800     END-IF                                                               
096900     .                                                                    
097000     EJECT                                                                
097100 GD-KOLLA-INPUT-4 SECTION.                                                
097200                                                                          
097300*                                                                         
097400* KOLLA SÅ ATT KAMPANJEN HAR STARTAT                                      
097500*                                                                         
097600     IF WS-START-DATUM = 0                                                
097700       MOVE NEJ TO INDATA-SW                                              
097800       MOVE MFS-NUM-FAELT-FEL TO                                          
097900            MOD-IDKAMPRF-IN-ATTR                                          
098000       MOVE 'CAMPAIGN NOT STARTED' TO MOD-TEMFSINF                        
098100     ELSE                                                                 
098200       IF WS-START-DATUM > DAGENS-DATUM                                   
098300         MOVE NEJ TO INDATA-SW                                            
098400         MOVE MFS-NUM-FAELT-FEL TO                                        
098500              MOD-IDKAMPRF-IN-ATTR                                        
098600         MOVE 'CAMPAIGN NOT STARTED' TO MOD-TEMFSINF                      
098700       END-IF                                                             
098800     END-IF                                                               
098900                                                                          
099000*                                                                         
099100* KOLLA SÅ ATT KAMPANJ INTE ÄR AVSLUTAD                                   
099200*                                                                         
099300     IF INDATA-OK                                                         
099400       IF WS-STOPP-DATUM < DAGENS-DATUM                                   
099500         MOVE NEJ TO INDATA-SW                                            
099600         MOVE MFS-NUM-FAELT-FEL TO                                        
099700              MOD-IDKAMPRF-IN-ATTR                                        
099800         MOVE 'CAMPAIGN HAS ENDED' TO MOD-TEMFSINF                        
099900       END-IF                                                             
100000     END-IF                                                               
100100                                                                          
100200* CAMPAIGN REF IS ONLY POSSIBLE TO UPDATE IF THE SENDING DC IS            
100300* DC 11                                                                   
100400     IF INDATA-OK                                                         
100500       IF MID-SEND-IDDC = WS-CDC-11                                       
100600         CONTINUE                                                         
100700       ELSE                                                               
100800         MOVE NEJ                  TO INDATA-SW                           
100900         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDKAMPRF-IN-ATTR                
101000         MOVE 'NOT POSSIBLE TO UPDATE'                                    
101100                                   TO MOD-TEMFSFEL                        
101200       END-IF                                                             
101300     END-IF                                                               
101400                                                                          
101500*                                                                         
101600* KOLLA ATT ALLA RADER ÄR I SAMMA KAMPANJ REF                             
101700*                                                                         
101800     IF INDATA-OK                                                         
101900       MOVE +1 TO IX                                                      
102000       PERFORM UNTIL IX > MAX-IX OR SEGMENT-SAKNAS OR INDATA-FEL          
102100         IF MID-IDARTNR-RAD(IX) NOT = ALL '+'                             
102200           PERFORM S31-FIXA-KUNDNR-ORDKL-FAKTYP                           
102300           MOVE MID-IDARTNR-RAD(IX)   TO W-IDARTNR                        
102400                                         W-KART-IDARTNR                   
102500           MOVE WS-IDKAMPRF           TO W-KAMP-IDKAMPRF                  
102600           MOVE MID-SEND-IDDC         TO W-KAMP-IDDC                      
102700                                                                          
102800           MOVE WS-IDDISTR            TO W-KMRK-IDDISTR-FOM               
102900           MOVE WS-IDDISTR            TO W-KMRK-IDDISTR-TOM               
103000           MOVE WS-IDKUNDNR           TO W-KMRK-IDKUNDNR-FOM              
103100           MOVE WS-IDKUNDNR           TO W-KMRK-IDKUNDNR-TOM              
103200           PERFORM S20-FINN-INTERVALL                                     
103300           PERFORM IMS-GU-WDM221                                          
103400           IF SEGMENT-FINNS                                               
103500**           KONTROLLERA ANTAL                                            
103600             COMPUTE WS-KVAR-KAMP = KMRK-KVBEART-KAMP -                   
103700                                    KMRK-KVBEART-KUND                     
103800             MOVE MID-KVBEART-RAD(IX) TO WS-KVBEART-TRUNK                 
103900             IF WS-KVBEART-TRUNK > WS-KVAR-KAMP                           
104000               MOVE NEJ TO INDATA-SW                                      
104100               MOVE MFS-NUM-FAELT-FEL TO                                  
104200                              MOD-KVBEART-RAD-IN-ATTR(IX)                 
104300               MOVE 'WRONG QUANTITY'  TO MOD-TEMFSINF                     
104400             END-IF                                                       
104500                                                                          
104600           ELSE                                                           
104700             MOVE NEJ TO INDATA-SW                                        
104800             MOVE MFS-NUM-FAELT-FEL   TO                                  
104900                                MOD-IDARTNR-RAD-IN-ATTR(IX)               
105000             MOVE 'NOT IN CAMPAIGN REF' TO MOD-TEMFSINF                   
105100           END-IF                                                         
105200         END-IF                                                           
105300         ADD  +1 TO IX                                                    
105400       END-PERFORM                                                        
105500     END-IF                                                               
105600     .                                                                    
105700     EJECT                                                                
105800                                                                          
105900 H-SKAPA-DISPATCHERTRANS SECTION.                                         
106000                                                                          
106100     PERFORM IMS-GU-WDB601                                                
107000     IF WS-IDDC-REF = '11'                                                
107100       MOVE DCS-IDDISTR-REFILL TO WS-IDDISTR                              
107200                                  WS-IDDISTR-TRUNK                        
107300     ELSE                                                                 
107500       PERFORM IMS-GNP-WDB616                                             
107600       MOVE B616-REF-IDDISTR-REFILL TO WS-IDDISTR                         
107700                                  WS-IDDISTR-TRUNK                        
107800     END-IF                                                               
107900     MOVE MSG-SIGNON-USERID TO W-IDUSER                                   
108000     MOVE WS-IDDISTR        TO W-IDDISTR                                  
108100     PERFORM IMS-GHU-WL250511                                             
108200     IF SEGMENT-SAKNAS                                                    
108300        PERFORM S31-FIXA-KUNDNR-ORDKL-FAKTYP                              
108400        PERFORM S32-SKAPA-ORDERNR-W411ORDN                                
108500        PERFORM S33-SKAPA-MSG-KOM-AREA                                    
108600        PERFORM S34-SKAPA-ORDERHUVUD                                      
108700        PERFORM S35-ORDERHUVUD-TILL-DISPATCHER                            
108800        PERFORM S36A-SKAPA-ORDERRADER-NY                                  
108900        IF MID-FLSLUT = JA OR YES                                         
109000           MOVE JA             TO ORAD-MID-FLSLUT                         
109100        ELSE                                                              
109200           MOVE NEJ            TO ORAD-MID-FLSLUT                         
109300           PERFORM S40-SKAPA-2506                                         
109400           PERFORM IMS-ISRT-WL250511                                      
109500        END-IF                                                            
109600        MOVE WS-IDKUNDRF    TO MOD-IDORDNR7                               
109700     ELSE                                                                 
109800        PERFORM S33-SKAPA-MSG-KOM-AREA                                    
109900        PERFORM S36B-SKAPA-ORDERRADER-GAMMAL                              
110000        IF MID-FLSLUT = JA OR YES                                         
110100           MOVE JA             TO ORAD-MID-FLSLUT                         
110200           PERFORM IMS-DLET-WL250511                                      
110300        ELSE                                                              
110400           MOVE NEJ            TO ORAD-MID-FLSLUT                         
110500        END-IF                                                            
110600        MOVE 2506-IDORDNR7  TO MOD-IDORDNR7                               
110700     END-IF                                                               
110800     IF ORAD-IX > 1                                                       
110900       PERFORM S37-ORDERRADER-TILL-DISPATCHER                             
111000     END-IF                                                               
111100     .                                                                    
111200     EJECT                                                                
111300                                                                          
111400                                                                          
111500 I-VISA-SKICKAD-ORDERDEL SECTION.                                         
111600                                                                          
111700     MOVE MFS-ADD-HILIGHT-FIELD  TO MOD-IDORDNR7-ATTR                     
111800                                                                          
111900     IF MID-FLSLUT = JA OR YES                                            
112000        MOVE MFS-RENSA-FAELT TO MOD-SEND-IDDC-IN                          
112100                                MOD-RECV-IDDC-IN                          
112200                                MOD-FLFLYG-IN                             
112300                                MOD-FLSLUT-IN                             
112400        MOVE MED-6           TO MOD-TEMFSINF                              
112500     ELSE                                                                 
112600        MOVE MFS-ROER-EJ-FAELT TO                                         
112700                            MOD-SEND-IDDC-IN                              
112800                            MOD-RECV-IDDC-IN                              
112900                            MOD-FLFLYG-IN                                 
113000                            MOD-FLSLUT-IN                                 
113100        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
113200                            MOD-SEND-IDDC-IN-ATTR                         
113300                            MOD-RECV-IDDC-IN-ATTR                         
113400                            MOD-FLFLYG-IN-ATTR                            
113500        MOVE MED-7             TO MOD-TEMFSINF                            
113600     END-IF                                                               
113700                                                                          
113800     PERFORM MFS-RENSA-UPPDAT-FAELT-IN                                    
113900     .                                                                    
114000     EJECT                                                                
114100                                                                          
114200 S20-FINN-INTERVALL SECTION.                                              
114300                                                                          
114400     PERFORM IMS-GU-WDM211                                                
114500     IF SEGMENT-FINNS                                                     
114600       PERFORM IMS-GNP-WDM221                                             
114700       PERFORM UNTIL SEGMENT-SAKNAS                                       
114800         IF  WS-IDDISTR > KMRK-IDDISTR-TOM                                
114900         OR  WS-IDDISTR < KMRK-IDDISTR-FOM                                
115000           CONTINUE                                                       
115100         ELSE                                                             
115200           IF  WS-IDDISTR  = KMRK-IDDISTR-TOM                             
115300           AND WS-IDKUNDNR > KMRK-IDKUNDNR-TOM                            
115400             CONTINUE                                                     
115500           ELSE                                                           
115600             IF  WS-IDDISTR  = KMRK-IDDISTR-FOM                           
115700             AND WS-IDKUNDNR < KMRK-IDKUNDNR-FOM                          
115800               CONTINUE                                                   
115900             ELSE                                                         
116000               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
116100               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
116200               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
116300               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
116400             END-IF                                                       
116500           END-IF                                                         
116600         END-IF                                                           
116700         PERFORM IMS-GNP-WDM221                                           
116800       END-PERFORM                                                        
116900     END-IF                                                               
117000     .                                                                    
117100     EJECT                                                                
117200                                                                          
117300 S31-FIXA-KUNDNR-ORDKL-FAKTYP SECTION.                                    
117400                                                                          
117500     PERFORM IMS-GU-WDB601                                                
117600     MOVE SPACE              TO WS-KDFAKTYP                               
120600     IF WS-IDDC-REF = '11'                                                
120700       IF MID-FLFLYG = YES OR JA                                          
120800         MOVE 1              TO WS-KDORDKL                                
120900         MOVE DCS-IDKUNDNR-SBPS                                           
121000                               TO WS-IDKUNDNR                             
121100         MOVE DCS-KDFRAKT-SBPS TO WS-KDFRAKT                              
121200       ELSE                                                               
121300         MOVE 4              TO WS-KDORDKL                                
121400         MOVE DCS-IDKUNDNR-BPS TO WS-IDKUNDNR                             
121500         MOVE DCS-KDFRAKT-BPS TO WS-KDFRAKT                               
121600       END-IF                                                             
121700       MOVE DCS-IDDISTR-REFILL TO WS-IDDISTR                              
121800                                  WS-IDDISTR-TRUNK                        
122000     ELSE                                                                 
122100       PERFORM IMS-GNP-WDB616                                             
122200       IF MID-FLFLYG = YES OR JA                                          
122300         MOVE 1              TO WS-KDORDKL                                
122400         MOVE B616-REF-IDKUNDNR-SBPS                                      
122500                               TO WS-IDKUNDNR                             
122600         MOVE DCS-KDFRAKT-SBPS TO WS-KDFRAKT                              
122700       ELSE                                                               
122800         MOVE 4              TO WS-KDORDKL                                
122900         MOVE B616-REF-IDKUNDNR-BPS TO WS-IDKUNDNR                        
123000         MOVE DCS-KDFRAKT-BPS TO WS-KDFRAKT                               
123100       END-IF                                                             
123200       MOVE B616-REF-IDDISTR-REFILL TO WS-IDDISTR                         
123300                                  WS-IDDISTR-TRUNK                        
124600     END-IF                                                               
124700*                                                                         
124800     MOVE WS-IDKUNDNR        TO WS-IDKUNDNR-TRUNK                         
124900     MOVE WS-KDFRAKT         TO WS-KDFRAKT-TRUNK                          
125000     MOVE WS-KDORDKL         TO WS-KDORDKL-TRUNK                          
125100     .                                                                    
125200     EJECT                                                                
125300                                                                          
125400                                                                          
125500 S32-SKAPA-ORDERNR-W411ORDN  SECTION.                                     
125600                                                                          
125700     MOVE 'W203'               TO ORDN-IDSYSTEM                           
125800                                                                          
125900     MOVE WS-IDDISTR           TO ORDN-IDDISTR                            
126000     MOVE WS-IDKUNDNR          TO ORDN-IDKUNDNR                           
126100                                                                          
126200     MOVE ZERO                 TO ORDN-IDORDNR-IN                         
126300                                                                          
126400     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
126500                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
126600                                                                          
126700     MOVE ORDN-IDORDNR-UT           TO WS-IDKUNDRF                        
126800     .                                                                    
126900     EJECT                                                                
127000                                                                          
127100                                                                          
127200 S33-SKAPA-MSG-KOM-AREA SECTION.                                          
127300     SKIP2                                                                
127400     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
127500     MOVE +54                    TO MSG-KOM-KVLL                          
127600     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
127700     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
127800     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
127900     MOVE 'BYPASS  '             TO MSG-KOM-IDSNDNOD                      
128000     MOVE 'W2036200'             TO MSG-KOM-IDSNDJOB                      
128100     MOVE 'W4I25101'             TO MSG-KOM-IDCPYTXT                      
128200     MOVE DAGENS-DATUM           TO MSG-KOM-TIREGDAT                      
128300     MOVE DAGENS-TID             TO MSG-KOM-TIKLOCK                       
128400     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
128500     .                                                                    
128600     EJECT                                                                
128700                                                                          
128800                                                                          
128900 S34-SKAPA-ORDERHUVUD SECTION.                                            
129000     SKIP2                                                                
129100     MOVE 'W4I25101'             TO MSG-KOM-IDCPYTXT                      
129200                                                                          
129300     MOVE SPACE TO OHUV-MID-W4I25101                                      
129400     IF MID-IDKAMPRF NOT = ALL '+'                                        
129500       MOVE 'REFT'          TO OHUV-MID-IDSYSTEM                          
129600     ELSE                                                                 
129700       MOVE 'REFB'          TO OHUV-MID-IDSYSTEM                          
129800     END-IF                                                               
129900     MOVE WS-IDDISTR-TRUNK  TO OHUV-MID-IDDISTR                           
130000     MOVE WS-IDKUNDNR-TRUNK TO OHUV-MID-IDKUNDNR                          
130100     MOVE WS-KDFRAKT-TRUNK  TO OHUV-MID-KDFRAKT                           
130200     MOVE WS-IDKUNDRF       TO OHUV-MID-IDORDNR                           
130300     MOVE WS-KDORDKL-TRUNK  TO OHUV-MID-KDORDKL                           
130400     IF MID-BELAGINS-DEL NOT = ALL '+'                                    
130500        MOVE MID-BELAGINS-DEL TO OHUV-MID-BELAGINS                        
130600     END-IF                                                               
130700     IF MID-IDKAMPRF NOT = ALL '+'                                        
130800        MOVE MID-IDKAMPRF     TO OHUV-MID-IDKAMPRF                        
130900     END-IF                                                               
131000                                                                          
131100     MOVE NEJ                  TO OHUV-MID-FLAUTPAC                       
131200                                  OHUV-MID-FLAUTFAK                       
131300                                  OHUV-MID-FLEMBORD                       
131400                                  OHUV-MID-FLOVRLEV                       
131500                                  OHUV-MID-FLRESTN                        
131600                                  OHUV-MID-FLFORBI                        
131700                                  OHUV-MID-FLORDTIL                       
131800     MOVE ZERO                 TO OHUV-MID-TIREPDAT                       
131900                                  OHUV-MID-IDGROSS                        
132000     .                                                                    
132100     EJECT                                                                
132200                                                                          
132300                                                                          
132400 S35-ORDERHUVUD-TILL-DISPATCHER SECTION.                                  
132500                                                                          
132600     MOVE LENGTH OF OHUV-MID-W4I25101 TO P-TO-P-KVLL                      
132700     ADD  +17              TO P-TO-P-KVLL                                 
132800     MOVE 'W4T251X '       TO P-TO-P-KDTRANS                              
132900     MOVE '4251'           TO P-TO-P-IDTRANS                              
133000     MOVE '1'              TO P-TO-P-KDMFSFOR                             
133100                                                                          
133200     MOVE KOM-AREA                TO P-TO-P-DATA                          
133300     CALL W006KOM USING MSG-PCB                                           
133400                        ALT-PCB                                           
133500                        KOMA-PCB                                          
133600                        MSG-KOM-WMSGKOM                                   
133700                        P-TO-P-SW                                         
133800     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
133900*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
134000*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
134100        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
134200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
134300     END-IF                                                               
134400     .                                                                    
134500     EJECT                                                                
134600                                                                          
134700                                                                          
134800 S36A-SKAPA-ORDERRADER-NY SECTION.                                        
134900     SKIP2                                                                
135000                                                                          
135100     MOVE 'W4I25201'        TO MSG-KOM-IDCPYTXT                           
135200                                                                          
135300     MOVE LENGTH OF ORAD-MID-W4I25201 TO P-TO-P-KVLL                      
135400     ADD  +17              TO P-TO-P-KVLL                                 
135500     MOVE 'W4T252X '       TO P-TO-P-KDTRANS                              
135600     MOVE '4252'           TO P-TO-P-IDTRANS                              
135700     MOVE '1'              TO P-TO-P-KDMFSFOR                             
135800                                                                          
135900     MOVE SPACE TO ORAD-MID-W4I25201                                      
136000     MOVE 'REPB'            TO ORAD-MID-IDSYSTEM                          
136100     MOVE WS-IDDISTR-TRUNK  TO ORAD-MID-IDDISTR                           
136200     MOVE WS-IDKUNDNR-TRUNK TO ORAD-MID-IDKUNDNR                          
136300     MOVE WS-IDKUNDRF       TO ORAD-MID-IDORDNR                           
136400     MOVE +1 TO IX                                                        
136500                ORAD-IX                                                   
136600     PERFORM  UNTIL IX > MAX-IX                                           
136700        IF MID-IDARTNR-RAD(IX) NOT = ALL '+'                              
136800           MOVE MID-IDARTNR-RAD(IX) TO W-IDARTNR                          
136900           MOVE MID-KVBEART-RAD(IX) TO WS-KVBEART                         
137000**************W-IDDC = MOTTAGANDE REFILL DC                               
137100           IF W-IDDC = '11'                                               
137200             PERFORM IMS-GHU-WDK611                                       
137300             COMPUTE CLAG-KVBEART =                                       
137400                     CLAG-KVBEART + WS-KVBEART                            
137500             END-COMPUTE                                                  
137600             PERFORM IMS-REPL-WDK611                                      
137700             PERFORM IMS-GHNP-WDK629                                      
137800             IF SEGMENT-FINNS                                             
137900               MOVE DAGENS-DATUM TO CREF-TIORDREG                         
138000               PERFORM IMS-REPL-WDK629                                    
138100             END-IF                                                       
138200           ELSE                                                           
138300             PERFORM IMS-GHU-ARTS11                                       
138400             COMPUTE SLAG-KVBEART =                                       
138500                     SLAG-KVBEART + WS-KVBEART                            
138600             END-COMPUTE                                                  
138700             IF SLAG-TIDATUM-CROSS > 0                                    
138800               MOVE ZERO TO SLAG-TIDATUM-CROSS                            
138900             END-IF                                                       
139000             MOVE DAGENS-DATUM TO SLAG-TIORDREG                           
139100             PERFORM IMS-REPL-ARTS11                                      
139200           END-IF                                                         
139300                                                                          
139400           MOVE MID-IDARTNR-RAD(IX) TO ORAD-MID-IDARTNR(ORAD-IX)          
139500           PERFORM S39-W009KSIF                                           
139600           MOVE MID-KVBEART-RAD(IX) TO WS-KVBEART-TRUNK                   
139700           MOVE WS-KVBEART-TRUNK TO ORAD-MID-KVBEART (ORAD-IX)            
139800           INSPECT ORAD-MID-KVBEART (ORAD-IX)                             
139900                       REPLACING ALL SPACE BY ZERO                        
140000           IF MID-BERADREF-RAD(IX) NOT = ALL '+'                          
140100              MOVE MID-BERADREF-RAD(IX) TO                                
140200                       ORAD-MID-BERADREF(ORAD-IX)                         
140300           ELSE                                                           
140400              MOVE TAB-ADART-X(IX)  TO                                    
140500                       ORAD-MID-BERADREF(ORAD-IX)                         
140600           END-IF                                                         
140700           MOVE NEJ              TO ORAD-MID-FLRESTN(ORAD-IX)             
140800* EFTERSOM PÅ BILDEN KAN MAN HA 12 RADER OCH 4252-RAD-TABELLEN            
140900* HAR MINSKATS TILL 6 SÅ MÅSTE MAN BRYTA HÄR OCH ANROPA W006KOM!          
141000           IF ORAD-IX = ORAD-IX-MAX                                       
141100             IF MID-IDARTNR-RAD(IX + 1) = ALL '+'                         
141200               IF MID-FLSLUT = JA OR YES                                  
141300                  MOVE JA      TO ORAD-MID-FLSLUT                         
141400               ELSE                                                       
141500                  MOVE NEJ     TO ORAD-MID-FLSLUT                         
141600               END-IF                                                     
141700             ELSE                                                         
141800               MOVE NEJ              TO ORAD-MID-FLSLUT                   
141900             END-IF                                                       
142000             PERFORM S37-ORDERRADER-TILL-DISPATCHER                       
142100* BLANKAR UT RAD-TABELLEN(BARA!)                                          
142200             MOVE +1    TO ORAD-IX                                        
142300             PERFORM  UNTIL ORAD-IX > ORAD-IX-MAX                         
142400               MOVE SPACE TO ORAD-MID-RADER(ORAD-IX)                      
142500               ADD +1   TO ORAD-IX                                        
142600             END-PERFORM                                                  
142700             MOVE +0    TO ORAD-IX                                        
142800           END-IF                                                         
142900                                                                          
143000           ADD +1 TO ORAD-IX                                              
143100        END-IF                                                            
143200        ADD +1 TO IX                                                      
143300     END-PERFORM                                                          
143400     .                                                                    
143500     EJECT                                                                
143600                                                                          
143700 S36B-SKAPA-ORDERRADER-GAMMAL SECTION.                                    
143800     SKIP2                                                                
143900                                                                          
144000     MOVE 'W4I25201'       TO MSG-KOM-IDCPYTXT                            
144100                                                                          
144200     MOVE LENGTH OF ORAD-MID-W4I25201 TO P-TO-P-KVLL                      
144300     ADD  +17              TO P-TO-P-KVLL                                 
144400     MOVE 'W4T252X '       TO P-TO-P-KDTRANS                              
144500     MOVE '4252'           TO P-TO-P-IDTRANS                              
144600     MOVE '1'              TO P-TO-P-KDMFSFOR                             
144700                                                                          
144800     MOVE SPACE TO ORAD-MID-W4I25201                                      
144900     MOVE 'REFB'             TO ORAD-MID-IDSYSTEM                         
145000     MOVE 2506-IDDISTR       TO WS-IDDISTR-TRUNK                          
145100     MOVE WS-IDDISTR-TRUNK   TO ORAD-MID-IDDISTR                          
145200     MOVE 2506-IDKUNDNR      TO WS-IDKUNDNR-TRUNK                         
145300     MOVE WS-IDKUNDNR-TRUNK  TO ORAD-MID-IDKUNDNR                         
145400     MOVE 2506-IDKUNDRF      TO ORAD-MID-IDORDNR                          
145500     MOVE +1    TO IX                                                     
145600                  ORAD-IX                                                 
145700     PERFORM  UNTIL IX > MAX-IX                                           
145800        IF MID-IDARTNR-RAD(IX) NOT = ALL '+'                              
145900           MOVE MID-IDARTNR-RAD(IX) TO W-IDARTNR                          
146000           MOVE MID-KVBEART-RAD(IX) TO WS-KVBEART                         
146100**************W-IDDC = MOTTAGANDE REFILL DC                               
146200           IF W-IDDC = '11'                                               
146300             PERFORM IMS-GHU-WDK611                                       
146400             COMPUTE CLAG-KVBEART =                                       
146500                     CLAG-KVBEART + WS-KVBEART                            
146600             END-COMPUTE                                                  
146700             PERFORM IMS-REPL-WDK611                                      
146800           ELSE                                                           
146900             PERFORM IMS-GHU-ARTS11                                       
147000             COMPUTE SLAG-KVBEART =                                       
147100                     SLAG-KVBEART + WS-KVBEART                            
147200             END-COMPUTE                                                  
147300             IF SLAG-TIDATUM-CROSS > 0                                    
147400               MOVE ZERO TO SLAG-TIDATUM-CROSS                            
147500             END-IF                                                       
147600             MOVE DAGENS-DATUM TO SLAG-TIORDREG                           
147700             PERFORM IMS-REPL-ARTS11                                      
147800           END-IF                                                         
147900                                                                          
148000           MOVE MID-IDARTNR-RAD(IX) TO ORAD-MID-IDARTNR(ORAD-IX)          
148100           PERFORM S39-W009KSIF                                           
148200           MOVE MID-KVBEART-RAD(IX) TO WS-KVBEART-TRUNK                   
148300           MOVE WS-KVBEART-TRUNK TO ORAD-MID-KVBEART (ORAD-IX)            
148400           INSPECT ORAD-MID-KVBEART (ORAD-IX)                             
148500                       REPLACING ALL SPACE BY ZERO                        
148600           IF MID-BERADREF-RAD(IX) NOT = ALL '+'                          
148700              MOVE MID-BERADREF-RAD(IX) TO                                
148800                       ORAD-MID-BERADREF(ORAD-IX)                         
148900           ELSE                                                           
149000              MOVE TAB-ADART-X(IX)  TO                                    
149100                       ORAD-MID-BERADREF(ORAD-IX)                         
149200           END-IF                                                         
149300* EFTERSOM PÅ BILDEN KAN MAN HA 12 RADER OCH 4252-RAD-TABELLEN            
149400* HAR MINSKATS TILL 6 SÅ MÅSTE MAN BRYTA HÄR OCH ANROPA W006KOM!          
149500           IF ORAD-IX = ORAD-IX-MAX                                       
149600             IF MID-IDARTNR-RAD(IX + 1) = ALL '+'                         
149700               IF MID-FLSLUT = JA OR YES                                  
149800                  MOVE JA      TO ORAD-MID-FLSLUT                         
149900               ELSE                                                       
150000                  MOVE NEJ     TO ORAD-MID-FLSLUT                         
150100               END-IF                                                     
150200             ELSE                                                         
150300               MOVE NEJ              TO ORAD-MID-FLSLUT                   
150400             END-IF                                                       
150500             PERFORM S37-ORDERRADER-TILL-DISPATCHER                       
150600* BLANKAR UT RAD-TABELLEN                                                 
150700             MOVE +1    TO ORAD-IX                                        
150800             PERFORM  UNTIL ORAD-IX > ORAD-IX-MAX                         
150900               MOVE SPACE TO ORAD-MID-RADER(ORAD-IX)                      
151000               ADD +1   TO ORAD-IX                                        
151100             END-PERFORM                                                  
151200             MOVE +0    TO ORAD-IX                                        
151300           END-IF                                                         
151400                                                                          
151500           ADD +1 TO ORAD-IX                                              
151600        END-IF                                                            
151700        ADD +1 TO IX                                                      
151800     END-PERFORM                                                          
151900     .                                                                    
152000     EJECT                                                                
152100                                                                          
152200 S37-ORDERRADER-TILL-DISPATCHER SECTION.                                  
152300                                                                          
152400     MOVE KOM-AREA         TO P-TO-P-DATA                                 
152500     CALL W006KOM USING MSG-PCB                                           
152600                        ALT-PCB                                           
152700                        KOMA-PCB                                          
152800                        MSG-KOM-WMSGKOM                                   
152900                        P-TO-P-SW                                         
153000                                                                          
153100     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
153200*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS-DB                         
153300*       FELAKTIG DATUM, TID EJ NUM FÅR EJ INTRÄFFS                        
153400        MOVE 'FELAKTIG PÅ INPUT TILL DISPATCHEN' TO FELTEXT               
153500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
153600     END-IF                                                               
153700                                                                          
153800     .                                                                    
153900     EJECT                                                                
154000                                                                          
154100                                                                          
154200 S39-W009KSIF  SECTION.                                                   
154300     SKIP2                                                                
154400*    BERÄKNA KONTROLLSIFFRA FÖR ARTIKELNR                                 
154500     IF ORAD-MID-REKSIFFR (ORAD-IX) = SPACE OR                            
154600        ORAD-MID-REKSIFFR (ORAD-IX) = '0'                                 
154700        IF ORAD-MID-IDARTNR (ORAD-IX) NUMERIC                             
154800           MOVE ORAD-MID-IDARTNR (ORAD-IX) TO REK-IDARTNR                 
154900           MOVE 0                TO REK-REKSIFFR                          
155000           MOVE 9                TO REK-LNGD                              
155100           CALL W009KSIF USING REK-IDARTNR                                
155200                               REK-LNGD                                   
155300                               REK-REKSIFFR                               
155400           MOVE REK-REKSIFFR     TO ORAD-MID-REKSIFFR (ORAD-IX)           
155500        END-IF                                                            
155600     END-IF                                                               
155700     .                                                                    
155800     EJECT                                                                
155900                                                                          
156000                                                                          
156100 S40-SKAPA-2506  SECTION.                                                 
156200                                                                          
156300     MOVE MSG-SIGNON-USERID TO 2506-IDUSER                                
156400     MOVE WS-IDDISTR        TO 2506-IDDISTR                               
156500     MOVE WS-IDKUNDNR       TO 2506-IDKUNDNR                              
156600     MOVE WS-IDKUNDRF       TO 2506-IDKUNDRF                              
156700     .                                                                    
156800     EJECT                                                                
156900                                                                          
157000                                                                          
157100 MFS-RENSA-FAELT-UT SECTION.                                              
157200                                                                          
157300*    --- ALLA UTDATA-FÄLT                                                 
157400*    --- INKL. BLÄDDRINGSNYCKLAR                                          
157500     MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7                                 
157600     .                                                                    
157700     SKIP3                                                                
157800                                                                          
157900 MFS-RENSA-FAELT-IN SECTION.                                              
158000                                                                          
158100*    --- ALLA INDATA-FÄLT                                                 
158200     MOVE MFS-RENSA-FAELT TO MOD-SEND-IDDC-IN                             
158300                             MOD-RECV-IDDC-IN                             
158400                             MOD-FLFLYG-IN                                
158500                             MOD-IDKAMPRF-IN                              
158600                             MOD-BELAGINS-DEL-IN                          
158700                             MOD-FLSLUT-IN                                
158800     MOVE 1 TO IX                                                         
158900     PERFORM UNTIL IX > MAX-IX                                            
159000        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RAD-IN(IX)                    
159100                                MOD-KVBEART-RAD-IN(IX)                    
159200                                MOD-BERADREF-RAD-IN(IX)                   
159300        ADD +1 TO IX                                                      
159400     END-PERFORM                                                          
159500     .                                                                    
159600     EJECT                                                                
159700                                                                          
159800                                                                          
159900 MFS-RENSA-UPPDAT-FAELT-IN SECTION.                                       
160000                                                                          
160100*    --- ALLA INDATA-FÄLT UTOM DISTR, FLFLYG ,FLSLUT OCH IDKAMPRF         
160200     MOVE MFS-RENSA-FAELT TO MOD-BELAGINS-DEL-IN                          
160300     MOVE 1 TO IX                                                         
160400     PERFORM UNTIL IX > MAX-IX                                            
160500        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RAD-IN(IX)                    
160600                                MOD-KVBEART-RAD-IN(IX)                    
160700                                MOD-BERADREF-RAD-IN(IX)                   
160800        ADD +1 TO IX                                                      
160900     END-PERFORM                                                          
161000     .                                                                    
161100     EJECT                                                                
161200                                                                          
161300                                                                          
161400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
161500                                                                          
161600*    --- ALLA UTDATA-FÄLT                                                 
161700*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
161800                                                                          
161900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDORDNR7                               
162000     .                                                                    
162100     EJECT                                                                
162200                                                                          
162300                                                                          
162400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
162500                                                                          
162600*    --- ALLA INDATA-FÄLT                                                 
162700     MOVE MFS-ROER-EJ-FAELT TO MOD-SEND-IDDC-IN                           
162800                               MOD-RECV-IDDC-IN                           
162900                               MOD-FLFLYG-IN                              
163000                               MOD-IDKAMPRF-IN                            
163100                               MOD-BELAGINS-DEL-IN                        
163200                               MOD-FLSLUT-IN                              
163300     MOVE 1 TO IX                                                         
163400     PERFORM UNTIL IX > MAX-IX                                            
163500        MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RAD-IN(IX)                  
163600                                  MOD-KVBEART-RAD-IN(IX)                  
163700                                  MOD-BERADREF-RAD-IN(IX)                 
163800        ADD +1 TO IX                                                      
163900     END-PERFORM                                                          
164000     .                                                                    
164100     EJECT                                                                
164200                                                                          
164300                                                                          
164400 MFS-FORM-ATTR SECTION.                                                   
164500                                                                          
164600*    --- ALLA INDATA-FÄLT                                                 
164700     MOVE MFS-FORMATETS-ATTR TO MOD-SEND-IDDC-IN-ATTR                     
164800                                MOD-RECV-IDDC-IN-ATTR                     
164900                                MOD-FLFLYG-IN-ATTR                        
165000                                MOD-IDKAMPRF-IN-ATTR                      
165100                                MOD-BELAGINS-DEL-IN-ATTR                  
165200                                MOD-FLSLUT-IN-ATTR                        
165300     MOVE 1 TO IX                                                         
165400     PERFORM UNTIL IX > MAX-IX                                            
165500        MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-RAD-IN-ATTR(IX)            
165600                                   MOD-KVBEART-RAD-IN-ATTR(IX)            
165700                                   MOD-BERADREF-RAD-IN-ATTR(IX)           
165800        ADD +1 TO IX                                                      
165900     END-PERFORM                                                          
166000     .                                                                    
166100     EJECT                                                                
166200                                                                          
166300                                                                          
166400* --- IMS SEKTIONER ---                                                   
166500     SKIP3                                                                
166600 IMS-GET-MSG SECTION.                                                     
166700                                                                          
166800     MOVE '  QC' TO GODK-STATUSKODER                                      
166900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
167000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
167100     PERFORM IMS-STATUSKONTROLL                                           
167200     .                                                                    
167300     SKIP3                                                                
167400                                                                          
167500                                                                          
167600 IMS-INSERT-MSG SECTION.                                                  
167700                                                                          
167800     IF ENGLISH-TEXT                                                      
167900       MOVE 'N' TO MFS-KDHUVOMR                                           
168000     END-IF                                                               
168100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
168200     MOVE SPACE TO GODK-STATUSKODER                                       
168300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
168400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
168500     PERFORM IMS-STATUSKONTROLL                                           
168600     .                                                                    
168700     EJECT                                                                
168800                                                                          
168900                                                                          
169000 IMS-GU-WDK601   SECTION.                                                 
169100     MOVE 'IMS-GU-WDK601       ' TO CURRENT-IMS-SECTION                   
169200                                                                          
169300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
169400          DELIMITED BY SIZE INTO SSA1                                     
169500     MOVE '  GE' TO GODK-STATUSKODER                                      
169600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
169700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
169800     PERFORM IMS-STATUSKONTROLL                                           
169900     .                                                                    
170000     EJECT                                                                
170100                                                                          
170200 IMS-GNP-WDK611 SECTION.                                                  
170300     MOVE 'IMS-GNP-WDK611      ' TO CURRENT-IMS-SECTION                   
170400                                                                          
170500     MOVE 'WDK611 ' TO SSA1                                               
170600     MOVE '  GE' TO GODK-STATUSKODER                                      
170700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
170800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
170900     PERFORM IMS-STATUSKONTROLL                                           
171000     .                                                                    
171100     EJECT                                                                
171200                                                                          
171300 IMS-GNP-WDK629 SECTION.                                                  
171400     MOVE 'IMS-GNP-WDK629      ' TO CURRENT-IMS-SECTION                   
171500                                                                          
171600     MOVE 'WDK629 ' TO SSA1                                               
171700     MOVE '  GE' TO GODK-STATUSKODER                                      
171800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
171900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
172000     PERFORM IMS-STATUSKONTROLL                                           
172100     .                                                                    
172200     EJECT                                                                
172300                                                                          
172400 IMS-GHNP-WDK629 SECTION.                                                 
172500                                                                          
172600     MOVE 'WDK629     ' TO SSA1                                           
172700     MOVE '  GE' TO GODK-STATUSKODER                                      
172800     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK629 SSA1                  
172900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
173000     PERFORM IMS-STATUSKONTROLL                                           
173100     .                                                                    
173200     SKIP3                                                                
173300                                                                          
173400 IMS-REPL-WDK629 SECTION.                                                 
173500                                                                          
173600     MOVE '  ' TO GODK-STATUSKODER                                        
173700     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK629                       
173800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
173900     PERFORM IMS-STATUSKONTROLL                                           
174000     .                                                                    
174100     SKIP3                                                                
174200                                                                          
174300 IMS-GHU-WDK611   SECTION.                                                
174400     MOVE 'IMS-GHU-WDK611      ' TO CURRENT-IMS-SECTION                   
174500                                                                          
174600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
174700          DELIMITED BY SIZE INTO SSA1                                     
174800     MOVE 'WDK611'         TO SSA2                                        
174900     MOVE '  ' TO GODK-STATUSKODER                                        
175000     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
175100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
175200     PERFORM IMS-STATUSKONTROLL                                           
175300     .                                                                    
175400     SKIP3                                                                
175500                                                                          
175600 IMS-REPL-WDK611 SECTION.                                                 
175700     MOVE 'IMS-REPL-WDK611     ' TO CURRENT-IMS-SECTION                   
175800                                                                          
175900     MOVE '  ' TO GODK-STATUSKODER                                        
176000     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
176100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
176200     PERFORM IMS-STATUSKONTROLL                                           
176300     .                                                                    
176400     EJECT                                                                
176500                                                                          
176600 IMS-GU-ARTS11 SECTION.                                                   
176700     MOVE 'IMS-GU-ARTS11       ' TO CURRENT-IMS-SECTION                   
176800                                                                          
176900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
177000          DELIMITED BY SIZE INTO SSA1                                     
177100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
177200          DELIMITED BY SIZE INTO SSA2                                     
177300     MOVE '  GE' TO GODK-STATUSKODER                                      
177400     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2             
177500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
177600     PERFORM IMS-STATUSKONTROLL                                           
177700     .                                                                    
177800     EJECT                                                                
177900                                                                          
178000                                                                          
178100 IMS-GHU-ARTS11 SECTION.                                                  
178200     MOVE 'IMS-GHU-ARTS11      ' TO CURRENT-IMS-SECTION                   
178300                                                                          
178400     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
178500          DELIMITED BY SIZE INTO SSA1                                     
178600     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
178700          DELIMITED BY SIZE INTO SSA2                                     
178800     MOVE '  GE' TO GODK-STATUSKODER                                      
178900     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2            
179000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
179100     PERFORM IMS-STATUSKONTROLL                                           
179200     .                                                                    
179300     EJECT                                                                
179400                                                                          
179500                                                                          
179600 IMS-REPL-ARTS11 SECTION.                                                 
179700     MOVE 'IMS-REPL-ARTS11     ' TO CURRENT-IMS-SECTION                   
179800                                                                          
179900     MOVE '  ' TO GODK-STATUSKODER                                        
180000     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-WLARTS11                     
180100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
180200     PERFORM IMS-STATUSKONTROLL                                           
180300     .                                                                    
180400     EJECT                                                                
180500                                                                          
180600                                                                          
180700 IMS-GHU-WL250511 SECTION.                                                
180800     MOVE 'IMS-GHU-WL250511    ' TO CURRENT-IMS-SECTION                   
180900                                                                          
181000     STRING 'WL250501(WDGXKEY  =' W-WDGXKEY-X ')'                         
181100          DELIMITED BY SIZE INTO SSA1                                     
181200     STRING 'WL250511(IDUSER   =' W-IDUSER-X                              
181300                    '&IDDISTR  =' W-IDDISTR-X ')'                         
181400          DELIMITED BY SIZE INTO SSA2                                     
181500     MOVE '  GE' TO GODK-STATUSKODER                                      
181600     CALL CBLTDLI USING GHU 2505-PCB DLI-IO-WL250511 SSA1 SSA2            
181700     MOVE 2505-STATUS-CODE TO STATUS-WS                                   
181800     PERFORM IMS-STATUSKONTROLL                                           
181900     .                                                                    
182000     EJECT                                                                
182100                                                                          
182200                                                                          
182300 IMS-ISRT-WL250511 SECTION.                                               
182400     MOVE 'IMS-ISRT-WL250511   ' TO CURRENT-IMS-SECTION                   
182500                                                                          
182600     STRING 'WL250501(WDGXKEY  =' W-WDGXKEY-X ')'                         
182700          DELIMITED BY SIZE INTO SSA1                                     
182800     MOVE 'WL250511 '         TO SSA2                                     
182900     MOVE '  ' TO GODK-STATUSKODER                                        
183000     CALL CBLTDLI USING ISRT 2505-PCB DLI-IO-WL250511 SSA1 SSA2           
183100     MOVE 2505-STATUS-CODE TO STATUS-WS                                   
183200     PERFORM IMS-STATUSKONTROLL                                           
183300     .                                                                    
183400     EJECT                                                                
183500                                                                          
183600                                                                          
183700 IMS-DLET-WL250511 SECTION.                                               
183800     MOVE 'IMS-DLET-WL250511   ' TO CURRENT-IMS-SECTION                   
183900                                                                          
184000     MOVE '  ' TO GODK-STATUSKODER                                        
184100     CALL CBLTDLI USING DLET 2505-PCB DLI-IO-WL250511                     
184200     MOVE 2505-STATUS-CODE TO STATUS-WS                                   
184300     PERFORM IMS-STATUSKONTROLL                                           
184400     .                                                                    
184500     EJECT                                                                
184600                                                                          
184700 IMS-GU-WDB601    SECTION.                                                
184800     MOVE 'IMS-GU-WDB601       ' TO CURRENT-IMS-SECTION                   
184900                                                                          
185000     STRING 'WDB601  (IDDC     =' W-IDDC-B601-X ')'                       
185100          DELIMITED BY SIZE INTO SSA1                                     
185200     MOVE '  ' TO GODK-STATUSKODER                                        
185300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
185400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
185500     PERFORM IMS-STATUSKONTROLL                                           
185600     .                                                                    
185700     EJECT                                                                
185800                                                                          
185900 IMS-GNP-WDB616    SECTION.                                               
186000     MOVE 'IMS-GNP-WDB616      ' TO CURRENT-IMS-SECTION                   
186100                                                                          
186200     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
186300          DELIMITED BY SIZE INTO SSA1                                     
186400     MOVE '  ' TO GODK-STATUSKODER                                        
186500     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B616 SSA1                
186600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
186700     PERFORM IMS-STATUSKONTROLL                                           
186800     .                                                                    
186900     EJECT                                                                
187000                                                                          
187010 IMS-GU-WDB616    SECTION.                                                
187020     MOVE 'IMS-GU-WDB616      ' TO CURRENT-IMS-SECTION                    
187030                                                                          
187031     STRING 'WDB601  (IDDC     =' W-IDDC-B601-X ')'                       
187032          DELIMITED BY SIZE INTO SSA1                                     
187040     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
187050          DELIMITED BY SIZE INTO SSA2                                     
187060     MOVE '  GE' TO GODK-STATUSKODER                                      
187070     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
187080     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
187090     PERFORM IMS-STATUSKONTROLL                                           
187091     .                                                                    
187092     EJECT                                                                
187093                                                                          
187100 IMS-GU-WDM201 SECTION.                                                   
187200     MOVE 'IMS-GU-WDM201       ' TO CURRENT-IMS-SECTION                   
187300                                                                          
187400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
187500          DELIMITED BY SIZE INTO SSA1                                     
187600     MOVE '  GE'              TO GODK-STATUSKODER                         
187700     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM201 SSA1                    
187800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
187900     PERFORM IMS-STATUSKONTROLL                                           
188000     .                                                                    
188100                                                                          
188200 IMS-GU-WDM211 SECTION.                                                   
188300                                                                          
188400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
188500          DELIMITED BY SIZE INTO SSA1                                     
188600     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
188700          DELIMITED BY SIZE INTO SSA2                                     
188800     MOVE '  GE'              TO GODK-STATUSKODER                         
188900     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
189000     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
189100     PERFORM IMS-STATUSKONTROLL                                           
189200     .                                                                    
189300                                                                          
189400 IMS-GNP-WDM221 SECTION.                                                  
189500                                                                          
189600     MOVE 'WDM221 '           TO SSA1                                     
189700     MOVE '    GE'            TO GODK-STATUSKODER                         
189800     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
189900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
190000     PERFORM IMS-STATUSKONTROLL                                           
190100     .                                                                    
190200                                                                          
190300                                                                          
190400 IMS-GU-WDM221 SECTION.                                                   
190500                                                                          
190600     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
190700          DELIMITED BY SIZE INTO SSA1                                     
190800     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
190900          DELIMITED BY SIZE INTO SSA2                                     
191000     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
191100          DELIMITED BY SIZE INTO SSA3                                     
191200     MOVE '  GE' TO GODK-STATUSKODER                                      
191300     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3          
191400     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
191500     PERFORM IMS-STATUSKONTROLL                                           
191600     .                                                                    
191700                                                                          
191800 IMS-STATUSKONTROLL SECTION.                                              
191900                                                                          
192000     SET STATUS-IX TO 1                                                   
192100     SEARCH GODK-STATUS                                                   
192200       AT END                                                             
192300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
192400         DELIMITED BY SIZE INTO FELTEXT                                   
192500         CALL FELLOG                                                      
192600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
192700         CONTINUE                                                         
193000     END-SEARCH                                                           
200000     .                                                                    
