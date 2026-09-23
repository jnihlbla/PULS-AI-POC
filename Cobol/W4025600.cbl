000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4025600.                                                
000400*AUTHOR.         ANNELIE ENGLUND.                                         
000500*DATE-WRITTEN.   91/06/05.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*     PROGRAMMET ÄR EN BAKGRUNDS-MPP SOM STARTAS UPP AV 0693              
001100*     (DISPATCHER-PROGRAMMET)                                             
001200*     DET TAR EMOT ANNULLERINGAR AV RO/TPO FRÅN VDI                       
001300*                                                                         
001400*    (OBS! DET FINNS ETT HÅL I LOGIKEN. DET GÅR INTE ATT ANNULLERA        
001500*          OBEKRÄFTADE TPO6'OR EFTERSOM DE INTE HAR TPO-DATUM PÅ          
001600*          WDA5 ÄNNU.)                                                    
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T256U                                             
002000*        MID:         W4I25601                                            
002100*                     WMSGKOM                                             
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         WMSGKOM                                             
002500                                                                          
002600* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
002700* ETRACK 7450328 2008-HÖST  VOHF                                          
002800* ETRACK 10254592 2015 DECOMISSION VOHF                                   
002900* ETRACK 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2             
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -COPY WY2000W3                                                       
003700     SKIP3                                                                
003800 77  IDPGM                       PIC X(08)   VALUE 'W4025600'.            
003900                                                                          
004000 77  FELTEXT                     PIC X(32) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  WS1-IDDC                     PIC X(2)    VALUE SPACE.                
004500 77  SPAR-KVART                  PIC 9(6).                                
004600 77  WS-TID                      PIC X(6)    VALUE ZERO.                  
004700 77  DATUM-MED-ARHUNDR           PIC 9(8).                                
004800                                                                          
004900 77  W-TITPO-TIAAVV            PIC 9(4)      VALUE ZERO.                  
005000 77  W-DAGENS-DAT-TIAAVV       PIC 9(4)      VALUE ZERO.                  
005100                                                                          
005200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005300     88  ALLT-OK                             VALUE 'J'.                   
005400*    --- VALID IDDC CODES                                                 
005500*                                                                         
005600*01  -COPY WWDCKONS                                                       
005700                                                                          
005710*01  -COPY WWBYT03                                                        
005720                                                                          
005800 01  WS-DABEHOV.                                                          
005900     03  WS-DABEHOV-SEKEL      PIC 9(2)      VALUE ZERO.                  
006000     03  WS-DABEHOV-AAVV       PIC 9(4)      VALUE ZERO.                  
006100                                                                          
006200*    --- FELKODER TILL MSGKOM-AREA                                        
006300 01  MESSAGE-CODES.                                                       
006400     03  ERR-EJ-NUM              PIC X(3)    VALUE '020'.                 
006500     03  ERR-RADER-SAKNAS        PIC X(3)    VALUE '029'.                 
006600     03  ERR-OTILL-UPPDAT        PIC X(3)    VALUE '007'.                 
006700     03  ERR-TPO-INOM-FRYS       PIC X(3)    VALUE '069'.                 
006800     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
006900     EJECT                                                                
007000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007100 01  GENERELLA-SUBPROGRAM.                                                
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007500     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
007600     SKIP3                                                                
007700*   --- PARAMETRAR TILL SUBPROGRAM W009VADD                               
007800     SKIP2                                                                
007900 01  W009VADD-AREA.                                                       
008000     03  VECKO-DATUM-AAVV         PIC S9(5) COMP-3.                       
008100     03  VECKO-ANTAL              PIC S9(3) COMP-3.                       
008200     EJECT                                                                
008300*   --- PARAMETRAR TILL SUBPROGRAM WDATKONV                               
008400     SKIP2                                                                
008500*01    FILLER -COPY WDATAREAC0                                            
008600     EJECT                                                                
008700*  ---PARAMETRAR TILL IDDISTR                                             
008800     SKIP2                                                                
008900 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
009000*01  FILLER -COPY WWDIST19 -RED TEST-IDDISTR.                             
009100     EJECT                                                                
009200*  ---COPYTEXT TILL RY9-TRANS                                             
009300     SKIP2                                                                
009400*01    -COPY WDGZRY9                                                      
009500     EJECT                                                                
009600*01    -COPY WDGZRY9S                                                     
009700     EJECT                                                                
009800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010100     SKIP3                                                                
010200*01  MID -COPY W4I25601                                                   
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)  VALUE 'MSG-IO-AREA'.          
010500     SKIP3                                                                
010600*01  -COPY WMSGAREA                                                       
010700     EJECT                                                                
010800     05  -COPY W2I10902 -PRE 2109-  -RED MSG-MID-OUT                      
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
011100 01  KOM-IO-AREA.                                                         
011200*03  -COPY WMSGKOM                                                        
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  NYCKLAR-TILL-DLI.                                                    
011700                                                                          
011800     03  W-IDGMTREF-X.                                                    
011900         05  W-IDDISTR            PIC S9(5) COMP-3   VALUE ZERO.          
012000         05  W-IDKUNDNR           PIC S9(7) COMP-3   VALUE ZERO.          
012100         05  W-IDKUNDRF.                                                  
012200             07  W-IDORDNR5       PIC 9(5)           VALUE ZERO.          
012300             07  FILLER           PIC X(5)           VALUE SPACE.         
012400                                                                          
012500     03  W-WDA5MIN-X.                                                     
012600         05  W-IDDISTR-MIN        PIC S9(5) COMP-3   VALUE ZERO.          
012700         05  W-IDKUNDNR-MIN       PIC S9(7) COMP-3   VALUE ZERO.          
012800         05  W-IDKUNDRF-MIN.                                              
012900             07  W-IDORDNR5-MIN   PIC 9(5)           VALUE ZERO.          
013000             07  FILLER           PIC X(5)           VALUE SPACE.         
013100         05  W-IDARTNR-MIN        PIC S9(9) COMP-3   VALUE ZERO.          
013200         05  FILLER               PIC X(2)  VALUE LOW-VALUE.              
013300                                                                          
013400     03  W-WDA5MAX-X.                                                     
013500         05  W-IDDISTR-MAX        PIC S9(5) COMP-3   VALUE ZERO.          
013600         05  W-IDKUNDNR-MAX       PIC S9(7) COMP-3   VALUE ZERO.          
013700         05  W-IDKUNDRF-MAX.                                              
013800             07  W-IDORDNR5-MAX   PIC 9(5)           VALUE ZERO.          
013900             07  FILLER           PIC X(5)           VALUE SPACE.         
014000         05  W-IDARTNR-MAX        PIC S9(9) COMP-3   VALUE ZERO.          
014100         05  FILLER               PIC X(2)  VALUE HIGH-VALUE.             
014200                                                                          
014300     03  W-IDGMTREF-X3.                                                   
014400         05  W-IDDISTR-N3         PIC S9(5) COMP-3   VALUE ZERO.          
014500         05  W-IDKUNDNR-N3        PIC S9(7) COMP-3   VALUE ZERO.          
014600         05  W-IDKUNDRF-N3.                                               
014700             07  W-IDORDNR7       PIC 9(7)           VALUE ZERO.          
014800             07  FILLER           PIC X(3)           VALUE SPACE.         
014900                                                                          
015000     03  W-IDGMT-X.                                                       
015100         05  W-IDDISTR-WDB2       PIC S9(5) COMP-3   VALUE ZERO.          
015200         05  W-IDKUNDNR-WDB2      PIC S9(7) COMP-3   VALUE ZERO.          
015300                                                                          
015400     03  W-IDARTNR-X.                                                     
015500         05  W-IDARTNR            PIC S9(9) COMP-3   VALUE ZERO.          
015600                                                                          
015700     03  W-IDDC-X.                                                        
015800         05  W-IDDC               PIC X(2)  VALUE SPACE.                  
015900                                                                          
016000     03  W-IDDISTR-X.                                                     
016100         05  W-IDDISTR-N4         PIC S9(5) COMP-3   VALUE ZERO.          
016200                                                                          
016300     03  W-KVART-X.                                                       
016400         05  W-KVART              PIC S9(7) COMP-3   VALUE ZERO.          
016500                                                                          
016600     03  W-KDSEGKEY-X.                                                    
016700         05  W-KDSEGKEY           PIC X     VALUE '1'.                    
016800                                                                          
016900     03  W-TITPO-X.                                                       
017000         05  W-TITPO              PIC S9(7) COMP-3   VALUE ZERO.          
017100                                                                          
017200     03  W-DABEHOV-X.                                                     
017300         05  W-DABEHOV            PIC 9(6)     VALUE ZERO.                
017400     EJECT                                                                
017500*                                                                         
017600     03  W-WDQ101KY-MIN-X.                                                
017700         05  W-IDORDER-Q1-MIN     PIC S9(7)    COMP-3.                    
017800         05  W-IDARTNR-Q1-MIN     PIC S9(9)    COMP-3.                    
017900         05  W-IDLOPNR-Q1-MIN     PIC S9(3)    COMP-3.                    
018000         05  W-IDSEKVNR-Q1-MIN    PIC S9(3)    COMP-3.                    
018100         05  FILLER               PIC  X(4)    VALUE LOW-VALUE.           
018200                                                                          
018300     03  W-WDQ101KY-MAX-X.                                                
018400         05  W-IDORDER-Q1-MAX     PIC S9(7)    COMP-3.                    
018500         05  W-IDARTNR-Q1-MAX     PIC S9(9)    COMP-3.                    
018600         05  W-IDLOPNR-Q1-MAX     PIC S9(3)    COMP-3.                    
018700         05  W-IDSEKVNR-Q1-MAX    PIC S9(3)    COMP-3.                    
018800         05  FILLER               PIC  X(4)    VALUE HIGH-VALUE.          
018900*                                                                         
019000     03  W-WDGX2223-X.                                                    
019100         05  W-IDHTYP             PIC X(4)           VALUE '2223'.        
019200         05  W-IDANSK-2223        PIC S9(3) COMP-3   VALUE ZERO.          
019300         05  FILLER               PIC X(24) VALUE LOW-VALUE.              
019400                                                                          
019500     03  W-WDGX2224-X.                                                    
019600         05  W-TISENBEK-DAG       PIC S9(7) COMP-3   VALUE ZERO.          
019700         05  W-TISENBEK-KL        PIC S9(7) COMP-3   VALUE ZERO.          
019800         05  W-KDLARM             PIC S9(3) COMP-3   VALUE ZERO.          
019900     EJECT                                                                
020000     03  W-WDM201-X.                                                      
020100         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
020200         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
020300                                                                          
020400     03  W-WDM211-X.                                                      
020500         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
020600                                                                          
020700     03  W-WDM221-X.                                                      
020800         05  W-KMRK-IDDISTR-FOM  PIC S9(05) VALUE ZERO COMP-3.            
020900         05  W-KMRK-IDDISTR-TOM  PIC S9(05) VALUE ZERO COMP-3.            
021000         05  W-KMRK-IDKUNDNR-FOM PIC S9(07) VALUE ZERO COMP-3.            
021100         05  W-KMRK-IDKUNDNR-TOM PIC S9(07) VALUE ZERO COMP-3.            
021200     EJECT                                                                
021300                                                                          
021400     03  W-WDGX2231-X.                                                    
021500         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
021600         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
021700                                                                          
021800     03  W-WDGX2232-X.                                                    
021900         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
022000         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
022100                                                                          
022200     03  W-IDDC-B6-X.                                                     
022300         05 W-IDDC-B6                  PIC X(2).                          
022400     EJECT                                                                
022500*    --- STATUS-KOD FRÅN IMS                                              
022600 01  STATUS-WS                   PIC XX.                                  
022700     88  SEGMENT-FINNS                       VALUE '  '.                  
022800     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
022900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023100     88  BASEN-SLUT                          VALUE 'GB'.                  
023200     SKIP2                                                                
023300 01  GODK-STATUSKODER.                                                    
023400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023500     SKIP3                                                                
023600 01  SSA1                        PIC X(320).                              
023700 01  SSA2                        PIC X(320).                              
023800 01  SSA3                        PIC X(320).                              
023900     EJECT                                                                
024000*    --- IMS FUNKTIONSKODER                                               
024100*01  -COPY W0003                                                          
024200     EJECT                                                                
024300*    ---  DLI INPUT-OUTPUT AREA                                           
024400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
024500 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ORDP01'.           
024600 01  DLI-IO-ORDP01.                                                       
024700*  03    WDA501 -COPY WDA501 -PRE ORDP-                                   
024800     EJECT                                                                
024900                                                                          
025000 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ARTC11'.           
025100 01  DLI-IO-ARTC11.                                                       
025200*  03    WDK611 -COPY WDK611                                              
025300     EJECT                                                                
025400 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ARTS11'.           
025500 01  DLI-IO-ARTS11.                                                       
025600*  03    WDK711 -COPY WDK711                                              
025700     EJECT                                                                
025800                                                                          
025900 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ZZAC01'.           
026000 01  DLI-IO-ZZAC01.                                                       
026100*  03    WDGZ01 -COPY WDGZ01 -PRE ZZAC-                                   
026200     EJECT                                                                
026300                                                                          
026400 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-GMTA01'.           
026500 01  DLI-IO-GMTA01.                                                       
026600*  03    WDB201 -COPY WDB201 -PRE GMTA-                                   
026700     EJECT                                                                
026800                                                                          
026900 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ARTM01'.           
027000 01  DLI-IO-ARTM01.                                                       
027100*  03    WDK901 -COPY WDK901                                              
027200     EJECT                                                                
027300                                                                          
027400 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ARTM11'.           
027500 01  DLI-IO-ARTM11.                                                       
027600*  03    WDK911 -COPY WDK911                                              
027700     EJECT                                                                
027800                                                                          
027900 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-XXBU11'.           
028000 01  DLI-IO-XXBU11.                                                       
028100*  03    WDGX   -COPY WDGX2224 -PRE XXBU-                                 
028200     EJECT                                                                
028300                                                                          
028400 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ORQI01'.           
028500 01    DLI-IO-ORQI01.                                                     
028600*  03    WDQ201 -COPY WDQ201                                              
028700     EJECT                                                                
028800                                                                          
028900 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-ORQM01'.           
029000 01    DLI-IO-ORQM01.                                                     
029100*  03    WDQ101 -COPY WDQ101 -PRE ORQM-                                   
029200     EJECT                                                                
029300                                                                          
029400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
029500 01  DLI-IO-WDM211.                                                       
029600*    03 -COPY WDM211                                                      
029700     EJECT                                                                
029800                                                                          
029900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
030000 01  DLI-IO-WDM221.                                                       
030100*    03 -COPY WDM221                                                      
030200     EJECT                                                                
030300                                                                          
030400 01  FILLER                  PIC X(16)  VALUE 'IO-AREA-XXBX11'.           
030500 01    DLI-IO-XXBX11.                                                     
030600*  03    WDGX2232 -COPY WDGX2232 -PRE XXBX-                               
030700                                                                          
030800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
030900 01   DLI-IO-AREA-B601.                                                   
031000*     03  -COPY WDB601                                                    
031100     EJECT                                                                
031200 LINKAGE SECTION.                                                         
031300*01    -COPY W0009     -PRE MSG-                                          
031400     EJECT                                                                
031500*01    -COPY W0009     -PRE DISP-                                         
031600     EJECT                                                                
031700*01  -COPY W0009       -PRE 2109-                                         
031800     EJECT                                                                
031900*01    -COPY W0008     -PRE ORDP-                                         
032000     05  FILLER                  PIC X.                                   
032100     EJECT                                                                
032200*01    -COPY W0008     -PRE ORQI-                                         
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01    -COPY W0008     -PRE XXBU-                                         
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800*01    -COPY W0008     -PRE ORQM-                                         
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01    -COPY W0008     -PRE ZZAC-                                         
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400*01    -COPY W0008     -PRE ARTM-                                         
033500     05  FILLER                  PIC X.                                   
033600     EJECT                                                                
033700*01    -COPY W0008     -PRE ARTC-                                         
033800     05  FILLER                  PIC X.                                   
033900     EJECT                                                                
034000*01    -COPY W0008     -PRE ARTS-                                         
034100     05  FILLER                  PIC X.                                   
034200     EJECT                                                                
034300*01    -COPY W0008     -PRE XXBX-                                         
034400     05  FILLER                  PIC X.                                   
034500     EJECT                                                                
034600*01    -COPY W0008     -PRE WDM2-                                         
034700     05  FILLER                  PIC X.                                   
034800     EJECT                                                                
034900*01    -COPY W0008     -PRE GMTA-                                         
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200*01    -COPY W0008     -PRE WDB6-                                         
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500 PROCEDURE DIVISION USING MSG-PCB DISP-PCB 2109-PCB                       
035600                         ORDP-PCB ORQI-PCB                                
035700                         XXBU-PCB ORQM-PCB ZZAC-PCB ARTM-PCB              
035800                         ARTC-PCB ARTS-PCB XXBX-PCB WDM2-PCB              
035900                         GMTA-PCB WDB6-PCB.                               
036000                                                                          
036100     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 2109-PCB                      
036200                         ORDP-PCB ORQI-PCB                                
036300                         XXBU-PCB ORQM-PCB ZZAC-PCB ARTM-PCB              
036400                         ARTC-PCB ARTS-PCB XXBX-PCB WDM2-PCB              
036500                         GMTA-PCB WDB6-PCB.                               
036600                                                                          
036700     PERFORM IMS-GET-MSG                                                  
036800     IF SEGMENT-FINNS                                                     
036900       PERFORM IMS-GN-MSG                                                 
037000     END-IF                                                               
037100     IF SEGMENT-FINNS                                                     
037200       PERFORM A-INIT                                                     
037300       PERFORM B-FORMELL-KONTROLL                                         
037400       IF ALLT-OK                                                         
037500         PERFORM C-KOLLA-OM-ORDERRAD-FINNS                                
037600         IF ALLT-OK                                                       
037700           PERFORM D-BEHANDLA-ANNULLERING                                 
037800         END-IF                                                           
037900       END-IF                                                             
038000     END-IF                                                               
038100     PERFORM Z-FINIT                                                      
038200     MOVE ZERO TO RETURN-CODE                                             
038300     GOBACK                                                               
038400     .                                                                    
038500     EJECT                                                                
038600 A-INIT SECTION.                                                          
038700                                                                          
038800     MOVE JA                          TO ALLT-SW                          
038900     MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I25601-CTX                 
039000     MOVE SPACE                       TO MSG-KOM-IDMFSMED                 
039100     .                                                                    
039200     EJECT                                                                
039300 B-FORMELL-KONTROLL SECTION.                                              
039400                                                                          
039500     IF MID-IDDISTR NUMERIC AND MID-IDDISTR > ZERO                        
039600       MOVE MID-IDDISTR TO W-IDDISTR                                      
039700                           W-IDDISTR-N3                                   
039800                           W-IDDISTR-N4                                   
039900                           W-IDDISTR-MIN                                  
040000                           W-IDDISTR-MAX                                  
040100                           TEST-IDDISTR                                   
040200       IF DIST19-SATS                                                     
040300         MOVE ERR-OTILL-UPPDAT TO MSG-KOM-IDMFSMED                        
040400         MOVE 'R'              TO MSG-KOM-KDSVAR                          
040500         MOVE NEJ TO ALLT-SW                                              
040600       END-IF                                                             
040700     ELSE                                                                 
040800       MOVE ERR-EJ-NUM  TO MSG-KOM-IDMFSMED                               
040900       MOVE 'R'         TO MSG-KOM-KDSVAR                                 
041000       MOVE NEJ TO ALLT-SW                                                
041100     END-IF                                                               
041200                                                                          
041300     IF MID-IDKUNDNR = SPACE                                              
041400       MOVE ZERO TO W-IDKUNDNR                                            
041500                    W-IDKUNDNR-N3                                         
041600                    W-IDKUNDNR-MIN                                        
041700                    W-IDKUNDNR-MAX                                        
041800     ELSE                                                                 
041900       IF MID-IDKUNDNR NUMERIC                                            
042000         MOVE MID-IDKUNDNR TO W-IDKUNDNR                                  
042100                              W-IDKUNDNR-N3                               
042200                              W-IDKUNDNR-MIN                              
042300                              W-IDKUNDNR-MAX                              
042400       ELSE                                                               
042500         MOVE ERR-EJ-NUM  TO MSG-KOM-IDMFSMED                             
042600         MOVE 'R'         TO MSG-KOM-KDSVAR                               
042700         MOVE NEJ TO ALLT-SW                                              
042800       END-IF                                                             
042900     END-IF                                                               
043000                                                                          
043100     IF MID-IDORDNR7 NUMERIC                                              
043200       MOVE MID-IDORDNR7      TO W-IDORDNR7                               
043300       MOVE MID-IDORDNR7(3:5) TO W-IDORDNR5                               
043400                                 W-IDORDNR5-MIN                           
043500                                 W-IDORDNR5-MAX                           
043600     ELSE                                                                 
043700       MOVE ERR-EJ-NUM  TO MSG-KOM-IDMFSMED                               
043800       MOVE 'R'         TO MSG-KOM-KDSVAR                                 
043900       MOVE NEJ TO ALLT-SW                                                
044000     END-IF                                                               
044100                                                                          
044200     IF MID-IDARTNR NUMERIC                                               
044300       MOVE MID-IDARTNR TO W-IDARTNR                                      
044400                           W-IDARTNR-MIN                                  
044500                           W-IDARTNR-MAX                                  
044600     ELSE                                                                 
044700       MOVE ERR-EJ-NUM  TO MSG-KOM-IDMFSMED                               
044800       MOVE 'R'         TO MSG-KOM-KDSVAR                                 
044900       MOVE NEJ TO ALLT-SW                                                
045000     END-IF                                                               
045100                                                                          
045200     IF MID-KVBEART NUMERIC                                               
045300       MOVE MID-KVBEART TO W-KVART                                        
045400                           SPAR-KVART                                     
045500     ELSE                                                                 
045600       MOVE ERR-EJ-NUM  TO MSG-KOM-IDMFSMED                               
045700       MOVE 'R'         TO MSG-KOM-KDSVAR                                 
045800       MOVE NEJ TO ALLT-SW                                                
045900     END-IF                                                               
046000                                                                          
046100     IF MID-TITPO NUMERIC                                                 
046200       MOVE MID-TITPO TO W-TITPO                                          
046300     ELSE                                                                 
046400       MOVE ERR-EJ-NUM  TO MSG-KOM-IDMFSMED                               
046500       MOVE 'R'         TO MSG-KOM-KDSVAR                                 
046600       MOVE NEJ TO ALLT-SW                                                
046700     END-IF                                                               
046800                                                                          
046900     IF MID-IDDC NOT = DCS-IDDC                                           
047000        MOVE MID-IDDC TO W-IDDC-B6                                        
047100        PERFORM IMS-GU-WDB601                                             
047200     END-IF                                                               
047300     IF DCS-KDDC NOT = SPACE AND NOT DCS-DDC                              
047400       MOVE MID-IDDC    TO WS1-IDDC                                       
047500     ELSE                                                                 
047600       MOVE ERR-EJ-NUM  TO MSG-KOM-IDMFSMED                               
047700       MOVE 'R'         TO MSG-KOM-KDSVAR                                 
047800       MOVE NEJ TO ALLT-SW                                                
047900     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200 C-KOLLA-OM-ORDERRAD-FINNS SECTION.                                       
048300                                                                          
048400     IF W-TITPO > ZERO                                                    
048500       PERFORM IMS-02-GHU-ORDP-WDA5                                       
048600       IF SEGMENT-SAKNAS                                                  
048700         MOVE ERR-RADER-SAKNAS TO MSG-KOM-IDMFSMED                        
048800         MOVE 'R'              TO MSG-KOM-KDSVAR                          
048900         MOVE NEJ TO ALLT-SW                                              
049000       END-IF                                                             
049100     ELSE                                                                 
049200       PERFORM IMS-01-GHU-ORDP-WDA5                                       
049300       IF SEGMENT-SAKNAS                                                  
049400         MOVE ERR-RADER-SAKNAS TO MSG-KOM-IDMFSMED                        
049500         MOVE 'R'              TO MSG-KOM-KDSVAR                          
049600         MOVE NEJ TO ALLT-SW                                              
049700       END-IF                                                             
049800     END-IF                                                               
049900     .                                                                    
050000     EJECT                                                                
050100 D-BEHANDLA-ANNULLERING SECTION.                                          
050200                                                                          
050300     PERFORM IMS-GHU-ARTC11                                               
050400                                                                          
050500     IF ORDP-RAD-KDSTARAD = 1 OR 2                                        
050600       PERFORM DA-KOLLA-OM-TPO-INOM-FRYSTID                               
050700       IF ALLT-OK                                                         
050800         PERFORM IMS-DLET-ORDP-WDA5                                       
050900         PERFORM DB-UPPDATERA-SALDO-1-2                                   
051000         PERFORM S01-ORDERBEK-OCH-TRANSAR                                 
051100       END-IF                                                             
051200     ELSE                                                                 
051300       IF ORDP-RAD-KDSTARAD = 3                                           
051400         PERFORM IMS-DLET-ORDP-WDA5                                       
051500         PERFORM DC-UPPDATERA-SALDO-3                                     
051600         PERFORM S01-ORDERBEK-OCH-TRANSAR                                 
051700       END-IF                                                             
051800     END-IF                                                               
051900                                                                          
052000     .                                                                    
052100     EJECT                                                                
052200 DA-KOLLA-OM-TPO-INOM-FRYSTID SECTION.                                    
052300                                                                          
052400     IF ORDP-RAD-KDSTARAD = 1 AND ORDP-RAD-FLTPOBEK = JA                  
052500       MOVE ORDP-RAD-TITPO TO DAT-I-TIDATUM                               
052600       MOVE 'AAMMDD'     TO DAT-KDDATFORM                                 
052700       CALL WDATKONV USING DAT-KDDATFORM                                  
052800                           DAT-I-TIDATUM                                  
052900                           DAT-O-TIDATUM                                  
053000                           DAT-KDSVAR                                     
053100                                                                          
053200       IF DAT-KDSVAR-OK                                                   
053300         MOVE DAT-TIAAVV-GRP TO W-TITPO-TIAAVV                            
053400       ELSE                                                               
053500         MOVE 'FEL TITPO PÅ WDA5 I DA-SECTION' TO FELTEXT                 
053600         CALL FELLOG                                                      
053700       END-IF                                                             
053800                                                                          
053900       MOVE ZERO         TO DAT-I-TIDATUM                                 
054000       MOVE 'IDAG  '     TO DAT-KDDATFORM                                 
054100       CALL WDATKONV USING DAT-KDDATFORM                                  
054200                           DAT-I-TIDATUM                                  
054300                           DAT-O-TIDATUM                                  
054400                           DAT-KDSVAR                                     
054500                                                                          
054600       IF DAT-KDSVAR-OK                                                   
054700         MOVE DAT-TIAAVV-GRP TO W-DAGENS-DAT-TIAAVV                       
054800       ELSE                                                               
054900         MOVE 'FEL FRÅN DATKONV I DA-SECTION' TO FELTEXT                  
055000         CALL FELLOG                                                      
055100       END-IF                                                             
055200                                                                          
055300       MOVE CLAG-KVFRYSTI TO VECKO-ANTAL                                  
055400       MOVE W-DAGENS-DAT-TIAAVV TO VECKO-DATUM-AAVV                       
055500                                                                          
055600       CALL W009VADD USING VECKO-DATUM-AAVV VECKO-ANTAL                   
055700                                                                          
055800       MOVE W-TITPO-TIAAVV     TO TMP1-YYWW                               
055900       MOVE VECKO-DATUM-AAVV   TO TMP2-YYWW                               
056000       PERFORM WY2000P3                                                   
056100       IF TMP1-YYWW   < TMP2-YYWW                                         
056200          MOVE ERR-TPO-INOM-FRYS TO MSG-KOM-IDMFSMED                      
056300          MOVE 'R'               TO MSG-KOM-KDSVAR                        
056400          MOVE NEJ TO ALLT-SW                                             
056500       END-IF                                                             
056600     END-IF                                                               
056700                                                                          
056800     .                                                                    
056900     EJECT                                                                
057000 DB-UPPDATERA-SALDO-1-2 SECTION.                                          
057100                                                                          
057200     IF ORDP-RAD-KDSTARAD = '1'                                           
057300       IF ORDP-RAD-FLTPOBEK = JA                                          
057400         IF ORDP-RAD-KDTPOTYP = +4                                        
057500           PERFORM DBA-UPPDATERA-WDM2                                     
057600         ELSE                                                             
057700           PERFORM DBB-UPPDATERA-WDK9                                     
057800         END-IF                                                           
057900       END-IF                                                             
058000     ELSE                                                                 
058100       IF ORDP-RAD-IDDC NOT = DCS-IDDC                                    
058200          MOVE ORDP-RAD-IDDC TO W-IDDC-B6                                 
058300          PERFORM IMS-GU-WDB601                                           
058400       END-IF                                                             
058500       IF DCS-CDC                                                         
058600         COMPUTE CLAG-KVROS = CLAG-KVROS - SPAR-KVART                     
058700         PERFORM IMS-REPL-ARTC11                                          
058800       ELSE                                                               
058900         MOVE ORDP-RAD-IDDC TO W-IDDC                                     
059000         PERFORM IMS-GHU-ARTS11                                           
059100         IF SEGMENT-FINNS                                                 
059200           IF ORDP-RAD-KDORDKL = +0 OR +1                                 
059300             COMPUTE SLAG-KVROS-DAG = SLAG-KVROS-DAG - SPAR-KVART         
059400           ELSE                                                           
059500             IF ORDP-RAD-KDORDKL = +2 OR +3 OR +4                         
059600               COMPUTE SLAG-KVROS-BULK =                                  
059700                       SLAG-KVROS-BULK - SPAR-KVART                       
059800             END-IF                                                       
059900           END-IF                                                         
060000           PERFORM IMS-REPL-ARTS11                                        
060100         END-IF                                                           
060200       END-IF                                                             
060300     END-IF                                                               
060400     IF (ORDP-RAD-KDTPOTYP =   2 OR 6) AND                                
060500         ORDP-RAD-FLTPOBEK = NEJ                                          
060600       PERFORM DBC-BEHANDLA-LARMKO                                        
060700     END-IF                                                               
060800                                                                          
060900     .                                                                    
061000     EJECT                                                                
061100 DBA-UPPDATERA-WDM2 SECTION.                                              
061200                                                                          
061300     MOVE ORDP-RAD-IDKAMPRF TO W-KAMP-IDKAMPRF                            
061400     MOVE ORDP-RAD-IDDC     TO W-KAMP-IDDC                                
061500     MOVE ORDP-RAD-IDARTNR  TO W-KART-IDARTNR                             
061600                                                                          
061700     PERFORM IMS-GHU-WDM211                                               
061800     IF SEGMENT-FINNS                                                     
061900       SUBTRACT ORDP-RAD-KVART FROM KART-KVBEART-KUND                     
062000       SUBTRACT ORDP-RAD-KVART FROM KART-KVBEART-TPO4                     
062100       PERFORM IMS-REPL-WDM211                                            
062200                                                                          
062300       MOVE ORDP-RAD-IDARTNR  TO W-KART-IDARTNR                           
062400       MOVE ORDP-RAD-IDDISTR  TO W-KMRK-IDDISTR-FOM                       
062500       MOVE ORDP-RAD-IDDISTR  TO W-KMRK-IDDISTR-TOM                       
062600       MOVE ORDP-RAD-IDKUNDNR TO W-KMRK-IDKUNDNR-FOM                      
062700       MOVE ORDP-RAD-IDKUNDNR TO W-KMRK-IDKUNDNR-TOM                      
062800                                                                          
062900       PERFORM S20-FINN-INTERVALL                                         
063100       PERFORM IMS-GHU-WDM221                                             
063200       IF SEGMENT-FINNS                                                   
063300         SUBTRACT ORDP-RAD-KVART FROM KMRK-KVBEART-KUND                   
063400         PERFORM IMS-REPL-WDM221                                          
063500       ELSE                                                               
063600         MOVE ZERO            TO W-KMRK-IDKUNDNR-FOM                      
063700         MOVE ZERO            TO W-KMRK-IDKUNDNR-TOM                      
063800         PERFORM IMS-GHU-WDM221                                           
063900         IF SEGMENT-FINNS                                                 
064000           SUBTRACT ORDP-RAD-KVART FROM KMRK-KVBEART-KUND                 
064100           PERFORM IMS-REPL-WDM221                                        
064200         END-IF                                                           
064300       END-IF                                                             
064400     END-IF                                                               
064500     .                                                                    
064600     EJECT                                                                
064700 DBB-UPPDATERA-WDK9 SECTION.                                              
064800                                                                          
064900     PERFORM IMS-GHU-ARTM-WDK901                                          
065000     SUBTRACT SPAR-KVART FROM ART-SUTPO-TOT                               
065100     PERFORM IMS-REPL-ARTM-WDK901                                         
065200     MOVE ORDP-RAD-TITPO TO DAT-I-TIDATUM                                 
065300     MOVE 'AAMMDD'  TO DAT-KDDATFORM                                      
065400     CALL WDATKONV USING DAT-KDDATFORM                                    
065500                         DAT-I-TIDATUM                                    
065600                         DAT-O-TIDATUM                                    
065700                         DAT-KDSVAR                                       
065800     IF DAT-KDSVAR-OK                                                     
065900       MOVE DAT-TIAAVV-GRP TO WS-DABEHOV-AAVV                             
066000       MOVE DAT-TISEKEL    TO WS-DABEHOV-SEKEL                            
066100       MOVE WS-DABEHOV      TO W-DABEHOV                                  
066200     ELSE                                                                 
066300       MOVE 'FEL TITPO PÅ WDA5 I DBB-SECTION' TO FELTEXT                  
066400       CALL FELLOG                                                        
066500     END-IF                                                               
066600                                                                          
066700     PERFORM IMS-GHNP-ARTM-WDK911                                         
066800     IF ORDP-RAD-KDTPOTYP = 1 OR 2                                        
066900       SUBTRACT SPAR-KVART FROM ANT-SUTPO-PB                              
067000     ELSE                                                                 
067100       SUBTRACT SPAR-KVART FROM ANT-SUTPO-EJPB                            
067200     END-IF                                                               
067300     IF ANT-SUTPO-PB = ZERO AND ANT-SUTPO-EJPB = ZERO                     
067400       PERFORM IMS-DLET-ARTM-WDK9                                         
067500     ELSE                                                                 
067600       PERFORM IMS-REPL-ARTM-WDK911                                       
067700     END-IF                                                               
067800                                                                          
067900     .                                                                    
068000     EJECT                                                                
068100 DBC-BEHANDLA-LARMKO SECTION.                                             
068200                                                                          
068300     MOVE ORDP-RAD-IDANSK TO W-IDANSK-2232                                
068400     PERFORM IMS-GU-XXBX-WDR220                                           
068500     IF SEGMENT-FINNS                                                     
068600       MOVE XXBX-2232-IDANSK-LARM TO W-IDANSK-2223                        
068700     ELSE                                                                 
068800       MOVE ZERO TO W-IDANSK-2223                                         
068900     END-IF                                                               
069000     MOVE ORDP-RAD-DASENDAT (3:6) TO W-TISENBEK-DAG                       
069100     MOVE ORDP-RAD-TISENBEK-KL  TO W-TISENBEK-KL                          
069200     IF ORDP-RAD-KDTPOTYP = 2                                             
069300       MOVE 110 TO W-KDLARM                                               
069400     ELSE                                                                 
069500       MOVE 100 TO W-KDLARM                                               
069600     END-IF                                                               
069700     PERFORM IMS-GHU-XXBU-WDR5                                            
069800     IF SEGMENT-FINNS                                                     
069900       PERFORM IMS-DLET-XXBU-WDR5                                         
070000     END-IF                                                               
070100                                                                          
070200     .                                                                    
070300     EJECT                                                                
070400 DC-UPPDATERA-SALDO-3 SECTION.                                            
070500                                                                          
070600     IF ORDP-RAD-DARODAT > ZERO OR ORDP-RAD-KDTPOTYP = +4                 
070700       IF ORDP-RAD-IDDC NOT = DCS-IDDC                                    
070800          MOVE ORDP-RAD-IDDC TO W-IDDC-B6                                 
070900          PERFORM IMS-GU-WDB601                                           
071000       END-IF                                                             
071100       IF DCS-CDC                                                         
071200         COMPUTE CLAG-KVRESS = CLAG-KVRESS - SPAR-KVART                   
071300         PERFORM IMS-REPL-ARTC11                                          
071400       ELSE                                                               
071500         MOVE ORDP-RAD-IDDC TO W-IDDC                                     
071600         PERFORM IMS-GHU-ARTS11                                           
071700         COMPUTE SLAG-KVRESS = SLAG-KVRESS - SPAR-KVART                   
071800         PERFORM IMS-REPL-ARTS11                                          
071900       END-IF                                                             
072000     ELSE                                                                 
072100       PERFORM IMS-GHU-ARTM-WDK901                                        
072200       IF ORDP-RAD-KDORDKL = 0                                            
072300         COMPUTE ART-KVOKS-VOR =                                          
072400         ART-KVOKS-VOR - SPAR-KVART                                       
072500       END-IF                                                             
072600       IF ORDP-RAD-KDORDKL = 1                                            
072700         COMPUTE ART-KVOKS-DAG =                                          
072800         ART-KVOKS-DAG - SPAR-KVART                                       
072900       END-IF                                                             
073000       IF ORDP-RAD-KDORDKL = 2 OR 3 OR 4                                  
073100         COMPUTE ART-KVOKS-BULK =                                         
073200         ART-KVOKS-BULK - SPAR-KVART                                      
073300       END-IF                                                             
073400       PERFORM IMS-REPL-ARTM-WDK901                                       
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800 Z-FINIT SECTION.                                                         
073900                                                                          
074000     IF MSG-KOM-IDMFSMED = SPACE                                          
074100       MOVE OK-BEHANDLAD TO MSG-KOM-IDMFSMED                              
074200     END-IF                                                               
074300     PERFORM IMS-INSERT-DISP-MSG                                          
074400                                                                          
074500     .                                                                    
074600     EJECT                                                                
074700 S01-ORDERBEK-OCH-TRANSAR SECTION.                                        
074800                                                                          
074900     PERFORM S01A-REDIGERA-WDQ1                                           
075000                                                                          
075100     IF ORDP-RAD-KDSTARAD = 1 OR 2                                        
075200       MOVE 85 TO ORQM-OBKR-KDORDBEK                                      
075300     ELSE                                                                 
075400       MOVE 87 TO ORQM-OBKR-KDORDBEK                                      
075500     END-IF                                                               
075600     MOVE IDPGM                   TO ORQM-OBKR-IDPGM                      
075700                                                                          
075800     PERFORM IMS-ISRT-ORQM-WDQ1                                           
075900                                                                          
076000     PERFORM S01B-SKAPA-RY9-TRANS                                         
076100     IF (ORDP-RAD-KDSTARAD = 2 OR 3) OR ORDP-RAD-KDTPOTYP = 6             
076200       PERFORM S01C-SKAPA-W2I109MID                                       
076300     END-IF                                                               
076400     .                                                                    
076500     EJECT                                                                
076600 S01A-REDIGERA-WDQ1 SECTION.                                              
076700                                                                          
076800***LÄS WDQ2 VIA SEKINDX                                                   
076900                                                                          
077000     PERFORM IMS-GU-ORQI-WDQ2                                             
077100                                                                          
077200     MOVE OHUV-IDORDER              TO ORQM-OBKR-IDORDER                  
077300                                       W-IDORDER-Q1-MIN                   
077400                                       W-IDORDER-Q1-MAX                   
077500     MOVE ORDP-RAD-IDARTNR          TO ORQM-OBKR-IDARTNR                  
077600                                       W-IDARTNR-Q1-MIN                   
077700                                       W-IDARTNR-Q1-MAX                   
077800     MOVE +1                        TO W-IDLOPNR-Q1-MIN                   
077900                                       W-IDLOPNR-Q1-MAX                   
078000                                       W-IDSEKVNR-Q1-MIN                  
078100                                       W-IDSEKVNR-Q1-MAX                  
078200     PERFORM IMS-GU-ORQM-WDQ1                                             
078300     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
078400        ADD +1                      TO W-IDLOPNR-Q1-MIN                   
078500                                       W-IDLOPNR-Q1-MAX                   
078600        PERFORM IMS-GN-ORQM-WDQ1                                          
078700     END-PERFORM                                                          
078800     MOVE W-IDLOPNR-Q1-MIN          TO ORQM-OBKR-IDLOPNR                  
078900     MOVE +1                        TO ORQM-OBKR-IDSEKVNR                 
079000     MOVE ORDP-RAD-IDDC             TO ORQM-OBKR-IDDC                     
079100     MOVE ORDP-RAD-IDDC-RO          TO ORQM-OBKR-IDDC-RO                  
079200     MOVE SPACE                     TO ORQM-OBKR-BEERS                    
079300     MOVE SPACE                     TO ORQM-OBKR-IDBIL                    
079400     MOVE ORDP-RAD-BEKUNDRF         TO ORQM-OBKR-BEKUNDRF                 
079500     MOVE ORDP-RAD-BERADREF         TO ORQM-OBKR-BERADREF                 
079600     MOVE ORDP-RAD-BEVOLREF         TO ORQM-OBKR-BEVOLREF                 
079700     MOVE ORDP-RAD-IDKAMPRF         TO ORQM-OBKR-IDKAMPRF                 
079800     MOVE ZERO                      TO ORQM-OBKR-DIERS-KVOT               
079900     MOVE NEJ                       TO ORQM-OBKR-FLAKPLOC                 
080000     MOVE ORDP-RAD-FLINVEST         TO ORQM-OBKR-FLINVEST                 
080100     MOVE JA                        TO ORQM-OBKR-FLOBOK                   
080200     MOVE NEJ                       TO ORQM-OBKR-FLOBTRAN                 
080300                                       ORQM-OBKR-FLOBPRT                  
080400     MOVE ORDP-RAD-FLPRTILL         TO ORQM-OBKR-FLPRTILL                 
080500     MOVE JA                        TO ORQM-OBKR-FLRESTN                  
080600     MOVE NEJ                       TO ORQM-OBKR-FLSLATT                  
080700     MOVE ORDP-RAD-FLERS            TO ORQM-OBKR-FLTILLK                  
080800     MOVE ZERO                      TO ORQM-OBKR-IDARTNR-TILLK            
080900     MOVE ORDP-RAD-IDDISTR          TO ORQM-OBKR-IDDISTR                  
081000     MOVE ORDP-RAD-IDKUNDNR         TO ORQM-OBKR-IDKUNDNR                 
081100     MOVE '0000000   '              TO ORQM-OBKR-IDKUNDRF                 
081200     MOVE ORDP-RAD-IDORDNR5         TO ORQM-OBKR-IDKUNDRF(3:5)            
081300     MOVE '0000000   '              TO ORQM-OBKR-IDKUNDRF-RO              
081400     MOVE ORDP-RAD-IDLEVNR          TO ORQM-OBKR-IDLEVNR                  
081500     MOVE ORDP-RAD-IDLOPNR          TO ORQM-OBKR-IDLOPNR-RO               
081600     MOVE ORDP-RAD-IDSYSTEM         TO ORQM-OBKR-IDSYSTEM                 
081700     MOVE ORDP-RAD-KDDSP            TO ORQM-OBKR-KDDSP                    
081800     MOVE ZERO                      TO ORQM-OBKR-KDERS                    
081900     MOVE ORDP-RAD-KDKVBRYT         TO ORQM-OBKR-KDKVBRYT                 
082000     MOVE ORDP-RAD-KDOI             TO ORQM-OBKR-KDOI                     
082100     MOVE ORDP-RAD-CLEARGROUP       TO ORQM-OBKR-CLEARGROUP               
082200     MOVE ORDP-RAD-KDPRTYP          TO ORQM-OBKR-KDPRTYP                  
082300     MOVE ORDP-RAD-KDTPOTYP         TO ORQM-OBKR-KDTPOTYP                 
082400     MOVE ORDP-RAD-KDVRINFO         TO ORQM-OBKR-KDVRINFO                 
082500     MOVE SPAR-KVART                TO ORQM-OBKR-KVANNANT                 
082600     MOVE ZERO                      TO ORQM-OBKR-KVAVBART                 
082700     MOVE ORDP-RAD-KVART            TO ORQM-OBKR-KVBEART                  
082800                                       ORQM-OBKR-KVBEART-Q                
082900     MOVE ZERO                      TO ORQM-OBKR-KVBEART-TILLK            
083000                                       ORQM-OBKR-KVPREAVB                 
083100                                       ORQM-OBKR-KVPRERO                  
083200                                       ORQM-OBKR-KVQPACK                  
083300                                       ORQM-OBKR-KVRO                     
083400                                       ORQM-OBKR-KVSLATT                  
083500     MOVE ORDP-RAD-PRARTNTO         TO ORQM-OBKR-PRARTNTO                 
083600     MOVE ZERO                      TO ORQM-OBKR-PRBPRIS                  
083700     MOVE ORDP-RAD-REKSIFFR         TO ORQM-OBKR-REKSIFFR                 
083800     MOVE ZERO                      TO ORQM-OBKR-REKSIFFR-TILLK           
083900                                       ORQM-OBKR-RERF-RAD                 
084000                                       ORQM-OBKR-TIDISPIN                 
084100     MOVE OHUV-TIREGDAT             TO ORQM-OBKR-TIORDREG                 
084200     MOVE ZERO                      TO ORQM-OBKR-TIPRIS                   
084300                                       ORQM-OBKR-TIRODAT                  
084400     ACCEPT ORQM-OBKR-TIREGDAT FROM DATE                                  
084500     ACCEPT WS-TID             FROM TIME                                  
084600     MOVE WS-TID   TO ORQM-OBKR-TIREGTID                                  
084700                                                                          
084800     IF ORQM-OBKR-TIREGDAT < 500000                                       
084900       ADD +20000000 TO ORQM-OBKR-TIREGDAT GIVING                         
085000                                       DATUM-MED-ARHUNDR                  
085100     ELSE                                                                 
085200       ADD +19000000 TO ORQM-OBKR-TIREGDAT GIVING                         
085300                                       DATUM-MED-ARHUNDR                  
085400     END-IF                                                               
085500     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
085600                                     ORQM-OBKR-TITIREGD-9KOMPL            
085700                                                                          
085800     MOVE ORDP-RAD-TITPO            TO ORQM-OBKR-TITPO                    
085900                                                                          
086000     IF OHUV-TIREGDAT < 500000                                            
086100       ADD +20000000 TO OHUV-TIREGDAT GIVING                              
086200                                       DATUM-MED-ARHUNDR                  
086300     ELSE                                                                 
086400       ADD +19000000 TO OHUV-TIREGDAT GIVING                              
086500                                       DATUM-MED-ARHUNDR                  
086600     END-IF                                                               
086700     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
086800                                     ORQM-OBKR-TITIORDD-9KOMPL            
086900                                                                          
087000     MOVE ORDP-RAD-KDFRAKT          TO ORQM-OBKR-KDFRAKT                  
087100     MOVE OHUV-KDORDKL              TO ORQM-OBKR-KDORDKL                  
087200     MOVE ORDP-RAD-IDPRQUES        TO ORQM-OBKR-IDPRQUES                  
087300     MOVE ORDP-RAD-PRARTNTO-LOC    TO ORQM-OBKR-PRARTNTO-LOC              
087400     MOVE ORDP-RAD-PRARTNTO-LOCPREL TO ORQM-OBKR-PRARTNTO-LOCPREL         
087500     MOVE ORDP-RAD-PRARTBTO-LOC    TO ORQM-OBKR-PRARTBTO-LOC              
087600     MOVE ORDP-RAD-KDVALISO        TO ORQM-OBKR-KDVALISO                  
087700     MOVE ORDP-RAD-KDVAT           TO ORQM-OBKR-KDVAT                     
087800     MOVE ORDP-RAD-RERAB           TO ORQM-OBKR-RERAB                     
087900     MOVE ORDP-RAD-KDRAB           TO ORQM-OBKR-KDRAB                     
088000     MOVE ORDP-RAD-BEART-VIPS      TO ORQM-OBKR-BEART-VIPS                
088100                                                                          
088200     MOVE OHUV-KDORDTYP-LDC        TO ORQM-OBKR-KDORDTYP-LDC              
088300     MOVE OHUV-TIREPDAT            TO ORQM-OBKR-TIREPDAT                  
088400     MOVE ORDP-RAD-IDKUNDRF-WIP    TO ORQM-OBKR-IDKUNDRF-WIP              
088510     MOVE ZERO                     TO ORQM-OBKR-TIDLEVDAT                 
088520     MOVE ORDP-RAD-PRAVCOST        TO ORQM-OBKR-PRAVCOST                  
088600     .                                                                    
088700     EJECT                                                                
088800 S01B-SKAPA-RY9-TRANS  SECTION.                                           
088900                                                                          
089000     MOVE 'RY9'                 TO RY9-IDPTYP                             
089100     MOVE ORDP-RAD-BERADREF     TO RY9-BERADREF                           
089200     MOVE ORDP-RAD-BEVOLREF     TO RY9-BEVOLREF                           
089300     MOVE ORDP-RAD-FLERS        TO RY9-FLERS                              
089400     MOVE ORDP-RAD-IDARTNR      TO RY9-IDARTNR                            
089500     MOVE ZERO                  TO RY9-IDDIVORD                           
089600     MOVE ORDP-RAD-IDKUNDRF     TO RY9-IDKUNDRF                           
089700     MOVE ORDP-RAD-IDLOPNR      TO RY9-IDLOPNR                            
089800     MOVE MSG-LTERM-NAME        TO RY9-IDUSER                             
089900     MOVE ORDP-RAD-KDFAKTYP     TO RY9-KDFAKTYP                           
090000     MOVE ORDP-RAD-KDKVBRYT     TO RY9-KDKVBRYT                           
090100     MOVE ORDP-RAD-KDORDKL      TO RY9-KDORDKL                            
090200     MOVE ZERO                  TO RY9-KVART                              
090300     MOVE ORDP-RAD-KDDSP        TO RY9-KDDSP                              
090400     MOVE ORDP-RAD-KDRAPRIO     TO RY9-KDRAPRIO                           
090500     MOVE ORDP-RAD-KDSTARAD     TO RY9-KDSTARAD                           
090600     MOVE ORDP-RAD-KDTPOTYP     TO RY9-KDTPOTYP                           
090700     MOVE ORDP-RAD-IDARTNR      TO W-IDARTNR                              
090800     MOVE CLAG-KDUART           TO RY9-KDUART                             
090900     IF ORDP-RAD-KDTPOTYP = 6 OR ORDP-RAD-KDSTARAD = 2 OR                 
091000                                 ORDP-RAD-KDSTARAD = 3                    
091100         MOVE ORDP-RAD-KVART   TO RY9-KVART                               
091200     END-IF                                                               
091300     MOVE ORDP-RAD-KDVRINFO     TO RY9-KDVRINFO                           
091400     IF ORDP-RAD-KDTPOTYP = 1 AND ORDP-RAD-IDSYSTEM = 'VR'                
091500       MOVE 1              TO RY9-KDVRTPO                                 
091600     END-IF                                                               
091700     IF ORDP-RAD-KDTPOTYP = 1 AND ORDP-RAD-IDSYSTEM NOT = 'VR'            
091800       MOVE 2              TO RY9-KDVRTPO                                 
091900     END-IF                                                               
092000     IF ORDP-RAD-KDTPOTYP NOT = 1                                         
092100       MOVE 0              TO RY9-KDVRTPO                                 
092200     END-IF                                                               
092300     MOVE ORDP-RAD-PRARTNTO     TO RY9-PRARTNTO                           
092400     MOVE ORDP-RAD-TIREGDAT     TO RY9-TIREGDAT                           
092500     MOVE ORDP-RAD-TIRES        TO RY9-TIRES                              
092600     MOVE ORDP-RAD-TITPO        TO RY9-TITPO                              
092700     MOVE ORDP-RAD-DARODAT (3:6) TO RY9-TIRODAT                           
092800     MOVE ORDP-RAD-IDDISTR      TO W-IDDISTR-WDB2                         
092900     MOVE ORDP-RAD-IDKUNDNR     TO W-IDKUNDNR-WDB2                        
093000     PERFORM IMS-GU-GMTA-WDB2                                             
093100     MOVE GMTA-GMT-FLVR         TO RY9-FLVR                               
093200     MOVE GMTA-GMT-FLNC         TO RY9-FLNC                               
093300                                                                          
093400     MOVE RY9-WDGZRY9      TO ZZAC-LOGGPOST                               
093500                                                                          
093600     MOVE SPACE                 TO RY9S-WDGZRY9S                          
093700     MOVE ORDP-RAD-IDDISTR      TO RY9S-IDDISTR                           
093800     MOVE ORDP-RAD-IDKUNDNR     TO RY9S-IDKUNDNR                          
093900     MOVE OHUV-IDORDER          TO RY9S-IDORDER                           
094000     MOVE WC-CDC-SE             TO RY9S-IDDC                              
094100     MOVE ORDP-RAD-KDFRAKT      TO RY9S-KDFRAKT                           
094200     MOVE ORDP-RAD-KDORDKL      TO RY9S-KDORDKL                           
094300                                                                          
094400     IF ORDP-RAD-FLTPOBEK = NEJ                                           
094500        MOVE 83                 TO RY9S-KDORDBEK                          
094600     ELSE                                                                 
094700        IF ORDP-RAD-KDSTARAD = 1 OR 2                                     
094800          MOVE 85               TO RY9S-KDORDBEK                          
094900        ELSE                                                              
095000          MOVE 87               TO RY9S-KDORDBEK                          
095100        END-IF                                                            
095200     END-IF                                                               
095300                                                                          
095400     MOVE RY9S-WDGZRY9S         TO ZZAC-SORTPOST                          
095500                                                                          
095600     PERFORM S02-SKRIV-LOGG                                               
095700     .                                                                    
095800     EJECT                                                                
095900 S01C-SKAPA-W2I109MID SECTION.                                            
096000                                                                          
096010*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
096020     MOVE ORDP-RAD-IDARTNR   TO BYT03-IDARTNR                             
096030     IF NOT BYT03-OBJEKT                                                  
096100                                                                          
096200        IF (ORDP-RAD-KDORDING < +3) OR                                    
096300             (ORDP-RAD-KDOI NOT = SPACE)                                  
096400                                                                          
096500          MOVE SPACE               TO 2109-MID2-W2I10902                  
096600                                                                          
096700          MOVE 1                   TO 2109-MID2-KVANTART                  
096800          MOVE ORDP-RAD-IDARTNR    TO 2109-MID2-IDARTNR(1)                
096900          MOVE OHUV-IDDC-PRIM      TO 2109-MID2-IDDC(1)                   
097000          MOVE ORDP-RAD-KDOI       TO 2109-MID2-KDOI(1)                   
097100          MOVE ORDP-RAD-CLEARGROUP TO 2109-MID2-CLEARGROUP(1)             
097200          MOVE '-'                 TO 2109-MID2-KDTECKEN(1)               
097300          MOVE ORDP-RAD-KVART      TO 2109-MID2-KVOI(1)                   
097400          IF ORDP-RAD-KDTPOTYP >= 1 AND <= 5                              
097500             AND ORDP-RAD-DARODAT = ZERO                                  
097600            MOVE ORDP-RAD-TITPO    TO 2109-MID2-TIUPPDAT(1)               
097700          ELSE                                                            
097800            MOVE ORDP-RAD-TIREGDAT TO 2109-MID2-TIUPPDAT(1)               
097900          END-IF                                                          
098000                                                                          
098100          COMPUTE MSG-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17            
098200          MOVE 'W2T109X '          TO MSG-KDTRANS-1                       
098300          MOVE '4256'              TO MSG-IDTRANS-1                       
098400                                                                          
098500          PERFORM IMS-PURG-ALT-MSG-2109                                   
098600                                                                          
098700        END-IF                                                            
098710     END-IF                                                               
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100 S02-SKRIV-LOGG SECTION.                                                  
099200                                                                          
099300     ACCEPT ZZAC-TIAAMMDD  FROM DATE                                      
099400     ACCEPT ZZAC-TIKLOCK   FROM TIME                                      
099500     MOVE  +1            TO ZZAC-IDLOGLOP                                 
099600                                                                          
099700     PERFORM IMS-ISRT-ZZAC-WDG6                                           
099800     IF SEGMENT-FINNS-REDAN                                               
099900       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
100000         ADD +1 TO ZZAC-IDLOGLOP                                          
100100         PERFORM IMS-ISRT-ZZAC-WDG6                                       
100200       END-PERFORM                                                        
100300     END-IF                                                               
100400     .                                                                    
100500     EJECT                                                                
100600 S20-FINN-INTERVALL SECTION.                                              
100700                                                                          
100800     PERFORM IMS-GU-WDM211                                                
100900     IF SEGMENT-FINNS                                                     
101000       PERFORM IMS-GNP-WDM221                                             
101100       PERFORM UNTIL SEGMENT-SAKNAS                                       
101200         IF  ORDP-RAD-IDDISTR > KMRK-IDDISTR-TOM                          
101300         OR  ORDP-RAD-IDDISTR < KMRK-IDDISTR-FOM                          
101400           CONTINUE                                                       
101500         ELSE                                                             
101600           IF  ORDP-RAD-IDDISTR  = KMRK-IDDISTR-TOM                       
101700           AND ORDP-RAD-IDKUNDNR > KMRK-IDKUNDNR-TOM                      
101800             CONTINUE                                                     
101900           ELSE                                                           
102000             IF  ORDP-RAD-IDDISTR  = KMRK-IDDISTR-FOM                     
102100             AND ORDP-RAD-IDKUNDNR < KMRK-IDKUNDNR-FOM                    
102200               CONTINUE                                                   
102300             ELSE                                                         
102400               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
102500               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
102600               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
102700               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
102800             END-IF                                                       
102900           END-IF                                                         
103000         END-IF                                                           
103100         PERFORM IMS-GNP-WDM221                                           
103200       END-PERFORM                                                        
103300     END-IF                                                               
103400     .                                                                    
103500     EJECT                                                                
103600                                                                          
103700* --- IMS SEKTIONER ---                                                   
103800     SKIP3                                                                
103900 IMS-GET-MSG SECTION.                                                     
104000                                                                          
104100     MOVE '  QC' TO GODK-STATUSKODER                                      
104200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
104300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
104400     PERFORM IMS-STATUSKONTROLL                                           
104500     .                                                                    
104600     SKIP3                                                                
104700 IMS-GN-MSG SECTION.                                                      
104800                                                                          
104900     MOVE SPACE TO GODK-STATUSKODER                                       
105000     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
105100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
105200     PERFORM IMS-STATUSKONTROLL                                           
105300     .                                                                    
105400     SKIP3                                                                
105500 IMS-INSERT-DISP-MSG SECTION.                                             
105600                                                                          
105700     MOVE SPACE TO GODK-STATUSKODER                                       
105800     CALL CBLTDLI USING ISRT DISP-PCB KOM-IO-AREA                         
105900     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
106000     PERFORM IMS-STATUSKONTROLL                                           
106100     .                                                                    
106200     EJECT                                                                
106300 IMS-PURG-ALT-MSG-2109 SECTION.                                           
106400                                                                          
106500     MOVE    LOW-VALUE        TO    MSG-KDZ1 MSG-KDZ2                     
106600     MOVE    '  '             TO    GODK-STATUSKODER                      
106700     CALL    CBLTDLI          USING PURG 2109-PCB MSG-IO-AREA             
106800     MOVE    2109-STATUS-CODE TO    STATUS-WS                             
106900     PERFORM IMS-STATUSKONTROLL                                           
107000     .                                                                    
107100     EJECT                                                                
107200                                                                          
107300 IMS-01-GHU-ORDP-WDA5 SECTION.                                            
107400                                                                          
107500     STRING 'WLORDP01(WDA501KY>=' W-WDA5MIN-X                             
107600                    '&WDA501KY<=' W-WDA5MAX-X                             
107700                    '&IDDC     =' WS1-IDDC                                
107800                    '&KVART    =' W-KVART-X ')'                           
107900            DELIMITED BY SIZE INTO SSA1                                   
108000     MOVE '  GE' TO GODK-STATUSKODER                                      
108100     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-ORDP01 SSA1                   
108200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
108300     PERFORM IMS-STATUSKONTROLL                                           
108400     .                                                                    
108500     SKIP2                                                                
108600 IMS-02-GHU-ORDP-WDA5 SECTION.                                            
108700                                                                          
108800     STRING 'WLORDP01(WDA501KY>=' W-WDA5MIN-X                             
108900                    '&WDA501KY<=' W-WDA5MAX-X                             
109000                    '&KVART    =' W-KVART-X                               
109100                    '&IDDC     =' WS1-IDDC                                
109200                    '&TITPO    =' W-TITPO-X ')'                           
109300            DELIMITED BY SIZE INTO SSA1                                   
109400     MOVE '  GE' TO GODK-STATUSKODER                                      
109500     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-ORDP01 SSA1                   
109600     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
109700     PERFORM IMS-STATUSKONTROLL                                           
109800     .                                                                    
109900     SKIP2                                                                
110000 IMS-DLET-ORDP-WDA5 SECTION.                                              
110100                                                                          
110200     MOVE '  ' TO GODK-STATUSKODER                                        
110300     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-ORDP01                       
110400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
110500     PERFORM IMS-STATUSKONTROLL                                           
110600     .                                                                    
110700     EJECT                                                                
110800 IMS-GHU-ARTC11 SECTION.                                                  
110900                                                                          
111000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
111100            DELIMITED BY SIZE INTO SSA1                                   
111200     MOVE   'WLARTC11'          TO SSA2                                   
111300     MOVE '  ' TO GODK-STATUSKODER                                        
111400     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2              
111500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
111600     PERFORM IMS-STATUSKONTROLL                                           
111700     .                                                                    
111800     SKIP2                                                                
111900 IMS-REPL-ARTC11 SECTION.                                                 
112000                                                                          
112100     MOVE '  ' TO GODK-STATUSKODER                                        
112200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC11                       
112300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
112400     PERFORM IMS-STATUSKONTROLL                                           
112500     .                                                                    
112600     EJECT                                                                
112700 IMS-GHU-ARTS11 SECTION.                                                  
112800                                                                          
112900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
113000            DELIMITED BY SIZE INTO SSA1                                   
113100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
113200            DELIMITED BY SIZE INTO SSA2                                   
113300     MOVE '  ' TO GODK-STATUSKODER                                        
113400     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-ARTS11 SSA1 SSA2              
113500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
113600     PERFORM IMS-STATUSKONTROLL                                           
113700     .                                                                    
113800     SKIP2                                                                
113900 IMS-REPL-ARTS11 SECTION.                                                 
114000                                                                          
114100     MOVE '  ' TO GODK-STATUSKODER                                        
114200     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-ARTS11                       
114300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
114400     PERFORM IMS-STATUSKONTROLL                                           
114500     .                                                                    
114600     EJECT                                                                
114700 IMS-GHU-ARTM-WDK901 SECTION.                                             
114800                                                                          
114900     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
115000          DELIMITED BY SIZE INTO SSA1                                     
115100     MOVE '  ' TO GODK-STATUSKODER                                        
115200     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-ARTM01 SSA1                   
115300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
115400     PERFORM IMS-STATUSKONTROLL                                           
115500     .                                                                    
115600     SKIP3                                                                
115700 IMS-GHNP-ARTM-WDK911 SECTION.                                            
115800                                                                          
115900     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
116000          DELIMITED BY SIZE INTO SSA1                                     
116100     MOVE '  ' TO GODK-STATUSKODER                                        
116200     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-ARTM11 SSA1                  
116300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
116400     PERFORM IMS-STATUSKONTROLL                                           
116500     .                                                                    
116600     SKIP2                                                                
116700 IMS-REPL-ARTM-WDK901 SECTION.                                            
116800                                                                          
116900     MOVE '  ' TO GODK-STATUSKODER                                        
117000     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-ARTM01                       
117100     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
117200     PERFORM IMS-STATUSKONTROLL                                           
117300     .                                                                    
117400     SKIP2                                                                
117500 IMS-REPL-ARTM-WDK911 SECTION.                                            
117600                                                                          
117700     MOVE '  ' TO GODK-STATUSKODER                                        
117800     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-ARTM11                       
117900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
118000     PERFORM IMS-STATUSKONTROLL                                           
118100     .                                                                    
118200     SKIP2                                                                
118300 IMS-DLET-ARTM-WDK9 SECTION.                                              
118400                                                                          
118500     MOVE '  ' TO GODK-STATUSKODER                                        
118600     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-ARTM11                       
118700     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
118800     PERFORM IMS-STATUSKONTROLL                                           
118900     .                                                                    
119000     EJECT                                                                
119100 IMS-GHU-XXBU-WDR5 SECTION.                                               
119200                                                                          
119300     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
119400          DELIMITED BY SIZE INTO SSA1                                     
119500     STRING 'WLXXBU11(WDGXKEY  =' W-WDGX2224-X ')'                        
119600          DELIMITED BY SIZE INTO SSA2                                     
119700     MOVE '  GE' TO GODK-STATUSKODER                                      
119800     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-XXBU11 SSA1 SSA2              
119900     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
120000     PERFORM IMS-STATUSKONTROLL                                           
120100     .                                                                    
120200     SKIP2                                                                
120300 IMS-DLET-XXBU-WDR5 SECTION.                                              
120400                                                                          
120500     MOVE '  ' TO GODK-STATUSKODER                                        
120600     CALL CBLTDLI USING DLET XXBU-PCB DLI-IO-XXBU11                       
120700     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
120800     PERFORM IMS-STATUSKONTROLL                                           
120900     .                                                                    
121000     EJECT                                                                
121100 IMS-ISRT-ZZAC-WDG6 SECTION.                                              
121200                                                                          
121300     MOVE 'WLZZAC01' TO SSA1                                              
121400     MOVE '  II' TO GODK-STATUSKODER                                      
121500     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-ZZAC01 SSA1                  
121600     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
121700     PERFORM IMS-STATUSKONTROLL                                           
121800     .                                                                    
121900     EJECT                                                                
122000 IMS-GU-GMTA-WDB2  SECTION.                                               
122100                                                                          
122200     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
122300            DELIMITED BY SIZE INTO SSA1                                   
122400     MOVE '  ' TO GODK-STATUSKODER                                        
122500     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-GMTA01 SSA1                    
122600     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
122700     PERFORM IMS-STATUSKONTROLL                                           
122800     .                                                                    
122900     EJECT                                                                
123000 IMS-GU-ORQM-WDQ1 SECTION.                                                
123100                                                                          
123200     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
123300                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
123400          DELIMITED BY SIZE INTO SSA1                                     
123500     MOVE '  GE'               TO GODK-STATUSKODER                        
123600     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-ORQM01 SSA1                  
123700     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
123800     PERFORM IMS-STATUSKONTROLL                                           
123900     .                                                                    
124000     SKIP2                                                                
124100 IMS-GN-ORQM-WDQ1 SECTION.                                                
124200                                                                          
124300     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
124400                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
124500          DELIMITED BY SIZE INTO SSA1                                     
124600     MOVE '  GEGB'             TO GODK-STATUSKODER                        
124700     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-ORQM01 SSA1                  
124800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
124900     PERFORM IMS-STATUSKONTROLL                                           
125000     .                                                                    
125100     SKIP2                                                                
125200 IMS-ISRT-ORQM-WDQ1 SECTION.                                              
125300                                                                          
125400     MOVE 'WLORQM01 ' TO SSA1                                             
125500     MOVE '    ' TO GODK-STATUSKODER                                      
125600     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-ORQM01 SSA1                  
125700     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
125800     PERFORM IMS-STATUSKONTROLL                                           
125900     .                                                                    
126000     EJECT                                                                
126100 IMS-GU-ORQI-WDQ2 SECTION.                                                
126200                                                                          
126300     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X3 ')'                       
126400          DELIMITED BY SIZE INTO SSA1                                     
126500     MOVE '  ' TO GODK-STATUSKODER                                        
126600     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-ORQI01 SSA1                    
126700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
126800     PERFORM IMS-STATUSKONTROLL                                           
126900     .                                                                    
127000     EJECT                                                                
127100 IMS-GU-WDM211 SECTION.                                                   
127200                                                                          
127300     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
127400          DELIMITED BY SIZE INTO SSA1                                     
127500     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
127600          DELIMITED BY SIZE INTO SSA2                                     
127700     MOVE '  GE'              TO GODK-STATUSKODER                         
127800     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
127900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
128000     PERFORM IMS-STATUSKONTROLL                                           
128100     .                                                                    
128200                                                                          
128300 IMS-GNP-WDM221 SECTION.                                                  
128400                                                                          
128500     MOVE 'WDM221 '           TO SSA1                                     
128600     MOVE '    GE'            TO GODK-STATUSKODER                         
128700     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
128800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
128900     PERFORM IMS-STATUSKONTROLL                                           
129000     .                                                                    
129100                                                                          
129200 IMS-GHU-WDM211 SECTION.                                                  
129300                                                                          
129400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
129500          DELIMITED BY SIZE INTO SSA1                                     
129600     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
129700          DELIMITED BY SIZE INTO SSA2                                     
129800     MOVE '  GE'              TO GODK-STATUSKODER                         
129900     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
130000     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
130100     PERFORM IMS-STATUSKONTROLL                                           
130200     .                                                                    
130300                                                                          
130400 IMS-REPL-WDM211 SECTION.                                                 
130500                                                                          
130600     MOVE '  '             TO GODK-STATUSKODER                            
130700     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
130800     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
130900     PERFORM IMS-STATUSKONTROLL                                           
131000     .                                                                    
131100     EJECT                                                                
131200 IMS-GHU-WDM221 SECTION.                                                  
131300                                                                          
131400     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
131500          DELIMITED BY SIZE INTO SSA1                                     
131600     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
131700          DELIMITED BY SIZE INTO SSA2                                     
131800     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
131900          DELIMITED BY SIZE INTO SSA3                                     
132000     MOVE '  GE' TO GODK-STATUSKODER                                      
132100     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
132200     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
132300     PERFORM IMS-STATUSKONTROLL                                           
132400     .                                                                    
132500                                                                          
132600 IMS-REPL-WDM221 SECTION.                                                 
132700                                                                          
132800     MOVE '  '             TO GODK-STATUSKODER                            
132900     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
133000     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
133100     PERFORM IMS-STATUSKONTROLL                                           
133200     .                                                                    
133300                                                                          
133400     EJECT                                                                
133500 IMS-GU-XXBX-WDR220 SECTION.                                              
133600                                                                          
133700     STRING 'WLXXBX01(WDGXKEY  =' W-WDGX2231-X ')'                        
133800          DELIMITED BY SIZE INTO SSA1                                     
133900     STRING 'WLXXBX11(WDGXKEY  =' W-WDGX2232-X ')'                        
134000          DELIMITED BY SIZE INTO SSA2                                     
134100     MOVE '  GE' TO GODK-STATUSKODER                                      
134200     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-XXBX11 SSA1 SSA2               
134300     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
134400     PERFORM IMS-STATUSKONTROLL                                           
134500     .                                                                    
134600     EJECT                                                                
134700                                                                          
134800 IMS-GU-WDB601    SECTION.                                                
134900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
135000          DELIMITED BY SIZE INTO SSA1                                     
135100     MOVE '  GE' TO GODK-STATUSKODER                                      
135200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
135300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
135400     PERFORM IMS-STATUSKONTROLL                                           
135500     IF SEGMENT-SAKNAS                                                    
135600         MOVE SPACE TO DCS-KDDC                                           
135700     END-IF                                                               
135800     .                                                                    
135900 IMS-STATUSKONTROLL SECTION.                                              
136000                                                                          
136100     SET STATUS-IX TO 1                                                   
136200     SEARCH GODK-STATUS                                                   
136300       AT END                                                             
136400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
136500         DELIMITED BY SIZE INTO FELTEXT                                   
136600         CALL FELLOG                                                      
136700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
136800     END-SEARCH                                                           
136900     .                                                                    
137000     EJECT                                                                
137100*    -COPY WY2000P3                                                       
