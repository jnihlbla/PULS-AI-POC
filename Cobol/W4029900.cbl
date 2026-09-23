000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4029900.                                                
000300 AUTHOR.         LASSI OLGRENER.                                          
000400 DATE-WRITTEN.   JUN 1992.                                                
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*        MPP FÖR BIPACKNING AV ORDERRADER.                                
000900*        - MSG LÄSES. FELAKTIG IDTRANS MEDFÖR FELLOG.                     
001000*        - W411BIPA ANROPAS                                               
001100*          (MAX 3 GGR MED 13 BIPA-RADER PER ANROP).                       
001200*        - FÖR VARJE RAD FRÅN W411BIPA                                    
001300*          LÄGGS ORDERBEKRÄFTELSE UPP PÅ WLORQM/WDQ1 OCH SKRIVS           
001400*          EV ORDERRAD PÅ WLORQF/WDQ4                                     
001500*        - OM FLER BIPA-RADER FINNS EFTER 3 ANROP GÖRS OMSTART,           
001600*          ANNARS STARTAS TRANS W4T298X FÖR ORDERAVSLUT.                  
001700*                                                                         
001800*    INDATA:                                                              
001900*        TRANSAKTION: W4T299X  MID: W4I29901                              
002000*                                                                         
002100*    UTDATA:                                                              
002200*        TRANSAKTION: W4T299X  MOD: W4I29901  FÖR OMSTART                 
002300*        TRANSAKTION: W4T298X  MOD: W4I29801                              
002400* CHANGE LOG:                                                             
002500*                                                                         
002600*    E'TRACKER: 5444132 DATED 2007-08-21                                  
002700*    E'TRACKER: 2218613 DATED 2008-03-11                                  
002800*    E'TRACKER: 7450328 DATED 2008-HÖST  VOHF                             
002900*    E'TRACKER: 10254592      2015       DECOMISSION VOHF                 
003000*                                                                         
003100*                                                                         
003200     SKIP2                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800*    --CHECKED BY WY2000                                                  
003900     SKIP3                                                                
004000 77  IDPGM                   PIC  X(8)           VALUE 'W4029900'.        
004100 77  FELTEXT                 PIC  X(32)          VALUE SPACE.             
004200 77  JA                      PIC  X(1)           VALUE 'J'.               
004300 77  NEJ                     PIC  X(1)           VALUE 'N'.               
004400 77  KDRC-DISPLAY            PIC Z(5)   VALUE ZERO.                       
004500 77  WS-PGM-POSITION         PIC X(24)  VALUE SPACE.                      
004600 77  RKOD-ABEND              PIC S9(4)  VALUE +33   COMP SYNC.            
004700 77  RKOD-ABEND-MED-DUMP     PIC S9(4)  VALUE +1000 COMP SYNC.            
004800 77  WS-BIPA-ANROP           PIC S9(9) COMP SYNC VALUE +0.                
004900 77  WS-INDEX-BIPA           PIC S9(9) COMP SYNC VALUE +0.                
005000 77  WS-INDEX-LAGOMR         PIC S9(9) COMP SYNC VALUE +0.                
005100 77  WS-INDEX-WOPS           PIC S9(9) COMP SYNC VALUE +0.                
005200 77  WS-RESLATT              PIC S9(3) COMP-3    VALUE +0.                
005300 77  SPAR-ORAD-KVSLATT       PIC S9(7) COMP-3    VALUE +0.                
005400 77  WS-IDDC                 PIC X(2)            VALUE SPACE.             
005500*                                                                         
005600 77  W-IDTRANS               PIC  X(4)           VALUE SPACE.             
005700     88  GODK-MID                                VALUE '4252'             
005800                                                       '4258'             
005810                                                       '4299'.            
005900 77  SW-EOF-BIPA             PIC X(1)    VALUE 'N'.                       
006000     88  FLER-BIPA-RADER                 VALUE 'N'.                       
006100     88  BIPA-RADER-SLUT                 VALUE 'J'.                       
006200*                                                                         
006300 01  DUMMY-PCB                   PIC X(4)   VALUE LOW-VALUE.              
006400*                                                                         
006500 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
006600 01  FILLER REDEFINES WS-TIHHMMSS.                                        
006700     03 WS-TIHHMM                PIC 9(4).                                
006800     03 FILLER                   PIC 9(2).                                
006900                                                                          
007000 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
007100 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
007200     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
007300     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
007400     EJECT                                                                
007500 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
007600*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
007700*    ----DIST79-DEALER-PRICE-----                                         
007800     EJECT                                                                
007900                                                                          
008000*01  -COPY WWPRODSL                                                       
008100                                                                          
008200 01  FILLER                  PIC X(16)   VALUE 'GEN-SUBPGM   '.           
008300 01  GENERELLA-SUBPROGRAM.                                                
008400*                                                                         
008500     03  CBLTDLI             PIC  X(8)           VALUE 'CBLTDLI '.        
008600     03  FELLOG              PIC  X(8)           VALUE 'FELLOG  '.        
008700     03  ABEND               PIC X(8)            VALUE 'ABEND   '.        
008800     03  W005INIT            PIC  X(8)           VALUE 'W005INIT'.        
008900     03  WZ01SEND            PIC X(8)            VALUE 'WZ01SEND'.        
009000*                                                                         
009100*    ------ PARAMETRAR TILL SUBPROGRAM W005INIT                           
009200*01  -COPY WMSGINIT                                                       
009300     EJECT                                                                
009400*                                                                         
009500 01  FILLER                  PIC X(16)   VALUE 'GEMEN-SUBPGM '.           
009600 01  GEMENSAMMA-SUBPROGRAM.                                               
009700*                                                                         
009800     03  W411BIPA            PIC  X(8)           VALUE 'W411BIPA'.        
009900     03  W411AREG            PIC  X(8)           VALUE 'W411AREG'.        
010100     03  W411LAST            PIC  X(8)           VALUE 'W411LAST'.        
010200     03  W411RANS            PIC  X(8)           VALUE 'W411RANS'.        
010300     03  W411CDCA            PIC  X(8)           VALUE 'W411CDCA'.        
010400     03  W413ADRS            PIC  X(8)           VALUE 'W413ADRS'.        
010500     03  W413AVSR            PIC  X(8)           VALUE 'W413AVSR'.        
010600     03  W335PRNO            PIC  X(8)           VALUE 'W335PRNO'.        
010700*        PRISFRÅGA                                                        
010800     03  W335PRQU            PIC  X(8)           VALUE 'W335PRQU'.        
010900*        PRISFRÅGA                                                        
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
011200 01  FILLER                  PIC X(16)   VALUE 'W411BIPA-AREA  '.         
011300     -COPY  W411BIPA                                                      
011400     EJECT                                                                
011500 01  FILLER                  PIC X(16)   VALUE 'W411AREG-AREA  '.         
011600     -COPY  W411AREG                                                      
011700     EJECT                                                                
012100 01  FILLER                  PIC X(16)   VALUE 'W411LAST-AREA  '.         
012200     -COPY  W411LAST                                                      
012300     EJECT                                                                
012400 01  FILLER                  PIC X(16)   VALUE 'W411RANS-AREA  '.         
012500     -COPY  W411RANS                                                      
012600     EJECT                                                                
012700 01  FILLER                  PIC X(16)   VALUE 'W411CDCA-AREA  '.         
012800     -COPY  W411CDCA                                                      
012900     EJECT                                                                
013000 01  FILLER                  PIC X(16)   VALUE 'W411ADRS-AREA  '.         
013100     -COPY  W413ADRS                                                      
013200     EJECT                                                                
013300 01  FILLER                  PIC X(16)   VALUE 'W411AVSR-AREA  '.         
013400     -COPY  W413AVSR                                                      
013500     EJECT                                                                
013600 01 FILLER                   PIC X(16)   VALUE 'W335PRNO-AREA'.           
013700*   -COPY W335PRNO                                                        
013800     EJECT                                                                
013900 01 FILLER                   PIC X(16)   VALUE 'W335PRQU-AREA'.           
014000*   -COPY W335PRQU                                                        
014100     EJECT                                                                
014200 01  FILLER                  PIC X(16)   VALUE 'MID-AREA       '.         
014300     -COPY  W4I29901                                                      
014400     EJECT                                                                
014500 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
014600     SKIP2                                                                
014700 01  -COPY WMSGAREA                                                       
014800     EJECT                                                                
014900     05  -COPY W4I29801 -PRE MOD4298-  -RED MSG-MID-OUT                   
015000     EJECT                                                                
015100 01  FILLER              PIC X(16)       VALUE 'IMS-WS         '.         
015200     SKIP2                                                                
015300 01  NYCKLAR-TILL-DLI.                                                    
015400*                                                                         
015500     03  W-WDQ101KY-MIN-X.                                                
015600         05  W-IDORDER-Q1-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
015700         05  W-IDARTNR-Q1-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
015800         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
015900         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
016000         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
016100                                                                          
016200     03  W-WDQ101KY-MAX-X.                                                
016300         05  W-IDORDER-Q1-MAX    PIC S9(7)   VALUE ZERO COMP-3.           
016400         05  W-IDARTNR-Q1-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
016500         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   VALUE ZERO COMP-3.           
016600         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   VALUE ZERO COMP-3.           
016700         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
016800                                                                          
016900     03  W-IDARTNR-X.                                                     
017000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017100                                                                          
017200     03  W-IDDC-X.                                                        
017300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017400                                                                          
017500     03  W-IDLAND-X.                                                      
017600         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
017700                                                                          
017800     EJECT                                                                
017900     03  W-IDGMT-X.                                                       
018000         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
018100         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
018200*                                                                         
018300     03  W-IDGMT-MIN-X.                                                   
018400         05  W-IDDISTR-WDB2-MIN  PIC S9(5) VALUE ZERO COMP-3.             
018500         05  W-IDKUNDNR-WDB2-MIN PIC S9(7) VALUE ZERO COMP-3.             
018600*                                                                         
018700     03  W-IDGMT-MAX-X.                                                   
018800         05  W-IDDISTR-WDB2-MAX  PIC S9(5) VALUE ZERO COMP-3.             
018900         05  W-IDKUNDNR-WDB2-MAX PIC S9(7) VALUE ZERO COMP-3.             
019000                                                                          
019500     03  W-IDDC-B6-X.                                                     
019600         05 W-IDDC-B6                  PIC X(2).                          
019700     EJECT                                                                
019800 01  FILLER              PIC X(16)       VALUE 'STATUSKOD      '.         
019900     SKIP2                                                                
020000 01  STATUS-WS           PIC XX.                                          
020100     88  SEGMENT-FINNS                   VALUE '  '.                      
020200     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
020300     88  BASEN-SLUT                      VALUE 'GB'.                      
020400     SKIP2                                                                
020500 01  GODK-STATUSKODER.                                                    
020600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
020700     SKIP2                                                                
020800 01  SSA1                PIC X(96).                                       
020900 01  SSA2                PIC X(96).                                       
021000     EJECT                                                                
021100 01  -COPY W0003                                                          
021200     EJECT                                                                
021300 01  FILLER    PIC X(16)                   VALUE 'DLI-IO-AREA'.           
021400     SKIP2                                                                
021500 01  FILLER                      PIC X(16) VALUE 'WDQ101-AREA'.           
021600 01  DLI-IO-AREA-OBKR.                                                    
021700   03 -COPY WDQ101                                                        
021800     EJECT                                                                
022300 01  FILLER                      PIC X(16) VALUE 'WDQ401-AREA'.           
022400 01  DLI-IO-AREA-ORAD.                                                    
022500   03 -COPY WDQ401                                                        
022600     EJECT                                                                
022700 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
022800 01  DLI-IO-AREA-WDK7.                                                    
022900*    03  -COPY WDK711                                                     
023000     EJECT                                                                
023100 01  FILLER                      PIC X(16)   VALUE 'WDK712-AREA'.         
023200 01  DLI-IO-AREA-WDK712.                                                  
023300*    03  -COPY WDK712.                                                    
023400     EJECT                                                                
023500 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
023600 01  DLI-IO-AREA-WDB201.                                                  
023700     03  WLGMTA01.                                                        
023800*        05  -COPY WDB201                                                 
023900     EJECT                                                                
024500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024600 01   DLI-IO-AREA-B601.                                                   
024700*     03  -COPY WDB601                                                    
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
025000     SKIP3                                                                
025100 01  -COPY WZ01SEND                                                       
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
025400     SKIP3                                                                
025500 01  SEND-AREA.                                                           
025600*    03  -COPY WZ01REQU  -PRE 3039-                                       
025700*    03  -COPY W30391I1  -PRE 3039-                                       
025800     EJECT                                                                
025900 LINKAGE SECTION.                                                         
026000     -COPY W0009  -PRE MSG-                                               
026100     EJECT                                                                
026200     -COPY W0009 -PRE 4299-                                               
026300     EJECT                                                                
026400     -COPY W0009 -PRE 4298-                                               
026500     EJECT                                                                
026600*01  -COPY W0009   -PRE PRQRY-                                            
026700     05  FILLER  PIC X.                                                   
026800     SKIP2                                                                
026900     -COPY W0008  -PRE USEA-                                              
027000     05  FILLER PIC X.                                                    
027100     -COPY W0008  -PRE ORQM-                                              
027200     05  FILLER PIC X.                                                    
027600     -COPY W0008  -PRE ORQF-                                              
027700     05  FILLER PIC X.                                                    
027800     EJECT                                                                
027900     -COPY W0008  -PRE WDK7-                                              
028000     05  FILLER PIC X.                                                    
028100     EJECT                                                                
028200*01  -COPY W0008   -PRE WDB2-                                             
028300     05  FILLER                  PIC X.                                   
028400     EJECT                                                                
028800*01  -COPY W0008   -PRE WDB6-                                             
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100 01  BIPA-ORDP-PCB         PIC X(1).                                      
029200 01  BIPA-WDB6-PCB         PIC X(1).                                      
029300 01  BIPA-WDK6-PCB         PIC X(1).                                      
029400 01  BIPA-LEVF-PCB         PIC X(1).                                      
029500 01  BIPA-LEVG-PCB         PIC X(1).                                      
029600 01  BIPA-WDF8-PCB         PIC X(1).                                      
029700 01  BIPA-WDF8A-PCB        PIC X(1).                                      
029800 01  BIPA-LEVA-PCB         PIC X(1).                                      
029900 01  BIPA-ARTS2-PCB        PIC X(1).                                      
030000                                                                          
030100 01  AREG-WDK6-PCB               PIC X.                                   
030200 01  AREG-WDK7-PCB               PIC X.                                   
030300                                                                          
031000 01  CDCA-ARTM-PCB               PIC X.                                   
031100 01  CDCA-INLB-PCB               PIC X.                                   
031200 01  CDCA-WDB2-PCB               PIC X.                                   
031300 01  CDCA-WDC1-PCB               PIC X.                                   
031400                                                                          
031500 01  RANS-XXKM-PCB               PIC X.                                   
031600 01  RANS-ARTM-PCB               PIC X.                                   
031700 01  RANS-ARTS-PCB               PIC X.                                   
031800                                                                          
031900 01  AVSR-LIST-PCB               PIC X.                                   
032000 01  AVSR-ORQI-PCB               PIC X.                                   
032100 01  AVSR-GMTB-PCB               PIC X.                                   
032200 01  AVSR-GMTC-PCB               PIC X.                                   
032300 01  AVSR-WDB2-PCB               PIC X.                                   
032400 01  AVSR-WDB6-PCB               PIC X.                                   
032500 01  TRAN-XXKB-PCB               PIC X.                                   
032600 01  KVAN-WDB2-PCB               PIC X.                                   
032700                                                                          
032800 01  PRNO-3107-PCB               PIC X.                                   
032900 01  PRQU-WDG2-PCB               PIC X.                                   
033000 01  PRQU-WDC7-PCB               PIC X.                                   
033100 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
033200                                                                          
033300                                                                          
033400     EJECT                                                                
033500 PROCEDURE DIVISION USING  MSG-PCB 4299-PCB 4298-PCB AVSR-LIST-PCB        
033600                     PRQRY-PCB                                            
033700                     USEA-PCB ORQM-PCB ORQF-PCB WDK7-PCB                  
033800                     WDB2-PCB WDB6-PCB                                    
033900                     BIPA-ORDP-PCB                                        
034000                     BIPA-WDB6-PCB                                        
034100                     BIPA-WDK6-PCB                                        
034200                     BIPA-LEVF-PCB                                        
034300                     BIPA-LEVG-PCB                                        
034400                     BIPA-WDF8-PCB                                        
034500                     BIPA-WDF8A-PCB                                       
034600                     BIPA-LEVA-PCB                                        
034700                     BIPA-ARTS2-PCB                                       
034800                     AREG-WDK6-PCB                                        
034900                     AREG-WDK7-PCB                                        
035500                     CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB            
035600                     CDCA-WDC1-PCB                                        
035700                     RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB            
035800                     AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB            
035900                     AVSR-WDB2-PCB AVSR-WDB6-PCB                          
036000                     TRAN-XXKB-PCB KVAN-WDB2-PCB                          
036100                     PRNO-3107-PCB                                        
036200                     PRQU-WDG2-PCB                                        
036300                     PRQU-WDC7-PCB                                        
036400                     PRQU-SJKO-WDK6-PCB.                                  
036500     ENTRY 'DLITCBL' USING MSG-PCB 4299-PCB 4298-PCB AVSR-LIST-PCB        
036600                     PRQRY-PCB                                            
036700                     USEA-PCB ORQM-PCB ORQF-PCB WDK7-PCB                  
036800                     WDB2-PCB WDB6-PCB                                    
036900                     BIPA-ORDP-PCB                                        
037000                     BIPA-WDB6-PCB                                        
037100                     BIPA-WDK6-PCB                                        
037200                     BIPA-LEVF-PCB                                        
037300                     BIPA-LEVG-PCB                                        
037400                     BIPA-WDF8-PCB                                        
037500                     BIPA-WDF8A-PCB                                       
037600                     BIPA-LEVA-PCB                                        
037700                     BIPA-ARTS2-PCB                                       
037800                     AREG-WDK6-PCB                                        
037900                     AREG-WDK7-PCB                                        
038500                     CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB            
038600                     CDCA-WDC1-PCB                                        
038700                     RANS-XXKM-PCB RANS-ARTM-PCB RANS-ARTS-PCB            
038800                     AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB            
038900                     AVSR-WDB2-PCB AVSR-WDB6-PCB                          
039000                     TRAN-XXKB-PCB KVAN-WDB2-PCB                          
039100                     PRNO-3107-PCB                                        
039200                     PRQU-WDG2-PCB                                        
039300                     PRQU-WDC7-PCB                                        
039400                     PRQU-SJKO-WDK6-PCB.                                  
039500     EJECT                                                                
039600     PERFORM IMS-GET-MSG                                                  
039700     IF SEGMENT-FINNS                                                     
039800        PERFORM A-INIT                                                    
039900                                                                          
040000        MOVE +1                 TO WS-BIPA-ANROP                          
040100        PERFORM UNTIL WS-BIPA-ANROP > +3  OR  BIPA-RADER-SLUT             
040200                                                                          
040300           PERFORM B-ANROPA-W411BIPA                                      
040400                                                                          
040500           MOVE +1               TO WS-INDEX-BIPA                         
040600           PERFORM UNTIL WS-INDEX-BIPA > BIPA-KVBIPACK  OR                
040700                         BIPA-IDARTNR(WS-INDEX-BIPA) = ZERO               
040800             PERFORM C-SKRIV-OBKR-FRAN-BIPA                               
040900             PERFORM D-BEHANDLA-BIPA-RAD                                  
041000             ADD +1              TO WS-INDEX-BIPA                         
041100           END-PERFORM                                                    
041200                                                                          
041300           PERFORM E-ANROPA-W413AVSR                                      
041400                                                                          
041500           IF  WS-INDEX-BIPA NOT > BIPA-KVBIPACK                          
041600             MOVE JA             TO SW-EOF-BIPA                           
041700           ELSE                                                           
041800             ADD +1              TO WS-BIPA-ANROP                         
041900           END-IF                                                         
042000        END-PERFORM                                                       
042100                                                                          
042200        IF FLER-BIPA-RADER                                                
042300           PERFORM F-OMSTARTA-EGEN-TRANS                                  
042400        ELSE                                                              
042500           PERFORM G-STARTA-4298-ORDERAVSLUT                              
042600        END-IF                                                            
042700     END-IF                                                               
042800     MOVE ZERO TO RETURN-CODE                                             
042900     GOBACK                                                               
043000     .                                                                    
043100     EJECT                                                                
043200 A-INIT SECTION.                                                          
043300                                                                          
043400     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I29901                    
043500     MOVE MSG-IDTRANS-1        TO W-IDTRANS                               
043600                                                                          
043700     IF NOT GODK-MID                                                      
043800       STRING 'FELAKTIG IDTRANS: ' W-IDTRANS                              
043900         DELIMITED BY SIZE INTO FELTEXT                                   
044000       CALL FELLOG                                                        
044100     END-IF                                                               
044200                                                                          
044300     MOVE LOW-VALUE            TO MSG-AREA                                
044400                                                                          
044500     MOVE NEJ                  TO SW-EOF-BIPA                             
044600                                                                          
044700     PERFORM S01-NOLLA-AVSR-TABELL                                        
044800     PERFORM AB-FIXA-LOKAL-TID                                            
044900     .                                                                    
045000     EJECT                                                                
045100                                                                          
045200 AB-FIXA-LOKAL-TID SECTION.                                               
045300                                                                          
045400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
045500     MOVE '013'             TO MSGI-KDCALL                                
045600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
045700     MOVE '4299'            TO MSGI-IDTRANS                               
045800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
045900                                                                          
046000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
046100     .                                                                    
046200     EJECT                                                                
046300                                                                          
046400 B-ANROPA-W411BIPA SECTION.                                               
046500                                                                          
046600     MOVE +2                   TO BIPA-KDORDBEH                           
046700     MOVE MID-IDDISTR          TO BIPA-IDDISTR                            
046800     MOVE MID-IDKUNDNR         TO BIPA-IDKUNDNR                           
046900     MOVE MID-IDKUNDRF (3:5)   TO BIPA-IDKUNDRF                           
047000     MOVE MID-IDKAMPRF         TO BIPA-IDKAMPRF                           
047100     MOVE MID-KDTPOTYP         TO BIPA-KDTPOTYP                           
047200     MOVE MID-KDORDKL          TO BIPA-KDORDKL                            
047300     MOVE MID-KDFAKTYP         TO BIPA-KDFAKTYP                           
047400     MOVE MID-IDSYSTEM         TO BIPA-IDSYSTEM                           
047500     MOVE MID-IDKONTO          TO BIPA-IDKONTO                            
047600     MOVE MID-IDKST            TO BIPA-IDKST                              
047700     MOVE ZERO                 TO BIPA-IDANALYS                           
047800     MOVE MID-IDDC             TO BIPA-IDDC                               
047900     MOVE SPACE                TO BIPA-IDPRC-RAD                          
048000     MOVE +13                  TO BIPA-KVBIPACK                           
048100     MOVE MID-FLFORBI          TO BIPA-FLFORBI                            
048200     MOVE NEJ                  TO BIPA-FLORDSPE                           
048300     MOVE MID-BEVARREF         TO BIPA-BEVARREF                           
048400     MOVE MID-FLOVRLEV         TO BIPA-FLOVRLEV                           
048500     MOVE MID-IDBIPREF         TO BIPA-IDBIPREF                           
048600     MOVE MID-KDROPACK         TO BIPA-KDROPACK                           
048700     MOVE MID-KDFRAKT          TO BIPA-KDFRAKT                            
048800     MOVE +1 TO WS-INDEX-LAGOMR                                           
048900     PERFORM UNTIL WS-INDEX-LAGOMR > +99                                  
049000        MOVE SPACE       TO BIPA-IDPRC-LAGOMR(WS-INDEX-LAGOMR)            
049100        ADD +1           TO WS-INDEX-LAGOMR                               
049200     END-PERFORM                                                          
049300                                                                          
049400     CALL W411BIPA USING BIPA-W411BIPA                                    
049500                         BIPA-ORDP-PCB                                    
049600                         BIPA-WDB6-PCB                                    
049700                         BIPA-WDK6-PCB                                    
049800                         AREG-WDK7-PCB                                    
049900                         BIPA-LEVF-PCB                                    
050000                         BIPA-LEVG-PCB                                    
050100                         BIPA-WDF8-PCB                                    
050200                         BIPA-WDF8A-PCB                                   
050300                         BIPA-LEVA-PCB                                    
050400                         BIPA-ARTS2-PCB                                   
050500     .                                                                    
050600     EJECT                                                                
050700 C-SKRIV-OBKR-FRAN-BIPA SECTION.                                          
050800                                                                          
050900     MOVE BIPA-IDDC-UT(WS-INDEX-BIPA) TO WS-IDDC                          
051000                                         W-IDDC-B6                        
051100     PERFORM IMS-GU-WDB601                                                
051200                                                                          
051300     IF DCS-NDC                                                           
051400       PERFORM CA-FIXA-LOKAL-TID                                          
051500     END-IF                                                               
051600                                                                          
051700     MOVE MID-IDORDER          TO OBKR-IDORDER                            
051800     MOVE BIPA-IDARTNR(WS-INDEX-BIPA)                                     
051900                               TO OBKR-IDARTNR                            
052000                                                                          
052100     MOVE OBKR-IDORDER         TO W-IDORDER-Q1-MIN                        
052200                                  W-IDORDER-Q1-MAX                        
052300     MOVE OBKR-IDARTNR         TO W-IDARTNR-Q1-MIN                        
052400                                  W-IDARTNR-Q1-MAX                        
052500     MOVE +1                   TO W-IDLOPNR-Q1-MIN                        
052600                                  W-IDLOPNR-Q1-MAX                        
052700                                  W-IDSEKVNR-Q1-MIN                       
052800                                  W-IDSEKVNR-Q1-MAX                       
052900     PERFORM IMS-GU-ORQM-WDQ101                                           
053000     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
053100        ADD +1                 TO W-IDLOPNR-Q1-MIN                        
053200                                  W-IDLOPNR-Q1-MAX                        
053300        PERFORM IMS-GN-ORQM-WDQ101                                        
053400     END-PERFORM                                                          
053500     MOVE W-IDLOPNR-Q1-MIN     TO OBKR-IDLOPNR                            
053600                                                                          
053700     MOVE +1                   TO OBKR-IDSEKVNR                           
053800     MOVE  10                  TO OBKR-KDORDBEK                           
053900     MOVE IDPGM                TO OBKR-IDPGM                              
054000     MOVE SPACE                TO OBKR-BEERS                              
054100     MOVE SPACE                TO OBKR-IDBIL                              
054200     MOVE MID-BEKUNDRF         TO OBKR-BEKUNDRF                           
054300     MOVE BIPA-BERADREF(WS-INDEX-BIPA)                                    
054400                               TO OBKR-BERADREF                           
054500     MOVE BIPA-BEVOLREF(WS-INDEX-BIPA)                                    
054600                               TO OBKR-BEVOLREF                           
054700     MOVE BIPA-IDKAMPRF-UT(WS-INDEX-BIPA)                                 
054800                               TO OBKR-IDKAMPRF                           
054900     MOVE +0                   TO OBKR-DIERS-KVOT                         
055000     MOVE NEJ                  TO OBKR-FLAKPLOC                           
055100     MOVE BIPA-FLINVEST(WS-INDEX-BIPA)                                    
055200                               TO OBKR-FLINVEST                           
055300     EJECT                                                                
055400     MOVE JA                   TO OBKR-FLOBOK                             
055500     MOVE JA                   TO OBKR-FLOBTRAN                           
055600     MOVE NEJ                  TO OBKR-FLOBPRT                            
055700     MOVE BIPA-FLPRTILL(WS-INDEX-BIPA)                                    
055800                               TO OBKR-FLPRTILL                           
055900     MOVE JA                   TO OBKR-FLRESTN                            
056000     MOVE JA                   TO OBKR-FLSLATT                            
056100     MOVE BIPA-FLERS(WS-INDEX-BIPA)                                       
056200                               TO OBKR-FLTILLK                            
056300     MOVE +0                   TO OBKR-IDARTNR-TILLK                      
056400     MOVE BIPA-IDDISTR         TO OBKR-IDDISTR                            
056500     MOVE BIPA-IDKUNDNR        TO OBKR-IDKUNDNR                           
056600     MOVE '0000000   '         TO OBKR-IDKUNDRF                           
056700     MOVE BIPA-IDKUNDRF(1:5)   TO OBKR-IDKUNDRF (3:5)                     
056800     MOVE '0000000   '         TO OBKR-IDKUNDRF-RO                        
056900     MOVE BIPA-IDKUNDRF-UT(WS-INDEX-BIPA) (1:5)                           
057000                               TO OBKR-IDKUNDRF-RO (3:5)                  
057100     MOVE BIPA-IDLEVNR(WS-INDEX-BIPA)                                     
057200                               TO OBKR-IDLEVNR                            
057300     MOVE BIPA-IDLOPNR(WS-INDEX-BIPA)                                     
057400                               TO OBKR-IDLOPNR-RO                         
057500     MOVE BIPA-IDSYSTEM-UT(WS-INDEX-BIPA)                                 
057600                               TO OBKR-IDSYSTEM                           
057700     MOVE BIPA-IDDC-UT(WS-INDEX-BIPA) TO OBKR-IDDC                        
057800     MOVE BIPA-IDDC-RO(WS-INDEX-BIPA) TO OBKR-IDDC-RO                     
057900     EJECT                                                                
058000     MOVE BIPA-KDDSP(WS-INDEX-BIPA)                                       
058100                               TO OBKR-KDDSP                              
058200     MOVE +0                   TO OBKR-KDERS                              
058300     MOVE BIPA-KDOI(WS-INDEX-BIPA)                                        
058400                               TO OBKR-KDOI                               
058500     MOVE BIPA-CLEARGROUP(WS-INDEX-BIPA)                                  
058600                               TO OBKR-CLEARGROUP                         
058700     MOVE BIPA-KDKVBRYT(WS-INDEX-BIPA)                                    
058800                               TO OBKR-KDKVBRYT                           
058900     MOVE BIPA-KDPRTYP(WS-INDEX-BIPA)                                     
059000                               TO OBKR-KDPRTYP                            
059100     MOVE BIPA-KDTPOTYP-UT(WS-INDEX-BIPA)                                 
059200                               TO OBKR-KDTPOTYP                           
059300     MOVE BIPA-KDVRINFO(WS-INDEX-BIPA)                                    
059400                               TO OBKR-KDVRINFO                           
059500     MOVE +0                   TO OBKR-KVANNANT                           
059600                                  OBKR-KVAVBART                           
059700     MOVE BIPA-KVART(WS-INDEX-BIPA)                                       
059800                               TO OBKR-KVBEART-Q                          
059900                                  OBKR-KVBEART                            
060000     MOVE +0                   TO OBKR-KVBEART-TILLK                      
060100                                  OBKR-KVPREAVB                           
060200                                  OBKR-KVPRERO                            
060300                                  OBKR-KVQPACK                            
060400                                  OBKR-KVRO                               
060500                                  OBKR-KVSLATT                            
060600     MOVE BIPA-DEAL-PR-LINE(WS-INDEX-BIPA)                                
060700                               TO OBKR-DEAL-PR-LINE                       
060800     MOVE BIPA-PRARTNTO(WS-INDEX-BIPA)                                    
060900                               TO OBKR-PRARTNTO                           
061000     MOVE BIPA-PRAVCOST(WS-INDEX-BIPA)                                    
061100                               TO OBKR-PRAVCOST                           
061200     MOVE +0                   TO OBKR-PRBPRIS                            
061300     MOVE BIPA-REKSIFFR(WS-INDEX-BIPA)                                    
061400                               TO OBKR-REKSIFFR                           
061500     MOVE +0                   TO OBKR-REKSIFFR-TILLK                     
061600     MOVE +0                   TO OBKR-RERF-RAD                           
061700     MOVE +0                   TO OBKR-TIDISPIN                           
061800     MOVE MID-TIREGDAT         TO OBKR-TIORDREG                           
061900     MOVE +0                   TO OBKR-TIPRIS                             
062000     EJECT                                                                
062100     MOVE MSGI-TILOKDAT        TO OBKR-TIREGDAT                           
062200     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
062300     MOVE WS-TIHHMMSS          TO OBKR-TIREGTID                           
062400     MOVE BIPA-TIRODAT(WS-INDEX-BIPA)                                     
062500                               TO OBKR-TIRODAT                            
062600     MOVE OBKR-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
062700     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
062800       MOVE 20                 TO WS-SEKEL-9KOMPL                         
062900     ELSE                                                                 
063000       MOVE 19                 TO WS-SEKEL-9KOMPL                         
063100     END-IF                                                               
063200     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
063300     MOVE BIPA-TITPO(WS-INDEX-BIPA)                                       
063400                               TO OBKR-TITPO                              
063500     MOVE MID-TIREGDAT         TO WS-AAMMDD-9KOMPL                        
063600     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
063700     MOVE BIPA-KDFRAKT-UT(WS-INDEX-BIPA)                                  
063800                               TO OBKR-KDFRAKT                            
063900     MOVE BIPA-KDORDKL-UT(WS-INDEX-BIPA)                                  
064000                               TO OBKR-KDORDKL                            
064100                                                                          
064200     MOVE BIPA-KDORDTYP-LDC(WS-INDEX-BIPA)                                
064300                               TO OBKR-KDORDTYP-LDC                       
064400     MOVE BIPA-TIREPDAT(WS-INDEX-BIPA)                                    
064500                               TO OBKR-TIREPDAT                           
064600     MOVE BIPA-IDKUNDRF-WIP(WS-INDEX-BIPA)                                
064700                               TO OBKR-IDKUNDRF-WIP                       
064800     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
064900     MOVE BIPA-PRAVCOST(WS-INDEX-BIPA)                                    
065000                               TO OBKR-PRAVCOST                           
065100     MOVE BIPA-KDVALISO(WS-INDEX-BIPA)                                    
065200                               TO OBKR-KDVALISO                           
065300                                                                          
065400     PERFORM IMS-ISRT-ORQM-WDQ101                                         
065500     .                                                                    
065600     EJECT                                                                
065700 CA-FIXA-LOKAL-TID SECTION.                                               
065800                                                                          
065900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
066000     MOVE '013'             TO MSGI-KDCALL                                
066100     MOVE 'WIDDC   '        TO MSGI-IDUSER                                
066200     MOVE WS-IDDC           TO MSGI-IDUSER(6:2)                           
066300     MOVE '4299'            TO MSGI-IDTRANS                               
066400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
066500                                                                          
066600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
066700     .                                                                    
066800     EJECT                                                                
066900                                                                          
067000 D-BEHANDLA-BIPA-RAD SECTION.                                             
067100                                                                          
067200     PERFORM DA-LAS-ARTIKELREG                                            
067300     PERFORM DB-BYGG-UPP-ORDERRAD-BIPA                                    
067400                                                                          
067500     IF OBKR-TIRODAT > +0                                                 
067600        PERFORM S09-KONTROLLERA-ENHETSLAST                                
067700        PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                               
067800     ELSE                                                                 
068100       IF DCS-CDC                                                         
068900         PERFORM DD-KOMPLETTERA-RANSONERING                               
069000         PERFORM DE-PREL-AVBOKNING-BIPA                                   
069100         PERFORM DF-SKRIV-Q1-OCH-Q4-RADER                                 
072400       END-IF                                                             
072600     END-IF                                                               
072700     .                                                                    
072800     EJECT                                                                
072900 DA-LAS-ARTIKELREG SECTION.                                               
073000                                                                          
073100     MOVE OBKR-IDARTNR         TO AREG-IDARTNR                            
073200                                                                          
073300     CALL W411AREG USING AREG-W411AREG                                    
073400                         AREG-WDK6-PCB                                    
073500                         AREG-WDK7-PCB                                    
073600     .                                                                    
073700     EJECT                                                                
073800                                                                          
073900 DB-BYGG-UPP-ORDERRAD-BIPA SECTION.                                       
074000                                                                          
074100     MOVE OBKR-IDORDER         TO ORAD-IDORDER                            
074200     MOVE OBKR-IDDC            TO ORAD-IDDC                               
074300     MOVE OBKR-IDDC-RO         TO ORAD-IDDC-RO                            
074700     IF DCS-SDC OR DCS-NDC                                                
074800       MOVE OBKR-IDARTNR       TO W-IDARTNR                               
074900       MOVE ORAD-IDDC          TO W-IDDC                                  
075000       PERFORM IMS-11-GU-WDK711                                           
075100       MOVE SLAG-ADLAGOMR      TO ORAD-ADLAGOMR                           
075200       MOVE SLAG-ADGANG        TO ORAD-ADGANG                             
075300       MOVE SLAG-ADPLATS       TO ORAD-ADPLATS                            
075400       MOVE DCS-IDLANDX2       TO W-IDLAND                                
075500       IF DCS-SDC OR DCS-NDC                                              
075600         PERFORM IMS-GU-WDK712                                            
075700         IF SEGMENT-FINNS                                                 
075800            IF LART-KDARTURS > SPACE                                      
075900               MOVE LART-KDARTURS TO ORAD-KDARTURS                        
076000            ELSE                                                          
076100               MOVE AREG-KDARTURS TO ORAD-KDARTURS                        
076200            END-IF                                                        
076300            IF LART-VKART > ZERO AND                                      
076400               LART-VKART NOT = AREG-VKART                                
076500               MOVE LART-VKART TO ORAD-VKART                              
076600                                  ORAD-VKART-NTO                          
076700            ELSE                                                          
076800               MOVE AREG-VKART     TO ORAD-VKART                          
076900               MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                      
077000            END-IF                                                        
077100            IF LART-VLARTNTO > 0                                          
077200               MOVE LART-VLARTNTO TO ORAD-VLARTNTO                        
077300            ELSE                                                          
077400               MOVE AREG-VLARTNTO TO ORAD-VLARTNTO                        
077500            END-IF                                                        
077600         ELSE                                                             
077700            MOVE AREG-KDARTURS  TO ORAD-KDARTURS                          
077800            MOVE AREG-VKART     TO ORAD-VKART                             
077900            MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                         
078000            MOVE AREG-VLARTNTO  TO ORAD-VLARTNTO                          
078100         END-IF                                                           
078200       ELSE                                                               
078300         MOVE AREG-KDARTURS  TO ORAD-KDARTURS                             
078400         MOVE AREG-VKART     TO ORAD-VKART                                
078500         MOVE AREG-VKART-NTO TO ORAD-VKART-NTO                            
078600         MOVE AREG-VLARTNTO  TO ORAD-VLARTNTO                             
078700       END-IF                                                             
078800     ELSE                                                                 
078900       MOVE AREG-ADLAGOMR      TO ORAD-ADLAGOMR                           
079000       MOVE AREG-ADGANG        TO ORAD-ADGANG                             
079100       MOVE AREG-ADPLATS       TO ORAD-ADPLATS                            
079200       MOVE AREG-KDARTURS      TO ORAD-KDARTURS                           
079300       MOVE AREG-VKART         TO ORAD-VKART                              
079400       MOVE AREG-VKART-NTO     TO ORAD-VKART-NTO                          
079500       MOVE AREG-VLARTNTO      TO ORAD-VLARTNTO                           
079600     END-IF                                                               
079700     MOVE OBKR-IDARTNR         TO ORAD-IDARTNR                            
079800     MOVE +1                   TO ORAD-IDLOPNR                            
079900     MOVE OBKR-BERADREF        TO ORAD-BERADREF                           
080000     MOVE OBKR-BEVOLREF        TO ORAD-BEVOLREF                           
080100     MOVE OBKR-FLAKPLOC        TO ORAD-FLAKPLOC                           
080200     MOVE SPACE                TO ORAD-IDBIL                              
080300                                  ORAD-IDKLIENT                           
080400                                  ORAD-IDARBREF                           
080500                                  ORAD-IDVIN                              
080600     MOVE OBKR-FLINVEST        TO ORAD-FLINVEST                           
080700     MOVE OBKR-FLOBTRAN        TO ORAD-FLOBTRAN                           
080800     MOVE OBKR-FLPRTILL        TO ORAD-FLPRTILL                           
080900     MOVE OBKR-FLRESTN         TO ORAD-FLRESTN                            
081000     MOVE OBKR-FLTILLK         TO ORAD-FLTILLK                            
081100     MOVE 'N'                  TO ORAD-FLSDCLEV                           
081200     MOVE OBKR-IDDISTR         TO ORAD-IDDISTR                            
081300                                  TEST-IDDISTR                            
081400     MOVE OBKR-IDKUNDNR        TO ORAD-IDKUNDNR                           
081500     MOVE OBKR-IDKUNDRF        TO ORAD-IDKUNDRF                           
081600     MOVE OBKR-IDKAMPRF        TO ORAD-IDKAMPRF                           
081700     MOVE OBKR-IDLEVNR         TO ORAD-IDLEVNR                            
081800     MOVE OBKR-IDLOPNR-RO      TO ORAD-IDLOPNR-RO                         
081900     MOVE OBKR-IDKUNDRF-RO     TO ORAD-IDKUNDRF-RO                        
082000     MOVE +0                   TO ORAD-IDSPECEMB                          
082100     MOVE OBKR-IDSYSTEM        TO ORAD-IDSYSTEM                           
082200     EJECT                                                                
082300     MOVE OBKR-KDVRINFO        TO ORAD-KDVRINFO                           
082400     MOVE OBKR-KDDSP           TO ORAD-KDDSP                              
082500     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
082600     MOVE OBKR-KDOI            TO ORAD-KDOI                               
082700     MOVE OBKR-CLEARGROUP      TO ORAD-CLEARGROUP                         
082800     MOVE OBKR-KDKVBRYT        TO ORAD-KDKVBRYT                           
082900     MOVE MID-KDORDING         TO ORAD-KDORDING                           
083000     IF OBKR-KDTPOTYP = +2                                                
083100        MOVE +2                TO ORAD-KDORDING                           
083200     END-IF                                                               
083300     MOVE JA                   TO ORAD-FLORDING                           
083400     MOVE OBKR-KDORDKL         TO ORAD-KDORDKL                            
083500     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
083600     MOVE OBKR-KDPRTYP         TO ORAD-KDPRTYP                            
083700     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
083800     MOVE OBKR-KDTPOTYP        TO ORAD-KDTPOTYP                           
083900     IF OBKR-KVPREAVB > +0     OR  OBKR-KVPRERO > +0                      
084000        COMPUTE ORAD-KVBEART = OBKR-KVPREAVB + OBKR-KVPRERO               
084100        MOVE ORAD-KVBEART      TO ORAD-KVBEART-Q                          
084200     ELSE                                                                 
084300        MOVE OBKR-KVBEART      TO ORAD-KVBEART                            
084400        MOVE OBKR-KVBEART-Q TO ORAD-KVBEART-Q                             
084500     END-IF                                                               
084600        MOVE OBKR-KVPREAVB     TO ORAD-KVPREAVB                           
084700     MOVE OBKR-KVPRERO         TO ORAD-KVPRERO                            
084800     MOVE +0                   TO ORAD-KVOKS-PREL                         
084900     MOVE OBKR-KVSLATT         TO ORAD-KVSLATT                            
085000     MOVE OBKR-PRARTNTO        TO ORAD-PRARTNTO                           
085100     MOVE OBKR-PRAVCOST        TO ORAD-PRAVCOST                           
085200     MOVE OBKR-DEAL-PR-LINE    TO ORAD-DEAL-PR-LINE                       
085300***VID T.EX ERSATTA BIPACKNINGSRADER, TPO6'OR ETC.                        
085400*** DE HAR INGEN PRISFRÅGA ÄNNU.                                          
085500     IF DIST79-DEALER-PRICE AND                                           
085600        (OBKR-PRARTNTO-LOC = +0 AND OBKR-PRARTNTO-LOCPREL = +0)           
085700        PERFORM DBA-ADD-PRICE-Q-LINE                                      
085800     END-IF                                                               
085900                                                                          
086000     MOVE OBKR-PRBPRIS         TO ORAD-PRBPRIS                            
086100     MOVE AREG-REKSIFFR        TO ORAD-REKSIFFR                           
086200     MOVE OBKR-RERF-RAD        TO ORAD-RERF-RAD                           
086300     MOVE OBKR-TIPRIS          TO ORAD-TIPRIS                             
086400     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
086500     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
086600     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
086700     MOVE OBKR-TIRODAT         TO ORAD-TIRODAT                            
086800     MOVE OBKR-TITPO           TO ORAD-TITPO                              
086900                                                                          
087000     MOVE OBKR-IDKUNDRF-WIP    TO ORAD-IDKUNDRF-WIP                       
087100     MOVE OBKR-PRAVCOST        TO ORAD-PRAVCOST                           
087200     MOVE OBKR-KDVALISO        TO ORAD-KDVALISO                           
087300     .                                                                    
087400                                                                          
087500 DBA-ADD-PRICE-Q-LINE SECTION.                                            
087600                                                                          
087700     IF DIST79-DEALER-PRICE                                               
087800       IF OBKR-PRARTNTO-LOC = +0    AND                                   
087900          OBKR-PRARTNTO-LOCPREL = +0                                      
088000         IF OBKR-IDPRQUES       = +0                                      
088100********* HÄMTAR NÄSTA LEDIGA PRISFRÅGENR                                 
088200           MOVE +0                    TO PRNO-IDPRQUES-IN                 
088300           MOVE +1                    TO PRNO-KDCALL                      
088400                                                                          
088500           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
088600                                                                          
088700**********UPPDATERAR WDC7 MED EN PRISFRÅGA                                
088800           MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                    
088900           MOVE +1                    TO PRQU-KDCALL                      
089000                                                                          
089100           MOVE OBKR-IDDISTR          TO PRQU-IDDISTR                     
089200           MOVE OBKR-IDKUNDNR         TO PRQU-IDKUNDNR                    
089300           MOVE OBKR-IDKUNDRF         TO PRQU-IDKUNDRF                    
089400           MOVE OBKR-IDORDER          TO PRQU-IDORDER                     
089500           MOVE OBKR-KDORDKL          TO PRQU-KDORDKL                     
089600           MOVE 'N'                   TO PRQU-KDPRSTA                     
089700           MOVE OBKR-IDARTNR          TO PRQU-IDARTNR                     
089800           MOVE OBKR-KVBEART-Q        TO PRQU-KVBEART-Q                   
089900           MOVE OBKR-PRARTNTO-LOC     TO PRQU-PRARTNTO-LOC                
090000           MOVE +0                    TO PRQU-PRARTNTO-LOCPREL            
090100           MOVE OBKR-IDSYSTEM         TO PRQU-IDSYSTEM                    
090200                                                                          
090300           PERFORM DBAA-HAEMTA-STA-STO-DATUM                              
090400                                                                          
090900           MOVE OBKR-KDVALISO            TO PRQU-KDVALISO                 
091100           IF OBKR-IDDISTR = 0778 AND OBKR-KDORDKL < 3                    
091200             AND OBKR-TIRODAT > 0                                         
091300             MOVE 4                      TO PRQU-KDORDKL                  
091400           END-IF                                                         
091500                                                                          
091600           CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                
091700                                           PRQU-WDC7-PCB                  
091800                                           PRQU-SJKO-WDK6-PCB             
091900                                                                          
092000           MOVE PRQU-IDPRQUES           TO  ORAD-IDPRQUES                 
092100           MOVE PRQU-FLPRTILL           TO  ORAD-FLPRTILL                 
092200           IF PRQU-KDORDKL = 4 AND ORAD-KDORDKL < 3                       
092300             MOVE 'N'                   TO ORAD-FLPRTILL                  
092400           END-IF                                                         
092500                                                                          
092600           IF OBKR-PRARTNTO-LOC = +0                                      
092700             MOVE PRQU-PRARTNTO-LOCPREL TO  ORAD-PRARTNTO-LOCPREL         
092800           ELSE                                                           
092900             MOVE OBKR-PRARTNTO-LOC     TO  ORAD-PRARTNTO-LOC             
093000           END-IF                                                         
093100                                                                          
093200*** UPPDATERA NÄSTA LEDIGA PRISFRÅGENR                                    
093300                                                                          
093400           MOVE PRQU-IDPRQUES           TO PRNO-IDPRQUES-IN               
093500           MOVE +3                      TO PRNO-KDCALL                    
093600                                                                          
093700           CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                
093800***SKICKA PRISFRÅGA                                                       
093900           PERFORM DBAC-SKICKA-PRISFRAGA                                  
094000         END-IF                                                           
094100       END-IF                                                             
094200     END-IF                                                               
094300     .                                                                    
094400 DBAA-HAEMTA-STA-STO-DATUM SECTION.                                       
094500                                                                          
094600      MOVE OBKR-IDDISTR                TO W-IDDISTR-WDB2                  
094700                                       W-IDDISTR-WDB2-MIN                 
094800                                       W-IDDISTR-WDB2-MAX                 
094900      MOVE OBKR-IDKUNDNR               TO W-IDKUNDNR-WDB2                 
095000      PERFORM IMS-GET-WDB201-UNIK                                         
095100      IF SEGMENT-FINNS                                                    
095200         CONTINUE                                                         
095300      ELSE                                                                
095400         PERFORM IMS-GU-WDB201                                            
095500      END-IF                                                              
095600      .                                                                   
095700      EJECT                                                               
097700 DBAC-SKICKA-PRISFRAGA SECTION.                                           
097800                                                                          
097900     MOVE 1                      TO 3039-REQU-IDMSGVER                    
098000     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
098100     MOVE 'W4029900'             TO 3039-REQU-IDUSER                      
098200                                                                          
098300     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
098400     MOVE OBKR-IDDISTR           TO 3039-MID-IDDISTR                      
098500     MOVE OBKR-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
098600     MOVE OBKR-IDORDNR7          TO 3039-MID-IDBUNDLE                     
098700     MOVE PRQU-IDPRQUES          TO 3039-MID-IDPRQUES                     
098800                                                                          
098900     PERFORM S11-SKICKA-OPEN                                              
099000     PERFORM S11-SKICKA-MEDDELANDE                                        
099100     PERFORM S11-SKICKA-CLOSE                                             
099200     .                                                                    
099300                                                                          
106200 DD-KOMPLETTERA-RANSONERING SECTION.                                      
106300                                                                          
106400     MOVE ORAD-BERADREF        TO RANS-BERADREF                           
106500     MOVE MID-FLEMBORD         TO RANS-FLEMBORD                           
106600     MOVE MID-FLFORBI          TO RANS-FLFORBI                            
106700     MOVE NEJ                  TO RANS-FLORDSPE                           
106800     MOVE MID-FLOVRLEV         TO RANS-FLOVRLEV                           
106900     MOVE ORAD-IDKAMPRF        TO RANS-IDKAMPRF                           
107000     MOVE ORAD-IDARTNR         TO RANS-IDARTNR                            
107100     MOVE ORAD-IDLEVNR         TO RANS-IDLEVNR                            
107200     MOVE MID-IDRFTAB          TO RANS-IDRFTAB                            
107300     MOVE ORAD-TIRODAT         TO RANS-TIRODAT                            
107400     MOVE ORAD-KDORDKL         TO RANS-KDORDKL                            
107500     MOVE +1                   TO RANS-KDORDBEH                           
107600     MOVE ORAD-KVBEART-Q       TO RANS-KVBEART-Q                          
107700     MOVE ORAD-KDTPOTYP        TO RANS-KDTPOTYP                           
107800     MOVE AREG-KDERS           TO RANS-KDERS                              
107900     MOVE AREG-KVLS            TO RANS-KVLS                               
108000     MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                          
108100     MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                           
108200     MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                           
108300     MOVE AREG-KVRESS          TO RANS-KVRESS                             
108400     MOVE AREG-KVSPANT         TO RANS-KVSPANT                            
108500     MOVE AREG-KVUTRS          TO RANS-KVUTRS                             
108600     MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                           
108700                                                                          
108800     MOVE AREG-KDPRODSL        TO TEST-KDPRODSL                           
108900     IF KDPRODSL-BIMA                                                     
109000       MOVE 1                  TO ORAD-RERF-RAD                           
109100                                RANS-RERF-RAD-UT                          
109200       MOVE ZERO               TO RANS-SUTPO-PB-UT                        
109300                                RANS-SUTPO-EJPB-UT                        
109400                                RANS-RERF-ART-UT                          
109500     ELSE                                                                 
109600       CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                    
109700                           RANS-ARTM-PCB RANS-ARTS-PCB                    
109800                                                                          
109900       MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                             
110000     END-IF                                                               
110100     .                                                                    
110200     EJECT                                                                
110300 DE-PREL-AVBOKNING-BIPA SECTION.                                          
110400                                                                          
110500     MOVE JA                   TO CDCA-FLFINLV-IN                         
110600     MOVE MID-FLFORBI          TO CDCA-FLFORBI-IN                         
110700     MOVE NEJ                  TO CDCA-FLORDSPE-IN                        
110800     MOVE MID-FLOVRLEV         TO CDCA-FLOVRLEV-IN                        
110900     MOVE MID-FLPRELRO         TO CDCA-FLPRELRO-IN                        
111000     MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                         
111100     MOVE OBKR-FLSLATT         TO CDCA-FLSLATT-IN                         
111200     MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                         
111300     MOVE ORAD-IDDISTR         TO CDCA-IDDISTR-IN                         
111400     MOVE ORAD-IDDC            TO CDCA-IDDC-IN                            
111500     MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                        
111600     MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                     
111700     MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                        
111800     MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                         
111900     MOVE +0                   TO CDCA-KDERS-IN                           
112000     MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                        
112100     MOVE ORAD-KDORDKL         TO CDCA-KDORDKL-IN                         
112200     MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                        
112300     MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                          
112400     MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                        
112500     MOVE AREG-KDUART          TO CDCA-KDUART-IN                          
112600     MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                         
112700     MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                       
112800     MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                       
112900     MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                       
113000     MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                        
113100     MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                        
113200     MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                        
113300     MOVE MID-RESLATT          TO CDCA-RESLATT-IN                         
113400     MOVE SPACE                TO CDCA-KDPROTYP-IN                        
113500     MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                       
113600     MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                       
113700     MOVE AREG-KVLS            TO CDCA-KVLS-IN                            
113800     MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                          
113900     MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                         
114000     MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                          
114100     MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                    
114200     MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                        
114300     MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                        
114400     MOVE +1                   TO CDCA-KDCALL                             
114500                                                                          
114600     CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                      
114700                                       CDCA-INLB-PCB                      
114800                                       CDCA-WDB2-PCB                      
114900                                       CDCA-WDC1-PCB                      
115000     MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                           
115100     MOVE CDCA-KVBEART-UT      TO ORAD-KVBEART                            
115200     MOVE CDCA-KVBEART-Q-UT    TO ORAD-KVBEART-Q                          
115300     MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                           
115400     MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                            
115500     MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                            
115600     MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                           
115700     .                                                                    
115800     EJECT                                                                
115900 DF-SKRIV-Q1-OCH-Q4-RADER SECTION.                                        
116000                                                                          
116100     IF ORAD-KVPREAVB > +0 OR ORAD-KVPRERO > +0                           
116200       PERFORM S09-KONTROLLERA-ENHETSLAST                                 
116300       PERFORM S10-BERAKNA-WOPS-SKRIV-ORAD                                
116400     END-IF                                                               
116500                                                                          
116600     IF CDCA-KDORDBEK-UT >  0                                             
116700       PERFORM DFA-SKRIV-ORDERBEKR                                        
117100     END-IF                                                               
117200     .                                                                    
117300     EJECT                                                                
117400 DFA-SKRIV-ORDERBEKR SECTION.                                             
117500                                                                          
117600     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
117700     MOVE ORAD-IDDC            TO OBKR-IDDC                               
117800     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
117900     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
118000     MOVE ORAD-KVPREAVB        TO OBKR-KVPREAVB                           
118100     MOVE ORAD-KVPRERO         TO OBKR-KVPRERO                            
118200     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
118300                                                                          
118400     IF CDCA-KDORDBEK-UT > +0                                             
118500*----(KOD 80, 92, 99)                                                     
118600        IF CDCA-KDORDBEK-UT =  80                                         
118700          MOVE CDCA-KVANNANT-UT TO OBKR-KVANNANT                          
118800        END-IF                                                            
118900                                                                          
119000        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
119100        MOVE '4299CDCA'        TO OBKR-IDPGM                              
119200        ADD +1                 TO OBKR-IDSEKVNR                           
119300        PERFORM  IMS-ISRT-ORQM-WDQ101                                     
119400     END-IF                                                               
119500     .                                                                    
119600     EJECT                                                                
119700                                                                          
119800 E-ANROPA-W413AVSR SECTION.                                               
119900                                                                          
120000*----ANROP BEHÖVER GÖRAS ENDAST OM RAD SKAPATS I AVSR-TABELLEN            
120100*                                                                         
120200     IF AVSR-IDDC(1) NOT = SPACE                                          
120300        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIST-PCB                   
120400              AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                   
120500              AVSR-WDB2-PCB AVSR-WDB6-PCB                                 
120600              TRAN-XXKB-PCB                                               
120700     END-IF                                                               
120800                                                                          
120900     PERFORM S01-NOLLA-AVSR-TABELL                                        
121000     .                                                                    
121100     EJECT                                                                
121200                                                                          
121300 F-OMSTARTA-EGEN-TRANS SECTION.                                           
121400                                                                          
121500     COMPUTE MSG-KVLL = LENGTH OF MID-W4I29901 + 17                       
121600     MOVE 'W4T299X '           TO MSG-KDTRANS-1                           
121700                                                                          
121800     MOVE '4299'               TO MSG-IDTRANS-1                           
121900                                                                          
122000     MOVE SPACE                TO MSG-KDMFSFOR-1                          
122100                                                                          
122200     MOVE MID-W4I29901         TO MSG-MID-OUT                             
122300                                                                          
122400     PERFORM IMS-ISRT-ALT-MSG-4299                                        
122500     .                                                                    
122600     EJECT                                                                
122700 G-STARTA-4298-ORDERAVSLUT SECTION.                                       
122800                                                                          
122900     COMPUTE MSG-KVLL = LENGTH OF MOD4298-MID-W4I29801 + 17               
123000     MOVE 'W4T298X '           TO MSG-KDTRANS-1                           
123100                                                                          
123200     MOVE '4299'               TO MSG-IDTRANS-1                           
123300                                                                          
123400     MOVE SPACE                TO MSG-KDMFSFOR-1                          
123500                                                                          
123600     MOVE MID-IDDISTR          TO MOD4298-MID-IDDISTR                     
123700     MOVE MID-IDKUNDNR         TO MOD4298-MID-IDKUNDNR                    
123800     MOVE MID-IDKUNDRF         TO MOD4298-MID-IDKUNDRF                    
123900     MOVE MID-IDORDER          TO MOD4298-MID-IDORDER                     
124000                                                                          
124100     PERFORM IMS-ISRT-ALT-MSG-4298                                        
124200     .                                                                    
124300     EJECT                                                                
124400 S01-NOLLA-AVSR-TABELL SECTION.                                           
124500                                                                          
124600     MOVE +1                   TO WS-INDEX-WOPS                           
124700     PERFORM UNTIL WS-INDEX-WOPS > +100                                   
124800        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
124900        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
125000        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
125100        MOVE +0                TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
125200        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
125300        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
125400        MOVE +0                TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
125500        MOVE SPACE             TO AVSR-KDVALISO(WS-INDEX-WOPS)            
125600                                  AVSR-KDVAT(WS-INDEX-WOPS)               
125700                                  AVSR-KDRAB(WS-INDEX-WOPS)               
125800        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
125900        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
126000        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
126100        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
126200        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
126300        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
126400        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
126500        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
126600        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
126700                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
126800        ADD +1                 TO WS-INDEX-WOPS                           
126900     END-PERFORM                                                          
127000                                                                          
127100     MOVE +1                   TO WS-INDEX-WOPS                           
127200     .                                                                    
127300     EJECT                                                                
127400 S09-KONTROLLERA-ENHETSLAST SECTION.                                      
127500                                                                          
127600     MOVE ZERO                 TO LAST-ADGANG-UT                          
127700     MOVE ZERO                 TO LAST-ADLAGOMR-UT                        
127800     MOVE ZERO                 TO LAST-KVANTAL-UT                         
127900     MOVE ZERO                 TO LAST-KVBEART-UT                         
128000     IF MID-FLFORBI = NEJ   AND MID-FLOVRLEV = NEJ AND                    
128100        ORAD-IDLEVNR = SPACE AND                                          
128200       (AREG-KVQPACK-3 > +0 OR AREG-KVQPACK-4 > +0)                       
128300                                                                          
128400        MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                        
128500        MOVE MID-FLFORBI          TO LAST-FLFORBI                         
128600        MOVE MID-FLOVRLEV         TO LAST-FLOVRLEV                        
128700        MOVE NEJ                  TO LAST-FLORDSPE                        
128800        MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                         
128900        MOVE ORAD-IDDC            TO LAST-IDDC                            
129000        MOVE MID-KDFDKRAV         TO LAST-KDFDKRAV                        
129100        IF MID-FLOVRLEV = JA                                              
129200           MOVE ORAD-KVBEART-Q    TO LAST-KVPREAVB                        
129300        ELSE                                                              
129400           MOVE ORAD-KVPREAVB     TO LAST-KVPREAVB                        
129500        END-IF                                                            
129600        MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                       
129700        MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                       
129800                                                                          
129900        CALL W411LAST USING LAST-W411LAST                                 
130000     END-IF                                                               
130100     .                                                                    
130200     EJECT                                                                
130300 S10-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
130400                                                                          
130500     IF LAST-ADLAGOMR-UT = +0 AND                                         
130600        LAST-KVANTAL-UT  = +0 AND                                         
130700        LAST-KVBEART-UT  = +0                                             
130800*------------------------------------------------------------*            
130900*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
131000*------------------------------------------------------------*            
131100        PERFORM S10A-FIXA-LAGEROMR-PLATS                                  
131200        PERFORM S10B-REDIGERA-WOPS-AREA                                   
131300        PERFORM IMS-ISRT-ORQF-WDQ401                                      
131400        PERFORM UNTIL SEGMENT-FINNS                                       
131500           ADD +1           TO ORAD-IDLOPNR                               
131600           PERFORM IMS-ISRT-ORQF-WDQ401                                   
131700        END-PERFORM                                                       
131800     ELSE                                                                 
131900                                                                          
132000        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
132100*------------------------------------------------------------*            
132200*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
132300*------------------------------------------------------------*            
132400                                                                          
132500           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
132600                                    ORAD-KVPRERO                          
132700           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
132800           PERFORM S10C-BERAEKNA-KVSLATT                                  
132900           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
133000           PERFORM S10B-REDIGERA-WOPS-AREA                                
133100           PERFORM IMS-ISRT-ORQF-WDQ401                                   
133200           PERFORM UNTIL SEGMENT-FINNS                                    
133300              ADD +1        TO ORAD-IDLOPNR                               
133400              PERFORM IMS-ISRT-ORQF-WDQ401                                
133500           END-PERFORM                                                    
133600     EJECT                                                                
133700*------------------------------------------------------------*            
133800*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
133900*------------------------------------------------------------*            
134000                                                                          
134100           MOVE +0                   TO ORAD-KVBEART                      
134200           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
134300                                        ORAD-KVPREAVB                     
134400           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
134500                                    ORAD-KVPRERO                          
134600           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
134700           IF   LAST-ADGANG-UT > ZERO                                     
134800             MOVE LAST-ADGANG-UT     TO ORAD-ADGANG                       
134900           END-IF                                                         
135000           MOVE +0                   TO ORAD-KVPRERO                      
135100           MOVE 1.0000               TO ORAD-RERF-RAD                     
135200           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
135300           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
135400           PERFORM S10B-REDIGERA-WOPS-AREA                                
135500           PERFORM IMS-ISRT-ORQF-WDQ401                                   
135600           PERFORM UNTIL SEGMENT-FINNS                                    
135700              ADD +1        TO ORAD-IDLOPNR                               
135800              PERFORM IMS-ISRT-ORQF-WDQ401                                
135900           END-PERFORM                                                    
136000        ELSE                                                              
136100*------------------------------------------------------------*            
136200*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
136300*------------------------------------------------------------*            
136400           MOVE LAST-ADLAGOMR-UT  TO ORAD-ADLAGOMR                        
136500           IF   LAST-ADGANG-UT > ZERO                                     
136600             MOVE LAST-ADGANG-UT  TO ORAD-ADGANG                          
136700           END-IF                                                         
136800           MOVE 1.0000            TO ORAD-RERF-RAD                        
136900           PERFORM S10A-FIXA-LAGEROMR-PLATS                               
137000           PERFORM S10B-REDIGERA-WOPS-AREA                                
137100           PERFORM IMS-ISRT-ORQF-WDQ401                                   
137200           PERFORM UNTIL SEGMENT-FINNS                                    
137300              ADD +1        TO ORAD-IDLOPNR                               
137400              PERFORM IMS-ISRT-ORQF-WDQ401                                
137500           END-PERFORM                                                    
137600        END-IF                                                            
137700     END-IF                                                               
137800     .                                                                    
137900     EJECT                                                                
138000 S10A-FIXA-LAGEROMR-PLATS SECTION.                                        
138100                                                                          
138200     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
138300     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
138400     MOVE ORAD-BERADREF        TO ADRS-BEVARREF-IN                        
138500     MOVE MID-FLFORBI          TO ADRS-FLFORBI-IN                         
138600     MOVE ORAD-IDDISTR         TO ADRS-IDDISTR-IN                         
138700     MOVE 1                    TO ADRS-KDCALL-IN                          
138800     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
138900     MOVE ORAD-KDORDKL         TO ADRS-KDORDKL-IN                         
139000     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
139100     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
139200                                                                          
139300     CALL W413ADRS USING ADRS-W413ADRS                                    
139400                                                                          
139500*- KAMPANJORDRAR FÅR ETT FIKTIVT LAGEROMRÅDE.E'TRACKER 2218613.           
139600     IF ORAD-IDKAMPRF > 0                                                 
139700       MOVE 8                  TO ORAD-ADLAGOMR                           
139800     ELSE                                                                 
139900       MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                           
140000     END-IF                                                               
140100                                                                          
140700     MOVE ORAD-IDDISTR         TO TEST-IDDISTR                            
140800     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
140900                                                                          
141000     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
141100     .                                                                    
141200     EJECT                                                                
141300 S10B-REDIGERA-WOPS-AREA SECTION.                                         
141400                                                                          
141500     MOVE +1                   TO AVSR-KDCALL                             
141600     MOVE ORAD-IDORDER         TO AVSR-IDORDER                            
141700     MOVE ORAD-KDORDKL         TO AVSR-KDORDKL                            
141800     MOVE MID-KDFRAKT          TO AVSR-KDFRAKT                            
141900     MOVE MID-KDROPACK         TO AVSR-KDROPACK                           
142000     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
142100     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
142200                                                                          
142300     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
142400     IF MID-FLEMBORD = JA OR MID-FLOVRLEV = JA                            
142500        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
142600     ELSE                                                                 
142700        MOVE ORAD-IDLEVNR      TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
142800     END-IF                                                               
142900     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
143000     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
143100     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
143200     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
143300     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
143400     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
143500     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
143600     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
143700     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
143800     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
143900     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
144000                                                                          
144100     ADD +1                    TO WS-INDEX-WOPS                           
144200     .                                                                    
144300     EJECT                                                                
144400 S10C-BERAEKNA-KVSLATT SECTION.                                           
144500                                                                          
144600     IF ORAD-FLRESTN = JA AND MID-RESLATT > ZERO                          
144700                                                                          
144800        COMPUTE WS-RESLATT = MID-RESLATT / 100                            
144900                                                                          
145000        COMPUTE ORAD-KVSLATT ROUNDED =                                    
145100               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
145200     END-IF                                                               
145300     .                                                                    
145400     EJECT                                                                
145500                                                                          
145600 S11-SKICKA-OPEN SECTION.                                                 
145700                                                                          
145800     MOVE 'OPEN'                     TO SEND-KDFUNC                       
145900     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
146000     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
146100                                                                          
146200     IF SEND-KDRC > 0                                                     
146300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
146400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
146500       DELIMITED BY SIZE INTO FELTEXT                                     
146600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
146700     END-IF                                                               
146800     .                                                                    
146900     SKIP3                                                                
147000 S11-SKICKA-MEDDELANDE SECTION.                                           
147100                                                                          
147200     MOVE 'PUT'                      TO SEND-KDFUNC                       
147300     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
147400     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
147500                                                                          
147600     IF SEND-KDRC > 0                                                     
147700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
147800       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
147900       DELIMITED BY SIZE INTO FELTEXT                                     
148000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
148100     END-IF                                                               
148200     .                                                                    
148300     SKIP3                                                                
148400 S11-SKICKA-CLOSE SECTION.                                                
148500                                                                          
148600     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
148700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
148800                                                                          
148900     IF SEND-KDRC > 0                                                     
149000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
149100       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
149200       DELIMITED BY SIZE INTO FELTEXT                                     
149300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
149400     END-IF                                                               
149500     .                                                                    
149600     EJECT                                                                
149700                                                                          
149800                                                                          
149900 IMS-GET-MSG SECTION.                                                     
150000                                                                          
150100     MOVE    '  QC'          TO    GODK-STATUSKODER                       
150200     CALL    CBLTDLI         USING GU   MSG-PCB MSG-IO-AREA               
150300     MOVE    MSG-STATUS-CODE TO    STATUS-WS                              
150400     PERFORM IMS-STATUSKONTROLL                                           
150500     .                                                                    
150600     SKIP3                                                                
150700 IMS-ISRT-ALT-MSG-4299 SECTION.                                           
150800                                                                          
150900     MOVE    LOW-VALUE        TO    MSG-KDZ1 MSG-KDZ2                     
151000     MOVE    '  '             TO    GODK-STATUSKODER                      
151100     CALL    CBLTDLI          USING ISRT 4299-PCB MSG-IO-AREA             
151200     MOVE    4299-STATUS-CODE TO    STATUS-WS                             
151300     PERFORM IMS-STATUSKONTROLL                                           
151400     .                                                                    
151500     SKIP3                                                                
151600 IMS-ISRT-ALT-MSG-4298 SECTION.                                           
151700                                                                          
151800     MOVE    LOW-VALUE        TO    MSG-KDZ1 MSG-KDZ2                     
151900     MOVE    '  '             TO    GODK-STATUSKODER                      
152000     CALL    CBLTDLI          USING ISRT 4298-PCB MSG-IO-AREA             
152100     MOVE    4298-STATUS-CODE TO    STATUS-WS                             
152200     PERFORM IMS-STATUSKONTROLL                                           
152300     .                                                                    
152400     EJECT                                                                
152500 IMS-ISRT-ORQM-WDQ101 SECTION.                                            
152600                                                                          
152700     MOVE 'WLORQM01 '          TO SSA1                                    
152800     MOVE '    '               TO GODK-STATUSKODER                        
152900     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
153000     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
153100     PERFORM IMS-STATUSKONTROLL                                           
153200     .                                                                    
153300     SKIP2                                                                
153400 IMS-GU-ORQM-WDQ101 SECTION.                                              
153500                                                                          
153600     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
153700                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
153800          DELIMITED BY SIZE INTO SSA1                                     
153900     MOVE '  GE'               TO GODK-STATUSKODER                        
154000     CALL CBLTDLI USING GU   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
154100     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
154200     PERFORM IMS-STATUSKONTROLL                                           
154300     .                                                                    
154400     SKIP2                                                                
154500 IMS-GN-ORQM-WDQ101 SECTION.                                              
154600                                                                          
154700     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
154800                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
154900          DELIMITED BY SIZE INTO SSA1                                     
155000     MOVE '  GEGB'             TO GODK-STATUSKODER                        
155100     CALL CBLTDLI USING GN   ORQM-PCB DLI-IO-AREA-OBKR SSA1               
155200     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
155300     PERFORM IMS-STATUSKONTROLL                                           
155400     .                                                                    
155500     EJECT                                                                
155600 IMS-ISRT-ORQF-WDQ401 SECTION.                                            
155700                                                                          
155800     MOVE 'WLORQF01 '          TO SSA1                                    
155900     MOVE '  II'               TO GODK-STATUSKODER                        
156000     CALL CBLTDLI USING ISRT ORQF-PCB DLI-IO-AREA-ORAD SSA1               
156100     MOVE ORQF-STATUS-CODE     TO STATUS-WS                               
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     .                                                                    
156400     SKIP2                                                                
156500 IMS-11-GU-WDK711 SECTION.                                                
156600                                                                          
156700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
156800          DELIMITED BY SIZE  INTO SSA1                                    
156900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
157000          DELIMITED BY SIZE  INTO SSA2                                    
157100     MOVE '  GE'               TO GODK-STATUSKODER                        
157200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2            
157300     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
157400     PERFORM IMS-STATUSKONTROLL                                           
157500     .                                                                    
157600     SKIP3                                                                
157700 IMS-GU-WDK712           SECTION.                                         
157800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
157900            DELIMITED BY SIZE INTO SSA1                                   
158000     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
158100            DELIMITED BY SIZE INTO SSA2                                   
158200     MOVE '  GE' TO GODK-STATUSKODER                                      
158300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
158400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
158500     PERFORM IMS-STATUSKONTROLL                                           
158600     .                                                                    
158700     SKIP3                                                                
160600 IMS-GET-WDB201-UNIK SECTION.                                             
160700                                                                          
160800     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
160900          DELIMITED BY SIZE INTO SSA1                                     
161000     MOVE '  GE'               TO GODK-STATUSKODER                        
161100     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
161200     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
161300     PERFORM IMS-STATUSKONTROLL                                           
161400     .                                                                    
161500     SKIP2                                                                
161600 IMS-GU-WDB201 SECTION.                                                   
161700                                                                          
161800     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
161900                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
162000            DELIMITED BY SIZE INTO SSA1                                   
162100     MOVE '  GE'               TO GODK-STATUSKODER                        
162200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
162300     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
162400     PERFORM IMS-STATUSKONTROLL                                           
162500     .                                                                    
162600     SKIP2                                                                
163700 IMS-GU-WDB601    SECTION.                                                
163800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
163900          DELIMITED BY SIZE INTO SSA1                                     
164000     MOVE '  '   TO GODK-STATUSKODER                                      
164100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
164200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
164300     PERFORM IMS-STATUSKONTROLL                                           
164400     .                                                                    
164500     SKIP2                                                                
164600 IMS-STATUSKONTROLL SECTION.                                              
164700                                                                          
164800     SET    STATUS-IX TO 1                                                
164900     SEARCH GODK-STATUS                                                   
165000       AT END                                                             
165100         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
165200           DELIMITED BY SIZE INTO FELTEXT                                 
165300         CALL FELLOG                                                      
165400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
165500         CONTINUE                                                         
165600     END-SEARCH                                                           
165700     .                                                                    
