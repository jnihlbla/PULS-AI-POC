000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4068000.                                                
000400 AUTHOR.         BO HAMMARIN, GDC GROUP.                                  
000500 DATE-WRITTEN.   APRIL-99.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET ÄR ETT BAKGRUNDS-MPP SOM                              
001000*        1 TAR EMOT PREL. ANNULLATIONSRADER FRÅN PULS/VOLVO-VISION        
001100*        2 KONTROLLERAR OM ORDERN ÄNNU EJ ÄR SKICKAD TILL EDI             
001200*          OCH I SÅ FALL GÖRS ANNULLATIONEN "DIREKT"                      
001300*        3 SKAPAR/SKICKAR ORDER-CHANGE POSTER TILL EDI                    
001400*          VIA VCOM ENLIGT VERSION 1 - EDIFACT D/96B                      
001500*          OM ORDERN ÄR SKICKAD TILL EDI                                  
001600*        4 SKAPAR LOGG-POSTER FÖR VIDARE BEARBETNING                      
001700*        5 LARM MAIL OM RADEN SAKNAS PÅ WDF4 VID UPPDATERING              
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T680X                                             
002100*        MID:         W4I68001 F                                          
002200*                                                                         
002300*    UTDATA:                                                              
002400*        UTSKRIVNA ORDER   (VIA MQ)                                       
002500*                                                                         
002600*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
002700*    E-TRACKER: 10129446 2013-HÖST  DIREKTLEVERANSLARM                    
002800*    E-TRACKER: 10254592 2015-HÖST  DECOMISSION VOHF                      
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500                                                                          
003600 77  IDPGM                        PIC X(08)   VALUE 'W4068000'.           
003700 77  JA                           PIC X       VALUE 'J'.                  
003800 77  YES                          PIC X       VALUE 'Y'.                  
003900 77  NEJ                          PIC X       VALUE 'N'.                  
004000 77  KDRC-DISPLAY                PIC Z(5).                                
004100                                                                          
004200 77  CURRENT-SECTION              PIC X(16)   VALUE SPACE.                
004300 77  CURRENT-IMS-SECTION          PIC X(16)   VALUE SPACE.                
004400 77  CURRENT-DB2-SECTION          PIC X(16)   VALUE SPACE.                
004500 77  CURRENT-DAP-SECTION          PIC X(16)   VALUE SPACE.                
004600                                                                          
004700 77  2109-IX                      PIC S9(3)   VALUE +0   COMP-3.          
004800 77  2109-IX-MAX                  PIC S9(3)   VALUE +18  COMP-3.          
004900                                                                          
005000 77  SW-ORDER-FINNS               PIC X(1)    VALUE 'N'.                  
005100     88  ORDER-FINNS                          VALUE 'J'.                  
005200                                                                          
005300 77  SW-DELETE-PA-WDF6            PIC X(1)    VALUE 'N'.                  
005400     88  BORTTAGEN-PA-WDF6                    VALUE 'J'.                  
005500                                                                          
005600 77  SW-MAIL-SKICKAT              PIC X(1)    VALUE 'N'.                  
005700     88  MAIL-SKICKAT                         VALUE 'J'.                  
005800     88  FIRST-MAIL-RAD                       VALUE 'N'.                  
005900                                                                          
006000 77  W-IDTRANS                    PIC X(4)    VALUE SPACE.                
006100     88  EGEN-MID                             VALUE '4680'.               
006200     88  GODK-MID                             VALUE '4680'.               
006300                                                                          
006400 77  WS-ANTAL-ANNULL-RADER        PIC S9(3)   VALUE ZERO  COMP-3.         
006500 01  WS-IDLEVNR-KOLL              PIC X(5) VALUE SPACE.                   
006600 77  WS-IDARTNR                   PIC 9(9).                               
006700 77  WS-IDORDNR5                  PIC 9(5).                               
006800 77  WS-IDORDNR7                  PIC 9(7).                               
006900 77  WS-NO-OF-SEGM                PIC S9(3)   COMP-3 VALUE ZERO.          
007000 77  WS-DATUM-9KOMPL              PIC 9(8).                               
007100 77  WS-DAGENS-DATUM              PIC 9(6)   VALUE ZERO.                  
007200 77  WS-TIME-WAIT                 PIC S9(9)  COMP VALUE +150.             
007300                                                                          
007400                                                                          
007500 01  WS-HHMMSSDD-RED.                                                     
007600     03  WS-HHMMSS                PIC  9(6).                              
007700     03  WS-DD                    PIC  9(2).                              
007800                                                                          
007900 01  WS-IDKUNDRF-OLD.                                                     
008000     03  WS-IDORDNR5-OLD          PIC 9(5).                               
008100     03  FILLER                   PIC X(5)    VALUE SPACE.                
008200                                                                          
008300 01  WS-IDKUNDRF-NEW.                                                     
008400     03  WS-IDORDNR7-NEW          PIC 9(7).                               
008500     03  FILLER                   PIC X(3)    VALUE SPACE.                
008600                                                                          
008700 01  WS-IDDISTR-IDKUNDNR.                                                 
008800     03  WS-IDDISTR               PIC 9(4).                               
008900     03  WS-IDKUNDNR              PIC 9(6).                               
009000                                                                          
009100 01  WS-UNB-RECIPIENT-ID-X.                                               
009200     03  WS-UNB-RECIPIENT-ID      PIC 9(5).                               
009300                                                                          
009400 01  WS-SAVE-IDCOM-TACDIS-ALARM  PIC S9(9)  COMP VALUE ZERO.              
009500 01  WS-SAVE-IDCOM-EDI           PIC S9(9)  COMP VALUE ZERO.              
009600                                                                          
009700 01  WS-DAGENS-DATUM-8            PIC 9(8).                               
009800 01  WS-DAGENS-DATUM-6            PIC 9(6).                               
009900 01  WS-DAGENS-KLOCKA-NUM         PIC 9(6).                               
010000 01  WS-DAGENS-KLOCKA.                                                    
010100     03  WS-DAGENS-KLOCKA-1-6.                                            
010200         05  WS-DAGENS-KLOCKA-1-4 PIC 9(4).                               
010300         05  WS-DAGENS-KLOCKA-5-6 PIC 9(2).                               
010400     03  WS-DAGENS-KLOCKA-7-9     PIC 9(3).                               
010500                                                                          
010600 01  WS-DAGENS-DATUM-KLOCKA.                                              
010700     03  WS-DAGENS-DATUM-ALFA     PIC X(8).                               
010800     03  WS-DAGENS-KLOCKA-ALFA    PIC X(4).                               
010900 01  WS-DAGENS-DATUM-KLOCKA-14.                                           
011000     03  WS-DAGENS-DATUM-ALFA-14  PIC X(8).                               
011100     03  WS-DAGENS-KLOCKA-ALFA-14 PIC X(6).                               
011200                                                                          
011300                                                                          
011400 01  W-VIMSID.                                                            
011500   03  W-IMSID               PIC X(4)    VALUE SPACE.                     
011600   03  FILLER                PIC X(4)    VALUE SPACE.                     
011700                                                                          
011800 01 NYCKLAR-TP4TRAN.                                                      
011900     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
012000                                                                          
012100                                                                          
012200                                                                          
012300 01  TEST-IDDISTR                 PIC S9(5) COMP-3.                       
012400*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
012500                                                                          
012600 01  FILLER                       PIC X(16) VALUE 'REFILLTABDC'.          
012700*   -COPY WWDIST57                                                        
012800                                                                          
012900 01  FILLER                       PIC X(16) VALUE 'BYTESARTIKLAR'.        
013000*   -COPY WWBYT03                                                         
013100                                                                          
013200 01  FILLER                       PIC X(16) VALUE '*W402TACD*'.           
013300*   -COPY W402TACD                                                        
013400                                                                          
013500 01  FILLER                       PIC X(10) VALUE 'SPAR-AREOR'.           
013600 01  SPAR-AREOR.                                                          
013700   03    SPAR-AREA.                                                       
013800     05  SPAR-VKORDNTO            PIC  9(6)V9(4)    VALUE ZERO.           
013900     05  SPAR-VLORDNTO            PIC  9(4)V9(7)    VALUE ZERO.           
014000     05  SPAR-SUORDV-LEVPL        PIC  S9(9)V9(2)   VALUE ZERO.           
014100     05  SPAR-SUORDV-LEVPL-LOC    PIC  S9(9)V9(2)   VALUE ZERO.           
014200     05  SPAR-SUORDV-LEVPL-LOCPREL PIC  S9(9)V9(2)   VALUE ZERO.          
014300                                                                          
014400                                                                          
014500 01  UT-AREOR-START               PIC X(24)   VALUE                       
014600                                 'UT-AREOR-START  '.                      
014700*01  -COPY WEDIBGM1                                                       
014800                                                                          
014900*01  -COPY WEDIDTM1                                                       
015000                                                                          
015100*01  -COPY WEDIRFF1                                                       
015200                                                                          
015300*01  -COPY WEDINAD1                                                       
015400                                                                          
015500*01  -COPY WEDILIN2                                                       
015600                                                                          
015700*01  -COPY WEDIQTY1                                                       
015800                                                                          
015900*01  -COPY WEDIH001                                                       
016000                                                                          
016100*01  -COPY WEDIUNB0                                                       
016200                                                                          
016300*01  -COPY WEDIUNH0                                                       
016400                                                                          
016500*01  -COPY WEDIUNS1                                                       
016600                                                                          
016700*01  -COPY WEDIUNT0                                                       
016800                                                                          
016900*01  -COPY WEDIT003                                                       
017000                                                                          
017100                                                                          
017200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017300 01  GENERELLA-SUBPROGRAM.                                                
017400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017700     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
017800     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
017900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
018000     03  WZ11OUTQ                PIC X(8)    VALUE 'WZ11OUTQ'.            
018100     03  VIMSID                  PIC X(8)    VALUE 'VIMSID  '.            
018200                                                                          
018300*    --- PARAMETRAR TILL ABEND                                            
018400                                                                          
018500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
018600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
018700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
018800                                                                          
018900 01  FELTEXT.                                                             
019000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
019100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
019200                                                                          
019300 01  FILLER                      PIC X(16) VALUE 'CIA-AREA'.              
019400                                                                          
019500*   -COPY W009CIA                                                         
019600*                                                                         
019700                                                                          
019800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
019900*                                                                         
020000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020100                                                                          
020200*01  MID -COPY W4I68001                                                   
020300                                                                          
020400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
020500                                                                          
020600*01  -COPY WMSGAREA                                                       
020700                                                                          
020800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020900                                                                          
021000*01  -COPY WMSGSPAR                                                       
021100                                                                          
021200                                                                          
021300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021400*                                                                         
021500                                                                          
021600 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
021700 01  W-IDPRODNR-X.                                                        
021800   03  W-IDPRODNR            PIC S9(7) COMP-3.                            
021900*                                                                         
022000 01  W-IDPURAD-X.                                                         
022100   03  W-IDPURAD             PIC S9(5) COMP-3.                            
022200*                                                                         
022300 01  W-WDE401-X.                                                          
022400   03  W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.              
022500   03  W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.              
022600   03  W-401-IDKUNDRF.                                                    
022700     05  W-401-IDORDNR       PIC 9(5)    VALUE ZERO.                      
022800     05  FILLER              PIC X(5)    VALUE SPACE.                     
022900   03  W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.              
023000   03  W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.              
023100 01  W-WDE411-X.                                                          
023200   03  W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.              
023300*                                                                         
023400 01    W-WDE601-X.                                                        
023500   03    W-601-IDPRODNR      PIC S9(7)   VALUE ZERO  COMP-3.              
023600 01  W-IDORDER-X.                                                         
023700   03  W-IDORDER             PIC S9(7)   COMP-3.                          
023800*                                                                         
023900 01  W-WDQ211-X.                                                          
024000   03  W-211-IDDC            PIC  X(2).                                   
024100   03  W-211-IDLEVNR         PIC  X(5)   VALUE SPACE.                     
024200*                                                                         
024300 01  W-Q301-KEY-X.                                                        
024400   03  W-Q301-IDORDER        PIC S9(7)   VALUE ZERO  COMP-3.              
024500   03  W-Q301-IDDC           PIC X(2).                                    
024600   03  W-Q301-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.              
024700   03  W-Q301-IDPLKLST       PIC S9(3)   VALUE ZERO  COMP-3.              
024800*                                                                         
024900 01  W-WDK601-IDARTNR-X.                                                  
025000   03  W-WDK601-IDARTNR-N    PIC S9(9)   VALUE ZERO  COMP-3.              
025100 01  W-WDK701-IDARTNR-X.                                                  
025200   03  W-WDK701-IDARTNR-N    PIC S9(9)   VALUE ZERO  COMP-3.              
025300 01  W-WDK711-IDDC-X.                                                     
025400   03  W-WDK711-IDDC         PIC X(2)    VALUE ZERO.                      
025500                                                                          
025600 01  W-IDLEVNR-F4-X.                                                      
025700   03  W-IDLEVNR-F4          PIC X(5)    VALUE SPACE.                     
025800                                                                          
025900 01  W-WDF411KY-MIN-X.                                                    
026000   03 W-IDPRODNR-F4-MIN      PIC S9(7) COMP-3  VALUE ZERO.                
026100   03 W-IDPURAD-F4-MIN       PIC S9(5) COMP-3  VALUE ZERO.                
026200   03 W-TIUTSKR-F4-MIN       PIC S9(7) COMP-3  VALUE ZERO.                
026300 01  W-WDF411KY-MAX-X.                                                    
026400   03 W-IDPRODNR-F4-MAX      PIC S9(7) COMP-3  VALUE ZERO.                
026500   03 W-IDPURAD-F4-MAX       PIC S9(5) COMP-3  VALUE ZERO.                
026600   03 W-TIUTSKR-F4-MAX       PIC S9(7) COMP-3  VALUE 9999999.             
026700                                                                          
026800 01  W-IDGMT-X.                                                           
026900   03 W-IDDISTR              PIC S9(5) COMP-3  VALUE ZERO.                
027000   03 W-IDKUNDNR             PIC S9(7) COMP-3  VALUE ZERO.                
027100                                                                          
027200*    --- STATUS-KOD FRÅN IMS                                              
027300 01  STATUS-WS                   PIC XX.                                  
027400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027500     88  SEGMENT-FINNS                       VALUE '  '.                  
027600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027700                                                                          
027800 01  GODK-STATUSKODER.                                                    
027900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028000                                                                          
028100 01  ALL-SSA.                                                             
028200     03 SSA1                     PIC X(128).                              
028300     03 SSA2                     PIC X(64).                               
028400                                                                          
028500*                            DB2 FUNKTIONSKODER                           
028600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
028700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
028800                                                                          
028900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
029000 01  DB2-WS.                                                              
029100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
029200         88  CURSOR-OK                       VALUE 000.                   
029300         88  RADER-FINNS                     VALUE 000.                   
029400         88  RADER-SAKNAS                    VALUE 100.                   
029500         88  ATKOMST-FEL                     VALUE 904.                   
029600     03  GODK-SQLCODEKODER.                                               
029700         05  GODK-SQLCODE OCCURS 5                                        
029800             INDEXED BY SQLCODE-IX PIC 9(3).                              
029900 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
030000                                                                          
030100*    --- IMS FUNKTIONSKODER                                               
030200*01  -COPY W0003                                                          
030300                                                                          
030400******************************************************************        
030500*    DLI-IO-AREAOR                                               *        
030600******************************************************************        
030700                                                                          
030800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF601  '.                    
030900 01  DLI-IO-WDF601.                                                       
031000*    03  -COPY WDF601                                                     
031100                                                                          
031200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF611  '.                    
031300 01  DLI-IO-WDF611.                                                       
031400*    03  -COPY WDF611                                                     
031500                                                                          
031600 01  FILLER         PIC X(16)   VALUE 'DLI-IO-ART '.                      
031700 01  DLI-IO-AREA-ART.                                                     
031800*    03  WLARTS11   -COPY WDK711                                          
031900                                                                          
032000                                                                          
032100 01  FILLER         PIC X(16)   VALUE 'DLI-IO-K611'.                      
032200 01  DLI-IO-WDK611.                                                       
032300*    03             -COPY WDK611                                          
032400                                                                          
032500                                                                          
032600 01  FILLER         PIC X(16)   VALUE 'DLI-IO-OBKR'.                      
032700 01  DLI-IO-AREA-OBKR.                                                    
032800*    03  WLORQM01   -COPY WDQ101                                          
032900                                                                          
033000                                                                          
033100 01  FILLER         PIC X(16)   VALUE 'DLI-IO-Q201'.                      
033200 01  DLI-IO-Q201.                                                         
033300*    03  -COPY WDQ201                                                     
033400                                                                          
033500                                                                          
033600 01  FILLER         PIC X(16)   VALUE 'DLI-IO-Q211'.                      
033700 01  DLI-IO-Q211.                                                         
033800*    03  -COPY WDQ211                                                     
033900                                                                          
034000                                                                          
034100 01  FILLER         PIC X(16)   VALUE 'DLI-IO-ORQA'.                      
034200 01  DLI-IO-AREA-ORQA.                                                    
034300*    03  WLORQA01   -COPY WDQ301                                          
034400                                                                          
034500                                                                          
034600 01  FILLER         PIC X(16)   VALUE 'DLI-IO-E601'.                      
034700 01  DLI-IO-E601.                                                         
034800*    03  -COPY WDE601                                                     
034900                                                                          
035000 01  FILLER         PIC X(16)   VALUE 'DLI-IO-E401'.                      
035100 01  DLI-IO-E401.                                                         
035200*    03  -COPY WDE401                                                     
035300                                                                          
035400 01  FILLER         PIC X(16)   VALUE 'DLI-IO-E411'.                      
035500 01  DLI-IO-E411.                                                         
035600*    03  -COPY WDE411                                                     
035700                                                                          
035800 01  FILLER         PIC X(16)   VALUE 'DLI-IO-LOGG'.                      
035900 01  DLI-IO-WDGZ01.                                                       
036000   03    IO-WDGZ01  PIC X(200)  VALUE SPACE.                              
036100                                                                          
036200*  03    WDGZ01     -COPY WDGZ01  -PRE LOGG- -RED IO-WDGZ01.              
036300                                                                          
036400 01  FILLER         PIC X(17)  VALUE 'ANNULLATIONSTRANS'.                 
036500                                                                          
036600*01  WDGZRY5        -COPY WDGZRY5.                                        
036700                                                                          
036800*01  WDGZRY5        -COPY WDGZRY5S.                                       
036900                                                                          
037000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLFILA01'.                    
037100*01  WLFILA01       -COPY WDR601                                          
037200*    05 -COPY W46341 -PRE EDI- -RED FIL-WDR601-DATA                       
037300                                                                          
037400 01  FILLER         PIC X(16)   VALUE 'DLI-IO-F411'.                      
037500 01  DLI-IO-F411.                                                         
037600*    03  -COPY WDF411                                                     
037700                                                                          
037800 01  FILLER         PIC X(16)   VALUE 'DLI-IO-WDB2'.                      
037900 01  DLI-IO-AREA-WDB201.                                                  
038000     03  WDB201.                                                          
038100*        05  -COPY WDB201                                                 
038200                                                                          
038300*    MSG-AREA FÖR HOPP TILL W20109                                        
038400 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
038500 01  W-PROG-TO-PROG-SW-1.                                                 
038600     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
038700     03  2109-Z1                   PIC X.                                 
038800     03  2109-Z2                   PIC X.                                 
038900     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
039000     03  2109-IDTRANS              PIC X(4)  VALUE '4360'.                
039100     03  2109-KDMFSFOR             PIC X.                                 
039200*    03  -COPY W2I10902    -PRE 2109-                                     
039300                                                                          
039400 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
039500                                                                          
039600*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
039700                                                                          
039800     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
039900                                                                          
040000*    --- COMMUNICATION AREAS (MAIL)                                       
040100*                                                                         
040200 01  HDR-AREA.                                                            
040300*    03  -COPY WZ01REQU                                                   
040400*    03  -COPY WZ04HDR                                                    
040500                                                                          
040600 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
040700 01  SEND-AREA.                                                           
040800*    03  -COPY WZ01SEND                                                   
040900                                                                          
041000 01  SEND-RAD                    PIC X(250)  VALUE SPACE.                 
041100                                                                          
041200 01  WS-RAD                      PIC X(250)  VALUE SPACE.                 
041300                                                                          
041400 01  FILLER                      PIC X(16)   VALUE 'OUTQ-AREA'.           
041500 01 OUTQ-AREA.                                                            
041600    03  OUTQ-CONTROL-AREA.                                                
041700        05  OUTQ-KDFUNC          PIC X(10).                               
041800        05  OUTQ-KDRC            PIC S9(9) COMP.                          
041900        05  OUTQ-IDCOM           PIC S9(9) COMP.                          
042000    03  OUTQ-OPEN-AREA.                                                   
042100        05  OUTQ-ADDISPABS       PIC X(50).                               
042200    03  OUTQ-PROPERTY-AREA.                                               
042300        05  OUTQ-PROPERTY-NAME   PIC X(100).                              
042400        05  OUTQ-PROPERTY-VALUE  PIC X(100).                              
042500    03  OUTQ-ADDITIONAL-INFO.                                             
042600        05  OUTQ-PHYSICALID      PIC X(100).                              
042700    03  OUTQ-KVDLEN              PIC S9(9) BINARY.                        
042800                                                                          
042900    03  OUTQ-DATA                PIC X(250)  VALUE SPACE.                 
043000                                                                          
043100 01  FILLER                      PIC X(16)   VALUE 'MAILAREA'.            
043200                                                                          
043300*    --- LISTLAYOUT                                                       
043400 01  F4-LARM.                                                             
043500     03  F4-RUBRIK1.                                                      
043600         05  FILLER              PIC X(26)   VALUE                        
043700            'VOLVO CAR CORP., WDF4 LARM'.                                 
043800         05  FILLER              PIC X(2)    VALUE SPACE.                 
043900         05  F4R1-DATUM          PIC 9(6).                                
044000                                                                          
044100     03  F4-RUBRIK2.                                                      
044200         05  FILLER              PIC X(31)   VALUE                        
044300            'FÖLJANDE RAD(ER) SAKNAS PÅ WDF4'.                            
044400                                                                          
044500     03  F4-KOMMENTAR-RAD.                                                
044600         05 F4-IDPGM             PIC X(8)    VALUE SPACE.                 
044700         05 FILLER               PIC X(2)    VALUE ': '.                  
044800         05 F4-KOMMENTAR         PIC X(25)   VALUE SPACE.                 
044900                                                                          
045000     03  F4-RAD.                                                          
045100         05  FILLER              PIC X(6)    VALUE 'LEVNR '.              
045200         05  FILLER              PIC X(1)    VALUE SPACE.                 
045300         05  F4-RAD-IDLEVNR      PIC X(5).                                
045400         05  FILLER              PIC X(9)    VALUE '  PRODNR '.           
045500         05  FILLER              PIC X(1)    VALUE SPACE.                 
045600         05  F4-RAD-IDPRODNR     PIC 9(7).                                
045700         05  FILLER              PIC X(6)    VALUE '  RAD '.              
045800         05  FILLER              PIC X(1)    VALUE SPACE.                 
045900         05  F4-RAD-IDPURAD      PIC 9(6).                                
046000         05  FILLER              PIC X(7)    VALUE '  UTSKR'.             
046100         05  FILLER              PIC X(1)    VALUE SPACE.                 
046200         05  F4-RAD-TIUTSKR      PIC 9(6).                                
046300                                                                          
046400                                                                          
046500                                                                          
046600 LINKAGE SECTION.                                                         
046700                                                                          
046800*01    -COPY W0009     -PRE MSG-                                          
046900                                                                          
047000*01    -COPY W0009     -PRE ALT-                                          
047100                                                                          
047200*01    -COPY W0009     -PRE 2109-                                         
047300                                                                          
047400*01    -COPY W0009     -PRE DISTRDOC-                                     
047500                                                                          
047600*01    -COPY W0008     -PRE WDF6-                                         
047700     05  FILLER                  PIC X.                                   
047800                                                                          
047900*01    -COPY W0008     -PRE WDE4-                                         
048000     05  FILLER                  PIC X.                                   
048100                                                                          
048200*01    -COPY W0008     -PRE WDE6-                                         
048300     05  FILLER                  PIC X.                                   
048400                                                                          
048500*01    -COPY W0008     -PRE ORQM-                                         
048600     05  FILLER                  PIC X.                                   
048700                                                                          
048800*01    -COPY W0008     -PRE ORQI-                                         
048900     05  FILLER                  PIC X.                                   
049000                                                                          
049100*01    -COPY W0008     -PRE ORQA-                                         
049200     05  FILLER                  PIC X.                                   
049300                                                                          
049400*01    -COPY W0008     -PRE ARTS-                                         
049500     05  FILLER                  PIC X.                                   
049600                                                                          
049700*01    -COPY W0008     -PRE ZZAC-                                         
049800     05  FILLER                  PIC X.                                   
049900                                                                          
050000*01    -COPY W0008     -PRE FILA-                                         
050100     05  FILLER                  PIC X.                                   
050200                                                                          
050300*01    -COPY W0008     -PRE WDF4-                                         
050400     05  FILLER                  PIC X.                                   
050500                                                                          
050600*01    -COPY W0008     -PRE WDK6-                                         
050700     05  FILLER                  PIC X.                                   
050800                                                                          
050900*01    -COPY W0008     -PRE WDB2-                                         
051000     05  FILLER                  PIC X.                                   
051100                                                                          
051200 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  2109-PCB                     
051300                           DISTRDOC-PCB                                   
051400                           WDF6-PCB                                       
051500                           WDE4-PCB WDE6-PCB                              
051600                           ORQM-PCB ORQI-PCB ORQA-PCB                     
051700                           ARTS-PCB                                       
051800                           ZZAC-PCB                                       
051900                           FILA-PCB WDF4-PCB WDK6-PCB                     
052000                           WDB2-PCB.                                      
052100 MAIN SECTION.                                                            
052200     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  2109-PCB                     
052300                           DISTRDOC-PCB                                   
052400                           WDF6-PCB                                       
052500                           WDE4-PCB WDE6-PCB                              
052600                           ORQM-PCB ORQI-PCB ORQA-PCB                     
052700                           ARTS-PCB                                       
052800                           ZZAC-PCB                                       
052900                           FILA-PCB WDF4-PCB WDK6-PCB                     
053000                           WDB2-PCB.                                      
053100                                                                          
053200     PERFORM IMS-GET-MSG                                                  
053300                                                                          
053400     IF SEGMENT-FINNS                                                     
053500       CALL W009WAIT USING WS-TIME-WAIT                                   
053600       PERFORM A-INIT                                                     
053700       PERFORM C-KONTROLLERA-ORDER                                        
053800       IF ORDER-FINNS                                                     
053900*        *THE ORDER EXISTS BUT IS NOT SENT TO SUPPLIER                    
054000         PERFORM D-ANNULLERA-ORDER                                        
054100         PERFORM E-RENSA-DIREKTLEVERANS                                   
054200       ELSE                                                               
054300*        *THE ORDER IS ALREADY SENT TO THE SUPPLIER                       
054400         PERFORM F-UPPDATERA-DIREKTLEVERANS                               
054500                                                                          
054600         PERFORM S01-OUTQ-OPEN                                            
054700         PERFORM B-SKAPA-MQ                                               
054800         PERFORM S03-OUTQ-CLOSE                                           
054900       END-IF                                                             
055000     END-IF                                                               
055100                                                                          
055200     PERFORM Z-FINIT                                                      
055300                                                                          
055400     MOVE ZERO         TO RETURN-CODE                                     
055500     GOBACK                                                               
055600     .                                                                    
055700                                                                          
055800                                                                          
055900 A-INIT SECTION.                                                          
056000     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
056100                                                                          
056200     ACCEPT WS-DAGENS-KLOCKA          FROM TIME                           
056300     MOVE WS-DAGENS-KLOCKA-1-6        TO WS-DAGENS-KLOCKA-ALFA-14         
056400     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-DAGENS-DATUM-8                
056500     MOVE WS-DAGENS-DATUM-8           TO WS-DAGENS-DATUM-ALFA-14          
056600     MOVE FUNCTION CURRENT-DATE(3:6)  TO WS-DAGENS-DATUM-6                
056700     MOVE WS-DAGENS-KLOCKA-1-6        TO WS-DAGENS-KLOCKA-NUM             
056800                                                                          
056900     MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I68001                     
057000     MOVE MSG-IDTRANS-1               TO MSG-SPAR-IDTRANS                 
057100     MOVE MSG-KDMFSFOR-1              TO MSG-SPAR-KDMFSFOR                
057200                                                                          
057300     MOVE MSG-KDTRTYP                 TO MSG-SPAR-KDTRTYP                 
057400     MOVE MSG-IDPFK                   TO MSG-SPAR-IDPFK                   
057500     MOVE MSG-SPAR-IDTRANS            TO W-IDTRANS                        
057600                                                                          
057700     MOVE LOW-VALUE                   TO MSG-AREA                         
057800     .                                                                    
057900                                                                          
058000 B-SKAPA-MQ  SECTION.                                                     
058100     MOVE 'B-SKAPA-MQ     ' TO CURRENT-SECTION                            
058200                                                                          
058300     PERFORM BA-BYGG-EDI-HEADER                                           
058400     PERFORM BB-BYGG-UNB                                                  
058500     PERFORM BC-BYGG-UNH                                                  
058600     PERFORM BD-BYGG-BGM                                                  
058700     PERFORM BE-BYGG-DTM                                                  
058800     PERFORM BF-BYGG-RFF                                                  
058900     PERFORM BG-BYGG-NAD-XX                                               
059000     PERFORM BH-BYGG-LIN                                                  
059100     PERFORM BI-BYGG-QTY                                                  
059200     PERFORM BJ-BYGG-UNS                                                  
059300     PERFORM BL-BYGG-EDI-TRAILER                                          
059400     PERFORM BM-BYGG-LOGG                                                 
059500     .                                                                    
059600                                                                          
059700                                                                          
059800 BA-BYGG-EDI-HEADER SECTION.                                              
059900     MOVE 'BA-BYGG-EDI-HEAD' TO CURRENT-SECTION                           
060000                                                                          
060100     MOVE SPACE                   TO WEDIH001                             
060200                                                                          
060300     MOVE '001'                   TO H001-IDPTYP                          
060400     MOVE 73                      TO H001-LENGTH                          
060500     MOVE 'WPAR'                  TO H001-SENDER-NODE                     
060600     MOVE 'VAMP'                  TO H001-RECEIVER-NODE                   
060700     MOVE 'ORDCH96B'              TO H001-FILE-NAME                       
060800     MOVE WS-DAGENS-DATUM-6       TO H001-DATE                            
060900     MOVE WS-DAGENS-KLOCKA-1-6    TO H001-TIME                            
061000                                                                          
061100                                                                          
061200     MOVE +79                     TO OUTQ-KVDLEN                          
061300     MOVE WEDIH001                TO WS-RAD                               
061400     PERFORM S02-OUTQ-PUT                                                 
061500     .                                                                    
061600                                                                          
061700                                                                          
061800 BB-BYGG-UNB SECTION.                                                     
061900     MOVE 'BB-BYGG-UNB     ' TO CURRENT-SECTION                           
062000                                                                          
062100     MOVE SPACE                   TO WEDIUNB                              
062200                                                                          
062300     MOVE 'UNB'                   TO UNB-IDPTYP                           
062400     MOVE 125                     TO UNB-LENGTH                           
062500     MOVE 'UNOA'                  TO UNB-SYNTAX-ID                        
062600     MOVE 1                       TO UNB-SYNTAX-VERS-NO                   
062700                                                                          
062800     MOVE MID-IDLEVNR             TO WS-IDLEVNR-KOLL                      
062900     INSPECT WS-IDLEVNR-KOLL      REPLACING ALL SPACE BY ZERO             
063000     IF WS-IDLEVNR-KOLL NUMERIC                                           
063100       MOVE '02796'               TO UNB-SENDER-ID                        
063200**FIX FÖR ATT HÖGERSTÄLLT NOLLUTFYLLT                                     
063300       UNSTRING MID-IDLEVNR DELIMITED BY SPACE                            
063400                                  INTO WS-UNB-RECIPIENT-ID                
063500       MOVE WS-UNB-RECIPIENT-ID-X TO UNB-RECIPIENT-ID                     
063600     ELSE                                                                 
063700       MOVE 'BP2T7'               TO UNB-SENDER-ID                        
063800       MOVE MID-IDLEVNR           TO UNB-RECIPIENT-ID                     
063900     END-IF                                                               
064000                                                                          
064100     MOVE 'VO'                    TO CIA-IDARTPRE-IN                      
064200     MOVE WS-DAGENS-DATUM-6       TO UNB-DATE                             
064300     MOVE WS-DAGENS-KLOCKA-1-4    TO UNB-TIME                             
064400     MOVE WS-DAGENS-DATUM-KLOCKA-14                                       
064500                                  TO UNB-INTERCHANGE-REF                  
064600                                                                          
064700     MOVE +131                    TO OUTQ-KVDLEN                          
064800     MOVE WEDIUNB                 TO WS-RAD                               
064900     PERFORM S02-OUTQ-PUT                                                 
065000     .                                                                    
065100                                                                          
065200                                                                          
065300 BC-BYGG-UNH SECTION.                                                     
065400     MOVE 'BC-BYGG-UNH     ' TO CURRENT-SECTION                           
065500     MOVE SPACE                 TO WEDIUNH                                
065600                                                                          
065700     MOVE 'UNH'                 TO UNH-IDPTYP                             
065800     MOVE 72                    TO UNH-LENGTH                             
065900     MOVE '1'                   TO UNH-REFNO                              
066000     MOVE 'ORDCHG'              TO UNH-TYPE                               
066100     MOVE 'D'                   TO UNH-VERSION                            
066200     MOVE '96B'                 TO UNH-RELEASE                            
066300     MOVE 'UN'                  TO UNH-CONTR-AGENCY                       
066400     MOVE 'A24010'              TO UNH-ASS-ASSIGN-CODE                    
066500     MOVE 'VO'                  TO CIA-IDARTPRE-IN                        
066600     MOVE +1                    TO CIA-IDARTBET-IN                        
066700     CALL W009CIA USING            CIA-W009CIA                            
066800     MOVE CIA-IDARTBET-UT       TO UNH-REFNO                              
066900                                                                          
067000     ADD +1                     TO WS-NO-OF-SEGM                          
067100                                                                          
067200     MOVE +78                   TO OUTQ-KVDLEN                            
067300     MOVE WEDIUNH               TO WS-RAD                                 
067400     PERFORM S02-OUTQ-PUT                                                 
067500     .                                                                    
067600                                                                          
067700                                                                          
067800 BD-BYGG-BGM SECTION.                                                     
067900     MOVE 'BD-BYGG-BGM     ' TO CURRENT-SECTION                           
068000                                                                          
068100     MOVE SPACE            TO WEDIBGM1                                    
068200                                                                          
068300     MOVE 'BGM'            TO BGM1-IDPTYP                                 
068400     MOVE 44               TO BGM1-LENGTH                                 
068500     MOVE '220'            TO BGM1-DOC-NAME-CODE                          
068600     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
068700     MOVE MID-IDPRODNR     TO CIA-IDARTBET-IN                             
068800                              EDI-IDPRODNR                                
068900     CALL W009CIA USING       CIA-W009CIA                                 
069000     MOVE CIA-IDARTBET-UT  TO BGM1-DOCNO                                  
069100     MOVE 'NA'             TO BGM1-RESPONSE-TYPE                          
069200     ADD +1                TO WS-NO-OF-SEGM                               
069300                                                                          
069400     MOVE +50              TO OUTQ-KVDLEN                                 
069500     MOVE WEDIBGM1         TO WS-RAD                                      
069600     PERFORM S02-OUTQ-PUT                                                 
069700     .                                                                    
069800                                                                          
069900                                                                          
070000 BE-BYGG-DTM SECTION.                                                     
070100     MOVE 'BE-BYGG-DTM     ' TO CURRENT-SECTION                           
070200                                                                          
070300     MOVE SPACE            TO WEDIDTM1                                    
070400                                                                          
070500     MOVE 'DTM'            TO DTM1-IDPTYP                                 
070600     MOVE 41               TO DTM1-LENGTH                                 
070700     MOVE '137'            TO DTM1-QUAL                                   
070800     MOVE MID-DABEKDAT     TO WS-DAGENS-DATUM-ALFA-14                     
070900                              EDI-DABEKDAT                                
071000     MOVE MID-TIBEKR       TO WS-DAGENS-KLOCKA-ALFA-14                    
071100                              EDI-TIBEKR                                  
071200     MOVE WS-DAGENS-DATUM-KLOCKA-14                                       
071300                           TO DTM1-DATE-TIME                              
071400     MOVE '204'            TO DTM1-FORMAT-QUAL                            
071500     ADD +1                TO WS-NO-OF-SEGM                               
071600                                                                          
071700     MOVE +47              TO OUTQ-KVDLEN                                 
071800     MOVE WEDIDTM1         TO WS-RAD                                      
071900     PERFORM S02-OUTQ-PUT                                                 
072000     .                                                                    
072100                                                                          
072200                                                                          
072300 BF-BYGG-RFF SECTION.                                                     
072400     MOVE 'BF-BYGG-RFF     ' TO CURRENT-SECTION                           
072500                                                                          
072600     MOVE SPACE            TO WEDIRFF1                                    
072700                                                                          
072800     MOVE 'RFF'            TO RFF1-IDPTYP                                 
072900     MOVE 38               TO RFF1-LENGTH                                 
073000     MOVE 'CR'             TO RFF1-QUAL                                   
073100     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
073200     MOVE MID-IDORDNR7     TO CIA-IDARTBET-IN                             
073300                              EDI-IDORDNR7                                
073400     CALL W009CIA USING       CIA-W009CIA                                 
073500     MOVE CIA-IDARTBET-UT  TO RFF1-REFNO                                  
073600     ADD +1                TO WS-NO-OF-SEGM                               
073700                                                                          
073800     MOVE +44              TO OUTQ-KVDLEN                                 
073900     MOVE WEDIRFF1         TO WS-RAD                                      
074000     PERFORM S02-OUTQ-PUT                                                 
074100     .                                                                    
074200                                                                          
074300                                                                          
074400 BG-BYGG-NAD-XX SECTION.                                                  
074500     MOVE 'BG-BYGG-NAD-XX  ' TO CURRENT-SECTION                           
074600                                                                          
074700     MOVE SPACE             TO WEDINAD1                                   
074800                                                                          
074900     MOVE 'NAD'             TO NAD1-IDPTYP                                
075000     MOVE 225               TO NAD1-LENGTH                                
075100     MOVE 'BY'              TO NAD1-QUAL                                  
075200                                                                          
075300     MOVE MID-IDLEVNR             TO WS-IDLEVNR-KOLL                      
075400     INSPECT WS-IDLEVNR-KOLL      REPLACING ALL SPACE BY ZERO             
075500     IF WS-IDLEVNR-KOLL NUMERIC                                           
075600       MOVE '02796'               TO NAD1-PARTY-ID                        
075700     ELSE                                                                 
075800       MOVE 'BP2T7'               TO NAD1-PARTY-ID                        
075900     END-IF                                                               
076000                                                                          
076100     MOVE '91'              TO NAD1-RESPONSIBLE                           
076200     MOVE 'VOLVO CAR CORP., CUSTOMER SERVICE'                             
076300                            TO NAD1-NAME-ADR-1                            
076400     MOVE MID-IDDC          TO NAD1-COUNTRY-CODE                          
076500                               EDI-IDDC                                   
076600     ADD +1                 TO WS-NO-OF-SEGM                              
076700                                                                          
076800     MOVE +231             TO OUTQ-KVDLEN                                 
076900     MOVE WEDINAD1         TO WS-RAD                                      
077000     PERFORM S02-OUTQ-PUT                                                 
077100                                                                          
077200     MOVE SPACE            TO WEDINAD1                                    
077300                                                                          
077400     MOVE 'NAD'            TO NAD1-IDPTYP                                 
077500     MOVE 225              TO NAD1-LENGTH                                 
077600     MOVE 'SE'             TO NAD1-QUAL                                   
077700     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
077800     MOVE MID-IDLEVNR      TO NAD1-PARTY-ID                               
077900                              EDI-IDLEVNR                                 
078000     MOVE '92'             TO NAD1-RESPONSIBLE                            
078100     ADD +1                TO WS-NO-OF-SEGM                               
078200                                                                          
078300     MOVE +231             TO OUTQ-KVDLEN                                 
078400     MOVE WEDINAD1         TO WS-RAD                                      
078500     PERFORM S02-OUTQ-PUT                                                 
078600                                                                          
078700     MOVE SPACE            TO WEDINAD1                                    
078800                                                                          
078900     MOVE 'NAD'            TO NAD1-IDPTYP                                 
079000     MOVE 225              TO NAD1-LENGTH                                 
079100     MOVE 'CN'             TO NAD1-QUAL                                   
079200     MOVE MID-IDDISTR      TO WS-IDDISTR                                  
079300                              EDI-IDDISTR                                 
079400                              TEST-IDDISTR                                
079500     MOVE MID-IDKUNDNR     TO WS-IDKUNDNR                                 
079600                              EDI-IDKUNDNR                                
079700     MOVE WS-IDDISTR-IDKUNDNR                                             
079800                           TO NAD1-PARTY-ID                               
079900     MOVE '92'             TO NAD1-RESPONSIBLE                            
080000     ADD +1                TO WS-NO-OF-SEGM                               
080100                                                                          
080200     MOVE +231             TO OUTQ-KVDLEN                                 
080300     MOVE WEDINAD1         TO WS-RAD                                      
080400     PERFORM S02-OUTQ-PUT                                                 
080500     .                                                                    
080600                                                                          
080700                                                                          
080800 BH-BYGG-LIN SECTION.                                                     
080900     MOVE 'BH-BYGG-LIN     ' TO CURRENT-SECTION                           
081000                                                                          
081100     MOVE SPACE            TO WEDILIN2                                    
081200                                                                          
081300     MOVE 'LIN'            TO LIN2-IDPTYP                                 
081400     MOVE 46               TO LIN2-LENGTH                                 
081500     MOVE MID-IDRADNR(1)   TO LIN2-LINENO                                 
081600                              EDI-IDRADNR                                 
081700     MOVE '2'              TO LIN2-ACTION-REQ                             
081800     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
081900     MOVE MID-IDARTNR(1)   TO CIA-IDARTBET-IN                             
082000                              EDI-IDARTNR                                 
082100     CALL W009CIA USING       CIA-W009CIA                                 
082200     MOVE CIA-IDARTBET-UT  TO LIN2-ITEMNO                                 
082300     MOVE 'IN'             TO LIN2-ITEMNO-TYPE                            
082400     MOVE ZERO             TO LIN2-CONFIG-LEVEL                           
082500     ADD +1                TO WS-NO-OF-SEGM                               
082600                                                                          
082700     MOVE +55              TO OUTQ-KVDLEN                                 
082800     MOVE WEDILIN2         TO WS-RAD                                      
082900     PERFORM S02-OUTQ-PUT                                                 
083000     .                                                                    
083100                                                                          
083200                                                                          
083300 BI-BYGG-QTY SECTION.                                                     
083400     MOVE 'BI-BYGG-QTY     ' TO CURRENT-SECTION                           
083500                                                                          
083600     MOVE SPACE            TO WEDIQTY1                                    
083700                                                                          
083800     MOVE 'QTY'            TO QTY1-IDPTYP                                 
083900     MOVE 22               TO QTY1-LENGTH                                 
084000     MOVE '21'             TO QTY1-QUALIFIER                              
084100     MOVE MID-KVBEART(1)   TO QTY1-QUANTITY                               
084200                              EDI-KVANTAL                                 
084300     MOVE 'PCE'            TO QTY1-UNIT-QUALIFIER                         
084400     ADD +1                TO WS-NO-OF-SEGM                               
084500                                                                          
084600     MOVE +28              TO OUTQ-KVDLEN                                 
084700     MOVE WEDIQTY1         TO WS-RAD                                      
084800     PERFORM S02-OUTQ-PUT                                                 
084900     .                                                                    
085000                                                                          
085100                                                                          
085200 BJ-BYGG-UNS SECTION.                                                     
085300     MOVE 'BJ-BYGG-UNS     ' TO CURRENT-SECTION                           
085400                                                                          
085500     MOVE SPACE            TO WEDIUNS1                                    
085600                                                                          
085700     MOVE 'UNS'            TO UNS1-IDPTYP                                 
085800     MOVE 1                TO UNS1-LENGTH                                 
085900     MOVE 'S'              TO UNS1-SECTION-ID                             
086000     ADD +1                TO WS-NO-OF-SEGM                               
086100                                                                          
086200     MOVE +7               TO OUTQ-KVDLEN                                 
086300     MOVE WEDIUNS1         TO WS-RAD                                      
086400     PERFORM S02-OUTQ-PUT                                                 
086500     .                                                                    
086600                                                                          
086700                                                                          
086800 BL-BYGG-EDI-TRAILER SECTION.                                             
086900     MOVE 'BL-BYGG-EDI-TRAI' TO CURRENT-SECTION                           
087000                                                                          
087100     MOVE SPACE                   TO WEDIT003                             
087200                                                                          
087300     MOVE '003'                   TO T003-IDPTYP                          
087400     MOVE 73                      TO T003-LENGTH                          
087500                                                                          
087600     MOVE +79                     TO OUTQ-KVDLEN                          
087700     MOVE WEDIT003                TO WS-RAD                               
087800     PERFORM S02-OUTQ-PUT                                                 
087900                                                                          
088000     .                                                                    
088100                                                                          
088200                                                                          
088300 BM-BYGG-LOGG SECTION.                                                    
088400     MOVE 'BM-BYGG-LOGG    ' TO CURRENT-SECTION                           
088500                                                                          
088600     MOVE 'W463'                      TO FIL-CT-IDSYSTEM                  
088700     MOVE 'CHG'                       TO FIL-CT-IDPTYP                    
088800     MOVE ' '                         TO FIL-CT-IDVTYP                    
088900                                                                          
089000     MOVE 'W4068000'                  TO FIL-IDPGM                        
089100     MOVE FUNCTION CURRENT-DATE (3:6) TO FIL-TIREGDAT                     
089200     MOVE FUNCTION CURRENT-DATE (1:8) TO EDI-DAREGDAT                     
089300     ACCEPT FIL-TIKLOCK               FROM TIME                           
089400     MOVE FIL-TIKLOCK                 TO EDI-TIREGTID                     
089500     MOVE 1                           TO FIL-IDSEKVNR                     
089600     MOVE 'CHG'                       TO EDI-IDPTYP                       
089700     MOVE ZERO                        TO EDI-IDKOLLI                      
089800                                         EDI-DAFAKT                       
089900                                         EDI-DALEVDAT                     
090000                                         EDI-DAPACKN                      
090100                                         EDI-DASKEPPN                     
090200                                         EDI-DASNDDAT                     
090300                                         EDI-DASUPREF                     
090400                                         EDI-TIPACTID                     
090500                                         EDI-TISNDTID                     
090600                                         EDI-TISUPTID                     
090700                                         EDI-KDORDBEK                     
090800                                         EDI-KDORDKL                      
090900     MOVE SPACE                       TO EDI-IDSUPREF                     
091000                                         EDI-BERADREF                     
091100                                         EDI-KDVIA                        
091200                                                                          
091300     PERFORM IMS-ISRT-LOGG                                                
091400     PERFORM UNTIL SEGMENT-FINNS                                          
091500       ADD +1  TO FIL-IDSEKVNR                                            
091600       PERFORM IMS-ISRT-LOGG                                              
091700     END-PERFORM                                                          
091800     .                                                                    
091900                                                                          
092000                                                                          
092100 C-KONTROLLERA-ORDER SECTION.                                             
092200     MOVE 'C-KOLL-ORDER    ' TO CURRENT-SECTION                           
092300                                                                          
092400     MOVE NEJ                     TO SW-ORDER-FINNS                       
092500                                     SW-DELETE-PA-WDF6                    
092600                                                                          
092700     MOVE MID-IDPRODNR            TO W-IDPRODNR                           
092800     MOVE MID-IDRADNR(1)          TO W-IDPURAD                            
092900                                                                          
093000     PERFORM IMS-GHU-WDF601                                               
093100     IF SEGMENT-FINNS                                                     
093200        PERFORM IMS-GHNP-WDF611                                           
093300        IF SEGMENT-FINNS                                                  
093400           MOVE JA                TO SW-DELETE-PA-WDF6                    
093500           PERFORM IMS-DLET-WDF611                                        
093600           MOVE JA                TO SW-ORDER-FINNS                       
093700           PERFORM IMS-GNP-WDF611-FIRST                                   
093800           IF SEGMENT-SAKNAS                                              
093900              PERFORM IMS-GHU-WDF601                                      
094000              PERFORM IMS-DLET-WDF601                                     
094100           END-IF                                                         
094200        END-IF                                                            
094300     END-IF                                                               
094400     .                                                                    
094500                                                                          
094600                                                                          
094700 D-ANNULLERA-ORDER SECTION.                                               
094800     MOVE 'D-ANNULL-ORDER  ' TO CURRENT-SECTION                           
094900                                                                          
095000     MOVE MID-IDDISTR                TO W-401-IDDISTR                     
095100     MOVE MID-IDKUNDNR               TO W-401-IDKUNDNR                    
095200     MOVE MID-IDORDNR7(3:5)          TO WS-IDORDNR5                       
095300     MOVE WS-IDORDNR5                TO W-401-IDORDNR                     
095400     MOVE MID-IDPRODNR               TO W-401-IDPRODNR                    
095500                                        W-601-IDPRODNR                    
095600     MOVE MID-IDPLKLST               TO W-401-IDPLKLST                    
095700     MOVE MID-IDRADNR (1)            TO W-411-IDPURAD                     
095800                                                                          
095900                                                                          
096000     PERFORM IMS-GHU-WDE401                                               
096100                                                                          
096200     MOVE KORD-IDORDER               TO W-IDORDER                         
096300                                                                          
096400     PERFORM IMS-GHU-WDE411                                               
096500     PERFORM IMS-GHU-WDQ201                                               
096600                                                                          
096700     PERFORM DA-UPPDATERA-E4-RAD                                          
096800     PERFORM DB-SKAPA-ANNULLATION                                         
096900     PERFORM DC-SKAPA-ORDERBEK-83                                         
097000                                                                          
097100     PERFORM DD-UPPDATERA-E6                                              
097200     PERFORM DE-UPPDATERA-E4-HUVUD                                        
097300     PERFORM DF-EV-UPPDATERA-K7                                           
097400                                                                          
097500     IF VORD-KVORDRAD = VORD-KVORDRAD-PACK                                
097600       PERFORM DG-UPPDATERA-Q3                                            
097700       PERFORM DH-UPPDATERA-Q2                                            
097800     END-IF                                                               
097900                                                                          
098000     IF 2109-IX > ZERO                                                    
098100       COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17              
098200       PERFORM IMS-PURG-MSG-2109                                          
098300     END-IF                                                               
098400     .                                                                    
098500                                                                          
098600                                                                          
098700 DA-UPPDATERA-E4-RAD SECTION.                                             
098800     MOVE 'DA-UPD-E4-RAD   ' TO CURRENT-SECTION                           
098900                                                                          
099000     MOVE ORAD-KVBEART       TO ORAD-KVANNANT                             
099100     MOVE ZERO               TO ORAD-KVAVBART                             
099200     MOVE +4                 TO ORAD-KDRADSTA                             
099300     MOVE '3'                TO ORAD-KDANNULL                             
099400     ADD +1                  TO WS-ANTAL-ANNULL-RADER                     
099500                                                                          
099600     PERFORM DAA-SPAR-UPPGIFTER                                           
099700                                                                          
099800     PERFORM IMS-REPL-WDE411                                              
099900     .                                                                    
100000                                                                          
100100                                                                          
100200 DAA-SPAR-UPPGIFTER SECTION.                                              
100300     MOVE 'DAA-SPAR-UPPGIFT' TO CURRENT-SECTION                           
100400                                                                          
100500     COMPUTE SPAR-VKORDNTO ROUNDED = SPAR-VKORDNTO +                      
100600                 (ORAD-VKARTNTO * ORAD-KVANNANT)                          
100700                                                                          
100800     COMPUTE SPAR-VLORDNTO ROUNDED = SPAR-VLORDNTO +                      
100900                 (ORAD-VLARTNTO * ORAD-KVANNANT / 1000000)                
101000                                                                          
101100     COMPUTE SPAR-SUORDV-LEVPL ROUNDED = SPAR-SUORDV-LEVPL +              
101200                 (ORAD-PRARTNTO * ORAD-KVANNANT)                          
101300                                                                          
101400     COMPUTE SPAR-SUORDV-LEVPL-LOC ROUNDED =                              
101500                               SPAR-SUORDV-LEVPL-LOC +                    
101600                 (ORAD-PRARTNTO-LOC * ORAD-KVANNANT)                      
101700                                                                          
101800     COMPUTE SPAR-SUORDV-LEVPL-LOCPREL ROUNDED =                          
101900                               SPAR-SUORDV-LEVPL-LOCPREL +                
102000                 (ORAD-PRARTNTO-LOCPREL * ORAD-KVANNANT)                  
102100     .                                                                    
102200                                                                          
102300                                                                          
102400 DB-SKAPA-ANNULLATION SECTION.                                            
102500     MOVE 'DB-SKAPA-ANNULL ' TO CURRENT-SECTION                           
102600                                                                          
102700     PERFORM DBA-GENERERA-ANNULL-TRANS                                    
102800                                                                          
102900     IF ORAD-KDOI NOT = SPACE                                             
103000       PERFORM DBB-GENERERA-2109-TRANS                                    
103100     END-IF                                                               
103200     .                                                                    
103300                                                                          
103400                                                                          
103500 DBA-GENERERA-ANNULL-TRANS SECTION.                                       
103600     MOVE 'DBA-GEN-ANNULL  ' TO CURRENT-SECTION                           
103700                                                                          
103800     ACCEPT LOGG-TIAAMMDD       FROM DATE                                 
103900     ACCEPT LOGG-TIKLOCK        FROM TIME                                 
104000     ADD +1                     TO  LOGG-IDLOGLOP                         
104100     MOVE 'RY5'                 TO  RY5-IDPTYP                            
104200                                    LOGG-IDPTYP                           
104300     MOVE ORAD-BERADREF         TO  RY5-BERADREF                          
104400     MOVE ORAD-BEVOLREF         TO  RY5-BEVOLREF                          
104500     MOVE KORD-IDKUNDRF         TO  RY5-IDKUNDRF                          
104600     MOVE ORAD-IDARTNR          TO  RY5-IDARTNR                           
104700     MOVE ORAD-FLRESTN          TO  RY5-FLRESTN                           
104800     MOVE ORAD-FLDIRLEV         TO  RY5-FLDIRLEV                          
104900     MOVE KORD-FLLSBOK          TO  RY5-FLLSBOK                           
105000     MOVE KORD-FLORDSPE         TO  RY5-FLORDSPE                          
105100     MOVE ORAD-IDKUNDRF-RO      TO  RY5-IDKUNDRF-RO                       
105200                                                                          
105300     IF ORAD-FLTILLK = JA                                                 
105400         MOVE 1                 TO  RY5-KDARTERS                          
105500     ELSE                                                                 
105600         MOVE ZERO              TO  RY5-KDARTERS                          
105700     END-IF                                                               
105800                                                                          
105900     MOVE KORD-IDDC             TO  RY5-IDDC                              
106000     MOVE ORAD-KDDSP            TO  RY5-KDDSP                             
106100     MOVE KORD-KDFAKTYP         TO  RY5-KDFAKTYP                          
106200     MOVE ORAD-KDFRAKT          TO  RY5-KDFRAKT                           
106300     MOVE ORAD-KDORDING         TO  RY5-KDORDING                          
106400     MOVE ORAD-KDORDKL          TO  RY5-KDORDKL                           
106500                                    RY5-KDORDKL-URS                       
106600     MOVE ORAD-KDORDTYP         TO  RY5-KDORDTYP                          
106700     MOVE ORAD-KDKVBRYT         TO  RY5-KDKVBRYT                          
106800     MOVE ORAD-KDVRINFO         TO  RY5-KDVRINFO                          
106900     MOVE ORAD-KVBEART          TO  RY5-KVBEART                           
107000     MOVE ORAD-KVAVBART         TO  RY5-KVAVBART                          
107100     MOVE ORAD-KVANNANT         TO  RY5-KVANNANT                          
107200                                    RY5-KVAVART                           
107300     MOVE ORAD-REKSIFFR         TO  RY5-REKSIFFR                          
107400     MOVE ORAD-TIUTSKR          TO  RY5-TIORDREG                          
107500     MOVE ORAD-TIRODAT          TO  RY5-TIRODAT                           
107600     MOVE RY5-WDGZRY5           TO  LOGG-LOGGPOST                         
107700     MOVE KORD-IDDISTR          TO  RY5S-IDDISTR                          
107800     MOVE KORD-IDKUNDNR         TO  RY5S-IDKUNDNR                         
107900     IF  OHUV-FLVORKO = JA                                                
108000     OR  OHUV-FLVORKO = YES                                               
108100         MOVE JA                TO  RY5S-FLVORKO                          
108200     ELSE                                                                 
108300         MOVE OHUV-FLVORKO      TO  RY5S-FLVORKO                          
108400     END-IF                                                               
108500     MOVE OHUV-FLFORBI          TO  RY5S-FLFORBI                          
108600     MOVE OHUV-FLOVRLEV         TO  RY5S-FLOVRLEV                         
108700     MOVE ORAD-IDSYSTEM         TO  RY5S-IDSYSTEM                         
108800     MOVE ORAD-KVSLATT          TO  RY5S-KVSLATT                          
108900     MOVE ORAD-KDPRODSL         TO  RY5S-KDPRODSL                         
109000     MOVE SPACE                 TO  RY5S-FILLERX5                         
109100                                    RY5S-FILLERX10                        
109200     MOVE ZERO                  TO  RY5S-KDTPOTYP                         
109300     MOVE RY5S-WDGZRY5S-CTX     TO  LOGG-SORTPOST                         
109400                                                                          
109500     PERFORM IMS-ISRT-ZZAC01                                              
109600     PERFORM UNTIL SEGMENT-FINNS                                          
109700       IF LOGG-IDLOGLOP = 9                                               
109800         MOVE ZERO              TO LOGG-IDLOGLOP                          
109900         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
110000       END-IF                                                             
110100       ADD +1                   TO LOGG-IDLOGLOP                          
110200       PERFORM IMS-ISRT-ZZAC01                                            
110300     END-PERFORM                                                          
110400     .                                                                    
110500                                                                          
110600                                                                          
110700 DBB-GENERERA-2109-TRANS SECTION.                                         
110800     MOVE 'DBB-GEN-2109-TR ' TO CURRENT-SECTION                           
110900                                                                          
111000*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
111100     MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                             
111200     IF NOT BYT03-OBJEKT                                                  
111300                                                                          
111400       ADD +1                   TO 2109-IX                                
111500       MOVE 2109-IX             TO 2109-MID2-KVANTART                     
111600       MOVE ORAD-IDARTNR        TO 2109-MID2-IDARTNR (2109-IX)            
111700       MOVE OHUV-IDDC-PRIM      TO 2109-MID2-IDDC (2109-IX)               
111800       MOVE ORAD-KDOI           TO 2109-MID2-KDOI (2109-IX)               
111900       MOVE ORAD-CLEARGROUP     TO 2109-MID2-CLEARGROUP(2109-IX)          
112000       MOVE '-'                 TO 2109-MID2-KDTECKEN (2109-IX)           
112100       MOVE ORAD-KVANNANT       TO 2109-MID2-KVOI (2109-IX)               
112200       MOVE KORD-TIORDREG        TO 2109-MID2-TIUPPDAT (2109-IX)          
112300                                                                          
112400       IF 2109-IX = 2109-IX-MAX                                           
112500         PERFORM DBBA-STARTA-2109                                         
112600       END-IF                                                             
112700     END-IF                                                               
112800     .                                                                    
112900                                                                          
113000                                                                          
113100 DBBA-STARTA-2109 SECTION.                                                
113200     MOVE 'DBBA-STARTA-2109' TO CURRENT-SECTION                           
113300                                                                          
113400     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
113500                                                                          
113600     PERFORM IMS-PURG-MSG-2109                                            
113700                                                                          
113800     MOVE SPACE              TO 2109-MID2-W2I10902                        
113900     MOVE +0                 TO 2109-IX                                   
114000     .                                                                    
114100                                                                          
114200                                                                          
114300 DC-SKAPA-ORDERBEK-83 SECTION.                                            
114400     MOVE 'DC-SKAPA-OBKR   ' TO CURRENT-SECTION                           
114500                                                                          
114600     MOVE KORD-IDORDER                    TO OBKR-IDORDER                 
114700     MOVE ORAD-IDARTNR                    TO OBKR-IDARTNR                 
114800     MOVE 1                               TO OBKR-IDLOPNR                 
114900     MOVE 1                               TO OBKR-IDSEKVNR                
115000     MOVE KORD-IDDC                       TO OBKR-IDDC                    
115100     MOVE 83                              TO OBKR-KDORDBEK                
115200     MOVE SPACE                           TO OBKR-BEERS                   
115300     MOVE OHUV-BEKUNDRF                   TO OBKR-BEKUNDRF                
115400     MOVE ORAD-BERADREF                   TO OBKR-BERADREF                
115500     MOVE ORAD-BEVOLREF                   TO OBKR-BEVOLREF                
115600     MOVE ORAD-IDKAMPRF                   TO OBKR-IDKAMPRF                
115700     MOVE IDPGM                           TO OBKR-IDPGM                   
115800     MOVE 0                               TO OBKR-DIERS-KVOT              
115900     MOVE NEJ                             TO OBKR-FLAKPLOC                
116000     MOVE NEJ                             TO OBKR-FLSLATT                 
116100     MOVE ORAD-FLINVEST                   TO OBKR-FLINVEST                
116200     MOVE JA                              TO OBKR-FLOBOK                  
116300     MOVE NEJ                             TO OBKR-FLOBTRAN                
116400     MOVE NEJ                             TO OBKR-FLOBPRT                 
116500     MOVE ORAD-FLPRTILL                   TO OBKR-FLPRTILL                
116600     MOVE ORAD-FLRESTN                    TO OBKR-FLRESTN                 
116700     MOVE NEJ                             TO OBKR-FLTILLK                 
116800     MOVE 0                               TO OBKR-IDARTNR-TILLK           
116900     MOVE ORAD-IDDC-RO                    TO OBKR-IDDC-RO                 
117000     MOVE KORD-IDDISTR                    TO OBKR-IDDISTR                 
117100     MOVE KORD-IDKUNDNR                   TO OBKR-IDKUNDNR                
117200                                                                          
117300     MOVE KORD-IDKUNDRF                   TO WS-IDKUNDRF-OLD              
117400     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
117500     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF                
117600                                                                          
117700     MOVE ORAD-IDKUNDRF-RO                TO WS-IDKUNDRF-OLD              
117800     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
117900     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF-RO             
118000                                                                          
118100     MOVE ORAD-IDLEVNR                    TO OBKR-IDLEVNR                 
118200     MOVE ORAD-IDLOPNR-RO                 TO OBKR-IDLOPNR-RO              
118300     MOVE ORAD-IDSYSTEM                   TO OBKR-IDSYSTEM                
118400     MOVE ORAD-KDDSP                      TO OBKR-KDDSP                   
118500     MOVE 0                               TO OBKR-KDERS                   
118600     MOVE ORAD-KDOI                       TO OBKR-KDOI                    
118700     MOVE ORAD-CLEARGROUP                 TO OBKR-CLEARGROUP              
118800     MOVE ORAD-KDKVBRYT                   TO OBKR-KDKVBRYT                
118900     MOVE ORAD-KDPRTYP                    TO OBKR-KDPRTYP                 
119000     MOVE 0                               TO OBKR-KDTPOTYP                
119100     MOVE ORAD-KDVRINFO                   TO OBKR-KDVRINFO                
119200                                                                          
119300     MOVE ORAD-KVANNANT                   TO OBKR-KVANNANT                
119400     MOVE ORAD-KVAVBART                   TO OBKR-KVAVBART                
119500     MOVE ORAD-KVBEART                    TO OBKR-KVBEART                 
119600                                             OBKR-KVBEART-Q               
119700     MOVE 0                               TO OBKR-KVBEART-TILLK           
119800     MOVE 0                               TO OBKR-KVPREAVB                
119900     MOVE 0                               TO OBKR-KVPRERO                 
120000     MOVE ZERO                            TO OBKR-KVQPACK                 
120100     MOVE 0                               TO OBKR-KVRO                    
120200     MOVE ZERO                            TO OBKR-TIRODAT                 
120300     MOVE ORAD-KVSLATT                    TO OBKR-KVSLATT                 
120400     MOVE ORAD-PRARTNTO                   TO OBKR-PRARTNTO                
120500     MOVE ORAD-PRARTNTO-LOC               TO OBKR-PRARTNTO-LOC            
120600     MOVE ORAD-PRARTNTO-LOCPREL           TO OBKR-PRARTNTO-LOCPREL        
120700     MOVE ORAD-PRARTBTO-LOC               TO OBKR-PRARTBTO-LOC            
120800     MOVE ORAD-IDPRQUES                   TO OBKR-IDPRQUES                
120900     MOVE ORAD-KDVALISO                   TO OBKR-KDVALISO                
121000     MOVE ORAD-KDVAT                      TO OBKR-KDVAT                   
121100     MOVE ORAD-RERAB                      TO OBKR-RERAB                   
121200     MOVE ORAD-KDRAB                      TO OBKR-KDRAB                   
121300     MOVE ORAD-BEART-VIPS                 TO OBKR-BEART-VIPS              
121400     MOVE 0                               TO OBKR-PRBPRIS                 
121500     MOVE ORAD-REKSIFFR                   TO OBKR-REKSIFFR                
121600     MOVE 0                               TO OBKR-REKSIFFR-TILLK          
121700     MOVE 0                               TO OBKR-RERF-RAD                
121800     MOVE +0                              TO OBKR-TIDISPIN                
121900     MOVE KORD-TIORDREG                   TO OBKR-TIORDREG                
122000                                             WS-DATUM-9KOMPL              
122100     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
122200     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
122300     END-COMPUTE                                                          
122400     MOVE ORAD-TIPRIS                     TO OBKR-TIPRIS                  
122500                                                                          
122600     MOVE WS-DAGENS-DATUM-6               TO OBKR-TIREGDAT                
122700     MOVE WS-DAGENS-KLOCKA-NUM            TO OBKR-TIREGTID                
122800     MOVE KORD-TIORDREG                   TO WS-DATUM-9KOMPL              
122900     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
123000                                                                          
123100     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
123200     END-COMPUTE                                                          
123300     MOVE 0                               TO OBKR-TITPO                   
123400     MOVE ORAD-KDFRAKT                    TO OBKR-KDFRAKT                 
123500     MOVE ORAD-KDORDKL                    TO OBKR-KDORDKL                 
123600     MOVE ORAD-IDBIL                      TO OBKR-IDBIL                   
123700                                                                          
123800     MOVE OHUV-KDORDTYP-LDC               TO OBKR-KDORDTYP-LDC            
123900     MOVE OHUV-TIREPDAT                   TO OBKR-TIREPDAT                
124000     MOVE ORAD-IDKUNDRF-WIP               TO OBKR-IDKUNDRF-WIP            
124100     MOVE ZERO                            TO OBKR-TIDLEVDAT               
124200     MOVE ORAD-PRAVCOST                   TO OBKR-PRAVCOST                
124300                                                                          
124400     PERFORM IMS-ISRT-WDQ101                                              
124500     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
124600        ADD +1                            TO OBKR-IDLOPNR                 
124700        PERFORM IMS-ISRT-WDQ101                                           
124800     END-PERFORM                                                          
124900     .                                                                    
125000                                                                          
125100                                                                          
125200 DD-UPPDATERA-E6 SECTION.                                                 
125300     MOVE 'DD-UPPDATERA-E6 ' TO CURRENT-SECTION                           
125400                                                                          
125500     PERFORM IMS-GHU-WDE601                                               
125600                                                                          
125700     ADD WS-ANTAL-ANNULL-RADER   TO VORD-KVORDRAD-PACK                    
125800     SUBTRACT SPAR-SUORDV-LEVPL  FROM VORD-SUORDV                         
125900     SUBTRACT SPAR-SUORDV-LEVPL-LOC  FROM VORD-SUORDV-LOC                 
126000     SUBTRACT SPAR-SUORDV-LEVPL-LOCPREL  FROM VORD-SUORDV-LOCPREL         
126100     SUBTRACT SPAR-VKORDNTO      FROM VORD-VKORDNTO                       
126200     SUBTRACT SPAR-VLORDNTO      FROM VORD-VLORDNTO                       
126300                                                                          
126400     IF VORD-KVORDRAD-PACK = VORD-KVORDRAD                                
126500       IF VORD-KVORDRAD = +1                                              
126600         MOVE WS-DAGENS-DATUM-6  TO VORD-TIPACKN-SK                       
126700         MOVE +5                 TO VORD-KDORDSTA                         
126800       ELSE                                                               
126900         IF VORD-KVKOLLI = VORD-KVKOLLI-FAKT AND                          
127000            VORD-KVKOLLI = VORD-KVKOLLI-LAST                              
127100           MOVE +5               TO VORD-KDORDSTA                         
127200         ELSE                                                             
127300           IF VORD-KVKOLLI-FL = VORD-KVKOLLI                              
127400             MOVE +4               TO VORD-KDORDSTA                       
127500           ELSE                                                           
127600             MOVE +3               TO VORD-KDORDSTA                       
127700           END-IF                                                         
127800         END-IF                                                           
127900       END-IF                                                             
128000     END-IF                                                               
128100                                                                          
128200     PERFORM IMS-REPL-WDE601                                              
128300     .                                                                    
128400                                                                          
128500                                                                          
128600 DE-UPPDATERA-E4-HUVUD SECTION.                                           
128700     MOVE 'DE-UPD-E4-HUVUD ' TO CURRENT-SECTION                           
128800                                                                          
128900     PERFORM IMS-GHU-WDE401                                               
129000     MOVE VORD-VKORDNTO             TO  KORD-VKORDNTO                     
129100     MOVE VORD-VLORDNTO             TO  KORD-VLORDNTO                     
129200     MOVE VORD-SUORDV               TO  KORD-SUORDV-LEVPL                 
129300     MOVE VORD-SUORDV-LOC           TO  KORD-SUORDV-LEVPL-LOC             
129400     MOVE VORD-SUORDV-LOCPREL       TO  KORD-SUORDV-LEVPL-LOCPREL         
129500     MOVE VORD-KVORDRAD-PACK        TO  KORD-KVORDRAD-PACK                
129600                                                                          
129700     PERFORM IMS-REPL-WDE401                                              
129800     .                                                                    
129900                                                                          
130000                                                                          
130100 DF-EV-UPPDATERA-K7 SECTION.                                              
130200     MOVE 'DF-EV-UPD-K7    ' TO CURRENT-SECTION                           
130300                                                                          
130400******************************************************************        
130500*                                                                         
130600*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
130700*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
130800*                                                                         
130900******************************************************************        
131000                                                                          
131100     MOVE KORD-IDDISTR        TO W-TP4TRAN-IDDISTR                        
131200                                                                          
131300     PERFORM DB2-SELECT-TP4TRAN                                           
131400                                                                          
131500     MOVE KORD-IDDISTR        TO TEST-IDDISTR                             
131600     IF  DIST35-REFILL                                                    
131700     OR  DIST35-REFILL-INOM-NDC                                           
131800     OR  DIST35-NONVCC-NONVCC-REFILL                                      
131900     OR  DIST35-NONVCC-NONVCC-TRANSFER                                    
132000     OR  DIST35-NA-TRANSFER                                               
132100     OR  DIST35-NA-NDC-RETURNS                                            
132200     OR  DIST35-PACIFIC-TRANSFER                                          
132300     OR  DIST35-REFILL-INOM-JP                                            
132400     OR  DIST35-NONVCC-VCC-REFILL                                         
132500     OR  DIST35-NONVCC-VCC-TRANSFER                                       
132600     OR  RADER-FINNS                                                      
132700                                                                          
132800       IF RADER-FINNS                                                     
132900         MOVE TP4TRAN-IDDC-REC  TO W-WDK711-IDDC                          
133000       ELSE                                                               
133100         SEARCH ALL DIST57-REFILL-DC                                      
133200            AT END                                                        
133300               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
133400                                TO FELTEXT                                
133500               CALL FELLOG                                                
133600            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
133700               MOVE DIST57-REFILL-TO-DC(DIST57-IX)                        
133800                                TO W-WDK711-IDDC                          
133900         END-SEARCH                                                       
134000       END-IF                                                             
134100                                                                          
134200       MOVE ORAD-IDARTNR      TO W-WDK701-IDARTNR-N                       
134300       PERFORM IMS-GHU-WDK711                                             
134400       SUBTRACT ORAD-KVBEART  FROM SLAG-KVBEART                           
134500                                                                          
134600       PERFORM IMS-REPL-WDK711                                            
134700     ELSE                                                                 
134800       IF DIST35-NONVCC-CDC-REFILL                                        
134900                                                                          
135000          SEARCH ALL DIST57-REFILL-DC                                     
135100             AT END                                                       
135200                MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                      
135300                                 TO FELTEXT                               
135400                CALL FELLOG                                               
135500             WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR            
135600                MOVE DIST57-REFILL-TO-DC(DIST57-IX)                       
135700                                 TO W-WDK711-IDDC                         
135800          END-SEARCH                                                      
135900                                                                          
136000          MOVE ORAD-IDARTNR    TO W-WDK601-IDARTNR-N                      
136100          PERFORM IMS-GHU-WDK611                                          
136200          SUBTRACT ORAD-KVBEART FROM CLAG-KVBEART                         
136300                                                                          
136400          PERFORM IMS-REPL-WDK611                                         
136500       END-IF                                                             
136600     END-IF                                                               
136700     .                                                                    
136800                                                                          
136900                                                                          
137000 DG-UPPDATERA-Q3 SECTION.                                                 
137100     MOVE 'DG-UPPDATERA-Q3 ' TO CURRENT-SECTION                           
137200                                                                          
137300     MOVE KORD-IDORDER               TO W-Q301-IDORDER                    
137400     MOVE KORD-IDDC                  TO W-Q301-IDDC                       
137500     MOVE KORD-IDPRODNR              TO W-Q301-IDPRODNR                   
137600     MOVE KORD-IDPLKLST              TO W-Q301-IDPLKLST                   
137700     PERFORM IMS-GHU-WDQ301                                               
137800                                                                          
137900     MOVE KORD-KVORDRAD-PACK         TO ODEL-KVPACKRAD-OD                 
138000     MOVE 'P'                        TO ODEL-KDODELSTA                    
138100     MOVE WS-DAGENS-DATUM-6          TO ODEL-TIPACKN                      
138200     MOVE WS-DAGENS-KLOCKA-1-6       TO ODEL-TIPACTID                     
138300     PERFORM IMS-REPL-WDQ301                                              
138400     .                                                                    
138500                                                                          
138600                                                                          
138700 DH-UPPDATERA-Q2 SECTION.                                                 
138800     MOVE 'DH-UPPDATERA-Q2 ' TO CURRENT-SECTION                           
138900                                                                          
139000     MOVE ORAD-IDLEVNR              TO W-211-IDLEVNR                      
139100     MOVE KORD-IDDC                 TO W-211-IDDC                         
139200     PERFORM IMS-GHNP-WDQ211                                              
139300                                                                          
139400     MOVE VORD-VKORDNTO             TO DIRL-VKORDNTO                      
139500     MOVE VORD-VLORDNTO             TO DIRL-VLORDNTO                      
139600     MOVE VORD-SUORDV               TO DIRL-SUORDV                        
139700     MOVE VORD-SUORDV-LOC           TO DIRL-SUORDV-LOC                    
139800     MOVE VORD-SUORDV-LOCPREL       TO DIRL-SUORDV-LOCPREL                
139900*    MOVE VORD-KVORDRAD-PACK        TO DIRL-KVRADER                       
140000     MOVE 'P'                       TO DIRL-KDORDSTA                      
140100     PERFORM IMS-REPL-WDQ211                                              
140200     .                                                                    
140300                                                                          
140400                                                                          
140500 E-RENSA-DIREKTLEVERANS SECTION.                                          
140600     MOVE 'E-RENSA-DIRLEV  ' TO CURRENT-SECTION                           
140700                                                                          
140800     IF SW-DELETE-PA-WDF6 = NEJ                                           
140900        MOVE ORAD-IDLEVNR    TO W-IDLEVNR-F4                              
141000        MOVE ORAD-IDPRODNR   TO W-IDPRODNR-F4-MIN                         
141100                                W-IDPRODNR-F4-MAX                         
141200        MOVE ORAD-IDPURAD    TO W-IDPURAD-F4-MIN                          
141300                                W-IDPURAD-F4-MAX                          
141400*    OM VI HAR FÖRDRÖJDA DDGS-ORDER STÄMMER INTE                          
141500*    ORDERRADENS TIUTSKR(ORDEREG-DATUM)                                   
141600*    MED DATUM PÅ WDF4(DATUM NÄR ORDEN SKICKAS TILL LEV.)                 
141700*    MOVE ORAD-TIUTSKR       TO W-TIUTSKR-F4                              
141800                                                                          
141900        PERFORM IMS-GHU-WDF411                                            
142000        IF SEGMENT-FINNS                                                  
142100           PERFORM IMS-DLET-WDF411                                        
142200        ELSE                                                              
142300           MOVE 'ANNULLERAD RAD' TO F4-KOMMENTAR                          
142400           PERFORM S10-GENERERA-LARM-MAIL                                 
142500        END-IF                                                            
142600     END-IF                                                               
142700     .                                                                    
142800                                                                          
142900                                                                          
143000 F-UPPDATERA-DIREKTLEVERANS SECTION.                                      
143100     MOVE 'F-UPPD-DIRLEV   ' TO CURRENT-SECTION                           
143200                                                                          
143300     MOVE MID-IDDISTR                TO W-401-IDDISTR                     
143400     MOVE MID-IDKUNDNR               TO W-401-IDKUNDNR                    
143500     MOVE MID-IDORDNR7(3:5)          TO WS-IDORDNR5                       
143600     MOVE WS-IDORDNR5                TO W-401-IDORDNR                     
143700     MOVE MID-IDPRODNR               TO W-401-IDPRODNR                    
143800     MOVE MID-IDPLKLST               TO W-401-IDPLKLST                    
143900     MOVE MID-IDRADNR (1)            TO W-411-IDPURAD                     
144000                                                                          
144100     PERFORM IMS-GU-WDE401                                                
144200     PERFORM IMS-GU-WDE411                                                
144300                                                                          
144400     MOVE ORAD-IDLEVNR       TO W-IDLEVNR-F4                              
144500     MOVE ORAD-IDPRODNR      TO W-IDPRODNR-F4-MIN                         
144600                                W-IDPRODNR-F4-MAX                         
144700     MOVE ORAD-IDPURAD       TO W-IDPURAD-F4-MIN                          
144800                                W-IDPURAD-F4-MAX                          
144900*    OM VI HAR FÖRDRÖJDA DDGS-ORDER STÄMMER INTE                          
145000*    ORDERRADENS TIUTSKR(ORDEREG-DATUM)                                   
145100*    MED DATUM PÅ WDF4(DATUM NÄR ORDEN SKICKAS TILL LEV.)                 
145200*    MOVE ORAD-TIUTSKR       TO W-TIUTSKR-F4                              
145300                                                                          
145400     PERFORM IMS-GHU-WDF411                                               
145500     MOVE WS-DAGENS-DATUM-6  TO DLOR-TIANNULL                             
145600     MOVE '1'                TO DLOR-KDANNULL                             
145700     IF SEGMENT-FINNS AND DLOR-TIPACKN = 0                                
145800        PERFORM IMS-REPL-WDF411                                           
145900        PERFORM FA-CREATE-ORDCONF-84                                      
146000        IF OHUV-IDSYSTEM = ('TACD' OR 'LDC')                              
146100        AND GMT-FLOBKR-TACD = JA                                          
146200          PERFORM FB-SEND-84-TO-TACDIS                                    
146300        END-IF                                                            
146400     ELSE                                                                 
146500        MOVE 'ANNULLATION BEGÄRD' TO F4-KOMMENTAR                         
146600        PERFORM S10-GENERERA-LARM-MAIL                                    
146700     END-IF                                                               
146800     .                                                                    
146900                                                                          
147000 FA-CREATE-ORDCONF-84 SECTION.                                            
147100     MOVE 'FA-CREATE-ORDERC' TO CURRENT-SECTION                           
147200                                                                          
147300     MOVE KORD-IDORDER                    TO W-IDORDER                    
147400     PERFORM IMS-GU-WDQ201                                                
147500                                                                          
147600     MOVE OHUV-IDDISTR                    TO W-IDDISTR                    
147700     MOVE OHUV-IDKUNDNR                   TO W-IDKUNDNR                   
147800     PERFORM IMS-GU-WDB201                                                
147900                                                                          
148000     MOVE OHUV-IDORDER                    TO OBKR-IDORDER                 
148100     MOVE ORAD-IDARTNR                    TO OBKR-IDARTNR                 
148200     MOVE 1                               TO OBKR-IDLOPNR                 
148300     MOVE 1                               TO OBKR-IDSEKVNR                
148400     MOVE KORD-IDDC                       TO OBKR-IDDC                    
148500     MOVE 84                              TO OBKR-KDORDBEK                
148600     MOVE SPACE                           TO OBKR-BEERS                   
148700     MOVE OHUV-BEKUNDRF                   TO OBKR-BEKUNDRF                
148800     MOVE ORAD-BERADREF                   TO OBKR-BERADREF                
148900     MOVE ORAD-BEVOLREF                   TO OBKR-BEVOLREF                
149000     MOVE ORAD-IDKAMPRF                   TO OBKR-IDKAMPRF                
149100     MOVE IDPGM                           TO OBKR-IDPGM                   
149200     MOVE 0                               TO OBKR-DIERS-KVOT              
149300     MOVE NEJ                             TO OBKR-FLAKPLOC                
149400     MOVE NEJ                             TO OBKR-FLSLATT                 
149500     MOVE ORAD-FLINVEST                   TO OBKR-FLINVEST                
149600     MOVE JA                              TO OBKR-FLOBOK                  
149700     MOVE NEJ                             TO OBKR-FLOBTRAN                
149800     MOVE NEJ                             TO OBKR-FLOBPRT                 
149900     MOVE ORAD-FLPRTILL                   TO OBKR-FLPRTILL                
150000     MOVE ORAD-FLRESTN                    TO OBKR-FLRESTN                 
150100     MOVE NEJ                             TO OBKR-FLTILLK                 
150200     MOVE 0                               TO OBKR-IDARTNR-TILLK           
150300     MOVE ORAD-IDDC-RO                    TO OBKR-IDDC-RO                 
150400     MOVE KORD-IDDISTR                    TO OBKR-IDDISTR                 
150500     MOVE KORD-IDKUNDNR                   TO OBKR-IDKUNDNR                
150600                                                                          
150700     MOVE KORD-IDKUNDRF                   TO WS-IDKUNDRF-OLD              
150800     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
150900     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF                
151000                                                                          
151100     MOVE ORAD-IDKUNDRF-RO                TO WS-IDKUNDRF-OLD              
151200     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
151300     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF-RO             
151400                                                                          
151500     MOVE ORAD-IDLEVNR                    TO OBKR-IDLEVNR                 
151600     MOVE ORAD-IDLOPNR-RO                 TO OBKR-IDLOPNR-RO              
151700     MOVE ORAD-IDSYSTEM                   TO OBKR-IDSYSTEM                
151800     MOVE ORAD-KDDSP                      TO OBKR-KDDSP                   
151900     MOVE 0                               TO OBKR-KDERS                   
152000     MOVE ORAD-KDOI                       TO OBKR-KDOI                    
152100     MOVE ORAD-CLEARGROUP                 TO OBKR-CLEARGROUP              
152200     MOVE ORAD-KDKVBRYT                   TO OBKR-KDKVBRYT                
152300     MOVE ORAD-KDPRTYP                    TO OBKR-KDPRTYP                 
152400     MOVE 0                               TO OBKR-KDTPOTYP                
152500     MOVE ORAD-KDVRINFO                   TO OBKR-KDVRINFO                
152600                                                                          
152700     MOVE ORAD-KVANNANT                   TO OBKR-KVANNANT                
152800     MOVE ORAD-KVAVBART                   TO OBKR-KVAVBART                
152900     MOVE ORAD-KVBEART                    TO OBKR-KVBEART                 
153000                                             OBKR-KVBEART-Q               
153100     MOVE 0                               TO OBKR-KVBEART-TILLK           
153200     MOVE 0                               TO OBKR-KVPREAVB                
153300     MOVE 0                               TO OBKR-KVPRERO                 
153400     MOVE ZERO                            TO OBKR-KVQPACK                 
153500     MOVE 0                               TO OBKR-KVRO                    
153600     MOVE ZERO                            TO OBKR-TIRODAT                 
153700     MOVE ORAD-KVSLATT                    TO OBKR-KVSLATT                 
153800     MOVE ORAD-PRARTNTO                   TO OBKR-PRARTNTO                
153900     MOVE ORAD-PRARTNTO-LOC               TO OBKR-PRARTNTO-LOC            
154000     MOVE ORAD-PRARTNTO-LOCPREL           TO OBKR-PRARTNTO-LOCPREL        
154100     MOVE ORAD-PRARTBTO-LOC               TO OBKR-PRARTBTO-LOC            
154200     MOVE ORAD-IDPRQUES                   TO OBKR-IDPRQUES                
154300     MOVE ORAD-KDVALISO                   TO OBKR-KDVALISO                
154400     MOVE ORAD-KDVAT                      TO OBKR-KDVAT                   
154500     MOVE ORAD-RERAB                      TO OBKR-RERAB                   
154600     MOVE ORAD-KDRAB                      TO OBKR-KDRAB                   
154700     MOVE ORAD-BEART-VIPS                 TO OBKR-BEART-VIPS              
154800     MOVE 0                               TO OBKR-PRBPRIS                 
154900     MOVE ORAD-REKSIFFR                   TO OBKR-REKSIFFR                
155000     MOVE 0                               TO OBKR-REKSIFFR-TILLK          
155100     MOVE 0                               TO OBKR-RERF-RAD                
155200     MOVE +0                              TO OBKR-TIDISPIN                
155300     MOVE KORD-TIORDREG                   TO OBKR-TIORDREG                
155400                                             WS-DATUM-9KOMPL              
155500     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
155600     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
155700     END-COMPUTE                                                          
155800     MOVE ORAD-TIPRIS                     TO OBKR-TIPRIS                  
155900                                                                          
156000     MOVE WS-DAGENS-DATUM-6               TO OBKR-TIREGDAT                
156100     MOVE WS-DAGENS-KLOCKA-NUM            TO OBKR-TIREGTID                
156200     MOVE KORD-TIORDREG                   TO WS-DATUM-9KOMPL              
156300     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
156400                                                                          
156500     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
156600     END-COMPUTE                                                          
156700     MOVE 0                               TO OBKR-TITPO                   
156800     MOVE ORAD-KDFRAKT                    TO OBKR-KDFRAKT                 
156900     MOVE ORAD-KDORDKL                    TO OBKR-KDORDKL                 
157000     MOVE ORAD-IDBIL                      TO OBKR-IDBIL                   
157100                                                                          
157200     MOVE OHUV-KDORDTYP-LDC               TO OBKR-KDORDTYP-LDC            
157300     MOVE OHUV-TIREPDAT                   TO OBKR-TIREPDAT                
157400     MOVE ORAD-IDKUNDRF-WIP               TO OBKR-IDKUNDRF-WIP            
157500     MOVE ZERO                            TO OBKR-TIDLEVDAT               
157600     MOVE ORAD-PRAVCOST                   TO OBKR-PRAVCOST                
157700                                                                          
157800     PERFORM IMS-ISRT-WDQ101                                              
157900     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
158000        ADD +1                            TO OBKR-IDLOPNR                 
158100        PERFORM IMS-ISRT-WDQ101                                           
158200     END-PERFORM                                                          
158300     .                                                                    
158400                                                                          
158500 FB-SEND-84-TO-TACDIS SECTION.                                            
158600                                                                          
158700     MOVE 'PU1'                  TO 402-IDPTYP                            
158800     MOVE 01                     TO 402-IDVTYP-TACDIS                     
158900     MOVE WS-DAGENS-DATUM-8      TO 402-DAREGDAT                          
159000     MOVE OHUV-IDDISTR           TO 402-IDDISTR                           
159100     MOVE OHUV-IDKUNDNR          TO 402-IDKUNDNR                          
159200     MOVE OHUV-IDORDNR7          TO 402-IDORDNR7                          
159300                                                                          
159400     MOVE 'VO '                  TO CIA-IDARTPRE-IN                       
159500     MOVE MID-IDARTNR(1)         TO CIA-IDARTBET-IN                       
159600     CALL W009CIA             USING CIA-W009CIA                           
159700     MOVE CIA-IDARTBET-UT        TO 402-IDARTBET                          
159800                                                                          
159900     MOVE 84                     TO 402-KDORDBEK                          
160000     MOVE 0                      TO 402-KVBEART                           
160100     MOVE +1                     TO 402-IDSEKVNR                          
160200                                                                          
160300     MOVE SPACE                  TO 402-IDARTBET-TILLK                    
160400     MOVE ZERO                   TO 402-KVLEVART                          
160500     MOVE SPACE                  TO 402-IDDC                              
160600     MOVE ZERO                   TO 402-DADLEVDAT                         
160700                                                                          
160800     PERFORM S90-SEND-OPEN                                                
160900     MOVE OHUV-IDKUNDNR          TO WS-IDKUNDNR                           
161000     MOVE OHUV-IDORDNR7          TO WS-IDORDNR7                           
161100                                                                          
161200     PERFORM S90-PUT-DAP-START-84                                         
161300     PERFORM S90-PUT-DOC-LINE-84                                          
161400     PERFORM S90-SEND-CLOSE                                               
161500                                                                          
161600     .                                                                    
161700     EJECT                                                                
161800                                                                          
161900                                                                          
162000 Z-FINIT SECTION.                                                         
162100     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
162200                                                                          
162300     .                                                                    
162400                                                                          
162500 S01-OUTQ-OPEN SECTION.                                                   
162600     MOVE 'S01-OUTQ-OPEN           '      TO CURRENT-SECTION              
162700                                                                          
162800     MOVE 'OPENONL'              TO OUTQ-KDFUNC                           
162900     MOVE 'CARPARTS.EDI.ORDERCANCELLATION'                                
163000                                 TO OUTQ-ADDISPABS                        
163100     MOVE ZERO                   TO OUTQ-KVDLEN                           
163200     CALL WZ11OUTQ            USING OUTQ-AREA                             
163300                                                                          
163400     IF OUTQ-KDRC > 0                                                     
163500       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
163600       STRING 'WZ11OUTQ OPEN ERROR RC = ' KDRC-DISPLAY                    
163700         DELIMITED BY SIZE     INTO FELTEXT                               
163800       CALL ABEND             USING RKOD-ABEND-MED-DUMP                   
163900     END-IF                                                               
164000     PERFORM S04-SET-PROP                                                 
164100     .                                                                    
164200                                                                          
164300 S02-OUTQ-PUT      SECTION.                                               
164400                                                                          
164500     MOVE 'PUT'                  TO OUTQ-KDFUNC                           
164600                                                                          
164700*    MOVE FUNCTION DISPLAY-OF (                                           
164800*         FUNCTION NATIONAL-OF (WS-RAD, 278)                              
164900*                               , 1047)                                   
165000     MOVE WS-RAD                 TO OUTQ-DATA                             
165100                                                                          
165200     CALL WZ11OUTQ            USING OUTQ-AREA                             
165300     IF OUTQ-KDRC > 0                                                     
165400       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
165500       STRING 'WZ11OUTQ PUT ERROR RC = ' KDRC-DISPLAY                     
165600         DELIMITED BY SIZE     INTO FELTEXT                               
165700       CALL ABEND             USING RKOD-ABEND-MED-DUMP                   
165800     END-IF                                                               
165900     .                                                                    
166000                                                                          
166100 S03-OUTQ-CLOSE SECTION.                                                  
166200     MOVE 'S03-OUTQ-CLOSE          '      TO CURRENT-SECTION              
166300                                                                          
166400     MOVE 'CLOSE'                TO OUTQ-KDFUNC                           
166500     MOVE ZERO                   TO OUTQ-KVDLEN                           
166600     CALL WZ11OUTQ            USING OUTQ-AREA                             
166700                                                                          
166800     IF OUTQ-KDRC > 0                                                     
166900       MOVE OUTQ-KDRC            TO KDRC-DISPLAY                          
167000       STRING 'WZ11OUTQ CLOSE ERROR RC = ' KDRC-DISPLAY                   
167100         DELIMITED BY SIZE     INTO FELTEXT                               
167200       CALL ABEND             USING RKOD-ABEND-MED-DUMP                   
167300     END-IF                                                               
167400     .                                                                    
167500                                                                          
167600 S04-SET-PROP SECTION.                                                    
167700     MOVE 'S04-SET-PROP   '      TO CURRENT-SECTION                       
167800                                                                          
167900     MOVE ZERO                   TO OUTQ-KVDLEN                           
168000     MOVE 'SETPROP'              TO OUTQ-KDFUNC                           
168100     MOVE 'SenderId'             TO OUTQ-PROPERTY-NAME                    
168200     MOVE 'BP2T7'                TO OUTQ-PROPERTY-VALUE                   
168300     CALL WZ11OUTQ            USING OUTQ-AREA                             
168400                                                                          
168500     IF OUTQ-KDRC > 0                                                     
168600        MOVE OUTQ-KDRC           TO KDRC-DISPLAY                          
168700        STRING 'WZ11OUTQ INQPROP ERROR RC = ' KDRC-DISPLAY                
168800          DELIMITED BY SIZE    INTO FELTEXT                               
168900        CALL ABEND                                                        
169000     END-IF                                                               
169100     .                                                                    
169200                                                                          
169300 S10-GENERERA-LARM-MAIL SECTION.                                          
169400     MOVE 'S10-LARM-MAIL   ' TO CURRENT-SECTION                           
169500                                                                          
169600     IF FIRST-MAIL-RAD                                                    
169700        PERFORM S90-SEND-OPEN                                             
169800        MOVE JA TO SW-MAIL-SKICKAT                                        
169900        PERFORM S90-PUT-DAP-START-WDF4                                    
170000        PERFORM S11-REDIGERA-RUBRIK                                       
170100        PERFORM S12-REDIGERA-KOMMENTAR                                    
170200     END-IF                                                               
170300                                                                          
170400     PERFORM S13-REDIGERA-RAD                                             
170500     PERFORM S90-SEND-CLOSE                                               
170600     .                                                                    
170700                                                                          
170800                                                                          
170900 S11-REDIGERA-RUBRIK    SECTION.                                          
171000     MOVE 'S11-REIGERA-RUBR' TO CURRENT-SECTION                           
171100                                                                          
171200                                                                          
171300     MOVE WS-DAGENS-DATUM-6 TO F4R1-DATUM                                 
171400     MOVE F4-RUBRIK1        TO SEND-RAD                                   
171500     PERFORM S90-PUT-DOC-LINE                                             
171600                                                                          
171700     MOVE SPACE             TO SEND-RAD                                   
171800     PERFORM S90-PUT-DOC-LINE                                             
171900                                                                          
172000     MOVE SPACE             TO SEND-RAD                                   
172100     PERFORM S90-PUT-DOC-LINE                                             
172200                                                                          
172300     MOVE F4-RUBRIK2        TO SEND-RAD                                   
172400     PERFORM S90-PUT-DOC-LINE                                             
172500                                                                          
172600     MOVE SPACE             TO SEND-RAD                                   
172700     PERFORM S90-PUT-DOC-LINE                                             
172800                                                                          
172900     .                                                                    
173000                                                                          
173100                                                                          
173200 S12-REDIGERA-KOMMENTAR SECTION.                                          
173300     MOVE 'S12-RED-KOMMENT ' TO CURRENT-SECTION                           
173400                                                                          
173500     MOVE IDPGM                    TO F4-IDPGM                            
173600                                                                          
173700     MOVE F4-KOMMENTAR-RAD         TO SEND-RAD                            
173800     PERFORM S90-PUT-DOC-LINE                                             
173900     .                                                                    
174000                                                                          
174100                                                                          
174200 S13-REDIGERA-RAD       SECTION.                                          
174300     MOVE 'S13-REIGERA-RAD ' TO CURRENT-SECTION                           
174400                                                                          
174500     MOVE W-IDLEVNR-F4             TO F4-RAD-IDLEVNR                      
174600     MOVE W-IDPRODNR-F4-MIN        TO F4-RAD-IDPRODNR                     
174700     MOVE W-IDPURAD-F4-MIN         TO F4-RAD-IDPURAD                      
174800     MOVE ORAD-TIUTSKR             TO F4-RAD-TIUTSKR                      
174900                                                                          
175000     MOVE F4-RAD                   TO SEND-RAD                            
175100     PERFORM S90-PUT-DOC-LINE                                             
175200     .                                                                    
175300                                                                          
175400                                                                          
175500 S90-SEND-OPEN SECTION.                                                   
175600     MOVE 'S90-SEND-OPEN'           TO CURRENT-SECTION.                   
175700                                                                          
175800     MOVE 'OPEN'                    TO SEND-KDFUNC                        
175900     MOVE 'CARPARTS.DAP.DISTRDOC'   TO SEND-ADDISPABS                     
176000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
176100                         SEND-OPEN-AREA                                   
176200     IF SEND-KDRC > 0                                                     
176300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
176400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
176500       DELIMITED BY SIZE INTO FELTEXT                                     
176600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
176700     END-IF                                                               
176800     MOVE SEND-IDCOM             TO WS-SAVE-IDCOM-TACDIS-ALARM            
176900     .                                                                    
177000                                                                          
177100                                                                          
177200 S90-PUT-DAP-START-WDF4 SECTION.                                          
177300     MOVE 'S90-PUT-DAP-START-WDF4' TO CURRENT-SECTION.                    
177400                                                                          
177500     MOVE 1                       TO REQU-IDMSGVER                        
177600     MOVE 'R'                     TO REQU-KDPGMACT                        
177700     MOVE IDPGM                   TO REQU-IDUSER                          
177800     MOVE 'WDF4'                  TO HDR-IDOUTTYPE                        
177900     MOVE SPACE                   TO HDR-IDOUTREC                         
178000                                     HDR-IDLIST                           
178100     MOVE 'LARM'                  TO HDR-IDOUTREC (1:4)                   
178200                                     HDR-IDLIST                           
178300     MOVE 'PUT'                   TO SEND-KDFUNC                          
178400     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
178500     MOVE WS-SAVE-IDCOM-TACDIS-ALARM                                      
178600                                  TO SEND-IDCOM                           
178700                                                                          
178800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
178900                         SEND-KVDLEN                                      
179000                         HDR-AREA                                         
179100     IF SEND-KDRC > ZERO                                                  
179200       MOVE SEND-KDRC               TO KDRC-DISPLAY                       
179300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
179400       DELIMITED BY SIZE          INTO FELTEXT-STR                        
179500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
179600     END-IF                                                               
179700     .                                                                    
179800                                                                          
179900                                                                          
180000 S90-PUT-DAP-START-84 SECTION.                                            
180100     MOVE 'S90-PUT-DAP-START-84' TO CURRENT-SECTION.                      
180200                                                                          
180300     MOVE 1                       TO REQU-IDMSGVER                        
180400     MOVE 'R'                     TO REQU-KDPGMACT                        
180500     MOVE IDPGM                   TO REQU-IDUSER                          
180600     MOVE 'ORDERCONF'             TO HDR-IDOUTTYPE                        
180700     MOVE WS-IDKUNDNR             TO HDR-IDOUTREC                         
180800     MOVE WS-IDORDNR7             TO HDR-IDLIST                           
180900     MOVE 'PUT'                   TO SEND-KDFUNC                          
181000     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
181100     MOVE WS-SAVE-IDCOM-TACDIS-ALARM                                      
181200                                  TO SEND-IDCOM                           
181300                                                                          
181400                                                                          
181500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
181600                         SEND-KVDLEN                                      
181700                         HDR-AREA                                         
181800     IF SEND-KDRC > ZERO                                                  
181900       MOVE SEND-KDRC               TO KDRC-DISPLAY                       
182000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
182100       DELIMITED BY SIZE          INTO FELTEXT-STR                        
182200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
182300     END-IF                                                               
182400     .                                                                    
182500                                                                          
182600                                                                          
182700 S90-PUT-DOC-LINE SECTION.                                                
182800     MOVE 'S90-PUT-DOC-LINE' TO CURRENT-SECTION.                          
182900                                                                          
183000     MOVE 'PUT'                           TO SEND-KDFUNC                  
183100     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
183200     MOVE WS-SAVE-IDCOM-TACDIS-ALARM      TO SEND-IDCOM                   
183300                                                                          
183400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
183500                         SEND-KVDLEN                                      
183600                         SEND-RAD                                         
183700     IF SEND-KDRC > ZERO                                                  
183800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
183900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
184000       DELIMITED BY SIZE INTO FELTEXT-STR                                 
184100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
184200     END-IF                                                               
184300     .                                                                    
184400                                                                          
184500                                                                          
184600 S90-PUT-DOC-LINE-84 SECTION.                                             
184700     MOVE 'S90-PUT-DOC-84  ' TO CURRENT-SECTION.                          
184800                                                                          
184900     MOVE 'PUT'                           TO SEND-KDFUNC                  
185000     MOVE LENGTH OF 402-W402TACD          TO SEND-KVDLEN                  
185100     MOVE WS-SAVE-IDCOM-TACDIS-ALARM      TO SEND-IDCOM                   
185200                                                                          
185300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
185400                         SEND-KVDLEN                                      
185500                         402-W402TACD                                     
185600     IF SEND-KDRC > ZERO                                                  
185700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
185800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
185900       DELIMITED BY SIZE INTO FELTEXT-STR                                 
186000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
186100     END-IF                                                               
186200     .                                                                    
186300                                                                          
186400                                                                          
186500 S90-SEND-CLOSE SECTION.                                                  
186600     MOVE 'S90-SEND-CLOSE' TO CURRENT-SECTION.                            
186700                                                                          
186800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
186900     MOVE WS-SAVE-IDCOM-TACDIS-ALARM TO SEND-IDCOM                        
187000                                                                          
187100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
187200                                                                          
187300     IF SEND-KDRC > 0                                                     
187400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
187500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
187600       DELIMITED BY SIZE INTO FELTEXT                                     
187700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
187800     END-IF                                                               
187900     .                                                                    
188000* --- IMS SEKTIONER ---                                                   
188100                                                                          
188200 IMS-GET-MSG SECTION.                                                     
188300     MOVE '  QC' TO GODK-STATUSKODER                                      
188400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
188500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
188600     PERFORM IMS-STATUSKONTROLL                                           
188700     .                                                                    
188800                                                                          
188900                                                                          
189000 IMS-PURG-MSG-2109 SECTION.                                               
189100     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
189200     MOVE '  '  TO GODK-STATUSKODER                                       
189300     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
189400     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
189500     PERFORM IMS-STATUSKONTROLL                                           
189600     .                                                                    
189700                                                                          
189800                                                                          
189900 IMS-GHU-WDF601 SECTION.                                                  
190000     MOVE 'IMS-GHU-WDF601  ' TO CURRENT-IMS-SECTION                       
190100                                                                          
190200     MOVE SPACE                  TO ALL-SSA                               
190300     STRING 'WDF601  (IDPRODNR =' W-IDPRODNR-X ')'                        
190400             DELIMITED BY SIZE INTO SSA1                                  
190500     MOVE '  GE'                 TO GODK-STATUSKODER                      
190600     CALL CBLTDLI USING GHU WDF6-PCB DLI-IO-WDF601 SSA1                   
190700     MOVE WDF6-STATUS-CODE       TO STATUS-WS                             
190800     PERFORM IMS-STATUSKONTROLL                                           
190900     .                                                                    
191000                                                                          
191100 IMS-DLET-WDF601 SECTION.                                                 
191200     MOVE 'IMS-DLET-WDF601 ' TO CURRENT-IMS-SECTION                       
191300                                                                          
191400     MOVE SPACE                  TO ALL-SSA                               
191500     MOVE '  ' TO GODK-STATUSKODER                                        
191600     CALL CBLTDLI USING DLET WDF6-PCB DLI-IO-WDF601                       
191700     MOVE WDF6-STATUS-CODE TO STATUS-WS                                   
191800     PERFORM IMS-STATUSKONTROLL                                           
191900     .                                                                    
192000                                                                          
192100 IMS-GHNP-WDF611 SECTION.                                                 
192200     MOVE 'IMS-GHNP-WDF611 ' TO CURRENT-IMS-SECTION                       
192300                                                                          
192400     MOVE SPACE                  TO ALL-SSA                               
192500     STRING 'WDF611  (IDPURAD  =' W-IDPURAD-X ')'                         
192600             DELIMITED BY SIZE INTO SSA1                                  
192700     MOVE '  GE'                 TO GODK-STATUSKODER                      
192800     CALL CBLTDLI USING GHNP WDF6-PCB DLI-IO-WDF611 SSA1                  
192900     MOVE WDF6-STATUS-CODE       TO STATUS-WS                             
193000     PERFORM IMS-STATUSKONTROLL                                           
193100     .                                                                    
193200                                                                          
193300 IMS-GNP-WDF611-FIRST SECTION.                                            
193400     MOVE 'IMS-GNP-WDF611-F' TO CURRENT-IMS-SECTION                       
193500                                                                          
193600     MOVE SPACE                  TO ALL-SSA                               
193700     MOVE 'WDF611  *F'           TO SSA1                                  
193800     MOVE '  GE'                 TO GODK-STATUSKODER                      
193900     CALL CBLTDLI USING GNP WDF6-PCB DLI-IO-WDF611 SSA1                   
194000     MOVE WDF6-STATUS-CODE       TO STATUS-WS                             
194100     PERFORM IMS-STATUSKONTROLL                                           
194200     .                                                                    
194300                                                                          
194400 IMS-DLET-WDF611 SECTION.                                                 
194500     MOVE 'IMS-DLET-WDF611 ' TO CURRENT-IMS-SECTION                       
194600                                                                          
194700     MOVE SPACE                  TO ALL-SSA                               
194800     MOVE '  ' TO GODK-STATUSKODER                                        
194900     CALL CBLTDLI USING DLET WDF6-PCB DLI-IO-WDF611                       
195000     MOVE WDF6-STATUS-CODE TO STATUS-WS                                   
195100     PERFORM IMS-STATUSKONTROLL                                           
195200     .                                                                    
195300                                                                          
195400 IMS-GHU-WDE401 SECTION.                                                  
195500     MOVE 'IMS-GHU-WDE401  ' TO CURRENT-IMS-SECTION                       
195600                                                                          
195700     MOVE SPACE                 TO ALL-SSA                                
195800     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
195900            DELIMITED BY SIZE INTO SSA1                                   
196000     MOVE '    '                TO GODK-STATUSKODER                       
196100     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-E401 SSA1                  
196200     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
196300     PERFORM IMS-STATUSKONTROLL                                           
196400     .                                                                    
196500                                                                          
196600                                                                          
196700 IMS-REPL-WDE401 SECTION.                                                 
196800     MOVE 'IMS-REPL-WDE401 ' TO CURRENT-IMS-SECTION                       
196900                                                                          
197000     MOVE SPACE              TO ALL-SSA                                   
197100     MOVE '  '               TO GODK-STATUSKODER                          
197200     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E401                         
197300     MOVE WDE4-STATUS-CODE   TO STATUS-WS                                 
197400     PERFORM IMS-STATUSKONTROLL                                           
197500     .                                                                    
197600                                                                          
197700                                                                          
197800 IMS-GHU-WDE411 SECTION.                                                  
197900     MOVE 'IMS-GHU-WDE411  ' TO CURRENT-IMS-SECTION                       
198000                                                                          
198100     MOVE SPACE                 TO ALL-SSA                                
198200     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
198300            DELIMITED BY SIZE INTO SSA1                                   
198400     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
198500            DELIMITED BY SIZE INTO SSA2                                   
198600     MOVE '    '                TO GODK-STATUSKODER                       
198700     CALL CBLTDLI USING GHU   WDE4-PCB DLI-IO-E411 SSA1 SSA2              
198800     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
198900     PERFORM IMS-STATUSKONTROLL                                           
199000     .                                                                    
199100                                                                          
199200                                                                          
199300 IMS-GU-WDE401 SECTION.                                                   
199400     MOVE 'IMS-GU-WDE401  ' TO CURRENT-IMS-SECTION                        
199500                                                                          
199600     MOVE SPACE                 TO ALL-SSA                                
199700     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
199800            DELIMITED BY SIZE INTO SSA1                                   
199900     MOVE '    '                TO GODK-STATUSKODER                       
200000     CALL CBLTDLI USING GU     WDE4-PCB DLI-IO-E401 SSA1                  
200100     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
200200     PERFORM IMS-STATUSKONTROLL                                           
200300     .                                                                    
200400                                                                          
200500                                                                          
200600 IMS-GU-WDE411 SECTION.                                                   
200700     MOVE 'IMS-GU-WDE411   ' TO CURRENT-IMS-SECTION                       
200800                                                                          
200900     MOVE SPACE                 TO ALL-SSA                                
201000     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
201100            DELIMITED BY SIZE INTO SSA1                                   
201200     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
201300            DELIMITED BY SIZE INTO SSA2                                   
201400     MOVE '    '                TO GODK-STATUSKODER                       
201500     CALL CBLTDLI USING GU    WDE4-PCB DLI-IO-E411 SSA1 SSA2              
201600     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
201700     PERFORM IMS-STATUSKONTROLL                                           
201800     .                                                                    
201900                                                                          
202000                                                                          
202100 IMS-REPL-WDE411 SECTION.                                                 
202200     MOVE 'IMS-REPL-WDE411 ' TO CURRENT-IMS-SECTION                       
202300                                                                          
202400     MOVE SPACE              TO ALL-SSA                                   
202500     MOVE '    '             TO GODK-STATUSKODER                          
202600     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E411                         
202700     MOVE WDE4-STATUS-CODE   TO STATUS-WS                                 
202800     PERFORM IMS-STATUSKONTROLL                                           
202900     .                                                                    
203000                                                                          
203100                                                                          
203200 IMS-GHU-WDE601 SECTION.                                                  
203300     MOVE 'IMS-GHU-WDE601  ' TO CURRENT-IMS-SECTION                       
203400                                                                          
203500     MOVE SPACE                 TO ALL-SSA                                
203600     STRING 'WDE601  (IDPRODNR =' W-WDE601-X ')'                          
203700            DELIMITED BY SIZE INTO SSA1                                   
203800     MOVE '  GE'                TO GODK-STATUSKODER                       
203900     CALL CBLTDLI USING GHU    WDE6-PCB DLI-IO-E601 SSA1                  
204000     MOVE WDE6-STATUS-CODE      TO STATUS-WS                              
204100     PERFORM IMS-STATUSKONTROLL                                           
204200     .                                                                    
204300                                                                          
204400                                                                          
204500 IMS-REPL-WDE601 SECTION.                                                 
204600     MOVE 'IMS-REPL-WDE601 ' TO CURRENT-IMS-SECTION                       
204700                                                                          
204800     MOVE SPACE              TO ALL-SSA                                   
204900     MOVE '    '             TO GODK-STATUSKODER                          
205000     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
205100     MOVE WDE6-STATUS-CODE   TO STATUS-WS                                 
205200     PERFORM IMS-STATUSKONTROLL                                           
205300     .                                                                    
205400                                                                          
205500                                                                          
205600 IMS-ISRT-WDQ101 SECTION.                                                 
205700     MOVE 'IMS-ISRT-WDQ101 ' TO CURRENT-IMS-SECTION                       
205800                                                                          
205900     MOVE SPACE              TO ALL-SSA                                   
206000     MOVE 'WLORQM01 '        TO SSA1                                      
206100     MOVE '  II'             TO GODK-STATUSKODER                          
206200     CALL CBLTDLI USING ISRT ORQM-PCB OBKR-WDQ101 SSA1                    
206300     MOVE ORQM-STATUS-CODE   TO STATUS-WS                                 
206400     PERFORM IMS-STATUSKONTROLL                                           
206500     .                                                                    
206600                                                                          
206700                                                                          
206800 IMS-GU-WDQ201 SECTION.                                                   
206900     MOVE 'IMS-GU-WDQ201  '  TO CURRENT-IMS-SECTION                       
207000                                                                          
207100     MOVE SPACE                 TO ALL-SSA                                
207200     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
207300            DELIMITED BY SIZE INTO SSA1                                   
207400     MOVE '  '                  TO GODK-STATUSKODER                       
207500     CALL CBLTDLI USING GU      ORQI-PCB DLI-IO-Q201 SSA1                 
207600     MOVE ORQI-STATUS-CODE      TO STATUS-WS                              
207700     PERFORM IMS-STATUSKONTROLL                                           
207800     .                                                                    
207900                                                                          
208000                                                                          
208100 IMS-GHU-WDQ201 SECTION.                                                  
208200     MOVE 'IMS-GHU-WDQ201  ' TO CURRENT-IMS-SECTION                       
208300                                                                          
208400     MOVE SPACE                 TO ALL-SSA                                
208500     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
208600            DELIMITED BY SIZE INTO SSA1                                   
208700     MOVE '  '                  TO GODK-STATUSKODER                       
208800     CALL CBLTDLI USING GHU     ORQI-PCB DLI-IO-Q201 SSA1                 
208900     MOVE ORQI-STATUS-CODE      TO STATUS-WS                              
209000     PERFORM IMS-STATUSKONTROLL                                           
209100     .                                                                    
209200                                                                          
209300                                                                          
209400 IMS-GHNP-WDQ211 SECTION.                                                 
209500     MOVE 'IMS-GHNP-WDQ211 ' TO CURRENT-IMS-SECTION                       
209600                                                                          
209700     MOVE SPACE                 TO ALL-SSA                                
209800     STRING 'WLORQI11(WDQ211KY =' W-WDQ211-X ')'                          
209900            DELIMITED BY SIZE INTO SSA1                                   
210000     MOVE '  '                  TO GODK-STATUSKODER                       
210100     CALL CBLTDLI USING GHNP    ORQI-PCB DLI-IO-Q211 SSA1                 
210200     MOVE ORQI-STATUS-CODE      TO STATUS-WS                              
210300     PERFORM IMS-STATUSKONTROLL                                           
210400     .                                                                    
210500                                                                          
210600                                                                          
210700 IMS-REPL-WDQ211 SECTION.                                                 
210800     MOVE 'IMS-REPL-WDQ211 ' TO CURRENT-IMS-SECTION                       
210900                                                                          
211000     MOVE SPACE              TO ALL-SSA                                   
211100     MOVE '    '             TO GODK-STATUSKODER                          
211200     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-Q211                         
211300     MOVE ORQI-STATUS-CODE   TO STATUS-WS                                 
211400     PERFORM IMS-STATUSKONTROLL                                           
211500     .                                                                    
211600                                                                          
211700                                                                          
211800 IMS-GHU-WDQ301 SECTION.                                                  
211900     MOVE 'IMS-REPL-WDQ211 ' TO CURRENT-IMS-SECTION                       
212000                                                                          
212100     MOVE SPACE                 TO ALL-SSA                                
212200     STRING 'WLORQA01(WDQ301KY =' W-Q301-KEY-X ')'                        
212300            DELIMITED BY SIZE INTO SSA1                                   
212400     MOVE '  '                  TO GODK-STATUSKODER                       
212500     CALL CBLTDLI USING GHU    ORQA-PCB ODEL-WDQ301 SSA1                  
212600     MOVE ORQA-STATUS-CODE      TO STATUS-WS                              
212700     PERFORM IMS-STATUSKONTROLL                                           
212800     .                                                                    
212900                                                                          
213000                                                                          
213100 IMS-REPL-WDQ301 SECTION.                                                 
213200     MOVE 'IMS-REPL-WDQ301 ' TO CURRENT-IMS-SECTION                       
213300                                                                          
213400     MOVE SPACE              TO ALL-SSA                                   
213500     MOVE '    '             TO GODK-STATUSKODER                          
213600     CALL CBLTDLI USING REPL ORQA-PCB ODEL-WDQ301                         
213700     MOVE ORQA-STATUS-CODE   TO STATUS-WS                                 
213800     PERFORM IMS-STATUSKONTROLL                                           
213900     .                                                                    
214000                                                                          
214100                                                                          
214200 IMS-GHU-WDK711 SECTION.                                                  
214300     MOVE 'IMS-GHU-WDK711  ' TO CURRENT-IMS-SECTION                       
214400                                                                          
214500     MOVE SPACE               TO ALL-SSA                                  
214600     STRING 'WLARTS01(IDARTNR  =' W-WDK701-IDARTNR-X ')'                  
214700          DELIMITED BY SIZE INTO SSA1                                     
214800     STRING 'WLARTS11(IDDC     =' W-WDK711-IDDC-X ')'                     
214900          DELIMITED BY SIZE INTO SSA2                                     
215000     MOVE '  GE'              TO GODK-STATUSKODER                         
215100     CALL CBLTDLI USING GHU ARTS-PCB SLAG-WDK711 SSA1 SSA2                
215200     MOVE ARTS-STATUS-CODE    TO STATUS-WS                                
215300     PERFORM IMS-STATUSKONTROLL                                           
215400     .                                                                    
215500                                                                          
215600                                                                          
215700 IMS-REPL-WDK711 SECTION.                                                 
215800     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
215900                                                                          
216000     MOVE SPACE              TO ALL-SSA                                   
216100     MOVE '  '               TO GODK-STATUSKODER                          
216200     CALL CBLTDLI USING REPL ARTS-PCB SLAG-WDK711                         
216300     MOVE ARTS-STATUS-CODE   TO STATUS-WS                                 
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600                                                                          
216700                                                                          
216800 IMS-GHU-WDK611 SECTION.                                                  
216900     MOVE 'IMS-GHU-WDK611  ' TO CURRENT-IMS-SECTION                       
217000                                                                          
217100     MOVE SPACE               TO ALL-SSA                                  
217200     STRING 'WDK601  (IDARTNR  =' W-WDK601-IDARTNR-X ')'                  
217300          DELIMITED BY SIZE INTO SSA1                                     
217400     MOVE   'WDK611 '         TO SSA2                                     
217500     MOVE '    '              TO GODK-STATUSKODER                         
217600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
217700     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
217800     PERFORM IMS-STATUSKONTROLL                                           
217900     .                                                                    
218000                                                                          
218100                                                                          
218200 IMS-REPL-WDK611 SECTION.                                                 
218300     MOVE 'IMS-REPL-WDK611 ' TO CURRENT-IMS-SECTION                       
218400                                                                          
218500     MOVE SPACE              TO ALL-SSA                                   
218600     MOVE '  '               TO GODK-STATUSKODER                          
218700     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
218800     MOVE ARTS-STATUS-CODE   TO STATUS-WS                                 
218900     PERFORM IMS-STATUSKONTROLL                                           
219000     .                                                                    
219100                                                                          
219200                                                                          
219300 IMS-ISRT-ZZAC01 SECTION.                                                 
219400     MOVE 'IMS-ISRT-ZZAC01 ' TO CURRENT-IMS-SECTION                       
219500                                                                          
219600     MOVE SPACE              TO ALL-SSA                                   
219700     MOVE 'WLZZAC01'         TO SSA1                                      
219800     MOVE '  II'             TO GODK-STATUSKODER                          
219900     CALL CBLTDLI USING ISRT ZZAC-PCB IO-WDGZ01 SSA1                      
220000     MOVE ZZAC-STATUS-CODE   TO STATUS-WS                                 
220100     PERFORM IMS-STATUSKONTROLL                                           
220200     .                                                                    
220300                                                                          
220400                                                                          
220500 IMS-ISRT-LOGG SECTION.                                                   
220600     MOVE 'IMS-ISRT-LOGG   ' TO CURRENT-IMS-SECTION                       
220700                                                                          
220800     MOVE SPACE              TO ALL-SSA                                   
220900     MOVE 'WLFILA01 '        TO SSA1                                      
221000     MOVE '  II'             TO GODK-STATUSKODER                          
221100     CALL CBLTDLI USING ISRT FILA-PCB WLFILA01 SSA1                       
221200     MOVE FILA-STATUS-CODE   TO STATUS-WS                                 
221300     PERFORM IMS-STATUSKONTROLL                                           
221400     .                                                                    
221500                                                                          
221600                                                                          
221700 IMS-GHU-WDF411 SECTION.                                                  
221800     MOVE 'IMS-GHU-WDF411  ' TO CURRENT-IMS-SECTION                       
221900                                                                          
222000     MOVE SPACE               TO ALL-SSA                                  
222100     STRING 'WDF401  (IDLEVNR  =' W-IDLEVNR-F4-X ')'                      
222200          DELIMITED BY SIZE INTO SSA1                                     
222300     STRING 'WDF411  (WDF411KY>=' W-WDF411KY-MIN-X                        
222400                    '&WDF411KY<=' W-WDF411KY-MAX-X ')'                    
222500          DELIMITED BY SIZE INTO SSA2                                     
222600     MOVE '  GE'              TO GODK-STATUSKODER                         
222700     CALL CBLTDLI USING GHU WDF4-PCB DLI-IO-F411 SSA1 SSA2                
222800     MOVE WDF4-STATUS-CODE    TO STATUS-WS                                
222900     PERFORM IMS-STATUSKONTROLL                                           
223000     .                                                                    
223100                                                                          
223200                                                                          
223300 IMS-REPL-WDF411 SECTION.                                                 
223400     MOVE 'IMS-REPL-WDF411 ' TO CURRENT-IMS-SECTION                       
223500                                                                          
223600     MOVE SPACE              TO ALL-SSA                                   
223700     MOVE '  '               TO GODK-STATUSKODER                          
223800     CALL CBLTDLI USING REPL WDF4-PCB DLI-IO-F411                         
223900     MOVE WDF4-STATUS-CODE   TO STATUS-WS                                 
224000     PERFORM IMS-STATUSKONTROLL                                           
224100     .                                                                    
224200                                                                          
224300                                                                          
224400 IMS-DLET-WDF411 SECTION.                                                 
224500     MOVE 'IMS-DLET-WDF411 ' TO CURRENT-IMS-SECTION                       
224600                                                                          
224700     MOVE SPACE              TO ALL-SSA                                   
224800     MOVE '  '               TO GODK-STATUSKODER                          
224900     CALL CBLTDLI USING DLET WDF4-PCB DLI-IO-F411                         
225000     MOVE WDF4-STATUS-CODE   TO STATUS-WS                                 
225100     PERFORM IMS-STATUSKONTROLL                                           
225200     .                                                                    
225300                                                                          
225400                                                                          
225500 IMS-GU-WDB201 SECTION.                                                   
225600                                                                          
225700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
225800          DELIMITED BY SIZE INTO SSA1                                     
225900     MOVE '  ' TO GODK-STATUSKODER                                        
226000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
226100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
226200     PERFORM IMS-STATUSKONTROLL                                           
226300     .                                                                    
226400                                                                          
226500                                                                          
226600 DB2-SELECT-TP4TRAN     SECTION.                                          
226700     MOVE 'DB2-SEL-TP4TRAN ' TO  CURRENT-DB2-SECTION                      
226800                                                                          
226900     MOVE 000100 TO GODK-SQLCODEKODER                                     
227000                                                                          
227100     EXEC SQL                                                             
227200           SELECT  DISTINCT                                               
227300                   IDDC_REC                                               
227400                                                                          
227500           INTO   :TP4TRAN-IDDC-REC                                       
227600                                                                          
227700           FROM    TP4TRAN                                                
227800                                                                          
227900           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
228000     END-EXEC                                                             
228100                                                                          
228200     MOVE SQLCODE TO SQLCODE-WS                                           
228300     PERFORM DB2-STATUSKONTROLL                                           
228400     .                                                                    
228500                                                                          
228600                                                                          
228700 IMS-STATUSKONTROLL SECTION.                                              
228800                                                                          
228900     SET STATUS-IX TO 1                                                   
229000     SEARCH GODK-STATUS                                                   
229100       AT END                                                             
229200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
229300         DELIMITED BY SIZE INTO FELTEXT-STR                               
229400         CALL FELLOG                                                      
229500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
229600         CONTINUE                                                         
229700     END-SEARCH                                                           
229800     .                                                                    
229900                                                                          
230000                                                                          
230100 DB2-STATUSKONTROLL  SECTION.                                             
230200                                                                          
230300     SET SQLCODE-IX TO 1                                                  
230400     SEARCH GODK-SQLCODE                                                  
230500       AT END                                                             
230600          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
230700          DELIMITED BY SIZE INTO FELTEXT                                  
230800          CALL ABEND USING RKOD-ABEND-DB2                                 
230900       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
231000     END-SEARCH                                                           
231100     .                                                                    
