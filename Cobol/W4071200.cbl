000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4071200.                                                
000300 AUTHOR.         LASSE C.                                                 
000400 DATE-WRITTEN.   SEPT  94.                                                
000500                                                                          
000600*                                                                         
000700*    FUNKTION.                                                            
000800*        PROGRAMMET LÄSER KREDIT.REG WLKREE                               
000900*        LEVERANSANMÄRKNINGS-ID MÅSTE VARA IFYLLT                         
001000*        FÖR ATT                                                          
001100*        SÖKNING SKALL SKE (UNDANTAG: OM KUNDNR EJ                        
001200*        ÄR IFYLLT, SÖKS PÅ KUND 0).                                      
001300*        EN RAD SKAPAS FÖR VARJE LEV.ANM.RAD.                             
001400*        PGM:ET KAN UPPDATERA KREE11 M.H.A. PF11, OM                      
001500*        FÄLTET BEH.KOD ÄR IFYLLT PÅ RESP. RAD ELLER OM                   
001600*        FÄLTEN PÅ RAD 18 INNEHÅLLER GODK. INFO.                          
001700*        GODKÄND INFO I FÄLTET BEH.KOD:                                   
001800*         'R  ' INNEBÄR REGISTERAD RAD (EJ UPPD)                          
001900*         'RR ' INNEBÄR SKICKAS PÅ REMISS                                 
002000*         'QR ' INNEBÄR SKICKAS PÅ REMISS FÖR MATRIX-KÖ ARTIKEL           
002100*               MAIL SKICKAS DÅ TILL REMISSANSVARIG                       
002200*         'Y  ' ELLER 'J  ' FÖR ATT GODKÄNNA RADEN                        
002300*         'NXX'             FÖR ATT AVVISA RADEN                          
002400*               MAIL SKICKAS DÅ TILL ADM.ANSVARIG                         
002500*               NÄR HELA LEVERANSANMÄRKNINGEN BEHANDLATS.                 
002600*        DEN LEV.ANM.RAD SOM ÄNDRATS PÅ RAD 19, GES                       
002700*        EN ÄNDRINGSKOD AV PGM:ET BEROENDE PÅ VAD SOM                     
002800*        ÄNDRATS.                                                         
002900*        NÄR BEHANDLING PÅBÖRJATS BYTS STATUS PÅ                          
003000*        KREE01 TILL:                                                     
003100*        KDLEVANM = 2                                                     
003200*        NÄR ALLA LEV.ANM.RADER ÄR BEHANDLADE BYTES                       
003300*        STATUS PÅ KREE01 TILL:                                           
003400*        KDLEVANM = 3                                                     
003500*                                                                         
003600*        KONTROLL MOT W418KTL3 FÖR RETUR-MATRIX ARTIKEL.                  
003700*                                                                         
003800*        PROGRAMMET LÄSER            WDR5 (WDGX6328)                      
003900*        PROGRAMMET LÄSER/UPPDATERAR WDR5 (WDGX4103)                      
004000*        PROGRAMMET LÄSER            WDB6                                 
004100*        PROGRAMMET LÄSER            WDK6                                 
004200*        PROGRAMMET LÄSER            WDK7                                 
004300*                                                                         
004400*    E-TRACKER: 1572353  DATE 2005-04-22                                  
004500*               2913019  DATE 2006-01-26                                  
004600*               1658417  DATE 2006-03-06                                  
004700*               5674920  DATE 2007-09-25                                  
004800*               5798675  DATE 2007-10-31                                  
004900*               5838822  DATE 2007-11-08                                  
005000*                                                                         
005100*    E-TRACKER  8635407  DATE 2009-10-28 RETURN CODES MATRIX              
005200*    E-TRACKER  8687963  DATE 2010-03-18 REFERRALS PICKING AREA           
005300*    E-TRACKER  9822116  DATE 2010-10-14 DISCR/RETURNS HAZ.MAT.           
005400*    E-TRACKER  10143271 DATE 2011-10-19 CHINA WAREHOUSE PROJECT-1        
005500*    STORY 1639344 / POSSIBILITY TO CHANGE DISCREPENCY CODE FROM          
005600*                     42-62 AND FROM 62-42                                
005700*                                                                         
005800*    INDATA.                                                              
005900*        TRANSAKTION: W4T712                                              
006000*                     W4T712U                                             
006100*                     W4T712V                                             
006200*        MID:         W4I71201                                            
006300*                                                                         
006400*    UTDATA.                                                              
006500*        MOD:         W4O71201                                            
006600 ENVIRONMENT DIVISION.                                                    
006700                                                                          
006800 DATA DIVISION.                                                           
006900                                                                          
007000     EJECT                                                                
007100 WORKING-STORAGE SECTION.                                                 
007200*    -- CHECKED BY WY2000                                                 
007300                                                                          
007400 77  PROGRAM-NAMN                PIC X(8)   VALUE 'W4071200'.             
007500 77  IDDISTR-WS                  PIC X(4)   VALUE SPACE.                  
007600 77  IDKUNDNR-WS                 PIC X(6)   VALUE SPACE.                  
007700 77  IDRAPPNR-WS                 PIC X(7)   VALUE SPACE.                  
007800 77  WS-IDARTNR                  PIC 9(9)   VALUE ZERO.                   
007900 77  IDARTNR-WS                  PIC 9(8)   VALUE ZERO.                   
008000 77  IDRADNR-WS                  PIC 9(4)   VALUE ZERO.                   
008100 77  KVLEVANM-WS                 PIC X(6)   VALUE SPACE.                  
008200 77  WS-KVRADER-ANN              PIC S9(7)  VALUE +0    COMP-3.           
008300 77  KDANMORS-WS                 PIC X(2)   VALUE SPACE.                  
008400 77  FLDIRLEV-WS                 PIC X(1)   VALUE SPACE.                  
008500 77  KDLEVANM-WS                 PIC 9      VALUE ZERO.                   
008600 77  WS-IDEDITDATA               PIC S9(7)V9(2) VALUE ZERO.               
008700 77  WS-CDC-SE                   PIC X(2)    VALUE '11'.                  
008800 77  DUMMY-IDARTNR               PIC S9(9)  VALUE +100  COMP-3.           
008900 77  W-KVPB-TOT                  PIC S9(6)V9 VALUE ZERO COMP-3.           
009000 77  JA                          PIC X      VALUE 'J'.                    
009100 77  NEJ                         PIC X      VALUE 'N'.                    
009200 77  YES                         PIC X      VALUE 'Y'.                    
009300 77  FEL                         PIC X      VALUE 'F'.                    
009400 77  INDX                        PIC S9(2)  VALUE +0    COMP SYNC.        
009500 77  IX                          PIC S9(2)  VALUE +0    COMP SYNC.        
009600 77  MEAN-IX                     PIC S9(2)  VALUE +0    COMP SYNC.        
009700 77  MAX-IX                      PIC S9(4)  VALUE +12   COMP SYNC.        
009800 77  4793-LAENGD                 PIC S9(4)  VALUE +55   COMP SYNC.        
009900 77  SEC-FELSVAR                 PIC X      VALUE 'F'.                    
010000 77  W-KONTO-FTGKOD-FEL          PIC X.                                   
010100 77  WS-DATUM                    PIC 9(6).                                
010200 77  WS-DATUM-Y2K                PIC 9(8).                                
010300 77  FELTEXT-STR                 PIC X(75)   VALUE SPACE.                 
010400 77  WC-KDANMORS                 PIC X(8)   VALUE 'KDANMORS'.             
010500 77  WC-IDFKNGRP                 PIC X(8)   VALUE 'IDFKNGRP'.             
010600 77  WC-IDARTNR                  PIC X(8)   VALUE 'IDARTNR '.             
010700 77  WC-IDDC-EXCP                PIC X(8)   VALUE 'IDDC    '.             
010800 77  WS-ANM-IDDC-RET             PIC X(2)   VALUE SPACE.                  
010900 77  WS-ANM-IXDCCLEAR            PIC 9      VALUE ZERO.                   
011000 77  SW-KDKREBEH-Q               PIC X       VALUE 'N'.                   
011100 77  SW-KDKREBEH-P               PIC X       VALUE 'N'.                   
011200 77  KDRC-DISPLAY                PIC Z(5).                                
011300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
011400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +100  COMP SYNC.        
011500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011600                                                                          
011700 77  WS-RADPRIS                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011800 77  WS-NYTT-RADPRIS             PIC S9(7)V9(2) VALUE ZERO COMP-3.        
011900 01    WS-CHAR-DATE.                                                      
012000   03  FILLER                    PIC X(2)    VALUE '20'.                  
012100   03  WS-YY-CHAR                PIC 9(2)    VALUE ZERO.                  
012200   03  FILLER                    PIC X(1)    VALUE '-'.                   
012300   03  WS-MM-CHAR                PIC 9(2)    VALUE ZERO.                  
012400   03  FILLER                    PIC X(1)    VALUE '-'.                   
012500   03  WS-DD-CHAR                PIC 9(2)    VALUE ZERO.                  
012600                                                                          
012700 01    WS-DATE-NUM6              PIC 9(6)    VALUE ZERO.                  
012800 01    WS-DATE-NUM6-EDIT REDEFINES WS-DATE-NUM6.                          
012900   03  WS-DATE-NUM6-YY           PIC 9(2).                                
013000   03  WS-DATE-NUM6-MM           PIC 9(2).                                
013100   03  WS-DATE-NUM6-DD           PIC 9(2).                                
013200                                                                          
013300 01    WS-DATE-NUM10             PIC 9(6)    VALUE ZERO.                  
013400 01    WS-DATE-NUM10-EDIT REDEFINES WS-DATE-NUM10.                        
013500   03  WS-DATE-NUM10-YY          PIC 9(2).                                
013600   03  WS-DATE-NUM10-MM          PIC 9(2).                                
013700   03  WS-DATE-NUM10-DD          PIC 9(2).                                
013800   03  FILLER                    PIC 9(4).                                
013900                                                                          
014000                                                                          
014100 77  SPAR-KDKRENOT-4104          PIC X(2)   VALUE SPACE.                  
014200 77  SPAR-KDANMORS               PIC X(2)   VALUE SPACE.                  
014300 77  SPAR-KVLEVANM-BEKR          PIC S9(7)  VALUE ZERO  COMP-3.           
014400 77  SPAR-PRARTBTO               PIC S9(7)V9(2) VALUE ZERO COMP-3.        
014500 77  SPAR-PRARTBTO-LOC           PIC S9(7)V9(2) VALUE ZERO COMP-3.        
014600 77  SPAR-PRARTBTO-LOCINV        PIC S9(7)V9(2) VALUE ZERO COMP-3.        
014700 77  SPAR-IDDC-RET               PIC X(2)   VALUE SPACE.                  
014800                                                                          
014900 01  W-RAD18-KDANMORS.                                                    
015000     02  W-RAD18-KDANMORS-POS1   PIC 9.                                   
015100     02  W-RAD18-KDANMORS-POS2   PIC 9.                                   
015200 01  W-LEV-KDANMORS              PIC X(2).                                
015300 01  FILLER REDEFINES W-LEV-KDANMORS.                                     
015400     02  W-LEV-KDANMORS-POS1     PIC X.                                   
015500     02  W-LEV-KDANMORS-POS2     PIC X.                                   
015600 77  KDKREBEH-SW                 PIC X(3)   VALUE SPACE.                  
015700   88  OK-KOD                                                             
015800         VALUE 'ANN' 'DEL' 'N70' 'N71' 'N72' 'N73'                        
015900               'N74' 'N75' 'N76' 'N77' 'N78' 'N79'                        
016000               'N80' 'Y  ' 'RR ' 'QR '.                                   
016100   88  OK-FELKOD                                                          
016200         VALUE 'N70' 'N71' 'N72' 'N73' 'N74' 'N75'                        
016300               'N76' 'N77' 'N78' 'N79' 'N80'.                             
016400   88  OK-GODKAEND                                                        
016500         VALUE 'Y  '.                                                     
016600   88  OK-REMISS                                                          
016700         VALUE 'RR ' 'QR '.                                               
016800   88  OK-ANN                                                             
016900         VALUE 'ANN'                                                      
017000               'DEL'.                                                     
017100 77  OBEH-RADER-FINNS-SW         PIC X.                                   
017200   88  OBEH-RADER-FINNS                     VALUE 'J'.                    
017300 77  OBEH-REMISS-FINNS-SW        PIC X.                                   
017400   88  OBEH-REMISS-FINNS                    VALUE 'J'.                    
017500 77  BEH-RADER-FINNS-SW          PIC X.                                   
017600   88  BEH-RADER-FINNS                      VALUE 'J'.                    
017700 77  AVVISADE-RADER-SW           PIC X.                                   
017800   88  AVVISADE-RADER-FINNS                 VALUE 'J'.                    
017900 77  ANNULERADE-RADER-SW         PIC X.                                   
018000   88  ANNULERADE-RADER-FINNS               VALUE 'J'.                    
018100 77  GODKAENDA-RADER-SW          PIC X.                                   
018200   88  GODKAENDA-RADER-FINNS                VALUE 'J'.                    
018300 77  NYCKLAR-SW                  PIC X.                                   
018400   88  NYCKLAR-OK                           VALUE 'J'.                    
018500 77  INDATA-SW                   PIC X.                                   
018600   88  INDATA-OK                            VALUE 'J'.                    
018700 77  OK-SW                       PIC X.                                   
018800   88  OK                                   VALUE 'J'.                    
018900 77  API-SW                      PIC X      VALUE 'N'.                    
019000 77  KDANMORS-UPPD               PIC X.                                   
019100 77  RAD-FINNS-SW                PIC X.                                   
019200   88  RAD-FINNS-PA-SKARMEN                 VALUE 'J'.                    
019300 77  RETUR-FINNS-PA-RETTERM-SW   PIC X      VALUE 'N'.                    
019400   88  RETUR-FINNS-PA-RETTERM               VALUE 'J'.                    
019500                                                                          
019600 77  DUMMY-NR-SW                 PIC X.                                   
019700   88  DUMMY-NR                             VALUE 'J'.                    
019800                                                                          
019900 77  NEKAD-RAD-SW                PIC X      VALUE 'N'.                    
020000     88  NEKAD-RAD                          VALUE 'J'.                    
020100                                                                          
020200 77  NEKAD-RETUR-RAD-SW          PIC X      VALUE 'N'.                    
020300     88  NEKAD-RETUR-RAD                    VALUE 'J'.                    
020400                                                                          
020500                                                                          
020600 77  ANGRA-NEKAD-RETUR-RAD-SW    PIC X      VALUE 'N'.                    
020700     88  ANGRA-NEKAD-RETUR-RAD              VALUE 'J'.                    
020800                                                                          
020900 77  ANGRA-NEKAD-RAD-SW          PIC X      VALUE 'N'.                    
021000     88  ANGRA-NEKAD-RAD                    VALUE 'J'.                    
021100                                                                          
021200 77  ANNULLERAD-RAD-SW           PIC X      VALUE 'N'.                    
021300     88  ANNULLERAD-RAD                     VALUE 'J'.                    
021400                                                                          
021500 77  PRIS-AENDRAT-SW             PIC X      VALUE 'N'.                    
021600     88  PRIS-AENDRAT                       VALUE 'J'.                    
021700                                                                          
021800 77  ANTAL-AENDRAT-SW            PIC X      VALUE 'N'.                    
021900     88  ANTAL-AENDRAT                      VALUE 'J'.                    
022000                                                                          
022100 77  KOD-AENDRAD-SW              PIC X      VALUE 'N'.                    
022200     88  KOD-AENDRAD                        VALUE 'J'.                    
022300                                                                          
022400 77  KOD-LDCKUND-AENDRAD-SW      PIC X      VALUE 'N'.                    
022500     88  KOD-LDCKUND-AENDRAD                VALUE 'J'.                    
022600                                                                          
022700 77  KNOTA-RAD-FINNS-SW          PIC X      VALUE 'N'.                    
022800     88  KNOTA-RAD-FINNS                    VALUE 'J'.                    
022900                                                                          
023000 77  RETURRADER-KVAR-SW         PIC X       VALUE 'N'.                    
023100     88 RETURRADER-KVAR                     VALUE 'J'.                    
023200                                                                          
023300 77  WL410901-SW                 PIC X       VALUE 'J'.                   
023400     88  WL410901-FINNS                      VALUE 'J'.                   
023500     88  WL410901-SAKNAS                     VALUE 'N'.                   
023600                                                                          
023700 77  WDR501-SW                   PIC X       VALUE 'N'.                   
023800     88  WDR501-FINNS                        VALUE 'J'.                   
023900                                                                          
024000 77  IXDCCLEAR-2-SW              PIC X       VALUE 'N'.                   
024100     88  IXDCCLEAR-2-RAD-FINNS               VALUE 'J'.                   
024200                                                                          
024300 77  IXDCCLEAR-3-SW              PIC X       VALUE 'N'.                   
024400     88  IXDCCLEAR-3-RAD-FINNS               VALUE 'J'.                   
024500                                                                          
024600 77  GODK-ADM-DC-SW              PIC X       VALUE 'Y'.                   
024700     88  GODK-ADM-DC                         VALUE 'Y'.                   
024800     88  EJ-GODK-ADM-DC                      VALUE 'N'.                   
024900                                                                          
025000 77  GODK-KOD-SW                 PIC X.                                   
025100     88  GODK-KOD                            VALUE 'J'.                   
025200     88  EJ-GODK-KOD                         VALUE 'N'.                   
025300     EJECT                                                                
025400                                                                          
025500 77  GODK-ARTIKEL-SW             PIC X.                                   
025600     88  GODK-ARTIKEL                        VALUE 'J'.                   
025700     88  EJ-GODK-ARTIKEL                     VALUE 'N'.                   
025800     EJECT                                                                
025900                                                                          
026000 77  GODK-IDFKNGRP-SW            PIC X.                                   
026100     88  GODK-IDFKNGRP                       VALUE 'J'.                   
026200     88  EJ-GODK-IDFKNGRP                    VALUE 'N'.                   
026300     EJECT                                                                
026400                                                                          
026500 77  GODK-DC-ARTIKEL-SW          PIC X.                                   
026600     88  GODK-DC-ARTIKEL                     VALUE 'J'.                   
026700     88  EJ-GODK-DC-ARTIKEL                  VALUE 'N'.                   
026800     EJECT                                                                
026900                                                                          
027000 77  GODK-DC-LEV-SW              PIC X.                                   
027100     88  GODK-DC-LEV                         VALUE 'J'.                   
027200     88  EJ-GODK-DC-LEV                      VALUE 'N'.                   
027300     EJECT                                                                
027400                                                                          
027500 77  WS-IDANALYS-UPPD            PIC X(12).                               
027600 77  WS-IDKONTO-UPPD             PIC 9(11)   VALUE ZERO COMP-3.           
027700 77  WS-IDKST-UPPD               PIC X(10)   VALUE SPACE.                 
027800                                                                          
027900 77  WS-IDTRANS                  PIC X(4).                                
028000   88  4711-BILD                            VALUE '4711'.                 
028100   88  4724-BILD                            VALUE '472D'.                 
028200   88  EGEN-BILD                            VALUE '4712'.                 
028300   88  GODKAEND-BILD                        VALUE '4711'                  
028400                                                  '4712'                  
028500                                                  '4713'                  
028600                                                  '4714'                  
028700                                                  '4715'                  
028800                                                  '4716'                  
028900                                                  '472D'.                 
029000                                                                          
029100     EJECT                                                                
029200                                                                          
029300 01  TEST-IDDISTR                PIC 9(5)   VALUE ZERO COMP-3.            
029400*01  FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                       
029500     EJECT                                                                
029600*01  FILLER  -COPY WWDIST07      -RED TEST-IDDISTR.                       
029700     EJECT                                                                
029800*01  FILLER  -COPY WWDIST35      -RED TEST-IDDISTR.                       
029900     EJECT                                                                
030000*01  FILLER  -COPY WWDIST34      -RED TEST-IDDISTR.                       
030100     EJECT                                                                
030200*                                                                         
030300                                                                          
030400*01  -COPY WWIDFTG                                                        
030500     EJECT                                                                
030600*                                                                         
030700 01  DYNAMISKA-SUBPROGRAM.                                                
030800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
030900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
031000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
031100     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
031200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
031300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
031400     03  W418ANSV                PIC X(8)    VALUE 'W418ANSV'.            
031500     03  W418MERE                PIC X(8)    VALUE 'W418MERE'.            
031600     03  W418MEAN                PIC X(8)    VALUE 'W418MEAN'.            
031700     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
031800     03  W418KTL3                PIC X(8)    VALUE 'W418KTL3'.            
031900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
032000     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
032100                                                                          
032200 01  KDKREBEH-SPAR.                                                       
032300   03  FILLER                    PIC X(1)   VALUE 'C'.                    
032400   03  KDKREBEH-SPAR-1           PIC 9(1)   VALUE ZERO.                   
032500   03  KDKREBEH-SPAR-2           PIC 9(1)   VALUE ZERO.                   
032600                                                                          
032700 03  JFR-WDA211-X.                                                        
032800   05  JFR-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.            
032900   05  JFR-IDRADNR             PIC S9(5)   VALUE ZERO  COMP-3.            
033000                                                                          
033100 01  TEST-KDKREBEH.                                                       
033200   03  KDKREBEH-1                PIC X(1)   VALUE SPACE.                  
033300   03  KDKREBEH-2                PIC X(1)   VALUE SPACE.                  
033400   03  KDKREBEH-3                PIC X(1)   VALUE SPACE.                  
033500                                                                          
033600 01  W-KDANMORS                  PIC 9(2)  VALUE ZERO.                    
033700 01  W-KDANMORS-RED.                                                      
033800   03  FILLER                    PIC 9(1).                                
033900   03  W-KDANMORS-2              PIC 9(1).                                
034000                                                                          
034100     EJECT                                                                
034200 01  FILLER                      PIC X(16)   VALUE 'W006PRT '.            
034300*01  -COPY W006PRT                                                        
034400     EJECT                                                                
034500 01  FILLER                      PIC X(16)   VALUE 'W418ANSV'.            
034600*    --- PARAMETRAR TILL SUBPROGRAM W418ANSV                              
034700*01 -COPY W418ANSV                                                        
034800     EJECT                                                                
034900 01  FILLER                      PIC X(16)   VALUE 'W418MERE'.            
035000*    --- PARAMETRAR TILL SUBPROGRAM W418MERE                              
035100*01 -COPY W418MERE                                                        
035200     EJECT                                                                
035300 01  FILLER                      PIC X(16)   VALUE 'W418MEAN'.            
035400*    --- PARAMETRAR TILL SUBPROGRAM W418MEAN                              
035500*01 -COPY W418MEAN                                                        
035600     EJECT                                                                
035700 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
035800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
035900*01 -COPY WMEDAREA                                                        
036000     EJECT                                                                
036100*    ---  LÄNKAREA TILL W418OKOD                                          
036200 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
036300                                                                          
036400*01 -COPY W418OKOD           -PRE OKOD-.                                  
036500     EJECT                                                                
036600 01  FILLER                      PIC X(16)   VALUE 'W418KTL3'.            
036700*    --- PARAMETERS FOR SUBPROGRAM W418KTL3                               
036800*01 -COPY W418KTL3                                                        
036900     EJECT                                                                
037000*    --- PARAMETRAR TILL SUBPROGRAM WZ01SUB                               
037100*01  -COPY WZ01SUB                                                        
037200 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH   '.         
037300                                                                          
037400*01  -COPY WZ01AUTH                                                       
037500*                                                                         
037600 01  SUB-DATA                    PIC X(4000000).                          
037700                                                                          
037800 01  MESSAGE-CODES.                                                       
037900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
038000     03  INF-UPDATE-OK           PIC X(3)    VALUE '101'.                 
038100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
038200     03  INF-PRINT-STARTED       PIC X(3)    VALUE '202'.                 
038300     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
038400     03  INF-MATRIS-KONFLIKT     PIC X(3)    VALUE '291'.                 
038500     03  INF-MATRIS-KONFLIKT-PSN PIC X(3)    VALUE '292'.                 
038600     03  ERR-UPDATE-NOT-OK       PIC X(3)    VALUE '007'.                 
038700     03  ERR-DELETE-NOT-ALLOWED  PIC X(3)    VALUE '066'.                 
038800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
038900     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
039000     03  ERR-HIGHL-FIELDS        PIC X(3)    VALUE '409'.                 
039100     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '405'.                 
039200     03  ERR-INF-MISS-4702       PIC X(3)    VALUE '332'.                 
039300     03  ERR-ALREADY-CANCELLED   PIC X(3)    VALUE '333'.                 
039400     03  ERR-PRIS-MISSING        PIC X(3)    VALUE '301'.                 
039500     03  ERR-VAT-NO-MISSING      PIC X(3)    VALUE '223'.                 
039600     03  ERR-EJ-BEHORIG-GODK-LA  PIC X(3)    VALUE '604'.                 
039700     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '777'.                 
039800     03  NOT-FOUND               PIC X(3)    VALUE '404'.                 
039900     03  BAD-REQUEST             PIC X(3)    VALUE '400'.                 
040000     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
040100     EJECT                                                                
040200 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
040300 01  NYCKLAR.                                                             
040400   03  W-IDLEVANM-X.                                                      
040500     05  W-IDDISTR               PIC S9(5)   VALUE ZERO  COMP-3.          
040600     05  W-IDKUNDNR              PIC S9(7)   VALUE ZERO  COMP-3.          
040700     05  W-IDRAPPNR              PIC  9(7).                               
040800                                                                          
040900   03  W-WDA3FSEQ-X.                                                      
041000     05  W-IDDC-A3               PIC  X(2)   VALUE SPACE.                 
041100     05  W-IDDISTR-A3            PIC S9(5)   VALUE ZERO  COMP-3.          
041200     05  W-IDKUNDNR-A3           PIC S9(7)   VALUE ZERO  COMP-3.          
041300     05  W-IDRAPPNR-A3           PIC  9(7).                               
041400                                                                          
041500   03  W-KDARBTYP-X.                                                      
041600     05  W-KDARBTYP              PIC  X(8).                               
041700                                                                          
041800   03  W-4109-X.                                                          
041900      05  FILLER                 PIC X(04)   VALUE '4109'.                
042000      05  W-IDFTG-4109           PIC 9(2)    VALUE ZERO.                  
042100      05  FILLER                 PIC X(24)   VALUE LOW-VALUE.             
042200                                                                          
042300   03  W-WDGXKEY-MIN-X.                                                   
042400     05 W-IDARTNR-4109-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
042500     05 W-KDANMORS-4109-MIN      PIC X(2)    VALUE SPACE.                 
042600     05 FILLER                   PIC X(8)    VALUE LOW-VALUE.             
042700                                                                          
042800   03  W-WDGXKEY-MAX-X.                                                   
042900     05 W-IDARTNR-4109-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
043000     05 W-KDANMORS-4109-MAX      PIC X(2)    VALUE SPACE.                 
043100     05 FILLER                   PIC X(8)    VALUE HIGH-VALUE.            
043200                                                                          
043300   03  W-IDPERSON-X.                                                      
043400     05  W-IDPERSON              PIC S9(3)   VALUE ZERO  COMP-3.          
043500                                                                          
043900   03  W-IDARTNR-X.                                                       
044000     05  W-IDARTNR-ARTC          PIC S9(9)   VALUE ZERO  COMP-3.          
044100                                                                          
044200   03  W-IDARTNR-K7-X.                                                    
044300     05  W-IDARTNR-K7            PIC S9(9)   VALUE ZERO  COMP-3.          
044400                                                                          
044800   03  W-IDSKYLT-X.                                                       
044900     05  W-IDSKYLT               PIC X(3)    VALUE 'S  '.                 
045000                                                                          
045100   03  W-IDDC-B6-X.                                                       
045200     05  W-IDDC-B6               PIC X(2)    VALUE SPACE.                 
045300                                                                          
045400   03  W-WDB611KY-X.                                                      
045500     05  W-URV-TEELMT            PIC X(16)  VALUE SPACE.                  
045600     05  W-URV-FILLER            PIC X(20)  VALUE SPACE.                  
045700     05  W-URV-IDARTNR-EXCP-FILLER REDEFINES W-URV-FILLER.                
045800       07  W-URV-IDARTNR-EXCP    PIC 9(9).                                
045900       07  FILLER                PIC X(11).                               
046000     05  W-URV-IDFKNGRP-EXCP-FILLER REDEFINES W-URV-FILLER.               
046100       07  W-URV-IDFKNGRP-EXCP   PIC 9(4).                                
046200       07  FILLER                PIC X(16).                               
046300     05  W-URV-KDANMORS-RET-FILLER REDEFINES W-URV-FILLER.                
046400       07  W-URV-KDANMORS-RET    PIC X(2).                                
046500       07  FILLER                PIC X(18).                               
046600     05  W-URV-IDDC-EXCP-FILLER REDEFINES W-URV-FILLER.                   
046700       07  W-URV-IDDC-EXCP       PIC X(2).                                
046800       07  FILLER                PIC X(18).                               
046900                                                                          
047000   03  W-IDDC-K7-X.                                                       
047100     05  W-IDDC-K7               PIC X(2)    VALUE SPACE.                 
047200                                                                          
047300   03  W-IDLAND-K7-X.                                                     
047400     05  W-IDLAND-K7             PIC X(2)    VALUE SPACE.                 
047500                                                                          
047600   03  W-WDA211KY-X.                                                      
047700     05  W-IDARTNR               PIC S9(9)   VALUE ZERO  COMP-3.          
047800     05  W-IDRADNR               PIC S9(5)   VALUE ZERO  COMP-3.          
047900                                                                          
049900   03  W-IDFAKT-X.                                                        
050000     05  W-IDFAKT                PIC S9(7)   COMP-3 VALUE ZERO.           
050200                                                                          
050300   03  W-IDGMTREF-X.                                                      
050400     05  W-IDDISTR-L5            PIC S9(5)   COMP-3 VALUE ZERO.           
050500     05  W-IDKUNDNR-L5           PIC S9(7)   COMP-3 VALUE ZERO.           
050600     05  W-IDKUNDRF-L5           PIC X(10).                               
050700                                                                          
050800   03  W-IDARTNR-L5-X.                                                    
050900     05  W-IDARTNR-L5            PIC S9(9)   VALUE ZERO  COMP-3.          
051000                                                                          
051100   03  W-WDL511KY-X.                                                      
051200     05 W-IDPRODNR-L5            PIC S9(7)   VALUE ZERO  COMP-3.          
051300     05 W-IDKOLLI-X.                                                      
051400       07 W-IDKOLLI-L5           PIC S9(5)   VALUE ZERO  COMP-3.          
051500                                                                          
051600     EJECT                                                                
051700                                                                          
051800*    -NYCKLAR TILL WDB201                                                 
051900     03  W-IDGMT-X.                                                       
052000       05  W-IDDISTR-WDB2        PIC S9(5) VALUE ZERO COMP-3.             
052100       05  W-IDKUNDNR-WDB2       PIC S9(7) VALUE ZERO COMP-3.             
052200     03  W-IDGMT-MIN-X.                                                   
052300       05  W-IDDISTR-WDB2-MIN    PIC S9(5) VALUE ZERO COMP-3.             
052400       05  W-IDKUNDNR-WDB2-MIN   PIC S9(7) VALUE ZERO COMP-3.             
052500     03  W-IDGMT-MAX-X.                                                   
052600       05  W-IDDISTR-WDB2-MAX    PIC S9(5) VALUE ZERO COMP-3.             
052700       05  W-IDKUNDNR-WDB2-MAX   PIC S9(7) VALUE ZERO COMP-3.             
052800                                                                          
052900* TILL WDB101                                                             
053000     03  W-WDB101KY-X.                                                    
053100       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
053200       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
053300                                                                          
053400                                                                          
053500* TILL WDR5 ATTESTANSVARIGTABELL KREDITNOTOR                              
053600     03  W-WDGXKEY-6327-X.                                                
053700         05  W-IDHTYP-6327       PIC X(4)    VALUE '6327'.                
053800         05  W-KDARBTYP-6327     PIC X(8)    VALUE 'DISC    '.            
053900         05  W-IDDC-6327         PIC X(2)    VALUE SPACE.                 
054000         05  FILLER              PIC X(16)   VALUE LOW-VALUE.             
054100                                                                          
054200     03  W-KY6328-MIN-X.                                                  
054300         05  W-SUBEL-6328-MIN    PIC 9(7)    VALUE ZERO.                  
054400         05  W-IDUSER-6328-MIN   PIC X(8)    VALUE LOW-VALUE.             
054500                                                                          
054600     03  W-KY6328-MAX-X.                                                  
054700         05  W-SUBEL-6328-MAX    PIC 9(7)    VALUE 9999999.               
054800         05  W-IDUSER-6328-MAX   PIC X(8)    VALUE HIGH-VALUE.            
054900                                                                          
055000     03  W-IDUSER-6328-X.                                                 
055100         05  W-IDUSER-GODK-6328  PIC X(8)    VALUE SPACE.                 
055200                                                                          
055300* TILL WDR5 ATTEST AV KREDITNOTOR                                         
055400     03  W-WDGXKEY-4103-X.                                                
055500         05  W-IDHTYP-4103       PIC X(4)    VALUE '4103'.                
055600         05  W-IDDISTR-4103      PIC S9(5)   VALUE ZERO COMP-3.           
055700         05  W-IDKUNDNR-4103     PIC S9(7)   VALUE ZERO COMP-3.           
055800         05  W-IDRAPPNR-4103     PIC  9(7)   VALUE ZERO.                  
055900         05  FILLER              PIC X(12)   VALUE LOW-VALUE.             
056000                                                                          
056100     03  W-WDGXKEY-4104-X.                                                
056200         05  W-IDDC-4104         PIC X(2)    VALUE SPACE.                 
056300         05  W-KDKRENOT-4104     PIC X(2)    VALUE SPACE.                 
056400                                                                          
056500     EJECT                                                                
056600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
056700*                                                                         
056800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
056900     SKIP3                                                                
057000*01 -COPY WMSGINIT                                                        
057100     EJECT                                                                
057200*01  FILLER  -COPY WSECAREA                                               
057300     EJECT                                                                
057400******************************************************************        
057500*                                                                         
057600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
057700*                                                                         
057800 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
057900                                                                          
058000*01  MID -COPY W4I71201                                                   
058100     EJECT                                                                
058200 01  FILLER                 PIC X(16) VALUE 'MID W4I79301 MID'.           
058300 01  4793-MID-IO-AREA.                                                    
058400                                                                          
058500     03  4793-MID-LL           PIC S9(4)   COMP SYNC.                     
058600     03  4793-MID-Z1           PIC X.                                     
058700     03  4793-MID-Z2           PIC X.                                     
058800     03  4793-MID-TRANSKOD     PIC X(8).                                  
058900     03  4793-MID-IDTRANS      PIC X(4).                                  
059000     03  4793-MID-KDMFSFOR     PIC X.                                     
059100     03  4793-MID-DATA-AREA    PIC X(34).                                 
059200*    03  MID -COPY W4I79301 -RED 4793-MID-DATA-AREA -PRE 4793-.           
059300     EJECT                                                                
059400*01  -COPY WMSGAREA                                                       
059500     EJECT                                                                
059600*  03  MOD -COPY W4O71201           -RED MSG-AREA.                        
059700     EJECT                                                                
059800*01  -COPY WMFSAREA                                                       
059900     EJECT                                                                
060000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
060100                                                                          
060200 01  REQU-AREA.                                                           
060300*    03  -COPY WZ01REQ2                                                   
060400*    03  -COPY W40712I1                                                   
060500                                                                          
060600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
060700                                                                          
060800 01  RESP-AREA.                                                           
060900*    03  -COPY WZ01RESP                                                   
061000*    03  -COPY W40712O1                                                   
061100                                                                          
061200******************************************************************        
061300*                                                                         
061400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
061500*                                                                         
061600 01  IMS-WS.                                                              
061700   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
061800                                                                          
061900*                        **** STATUS-KOD FRÅN IMS                         
062000   03  STATUS-WS                 PIC XX.                                  
062100     88  SEGMENT-FINNS                       VALUE '  '.                  
062200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
062300     88  END-OF-DATA                         VALUE 'GB'.                  
062400                                                                          
062500   03  GODK-STATUSKODER.                                                  
062600     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
062700                                                                          
062800 01  FILLER                      PIC X(16)   VALUE 'SSA:ER'.              
062900                                                                          
063000 01    SSA1                      PIC X(128).                              
063100 01    SSA2                      PIC X(128).                              
063200 01    SSA3                      PIC X(128).                              
063300     EJECT                                                                
063400*                            IMS FUNKTIONSKODER                           
063500*01    -COPY W0003                                                        
063600     EJECT                                                                
063700                                                                          
063800 01  FILLER                      PIC X(16)   VALUE 'DECAREA '.            
063900                                                                          
064000 01  DECAREA.                                                             
064100* 03  WDECAREA   -COPY WDECAREA                                           
064200     EJECT                                                                
064300*                            DLI INPUT-OUTPUT AREA                        
064400 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-WDA201'.          
064500                                                                          
064600 01  DLI-IO-AREA-WDA201.                                                  
064700*        05  -COPY WDA201                                                 
064800     EJECT                                                                
064900*                                                                         
065000 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-WDA211'.          
065100                                                                          
065200 01  DLI-IO-AREA-WDA211.                                                  
065300*        05  -COPY WDA211                                                 
065400     EJECT                                                                
065500*                                                                         
065600 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-WDA221'.          
065700                                                                          
065800 01  DLI-IO-AREA-WDA221.                                                  
065900*        05  -COPY WDA221                                                 
066000     EJECT                                                                
066100*                                                                         
066200 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-WDA301'.          
066300                                                                          
066400 01  DLI-IO-AREA-WDA301.                                                  
066500*        05  -COPY WDA301                                                 
066600     EJECT                                                                
066700*                                                                         
066800 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-P311'.            
066900                                                                          
067000 01  DLI-IO-AREA-P311.                                                    
067100*        05  -COPY WDP311                                                 
067200     EJECT                                                                
067300*                                                                         
067400 01  FILLER                PIC X(20) VALUE 'DLI-IO-WDL501'.               
067500                                                                          
067700 01  DLI-IO-WDL501.                                                       
067800*    03  -COPY WDL501                                                     
067900     EJECT                                                                
068000 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDL511'.               
068100 01  DLI-IO-WDL511.                                                       
068200*    03  -COPY WDL511                                                     
068300     EJECT                                                                
068400 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDL521'.               
068500 01  DLI-IO-WDL521.                                                       
068600*    03  -COPY WDL521                                                     
068700     EJECT                                                                
068800 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-WDK601'.          
068900                                                                          
069000 01  DLI-IO-AREA-WDK601.                                                  
069100*        05  -COPY WDK601                                                 
069200     EJECT                                                                
069300*                                                                         
069400 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-WDK611'.          
069500                                                                          
069600 01  DLI-IO-AREA-WDK611.                                                  
069700*        05  -COPY WDK611                                                 
069800     EJECT                                                                
069900*                                                                         
070000 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-WDD311'.          
070100                                                                          
070200 01  DLI-IO-AREA-WDD311.                                                  
070300*        05  -COPY WDD311                                                 
070400     EJECT                                                                
070500 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4110'.            
070600                                                                          
070700 01  DLI-IO-AREA-4110.                                                    
070800*        05  -COPY WDGX4110                                               
070900     EJECT                                                                
071000 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4109'.            
071100                                                                          
071200 01  DLI-IO-AREA-4109.                                                    
071300*        05  -COPY WDGX4109                                               
071400     EJECT                                                                
071500*                                                                         
071600 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4113'.            
071700                                                                          
071800 01  DLI-IO-AREA-4113.                                                    
071900*        05  -COPY WDGX4113                                               
072000     EJECT                                                                
072100*                                                                         
072200 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4114'.            
072300                                                                          
072400 01  DLI-IO-AREA-4114.                                                    
072500*        05  -COPY WDGX4114                                               
072600     EJECT                                                                
072700*                                                                         
072800 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4115'.            
072900                                                                          
073000 01  DLI-IO-AREA-4115.                                                    
073100*        05  -COPY WDGX4115                                               
073200     EJECT                                                                
073300*                                                                         
073400 01  FILLER                PIC X(20) VALUE 'DLI-IO-AREA-4116'.            
073500                                                                          
073600 01  DLI-IO-AREA-4116.                                                    
073700*        05  -COPY WDGX4116                                               
073800     EJECT                                                                
073900*                                                                         
074000 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDB101'.               
074100 01  DLI-IO-WDB101.                                                       
074200*     03  -COPY WDB101.                                                   
074300     EJECT                                                                
074400*                                                                         
074500 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDB201'.               
074600 01  DLI-IO-WDB201.                                                       
074700*     03  -COPY WDB201.                                                   
074800     EJECT                                                                
074900*                                                                         
075000 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX6327'.             
075100 01  DLI-IO-WDGX6327.                                                     
075200*    03  -COPY WDGX6327                                                   
075300     EJECT                                                                
075400*                                                                         
075500 01  FILLER               PIC X(16) VALUE 'DLI-IO-WDGX6328'.              
075600 01  DLI-IO-WDGX6328.                                                     
075700*    03  -COPY WDGX6328                                                   
075800     EJECT                                                                
075900*                                                                         
076000 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4103'.             
076100 01  DLI-IO-WDGX4103.                                                     
076200*    03  -COPY WDGX4103                                                   
076300     EJECT                                                                
076400*                                                                         
076500 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4104'.             
076600 01  DLI-IO-WDGX4104.                                                     
076700*    03  -COPY WDGX4104                                                   
076800     EJECT                                                                
076900                                                                          
077000 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDB601'.               
077100 01  DLI-IO-WDB601.                                                       
077200*    03  -COPY WDB601                                                     
077300     EJECT                                                                
077400 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDB611'.        
077500 01  DLI-IO-WDB611.                                                       
077600*    03  -COPY WDB611                                                     
077700     EJECT                                                                
077800 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK711'.        
077900 01  DLI-IO-WDK711.                                                       
078000*    03  -COPY WDK711                                                     
078100     EJECT                                                                
078200 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK712'.        
078300 01  DLI-IO-WDK712.                                                       
078400*    03  -COPY WDK712                                                     
078500     EJECT                                                                
078600 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK722'.        
078700 01  DLI-IO-WDK722.                                                       
078800*    03  -COPY WDK722                                                     
078900     EJECT                                                                
079000 LINKAGE SECTION.                                                         
079100*01  -COPY W0009     -PRE MSG-                                            
079200     EJECT                                                                
079300*01  -COPY W0009     -PRE ALT-                                            
079400     EJECT                                                                
079500*01  -COPY W0009     -PRE MAIL-                                           
079600     EJECT                                                                
079700*01  -COPY W0009     -PRE 4793-                                           
079800     EJECT                                                                
079900 01  ATAB-PCB                    PIC X.                                   
080000                                                                          
080100*01  -COPY W0008     -PRE USEA-                                           
080200     05  FILLER                  PIC X.                                   
080300     EJECT                                                                
080400*01  -COPY W0008     -PRE KREE-                                           
080500     05  FILLER                  PIC X.                                   
080600     EJECT                                                                
080700*01  -COPY W0008     -PRE WDL5-                                           
080800     05  FILLER                  PIC X.                                   
080900     EJECT                                                                
081000*01  -COPY W0008     -PRE ARTC-                                           
081100     05  FILLER                  PIC X.                                   
081200     EJECT                                                                
081300*01  -COPY W0008     -PRE BENA-                                           
081400     05  FILLER                  PIC X.                                   
081500     EJECT                                                                
081600*01  -COPY W0008     -PRE 4113-                                           
081700     05  FILLER                  PIC X.                                   
081800     EJECT                                                                
081900*01  -COPY W0008     -PRE WDP3-                                           
082000     05  FILLER                  PIC X.                                   
082100     EJECT                                                                
082200*01  -COPY W0008     -PRE 4109-                                           
082300     05  FILLER                  PIC X.                                   
082400     EJECT                                                                
082500*01  -COPY W0008     -PRE RETA-                                           
082600     05  FILLER                  PIC X.                                   
082700     EJECT                                                                
082800*01  -COPY W0008     -PRE 4115-                                           
082900     05  FILLER                  PIC X.                                   
083000     EJECT                                                                
083100*01  -COPY W0008     -PRE 4117-                                           
083200     05  FILLER                  PIC X.                                   
083300     EJECT                                                                
083400*01  -COPY W0008      -PRE WDB1-                                          
083500     05  FILLER                  PIC X.                                   
083600     EJECT                                                                
083700*01  -COPY W0008      -PRE WDB2-                                          
083800     05  FILLER                  PIC X.                                   
083900     EJECT                                                                
084000*01  -COPY W0008      -PRE 6327-                                          
084100     05  FILLER                  PIC X.                                   
084200     EJECT                                                                
084300*01  -COPY W0008      -PRE 4103-                                          
084400     05  FILLER                  PIC X.                                   
084500     EJECT                                                                
084600*01  -COPY W0008      -PRE WDB6-                                          
084700     05  FILLER                  PIC X.                                   
084800     EJECT                                                                
084900*01  -COPY W0008      -PRE WDK7-                                          
085000     05  FILLER                  PIC X.                                   
085100     EJECT                                                                
085200 01  KTL3-WDA8-PCB               PIC X.                                   
085300 01  KTL3-WDB2-PCB               PIC X.                                   
085400 01  KTL3-WDK6-PCB               PIC X.                                   
085500 01  KTL3-WDK7-PCB               PIC X.                                   
085600 01  KTL3-WDB6-PCB               PIC X.                                   
085700 01  KTL3-1165-PCB               PIC X.                                   
085800     EJECT                                                                
085900 PROCEDURE DIVISION USING  MSG-PCB                                        
086000                           ALT-PCB                                        
086100                           MAIL-PCB                                       
086200                           4793-PCB                                       
086300                           ATAB-PCB                                       
086400                           USEA-PCB                                       
086500                           KREE-PCB                                       
086600                           WDL5-PCB                                       
086700                           ARTC-PCB                                       
086800                           BENA-PCB                                       
086900                           4113-PCB                                       
087000                           WDP3-PCB                                       
087100                           4109-PCB                                       
087200                           RETA-PCB                                       
087300                           4115-PCB                                       
087400                           4117-PCB                                       
087500                           WDB1-PCB                                       
087600                           WDB2-PCB                                       
087700                           6327-PCB                                       
087800                           4103-PCB                                       
087900                           WDB6-PCB                                       
088000                           WDK7-PCB                                       
088100                      KTL3-WDA8-PCB                                       
088200                      KTL3-WDB2-PCB                                       
088300                      KTL3-WDK6-PCB                                       
088400                      KTL3-WDK7-PCB                                       
088500                      KTL3-WDB6-PCB                                       
088600                      KTL3-1165-PCB.                                      
088700 MAIN SECTION.                                                            
088800     ENTRY 'DLITCBL' USING MSG-PCB                                        
088900                           ALT-PCB                                        
089000                           MAIL-PCB                                       
089100                           4793-PCB                                       
089200                           ATAB-PCB                                       
089300                           USEA-PCB                                       
089400                           KREE-PCB                                       
089500                           WDL5-PCB                                       
089600                           ARTC-PCB                                       
089700                           BENA-PCB                                       
089800                           4113-PCB                                       
089900                           WDP3-PCB                                       
090000                           4109-PCB                                       
090100                           RETA-PCB                                       
090200                           4115-PCB                                       
090300                           4117-PCB                                       
090400                           WDB1-PCB                                       
090500                           WDB2-PCB                                       
090600                           6327-PCB                                       
090700                           4103-PCB                                       
090800                           WDB6-PCB                                       
090900                           WDK7-PCB                                       
091000                      KTL3-WDA8-PCB                                       
091100                      KTL3-WDB2-PCB                                       
091200                      KTL3-WDK6-PCB                                       
091300                      KTL3-WDK7-PCB                                       
091400                      KTL3-WDB6-PCB                                       
091500                      KTL3-1165-PCB.                                      
091600                                                                          
091700     EJECT                                                                
091800     PERFORM S17-FETCH-REQUEST-ARGUMENT                                   
091900     IF SUB-KDRC = 0                                                      
092000       PERFORM A-INIT                                                     
092100       PERFORM B-KOLLA-NYCKLAR                                            
092200       IF NYCKLAR-OK                                                      
092300         PERFORM I-SECURIT-KONTROLL                                       
092400         IF SEC-KDSVAR NOT = SEC-FELSVAR OR API-SW = 'Y'                  
092500            IF MFS-UPDATE OR MFS-UPD-V                                    
092600              MOVE MSGI-IDFTG  TO WS-IDFTG                                
092700              IF IDFTG-PV OR IDFTG-NON-VCC                                
092800                PERFORM G-GODK-ADM-KONTROLL                               
092900              ELSE                                                        
093000                MOVE JA TO INDATA-SW                                      
093100              END-IF                                                      
093200              IF INDATA-OK                                                
093300                PERFORM C-KOLLA-INDATA                                    
093400                IF INDATA-OK                                              
093500                   PERFORM D-UPPDATERA                                    
093600                   PERFORM E-KOLLA-AENDRA-STATUS                          
093700                   PERFORM S04-RENSA-RAD18                                
093800                ELSE                                                      
093900                  IF ANM-KDLEVANM > '0' AND < '4'                         
094000                    IF MED-IDMFSFEL = SPACE                               
094100                       MOVE ERR-UPDATE-NOT-OK TO MED-IDMFSFEL             
094200                    END-IF                                                
094300                    CALL WMEDKONV USING MED-WMEDAREA                      
094400                    MOVE MED-MFSFEL TO MOD-TEMFSFEL                       
094500                  ELSE                                                    
094600                    IF RETUR-FINNS-PA-RETTERM                             
094700                       MOVE ERR-DELETE-NOT-ALLOWED TO MED-IDMFSFEL        
094800                    ELSE                                                  
094900                     IF MED-IDMFSFEL = '333'                              
095000                        CONTINUE                                          
095100                      ELSE                                                
095200                        MOVE ERR-HIGHL-FIELDS TO MED-IDMFSFEL             
095300                      END-IF                                              
095400                    END-IF                                                
095500                    CALL WMEDKONV USING MED-WMEDAREA                      
095600                    MOVE MED-MFSFEL TO MOD-TEMFSFEL                       
095700                  END-IF                                                  
095800                END-IF                                                    
095900              END-IF                                                      
096000              PERFORM S01-LAES-WLKREE                                     
096100            ELSE                                                          
096200               IF MFS-PRINT                                               
096300                 PERFORM F-SKAPA-LEVANMLISTA                              
096400               END-IF                                                     
096500               PERFORM S01-LAES-WLKREE                                    
096600               PERFORM S04-RENSA-RAD18                                    
096700            END-IF                                                        
096800         ELSE                                                             
096900           MOVE ERR-UNAUTHORIZED TO MED-IDMFSFEL                          
097000           CALL WMEDKONV USING MED-WMEDAREA                               
097100           MOVE MED-MFSFEL      TO MOD-TEMFSFEL                           
097200         END-IF                                                           
097300       ELSE                                                               
097400          PERFORM H-RENSA-NYCKLAR                                         
097500          PERFORM S04-RENSA-RAD18                                         
097600       END-IF                                                             
097700       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71201 + 4                      
097800       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71201 + 4                      
097902        IF API-SW = 'N'                                                   
098000          PERFORM IMS-INSERT-MSG                                          
098100        ELSE                                                              
098200          PERFORM S18-RETURN-RESPONSE                                     
098300        END-IF                                                            
098400     END-IF                                                               
098500                                                                          
098600     MOVE ZERO                  TO RETURN-CODE                            
098700     GOBACK.                                                              
098800     EJECT                                                                
098900 A-INIT                         SECTION.                                  
099000                                                                          
099100*  IF CALL IS FROM CLASSIC SCREEN                                         
099200     IF SUB-KDTRANS(1:6) = 'W4T712'                                       
099300        MOVE SUB-KDTRANS                TO MSG-KDTRANS-1                  
099400        MOVE SUB-DATA                   TO MSG-AREA(9:1925)               
099500        IF MSG-DUBBLA-TRANSKODER                                          
099600          MOVE MSG-INDATA-MINUS-2-TRANSKODER                              
099700                                        TO MID-W4I71201                   
099800          MOVE MSG-IDTRANS-2            TO MFS-IDTRANS                    
099900          MOVE MSG-KDMFSFOR-2           TO MFS-KDMFSFOR                   
100000          MOVE MSG-KDTRTYP              TO MFS-KDTRTYP                    
100100          MOVE MSG-IDPFK                TO MFS-IDPFK                      
100200        ELSE                                                              
100300          MOVE MSG-INDATA-MINUS-1-TRANSKOD                                
100400                                        TO MID-W4I71201                   
100500          MOVE MSG-IDTRANS-1            TO MFS-IDTRANS                    
100600                                                                          
100700          MOVE MSG-KDMFSFOR-1           TO MFS-KDMFSFOR                   
100800                                                                          
100900          MOVE SPACE               TO MFS-KDTRTYP        MFS-IDPFK        
101000                                                                          
101100        END-IF                                                            
101200       MOVE MFS-IDTRANS                 TO WS-IDTRANS                     
101300                                                                          
101400     ELSE                                                                 
101500*  IF CALL IS FROM API                                                    
101600        MOVE SUB-DATA(1:SUB-KVDLEN)    TO REQU-AREA                       
101700        MOVE 001                       TO AUTH-KDCALL                     
101800        CALL WZ01AUTH               USING AUTH-WZ01AUTH                   
101900                                          REQU-WZ01REQ2                   
102000        IF AUTH-KDRC = 0                                                  
102100          IF REQU-KDPGMACT = 'S'                                          
102200            MOVE REQU-IDDISTR          TO MID-IDDISTR-IN                  
102300            MOVE REQU-IDKUNDNR         TO MID-IDKUNDNR-IN                 
102400            MOVE REQU-IDRAPPNR         TO MID-IDRAPPNR-IN                 
102500            MOVE 'Y'                   TO API-SW                          
102600            MOVE '4712'                TO WS-IDTRANS                      
102700          ELSE                                                            
102800            MOVE SYS-ERROR             TO RESP-IDMSG-ERROR                
102900          END-IF                                                          
103000        ELSE                                                              
103100          IF AUTH-KDRC = 4                                                
103200             MOVE BAD-REQUEST TO RESP-IDMSG-ERROR                         
103300                                                                          
103400             MOVE AUTH-KDRC TO KDRC-DISPLAY                               
103500             STRING 'WZ01AUTH GETARG ERROR RC=' KDRC-DISPLAY              
103600             DELIMITED BY SIZE INTO ERROR-TEXT                            
103700             CALL FELLOG USING RKOD-ABEND-WITH-DUMP                       
103800          END-IF                                                          
103900        END-IF                                                            
104000     END-IF                                                               
104100     MOVE LOW-VALUE             TO MSG-AREA                               
104200     MOVE 'W4O71201'            TO MFS-IDMOD                              
104300     MOVE '4712'                TO MOD-IDTRANS                            
104400     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL                           
104500                                   MOD-TEMFSINF                           
104600                                   MOD-IDDISTR-IN                         
104700                                   MOD-IDKUNDNR-IN                        
104800                                   MOD-IDRAPPNR-IN                        
104900                                   MOD-IDARTNR-IN                         
105000                                   MOD-IDRADNR-IN                         
105100                                                                          
105200     MOVE SPACE                 TO MED-IDMFSFEL                           
105300                                                                          
105400     ACCEPT WS-DATUM            FROM DATE                                 
105500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATUM-Y2K                     
105600                                                                          
105700     IF NOT EGEN-BILD                                                     
105800       MOVE SPACE               TO MFS-KDTRTYP                            
105900       IF NOT GODKAEND-BILD                                               
106000         MOVE '7'               TO MFS-IDPFK                              
106100       END-IF                                                             
106200     END-IF                                                               
106300     MOVE +1                    TO INDX                                   
106400     PERFORM UNTIL INDX      >  13                                        
106500        MOVE ZERO               TO MEAN-IDDISTR(INDX)                     
106600                                MEAN-IDKUNDNR(INDX)                       
106700                                MEAN-IDRAPPNR(INDX)                       
106800                                MEAN-IDARTNR(INDX)                        
106900                                MEAN-IDRADNR(INDX)                        
107000        MOVE SPACE              TO MEAN-KDKREBEH (INDX)                   
107100        MOVE +1                 TO MEAN-IX                                
107200        PERFORM UNTIL MEAN-IX > 3                                         
107301          MOVE SPACE         TO MEAN-TEANMNOT-ADM (INDX, MEAN-IX)         
107401                                MEAN-TEANMNOT-REM (INDX, MEAN-IX)         
107500          ADD +1                TO MEAN-IX                                
107600        END-PERFORM                                                       
107700        ADD +1                  TO INDX                                   
107800     END-PERFORM                                                          
107900                                                                          
108000     MOVE LOW-VALUE             TO W-IDGMT-MIN-X                          
108100                                                                          
108200     MOVE HIGH-VALUE            TO W-IDGMT-MAX-X                          
108300     .                                                                    
108400     EJECT                                                                
108500 B-KOLLA-NYCKLAR                SECTION.                                  
108600                                                                          
108700     MOVE ALL '+'               TO MSGI-WMSGINIT                          
108800     MOVE '001'                 TO MSGI-KDCALL                            
108900     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
109000     MOVE '4712'            TO MSGI-IDTRANS                               
109100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
109200     IF GODKAEND-BILD                                                     
109300       MOVE MID-IDDISTR-IN      TO MSGI-IDDISTR                           
109400       MOVE MID-IDKUNDNR-IN     TO MSGI-IDKUNDNR                          
109500       MOVE MID-IDRAPPNR-IN     TO MSGI-IDRAPPNR                          
109600       IF MID-IDARTNR-IN  = ALL '+'                                       
109700          MOVE '+++++++++'      TO MSGI-IDARTNR                           
109800       ELSE                                                               
109900          MOVE MID-IDARTNR-IN   TO WS-IDARTNR                             
110000          MOVE WS-IDARTNR       TO MSGI-IDARTNR                           
110100       END-IF                                                             
110200       MOVE MID-IDRADNR-IN      TO MSGI-IDRADNR                           
110300     END-IF                                                               
110400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
110500                                                                          
110600     IF MSGI-IDLAND-SPR = 'GB'                                            
110700       MOVE 'GB'                TO MED-IDSKYLT                            
110800     ELSE                                                                 
110900       MOVE 'S '                TO MED-IDSKYLT                            
111000     END-IF                                                               
111100                                                                          
111200     IF MID-IDDISTR-IN         NOT = ALL '+'                              
111300        MOVE '7'                TO MFS-IDPFK                              
111400        MOVE SPACE              TO MFS-KDTRTYP                            
111500     END-IF                                                               
111600                                                                          
111700     IF  MID-IDKUNDNR-IN         NOT = ALL '+'                            
111800        MOVE '7'                TO MFS-IDPFK                              
111900        MOVE SPACE              TO MFS-KDTRTYP                            
112000     END-IF                                                               
112100                                                                          
112200     IF MID-IDRAPPNR-IN         NOT = ALL '+'                             
112300        MOVE '7'                TO MFS-IDPFK                              
112400        MOVE SPACE              TO MFS-KDTRTYP                            
112500     END-IF                                                               
112600                                                                          
112700     IF MID-IDARTNR-IN           = ALL '+'                                
112800        MOVE MID-IDARTNR-UT     TO IDARTNR-WS                             
112900     ELSE                                                                 
113000        MOVE MID-IDARTNR-IN     TO IDARTNR-WS                             
113100        MOVE '7'                TO MFS-IDPFK                              
113200        MOVE SPACE              TO MFS-KDTRTYP                            
113300     END-IF                                                               
113400                                                                          
113500     IF MID-IDRADNR-IN           = ALL '+'                                
113600        MOVE MID-IDRADNR-UT     TO IDRADNR-WS                             
113700     ELSE                                                                 
113800        IF MID-IDRADNR-IN NUMERIC                                         
113900          MOVE MID-IDRADNR-IN     TO IDRADNR-WS                           
114000          MOVE '7'                TO MFS-IDPFK                            
114100          MOVE SPACE              TO MFS-KDTRTYP                          
114200        ELSE                                                              
114300          MOVE 0                  TO IDRADNR-WS                           
114400        END-IF                                                            
114500     END-IF                                                               
114600                                                                          
114700     IF MID-IDKUNDNR-IN          = ALL '+' AND                            
114800        MID-IDDISTR-IN NOT       = ALL '+'                                
114900        MOVE ZERO               TO IDKUNDNR-WS                            
115000     END-IF                                                               
115100                                                                          
115200     INSPECT MID-IDARTNR-ENTER REPLACING ALL SPACE BY ZERO                
115300     INSPECT MID-IDRADNR-NEXT  REPLACING ALL SPACE BY ZERO                
115400     EJECT                                                                
115500                                                                          
115600     MOVE JA                    TO NYCKLAR-SW                             
115700     MOVE MSGI-IDDISTR          TO IDDISTR-WS                             
115800     MOVE MSGI-IDKUNDNR         TO IDKUNDNR-WS                            
115900     MOVE MSGI-IDRAPPNR         TO IDRAPPNR-WS                            
116000     MOVE MSGI-IDFTG            TO W-IDFTG-4109                           
116100                                                                          
116200     INSPECT IDDISTR-WS  REPLACING ALL SPACE BY ZERO                      
116300     IF IDDISTR-WS NUMERIC                                                
116400        MOVE IDDISTR-WS         TO W-IDDISTR                              
116500                                   W-IDDISTR-A3                           
116600                                   W-IDDISTR-WDB2                         
116700                                   W-IDDISTR-WDB2-MIN                     
116800                                   W-IDDISTR-WDB2-MAX                     
116900                                   TEST-IDDISTR                           
117000                                   W-IDDISTR-4103                         
117100                                   RESP-IDDISTR                           
117200     ELSE                                                                 
117300        MOVE NEJ                TO NYCKLAR-SW                             
117400        MOVE BAD-REQUEST                   TO RESP-IDMFSINF               
117500        MOVE 'WRONG DISTRICT '                                            
117600                                           TO RESP-TEMFSINF               
117700     END-IF                                                               
117800                                                                          
117900     INSPECT IDKUNDNR-WS  REPLACING ALL SPACE BY ZERO                     
118000     IF IDKUNDNR-WS NUMERIC                                               
118100        MOVE IDKUNDNR-WS        TO W-IDKUNDNR                             
118200                                   W-IDKUNDNR-A3                          
118300                                   W-IDKUNDNR-WDB2                        
118400                                   W-IDKUNDNR-4103                        
118500                                   RESP-IDKUNDNR                          
118600     ELSE                                                                 
118700        MOVE NEJ                TO NYCKLAR-SW                             
118800        MOVE BAD-REQUEST                   TO RESP-IDMFSINF               
118900        MOVE 'WRONG CUSTOMER '                                            
119000                                           TO RESP-TEMFSINF               
119100     END-IF                                                               
119200                                                                          
119300     INSPECT IDRAPPNR-WS  REPLACING ALL SPACE BY ZERO                     
119400                                                                          
119500     IF IDRAPPNR-WS NUMERIC                                               
119600        MOVE IDRAPPNR-WS        TO W-IDRAPPNR                             
119700                                   W-IDRAPPNR-A3                          
119800                                   W-IDRAPPNR-4103                        
119900                                   RESP-IDRAPPNR                          
120000     ELSE                                                                 
120100        MOVE NEJ                TO NYCKLAR-SW                             
120200        MOVE BAD-REQUEST                   TO RESP-IDMFSINF               
120300        MOVE 'WRONG DISCREPANCY REPORTNO '                                
120400                                           TO RESP-TEMFSINF               
120500     END-IF                                                               
120600                                                                          
120700     IF MID-IDARTNR-ENTER NOT NUMERIC                                     
120800        MOVE ZERO               TO MID-IDARTNR-ENTER                      
120900     END-IF                                                               
121000     IF MID-IDRADNR-ENTER NOT NUMERIC                                     
121100        MOVE ZERO               TO MID-IDRADNR-ENTER                      
121200     END-IF                                                               
121300     IF MID-IDARTNR-NEXT NOT NUMERIC                                      
121400        MOVE ZERO               TO MID-IDARTNR-NEXT                       
121500     END-IF                                                               
121600     IF MID-IDRADNR-NEXT NOT NUMERIC                                      
121700        MOVE ZERO               TO MID-IDRADNR-NEXT                       
121800     END-IF                                                               
121900     MOVE IDDISTR-WS            TO MOD-IDDISTR-UT                         
122000     MOVE IDKUNDNR-WS           TO MOD-IDKUNDNR-UT                        
122100     MOVE IDRAPPNR-WS           TO MOD-IDRAPPNR-UT                        
122200     IF IDARTNR-WS              NUMERIC                                   
122300       MOVE IDARTNR-WS          TO MOD-IDARTNR-UT                         
122400     END-IF                                                               
122500     IF IDRADNR-WS              NUMERIC                                   
122600       MOVE IDRADNR-WS          TO MOD-IDRADNR-UT                         
122700     END-IF                                                               
122800     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
122900     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
123000     INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE              
123100     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
123200     INSPECT MOD-IDRADNR-UT  REPLACING LEADING ZERO BY SPACE              
123300     IF MOD-IDKUNDNR-UT          = SPACE                                  
123400        MOVE '     0'           TO MOD-IDKUNDNR-UT                        
123500     END-IF                                                               
123600     .                                                                    
123700     EJECT                                                                
123800 C-KOLLA-INDATA                 SECTION.                                  
123900                                                                          
124000     MOVE NEJ                   TO DUMMY-NR-SW                            
124100     MOVE JA                    TO INDATA-SW                              
124200     MOVE +1                    TO IX                                     
124300     PERFORM IMS-GET-KREE01-KVAL                                          
124400     IF SEGMENT-FINNS                                                     
124500        IF ANM-KDLEVANM < '1' OR > '3'                                    
124600           MOVE NEJ             TO INDATA-SW                              
124700           IF ANM-KDLEVANM = '4'                                          
124800              PERFORM CL-KOLLA-R78                                        
124900           END-IF                                                         
125000        ELSE                                                              
125100           PERFORM CE-KOLLA-KANTKOD                                       
125200           IF MID-RAD18-IDARTNR = ALL '+' OR                              
125300                       MID-RAD18-IDRADNR = ALL '+'                        
125400              CONTINUE                                                    
125500           ELSE                                                           
125600             IF ANM-KDLEVATT = 2                                          
125700               MOVE NEJ      TO INDATA-SW                                 
125800               MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                
125900             ELSE                                                         
126000               IF MID-RAD18-IDARTNR NUMERIC AND                           
126100                            MID-RAD18-IDRADNR NUMERIC                     
126200                 PERFORM CA-FINNS-RAD-PA-SKARMEN                          
126300                 IF RAD-FINNS-PA-SKARMEN                                  
126400                   MOVE MID-RAD18-IDARTNR TO W-IDARTNR                    
126500                                             W-IDARTNR-4109-MIN           
126600                                             W-IDARTNR-4109-MAX           
126700                   MOVE MID-RAD18-IDRADNR TO W-IDRADNR                    
126800                   PERFORM IMS-GET-KREE11-KVAL                            
126900                   IF LEV-IDARTNR = DUMMY-IDARTNR                         
127000                     MOVE NEJ      TO INDATA-SW                           
127100                     MOVE  JA      TO DUMMY-NR-SW                         
127200                   ELSE                                                   
127300                     IF MID-RAD18-PRARTBTO = ALL '+'                      
127400                       CONTINUE                                           
127500                     ELSE                                                 
127600*-20111019 KINA FÅR INTE LOV ATT ÄNDRA PRIS PÅ LEV.ANM. RAD.              
127700*-20190826 KOREA DOESN'T ALLOWED TO CHANGE PRICE ON DISCR.LINE            
127800*-20211130 TURKEY, BRAZIL, MEXICO, SOUTH AFRICA                           
127900*-20211130 DOESN'T ALLOWED TO CHANGE PRICE ON DISCR.LINE                  
128000                       MOVE MSGI-IDFTG   TO WS-IDFTG                      
128100                       IF (LEV-KDANMORS = '74') OR                        
128200                           IDFTG-NON-VCC                                  
128300                          MOVE NEJ      TO INDATA-SW                      
128400                          MOVE MFS-NUM-FAELT-FEL TO                       
128500                               MOD-RAD18-PRARTBTO-ATTRIBUT                
128600                   MOVE MFS-DO-NOT-TOUCH-FIELD TO                         
128700                                          MOD-RAD18-IDARTNR               
128800                                          MOD-RAD18-IDRADNR               
128900                                          MOD-RAD18-PRARTBTO              
129000                       ELSE                                               
129100                          PERFORM CF-KOLLA-PRIS                           
129200                       END-IF                                             
129300                     END-IF                                               
129400                     IF MID-RAD18-KDANMORS = ALL '+'                      
129500                       CONTINUE                                           
129600                     ELSE                                                 
129700                       PERFORM CG-KOLLA-KDANMORS                          
129800                     END-IF                                               
129900                     IF MID-RAD18-KVLEVANM = ALL '+'                      
130000                       CONTINUE                                           
130100                     ELSE                                                 
130200                        PERFORM CH-KOLLA-ANTAL                            
130300                     END-IF                                               
130400                     IF MID-RAD18-FLDIRLEV = '+'                          
130500                        CONTINUE                                          
130600                     ELSE                                                 
130700                        PERFORM CJ-KOLLA-DIREKTLEVERANS-FLAGGA            
130800                     END-IF                                               
130900                     IF MID-RAD18-KVLEVANM NUMERIC                        
131000                       MOVE MID-RAD18-KVLEVANM TO KVLEVANM-WS             
131100                       IF MID-RAD18-KDANMORS NUMERIC                      
131200                         MOVE MID-RAD18-KDANMORS TO KDANMORS-WS           
131300                         IF DEC-KDSVAR-OK                                 
131400                           MOVE 4  TO KDKREBEH-SPAR-2                     
131500                         ELSE                                             
131600                           MOVE 2  TO KDKREBEH-SPAR-2                     
131700                         END-IF                                           
131800                       ELSE                                               
131900                         IF DEC-KDSVAR-OK                                 
132000                           MOVE 3  TO KDKREBEH-SPAR-2                     
132100                         ELSE                                             
132200                           MOVE 1  TO KDKREBEH-SPAR-2                     
132300                         END-IF                                           
132400                       END-IF                                             
132500                     ELSE                                                 
132600                       IF MID-RAD18-KDANMORS NUMERIC                      
132700                         MOVE MID-RAD18-KDANMORS TO KDANMORS-WS           
132800                         IF DEC-KDSVAR-OK                                 
132900                           MOVE 7  TO KDKREBEH-SPAR-2                     
133000                         ELSE                                             
133100                           MOVE 5  TO KDKREBEH-SPAR-2                     
133200                         END-IF                                           
133300                       ELSE                                               
133400                         IF DEC-KDSVAR-OK                                 
133500                           MOVE 6  TO KDKREBEH-SPAR-2                     
133600                         END-IF                                           
133700                       END-IF                                             
133800                     END-IF                                               
133900                   END-IF                                                 
134000                 ELSE                                                     
134100                   MOVE NEJ        TO INDATA-SW                           
134200                   MOVE MFS-NUM-FAELT-FEL TO                              
134300                                        MOD-RAD18-IDARTNR-ATTRIBUT        
134400                                        MOD-RAD18-IDRADNR-ATTRIBUT        
134500                   MOVE MFS-DO-NOT-TOUCH-FIELD TO                         
134600                                          MOD-RAD18-IDARTNR               
134700                                          MOD-RAD18-IDRADNR               
134800                 END-IF                                                   
134900               ELSE                                                       
135000                  MOVE NEJ         TO INDATA-SW                           
135100                  MOVE MFS-NUM-FAELT-FEL TO                               
135200                                       MOD-RAD18-IDARTNR-ATTRIBUT         
135300                                       MOD-RAD18-IDRADNR-ATTRIBUT         
135400                  MOVE MFS-DO-NOT-TOUCH-FIELD TO                          
135500                                          MOD-RAD18-IDARTNR               
135600                                          MOD-RAD18-IDRADNR               
135700               END-IF                                                     
135800             END-IF                                                       
135900           END-IF                                                         
136000                                                                          
136100           MOVE MSGI-IDFTG    TO WS-IDFTG                                 
136202           IF DIST79-DEALER-PRICE  OR                                     
136400              IDFTG-US OR IDFTG-CA OR                                     
136500              IDFTG-CN OR IDFTG-IN OR                                     
136600              IDFTG-KR OR IDFTG-TR OR                                     
136700              IDFTG-MX OR IDFTG-BR OR                                     
136800              IDFTG-MY OR IDFTG-TH OR IDFTG-TW                            
136810                       OR IDFTG-ZA                                        
136900             PERFORM S10-HAMTA-KDVALISO                                   
137000           ELSE                                                           
137100             IF DIST79-ECOM-PRICE                                         
137200               MOVE SPACE           TO MOD-KDVALISO                       
137300             ELSE                                                         
137400               MOVE 'SEK'           TO MOD-KDVALISO                       
137500             END-IF                                                       
137600           END-IF                                                         
137700        END-IF                                                            
137800     ELSE                                                                 
137900        MOVE NEJ     TO INDATA-SW                                         
138000     END-IF                                                               
138100     .                                                                    
138200     EJECT                                                                
138300 CA-FINNS-RAD-PA-SKARMEN        SECTION.                                  
138400                                                                          
138500     MOVE +1                    TO IX                                     
138600     MOVE NEJ                   TO RAD-FINNS-SW                           
138700     PERFORM UNTIL IX > MAX-IX OR RAD-FINNS-PA-SKARMEN                    
138800        INSPECT MID-IDARTNR (IX) REPLACING LEADING SPACE BY ZERO          
138900        INSPECT MID-IDRADNR (IX) REPLACING LEADING SPACE BY ZERO          
139000        IF MID-RAD18-IDARTNR = MID-IDARTNR (IX)                           
139100           IF MID-RAD18-IDRADNR = MID-IDRADNR (IX)                        
139200              MOVE JA           TO RAD-FINNS-SW                           
139300           END-IF                                                         
139400        END-IF                                                            
139500        ADD +1                  TO IX                                     
139600     END-PERFORM                                                          
139700     .                                                                    
139800     EJECT                                                                
139900 CB-KOLLA-KDANMORS              SECTION.                                  
140000                                                                          
140100*       ********************************************************          
140200*       * EJ TILLÅTET ATT UPPDATERA ORSAKSKODENS FÖRSTA SIFFRA *          
140300*       * TILL NÅGOT ANNAT ÄN VAD SOM REDAN LIGGER PÅ REGISTRET*          
140400*       *                                                      *          
140500*       * ORSAKSKOD 12,22,23,27,28 FÅR EJ HA PRIS              *          
140600*       *                                                      *          
140700*       * ORSAKSKOD 72 FÖR LDC-KUND FÅR EJ ÄNDRAS.             *          
140800*       *                                                      *          
140900*       * ORSAKSKOD 74 FÖR N-FAKT.  FÅR EJ ÄNDRAS.             *          
141000*       *                                                      *          
141100*       * ORSAKSKOD 20,30,31,42,43,52,53,62,63,72,73,92,93,98  *          
141200*       *           80,82,83,75,25,74                          *          
141300*       *           MÅSTE HA PRIS                              *          
141400*       * ORSAKSKOD 52 MÅSTE HA FÖRETAGSKOD OCH ANALYSNR       *          
141500*       *           53 MÅSTE HA FÖRETAGSKOD OCH KONTO,         *          
141600*       *              KOSTN.STÄLLE OCH ANALYSNR               *          
141700*       * ORSAKSKOD 42,43 KAN EJ ÄNDRAS FRÅN NÅGOT ANNAT ÄN    *          
141800*       *           43 RESP 42 .SKALL HA EMB.KOD               *          
141900*       * ORSAKSKOD 82,83 KAN EJ ÄNDRAS FRÅN NÅGOT ANNAT ÄN    *          
142000*       *           83 RESP 82                                 *          
142100*       * ORSAKSKOD 20,30,42,43,72,80,82,83 MÅSTE HA FAKTTYP,  *          
142200*       *           FAKTNR OCH FAKTDAT.ÄVEN KOD 00,60,62,75,25 *          
142300*       * ORSAKSKOD 80,82,83 MÅSTE HA ANTAL                    *          
142400*       *                                                      *          
142500*       * RETURKODER KOLLAS MOT W418KTL3 RETUR-MATRIX OM OK.   *          
142600*       ********************************************************          
142700                                                                          
142800     MOVE MID-RAD18-KDANMORS    TO OKOD-KDANMORS                          
142900                                   W-KDANMORS-4109-MIN                    
143000                                   W-KDANMORS-4109-MAX                    
143100                                                                          
143200     CALL W418OKOD USING OKOD-W418OKOD                                    
143300                                                                          
143400     IF OKOD-FL-PRIS-ZERO = JA                                            
143500        MOVE LEV-IDFTG   TO WS-IDFTG                                      
143600        IF IDFTG-PV                                                       
143700          IF DIST79-DEALER-PRICE OR                                       
143900             DIST79-ECOM-PRICE                                            
144000            IF LEV-PRARTBTO-LOC = ZERO AND                                
144100                         MID-RAD18-PRARTBTO = ALL '+'                     
144200             CONTINUE                                                     
144300            ELSE                                                          
144400              IF WS-IDEDITDATA > ZERO OR                                  
144500                       LEV-PRARTBTO-LOC > ZERO                            
144600                 MOVE FEL      TO KDANMORS-UPPD                           
144700              END-IF                                                      
144800            END-IF                                                        
144900          ELSE                                                            
145000            IF LEV-PRARTBTO = ZERO AND                                    
145100                         MID-RAD18-PRARTBTO = ALL '+'                     
145200              CONTINUE                                                    
145300            ELSE                                                          
145400              IF WS-IDEDITDATA > ZERO OR                                  
145500                         LEV-PRARTBTO > ZERO                              
145600                 MOVE FEL        TO KDANMORS-UPPD                         
145700              END-IF                                                      
145800            END-IF                                                        
145900          END-IF                                                          
146000        ELSE                                                              
146100          IF IDFTG-US OR IDFTG-CA OR IDFTG-CN OR IDFTG-IN OR              
146200             IDFTG-KR OR IDFTG-TR OR IDFTG-MY OR IDFTG-TH OR              
146300             IDFTG-TW OR IDFTG-MX OR IDFTG-BR OR IDFTG-ZA                 
146400**- USA SER VIPS-FAKTURA-PRISET PÅ 4712 - PRISÄNDRING STANNAR HÄR         
146500**- IDAG. DEN GÅR INTE ÖVER TILL LAB. BARA PRARTBTO !!! SE W41830         
146600**- 2011-10-19 SAMMA SAK GÄLLER FÖR KINA. FÅR EJ ÄNDRA PRIS.              
146700            IF LEV-PRARTBTO-LOCINV = ZERO AND                             
146800                         MID-RAD18-PRARTBTO = ALL '+'                     
146900             CONTINUE                                                     
147000            ELSE                                                          
147100              IF WS-IDEDITDATA > ZERO OR                                  
147200                       LEV-PRARTBTO-LOCINV > ZERO                         
147300                 MOVE FEL      TO KDANMORS-UPPD                           
147400              END-IF                                                      
147500            END-IF                                                        
147600          END-IF                                                          
147700        END-IF                                                            
147800     END-IF                                                               
147900                                                                          
148000     IF OKOD-FL-GODK-PRIS-ZERO = NEJ                                      
148100        MOVE LEV-IDFTG   TO WS-IDFTG                                      
148200        IF IDFTG-PV                                                       
148300          IF DIST79-DEALER-PRICE OR                                       
148500             DIST79-ECOM-PRICE                                            
148600            IF LEV-PRARTBTO-LOC   = ZERO AND                              
148700                           MID-RAD18-PRARTBTO = ALL '+'                   
148800               MOVE FEL           TO KDANMORS-UPPD                        
148900            END-IF                                                        
149000          ELSE                                                            
149100            IF LEV-PRARTBTO   = ZERO AND                                  
149200                           MID-RAD18-PRARTBTO = ALL '+'                   
149300               MOVE FEL           TO KDANMORS-UPPD                        
149400            END-IF                                                        
149500          END-IF                                                          
149600        ELSE                                                              
149700          IF IDFTG-US OR IDFTG-CA OR IDFTG-CN OR IDFTG-IN OR              
149800             IDFTG-KR OR IDFTG-TR OR IDFTG-MY OR IDFTG-TH OR              
149900             IDFTG-TW OR IDFTG-MX OR IDFTG-BR OR IDFTG-ZA                 
150000            IF LEV-PRARTBTO-LOCINV = ZERO AND                             
150100                           MID-RAD18-PRARTBTO = ALL '+'                   
150200               MOVE FEL         TO KDANMORS-UPPD                          
150300            END-IF                                                        
150400          END-IF                                                          
150500        END-IF                                                            
150600     END-IF                                                               
150700                                                                          
150800     IF OKOD-FL-IDFAKT = JA                                               
150900        IF  LEV-IDFAKT         = ZERO                                     
151000          MOVE FEL             TO KDANMORS-UPPD                           
151100        END-IF                                                            
151200     END-IF                                                               
151300                                                                          
151400     IF OKOD-FL-KDFAKTYP-R = JA                                           
151500        IF  LEV-KDFAKTYP       = SPACE                                    
151600           MOVE FEL             TO KDANMORS-UPPD                          
151700        END-IF                                                            
151800     END-IF                                                               
151900                                                                          
152000     IF OKOD-FL-ANALYSNR = JA                                             
152100       PERFORM  S06-LAS-ANALYSNRREGISTRET                                 
152200                                                                          
152300        IF WL410901-FINNS                                                 
152400           CONTINUE                                                       
152500        ELSE                                                              
152600           MOVE FEL                  TO KDANMORS-UPPD                     
152700           MOVE ERR-INF-MISS-4702    TO MED-IDMFSFEL                      
152800        END-IF                                                            
152900     END-IF                                                               
153000                                                                          
153100*-KOD 70 FÅR ENBART ANVÄNDAS FÖR SOFTWARE-ARTIKEL.                        
153200     IF MID-RAD18-KDANMORS = '70'                                         
153300       IF  LEV-KDANMORS       = '75' OR '73' OR '72' OR '74'              
153400         MOVE FEL             TO KDANMORS-UPPD                            
153500       END-IF                                                             
153600     END-IF                                                               
153700                                                                          
153800     IF MID-RAD18-KDANMORS = '72' OR '73' OR '75' OR '74'                 
153900       IF  LEV-KDANMORS       = '70'                                      
154000         MOVE FEL             TO KDANMORS-UPPD                            
154100       END-IF                                                             
154200     END-IF                                                               
154300                                                                          
154400*-KOD 74 FÅR ENBART ANVÄNDAS FÖR INTERNKUNDER MED N-FAKTURA.              
154500     IF MID-RAD18-KDANMORS = '74'                                         
154600       IF  LEV-KDANMORS       = '75' OR '73' OR '72'                      
154700         MOVE FEL             TO KDANMORS-UPPD                            
154800       END-IF                                                             
154900     END-IF                                                               
155000                                                                          
155100     IF MID-RAD18-KDANMORS = '72' OR '73' OR '75'                         
155200       IF  LEV-KDANMORS       = '74'                                      
155300         MOVE FEL             TO KDANMORS-UPPD                            
155400       END-IF                                                             
155500     END-IF                                                               
155600                                                                          
155700*- 2006-01-30 OM DET ÄR EN LDC-KUND FÅR EJ KOD 72 ÄNDRAS.                 
155800     IF MID-RAD18-KDANMORS  = '73' OR '75' OR '72'                        
155900       PERFORM IMS-GU-GMTA-WDB201                                         
156000       IF SEGMENT-FINNS                                                   
156100         CONTINUE                                                         
156200       ELSE                                                               
156300         PERFORM IMS-GET-WDB201                                           
156400       END-IF                                                             
156500       IF GMT-FLLDCKND = JA OR                                            
156600          GMT-FLRETUR  = JA                                               
156700         IF MID-RAD18-KDANMORS = '72'                                     
156800           IF  LEV-KDANMORS       = '75' OR '73'                          
156900             MOVE FEL             TO KDANMORS-UPPD                        
157000           END-IF                                                         
157100         END-IF                                                           
157200         IF MID-RAD18-KDANMORS = '73' OR '75'                             
157300           IF  LEV-KDANMORS       = '72'                                  
157400             MOVE FEL             TO KDANMORS-UPPD                        
157500           END-IF                                                         
157600         END-IF                                                           
157700       END-IF                                                             
157800     END-IF                                                               
157900                                                                          
158000     IF MID-RAD18-KDANMORS = '42'                                         
158100        IF  LEV-KDANMORS       = '43' OR '62'                             
158200           CONTINUE                                                       
158300        ELSE                                                              
158400           MOVE FEL               TO KDANMORS-UPPD                        
158500        END-IF                                                            
158600     END-IF                                                               
158700                                                                          
158800     IF MID-RAD18-KDANMORS = '43' OR '62'                                 
158900        IF  LEV-KDANMORS       = '42'                                     
159000           CONTINUE                                                       
159100        ELSE                                                              
159200           IF LEV-KDANMORS       = '63' AND                               
159300              MID-RAD18-KDANMORS = '62'                                   
159400               CONTINUE                                                   
159500           ELSE                                                           
159600               MOVE FEL               TO KDANMORS-UPPD                    
159700           END-IF                                                         
159800        END-IF                                                            
159900     END-IF                                                               
160000                                                                          
160100     IF MID-RAD18-KDANMORS = '82' OR '83'                                 
160200        IF  LEV-KDANMORS       = '82' OR '83'                             
160300           CONTINUE                                                       
160400        ELSE                                                              
160500           MOVE FEL             TO KDANMORS-UPPD                          
160600        END-IF                                                            
160700     END-IF                                                               
160800                                                                          
160900*- KOD 20 FÅR BARA ÄNDRAS TILL 25 (=KOD 20 MEN EJ LAGERAVBOKNING).        
161000*- KOD 22 FÅR BARA ÄNDRAS TILL 26,27 OCH 28 FÖR IDFTG=57, EJ NA.          
161100     IF MID-RAD18-KDANMORS = '25' OR '26' OR '27' OR '28'                 
161200       MOVE LEV-IDFTG  TO WS-IDFTG                                        
161300                                                                          
161400       IF DIST07-USA-RET-DISCR  OR                                        
161500          DIST07-CAN-RET-DISCR  OR                                        
161600          DIST35-REFILL-NA-JAP  OR                                        
161700          DIST35-REFILL-NA      OR                                        
161800          DIST35-REFILL-CN      OR                                        
161900          DIST35-CDC-IN-REFILL  OR                                        
162000          DIST35-CDC-KR-REFILL  OR                                        
162100          DIST35-CDC-TR-REFILL  OR                                        
162200          DIST35-CDC-BR-REFILL  OR                                        
162300          DIST35-CDC-ZA-REFILL  OR                                        
162400          DIST35-CDC-MX-REFILL  OR                                        
162500          DIST35-CDC-AE-REFILL  OR                                        
162600          DIST35-NONVCC-NONVCC-REFILL OR                                  
162700          DIST35-NONVCC-NONVCC-TRANSFER OR                                
162800          DIST35-CDC-MY-REFILL  OR                                        
162900          DIST35-CDC-TH-REFILL  OR                                        
163000          DIST35-CDC-TW-REFILL  OR                                        
163100          IDFTG-US OR IDFTG-CA                                            
163200                                                                          
163300         MOVE FEL             TO KDANMORS-UPPD                            
163400       ELSE                                                               
163500         IF MID-RAD18-KDANMORS = '20' OR '25'                             
163600           IF  LEV-KDANMORS       = '20' OR '25'                          
163700             CONTINUE                                                     
163800           ELSE                                                           
163900             MOVE FEL             TO KDANMORS-UPPD                        
164000           END-IF                                                         
164100         END-IF                                                           
164200                                                                          
164300         IF MID-RAD18-KDANMORS = '26' OR '27' OR '28'                     
164400           IF  LEV-KDANMORS       = '21' OR '22' OR '23'                  
164500             CONTINUE                                                     
164600           ELSE                                                           
164700             MOVE FEL             TO KDANMORS-UPPD                        
164800           END-IF                                                         
164900         END-IF                                                           
165000       END-IF                                                             
165100     END-IF                                                               
165200                                                                          
165300     MOVE LEV-KDANMORS          TO W-LEV-KDANMORS                         
165400     MOVE MID-RAD18-KDANMORS    TO W-RAD18-KDANMORS                       
165500     IF W-RAD18-KDANMORS-POS1 = W-LEV-KDANMORS-POS1                       
165600       CONTINUE                                                           
165700     ELSE                                                                 
165800       IF MID-RAD18-KDANMORS = '00' OR '60'                               
165900          IF  LEV-KDANMORS       = '00' OR '60'                           
166000             CONTINUE                                                     
166100          ELSE                                                            
166200             MOVE FEL           TO KDANMORS-UPPD                          
166300          END-IF                                                          
166400       ELSE                                                               
166500          IF MID-RAD18-KDANMORS = '42' OR '62'                            
166600            IF  LEV-KDANMORS       = '42' OR '62'                         
166700               CONTINUE                                                   
166800            ELSE                                                          
166900               MOVE FEL         TO KDANMORS-UPPD                          
167000            END-IF                                                        
167100          ELSE                                                            
167200            MOVE FEL            TO KDANMORS-UPPD                          
167300          END-IF                                                          
167400       END-IF                                                             
167500     END-IF                                                               
167600                                                                          
167700*- KOLLA MOT RETUR MATRIXEN.                                              
167800     IF LEV-IDARTNR = DUMMY-IDARTNR                                       
167900       CONTINUE                                                           
168000     ELSE                                                                 
168100       IF OKOD-FL-RETILL = JA                                             
168200         MOVE NEJ                TO SW-KDKREBEH-Q                         
168300         MOVE NEJ                TO SW-KDKREBEH-P                         
168400                                                                          
168500         MOVE PROGRAM-NAMN       TO KTL3-IDPGM                            
168600         MOVE LEV-IDARTNR        TO KTL3-IDARTNR                          
168700         MOVE MID-RAD18-KDANMORS TO KTL3-KDANMORS                         
168800         MOVE LEV-IDDC           TO KTL3-IDDC                             
168900         MOVE MSGI-IDDISTR       TO KTL3-IDDISTR                          
169000         MOVE MSGI-IDKUNDNR      TO KTL3-IDKUNDNR                         
169100         CALL W418KTL3 USING  KTL3-W418KTL3 KTL3-WDA8-PCB                 
169200                                            KTL3-WDB2-PCB                 
169300                                            KTL3-WDK6-PCB                 
169400                                            KTL3-WDK7-PCB                 
169500                                            KTL3-WDB6-PCB                 
169600                                            KTL3-1165-PCB                 
169700                                                                          
169800         IF KTL3-KDSVAR = YES                                             
169900           IF KTL3-KDRETBEH = 'S'                                         
170000             MOVE FEL                TO KDANMORS-UPPD                     
170100           END-IF                                                         
170200           IF KTL3-KDRETBEH = 'Q'                                         
170300             MOVE JA                 TO SW-KDKREBEH-Q                     
170400           END-IF                                                         
170500           IF KTL3-KDRETBEH = 'P'                                         
170600             MOVE JA                 TO SW-KDKREBEH-P                     
170700           END-IF                                                         
170800         END-IF                                                           
170900       END-IF                                                             
171000     END-IF                                                               
171100                                                                          
171200     IF KDANMORS-UPPD = FEL                                               
171300       MOVE MFS-ALFA-FAELT-FEL  TO                                        
171400                                      MOD-RAD18-KDANMORS-ATTRIBUT         
171500       MOVE MFS-ADD-LAES-IN-FAELT TO                                      
171600                                      MOD-RAD18-IDARTNR-ATTRIBUT          
171700                                      MOD-RAD18-IDRADNR-ATTRIBUT          
171800       MOVE MFS-DO-NOT-TOUCH-FIELD TO                                     
171900                                      MOD-RAD18-KDANMORS                  
172000                                      MOD-RAD18-IDARTNR                   
172100                                      MOD-RAD18-IDRADNR                   
172200     ELSE                                                                 
172300       MOVE MFS-ADD-LAES-IN-FAELT TO                                      
172400                                      MOD-RAD18-KDANMORS-ATTRIBUT         
172500     END-IF                                                               
172600     .                                                                    
172700     EJECT                                                                
172800 CE-KOLLA-KANTKOD               SECTION.                                  
172900                                                                          
173000     PERFORM UNTIL IX > MAX-IX                                            
173100        MOVE MID-KDKREBEH (IX)  TO TEST-KDKREBEH                          
173200                                   KDKREBEH-SW                            
173300        IF KDKREBEH-1 = 'N'                                               
173400           IF OK-FELKOD                                                   
173500             INSPECT MID-IDARTNR (IX)                                     
173600                                REPLACING LEADING SPACE BY ZERO           
173700             INSPECT MID-IDRADNR (IX)                                     
173800                                REPLACING LEADING SPACE BY ZERO           
173900             IF MID-IDARTNR (IX) NUMERIC AND                              
174000                MID-IDRADNR (IX) NUMERIC                                  
174100               MOVE MID-IDARTNR (IX) TO W-IDARTNR                         
174200               MOVE MID-IDRADNR (IX) TO W-IDRADNR                         
174300               PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL                     
174400               MOVE LEV-KDANMORS TO OKOD-KDANMORS                         
174500               CALL W418OKOD USING OKOD-W418OKOD                          
174600               IF OKOD-FL-INTERNUPPACKNING = JA                           
174700                 MOVE NEJ        TO INDATA-SW                             
174800                 MOVE MFS-ALFA-FAELT-FEL TO                               
174900                             MOD-KDKREBEH-ATTRIBUT (IX)                   
175000                 MOVE MFS-DO-NOT-TOUCH-FIELD TO                           
175100                             MOD-KDKREBEH        (IX)                     
175200               ELSE                                                       
175300                 MOVE MFS-ALFA-FAELT-RAETT TO                             
175400                            MOD-KDKREBEH-ATTRIBUT (IX)                    
175500               END-IF                                                     
175600             END-IF                                                       
175700           ELSE                                                           
175800              MOVE NEJ          TO INDATA-SW                              
175900              MOVE MFS-ALFA-FAELT-FEL TO                                  
176000                         MOD-KDKREBEH-ATTRIBUT (IX)                       
176100              MOVE MFS-DO-NOT-TOUCH-FIELD TO                              
176200                         MOD-KDKREBEH          (IX)                       
176300           END-IF                                                         
176400        ELSE                                                              
176500           IF KDKREBEH-1 = 'Y' OR 'J'                                     
176600              IF KDKREBEH-2 = ' ' AND KDKREBEH-3 = ' '                    
176700                 MOVE MFS-ALFA-FAELT-RAETT TO                             
176800                            MOD-KDKREBEH-ATTRIBUT (IX)                    
176900*- KOLLA SÅ ATT ART.ÄR PRISSATT FÖR DDI-/DEALER-NET-PRICE-KUND            
177000                 IF DIST79-DEALER-PRICE                                   
177100                   INSPECT MID-IDARTNR (IX)                               
177200                                 REPLACING LEADING SPACE BY ZERO          
177300                   INSPECT MID-IDRADNR (IX)                               
177400                                 REPLACING LEADING SPACE BY ZERO          
177500                   IF MID-IDARTNR (IX) NUMERIC AND                        
177600                      MID-IDRADNR (IX) NUMERIC                            
177700                     MOVE MID-IDARTNR (IX) TO W-IDARTNR                   
177800                     MOVE MID-IDRADNR (IX) TO W-IDRADNR                   
177900                     PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL               
178000                     PERFORM CM-KOLLA-PRISSATT                            
178100                   END-IF                                                 
178200                 END-IF                                                   
178300                                                                          
178400*- KOLLA OM MATRIX-KONFLIKT OCH QUEUED PART.(SE BILD 4751/4752)           
178500                 INSPECT MID-IDARTNR (IX)                                 
178600                               REPLACING LEADING SPACE BY ZERO            
178700                 INSPECT MID-IDRADNR (IX)                                 
178800                               REPLACING LEADING SPACE BY ZERO            
178900                 IF MID-IDARTNR (IX) NUMERIC AND                          
179000                    MID-IDRADNR (IX) NUMERIC                              
179100                   MOVE MID-IDARTNR (IX) TO W-IDARTNR                     
179200                   MOVE MID-IDRADNR (IX) TO W-IDRADNR                     
179300                   PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL                 
179400                                                                          
179500                   IF LEV-KDKREBEH(1:1) = ('Q' OR 'P') AND                
179600                                          MFS-UPDATE                      
179700                     MOVE NEJ                TO INDATA-SW                 
179800                     MOVE MFS-ALFA-FAELT-FEL TO                           
179900                                      MOD-KDKREBEH-ATTRIBUT (IX)          
180000                     MOVE MFS-ROER-EJ-FAELT TO MOD-KDKREBEH (IX)          
180100                     MOVE INF-PRESS-PF23    TO MED-IDMFSFEL               
180200                     CALL WMEDKONV USING MED-WMEDAREA                     
180300                     MOVE MED-MFSFEL TO MOD-TEMFSFEL                      
180400                   END-IF                                                 
180500                 END-IF                                                   
180600              ELSE                                                        
180700                 MOVE NEJ       TO INDATA-SW                              
180800                 MOVE MFS-ALFA-FAELT-FEL TO                               
180900                            MOD-KDKREBEH-ATTRIBUT (IX)                    
181000                 MOVE MFS-DO-NOT-TOUCH-FIELD TO                           
181100                            MOD-KDKREBEH          (IX)                    
181200              END-IF                                                      
181300           ELSE                                                           
181400             IF TEST-KDKREBEH = 'RR ' OR 'QR '                            
181500               INSPECT MID-IDARTNR (IX)                                   
181600                                  REPLACING LEADING SPACE BY ZERO         
181700               INSPECT MID-IDRADNR (IX)                                   
181800                                  REPLACING LEADING SPACE BY ZERO         
181900               IF MID-IDARTNR (IX) NUMERIC AND                            
182000                  MID-IDRADNR (IX) NUMERIC                                
182100                 MOVE MID-IDARTNR (IX) TO W-IDARTNR                       
182200                 MOVE MID-IDRADNR (IX) TO W-IDRADNR                       
182300                 PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL                   
182400                 PERFORM S03-GET-RESPONSIBLE                              
182500                 IF ANSV-OK                                               
182600                   MOVE MFS-ALFA-FAELT-RAETT TO                           
182700                              MOD-KDKREBEH-ATTRIBUT (IX)                  
182800                 ELSE                                                     
182900                   MOVE NEJ     TO INDATA-SW                              
183000                   MOVE MFS-ALFA-FAELT-FEL TO                             
183100                              MOD-KDKREBEH-ATTRIBUT (IX)                  
183200                   MOVE MFS-DO-NOT-TOUCH-FIELD TO                         
183300                              MOD-KDKREBEH        (IX)                    
183400                 END-IF                                                   
183500                 IF DIST79-DEALER-PRICE                                   
183600                   PERFORM CM-KOLLA-PRISSATT                              
183700                 END-IF                                                   
183800               END-IF                                                     
183900             ELSE                                                         
184000               IF TEST-KDKREBEH = 'ANN'  AND                              
184100                   ANM-KDLEVANM = '4'                                     
184200                 MOVE MFS-ALFA-FAELT-RAETT TO                             
184300                            MOD-KDKREBEH-ATTRIBUT (IX)                    
184400               ELSE                                                       
184500                 IF KDKREBEH-1 = ALL '+'                                  
184600                   CONTINUE                                               
184700                 ELSE                                                     
184800                   MOVE NEJ   TO INDATA-SW                                
184900                   MOVE MFS-ALFA-FAELT-FEL TO                             
185000                              MOD-KDKREBEH-ATTRIBUT (IX)                  
185100                   MOVE MFS-DO-NOT-TOUCH-FIELD TO                         
185200                              MOD-KDKREBEH      (IX)                      
185300                 END-IF                                                   
185400               END-IF                                                     
185500             END-IF                                                       
185600           END-IF                                                         
185700        END-IF                                                            
185800        ADD +1                  TO IX                                     
185900     END-PERFORM                                                          
186000                                                                          
186100     IF MED-IDMFSFEL = '206'                                              
186200       PERFORM S16-INDATA-TILL-MOD                                        
186300     END-IF                                                               
186400                                                                          
186500     .                                                                    
186600     EJECT                                                                
186700 CF-KOLLA-PRIS                  SECTION.                                  
186800                                                                          
186900     IF LEV-IDFTG = MSGI-IDFTG                                            
187000        IF DIST79-DEALER-PRICE OR                                         
187200           DIST79-ECOM-PRICE                                              
187300            MOVE NEJ                       TO INDATA-SW                   
187400            MOVE MFS-NUM-FAELT-FEL         TO                             
187500                                   MOD-RAD18-PRARTBTO-ATTRIBUT            
187600            MOVE MFS-ADD-LAES-IN-FAELT     TO                             
187700                                   MOD-RAD18-IDARTNR-ATTRIBUT             
187800                                   MOD-RAD18-IDRADNR-ATTRIBUT             
187900            MOVE MFS-DO-NOT-TOUCH-FIELD    TO                             
188000                                   MOD-RAD18-PRARTBTO                     
188100                                   MOD-RAD18-IDARTNR                      
188200                                   MOD-RAD18-IDRADNR                      
188300        ELSE                                                              
188400          MOVE MID-RAD18-PRARTBTO        TO DEC-IDFRIDATA                 
188500          MOVE 7                         TO DEC-KVHELTAL                  
188600          MOVE 2                         TO DEC-KVDECIMAL                 
188700          CALL WDECEDIT USING WDECAREA                                    
188800          IF DEC-KDSVAR-OK                                                
188900             MOVE DEC-IDEDITDATA         TO WS-IDEDITDATA                 
189000             PERFORM CFA-KOLLA-OM-PRISUPPD                                
189100          ELSE                                                            
189200             MOVE NEJ                    TO INDATA-SW                     
189300             MOVE MFS-NUM-FAELT-FEL      TO                               
189400                                      MOD-RAD18-PRARTBTO-ATTRIBUT         
189500             MOVE MFS-ADD-LAES-IN-FAELT  TO                               
189600                                      MOD-RAD18-IDARTNR-ATTRIBUT          
189700                                      MOD-RAD18-IDRADNR-ATTRIBUT          
189800             MOVE MFS-DO-NOT-TOUCH-FIELD TO                               
189900                                      MOD-RAD18-PRARTBTO                  
190000                                      MOD-RAD18-IDARTNR                   
190100                                      MOD-RAD18-IDRADNR                   
190200          END-IF                                                          
190300        END-IF                                                            
190400     ELSE                                                                 
190500        MOVE NEJ                       TO INDATA-SW                       
190600        MOVE MFS-NUM-FAELT-FEL         TO                                 
190700                                 MOD-RAD18-PRARTBTO-ATTRIBUT              
190800        MOVE MFS-ADD-LAES-IN-FAELT     TO                                 
190900                                 MOD-RAD18-IDARTNR-ATTRIBUT               
191000                                 MOD-RAD18-IDRADNR-ATTRIBUT               
191100        MOVE MFS-DO-NOT-TOUCH-FIELD    TO                                 
191200                                         MOD-RAD18-PRARTBTO               
191300                                         MOD-RAD18-IDARTNR                
191400                                         MOD-RAD18-IDRADNR                
191500     END-IF                                                               
191600     .                                                                    
191700     EJECT                                                                
191800 CFA-KOLLA-OM-PRISUPPD          SECTION.                                  
191900                                                                          
192000     IF WS-IDEDITDATA > ZERO                                              
192100        IF MID-RAD18-KDANMORS = ALL '+'                                   
192200           MOVE LEV-KDANMORS       TO OKOD-KDANMORS                       
192300        ELSE                                                              
192400           MOVE MID-RAD18-KDANMORS TO OKOD-KDANMORS                       
192500        END-IF                                                            
192600        CALL W418OKOD USING OKOD-W418OKOD                                 
192700        IF OKOD-FL-PRIS-ZERO = JA                                         
192800           MOVE NEJ                TO INDATA-SW                           
192900           MOVE MFS-NUM-FAELT-FEL  TO                                     
193000                                      MOD-RAD18-PRARTBTO-ATTRIBUT         
193100           MOVE MFS-ADD-LAES-IN-FAELT  TO                                 
193200                                      MOD-RAD18-IDARTNR-ATTRIBUT          
193300                                      MOD-RAD18-IDRADNR-ATTRIBUT          
193400           MOVE MFS-DO-NOT-TOUCH-FIELD TO                                 
193500                                      MOD-RAD18-PRARTBTO                  
193600                                      MOD-RAD18-IDARTNR                   
193700                                      MOD-RAD18-IDRADNR                   
193800        ELSE                                                              
193900           IF MID-RAD18-KDANMORS = '++'                                   
194000              MOVE LEV-KDANMORS TO OKOD-KDANMORS                          
194100              CALL W418OKOD USING OKOD-W418OKOD                           
194200              IF OKOD-FL-PRIS-ZERO = JA                                   
194300                 MOVE NEJ       TO INDATA-SW                              
194400                 MOVE MFS-NUM-FAELT-FEL      TO                           
194500                                      MOD-RAD18-PRARTBTO-ATTRIBUT         
194600                 MOVE MFS-ADD-LAES-IN-FAELT  TO                           
194700                                      MOD-RAD18-IDARTNR-ATTRIBUT          
194800                                      MOD-RAD18-IDRADNR-ATTRIBUT          
194900                 MOVE MFS-DO-NOT-TOUCH-FIELD TO                           
195000                                      MOD-RAD18-PRARTBTO                  
195100                                      MOD-RAD18-IDARTNR                   
195200                                      MOD-RAD18-IDRADNR                   
195300              ELSE                                                        
195400                 MOVE MFS-ALFA-FAELT-RAETT TO                             
195500                            MOD-RAD18-KDANMORS-ATTRIBUT                   
195600              END-IF                                                      
195700           ELSE                                                           
195800              MOVE MFS-ALFA-FAELT-RAETT TO                                
195900                         MOD-RAD18-PRARTBTO-ATTRIBUT                      
196000           END-IF                                                         
196100        END-IF                                                            
196200     ELSE                                                                 
196300        IF MID-RAD18-KDANMORS = ALL '+'                                   
196400           MOVE LEV-KDANMORS       TO OKOD-KDANMORS                       
196500        ELSE                                                              
196600           MOVE MID-RAD18-KDANMORS TO OKOD-KDANMORS                       
196700        END-IF                                                            
196800        CALL W418OKOD USING OKOD-W418OKOD                                 
196900        IF OKOD-FL-PRIS-ZERO = JA                                         
197000           MOVE MFS-ALFA-FAELT-RAETT         TO                           
197100                                   MOD-RAD18-KDANMORS-ATTRIBUT            
197200        ELSE                                                              
197300           IF OKOD-FL-PRIS-ZERO = NEJ        AND                          
197400              OKOD-FL-GODK-PRIS-ZERO = JA                                 
197500              MOVE MFS-ALFA-FAELT-RAETT      TO                           
197600                                   MOD-RAD18-KDANMORS-ATTRIBUT            
197700           ELSE                                                           
197800              MOVE NEJ                       TO INDATA-SW                 
197900              MOVE MFS-NUM-FAELT-FEL         TO                           
198000                                   MOD-RAD18-PRARTBTO-ATTRIBUT            
198100              MOVE MFS-ADD-LAES-IN-FAELT     TO                           
198200                                   MOD-RAD18-IDARTNR-ATTRIBUT             
198300                                   MOD-RAD18-IDRADNR-ATTRIBUT             
198400              MOVE MFS-DO-NOT-TOUCH-FIELD TO                              
198500                                   MOD-RAD18-PRARTBTO                     
198600                                   MOD-RAD18-IDARTNR                      
198700                                   MOD-RAD18-IDRADNR                      
198800           END-IF                                                         
198900        END-IF                                                            
199000     END-IF                                                               
199100     .                                                                    
199200     EJECT                                                                
199300 CG-KOLLA-KDANMORS              SECTION.                                  
199400                                                                          
199500     MOVE MID-RAD18-KDANMORS TO OKOD-KDANMORS                             
199600     CALL W418OKOD USING OKOD-W418OKOD                                    
199700     IF OKOD-FL-GODK-KOD = JA                                             
199800        PERFORM CB-KOLLA-KDANMORS                                         
199900        IF KDANMORS-UPPD = FEL                                            
200000           MOVE NEJ             TO INDATA-SW                              
200100           MOVE MFS-NUM-FAELT-FEL TO                                      
200200                                     MOD-RAD18-KDANMORS-ATTRIBUT          
200300           MOVE MFS-ADD-LAES-IN-FAELT  TO                                 
200400                                     MOD-RAD18-IDARTNR-ATTRIBUT           
200500                                     MOD-RAD18-IDRADNR-ATTRIBUT           
200600           MOVE MFS-DO-NOT-TOUCH-FIELD TO                                 
200700                                     MOD-RAD18-KDANMORS                   
200800                                     MOD-RAD18-IDARTNR                    
200900                                     MOD-RAD18-IDRADNR                    
201000        ELSE                                                              
201100           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
201200                         MOD-RAD18-KDANMORS-ATTRIBUT                      
201300        END-IF                                                            
201400     ELSE                                                                 
201500        MOVE NEJ                TO INDATA-SW                              
201600        MOVE MFS-NUM-FAELT-FEL  TO                                        
201700                                     MOD-RAD18-KDANMORS-ATTRIBUT          
201800        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
201900                                     MOD-RAD18-IDARTNR-ATTRIBUT           
202000                                     MOD-RAD18-IDRADNR-ATTRIBUT           
202100        MOVE MFS-DO-NOT-TOUCH-FIELD TO                                    
202200                                     MOD-RAD18-KDANMORS                   
202300                                     MOD-RAD18-IDARTNR                    
202400                                     MOD-RAD18-IDRADNR                    
202500     END-IF                                                               
202600     .                                                                    
202700     EJECT                                                                
202800 CH-KOLLA-ANTAL                 SECTION.                                  
202900                                                                          
203000     IF MID-RAD18-KVLEVANM NUMERIC                                        
203100        IF MID-RAD18-KVLEVANM = '000000'                                  
203200           IF MID-RAD18-KDANMORS = '++'                                   
203300              MOVE LEV-KDANMORS TO OKOD-KDANMORS                          
203400              CALL W418OKOD USING OKOD-W418OKOD                           
203500              IF OKOD-FL-ANT-LEVANM = JA                                  
203600                 MOVE NEJ       TO INDATA-SW                              
203700                 MOVE MFS-NUM-FAELT-FEL      TO                           
203800                                     MOD-RAD18-KVLEVANM-ATTRIBUT          
203900                 MOVE MFS-ADD-LAES-IN-FAELT  TO                           
204000                                     MOD-RAD18-IDARTNR-ATTRIBUT           
204100                                     MOD-RAD18-IDRADNR-ATTRIBUT           
204200                 MOVE MFS-DO-NOT-TOUCH-FIELD TO                           
204300                                     MOD-RAD18-KVLEVANM                   
204400                                     MOD-RAD18-IDARTNR                    
204500                                     MOD-RAD18-IDRADNR                    
204600              END-IF                                                      
204700           ELSE                                                           
204800              MOVE MID-RAD18-KDANMORS TO OKOD-KDANMORS                    
204900              CALL W418OKOD USING OKOD-W418OKOD                           
205000              IF OKOD-FL-ANT-LEVANM = JA                                  
205100                 MOVE NEJ       TO INDATA-SW                              
205200                 MOVE MFS-NUM-FAELT-FEL      TO                           
205300                                     MOD-RAD18-KVLEVANM-ATTRIBUT          
205400                 MOVE MFS-ADD-LAES-IN-FAELT  TO                           
205500                                     MOD-RAD18-IDARTNR-ATTRIBUT           
205600                                     MOD-RAD18-IDRADNR-ATTRIBUT           
205700                 MOVE MFS-DO-NOT-TOUCH-FIELD TO                           
205800                                     MOD-RAD18-KVLEVANM                   
205900                                     MOD-RAD18-IDARTNR                    
206000                                     MOD-RAD18-IDRADNR                    
206100              END-IF                                                      
206200           END-IF                                                         
206300        ELSE                                                              
206400           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
206500                                    MOD-RAD18-KVLEVANM-ATTRIBUT           
206600        END-IF                                                            
206700     ELSE                                                                 
206800        MOVE NEJ                TO INDATA-SW                              
206900        MOVE MFS-NUM-FAELT-FEL  TO                                        
207000                                     MOD-RAD18-KVLEVANM-ATTRIBUT          
207100        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
207200                                     MOD-RAD18-IDARTNR-ATTRIBUT           
207300                                     MOD-RAD18-IDRADNR-ATTRIBUT           
207400        MOVE MFS-DO-NOT-TOUCH-FIELD TO                                    
207500                                     MOD-RAD18-KVLEVANM                   
207600                                     MOD-RAD18-IDARTNR                    
207700                                     MOD-RAD18-IDRADNR                    
207800     END-IF                                                               
207900     .                                                                    
208000     EJECT                                                                
208100 CJ-KOLLA-DIREKTLEVERANS-FLAGGA SECTION.                                  
208200                                                                          
208300*-FÖR 74 FÅR FLDIRLEV INTE ANGES                                          
208400     IF LEV-KDANMORS = '74'                                               
208500        MOVE NEJ             TO FLDIRLEV-WS                               
208600        MOVE NEJ             TO INDATA-SW                                 
208700        MOVE MFS-ALFA-FAELT-FEL TO                                        
208800             MOD-RAD18-FLDIRLEV-ATTRIBUT                                  
208900        MOVE MFS-DO-NOT-TOUCH-FIELD TO                                    
209000             MOD-RAD18-IDARTNR                                            
209100             MOD-RAD18-IDRADNR                                            
209200             MOD-RAD18-FLDIRLEV                                           
209300     ELSE                                                                 
209400        IF MID-RAD18-FLDIRLEV = 'Y' OR 'J'                                
209500           MOVE JA                 TO FLDIRLEV-WS                         
209600           MOVE MFS-ALFA-FAELT-RAETT TO                                   
209700                         MOD-RAD18-FLDIRLEV-ATTRIBUT                      
209800        ELSE                                                              
209900           IF MID-RAD18-FLDIRLEV = 'N'                                    
210000              MOVE NEJ             TO FLDIRLEV-WS                         
210100              MOVE MFS-ALFA-FAELT-FEL TO                                  
210200                            MOD-RAD18-FLDIRLEV-ATTRIBUT                   
210300           ELSE                                                           
210400              MOVE NEJ             TO INDATA-SW                           
210500              MOVE MFS-ALFA-FAELT-FEL TO                                  
210600                   MOD-RAD18-FLDIRLEV-ATTRIBUT                            
210700              MOVE MFS-ADD-LAES-IN-FAELT  TO                              
210800                   MOD-RAD18-IDARTNR-ATTRIBUT                             
210900                   MOD-RAD18-IDRADNR-ATTRIBUT                             
211000              MOVE MFS-DO-NOT-TOUCH-FIELD TO                              
211100                   MOD-RAD18-FLDIRLEV                                     
211200                   MOD-RAD18-IDARTNR                                      
211300                   MOD-RAD18-IDRADNR                                      
211400           END-IF                                                         
211500        END-IF                                                            
211600     END-IF                                                               
211700     .                                                                    
211800     EJECT                                                                
211900 CL-KOLLA-R78 SECTION.                                                    
212000                                                                          
212100     MOVE JA                    TO INDATA-SW                              
212200     MOVE +0                    TO WS-KVRADER-ANN                         
212300     PERFORM IMS-GET-KREE11-GHNP-FIRST-OKV                                
212400     PERFORM UNTIL IX > MAX-IX                                            
212500       MOVE MID-KDKREBEH (IX)   TO TEST-KDKREBEH                          
212600       IF (KDKREBEH-1 = 'A'    AND KDKREBEH-2 = 'N' AND                   
212700           KDKREBEH-3 = 'N')    OR                                        
212800          (KDKREBEH-1 = 'D'    AND KDKREBEH-2 = 'E' AND                   
212900           KDKREBEH-3 = 'L')                                              
213000         PERFORM CLA-KOLLA-OM-GODKAENT                                    
213100         ADD +1                 TO WS-KVRADER-ANN                         
213200         IF OK                                                            
213300           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
213400                MOD-KDKREBEH-ATTRIBUT (IX)                                
213500         ELSE                                                             
213600           MOVE MFS-ALFA-FAELT-FEL TO                                     
213700                MOD-KDKREBEH-ATTRIBUT (IX)                                
213800           MOVE NEJ             TO INDATA-SW                              
213900         END-IF                                                           
214000       ELSE                                                               
214100         IF KDKREBEH-1 = ALL '+'                                          
214200           CONTINUE                                                       
214300         ELSE                                                             
214400           MOVE MFS-ALFA-FAELT-FEL TO                                     
214500                MOD-KDKREBEH-ATTRIBUT (IX)                                
214600           MOVE NEJ             TO INDATA-SW                              
214700         END-IF                                                           
214800       END-IF                                                             
214900       ADD +1                   TO IX                                     
215000     END-PERFORM                                                          
215100     .                                                                    
215200     EJECT                                                                
215300 CLA-KOLLA-OM-GODKAENT          SECTION.                                  
215400                                                                          
215500     INSPECT MID-IDARTNR (IX) REPLACING LEADING SPACE BY ZERO             
215600     INSPECT MID-IDRADNR (IX) REPLACING LEADING SPACE BY ZERO             
215700     IF MID-IDARTNR (IX) NUMERIC AND                                      
215800        MID-IDRADNR (IX) NUMERIC                                          
215900       MOVE MID-IDARTNR (IX)    TO W-IDARTNR                              
216000       MOVE MID-IDRADNR (IX)    TO W-IDRADNR                              
216100                                                                          
216200       PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL                             
216300       IF LEV-KDKREBEH = 'ANN' OR 'DEL'                                   
216400         MOVE NEJ                     TO OK-SW                            
216500         MOVE ERR-ALREADY-CANCELLED   TO MED-IDMFSFEL                     
216600       ELSE                                                               
216700         MOVE LEV-KDANMORS        TO OKOD-KDANMORS                        
216800         CALL W418OKOD USING OKOD-W418OKOD                                
216900         IF (OKOD-FL-RETILL = 'J'          ) OR                           
217000            (OKOD-FL-INTERNUPPACKNING = 'J')                              
217100           IF OKOD-FL-RETILL = JA                                         
217200              PERFORM CLAA-KOLLA-RETURTERMINAL                            
217300           ELSE                                                           
217400              MOVE JA             TO OK-SW                                
217500           END-IF                                                         
217600         ELSE                                                             
217700           MOVE NEJ               TO OK-SW                                
217800         END-IF                                                           
217900       END-IF                                                             
218000     END-IF                                                               
218100     .                                                                    
218200     EJECT                                                                
218300 CLAA-KOLLA-RETURTERMINAL       SECTION.                                  
218400                                                                          
218500     MOVE LEV-IDDC-RET          TO W-IDDC-A3                              
218600     PERFORM IMS-GU-WLRETA01                                              
218700     IF SEGMENT-FINNS                                                     
218800        MOVE JA                 TO RETUR-FINNS-PA-RETTERM-SW              
218900        MOVE NEJ                TO OK-SW                                  
219000     ELSE                                                                 
219100        MOVE NEJ                TO RETUR-FINNS-PA-RETTERM-SW              
219200        MOVE JA                 TO OK-SW                                  
219300     END-IF                                                               
219400     .                                                                    
219500     EJECT                                                                
219600                                                                          
219700 CM-KOLLA-PRISSATT              SECTION.                                  
219800                                                                          
219900     IF LEV-FLPRQUES = JA                                                 
220000       IF LEV-KDVAT = SPACE  OR  LEV-PRARTBTO-LOC = ZERO                  
220100         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDKREBEH-ATTRIBUT (IX)            
220200         MOVE MFS-ROER-EJ-FAELT  TO MOD-KDKREBEH      (IX)                
220300         MOVE NEJ                TO INDATA-SW                             
220400         IF LEV-PRARTBTO-LOC = ZERO                                       
220500           MOVE ERR-PRIS-MISSING   TO MED-IDMFSFEL                        
220600         ELSE                                                             
220700           MOVE ERR-VAT-NO-MISSING TO MED-IDMFSFEL                        
220800         END-IF                                                           
220900         CALL WMEDKONV USING MED-WMEDAREA                                 
221000         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
221100       END-IF                                                             
221200     END-IF                                                               
221300     .                                                                    
221400     EJECT                                                                
221500 D-UPPDATERA                    SECTION.                                  
221600                                                                          
221700     MOVE NEJ    TO NEKAD-RAD-SW                                          
221800     MOVE NEJ    TO NEKAD-RETUR-RAD-SW                                    
221900     MOVE NEJ    TO ANGRA-NEKAD-RETUR-RAD-SW                              
222000     MOVE NEJ    TO ANGRA-NEKAD-RAD-SW                                    
222100     MOVE NEJ    TO ANNULLERAD-RAD-SW                                     
222200     MOVE NEJ    TO KOD-AENDRAD-SW                                        
222300     MOVE NEJ    TO ANTAL-AENDRAT-SW                                      
222400     MOVE NEJ    TO PRIS-AENDRAT-SW                                       
222500     MOVE NEJ    TO WDR501-SW                                             
222600     MOVE NEJ    TO KOD-LDCKUND-AENDRAD-SW                                
222700                                                                          
222800     PERFORM IMS-GU-WDGX4103                                              
222900     IF SEGMENT-FINNS                                                     
223000       MOVE JA                         TO WDR501-SW                       
223100     END-IF                                                               
223200                                                                          
223300     MOVE +1                    TO IX                                     
223400     PERFORM IMS-GET-KREE11-GHNP-FIRST-OKV                                
223500     PERFORM UNTIL IX > MAX-IX                                            
223600       INSPECT MID-IDARTNR (IX) REPLACING LEADING SPACE BY ZERO           
223700       INSPECT MID-IDRADNR (IX) REPLACING LEADING SPACE BY ZERO           
223800       INSPECT MID-KDANMORS(IX) REPLACING LEADING SPACE BY ZERO           
223900       MOVE MID-KDKREBEH (IX)   TO TEST-KDKREBEH                          
224000       IF KDKREBEH-1 = 'Y' OR 'J' OR 'N'                                  
224100         IF KDKREBEH-1 = 'J'                                              
224200            MOVE 'Y'            TO KDKREBEH-1                             
224300         END-IF                                                           
224400         IF MID-IDARTNR (IX) NUMERIC AND                                  
224500            MID-IDRADNR (IX) NUMERIC                                      
224600           MOVE MID-IDARTNR (IX)  TO W-IDARTNR                            
224700                                     W-IDARTNR-4109-MIN                   
224800                                     W-IDARTNR-4109-MAX                   
224900           MOVE MID-IDRADNR  (IX) TO W-IDRADNR                            
225000           IF MID-KDANMORS (IX) NUMERIC                                   
225100             MOVE MID-KDANMORS (IX) TO W-KDANMORS-4109-MIN                
225200                                       W-KDANMORS-4109-MAX                
225300           END-IF                                                         
225400                                                                          
225500           PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL                         
225600                                                                          
225700**-- OM MAN FÖRST NEKAR EN RAD,(=>STATUS=3) OCH SEDAN ÅNGRAR SIG          
225800**-- OCH UPPDATERAR KDKREBEH=JA FÅR MAN KOLLA IDDC-RET PÅ WDA201          
225900**-- IGEN.ÄVEN KDLEVATT.NY KOLL INLAGD 2007-09-25.                        
226000           MOVE LEV-KDANMORS    TO OKOD-KDANMORS                          
226100           CALL W418OKOD USING OKOD-W418OKOD                              
226200           IF KDKREBEH-1 = 'Y' AND LEV-KDKREBEH(1:1) = 'N'                
226300             MOVE JA    TO ANGRA-NEKAD-RAD-SW                             
226400             IF OKOD-FL-RETILL = JA                                       
226500               MOVE JA    TO ANGRA-NEKAD-RETUR-RAD-SW                     
226600             END-IF                                                       
226700           END-IF                                                         
226800                                                                          
226900           IF TEST-KDKREBEH(1:1) = 'J'                                    
227000             MOVE 'Y'           TO TEST-KDKREBEH(1:1)                     
227100           END-IF                                                         
227200           MOVE TEST-KDKREBEH   TO LEV-KDKREBEH                           
227300           PERFORM IMS-REPL-KREE11                                        
227400           MOVE LEV-KDANMORS    TO OKOD-KDANMORS                          
227500           CALL W418OKOD USING OKOD-W418OKOD                              
227600                                                                          
227700           IF KDKREBEH-1 = 'N' AND OKOD-FL-RETILL = JA                    
227800             MOVE JA    TO NEKAD-RETUR-RAD-SW                             
227900           END-IF                                                         
228000                                                                          
228100           IF KDKREBEH-1 = 'Y'                                            
228200             PERFORM S05-UPPD-ANALYSNRREGISTRET                           
228300           END-IF                                                         
228400                                                                          
228500**-- OM RAD NEKAS SKALL RADPRISET FÖR KNOTARAD RÄKNAS AV FRÅN WDR5        
228600**-- WDGX4104 - SOX-ÄNDRING 20050425.UNDANTAG FÖR USA/CAN.                
228700           MOVE MSGI-IDFTG  TO WS-IDFTG                                   
228800           IF IDFTG-PV OR IDFTG-CN OR IDFTG-NON-VCC                       
228900             IF LEV-IDDC NOT = W-IDDC-B6                                  
229000               MOVE LEV-IDDC  TO W-IDDC-B6                                
229100               PERFORM IMS-GU-WDB601                                      
229200             END-IF                                                       
229300             IF DCS-NDC-PF AND IDFTG-PV                                   
229400               CONTINUE                                                   
229500             ELSE                                                         
229600               IF KDKREBEH-1 = 'N'                                        
229700                 MOVE JA     TO NEKAD-RAD-SW                              
229800                 PERFORM S07-EV-UPPDATERA-KN-WDGX4103                     
229900               END-IF                                                     
230000**-- OM MAN FÖRST NEKAR RAD OCH SEDAN SÄTTER JA MÅSTE WDR5 KOLLAS.        
230100               IF ANGRA-NEKAD-RAD                                         
230200                 PERFORM S15-EV-UPPDAT-KN-WDGX4103-IGEN                   
230300               END-IF                                                     
230400             END-IF                                                       
230500           END-IF                                                         
230600                                                                          
230700           MOVE MFS-ADD-LYS-UPP-FAELT TO                                  
230800                                    MOD-KDKREBEH-ATTRIBUT (IX)            
230900           MOVE INF-UPDATE-OK   TO MED-IDMFSFEL                           
231000           CALL WMEDKONV USING MED-WMEDAREA                               
231100           MOVE MED-MFSFEL      TO MOD-TEMFSINF                           
231200         END-IF                                                           
231300       ELSE                                                               
231400         IF (KDKREBEH-1 = 'A'   AND                                       
231500             KDKREBEH-2 = 'N'   AND                                       
231600             KDKREBEH-3 = 'N')  OR                                        
231700            (KDKREBEH-1 = 'D'   AND                                       
231800             KDKREBEH-2 = 'E'   AND                                       
231900             KDKREBEH-3 = 'L')                                            
232000           IF MID-IDARTNR (IX)  NUMERIC AND                               
232100              MID-IDRADNR (IX)  NUMERIC                                   
232200                                                                          
232300              MOVE MID-IDARTNR (IX) TO W-IDARTNR                          
232400              MOVE MID-IDRADNR (IX) TO W-IDRADNR                          
232500                                                                          
232600              PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL                      
232700              IF TEST-KDKREBEH(1:1) = 'J'                                 
232800                MOVE 'Y'        TO TEST-KDKREBEH(1:1)                     
232900              END-IF                                                      
233000              MOVE TEST-KDKREBEH TO LEV-KDKREBEH                          
233100              MOVE LEV-KDANMORS  TO OKOD-KDANMORS                         
233200              MOVE JA            TO LEV-FLANNULL                          
233300              PERFORM IMS-REPL-KREE11                                     
233400                                                                          
233500**-- OM RAD NEKAS SKALL RADPRISET FÖR KNOTARAD RÄKNAS AV FRÅN WDR5        
233600**-- WDGX4104 - SOX-ÄNDRING 20050425.UNDANTAG FÖR USA/CAN.                
233700              MOVE JA            TO ANNULLERAD-RAD-SW                     
233800              CALL W418OKOD USING OKOD-W418OKOD                           
233900              MOVE MSGI-IDFTG  TO WS-IDFTG                                
234000              IF IDFTG-PV OR IDFTG-CN OR IDFTG-NON-VCC                    
234100                IF LEV-IDDC NOT = W-IDDC-B6                               
234200                  MOVE LEV-IDDC  TO W-IDDC-B6                             
234300                  PERFORM IMS-GU-WDB601                                   
234400                END-IF                                                    
234500                IF DCS-NDC-PF AND IDFTG-PV                                
234600                  CONTINUE                                                
234700                ELSE                                                      
234800                  PERFORM S07-EV-UPPDATERA-KN-WDGX4103                    
234900                END-IF                                                    
235000              END-IF                                                      
235100                                                                          
235200              MOVE MFS-ADD-LYS-UPP-FAELT TO                               
235300                                    MOD-KDKREBEH-ATTRIBUT (IX)            
235400              MOVE INF-UPDATE-OK TO MED-IDMFSFEL                          
235500              CALL WMEDKONV   USING MED-WMEDAREA                          
235600              MOVE MED-MFSFEL TO MOD-TEMFSINF                             
235700           END-IF                                                         
235800         ELSE                                                             
235900           IF (KDKREBEH-1 = 'R' AND                                       
236000               KDKREBEH-2 = 'R' AND                                       
236100               KDKREBEH-3 = ' ') OR                                       
236200              (KDKREBEH-1 = 'Q' AND                                       
236300               KDKREBEH-2 = 'R' AND                                       
236400               KDKREBEH-3 = ' ')                                          
236500                                                                          
236600             PERFORM DB-SKAPA-REMISS-MAIL                                 
236700             PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL                       
236800             IF TEST-KDKREBEH(1:1) = 'J'                                  
236900               MOVE 'Y'         TO TEST-KDKREBEH(1:1)                     
237000             END-IF                                                       
237100             IF LEV-KDKREBEH (1:1) = 'Q'                                  
237200               MOVE 'QR '         TO LEV-KDKREBEH                         
237300             ELSE                                                         
237400               IF LEV-KDKREBEH (1:1) = 'P'                                
237500                 MOVE 'PR '         TO LEV-KDKREBEH                       
237600               ELSE                                                       
237700                 MOVE TEST-KDKREBEH TO LEV-KDKREBEH                       
237800               END-IF                                                     
237900             END-IF                                                       
238000             MOVE W-KDARBTYP    TO LEV-KDARBTYP                           
238100                                   LEV-KDARBTYP-REM                       
238200             MOVE W-IDPERSON    TO LEV-IDPERSON                           
238300                                   LEV-IDPERSON-REM                       
238400             IF LEV-TIREMISS-UT = +0                                      
238500               MOVE WS-DATUM    TO LEV-TIREMISS-UT                        
238600             END-IF                                                       
238700             PERFORM IMS-REPL-KREE11                                      
238800             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
238900                  MOD-KDKREBEH-ATTRIBUT (IX)                              
239000             MOVE INF-UPDATE-OK TO MED-IDMFSFEL                           
239100             CALL WMEDKONV      USING MED-WMEDAREA                        
239200             MOVE MED-MFSFEL    TO MOD-TEMFSINF                           
239300           END-IF                                                         
239400         END-IF                                                           
239500       END-IF                                                             
239600       ADD +1                   TO IX                                     
239700     END-PERFORM                                                          
239800     IF MID-RAD18-IDARTNR NUMERIC AND MID-RAD18-IDRADNR NUMERIC           
239900        MOVE MID-RAD18-IDARTNR  TO W-IDARTNR                              
240000                                   W-IDARTNR-4109-MIN                     
240100                                   W-IDARTNR-4109-MAX                     
240200        MOVE MID-RAD18-IDRADNR  TO W-IDRADNR                              
240300        IF KREE-SEG-LEVEL = '01'                                          
240400           PERFORM IMS-GET-KREE11-GHNP                                    
240500        ELSE                                                              
240600           MOVE LEV-IDARTNR     TO JFR-IDARTNR                            
240700           MOVE LEV-IDRADNR     TO JFR-IDRADNR                            
240800           IF W-WDA211KY-X < JFR-WDA211-X                                 
240900              PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL                      
241000           ELSE                                                           
241100              IF W-WDA211KY-X > JFR-WDA211-X                              
241200                 PERFORM IMS-GET-KREE11-GHNP                              
241300              END-IF                                                      
241400           END-IF                                                         
241500        END-IF                                                            
241600                                                                          
241700        MOVE LEV-KDANMORS      TO SPAR-KDANMORS                           
241800        MOVE LEV-KVLEVANM-BEKR TO SPAR-KVLEVANM-BEKR                      
241900        MOVE LEV-PRARTBTO      TO SPAR-PRARTBTO                           
242000        MOVE LEV-PRARTBTO-LOC  TO SPAR-PRARTBTO-LOC                       
242100        MOVE LEV-PRARTBTO-LOCINV  TO SPAR-PRARTBTO-LOCINV                 
242200        MOVE LEV-IDDC-RET      TO SPAR-IDDC-RET                           
242300                                                                          
242400        IF KVLEVANM-WS NUMERIC                                            
242500           MOVE JA     TO ANTAL-AENDRAT-SW                                
242600           MOVE KVLEVANM-WS    TO LEV-KVLEVANM-BEKR                       
242700        END-IF                                                            
242800        IF KDANMORS-WS NUMERIC                                            
242900          MOVE JA  TO KOD-AENDRAD-SW                                      
243000          MOVE LEV-IDFTG  TO WS-IDFTG                                     
243100                                                                          
243200*- 2006-01-24 OM DET ÄR EN LDC-KUND KAN RETUR-DC VARA OLIKA.              
243300          PERFORM IMS-GU-GMTA-WDB201                                      
243400          IF SEGMENT-FINNS                                                
243500            CONTINUE                                                      
243600          ELSE                                                            
243700            PERFORM IMS-GET-WDB201                                        
243800          END-IF                                                          
243900          IF GMT-FLLDCKND = JA OR                                         
244000             GMT-FLRETUR  = JA                                            
244100            MOVE JA  TO KOD-LDCKUND-AENDRAD-SW                            
244200            PERFORM DD-KOLLA-IDDC-RET                                     
244300          END-IF                                                          
244400                                                                          
244500          MOVE KDANMORS-WS         TO LEV-KDANMORS                        
244600          IF KDANMORS-WS = '53' OR                                        
244700            ( IDFTG-PV AND KDANMORS-WS = '55' )                           
244800            MOVE WS-IDANALYS-UPPD  TO LEV-IDANALYS                        
244900            MOVE WS-IDKONTO-UPPD   TO LEV-IDKONTO                         
245000            MOVE WS-IDKST-UPPD     TO LEV-IDKST                           
245100          ELSE                                                            
245200             IF KDANMORS-WS = '52' OR                                     
245300             ( IDFTG-PV AND KDANMORS-WS = '54' )                          
245400               MOVE WS-IDANALYS-UPPD  TO LEV-IDANALYS                     
245500               MOVE ZERO              TO LEV-IDKONTO                      
245600               MOVE SPACE             TO LEV-IDKST                        
245700             END-IF                                                       
245800          END-IF                                                          
245900        END-IF                                                            
246000                                                                          
246100        IF DEC-KDSVAR-OK                                                  
246200          MOVE JA         TO PRIS-AENDRAT-SW                              
246300          MOVE LEV-IDFTG  TO WS-IDFTG                                     
246400          IF IDFTG-PV                                                     
246500            IF DIST79-DEALER-PRICE OR                                     
246700               DIST79-ECOM-PRICE                                          
246800              MOVE WS-IDEDITDATA TO LEV-PRARTBTO-LOC                      
246900            ELSE                                                          
247000              MOVE WS-IDEDITDATA TO LEV-PRARTBTO                          
247100            END-IF                                                        
247200          ELSE                                                            
247300            IF IDFTG-US OR IDFTG-CA OR IDFTG-CN OR IDFTG-IN OR            
247400               IDFTG-KR OR IDFTG-TR OR IDFTG-MY OR IDFTG-TH OR            
247500               IDFTG-TW OR IDFTG-MX OR IDFTG-BR OR IDFTG-ZA               
247600              MOVE WS-IDEDITDATA TO LEV-PRARTBTO-LOCINV                   
247700            END-IF                                                        
247800          END-IF                                                          
247900        END-IF                                                            
248000                                                                          
248100        IF FLDIRLEV-WS = JA                                               
248200           MOVE JA              TO LEV-FLDIRLEV                           
248300        ELSE                                                              
248400           IF FLDIRLEV-WS = NEJ                                           
248500              MOVE NEJ          TO LEV-FLDIRLEV                           
248600           END-IF                                                         
248700        END-IF                                                            
248800                                                                          
248900        IF SW-KDKREBEH-Q = JA                                             
249000          MOVE 'Q  '              TO LEV-KDKREBEH                         
249100        ELSE                                                              
249200          IF SW-KDKREBEH-P = JA                                           
249300            MOVE 'P  '            TO LEV-KDKREBEH                         
249400          ELSE                                                            
249500            MOVE LEV-KDKREBEH     TO TEST-KDKREBEH                        
249600            IF KDKREBEH-1 = 'C'                                           
249700               PERFORM DA-KONTROLLERA-KDKREBEH                            
249800            ELSE                                                          
249900               IF KDKREBEH-SPAR(1:1) = 'J'                                
250000                 MOVE 'Y'         TO KDKREBEH-SPAR(1:1)                   
250100               END-IF                                                     
250200               MOVE KDKREBEH-SPAR TO LEV-KDKREBEH                         
250300            END-IF                                                        
250400          END-IF                                                          
250500        END-IF                                                            
250600        PERFORM IMS-REPL-KREE11                                           
250700        MOVE LEV-KDANMORS TO OKOD-KDANMORS                                
250800                             W-KDANMORS-4109-MIN                          
250900                             W-KDANMORS-4109-MAX                          
251000        MOVE LEV-IDFTG    TO W-IDFTG-4109                                 
251100        CALL W418OKOD USING OKOD-W418OKOD                                 
251200        PERFORM S05-UPPD-ANALYSNRREGISTRET                                
251300                                                                          
251400**-- OM RAD ÄNDRAS SKALL SUMMAN FÖR KNOTAN RÄKNAS OM PÅ WDR5              
251500**-- WDGX4104 - SOX-ÄNDRING 20050425.UNDANTAG USA/CAN.                    
251600        MOVE MSGI-IDFTG  TO WS-IDFTG                                      
251700        IF IDFTG-PV OR IDFTG-NON-VCC                                      
251800          IF LEV-IDDC NOT = W-IDDC-B6                                     
251900            MOVE LEV-IDDC  TO W-IDDC-B6                                   
252000            PERFORM IMS-GU-WDB601                                         
252100          END-IF                                                          
252200          IF DCS-NDC-PF                                                   
252300            CONTINUE                                                      
252400          ELSE                                                            
252500            IF KDANMORS-WS NUMERIC                                        
252600              PERFORM S08-EV-UPPDAT-KOD-WDGX4103                          
252700            ELSE                                                          
252800              IF ANTAL-AENDRAT OR PRIS-AENDRAT                            
252900                PERFORM S09-EV-UPPDAT-RAD18-WDGX4103                      
253000              END-IF                                                      
253100            END-IF                                                        
253200          END-IF                                                          
253300        END-IF                                                            
253400                                                                          
253500**-- OM KOD ÄNDRAS FÖR LDC-KUND SÅ MÅSTE MAN KOLLA IGEN OM RETUR-         
253600**-  DC'T PÅ WDA201 BEHÖVER ÄNDRAS.ETT RETUR-DC PER LA.                   
253700        IF KOD-AENDRAD                                                    
253800          IF KOD-LDCKUND-AENDRAD                                          
253900            PERFORM DE-KOLLA-IDDC-RET-WDA201                              
254000          ELSE                                                            
254100            PERFORM DF-KOLLA-IDDC-RET-OEVRIGT                             
254200          END-IF                                                          
254300        END-IF                                                            
254400                                                                          
254500        PERFORM S04-RENSA-RAD18                                           
254600        MOVE INF-UPDATE-OK   TO MED-IDMFSFEL                              
254700        CALL WMEDKONV USING MED-WMEDAREA                                  
254800        MOVE MED-MFSFEL      TO MOD-TEMFSINF                              
254900     END-IF                                                               
255000                                                                          
255100*-KOLLA IFALL DET FORTFARANDE FINNS RADER SOM GER KN OCH SKALL            
255200*-ATTESTERAS.                                                             
255300     MOVE MSGI-IDFTG  TO WS-IDFTG                                         
255400     IF IDFTG-PV OR IDFTG-CN                                              
255500       IF ANTAL-AENDRAT OR                                                
255600          PRIS-AENDRAT  OR                                                
255700          KOD-AENDRAD   OR                                                
255800          NEKAD-RAD     OR                                                
255900          ANNULLERAD-RAD OR                                               
256000          ANGRA-NEKAD-RAD                                                 
256100                                                                          
256200         PERFORM DC-KOLLA-KDLEVATT-WDA201                                 
256300       END-IF                                                             
256400     END-IF                                                               
256500                                                                          
256600**-- OM RAD NEKAS SKALL IDDC-RET KOLLAS/ÄNDRAS PÅ WDA201.                 
256700     IF NEKAD-RETUR-RAD                                                   
256800         PERFORM IMS-GU-GMTA-WDB201                                       
256900         IF SEGMENT-FINNS                                                 
257000           CONTINUE                                                       
257100         ELSE                                                             
257200           PERFORM IMS-GET-WDB201                                         
257300         END-IF                                                           
257400         IF GMT-FLLDCKND = JA OR                                          
257500            GMT-FLRETUR  = JA                                             
257600           PERFORM DG-EV-UPPD-DC-RET-WDA201                               
257700         ELSE                                                             
257800           PERFORM DH-EV-UPPD-DC-RET-OEVRIGT                              
257900         END-IF                                                           
258000     END-IF                                                               
258100                                                                          
258200     IF ANGRA-NEKAD-RETUR-RAD                                             
258300         PERFORM IMS-GU-GMTA-WDB201                                       
258400         IF SEGMENT-FINNS                                                 
258500           CONTINUE                                                       
258600         ELSE                                                             
258700           PERFORM IMS-GET-WDB201                                         
258800         END-IF                                                           
258900         IF GMT-FLLDCKND = JA OR                                          
259000            GMT-FLRETUR  = JA                                             
259100           PERFORM DI-UPPD-DC-RET-LDC                                     
259200         ELSE                                                             
259300           PERFORM DJ-UPPD-DC-RET-EJ-LDC                                  
259400         END-IF                                                           
259500     END-IF                                                               
259600     .                                                                    
259700     EJECT                                                                
259800 DA-KONTROLLERA-KDKREBEH        SECTION.                                  
259900                                                                          
260000     IF KDKREBEH-3 = '1'                                                  
260100        IF KDKREBEH-SPAR-2 = 2 OR 5                                       
260200           MOVE '2'             TO KDKREBEH-3                             
260300        ELSE                                                              
260400           IF KDKREBEH-SPAR-2 = 3 OR 6                                    
260500              MOVE '3'          TO KDKREBEH-3                             
260600           ELSE                                                           
260700              IF KDKREBEH-SPAR-2 = 4 OR 7                                 
260800                 MOVE '4'       TO KDKREBEH-3                             
260900              END-IF                                                      
261000           END-IF                                                         
261100        END-IF                                                            
261200     ELSE                                                                 
261300        IF KDKREBEH-3 = '2'                                               
261400           IF KDKREBEH-SPAR-2 = 3 OR 4 OR 6 OR 7                          
261500              MOVE '4'          TO KDKREBEH-3                             
261600           END-IF                                                         
261700        ELSE                                                              
261800           IF KDKREBEH-3 = '3'                                            
261900              IF KDKREBEH-SPAR-2 = 2 OR 4 OR 5 OR 7                       
262000                 MOVE '4'       TO KDKREBEH-3                             
262100              END-IF                                                      
262200           ELSE                                                           
262300              IF KDKREBEH-3 = '5'                                         
262400                 IF KDKREBEH-SPAR-2 = 1 OR 2                              
262500                    MOVE '2'    TO KDKREBEH-3                             
262600                 ELSE                                                     
262700                    IF KDKREBEH-SPAR-2 = 3 OR 4                           
262800                       MOVE '4' TO KDKREBEH-3                             
262900                    ELSE                                                  
263000                       IF KDKREBEH-SPAR-2 = 7                             
263100                          MOVE '7' TO KDKREBEH-3                          
263200                       END-IF                                             
263300                    END-IF                                                
263400                 END-IF                                                   
263500              ELSE                                                        
263600                 IF KDKREBEH-3 = '6'                                      
263700                    IF KDKREBEH-SPAR-2 = 1                                
263800                       MOVE '3' TO KDKREBEH-3                             
263900                    ELSE                                                  
264000                       IF KDKREBEH-SPAR-2 = 2 OR 4                        
264100                          MOVE '4' TO KDKREBEH-3                          
264200                       ELSE                                               
264300                          IF KDKREBEH-SPAR-2 = 3                          
264400                             MOVE '3' TO KDKREBEH-3                       
264500                          ELSE                                            
264600                             IF KDKREBEH-SPAR-2 = 5 OR 7                  
264700                                MOVE '7' TO KDKREBEH-3                    
264800                             END-IF                                       
264900                          END-IF                                          
265000                       END-IF                                             
265100                    END-IF                                                
265200                 ELSE                                                     
265300                    IF KDKREBEH-3 = '7'                                   
265400                       IF KDKREBEH-SPAR-2 = 1 OR 2 OR 3 OR 4 OR 7         
265500                          MOVE '4' TO KDKREBEH-3                          
265600                       END-IF                                             
265700                    ELSE                                                  
265800                       IF KDKREBEH-3 = '0'                                
265900                          MOVE KDKREBEH-SPAR-2 TO KDKREBEH-3              
266000                       END-IF                                             
266100                    END-IF                                                
266200                 END-IF                                                   
266300              END-IF                                                      
266400           END-IF                                                         
266500        END-IF                                                            
266600     END-IF                                                               
266700     IF KDKREBEH-SPAR-1 = 1                                               
266800        MOVE '1'                TO KDKREBEH-2                             
266900     END-IF                                                               
267000     IF TEST-KDKREBEH(1:1) = 'J'                                          
267100       MOVE 'Y'                 TO TEST-KDKREBEH(1:1)                     
267200     END-IF                                                               
267300     MOVE TEST-KDKREBEH         TO LEV-KDKREBEH                           
267400     .                                                                    
267500     EJECT                                                                
267600 DB-SKAPA-REMISS-MAIL           SECTION.                                  
267700                                                                          
267800     INSPECT MID-IDARTNR(IX) REPLACING LEADING SPACE BY ZERO              
267900     INSPECT MID-IDRADNR(IX) REPLACING LEADING SPACE BY ZERO              
268000                                                                          
268100     MOVE MID-IDARTNR(IX)       TO W-IDARTNR                              
268200                                   W-IDARTNR-ARTC                         
268300     MOVE MID-IDRADNR(IX)       TO W-IDRADNR                              
268400                                                                          
268500     PERFORM IMS-GET-KREE11-GHNP-FIRST-KVAL                               
268600                                                                          
268700     PERFORM S03-GET-RESPONSIBLE                                          
268800                                                                          
268900     IF ANSV-OK AND SEGMENT-FINNS                                         
269000                                                                          
269100       PERFORM DBB-FIXA-LEVANM-INFO                                       
269200                                                                          
269300       PERFORM DBC-FIXA-FAKT-INFO                                         
269400       PERFORM DBD-FIXA-ART-INFO                                          
269500                                                                          
269600       CALL W418MERE USING MERE-W418MERE MAIL-PCB                         
269700     END-IF                                                               
269800     .                                                                    
269900                                                                          
270000     EJECT                                                                
270100 DBB-FIXA-LEVANM-INFO           SECTION.                                  
270200                                                                          
270300     MOVE MSGI-IDDISTR          TO MERE-IDDISTR                           
270400     MOVE MSGI-IDKUNDNR         TO MERE-IDKUNDNR                          
270500     MOVE MSGI-IDRAPPNR         TO MERE-IDRAPPNR                          
270600                                                                          
270700     MOVE LEV-IDARTNR           TO MERE-IDARTNR                           
270800                                   W-IDARTNR                              
270900     MOVE LEV-IDRADNR           TO MERE-IDRADNR                           
271000                                   W-IDRADNR                              
271100     MOVE LEV-KDANMORS          TO MERE-KDANMORS                          
271200     MOVE LEV-KVLEVANM-BEKR     TO MERE-KVLEVANM-BEKR                     
271300                                                                          
271400     MOVE LEV-IDFTG   TO WS-IDFTG                                         
271500     IF IDFTG-PV                                                          
271600       IF DIST79-DEALER-PRICE OR                                          
271800          DIST79-ECOM-PRICE                                               
271900         MOVE LEV-PRARTBTO-LOC  TO MERE-PRARTBTO                          
272000       ELSE                                                               
272100         MOVE LEV-PRARTBTO      TO MERE-PRARTBTO                          
272200       END-IF                                                             
272300     ELSE                                                                 
272400       IF IDFTG-US OR IDFTG-CA OR IDFTG-CN OR IDFTG-IN OR                 
272500          IDFTG-KR OR IDFTG-TR OR IDFTG-MY OR IDFTG-TH OR                 
272600          IDFTG-TW OR IDFTG-MX OR IDFTG-BR OR IDFTG-ZA                    
272700         MOVE LEV-PRARTBTO-LOCINV  TO MERE-PRARTBTO                       
272800       END-IF                                                             
272900     END-IF                                                               
273000                                                                          
273100     MOVE LEV-IDKUNDRF          TO MERE-IDKUNDRF                          
273200     MOVE LEV-IDKOLLI           TO MERE-IDKOLLI                           
273300     MOVE LEV-KDFAKTYP          TO MERE-KDFAKTYP                          
273400                                                                          
273500     MOVE LEV-IDFAKT            TO MERE-IDFAKT                            
273600     MOVE LEV-TIFAKT            TO MERE-TIFAKT                            
273700                                                                          
273800     MOVE +1                    TO INDX                                   
273900     PERFORM UNTIL INDX        >  3                                       
274000      MOVE SPACE                TO MERE-TEANMNOT-REG(INDX)                
274100                                   MERE-TEANMNOT-ADM(INDX)                
274200                                   MERE-TEANMNOT-REM(INDX)                
274300                                   MERE-TEANMNOT-RET(INDX)                
274400                                                                          
274500      ADD +1                    TO INDX                                   
274600     END-PERFORM                                                          
274700                                                                          
274800     IF LEV-FLTEXT             =  JA                                      
274900        PERFORM IMS-GET-KREE21-GNP                                        
275000        IF SEGMENT-FINNS                                                  
275100           MOVE +1              TO INDX                                   
275200           PERFORM UNTIL INDX           >  3                              
275300            MOVE TXT-TEANMNOT-REG(INDX) TO MERE-TEANMNOT-REG(INDX)        
275400            MOVE TXT-TEANMNOT-ADM(INDX) TO MERE-TEANMNOT-ADM(INDX)        
275500            MOVE TXT-TEANMNOT-REM(INDX) TO MERE-TEANMNOT-REM(INDX)        
275600            MOVE TXT-TEANMNOT-RET(INDX) TO MERE-TEANMNOT-RET(INDX)        
275700                                                                          
275800            ADD +1              TO INDX                                   
275900           END-PERFORM                                                    
276000        END-IF                                                            
276100     END-IF                                                               
276200                                                                          
276300     .                                                                    
276400     EJECT                                                                
276500                                                                          
276600 DBC-FIXA-FAKT-INFO             SECTION.                                  
276700                                                                          
277300     MOVE MSGI-IDDISTR          TO W-IDDISTR-L5                           
277400     MOVE MSGI-IDKUNDNR         TO W-IDKUNDNR-L5                          
277500     MOVE LEV-IDKUNDRF          TO W-IDKUNDRF-L5                          
277600     MOVE LEV-IDKOLLI           TO W-IDKOLLI-L5                           
277700     MOVE LEV-IDARTNR           TO W-IDARTNR-L5                           
277800                                                                          
277900     MOVE LEV-IDFAKT            TO W-IDFAKT                               
278300     PERFORM IMS-GU-WDL501                                                
278400     IF SEGMENT-FINNS                                                     
278500                                                                          
279200       PERFORM IMS-GNP-WDL511                                             
279300       IF SEGMENT-FINNS                                                   
279400         MOVE FAKC-IDPRODNR       TO W-IDPRODNR-L5                        
279500                                     MERE-IDPRODNR                        
279600         MOVE FAKC-VKORDBTO-KOLLI TO MERE-VKORDBTO-KOLLI                  
279700         MOVE FAKC-VKORDNTO-KOLLI TO MERE-VKORDNTO-KOLLI                  
279800         MOVE ZERO                TO MERE-VKORDNTO-TOT                    
279900                                     MERE-VKTARA                          
280000         MOVE FAKC-KDORDKL        TO MERE-KDORDKL                         
280100         MOVE FAKC-TIORDREG       TO MERE-TIREGDAT                        
280200         MOVE FAK-FLDIRLEV        TO MERE-FLDIRLEV                        
280300                                                                          
280400         PERFORM IMS-GNP-WDL521                                           
280500         IF SEGMENT-FINNS                                                 
280600           MOVE FAKL-KVBEART-Q    TO MERE-KVBEART-Q                       
280700           MOVE FAKL-KVLEVART     TO MERE-KVLEVART                        
281100           MOVE FAKL-KVORDRAD     TO MERE-KVORDRAD                        
281200           MOVE FAKL-IDUSER-OREG  TO MERE-IDUSER-OREG                     
281400           MOVE FAKL-IDUSER-PACK  TO MERE-IDUSER-PACK                     
281500         ELSE                                                             
281600          MOVE ZERO               TO MERE-KVBEART-Q                       
281700                                     MERE-KVLEVART                        
281800                                     MERE-KVORDRAD                        
281900                                     MERE-TIREGDAT                        
282000          MOVE SPACE              TO MERE-IDUSER-PACK                     
282100                                     MERE-IDUSER-OREG                     
282200         END-IF                                                           
282300       ELSE                                                               
282400         MOVE ZERO                TO MERE-VKORDBTO-KOLLI                  
282500                                     MERE-VKORDNTO-KOLLI                  
282600                                     MERE-VKORDNTO-TOT                    
282700                                     MERE-VKTARA                          
282800                                     MERE-KDORDKL                         
282900                                     MERE-IDPRODNR                        
283000          MOVE SPACE              TO MERE-FLDIRLEV                        
283100                                                                          
283900       END-IF                                                             
284000     ELSE                                                                 
284100       MOVE ZERO                  TO MERE-KVBEART-Q                       
284200                                     MERE-KVLEVART                        
284300                                     MERE-KVORDRAD                        
284400                                     MERE-TIREGDAT                        
284700                                     MERE-VKORDBTO-KOLLI                  
284800                                     MERE-VKORDNTO-KOLLI                  
284900                                     MERE-VKORDNTO-TOT                    
285000                                     MERE-VKTARA                          
285100                                     MERE-KDORDKL                         
285200                                     MERE-IDPRODNR                        
285300       MOVE SPACE                 TO MERE-IDUSER-PACK                     
285400                                     MERE-IDUSER-OREG                     
285500                                     MERE-FLDIRLEV                        
285600     END-IF                                                               
285700                                                                          
285800     .                                                                    
285900     EJECT                                                                
286000                                                                          
286100 DBD-FIXA-ART-INFO              SECTION.                                  
286200                                                                          
286300     PERFORM IMS-GU-ARTC01                                                
286400                                                                          
286500     MOVE ART-REKSIFFR          TO MERE-REKSIFFR                          
286600     MOVE ART-IDFKNGRP          TO MERE-IDFKNGRP                          
286700     MOVE ART-KDPRODSL          TO MERE-KDPRODSL                          
286800                                                                          
286900     PERFORM IMS-GNP-ARTC11                                               
287000                                                                          
287100     COMPUTE W-KVPB-TOT          = CLAG-KVPB-SEP +                        
287200                                 CLAG-KVPB-SATS +                         
287300                                 CLAG-KVPB-TPO                            
287400                                                                          
287500     MOVE CLAG-ADLAGOMR         TO MERE-ADLAGOMR                          
287600     MOVE CLAG-ADGANG           TO MERE-ADGANG                            
287700     MOVE CLAG-ADPLATS          TO MERE-ADPLATS                           
287800     MOVE CLAG-KVLS             TO MERE-KVLS                              
287900     MOVE W-KVPB-TOT            TO MERE-KVPB-TOT                          
288000     MOVE CLAG-KDERS            TO MERE-KDERS                             
288100     MOVE CLAG-VKART            TO MERE-VKART                             
288200     MOVE CLAG-PRARTBTO-EXP     TO MERE-PRARTBTO-EXP                      
288300     MOVE CLAG-PRINK            TO MERE-PRINK                             
288400     MOVE CLAG-TIINVDAT         TO MERE-TIINVDAT                          
288500     MOVE CLAG-KVINVS           TO MERE-KVINVS                            
288600     MOVE CLAG-IDANSK           TO MERE-IDANSK                            
288700                                                                          
288800     MOVE LEV-IDARTNR           TO W-IDARTNR-K7                           
288900     MOVE LEV-IDDC              TO W-IDDC-K7                              
289000     PERFORM IMS-GU-WDK722                                                
289100     IF SEGMENT-FINNS                                                     
289200        MOVE XLAG-IDANSK        TO MERE-IDANSK                            
289300     END-IF                                                               
289400                                                                          
289500     IF LEV-IDDC NOT = W-IDDC-B6                                          
289600        MOVE LEV-IDDC  TO W-IDDC-B6                                       
289700        PERFORM IMS-GU-WDB601                                             
289800     END-IF                                                               
289900     MOVE DCS-IDLANDX2          TO W-IDLAND-K7                            
290000     PERFORM IMS-GU-WDK712                                                
290100     IF SEGMENT-FINNS                                                     
290200       IF LART-VKART > 0                                                  
290300         MOVE LART-VKART         TO MERE-VKART                            
290400       END-IF                                                             
290500     END-IF                                                               
290600                                                                          
290700     PERFORM IMS-GU-BENA11                                                
290800     IF SEGMENT-FINNS                                                     
290900        MOVE TEXT-BEART         TO MERE-BEART                             
291000     ELSE                                                                 
291100        MOVE SPACE              TO MERE-BEART                             
291200     END-IF                                                               
291300                                                                          
291400     .                                                                    
291500     EJECT                                                                
291600 DC-KOLLA-KDLEVATT-WDA201       SECTION.                                  
291700                                                                          
291800     MOVE NEJ TO KNOTA-RAD-FINNS-SW                                       
291900                                                                          
292000     PERFORM IMS-GU-WDGX4103                                              
292100     IF SEGMENT-FINNS                                                     
292200       PERFORM IMS-GHNP-WDGX4104                                          
292300       PERFORM UNTIL SEGMENT-SAKNAS                                       
292400         IF 4104-SUKRENOT = ZERO                                          
292500           PERFORM IMS-DLET-WDGX4104                                      
292600         ELSE                                                             
292700           MOVE JA  TO KNOTA-RAD-FINNS-SW                                 
292800         END-IF                                                           
292900         PERFORM IMS-GHNP-WDGX4104                                        
293000       END-PERFORM                                                        
293100                                                                          
293200       PERFORM IMS-GET-KREE01-GHU                                         
293300       IF SEGMENT-FINNS                                                   
293400         IF KNOTA-RAD-FINNS                                               
293500           IF ANM-KDLEVATT = 0                                            
293600             MOVE 1 TO ANM-KDLEVATT                                       
293700             PERFORM IMS-REPL-KREE01                                      
293800           END-IF                                                         
293900         ELSE                                                             
294000           IF ANM-KDLEVATT = 1                                            
294100             MOVE 0 TO ANM-KDLEVATT                                       
294200             PERFORM IMS-REPL-KREE01                                      
294300           END-IF                                                         
294400         END-IF                                                           
294500       END-IF                                                             
294600     END-IF                                                               
294700     .                                                                    
294800     EJECT                                                                
294900 DD-KOLLA-IDDC-RET    SECTION.                                            
295000                                                                          
295100*-IFALL MAN ÄNDRAR KOD PÅ EN GAMMAL RAD ( FÖRE ÖVERGÅNG TILL OLIKA        
295200*-RETUR-DC FÖR LDC'R) SÅ BLIR RETUR-DC O IX SAMMA SOM TIDIGARE.           
295300                                                                          
295400     MOVE KDANMORS-WS  TO OKOD-KDANMORS                                   
295500     CALL W418OKOD USING OKOD-W418OKOD                                    
295600                                                                          
295700     IF OKOD-FL-RETILL = JA                                               
295800                                                                          
295900       IF ANM-IXDCCLEAR = 0 AND (ANM-IDDC-RET NOT = SPACE)                
296000         MOVE ANM-IDDC-RET          TO LEV-IDDC-RET                       
296100                                       WS-ANM-IDDC-RET                    
296200         MOVE 0                     TO WS-ANM-IXDCCLEAR                   
296300       ELSE                                                               
296400         IF GMT-IDDC-RET72(1) = SPACE                                     
296500           IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                     
296600              DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                    
296700              DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR                 
296800              DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                 
296810              DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC                
296900             MOVE GMT-IDDC-RET      TO LEV-IDDC-RET                       
297000                                       WS-ANM-IDDC-RET                    
297100           ELSE                                                           
297200             MOVE WS-CDC-SE         TO LEV-IDDC-RET                       
297300                                       WS-ANM-IDDC-RET                    
297400           END-IF                                                         
297500           MOVE 3                   TO WS-ANM-IXDCCLEAR                   
297600         ELSE                                                             
297700           IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                     
297800              DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                    
297900              DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR                 
298000              DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                 
298010              DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC                
298100             IF GMT-IDDC-RET72(1) = GMT-IDDC-RET                          
298200               MOVE GMT-IDDC-RET      TO LEV-IDDC-RET                     
298300                                         WS-ANM-IDDC-RET                  
298400               MOVE 3                 TO WS-ANM-IXDCCLEAR                 
298500             ELSE                                                         
298600               PERFORM S13-KOLLA-RETUR-DC                                 
298700             END-IF                                                       
298800           ELSE                                                           
298900             IF GMT-IDDC-RET72(1) = WS-CDC-SE                             
299000               MOVE WS-CDC-SE         TO LEV-IDDC-RET                     
299100                                         WS-ANM-IDDC-RET                  
299200               MOVE 3                 TO WS-ANM-IXDCCLEAR                 
299300             ELSE                                                         
299400               PERFORM S13-KOLLA-RETUR-DC                                 
299500             END-IF                                                       
299600           END-IF                                                         
299700         END-IF                                                           
299800       END-IF                                                             
299900     ELSE                                                                 
300000       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
300100          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
300200          DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR                     
300300          DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                     
300310          DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC                    
300400         IF SPAR-IDDC-RET = GMT-IDDC-RET                                  
300500           CONTINUE                                                       
300600         ELSE                                                             
300700           MOVE GMT-IDDC-RET       TO LEV-IDDC-RET                        
300800         END-IF                                                           
300900       ELSE                                                               
301000         IF SPAR-IDDC-RET = WS-CDC-SE                                     
301100           CONTINUE                                                       
301200         ELSE                                                             
301300           MOVE WS-CDC-SE          TO LEV-IDDC-RET                        
301400         END-IF                                                           
301500       END-IF                                                             
301600       MOVE SPACE                TO WS-ANM-IDDC-RET                       
301700       MOVE 0                    TO WS-ANM-IXDCCLEAR                      
301800     END-IF                                                               
301900     .                                                                    
302000     EJECT                                                                
302100 DE-KOLLA-IDDC-RET-WDA201       SECTION.                                  
302200                                                                          
302300*- KOLLA OM GAMLA KODEN ÄR EN RETURKOD.                                   
302400     MOVE SPAR-KDANMORS  TO OKOD-KDANMORS                                 
302500     CALL W418OKOD USING OKOD-W418OKOD                                    
302600                                                                          
302700     IF OKOD-FL-RETILL = JA                                               
302800       PERFORM DEA-KOLLA-OM-RETUR-KVAR                                    
302900     ELSE                                                                 
303000       MOVE KDANMORS-WS  TO OKOD-KDANMORS                                 
303100       CALL W418OKOD USING OKOD-W418OKOD                                  
303200       IF OKOD-FL-RETILL = JA                                             
303300         PERFORM IMS-GET-KREE01-GHU                                       
303400         IF SEGMENT-FINNS                                                 
303500           IF ANM-IDDC-RET = SPACE                                        
303600             MOVE WS-ANM-IDDC-RET  TO ANM-IDDC-RET                        
303700             MOVE WS-ANM-IXDCCLEAR TO ANM-IXDCCLEAR                       
303800             PERFORM IMS-REPL-KREE01                                      
303900           ELSE                                                           
304000             IF ANM-IXDCCLEAR = 3 OR 0                                    
304100               CONTINUE                                                   
304200             ELSE                                                         
304300               IF WS-ANM-IXDCCLEAR = 3                                    
304400                 MOVE WS-ANM-IDDC-RET  TO ANM-IDDC-RET                    
304500                 MOVE WS-ANM-IXDCCLEAR TO ANM-IXDCCLEAR                   
304600                 PERFORM IMS-REPL-KREE01                                  
304700               ELSE                                                       
304800                 IF ANM-IXDCCLEAR = 1 AND WS-ANM-IXDCCLEAR = 2            
304900                   MOVE WS-ANM-IDDC-RET  TO ANM-IDDC-RET                  
305000                   MOVE WS-ANM-IXDCCLEAR TO ANM-IXDCCLEAR                 
305100                   PERFORM IMS-REPL-KREE01                                
305200                 END-IF                                                   
305300               END-IF                                                     
305400             END-IF                                                       
305500           END-IF                                                         
305600         END-IF                                                           
305700       ELSE                                                               
305800         CONTINUE                                                         
305900       END-IF                                                             
306000     END-IF                                                               
306100     .                                                                    
306200     EJECT                                                                
306300 DEA-KOLLA-OM-RETUR-KVAR        SECTION.                                  
306400                                                                          
306500     MOVE NEJ TO RETURRADER-KVAR-SW                                       
306600     MOVE NEJ TO IXDCCLEAR-2-SW                                           
306700     MOVE NEJ TO IXDCCLEAR-3-SW                                           
306800                                                                          
306900     PERFORM IMS-GET-KREE01-KVAL                                          
307000     IF SEGMENT-FINNS                                                     
307100       PERFORM IMS-GET-KREE11-GNP                                         
307200       PERFORM UNTIL SEGMENT-SAKNAS                                       
307300         IF LEV-KDKREBEH(1:1) = 'N'                                       
307400           CONTINUE                                                       
307500         ELSE                                                             
307600           MOVE LEV-KDANMORS  TO OKOD-KDANMORS                            
307700           CALL W418OKOD USING OKOD-W418OKOD                              
307800                                                                          
307900           IF OKOD-FL-RETILL = JA                                         
308000             MOVE JA TO RETURRADER-KVAR-SW                                
308100             IF ANM-IXDCCLEAR = 0                                         
308200               CONTINUE                                                   
308300             ELSE                                                         
308400               IF LEV-IDDC-RET = GMT-IDDC-RET72 (2)                       
308500                 MOVE JA TO IXDCCLEAR-2-SW                                
308600               END-IF                                                     
308700               IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                 
308800                  DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                
308900                  DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR             
309000                  DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR             
309010                  DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC            
309100                 IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                  
309200                    LEV-IDDC-RET = GMT-IDDC-RET                           
309300                   MOVE JA TO IXDCCLEAR-3-SW                              
309400                 END-IF                                                   
309500               ELSE                                                       
309600                 IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                  
309700                    LEV-IDDC-RET = WS-CDC-SE                              
309800                   MOVE JA TO IXDCCLEAR-3-SW                              
309900                 END-IF                                                   
310000               END-IF                                                     
310100             END-IF                                                       
310200           END-IF                                                         
310300         END-IF                                                           
310400                                                                          
310500         PERFORM IMS-GET-KREE11-GNP                                       
310600       END-PERFORM                                                        
310700     END-IF                                                               
310800                                                                          
310900     IF RETURRADER-KVAR                                                   
311000       IF ANM-IXDCCLEAR = 3                                               
311100         IF IXDCCLEAR-3-SW = JA                                           
311200           CONTINUE                                                       
311300         ELSE                                                             
311400           IF IXDCCLEAR-2-SW = JA                                         
311500             PERFORM IMS-GET-KREE01-GHU                                   
311600             MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                     
311700             MOVE 2                   TO ANM-IXDCCLEAR                    
311800             PERFORM IMS-REPL-KREE01                                      
311900           ELSE                                                           
312000             PERFORM IMS-GET-KREE01-GHU                                   
312100             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
312200             MOVE 1                   TO ANM-IXDCCLEAR                    
312300             PERFORM IMS-REPL-KREE01                                      
312400           END-IF                                                         
312500         END-IF                                                           
312600       ELSE                                                               
312700         IF ANM-IXDCCLEAR = 2                                             
312800           IF IXDCCLEAR-2-SW = JA                                         
312900             CONTINUE                                                     
313000           ELSE                                                           
313100             PERFORM IMS-GET-KREE01-GHU                                   
313200             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
313300             MOVE 1                   TO ANM-IXDCCLEAR                    
313400             PERFORM IMS-REPL-KREE01                                      
313500           END-IF                                                         
313600         ELSE                                                             
313700           IF ANM-IXDCCLEAR = 1                                           
313800             IF IXDCCLEAR-3-SW = JA                                       
313900               PERFORM IMS-GET-KREE01-GHU                                 
314000               MOVE GMT-IDDC-RET72 (3)  TO ANM-IDDC-RET                   
314100               IF ANM-IDDC-RET = SPACE                                    
314200                 IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR               
314300                    DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR              
314400                    DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR           
314500                    DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR           
314510                    DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC          
314600                   MOVE GMT-IDDC-RET    TO ANM-IDDC-RET                   
314700                 ELSE                                                     
314800                   MOVE WS-CDC-SE       TO ANM-IDDC-RET                   
314900                 END-IF                                                   
315000               END-IF                                                     
315100               MOVE 3                   TO ANM-IXDCCLEAR                  
315200               PERFORM IMS-REPL-KREE01                                    
315300             ELSE                                                         
315400               IF IXDCCLEAR-2-SW = JA                                     
315500                 PERFORM IMS-GET-KREE01-GHU                               
315600                 MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                 
315700                 MOVE 2                   TO ANM-IXDCCLEAR                
315800                 PERFORM IMS-REPL-KREE01                                  
315900               END-IF                                                     
316000             END-IF                                                       
316100           END-IF                                                         
316200         END-IF                                                           
316300       END-IF                                                             
316400     ELSE                                                                 
316500       PERFORM IMS-GET-KREE01-GHU                                         
316600       MOVE SPACE            TO ANM-IDDC-RET                              
316700       MOVE 0                TO ANM-IXDCCLEAR                             
316800       PERFORM IMS-REPL-KREE01                                            
316900     END-IF                                                               
317000     .                                                                    
317100     EJECT                                                                
317200 DF-KOLLA-IDDC-RET-OEVRIGT SECTION.                                       
317300                                                                          
317400*- KOLLA OM GAMLA KODEN ÄR EN RETURKOD.                                   
317500     MOVE SPAR-KDANMORS  TO OKOD-KDANMORS                                 
317600     CALL W418OKOD USING OKOD-W418OKOD                                    
317700                                                                          
317800     IF OKOD-FL-RETILL = JA                                               
317900       PERFORM DFA-KOLLA-RETURRADER                                       
318000     ELSE                                                                 
318100       MOVE KDANMORS-WS   TO OKOD-KDANMORS                                
318200       CALL W418OKOD USING OKOD-W418OKOD                                  
318300       IF OKOD-FL-RETILL = JA                                             
318400         PERFORM IMS-GET-KREE01-GHU                                       
318500         IF SEGMENT-FINNS                                                 
318600           IF ANM-IDDC-RET = SPACE                                        
318700             MOVE GMT-IDDC-RET     TO ANM-IDDC-RET                        
318800             MOVE 3                TO ANM-IXDCCLEAR                       
318900             PERFORM IMS-REPL-KREE01                                      
319000           ELSE                                                           
319100             CONTINUE                                                     
319200           END-IF                                                         
319300         END-IF                                                           
319400       ELSE                                                               
319500         CONTINUE                                                         
319600       END-IF                                                             
319700     END-IF                                                               
319800     .                                                                    
319900     EJECT                                                                
320000 DFA-KOLLA-RETURRADER  SECTION.                                           
320100                                                                          
320200     MOVE NEJ TO RETURRADER-KVAR-SW                                       
320300                                                                          
320400     PERFORM IMS-GET-KREE01-KVAL                                          
320500     IF SEGMENT-FINNS                                                     
320600       PERFORM IMS-GET-KREE11-GNP                                         
320700       PERFORM UNTIL SEGMENT-SAKNAS OR RETURRADER-KVAR-SW = JA            
320800         IF LEV-KDKREBEH(1:1) = 'N'                                       
320900           CONTINUE                                                       
321000         ELSE                                                             
321100           MOVE LEV-KDANMORS   TO OKOD-KDANMORS                           
321200           CALL W418OKOD USING OKOD-W418OKOD                              
321300                                                                          
321400           IF OKOD-FL-RETILL = JA                                         
321500             MOVE JA TO RETURRADER-KVAR-SW                                
321600           END-IF                                                         
321700         END-IF                                                           
321800                                                                          
321900         PERFORM IMS-GET-KREE11-GNP                                       
322000       END-PERFORM                                                        
322100     END-IF                                                               
322200                                                                          
322300     IF RETURRADER-KVAR                                                   
322400       CONTINUE                                                           
322500     ELSE                                                                 
322600       PERFORM IMS-GET-KREE01-GHU                                         
322700       MOVE SPACE            TO ANM-IDDC-RET                              
322800       MOVE 0                TO ANM-IXDCCLEAR                             
322900       PERFORM IMS-REPL-KREE01                                            
323000     END-IF                                                               
323100     .                                                                    
323200     EJECT                                                                
323300 DG-EV-UPPD-DC-RET-WDA201 SECTION.                                        
323400                                                                          
323500     MOVE NEJ TO RETURRADER-KVAR-SW                                       
323600     MOVE NEJ TO IXDCCLEAR-2-SW                                           
323700     MOVE NEJ TO IXDCCLEAR-3-SW                                           
323800                                                                          
323900     PERFORM IMS-GET-KREE01-KVAL                                          
324000     IF SEGMENT-FINNS                                                     
324100       PERFORM IMS-GET-KREE11-GNP                                         
324200       PERFORM UNTIL SEGMENT-SAKNAS                                       
324300         IF LEV-KDKREBEH(1:1) = 'N'                                       
324400           CONTINUE                                                       
324500         ELSE                                                             
324600           MOVE LEV-KDANMORS  TO OKOD-KDANMORS                            
324700           CALL W418OKOD USING OKOD-W418OKOD                              
324800                                                                          
324900           IF OKOD-FL-RETILL = JA                                         
325000             MOVE JA TO RETURRADER-KVAR-SW                                
325100             IF ANM-IXDCCLEAR = 0                                         
325200               CONTINUE                                                   
325300             ELSE                                                         
325400               IF LEV-IDDC-RET = GMT-IDDC-RET72 (2)                       
325500                 MOVE JA TO IXDCCLEAR-2-SW                                
325600               END-IF                                                     
325700               IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                 
325800                  DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                
325900                  DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR             
326000                  DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR             
326010                  DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC            
326100                 IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                  
326200                    LEV-IDDC-RET = GMT-IDDC-RET                           
326300                   MOVE JA TO IXDCCLEAR-3-SW                              
326400                 END-IF                                                   
326500               ELSE                                                       
326600                 IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                  
326700                    LEV-IDDC-RET = WS-CDC-SE                              
326800                   MOVE JA TO IXDCCLEAR-3-SW                              
326900                 END-IF                                                   
327000               END-IF                                                     
327100             END-IF                                                       
327200           END-IF                                                         
327300         END-IF                                                           
327400                                                                          
327500         PERFORM IMS-GET-KREE11-GNP                                       
327600       END-PERFORM                                                        
327700     END-IF                                                               
327800                                                                          
327900     IF RETURRADER-KVAR                                                   
328000       IF ANM-IXDCCLEAR = 3                                               
328100         IF IXDCCLEAR-3-SW = JA                                           
328200           CONTINUE                                                       
328300         ELSE                                                             
328400           IF IXDCCLEAR-2-SW = JA                                         
328500             PERFORM IMS-GET-KREE01-GHU                                   
328600             MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                     
328700             MOVE 2                   TO ANM-IXDCCLEAR                    
328800             PERFORM IMS-REPL-KREE01                                      
328900           ELSE                                                           
329000             PERFORM IMS-GET-KREE01-GHU                                   
329100             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
329200             MOVE 1                   TO ANM-IXDCCLEAR                    
329300             PERFORM IMS-REPL-KREE01                                      
329400           END-IF                                                         
329500         END-IF                                                           
329600       ELSE                                                               
329700         IF ANM-IXDCCLEAR = 2                                             
329800           IF IXDCCLEAR-2-SW = JA                                         
329900             CONTINUE                                                     
330000           ELSE                                                           
330100             PERFORM IMS-GET-KREE01-GHU                                   
330200             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
330300             MOVE 1                   TO ANM-IXDCCLEAR                    
330400             PERFORM IMS-REPL-KREE01                                      
330500           END-IF                                                         
330600         END-IF                                                           
330700       END-IF                                                             
330800     ELSE                                                                 
330900       PERFORM IMS-GET-KREE01-GHU                                         
331000       MOVE SPACE            TO ANM-IDDC-RET                              
331100       MOVE 0                TO ANM-IXDCCLEAR                             
331200       PERFORM IMS-REPL-KREE01                                            
331300     END-IF                                                               
331400     .                                                                    
331500     EJECT                                                                
331600 DH-EV-UPPD-DC-RET-OEVRIGT SECTION.                                       
331700                                                                          
331800     MOVE NEJ TO RETURRADER-KVAR-SW                                       
331900                                                                          
332000     PERFORM IMS-GET-KREE01-KVAL                                          
332100     IF SEGMENT-FINNS                                                     
332200       PERFORM IMS-GET-KREE11-GNP                                         
332300       PERFORM UNTIL SEGMENT-SAKNAS OR RETURRADER-KVAR-SW = JA            
332400         IF LEV-KDKREBEH(1:1) = 'N'                                       
332500           CONTINUE                                                       
332600         ELSE                                                             
332700           MOVE LEV-KDANMORS   TO OKOD-KDANMORS                           
332800           CALL W418OKOD USING OKOD-W418OKOD                              
332900                                                                          
333000           IF OKOD-FL-RETILL = JA                                         
333100             MOVE JA TO RETURRADER-KVAR-SW                                
333200           END-IF                                                         
333300         END-IF                                                           
333400                                                                          
333500         PERFORM IMS-GET-KREE11-GNP                                       
333600       END-PERFORM                                                        
333700     END-IF                                                               
333800                                                                          
333900     IF RETURRADER-KVAR                                                   
334000       CONTINUE                                                           
334100     ELSE                                                                 
334200       PERFORM IMS-GET-KREE01-GHU                                         
334300       MOVE SPACE            TO ANM-IDDC-RET                              
334400       MOVE 0                TO ANM-IXDCCLEAR                             
334500       PERFORM IMS-REPL-KREE01                                            
334600     END-IF                                                               
334700     .                                                                    
334800     EJECT                                                                
334900 DI-UPPD-DC-RET-LDC     SECTION.                                          
335000                                                                          
335100     MOVE NEJ TO IXDCCLEAR-2-SW                                           
335200     MOVE NEJ TO IXDCCLEAR-3-SW                                           
335300                                                                          
335400     PERFORM IMS-GET-KREE01-KVAL                                          
335500     IF SEGMENT-FINNS                                                     
335600       PERFORM IMS-GET-KREE11-GNP                                         
335700       PERFORM UNTIL SEGMENT-SAKNAS                                       
335800         IF LEV-KDKREBEH(1:1) = 'N'                                       
335900           CONTINUE                                                       
336000         ELSE                                                             
336100           MOVE LEV-KDANMORS  TO OKOD-KDANMORS                            
336200           CALL W418OKOD USING OKOD-W418OKOD                              
336300                                                                          
336400           IF OKOD-FL-RETILL = JA                                         
336500             IF LEV-IDDC-RET = GMT-IDDC-RET72 (2)                         
336600               MOVE JA TO IXDCCLEAR-2-SW                                  
336700             END-IF                                                       
336800             IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                   
336900                DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                  
337000                DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR               
337100                DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR               
337110                DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC              
337200               IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                    
337300                  LEV-IDDC-RET = GMT-IDDC-RET                             
337400                 MOVE JA TO IXDCCLEAR-3-SW                                
337500               END-IF                                                     
337600             ELSE                                                         
337700               IF LEV-IDDC-RET = GMT-IDDC-RET72 (3) OR                    
337800                  LEV-IDDC-RET = WS-CDC-SE                                
337900                 MOVE JA TO IXDCCLEAR-3-SW                                
338000               END-IF                                                     
338100             END-IF                                                       
338200           END-IF                                                         
338300         END-IF                                                           
338400                                                                          
338500         PERFORM IMS-GET-KREE11-GNP                                       
338600       END-PERFORM                                                        
338700     END-IF                                                               
338800                                                                          
338900     IF ANM-IXDCCLEAR = 3                                                 
339000       IF IXDCCLEAR-3-SW = JA                                             
339100         CONTINUE                                                         
339200       ELSE                                                               
339300         IF IXDCCLEAR-2-SW = JA                                           
339400           PERFORM IMS-GET-KREE01-GHU                                     
339500           MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                       
339600           MOVE 2                   TO ANM-IXDCCLEAR                      
339700           PERFORM IMS-REPL-KREE01                                        
339800         ELSE                                                             
339900           PERFORM IMS-GET-KREE01-GHU                                     
340000           MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                       
340100           MOVE 1                   TO ANM-IXDCCLEAR                      
340200           PERFORM IMS-REPL-KREE01                                        
340300         END-IF                                                           
340400       END-IF                                                             
340500     ELSE                                                                 
340600       IF ANM-IXDCCLEAR = 2                                               
340700         IF IXDCCLEAR-3-SW = JA                                           
340800           PERFORM IMS-GET-KREE01-GHU                                     
340900           MOVE GMT-IDDC-RET72 (3)  TO ANM-IDDC-RET                       
341000           IF ANM-IDDC-RET = SPACE                                        
341100             IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                   
341200                DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                  
341300                DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR               
341400                DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR               
341410                DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC              
341500               MOVE GMT-IDDC-RET      TO ANM-IDDC-RET                     
341600             ELSE                                                         
341700               MOVE WS-CDC-SE         TO ANM-IDDC-RET                     
341800             END-IF                                                       
341900           END-IF                                                         
342000           MOVE 3                   TO ANM-IXDCCLEAR                      
342100           PERFORM IMS-REPL-KREE01                                        
342200         ELSE                                                             
342300           IF IXDCCLEAR-2-SW = JA                                         
342400             CONTINUE                                                     
342500           ELSE                                                           
342600             PERFORM IMS-GET-KREE01-GHU                                   
342700             MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                     
342800             MOVE 1                   TO ANM-IXDCCLEAR                    
342900             PERFORM IMS-REPL-KREE01                                      
343000           END-IF                                                         
343100         END-IF                                                           
343200       ELSE                                                               
343300         IF ANM-IXDCCLEAR = 1                                             
343400           IF IXDCCLEAR-3-SW = JA                                         
343500             PERFORM IMS-GET-KREE01-GHU                                   
343600             MOVE GMT-IDDC-RET72 (3)  TO ANM-IDDC-RET                     
343700             IF ANM-IDDC-RET = SPACE                                      
343800               IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                 
343900                  DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                
344000                  DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR             
344100                  DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR             
344110                  DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC            
344200                 MOVE GMT-IDDC-RET    TO ANM-IDDC-RET                     
344300               ELSE                                                       
344400                 MOVE WS-CDC-SE       TO ANM-IDDC-RET                     
344500               END-IF                                                     
344600             END-IF                                                       
344700             MOVE 3                   TO ANM-IXDCCLEAR                    
344800             PERFORM IMS-REPL-KREE01                                      
344900           ELSE                                                           
345000             IF IXDCCLEAR-2-SW = JA                                       
345100               PERFORM IMS-GET-KREE01-GHU                                 
345200               MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                   
345300               MOVE 2                   TO ANM-IXDCCLEAR                  
345400               PERFORM IMS-REPL-KREE01                                    
345500             ELSE                                                         
345600               CONTINUE                                                   
345700             END-IF                                                       
345800           END-IF                                                         
345900         ELSE                                                             
346000           IF ANM-IXDCCLEAR = 0 AND ANM-IDDC-RET = SPACE                  
346100             IF IXDCCLEAR-3-SW = JA                                       
346200               PERFORM IMS-GET-KREE01-GHU                                 
346300               MOVE GMT-IDDC-RET72 (3)  TO ANM-IDDC-RET                   
346400               IF ANM-IDDC-RET = SPACE                                    
346500                 IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR               
346600                    DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR              
346700                    DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR           
346800                    DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR           
346810                    DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC          
346900                   MOVE GMT-IDDC-RET    TO ANM-IDDC-RET                   
347000                 ELSE                                                     
347100                   MOVE WS-CDC-SE       TO ANM-IDDC-RET                   
347200                 END-IF                                                   
347300               END-IF                                                     
347400               MOVE 3                   TO ANM-IXDCCLEAR                  
347500               PERFORM IMS-REPL-KREE01                                    
347600             ELSE                                                         
347700               IF IXDCCLEAR-2-SW = JA                                     
347800                 PERFORM IMS-GET-KREE01-GHU                               
347900                 MOVE GMT-IDDC-RET72 (2)  TO ANM-IDDC-RET                 
348000                 MOVE 2                   TO ANM-IXDCCLEAR                
348100                 PERFORM IMS-REPL-KREE01                                  
348200               ELSE                                                       
348300                 PERFORM IMS-GET-KREE01-GHU                               
348400                 MOVE GMT-IDDC-RET72 (1)  TO ANM-IDDC-RET                 
348500                 MOVE 1                   TO ANM-IXDCCLEAR                
348600                 PERFORM IMS-REPL-KREE01                                  
348700               END-IF                                                     
348800             END-IF                                                       
348900           END-IF                                                         
349000         END-IF                                                           
349100       END-IF                                                             
349200     END-IF                                                               
349300     .                                                                    
349400     EJECT                                                                
349500 DJ-UPPD-DC-RET-EJ-LDC  SECTION.                                          
349600                                                                          
349700     PERFORM IMS-GET-KREE01-GHU                                           
349800     IF SEGMENT-FINNS                                                     
349900       IF ANM-IDDC-RET = SPACE                                            
350000         MOVE GMT-IDDC-RET   TO ANM-IDDC-RET                              
350100         MOVE 3              TO ANM-IXDCCLEAR                             
350200         PERFORM IMS-REPL-KREE01                                          
350300       END-IF                                                             
350400     END-IF                                                               
350500     .                                                                    
350600     EJECT                                                                
350700 E-KOLLA-AENDRA-STATUS          SECTION.                                  
350800                                                                          
350900     MOVE +1                    TO MEAN-IX                                
351000     PERFORM IMS-GET-KREE01-KVAL                                          
351100     PERFORM IMS-GET-KREE11-GNP                                           
351200     PERFORM UNTIL SEGMENT-SAKNAS                                         
351300       MOVE LEV-KDKREBEH        TO TEST-KDKREBEH                          
351400       IF KDKREBEH-1 = 'R' OR 'Q' OR 'P'                                  
351500         MOVE JA                TO OBEH-RADER-FINNS-SW                    
351600         IF KDKREBEH-2 = 'R'                                              
351700           MOVE JA              TO OBEH-REMISS-FINNS-SW                   
351800         END-IF                                                           
351900       ELSE                                                               
352000         MOVE JA                TO BEH-RADER-FINNS-SW                     
352100         IF KDKREBEH-1 = 'N'                                              
352200           PERFORM EA-FYLL-MEAN-AREA                                      
352300           MOVE JA              TO AVVISADE-RADER-SW                      
352400         ELSE                                                             
352500           IF LEV-KDKREBEH = 'ANN' OR 'DEL'                               
352600             MOVE JA            TO ANNULERADE-RADER-SW                    
352700           ELSE                                                           
352800             IF KDKREBEH-1 = 'Y' OR 'C' OR 'J'                            
352900               MOVE JA          TO GODKAENDA-RADER-SW                     
353000             END-IF                                                       
353100           END-IF                                                         
353200         END-IF                                                           
353300       END-IF                                                             
353400       PERFORM IMS-GET-KREE11-GNP                                         
353500     END-PERFORM                                                          
353600                                                                          
353700     PERFORM IMS-GET-KREE01-GHU                                           
353800     IF OBEH-RADER-FINNS                                                  
353900       IF BEH-RADER-FINNS OR OBEH-REMISS-FINNS                            
354000         MOVE '2'               TO ANM-KDLEVANM                           
354100         PERFORM IMS-REPL-KREE01                                          
354200         MOVE INF-UPDATE-OK   TO MED-IDMFSFEL                             
354300         CALL WMEDKONV USING MED-WMEDAREA                                 
354400         MOVE MED-MFSFEL      TO MOD-TEMFSINF                             
354500       END-IF                                                             
354600     ELSE                                                                 
354700       IF ANM-KDLEVANM = '4'                                              
354800         COMPUTE ANM-KVRADER-RT   =                                       
354900                 ANM-KVRADER-RT   - WS-KVRADER-ANN                        
355000         COMPUTE ANM-KVRADER-OBEH =                                       
355100                 ANM-KVRADER-OBEH - WS-KVRADER-ANN                        
355200         IF ANM-KVRADER-RT      = +0                                      
355300           MOVE '7'             TO ANM-KDLEVANM                           
355400         END-IF                                                           
355500         PERFORM IMS-REPL-KREE01                                          
355600         MOVE INF-UPDATE-OK TO MED-IDMFSFEL                               
355700         CALL WMEDKONV USING MED-WMEDAREA                                 
355800         MOVE MED-MFSFEL    TO MOD-TEMFSINF                               
355900       ELSE                                                               
356000         MOVE MSGI-IDFTG  TO WS-IDFTG                                     
356100         IF GODKAENDA-RADER-FINNS                                         
356200           IF IDFTG-US OR IDFTG-CA                                        
356300             CONTINUE                                                     
356400           ELSE                                                           
356500             IF ANM-IDUSER-ADM = SPACE                                    
356600               MOVE MSGI-IDUSER   TO ANM-IDUSER-ADM                       
356700               MOVE MSGI-BEANST   TO ANM-BEANST                           
356800             END-IF                                                       
356900           END-IF                                                         
357000           MOVE '3'             TO ANM-KDLEVANM                           
357100           PERFORM IMS-REPL-KREE01                                        
357200           MOVE INF-UPDATE-OK TO MED-IDMFSFEL                             
357300           CALL WMEDKONV USING MED-WMEDAREA                               
357400           MOVE MED-MFSFEL  TO MOD-TEMFSINF                               
357500           IF AVVISADE-RADER-FINNS                                        
357600             IF IDFTG-US                                                  
357700               CONTINUE                                                   
357800             ELSE                                                         
357900               PERFORM EB-SKRIV-MEAN-AREA                                 
358000             END-IF                                                       
358100           END-IF                                                         
358200         ELSE                                                             
358300           MOVE '7'             TO ANM-KDLEVANM                           
358400           PERFORM IMS-REPL-KREE01                                        
358500           MOVE INF-UPDATE-OK TO MED-IDMFSFEL                             
358600           CALL WMEDKONV USING MED-WMEDAREA                               
358700           MOVE MED-MFSFEL  TO MOD-TEMFSINF                               
358800           IF AVVISADE-RADER-FINNS                                        
358900             IF IDFTG-US                                                  
359000               CONTINUE                                                   
359100             ELSE                                                         
359200               PERFORM EB-SKRIV-MEAN-AREA                                 
359300             END-IF                                                       
359400           END-IF                                                         
359500         END-IF                                                           
359600       END-IF                                                             
359700     END-IF                                                               
359800     .                                                                    
359900     EJECT                                                                
360000 EA-FYLL-MEAN-AREA              SECTION.                                  
360100                                                                          
360200     IF MEAN-IX < +14                                                     
360300       MOVE MSGI-IDDISTR        TO MEAN-IDDISTR  (MEAN-IX)                
360400       MOVE MSGI-IDKUNDNR       TO MEAN-IDKUNDNR (MEAN-IX)                
360500       MOVE MSGI-IDRAPPNR       TO MEAN-IDRAPPNR (MEAN-IX)                
360600                                                                          
360700       MOVE LEV-IDARTNR         TO MEAN-IDARTNR  (MEAN-IX)                
360800                                   W-IDARTNR                              
360900       MOVE LEV-IDRADNR         TO MEAN-IDRADNR  (MEAN-IX)                
361000                                   W-IDRADNR                              
361100       MOVE LEV-KDKREBEH        TO MEAN-KDKREBEH (MEAN-IX)                
361200                                                                          
361300       PERFORM IMS-GET-KREE21-GNP                                         
361400                                                                          
361500       IF SEGMENT-FINNS                                                   
361600        MOVE TXT-TEANMNOT-ADM (1) TO                                      
361700             MEAN-TEANMNOT-ADM (MEAN-IX, 1)                               
361800        MOVE TXT-TEANMNOT-ADM (2) TO                                      
361900             MEAN-TEANMNOT-ADM (MEAN-IX, 2)                               
362000        MOVE TXT-TEANMNOT-ADM (3) TO                                      
362100             MEAN-TEANMNOT-ADM (MEAN-IX, 3)                               
362200                                                                          
362300        MOVE TXT-TEANMNOT-REM (1) TO                                      
362400             MEAN-TEANMNOT-REM (MEAN-IX, 1)                               
362500        MOVE TXT-TEANMNOT-REM (2) TO                                      
362600             MEAN-TEANMNOT-REM (MEAN-IX, 2)                               
362700        MOVE TXT-TEANMNOT-REM (3) TO                                      
362800             MEAN-TEANMNOT-REM (MEAN-IX, 3)                               
362900       ELSE                                                               
363000         MOVE SPACE             TO MEAN-TEANMNOT-ADM-GRP (MEAN-IX)        
363100                                   MEAN-TEANMNOT-REM-GRP (MEAN-IX)        
363200       END-IF                                                             
363300                                                                          
363400       ADD +1                   TO MEAN-IX                                
363500     END-IF                                                               
363600     .                                                                    
363700     EJECT                                                                
363800 EB-SKRIV-MEAN-AREA             SECTION.                                  
363900                                                                          
364000     MOVE +1                    TO ANSV-KDCALL                            
364100     MOVE W-IDDISTR             TO ANSV-IDDISTR                           
364200     MOVE MSGI-IDFTG            TO ANSV-IDFTG                             
364300     MOVE W-IDKUNDNR            TO ANSV-IDKUNDNR                          
364400*- 4712-BILDEN JOBBAR MED HELA REKL. EJ RADVIS, DÄRFÖR KAN EJ KOD         
364500*- TAS FRAM FÖR ATT SÖKA EFTER ANSVARIG.USA VILL EJ HA MAIL ENL.          
364600*- NIC NAUDE 991216, DE HAR EGEN, LOCAL RUTIN I VIPS.                     
364700     MOVE '00'                  TO ANSV-KDANMORS                          
364800     MOVE ZERO                  TO ANSV-KDORDKL                           
364900     MOVE ZERO                  TO ANSV-ADLAGOMR                          
365000                                                                          
365100     CALL W418ANSV USING ANSV-W418ANSV 4113-PCB 4115-PCB 4117-PCB         
365200                                                                          
365300     IF ANSV-OK                                                           
365400       MOVE ANSV-KDARBTYP       TO W-KDARBTYP                             
365500       MOVE ANSV-IDPERSON       TO W-IDPERSON                             
365600       PERFORM IMS-GET-WDP311                                             
365700       IF SEGMENT-FINNS                                                   
365800          MOVE PERS-IDMAIL      TO MEAN-IDMAIL                            
365900          CALL W418MEAN USING MEAN-W418MEAN MAIL-PCB                      
366000       END-IF                                                             
366100     ELSE                                                                 
366200       IF ANSV-KDSVAR = 'S'                                               
366300         MOVE HIGH-VALUE            TO ANSV-KDANMORS                      
366400         CALL W418ANSV USING ANSV-W418ANSV 4113-PCB                       
366500         IF ANSV-OK                                                       
366600           MOVE ANSV-KDARBTYP       TO W-KDARBTYP                         
366700           MOVE ANSV-IDPERSON       TO W-IDPERSON                         
366800           PERFORM IMS-GET-WDP311                                         
366900           IF SEGMENT-FINNS                                               
367000              MOVE PERS-IDMAIL      TO MEAN-IDMAIL                        
367100              CALL W418MEAN USING MEAN-W418MEAN MAIL-PCB                  
367200           END-IF                                                         
367300         END-IF                                                           
367400       END-IF                                                             
367500     END-IF                                                               
367600     .                                                                    
367700     EJECT                                                                
367800 F-SKAPA-LEVANMLISTA            SECTION.                                  
367900                                                                          
368000     MOVE JA                    TO INDATA-SW                              
368100*    PERFORM FA-KOLLA-PRINTER                                             
368200     IF INDATA-OK                                                         
368300       MOVE 'W4T793X '          TO 4793-MID-TRANSKOD                      
368400       MOVE '4712'              TO 4793-MID-IDTRANS                       
368500       MOVE '1'                 TO 4793-MID-KDMFSFOR                      
368600       MOVE MSGI-IDDISTR        TO 4793-MID-IDDISTR-IN                    
368700                                     4793-MID-IDDISTR-UT                  
368800       MOVE MSGI-IDKUNDNR       TO 4793-MID-IDKUNDNR-IN                   
368900                                     4793-MID-IDKUNDNR-UT                 
369000       MOVE MSGI-IDRAPPNR       TO 4793-MID-IDRAPPNR-IN                   
369100                                     4793-MID-IDRAPPNR-UT                 
369200       MOVE 4793-LAENGD         TO 4793-MID-LL                            
369300                                                                          
369400       PERFORM IMS-PURGE-ALTMSG-4793                                      
369500                                                                          
369600       MOVE INF-PRINT-STARTED   TO MED-IDMFSFEL                           
369700       CALL WMEDKONV USING MED-WMEDAREA                                   
369800       MOVE MED-MFSFEL          TO MOD-TEMFSINF                           
369900     END-IF                                                               
370000     .                                                                    
370100     EJECT                                                                
370200 FA-KOLLA-PRINTER               SECTION.                                  
370300     CONTINUE                                                             
370400**   MOVE SPACE                TO PRT-IDPRTLST                            
370500**   MOVE '4LA'                TO PRT-IDPRTLST(1:3)                       
370600**                                                                        
370700**   MOVE MID-IDPRT            TO PRT-IDPRTLST(4:3)                       
370800**   MOVE 1                    TO PRT-KDCALL                              
370900**   CALL W006PRT USING PRT-W006PRT                                       
371000**                                                                        
371100**   IF PRT-KDSVAR                     = 'F'                              
371200**       MOVE ERR-WRONG-PRINTER TO MED-IDMFSFEL                           
371300**       MOVE NEJ               TO INDATA-SW                              
371400**       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPRT-ATTR                        
371500**       PERFORM MFS-ROER-EJ-FAELT-IN                                     
371600**       PERFORM MFS-ROER-EJ-FAELT-UT                                     
371700**       CALL WMEDKONV USING MED-WMEDAREA                                 
371800**       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
371900**   END-IF                                                               
372000     .                                                                    
372100     EJECT                                                                
372200 G-GODK-ADM-KONTROLL  SECTION.                                            
372300                                                                          
372400     MOVE JA                    TO GODK-ADM-DC-SW                         
372500     MOVE JA                    TO INDATA-SW                              
372600                                                                          
372700     PERFORM IMS-GET-KREE01-KVAL                                          
372800     IF SEGMENT-FINNS                                                     
372900       PERFORM IMS-GET-KREE11-GNP                                         
373000       PERFORM UNTIL SEGMENT-SAKNAS OR EJ-GODK-ADM-DC                     
373100         IF LEV-IDDC NOT = W-IDDC-B6                                      
373200           MOVE LEV-IDDC  TO W-IDDC-B6                                    
373300           PERFORM IMS-GU-WDB601                                          
373400         END-IF                                                           
373500         IF DCS-NDC-PF    OR                                              
373600            DCS-NDC-OTHERS OR                                             
373700            DCS-NDC-SA                                                    
373800           CONTINUE                                                       
373900         ELSE                                                             
374000           MOVE 'DISC'          TO W-KDARBTYP-6327                        
374100           MOVE LEV-IDDC        TO W-IDDC-6327                            
374200           MOVE LOW-VALUE       TO W-IDUSER-6328-MIN                      
374300           MOVE ZERO            TO W-SUBEL-6328-MIN                       
374400           MOVE HIGH-VALUE      TO W-IDUSER-6328-MAX                      
374500           MOVE 9999999         TO W-SUBEL-6328-MAX                       
374600           MOVE MSGI-IDUSER     TO W-IDUSER-GODK-6328                     
374700                                                                          
374800           PERFORM IMS-GU-WDGX6327                                        
374900           IF SEGMENT-FINNS                                               
375000             PERFORM IMS-GNP-WDGX6328                                     
375100             IF SEGMENT-SAKNAS                                            
375200               MOVE NEJ                    TO INDATA-SW                   
375300               MOVE NEJ                    TO GODK-ADM-DC-SW              
375400               MOVE ERR-EJ-BEHORIG-GODK-LA TO MED-IDMFSFEL                
375500               CALL WMEDKONV USING MED-WMEDAREA                           
375600               MOVE MED-MFSFEL         TO MOD-TEMFSFEL                    
375700             END-IF                                                       
375800           ELSE                                                           
375900             MOVE NEJ                    TO INDATA-SW                     
376000             MOVE NEJ                    TO GODK-ADM-DC-SW                
376100             MOVE ERR-EJ-BEHORIG-GODK-LA TO MED-IDMFSFEL                  
376200             CALL WMEDKONV USING MED-WMEDAREA                             
376300             MOVE MED-MFSFEL         TO MOD-TEMFSFEL                      
376400           END-IF                                                         
376500         END-IF                                                           
376600         IF EJ-GODK-ADM-DC                                                
376700           CONTINUE                                                       
376800         ELSE                                                             
376900           PERFORM IMS-GET-KREE11-GNP                                     
377000         END-IF                                                           
377100       END-PERFORM                                                        
377200     END-IF                                                               
377300     .                                                                    
377400     EJECT                                                                
377500 H-RENSA-NYCKLAR                SECTION.                                  
377600                                                                          
377700     MOVE MFS-RENSA-FAELT       TO MOD-IDRAPPNR-UT                        
377800                                   RESP-IDRAPPNR                          
377900     IF MFS-IDTRANS              = '4711'                                 
378000        CONTINUE                                                          
378100     ELSE                                                                 
378200       MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-UT                         
378300                                   MOD-IDKUNDNR-UT                        
378400                                   RESP-IDKUNDNR                          
378500     END-IF                                                               
378600     MOVE +1                    TO INDX                                   
378700     PERFORM UNTIL INDX > MAX-IX                                          
378800       MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR  (INDX)                      
378900                                 MOD-IDRADNR  (INDX)                      
379000                                 MOD-IDORDNR  (INDX)                      
379100                                 MOD-IDKOLLI  (INDX)                      
379200                                 MOD-IDDC     (INDX)                      
379300                                 MOD-KVLEVANM (INDX)                      
379400                                 MOD-KDANMORS (INDX)                      
379500                                 MOD-KDEMBLEV (INDX)                      
379600                                 MOD-PRARTBTO (INDX)                      
379700                                 MOD-KDFAKTYP (INDX)                      
379800                                 MOD-STRECK   (INDX)                      
379900                                 MOD-IDFAKT   (INDX)                      
380000                                 MOD-TIFAKT   (INDX)                      
380100                                 MOD-FLDIRLEV (INDX)                      
380200                                 MOD-KDKREBEH (INDX)                      
380300       MOVE MFS-STAENG-FAELT TO MOD-KDKREBEH-ATTRIBUT (INDX)              
380400       ADD +1                   TO INDX                                   
380500     END-PERFORM                                                          
380600     MOVE NOT-FOUND             TO RESP-IDMFSINF                          
380700     STRING 'DISCREPANCY-ID' REQU-IDAPIDISCREF ' NOT FOUND'               
380800     DELIMITED BY SIZE INTO        RESP-TEMFSINF                          
380900     SKIP3                                                                
381000     .                                                                    
381100 I-SECURIT-KONTROLL             SECTION.                                  
381200                                                                          
381300     IF IDDISTR-WS NUMERIC                                                
381400        MOVE MSG-SIGNON-USERID  TO SEC-IDUSER                             
381500        MOVE '4712'             TO SEC-IDTRANS                            
381600        MOVE IDDISTR-WS         TO SEC-IDKEY                              
381700        CALL WSECURIT USING        SEC-IDUSER                             
381800                                   SEC-IDTRANS                            
381900                                   SEC-IDKEY                              
382000                                   SEC-KDSVAR                             
382100     END-IF                                                               
382200     .                                                                    
382300     EJECT                                                                
382400 S01-LAES-WLKREE                SECTION.                                  
382500                                                                          
382600     PERFORM IMS-GET-KREE01-KVAL                                          
382700     IF SEGMENT-FINNS                                                     
382800        MOVE ANM-DALEVANM(3:6)    TO MOD-TILEVANM                         
382900                                                                          
383000        IF  API-SW = 'Y'                                                  
383100           IF ANM-DALEVANM  NOT = ZERO                                    
383200              MOVE ANM-DALEVANM(3:6)                                      
383300                                 TO WS-DATE-NUM6                          
383400              MOVE ZERO          TO WS-DATE-NUM10                         
383500              PERFORM S60-CONVERT-DATE                                    
383600              MOVE WS-CHAR-DATE  TO RESP-TILEVANM                         
383700           ELSE                                                           
383800              MOVE SPACE         TO RESP-TILEVANM                         
383900           END-IF                                                         
384000        END-IF                                                            
384100        MOVE ANM-KDLEVANM         TO MOD-KDLEVANM                         
384200                                     KDLEVANM-WS                          
384300                                     RESP-KDLEVANM                        
384400                                     KDLEVANM-WS                          
384500                                                                          
384600        MOVE ANM-IDFTG            TO WS-IDFTG                             
384700        IF DIST79-DEALER-PRICE OR                                         
384800                                  IDFTG-US OR IDFTG-CA OR                 
384900                                  IDFTG-CN OR IDFTG-IN OR                 
385000                                  IDFTG-KR OR IDFTG-TR OR                 
385100                                  IDFTG-MY OR IDFTG-TH OR                 
385200                                  IDFTG-TW OR IDFTG-MX OR IDFTG-BR        
385210                                           OR IDFTG-ZA                    
385300          PERFORM S10-HAMTA-KDVALISO                                      
385400        ELSE                                                              
385500          IF DIST79-ECOM-PRICE                                            
385600            MOVE ANM-KDVALISO     TO RESP-KDVALISO                        
385700                                     RESP-KDVALISO                        
385800          ELSE                                                            
385900             MOVE 'SEK'           TO MOD-KDVALISO                         
386000                                       RESP-KDVALISO                      
386100          END-IF                                                          
386200        END-IF                                                            
386300                                                                          
386400        PERFORM S02-LAES-WLKREE11                                         
386500     ELSE                                                                 
386600        MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                         
386700        CALL WMEDKONV USING MED-WMEDAREA                                  
386800        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
386900        MOVE NOT-FOUND                 TO RESP-IDMFSINF                   
387000        MOVE 'CUSTOMER INFO MISSING'                                      
387100                                       TO RESP-TEMFSINF                   
387200        PERFORM H-RENSA-NYCKLAR                                           
387300     END-IF                                                               
387400     .                                                                    
387500     EJECT                                                                
387600 S02-LAES-WLKREE11              SECTION.                                  
387700                                                                          
387800     IF MFS-IDPFK = '7'                                                   
387900        MOVE ZERO                 TO W-IDARTNR                            
388000                                     W-IDRADNR                            
388100        IF IDARTNR-WS NUMERIC                                             
388200           IF IDARTNR-WS > 0                                              
388300             MOVE IDARTNR-WS      TO W-IDARTNR                            
388400             MOVE IDRADNR-WS      TO W-IDRADNR                            
388500           END-IF                                                         
388600        END-IF                                                            
388700        MOVE INF-FIRST-PAGE       TO MED-IDMFSFEL                         
388800        CALL WMEDKONV USING MED-WMEDAREA                                  
388900        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
389000     ELSE                                                                 
389100        IF MFS-IDPFK = '8'                                                
389200           MOVE MID-IDARTNR-NEXT  TO W-IDARTNR                            
389300           MOVE MID-IDRADNR-NEXT  TO W-IDRADNR                            
389400        ELSE                                                              
389500           MOVE MID-IDARTNR-ENTER TO W-IDARTNR                            
389600           MOVE MID-IDRADNR-ENTER TO W-IDRADNR                            
389700        END-IF                                                            
389800     END-IF                                                               
389900     MOVE W-IDARTNR               TO MOD-IDARTNR-ENTER                    
390000     MOVE W-IDRADNR               TO MOD-IDRADNR-ENTER                    
390100                                                                          
390200     IF API-SW  = 'Y'                                                     
390300       PERFORM IMS-GET-KREE11-KVAL                                        
390400       MOVE   ZERO              TO RESP-KVRADER                           
390500       PERFORM UNTIL SEGMENT-SAKNAS  OR END-OF-DATA                       
390600         ADD +1                 TO RESP-KVRADER                           
390700         MOVE LEV-IDARTNR       TO RESP-IDARTNR(RESP-KVRADER)             
390800                                   W-IDARTNR                              
390900                                                                          
391000         MOVE LEV-IDRADNR       TO RESP-IDRADNR(RESP-KVRADER)             
391100                                   W-IDRADNR                              
391200                                                                          
391300         MOVE LEV-IDORDNR7      TO RESP-IDORDNR(RESP-KVRADER)             
391400                                                                          
391500         MOVE LEV-IDKOLLI       TO RESP-IDKOLLI(RESP-KVRADER)             
391600         MOVE LEV-IDDC          TO RESP-IDDC   (RESP-KVRADER)             
391700                                                                          
391800         MOVE LEV-KVLEVANM-BEKR TO RESP-KVLEVANM(RESP-KVRADER)            
391900                                                                          
392000         MOVE LEV-KDANMORS      TO RESP-KDANMORS (RESP-KVRADER)           
392100                                   W-KDANMORS                             
392200                                                                          
392300         MOVE LEV-KDFAKTYP      TO RESP-KDFAKTYP (RESP-KVRADER)           
392400                                                                          
392500         MOVE LEV-KVRETINL      TO RESP-KVRETINL (RESP-KVRADER)           
392600         MOVE LEV-KVAVV-KVANT                                             
392700                                TO RESP-KVAVV-KVANT(RESP-KVRADER)         
392800         MOVE LEV-KVAVV-KVAL    TO RESP-KVAVV-KVAL(RESP-KVRADER)          
392900         MOVE LEV-IDFTG         TO WS-IDFTG                               
393000         IF IDFTG-PV                                                      
393100            MOVE LEV-IDFAKT     TO RESP-IDFAKT(RESP-KVRADER)              
393200            IF DIST79-DEALER-PRICE OR                                     
393400               DIST79-ECOM-PRICE                                          
393500                MOVE LEV-PRARTBTO-LOC                                     
393600                                TO RESP-PRARTBTO(RESP-KVRADER)            
393700            ELSE                                                          
393800                MOVE LEV-PRARTBTO                                         
393900                                TO RESP-PRARTBTO(RESP-KVRADER)            
394000                                                                          
394100            END-IF                                                        
394200            IF LEV-TIFAKT    NOT = ZERO                                   
394300              MOVE LEV-TIFAKT                                             
394400                                 TO WS-DATE-NUM6                          
394500              MOVE ZERO          TO WS-DATE-NUM10                         
394600              PERFORM S60-CONVERT-DATE                                    
394700              MOVE WS-CHAR-DATE  TO RESP-TIFAKT(RESP-KVRADER)             
394800            ELSE                                                          
394900              MOVE SPACE         TO RESP-TIFAKT(RESP-KVRADER)             
395000            END-IF                                                        
395100         ELSE                                                             
395200            MOVE LEV-IDFAKT-LOC  TO RESP-IDFAKT(RESP-KVRADER)             
395300            MOVE LEV-PRARTBTO-LOCINV                                      
395400                                 TO RESP-PRARTBTO(RESP-KVRADER)           
395500            IF LEV-TIFAKT-LOC NOT = ZERO                                  
395600              MOVE LEV-TIFAKT-LOC                                         
395700                                 TO WS-DATE-NUM6                          
395800              MOVE ZERO          TO WS-DATE-NUM10                         
395900              PERFORM S60-CONVERT-DATE                                    
396000              MOVE WS-CHAR-DATE  TO RESP-TIFAKT(RESP-KVRADER)             
396100            ELSE                                                          
396200              MOVE SPACE         TO RESP-TIFAKT(RESP-KVRADER)             
396300            END-IF                                                        
396400         END-IF                                                           
396500         MOVE LEV-KDKREBEH       TO RESP-KDKREBEH(RESP-KVRADER)           
396600                                                                          
396700         PERFORM IMS-GET-KREE21-GNP                                       
396800         IF SEGMENT-FINNS                                                 
396900            MOVE TXT-TEANMNOT-DLR (1)                                     
397000                                TO RESP-TEANMNOT-DLR (1)                  
397100            MOVE TXT-TEANMNOT-DLR (2)                                     
397200                                TO RESP-TEANMNOT-DLR (2)                  
397300            MOVE TXT-TEANMNOT-DLR (3)                                     
397400                                TO RESP-TEANMNOT-DLR (3)                  
397500                                                                          
397600         END-IF                                                           
397700         PERFORM IMS-GET-KREE11-GNP                                       
397800       END-PERFORM                                                        
397900     ELSE                                                                 
398000        PERFORM IMS-GET-KREE11-KVAL                                       
398100        MOVE +1                      TO INDX                              
398200        PERFORM UNTIL INDX > MAX-IX                                       
398300        IF SEGMENT-FINNS                                                  
398400           MOVE LEV-IDARTNR       TO MOD-IDARTNR  (INDX)                  
398500           MOVE LEV-IDRADNR       TO MOD-IDRADNR  (INDX)                  
398600           MOVE LEV-IDORDNR7      TO MOD-IDORDNR  (INDX)                  
398700           MOVE LEV-IDKOLLI       TO MOD-IDKOLLI  (INDX)                  
398800           MOVE LEV-IDDC          TO MOD-IDDC     (INDX)                  
398900           MOVE LEV-KVLEVANM-BEKR TO MOD-KVLEVANM (INDX)                  
399000           MOVE LEV-KDANMORS      TO MOD-KDANMORS (INDX)                  
399100                                     W-KDANMORS                           
399200           MOVE LEV-KDEMBLEV      TO MOD-KDEMBLEV (INDX)                  
399300           MOVE LEV-KDFAKTYP      TO MOD-KDFAKTYP (INDX)                  
399400           MOVE '-'               TO MOD-STRECK   (INDX)                  
399500           MOVE LEV-IDFTG         TO WS-IDFTG                             
399600           IF IDFTG-PV                                                    
399700             IF DIST79-DEALER-PRICE OR                                    
399900                DIST79-ECOM-PRICE                                         
400000               MOVE LEV-PRARTBTO-LOC  TO MOD-PRARTBTO (INDX)              
400100             ELSE                                                         
400200               MOVE LEV-PRARTBTO      TO MOD-PRARTBTO (INDX)              
400300                                                                          
400400             END-IF                                                       
400500             MOVE LEV-IDFAKT          TO MOD-IDFAKT   (INDX)              
400600             MOVE LEV-TIFAKT          TO MOD-TIFAKT   (INDX)              
400700           ELSE                                                           
400800              MOVE LEV-PRARTBTO-LOCINV TO MOD-PRARTBTO (INDX)             
400900              MOVE LEV-IDFAKT-LOC TO MOD-IDFAKT   (INDX)                  
401000              MOVE LEV-TIFAKT-LOC TO MOD-TIFAKT   (INDX)                  
401100           END-IF                                                         
401200           IF LEV-FLDIRLEV = JA                                           
401300             IF MSGI-IDLAND-SPR = 'GB'                                    
401400               MOVE YES           TO MOD-FLDIRLEV (INDX)                  
401500             ELSE                                                         
401600               MOVE LEV-FLDIRLEV  TO MOD-FLDIRLEV (INDX)                  
401700             END-IF                                                       
401800           ELSE                                                           
401900             MOVE LEV-FLDIRLEV    TO MOD-FLDIRLEV (INDX)                  
402000           END-IF                                                         
402100           IF LEV-FLTEXT = JA                                             
402200             IF MSGI-IDLAND-SPR = 'GB'                                    
402300               MOVE YES           TO MOD-FLTEXT   (INDX)                  
402400             ELSE                                                         
402500               MOVE LEV-FLTEXT    TO MOD-FLTEXT   (INDX)                  
402600             END-IF                                                       
402700           ELSE                                                           
402800             MOVE LEV-FLTEXT      TO MOD-FLTEXT   (INDX)                  
402900           END-IF                                                         
403000           IF LEV-FLANNULL = JA                                           
403100              MOVE 'ANN'        TO TEST-KDKREBEH                          
403200           ELSE                                                           
403300              MOVE LEV-KDKREBEH TO TEST-KDKREBEH                          
403400           END-IF                                                         
403500           IF KDKREBEH-1 = 'C'                                            
403600              IF KDKREBEH-2 = '1'                                         
403700                 MOVE ZERO        TO KDKREBEH-2                           
403800              END-IF                                                      
403900           END-IF                                                         
404000           IF SWEDISH-TEXT                                                
404100              IF KDKREBEH-1 = 'Y'                                         
404200                 MOVE 'J'         TO KDKREBEH-1                           
404300              ELSE                                                        
404400                 IF KDKREBEH-1 = 'C'                                      
404500                    MOVE 'Ä'      TO KDKREBEH-1                           
404600                 ELSE                                                     
404700                    IF TEST-KDKREBEH = 'DEL'                              
404800                       MOVE 'ANN' TO TEST-KDKREBEH                        
404900                    END-IF                                                
405000                 END-IF                                                   
405100              END-IF                                                      
405200           ELSE                                                           
405300              IF KDKREBEH-1 = 'J'                                         
405400                 MOVE 'Y'         TO KDKREBEH-1                           
405500              ELSE                                                        
405600                 IF TEST-KDKREBEH = 'ANN'                                 
405700                    MOVE 'DEL'    TO TEST-KDKREBEH                        
405800                 END-IF                                                   
405900              END-IF                                                      
406000           END-IF                                                         
406100                                                                          
406200           IF MFS-UPDATE OR MFS-UPD-V                                     
406300             IF MID-KDKREBEH (INDX) = ALL '+'                             
406400               MOVE TEST-KDKREBEH     TO MOD-KDKREBEH (INDX)              
406500             ELSE                                                         
406600               MOVE MFS-ROER-EJ-FAELT TO MOD-KDKREBEH (INDX)              
406700             END-IF                                                       
406800           ELSE                                                           
406900             MOVE TEST-KDKREBEH       TO MOD-KDKREBEH (INDX)              
407000                                                                          
407100             IF KDKREBEH-1 = 'Q'                                          
407200               MOVE MFS-ADD-LYS-UPP-FAELT TO                              
407300                                    MOD-IDARTNR-ATTRIBUT  (INDX)          
407400                                    MOD-KDANMORS-ATTRIBUT (INDX)          
407500                                                                          
407600               MOVE INF-MATRIS-KONFLIKT  TO MED-IDMFSINF                  
407700               CALL WMEDKONV USING MED-WMEDAREA                           
407800               MOVE MED-MFSINF TO MOD-TEMFSINF                            
407900             END-IF                                                       
408000             IF KDKREBEH-1 = 'P'                                          
408100               MOVE MFS-ADD-LYS-UPP-FAELT TO                              
408200                                    MOD-IDARTNR-ATTRIBUT  (INDX)          
408300                                    MOD-KDANMORS-ATTRIBUT (INDX)          
408400                                                                          
408500               MOVE INF-MATRIS-KONFLIKT-PSN  TO MED-IDMFSINF              
408600               MOVE 'Q  '             TO MOD-KDKREBEH (INDX)              
408700               CALL WMEDKONV USING MED-WMEDAREA                           
408800               MOVE MED-MFSINF TO MOD-TEMFSINF                            
408900             END-IF                                                       
409000           END-IF                                                         
409100                                                                          
409200           IF KDLEVANM-WS > '4'                                           
409300             MOVE MFS-STAENG-FAELT TO MOD-KDKREBEH-ATTRIBUT (INDX)        
409400           END-IF                                                         
409500           PERFORM IMS-GET-KREE11-GNP                                     
409600        ELSE                                                              
409700           MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR  (INDX)                  
409800                                     MOD-IDRADNR  (INDX)                  
409900                                     MOD-IDORDNR  (INDX)                  
410000                                     MOD-IDKOLLI  (INDX)                  
410100                                     MOD-IDDC     (INDX)                  
410200                                     MOD-KVLEVANM (INDX)                  
410300                                     MOD-KDANMORS (INDX)                  
410400                                     MOD-KDEMBLEV (INDX)                  
410500                                     MOD-PRARTBTO (INDX)                  
410600                                     MOD-KDFAKTYP (INDX)                  
410700                                     MOD-STRECK   (INDX)                  
410800                                     MOD-IDFAKT   (INDX)                  
410900                                     MOD-TIFAKT   (INDX)                  
411000                                     MOD-FLDIRLEV (INDX)                  
411100                                     MOD-KDKREBEH (INDX)                  
411200           MOVE MFS-STAENG-FAELT TO MOD-KDKREBEH-ATTRIBUT (INDX)          
411300        END-IF                                                            
411400        ADD +1                  TO INDX                                   
411500     END-PERFORM                                                          
411600     END-IF                                                               
411700* FOR API DLR-TXT IS PRESENT                                              
411800     IF SEGMENT-FINNS                                                     
411900        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSFEL                         
412000        CALL WMEDKONV USING MED-WMEDAREA                                  
412100        MOVE MED-MFSFEL         TO MOD-TEMFSINF                           
412200        MOVE LEV-IDARTNR        TO MOD-IDARTNR-NEXT                       
412300        MOVE LEV-IDRADNR        TO MOD-IDRADNR-NEXT                       
412400     END-IF                                                               
412500     .                                                                    
412600     EJECT                                                                
412700 S03-GET-RESPONSIBLE            SECTION.                                  
412800                                                                          
412900     MOVE +2                    TO ANSV-KDCALL                            
413000     MOVE W-IDDISTR             TO ANSV-IDDISTR                           
413100     MOVE LEV-IDDC              TO ANSV-IDDC                              
413200     MOVE MSGI-IDFTG            TO ANSV-IDFTG                             
413300     MOVE W-IDKUNDNR            TO ANSV-IDKUNDNR                          
413400     MOVE LEV-KDANMORS          TO ANSV-KDANMORS                          
413500                                                                          
414200     MOVE W-IDDISTR             TO W-IDDISTR-L5                           
414300     MOVE W-IDKUNDNR            TO W-IDKUNDNR-L5                          
414400     MOVE LEV-IDKUNDRF          TO W-IDKUNDRF-L5                          
414500     MOVE LEV-IDKOLLI           TO W-IDKOLLI-L5                           
414600     MOVE LEV-IDARTNR           TO W-IDARTNR-L5                           
414700                                                                          
414800     PERFORM IMS-GU-WDL501                                                
414900     IF SEGMENT-FINNS                                                     
415000                                                                          
415100       PERFORM IMS-GNP-WDL511                                             
415200       IF SEGMENT-FINNS                                                   
415300                                                                          
415600         MOVE FAKC-KDORDKL      TO ANSV-KDORDKL                           
415900         MOVE FAKC-IDPRODNR     TO W-IDPRODNR-L5                          
416000                                                                          
416100         PERFORM IMS-GNP-WDL521                                           
416200         IF SEGMENT-FINNS                                                 
416300           MOVE FAKL-ADLAGOMR   TO ANSV-ADLAGOMR                          
416400         ELSE                                                             
416500           MOVE ZERO            TO ANSV-ADLAGOMR                          
416600         END-IF                                                           
416700       ELSE                                                               
416800         MOVE ZERO              TO ANSV-ADLAGOMR                          
416900         MOVE LEV-KDORDKL       TO ANSV-KDORDKL                           
417000       END-IF                                                             
417100     ELSE                                                                 
417200       MOVE ZERO                TO ANSV-ADLAGOMR                          
417300       MOVE LEV-KDORDKL         TO ANSV-KDORDKL                           
417400     END-IF                                                               
417500                                                                          
417600                                                                          
417700     CALL W418ANSV USING ANSV-W418ANSV 4113-PCB 4115-PCB 4117-PCB         
417800                                                                          
417900     IF ANSV-OK                                                           
418000       MOVE ANSV-KDARBTYP       TO W-KDARBTYP                             
418100                                   MERE-KDARBTYP                          
418200       MOVE ANSV-IDPERSON       TO W-IDPERSON                             
418300                                   MERE-IDPERSON                          
418400       PERFORM IMS-GET-WDP311                                             
418500       IF SEGMENT-FINNS                                                   
418600          MOVE PERS-IDMAIL      TO MERE-IDMAIL                            
418700       END-IF                                                             
418800     END-IF                                                               
418900                                                                          
419000     .                                                                    
419100     EJECT                                                                
419200                                                                          
419300 S04-RENSA-RAD18                SECTION.                                  
419400                                                                          
419500     MOVE MFS-RENSA-FAELT       TO MOD-RAD18-IDARTNR                      
419600                                     MOD-RAD18-IDRADNR                    
419700                                     MOD-RAD18-KVLEVANM                   
419800                                     MOD-RAD18-KDANMORS                   
419900                                     MOD-RAD18-PRARTBTO                   
420000                                     MOD-RAD18-FLDIRLEV                   
420100     IF KDLEVANM-WS < 4                                                   
420200        MOVE MFS-FORMATETS-ATTR TO MOD-RAD18-IDARTNR-ATTRIBUT             
420300                                     MOD-RAD18-IDRADNR-ATTRIBUT           
420400                                     MOD-RAD18-KVLEVANM-ATTRIBUT          
420500                                     MOD-RAD18-KDANMORS-ATTRIBUT          
420600                                     MOD-RAD18-PRARTBTO-ATTRIBUT          
420700                                     MOD-RAD18-FLDIRLEV-ATTRIBUT          
420800     ELSE                                                                 
420900        MOVE MFS-STAENG-FAELT   TO MOD-RAD18-IDARTNR-ATTRIBUT             
421000                                     MOD-RAD18-IDRADNR-ATTRIBUT           
421100                                     MOD-RAD18-KVLEVANM-ATTRIBUT          
421200                                     MOD-RAD18-KDANMORS-ATTRIBUT          
421300                                     MOD-RAD18-PRARTBTO-ATTRIBUT          
421400                                     MOD-RAD18-FLDIRLEV-ATTRIBUT          
421500     END-IF                                                               
421600     .                                                                    
421700     EJECT                                                                
421800 S05-UPPD-ANALYSNRREGISTRET SECTION.                                      
421900                                                                          
422000     IF OKOD-FL-ANALYSNR = JA                                             
422100       PERFORM IMS-GU-410901-ROT                                          
422200                                                                          
422300       PERFORM IMS-GHNP-410911-KVAL                                       
422400                                                                          
422500       MOVE NEJ              TO WL410901-SW                               
422600       PERFORM UNTIL SEGMENT-SAKNAS OR WL410901-SW = JA                   
422700          IF WS-DATUM-Y2K >= 4110-DAGILTIG-FOM AND                        
422800                          <= 4110-DAGILTIG-TOM                            
422900             ADD LEV-KVLEVANM-BEKR TO 4110-KVART                          
423000             PERFORM IMS-REPL-4110                                        
423100             MOVE JA               TO WL410901-SW                         
423200          ELSE                                                            
423300             PERFORM IMS-GHNP-410911-KVAL                                 
423400          END-IF                                                          
423500       END-PERFORM                                                        
423600     END-IF                                                               
423700     .                                                                    
423800     EJECT                                                                
423900 S06-LAS-ANALYSNRREGISTRET SECTION.                                       
424000     MOVE '*** S06-LAS-ANALYSNRREGISTRET *** '                            
424100                              TO FELTEXT-STR                              
424200                                                                          
424300     PERFORM IMS-GU-410901-ROT                                            
424400                                                                          
424500     PERFORM IMS-GNP-410911-KVAL                                          
424600     IF SEGMENT-SAKNAS                                                    
424700       MOVE NEJ              TO WL410901-SW                               
424800     ELSE                                                                 
424900       MOVE NEJ              TO WL410901-SW                               
425000       PERFORM UNTIL SEGMENT-SAKNAS OR WL410901-SW = JA                   
425100                                                                          
425200         IF WS-DATUM-Y2K >= 4110-DAGILTIG-FOM AND                         
425300                         <= 4110-DAGILTIG-TOM                             
425400            MOVE JA         TO WL410901-SW                                
425500            MOVE 4110-IDANALYS  TO WS-IDANALYS-UPPD                       
425600            MOVE 4110-IDKONTO   TO WS-IDKONTO-UPPD                        
425700            MOVE 4110-IDKST     TO WS-IDKST-UPPD                          
425800         ELSE                                                             
425900            PERFORM IMS-GNP-410911-KVAL                                   
426000         END-IF                                                           
426100       END-PERFORM                                                        
426200     END-IF                                                               
426300     .                                                                    
426400     EJECT                                                                
426500 S07-EV-UPPDATERA-KN-WDGX4103  SECTION.                                   
426600                                                                          
426700     MOVE ZERO TO WS-RADPRIS                                              
426800                                                                          
426900*** KOLLA IFALL KODEN GER KNOTA SOM SKALL ATTESTERAS.                     
427000     IF LEV-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR           
427100                       '84' OR '27' OR '28' OR  '74'                      
427200       CONTINUE                                                           
427300     ELSE                                                                 
427400       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
427500          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
427600          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
427700          LEV-KDANMORS = '97'                                             
427800                                                                          
427900         MOVE LEV-IDDC            TO W-IDDC-4104                          
428000                                                                          
428100         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
428200              LEV-KDANMORS = '97'                                         
428300           MOVE 'RP'  TO W-KDKRENOT-4104                                  
428400         ELSE                                                             
428500           MOVE 'CN'  TO W-KDKRENOT-4104                                  
428600         END-IF                                                           
428700                                                                          
428800         IF WDR501-FINNS                                                  
428900           PERFORM IMS-GHNP-WDGX4104-KVAL                                 
429000           IF SEGMENT-FINNS                                               
429100             IF DIST79-DEALER-PRICE OR                                    
429300                DIST79-ECOM-PRICE                                         
429400               COMPUTE WS-RADPRIS ROUNDED =                               
429500                       LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC               
429600             ELSE                                                         
429700*CHINA-PRICE1                                                             
429800*INDIA-PRICE1                                                             
429900*KOREA-PRICE1                                                             
430000               MOVE LEV-IDFTG          TO WS-IDFTG                        
430100               EVALUATE TRUE                                              
430200               WHEN IDFTG-CN                                              
430300                   MOVE 'CNY'          TO 4104-KDVALISO                   
430400                   COMPUTE WS-RADPRIS ROUNDED =                           
430500                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
430600               WHEN IDFTG-IN                                              
430700                   MOVE 'INR'          TO 4104-KDVALISO                   
430800                   COMPUTE WS-RADPRIS ROUNDED =                           
430900                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
431000               WHEN IDFTG-KR                                              
431100                   MOVE 'KRW'          TO 4104-KDVALISO                   
431200                   COMPUTE WS-RADPRIS ROUNDED =                           
431300                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
431400               WHEN IDFTG-TR                                              
431500                   MOVE 'TRY'          TO 4104-KDVALISO                   
431600                   COMPUTE WS-RADPRIS ROUNDED =                           
431700                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
431800               WHEN IDFTG-MX                                              
431900                   MOVE 'MXN'          TO 4104-KDVALISO                   
432000                   COMPUTE WS-RADPRIS ROUNDED =                           
432100                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
432200               WHEN IDFTG-BR                                              
432300                   MOVE 'BRL'          TO 4104-KDVALISO                   
432400                   COMPUTE WS-RADPRIS ROUNDED =                           
432500                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
432600               WHEN IDFTG-MY                                              
432700                   MOVE 'MYR'          TO 4104-KDVALISO                   
432800                   COMPUTE WS-RADPRIS ROUNDED =                           
432900                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
433000               WHEN IDFTG-TH                                              
433100                   MOVE 'THB'          TO 4104-KDVALISO                   
433200                   COMPUTE WS-RADPRIS ROUNDED =                           
433300                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
433400               WHEN IDFTG-TW                                              
433500                   MOVE 'TWD'          TO 4104-KDVALISO                   
433600                   COMPUTE WS-RADPRIS ROUNDED =                           
433700                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
433710               WHEN IDFTG-ZA                                              
433720                   MOVE 'ZAR'          TO 4104-KDVALISO                   
433730                   COMPUTE WS-RADPRIS ROUNDED =                           
433740                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
433800               WHEN OTHER                                                 
433900                   COMPUTE WS-RADPRIS ROUNDED =                           
434000                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO                 
434100               END-EVALUATE                                               
434200             END-IF                                                       
434300                                                                          
434400             SUBTRACT WS-RADPRIS FROM 4104-SUKRENOT                       
434500                                                                          
434600             PERFORM IMS-REPL-WDGX4104                                    
434700           END-IF                                                         
434800         END-IF                                                           
434900       END-IF                                                             
435000     END-IF                                                               
435100     .                                                                    
435200     EJECT                                                                
435300 S08-EV-UPPDAT-KOD-WDGX4103  SECTION.                                     
435400                                                                          
435500     MOVE ZERO TO WS-RADPRIS                                              
435600     MOVE ZERO TO WS-NYTT-RADPRIS                                         
435700                                                                          
435800     IF SPAR-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR          
435900                        '84' OR '27' OR '28' OR  '74'                     
436000       PERFORM S08A-EV-SKAPA-NYTT-WDGX4104                                
436100     ELSE                                                                 
436200       MOVE SPAR-KDANMORS TO OKOD-KDANMORS                                
436300       CALL W418OKOD USING OKOD-W418OKOD                                  
436400       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
436500          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
436600          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
436700          SPAR-KDANMORS = '97'                                            
436800                                                                          
436900         MOVE LEV-IDDC     TO W-IDDC-4104                                 
437000                                                                          
437100         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
437200             SPAR-KDANMORS = '97'                                         
437300           MOVE 'RP'       TO W-KDKRENOT-4104                             
437400                              SPAR-KDKRENOT-4104                          
437500         ELSE                                                             
437600           MOVE 'CN'       TO W-KDKRENOT-4104                             
437700                              SPAR-KDKRENOT-4104                          
437800         END-IF                                                           
437900                                                                          
438000         IF WDR501-FINNS                                                  
438100           PERFORM IMS-GNP-WDGX4104-KVAL                                  
438200           IF SEGMENT-FINNS                                               
438300             MOVE LEV-KDANMORS TO OKOD-KDANMORS                           
438400             CALL W418OKOD USING OKOD-W418OKOD                            
438500             IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                          
438600                (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                      
438700                (OKOD-FL-KRENOT-EFTER-RT = JA) OR                         
438800                LEV-KDANMORS = '97'                                       
438900                                                                          
439000               MOVE LEV-IDDC     TO W-IDDC-4104                           
439100                                                                          
439200               IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                      
439300                   LEV-KDANMORS = '97'                                    
439400                 MOVE 'RP'       TO W-KDKRENOT-4104                       
439500               ELSE                                                       
439600                 MOVE 'CN'       TO W-KDKRENOT-4104                       
439700               END-IF                                                     
439800                                                                          
439900               IF SPAR-KDKRENOT-4104 = W-KDKRENOT-4104                    
440000                 PERFORM S08B-BEHANDLA-WDGX4104                           
440100               ELSE                                                       
440200                 PERFORM S08C-RAKNA-AV-WDGX4104-OLD                       
440300                 PERFORM S08A-EV-SKAPA-NYTT-WDGX4104                      
440400               END-IF                                                     
440500             ELSE                                                         
440600               PERFORM S08C-RAKNA-AV-WDGX4104-OLD                         
440700             END-IF                                                       
440800           ELSE                                                           
440900             MOVE LEV-KDANMORS TO OKOD-KDANMORS                           
441000             CALL W418OKOD USING OKOD-W418OKOD                            
441100             PERFORM S08A-EV-SKAPA-NYTT-WDGX4104                          
441200           END-IF                                                         
441300         ELSE                                                             
441400           MOVE LEV-KDANMORS TO OKOD-KDANMORS                             
441500           CALL W418OKOD USING OKOD-W418OKOD                              
441600           PERFORM S08A-EV-SKAPA-NYTT-WDGX4104                            
441700         END-IF                                                           
441800       ELSE                                                               
441900         MOVE LEV-KDANMORS TO OKOD-KDANMORS                               
442000         CALL W418OKOD USING OKOD-W418OKOD                                
442100         PERFORM S08A-EV-SKAPA-NYTT-WDGX4104                              
442200       END-IF                                                             
442300     END-IF                                                               
442400     .                                                                    
442500     EJECT                                                                
442600 S08A-EV-SKAPA-NYTT-WDGX4104  SECTION.                                    
442700                                                                          
442800     MOVE ZERO TO WS-RADPRIS                                              
442900     MOVE ZERO TO WS-NYTT-RADPRIS                                         
443000                                                                          
443100*- URSPRUNGLIGA KODEN GAV EJ KREDIT OCH FINNS EJ HELLER PÅ WDR5.          
443200*- NYA KODEN:                                                             
443300     IF LEV-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR           
443400                       '84' OR '27' OR '28' OR  '74'                      
443500       CONTINUE                                                           
443600     ELSE                                                                 
443700       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
443800          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
443900          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
444000          LEV-KDANMORS = '97'                                             
444100                                                                          
444200         MOVE LEV-IDDC            TO W-IDDC-4104                          
444300                                                                          
444400         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
444500              LEV-KDANMORS = '97'                                         
444600           MOVE 'RP'  TO W-KDKRENOT-4104                                  
444700         ELSE                                                             
444800           MOVE 'CN'  TO W-KDKRENOT-4104                                  
444900         END-IF                                                           
445000                                                                          
445100         IF WDR501-FINNS                                                  
445200           PERFORM IMS-GHNP-WDGX4104-KVAL                                 
445300           IF SEGMENT-FINNS                                               
445400             PERFORM S11-REPL-WDGX4104                                    
445500           ELSE                                                           
445600             PERFORM S12-INSERT-WDGX4104                                  
445700           END-IF                                                         
445800         ELSE                                                             
445900           MOVE '4103'           TO 4103-IDHTYP                           
446000           MOVE W-IDDISTR-4103   TO 4103-IDDISTR                          
446100           MOVE W-IDKUNDNR-4103  TO 4103-IDKUNDNR                         
446200           MOVE W-IDRAPPNR-4103  TO 4103-IDRAPPNR                         
446300           MOVE LOW-VALUE        TO 4103-LOW-VALUE                        
446400                                                                          
446500           PERFORM IMS-ISRT-WDGX4103                                      
446600           MOVE JA    TO WDR501-SW                                        
446700           PERFORM S12-INSERT-WDGX4104                                    
446800         END-IF                                                           
446900       END-IF                                                             
447000     END-IF                                                               
447100     .                                                                    
447200     EJECT                                                                
447300 S08B-BEHANDLA-WDGX4104 SECTION.                                          
447400                                                                          
447500     MOVE ZERO TO WS-RADPRIS                                              
447600     MOVE ZERO TO WS-NYTT-RADPRIS                                         
447700                                                                          
447800     IF PRIS-AENDRAT OR ANTAL-AENDRAT                                     
447900       IF PRIS-AENDRAT                                                    
448000         MOVE LEV-IDFTG  TO WS-IDFTG                                      
448100         IF ANTAL-AENDRAT                                                 
448200           IF IDFTG-PV                                                    
448300             COMPUTE WS-NYTT-RADPRIS ROUNDED =                            
448400                     LEV-KVLEVANM-BEKR * WS-IDEDITDATA                    
448500           ELSE                                                           
448600             IF DIST79-DEALER-PRICE OR                                    
448800                DIST79-ECOM-PRICE                                         
448900               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
449000                     LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOC                
449100             ELSE                                                         
449200*CHINA-PRICE2                                                             
449300*INDIA-PRICE2                                                             
449400*KOREA-PRICE2                                                             
449500               EVALUATE TRUE                                              
449600               WHEN IDFTG-CN                                              
449700                   MOVE 'CNY'          TO 4104-KDVALISO                   
449800                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
449900                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
450000               WHEN IDFTG-IN                                              
450100                   MOVE 'INR'          TO 4104-KDVALISO                   
450200                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
450300                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
450400               WHEN IDFTG-KR                                              
450500                   MOVE 'KRW'          TO 4104-KDVALISO                   
450600                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
450700                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
450800               WHEN IDFTG-TR                                              
450900                   MOVE 'TRY'          TO 4104-KDVALISO                   
451000                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
451100                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
451200               WHEN IDFTG-MX                                              
451300                   MOVE 'MXN'          TO 4104-KDVALISO                   
451400                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
451500                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
451600               WHEN IDFTG-BR                                              
451700                   MOVE 'BRL'          TO 4104-KDVALISO                   
451800                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
451900                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
452000               WHEN IDFTG-MY                                              
452100                   MOVE 'MYR'          TO 4104-KDVALISO                   
452200                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
452300                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
452400               WHEN IDFTG-TH                                              
452500                   MOVE 'THB'          TO 4104-KDVALISO                   
452600                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
452700                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
452800               WHEN IDFTG-TW                                              
452900                   MOVE 'TWD'          TO 4104-KDVALISO                   
453000                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
453100                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
453110               WHEN IDFTG-ZA                                              
453120                   MOVE 'ZAR'          TO 4104-KDVALISO                   
453130                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
453140                         LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV         
453200               WHEN OTHER                                                 
453300                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
453400                          LEV-KVLEVANM-BEKR * SPAR-PRARTBTO               
453500               END-EVALUATE                                               
453600             END-IF                                                       
453700           END-IF                                                         
453800         ELSE                                                             
453900           IF IDFTG-PV                                                    
454000             COMPUTE WS-NYTT-RADPRIS ROUNDED =                            
454100                     LEV-KVLEVANM-BEKR * WS-IDEDITDATA                    
454200           END-IF                                                         
454300         END-IF                                                           
454400       ELSE                                                               
454500*-ANTALET ÄNDRAT.                                                         
454600         IF DIST79-DEALER-PRICE OR                                        
454800            DIST79-ECOM-PRICE                                             
454900           COMPUTE WS-NYTT-RADPRIS ROUNDED =                              
455000                  LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC                    
455100         ELSE                                                             
455200*                                                                         
455300*CHINA-PRICE3                                                             
455400*INDIA-PRICE3                                                             
455500*KOREA-PRICE3                                                             
455600           MOVE LEV-IDFTG              TO WS-IDFTG                        
455700           EVALUATE TRUE                                                  
455800           WHEN IDFTG-CN                                                  
455900               MOVE 'CNY'          TO 4104-KDVALISO                       
456000               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
456100                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
456200           WHEN IDFTG-IN                                                  
456300               MOVE 'INR'          TO 4104-KDVALISO                       
456400               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
456500                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
456600           WHEN IDFTG-KR                                                  
456700               MOVE 'KRW'          TO 4104-KDVALISO                       
456800               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
456900                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
457000           WHEN IDFTG-TR                                                  
457100               MOVE 'TRY'          TO 4104-KDVALISO                       
457200               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
457300                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
457400           WHEN IDFTG-MX                                                  
457500               MOVE 'MXN'          TO 4104-KDVALISO                       
457600               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
457700                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
457800           WHEN IDFTG-BR                                                  
457900               MOVE 'BRL'          TO 4104-KDVALISO                       
458000               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
458100                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
458200           WHEN IDFTG-MY                                                  
458300               MOVE 'MYR'          TO 4104-KDVALISO                       
458400               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
458500                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
458600           WHEN IDFTG-TH                                                  
458700               MOVE 'THB'          TO 4104-KDVALISO                       
458800               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
458900                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
459000           WHEN IDFTG-TW                                                  
459100               MOVE 'TWD'          TO 4104-KDVALISO                       
459200               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
459300                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
459310           WHEN IDFTG-ZA                                                  
459320               MOVE 'ZAR'          TO 4104-KDVALISO                       
459330               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
459340                     LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV              
459400           WHEN OTHER                                                     
459500               COMPUTE WS-NYTT-RADPRIS ROUNDED =                          
459600                      LEV-KVLEVANM-BEKR * LEV-PRARTBTO                    
459700           END-EVALUATE                                                   
459800         END-IF                                                           
459900       END-IF                                                             
460000                                                                          
460100*- TAG BORT DET GAMLA PRISET OCH BYT TILL NYA.WDGX4104 = KN-SUMMA.        
460200       IF DIST79-DEALER-PRICE OR                                          
460400          DIST79-ECOM-PRICE                                               
460500         COMPUTE WS-RADPRIS ROUNDED =                                     
460600                 SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOC                   
460700       ELSE                                                               
460800*                                                                         
460900*CHINA-PRICE4                                                             
461000*INDIA-PRICE4                                                             
461100*KOREA-PRICE4                                                             
461200         MOVE LEV-IDFTG                TO WS-IDFTG                        
461300         EVALUATE TRUE                                                    
461400         WHEN IDFTG-CN                                                    
461500             MOVE 'CNY'          TO 4104-KDVALISO                         
461600             COMPUTE WS-RADPRIS ROUNDED =                                 
461700                   SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV              
461800         WHEN IDFTG-IN                                                    
461900             MOVE 'INR'          TO 4104-KDVALISO                         
462000             COMPUTE WS-RADPRIS ROUNDED =                                 
462100                   SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV              
462200         WHEN IDFTG-KR                                                    
462300             MOVE 'KRW'          TO 4104-KDVALISO                         
462400             COMPUTE WS-RADPRIS ROUNDED =                                 
462500                   LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV              
462600         WHEN IDFTG-TR                                                    
462700             MOVE 'TRY'          TO 4104-KDVALISO                         
462800             COMPUTE WS-RADPRIS ROUNDED =                                 
462900                   LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV              
463000         WHEN IDFTG-MX                                                    
463100             MOVE 'MXN'          TO 4104-KDVALISO                         
463200             COMPUTE WS-RADPRIS ROUNDED =                                 
463300                   LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV              
463400         WHEN IDFTG-BR                                                    
463500             MOVE 'BRL'          TO 4104-KDVALISO                         
463600             COMPUTE WS-RADPRIS ROUNDED =                                 
463700                   LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV              
463800         WHEN IDFTG-MY                                                    
463900             MOVE 'MYR'          TO 4104-KDVALISO                         
464000             COMPUTE WS-RADPRIS ROUNDED =                                 
464100                   LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV              
464200         WHEN IDFTG-TH                                                    
464300             MOVE 'THB'          TO 4104-KDVALISO                         
464400             COMPUTE WS-RADPRIS ROUNDED =                                 
464500                   LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV              
464600         WHEN IDFTG-TW                                                    
464700             MOVE 'TWD'          TO 4104-KDVALISO                         
464800             COMPUTE WS-RADPRIS ROUNDED =                                 
464900                   LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV              
464910         WHEN IDFTG-ZA                                                    
464920             MOVE 'ZAR'          TO 4104-KDVALISO                         
464930             COMPUTE WS-RADPRIS ROUNDED =                                 
464940                   LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV              
465000         WHEN OTHER                                                       
465100             COMPUTE WS-RADPRIS ROUNDED =                                 
465200                    SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO                    
465300         END-EVALUATE                                                     
465400       END-IF                                                             
465500                                                                          
465600       IF WDR501-FINNS                                                    
465700         PERFORM IMS-GHNP-WDGX4104-KVAL                                   
465800         IF SEGMENT-FINNS                                                 
465900           SUBTRACT WS-RADPRIS FROM 4104-SUKRENOT                         
466000           ADD WS-NYTT-RADPRIS   TO 4104-SUKRENOT                         
466100                                                                          
466200           PERFORM IMS-REPL-WDGX4104                                      
466300         END-IF                                                           
466400       END-IF                                                             
466500     END-IF                                                               
466600     .                                                                    
466700     EJECT                                                                
466800 S08C-RAKNA-AV-WDGX4104-OLD  SECTION.                                     
466900                                                                          
467000     MOVE ZERO TO WS-RADPRIS                                              
467100                                                                          
467200     MOVE LEV-IDDC           TO W-IDDC-4104                               
467300     MOVE SPAR-KDKRENOT-4104 TO W-KDKRENOT-4104                           
467400                                                                          
467500     IF WDR501-FINNS                                                      
467600       PERFORM IMS-GHNP-WDGX4104-KVAL                                     
467700       IF SEGMENT-FINNS                                                   
467800         IF DIST79-DEALER-PRICE OR                                        
468000            DIST79-ECOM-PRICE                                             
468100           COMPUTE WS-RADPRIS ROUNDED =                                   
468200                   SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOC                 
468300         ELSE                                                             
468400*                                                                         
468500*CHINA-PRICE5                                                             
468600*INDIA-PRICE5                                                             
468700*KOREA-PRICE5                                                             
468800           MOVE LEV-IDFTG              TO WS-IDFTG                        
468900           EVALUATE TRUE                                                  
469000           WHEN IDFTG-CN                                                  
469100               MOVE 'CNY'          TO 4104-KDVALISO                       
469200               COMPUTE WS-RADPRIS ROUNDED =                               
469300                     SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV            
469400           WHEN IDFTG-IN                                                  
469500               MOVE 'INR'          TO 4104-KDVALISO                       
469600               COMPUTE WS-RADPRIS ROUNDED =                               
469700                     SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV            
469800           WHEN IDFTG-KR                                                  
469900               MOVE 'KRW'          TO 4104-KDVALISO                       
470000               COMPUTE WS-RADPRIS ROUNDED =                               
470100                     LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV            
470200           WHEN IDFTG-TR                                                  
470300               MOVE 'TRY'          TO 4104-KDVALISO                       
470400               COMPUTE WS-RADPRIS ROUNDED =                               
470500                     LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV            
470600           WHEN IDFTG-MX                                                  
470700               MOVE 'MXN'          TO 4104-KDVALISO                       
470800               COMPUTE WS-RADPRIS ROUNDED =                               
470900                     LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV            
471000           WHEN IDFTG-BR                                                  
471100               MOVE 'BRL'          TO 4104-KDVALISO                       
471200               COMPUTE WS-RADPRIS ROUNDED =                               
471300                     LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV            
471400           WHEN IDFTG-MY                                                  
471500               MOVE 'MYR'          TO 4104-KDVALISO                       
471600               COMPUTE WS-RADPRIS ROUNDED =                               
471700                     LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV            
471800           WHEN IDFTG-TH                                                  
471900               MOVE 'THB'          TO 4104-KDVALISO                       
472000               COMPUTE WS-RADPRIS ROUNDED =                               
472100                     LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV            
472200           WHEN IDFTG-TW                                                  
472300               MOVE 'TWD'          TO 4104-KDVALISO                       
472400               COMPUTE WS-RADPRIS ROUNDED =                               
472500                     LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV            
472510           WHEN IDFTG-ZA                                                  
472520               MOVE 'ZAR'          TO 4104-KDVALISO                       
472530               COMPUTE WS-RADPRIS ROUNDED =                               
472540                     LEV-KVLEVANM-BEKR  * SPAR-PRARTBTO-LOCINV            
472600           WHEN OTHER                                                     
472700               COMPUTE WS-RADPRIS ROUNDED =                               
472800                      SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO                  
472900           END-EVALUATE                                                   
473000         END-IF                                                           
473100                                                                          
473200         SUBTRACT WS-RADPRIS FROM 4104-SUKRENOT                           
473300                                                                          
473400         PERFORM IMS-REPL-WDGX4104                                        
473500       END-IF                                                             
473600     END-IF                                                               
473700     .                                                                    
473800     EJECT                                                                
473900 S09-EV-UPPDAT-RAD18-WDGX4103  SECTION.                                   
474000                                                                          
474100     MOVE ZERO TO WS-RADPRIS                                              
474200     MOVE ZERO TO WS-NYTT-RADPRIS                                         
474300                                                                          
474400     IF LEV-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR           
474500                       '84' OR '27' OR '28' OR  '74'                      
474600       CONTINUE                                                           
474700     ELSE                                                                 
474800       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
474900          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
475000          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
475100          LEV-KDANMORS = '97'                                             
475200                                                                          
475300         MOVE LEV-IDDC            TO W-IDDC-4104                          
475400                                                                          
475500         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
475600              LEV-KDANMORS = '97'                                         
475700           MOVE 'RP'  TO W-KDKRENOT-4104                                  
475800         ELSE                                                             
475900           MOVE 'CN'  TO W-KDKRENOT-4104                                  
476000         END-IF                                                           
476100                                                                          
476200         IF WDR501-FINNS                                                  
476300           PERFORM IMS-GHNP-WDGX4104-KVAL                                 
476400           IF SEGMENT-FINNS                                               
476500             IF PRIS-AENDRAT                                              
476600               MOVE LEV-IDFTG  TO WS-IDFTG                                
476700               IF ANTAL-AENDRAT                                           
476800                 IF IDFTG-PV                                              
476900                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
477000                           LEV-KVLEVANM-BEKR * WS-IDEDITDATA              
477100                 ELSE                                                     
477200                   IF DIST79-DEALER-PRICE OR                              
477400                      DIST79-ECOM-PRICE                                   
477500                     COMPUTE WS-NYTT-RADPRIS ROUNDED =                    
477600                           LEV-KVLEVANM-BEKR * SPAR-PRARTBTO-LOC          
477700                   ELSE                                                   
477800*CHINA-PRICE6                                                             
477900*INDIA-PRICE6                                                             
478000*KOREA-PRICE6                                                             
478100                     MOVE LEV-IDFTG    TO WS-IDFTG                        
478200                     EVALUATE TRUE                                        
478300                     WHEN IDFTG-CN                                        
478400                         MOVE 'CNY' TO 4104-KDVALISO                      
478500                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
478600                                 LEV-KVLEVANM-BEKR  *                     
478700                                 SPAR-PRARTBTO-LOCINV                     
478800                     WHEN IDFTG-IN                                        
478900                         MOVE 'INR' TO 4104-KDVALISO                      
479000                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
479100                                 LEV-KVLEVANM-BEKR  *                     
479200                                 SPAR-PRARTBTO-LOCINV                     
479300                     WHEN IDFTG-KR                                        
479400                         MOVE 'KRW' TO 4104-KDVALISO                      
479500                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
479600                                 LEV-KVLEVANM-BEKR *                      
479700                                 SPAR-PRARTBTO-LOCINV                     
479800                     WHEN IDFTG-TR                                        
479900                         MOVE 'TRY' TO 4104-KDVALISO                      
480000                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
480100                                 LEV-KVLEVANM-BEKR *                      
480200                                 SPAR-PRARTBTO-LOCINV                     
480300                     WHEN IDFTG-MX                                        
480400                         MOVE 'MXN' TO 4104-KDVALISO                      
480500                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
480600                                 LEV-KVLEVANM-BEKR *                      
480700                                 SPAR-PRARTBTO-LOCINV                     
480800                     WHEN IDFTG-BR                                        
480900                         MOVE 'BRL' TO 4104-KDVALISO                      
481000                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
481100                                 LEV-KVLEVANM-BEKR *                      
481200                                 SPAR-PRARTBTO-LOCINV                     
481300                     WHEN IDFTG-MY                                        
481400                         MOVE 'MYR' TO 4104-KDVALISO                      
481500                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
481600                                 LEV-KVLEVANM-BEKR *                      
481700                                 SPAR-PRARTBTO-LOCINV                     
481800                     WHEN IDFTG-TH                                        
481900                         MOVE 'THB' TO 4104-KDVALISO                      
482000                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
482100                                 LEV-KVLEVANM-BEKR *                      
482200                                 SPAR-PRARTBTO-LOCINV                     
482300                     WHEN IDFTG-TW                                        
482400                         MOVE 'TWD' TO 4104-KDVALISO                      
482500                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
482600                                 LEV-KVLEVANM-BEKR *                      
482700                                 SPAR-PRARTBTO-LOCINV                     
482710                     WHEN IDFTG-ZA                                        
482720                         MOVE 'ZAR' TO 4104-KDVALISO                      
482730                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
482740                                 LEV-KVLEVANM-BEKR *                      
482750                                 SPAR-PRARTBTO-LOCINV                     
482800                     WHEN OTHER                                           
482900                         COMPUTE WS-NYTT-RADPRIS ROUNDED =                
483000                                 LEV-KVLEVANM-BEKR *                      
483100                                 SPAR-PRARTBTO                            
483200                     END-EVALUATE                                         
483300                   END-IF                                                 
483400                 END-IF                                                   
483500               ELSE                                                       
483600                 IF IDFTG-PV                                              
483700                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
483800                           LEV-KVLEVANM-BEKR * WS-IDEDITDATA              
483900                 END-IF                                                   
484000               END-IF                                                     
484100             ELSE                                                         
484200*-ANTALET ÄNDRAT.                                                         
484300               IF ANTAL-AENDRAT                                           
484400                 IF DIST79-DEALER-PRICE OR                                
484600                    DIST79-ECOM-PRICE                                     
484700                   COMPUTE WS-NYTT-RADPRIS ROUNDED =                      
484800                          LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC            
484900                 ELSE                                                     
485000*CHINA-PRICE7                                                             
485100*INDIA-PRICE7                                                             
485200*KOREA-PRICE7                                                             
485300                   MOVE LEV-IDFTG      TO WS-IDFTG                        
485400                   EVALUATE TRUE                                          
485500                   WHEN IDFTG-CN                                          
485600                       MOVE 'CNY'      TO 4104-KDVALISO                   
485700                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
485800                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
485900                   WHEN IDFTG-IN                                          
486000                       MOVE 'INR'      TO 4104-KDVALISO                   
486100                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
486200                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
486300                   WHEN IDFTG-KR                                          
486400                       MOVE 'KRW'      TO 4104-KDVALISO                   
486500                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
486600                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
486700                   WHEN IDFTG-TR                                          
486800                       MOVE 'TRY'      TO 4104-KDVALISO                   
486900                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
487000                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
487100                   WHEN IDFTG-MX                                          
487200                       MOVE 'MXN'      TO 4104-KDVALISO                   
487300                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
487400                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
487500                   WHEN IDFTG-BR                                          
487600                       MOVE 'BRL'      TO 4104-KDVALISO                   
487700                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
487800                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
487900                   WHEN IDFTG-MY                                          
488000                       MOVE 'MYR'      TO 4104-KDVALISO                   
488100                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
488200                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
488300                   WHEN IDFTG-TH                                          
488400                       MOVE 'THB'      TO 4104-KDVALISO                   
488500                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
488600                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
488700                   WHEN IDFTG-TW                                          
488800                       MOVE 'TWD'      TO 4104-KDVALISO                   
488900                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
489000                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
489010                   WHEN IDFTG-ZA                                          
489020                       MOVE 'ZAR'      TO 4104-KDVALISO                   
489030                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
489040                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV        
489100                   WHEN OTHER                                             
489200                       COMPUTE WS-NYTT-RADPRIS ROUNDED =                  
489300                           LEV-KVLEVANM-BEKR * LEV-PRARTBTO               
489400                   END-EVALUATE                                           
489500                 END-IF                                                   
489600               END-IF                                                     
489700             END-IF                                                       
489800                                                                          
489900*- TAG BORT DET GAMLA PRISET OCH BYT TILL NYA.WDGX4104 = KN-SUMMA.        
490000             IF DIST79-DEALER-PRICE OR                                    
490200                DIST79-ECOM-PRICE                                         
490300               COMPUTE WS-RADPRIS ROUNDED =                               
490400                       SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOC             
490500             ELSE                                                         
490600*CHINA-PRICE8                                                             
490700*INDIA-PRICE8                                                             
490800*KOREA-PRICE8                                                             
490900               MOVE LEV-IDFTG          TO WS-IDFTG                        
491000               EVALUATE TRUE                                              
491100               WHEN IDFTG-CN                                              
491200                   MOVE 'CNY'          TO 4104-KDVALISO                   
491300                   COMPUTE WS-RADPRIS ROUNDED =                           
491400                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
491500               WHEN IDFTG-IN                                              
491600                   MOVE 'INR'          TO 4104-KDVALISO                   
491700                   COMPUTE WS-RADPRIS ROUNDED =                           
491800                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
491900               WHEN IDFTG-KR                                              
492000                   MOVE 'KRW'          TO 4104-KDVALISO                   
492100                   COMPUTE WS-RADPRIS ROUNDED =                           
492200                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
492300               WHEN IDFTG-TR                                              
492400                   MOVE 'TRY'          TO 4104-KDVALISO                   
492500                   COMPUTE WS-RADPRIS ROUNDED =                           
492600                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
492700               WHEN IDFTG-MX                                              
492800                   MOVE 'MXN'          TO 4104-KDVALISO                   
492900                   COMPUTE WS-RADPRIS ROUNDED =                           
493000                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
493100               WHEN IDFTG-BR                                              
493200                   MOVE 'BRL'          TO 4104-KDVALISO                   
493300                   COMPUTE WS-RADPRIS ROUNDED =                           
493400                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
493500               WHEN IDFTG-MY                                              
493600                   MOVE 'MYR'          TO 4104-KDVALISO                   
493700                   COMPUTE WS-RADPRIS ROUNDED =                           
493800                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
493900               WHEN IDFTG-TH                                              
494000                   MOVE 'THB'          TO 4104-KDVALISO                   
494100                   COMPUTE WS-RADPRIS ROUNDED =                           
494200                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
494300               WHEN IDFTG-TW                                              
494400                   MOVE 'TWD'          TO 4104-KDVALISO                   
494500                   COMPUTE WS-RADPRIS ROUNDED =                           
494600                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
494610               WHEN IDFTG-ZA                                              
494620                   MOVE 'ZAR'          TO 4104-KDVALISO                   
494630                   COMPUTE WS-RADPRIS ROUNDED =                           
494640                         SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO-LOCINV        
494700               WHEN OTHER                                                 
494800                   COMPUTE WS-RADPRIS ROUNDED =                           
494900                          SPAR-KVLEVANM-BEKR * SPAR-PRARTBTO              
495000               END-EVALUATE                                               
495100             END-IF                                                       
495200                                                                          
495300             SUBTRACT WS-RADPRIS FROM 4104-SUKRENOT                       
495400             ADD WS-NYTT-RADPRIS   TO 4104-SUKRENOT                       
495500                                                                          
495600             PERFORM IMS-REPL-WDGX4104                                    
495700           END-IF                                                         
495800         END-IF                                                           
495900       END-IF                                                             
496000     END-IF                                                               
496100     .                                                                    
496200     EJECT                                                                
496300 S10-HAMTA-KDVALISO SECTION.                                              
496400                                                                          
496500     PERFORM IMS-GU-GMTA-WDB201                                           
496600     IF SEGMENT-FINNS                                                     
496700       CONTINUE                                                           
496800     ELSE                                                                 
496900       PERFORM IMS-GET-WDB201                                             
497000     END-IF                                                               
497100     MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                           
497200     MOVE GMT-IDFTG          TO W-WDB1-IDFTG                              
497300     PERFORM IMS-GU-WDB1-WDB101                                           
497400     IF SEGMENT-FINNS                                                     
497500       MOVE MSGI-IDFTG       TO WS-IDFTG                                  
497600       IF DIST79-DEALER-PRICE OR                                          
497700                                 IDFTG-US OR IDFTG-CA OR                  
497800                                 IDFTG-CN OR IDFTG-IN OR                  
497900                                 IDFTG-KR OR IDFTG-TR OR                  
498000                                 IDFTG-MX OR IDFTG-BR OR                  
498100                                 IDFTG-MY OR IDFTG-TH OR IDFTG-TW         
498110                                          OR IDFTG-ZA                     
498200                                                                          
498300**-- HÅRDKODA ENLIGT BOSSE H EFTERSOM BET-KDVALISO = SEK (5131)           
498400**-- FÖR KINA OCH VIPS FAKTURERAR I CNY.                                  
498500         EVALUATE TRUE                                                    
498600         WHEN IDFTG-CN                                                    
498700             MOVE 'CNY'        TO MOD-KDVALISO                            
498800         WHEN IDFTG-IN                                                    
498900             MOVE 'INR'        TO MOD-KDVALISO                            
499000         WHEN IDFTG-KR                                                    
499100             MOVE 'KRW'        TO MOD-KDVALISO                            
499200         WHEN IDFTG-TR                                                    
499300             MOVE 'TRY'        TO MOD-KDVALISO                            
499400         WHEN IDFTG-MX                                                    
499500             MOVE 'MXN'        TO MOD-KDVALISO                            
499600         WHEN IDFTG-BR                                                    
499700             MOVE 'BRL'        TO MOD-KDVALISO                            
499800         WHEN IDFTG-MY                                                    
499900             MOVE 'MYR'        TO MOD-KDVALISO                            
500000         WHEN IDFTG-TH                                                    
500100             MOVE 'THB'        TO MOD-KDVALISO                            
500200         WHEN IDFTG-TW                                                    
500300             MOVE 'TWD'        TO MOD-KDVALISO                            
500310         WHEN IDFTG-ZA                                                    
500320             MOVE 'ZAR'        TO MOD-KDVALISO                            
500400         WHEN DIST79-ECOM-PRICE                                           
500500             MOVE ANM-KDVALISO TO MOD-KDVALISO                            
500600                                  RESP-KDVALISO                           
500700         WHEN OTHER                                                       
500800             MOVE BET-KDVALISO TO MOD-KDVALISO                            
500900                                  RESP-KDVALISO                           
501000         END-EVALUATE                                                     
501100       ELSE                                                               
501200         MOVE 'SEK'          TO MOD-KDVALISO                              
501300                                RESP-KDVALISO                             
501400       END-IF                                                             
501500     ELSE                                                                 
501600       MOVE SPACE            TO MOD-KDVALISO                              
501700                                RESP-KDVALISO                             
501800     END-IF                                                               
501900     .                                                                    
502000     EJECT                                                                
502100 S11-REPL-WDGX4104 SECTION.                                               
502200                                                                          
502300*- GAMLA KODEN FINNS EJ UPPLAGD SOM KNOTA MEN ANDRA DC/RADER FINNS        
502400                                                                          
502500     IF DIST79-DEALER-PRICE OR                                            
502700        DIST79-ECOM-PRICE                                                 
502800       COMPUTE WS-RADPRIS ROUNDED =                                       
502900                      LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC                
503000     ELSE                                                                 
503100*                                                                         
503200*CHINA-PRICE9                                                             
503300*INDIA-PRICE9                                                             
503400*KOREA-PRICE9                                                             
503500       MOVE LEV-IDFTG                  TO WS-IDFTG                        
503600       EVALUATE TRUE                                                      
503700       WHEN IDFTG-CN                                                      
503800           MOVE 'CNY'          TO 4104-KDVALISO                           
503900           COMPUTE WS-RADPRIS ROUNDED =                                   
504000                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
504100       WHEN IDFTG-IN                                                      
504200           MOVE 'INR'          TO 4104-KDVALISO                           
504300           COMPUTE WS-RADPRIS ROUNDED =                                   
504400                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
504500       WHEN IDFTG-KR                                                      
504600           MOVE 'KRW'          TO 4104-KDVALISO                           
504700           COMPUTE WS-RADPRIS ROUNDED =                                   
504800                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
504900       WHEN IDFTG-TR                                                      
505000           MOVE 'TRY'          TO 4104-KDVALISO                           
505100           COMPUTE WS-RADPRIS ROUNDED =                                   
505200                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
505300       WHEN IDFTG-MX                                                      
505400           MOVE 'MXN'          TO 4104-KDVALISO                           
505500           COMPUTE WS-RADPRIS ROUNDED =                                   
505600                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
505700       WHEN IDFTG-BR                                                      
505800           MOVE 'BRL'          TO 4104-KDVALISO                           
505900           COMPUTE WS-RADPRIS ROUNDED =                                   
506000                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
506100       WHEN IDFTG-MY                                                      
506200           MOVE 'MYR'          TO 4104-KDVALISO                           
506300           COMPUTE WS-RADPRIS ROUNDED =                                   
506400                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
506500       WHEN IDFTG-TH                                                      
506600           MOVE 'THB'          TO 4104-KDVALISO                           
506700           COMPUTE WS-RADPRIS ROUNDED =                                   
506800                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
506900       WHEN IDFTG-TW                                                      
507000           MOVE 'TWD'          TO 4104-KDVALISO                           
507100           COMPUTE WS-RADPRIS ROUNDED =                                   
507200                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
507210       WHEN IDFTG-ZA                                                      
507220           MOVE 'ZAR'          TO 4104-KDVALISO                           
507230           COMPUTE WS-RADPRIS ROUNDED =                                   
507240                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
507300       WHEN OTHER                                                         
507400           COMPUTE WS-RADPRIS ROUNDED =                                   
507500                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO                         
507600       END-EVALUATE                                                       
507700     END-IF                                                               
507800                                                                          
507900     ADD WS-RADPRIS   TO 4104-SUKRENOT                                    
508000                                                                          
508100     PERFORM IMS-REPL-WDGX4104                                            
508200     .                                                                    
508300     EJECT                                                                
508400 S12-INSERT-WDGX4104 SECTION.                                             
508500                                                                          
508600     MOVE LEV-IDDC          TO 4104-IDDC                                  
508700     MOVE W-KDKRENOT-4104   TO 4104-KDKRENOT                              
508800     MOVE NEJ               TO 4104-FLKREATT                              
508900     MOVE ANM-KDVALISO      TO 4104-KDVALISO                              
509000     MOVE SPACE             TO 4104-FILLER                                
509100                                                                          
509200     IF DIST79-DEALER-PRICE OR                                            
509400        DIST79-ECOM-PRICE                                                 
509500       COMPUTE WS-RADPRIS ROUNDED =                                       
509600                      LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC                
509700     ELSE                                                                 
509800*                                                                         
509900*CHINA-PRICE6                                                             
510000*INDIA-PRICE6                                                             
510100*KOREA-PRICE6                                                             
510200       MOVE LEV-IDFTG                  TO WS-IDFTG                        
510300       EVALUATE TRUE                                                      
510400       WHEN IDFTG-CN                                                      
510500           MOVE 'CNY'          TO 4104-KDVALISO                           
510600           COMPUTE WS-RADPRIS ROUNDED =                                   
510700                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
510800       WHEN IDFTG-IN                                                      
510900           MOVE 'INR'          TO 4104-KDVALISO                           
511000           COMPUTE WS-RADPRIS ROUNDED =                                   
511100                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
511200       WHEN IDFTG-KR                                                      
511300           MOVE 'KRW'          TO 4104-KDVALISO                           
511400           COMPUTE WS-RADPRIS ROUNDED =                                   
511500                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
511600       WHEN IDFTG-TR                                                      
511700           MOVE 'TRY'          TO 4104-KDVALISO                           
511800           COMPUTE WS-RADPRIS ROUNDED =                                   
511900                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
512000       WHEN IDFTG-MX                                                      
512100           MOVE 'MXN'          TO 4104-KDVALISO                           
512200           COMPUTE WS-RADPRIS ROUNDED =                                   
512300                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
512400       WHEN IDFTG-BR                                                      
512500           MOVE 'BRL'          TO 4104-KDVALISO                           
512600           COMPUTE WS-RADPRIS ROUNDED =                                   
512700                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
512800       WHEN IDFTG-MY                                                      
512900           MOVE 'MYR'          TO 4104-KDVALISO                           
513000           COMPUTE WS-RADPRIS ROUNDED =                                   
513100                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
513200       WHEN IDFTG-TH                                                      
513300           MOVE 'THB'          TO 4104-KDVALISO                           
513400           COMPUTE WS-RADPRIS ROUNDED =                                   
513500                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
513600       WHEN IDFTG-TW                                                      
513700           MOVE 'TWD'          TO 4104-KDVALISO                           
513800           COMPUTE WS-RADPRIS ROUNDED =                                   
513900                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
513910       WHEN IDFTG-ZA                                                      
513920           MOVE 'ZAR'          TO 4104-KDVALISO                           
513930           COMPUTE WS-RADPRIS ROUNDED =                                   
513940                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV                  
514000       WHEN OTHER                                                         
514100           COMPUTE WS-RADPRIS ROUNDED =                                   
514200                 LEV-KVLEVANM-BEKR * LEV-PRARTBTO                         
514300       END-EVALUATE                                                       
514400     END-IF                                                               
514500                                                                          
514600     MOVE WS-RADPRIS        TO 4104-SUKRENOT                              
514700                                                                          
514800     PERFORM IMS-ISRT-WDGX4104                                            
514900                                                                          
515000     .                                                                    
515100     EJECT                                                                
515200 S13-KOLLA-RETUR-DC    SECTION.                                           
515300                                                                          
515400     MOVE NEJ                       TO  GODK-KOD-SW                       
515500     MOVE JA                        TO  GODK-ARTIKEL-SW                   
515600                                        GODK-IDFKNGRP-SW                  
515700                                        GODK-DC-ARTIKEL-SW                
515800                                        GODK-DC-LEV-SW                    
515900                                                                          
516000     MOVE GMT-IDDC-RET72(1)   TO W-IDDC-B6                                
516100                                 W-IDDC-K7                                
516200     PERFORM IMS-GU-WDB601                                                
516300     IF SEGMENT-SAKNAS                                                    
516400       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
516500          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
516600          DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR                     
516700          DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                     
516710          DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC                    
516800         MOVE GMT-IDDC-RET    TO LEV-IDDC-RET                             
516900                                 WS-ANM-IDDC-RET                          
517000       ELSE                                                               
517100         MOVE WS-CDC-SE       TO LEV-IDDC-RET                             
517200                                 WS-ANM-IDDC-RET                          
517300       END-IF                                                             
517400       MOVE 3                 TO WS-ANM-IXDCCLEAR                         
517500     ELSE                                                                 
517600       IF DCS-FLARTDC = JA                                                
517700*- KOLLA OM ARTIKELN FINNS PÅ DC'T. KRAV FÖR ATT TA EMOT RETUR.           
517800         MOVE LEV-IDARTNR     TO W-IDARTNR-K7                             
517900         PERFORM IMS-GU-WDK711                                            
518000         IF SEGMENT-SAKNAS                                                
518100           MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                               
518200         END-IF                                                           
518300       END-IF                                                             
518400                                                                          
518500       IF EJ-GODK-DC-ARTIKEL                                              
518600         CONTINUE                                                         
518700       ELSE                                                               
518800         MOVE SPACE           TO W-URV-TEELMT                             
518900         MOVE WC-IDARTNR      TO W-URV-TEELMT                             
519000         MOVE SPACE           TO W-URV-FILLER                             
519100         MOVE LEV-IDARTNR     TO W-URV-IDARTNR-EXCP                       
519200                                                                          
519300         PERFORM IMS-GNP-WDB611-FIRST                                     
519400         IF SEGMENT-FINNS                                                 
519500           MOVE NEJ           TO GODK-ARTIKEL-SW                          
519600         ELSE                                                             
519700           MOVE LEV-IDARTNR   TO W-IDARTNR-ARTC                           
519800           PERFORM IMS-GU-ARTC01                                          
519900           IF SEGMENT-SAKNAS                                              
520000             MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                             
520100           ELSE                                                           
520200             MOVE SPACE           TO W-URV-TEELMT                         
520300             MOVE WC-IDFKNGRP     TO W-URV-TEELMT                         
520400             MOVE SPACE           TO W-URV-FILLER                         
520500             MOVE ART-IDFKNGRP    TO W-URV-IDFKNGRP-EXCP                  
520600                                                                          
520700             PERFORM IMS-GNP-WDB611-FIRST                                 
520800             IF SEGMENT-FINNS                                             
520900               MOVE NEJ           TO GODK-IDFKNGRP-SW                     
521000             ELSE                                                         
521100               MOVE SPACE           TO W-URV-TEELMT                       
521200               MOVE WC-IDDC-EXCP    TO W-URV-TEELMT                       
521300               MOVE SPACE           TO W-URV-FILLER                       
521400               MOVE LEV-IDDC        TO W-URV-IDDC-EXCP                    
521500                                                                          
521600               PERFORM IMS-GNP-WDB611-FIRST                               
521700               IF SEGMENT-FINNS                                           
521800                 MOVE NEJ           TO GODK-DC-LEV-SW                     
521900               ELSE                                                       
522000                 MOVE SPACE           TO W-URV-TEELMT                     
522100                 MOVE WC-KDANMORS     TO W-URV-TEELMT                     
522200                 MOVE SPACE           TO W-URV-FILLER                     
522300                 MOVE KDANMORS-WS     TO W-URV-KDANMORS-RET               
522400                                                                          
522500                 PERFORM IMS-GNP-WDB611-FIRST                             
522600                 IF SEGMENT-FINNS                                         
522700                   MOVE JA            TO GODK-KOD-SW                      
522800                 END-IF                                                   
522900               END-IF                                                     
523000             END-IF                                                       
523100           END-IF                                                         
523200         END-IF                                                           
523300       END-IF                                                             
523400                                                                          
523500       IF EJ-GODK-DC-ARTIKEL OR                                           
523600          EJ-GODK-ARTIKEL OR                                              
523700          EJ-GODK-IDFKNGRP OR                                             
523800          EJ-GODK-DC-LEV   OR                                             
523900          EJ-GODK-KOD                                                     
524000                                                                          
524100          IF GMT-IDDC-RET72(2) = SPACE                                    
524200            MOVE GMT-IDDC-RET72(3)  TO LEV-IDDC-RET                       
524300                                       WS-ANM-IDDC-RET                    
524400            MOVE 3                  TO WS-ANM-IXDCCLEAR                   
524500            IF LEV-IDDC-RET  = SPACE                                      
524600              IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                  
524700                 DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                 
524800                 DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR              
524900                 DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR              
524910                 DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC             
525000                MOVE GMT-IDDC-RET   TO LEV-IDDC-RET                       
525100                                       WS-ANM-IDDC-RET                    
525200              ELSE                                                        
525300                MOVE WS-CDC-SE      TO LEV-IDDC-RET                       
525400                                       WS-ANM-IDDC-RET                    
525500              END-IF                                                      
525600              MOVE 3                TO WS-ANM-IXDCCLEAR                   
525700            END-IF                                                        
525800          ELSE                                                            
525900            PERFORM S14-KOLLA-RETUR-DC-2                                  
526000          END-IF                                                          
526100       ELSE                                                               
526200         MOVE GMT-IDDC-RET72(1)     TO LEV-IDDC-RET                       
526300                                       WS-ANM-IDDC-RET                    
526400         MOVE 1                     TO WS-ANM-IXDCCLEAR                   
526500       END-IF                                                             
526600     END-IF                                                               
526700     .                                                                    
526800     EJECT                                                                
526900 S14-KOLLA-RETUR-DC-2  SECTION.                                           
527000                                                                          
527100     MOVE NEJ                       TO  GODK-KOD-SW                       
527200     MOVE JA                        TO  GODK-ARTIKEL-SW                   
527300                                        GODK-IDFKNGRP-SW                  
527400                                        GODK-DC-ARTIKEL-SW                
527500                                        GODK-DC-LEV-SW                    
527600                                                                          
527700     MOVE GMT-IDDC-RET72(2)   TO W-IDDC-B6                                
527800                                 W-IDDC-K7                                
527900     PERFORM IMS-GU-WDB601                                                
528000     IF SEGMENT-SAKNAS                                                    
528100       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
528200          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
528300          DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR                     
528400          DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                     
528410          DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC                    
528500         MOVE GMT-IDDC-RET      TO LEV-IDDC-RET                           
528600                                   WS-ANM-IDDC-RET                        
528700       ELSE                                                               
528800         MOVE WS-CDC-SE         TO LEV-IDDC-RET                           
528900                                   WS-ANM-IDDC-RET                        
529000       END-IF                                                             
529100       MOVE 3                TO WS-ANM-IXDCCLEAR                          
529200     ELSE                                                                 
529300       IF DCS-FLARTDC = JA                                                
529400*- KOLLA OM ARTIKELN FINNS PÅ DC'T. KRAV FÖR ATT TA EMOT RETUR.           
529500         MOVE LEV-IDARTNR  TO W-IDARTNR-K7                                
529600         PERFORM IMS-GU-WDK711                                            
529700         IF SEGMENT-SAKNAS                                                
529800           MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                               
529900         END-IF                                                           
530000       END-IF                                                             
530100                                                                          
530200       IF EJ-GODK-DC-ARTIKEL                                              
530300         CONTINUE                                                         
530400       ELSE                                                               
530500         MOVE SPACE           TO W-URV-TEELMT                             
530600         MOVE WC-IDARTNR      TO W-URV-TEELMT                             
530700         MOVE SPACE           TO W-URV-FILLER                             
530800         MOVE LEV-IDARTNR     TO W-URV-IDARTNR-EXCP                       
530900                                                                          
531000         PERFORM IMS-GNP-WDB611-FIRST                                     
531100         IF SEGMENT-FINNS                                                 
531200           MOVE NEJ           TO GODK-ARTIKEL-SW                          
531300         ELSE                                                             
531400           MOVE LEV-IDARTNR  TO W-IDARTNR-ARTC                            
531500           PERFORM IMS-GU-ARTC01                                          
531600           IF SEGMENT-SAKNAS                                              
531700             MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                             
531800           ELSE                                                           
531900             MOVE SPACE            TO W-URV-TEELMT                        
532000             MOVE WC-IDFKNGRP      TO W-URV-TEELMT                        
532100             MOVE SPACE            TO W-URV-FILLER                        
532200             MOVE ART-IDFKNGRP     TO W-URV-IDFKNGRP-EXCP                 
532300                                                                          
532400             PERFORM IMS-GNP-WDB611-FIRST                                 
532500             IF SEGMENT-FINNS                                             
532600               MOVE NEJ            TO GODK-IDFKNGRP-SW                    
532700             ELSE                                                         
532800               MOVE SPACE            TO W-URV-TEELMT                      
532900               MOVE WC-IDDC-EXCP     TO W-URV-TEELMT                      
533000               MOVE SPACE            TO W-URV-FILLER                      
533100               MOVE LEV-IDDC         TO W-URV-IDDC-EXCP                   
533200                                                                          
533300               PERFORM IMS-GNP-WDB611-FIRST                               
533400               IF SEGMENT-FINNS                                           
533500                 MOVE NEJ            TO GODK-DC-LEV-SW                    
533600               ELSE                                                       
533700                 MOVE SPACE          TO W-URV-TEELMT                      
533800                 MOVE WC-KDANMORS    TO W-URV-TEELMT                      
533900                 MOVE SPACE          TO W-URV-FILLER                      
534000                 MOVE KDANMORS-WS    TO W-URV-KDANMORS-RET                
534100                                                                          
534200                 PERFORM IMS-GNP-WDB611-FIRST                             
534300                 IF SEGMENT-FINNS                                         
534400                   MOVE JA           TO GODK-KOD-SW                       
534500                 END-IF                                                   
534600               END-IF                                                     
534700             END-IF                                                       
534800           END-IF                                                         
534900         END-IF                                                           
535000       END-IF                                                             
535100                                                                          
535200       IF EJ-GODK-DC-ARTIKEL OR                                           
535300          EJ-GODK-ARTIKEL OR                                              
535400          EJ-GODK-IDFKNGRP OR                                             
535500          EJ-GODK-DC-LEV OR                                               
535600          EJ-GODK-KOD                                                     
535700                                                                          
535800          MOVE GMT-IDDC-RET72(3)   TO LEV-IDDC-RET                        
535900                                      WS-ANM-IDDC-RET                     
536000          MOVE 3                   TO WS-ANM-IXDCCLEAR                    
536100          IF LEV-IDDC-RET  = SPACE                                        
536200            IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                    
536300               DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                   
536400               DIST34-MALAYSIA-NDC OR DIST34-MEXICO-NDC OR                
536500               DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                
536510               DIST34-SOUTH-AFRICA-NDC OR DIST34-BRAZIL-NDC               
536600              MOVE GMT-IDDC-RET      TO LEV-IDDC-RET                      
536700                                        WS-ANM-IDDC-RET                   
536800            ELSE                                                          
536900              MOVE WS-CDC-SE         TO LEV-IDDC-RET                      
537000                                        WS-ANM-IDDC-RET                   
537100            END-IF                                                        
537200            MOVE 3                 TO WS-ANM-IXDCCLEAR                    
537300          END-IF                                                          
537400       ELSE                                                               
537500         MOVE GMT-IDDC-RET72(2)    TO LEV-IDDC-RET                        
537600                                      WS-ANM-IDDC-RET                     
537700         MOVE 2                    TO WS-ANM-IXDCCLEAR                    
537800       END-IF                                                             
537900     END-IF                                                               
538000     .                                                                    
538100     EJECT                                                                
538200 S15-EV-UPPDAT-KN-WDGX4103-IGEN  SECTION.                                 
538300                                                                          
538400     MOVE ZERO TO WS-RADPRIS                                              
538500                                                                          
538600*** KOLLA IFALL KODEN GER KNOTA SOM SKALL ATTESTERAS.                     
538700     IF LEV-KDANMORS = '12' OR '22' OR '99' OR  '13' OR '23' OR           
538800                       '84' OR '27' OR '28' OR  '74'                      
538900       CONTINUE                                                           
539000     ELSE                                                                 
539100       IF (OKOD-FL-KRENOT-DIREKT = JA)  OR                                
539200          (OKOD-FL-KRENOT-DIREKT-SKR = JA)  OR                            
539300          (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
539400          LEV-KDANMORS = '97'                                             
539500                                                                          
539600         MOVE LEV-IDDC            TO W-IDDC-4104                          
539700                                                                          
539800         IF (OKOD-FL-KRENOT-EFTER-RT = JA)  OR                            
539900              LEV-KDANMORS = '97'                                         
540000           MOVE 'RP'  TO W-KDKRENOT-4104                                  
540100         ELSE                                                             
540200           MOVE 'CN'  TO W-KDKRENOT-4104                                  
540300         END-IF                                                           
540400                                                                          
540500         IF WDR501-FINNS                                                  
540600           PERFORM IMS-GHNP-WDGX4104-KVAL                                 
540700           IF SEGMENT-FINNS                                               
540800             IF DIST79-DEALER-PRICE OR                                    
541000                DIST79-ECOM-PRICE                                         
541100               COMPUTE WS-RADPRIS ROUNDED =                               
541200                       LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOC               
541300             ELSE                                                         
541400*CHINA-PRICE11                                                            
541500*INDIA-PRICE11                                                            
541600*KOREA-PRICE11                                                            
541700               MOVE LEV-IDFTG          TO WS-IDFTG                        
541800               EVALUATE TRUE                                              
541900               WHEN IDFTG-CN                                              
542000                   MOVE 'CNY'          TO 4104-KDVALISO                   
542100                   COMPUTE WS-RADPRIS ROUNDED =                           
542200                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
542300               WHEN IDFTG-IN                                              
542400                   MOVE 'INR'          TO 4104-KDVALISO                   
542500                   COMPUTE WS-RADPRIS ROUNDED =                           
542600                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
542700               WHEN IDFTG-KR                                              
542800                   MOVE 'KRW'          TO 4104-KDVALISO                   
542900                   COMPUTE WS-RADPRIS ROUNDED =                           
543000                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
543100               WHEN IDFTG-TR                                              
543200                   MOVE 'TRY'          TO 4104-KDVALISO                   
543300                   COMPUTE WS-RADPRIS ROUNDED =                           
543400                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
543500               WHEN IDFTG-MX                                              
543600                   MOVE 'MXN'          TO 4104-KDVALISO                   
543700                   COMPUTE WS-RADPRIS ROUNDED =                           
543800                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
543900               WHEN IDFTG-BR                                              
544000                   MOVE 'BRL'          TO 4104-KDVALISO                   
544100                   COMPUTE WS-RADPRIS ROUNDED =                           
544200                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
544300               WHEN IDFTG-MY                                              
544400                   MOVE 'MYR'          TO 4104-KDVALISO                   
544500                   COMPUTE WS-RADPRIS ROUNDED =                           
544600                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
544700               WHEN IDFTG-TH                                              
544800                   MOVE 'THB'          TO 4104-KDVALISO                   
544900                   COMPUTE WS-RADPRIS ROUNDED =                           
545000                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
545100               WHEN IDFTG-TW                                              
545200                   MOVE 'TWD'          TO 4104-KDVALISO                   
545300                   COMPUTE WS-RADPRIS ROUNDED =                           
545400                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
545410               WHEN IDFTG-ZA                                              
545420                   MOVE 'ZAR'          TO 4104-KDVALISO                   
545430                   COMPUTE WS-RADPRIS ROUNDED =                           
545440                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO-LOCINV          
545500               WHEN OTHER                                                 
545600                   COMPUTE WS-RADPRIS ROUNDED =                           
545700                         LEV-KVLEVANM-BEKR * LEV-PRARTBTO                 
545800               END-EVALUATE                                               
545900             END-IF                                                       
546000                                                                          
546100             ADD WS-RADPRIS TO 4104-SUKRENOT                              
546200                                                                          
546300             PERFORM IMS-REPL-WDGX4104                                    
546400           ELSE                                                           
546500             PERFORM S12-INSERT-WDGX4104                                  
546600           END-IF                                                         
546700         ELSE                                                             
546800           MOVE '4103'           TO 4103-IDHTYP                           
546900           MOVE W-IDDISTR-4103   TO 4103-IDDISTR                          
547000           MOVE W-IDKUNDNR-4103  TO 4103-IDKUNDNR                         
547100           MOVE W-IDRAPPNR-4103  TO 4103-IDRAPPNR                         
547200           MOVE LOW-VALUE        TO 4103-LOW-VALUE                        
547300                                                                          
547400           PERFORM IMS-ISRT-WDGX4103                                      
547500           MOVE JA    TO WDR501-SW                                        
547600           PERFORM S12-INSERT-WDGX4104                                    
547700         END-IF                                                           
547800       END-IF                                                             
547900     END-IF                                                               
548000     .                                                                    
548100     EJECT                                                                
548200 S16-INDATA-TILL-MOD    SECTION.                                          
548300                                                                          
548400     IF MID-RAD18-IDARTNR = ALL '+'                                       
548500       MOVE MFS-RENSA-FAELT       TO MOD-RAD18-IDARTNR                    
548600     ELSE                                                                 
548700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-RAD18-IDARTNR-ATTRIBUT           
548800       MOVE MFS-ROER-EJ-FAELT     TO MOD-RAD18-IDARTNR                    
548900     END-IF                                                               
549000                                                                          
549100     IF MID-RAD18-IDRADNR = ALL '+'                                       
549200       MOVE MFS-RENSA-FAELT       TO MOD-RAD18-IDRADNR                    
549300     ELSE                                                                 
549400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-RAD18-IDRADNR-ATTRIBUT           
549500       MOVE MFS-ROER-EJ-FAELT     TO MOD-RAD18-IDRADNR                    
549600     END-IF                                                               
549700                                                                          
549800     IF MID-RAD18-KDANMORS = ALL '+'                                      
549900       MOVE MFS-RENSA-FAELT       TO MOD-RAD18-KDANMORS                   
550000     ELSE                                                                 
550100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-RAD18-KDANMORS-ATTRIBUT          
550200       MOVE MFS-ROER-EJ-FAELT     TO MOD-RAD18-KDANMORS                   
550300     END-IF                                                               
550400                                                                          
550500     IF MID-RAD18-KVLEVANM = ALL '+'                                      
550600       MOVE MFS-RENSA-FAELT       TO MOD-RAD18-KVLEVANM                   
550700     ELSE                                                                 
550800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-RAD18-KVLEVANM-ATTRIBUT          
550900       MOVE MFS-ROER-EJ-FAELT     TO MOD-RAD18-KVLEVANM                   
551000     END-IF                                                               
551100                                                                          
551200     IF MID-RAD18-PRARTBTO = ALL '+'                                      
551300       MOVE MFS-RENSA-FAELT       TO MOD-RAD18-PRARTBTO                   
551400     ELSE                                                                 
551500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-RAD18-PRARTBTO-ATTRIBUT          
551600       MOVE MFS-ROER-EJ-FAELT     TO MOD-RAD18-PRARTBTO                   
551700     END-IF                                                               
551800                                                                          
551900     IF MID-RAD18-FLDIRLEV = ALL '+'                                      
552000       MOVE MFS-RENSA-FAELT       TO MOD-RAD18-FLDIRLEV                   
552100     ELSE                                                                 
552200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-RAD18-FLDIRLEV-ATTRIBUT          
552300       MOVE MFS-ROER-EJ-FAELT     TO MOD-RAD18-FLDIRLEV                   
552400     END-IF                                                               
552500                                                                          
552600     .                                                                    
552700     EJECT                                                                
552800*    --- DISPATCHER SECTIONS                                              
552900 S17-FETCH-REQUEST-ARGUMENT SECTION.                                      
553000                                                                          
553100     MOVE 'GETARG'               TO SUB-KDFUNC                            
553200     MOVE 'CARPARTS.PULS.APIDISCRQUERY'     TO SUB-ADDISPABS              
553300     MOVE SPACE TO SUB-DATA                                               
553400     MOVE LENGTH OF SUB-DATA          TO SUB-KVDLEN                       
553500                                                                          
553600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN SUB-DATA              
553700                                                                          
553800     IF SUB-KDRC > 0                                                      
553900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
554000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
554100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
554200       CALL FELLOG USING RKOD-ABEND-WITH-DUMP                             
554300     END-IF                                                               
554400     .                                                                    
554500     SKIP3                                                                
554600 S18-RETURN-RESPONSE SECTION.                                             
554700                                                                          
554800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
554900     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
555000                                                                          
555100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
555200                                                                          
555300     IF SUB-KDRC > 0                                                      
555400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
555500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
555600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
555700       CALL FELLOG USING RKOD-ABEND-WITH-DUMP                             
555800     END-IF                                                               
555900     .                                                                    
556000 S60-CONVERT-DATE SECTION.                                                
556100                                                                          
556200     IF WS-DATE-NUM6 NOT = ZERO                                           
556300        MOVE WS-DATE-NUM6-YY   TO WS-YY-CHAR                              
556400        MOVE WS-DATE-NUM6-MM   TO WS-MM-CHAR                              
556500        MOVE WS-DATE-NUM6-DD   TO WS-DD-CHAR                              
556600     ELSE                                                                 
556700        MOVE WS-DATE-NUM10-YY  TO WS-YY-CHAR                              
556800        MOVE WS-DATE-NUM10-MM  TO WS-MM-CHAR                              
556900        MOVE WS-DATE-NUM10-DD  TO WS-DD-CHAR                              
557000     END-IF                                                               
557100     .                                                                    
557200                                                                          
557300                                                                          
557400* IMS SEKTIONER                                                           
557500                                                                          
557600 IMS-INSERT-MSG                 SECTION.                                  
557700                                                                          
557800     IF MSGI-IDLAND-SPR = 'GB'                                            
557900       MOVE 'N' TO MFS-KDHUVOMR                                           
558000     END-IF                                                               
558100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
558200     MOVE SPACE TO GODK-STATUSKODER                                       
558300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
558400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
558500     PERFORM IMS-STATUSKONTROLL                                           
558600     .                                                                    
558700     EJECT                                                                
558800 IMS-PURGE-ALTMSG-4793  SECTION.                                          
558900                                                                          
559000     MOVE LOW-VALUE TO 4793-MID-Z1 4793-MID-Z2                            
559100     MOVE '  ' TO GODK-STATUSKODER                                        
559200     CALL CBLTDLI USING PURG 4793-PCB 4793-MID-IO-AREA                    
559300     MOVE 4793-STATUS-CODE TO STATUS-WS                                   
559400     PERFORM IMS-STATUSKONTROLL                                           
559500     .                                                                    
559600     EJECT                                                                
559700 IMS-GET-KREE01-KVAL            SECTION.                                  
559800                                                                          
559900     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
560000            DELIMITED BY SIZE INTO SSA1                                   
560100     MOVE '  GE' TO GODK-STATUSKODER                                      
560200     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA-WDA201 SSA1               
560300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
560400     PERFORM IMS-STATUSKONTROLL                                           
560500     .                                                                    
560600 IMS-GET-KREE01-GHU             SECTION.                                  
560700                                                                          
560800     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
560900            DELIMITED BY SIZE INTO SSA1                                   
561000     MOVE '  GE' TO GODK-STATUSKODER                                      
561100     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA-WDA201 SSA1              
561200     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
561300     PERFORM IMS-STATUSKONTROLL                                           
561400     .                                                                    
561500 IMS-REPL-KREE01                SECTION.                                  
561600                                                                          
561700     MOVE '  ' TO GODK-STATUSKODER                                        
561800     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA-WDA201                  
561900     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
562000     PERFORM IMS-STATUSKONTROLL                                           
562100     .                                                                    
562200     EJECT                                                                
562300 IMS-GET-KREE11-KVAL            SECTION.                                  
562400                                                                          
562500     STRING 'WLKREE11(WDA211KY>=' W-WDA211KY-X ')'                        
562600            DELIMITED BY SIZE INTO SSA1                                   
562700     MOVE '  GE' TO GODK-STATUSKODER                                      
562800     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA-WDA211 SSA1              
562900     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
563000     PERFORM IMS-STATUSKONTROLL                                           
563100                                                                          
563200     .                                                                    
563300 IMS-GET-KREE11-GNP             SECTION.                                  
563400                                                                          
563500     MOVE 'WLKREE11 ' TO SSA1                                             
563600     MOVE '  GE' TO GODK-STATUSKODER                                      
563700     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA-WDA211 SSA1              
563800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
563900     PERFORM IMS-STATUSKONTROLL                                           
564000     .                                                                    
564100 IMS-GET-KREE11-GHNP-FIRST-OKV  SECTION.                                  
564200                                                                          
564300     STRING 'WLKREE11*F '                                                 
564400             DELIMITED BY SIZE INTO SSA1                                  
564500     MOVE '  GE' TO GODK-STATUSKODER                                      
564600     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA-WDA211 SSA1             
564700     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
564800     PERFORM IMS-STATUSKONTROLL                                           
564900     .                                                                    
565000 IMS-GET-KREE11-GHNP-FIRST-KVAL SECTION.                                  
565100                                                                          
565200     STRING 'WLKREE11*F(WDA211KY =' W-WDA211KY-X ')'                      
565300             DELIMITED BY SIZE INTO SSA1                                  
565400     MOVE '  GE' TO GODK-STATUSKODER                                      
565500     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA-WDA211 SSA1             
565600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
565700     PERFORM IMS-STATUSKONTROLL                                           
565800     .                                                                    
565900     EJECT                                                                
566000 IMS-GET-KREE11-GHNP            SECTION.                                  
566100                                                                          
566200     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
566300             DELIMITED BY SIZE INTO SSA1                                  
566400     MOVE '  GE' TO GODK-STATUSKODER                                      
566500     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA-WDA211 SSA1             
566600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
566700     PERFORM IMS-STATUSKONTROLL                                           
566800     .                                                                    
566900 IMS-REPL-KREE11                SECTION.                                  
567000                                                                          
567100     MOVE '  ' TO GODK-STATUSKODER                                        
567200     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA-WDA211                  
567300     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
567400     PERFORM IMS-STATUSKONTROLL                                           
567500     .                                                                    
567600 IMS-GET-KREE21-GNP             SECTION.                                  
567700                                                                          
567800     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
567900            DELIMITED BY SIZE INTO SSA1                                   
568000     STRING 'WLKREE11(WDA211KY =' W-WDA211KY-X ')'                        
568100             DELIMITED BY SIZE INTO SSA2                                  
568200     MOVE 'WLKREE21 ' TO SSA3                                             
568300     MOVE '  GE' TO GODK-STATUSKODER                                      
568400     CALL CBLTDLI USING                                                   
568500                  GNP KREE-PCB DLI-IO-AREA-WDA221 SSA1 SSA2 SSA3          
568600     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
568700     PERFORM IMS-STATUSKONTROLL                                           
568800     .                                                                    
568900     EJECT                                                                
569000 IMS-GU-WDL501                  SECTION.                                  
569100                                                                          
569200     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
569300          DELIMITED BY SIZE INTO SSA1                                     
569400     MOVE '  GE'           TO GODK-STATUSKODER                            
569500     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-WDL501 SSA1                    
569600     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
569700     PERFORM IMS-STATUSKONTROLL                                           
569800     .                                                                    
569900     EJECT                                                                
570000 IMS-GNP-WDL511                 SECTION.                                  
570100                                                                          
570200     STRING 'WDL511  (IDGMTREF =' W-IDGMTREF-X                            
570300                    '&IDKOLLI  =' W-IDKOLLI-X ')'                         
570400          DELIMITED BY SIZE INTO SSA1                                     
570500     MOVE '  GE'           TO GODK-STATUSKODER                            
570600     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL511 SSA1                   
570700     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
570800     PERFORM IMS-STATUSKONTROLL                                           
570900     .                                                                    
571000     EJECT                                                                
571100 IMS-GNP-WDL521                 SECTION.                                  
571200                                                                          
571300     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
571400          DELIMITED BY SIZE INTO SSA1                                     
571500     STRING 'WDL521  (IDARTNR  =' W-IDARTNR-L5-X ')'                      
571600          DELIMITED BY SIZE INTO SSA2                                     
571700     MOVE '  GE'           TO GODK-STATUSKODER                            
571800     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-WDL521 SSA1 SSA2              
571900     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
572000     PERFORM IMS-STATUSKONTROLL                                           
572100     .                                                                    
572200     EJECT                                                                
572300 IMS-GU-ARTC01                  SECTION.                                  
572400                                                                          
572500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
572600          DELIMITED BY SIZE INTO SSA1                                     
572700     MOVE '  GE'           TO GODK-STATUSKODER                            
572800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-WDK601 SSA1               
572900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
573000     PERFORM IMS-STATUSKONTROLL                                           
573100     .                                                                    
573200                                                                          
573300 IMS-GNP-ARTC11                 SECTION.                                  
573400                                                                          
573500     MOVE 'WLARTC11'       TO SSA1                                        
573600     MOVE '  GE'           TO GODK-STATUSKODER                            
573700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-WDK611 SSA1              
573800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
573900     PERFORM IMS-STATUSKONTROLL                                           
574000     .                                                                    
574100     EJECT                                                                
574200 IMS-GU-BENA11                  SECTION.                                  
574300                                                                          
574400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
574500            DELIMITED BY SIZE INTO SSA1                                   
574600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
574700            DELIMITED BY SIZE INTO SSA2                                   
574800     MOVE '  ' TO GODK-STATUSKODER                                        
574900     CALL CBLTDLI USING GU  BENA-PCB DLI-IO-AREA-WDD311 SSA1 SSA2         
575000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
575100     PERFORM IMS-STATUSKONTROLL                                           
575200     .                                                                    
575300     EJECT                                                                
575400 IMS-GET-WDP311                 SECTION.                                  
575500                                                                          
575600     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
575700            DELIMITED BY SIZE INTO SSA1                                   
575800     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
575900            DELIMITED BY SIZE INTO SSA2                                   
576000     MOVE '  GE' TO GODK-STATUSKODER                                      
576100     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-AREA-P311 SSA1 SSA2           
576200     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
576300     PERFORM IMS-STATUSKONTROLL                                           
576400     .                                                                    
576500     EJECT                                                                
576600 IMS-GU-410901-ROT              SECTION.                                  
576700     STRING 'WL410901(WDGXKEY  =' W-4109-X ')'                            
576800          DELIMITED BY SIZE INTO SSA1                                     
576900     MOVE '    ' TO GODK-STATUSKODER                                      
577000     CALL CBLTDLI USING GU 4109-PCB DLI-IO-AREA-4109 SSA1                 
577100     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
577200     PERFORM IMS-STATUSKONTROLL                                           
577300     .                                                                    
577400                                                                          
577500 IMS-GHNP-410911-KVAL            SECTION.                                 
577600     STRING 'WL410911(KEY4110 =>' W-WDGXKEY-MIN-X                         
577700                    '&KEY4110 =<' W-WDGXKEY-MAX-X ')'                     
577800          DELIMITED BY SIZE INTO SSA1                                     
577900     MOVE '  GE' TO GODK-STATUSKODER                                      
578000     CALL CBLTDLI USING GHNP 4109-PCB DLI-IO-AREA-4110 SSA1               
578100     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
578200     PERFORM IMS-STATUSKONTROLL                                           
578300     .                                                                    
578400                                                                          
578500 IMS-REPL-4110                   SECTION.                                 
578600                                                                          
578700     MOVE '  ' TO GODK-STATUSKODER                                        
578800     CALL CBLTDLI USING REPL 4109-PCB DLI-IO-AREA-4110                    
578900     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
579000     PERFORM IMS-STATUSKONTROLL                                           
579100     .                                                                    
579200     EJECT                                                                
579300 IMS-GNP-410911-KVAL             SECTION.                                 
579400     MOVE '*** IMS-GNP-410911-KVAL *** '                                  
579500                              TO FELTEXT-STR                              
579600                                                                          
579700     STRING 'WL410911(KEY4110 =>' W-WDGXKEY-MIN-X                         
579800                    '&KEY4110 =<' W-WDGXKEY-MAX-X ')'                     
579900          DELIMITED BY SIZE INTO SSA1                                     
580000     MOVE '  GE' TO GODK-STATUSKODER                                      
580100     CALL CBLTDLI USING                                                   
580200           GNP 4109-PCB DLI-IO-AREA-4110 SSA1                             
580300     MOVE 4109-STATUS-CODE TO STATUS-WS                                   
580400     PERFORM IMS-STATUSKONTROLL                                           
580500     .                                                                    
580600     EJECT                                                                
580700 IMS-GU-WLRETA01                 SECTION.                                 
580800                                                                          
580900     STRING 'WLRETA01(WDA3FSEQ =' W-WDA3FSEQ-X ')'                        
581000          DELIMITED BY SIZE INTO SSA1                                     
581100     MOVE '  GE' TO GODK-STATUSKODER                                      
581200     CALL CBLTDLI USING GU RETA-PCB DLI-IO-AREA-WDA301 SSA1               
581300     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
581400     PERFORM IMS-STATUSKONTROLL                                           
581500     .                                                                    
581600     EJECT                                                                
581700 IMS-GU-GMTA-WDB201              SECTION.                                 
581800                                                                          
581900     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
582000          DELIMITED BY SIZE INTO SSA1                                     
582100     MOVE '  GE'              TO GODK-STATUSKODER                         
582200                                                                          
582300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
582400     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
582500     PERFORM IMS-STATUSKONTROLL                                           
582600     .                                                                    
582700     EJECT                                                                
582800 IMS-GET-WDB201 SECTION.                                                  
582900                                                                          
583000     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
583100                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
583200          DELIMITED BY SIZE INTO SSA1                                     
583300     MOVE '    '              TO GODK-STATUSKODER                         
583400                                                                          
583500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
583600     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
583700     PERFORM IMS-STATUSKONTROLL                                           
583800     .                                                                    
583900     EJECT                                                                
584000 IMS-GU-WDB1-WDB101              SECTION.                                 
584100                                                                          
584200     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
584300          DELIMITED BY SIZE INTO SSA1                                     
584400     MOVE '  GE'              TO GODK-STATUSKODER                         
584500                                                                          
584600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
584700     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
584800     PERFORM IMS-STATUSKONTROLL                                           
584900     .                                                                    
585000     EJECT                                                                
585100 IMS-GU-WDGX6327 SECTION.                                                 
585200                                                                          
585300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6327-X ')'                    
585400          DELIMITED BY SIZE INTO SSA1                                     
585500     MOVE '  GE' TO GODK-STATUSKODER                                      
585600     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6327 SSA1                  
585700     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
585800     PERFORM IMS-STATUSKONTROLL                                           
585900     .                                                                    
586000     EJECT                                                                
586100 IMS-GNP-WDGX6328 SECTION.                                                
586200                                                                          
586300     STRING 'WDGX6328(KY6328  >=' W-KY6328-MIN-X                          
586400                    '&KY6328  <=' W-KY6328-MAX-X                          
586500                    '&IDUSERGK =' W-IDUSER-6328-X ')'                     
586600          DELIMITED BY SIZE INTO SSA1                                     
586700     MOVE '  GE' TO GODK-STATUSKODER                                      
586800     CALL CBLTDLI USING GNP 6327-PCB DLI-IO-WDGX6328 SSA1                 
586900     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
587000     PERFORM IMS-STATUSKONTROLL                                           
587100     .                                                                    
587200     EJECT                                                                
587300 IMS-GU-WDGX4103 SECTION.                                                 
587400                                                                          
587500     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
587600          DELIMITED BY SIZE INTO SSA1                                     
587700     MOVE '  GE' TO GODK-STATUSKODER                                      
587800     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4103 SSA1                  
587900     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
588000     PERFORM IMS-STATUSKONTROLL                                           
588100     .                                                                    
588200     EJECT                                                                
588300 IMS-GNP-WDGX4104-KVAL SECTION.                                           
588400                                                                          
588500     STRING 'WDGX4104*F(KEY4104  =' W-WDGXKEY-4104-X ')'                  
588600          DELIMITED BY SIZE INTO SSA1                                     
588700     MOVE '  GE' TO GODK-STATUSKODER                                      
588800     CALL CBLTDLI USING GNP 4103-PCB DLI-IO-WDGX4104 SSA1                 
588900     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
589000     PERFORM IMS-STATUSKONTROLL                                           
589100     .                                                                    
589200     EJECT                                                                
589300 IMS-GHNP-WDGX4104 SECTION.                                               
589400                                                                          
589500     MOVE 'WDGX4104 ' TO SSA1                                             
589600     MOVE '  GE' TO GODK-STATUSKODER                                      
589700     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
589800     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
589900     PERFORM IMS-STATUSKONTROLL                                           
590000     .                                                                    
590100     EJECT                                                                
590200 IMS-DLET-WDGX4104 SECTION.                                               
590300                                                                          
590400     MOVE '  ' TO GODK-STATUSKODER                                        
590500     CALL CBLTDLI USING DLET 4103-PCB DLI-IO-WDGX4104                     
590600     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
590700     PERFORM IMS-STATUSKONTROLL                                           
590800     .                                                                    
590900     EJECT                                                                
591000 IMS-GHNP-WDGX4104-KVAL SECTION.                                          
591100                                                                          
591200     STRING 'WDGX4104*F(KEY4104  =' W-WDGXKEY-4104-X ')'                  
591300          DELIMITED BY SIZE INTO SSA1                                     
591400     MOVE '  GE' TO GODK-STATUSKODER                                      
591500     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
591600     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
591700     PERFORM IMS-STATUSKONTROLL                                           
591800     .                                                                    
591900     EJECT                                                                
592000 IMS-REPL-WDGX4104 SECTION.                                               
592100                                                                          
592200     MOVE '  ' TO GODK-STATUSKODER                                        
592300     CALL CBLTDLI USING REPL 4103-PCB DLI-IO-WDGX4104                     
592400     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
592500     PERFORM IMS-STATUSKONTROLL                                           
592600     .                                                                    
592700     EJECT                                                                
592800 IMS-ISRT-WDGX4103 SECTION.                                               
592900     MOVE 'WDR501  ' TO SSA1                                              
593000     MOVE '    ' TO GODK-STATUSKODER                                      
593100     CALL CBLTDLI USING ISRT 4103-PCB DLI-IO-WDGX4103 SSA1                
593200     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
593300     PERFORM IMS-STATUSKONTROLL                                           
593400     .                                                                    
593500     EJECT                                                                
593600 IMS-ISRT-WDGX4104 SECTION.                                               
593700                                                                          
593800     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
593900          DELIMITED BY SIZE INTO SSA1                                     
594000     MOVE 'WDGX4104 ' TO SSA2                                             
594100     MOVE '    ' TO GODK-STATUSKODER                                      
594200     CALL CBLTDLI USING ISRT 4103-PCB DLI-IO-WDGX4104 SSA1 SSA2           
594300     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
594400     PERFORM IMS-STATUSKONTROLL                                           
594500     .                                                                    
594600     EJECT                                                                
594700 IMS-GU-WDB601    SECTION.                                                
594800                                                                          
594900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
595000          DELIMITED BY SIZE INTO SSA1                                     
595100     MOVE '  ' TO GODK-STATUSKODER                                        
595200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
595300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
595400     PERFORM IMS-STATUSKONTROLL                                           
595500     .                                                                    
595600     EJECT                                                                
595700 IMS-GNP-WDB611-FIRST    SECTION.                                         
595800                                                                          
595900     STRING 'WDB611  *F(WDB611KY =' W-WDB611KY-X ')'                      
596000          DELIMITED BY SIZE INTO SSA1                                     
596100     MOVE '  GE' TO GODK-STATUSKODER                                      
596200     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB611 SSA1                   
596300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
596400     PERFORM IMS-STATUSKONTROLL                                           
596500     .                                                                    
596600     EJECT                                                                
596700 IMS-GU-WDK711    SECTION.                                                
596800                                                                          
596900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
597000     DELIMITED BY SIZE INTO SSA1                                          
597100     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
597200     DELIMITED BY SIZE INTO SSA2                                          
597300     MOVE '  GE' TO GODK-STATUSKODER                                      
597400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
597500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
597600     PERFORM IMS-STATUSKONTROLL                                           
597700     .                                                                    
597800     EJECT                                                                
597900 IMS-GU-WDK712    SECTION.                                                
598000                                                                          
598100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
598200     DELIMITED BY SIZE INTO SSA1                                          
598300     STRING 'WDK712  (IDLAND   =' W-IDLAND-K7-X ')'                       
598400     DELIMITED BY SIZE INTO SSA2                                          
598500     MOVE '  GE' TO GODK-STATUSKODER                                      
598600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
598700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
598800     PERFORM IMS-STATUSKONTROLL                                           
598900     .                                                                    
599000     EJECT                                                                
599100 IMS-GU-WDK722    SECTION.                                                
599200                                                                          
599300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
599400     DELIMITED BY SIZE INTO SSA1                                          
599500     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
599600     DELIMITED BY SIZE INTO SSA2                                          
599700     STRING 'WDK722  (KDSEGKEY =1)'                                       
599800     DELIMITED BY SIZE INTO SSA3                                          
599900     MOVE '  GE' TO GODK-STATUSKODER                                      
600000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
600100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
600200     PERFORM IMS-STATUSKONTROLL                                           
600300     .                                                                    
600400     EJECT                                                                
600500 IMS-STATUSKONTROLL              SECTION.                                 
600600                                                                          
600700     SET STATUS-IX TO 1                                                   
600800     SEARCH GODK-STATUS AT END CALL FELLOG                                
600900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
601000     END-SEARCH                                                           
602000     .                                                                    
610000     EJECT                                                                
