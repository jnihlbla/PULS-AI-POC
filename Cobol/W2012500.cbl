000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2012500.                                                
000400 AUTHOR.         J-O.                                                     
000500 DATE-WRITTEN.   DECEMBER 1988.                                           
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*     BESKRIVNING                                                         
001200*                                                                         
001300*     DETTA PROGRAM HANTERAR BILD 2125 RESTORDER/TPO.                     
001400*     ENDAST TPO1 TPO2 OCH TPO6 FÅR UPPDATERAS I PROGRAMMET.              
001500*     TPO1:   ANNULLERA RAD, ÄNDRA DATUM SAMT ÄNDRA TOTALA ANTALET        
001600*             BÅDE UPP OCH NER                                            
001700*     TPO2,6: ÄNDRA DATUM SAMT SPLIT AV RAD, MEN EJ ÄNDRA TOTALA          
001800*             ANTALET                                                     
001900*     PROGRAMMET TILLÅTER LÄSNING AV RO OCH SAMTLIGA TPO:ER.              
002000*     I PROGRAMMET ANVÄNDS FÖLJANDE BASER:                                
002100*                                                                         
002200*     RESTORDERREG. (WDA5)                                                
002300*     LOGGREG.      (WDG6)                                                
002400*     ARTREG.       (WDK6 WDK9)                                           
002500*     KUNDREG.      (WDB2)                                                
002600*     ORDERBEKR     (WDQ1 WDQ2)                                           
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W2T125                                              
003000*        MID:         W2I12501                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         W2O12501                                            
003400*                                                                         
003500*   ÄNDRINGAR:                                                            
003600*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
003700*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
003800*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
003900*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
004004*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
004005*    E-TRACKER: 10254592 2015 DECOMISSION VOHF                            
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     SKIP3                                                                
004400 DATA DIVISION.                                                           
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP3                                                                
004800*    -COPY WY2000W1                                                       
004900     SKIP3                                                                
005000 77  PROGRAM-NAMN              PIC X(8)     VALUE 'W2012500'.             
005100 77  FELTEXT                   PIC X(80)    VALUE SPACE.                  
005200 77  JA                        PIC X(1)     VALUE 'J'.                    
005300 77  NEJ                       PIC X(1)     VALUE 'N'.                    
005400 77  W-KVRAD                   PIC 9(6)     VALUE ZERO.                   
005500 77  W-ANTAL-FORP              PIC 9(6)     VALUE ZERO.                   
005600 77  W-ANTAL-KVART             PIC 9(7)     VALUE ZERO.                   
005700 77  W-VALKOD                  PIC X(1)     VALUE ' '.                    
005800 77  ANTAL-OK                  PIC X(1)     VALUE 'J'.                    
005900 77  KVQPACK-OK                PIC X(1)     VALUE 'J'.                    
006000 77  TITPO-OK                  PIC X(1)     VALUE 'J'.                    
006100 77  INPUT-OK                  PIC X(1)     VALUE 'J'.                    
006200 77  VALKOD-OK                 PIC X(1)     VALUE ' '.                    
006300 77  VALKOD-DELETE-OK          PIC X(1)     VALUE 'J'.                    
006400 77  SATS-DELETE-OK            PIC X(1)     VALUE 'J'.                    
006500 77  OPP-FAELT-OK              PIC X(1)     VALUE 'J'.                    
006600 77  W-UPDATE                  PIC X(1)     VALUE 'N'.                    
006700 77  INDATA-SW                 PIC X.                                     
006800     88  INDATA-OK                          VALUE 'J'.                    
006900     88  INDATA-FEL                         VALUE 'N'.                    
007000 77  WS-IDTRANS                PIC X(04).                                 
007100     88  WS-EGEN-BILD          VALUE '2125'.                              
007200                                                                          
007300 77  W-ORDQ01-NYCKLAR          PIC X(1)     VALUE SPACE.                  
007400 77  W-OWDS01-NYCKLAR          PIC X(1)     VALUE SPACE.                  
007500                                                                          
007600 77  IDARTNR-WS                PIC X(9)     VALUE SPACE.                  
007700 77  IDDISTR-WS                PIC X(4)     VALUE SPACE.                  
007800 77  IDKUNDNR-WS               PIC X(6)     VALUE SPACE.                  
007900 77  IDANSK-WS                 PIC X(3)     VALUE SPACE.                  
008000 77  KDROO-WS                  PIC X(1)     VALUE SPACE.                  
008100 77  IDORDNR-WS                PIC X(5)     VALUE SPACE.                  
008200 77  KDTPOTYP-FOM-WS           PIC X(1)     VALUE SPACE.                  
008300 77  KDTPOTYP-TOM-WS           PIC X(1)     VALUE SPACE.                  
008400 77  KDSTARAD-WS               PIC X(1)     VALUE SPACE.                  
008500 77  IDLOPNR-WS                PIC X(2)     VALUE SPACE.                  
008600                                                                          
008700 77  BRUTEN-KVANT-SW           PIC X        VALUE 'N'.                    
008800     88  BRUTEN-KVANT                       VALUE 'J'.                    
008900 77  DELANNULL-SW              PIC X        VALUE 'N'.                    
009000     88  DELANNULLATION                     VALUE 'J'.                    
009100                                                                          
009200 77  INDX                      PIC S9(9)    VALUE ZERO  COMP SYNC.        
009300 77  SPRAK-IX                  PIC S9(9)    VALUE ZERO  COMP SYNC.        
009400 77  MAX-LINE                  PIC S9(9)    VALUE +13   COMP SYNC.        
009500 77  MAX-MOD-LAENGD            PIC S9(4)    VALUE +1112 COMP SYNC.        
009600*                                                                         
009700 77  W-IDARTNR                 PIC 9(9)      VALUE ZERO.                  
009800 77  W-IDKUNDNR                PIC 9(7)      VALUE ZERO.                  
009900 77  W-IDDISTR                 PIC 9(4)      VALUE ZERO.                  
010000 77  W-IDANSK                  PIC 9(3)      VALUE ZERO.                  
010100 77  W-KDROO                   PIC 9(1)      VALUE ZERO.                  
010200 77  W-KDTPOTYP-FOM            PIC 9(1)      VALUE ZERO.                  
010300 77  W-KDTPOTYP-TOM            PIC 9(1)      VALUE ZERO.                  
010400 77  W-KDSTARAD                PIC 9(1)      VALUE ZERO.                  
010500*                                                                         
010600 77  W-SPAR-KVART              PIC 9(7)     VALUE ZERO.                   
010700 77  W-SPAR-TITPO              PIC S9(7)    VALUE +0 COMP-3.              
010800 77  W-SPAR-IDLOPNR            PIC S9(3)    VALUE +0 COMP-3.              
010900                                                                          
011000 77  SEKEL                     PIC 9(2).                                  
011100 77  SEKEL-TAL                 PIC S9(9) COMP-3.                          
011200                                                                          
011300*    ----  ARBETSFÄLT                                                     
011400 01  ARBETSFAELT.                                                         
011500     03  RKOD-ABEND            PIC S9(4) VALUE +33 COMP SYNC.             
011600     03  WS-TIAAAAVV-KONV.                                                
011700       05  WS-TISEKEL          PIC 9(2) VALUE ZERO.                       
011800       05  WS-TIAA             PIC 9(2) VALUE ZERO.                       
011900       05  WS-TIVV             PIC 9(2) VALUE ZERO.                       
012000     03  WS-TIAAAAVV           PIC 9(6) VALUE ZERO.                       
012100     03  WS-TIAAVVD-MID.                                                  
012200       05  WS-TIAAVV-MID       PIC 9(4) VALUE ZERO.                       
012300       05  FILLER              PIC 9(1).                                  
012400     03  WS-TIAAMMDD           PIC 9(6) VALUE ZERO.                       
012500     03  WS-FLNC               PIC X(1).                                  
012600     03  WS-MINSKNING          PIC S9(7) VALUE ZERO.                      
012700     03  WS-IDKUNDRF.                                                     
012800       05  WS-IDKUNDRF-1-5     PIC 9(5).                                  
012900       05  FILLER              PIC X(5)  VALUE SPACE.                     
013000     03  WS-IDKUNDRF-IHOP      PIC X(10) VALUE SPACE.                     
013100     03  WS-IDORDER            PIC S9(7) VALUE ZERO COMP-3.               
013200     03  WS-IDKUNDRF-ORQI      PIC X(10) VALUE SPACE.                     
013300     03  WS-IDLEVNR-8          PIC X(8)  VALUE SPACE.                     
013400     03  WS-TIDISPIN           PIC S9(7) VALUE ZERO COMP-3.               
013500     03  WS-KDORDBEK           PIC 9(2)  VALUE ZERO.                      
013600     03  WS-TIRES              PIC 9(6)  VALUE ZERO.                      
013700     03  ART-S-KDSORT          PIC X(2)  VALUE SPACE.                     
013800     03  DATUM-MED-ARHUNDR     PIC S9(9) VALUE ZERO COMP-3.               
013900     03  DAGENS-DATUM          PIC S9(6) VALUE ZERO.                      
014000     SKIP3                                                                
014100 01  MEDDELANDEN.                                                         
014200     03  FEL-1                 PIC X(19) VALUE                            
014300         'BORTTAG EJ TILLÅTET'.                                           
014400     03  FEL-2                 PIC X(23) VALUE                            
014500         'UPPDATERING EJ TILLÅTEN'.                                       
014600     03  FEL-3                 PIC X(16) VALUE                            
014700         'RAD EJ BEKRÄFTAD'.                                              
014800     03  FEL-4                 PIC X(15) VALUE                            
014900         'BIPACKNINGSKLAR'.                                               
015000     03  FEL-5                 PIC X(28) VALUE                            
015100         'ANGE KOD "D" VID ANNULLERING'.                                  
015200   03    FEL-6                 PIC X(40) VALUE                            
015300         'OBEHÖRIG ANVÄNDARE '.                                           
015400   03    FEL-7                 PIC X(40) VALUE                            
015500         'ARTIKEL SAKNAS     '.                                           
015600     EJECT                                                                
015700*    ----  PARAMETRAR TILL IDDISTR                                        
015800                                                                          
015900 01  W-TEST-IDDIST.                                                       
016000     03  W-TEST-IDDISTR        PIC 9(4).                                  
016100                                                                          
016200 01  TEST-IDDISTR              PIC 9(5) COMP-3.                           
016300                                                                          
016400*01  FILLER -COPY WWDIST19 -RED TEST-IDDISTR.                             
016500     EJECT                                                                
016600*      --- VALID IDDC CODES                                               
016700*                                                                         
016800*01    -COPY WWDCKONS                                                     
016900       EJECT                                                              
017000*    ----  SUBPROGRAM OCH PARAMETER-AREOR.                                
017100                                                                          
017200 01  DYNAMISK-SUBPROGRAM.                                                 
017300     03  WDATKONV              PIC X(8) VALUE 'WDATKONV'.                 
017400     03  WSECURIT              PIC X(8) VALUE 'WSECURIT'.                 
017500     03  CBLTDLI               PIC X(8) VALUE 'CBLTDLI '.                 
017600     03  FELLOG                PIC X(8) VALUE 'FELLOG  '.                 
017700     03  ABEND                 PIC X(8) VALUE 'ABEND   '.                 
017800     03  W005INIT              PIC X(8) VALUE 'W005INIT'.                 
017900     EJECT                                                                
018000*  03  FILLER -COPY WSECAREA.                                             
018100     EJECT                                                                
018200*    ----  PARAMETRAR TILL DATUMKORT.                                     
018300                                                                          
018400 01  DATUMKORT-ID              PIC X(6) VALUE 'WDATUM'.                   
018500     SKIP3                                                                
018600*    -COPY WDATAREAC0                                                     
018700     EJECT                                                                
018800*    ----  PARAMETRAR TILL W005INIT                                       
018900                                                                          
019000*    -COPY WMSGINIT                                                       
019100     EJECT                                                                
019200*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
019300                                                                          
019400 01  FILLER                   PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.         
019500 01  NYCKLAR-TILL-DLI.                                                    
019600     03  W-SOK-NYCKLAR-MIN.                                               
019700         05  W-IDARTNR-MIN-X.                                             
019800             07  W-IDARTNR-MIN    PIC S9(9)  COMP-3 VALUE ZERO.           
019900         05  W-IDDISTR-MIN-X.                                             
020000             07  W-IDDISTR-MIN    PIC S9(5)  COMP-3 VALUE ZERO.           
020100         05  W-IDKUNDNR-MIN-X.                                            
020200             07  W-IDKUNDNR-MIN   PIC S9(7)  COMP-3 VALUE ZERO.           
020300         05  W-IDKUNDRF-MIN.                                              
020400             07  W-IDORDNR-MIN    PIC 9(5)           VALUE ZERO.          
020500             07  FILLER           PIC X(5)           VALUE SPACE.         
020600         05  W-IDANSK-MIN-X.                                              
020700             07  W-IDANSK-MIN     PIC S9(3)  COMP-3 VALUE ZERO.           
020800         05  W-KDROO-MIN-X.                                               
020900             07  W-KDROO-MIN      PIC S9(1)  COMP-3 VALUE ZERO.           
021000         05  W-KDTPOTYP-MIN-X.                                            
021100             07  W-KDTPOTYP-MIN   PIC S9(1)  COMP-3 VALUE ZERO.           
021200         05  W-KDSTARAD-MIN       PIC  X(1)         VALUE SPACE.          
021300         05  W-IDLOPNR-MIN-X.                                             
021400             07  W-IDLOPNR-MIN    PIC S9(3)  COMP-3 VALUE ZERO.           
021500                                                                          
021600     03  W-SOK-NYCKLAR-MAX.                                               
021700         05  W-IDARTNR-MAX-X.                                             
021800             07  W-IDARTNR-MAX    PIC S9(9)  COMP-3 VALUE ZERO.           
021900         05  W-IDDISTR-MAX-X.                                             
022000             07  W-IDDISTR-MAX    PIC S9(5)  COMP-3 VALUE ZERO.           
022100         05  W-IDKUNDNR-MAX-X.                                            
022200             07  W-IDKUNDNR-MAX   PIC S9(7)  COMP-3 VALUE ZERO.           
022300         05  W-IDKUNDRF-MAX.                                              
022400             07  W-IDORDNR-MAX    PIC 9(5)           VALUE ZERO.          
022500             07  FILLER           PIC X(5)           VALUE SPACE.         
022600         05  W-IDANSK-MAX-X.                                              
022700             07  W-IDANSK-MAX     PIC S9(3)  COMP-3 VALUE ZERO.           
022800         05  W-KDROO-MAX-X.                                               
022900             07  W-KDROO-MAX      PIC S9(1)  COMP-3 VALUE ZERO.           
023000         05  W-KDTPOTYP-MAX-X.                                            
023100             07  W-KDTPOTYP-MAX   PIC S9(1)  COMP-3 VALUE ZERO.           
023200         05  W-KDSTARAD-MAX       PIC  X(1)         VALUE SPACE.          
023300         05  W-IDLOPNR-MAX-X.                                             
023400             07  W-IDLOPNR-MAX    PIC S9(3)  COMP-3 VALUE ZERO.           
023500     EJECT                                                                
023600     03  W-WDA5C1KY-MIN.                                                  
023700         05  W-IDANSK-N1-MIN      PIC S9(3) COMP-3   VALUE ZERO.          
023800         05  W-KDROO-N1-MIN       PIC S9(1) COMP-3   VALUE ZERO.          
023900         05  W-IDARTNR-N1-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
024000         05  W-IDDISTR-N1-MIN     PIC S9(5) COMP-3   VALUE ZERO.          
024100         05  W-IDKUNDNR-N1-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
024200         05  W-IDKUNDRF-N1-MIN.                                           
024300             07  W-IDORDNR-N1-MIN PIC 9(5)           VALUE ZERO.          
024400             07  FILLER           PIC X(5)           VALUE SPACE.         
024500         05  W-IDLOPNR-N1-MIN     PIC S9(3) COMP-3   VALUE ZERO.          
024600                                                                          
024700     03  W-WDA5C1KY-MAX.                                                  
024800         05  W-IDANSK-N1-MAX      PIC S9(3) COMP-3   VALUE ZERO.          
024900         05  W-KDROO-N1-MAX       PIC S9(1) COMP-3   VALUE ZERO.          
025000         05  W-IDARTNR-N1-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
025100         05  W-IDDISTR-N1-MAX     PIC S9(5) COMP-3   VALUE ZERO.          
025200         05  W-IDKUNDNR-N1-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
025300         05  W-IDKUNDRF-N1-MAX.                                           
025400             07  W-IDORDNR-N1-MAX PIC 9(5)           VALUE ZERO.          
025500             07  FILLER           PIC X(5)           VALUE SPACE.         
025600         05  W-IDLOPNR-N1-MAX     PIC S9(3) COMP-3   VALUE ZERO.          
025700                                                                          
025800     03  W-WDA5A1KY-MIN.                                                  
025900         05  W-IDARTNR-N3-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
026000         05  W-IDDC-N3-MIN        PIC X(2)           VALUE '11'.          
026100         05  W-KDRAPRIO-N3-MIN    PIC S9(3) COMP-3   VALUE ZERO.          
026200         05  W-DARODAT-N3-MIN     PIC  9(8)          VALUE ZERO.          
026210         05  W-TIREGTID-N3-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
026300         05  W-IDDISTR-N3-MIN     PIC S9(5) COMP-3   VALUE ZERO.          
026400         05  W-IDKUNDNR-N3-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
026500         05  W-IDKUNDRF-N3-MIN.                                           
026600             07  W-IDORDNR-N3-MIN PIC 9(5)           VALUE ZERO.          
026700             07  FILLER           PIC X(5)           VALUE SPACE.         
026800         05  W-IDLOPNR-N3-MIN     PIC S9(3) COMP-3   VALUE ZERO.          
026900                                                                          
027000     03  W-WDA5A1KY-MAX.                                                  
027100         05  W-IDARTNR-N3-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
027200         05  W-IDDC-N3-MAX        PIC X(2)           VALUE '11'.          
027300         05  W-KDRAPRIO-N3-MAX    PIC S9(3) COMP-3   VALUE ZERO.          
027400         05  W-DARODAT-N3-MAX     PIC  9(8)          VALUE ZERO.          
027410         05  W-TIREGTID-N3-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
027500         05  W-IDDISTR-N3-MAX     PIC S9(5) COMP-3   VALUE ZERO.          
027600         05  W-IDKUNDNR-N3-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
027700         05  W-IDKUNDRF-N3-MAX.                                           
027800             07  W-IDORDNR-N3-MAX PIC 9(5)           VALUE ZERO.          
027900             07  FILLER           PIC X(5)           VALUE SPACE.         
028000         05  W-IDLOPNR-N3-MAX     PIC S9(3) COMP-3   VALUE ZERO.          
028100                                                                          
028200     03  W-WDA501KY.                                                      
028300         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
028400         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
028500         05  W-IDKUNDRF-N2.                                               
028600             07  W-IDORDNR-N2     PIC 9(5)           VALUE ZERO.          
028700             07  FILLER           PIC X(5)           VALUE SPACE.         
028800         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
028900         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
029000                                                                          
029100     03  W-WDK601KY.                                                      
029200         05  W-IDARTNR-N3         PIC S9(9) COMP-3   VALUE ZERO.          
029300                                                                          
029400     03  W-IDGMT-X.                                                       
029500         05  W-IDDISTR-N5         PIC S9(5)  COMP-3  VALUE ZERO.          
029600         05  W-IDKUNDNR-N5        PIC S9(7)  COMP-3  VALUE ZERO.          
029700                                                                          
029800     03  W-DABEHOV-X.                                                     
029900         05  W-DABEHOV            PIC  9(6)          VALUE ZERO.          
030000                                                                          
030100     03  W-IDGMTREF-X.                                                    
030200         05  W-IDDISTR-XX.                                                
030300             07 W-IDDISTR-R       PIC S9(5)  COMP-3  VALUE ZERO.          
030400         05  W-IDKUNDNR-XX.                                               
030500             07 W-IDKUNDNR-R      PIC S9(7)  COMP-3  VALUE ZERO.          
030600         05  W-IDKUNDRF-XX.                                               
030700             07 W-IDKUNDRF-R      PIC X(10)  VALUE SPACE.                 
030800     EJECT                                                                
030900*01    -COPY WWTEXT01                                                     
031000     EJECT                                                                
031100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
031200                                                                          
031300 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
031400     SKIP3                                                                
031500*01    MID -COPY W2I12501.                                                
031600     EJECT                                                                
031700*01    -COPY WMSGAREA                                                     
031800     EJECT                                                                
031900*  03    MOD -COPY W2O12501  -RED MSG-AREA.                               
032000     EJECT                                                                
032100*01    -COPY WMFSAREA                                                     
032200     EJECT                                                                
032300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032400                                                                          
032500 01    IMS-WS.                                                            
032600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
032700     SKIP3                                                                
032800   03    STATUS-WS               PIC XX.                                  
032900     88    SEGMENT-FINNS                    VALUE '  '.                   
033000     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
033100     88    SEGMENT-SLUT                     VALUE 'GB'.                   
033200     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
033300     SKIP3                                                                
033400   03    GODK-STATUSKODER.                                                
033500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
033600     SKIP3                                                                
033700 01    SSA1                      PIC X(320).                              
033800 01    SSA2                      PIC X(320).                              
033900 01    SSA3                      PIC X(320).                              
034000     EJECT                                                                
034100*                            IMS FUNKTIONSKODER                           
034200*01    -COPY W0003                                                        
034300     EJECT                                                                
034400 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA1'.        
034500 01    DLI-IO-AREA1.                                                      
034600*  03    WDA501 -COPY WDA501                                              
034700     EJECT                                                                
034800 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA2'.        
034900 01    DLI-IO-AREA2.                                                      
035000*  03    WDA5C1 -COPY WDA5C1                                              
035100     EJECT                                                                
035200 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA3'.        
035300 01    DLI-IO-AREA3.                                                      
035400*  03    WDA5A1 -COPY WDA5A1                                              
035500     EJECT                                                                
035600 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA4'.        
035700 01    DLI-IO-AREA4-01.                                                   
035800*  03    WDK601 -COPY WDK601                                              
035900     EJECT                                                                
036000 01    DLI-IO-AREA4-11.                                                   
036100*  03    WDK611 -COPY WDK611                                              
036200     EJECT                                                                
036300 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA5'.        
036400 01    DLI-IO-AREA5.                                                      
036500*  03    WDGZ01 -COPY WDGZ01 -PRE LOGG-                                   
036600     EJECT                                                                
036700*  03    WDGZRY9S -COPY WDGZRY9S                                          
036800     EJECT                                                                
036900*  03    WDGZRY -COPY WDGZRY9                                             
037000     EJECT                                                                
037100 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA6'.        
037200 01    DLI-IO-AREA6.                                                      
037300   03    IO-AREA6                PIC X(700)  VALUE SPACE.                 
037400     SKIP3                                                                
037500*  03    WDK901 -COPY WDK901 -PRE ARTM-   -RED IO-AREA6.                  
037600     EJECT                                                                
037700*  03    WDK911 -COPY WDK911 -PRE ARTM-   -RED IO-AREA6.                  
037800     EJECT                                                                
037900*  03    WDQ101 -COPY WDQ101 -PRE ORQM-   -RED IO-AREA6.                  
038000     EJECT                                                                
038100*  03    WDQ201 -COPY WDQ201 -PRE ORQI-   -RED IO-AREA6.                  
038200     EJECT                                                                
038300 01    FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA7'.        
038400 01    DLI-IO-AREA7.                                                      
038500*  03    WDB201 -COPY WDB201 -PRE GMTA-                                   
038600     EJECT                                                                
038700 LINKAGE SECTION.                                                         
038800*01    -COPY W0009     -PRE MSG-                                          
038900     EJECT                                                                
039000*01    -COPY W0008     -PRE USEA-                                         
039100     05  FILLER                  PIC X.                                   
039200     EJECT                                                                
039300*01    -COPY W0008     -PRE ORDP-                                         
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600*01    -COPY W0008     -PRE ORDS-                                         
039700     05  FILLER                  PIC X.                                   
039800     EJECT                                                                
039900*01    -COPY W0008     -PRE ORDQ-                                         
040000     05  FILLER                  PIC X.                                   
040100     EJECT                                                                
040200*01    -COPY W0008     -PRE LOGG-                                         
040300     05  FILLER                  PIC X.                                   
040400     EJECT                                                                
040500*01    -COPY W0008     -PRE ARTC-                                         
040600     05  FILLER                  PIC X.                                   
040700     EJECT                                                                
040800*01    -COPY W0008     -PRE ARTM-                                         
040900     05  FILLER                  PIC X.                                   
041000     EJECT                                                                
041100*01    -COPY W0008     -PRE GMTA-                                         
041200     05  FILLER                  PIC X.                                   
041300     EJECT                                                                
041400*01    -COPY W0008     -PRE ORQM-                                         
041500     05  FILLER                  PIC X.                                   
041600     EJECT                                                                
041700*01    -COPY W0008     -PRE ORQI-                                         
041800     05  FILLER                  PIC X.                                   
041900     EJECT                                                                
042000 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
042100                                  ORDP-PCB ORDS-PCB ORDQ-PCB              
042200                  LOGG-PCB ARTC-PCB ARTM-PCB GMTA-PCB                     
042300                  ORQM-PCB ORQI-PCB.                                      
042400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
042500                                   ORDP-PCB ORDS-PCB ORDQ-PCB             
042600                  LOGG-PCB ARTC-PCB ARTM-PCB GMTA-PCB                     
042700                  ORQM-PCB ORQI-PCB.                                      
042800                                                                          
042900     PERFORM IMS-GU-MSG                                                   
043000     IF SEGMENT-FINNS                                                     
043100        PERFORM A-INIT                                                    
043200        IF INDATA-OK                                                      
043300           PERFORM B-KOLLA-NYCKLAR                                        
043400           IF INDATA-OK                                                   
043500              PERFORM C-KOLLA-VALKOD                                      
043600              IF  VALKOD-OK        = JA OR ' '                            
043700              AND VALKOD-DELETE-OK = JA                                   
043800                 MOVE +1 TO INDX                                          
043900                 IF MFS-IDPFK = 7                                         
044000                    PERFORM S01-LAES-WDA5-PF7                             
044100                 ELSE                                                     
044200                    IF MFS-IDPFK = 8                                      
044300                       PERFORM D-LAES-WDA5-PF8                            
044400                    ELSE                                                  
044500                       IF MFS-UPDATE                                      
044600                          IF OPP-FAELT-OK = JA                            
044700                             PERFORM UNTIL INDX > MID-KVRAD-SPAR          
044800                               PERFORM S40-LOGGTID                        
044900                               IF MID-VALKOD(INDX) = ' ' AND              
045000                                  MID-KVRAD-SPAR   = +1                   
045100                                  PERFORM E-UPPDATERA-RAD                 
045200                               ELSE                                       
045300                                  IF MID-VALKOD(INDX) = 'D'               
045400                                     PERFORM F-DELETE-RAD                 
045500                                  END-IF                                  
045600                               END-IF                                     
045700                               ADD +1 TO INDX                             
045800                             END-PERFORM                                  
045900                                                                          
046000                             PERFORM S02-LAES-WDA5-ENTER                  
046100                             IF W-UPDATE = JA                             
046200                                 MOVE TEXT-0404 (SPRAK-IX) TO             
046300                                                  MOD-TEMFSINF            
046400                             END-IF                                       
046500                          ELSE                                            
046600                             PERFORM H-FELTEXT                            
046700                          END-IF                                          
046800                       ELSE                                               
046900                          IF VALKOD-OK = JA                               
047000                            PERFORM UNTIL INDX > MID-KVRAD-SPAR           
047100                               IF MID-VALKOD(INDX) = 'S'                  
047200                                  PERFORM G-OPPNA-RAD                     
047300                               END-IF                                     
047400                               ADD +1 TO INDX                             
047500                            END-PERFORM                                   
047600                            MOVE +1 TO INDX                               
047700                          ELSE                                            
047800                            PERFORM S02-LAES-WDA5-ENTER                   
047900                          END-IF                                          
048000                       END-IF                                             
048100                    END-IF                                                
048200                 END-IF                                                   
048300              END-IF                                                      
048400           END-IF                                                         
048500        END-IF                                                            
048600        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
048700        PERFORM IMS-ISRT-MSG                                              
048800     END-IF                                                               
048900*****CALL FELLOG                                                          
049000                                                                          
049100     MOVE ZERO TO RETURN-CODE                                             
049200     GOBACK.                                                              
049300     EJECT                                                                
049400 A-INIT SECTION.                                                          
049500                                                                          
049600     IF MSG-DUBBLA-TRANSKODER                                             
049700         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I12501               
049800         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
049900         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
050000         MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                
050100         MOVE MSG-IDPFK                     TO MFS-IDPFK                  
050200     ELSE                                                                 
050300         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I12501               
050400         MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                
050500         MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR               
050600         MOVE SPACE                         TO MFS-KDTRTYP                
050700                                               MFS-IDPFK                  
050800     END-IF                                                               
050900     MOVE MFS-IDTRANS     TO WS-IDTRANS                                   
051000                                                                          
051100     MOVE LOW-VALUE       TO MSG-AREA                                     
051200     MOVE 'W2O12501'      TO MFS-IDMOD                                    
051300     MOVE '2125'          TO MOD-IDTRANS                                  
051400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
051500                             MOD-TEMFSINF                                 
051600                             MOD-KVART-OPP                                
051700                             MOD-TITPO-OPP                                
051800                                                                          
051900     PERFORM MFS-RENSA-FALT-IN                                            
052000     PERFORM MFS-RENSA-FALT-UT                                            
052100                                                                          
052200     IF MFS-KDMFSFOR = '1'                                                
052300        MOVE +1 TO SPRAK-IX                                               
052400     ELSE                                                                 
052500        MOVE +2 TO SPRAK-IX                                               
052600     END-IF                                                               
052700                                                                          
052800                                                                          
052900***  KOMMENTARSMÄRKT TILLS VIDARE 920903                                  
053000*    PERFORM S30-SECURIT                                                  
053100*    IF SEC-KDSVAR = ' '                                                  
053200        MOVE JA  TO INDATA-SW                                             
053300*    ELSE                                                                 
053400*       MOVE TEXT-0405 (SPRAK-IX) TO MOD-TEMFSFEL                         
053500*       MOVE NEJ TO INDATA-SW                                             
053600*    END-IF                                                               
053700                                                                          
053800     ACCEPT DAGENS-DATUM FROM DATE                                        
053900     MOVE FUNCTION CURRENT-DATE (1:2) TO SEKEL                            
054000     COMPUTE SEKEL-TAL = SEKEL * 1000000                                  
054100                                                                          
054200     .                                                                    
054300     EJECT                                                                
054400 B-KOLLA-NYCKLAR SECTION.                                                 
054500                                                                          
054600     MOVE NEJ TO W-ORDQ01-NYCKLAR W-OWDS01-NYCKLAR                        
054700     MOVE JA  TO INDATA-SW                                                
054800                                                                          
054900     IF NOT WS-EGEN-BILD                                                  
055000        MOVE ZERO TO MID-IDDISTR-IN                                       
055100                     MID-IDANSK-IN                                        
055200                     MID-KDROO-IN                                         
055300                     MID-KDTPOTYP-FOM-IN                                  
055400                     MID-KDTPOTYP-TOM-IN                                  
055500                     MID-KDSTARAD-IN                                      
055600                     MID-KVART-OPP                                        
055700                     MID-TITPO-OPP                                        
055800     END-IF                                                               
055900                                                                          
056000     PERFORM BA-SPARA-INPUT                                               
056100*       --- KOLLA BEHÖRIG ANVÄNDARE                                       
056200     IF IDARTNR-WS NUMERIC                                                
056300        MOVE IDARTNR-WS TO W-IDARTNR-N3                                   
056400        PERFORM IMS-GU-WDK601                                             
056500        IF SEGMENT-FINNS                                                  
056600           MOVE ART-IDLEVNR   TO WS-IDLEVNR-8                             
056700           IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                      
056800           OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                
056900*   *         --- BEHÖRIG ANVÄNDARE                                       
057000              CONTINUE                                                    
057100           ELSE                                                           
057200              MOVE NEJ TO INDATA-SW                                       
057300              MOVE +1 TO INDX                                             
057400              PERFORM MFS-RENSA-FALT                                      
057500              MOVE FEL-6 TO MOD-TEMFSFEL                                  
057600           END-IF                                                         
057700        ELSE                                                              
057800           MOVE NEJ TO INDATA-SW                                          
057900           MOVE +1 TO INDX                                                
058000           PERFORM MFS-RENSA-FALT                                         
058100           MOVE FEL-7 TO MOD-TEMFSFEL                                     
058200        END-IF                                                            
058300     END-IF                                                               
058400                                                                          
058500     IF INDATA-OK                                                         
058600       IF IDANSK-WS > ZERO                                                
058700       AND KDROO-WS > ZERO                                                
058800           PERFORM BC-KOLLA-OWDS01-NYCKLAR                                
058900       ELSE                                                               
059000          IF IDARTNR-WS > ZERO                                            
059100             PERFORM BB-KOLLA-ORDQ01-NYCKLAR                              
059200          ELSE                                                            
059300             MOVE NEJ TO INDATA-SW                                        
059400             MOVE TEXT-0401 (SPRAK-IX) TO MOD-TEMFSFEL                    
059500          END-IF                                                          
059600       END-IF                                                             
059700     END-IF                                                               
059800                                                                          
059900     IF IDARTNR-WS NOT NUMERIC                                            
060000        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                            
060100                                MOD-TEMFSFEL                              
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 BA-SPARA-INPUT    SECTION.                                               
060600                                                                          
060700     MOVE ALL '+' TO MSGI-WMSGINIT                                        
060800     MOVE '001'             TO MSGI-KDCALL                                
060900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
061000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
061100     MOVE '2125'            TO MSGI-IDTRANS                               
061200     IF MFS-IDTRANS = '2125'                                              
061300         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
061400     ELSE                                                                 
061500       IF MID-IDARTNR-IN NUMERIC                                          
061600       AND MID-IDARTNR-IN > ZERO                                          
061700           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
061800       END-IF                                                             
061900     END-IF                                                               
062000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062100     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
062200     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
062300                                                                          
062400     IF MID-IDARTNR-IN = ALL '+'                                          
062500         CONTINUE                                                         
062600     ELSE                                                                 
062700         MOVE SPACE TO MFS-IDPFK                                          
062800     END-IF                                                               
062900                                                                          
063000     IF MID-IDDISTR-IN = ALL '+'                                          
063100         MOVE MID-IDDISTR-UT TO IDDISTR-WS                                
063200         INSPECT IDDISTR-WS REPLACING LEADING SPACE BY ZERO               
063300     ELSE                                                                 
063400         MOVE MID-IDDISTR-IN TO IDDISTR-WS                                
063500         MOVE SPACE TO MFS-IDPFK                                          
063600     END-IF                                                               
063700                                                                          
063800     IF MID-IDANSK-IN = ALL '+'                                           
063900         MOVE MID-IDANSK-UT TO IDANSK-WS                                  
064000         INSPECT IDANSK-WS REPLACING LEADING SPACE BY ZERO                
064100     ELSE                                                                 
064200         MOVE MID-IDANSK-IN TO IDANSK-WS                                  
064300         MOVE SPACE TO MFS-IDPFK                                          
064400     END-IF                                                               
064500                                                                          
064600     IF MID-KDROO-IN = ALL '+'                                            
064700         MOVE MID-KDROO-UT TO KDROO-WS                                    
064800         INSPECT KDROO-WS REPLACING LEADING SPACE BY ZERO                 
064900     ELSE                                                                 
065000         MOVE MID-KDROO-IN TO KDROO-WS                                    
065100         MOVE SPACE TO MFS-IDPFK                                          
065200     END-IF                                                               
065300                                                                          
065400     IF MID-KDTPOTYP-FOM-IN = ALL '+'                                     
065500       MOVE MID-KDTPOTYP-FOM-UT TO KDTPOTYP-FOM-WS                        
065600       INSPECT KDTPOTYP-FOM-WS REPLACING LEADING SPACE BY ZERO            
065700       IF MID-KDTPOTYP-TOM-IN = ALL '+'                                   
065800         MOVE MID-KDTPOTYP-TOM-UT TO KDTPOTYP-TOM-WS                      
065900         INSPECT KDTPOTYP-TOM-WS REPLACING LEADING SPACE                  
066000                                                       BY ZERO            
066100       ELSE                                                               
066200         MOVE MID-KDTPOTYP-TOM-IN TO KDTPOTYP-TOM-WS                      
066300         MOVE SPACE TO MFS-IDPFK                                          
066400       END-IF                                                             
066500     ELSE                                                                 
066600       MOVE MID-KDTPOTYP-FOM-IN TO KDTPOTYP-FOM-WS                        
066700       MOVE SPACE TO MFS-IDPFK                                            
066800       IF MID-KDTPOTYP-TOM-IN = ALL '+'                                   
066900         MOVE MID-KDTPOTYP-FOM-IN TO KDTPOTYP-TOM-WS                      
067000       ELSE                                                               
067100         MOVE MID-KDTPOTYP-TOM-IN TO KDTPOTYP-TOM-WS                      
067200       END-IF                                                             
067300     END-IF                                                               
067400                                                                          
067500     IF MID-KDSTARAD-IN = ALL '+'                                         
067600         MOVE MID-KDSTARAD-UT TO KDSTARAD-WS                              
067700         INSPECT KDSTARAD-WS REPLACING LEADING SPACE BY ZERO              
067800     ELSE                                                                 
067900         MOVE MID-KDSTARAD-IN TO KDSTARAD-WS                              
068000         MOVE SPACE TO MFS-IDPFK                                          
068100     END-IF                                                               
068200                                                                          
068300     MOVE IDARTNR-WS          TO MOD-IDARTNR-UT                           
068400     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
068500                                                                          
068600     MOVE IDDISTR-WS          TO MOD-IDDISTR-UT                           
068700     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
068800                                                                          
068900     MOVE IDANSK-WS           TO MOD-IDANSK-UT                            
069000     INSPECT MOD-IDANSK-UT  REPLACING LEADING ZERO BY SPACE               
069100                                                                          
069200     MOVE KDROO-WS            TO MOD-KDROO-UT                             
069300     INSPECT MOD-KDROO-UT   REPLACING LEADING ZERO BY SPACE               
069400                                                                          
069500     MOVE KDTPOTYP-FOM-WS     TO MOD-KDTPOTYP-FOM-UT                      
069600     INSPECT MOD-KDTPOTYP-FOM-UT REPLACING LEADING ZERO                   
069700                                                   BY SPACE               
069800                                                                          
069900     MOVE KDTPOTYP-TOM-WS     TO MOD-KDTPOTYP-TOM-UT                      
070000     INSPECT MOD-KDTPOTYP-TOM-UT REPLACING LEADING ZERO                   
070100                                                   BY SPACE               
070200                                                                          
070300     MOVE KDSTARAD-WS         TO MOD-KDSTARAD-UT                          
070400     INSPECT MOD-KDSTARAD-UT REPLACING LEADING ZERO BY SPACE              
070500     .                                                                    
070600     EJECT                                                                
070700 BB-KOLLA-ORDQ01-NYCKLAR SECTION.                                         
070800                                                                          
070900     MOVE JA TO W-ORDQ01-NYCKLAR                                          
071000                                                                          
071100     IF MID-IDARTNR-IN  = ALL '+'                                         
071200        IF MFS-IDPFK = SPACE AND MFS-KDTRTYP      = SPACE                 
071300           IF MID-KVART-OPP  NOT = ALL '0'                                
071400           OR MID-TITPO-OPP  NOT = ALL '0'                                
071500              MOVE NEJ        TO INDATA-SW                                
071600              MOVE TEXT-0407 (SPRAK-IX) TO MOD-TEMFSFEL                   
071700              PERFORM MFS-ROR-EJ-IN-UT-NYCKLAR                            
071800              PERFORM MFS-ROR-EJ-SPARADE-NYCKLAR                          
071900              PERFORM MFS-ROR-EJ-FAELT                                    
072000              MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-OPP-ATTR            
072100                                            MOD-TITPO-OPP-ATTR            
072200           END-IF                                                         
072300        END-IF                                                            
072400     ELSE                                                                 
072500        IF MFS-UPDATE                                                     
072600           MOVE TEXT-0401 (SPRAK-IX) TO MOD-TEMFSFEL                      
072700           MOVE NEJ TO INDATA-SW                                          
072800           MOVE +1  TO INDX                                               
072900           PERFORM MFS-RENSA-FALT-IN                                      
073000           PERFORM MFS-RENSA-FALT                                         
073100        END-IF                                                            
073200     END-IF                                                               
073300                                                                          
073400     IF  IDARTNR-WS      NUMERIC                                          
073500     AND IDDISTR-WS      NUMERIC                                          
073600     AND IDANSK-WS       NUMERIC                                          
073700     AND KDROO-WS        NUMERIC                                          
073800     AND KDTPOTYP-FOM-WS NUMERIC                                          
073900     AND KDTPOTYP-TOM-WS NUMERIC                                          
074000     AND KDSTARAD-WS     NUMERIC                                          
074100     AND KDSTARAD-WS < '4'                                                
074200        MOVE IDARTNR-WS       TO W-IDARTNR                                
074300        MOVE IDDISTR-WS       TO W-IDDISTR                                
074400        MOVE IDANSK-WS        TO W-IDANSK                                 
074500        MOVE KDROO-WS         TO W-KDROO                                  
074600        MOVE KDTPOTYP-FOM-WS  TO W-KDTPOTYP-FOM                           
074700        MOVE KDTPOTYP-TOM-WS  TO W-KDTPOTYP-TOM                           
074800        MOVE KDSTARAD-WS      TO W-KDSTARAD                               
074900     ELSE                                                                 
075000        MOVE NEJ        TO INDATA-SW                                      
075100        MOVE TEXT-0401 (SPRAK-IX) TO MOD-TEMFSFEL                         
075200        PERFORM MFS-RENSA-FALT-IN                                         
075300        MOVE +1         TO INDX                                           
075400        PERFORM MFS-RENSA-FALT                                            
075500     END-IF                                                               
075600     .                                                                    
075700     EJECT                                                                
075800 BC-KOLLA-OWDS01-NYCKLAR SECTION.                                         
075900                                                                          
076000     MOVE JA TO W-OWDS01-NYCKLAR                                          
076100                                                                          
076200     IF   MID-IDANSK-IN  = ALL '+'                                        
076300     AND  MID-KDROO-IN   = ALL '+'                                        
076400        IF MFS-IDPFK = SPACE AND MFS-KDTRTYP      = SPACE                 
076500           IF MID-KVART-OPP  NOT = ALL '0'                                
076600           OR MID-TITPO-OPP  NOT = ALL '0'                                
076700              MOVE NEJ        TO INDATA-SW                                
076800              MOVE TEXT-0407 (SPRAK-IX) TO MOD-TEMFSFEL                   
076900              MOVE MID-KVART-OPP        TO MOD-TEMFSFEL                   
077000              PERFORM MFS-ROR-EJ-IN-UT-NYCKLAR                            
077100              PERFORM MFS-ROR-EJ-SPARADE-NYCKLAR                          
077200              PERFORM MFS-ROR-EJ-FAELT                                    
077300              MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVART-OPP-ATTR            
077400                                            MOD-TITPO-OPP-ATTR            
077500           END-IF                                                         
077600        END-IF                                                            
077700     ELSE                                                                 
077800        IF MFS-UPDATE                                                     
077900           MOVE TEXT-0401 (SPRAK-IX) TO MOD-TEMFSFEL                      
078000           MOVE NEJ TO INDATA-SW                                          
078100           MOVE +1  TO INDX                                               
078200           PERFORM MFS-RENSA-FALT-IN                                      
078300           PERFORM MFS-RENSA-FALT                                         
078400        END-IF                                                            
078500     END-IF                                                               
078600                                                                          
078700     IF  IDARTNR-WS      NUMERIC                                          
078800     AND IDDISTR-WS      NUMERIC                                          
078900     AND IDANSK-WS       NUMERIC                                          
079000     AND KDROO-WS        NUMERIC                                          
079100     AND KDTPOTYP-FOM-WS NUMERIC                                          
079200     AND KDTPOTYP-TOM-WS NUMERIC                                          
079300     AND KDSTARAD-WS     NUMERIC                                          
079400     AND KDSTARAD-WS < '4'                                                
079500        MOVE IDARTNR-WS       TO W-IDARTNR                                
079600        MOVE IDDISTR-WS       TO W-IDDISTR                                
079700        MOVE IDANSK-WS        TO W-IDANSK                                 
079800        MOVE KDROO-WS         TO W-KDROO                                  
079900        MOVE KDTPOTYP-FOM-WS  TO W-KDTPOTYP-FOM                           
080000        MOVE KDTPOTYP-TOM-WS  TO W-KDTPOTYP-TOM                           
080100        MOVE KDSTARAD-WS      TO W-KDSTARAD                               
080200     ELSE                                                                 
080300        MOVE NEJ        TO INDATA-SW                                      
080400        MOVE TEXT-0401 (SPRAK-IX) TO MOD-TEMFSFEL                         
080500        PERFORM MFS-RENSA-FALT-IN                                         
080600        MOVE +1         TO INDX                                           
080700        PERFORM MFS-RENSA-FALT                                            
080800     END-IF                                                               
080900     .                                                                    
081000     EJECT                                                                
081100 C-KOLLA-VALKOD   SECTION.                                                
081200                                                                          
081300     MOVE +1 TO INDX                                                      
081400                                                                          
081500     IF  MID-IDARTNR-IN      = ALL '+'                                    
081600     AND MID-IDDISTR-IN      = ALL '+'                                    
081700     AND MID-IDANSK-IN       = ALL '+'                                    
081800     AND MID-KDROO-IN        = ALL '+'                                    
081900     AND MID-KDTPOTYP-FOM-IN = ALL '+'                                    
082000     AND MID-KDTPOTYP-TOM-IN = ALL '+'                                    
082100     AND MID-KDSTARAD-IN     = ALL '+'                                    
082200        IF MFS-IDPFK = 7 OR 8                                             
082300           MOVE JA TO VALKOD-OK                                           
082400        ELSE                                                              
082500           IF MFS-UPDATE                                                  
082600             MOVE JA TO VALKOD-OK                                         
082700             PERFORM UNTIL INDX > 13 OR                                   
082800                      VALKOD-OK = NEJ                                     
082900               IF MID-VALKOD(INDX) = 'D'                                  
083000                 IF MID-KDTPOTYP(INDX) = 1                                
083100                   IF MID-IDDISTR(INDX) = ZERO                            
083200                     MOVE NEJ TO VALKOD-OK                                
083300                   ELSE                                                   
083400                     MOVE MID-IDDISTR(INDX) TO W-TEST-IDDIST              
083500                     MOVE W-TEST-IDDISTR    TO TEST-IDDISTR               
083600                     IF  DIST19-SATS                                      
083800                       MOVE NEJ TO VALKOD-OK                              
083900                       MOVE NEJ TO SATS-DELETE-OK                         
084000                     ELSE                                                 
084100                       PERFORM CA-KOLLA-INMATAD-RAD                       
084200                     END-IF                                               
084300                   END-IF                                                 
084400                 ELSE                                                     
084500                   MOVE NEJ TO VALKOD-OK                                  
084600                   MOVE FEL-1 TO MOD-TEMFSINF                             
084700                 END-IF                                                   
084800               ELSE                                                       
084900                 IF MID-VALKOD(INDX) = ' '                                
085000                   IF MID-KVRAD-SPAR = +1 AND INDX = +1                   
085100                     PERFORM CA-KOLLA-INMATAD-RAD                         
085200                   END-IF                                                 
085300                 ELSE                                                     
085400                   MOVE NEJ       TO VALKOD-OK                            
085500                 END-IF                                                   
085600               END-IF                                                     
085700               ADD +1 TO INDX                                             
085800             END-PERFORM                                                  
085900           ELSE                                                           
086000             PERFORM UNTIL INDX > 13 OR                                   
086100                       VALKOD-OK = NEJ                                    
086200               IF MID-VALKOD (INDX) NOT = ' '                             
086300                  IF MID-IDARTNR(INDX) = ZERO                             
086400                     MOVE NEJ TO VALKOD-OK                                
086500                  ELSE                                                    
086600                     IF MID-VALKOD(INDX) = 'S'                            
086700                        IF W-VALKOD = ' '                                 
086800                           MOVE 'S' TO W-VALKOD                           
086900                           MOVE JA TO VALKOD-OK                           
087000                        ELSE                                              
087100                           MOVE NEJ TO VALKOD-OK                          
087200                        END-IF                                            
087300                        PERFORM S04-FLYTTA-RO-NYCKLAR                     
087400                        PERFORM IMS-GU-ORDP01-2                           
087500                        IF SEGMENT-FINNS                                  
087600                          IF RAD-KDTPOTYP = 1 OR 2 OR 6                   
087700                            CONTINUE                                      
087800                          ELSE                                            
087900                            MOVE NEJ TO VALKOD-OK                         
088000                            MOVE FEL-2 TO MOD-TEMFSINF                    
088100                          END-IF                                          
088200                          IF RAD-FLTPOBEK = NEJ                           
088300                            MOVE NEJ TO VALKOD-OK                         
088400                            MOVE FEL-3 TO MOD-TEMFSINF                    
088500                          ELSE                                            
088600                            IF RAD-KDSTARAD = 2 OR 3                      
088700                              MOVE NEJ TO VALKOD-OK                       
088800                              MOVE FEL-4 TO MOD-TEMFSINF                  
088900                            END-IF                                        
089000                          END-IF                                          
089100                        END-IF                                            
089200                     ELSE                                                 
089300                       IF MID-VALKOD(INDX) = 'D'                          
089400                          IF W-VALKOD = 'D' OR ' '                        
089500                             MOVE NEJ TO VALKOD-DELETE-OK                 
089600                             MOVE 'D' TO W-VALKOD                         
089700                          ELSE                                            
089800                             MOVE NEJ TO VALKOD-OK                        
089900                          END-IF                                          
090000                       ELSE                                               
090100                          IF MID-VALKOD(INDX) NOT = ' '                   
090200                             MOVE NEJ TO VALKOD-OK                        
090300                          END-IF                                          
090400                       END-IF                                             
090500                     END-IF                                               
090600                  END-IF                                                  
090700               END-IF                                                     
090800               ADD +1 TO INDX                                             
090900             END-PERFORM                                                  
091000           END-IF                                                         
091100        END-IF                                                            
091200     END-IF                                                               
091300                                                                          
091400     IF VALKOD-OK        = NEJ                                            
091500     OR VALKOD-DELETE-OK = NEJ                                            
091600     OR SATS-DELETE-OK   = NEJ                                            
091700        PERFORM MFS-ROR-EJ-IN-UT-NYCKLAR                                  
091800        PERFORM MFS-ROR-EJ-SPARADE-NYCKLAR                                
091900        PERFORM MFS-ROR-EJ-FAELT                                          
092000        MOVE +1 TO INDX                                                   
092100        PERFORM UNTIL INDX > 13                                           
092200          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VALKOD-ATTR (INDX)            
092300          ADD +1 TO INDX                                                  
092400        END-PERFORM                                                       
092500                                                                          
092600        IF SATS-DELETE-OK = NEJ                                           
092700              MOVE TEXT-0422 (SPRAK-IX) TO MOD-TEMFSFEL                   
092800        ELSE                                                              
092900           IF VALKOD-OK = NEJ                                             
093000              MOVE TEXT-0416 (SPRAK-IX) TO MOD-TEMFSFEL                   
093100           ELSE                                                           
093200              MOVE TEXT-0407 (SPRAK-IX) TO MOD-TEMFSFEL                   
093300           END-IF                                                         
093400        END-IF                                                            
093500     END-IF                                                               
093600     .                                                                    
093700     EJECT                                                                
093800 CA-KOLLA-INMATAD-RAD SECTION.                                            
093900                                                                          
094000     PERFORM S04-FLYTTA-RO-NYCKLAR                                        
094100     PERFORM IMS-GHU-ORDP01                                               
094200     IF SEGMENT-FINNS                                                     
094300       IF RAD-KDSTARAD = 1                                                
094400         IF MID-KVART-OPP > 0                                             
094500         OR MID-TITPO-OPP > 0                                             
094600           PERFORM CAA-KOLLA-ANTAL                                        
094700           PERFORM CAB-KOLLA-TITPO                                        
094800         END-IF                                                           
094900       ELSE                                                               
095000         MOVE NEJ TO VALKOD-OK                                            
095100         MOVE FEL-4 TO MOD-TEMFSINF                                       
095200       END-IF                                                             
095300     ELSE                                                                 
095400       MOVE NEJ TO VALKOD-OK                                              
095500     END-IF                                                               
095600     .                                                                    
095700     SKIP3                                                                
095800 CAA-KOLLA-ANTAL           SECTION.                                       
095900                                                                          
096000     IF MID-KVART-OPP > ZERO                                              
096100     OR RAD-KDTPOTYP = 2                                                  
096200                                                                          
096300       MOVE RAD-IDARTNR     TO W-IDARTNR-N3                               
096400       PERFORM IMS-GU-ART-01                                              
096500       MOVE ART-KDSORT      TO ART-S-KDSORT                               
096600       PERFORM IMS-GU-ART-11                                              
096700                                                                          
096800       IF MID-KVART-OPP > ZERO                                            
096900         IF MID-KVART-OPP NUMERIC                                         
097000           IF MID-VALKOD(INDX) = 'D'                                      
097100             IF MID-KVART-OPP > RAD-KVART                                 
097200               MOVE NEJ TO ANTAL-OK                                       
097300                           OPP-FAELT-OK                                   
097400             END-IF                                                       
097500           ELSE                                                           
097600             IF MID-KVART-OPP = RAD-KVART                                 
097700               MOVE NEJ TO ANTAL-OK                                       
097800                           OPP-FAELT-OK                                   
097900             ELSE                                                         
098000               IF MID-KDTPOTYP(INDX) = 2 OR 6                             
098100                 IF MID-KVART-OPP > RAD-KVART                             
098200                   MOVE NEJ TO ANTAL-OK                                   
098300                               OPP-FAELT-OK                               
098400                 END-IF                                                   
098500                 IF MID-TITPO-OPP = ZERO                                  
098600                   MOVE NEJ TO ANTAL-OK                                   
098700                               OPP-FAELT-OK                               
098800                 END-IF                                                   
098900               ELSE                                                       
099000                 IF  MID-KVART-OPP < RAD-KVART                            
099100                 AND MID-TITPO-OPP = ZERO                                 
099200                   MOVE NEJ TO ANTAL-OK                                   
099300                               OPP-FAELT-OK                               
099400                 END-IF                                                   
099500               END-IF                                                     
099600             END-IF                                                       
099700           END-IF                                                         
099800         ELSE                                                             
099900           MOVE NEJ TO ANTAL-OK                                           
100000                       OPP-FAELT-OK                                       
100100         END-IF                                                           
100200         IF ANTAL-OK = JA                                                 
100300           MOVE MID-KVART-OPP   TO W-SPAR-KVART                           
100400                                                                          
100500           IF (ART-S-KDSORT = 'KG' OR = 'L ' OR = 'M ')                   
100600           OR (RAD-KDKVBRYT = 0 AND CLAG-KVQPACK-1 > +1)                  
100700           OR  RAD-KDKVBRYT = 2                                           
100800             IF CLAG-KVQPACK-1 = ZERO                                     
100900                MOVE ZERO TO W-ANTAL-FORP                                 
101000             ELSE                                                         
101100                COMPUTE W-ANTAL-FORP =                                    
101200                        W-SPAR-KVART / CLAG-KVQPACK-1                     
101300             END-IF                                                       
101400             MULTIPLY W-ANTAL-FORP BY CLAG-KVQPACK-1                      
101500                                   GIVING W-ANTAL-KVART                   
101600             IF W-ANTAL-KVART NOT = W-SPAR-KVART                          
101700               IF RAD-KDKVBRYT = 2                                        
101800                 MOVE JA TO BRUTEN-KVANT-SW                               
101900               ELSE                                                       
102000                 MOVE NEJ TO OPP-FAELT-OK                                 
102100                 MOVE NEJ TO KVQPACK-OK                                   
102200                 MOVE NEJ TO ANTAL-OK                                     
102300               END-IF                                                     
102400             END-IF                                                       
102500           END-IF                                                         
102600         END-IF                                                           
102700       END-IF                                                             
102800     END-IF                                                               
102900     .                                                                    
103000     EJECT                                                                
103100 CAB-KOLLA-TITPO           SECTION.                                       
103200                                                                          
103300     IF MID-TITPO-OPP NOT NUMERIC                                         
103400       MOVE NEJ TO TITPO-OK                                               
103500       MOVE NEJ TO OPP-FAELT-OK                                           
103600     ELSE                                                                 
103700       IF MID-TITPO-OPP > ZERO                                            
103800         MOVE MID-TITPO-OPP TO DAT-I-TIDATUM                              
103900         MOVE 'AAVVD '      TO DAT-KDDATFORM                              
104000         CALL WDATKONV USING DAT-KDDATFORM                                
104100                             DAT-I-TIDATUM                                
104200                             DAT-O-TIDATUM                                
104300                             DAT-KDSVAR                                   
104400         MOVE DAT-TIAAMMDD   TO TMP1-YYMMDD                               
104500         MOVE DAGENS-DATUM   TO TMP2-YYMMDD                               
104600         PERFORM WY2000P1                                                 
104700         IF DAT-KDSVAR-FEL                                                
104800         OR TMP1-YYMMDD  < TMP2-YYMMDD                                    
104900           MOVE NEJ TO TITPO-OK OPP-FAELT-OK                              
105000         ELSE                                                             
105100           MOVE DAT-TIAAMMDD TO WS-TIAAMMDD                               
105200         END-IF                                                           
105300         IF MID-VALKOD(1) = 'D'                                           
105400           IF WS-TIAAMMDD = RAD-TITPO                                     
105500             CONTINUE                                                     
105600           ELSE                                                           
105700             MOVE NEJ TO TITPO-OK OPP-FAELT-OK                            
105800           END-IF                                                         
105900         ELSE                                                             
106000           IF  WS-TIAAMMDD = RAD-TITPO                                    
106100           AND (MID-KVART-OPP > ZERO AND                                  
106200                MID-KVART-OPP < RAD-KVART)                                
106300             MOVE NEJ TO TITPO-OK OPP-FAELT-OK                            
106400             MOVE FEL-5 TO MOD-TEMFSFEL                                   
106500           END-IF                                                         
106600         END-IF                                                           
106700       END-IF                                                             
106800     END-IF                                                               
106900     .                                                                    
107000     EJECT                                                                
107100 D-LAES-WDA5-PF8  SECTION.                                                
107200                                                                          
107300     MOVE LOW-VALUE              TO W-SOK-NYCKLAR-MIN                     
107400     MOVE HIGH-VALUE             TO W-SOK-NYCKLAR-MAX                     
107500                                                                          
107600     IF W-IDDISTR > ZERO                                                  
107700        MOVE W-IDDISTR           TO W-IDDISTR-MIN                         
107800                                    W-IDDISTR-MAX                         
107900     END-IF                                                               
108000                                                                          
108100     IF W-KDTPOTYP-FOM > ZERO                                             
108200        MOVE W-KDTPOTYP-FOM       TO W-KDTPOTYP-MIN                       
108300     END-IF                                                               
108400                                                                          
108500     IF W-KDTPOTYP-TOM > ZERO                                             
108600        MOVE W-KDTPOTYP-TOM       TO W-KDTPOTYP-MAX                       
108700     END-IF                                                               
108800                                                                          
108900     IF W-KDSTARAD > ZERO                                                 
109000        MOVE W-KDSTARAD           TO W-KDSTARAD-MIN                       
109100                                     W-KDSTARAD-MAX                       
109200     ELSE                                                                 
109300        MOVE '1'                  TO W-KDSTARAD-MIN                       
109400        MOVE '3'                  TO W-KDSTARAD-MAX                       
109500     END-IF                                                               
109600                                                                          
109700     IF W-ORDQ01-NYCKLAR = JA                                             
109800                                                                          
109900        IF W-IDANSK  > ZERO                                               
110000           MOVE W-IDANSK        TO W-IDANSK-MIN                           
110100                                   W-IDANSK-MAX                           
110200        END-IF                                                            
110300                                                                          
110400        IF W-KDROO   > ZERO                                               
110500           MOVE W-KDROO         TO W-KDROO-MIN                            
110600                                   W-KDROO-MAX                            
110700        END-IF                                                            
110800                                                                          
110900        IF MID-IDARTNR-SPAR = ZERO                                        
111000           MOVE MID-IDARTNR-SPAR-E  TO W-IDARTNR-N3-MIN                   
111100           MOVE WC-CDC-SE           TO W-IDDC-N3-MIN                      
111200           MOVE MID-KDRAPRIO-SPAR-E TO W-KDRAPRIO-N3-MIN                  
111300           MOVE MID-TIRODAT-SPAR-E  TO W-DARODAT-N3-MIN                   
111400           IF MID-TIRODAT-SPAR-E NOT = ZERO                               
111500             IF MID-TIRODAT-SPAR-E < 500000                               
111600               MOVE 20              TO W-DARODAT-N3-MIN (1:2)             
111700             ELSE                                                         
111800               IF MID-TIRODAT-SPAR-E < 999999                             
111900                 MOVE 19            TO W-DARODAT-N3-MIN (1:2)             
112000               ELSE                                                       
112100                 MOVE 99999999      TO W-DARODAT-N3-MIN                   
112200               END-IF                                                     
112300             END-IF                                                       
112400           END-IF                                                         
112410           MOVE MID-TIREGTID-SPAR-E TO W-TIREGTID-N3-MIN                  
112500           MOVE MID-IDDISTR-SPAR-E  TO W-IDDISTR-N3-MIN                   
112600           MOVE MID-IDKUNDNR-SPAR-E TO W-IDKUNDNR-N3-MIN                  
112700           MOVE MID-IDKUNDRF-SPAR-E TO W-IDKUNDRF-N3-MIN                  
112800           MOVE MID-IDLOPNR-SPAR-E  TO W-IDLOPNR-N3-MIN                   
112900        ELSE                                                              
113000           MOVE MID-IDARTNR-SPAR    TO W-IDARTNR-N3-MIN                   
113100           MOVE WC-CDC-SE           TO W-IDDC-N3-MIN                      
113200           MOVE MID-KDRAPRIO-SPAR   TO W-KDRAPRIO-N3-MIN                  
113300           MOVE MID-TIRODAT-SPAR    TO W-DARODAT-N3-MIN                   
113400           IF MID-TIRODAT-SPAR   NOT = ZERO                               
113500             IF MID-TIRODAT-SPAR   < 500000                               
113600               MOVE 20              TO W-DARODAT-N3-MIN (1:2)             
113700             ELSE                                                         
113800               IF MID-TIRODAT-SPAR   < 999999                             
113900                 MOVE 19            TO W-DARODAT-N3-MIN (1:2)             
114000               ELSE                                                       
114100                 MOVE 99999999      TO W-DARODAT-N3-MIN                   
114200               END-IF                                                     
114300             END-IF                                                       
114400           END-IF                                                         
114410           MOVE MID-TIREGTID-SPAR   TO W-TIREGTID-N3-MIN                  
114500           MOVE MID-IDDISTR-SPAR    TO W-IDDISTR-N3-MIN                   
114600           MOVE MID-IDKUNDNR-SPAR   TO W-IDKUNDNR-N3-MIN                  
114700           MOVE MID-IDKUNDRF-SPAR   TO W-IDKUNDRF-N3-MIN                  
114800           MOVE MID-IDLOPNR-SPAR    TO W-IDLOPNR-N3-MIN                   
114900        END-IF                                                            
115000                                                                          
115100        PERFORM IMS-GU-ORDQ01                                             
115200                                                                          
115300        MOVE LOW-VALUE              TO W-WDA5A1KY-MIN                     
115400        MOVE HIGH-VALUE             TO W-WDA5A1KY-MAX                     
115500        MOVE W-IDARTNR              TO W-IDARTNR-N3-MIN                   
115600                                       W-IDARTNR-N3-MAX                   
115700        MOVE WC-CDC-SE              TO W-IDDC-N3-MAX                      
115800        IF SEGMENT-SAKNAS                                                 
115900           PERFORM IMS-GN-ORDQ01                                          
116000        END-IF                                                            
116100        PERFORM S03-LAES-WDA5                                             
116200     ELSE                                                                 
116300        IF  MID-IDANSK-SPAR = ZERO                                        
116400        AND MID-KDROO-SPAR  = ZERO                                        
116500           MOVE MID-IDANSK-SPAR-E   TO W-IDANSK-N1-MIN                    
116600           MOVE MID-KDROO-SPAR-E    TO W-KDROO-N1-MIN                     
116700           MOVE MID-IDARTNR-SPAR-E  TO W-IDARTNR-N1-MIN                   
116800           MOVE MID-IDDISTR-SPAR-E  TO W-IDDISTR-N1-MIN                   
116900           MOVE MID-IDKUNDNR-SPAR-E TO W-IDKUNDNR-N1-MIN                  
117000           MOVE MID-IDKUNDRF-SPAR-E TO W-IDKUNDRF-N1-MIN                  
117100           MOVE MID-IDLOPNR-SPAR-E  TO W-IDLOPNR-N1-MIN                   
117200        ELSE                                                              
117300           MOVE MID-IDANSK-SPAR     TO W-IDANSK-N1-MIN                    
117400           MOVE MID-KDROO-SPAR      TO W-KDROO-N1-MIN                     
117500           MOVE MID-IDARTNR-SPAR    TO W-IDARTNR-N1-MIN                   
117600           MOVE MID-IDDISTR-SPAR    TO W-IDDISTR-N1-MIN                   
117700           MOVE MID-IDKUNDNR-SPAR   TO W-IDKUNDNR-N1-MIN                  
117800           MOVE MID-IDKUNDRF-SPAR   TO W-IDKUNDRF-N1-MIN                  
117900           MOVE MID-IDLOPNR-SPAR    TO W-IDLOPNR-N1-MIN                   
118000        END-IF                                                            
118100                                                                          
118200        PERFORM IMS-GU-OWDS01                                             
118300                                                                          
118400        MOVE LOW-VALUE              TO W-WDA5C1KY-MIN                     
118500        MOVE HIGH-VALUE             TO W-WDA5C1KY-MAX                     
118600        MOVE W-IDANSK               TO W-IDANSK-N1-MIN                    
118700                                       W-IDANSK-N1-MAX                    
118800        MOVE W-KDROO                TO W-KDROO-N1-MIN                     
118900                                       W-KDROO-N1-MAX                     
119000        IF W-IDARTNR > ZERO                                               
119100           MOVE W-IDARTNR           TO W-IDARTNR-N1-MIN                   
119200                                       W-IDARTNR-N1-MAX                   
119300           IF W-IDDISTR > ZERO                                            
119400              MOVE W-IDDISTR        TO W-IDDISTR-N1-MIN                   
119500                                       W-IDDISTR-N1-MAX                   
119600           END-IF                                                         
119700        END-IF                                                            
119800                                                                          
119900        IF SEGMENT-SAKNAS                                                 
120000           PERFORM IMS-GN-OWDS01                                          
120100        END-IF                                                            
120200        PERFORM S03-LAES-WDA5                                             
120300     END-IF                                                               
120400     .                                                                    
120500     EJECT                                                                
120600 E-UPPDATERA-RAD SECTION.                                                 
120700                                                                          
120800     MOVE ZERO TO W-SPAR-KVART W-SPAR-TITPO                               
120900     IF OPP-FAELT-OK = JA                                                 
121000        IF MID-KVART-OPP > ZERO                                           
121100        OR MID-TITPO-OPP > ZERO                                           
121200           PERFORM S04-FLYTTA-RO-NYCKLAR                                  
121300           PERFORM IMS-GHU-ORDP01                                         
121400           IF SEGMENT-FINNS                                               
121500              PERFORM S09-HAEMTA-TIDISPIN                                 
121600              MOVE JA                   TO W-UPDATE                       
121700                                                                          
121800              MOVE RAD-IDLOPNR          TO W-SPAR-IDLOPNR                 
121900              IF  MID-TITPO-OPP > ZERO                                    
122000              AND MID-KVART-OPP > ZERO                                    
122100                IF  MID-KVART-OPP < RAD-KVART                             
122200                AND WS-TIAAMMDD NOT = RAD-TITPO                           
122300                  PERFORM EA-BEH-SPLITTAD-RAD                             
122400                ELSE                                                      
122500                  PERFORM EB-BEH-NY-TITPO-OCH-OEKN-KVART                  
122600                END-IF                                                    
122700              ELSE                                                        
122800                IF MID-KVART-OPP > ZERO                                   
122900* END TPO1:OR                                                             
123000                  IF MID-KVART-OPP > RAD-KVART                            
123100                    PERFORM EC-BEH-OEKN-KVART                             
123200                  END-IF                                                  
123300                ELSE                                                      
123400                  PERFORM ED-BEH-NY-TITPO                                 
123500                END-IF                                                    
123600              END-IF                                                      
123700              MOVE RAD-IDARTNR           TO MID-IDARTNR-SPAR-E            
123800              MOVE RAD-KDRAPRIO          TO MID-KDRAPRIO-SPAR-E           
123900              MOVE RAD-DARODAT (3:6)     TO MID-TIRODAT-SPAR-E            
123910              MOVE RAD-TIREGTID          TO MID-TIREGTID-SPAR-E           
124000              MOVE RAD-IDDISTR           TO MID-IDDISTR-SPAR-E            
124100              MOVE RAD-IDKUNDNR          TO MID-IDKUNDNR-SPAR-E           
124200              MOVE RAD-IDKUNDRF          TO MID-IDKUNDRF-SPAR-E           
124300              MOVE W-SPAR-IDLOPNR        TO MID-IDLOPNR-SPAR-E            
124400           END-IF                                                         
124500        ELSE                                                              
124600           MOVE TEXT-0414 (SPRAK-IX) TO MOD-TEMFSFEL                      
124700        END-IF                                                            
124800     END-IF                                                               
124900     .                                                                    
125000     EJECT                                                                
125100 EA-BEH-SPLITTAD-RAD SECTION.                                             
125200                                                                          
125300     COMPUTE W-SPAR-KVART = RAD-KVART - MID-KVART-OPP                     
125400     MOVE RAD-TITPO         TO W-SPAR-TITPO                               
125500                                                                          
125600****** NYA RADEN                                                          
125700     MOVE MID-KVART-OPP     TO RAD-KVART                                  
125800     MOVE WS-TIAAMMDD       TO RAD-TITPO                                  
125900     IF RAD-KDKVBRYT = 2 AND BRUTEN-KVANT                                 
126000       MOVE +1 TO RAD-KDKVBRYT                                            
126100     END-IF                                                               
126200     PERFORM IMS-REPL-ORDP01                                              
126300                                                                          
126400     MOVE 71 TO WS-KDORDBEK                                               
126500     PERFORM S06-ORDER-BEKR                                               
126600     IF RAD-KDTPOTYP = 6                                                  
126700       CONTINUE                                                           
126800     ELSE                                                                 
126900       PERFORM S11-SKAPA-RY9-TRANS                                        
127000     END-IF                                                               
127100     PERFORM EAA-UPPD-SUTPO-NY-VECKA                                      
127200                                                                          
127300****** G:A RADEN                                                          
127400     MOVE W-SPAR-KVART      TO RAD-KVART                                  
127500     MOVE W-SPAR-TITPO      TO RAD-TITPO                                  
127700     ADD +1                 TO RAD-IDLOPNR                                
127800     PERFORM IMS-ISRT-ORDP01                                              
127900     PERFORM UNTIL SEGMENT-FINNS                                          
128000       ADD +1               TO RAD-IDLOPNR                                
128100       PERFORM IMS-ISRT-ORDP01                                            
128200     END-PERFORM                                                          
128300                                                                          
128400     MOVE 71 TO WS-KDORDBEK                                               
128500     PERFORM S06-ORDER-BEKR                                               
128600     IF RAD-KDTPOTYP = 6                                                  
128700       CONTINUE                                                           
128800     ELSE                                                                 
128900       PERFORM S11-SKAPA-RY9-TRANS                                        
129000     END-IF                                                               
129100     PERFORM EAB-MINSKA-SUTPO-GAMMAL-VECKA                                
129200     .                                                                    
129300     EJECT                                                                
129400 EAA-UPPD-SUTPO-NY-VECKA SECTION.                                         
129500                                                                          
129600     PERFORM S05-KONV-TITPO                                               
129700     PERFORM IMS-GHU-ARTM-ANT-GE                                          
129800     IF SEGMENT-FINNS                                                     
129900       IF RAD-KDTPOTYP = 6                                                
130000         ADD RAD-KVART TO ARTM-ANT-SUTPO-EJPB                             
130100       ELSE                                                               
130200         ADD RAD-KVART TO ARTM-ANT-SUTPO-PB                               
130300       END-IF                                                             
130400       PERFORM IMS-REPL-ARTM                                              
130500     ELSE                                                                 
130600       MOVE W-DABEHOV  TO ARTM-ANT-DABEHOV                                
130700       IF RAD-KDTPOTYP = 6                                                
130800         MOVE RAD-KVART TO ARTM-ANT-SUTPO-EJPB                            
130900         MOVE ZERO      TO ARTM-ANT-SUTPO-PB                              
131000       ELSE                                                               
131100         MOVE RAD-KVART TO ARTM-ANT-SUTPO-PB                              
131200         MOVE ZERO      TO ARTM-ANT-SUTPO-EJPB                            
131300       END-IF                                                             
131400       PERFORM IMS-ISRT-ARTM-ANT                                          
131500     END-IF                                                               
131600     .                                                                    
131700     EJECT                                                                
131800 EAB-MINSKA-SUTPO-GAMMAL-VECKA SECTION.                                   
131900                                                                          
132000     PERFORM S05-KONV-TITPO                                               
132100     PERFORM IMS-GHU-ARTM-ANT-GE                                          
132200     IF SEGMENT-FINNS                                                     
132300     IF RAD-KDTPOTYP = 6                                                  
132400       SUBTRACT MID-KVART-OPP FROM ARTM-ANT-SUTPO-EJPB                    
132500     ELSE                                                                 
132600       SUBTRACT MID-KVART-OPP FROM ARTM-ANT-SUTPO-PB                      
132700     END-IF                                                               
132800     IF ARTM-ANT-SUTPO-PB > 0 OR ARTM-ANT-SUTPO-EJPB > 0                  
132900       PERFORM IMS-REPL-ARTM                                              
133000     ELSE                                                                 
133100       PERFORM IMS-DLET-ARTM                                              
133200     END-IF                                                               
133300     END-IF                                                               
133400     .                                                                    
133500     EJECT                                                                
133600 EB-BEH-NY-TITPO-OCH-OEKN-KVART SECTION.                                  
133700                                                                          
133800     PERFORM EBA-UPPDAT-SUTPO-TOT                                         
133900     PERFORM EBB-MINSKA-SUTPO-GAMMAL-VECKA                                
134000     PERFORM EBC-UPPDAT-SUTPO-NY-VECKA                                    
134100     MOVE RAD-TITPO          TO W-SPAR-TITPO                              
134200     MOVE WS-TIAAMMDD        TO RAD-TITPO                                 
134300     MOVE MID-KVART-OPP      TO RAD-KVART                                 
134400     PERFORM IMS-REPL-ORDP01                                              
134500     MOVE 71 TO WS-KDORDBEK                                               
134600     PERFORM S06-ORDER-BEKR                                               
134700     PERFORM S11-SKAPA-RY9-TRANS                                          
134800     MOVE ZERO TO RY9-KVART                                               
134900     MOVE W-SPAR-TITPO TO RY9-TITPO                                       
135000     PERFORM S10-SKRIV-RY9                                                
135100     .                                                                    
135200     EJECT                                                                
135300 EBA-UPPDAT-SUTPO-TOT SECTION.                                            
135400                                                                          
135500     PERFORM IMS-GHU-ARTM                                                 
135600     COMPUTE W-SPAR-KVART = MID-KVART-OPP - RAD-KVART                     
135700     ADD W-SPAR-KVART TO ARTM-ART-SUTPO-TOT                               
135800     PERFORM IMS-REPL-ARTM                                                
135900     .                                                                    
136000     SKIP3                                                                
136100 EBB-MINSKA-SUTPO-GAMMAL-VECKA SECTION.                                   
136200                                                                          
136300     PERFORM S05-KONV-TITPO                                               
136400     PERFORM IMS-GHU-ARTM-ANT-GE                                          
136500     IF SEGMENT-FINNS                                                     
136600     IF RAD-KDTPOTYP = 6                                                  
136700       SUBTRACT RAD-KVART FROM ARTM-ANT-SUTPO-EJPB                        
136800     ELSE                                                                 
136900       SUBTRACT RAD-KVART FROM ARTM-ANT-SUTPO-PB                          
137000     END-IF                                                               
137100     IF ARTM-ANT-SUTPO-PB > 0 OR ARTM-ANT-SUTPO-EJPB > 0                  
137200       PERFORM IMS-REPL-ARTM                                              
137300     ELSE                                                                 
137400       PERFORM IMS-DLET-ARTM                                              
137500     END-IF                                                               
137600     END-IF                                                               
137700     .                                                                    
137800     EJECT                                                                
137900 EBC-UPPDAT-SUTPO-NY-VECKA SECTION.                                       
138000                                                                          
138100     MOVE MID-TITPO-OPP TO WS-TIAAVVD-MID                                 
138200     MOVE WS-TIAAVV-MID TO W-DABEHOV                                      
138300     IF WS-TIAAVV-MID NOT = ZERO                                          
138400       IF WS-TIAAVV-MID < 5000                                            
138500         MOVE 20          TO W-DABEHOV (1:2)                              
138600       ELSE                                                               
138700         IF WS-TIAAVV-MID < 9999                                          
138800           MOVE 19        TO W-DABEHOV (1:2)                              
138900         ELSE                                                             
139000           MOVE 9999      TO W-DABEHOV                                    
139100         END-IF                                                           
139200       END-IF                                                             
139300     END-IF                                                               
139400     PERFORM IMS-GHU-ARTM-ANT-GE                                          
139500     IF SEGMENT-FINNS                                                     
139600       IF RAD-KDTPOTYP = 6                                                
139700         ADD MID-KVART-OPP  TO ARTM-ANT-SUTPO-EJPB                        
139800       ELSE                                                               
139900         ADD MID-KVART-OPP  TO ARTM-ANT-SUTPO-PB                          
140000       END-IF                                                             
140100       PERFORM IMS-REPL-ARTM                                              
140200     ELSE                                                                 
140300       MOVE W-DABEHOV       TO ARTM-ANT-DABEHOV                           
140400       IF RAD-KDTPOTYP = 6                                                
140500         MOVE MID-KVART-OPP TO ARTM-ANT-SUTPO-EJPB                        
140600         MOVE ZERO          TO ARTM-ANT-SUTPO-PB                          
140700       ELSE                                                               
140800         MOVE MID-KVART-OPP TO ARTM-ANT-SUTPO-PB                          
140900         MOVE ZERO          TO ARTM-ANT-SUTPO-EJPB                        
141000       END-IF                                                             
141100       PERFORM IMS-ISRT-ARTM-ANT                                          
141200     END-IF                                                               
141300     .                                                                    
141400     EJECT                                                                
141500 EC-BEH-OEKN-KVART SECTION.                                               
141600                                                                          
141700     PERFORM EBA-UPPDAT-SUTPO-TOT                                         
141800     PERFORM ECA-UPPD-SUTPO-OEKNING                                       
141900     MOVE MID-KVART-OPP   TO RAD-KVART                                    
142000     PERFORM IMS-REPL-ORDP01                                              
142100     MOVE 71 TO WS-KDORDBEK                                               
142200     PERFORM S06-ORDER-BEKR                                               
142300     PERFORM S11-SKAPA-RY9-TRANS                                          
142400     .                                                                    
142500     SKIP3                                                                
142600 ECA-UPPD-SUTPO-OEKNING SECTION.                                          
142700                                                                          
142800     PERFORM S05-KONV-TITPO                                               
142900     PERFORM IMS-GHU-ARTM-ANT-GE                                          
143000     IF SEGMENT-FINNS                                                     
143100     ADD W-SPAR-KVART TO ARTM-ANT-SUTPO-PB                                
143200     PERFORM IMS-REPL-ARTM                                                
143300     END-IF                                                               
143400     .                                                                    
143500     EJECT                                                                
143600 ED-BEH-NY-TITPO SECTION.                                                 
143700                                                                          
143800     PERFORM EBB-MINSKA-SUTPO-GAMMAL-VECKA                                
143900     MOVE RAD-TITPO TO W-SPAR-TITPO                                       
144000     MOVE WS-TIAAMMDD TO RAD-TITPO                                        
144100     PERFORM EAA-UPPD-SUTPO-NY-VECKA                                      
144200     PERFORM IMS-REPL-ORDP01                                              
144300     MOVE 71 TO WS-KDORDBEK                                               
144400     PERFORM S06-ORDER-BEKR                                               
144500     IF RAD-KDTPOTYP = 6                                                  
144600       CONTINUE                                                           
144700     ELSE                                                                 
144800       PERFORM S11-SKAPA-RY9-TRANS                                        
144900       MOVE ZERO TO RY9-KVART                                             
145000       MOVE W-SPAR-TITPO TO RY9-TITPO                                     
145100       PERFORM S10-SKRIV-RY9                                              
145200     END-IF                                                               
145300     .                                                                    
145400     EJECT                                                                
145500 F-DELETE-RAD SECTION.                                                    
145600                                                                          
145700     IF MID-KVART-OPP > 0                                                 
145800       MOVE JA TO W-UPDATE DELANNULL-SW                                   
145900       MOVE MID-KVART-OPP TO WS-MINSKNING                                 
146000     ELSE                                                                 
146100       PERFORM S04-FLYTTA-RO-NYCKLAR                                      
146200       PERFORM IMS-GHU-ORDP01                                             
146300                                                                          
146400       IF SEGMENT-FINNS                                                   
146500         MOVE JA            TO W-UPDATE                                   
146600         MOVE RAD-KVART     TO WS-MINSKNING                               
146700       END-IF                                                             
146800     END-IF                                                               
146900                                                                          
147000     IF W-UPDATE = JA                                                     
147100       PERFORM S09-HAEMTA-TIDISPIN                                        
147200       PERFORM S05-KONV-TITPO                                             
147300       PERFORM IMS-GHU-ARTM                                               
147400       IF ARTM-ART-SUTPO-TOT > WS-MINSKNING                               
147500         SUBTRACT WS-MINSKNING FROM                                       
147600                      ARTM-ART-SUTPO-TOT                                  
147700       ELSE                                                               
147800         MOVE ZERO TO ARTM-ART-SUTPO-TOT                                  
147900       END-IF                                                             
148000       PERFORM IMS-REPL-ARTM                                              
148100       PERFORM IMS-GHU-ARTM-ANT-GE                                        
148200       IF SEGMENT-FINNS                                                   
148300       SUBTRACT WS-MINSKNING FROM ARTM-ANT-SUTPO-PB                       
148400       IF ARTM-ANT-SUTPO-PB > 0 OR ARTM-ANT-SUTPO-EJPB > 0                
148500         PERFORM IMS-REPL-ARTM                                            
148600       ELSE                                                               
148700         PERFORM IMS-DLET-ARTM                                            
148800       END-IF                                                             
148900       END-IF                                                             
149000       SUBTRACT WS-MINSKNING FROM RAD-KVART                               
149100                                                                          
149200       IF DELANNULLATION                                                  
149300         PERFORM IMS-REPL-ORDP01                                          
149400         MOVE 71 TO WS-KDORDBEK                                           
149500         PERFORM S06-ORDER-BEKR                                           
149600         MOVE 85 TO WS-KDORDBEK                                           
149700         PERFORM S06-ORDER-BEKR                                           
149800         PERFORM S11-SKAPA-RY9-TRANS                                      
149900       ELSE                                                               
150000         PERFORM IMS-DLET-ORDP01                                          
150100         MOVE 85 TO WS-KDORDBEK                                           
150200         PERFORM S06-ORDER-BEKR                                           
150300         MOVE ZERO TO RAD-KVART                                           
150400         PERFORM S11-SKAPA-RY9-TRANS                                      
150500       END-IF                                                             
150600                                                                          
150700     END-IF                                                               
150800     .                                                                    
150900     EJECT                                                                
151000 G-OPPNA-RAD SECTION.                                                     
151100                                                                          
151200* I DENNA SEKTION FLYTTAS MARKERAD RAD TILL RAD 1, FÖRÄNDRINGS-           
151300* RADEN ÖPPNAS OCH ÖVRIGA RENSAS                                          
151400                                                                          
151500     PERFORM S04-FLYTTA-RO-NYCKLAR                                        
151600                                                                          
151700     PERFORM IMS-GU-ORDP01-2                                              
151800                                                                          
151900     IF SEGMENT-FINNS                                                     
152000        MOVE RAD-IDARTNR          TO  MOD-IDARTNR(+1)                     
152100        MOVE RAD-IDDISTR          TO  MOD-IDDISTR(+1)                     
152200        MOVE RAD-IDKUNDNR         TO  MOD-IDKUNDNR(+1)                    
152300        MOVE RAD-IDKUNDRF         TO  WS-IDKUNDRF                         
152400        MOVE WS-IDKUNDRF-1-5      TO  MOD-IDORDNR(+1)                     
152500        MOVE RAD-KDTPOTYP         TO  MOD-KDTPOTYP(+1)                    
152600        MOVE RAD-KVART            TO  MOD-KVART  (+1)                     
152700        IF RAD-DARODAT > ZERO                                             
152800           MOVE RAD-DARODAT (3:6) TO  MOD-TIRODAT(+1)                     
152900        ELSE                                                              
153000           MOVE SPACE             TO  MOD-TIRODAT(+1)                     
153100        END-IF                                                            
153200        MOVE RAD-TIRES            TO  WS-TIRES                            
153300        IF WS-TIRES > ZERO                                                
153400           MOVE WS-TIRES          TO  MOD-TIRES  (+1)                     
153500        ELSE                                                              
153600           MOVE SPACE             TO  MOD-TIRES  (+1)                     
153700        END-IF                                                            
153800        MOVE RAD-KDORDKL          TO  MOD-KDORDKL(+1)                     
154100        MOVE RAD-KDROO            TO  MOD-KDROO(+1)                       
154200        MOVE RAD-IDANSK           TO  MOD-IDANSK(+1)                      
154300                                                                          
154400        IF RAD-TITPO    > ZERO                                            
154500           MOVE RAD-TITPO            TO DAT-I-TIDATUM                     
154600           MOVE 'AAMMDD'             TO DAT-KDDATFORM                     
154700           CALL WDATKONV          USING DAT-KDDATFORM                     
154800                                        DAT-I-TIDATUM                     
154900                                        DAT-O-TIDATUM                     
155000                                        DAT-KDSVAR                        
155100           MOVE DAT-TIAAVVD          TO MOD-TITPO(+1)                     
155200        END-IF                                                            
155300                                                                          
155400        MOVE RAD-KDRAPRIO         TO  MOD-KDRAPRIO(+1)                    
155500        MOVE RAD-IDLOPNR          TO  MOD-IDLOPNR (+1)                    
155600        PERFORM MFS-ROR-EJ-SPARADE-NYCKLAR                                
155700        MOVE RAD-IDDISTR          TO  TEST-IDDISTR                        
155800        IF NOT DIST19-SATS                                                
155900           MOVE MFS-OEPPNA-NUM-FAELT TO  MOD-KVART-OPP-ATTR               
156000        END-IF                                                            
156100        IF RAD-KDSTARAD = '1'                                             
156200           MOVE MFS-OEPPNA-NUM-FAELT TO MOD-TITPO-OPP-ATTR                
156300        END-IF                                                            
156400     ELSE                                                                 
156500        MOVE TEXT-0403 (SPRAK-IX) TO MOD-TEMFSFEL                         
156600     END-IF                                                               
156700     MOVE +1                    TO MOD-KVRAD-SPAR                         
156800                                                                          
156900     MOVE +2 TO INDX                                                      
157000     PERFORM MFS-RENSA-FALT                                               
157100     .                                                                    
157200     EJECT                                                                
157300 H-FELTEXT SECTION.                                                       
157400                                                                          
157500     MOVE MID-IDARTNR (+1)      TO MOD-IDARTNR (+1)                       
157600     MOVE MID-IDDISTR (+1)      TO MOD-IDDISTR (+1)                       
157700     MOVE MID-IDKUNDNR(+1)      TO MOD-IDKUNDNR(+1)                       
157800     MOVE MID-IDORDNR7(+1)      TO MOD-IDORDNR (+1)                       
157900     MOVE MID-KDTPOTYP(+1)      TO MOD-KDTPOTYP(+1)                       
158000     MOVE MID-KVART   (+1)      TO MOD-KVART   (+1)                       
158100     MOVE MID-TIRODAT (+1)      TO MOD-TIRODAT (+1)                       
158200     MOVE MID-TIRES   (+1)      TO MOD-TIRES   (+1)                       
158300     MOVE MID-KDORDKL (+1)      TO MOD-KDORDKL (+1)                       
158500     MOVE MID-KDROO   (+1)      TO MOD-KDROO   (+1)                       
158600     MOVE MID-IDANSK  (+1)      TO MOD-IDANSK  (+1)                       
158700     MOVE MID-TITPO   (+1)      TO MOD-TITPO   (+1)                       
158800     MOVE MID-KDRAPRIO(+1)      TO MOD-KDRAPRIO(+1)                       
158900     MOVE MID-IDLOPNR (+1)      TO MOD-IDLOPNR (+1)                       
159000     MOVE +1                    TO MOD-KVRAD-SPAR                         
159100                                                                          
159200     MOVE RAD-IDARTNR           TO MOD-IDARTNR-SPAR-E                     
159300     MOVE RAD-KDRAPRIO          TO MOD-KDRAPRIO-SPAR-E                    
159400     MOVE RAD-DARODAT (3:6)     TO MOD-TIRODAT-SPAR-E                     
159410     MOVE RAD-TIREGTID          TO MOD-TIREGTID-SPAR-E                    
159500     MOVE RAD-IDDISTR           TO MOD-IDDISTR-SPAR-E                     
159600     MOVE RAD-IDKUNDNR          TO MOD-IDKUNDNR-SPAR-E                    
159700     MOVE RAD-IDKUNDRF          TO MOD-IDKUNDRF-SPAR-E                    
159800     MOVE RAD-IDLOPNR           TO MOD-IDLOPNR-SPAR-E                     
159900     MOVE RAD-IDANSK            TO MOD-IDANSK-SPAR-E                      
160000     MOVE RAD-KDROO             TO MOD-KDROO-SPAR-E                       
160300                                                                          
160400     IF ANTAL-OK = NEJ                                                    
160500        MOVE MFS-NUM-FAELT-FEL  TO MOD-KVART-OPP-ATTR                     
160600     ELSE                                                                 
160700        MOVE MFS-NUM-FAELT-RAETT TO MOD-KVART-OPP-ATTR                    
160800     END-IF                                                               
160900                                                                          
161000     IF RAD-KDSTARAD = '1'                                                
161100        IF TITPO-OK = NEJ                                                 
161200           MOVE MFS-NUM-FAELT-FEL TO MOD-TITPO-OPP-ATTR                   
161300        ELSE                                                              
161400           MOVE MFS-NUM-FAELT-RAETT TO MOD-TITPO-OPP-ATTR                 
161500        END-IF                                                            
161600     END-IF                                                               
161700                                                                          
161800     MOVE MFS-ROER-EJ-FAELT           TO MOD-KVART-OPP                    
161900     MOVE MFS-ROER-EJ-FAELT           TO MOD-TITPO-OPP                    
162000                                                                          
162100     IF ANTAL-OK = NEJ                                                    
162200        IF KVQPACK-OK = NEJ                                               
162300           MOVE TEXT-0419 (SPRAK-IX)  TO MOD-TEMFSFEL                     
162400        ELSE                                                              
162500           MOVE TEXT-0409 (SPRAK-IX)  TO MOD-TEMFSFEL                     
162600        END-IF                                                            
162700     END-IF                                                               
162800     .                                                                    
162900     EJECT                                                                
163000 S01-LAES-WDA5-PF7 SECTION.                                               
163100                                                                          
163200     MOVE LOW-VALUE           TO W-SOK-NYCKLAR-MIN                        
163300     MOVE HIGH-VALUE          TO W-SOK-NYCKLAR-MAX                        
163400                                                                          
163500     IF W-IDDISTR      > ZERO                                             
163600        MOVE W-IDDISTR        TO W-IDDISTR-MIN                            
163700                                 W-IDDISTR-MAX                            
163800     END-IF                                                               
163900                                                                          
164000     IF W-KDTPOTYP-FOM     > ZERO                                         
164100        MOVE W-KDTPOTYP-FOM   TO W-KDTPOTYP-MIN                           
164200     END-IF                                                               
164300                                                                          
164400     IF W-KDTPOTYP-TOM     > ZERO                                         
164500        MOVE W-KDTPOTYP-TOM   TO W-KDTPOTYP-MAX                           
164600     END-IF                                                               
164700                                                                          
164800     IF W-KDSTARAD     > ZERO                                             
164900        MOVE W-KDSTARAD       TO W-KDSTARAD-MIN                           
165000                                 W-KDSTARAD-MAX                           
165100     ELSE                                                                 
165200        MOVE '1'              TO W-KDSTARAD-MIN                           
165300        MOVE '3'              TO W-KDSTARAD-MAX                           
165400     END-IF                                                               
165500                                                                          
165600     IF W-ORDQ01-NYCKLAR = JA                                             
165700        MOVE LOW-VALUE        TO W-WDA5A1KY-MIN                           
165800        MOVE HIGH-VALUE       TO W-WDA5A1KY-MAX                           
165900                                                                          
166000        MOVE W-IDARTNR        TO W-IDARTNR-N3-MIN                         
166100                                 W-IDARTNR-N3-MAX                         
166200        MOVE WC-CDC-SE        TO W-IDDC-N3-MAX                            
166300                                                                          
166400        IF W-IDANSK       > ZERO                                          
166500           MOVE W-IDANSK      TO W-IDANSK-MIN                             
166600                                 W-IDANSK-MAX                             
166700        END-IF                                                            
166800                                                                          
166900        IF W-KDROO        > ZERO                                          
167000          MOVE W-KDROO        TO W-KDROO-MIN                              
167100                                 W-KDROO-MAX                              
167200        END-IF                                                            
167300                                                                          
167400        PERFORM IMS-GN-ORDQ01                                             
167500                                                                          
167600        PERFORM S03-LAES-WDA5                                             
167700     ELSE                                                                 
167800        MOVE LOW-VALUE                TO W-WDA5C1KY-MIN                   
167900        MOVE HIGH-VALUE               TO W-WDA5C1KY-MAX                   
168000                                                                          
168100        MOVE MID-IDANSK-SPAR-E        TO W-IDANSK-N1-MIN                  
168200                                         W-IDANSK-N1-MAX                  
168300        MOVE MID-KDROO-SPAR-E         TO W-KDROO-N1-MIN                   
168400                                         W-KDROO-N1-MAX                   
168500        IF W-IDARTNR > ZERO                                               
168600           MOVE MID-IDARTNR-SPAR-E    TO W-IDARTNR-N1-MIN                 
168700                                         W-IDARTNR-N1-MAX                 
168800           IF W-IDDISTR > ZERO                                            
168900              MOVE MID-IDDISTR-SPAR-E TO W-IDDISTR-N1-MIN                 
169000                                         W-IDDISTR-N1-MAX                 
169100           END-IF                                                         
169200        END-IF                                                            
169300                                                                          
169400        PERFORM IMS-GN-OWDS01                                             
169500                                                                          
169600        PERFORM S03-LAES-WDA5                                             
169700     END-IF                                                               
169800     .                                                                    
169900     EJECT                                                                
170000 S02-LAES-WDA5-ENTER SECTION.                                             
170100                                                                          
170200     MOVE LOW-VALUE                      TO W-SOK-NYCKLAR-MIN             
170300     MOVE HIGH-VALUE                     TO W-SOK-NYCKLAR-MAX             
170400                                                                          
170500     IF W-IDDISTR       > ZERO                                            
170600        MOVE W-IDDISTR                   TO W-IDDISTR-MIN                 
170700                                            W-IDDISTR-MAX                 
170800     END-IF                                                               
170900                                                                          
171000     IF W-KDTPOTYP-FOM      > ZERO                                        
171100        MOVE W-KDTPOTYP-FOM              TO W-KDTPOTYP-MIN                
171200     END-IF                                                               
171300                                                                          
171400     IF W-KDTPOTYP-TOM      > ZERO                                        
171500        MOVE W-KDTPOTYP-TOM              TO W-KDTPOTYP-MAX                
171600     END-IF                                                               
171700                                                                          
171800     IF W-KDSTARAD      > ZERO                                            
171900        MOVE W-KDSTARAD                  TO W-KDSTARAD-MIN                
172000                                            W-KDSTARAD-MAX                
172100     ELSE                                                                 
172200        MOVE '1'                         TO W-KDSTARAD-MIN                
172300        MOVE '3'                         TO W-KDSTARAD-MAX                
172400     END-IF                                                               
172500                                                                          
172600     IF W-ORDQ01-NYCKLAR = JA                                             
172700        IF W-IDANSK        > ZERO                                         
172800           MOVE W-IDANSK                 TO W-IDANSK-MIN                  
172900                                            W-IDANSK-MAX                  
173000        END-IF                                                            
173100                                                                          
173200        IF W-KDROO         > ZERO                                         
173300           MOVE W-KDROO                  TO W-KDROO-MIN                   
173400                                            W-KDROO-MAX                   
173500        END-IF                                                            
173600                                                                          
173700        IF MID-IDARTNR-IN      NOT = ALL '+' OR                           
173800           MID-IDDISTR-IN      NOT = ALL '+' OR                           
173900           MID-IDANSK-IN       NOT = ALL '+' OR                           
174000           MID-KDROO-IN        NOT = ALL '+' OR                           
174100           MID-KDTPOTYP-FOM-IN NOT = ALL '+' OR                           
174200           MID-KDTPOTYP-TOM-IN NOT = ALL '+' OR                           
174300           MID-KDSTARAD-IN     NOT = ALL '+'                              
174400                                                                          
174500           MOVE LOW-VALUE                TO W-WDA5A1KY-MIN                
174600           MOVE HIGH-VALUE               TO W-WDA5A1KY-MAX                
174700                                                                          
174800           MOVE W-IDARTNR                TO W-IDARTNR-N3-MIN              
174900                                            W-IDARTNR-N3-MAX              
175000           MOVE WC-CDC-SE                TO W-IDDC-N3-MAX                 
175100           PERFORM IMS-GN-ORDQ01                                          
175200        ELSE                                                              
175300*    VID ENTER UTAN NYA NYCKLAR                                           
175400                                                                          
175500           MOVE MID-IDARTNR-SPAR-E       TO W-IDARTNR-N3-MIN              
175600           MOVE WC-CDC-SE                TO W-IDDC-N3-MIN                 
175700           MOVE MID-KDRAPRIO-SPAR-E      TO W-KDRAPRIO-N3-MIN             
175800           MOVE MID-TIRODAT-SPAR-E       TO W-DARODAT-N3-MIN              
175900           IF MID-TIRODAT-SPAR-E NOT = ZERO                               
176000             IF MID-TIRODAT-SPAR-E < 500000                               
176100               MOVE 20                   TO W-DARODAT-N3-MIN (1:2)        
176200             ELSE                                                         
176300               IF MID-TIRODAT-SPAR-E < 999999                             
176400                 MOVE 19                 TO W-DARODAT-N3-MIN (1:2)        
176500               ELSE                                                       
176600                 MOVE 99999999           TO W-DARODAT-N3-MIN              
176700               END-IF                                                     
176800             END-IF                                                       
176900           END-IF                                                         
176910           MOVE MID-TIREGTID-SPAR-E      TO W-TIREGTID-N3-MIN             
177000           MOVE MID-IDDISTR-SPAR-E       TO W-IDDISTR-N3-MIN              
177100           MOVE MID-IDKUNDNR-SPAR-E      TO W-IDKUNDNR-N3-MIN             
177200           MOVE MID-IDKUNDRF-SPAR-E      TO W-IDKUNDRF-N3-MIN             
177300           MOVE MID-IDLOPNR-SPAR-E       TO W-IDLOPNR-N3-MIN              
177400                                                                          
177500           PERFORM IMS-GU-ORDQ01                                          
177600                                                                          
177700           MOVE LOW-VALUE                TO W-WDA5A1KY-MIN                
177800           MOVE HIGH-VALUE               TO W-WDA5A1KY-MAX                
177900           MOVE MID-IDARTNR-SPAR-E       TO W-IDARTNR-N3-MIN              
178000                                            W-IDARTNR-N3-MAX              
178100           MOVE WC-CDC-SE                TO W-IDDC-N3-MAX                 
178200           IF SEGMENT-SAKNAS                                              
178300              PERFORM IMS-GN-ORDQ01                                       
178400           END-IF                                                         
178500                                                                          
178600        END-IF                                                            
178700        PERFORM S03-LAES-WDA5                                             
178800     ELSE                                                                 
178900        IF MID-IDARTNR-IN      NOT = ALL '+' OR                           
179000           MID-IDDISTR-IN      NOT = ALL '+' OR                           
179100           MID-IDANSK-IN       NOT = ALL '+' OR                           
179200           MID-KDROO-IN        NOT = ALL '+' OR                           
179300           MID-KDTPOTYP-FOM-IN NOT = ALL '+' OR                           
179400           MID-KDTPOTYP-TOM-IN NOT = ALL '+' OR                           
179500           MID-KDSTARAD-IN     NOT = ALL '+' OR                           
179600          (MID-IDANSK-SPAR-E       = ALL '0' AND                          
179700           MID-KDROO-SPAR-E        = ALL '0')                             
179800                                                                          
179900           MOVE LOW-VALUE                TO W-WDA5C1KY-MIN                
180000           MOVE HIGH-VALUE               TO W-WDA5C1KY-MAX                
180100                                                                          
180200           MOVE W-IDANSK                 TO W-IDANSK-N1-MIN               
180300                                            W-IDANSK-N1-MAX               
180400           MOVE W-KDROO                  TO W-KDROO-N1-MIN                
180500                                            W-KDROO-N1-MAX                
180600                                                                          
180700           IF W-IDARTNR > ZERO                                            
180800              MOVE W-IDARTNR             TO W-IDARTNR-N1-MIN              
180900                                            W-IDARTNR-N1-MAX              
181000              IF W-IDDISTR > ZERO                                         
181100                 MOVE W-IDDISTR          TO W-IDDISTR-N1-MIN              
181200                                            W-IDDISTR-N1-MAX              
181300              END-IF                                                      
181400           END-IF                                                         
181500                                                                          
181600           PERFORM IMS-GN-OWDS01                                          
181700        ELSE                                                              
181800*    VID ENTER UTAN NYA NYCKLAR                                           
181900                                                                          
182000           MOVE MID-IDANSK-SPAR-E        TO W-IDANSK-N1-MIN               
182100           MOVE MID-KDROO-SPAR-E         TO W-KDROO-N1-MIN                
182200           MOVE MID-IDARTNR-SPAR-E       TO W-IDARTNR-N1-MIN              
182300           MOVE MID-IDDISTR-SPAR-E       TO W-IDDISTR-N1-MIN              
182400           MOVE MID-IDKUNDNR-SPAR-E      TO W-IDKUNDNR-N1-MIN             
182500           MOVE MID-IDKUNDRF-SPAR-E      TO W-IDKUNDRF-N1-MIN             
182600           MOVE MID-IDLOPNR-SPAR-E       TO W-IDLOPNR-N1-MIN              
182700                                                                          
182800           PERFORM IMS-GU-OWDS01                                          
182900                                                                          
183000           MOVE LOW-VALUE                TO W-WDA5C1KY-MIN                
183100           MOVE HIGH-VALUE               TO W-WDA5C1KY-MAX                
183200           MOVE MID-IDANSK-SPAR-E        TO W-IDANSK-N1-MIN               
183300                                            W-IDANSK-N1-MAX               
183400           MOVE MID-KDROO-SPAR-E         TO W-KDROO-N1-MIN                
183500                                            W-KDROO-N1-MAX                
183600           IF W-IDARTNR > ZERO                                            
183700              MOVE MID-IDARTNR-SPAR-E    TO W-IDARTNR-N1-MIN              
183800                                            W-IDARTNR-N1-MAX              
183900              IF W-IDDISTR > ZERO                                         
184000                 MOVE MID-IDDISTR-SPAR-E TO W-IDDISTR-N1-MIN              
184100                                            W-IDDISTR-N1-MAX              
184200              END-IF                                                      
184300           END-IF                                                         
184400                                                                          
184500           IF SEGMENT-SAKNAS                                              
184600              PERFORM IMS-GN-OWDS01                                       
184700           END-IF                                                         
184800                                                                          
184900        END-IF                                                            
185000        PERFORM S03-LAES-WDA5                                             
185100     END-IF                                                               
185200     .                                                                    
185300     EJECT                                                                
185400 S03-LAES-WDA5     SECTION.                                               
185500     SKIP2                                                                
185600     IF W-ORDQ01-NYCKLAR = JA                                             
185700        MOVE +1 TO INDX                                                   
185800        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
185900                      SEGMENT-SLUT   OR                                   
186000                      INDX > MAX-LINE                                     
186100                                                                          
186200           MOVE SEQA-IDDISTR    TO  W-IDDISTR-N2                          
186300           MOVE SEQA-IDKUNDNR   TO  W-IDKUNDNR-N2                         
186400           MOVE SEQA-IDKUNDRF   TO  W-IDKUNDRF-N2                         
186500           MOVE SEQA-IDARTNR    TO  W-IDARTNR-N2                          
186600           MOVE SEQA-IDLOPNR    TO  W-IDLOPNR-N2                          
186700                                                                          
186800           PERFORM IMS-GU-ORDP01-1                                        
186900           IF SEGMENT-FINNS                                               
187000              PERFORM S03A-FLYTTA-TILL-MOD                                
187100              ADD +1 TO W-KVRAD                                           
187200              ADD +1 TO INDX                                              
187300           END-IF                                                         
187400                                                                          
187500           PERFORM IMS-GN-ORDQ01                                          
187600                                                                          
187700        END-PERFORM                                                       
187800                                                                          
187900        IF SEGMENT-FINNS                                                  
188000           MOVE SEQA-IDDISTR    TO  W-IDDISTR-N2                          
188100           MOVE SEQA-IDKUNDNR   TO  W-IDKUNDNR-N2                         
188200           MOVE SEQA-IDKUNDRF   TO  W-IDKUNDRF-N2                         
188300           MOVE SEQA-IDARTNR    TO  W-IDARTNR-N2                          
188400           MOVE SEQA-IDLOPNR    TO  W-IDLOPNR-N2                          
188500                                                                          
188600           PERFORM IMS-GU-ORDP01-1                                        
188700           IF SEGMENT-FINNS                                               
188800              PERFORM S03B-FLYTTA-TILL-SPAR-MOD                           
188900           END-IF                                                         
189000        ELSE                                                              
189100           MOVE ZERO TO MOD-SPARADE-NYCKLAR                               
189200        END-IF                                                            
189300        MOVE W-KVRAD TO MOD-KVRAD-SPAR                                    
189400                                                                          
189500        IF INDX = +1                                                      
189600           MOVE TEXT-0403 (SPRAK-IX) TO MOD-TEMFSFEL                      
189700        END-IF                                                            
189800                                                                          
189900        PERFORM MFS-RENSA-FALT                                            
190000     ELSE                                                                 
190100        MOVE +1 TO INDX                                                   
190200        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
190300                      SEGMENT-SLUT   OR                                   
190400                      INDX > MAX-LINE                                     
190500           MOVE SEQC-IDDISTR    TO  W-IDDISTR-N2                          
190600           MOVE SEQC-IDKUNDNR   TO  W-IDKUNDNR-N2                         
190700           MOVE SEQC-IDKUNDRF   TO  W-IDKUNDRF-N2                         
190800           MOVE SEQC-IDARTNR    TO  W-IDARTNR-N2                          
190900           MOVE SEQC-IDLOPNR    TO  W-IDLOPNR-N2                          
191000                                                                          
191100           PERFORM IMS-GU-ORDP01-2                                        
191200           IF SEGMENT-FINNS                                               
191300              PERFORM S03A-FLYTTA-TILL-MOD                                
191400              ADD +1 TO W-KVRAD                                           
191500              ADD +1 TO INDX                                              
191600           END-IF                                                         
191700           PERFORM IMS-GN-OWDS01                                          
191800        END-PERFORM                                                       
191900                                                                          
192000        IF SEGMENT-FINNS                                                  
192100           MOVE SEQC-IDDISTR    TO  W-IDDISTR-N2                          
192200           MOVE SEQC-IDKUNDNR   TO  W-IDKUNDNR-N2                         
192300           MOVE SEQC-IDKUNDRF   TO  W-IDKUNDRF-N2                         
192400           MOVE SEQC-IDARTNR    TO  W-IDARTNR-N2                          
192500           MOVE SEQC-IDLOPNR    TO  W-IDLOPNR-N2                          
192600                                                                          
192700           PERFORM IMS-GU-ORDP01-2                                        
192800           IF SEGMENT-FINNS                                               
192900              PERFORM S03B-FLYTTA-TILL-SPAR-MOD                           
193000           END-IF                                                         
193100        ELSE                                                              
193200           MOVE ZERO TO MOD-SPARADE-NYCKLAR                               
193300        END-IF                                                            
193400        MOVE W-KVRAD TO MOD-KVRAD-SPAR                                    
193500                                                                          
193600        IF INDX = +1                                                      
193700           MOVE TEXT-0403 (SPRAK-IX) TO MOD-TEMFSFEL                      
193800        END-IF                                                            
193900                                                                          
194000        PERFORM MFS-RENSA-FALT                                            
194100     END-IF                                                               
194200     .                                                                    
194300     EJECT                                                                
194400 S03A-FLYTTA-TILL-MOD SECTION.                                            
194500                                                                          
194600     IF RAD-FLTPOBEK = NEJ                                                
194700        MOVE '*'         TO MOD-VALKOD   (INDX)                           
194800     END-IF                                                               
194900     MOVE RAD-IDARTNR    TO MOD-IDARTNR  (INDX)                           
195000     MOVE RAD-IDDISTR    TO MOD-IDDISTR  (INDX)                           
195100     MOVE RAD-IDKUNDNR   TO MOD-IDKUNDNR (INDX)                           
195200     MOVE RAD-IDKUNDRF   TO WS-IDKUNDRF                                   
195300     MOVE WS-IDKUNDRF-1-5 TO MOD-IDORDNR (INDX)                           
195400     MOVE RAD-KDTPOTYP   TO MOD-KDTPOTYP (INDX)                           
195500     MOVE RAD-KVART      TO MOD-KVART    (INDX)                           
195600     IF RAD-DARODAT > ZERO                                                
195700        MOVE RAD-DARODAT (3:6) TO MOD-TIRODAT  (INDX)                     
195800     ELSE                                                                 
195900        MOVE SPACE             TO MOD-TIRODAT  (INDX)                     
196000     END-IF                                                               
196100     MOVE RAD-TIRES           TO WS-TIRES                                 
196200     IF WS-TIRES > ZERO                                                   
196300        MOVE WS-TIRES         TO  MOD-TIRES  (INDX)                       
196400     ELSE                                                                 
196500        MOVE SPACE            TO  MOD-TIRES  (INDX)                       
196600     END-IF                                                               
196700     MOVE RAD-KDORDKL    TO MOD-KDORDKL  (INDX)                           
197000     MOVE RAD-KDROO      TO MOD-KDROO    (INDX)                           
197100     MOVE RAD-IDANSK     TO MOD-IDANSK   (INDX)                           
197200                                                                          
197300     IF RAD-TITPO > ZERO                                                  
197400        MOVE RAD-TITPO TO DAT-I-TIDATUM                                   
197500        MOVE 'AAMMDD'  TO DAT-KDDATFORM                                   
197600        CALL WDATKONV USING DAT-KDDATFORM                                 
197700                            DAT-I-TIDATUM                                 
197800                            DAT-O-TIDATUM                                 
197900                            DAT-KDSVAR                                    
198000        MOVE DAT-TIAAVVD    TO MOD-TITPO    (INDX)                        
198100     ELSE                                                                 
198200        MOVE SPACE          TO MOD-TITPO    (INDX)                        
198300     END-IF                                                               
198400     MOVE RAD-KDRAPRIO     TO MOD-KDRAPRIO (INDX)                         
198500     MOVE RAD-IDLOPNR      TO MOD-IDLOPNR  (INDX)                         
198600                                                                          
198700     IF INDX = 1                                                          
198800        MOVE RAD-IDARTNR  TO MOD-IDARTNR-SPAR-E                           
198900        MOVE RAD-KDRAPRIO TO MOD-KDRAPRIO-SPAR-E                          
199000        MOVE RAD-DARODAT (3:6)  TO MOD-TIRODAT-SPAR-E                     
199010        MOVE RAD-TIREGTID TO MOD-TIREGTID-SPAR-E                          
199100        MOVE RAD-IDDISTR  TO MOD-IDDISTR-SPAR-E                           
199200        MOVE RAD-IDKUNDNR TO MOD-IDKUNDNR-SPAR-E                          
199300        MOVE RAD-IDKUNDRF TO MOD-IDKUNDRF-SPAR-E                          
199400        MOVE RAD-IDLOPNR  TO MOD-IDLOPNR-SPAR-E                           
199500        MOVE RAD-IDANSK   TO MOD-IDANSK-SPAR-E                            
199600        MOVE RAD-KDROO    TO MOD-KDROO-SPAR-E                             
199900                                                                          
200000     END-IF                                                               
200100     .                                                                    
200200     EJECT                                                                
200300 S03B-FLYTTA-TILL-SPAR-MOD SECTION.                                       
200400                                                                          
200500     MOVE RAD-IDARTNR   TO MOD-IDARTNR-SPAR                               
200600     MOVE RAD-KDRAPRIO  TO MOD-KDRAPRIO-SPAR                              
200700     MOVE RAD-DARODAT (3:6)   TO MOD-TIRODAT-SPAR                         
200710     MOVE RAD-TIREGTID  TO MOD-TIREGTID-SPAR                              
200800     MOVE RAD-IDDISTR   TO MOD-IDDISTR-SPAR                               
200900     MOVE RAD-IDKUNDNR  TO MOD-IDKUNDNR-SPAR                              
201000     MOVE RAD-IDKUNDRF  TO MOD-IDKUNDRF-SPAR                              
201100     MOVE RAD-IDLOPNR   TO MOD-IDLOPNR-SPAR                               
201200     MOVE RAD-IDANSK    TO MOD-IDANSK-SPAR                                
201300     MOVE RAD-KDROO     TO MOD-KDROO-SPAR                                 
201600                                                                          
201700     MOVE TEXT-0402 (SPRAK-IX) TO MOD-TEMFSINF                            
201800     .                                                                    
201900     EJECT                                                                
202000 S04-FLYTTA-RO-NYCKLAR SECTION.                                           
202100                                                                          
202200     INSPECT MID-IDDISTR (INDX) REPLACING LEADING SPACE BY ZERO           
202300     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
202400     INSPECT MID-IDORDNR7(INDX) REPLACING LEADING SPACE BY ZERO           
202500     INSPECT MID-IDARTNR (INDX) REPLACING LEADING SPACE BY ZERO           
202600     INSPECT MID-IDLOPNR (INDX) REPLACING LEADING SPACE BY ZERO           
202700     MOVE MID-IDDISTR (INDX) TO  W-IDDISTR-N2                             
202800     MOVE MID-IDKUNDNR(INDX) TO  W-IDKUNDNR-N2                            
202900     MOVE MID-IDORDNR7(INDX) TO  W-IDORDNR-N2                             
203000     MOVE MID-IDARTNR (INDX) TO  W-IDARTNR-N2                             
203100     MOVE MID-IDLOPNR (INDX) TO  W-IDLOPNR-N2                             
203200     .                                                                    
203300     EJECT                                                                
203400 S05-KONV-TITPO SECTION.                                                  
203500                                                                          
203600     MOVE RAD-TITPO TO DAT-I-TIDATUM                                      
203700     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
203800     CALL WDATKONV USING DAT-KDDATFORM                                    
203900                         DAT-I-TIDATUM                                    
204000                         DAT-O-TIDATUM                                    
204100                         DAT-KDSVAR                                       
204200     IF DAT-KDSVAR-OK                                                     
204300       MOVE DAT-TIAA-VECKA TO WS-TIAA                                     
204400       MOVE DAT-TIVV       TO WS-TIVV                                     
204500       MOVE DAT-TISEKEL    TO WS-TISEKEL                                  
204600       MOVE WS-TIAAAAVV-KONV TO WS-TIAAAAVV                               
204700       MOVE WS-TIAAAAVV    TO W-DABEHOV                                   
204800     ELSE                                                                 
204900       MOVE 'FELAKTIG KONV. AV TITPO' TO FELTEXT                          
205000       CALL ABEND USING RKOD-ABEND                                        
205100     END-IF                                                               
205200********  FIX BORTTAGEN. FUNGERAR INTE EFTER ÅR 2000 **********           
205300***  IF (RAD-TIREGDAT > 0 AND < 961216) AND                               
205400***     (RAD-TITPO  = 961230 OR 961231) AND                               
205500***      W-TIBEHOV = 9652                                                 
205600***      MOVE 9701 TO W-TIBEHOV                                           
205700***  END-IF                                                               
205800     .                                                                    
205900     EJECT                                                                
206000 S06-ORDER-BEKR    SECTION.                                               
206100                                                                          
206200     MOVE RAD-IDDISTR  TO W-IDDISTR-R                                     
206300     MOVE RAD-IDKUNDNR TO W-IDKUNDNR-R                                    
206400     MOVE '00'         TO W-IDKUNDRF-R(1:2)                               
206500     MOVE RAD-IDKUNDRF TO W-IDKUNDRF-R(3:5)                               
206600     PERFORM IMS-GET-ORQI-CSEQ                                            
206700     MOVE ORQI-OHUV-IDORDER   TO WS-IDORDER                               
206800       MOVE W-IDKUNDRF-R            TO WS-IDKUNDRF                        
206900                                                                          
207000     MOVE WS-IDORDER             TO ORQM-OBKR-IDORDER                     
207100     MOVE RAD-IDARTNR            TO ORQM-OBKR-IDARTNR                     
207200     MOVE +1                     TO ORQM-OBKR-IDLOPNR                     
207300                                    ORQM-OBKR-IDSEKVNR                    
207400     MOVE WC-CDC-SE              TO ORQM-OBKR-IDDC                        
207500     MOVE WS-KDORDBEK            TO ORQM-OBKR-KDORDBEK                    
207600     MOVE SPACE                  TO ORQM-OBKR-BEERS                       
207700     MOVE RAD-BEKUNDRF           TO ORQM-OBKR-BEKUNDRF                    
207800     MOVE RAD-BERADREF           TO ORQM-OBKR-BERADREF                    
207900     MOVE RAD-BEVOLREF           TO ORQM-OBKR-BEVOLREF                    
208000     MOVE RAD-IDKAMPRF           TO ORQM-OBKR-IDKAMPRF                    
208100     MOVE ZERO                   TO ORQM-OBKR-DIERS-KVOT                  
208200     MOVE NEJ                    TO ORQM-OBKR-FLAKPLOC                    
208300     MOVE RAD-FLINVEST           TO ORQM-OBKR-FLINVEST                    
208400     MOVE JA                     TO ORQM-OBKR-FLOBOK                      
208500     MOVE NEJ                    TO ORQM-OBKR-FLOBTRAN                    
208600                                    ORQM-OBKR-FLOBPRT                     
208700     MOVE RAD-FLPRTILL           TO ORQM-OBKR-FLPRTILL                    
208800     MOVE JA                     TO ORQM-OBKR-FLRESTN                     
208900     MOVE NEJ                    TO ORQM-OBKR-FLSLATT                     
209000     MOVE RAD-FLERS              TO ORQM-OBKR-FLTILLK                     
209100     MOVE ZERO                   TO ORQM-OBKR-IDARTNR-TILLK               
209200     MOVE RAD-IDDISTR            TO ORQM-OBKR-IDDISTR                     
209300     MOVE RAD-IDKUNDNR           TO ORQM-OBKR-IDKUNDNR                    
209400     MOVE WS-IDKUNDRF            TO ORQM-OBKR-IDKUNDRF                    
209500     MOVE '0000000   '           TO ORQM-OBKR-IDKUNDRF-RO                 
209600     MOVE RAD-IDLEVNR            TO ORQM-OBKR-IDLEVNR                     
209700     MOVE RAD-IDLOPNR            TO ORQM-OBKR-IDLOPNR-RO                  
209800     MOVE RAD-IDSYSTEM           TO ORQM-OBKR-IDSYSTEM                    
209900     MOVE RAD-IDDC-RO            TO ORQM-OBKR-IDDC-RO                     
210000     MOVE PROGRAM-NAMN           TO ORQM-OBKR-IDPGM                       
210100     MOVE RAD-KDDSP              TO ORQM-OBKR-KDDSP                       
210200     MOVE ZERO                   TO ORQM-OBKR-KDERS                       
210300     MOVE RAD-KDOI               TO ORQM-OBKR-KDOI                        
210400     MOVE RAD-CLEARGROUP         TO ORQM-OBKR-CLEARGROUP                  
210500     MOVE RAD-KDKVBRYT           TO ORQM-OBKR-KDKVBRYT                    
210600     MOVE RAD-KDPRTYP            TO ORQM-OBKR-KDPRTYP                     
210700     MOVE RAD-KDTPOTYP           TO ORQM-OBKR-KDTPOTYP                    
210800     MOVE RAD-KDVRINFO           TO ORQM-OBKR-KDVRINFO                    
210900     IF ORQM-OBKR-KDORDBEK = 71 OR 77                                     
211000       MOVE RAD-KVART            TO ORQM-OBKR-KVBEART                     
211100                                    ORQM-OBKR-KVBEART-Q                   
211200       MOVE ZERO                 TO ORQM-OBKR-KVANNANT                    
211300                                    ORQM-OBKR-KVAVBART                    
211400     ELSE                                                                 
211500       MOVE WS-MINSKNING         TO ORQM-OBKR-KVANNANT                    
211600       MOVE ZERO                 TO ORQM-OBKR-KVAVBART                    
211700                                    ORQM-OBKR-KVBEART                     
211800                                    ORQM-OBKR-KVBEART-Q                   
211900     END-IF                                                               
212000     MOVE ZERO                   TO ORQM-OBKR-KVBEART-TILLK               
212100                                    ORQM-OBKR-KVPREAVB                    
212200                                    ORQM-OBKR-KVPRERO                     
212300                                    ORQM-OBKR-KVQPACK                     
212400                                    ORQM-OBKR-KVRO                        
212500                                    ORQM-OBKR-KVSLATT                     
212600     MOVE RAD-PRARTNTO           TO ORQM-OBKR-PRARTNTO                    
212700     MOVE ZERO                   TO ORQM-OBKR-PRBPRIS                     
212800     MOVE RAD-REKSIFFR           TO ORQM-OBKR-REKSIFFR                    
212900     MOVE ZERO                   TO ORQM-OBKR-REKSIFFR-TILLK              
213000                                   ORQM-OBKR-RERF-RAD                     
213100     MOVE WS-TIDISPIN            TO ORQM-OBKR-TIDISPIN                    
213200     MOVE RAD-TIREGDAT           TO ORQM-OBKR-TIORDREG                    
213300     MOVE ZERO                   TO ORQM-OBKR-TIPRIS                      
213400                                   ORQM-OBKR-TIRODAT                      
213500     MOVE LOGG-TIAAMMDD          TO ORQM-OBKR-TIREGDAT                    
213600     MOVE LOGG-TIKLOCK           TO ORQM-OBKR-TIREGTID                    
213700     ADD SEKEL-TAL TO ORQM-OBKR-TIREGDAT GIVING                           
213800                                   DATUM-MED-ARHUNDR                      
213900     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
214000                                   ORQM-OBKR-TITIREGD-9KOMPL              
214100     MOVE RAD-TITPO              TO ORQM-OBKR-TITPO                       
214200     ADD SEKEL-TAL TO ORQM-OBKR-TIORDREG GIVING                           
214300                                   DATUM-MED-ARHUNDR                      
214400     SUBTRACT DATUM-MED-ARHUNDR FROM +999999999 GIVING                    
214500                                   ORQM-OBKR-TITIORDD-9KOMPL              
214600     MOVE RAD-KDFRAKT            TO ORQM-OBKR-KDFRAKT                     
214700     MOVE RAD-KDORDKL            TO ORQM-OBKR-KDORDKL                     
214800     MOVE RAD-DEAL-PR-LINE       TO ORQM-OBKR-DEAL-PR-LINE                
215000     MOVE ORQI-OHUV-KDORDTYP-LDC TO ORQM-OBKR-KDORDTYP-LDC                
215100     MOVE ORQI-OHUV-TIREPDAT     TO ORQM-OBKR-TIREPDAT                    
215200     MOVE RAD-IDKUNDRF-WIP       TO ORQM-OBKR-IDKUNDRF-WIP                
215400     MOVE ZERO                   TO ORQM-OBKR-TIDLEVDAT                   
215410     MOVE RAD-PRAVCOST           TO ORQM-OBKR-PRAVCOST                    
215500                                                                          
215600     PERFORM IMS-ISRT-ORQM                                                
215700     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
215800       ADD +1 TO ORQM-OBKR-IDLOPNR                                        
215900       PERFORM IMS-ISRT-ORQM                                              
216000     END-PERFORM                                                          
216100     .                                                                    
216200     EJECT                                                                
216300 S09-HAEMTA-TIDISPIN SECTION.                                             
216400                                                                          
216500     MOVE RAD-IDARTNR  TO W-IDARTNR-N3                                    
216600     PERFORM IMS-GU-ART-11                                                
216700     IF SEGMENT-FINNS                                                     
216800       MOVE CLAG-TIDISPIN TO WS-TIDISPIN                                  
216900     ELSE                                                                 
217000       MOVE ZERO            TO WS-TIDISPIN                                
217100     END-IF                                                               
217200     .                                                                    
217300     EJECT                                                                
217400 S10-SKRIV-RY9 SECTION.                                                   
217500                                                                          
217600     ADD +1                               TO   LOGG-IDLOGLOP              
217700     MOVE 'RY9'                           TO   RY9-IDPTYP                 
217800                                               LOGG-IDPTYP                
217900                                                                          
218000     MOVE WDGZRY9S                        TO  LOGG-SORTPOST               
218100     MOVE RY9-WDGZRY9                     TO  LOGG-LOGGPOST               
218200                                                                          
218300     PERFORM IMS-ISRT-LOGG                                                
218400     .                                                                    
218500     EJECT                                                                
218600 S11-SKAPA-RY9-TRANS SECTION.                                             
218700                                                                          
218800     PERFORM S20-FLYTTA-RY9                                               
218900     PERFORM S10-SKRIV-RY9                                                
219000     .                                                                    
219100     SKIP3                                                                
219200 S20-FLYTTA-RY9 SECTION.                                                  
219300                                                                          
219400     MOVE RAD-IDDISTR     TO RY9S-IDDISTR                                 
219500     MOVE RAD-IDKUNDNR    TO RY9S-IDKUNDNR                                
219700     MOVE WC-CDC-SE       TO RY9S-IDDC                                    
219800     MOVE RAD-KDFRAKT     TO RY9S-KDFRAKT                                 
219900     MOVE RAD-KDORDKL     TO RY9S-KDORDKL                                 
220000     MOVE WS-KDORDBEK     TO RY9S-KDORDBEK                                
220100     MOVE WS-IDORDER      TO RY9S-IDORDER                                 
220200                                                                          
220300     MOVE RAD-BERADREF    TO RY9-BERADREF                                 
220400     MOVE RAD-BEVOLREF    TO RY9-BEVOLREF                                 
220500     MOVE RAD-FLERS       TO RY9-FLERS                                    
220600     IF RAD-IDDISTR NOT = W-IDDISTR-N5                                    
220700        MOVE RAD-IDDISTR  TO W-IDDISTR-N5                                 
220800        MOVE RAD-IDKUNDNR TO W-IDKUNDNR-N5                                
220900        PERFORM IMS-GU-GMTA01                                             
221000     END-IF                                                               
221100     MOVE GMTA-GMT-FLVR   TO RY9-FLVR                                     
221200     MOVE GMTA-GMT-FLNC   TO RY9-FLNC                                     
221300     MOVE RAD-IDARTNR     TO RY9-IDARTNR                                  
221400     MOVE ZERO            TO RY9-IDDIVORD                                 
221500     MOVE RAD-IDKUNDRF    TO RY9-IDKUNDRF                                 
221600     MOVE RAD-IDLOPNR     TO RY9-IDLOPNR                                  
221700     MOVE MSG-LTERM-NAME  TO RY9-IDUSER                                   
221800     MOVE RAD-KDFAKTYP    TO RY9-KDFAKTYP                                 
221900     MOVE RAD-KDKVBRYT    TO RY9-KDKVBRYT                                 
222000     MOVE RAD-KDORDKL     TO RY9-KDORDKL                                  
222100     MOVE RAD-KVART       TO RY9-KVART                                    
222200     MOVE RAD-KDDSP       TO RY9-KDDSP                                    
222300     MOVE RAD-KDRAPRIO    TO RY9-KDRAPRIO                                 
222400     MOVE RAD-KDSTARAD    TO RY9-KDSTARAD                                 
222500     MOVE RAD-KDTPOTYP    TO RY9-KDTPOTYP                                 
222600     MOVE SPACE           TO RY9-KDUART                                   
222700     MOVE RAD-KDVRINFO    TO RY9-KDVRINFO                                 
222800     IF RAD-KDTPOTYP = 1                                                  
222900       IF RAD-IDSYSTEM = 'VR'                                             
223000         MOVE +1          TO RY9-KDVRTPO                                  
223100       ELSE                                                               
223200         MOVE +2          TO RY9-KDVRTPO                                  
223300       END-IF                                                             
223400     ELSE                                                                 
223500       MOVE ZERO          TO RY9-KDVRTPO                                  
223600     END-IF                                                               
223700     MOVE RAD-PRARTNTO    TO RY9-PRARTNTO                                 
223800     MOVE RAD-TIREGDAT    TO RY9-TIREGDAT                                 
223900     MOVE RAD-TIRES       TO RY9-TIRES                                    
224000     MOVE RAD-TITPO       TO RY9-TITPO                                    
224100     MOVE RAD-DARODAT (3:6) TO RY9-TIRODAT                                
224200     .                                                                    
224300     EJECT                                                                
224400*S30-SECURIT     SECTION.  KOMMENTARSMÄRKT TILLS VIDARE 920903            
224500*                                                                         
224600*    MOVE MSG-SIGNON-USERID               TO SEC-IDUSER                   
224700*    MOVE '2125'                          TO SEC-IDTRANS                  
224800*    MOVE ALL ZERO                        TO SEC-IDKEY                    
224900*                                                                         
225000*    CALL WSECURIT                     USING SEC-IDUSER                   
225100*                                            SEC-IDTRANS                  
225200*                                            SEC-IDKEY                    
225300*                                            SEC-KDSVAR                   
225400*    .                                                                    
225500*    SKIP3                                                                
225600 S40-LOGGTID     SECTION.                                                 
225700                                                                          
225800     ACCEPT LOGG-TIAAMMDD                 FROM DATE                       
225900     ACCEPT LOGG-TIKLOCK                  FROM TIME                       
226000     MOVE  +0                             TO   LOGG-IDLOGLOP              
226100     .                                                                    
226200     EJECT                                                                
226300* MFS SEKTIONER                                                           
226400                                                                          
226500 MFS-ROR-EJ-IN-UT-NYCKLAR SECTION.                                        
226600                                                                          
226700     MOVE MFS-ROER-EJ-FAELT     TO MOD-IDARTNR-IN                         
226800                                   MOD-IDDISTR-IN                         
226900                                   MOD-IDANSK-IN                          
227000                                   MOD-KDROO-IN                           
227100                                   MOD-KDTPOTYP-FOM-IN                    
227200                                   MOD-KDTPOTYP-TOM-IN                    
227300                                   MOD-KDSTARAD-IN                        
227400                                                                          
227500                                   MOD-IDARTNR-UT                         
227600                                   MOD-IDDISTR-UT                         
227700                                   MOD-IDANSK-UT                          
227800                                   MOD-KDROO-UT                           
227900                                   MOD-KDTPOTYP-FOM-UT                    
228000                                   MOD-KDTPOTYP-TOM-UT                    
228100                                   MOD-KDSTARAD-UT                        
228200     .                                                                    
228300     EJECT                                                                
228400 MFS-ROR-EJ-SPARADE-NYCKLAR  SECTION.                                     
228500                                                                          
228600     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDARTNR-SPAR-E                      
228700                                  MOD-KDRAPRIO-SPAR-E                     
228800                                  MOD-TIRODAT-SPAR-E                      
228810                                  MOD-TIREGTID-SPAR-E                     
228900                                  MOD-IDDISTR-SPAR-E                      
229000                                  MOD-IDKUNDNR-SPAR-E                     
229100                                  MOD-IDKUNDRF-SPAR-E                     
229200                                  MOD-IDLOPNR-SPAR-E                      
229300                                  MOD-IDANSK-SPAR-E                       
229400                                  MOD-KDROO-SPAR-E                        
229700                                                                          
229800                                  MOD-IDARTNR-SPAR                        
229900                                  MOD-KDRAPRIO-SPAR                       
230000                                  MOD-TIREGTID-SPAR                       
230100                                  MOD-IDDISTR-SPAR                        
230200                                  MOD-IDKUNDNR-SPAR                       
230300                                  MOD-IDKUNDRF-SPAR                       
230400                                  MOD-IDLOPNR-SPAR                        
230500                                  MOD-IDANSK-SPAR                         
230600                                  MOD-KDROO-SPAR                          
230900                                  MOD-KVRAD-SPAR                          
231000     .                                                                    
231100     EJECT                                                                
231200 MFS-ROR-EJ-FAELT  SECTION.                                               
231300                                                                          
231400     MOVE MFS-ROER-EJ-FAELT    TO MOD-KVART-OPP                           
231500                                  MOD-TITPO-OPP                           
231600                                                                          
231700     MOVE +1 TO INDX                                                      
231800     PERFORM UNTIL INDX > MAX-LINE                                        
231900        MOVE MFS-ROER-EJ-FAELT TO MOD-VALKOD  (INDX)                      
232000                                  MOD-IDARTNR (INDX)                      
232100                                  MOD-IDDISTR (INDX)                      
232200                                  MOD-IDKUNDNR(INDX)                      
232300                                  MOD-IDORDNR (INDX)                      
232400                                  MOD-KDTPOTYP(INDX)                      
232500                                  MOD-KVART   (INDX)                      
232600                                  MOD-TIRODAT (INDX)                      
232700                                  MOD-TIRES   (INDX)                      
232800                                  MOD-KDORDKL (INDX)                      
233000                                  MOD-KDROO   (INDX)                      
233100                                  MOD-IDANSK  (INDX)                      
233200                                  MOD-TITPO   (INDX)                      
233300                                  MOD-KDRAPRIO(INDX)                      
233400                                  MOD-IDLOPNR (INDX)                      
233500        ADD +1 TO INDX                                                    
233600     END-PERFORM                                                          
233700     .                                                                    
233800     EJECT                                                                
233900 MFS-RENSA-FALT-IN  SECTION.                                              
234000                                                                          
234100     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-IN                         
234200                                   MOD-IDDISTR-IN                         
234300                                   MOD-IDANSK-IN                          
234400                                   MOD-KDROO-IN                           
234500                                   MOD-KDTPOTYP-FOM-IN                    
234600                                   MOD-KDTPOTYP-TOM-IN                    
234700                                   MOD-KDSTARAD-IN                        
234800     .                                                                    
234900     SKIP3                                                                
235000 MFS-RENSA-FALT-UT  SECTION.                                              
235100                                                                          
235200     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-UT                         
235300                                   MOD-IDDISTR-UT                         
235400                                   MOD-IDANSK-UT                          
235500                                   MOD-KDROO-UT                           
235600                                   MOD-KDTPOTYP-FOM-UT                    
235700                                   MOD-KDTPOTYP-TOM-UT                    
235800                                   MOD-KDSTARAD-UT                        
235900     .                                                                    
236000     EJECT                                                                
236100 MFS-RENSA-FALT     SECTION.                                              
236200                                                                          
236300     MOVE MFS-RENSA-FAELT      TO MOD-KVART-OPP                           
236400                                  MOD-TITPO-OPP                           
236500                                                                          
236600     PERFORM UNTIL INDX > MAX-LINE                                        
236700        MOVE MFS-RENSA-FAELT   TO MOD-VALKOD  (INDX)                      
236800                                  MOD-IDARTNR (INDX)                      
236900                                  MOD-IDDISTR (INDX)                      
237000                                  MOD-IDKUNDNR(INDX)                      
237100                                  MOD-IDORDNR (INDX)                      
237200                                  MOD-KDTPOTYP(INDX)                      
237300                                  MOD-KVART   (INDX)                      
237400                                  MOD-TIRODAT (INDX)                      
237500                                  MOD-TIRES   (INDX)                      
237600                                  MOD-KDORDKL (INDX)                      
237800                                  MOD-KDROO   (INDX)                      
237900                                  MOD-IDANSK  (INDX)                      
238000                                  MOD-TITPO   (INDX)                      
238100                                  MOD-KDRAPRIO(INDX)                      
238200                                  MOD-IDLOPNR (INDX)                      
238300        ADD +1 TO INDX                                                    
238400     END-PERFORM                                                          
238500     .                                                                    
238600     EJECT                                                                
238700* IMS SEKTIONER                                                           
238800                                                                          
238900 IMS-GU-MSG SECTION.                                                      
239000                                                                          
239100     SKIP2                                                                
239200     MOVE '  QC' TO GODK-STATUSKODER                                      
239300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
239400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
239500     PERFORM IMS-STATUSKONTROLL                                           
239600     .                                                                    
239700     SKIP3                                                                
239800 IMS-ISRT-MSG SECTION.                                                    
239900                                                                          
240000     IF SPRAK-IX = +2                                                     
240100        MOVE 'N'    TO MFS-KDHUVOMR                                       
240200     END-IF                                                               
240300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
240400     MOVE SPACE TO GODK-STATUSKODER                                       
240500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
240600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
240700     PERFORM IMS-STATUSKONTROLL                                           
240800     .                                                                    
240900     EJECT                                                                
241000 IMS-GU-OWDS01 SECTION.                                                   
241100                                                                          
241200     STRING 'WLORDS01(WDA5C1KY =' W-WDA5C1KY-MIN ')'                      
241300            DELIMITED BY SIZE INTO SSA1                                   
241400     MOVE '  GE' TO GODK-STATUSKODER                                      
241500     CALL CBLTDLI USING GU ORDS-PCB DLI-IO-AREA2 SSA1                     
241600     MOVE ORDS-STATUS-CODE TO STATUS-WS                                   
241700     PERFORM IMS-STATUSKONTROLL                                           
241800     .                                                                    
241900     SKIP2                                                                
242000 IMS-GN-OWDS01 SECTION.                                                   
242100                                                                          
242200     STRING 'WLORDS01(WDA5C1KY>=' W-WDA5C1KY-MIN                          
242300                    '&WDA5C1KY<=' W-WDA5C1KY-MAX                          
242400                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
242500                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
242600                    '&KDTPOTYP>=' W-KDTPOTYP-MIN-X                        
242700                    '&KDTPOTYP<=' W-KDTPOTYP-MAX-X                        
242800                    '&KDSTARAD>=' W-KDSTARAD-MIN                          
242900                    '&KDSTARAD<=' W-KDSTARAD-MAX ')'                      
243000            DELIMITED BY SIZE INTO SSA1                                   
243100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
243200     CALL CBLTDLI USING GN ORDS-PCB DLI-IO-AREA2 SSA1                     
243300     MOVE ORDS-STATUS-CODE TO STATUS-WS                                   
243400     PERFORM IMS-STATUSKONTROLL                                           
243500     .                                                                    
243600     EJECT                                                                
243700 IMS-GU-ORDQ01 SECTION.                                                   
243800                                                                          
243900     STRING 'WLORDQ01(WDA5A1KY =' W-WDA5A1KY-MIN ')'                      
244000            DELIMITED BY SIZE INTO SSA1                                   
244100     MOVE '  GE' TO GODK-STATUSKODER                                      
244200     CALL CBLTDLI USING GU ORDQ-PCB DLI-IO-AREA3 SSA1                     
244300     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
244400     PERFORM IMS-STATUSKONTROLL                                           
244500     .                                                                    
244600     SKIP2                                                                
244700 IMS-GN-ORDQ01 SECTION.                                                   
244800                                                                          
244900     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN                          
245000                    '&WDA5A1KY<=' W-WDA5A1KY-MAX                          
245100                    '&IDDISTR >=' W-IDDISTR-MIN-X                         
245200                    '&IDDISTR <=' W-IDDISTR-MAX-X                         
245300                    '&KDTPOTYP>=' W-KDTPOTYP-MIN-X                        
245400                    '&KDTPOTYP<=' W-KDTPOTYP-MAX-X                        
245500                    '&KDSTARAD>=' W-KDSTARAD-MIN                          
245600                    '&KDSTARAD<=' W-KDSTARAD-MAX ')'                      
245700            DELIMITED BY SIZE INTO SSA1                                   
245800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
245900     CALL CBLTDLI USING GN ORDQ-PCB DLI-IO-AREA3 SSA1                     
246000     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
246100     PERFORM IMS-STATUSKONTROLL                                           
246200     .                                                                    
246300     EJECT                                                                
246400 IMS-GU-ORDP01-1  SECTION.                                                
246500                                                                          
246600     STRING 'WLORDP01(WDA501KY =' W-WDA501KY                              
246700                    '&KDROO   >=' W-KDROO-MIN-X                           
246800                    '&KDROO   <=' W-KDROO-MAX-X                           
246900                    '&IDANSK  >=' W-IDANSK-MIN-X                          
247000                    '&IDANSK  <=' W-IDANSK-MAX-X ')'                      
247100            DELIMITED BY SIZE INTO SSA1                                   
247200     MOVE '  GE' TO GODK-STATUSKODER                                      
247300     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-AREA1 SSA1                     
247400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
247500     PERFORM IMS-STATUSKONTROLL                                           
247600     .                                                                    
247700     SKIP2                                                                
247800 IMS-GU-ORDP01-2  SECTION.                                                
247900                                                                          
248000     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
248100            DELIMITED BY SIZE INTO SSA1                                   
248200     MOVE '  GE' TO GODK-STATUSKODER                                      
248300     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-AREA1 SSA1                     
248400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
248500     PERFORM IMS-STATUSKONTROLL                                           
248600     .                                                                    
248700     SKIP2                                                                
248800 IMS-GHU-ORDP01  SECTION.                                                 
248900                                                                          
249000     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
249100            DELIMITED BY SIZE INTO SSA1                                   
249200     MOVE '  GE' TO GODK-STATUSKODER                                      
249300     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA1 SSA1                    
249400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
249500     PERFORM IMS-STATUSKONTROLL                                           
249600     .                                                                    
249700     EJECT                                                                
249800 IMS-DLET-ORDP01   SECTION.                                               
249900                                                                          
250000     MOVE '  ' TO GODK-STATUSKODER                                        
250100     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-AREA1                        
250200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
250300     PERFORM IMS-STATUSKONTROLL                                           
250400     .                                                                    
250500     SKIP2                                                                
250600 IMS-REPL-ORDP01 SECTION.                                                 
250700                                                                          
250800     MOVE '  ' TO GODK-STATUSKODER                                        
250900     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA1                        
251000     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
251100     PERFORM IMS-STATUSKONTROLL                                           
251200     .                                                                    
251300     SKIP2                                                                
251400 IMS-ISRT-ORDP01 SECTION.                                                 
251500                                                                          
251600     MOVE 'WLORDP01 '      TO SSA1                                        
251700     MOVE '  II'           TO GODK-STATUSKODER                            
251800     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA1 SSA1                   
251900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
252000     PERFORM IMS-STATUSKONTROLL                                           
252100     .                                                                    
252200     EJECT                                                                
252300 IMS-GU-WDK601 SECTION.                                                   
252400                                                                          
252500     STRING 'WLARTC01(IDARTNR  =' W-WDK601KY ')'                          
252600            DELIMITED BY SIZE INTO SSA1                                   
252700     MOVE '  GE' TO GODK-STATUSKODER                                      
252800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4-01 SSA1                  
252900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
253000     PERFORM IMS-STATUSKONTROLL                                           
253100     .                                                                    
253200     SKIP2                                                                
253300 IMS-GU-ART-01 SECTION.                                                   
253400                                                                          
253500     STRING 'WLARTC01(IDARTNR  =' W-WDK601KY ')'                          
253600            DELIMITED BY SIZE INTO SSA1                                   
253700     MOVE '  ' TO GODK-STATUSKODER                                        
253800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4-01 SSA1                  
253900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
254000     PERFORM IMS-STATUSKONTROLL                                           
254100     .                                                                    
254200     SKIP2                                                                
254300 IMS-GU-ART-11 SECTION.                                                   
254400                                                                          
254500     STRING 'WLARTC01(IDARTNR  =' W-WDK601KY ')'                          
254600            DELIMITED BY SIZE INTO SSA1                                   
254700     STRING 'WLARTC11   '                                                 
254800            DELIMITED BY SIZE INTO SSA2                                   
254900     MOVE '  ' TO GODK-STATUSKODER                                        
255000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4-11 SSA1 SSA2             
255100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
255200     PERFORM IMS-STATUSKONTROLL                                           
255300     .                                                                    
255400     EJECT                                                                
255500 IMS-GHU-ARTM    SECTION.                                                 
255600                                                                          
255700     STRING 'WLARTM01(IDARTNR  =' W-WDK601KY ')'                          
255800            DELIMITED BY SIZE INTO SSA1                                   
255900     MOVE '  ' TO GODK-STATUSKODER                                        
256000     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA6 SSA1                    
256100     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
256200     PERFORM IMS-STATUSKONTROLL                                           
256300     .                                                                    
256400     SKIP2                                                                
256500 IMS-GHU-ARTM-ANT-GE SECTION.                                             
256600                                                                          
256700     STRING 'WLARTM01(IDARTNR  =' W-WDK601KY ')'                          
256800            DELIMITED BY SIZE INTO SSA1                                   
256900     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
257000            DELIMITED BY SIZE INTO SSA2                                   
257100     MOVE '  GE' TO GODK-STATUSKODER                                      
257200     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA6 SSA1 SSA2               
257300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
257400     PERFORM IMS-STATUSKONTROLL                                           
257500     .                                                                    
257600     SKIP2                                                                
257700 IMS-REPL-ARTM SECTION.                                                   
257800                                                                          
257900     MOVE '  ' TO GODK-STATUSKODER                                        
258000     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA6                        
258100     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
258200     PERFORM IMS-STATUSKONTROLL                                           
258300     .                                                                    
258400     EJECT                                                                
258500 IMS-ISRT-ARTM-ANT  SECTION.                                              
258600                                                                          
258700     STRING 'WLARTM01(IDARTNR  =' W-WDK601KY ')'                          
258800            DELIMITED BY SIZE INTO SSA1                                   
258900     MOVE 'WLARTM11 ' TO SSA2                                             
259000     MOVE '  ' TO GODK-STATUSKODER                                        
259100     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-AREA6 SSA1 SSA2              
259200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
259300     PERFORM IMS-STATUSKONTROLL                                           
259400     .                                                                    
259500     SKIP2                                                                
259600 IMS-DLET-ARTM SECTION.                                                   
259700                                                                          
259800     MOVE '  ' TO GODK-STATUSKODER                                        
259900     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-AREA6                        
260000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
260100     PERFORM IMS-STATUSKONTROLL                                           
260200     .                                                                    
260300     EJECT                                                                
260400 IMS-GU-GMTA01 SECTION.                                                   
260500                                                                          
260600     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
260700            DELIMITED BY SIZE INTO SSA1                                   
260800     MOVE '  ' TO GODK-STATUSKODER                                        
260900     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA7 SSA1                     
261000     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
261100     PERFORM IMS-STATUSKONTROLL                                           
261200     .                                                                    
261300     SKIP2                                                                
261400 IMS-ISRT-ORQM     SECTION.                                               
261500                                                                          
261600     MOVE 'WLORQM01 ' TO SSA1                                             
261700     MOVE '  II' TO GODK-STATUSKODER                                      
261800     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA6 SSA1                   
261900     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
262000     PERFORM IMS-STATUSKONTROLL                                           
262100     .                                                                    
262200     SKIP2                                                                
262300 IMS-GET-ORQI-CSEQ SECTION.                                               
262400                                                                          
262500     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
262600            DELIMITED BY SIZE INTO SSA1                                   
262700     MOVE '  ' TO GODK-STATUSKODER                                        
262800     CALL CBLTDLI USING GU  ORQI-PCB DLI-IO-AREA6 SSA1                    
262900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
263000     PERFORM IMS-STATUSKONTROLL                                           
263100     .                                                                    
263200     SKIP2                                                                
263300 IMS-ISRT-LOGG SECTION.                                                   
263400                                                                          
263500     MOVE 'WLZZAC01' TO SSA1                                              
263600     MOVE '  II' TO GODK-STATUSKODER                                      
263700     CALL CBLTDLI USING ISRT LOGG-PCB DLI-IO-AREA5 SSA1                   
263800     MOVE LOGG-STATUS-CODE TO STATUS-WS                                   
263900     PERFORM IMS-STATUSKONTROLL                                           
264000     .                                                                    
264100     SKIP2                                                                
264200 IMS-STATUSKONTROLL SECTION.                                              
264300                                                                          
264400     SET STATUS-IX TO 1                                                   
264500     SEARCH GODK-STATUS AT END CALL FELLOG                                
264600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
264700         CONTINUE                                                         
264800     END-SEARCH                                                           
264900     .                                                                    
265000     EJECT                                                                
266000*    -COPY WY2000P1                                                       
