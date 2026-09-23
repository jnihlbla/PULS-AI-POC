000100 ID DIVISION.                                                             
000200 PROGRAM-ID. W2012100.                                                    
000300 AUTHOR. JAN MELANDER.                                                    
000400 DATE-COMPILED.                                                           
000500 DATE-WRITTEN. NOV  86.                                                   
000600     REMARKS.                                                             
000700*    FUNKTION.   UPP/NED OCH ANNULATION AV BESTÄLLNINSREST.               
000800*                                                                         
000900*    INDATA.                                                              
001000*        TRANSAKTION: W2T121                                              
001100*        MID:         W2I12101                                            
001200*    UTDATA.                                                              
001300*        MOD:         W2O12101                                            
001400*    SUBPROGRAM.                                                          
001500*        FELLOG                                                           
001600*        CBLTDLI                                                          
001700*                                                                         
001800*   ÄNDRINGAR:                                                            
001900*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002000*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
002100*                  INTE ÄR LIKA MED LEVERANTÖR-ID PÅ UTDATA-RADER         
002200*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
002300*        MAJ 2012  NYCKEL WDD901 UTÖKAD MED IDDC                          
002310*                                                                         
002320*                                                                         
002330*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77      WS-DATUM        PIC 9(6)    VALUE ZERO.                          
003300 77      IDX             PIC S9(9)   VALUE +0    COMP SYNC.               
003400 77      RLIND           PIC S9(9)   VALUE +0    COMP SYNC.               
003500 77      INDEXET         PIC S9(9)   VALUE +0    COMP SYNC.               
003600 77      IX              PIC S9(9)   VALUE +0    COMP SYNC.               
003700 77      IND             PIC S9(9)   VALUE +0    COMP SYNC.               
003800 77      INDX            PIC S9(9)   VALUE +0    COMP SYNC.               
003900 77      MAX-MOD-LAENGD  PIC S9(4)   VALUE +656  COMP SYNC.               
004000 77      UPPDATERA            PIC X       VALUE 'U'.                      
004100 77      JA                   PIC X       VALUE 'J'.                      
004200 77      NEJ                  PIC X       VALUE 'N'.                      
004300 77      U                    PIC X       VALUE 'U'.                      
004400 77      N                    PIC X       VALUE 'N'.                      
004500 77      A                    PIC X       VALUE 'A'.                      
004600 77      SW-INDATA-OK         PIC X(1)   VALUE SPACE.                     
004700 77      SW-NY-ART            PIC X(1)   VALUE SPACE.                     
004800 77      SW-611               PIC X(1)   VALUE SPACE.                     
004900 77      SW-TEST-BESTREST     PIC X(1)   VALUE SPACE.                     
005000 77      SW-TEST-BESTNR-LEVNR PIC X(1)   VALUE SPACE.                     
005100 77      SW-W2214             PIC X(1)   VALUE SPACE.                     
005200 77      SW-AVTAL             PIC X(1)   VALUE SPACE.                     
005300 88      SW-AVT-OK                       VALUE 'J'.                       
005400 77      SW-REPL-INLB11       PIC X(1)   VALUE 'N'.                       
005500 77      TRAEFF               PIC X(1)   VALUE SPACE.                     
005600                                                                          
005800 77      WS-LEVPL-KVBR      PIC 9(9)   VALUE ZERO.                        
005900 77      WS-TOT-BR          PIC S9(9)   VALUE +0   COMP-3.                
006000 77      WS-KVBR-TOT        PIC 9(9)   VALUE ZERO.                        
006100 77      WS-BESTREST        PIC S9(9)   VALUE ZERO.                       
006200 77      WS-BESTREST-VIS    PIC 9(9)   VALUE ZERO.                        
006300 77      WS-IDBEST          PIC 9(12).                                    
006400 77      WS-IDAVTAL         PIC 9(12).                                    
006500 77      WS-KDAVT           PIC S9      VALUE +0 COMP SYNC.               
006600 77      WS-AVT-IDAVTAL     PIC S9(13)  COMP-3.                           
006700 77      WS-KDKSP           PIC S9      VALUE +0 COMP SYNC.               
006800 77      WS-IDINK           PIC 9(3)    VALUE ZERO.                       
006900 77      TEST-IDINK         PIC 9(3)    VALUE ZERO.                       
007000 77      WS-KDLPORS         PIC 9(3)    VALUE ZERO.                       
007100 77      WS-KVBR            PIC 9(7)    VALUE ZERO.                       
007200 77      WS-IDARTNR         PIC X(09).                                    
007300 77      WS-IDARTNR-8       PIC 9(08).                                    
007400 77      WS-IDLOGLOP        PIC 9(01) COMP-3 VALUE ZERO.                  
007500 77      TEST-WDK6-IDLEVNR  PIC X(5).                                     
007600 77      WS-IDLEVNR-NUM     PIC 9(5)    VALUE ZERO.                       
007700 77      WS-IDLEVNR-8       PIC X(8)    VALUE SPACE.                      
007800 77      WS-TIBEST          PIC S9(7)   VALUE ZERO.                       
007900 77      WS-JUST-KVANT      PIC S9(9)   VALUE ZERO.                       
008000 77      WS-IDLEVNR-21-1    PIC X(5)    VALUE SPACE.                      
008100 77      WS-IDLEVNR-21-2    PIC X(5)    VALUE SPACE.                      
008200                                                                          
008300 01  TABELL.                                                              
008400     03  TAB-IDBEST     OCCURS 7 TIMES    PIC S9(13).                     
008500     03  TAB-KDBEH-BEST OCCURS 7 TIMES    PIC S9.                         
008600     03  TAB-TIBEST     OCCURS 7 TIMES    PIC S9(7).                      
008700     EJECT                                                                
008800                                                                          
008900 01  FILLER              PIC  X(16)  VALUE 'BYTES-DIST'.                  
009000 01  TEST-IDDISTR        PIC  9(5)   COMP-3.                              
009100*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
009200     EJECT                                                                
009300                                                                          
009400 01  FILLER              PIC  X(16)  VALUE 'BYTES-ART '.                  
009500 01  TEST-IDARTNR        PIC  9(9)   COMP-3 VALUE ZERO.                   
009600*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
009700     EJECT                                                                
009800*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
009900     EJECT                                                                
010000                                                                          
010010*01    -COPY WWDC99                                                       
010021     EJECT                                                                
010030                                                                          
010100 01      NYCKLAR-TILL-DLI.                                                
010200   03    W-WDD901KY-X.                                                    
010300     05  W-IDARTNR-INLB  PIC S9(9)   VALUE ZERO  COMP-3.                  
010310     05  W-IDDC-INLB     PIC X(2)    VALUE '11'.                          
010400                                                                          
010410   03    W-IDARTNR-X.                                                     
010420     05  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
010430                                                                          
010800   03    W-IDBEST-X.                                                      
010900     05  W-IDBEST        PIC S9(13)  VALUE ZERO  COMP-3.                  
011000                                                                          
011100   03    W-IDAVTAL-X.                                                     
011200     05  W-IDAVTAL       PIC S9(13)  VALUE ZERO  COMP-3.                  
011300                                                                          
011400   03    W-IDLEVNR-X.                                                     
011500     05  W-IDLEVNR       PIC X(5)   VALUE SPACE.                          
011600                                                                          
011700   03    W-WDG3KEY-2203-X.                                                
011800     05  FILLER          PIC X(04)  VALUE '2203'.                         
011810     05  W-IDDC-2203     PIC X(02)  VALUE '11'.                           
011900     05  FILLER          PIC X(24)  VALUE LOW-VALUE.                      
012000                                                                          
012100   03    W-WDG3KEY-2213-X.                                                
012200     05  FILLER          PIC X(04)  VALUE '2213'.                         
012210     05  W-IDDC-2213     PIC X(02)  VALUE '11'.                           
012300     05  FILLER          PIC X(24)  VALUE LOW-VALUE.                      
012400                                                                          
012500   03    W-1141-KEY-X.                                                    
012600     05  FILLER          PIC X(04)  VALUE '1141'.                         
012700     05  FILLER          PIC X(26)  VALUE LOW-VALUE.                      
012800  03  W-IDLEVNR-21-X.                                                     
012900     05  W-IDLEVNR-21    PIC X(5) VALUE SPACE.                            
013000     EJECT                                                                
013100                                                                          
013200*** AREA FÖR ATT AVGÖRA OM BYTE AV                                        
013300*** HUVUDLEVERANTÖR                                                       
013400*                                                                         
013500 01   BEST-ID                 PIC 9(12).                                  
013600 01   BYTE-IDBEST     REDEFINES BEST-ID.                                  
013700      03  BYTE-IDINK          PIC 9(3).                                   
013800      03  BYTE-BESTNR         PIC 9(6).                                   
013900      03  BYTE-SUFFIX         PIC 9(3).                                   
014000*                                                                         
014100 01   IDAVTAL-ID              PIC 9(12).                                  
014200 01   WORK-IDAVTAL   REDEFINES IDAVTAL-ID.                                
014300      03  WORK-IDINK      PIC X(3).                                       
014400      03  WORK-BESTNR     PIC X(6).                                       
014500      03  WORK-SUFFIX     PIC X(3).                                       
014600 01   WORK-IDINK-NUM      PIC 9(3).                                       
014700*                                                                         
014800 01  WS-TEST-AVTNR-1     PIC 9(12).                                       
014900 01  WS-TEST-AVTNR-2     PIC 9(12).                                       
015000 01  WS-TEST-IDLEVNR-AVT-1 PIC X(5).                                      
015100 01  WS-TEST-IDLEVNR-AVT-2 PIC X(5).                                      
015200                                                                          
015300 01  W2-IDAVTAL-12       PIC 9(12)  VALUE ZERO.                           
015400 01  W2-IDAVTAL-12-X  REDEFINES W2-IDAVTAL-12.                            
015500     03  W2-IDAVTAL-1-NUM  PIC 9(3).                                      
015600     03  W2-IDAVTAL-2-NUM  PIC 9(6).                                      
015700     03  W2-IDAVTAL-3-NUM  PIC 9(3).                                      
015800*                                                                         
015900 01      MEDDELANDE.                                                      
016000   03    FEL-1           PIC X(26)   VALUE                                
016100                                   'ARTIKELNUMMER EJ NUMERISKT'.          
016200                                                                          
016300   03    FEL-2           PIC X(23)   VALUE                                
016400                                     'ARTIKELN SAKNAS I BASEN'.           
016500                                                                          
016600   03    FEL-3           PIC X(20)   VALUE                                
016700                                     'ARTIKELN ÄR UTGÅNGEN'.              
016800                                                                          
016900   03    FEL-4           PIC X(17)   VALUE 'UPPLYSTA FÄLT FEL'.           
017000                                                                          
017100   03    FEL-5           PIC X(20)   VALUE                                
017200                                     'LEVNR SAKNAS I BASEN'.              
017300                                                                          
017400   03    FEL-6           PIC X(40)   VALUE                                
017500                        'NEDCAR-AVTAL FÅR EJ ANNULLERAS HÄR'.             
017600                                                                          
017700   03    FEL-7           PIC X(40)   VALUE                                
017800                        'AVTAL EL BEST SAKNAS FÖR ANGIVEN LEV'.           
017900                                                                          
018000   03    FEL-8           PIC X(31)   VALUE                                
018100                             'BESTREST SAKNAS FOR ANGIVEN LEV'.           
018200                                                                          
018300   03    FEL-9           PIC X(19)   VALUE                                
018400                             'BESTNR/LEVNR SAKNAS'.                       
018500                                                                          
018600   03    FEL-10          PIC X(36).                                       
018700   03  WORK-FEL-10   REDEFINES FEL-10.                                    
018800      05 WORK-TEXT-10    PIC X(27).                                       
018900      05 WORK-VARDE-10    PIC X(9).                                       
019000                                                                          
019100   03    FEL-11          PIC X(37).                                       
019200   03  WORK-FEL-11   REDEFINES FEL-11.                                    
019300      05 WORK-TEXT-11    PIC X(28).                                       
019400      05 WORK-VARDE-11    PIC X(9).                                       
019500                                                                          
019600   03    FEL-12          PIC X(37).                                       
019700   03  WORK-FEL-12   REDEFINES FEL-12.                                    
019800      05 WORK-TEXT-12    PIC X(28).                                       
019900      05 WORK-VARDE-12    PIC X(9).                                       
020000                                                                          
020100   03    FEL-22          PIC X(40)                                        
020200                VALUE 'OBEHÖRIG ANVÄNDARE '.                              
020300                                                                          
020400   03    FEL-33          PIC X(40)  VALUE                                 
020500                        'NAP-AVTAL FÅR EJ ANNULLERAS HÄR'.                
020600                                                                          
020700   03    MED-1           PIC X(12)   VALUE 'KÖPSPÄRR = 1'.                
020800                                                                          
020900                                                                          
021000   03    MED-2           PIC X(17)   VALUE 'UPPDATERING GJORD'.           
021100                                                                          
021200                                                                          
021300   03    MED-3           PIC X(17)   VALUE 'BESTREST SAKNAS  '.           
021400                                                                          
021500                                                                          
021600   03    MED-4           PIC X(17)   VALUE 'LEVERANTOR SAKNAS'.           
021700                                                                          
021800                                                                          
021900   03    MED-5           PIC X(32)   VALUE                                
022000                 'TRYCK PF11 FÖR UPPDATERING     '.                       
022100                                                                          
022200   03    MED-6           PIC X(32)   VALUE                                
022300                 'FLER AVTAL FINNS, TRYCK PF8    '.                       
022400                                                                          
022500     EJECT                                                                
022600**********************************************************                
022700***   D Y N A M I S K A   S U B P R O G R A M                             
022800**********************************************************                
022900 01   DYNAMISKA-SUBPROGRAM.                                               
023000   03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
023100   03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
023200   03    W005INIT        PIC X(8)    VALUE 'W005INIT'.                    
023300     SKIP3                                                                
023400*01  AREA -COPY W092W001           -PRE W092-                             
023500     EJECT                                                                
023600**********************************************************                
023700***   I N K Ö P S - P O S T   P V                                         
023800**********************************************************                
023900*                                                                         
024000*01  -COPY A310TB65                -PRE A310-                             
024100     EJECT                                                                
024200******************************************************************        
024300*                                                                         
024400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
024500*                                                                         
024600 01      FILLER          PIC X(16)   VALUE 'MFS-WS          '.            
024700     SKIP3                                                                
024800*01      MID -COPY W2I12101                                               
024900     EJECT                                                                
025000*01      -COPY WMSGAREA                                                   
025100     EJECT                                                                
025200*  03    MOD -COPY W2O12101           -RED MSG-AREA.                      
025300     EJECT                                                                
025400*01      -COPY WMFSAREA                                                   
025500     EJECT                                                                
025600*                       ****   PARAMETRAR TILL W005INIT                   
025700*01      -COPY WMSGINIT                                                   
025800     EJECT                                                                
025900******************************************************************        
026000*****                                                                     
026100*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026200*****                                                                     
026300 01  IMS-WS.                                                              
026400   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
026500     SKIP3                                                                
026600*****                    **** STATUS-KOD FRÅN IMS                         
026700   03    STATUS-WS       PIC XX.                                          
026800         88  SEGMENT-FINNS       VALUE '  '.                              
026900         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
027000     SKIP3                                                                
027100   03    GODK-STATUSKODER.                                                
027200     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027300     SKIP3                                                                
027400 01      SSA1            PIC X(128) VALUE SPACE.                          
027500 01      SSA2            PIC X(128)  VALUE SPACE.                         
027600 01      SSA3            PIC X(128) VALUE SPACE.                          
027700     SKIP3                                                                
027800*                            IMS FUNKTIONSKODER                           
027900*01      -COPY W0003                                                      
028000     EJECT                                                                
028100 01  FILLER              PIC X(16)   VALUE 'IO-AREA  IO-AREA'.            
028200 01  DLI-IO-AREA-01.                                                      
028300     03  IO-AREA-01      PIC X(200)  VALUE SPACE.                         
028400     SKIP3                                                                
028500*    03  WLARTC01 -COPY WDK601 -RED IO-AREA-01.                           
028600     EJECT                                                                
028700                                                                          
028800 01  FILLER              PIC X(16)   VALUE 'IO-AREA-K601'.                
028900 01  DLI-IO-AREA-K601.                                                    
029000*    03  -COPY WDK601 -PRE K6-                                            
029100     EJECT                                                                
029200 01  DLI-IO-AREA-11.                                                      
029300     03  IO-AREA-11      PIC X(900)  VALUE SPACE.                         
029400     SKIP3                                                                
029500*    03  WLARTC11 -COPY WDK611 -RED IO-AREA-11.                           
029600     EJECT                                                                
029700 01  FILLER              PIC X(16)   VALUE 'IO-AREA-21'.                  
029800 01  DLI-IO-AREA-21.                                                      
029900*    03  -COPY WDK621                                                     
030000     EJECT                                                                
030100 01  DLI-IO-AREA-22.                                                      
030200     03  IO-AREA-22      PIC X(200)  VALUE SPACE.                         
030300     SKIP3                                                                
030400*    03  WLARTC22 -COPY WDK622 -RED IO-AREA-22.                           
030500     EJECT                                                                
030600 01  DLI-IO-AREA-23.                                                      
030700     03  IO-AREA-23      PIC X(200)  VALUE SPACE.                         
030800     SKIP3                                                                
030900*    03  WLARTC23 -COPY WDK623 -RED IO-AREA-23.                           
031000 01  DLI-IO-AREA.                                                         
031100     03  IO-AREA         PIC X(200)  VALUE SPACE.                         
031200     EJECT                                                                
031300*    03  WLLEVA01 -COPY WDF101 -PRE LEVA-   -RED IO-AREA.                 
031400     EJECT                                                                
031500 01  DLI-IO-AREA3.                                                        
031600     03  IO-AREA3              PIC X(50) VALUE SPACE.                     
031700                                                                          
031800*    03  WLINLB01 -COPY WDD901 -PRE INLB01- -RED IO-AREA3.                
031900     EJECT                                                                
032000*01  WLINLB11 -COPY WDD902 -PRE LEVPL-     -RED DLI-IO-AREA3.             
032100     EJECT                                                                
032200 01  DLI-IO-AREA4.                                                        
032300     03  IO-AREA4              PIC X(50) VALUE SPACE.                     
032400                                                                          
032500*    03  XXBI -COPY WDGX01     -PRE XXBI-  -RED IO-AREA4.                 
032600                                                                          
032700*    03  XXBI -COPY WDGX2214   -PRE XXBI-  -RED IO-AREA4.                 
032800     EJECT                                                                
032900*    03  XXBJ -COPY WDGX01     -PRE XXBJ-  -RED IO-AREA4.                 
033000                                                                          
033100*    03  XXBJ -COPY WDGX2204   -PRE XXBJ-  -RED IO-AREA4.                 
033200     EJECT                                                                
033300 01  DLI-IO-AREA5.                                                        
033400     03  IO-AREA5              PIC X(150) VALUE SPACE.                    
033500                                                                          
033600*    03  ZZAC -COPY WDGZ01     -PRE ZZAC-  -RED IO-AREA5.                 
033700     EJECT                                                                
034700     EJECT                                                                
034800                                                                          
034900 LINKAGE SECTION.                                                         
035000*01  -COPY W0009     -PRE MSG-                                            
035100     EJECT                                                                
035200*01  -COPY W0008     -PRE USEA-                                           
035300         05  FILLER           PIC X.                                      
035400     EJECT                                                                
035500*01  -COPY W0008     -PRE ARTC-                                           
035600         05  FILLER           PIC X.                                      
035700     EJECT                                                                
035800*01  -COPY W0008     -PRE INLB-                                           
035900         05  FILLER           PIC X.                                      
036000     EJECT                                                                
036100*01  -COPY W0008     -PRE LEVA-                                           
036200         05  FILLER           PIC X.                                      
036300     EJECT                                                                
036400*01  -COPY W0008     -PRE XXBI-                                           
036500         05  FILLER           PIC X.                                      
036600     EJECT                                                                
036700*01  -COPY W0008     -PRE XXBJ-                                           
036800         05  FILLER           PIC X.                                      
036900     EJECT                                                                
037000*01  -COPY W0008     -PRE ZZAC-                                           
037100         05  FILLER           PIC X.                                      
037500     EJECT                                                                
037600*01  -COPY W0008     -PRE WDK6-                                           
037700         05  FILLER           PIC X.                                      
037830     EJECT                                                                
037900 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
038000                                  ARTC-PCB INLB-PCB LEVA-PCB              
038100                         XXBI-PCB XXBJ-PCB ZZAC-PCB                       
038200                                  WDK6-PCB.                               
038300     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
038400                                   ARTC-PCB INLB-PCB LEVA-PCB             
038500                          XXBI-PCB XXBJ-PCB ZZAC-PCB                      
038600                                   WDK6-PCB.                              
038700*                                                                         
038800     PERFORM IMS-GET-MSG                                                  
038900     IF SEGMENT-FINNS                                                     
039000       PERFORM A-INIT-SPARA-INPUT                                         
039100       IF WS-IDARTNR  NUMERIC                                             
039200         MOVE WS-IDARTNR TO W-IDARTNR                                     
039210                            W-IDARTNR-INLB                                
039300                            TEST-IDARTNR                                  
039400         PERFORM IMS-GET-WLARTC01                                         
039500         IF SEGMENT-FINNS                                                 
039600*          -- SPARA HUVUDLEVERATÖRENS ID FÖR KOLL AV UT-RADERNA           
039700           MOVE ART-IDLEVNR   TO WS-IDLEVNR-8                             
039800                                                                          
039900           IF ART-KDERS-UTG = 0                                           
040000              IF MFS-UPDATE                                               
040100                 PERFORM B-KONTROLL-INDATA                                
040200                 IF SW-INDATA-OK = JA                                     
040300                    IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8             
040400                    OR MSGI-KDARBTYP-SEC-IDLEV =                          
040500                                        SPACE OR LOW-VALUE                
040600                      PERFORM S06-ROER-EJ-FAELT-VISA                      
040700*   *                 --- BEHÖRIG ANVÄNDARE FÖR HUVUD-LEVERANTÖR          
040800                      PERFORM D-UPPDATERA                                 
040900                    ELSE                                                  
041000*                     --- OBEHÖRIG ANVÄNDARE                              
041100                      MOVE FEL-22 TO MOD-TEMFSFEL                         
041200                    END-IF                                                
041300                 ELSE                                                     
041400                    PERFORM E-ROER-EJ-FAELT-IN                            
041500                    PERFORM S06-ROER-EJ-FAELT-VISA                        
041600                    MOVE FEL-4 TO MOD-TEMFSFEL                            
041700                 END-IF                                                   
041800              ELSE                                                        
041900                 PERFORM F-FRAGA                                          
042000              END-IF                                                      
042100           ELSE                                                           
042200              MOVE FEL-3 TO MOD-TEMFSFEL                                  
042300           END-IF                                                         
042400         ELSE                                                             
042500            MOVE FEL-2 TO MOD-TEMFSFEL                                    
042600         END-IF                                                           
042700       ELSE                                                               
042800         MOVE FEL-1 TO MOD-TEMFSFEL                                       
042900       END-IF                                                             
043000       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O12101 + 4                      
043100       PERFORM IMS-INSERT-MSG                                             
043200     END-IF                                                               
043300     MOVE ZERO TO RETURN-CODE                                             
043400     GOBACK.                                                              
043500     EJECT                                                                
043600 A-INIT-SPARA-INPUT SECTION.                                              
043700*                                                                         
043800     IF MSG-DUBBLA-TRANSKODER                                             
043900         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I12101               
044000         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
044100         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
044200         MOVE MSG-KDTRTYP               TO MFS-KDTRTYP                    
044300     ELSE                                                                 
044400         MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I12101                 
044500         MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                  
044600         MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                 
044700         MOVE ' '                       TO MFS-KDTRTYP                    
044800     END-IF                                                               
044900     MOVE MSG-IDPFK         TO MFS-IDPFK                                  
045000                                                                          
045100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
045200     MOVE '001'             TO MSGI-KDCALL                                
045300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
045400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
045500     MOVE '2121'            TO MSGI-IDTRANS                               
045600     IF MFS-IDTRANS = '2121'                                              
045700     OR (MID-IDARTNR-IN NUMERIC                                           
045800     AND MID-IDARTNR-IN > ZERO)                                           
045900         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
046000     END-IF                                                               
046100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
046200     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
046300                                                                          
046400     IF MID-IDARTNR-IN = ALL '+'                                          
046500       MOVE JA TO SW-NY-ART                                               
046600     ELSE                                                                 
046700       MOVE NEJ TO SW-NY-ART                                              
046800       MOVE ' ' TO MFS-KDTRTYP                                            
046900     END-IF                                                               
047000                                                                          
047100     IF MFS-IDTRANS NOT = '2121'                                          
047200        MOVE ' ' TO MFS-KDTRTYP                                           
047300     END-IF                                                               
047400                                                                          
047500     MOVE LOW-VALUE TO MSG-AREA                                           
047600     MOVE 'W2O12101' TO MFS-IDMOD                                         
047700     MOVE '2121'     TO MOD-IDTRANS                                       
047800     MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                    
047900     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
048000*                                                                         
048100     PERFORM AA-RENSA-FAELT-MOD                                           
048200*                                                                         
048300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
048400                             MOD-TEMFSFEL                                 
048500                             MOD-TEMFSINF                                 
048600                                                                          
048700     .                                                                    
048800     EJECT                                                                
048900 AA-RENSA-FAELT-MOD SECTION.                                              
049000                                                                          
049100     MOVE +1 TO INDX                                                      
049200     PERFORM UNTIL INDX > 7                                               
049300       MOVE MFS-RENSA-FAELT TO MOD-IDBEST-BEST (INDX)                     
049400                               MOD-IDLEVNR-BEST(INDX)                     
049500                               MOD-TIBEST-BEST (INDX)                     
049600                               MOD-KVBEST-BEST (INDX)                     
049700                               MOD-KVBEST-BEKR-BEST (INDX)                
049800                               MOD-MARKING-BEST-BEST(INDX)                
049900       ADD +1 TO INDX                                                     
050000     END-PERFORM                                                          
050100                                                                          
050200       MOVE MFS-RENSA-FAELT TO   MOD-IDBEST-IN                            
050300                                 MOD-IDLEVNR-IN                           
050400                                 MOD-JUST-KVANT-IN                        
050500                                 MOD-U-N-A-IN                             
050700                                 MOD-BEF-TOT-BR                           
050800                                 MOD-N-BESTREST                           
050900                                 MOD-IDAVTAL-AVT(1)                       
051000                                 MOD-IDAVTAL-AVT(2)                       
051100                                 MOD-IDLEVNR-AVT(1)                       
051200                                 MOD-IDLEVNR-AVT(2)                       
051210                                 MOD-IDLEVNR-SHIP(1)                      
051220                                 MOD-IDLEVNR-SHIP(2)                      
051300                                 MOD-DATUM-AVT(1)                         
051400                                 MOD-DATUM-AVT(2)                         
051500                                 MOD-ARSANTAL-AVT(1)                      
051600                                 MOD-ARSANTAL-AVT(2)                      
051700                                 MOD-PRINK                                
051800                                 MOD-PRARTSTD                             
051900                                 MOD-PRARTBES                             
052000                                 MOD-FLMANBK                              
052100                                 MOD-KVAL                                 
052200                                 MOD-IDBEST-SW                            
052300                                 MOD-TEST-AVTNR-1                         
052400                                 MOD-TEST-AVTNR-2                         
052500                                 MOD-TEST-IDLEVNR-AVT-1                   
052600                                 MOD-TEST-IDLEVNR-AVT-2                   
052700     .                                                                    
052800     EJECT                                                                
052900                                                                          
053000 B-KONTROLL-INDATA  SECTION.                                              
053100*                                                                         
053200*************************************************************             
053300***   LEVNR,JUST-KVANT, OCH U-N-A MÅSTE VARA IFYLLDA.     ***             
053400***   OM U-N-A = A MÅSTE BESTÄLLNINGSNR VARA I FYLLT.     ***             
053500***   OM DATUM INTE ÄR IFYLLT TAR MAN DAGENS DATUM.       ***             
053600***   OM U-N-A = U MÅSTE AVTAL (WDK623) ELLER BESTÄLLNING ***             
053700***   (WDK622) FINNAS FÖR ANGIVEN LEVERANTÖR,             ***             
053800***   MED UNDANTAG FÖR DE ARTIKLAR MED AVTKOD=3 (WDK611), ***             
053900***   VILKA ÄR KONCERNLEVERANTÖRER.                       ***             
054000*************************************************************             
054100*                                                                         
054200     MOVE JA TO SW-INDATA-OK                                              
054300*                                                                         
054400     ACCEPT WS-DATUM FROM DATE                                            
054500*                                                                         
054600     IF MID-IDLEVNR NOT = ALL '+'                                         
054700***     IF MID-IDLEVNR = ZERO                                             
054800***        MOVE NEJ                 TO SW-INDATA-OK                       
054900***        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-IN-ATTR                
055000***     ELSE                                                              
055100***        IF MID-IDLEVNR NUMERIC                                         
055200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR            
055300***        ELSE                                                           
055400***           MOVE NEJ                 TO SW-INDATA-OK                    
055500***           MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-IN-ATTR             
055600***        END-IF                                                         
055700***     END-IF                                                            
055800     ELSE                                                                 
055900        MOVE NEJ                 TO SW-INDATA-OK                          
056000        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDLEVNR-IN-ATTR                   
056100     END-IF                                                               
056200                                                                          
056300     IF MID-JUST-KVANT NOT = ALL '+'                                      
056400        IF MID-JUST-KVANT NUMERIC                                         
056500           MOVE MFS-NUM-FAELT-RAETT TO MOD-JUST-KVANT-IN-ATTR             
056600        ELSE                                                              
056700           MOVE NEJ                 TO SW-INDATA-OK                       
056800           MOVE MFS-NUM-FAELT-FEL   TO MOD-JUST-KVANT-IN-ATTR             
056900        END-IF                                                            
057000     ELSE                                                                 
057100        MOVE NEJ                 TO SW-INDATA-OK                          
057200        MOVE MFS-NUM-FAELT-FEL   TO MOD-JUST-KVANT-IN-ATTR                
057300     END-IF                                                               
057400                                                                          
057500     IF MID-U-N-A NOT = ALL '+'                                           
057600        IF MID-U-N-A = 'U' OR 'N' OR 'A'                                  
057700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-U-N-A-IN-ATTR                 
057800           IF MID-U-N-A = A                                               
057900              IF MID-IDBEST-IN NOT = ALL '+'                              
058000                 IF MID-IDBEST-IN NUMERIC                                 
058100                    MOVE MFS-NUM-FAELT-RAETT TO                           
058200                                           MOD-IDBEST-IN-ATTR             
058300                 ELSE                                                     
058400                    MOVE NEJ                 TO SW-INDATA-OK              
058500                    MOVE MFS-NUM-FAELT-FEL TO                             
058600                                           MOD-IDBEST-IN-ATTR             
058700                 END-IF                                                   
058800              ELSE                                                        
058900                 MOVE NEJ                 TO SW-INDATA-OK                 
059000                 MOVE MFS-NUM-FAELT-FEL   TO                              
059100                                        MOD-IDBEST-IN-ATTR                
059200              END-IF                                                      
059300           END-IF                                                         
059400                                                                          
059500           IF MID-U-N-A = U                                               
059600             PERFORM IMS-GET-WLARTC22                                     
059700             PERFORM UNTIL (SEGMENT-SAKNAS) OR                            
059800                           ((MID-IDLEVNR = BEST-IDLEVNR-BEST) AND         
059900                           (BEST-KDBEH-BEST = 1))                         
060000               PERFORM IMS-GET-WLARTC22                                   
060100             END-PERFORM                                                  
060200             IF SEGMENT-SAKNAS                                            
060300               PERFORM IMS-GET-WLARTC23                                   
060400               PERFORM UNTIL (SEGMENT-SAKNAS) OR                          
060500                             (MID-IDLEVNR = AVT-IDLEVNR-AVT)              
060600                 PERFORM IMS-GET-WLARTC23                                 
060700               END-PERFORM                                                
060800             END-IF                                                       
060900             IF SEGMENT-SAKNAS                                            
061000               PERFORM IMS-GET-WLARTC11                                   
061100               IF CLAG-KDAVT NOT = 3                                      
061200                 MOVE NEJ                    TO SW-INDATA-OK              
061300                 PERFORM S09-SPARA-FAELT                                  
061400                 MOVE FEL-7 TO MOD-TEMFSINF                               
061500                 MOVE MFS-ALFA-FAELT-FEL TO                               
061600                                 MOD-IDLEVNR-IN-ATTR                      
061700               END-IF                                                     
061800             ELSE                                                         
061900               MOVE MFS-ALFA-FAELT-RAETT TO  MOD-IDLEVNR-IN-ATTR          
062000             END-IF                                                       
062100           END-IF                                                         
062200        ELSE                                                              
062300           MOVE NEJ                  TO SW-INDATA-OK                      
062400           MOVE MFS-ALFA-FAELT-FEL   TO MOD-U-N-A-IN-ATTR                 
062500        END-IF                                                            
062600     ELSE                                                                 
062700        MOVE NEJ                      TO SW-INDATA-OK                     
062800        MOVE MFS-ALFA-FAELT-FEL       TO MOD-U-N-A-IN-ATTR                
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200*                                                                         
063300                                                                          
063400 D-UPPDATERA SECTION.                                                     
063500     MOVE MID-IDLEVNR    TO W-IDLEVNR                                     
063600     IF MID-U-N-A = U                                                     
063700        PERFORM DA-UPPDATERA-UPPJUST                                      
063800     ELSE                                                                 
063900        IF MID-U-N-A = N                                                  
064000           PERFORM DB-UPPDATERA-NEDJUST                                   
064100        ELSE                                                              
064200           PERFORM DC-UPPDATERA-ANNUL                                     
064300        END-IF                                                            
064400     END-IF                                                               
064500     .                                                                    
064600     EJECT                                                                
064700                                                                          
064800                                                                          
064900 DA-UPPDATERA-UPPJUST SECTION.                                            
065000*                                                                         
065100***********************************************************               
065200*** OM LEVPLAN(WDD902)SAKNAS LÄGGS ETT NYTT SEGM UPP    ***               
065300*** OM LEVPLAN(WDD902)FINNS ADDERAS KVBR MED JUST-KVANT ***               
065400*** OM BESTÄLLNINGAR(WDK622)=7 TAR MAN BORT DEN ÄLSTA   ***               
065500*** OCH ETT NYTT SEGMENT LÄGGS UPP.                     ***               
065600*** OM BESTÄLLNINGAR(WDK622)<7 INGET SEGM TAS BORT      ***               
065700*** MEN ETT NYTT SEGM LÄGGS UPP.                        ***               
065800***********************************************************               
065900*                                                                         
066000     PERFORM IMS-GET-LEVNR-SEGM                                           
066100     IF SEGMENT-FINNS                                                     
066200        PERFORM IMS-GET-WLINLB01                                          
066300        IF SEGMENT-FINNS                                                  
066400           PERFORM IMS-GET-WLINLB11                                       
066500           IF SEGMENT-FINNS                                               
066600              PERFORM DAA-REPL-LEVPLAN                                    
066700           ELSE                                                           
066800              PERFORM DAB-ISRT-LEVPLAN                                    
066900           END-IF                                                         
067000        ELSE                                                              
067100           PERFORM DAC-ISRT-LEVPLAN                                       
067200        END-IF                                                            
067300        IF MID-SW-633 = JA                                                
067400           PERFORM S07-DELETE-WLARTC22                                    
067500        END-IF                                                            
067600        MOVE ZERO              TO BEST-IDBEST                             
067700        MOVE WS-DATUM          TO BEST-TIBEST                             
067800        MOVE '3'               TO BEST-KDBEH-BEST                         
067900        MOVE MID-JUST-KVANT    TO BEST-KVBEST                             
068000        MOVE MID-IDLEVNR       TO BEST-IDLEVNR-BEST                       
068100        MOVE ZERO              TO BEST-KVBEST-BEKR                        
068200        PERFORM IMS-ISRT-WLARTC22                                         
068300     ELSE                                                                 
068400        PERFORM S09-SPARA-FAELT                                           
068500        MOVE FEL-5             TO MOD-TEMFSFEL                            
068600        MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-IN-ATTR                  
068700     END-IF                                                               
068800     .                                                                    
068900     EJECT                                                                
069000 DAA-REPL-LEVPLAN SECTION.                                                
069100                                                                          
069200     MOVE LEVPL-KVBR           TO WS-LEVPL-KVBR                           
069300     COMPUTE WS-TOT-BR = MID-JUST-KVANT +                                 
069400                         WS-LEVPL-KVBR                                    
069600     MOVE MID-IDLEVNR         TO W-IDLEVNR                                
069700                                                                          
069800                                                                          
069900     MOVE WS-TOT-BR             TO LEVPL-KVBR                             
070000     MOVE MID-IDLEVNR           TO LEVPL-IDLEVNR                          
070100                                                                          
070200     PERFORM IMS-REPL-INLB                                                
070300                                                                          
070400     MOVE WS-TOT-BR             TO MOD-N-BESTREST                         
070500     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-N-BEST-REST-ATTR                   
070600     MOVE MED-2                 TO MOD-TEMFSINF                           
070700     .                                                                    
070800     EJECT                                                                
070900*                                                                         
071000 DAB-ISRT-LEVPLAN SECTION.                                                
071100                                                                          
071300     MOVE ZERO                TO LEVPL-TILEVPL                            
071400     MOVE MID-JUST-KVANT      TO LEVPL-KVBR                               
071500     MOVE MID-IDLEVNR         TO LEVPL-IDLEVNR                            
071600                                                                          
071700     PERFORM IMS-ISRT-WLINLB11                                            
071800                                                                          
071900     MOVE MID-JUST-KVANT        TO MOD-N-BESTREST                         
072000     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-N-BEST-REST-ATTR                   
072100     MOVE MED-2                 TO MOD-TEMFSINF                           
072200     .                                                                    
072300     EJECT                                                                
072400 DAC-ISRT-LEVPLAN SECTION.                                                
072500                                                                          
072710     MOVE MID-IDLEVNR         TO W-IDLEVNR                                
072800     MOVE WS-IDARTNR          TO INLB01-IDARTNR                           
072810     MOVE W-IDDC-INLB         TO INLB01-IDDC                              
072900*                                                                         
073000     PERFORM IMS-ISRT-WLINLB01                                            
073100*                                                                         
073200     MOVE ZERO                TO LEVPL-TILEVPL                            
073300     MOVE MID-JUST-KVANT      TO LEVPL-KVBR                               
073400     MOVE MID-IDLEVNR         TO LEVPL-IDLEVNR                            
073500                                                                          
073600     PERFORM IMS-ISRT-WLINLB11                                            
073700                                                                          
073800     MOVE MID-JUST-KVANT        TO MOD-N-BESTREST                         
073900     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-N-BEST-REST-ATTR                   
074000     MOVE MED-2                 TO MOD-TEMFSINF                           
074100     .                                                                    
074200     EJECT                                                                
074300*                                                                         
074400 DB-UPPDATERA-NEDJUST SECTION.                                            
074500*                                                                         
074600***********************************************************               
074700*** OM LEVPLAN(WDD902)SAKNAS LÄGG UT FELTEXT.           ***               
074800*** OM LEVPLAN(WDD902)KVBR MINUS JUST-KVANT = <0 LÄGG UT***               
074900*** FELTEXT.      OM  KVBR MINUS JUST-KVANT = >= 0 LÄGG ***               
075000*** I NYA VÄRDET. OM BESTÄLLNINGAR(WDK622)=7 TAR BORT   ***               
075100*** ÄLSTA OCH LÄGGER UPP ETT NYTT SEGMENT.              ***               
075200*** OM BESTÄLLNINGAR(WDK622)<7 INGET SEGM TAS BORT MEN  ***               
075300*** ETT NYTT SEGM LÄGGS UPP.                            ***               
075400***********************************************************               
075500*                                                                         
075600     PERFORM IMS-GET-LEVNR-SEGM                                           
075700     IF SEGMENT-FINNS                                                     
075800        PERFORM IMS-GET-WLINLB01                                          
075900        IF SEGMENT-FINNS                                                  
076000           PERFORM IMS-GET-WLINLB11                                       
076100           IF SEGMENT-FINNS                                               
076200              PERFORM DBA-TEST-BESTREST-OK                                
076300              IF SW-TEST-BESTREST = JA                                    
076400                 IF MID-SW-633 = JA                                       
076500                    PERFORM S07-DELETE-WLARTC22                           
076600                 END-IF                                                   
076700                 MOVE ZERO              TO BEST-IDBEST                    
076800                 MOVE WS-DATUM          TO BEST-TIBEST                    
076900                 MOVE '4'               TO BEST-KDBEH-BEST                
077000                 MOVE MID-JUST-KVANT    TO BEST-KVBEST                    
077100                 MOVE MID-IDLEVNR       TO BEST-IDLEVNR-BEST              
077200                 MOVE ZERO              TO BEST-KVBEST-BEKR               
077300                 PERFORM IMS-ISRT-WLARTC22                                
077400              ELSE                                                        
077500                 PERFORM S08-SPARA-FAELT                                  
077600              END-IF                                                      
077700           ELSE                                                           
077800              PERFORM S09-SPARA-FAELT                                     
077900              MOVE FEL-8         TO MOD-TEMFSFEL                          
078000              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-IN-ATTR              
078100           END-IF                                                         
078200        ELSE                                                              
078300           PERFORM S09-SPARA-FAELT                                        
078400           MOVE FEL-8         TO MOD-TEMFSFEL                             
078500           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDLEVNR-IN-ATTR              
078600        END-IF                                                            
078700     ELSE                                                                 
078800        PERFORM S09-SPARA-FAELT                                           
078900        MOVE FEL-5             TO MOD-TEMFSFEL                            
079000        MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDLEVNR-IN-ATTR              
079100     END-IF                                                               
079200     .                                                                    
079300     EJECT                                                                
079400 DBA-TEST-BESTREST-OK SECTION.                                            
079500*                                                                         
079600     MOVE NEJ                  TO SW-TEST-BESTREST                        
079700     MOVE LEVPL-KVBR           TO WS-LEVPL-KVBR                           
079800     COMPUTE WS-BESTREST = WS-LEVPL-KVBR - MID-JUST-KVANT                 
079900                                                                          
080000     IF WS-BESTREST  < 0                                                  
080100        MOVE WS-BESTREST         TO WS-BESTREST-VIS                       
080200        MOVE WS-BESTREST-VIS     TO WORK-VARDE-12                         
080300        MOVE 'FÖR STOR NEDJUSTERING MED =' TO                             
080400                                    WORK-TEXT-12                          
080500        MOVE FEL-12              TO MOD-TEMFSFEL                          
080600        MOVE MFS-NUM-FAELT-FEL   TO MOD-JUST-KVANT-IN-ATTR                
080700     ELSE                                                                 
080800        MOVE JA                    TO SW-TEST-BESTREST                    
081000        MOVE MID-IDLEVNR           TO W-IDLEVNR                           
081100                                                                          
081200        MOVE WS-BESTREST           TO LEVPL-KVBR                          
081300        MOVE MID-IDLEVNR           TO LEVPL-IDLEVNR                       
081400                                                                          
081500        PERFORM IMS-REPL-INLB                                             
081600        COMPUTE WS-BESTREST = WS-LEVPL-KVBR -                             
081700                              MID-JUST-KVANT                              
081800                                                                          
081900        MOVE WS-BESTREST           TO MOD-N-BESTREST                      
082000        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-N-BEST-REST-ATTR                
082100        MOVE MED-2                 TO MOD-TEMFSINF                        
082200     END-IF                                                               
082300     .                                                                    
082400     EJECT                                                                
082500*                                                                         
082600 DC-UPPDATERA-ANNUL SECTION.                                              
082700*                                                                         
082800***********************************************************               
082900*** OM LEVPLAN(WDD902)SAKNAS LÄGG UT FELTEXT.           ***               
083000*** OM LEVPLAN(WDD902)KVBR MINUS JUST-KVANT = <>0 LÄGG  ***               
083100*** UT FELTEXT.   OM  KVBR MINUS JUST-KVANT = 0  LÄGG   ***               
083200*** I NYA VÄRDET. OM BESTÄLLNINGAR(WDK622)=7 TAR BORT   ***               
083300*** ÄLSTA OCH LÄGGER UPP ETT NYTT SEGMENT.              ***               
083400*** OM BESTÄLLNINGAR(WDK622)<7 INGET SEGM TAS BORT MEN  ***               
083500*** ETT NYTT SEGM LÄGGS UPP.                            ***               
083600***                                                     ***               
083700*** SE I KODEN VILKA ÖVRIGA BEHANDLINGSREGLER SOM       ***               
083800*** GÄLLER.                                             ***               
083900***                                                     ***               
084000***********************************************************               
084100*                                                                         
084200     MOVE MID-IDBEST-IN       TO BEST-ID                                  
084300                                                                          
084400     PERFORM DCA-TEST-BESTNR-LEVNR                                        
084500                                                                          
084600     PERFORM DCH-LAS-WLARTC23-AVTAL                                       
084700                                                                          
084800     IF WS-TEST-AVTNR-1 > ZERO  AND                                       
084900        WS-TEST-AVTNR-1 = BEST-ID   OR                                    
085000        WS-TEST-AVTNR-2 > ZERO  AND                                       
085100        WS-TEST-AVTNR-2 = BEST-ID                                         
085200        MOVE NEJ TO SW-TEST-BESTNR-LEVNR                                  
085300***     AVTAL FINNS                                                       
085400     END-IF                                                               
085500                                                                          
085600     IF SW-TEST-BESTNR-LEVNR = JA                                         
085700                                                                          
085800        MOVE NEJ         TO SW-W2214                                      
085900        PERFORM IMS-GET-LEVNR-SEGM                                        
086000        IF SEGMENT-FINNS                                                  
086100           PERFORM IMS-GET-WLARTC01                                       
086200           MOVE ART-IDLEVNR TO TEST-WDK6-IDLEVNR                          
086300           PERFORM IMS-GET-WLINLB01                                       
086400           IF SEGMENT-FINNS                                               
086500              PERFORM IMS-GET-WLINLB11                                    
086600              IF SEGMENT-FINNS                                            
086700                 PERFORM DCB-TEST-BESTREST-OK                             
086800                    PERFORM IMS-GET-WLARTC11                              
086900                 IF SW-TEST-BESTREST = JA                                 
087000                    AND CLAG-IDINK (1:3) NUMERIC                          
087100                    AND MID-IDBEST-IN (1:3) NOT = '004'                   
087200                    IF MID-SW-633 = JA                                    
087300                       PERFORM S07-DELETE-WLARTC22                        
087400                    END-IF                                                
087500*                                                                         
087600                    PERFORM DCF-KONTROLL-BEST                             
087700                    PERFORM DCG-ANNULLERA-BEST                            
087800                 ELSE                                                     
087900                    IF MID-IDBEST-IN (1:3) = '004'                        
088000                       MOVE FEL-33 TO MOD-TEMFSFEL                        
088100                       MOVE MFS-NUM-FAELT-FEL TO                          
088200                                         MOD-IDBEST-IN-ATTR               
088300                    END-IF                                                
088400                    PERFORM S08-SPARA-FAELT                               
088500                 END-IF                                                   
088600              ELSE                                                        
088700                 PERFORM S09-SPARA-FAELT                                  
088800                 MOVE FEL-8                 TO MOD-TEMFSFEL               
088900                 MOVE MFS-ALFA-FAELT-FEL    TO                            
089000                                         MOD-IDLEVNR-IN-ATTR              
089100              END-IF                                                      
089200           ELSE                                                           
089300              PERFORM S09-SPARA-FAELT                                     
089400              MOVE FEL-8                 TO MOD-TEMFSFEL                  
089500              MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDLEVNR-IN-ATTR           
089600           END-IF                                                         
089700        ELSE                                                              
089800           PERFORM S09-SPARA-FAELT                                        
089900           MOVE FEL-5                 TO MOD-TEMFSFEL                     
090000           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDLEVNR-IN-ATTR              
090100        END-IF                                                            
090200     ELSE                                                                 
090300        MOVE JA          TO SW-AVTAL                                      
090400        MOVE NEJ         TO SW-W2214                                      
090500        MOVE NEJ         TO SW-TEST-BESTREST                              
090600        MOVE NEJ         TO SW-REPL-INLB11                                
090700        PERFORM IMS-GET-LEVNR-SEGM                                        
090800        IF SEGMENT-FINNS                                                  
090900           PERFORM IMS-GET-WLARTC01                                       
091000           MOVE ART-IDLEVNR    TO TEST-WDK6-IDLEVNR                       
091100           MOVE MID-JUST-KVANT TO WS-JUST-KVANT                           
091200           PERFORM IMS-GET-WLINLB01                                       
091300           IF SEGMENT-FINNS                                               
091400              PERFORM IMS-GET-WLINLB11                                    
091500              IF SEGMENT-FINNS                                            
091600                PERFORM DCC-TEST-BESTREST-OK                              
091700              ELSE                                                        
091800                MOVE JA                    TO SW-TEST-BESTREST            
091900                MOVE ZERO                  TO WS-BESTREST                 
092000              END-IF                                                      
092100           ELSE                                                           
092200              MOVE JA                      TO SW-TEST-BESTREST            
092300              MOVE ZERO                    TO WS-BESTREST                 
092400           END-IF                                                         
092500              PERFORM IMS-GET-WLARTC11                                    
092600           IF SW-TEST-BESTREST = JA                                       
092700              AND CLAG-IDINK (1:3) NUMERIC                                
092800              AND MID-IDBEST-IN (1:3) NOT = '004'                         
092900*                                                                         
093000              MOVE CLAG-KDAVT     TO WS-KDAVT                             
093100              PERFORM DCD-KONTROLL-AVTAL                                  
093200                                                                          
093300              IF SW-AVT-OK                                                
093400                 PERFORM DCE-ANNULLERA-AVTAL                              
093500              END-IF                                                      
093600           ELSE                                                           
093700              IF MID-IDBEST-IN (1:3) = '004'                              
093800                 MOVE FEL-33 TO MOD-TEMFSFEL                              
093900                 MOVE MFS-NUM-FAELT-FEL TO                                
094000                                         MOD-IDBEST-IN-ATTR               
094100              END-IF                                                      
094200              PERFORM S08-SPARA-FAELT                                     
094300           END-IF                                                         
094400        ELSE                                                              
094500           PERFORM S09-SPARA-FAELT                                        
094600           MOVE FEL-5                 TO MOD-TEMFSFEL                     
094700           MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDLEVNR-IN-ATTR              
094800        END-IF                                                            
094900     END-IF                                                               
095000     .                                                                    
095100     EJECT                                                                
095200 DCA-TEST-BESTNR-LEVNR SECTION.                                           
095300*                                                                         
095400     MOVE NEJ                 TO SW-TEST-BESTNR-LEVNR                     
095500     MOVE +1                  TO INDX                                     
095600     MOVE BEST-ID             TO W-IDBEST                                 
095700     PERFORM IMS-GET-BEST-SEGM                                            
095800     PERFORM UNTIL (SEGMENT-SAKNAS) OR (INDX > 7)                         
095900         IF BEST-IDLEVNR-BEST = W-IDLEVNR                                 
096000            IF BEST-IDBEST = W-IDBEST                                     
096100               MOVE JA        TO SW-TEST-BESTNR-LEVNR                     
096200            END-IF                                                        
096300         END-IF                                                           
096400         ADD +1               TO INDX                                     
096500         PERFORM IMS-GET-WLARTC22                                         
096600     END-PERFORM                                                          
096700     .                                                                    
096800     EJECT                                                                
096900 DCB-TEST-BESTREST-OK SECTION.                                            
097000*                                                                         
097100     MOVE NEJ                    TO SW-TEST-BESTREST                      
097200                                                                          
097300     COMPUTE WS-BESTREST = LEVPL-KVBR - MID-JUST-KVANT                    
097400                                                                          
097500     IF WS-BESTREST  = 0                                                  
097600        MOVE JA                  TO SW-TEST-BESTREST                      
097800        MOVE MID-IDLEVNR         TO W-IDLEVNR                             
097900                                                                          
098000        MOVE WS-BESTREST           TO LEVPL-KVBR                          
098100        MOVE MID-IDLEVNR           TO LEVPL-IDLEVNR                       
098200                                                                          
098300        PERFORM IMS-REPL-INLB                                             
098400                                                                          
098500        MOVE WS-BESTREST           TO MOD-N-BESTREST                      
098600        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-N-BEST-REST-ATTR                
098700        MOVE MED-2                 TO MOD-TEMFSINF                        
098800     ELSE                                                                 
098900        IF WS-BESTREST > 0                                                
099000           MOVE WS-BESTREST        TO WS-BESTREST-VIS                     
099100           MOVE WS-BESTREST-VIS    TO WORK-VARDE-11                       
099200           MOVE MFS-NUM-FAELT-FEL  TO MOD-JUST-KVANT-IN-ATTR              
099300           MOVE 'FÖR LITEN ANNULLATION MED =' TO                          
099400                                      WORK-TEXT-11                        
099500           MOVE FEL-11             TO MOD-TEMFSFEL                        
099600        ELSE                                                              
099700           MOVE WS-BESTREST        TO WS-BESTREST-VIS                     
099800           MOVE WS-BESTREST-VIS    TO WORK-VARDE-10                       
099900           MOVE MFS-NUM-FAELT-FEL  TO MOD-JUST-KVANT-IN-ATTR              
100000           MOVE 'FÖR STOR ANNULLATION MED =' TO                           
100100                                      WORK-TEXT-10                        
100200           MOVE FEL-10             TO MOD-TEMFSFEL                        
100300        END-IF                                                            
100400     END-IF                                                               
100500     .                                                                    
100600     EJECT                                                                
100700 DCC-TEST-BESTREST-OK SECTION.                                            
100800*                                                                         
100900     MOVE NEJ                    TO SW-TEST-BESTREST                      
101000                                                                          
101100     COMPUTE WS-BESTREST = LEVPL-KVBR - MID-JUST-KVANT                    
101200                                                                          
101300     IF WS-BESTREST  NOT > ZERO                                           
101400        MOVE JA                  TO SW-TEST-BESTREST                      
101600        MOVE MID-IDLEVNR         TO W-IDLEVNR                             
101700                                                                          
101800        IF WS-BESTREST < ZERO                                             
101900           MOVE ZERO TO WS-BESTREST                                       
102000           MOVE LEVPL-KVBR TO WS-JUST-KVANT                               
102100        END-IF                                                            
102200                                                                          
102300        MOVE JA TO SW-REPL-INLB11                                         
102400     ELSE                                                                 
102500****       WS-BESTREST > 0                                                
102600           MOVE WS-BESTREST        TO WS-BESTREST-VIS                     
102700           MOVE WS-BESTREST-VIS    TO WORK-VARDE-11                       
102800           MOVE MFS-NUM-FAELT-FEL  TO MOD-JUST-KVANT-IN-ATTR              
102900           MOVE 'FÖR LITEN ANNULLATION MED =' TO                          
103000                                      WORK-TEXT-11                        
103100           MOVE FEL-11             TO MOD-TEMFSFEL                        
103200     END-IF                                                               
103300     .                                                                    
103400     EJECT                                                                
103500 DCD-KONTROLL-AVTAL      SECTION.                                         
103600*                                                                         
103700              IF CLAG-KDAVT > ZERO                                        
103800                 IF (WS-TEST-AVTNR-1 > ZERO    AND                        
103900                     WS-TEST-AVTNR-1 = BEST-ID AND                        
104000                     WS-TEST-IDLEVNR-AVT-1 = MID-IDLEVNR) OR              
104100                    (WS-TEST-AVTNR-2 > ZERO    AND                        
104200                     WS-TEST-AVTNR-2 = BEST-ID AND                        
104300                     WS-TEST-IDLEVNR-AVT-2 = MID-IDLEVNR)                 
104400                    IF WS-TEST-AVTNR-1 > ZERO  AND                        
104500                       WS-TEST-AVTNR-2 > ZERO  AND                        
104600                       MID-IDBEST-IN (1:3) = '640'                        
104700                       MOVE FEL-6 TO MOD-TEMFSFEL                         
104800                       PERFORM DCDA-FEL-INGEN-AVT-BEST                    
104900***************     ELSE                                                  
105000***************        * ALLT OK *    *******************                 
105100                   END-IF                                                 
105200                ELSE                                                      
105300                   MOVE FEL-9 TO MOD-TEMFSFEL                             
105400                   PERFORM DCDA-FEL-INGEN-AVT-BEST                        
105500****               AVTALSID EJ RÄTT                                       
105600****               AVBRYT                                                 
105700                END-IF                                                    
105800             ELSE                                                         
105900                MOVE FEL-9    TO MOD-TEMFSFEL                             
106000                PERFORM DCDA-FEL-INGEN-AVT-BEST                           
106100****            AVBRYT                                                    
106200             END-IF                                                       
106300     .                                                                    
106400     EJECT                                                                
106500 DCDA-FEL-INGEN-AVT-BEST  SECTION.                                        
106600*                                                                         
106700        MOVE NEJ                     TO SW-AVTAL                          
106800        MOVE MFS-NUM-FAELT-FEL       TO MOD-IDBEST-IN-ATTR                
106900        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDLEVNR-IN                    
107000                                        MOD-JUST-KVANT-IN                 
107100                                        MOD-U-N-A-IN                      
107200                                        MOD-IDBEST-IN                     
107300     MOVE MFS-ADD-LAES-IN-FAELT      TO MOD-IDLEVNR-IN-ATTR               
107400                                        MOD-U-N-A-IN-ATTR                 
107500     .                                                                    
107600     EJECT                                                                
107700 DCE-ANNULLERA-AVTAL     SECTION.                                         
107800                                                                          
107900     IF SW-REPL-INLB11 = JA                                               
108000        MOVE WS-BESTREST           TO LEVPL-KVBR                          
108100        MOVE MID-IDLEVNR           TO LEVPL-IDLEVNR                       
108200                                                                          
108300        PERFORM IMS-REPL-INLB                                             
108400     END-IF                                                               
108500                                                                          
108600        MOVE WS-BESTREST           TO MOD-N-BESTREST                      
108700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-N-BEST-REST-ATTR                
108800        MOVE MED-2                 TO MOD-TEMFSINF                        
108900                                                                          
109000                       PERFORM S01-UPPD-WDGX2214                          
109100                       MOVE JA             TO SW-W2214                    
109200                                                                          
109300                       IF TEST-WDK6-IDLEVNR =                             
109400                                          WS-TEST-IDLEVNR-AVT-1           
109500                          MOVE WS-TEST-AVTNR-1 TO IDAVTAL-ID              
109600                       ELSE                                               
109700                         IF TEST-WDK6-IDLEVNR =                           
109800                                      WS-TEST-IDLEVNR-AVT-2               
109900                           MOVE WS-TEST-AVTNR-2 TO IDAVTAL-ID             
110000                         ELSE                                             
110100                          MOVE ZERO             TO IDAVTAL-ID             
110200                         END-IF                                           
110300                       END-IF                                             
110400                                                                          
110500*                                                                         
110600                       IF IDAVTAL-ID NOT = ZERO                           
110700                          MOVE WORK-IDINK TO WORK-IDINK-NUM               
110800                          ACCEPT ZZAC-TIKLOCK  FROM  TIME                 
110900                          ACCEPT ZZAC-TIAAMMDD FROM  DATE                 
111000                          ADD 1               TO WS-IDLOGLOP              
111100                          MOVE WS-IDLOGLOP    TO ZZAC-IDLOGLOP            
111200                          MOVE CLAG-IDINK (1:3) TO TEST-IDINK             
111300                                                                          
111400***                        ? TAG ÄVEN MED NAP-AVTAL, = 004                
111500                          IF (WORK-IDINK-NUM >   99 AND                   
111600                              WORK-IDINK-NUM < 790) OR                    
111700                             (WORK-IDINK-NUM > 799 AND                    
111800                              WORK-IDINK-NUM < 987) OR                    
111900                             (WORK-IDINK-NUM > 987 AND                    
111910                              WORK-IDINK-NUM < 1000) OR                   
112000                             (WORK-IDINK-NUM = 004)                       
112100                             PERFORM S02-SKAPA-B65                        
112200                          END-IF                                          
112300                       END-IF                                             
112400*                                                                         
112500                    IF CLAG-KDKSP = 1                                     
112600                       MOVE MED-1          TO MOD-TEMFSFEL                
112700                    ELSE                                                  
112800                      IF CLAG-KDKSP = 3                                   
112900                        MOVE ZERO          TO WS-KDKSP                    
113000                        MOVE JA            TO SW-611                      
113100                      ELSE                                                
113200                        IF CLAG-KDKSP = 4                                 
113300                          MOVE 1           TO WS-KDKSP                    
113400                          MOVE JA          TO SW-611                      
113500                        ELSE                                              
113600                            MOVE CLAG-KDKSP TO WS-KDKSP                   
113700                        END-IF                                            
113800                      END-IF                                              
113900                    END-IF                                                
114000*                                                                         
114100                    MOVE MID-IDBEST-IN     TO BEST-ID                     
114200*                                                                         
114300                    IF BYTE-IDINK NOT = CLAG-IDINK (1:3)                  
114400                       MOVE BYTE-IDINK     TO WS-IDINK                    
114500                       MOVE JA             TO SW-611                      
114600                       IF SW-W2214 = NEJ                                  
114700                          PERFORM S01-UPPD-WDGX2214                       
114800                       END-IF                                             
114900                    ELSE                                                  
115000                       MOVE CLAG-IDINK (1:3) TO WS-IDINK                  
115100                    END-IF                                                
115200*                                                                         
115300                    IF SW-611 = JA                                        
115400                       PERFORM S04-UPPD-WLARTC11                          
115500                    END-IF                                                
115600*                                                                         
115700                    MOVE BEST-ID         TO BEST-IDBEST                   
115800                    MOVE WS-DATUM        TO BEST-TIBEST                   
115900                    MOVE '5'             TO BEST-KDBEH-BEST               
116000                    MOVE WS-JUST-KVANT   TO BEST-KVBEST                   
116100                    MOVE MID-IDLEVNR     TO BEST-IDLEVNR-BEST             
116200                    MOVE ZERO            TO BEST-KVBEST-BEKR              
116300                    PERFORM IMS-ISRT-WLARTC22                             
116400*                                                                         
116500                    MOVE +4                TO WS-KDLPORS                  
116600                    PERFORM S05-OMSPEC-WLINLB11                           
116700     .                                                                    
116800     EJECT                                                                
116900 DCF-KONTROLL-BEST       SECTION.                                         
117000*                                                                         
117100                    IF CLAG-KDAVT = 1                                     
117200                       MOVE ZERO           TO WS-KDAVT                    
117300                       MOVE JA             TO SW-611                      
117400                       PERFORM S01-UPPD-WDGX2214                          
117500                       MOVE JA             TO SW-W2214                    
117600                                                                          
117700                       IF TEST-WDK6-IDLEVNR =                             
117800                                          WS-TEST-IDLEVNR-AVT-1           
117900                          MOVE WS-TEST-AVTNR-1 TO IDAVTAL-ID              
118000                       ELSE                                               
118100                         IF TEST-WDK6-IDLEVNR =                           
118200                                      WS-TEST-IDLEVNR-AVT-2               
118300                           MOVE WS-TEST-AVTNR-2 TO IDAVTAL-ID             
118400                         ELSE                                             
118500                          MOVE ZERO             TO IDAVTAL-ID             
118600                         END-IF                                           
118700                       END-IF                                             
118800                                                                          
118900*                                                                         
119000                       IF IDAVTAL-ID NOT = ZERO                           
119100                          MOVE WORK-IDINK TO WORK-IDINK-NUM               
119200                          ACCEPT ZZAC-TIKLOCK  FROM  TIME                 
119300                          ACCEPT ZZAC-TIAAMMDD FROM  DATE                 
119400                          ADD 1               TO WS-IDLOGLOP              
119500                          MOVE WS-IDLOGLOP    TO ZZAC-IDLOGLOP            
119600                          MOVE CLAG-IDINK (1:3) TO TEST-IDINK             
119700                                                                          
119800***                        ? TAG ÄVEN MED NAP-AVTAL, = 004                
119900                          IF (WORK-IDINK-NUM > 99 AND                     
120000                              WORK-IDINK-NUM < 790) OR                    
120100                             (WORK-IDINK-NUM > 799 AND                    
120200                              WORK-IDINK-NUM < 987) OR                    
120210                             (WORK-IDINK-NUM > 987 AND                    
120220                              WORK-IDINK-NUM < 1000) OR                   
120300                             (WORK-IDINK-NUM  = 004)                      
120400                             PERFORM S02-SKAPA-B65                        
120500                          END-IF                                          
120600                       END-IF                                             
120700                    ELSE                                                  
120800                       MOVE CLAG-KDAVT     TO WS-KDAVT                    
120900                    END-IF                                                
121000     .                                                                    
121100     EJECT                                                                
121200 DCG-ANNULLERA-BEST      SECTION.                                         
121300*                                                                         
121400                    IF CLAG-KDKSP = 1                                     
121500                       MOVE MED-1          TO MOD-TEMFSFEL                
121600                    ELSE                                                  
121700                      IF CLAG-KDKSP = 3                                   
121800                        MOVE ZERO          TO WS-KDKSP                    
121900                        MOVE JA            TO SW-611                      
122000                      ELSE                                                
122100                        IF CLAG-KDKSP = 4                                 
122200                          MOVE 1           TO WS-KDKSP                    
122300                          MOVE JA          TO SW-611                      
122400                        ELSE                                              
122500                            MOVE CLAG-KDKSP TO WS-KDKSP                   
122600                        END-IF                                            
122700                      END-IF                                              
122800                    END-IF                                                
122900*                                                                         
123000                    MOVE MID-IDBEST-IN     TO BEST-ID                     
123100*                                                                         
123200                    IF BYTE-IDINK NOT = CLAG-IDINK (1:3)                  
123300                       MOVE BYTE-IDINK     TO WS-IDINK                    
123400                       MOVE JA             TO SW-611                      
123500                       IF SW-W2214 = NEJ                                  
123600                          PERFORM S01-UPPD-WDGX2214                       
123700                       END-IF                                             
123800                    ELSE                                                  
123900                       MOVE CLAG-IDINK (1:3) TO WS-IDINK                  
124000                    END-IF                                                
124100*                                                                         
124200                    IF SW-611 = JA                                        
124300                       PERFORM S04-UPPD-WLARTC11                          
124400                    END-IF                                                
124500*                                                                         
124600                    MOVE BEST-ID         TO BEST-IDBEST                   
124700                    MOVE WS-DATUM        TO BEST-TIBEST                   
124800                    MOVE '5'             TO BEST-KDBEH-BEST               
124900                    MOVE MID-JUST-KVANT  TO BEST-KVBEST                   
125000                    MOVE MID-IDLEVNR     TO BEST-IDLEVNR-BEST             
125100                    MOVE ZERO            TO BEST-KVBEST-BEKR              
125200                    PERFORM IMS-ISRT-WLARTC22                             
125300*                                                                         
125400                    MOVE +4                TO WS-KDLPORS                  
125500                    PERFORM S05-OMSPEC-WLINLB11                           
125600     .                                                                    
125700     EJECT                                                                
125800  DCH-LAS-WLARTC23-AVTAL  SECTION.                                        
125900*************************                                                 
126000***LÄSER AVTAL 2 ST.  ***                                                 
126100*************************                                                 
126200*                                                                         
126300     MOVE ZERO                  TO WS-TEST-AVTNR-1                        
126400                                   WS-TEST-IDLEVNR-AVT-1                  
126500                                   WS-TEST-AVTNR-2                        
126600                                   WS-TEST-IDLEVNR-AVT-2                  
126700                                                                          
126800     MOVE +1    TO IND                                                    
126900     PERFORM IMS-GET-WLARTC23                                             
127000     PERFORM UNTIL (SEGMENT-SAKNAS) OR (IND > 2)                          
127100        IF IND = 1                                                        
127200           MOVE AVT-IDAVTAL     TO WS-TEST-AVTNR-1                        
127300           MOVE AVT-IDLEVNR-AVT TO WS-TEST-IDLEVNR-AVT-1                  
127400        ELSE                                                              
127500           MOVE AVT-IDAVTAL     TO WS-TEST-AVTNR-2                        
127600           MOVE AVT-IDLEVNR-AVT TO WS-TEST-IDLEVNR-AVT-2                  
127700        END-IF                                                            
127800        ADD +1                  TO IND                                    
127900        PERFORM IMS-GET-WLARTC23                                          
128000     END-PERFORM                                                          
128100     .                                                                    
128200     EJECT                                                                
128300*                                                                         
128400 E-ROER-EJ-FAELT-IN SECTION.                                              
128500*                                                                         
128600     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDBEST-IN                            
128700                                 MOD-IDLEVNR-IN                           
128800                                 MOD-JUST-KVANT-IN                        
128900                                 MOD-U-N-A-IN                             
129000                                 MOD-BEF-TOT-BR                           
129100     .                                                                    
129200     EJECT                                                                
129300 F-FRAGA SECTION.                                                         
129400*                                                                         
129500     IF SW-NY-ART = JA                                                    
129600        IF MID-IDLEVNR NOT = ALL '+'                                      
129700           MOVE MED-5                 TO MOD-TEMFSINF                     
129800           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDLEVNR-IN                   
129900*          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-IN-ATTR              
130000        END-IF                                                            
130100        IF MID-JUST-KVANT NOT = ALL '+'                                   
130200           MOVE MED-5                 TO MOD-TEMFSINF                     
130300           MOVE MFS-ROER-EJ-FAELT     TO MOD-JUST-KVANT-IN                
130400*          MOVE MFS-ADD-LAES-IN-FAELT TO                                  
130500*                                 MOD-JUST-KVANT-IN-ATTR                  
130600        END-IF                                                            
130700        IF MID-U-N-A NOT = ALL '+'                                        
130800           MOVE MED-5                 TO MOD-TEMFSINF                     
130900           MOVE MFS-ROER-EJ-FAELT     TO MOD-U-N-A-IN                     
131000*          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-U-N-A-IN-ATTR                
131100        END-IF                                                            
131200        IF MID-IDBEST-IN NOT = ALL '+'                                    
131300           MOVE MED-5                 TO MOD-TEMFSINF                     
131400           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDBEST-IN                    
131500*          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDBEST-IN-ATTR               
131600        END-IF                                                            
131700     END-IF                                                               
131800     MOVE '-' TO MOD-BINDESTRECK                                          
131900     MOVE ART-REKSIFFR TO MOD-REKSIFFR                                    
132000                                                                          
132100     PERFORM IMS-GET-WLARTC11                                             
132200                                                                          
132300     MOVE MFS-RENSA-FAELT           TO MOD-FLMANBK                        
132400                                       MOD-PRINK                          
132500                                       MOD-PRARTSTD                       
132600                                       MOD-PRARTBES                       
132700     IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                            
132800     OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                      
132900*      --- BEHÖRIG ANVÄNDARE PÅ HUVUDLEV.                                 
133000       MOVE CLAG-FLMANBK            TO MOD-FLMANBK                        
133100       MOVE CLAG-PRINK              TO MOD-PRINK                          
133200       MOVE CLAG-PRARTSTD           TO MOD-PRARTSTD                       
133400     ELSE                                                                 
133500*      --- OBEHÖRIG ANVÄNDARE PÅ HUVUDLEV. FÅR EJ SE PRISERNA             
133600       CONTINUE                                                           
133700     END-IF                                                               
133800                                                                          
134010     PERFORM FB-LAS-WLARTC22                                              
134020                                                                          
134100     PERFORM FC-LAS-WLARTC23                                              
134200                                                                          
134300     PERFORM FD-LAS-WDD902                                                
134600     .                                                                    
134700     EJECT                                                                
135394                                                                          
135395  FB-LAS-WLARTC22     SECTION.                                            
135396*********************************                                         
135397***LÄSER BESTÄLLNINGAR 7 ST.  ***                                         
135398*********************************                                         
135399*                                                                         
135400     MOVE +1 TO IX                                                        
135500     PERFORM IMS-GET-WLARTC22-FIRST                                       
135600     PERFORM UNTIL (SEGMENT-SAKNAS) OR (IX > 7)                           
135700        IF MSGI-KDARBTYP-SEC-IDLEV = BEST-IDLEVNR-BEST                    
135800        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
135900*         ---> VISA INFORAD FÖR BEHÖRIG USER                              
136000          MOVE BEST-IDBEST        TO WS-IDBEST                            
136100          MOVE WS-IDBEST          TO MOD-IDBEST-BEST(IX)                  
136200          MOVE BEST-IDLEVNR-BEST  TO MOD-IDLEVNR-BEST(IX)                 
136300          MOVE BEST-IDLEVNR-BEST  TO W-IDLEVNR                            
136400          MOVE BEST-KVBEST        TO MOD-KVBEST-BEST(IX)                  
136500          MOVE BEST-KVBEST-BEKR   TO MOD-KVBEST-BEKR-BEST(IX)             
136600          MOVE BEST-TIBEST        TO MOD-TIBEST-BEST(IX)                  
136700*                                                                         
136800          IF BEST-KDBEH-BEST = 1                                          
136900             MOVE 'BESTÄLLN' TO MOD-MARKING-BEST-BEST(IX)                 
137000          ELSE                                                            
137100             IF BEST-KDBEH-BEST = 2                                       
137200                MOVE 'AVTALSKÖP' TO MOD-MARKING-BEST-BEST(IX)             
137300             ELSE                                                         
137400                IF BEST-KDBEH-BEST = 3                                    
137500                   MOVE 'MAN.UPPJUST' TO MOD-MARKING-BEST-BEST(IX)        
137600                ELSE                                                      
137700                   IF BEST-KDBEH-BEST = 4                                 
137800                      MOVE 'MAN.NEDJUST' TO                               
137900                           MOD-MARKING-BEST-BEST(IX)                      
138000                   ELSE                                                   
138100                      IF BEST-KDBEH-BEST = 5                              
138200                         MOVE 'ANNUL' TO                                  
138300                              MOD-MARKING-BEST-BEST(IX)                   
138400                      ELSE                                                
138500                         MOVE 'BEKR ANN' TO                               
138600                              MOD-MARKING-BEST-BEST(IX)                   
138700                      END-IF                                              
138800                   END-IF                                                 
138900                END-IF                                                    
139000             END-IF                                                       
139100          END-IF                                                          
139200          ADD +1     TO IX                                                
139300        END-IF                                                            
139400        PERFORM IMS-GET-WLARTC22                                          
139500     END-PERFORM                                                          
139600     IF IX = 8                                                            
139700        MOVE JA  TO MOD-SW-633                                            
139800        MOVE BEST-IDBEST TO MOD-IDBEST-SW                                 
139900     END-IF                                                               
140000     .                                                                    
140100     EJECT                                                                
140200*                                                                         
140300                                                                          
140400  FC-LAS-WLARTC23 SECTION.                                                
140500***********************************                                       
140600***LÄSER AVTAL 2 ST. ÅT GÅNGEN  ***                                       
140700***********************************                                       
140800*                                                                         
140900     IF MFS-IDPFK = '8'                                                   
141000        MOVE MID-IDAVTAL-AVT-2 TO W2-IDAVTAL-12-X                         
141100        IF W2-IDAVTAL-12 NUMERIC                                          
141200           MOVE W2-IDAVTAL-12  TO W-IDAVTAL                               
141300           PERFORM IMS-GET-WLARTC23-KVAL                                  
141400        END-IF                                                            
141500     END-IF                                                               
141600                                                                          
141700     MOVE +1    TO IND                                                    
141800     PERFORM IMS-GET-WLARTC23                                             
141900     PERFORM UNTIL (SEGMENT-SAKNAS) OR (IND > 2)                          
142000        IF MSGI-KDARBTYP-SEC-IDLEV = AVT-IDLEVNR-AVT                      
142100        OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE                   
142200*         ---> VISA INFORAD FÖR BEHÖRIG USER                              
142300          IF IND = 1                                                      
142400             MOVE AVT-IDAVTAL      TO MOD-TEST-AVTNR-1                    
142500             MOVE AVT-IDAVTAL      TO WS-IDAVTAL                          
142600             MOVE WS-IDAVTAL       TO MOD-IDAVTAL-AVT(IND)                
142700             MOVE AVT-IDLEVNR-AVT  TO MOD-IDLEVNR-AVT(IND)                
142710             MOVE AVT-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP(IND)               
142800             MOVE AVT-IDLEVNR-AVT  TO MOD-TEST-IDLEVNR-AVT-1              
142900             MOVE AVT-TIAVTAL      TO MOD-DATUM-AVT(IND)                  
143000             MOVE AVT-KVAVTANT     TO MOD-ARSANTAL-AVT(IND)               
143100             MOVE AVT-IDLEVNR-AVT  TO WS-IDLEVNR-21-1                     
143200          ELSE                                                            
143300             MOVE AVT-IDAVTAL      TO MOD-TEST-AVTNR-2                    
143400             MOVE AVT-IDAVTAL      TO WS-IDAVTAL                          
143500             MOVE WS-IDAVTAL       TO MOD-IDAVTAL-AVT(IND)                
143600             MOVE AVT-IDLEVNR-AVT  TO MOD-IDLEVNR-AVT(IND)                
143610             MOVE AVT-IDLEVNR-SHIP TO MOD-IDLEVNR-SHIP(IND)               
143700             MOVE AVT-IDLEVNR-AVT  TO MOD-TEST-IDLEVNR-AVT-2              
143800             MOVE AVT-TIAVTAL      TO MOD-DATUM-AVT(IND)                  
143900             MOVE AVT-KVAVTANT     TO MOD-ARSANTAL-AVT(IND)               
144000             MOVE AVT-IDLEVNR-AVT  TO WS-IDLEVNR-21-2                     
144100          END-IF                                                          
144200          ADD +1                TO IND                                    
144300        END-IF                                                            
144400        PERFORM IMS-GET-WLARTC23                                          
144500        IF SEGMENT-FINNS AND IND = 3                                      
144600           MOVE MED-6           TO MOD-TEMFSINF                           
144700        END-IF                                                            
144800                                                                          
144900     END-PERFORM                                                          
145000     IF WS-IDLEVNR-21-1 NOT = SPACE                                       
145100       PERFORM IMS-GET-WDK601                                             
145200     END-IF                                                               
145300     IF WS-IDLEVNR-21-1 NOT = SPACE                                       
145400       MOVE WS-IDLEVNR-21-1 TO W-IDLEVNR-21                               
145500       PERFORM IMS-GNP-WDK621-LEV                                         
145600       IF SEGMENT-FINNS                                                   
145700         MOVE PRL-KDFPKPRI     TO MOD-KDFPKPRI (1)                        
145710         MOVE PRL-PRARTBES-PR  TO MOD-PRARTBES                            
145800       ELSE                                                               
145900         MOVE SPACE         TO MOD-KDFPKPRI (1)                           
145910         MOVE 0.1           TO MOD-PRARTBES                               
146000       END-IF                                                             
146100     END-IF                                                               
146200     IF WS-IDLEVNR-21-2 NOT = SPACE                                       
146300       PERFORM IMS-GET-WDK601                                             
146400       MOVE WS-IDLEVNR-21-2 TO W-IDLEVNR-21                               
146500       PERFORM IMS-GNP-WDK621-LEV                                         
146600       IF SEGMENT-FINNS                                                   
146700         MOVE PRL-KDFPKPRI     TO MOD-KDFPKPRI (2)                        
146710         MOVE PRL-PRARTBES-PR  TO MOD-PRARTBES                            
146800       ELSE                                                               
146900         MOVE SPACE         TO MOD-KDFPKPRI (2)                           
146910         MOVE 0.1           TO MOD-PRARTBES                               
147000       END-IF                                                             
147100     END-IF                                                               
147200     .                                                                    
147300     EJECT                                                                
147400                                                                          
147500  FD-LAS-WDD902       SECTION.                                            
147600*                                                                         
147700******************************************************                    
147800*** PER ARTIKEL.LÄSER ALLA (1-N)LEVERANTÖRERS      ***                    
147900*** BESTÄLLNINGSREST(KVBR) OCH ADDERAR IHOP DESSA  ***                    
148000******************************************************                    
148100*                                                                         
148200     MOVE +0         TO INDEXET                                           
148230                                                                          
148300        PERFORM IMS-GET-WLINLB01                                          
148400        IF SEGMENT-FINNS                                                  
148500           PERFORM IMS-LAES-NY-WDD902                                     
148600           PERFORM UNTIL SEGMENT-SAKNAS                                   
148700              ADD +1            TO INDEXET                                
148800              MOVE LEVPL-KVBR   TO WS-KVBR                                
148900              COMPUTE WS-KVBR-TOT = WS-KVBR-TOT + WS-KVBR                 
149000              PERFORM IMS-LAES-NY-WDD902                                  
149100           END-PERFORM                                                    
149200           MOVE WS-KVBR-TOT  TO MOD-BEF-TOT-BR                            
149300           MOVE INDEXET      TO MOD-KVAL                                  
149400        ELSE                                                              
149500           MOVE MED-3        TO MOD-TEMFSINF                              
149600        END-IF                                                            
149700     .                                                                    
153300     EJECT                                                                
153400  S01-UPPD-WDGX2214 SECTION.                                              
153500                                                                          
153600     MOVE WS-IDARTNR         TO XXBI-2214-IDARTNR                         
153700     PERFORM IMS-ISRT-OMRAKN-TID-SEGM                                     
153800     .                                                                    
153900     EJECT                                                                
154000*                                                                         
154100  S02-SKAPA-B65 SECTION.                                                  
154200                                                                          
154300     MOVE SPACE              TO A310-LEVNUM-GODSM                         
154400                                A310-ANT-BESTANN                          
154500     MOVE 'RY2'              TO A310-KT                                   
154600     MOVE WS-DATUM           TO A310-DATUM-UTSKR                          
154700     IF W-IDLEVNR (5:1) = SPACE                                           
154800*      LEVNUM SKALL TILLS VIDARE VARA NUMERISKT I X(5)                    
154900       MOVE ZERO             TO TALLY                                     
155000       INSPECT W-IDLEVNR TALLYING TALLY                                   
155100                          FOR CHARACTERS BEFORE INITIAL SPACE             
155200       IF TALLY = ZERO                                                    
155300          MOVE ZERO          TO WS-IDLEVNR-NUM                            
155400       ELSE                                                               
155500          MOVE W-IDLEVNR (1:TALLY)                                        
155600                             TO WS-IDLEVNR-NUM                            
155700       END-IF                                                             
155800       MOVE WS-IDLEVNR-NUM   TO A310-LEVNUM                               
155900     ELSE                                                                 
156000       MOVE W-IDLEVNR        TO A310-LEVNUM                               
156100     END-IF                                                               
156200     MOVE WS-IDARTNR         TO WS-IDARTNR-8                              
156300     MOVE WS-IDARTNR-8       TO A310-ARTNR                                
156400                                W092-SORTBGP                              
156500                                                                          
156600     MOVE WORK-IDINK         TO A310-BESTPREF                             
156700     MOVE WORK-BESTNR        TO A310-BESTLNR                              
156800     MOVE WORK-SUFFIX        TO A310-BESTSUFF                             
156900     MOVE A310-A310B65       TO ZZAC-LOGGPOST                             
157000     MOVE W092-AREA          TO ZZAC-SORTPOST                             
157100     PERFORM IMS-ISRT-ZZAC                                                
157200     .                                                                    
157300     EJECT                                                                
157400*                                                                         
157500  S04-UPPD-WLARTC11 SECTION.                                              
157600*                                                                         
157800     MOVE MID-IDLEVNR        TO W-IDLEVNR                                 
157900*                                                                         
158000     PERFORM IMS-GET-CLAG-SEGM                                            
158100*                                                                         
158200     MOVE WS-KDAVT           TO CLAG-KDAVT                                
158300     MOVE WS-KDKSP           TO CLAG-KDKSP                                
158400     MOVE WS-IDINK           TO CLAG-IDINK                                
158500                                                                          
158600     PERFORM IMS-REPL-ARTC-11                                             
158700     .                                                                    
158800     EJECT                                                                
158900                                                                          
159000  S05-OMSPEC-WLINLB11 SECTION.                                            
159100*                                                                         
159200     MOVE WS-IDARTNR         TO XXBJ-2204-IDARTNR                         
159300     MOVE WS-KDLPORS         TO XXBJ-2204-KDLPORS                         
159400     PERFORM IMS-ISRT-OMSPEC-LEVPL-SEGM                                   
159500     .                                                                    
159600     EJECT                                                                
159700                                                                          
159800 S06-ROER-EJ-FAELT-VISA SECTION.                                          
159900     MOVE +1 TO INDX                                                      
160000     PERFORM UNTIL INDX > 7                                               
160100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDBEST-BEST (INDX)                   
160200                                 MOD-IDLEVNR-BEST(INDX)                   
160300                                 MOD-TIBEST-BEST (INDX)                   
160400                                 MOD-KVBEST-BEST (INDX)                   
160500                                 MOD-KVBEST-BEKR-BEST (INDX)              
160600                                 MOD-MARKING-BEST-BEST(INDX)              
160700       ADD +1 TO INDX                                                     
160800     END-PERFORM                                                          
160900                                                                          
161000     MOVE MFS-ROER-EJ-FAELT   TO MOD-BEF-TOT-BR                           
161100                                 MOD-N-BESTREST                           
161200                                 MOD-IDAVTAL-AVT(1)                       
161300                                 MOD-IDAVTAL-AVT(2)                       
161400                                 MOD-IDLEVNR-AVT(1)                       
161500                                 MOD-IDLEVNR-AVT(2)                       
161510                                 MOD-IDLEVNR-SHIP(1)                      
161520                                 MOD-IDLEVNR-SHIP(2)                      
161600                                 MOD-DATUM-AVT(1)                         
161700                                 MOD-DATUM-AVT(2)                         
161800                                 MOD-ARSANTAL-AVT(1)                      
161900                                 MOD-ARSANTAL-AVT(2)                      
162000                                 MOD-PRINK                                
162100                                 MOD-PRARTSTD                             
162200                                 MOD-PRARTBES                             
162300                                 MOD-FLMANBK                              
162400                                 MOD-KVAL                                 
162500                                 MOD-BINDESTRECK                          
162600                                 MOD-REKSIFFR                             
162700     .                                                                    
162800     EJECT                                                                
162900 S07-DELETE-WLARTC22    SECTION.                                          
163000     MOVE +1                  TO RLIND                                    
163100     PERFORM IMS-GET-WLARTC22-FIRST                                       
163200     PERFORM UNTIL (SEGMENT-SAKNAS) OR (RLIND > 7)                        
163300        MOVE BEST-IDBEST       TO TAB-IDBEST(RLIND)                       
163400        MOVE BEST-KDBEH-BEST TO TAB-KDBEH-BEST(RLIND)                     
163500        MOVE BEST-TIBEST       TO TAB-TIBEST(RLIND)                       
163600        ADD +1                 TO RLIND                                   
163700        PERFORM IMS-GET-WLARTC22-NXT                                      
163800     END-PERFORM                                                          
163900     IF RLIND > 7                                                         
164000        MOVE NEJ TO TRAEFF                                                
164100        MOVE +7  TO RLIND                                                 
164200        PERFORM UNTIL (RLIND NOT > 0) OR (TRAEFF = JA)                    
164300           IF TAB-KDBEH-BEST(RLIND) = 1                                   
164400              MOVE RLIND TO INDX                                          
164500              ADD -1     TO INDX                                          
164600              PERFORM UNTIL (INDX NOT > 0) OR (TRAEFF = JA)               
164700                 IF TAB-KDBEH-BEST(INDX) = 1                              
164800                    MOVE JA TO TRAEFF                                     
164900                    MOVE TAB-IDBEST(RLIND) TO W-IDBEST                    
165000                    MOVE TAB-IDBEST(RLIND) TO MOD-TEMFSFEL                
165100                    MOVE TAB-TIBEST(RLIND) TO WS-TIBEST                   
165200                 ELSE                                                     
165300                    ADD -1 TO INDX                                        
165400                 END-IF                                                   
165500              END-PERFORM                                                 
165600              IF TRAEFF = NEJ                                             
165700                 ADD -1 TO RLIND                                          
165800              END-IF                                                      
165900           ELSE                                                           
166000              IF TAB-KDBEH-BEST(RLIND) = 5                                
166100                 MOVE RLIND TO INDX                                       
166200                 ADD -1     TO INDX                                       
166300                 PERFORM UNTIL (INDX NOT > 0) OR (TRAEFF = JA)            
166400                    IF TAB-KDBEH-BEST(INDX) = 5                           
166500                       MOVE JA TO TRAEFF                                  
166600                       MOVE TAB-IDBEST(RLIND) TO W-IDBEST                 
166700                       MOVE TAB-IDBEST(RLIND) TO MOD-TEMFSFEL             
166800                       MOVE TAB-TIBEST(RLIND) TO WS-TIBEST                
166900                    ELSE                                                  
167000                       ADD -1 TO INDX                                     
167100                    END-IF                                                
167200                 END-PERFORM                                              
167300                 IF TRAEFF = NEJ                                          
167400                    ADD -1 TO RLIND                                       
167500                 END-IF                                                   
167600              ELSE                                                        
167700                 MOVE JA TO TRAEFF                                        
167800                 MOVE TAB-IDBEST(RLIND) TO W-IDBEST                       
167900                 MOVE TAB-IDBEST(RLIND) TO MOD-TEMFSFEL                   
168000                 MOVE TAB-TIBEST(RLIND) TO WS-TIBEST                      
168100              END-IF                                                      
168200           END-IF                                                         
168300        END-PERFORM                                                       
168400        MOVE NEJ TO TRAEFF                                                
168500        PERFORM IMS-GET-CLAG-SEGM-UNIK                                    
168600        PERFORM IMS-GET-BEST-SEGM                                         
168700        PERFORM UNTIL (SEGMENT-SAKNAS) OR (TRAEFF = JA)                   
168800           IF BEST-TIBEST = WS-TIBEST                                     
168900              PERFORM IMS-DLET-ARTC                                       
169000              MOVE JA TO TRAEFF                                           
169100           ELSE                                                           
169200              PERFORM IMS-GET-NEXT-BEST-SEGM                              
169300           END-IF                                                         
169400        END-PERFORM                                                       
169500     END-IF                                                               
169600     .                                                                    
169700     EJECT                                                                
169800 S08-SPARA-FAELT        SECTION.                                          
169900     MOVE MFS-ROER-EJ-FAELT          TO MOD-IDLEVNR-IN                    
170000                                        MOD-JUST-KVANT-IN                 
170100                                        MOD-U-N-A-IN                      
170200                                        MOD-IDBEST-IN                     
170300                                                                          
170400     MOVE MFS-ADD-LAES-IN-FAELT      TO MOD-IDLEVNR-IN-ATTR               
170500                                        MOD-U-N-A-IN-ATTR                 
170600                                        MOD-IDBEST-IN-ATTR                
170700     .                                                                    
170800     EJECT                                                                
170900 S09-SPARA-FAELT        SECTION.                                          
171000     MOVE MFS-ROER-EJ-FAELT          TO MOD-IDLEVNR-IN                    
171100                                        MOD-JUST-KVANT-IN                 
171200                                        MOD-U-N-A-IN                      
171300                                        MOD-IDBEST-IN                     
171400     .                                                                    
171500     EJECT                                                                
171600* IMS SEKTIONER                                                           
171700     SKIP3                                                                
171800 IMS-GET-MSG SECTION.                                                     
171900     MOVE '  QC' TO GODK-STATUSKODER                                      
172000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
172100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
172200     PERFORM IMS-STATUSKONTROLL                                           
172300     .                                                                    
172400     SKIP3                                                                
172500 IMS-INSERT-MSG SECTION.                                                  
172600     IF MSGI-IDLAND-SPR = 'GB'                                            
172700        MOVE 'N' TO MFS-KDHUVOMR                                          
172800     END-IF                                                               
172900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
173000     MOVE SPACE TO GODK-STATUSKODER                                       
173100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
173200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
173300     PERFORM IMS-STATUSKONTROLL                                           
173400     .                                                                    
173500     EJECT                                                                
173600 IMS-GET-WLARTC01 SECTION.                                                
173700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
173800            DELIMITED BY SIZE INTO SSA1                                   
173900     MOVE '  GE' TO GODK-STATUSKODER                                      
174000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
174100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
174200     PERFORM IMS-STATUSKONTROLL                                           
174300     .                                                                    
174400     SKIP3                                                                
174500 IMS-GET-CLAG-SEGM SECTION.                                               
174600     MOVE 'WLARTC11*F(KDSEGKEY =1)' TO SSA1                               
174700     MOVE '  '  TO GODK-STATUSKODER                                       
174800     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-11 SSA1                 
174900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
175000     PERFORM IMS-STATUSKONTROLL                                           
175100     .                                                                    
175200     EJECT                                                                
175300 IMS-GET-CLAG-SEGM-UNIK SECTION.                                          
175400     STRING 'WLARTC01*P(IDARTNR  =' W-IDARTNR-X ')'                       
175500             DELIMITED BY SIZE INTO SSA1                                  
175600     MOVE 'WLARTC11 ' TO SSA2                                             
175700     MOVE '  '  TO GODK-STATUSKODER                                       
175800     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-11 SSA1 SSA2             
175900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
176000     PERFORM IMS-STATUSKONTROLL                                           
176100     .                                                                    
176200     EJECT                                                                
176300 IMS-GET-WLARTC11 SECTION.                                                
176400     MOVE   'WLARTC11*F  ' TO SSA1                                        
176500     MOVE '  ' TO GODK-STATUSKODER                                        
176600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
176700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
176800     PERFORM IMS-STATUSKONTROLL                                           
176900     .                                                                    
177080     EJECT                                                                
177100 IMS-GET-WDK601   SECTION.                                                
177200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
177300            DELIMITED BY SIZE INTO SSA1                                   
177400     MOVE '  GE' TO GODK-STATUSKODER                                      
177500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K601 SSA1                 
177600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
177700     PERFORM IMS-STATUSKONTROLL                                           
177800     .                                                                    
177900     SKIP3                                                                
178000 IMS-GNP-WDK621-LEV SECTION.                                              
178100     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
178200     STRING 'WLARTC21(IDLEVNR  =' W-IDLEVNR-21-X                          
178300                     '&FLHUVLEV =J)'                                      
178400          DELIMITED BY SIZE INTO SSA2                                     
178500     MOVE '  GE' TO GODK-STATUSKODER                                      
178600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-21 SSA1 SSA2             
178700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
178800     PERFORM IMS-STATUSKONTROLL                                           
178900     .                                                                    
179000     SKIP3                                                                
179091 IMS-GNP-WDK621    SECTION.                                               
179092     MOVE 'WLARTC21 '      TO SSA1                                        
179093     MOVE '  GE'              TO GODK-STATUSKODER                         
179094     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-21 SSA1                  
179095     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
179096     PERFORM IMS-STATUSKONTROLL                                           
179097     .                                                                    
179098     SKIP3                                                                
179100 IMS-GET-WLARTC22 SECTION.                                                
179200*                                                                         
179300     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
179400     MOVE 'WLARTC22 '    TO SSA2                                          
179500     MOVE '  GE' TO GODK-STATUSKODER                                      
179600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-22 SSA1 SSA2             
179700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
179800     PERFORM IMS-STATUSKONTROLL                                           
179900     .                                                                    
180000     SKIP3                                                                
180100 IMS-GET-WLARTC22-FIRST SECTION.                                          
180200*                                                                         
180300     MOVE 'WLARTC11*F(KDSEGKEY =1)' TO SSA1                               
180400     MOVE 'WLARTC22 '    TO SSA2                                          
180500     MOVE '  GE' TO GODK-STATUSKODER                                      
180600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-22                       
180700                                       SSA1 SSA2                          
180800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
180900     PERFORM IMS-STATUSKONTROLL                                           
181000     .                                                                    
181100     EJECT                                                                
181200 IMS-GET-WLARTC22-NXT   SECTION.                                          
181300*                                                                         
181400     MOVE 'WLARTC22 '    TO SSA1                                          
181500     MOVE '  GE' TO GODK-STATUSKODER                                      
181600     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-22 SSA1                 
181700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
181800     PERFORM IMS-STATUSKONTROLL                                           
181900     .                                                                    
182000     SKIP3                                                                
182100 IMS-GET-WLARTC23 SECTION.                                                
182200*                                                                         
182300     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
182400     STRING 'WLARTC23   '                                                 
182500             DELIMITED BY SIZE INTO SSA2                                  
182600     MOVE '  GE' TO GODK-STATUSKODER                                      
182700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-23 SSA1 SSA2             
182800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
182900     PERFORM IMS-STATUSKONTROLL                                           
183000     .                                                                    
183100     SKIP3                                                                
183200 IMS-GET-WLARTC23-KVAL SECTION.                                           
183300*                                                                         
183400     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
183500     STRING 'WLARTC23(IDAVTAL  =' W-IDAVTAL-X ')'                         
183600             DELIMITED BY SIZE INTO SSA2                                  
183700     MOVE '  GE' TO GODK-STATUSKODER                                      
183800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-23 SSA1 SSA2             
183900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
184000     PERFORM IMS-STATUSKONTROLL                                           
184100     .                                                                    
184200     SKIP3                                                                
184300 IMS-GET-BEST-SEGM  SECTION.                                              
184400*                                                                         
184500     MOVE 'WLARTC11*F(KDSEGKEY =1)' TO SSA1                               
184600     STRING 'WLARTC22(IDBEST   =' W-IDBEST-X ')'                          
184700             DELIMITED BY SIZE INTO SSA2                                  
184800     MOVE '  GE'  TO GODK-STATUSKODER                                     
184900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-22 SSA1 SSA2            
185000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
185100     PERFORM IMS-STATUSKONTROLL                                           
185200     .                                                                    
185300     EJECT                                                                
185400 IMS-GET-NEXT-BEST-SEGM  SECTION.                                         
185500*                                                                         
185600     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
185700     STRING 'WLARTC22(IDBEST   =' W-IDBEST-X ')'                          
185800             DELIMITED BY SIZE INTO SSA2                                  
185900     MOVE '  GE'  TO GODK-STATUSKODER                                     
186000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-22 SSA1 SSA2            
186100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
186200     PERFORM IMS-STATUSKONTROLL                                           
186300     .                                                                    
186400     EJECT                                                                
186500 IMS-GET-LEVNR-SEGM SECTION.                                              
186600*                                                                         
186700     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
186800             DELIMITED BY SIZE INTO SSA1                                  
186900     MOVE '  GE' TO GODK-STATUSKODER                                      
187000     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA SSA1                      
187100     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
187200     PERFORM IMS-STATUSKONTROLL                                           
187300     .                                                                    
187400     SKIP3                                                                
187500 IMS-GET-WLINLB01 SECTION.                                                
187600*                                                                         
187700     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
187800             DELIMITED BY SIZE INTO SSA1                                  
187900     MOVE '  GE' TO GODK-STATUSKODER                                      
188000     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA3 SSA1                     
188100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
188200     PERFORM IMS-STATUSKONTROLL                                           
188300     .                                                                    
188400     SKIP3                                                                
188500 IMS-GET-WLINLB11 SECTION.                                                
188600*                                                                         
188700     STRING 'WLINLB11*F(IDLEVNR  =' W-IDLEVNR-X ')'                       
188800             DELIMITED BY SIZE INTO SSA1                                  
188900     MOVE '  GE' TO GODK-STATUSKODER                                      
189000     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA3 SSA1                   
189100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
189200     PERFORM IMS-STATUSKONTROLL                                           
189300     .                                                                    
189400     EJECT                                                                
189500 IMS-LAES-NY-WDD902 SECTION.                                              
189600*                                                                         
189700     MOVE 'WLINLB11 '            TO SSA1                                  
189800     MOVE '  GE' TO GODK-STATUSKODER                                      
189900     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA3 SSA1                    
190000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
190100     PERFORM IMS-STATUSKONTROLL                                           
190200     .                                                                    
190300     SKIP2                                                                
190400 IMS-ISRT-WLINLB01  SECTION.                                              
190500                                                                          
190600     MOVE 'WLINLB01 '            TO SSA1                                  
190700     MOVE '  ' TO GODK-STATUSKODER                                        
190800     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA3 SSA1                   
190900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
191000     PERFORM IMS-STATUSKONTROLL                                           
191100     .                                                                    
191200     EJECT                                                                
191300 IMS-ISRT-WLINLB11  SECTION.                                              
191400                                                                          
191500     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
191600             DELIMITED BY SIZE INTO SSA1                                  
191700     MOVE 'WLINLB11 '            TO SSA2                                  
191800     MOVE '  ' TO GODK-STATUSKODER                                        
191900     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA3 SSA1 SSA2              
192000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
192100     PERFORM IMS-STATUSKONTROLL                                           
192200     .                                                                    
192300     SKIP3                                                                
192400 IMS-ISRT-WLARTC22  SECTION.                                              
192500                                                                          
192600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
192700             DELIMITED BY SIZE INTO SSA1                                  
192800     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
192900     MOVE 'WLARTC22*F'           TO SSA3                                  
193000     MOVE '  ' TO GODK-STATUSKODER                                        
193100     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-AREA-22                      
193200                                      SSA1 SSA2 SSA3                      
193300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
193400     PERFORM IMS-STATUSKONTROLL                                           
193500     .                                                                    
193600     SKIP3                                                                
193700 IMS-ISRT-OMSPEC-LEVPL-SEGM SECTION.                                      
193800                                                                          
193900     STRING 'WLXXBJ01(WDG3KEY  =' W-WDG3KEY-2203-X ')'                    
194000             DELIMITED BY SIZE INTO SSA1                                  
194100     MOVE 'WLXXBJ11 '            TO SSA2                                  
194200     MOVE '  ' TO GODK-STATUSKODER                                        
194300     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA4 SSA1 SSA2              
194400     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
194500     PERFORM IMS-STATUSKONTROLL                                           
194600     .                                                                    
194700     EJECT                                                                
194800 IMS-ISRT-ZZAC SECTION.                                                   
194900                                                                          
195000     MOVE 'WLZZAC01 '        TO SSA1                                      
195100     MOVE '  '               TO GODK-STATUSKODER                          
195200     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA5 SSA1                   
195300     MOVE ZZAC-STATUS-CODE     TO STATUS-WS                               
195400     PERFORM IMS-STATUSKONTROLL                                           
195500     .                                                                    
195600     SKIP3                                                                
195700 IMS-ISRT-OMRAKN-TID-SEGM SECTION.                                        
195800                                                                          
195900     STRING 'WLXXBI01(WDG3KEY  =' W-WDG3KEY-2213-X ')'                    
196000             DELIMITED BY SIZE INTO SSA1                                  
196100     MOVE 'WLXXBI11 '            TO SSA2                                  
196200     MOVE '  ' TO GODK-STATUSKODER                                        
196300     CALL CBLTDLI USING ISRT XXBI-PCB DLI-IO-AREA4 SSA1 SSA2              
196400     MOVE XXBI-STATUS-CODE     TO STATUS-WS                               
196500     PERFORM IMS-STATUSKONTROLL                                           
196600     .                                                                    
196700     EJECT                                                                
196800 IMS-REPL-ARTC-11 SECTION.                                                
196900                                                                          
197000     MOVE '  ' TO GODK-STATUSKODER                                        
197100     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-11                      
197200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
197300     PERFORM IMS-STATUSKONTROLL                                           
197400     .                                                                    
197500     SKIP3                                                                
197600 IMS-REPL-INLB SECTION.                                                   
197700                                                                          
197800     MOVE '  ' TO GODK-STATUSKODER                                        
197900     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA3                        
198000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
198100     PERFORM IMS-STATUSKONTROLL                                           
198200     .                                                                    
198300     SKIP3                                                                
198400 IMS-DLET-ARTC SECTION.                                                   
198500                                                                          
198600     MOVE '  ' TO GODK-STATUSKODER                                        
198700     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA                         
198800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
198900     PERFORM IMS-STATUSKONTROLL                                           
199000     .                                                                    
200800     EJECT                                                                
200900                                                                          
201010 IMS-STATUSKONTROLL SECTION.                                              
201100     SET STATUS-IX TO 1                                                   
201200     SEARCH GODK-STATUS                                                   
201300       AT END CALL FELLOG                                                 
201400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
201500     END-SEARCH                                                           
201600     .                                                                    
