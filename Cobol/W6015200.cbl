000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6015200.                                                
000400 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500 DATE-WRITTEN.   02/01/30.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000710*    FUNKTION:                                                            
000720*        PÅFYLLNING FRÅN CDC TILL SVS OCH VICE VERSA                      
000750*                                                                         
000760*        KOPIERAT PRINTNING FRÅN W6019800                                 
000761*                 SORTERING FRÅN W2035300                                 
000762*                 W6GX      FRÅN W6011500                                 
000770*                                                                         
000780*                                                                         
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDK6                                       
001110*                              WDM5                                       
001120*                              WDP7                                       
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDM5 (ARBETSBAS)                           
001400*                              WDM9 (HISTORIKBAS)                         
001500*                              WDP7 (USERDATABASEN)                       
001600*                              W6GX (LÖPNUMMER)                           
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T152                                              
002000*        MID:         W6I15201                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W6O15201                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
002901                                                                          
002910*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W6015200'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  FOERSTA-RADEN               PIC X       VALUE 'N'.                   
003701 77  FLANTAL                     PIC X       VALUE 'N'.                   
003710                                                                          
003745 77  DAGENS-DATUM                PIC X(6)    VALUE SPACE.                 
003750                                                                          
003900 77  INDX                        PIC S9(4)  VALUE +0.                     
004000 77  RAD-MAX                     PIC S9(4)  VALUE +13.                    
004010 77  RAD-IX                      PIC S9(3)  VALUE ZERO.                   
004100 77  IDSID-RAKN                  PIC S9(3)  VALUE ZERO.                   
004800                                                                          
004810     EJECT                                                                
004811 01  FILLER                      PIC X(16)   VALUE 'TABELL'.              
004812     SKIP3                                                                
004820 01  TAB-MAX                     PIC S9(9) COMP VALUE 200.                
004830     EJECT                                                                
004840*    --- TABELL SOM SORTERAS AV WINTSOR                                   
004850 01  TABELL.                                                              
004860     03  TAB-POST  OCCURS 200.                                            
004870       04  TAB-RAD.                                                       
004880         05  TAB-IDARTNR         PIC 9(9)    BLANK WHEN ZERO.             
004890         05  TAB-KVANTAL         PIC 9(6)    BLANK WHEN ZERO.             
004891         05  TAB-ADART.                                                   
004892           07  TAB-ADLAGOMR      PIC 9(2)    BLANK WHEN ZERO.             
004893           07  TAB-ADGANG        PIC 9(2)    BLANK WHEN ZERO.             
004894           07  TAB-ADPLATS       PIC 9(5)    BLANK WHEN ZERO.             
004895         05  TAB-TETRPMED        PIC X(20).                               
004896       04  TAB-SORT.                                                      
004897           07  TAB-ADLAGOMR-SORT PIC 9(2)    BLANK WHEN ZERO.             
004898           07  TAB-ADGANG-SORT   PIC 9(2)    BLANK WHEN ZERO.             
004899           07  TAB-ADPLATS-SORT  PIC 9(5)    BLANK WHEN ZERO.             
004910 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005000     88  INDATA-OK                           VALUE 'J'.                   
005100     88  INDATA-FEL                          VALUE 'N'.                   
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005510                                                                          
005520 77  UPD-RAD-SW                  PIC X       VALUE 'N'.                   
005530     88  UPD-RAD-JA                          VALUE 'J'.                   
005540     88  UPD-RAD-NEJ                         VALUE 'N'.                   
005550                                                                          
005551 77  LAST-KLAR-SW                PIC X       VALUE 'N'.                   
005552     88  LAST-KLAR-JA                        VALUE 'J'.                   
005553     88  LAST-KLAR-NEJ                       VALUE 'N'.                   
005554                                                                          
005555 77  PRINT-RAD-SW                PIC X       VALUE 'N'.                   
005556     88  PRINT-RAD-JA                        VALUE 'J'.                   
005557     88  PRINT-RAD-NEJ                       VALUE 'N'.                   
005558                                                                          
005559 77  PRINTER-VAL-SW              PIC X       VALUE 'N'.                   
005560     88  PRINTER-VAL-JA                      VALUE 'J'.                   
005561     88  PRINTER-VAL-NEJ                     VALUE 'N'.                   
005565                                                                          
005566 77  PRINTER-OK                  PIC X       VALUE 'J'.                   
005600                                                                          
006600                                                                          
006700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006800     88  EGEN-MID                            VALUE '6152'.                
006900     88  GODK-MID                            VALUE '6152'.                
007100     88  HELP-MID                            VALUE '0551'.                
007200                                                                          
007300*    --- GENERELLA ARBETSAREAOR.                                          
008301                                                                          
008302     EJECT                                                                
008303 01  FILLER                      PIC X(16)   VALUE 'WS'.                  
008304     SKIP3                                                                
008310 01  WS.                                                                  
008400*********************************************************                 
008500*    WS-MSGI-AREA-6152                                                    
008600*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
008700*           (I MSGI-SPAR-AREA)                                            
008800*********************************************************                 
008900  05 WS-MSGI-AREA-6152.                                                   
009000    10 WS-MSGI-IDTRANS-6152      PIC X(4)    VALUE '6152'.                
009100    10 WS-MSGI-SSA-KEY-ENTER     PIC X(14)   VALUE SPACE.                 
009200    10 WS-MSGI-SSA-KEY-NEXT      PIC X(14)   VALUE SPACE.                 
009210    10 WS-MSGI-RAD-KEY           OCCURS 13                                
009220                                 PIC 9(14)   VALUE ZERO.                  
009300    10 FILLER                    PIC X(786)  VALUE SPACE.                 
009301                                                                          
009310  05 WS-ADTRDEST                 PIC X(3)    VALUE SPACE.                 
009311  05 WS-ADLASTPL                 PIC X(3)    VALUE SPACE.                 
009312  05 WS-KDTRPSTA                 PIC X       VALUE SPACE.                 
009313  05 WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
009314  05 WS-IDARTNR-NUM              PIC 9(9)    VALUE ZERO.                  
009363                                                                          
009364  05 FILLER                      PIC X(16)   VALUE 'WS-SECTION'.          
009365  05 WS-SECTION                  PIC X(16)   VALUE SPACE.                 
010900                                                                          
011505       EJECT                                                              
012180     EJECT                                                                
012200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012300 01  GENERELLA-SUBPROGRAM.                                                
012400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
013000     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
013010     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
013020     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
013030     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
013100     EJECT                                                                
013110*01 -COPY W006PRT                                                         
013120     EJECT                                                                
013140*    ---AREA FÖR SUBPGM W006PRS1                                          
013150 01 FILLER                      PIC X(16)   VALUE 'W006PRS1'.             
013160                                                                          
013170*01 -COPY W006PRAR                                                        
013180                                                                          
013190 01 WS-PRINTER-PARM.                                                      
013191     03 WS-LIST-PRINTER          PIC X(8).                                
013192     03 WS-RAD.                                                           
013193       05 WS-FILLER              PIC X(1).                                
013194       05 WS-LISTRAD             PIC X(120).                              
013195     03 WS-DUMMY                 PIC X(1).                                
013196     EJECT                                                                
013197                                                                          
013198*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
013199 01  TABENTRY-PARM.                                                       
013200     03  STEGLANGD               PIC S9(9) COMP  VALUE 53.                
013201     03  ANTAL                   PIC S9(9) COMP.                          
013202     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
013203                                                                          
013204     EJECT                                                                
013210*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013300*01 -COPY WMSGINIT                                                        
013400     EJECT                                                                
013500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013600*01 -COPY WMEDAREA                                                        
013700     SKIP3                                                                
013800 01  MESSAGE-CODES.                                                       
013900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013910     03  CONFLICT                PIC X(3)    VALUE '002'.                 
014000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014010     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
014020     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014110     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
014200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014700     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
014710     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
014720     03  INF-PRINT-START         PIC X(3)    VALUE '202'.                 
014730     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014811     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
014812     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
014813     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
015030     EJECT                                                                
015100 01  FILLER                      PIC X(16)   VALUE 'DAT-AREA'.            
015200     SKIP3                                                                
015300 01  DAT-IO-AREA.                                                         
015400*    03  -COPY WDATAREA                                                   
015500     EJECT                                                                
015510                                                                          
015600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015700*                                                                         
015800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015900     SKIP3                                                                
016000*01  MID -COPY W6I15201                                                   
016100     EJECT                                                                
017200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017300     SKIP3                                                                
017400*01  -COPY WMSGAREA                                                       
017500     EJECT                                                                
017600     03  MOD REDEFINES MSG-AREA.                                          
017700*      05  -COPY W6O15201                                                 
017800     EJECT                                                                
018000*    --- AREOR FÖR W006KOM SUBMODUL                                       
018100*                                                                         
018200 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
018300*01  -COPY WMSGKOM                                                        
020693     EJECT                                                                
020700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020800     SKIP3                                                                
020900*01  -COPY WMFSAREA                                                       
021000     EJECT                                                                
021100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021200*                                                                         
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021500     SKIP3                                                                
021600 01  NYCKLAR-TILL-DLI.                                                    
021700     03  W-IDARTNR-X.                                                     
021800         05  W-IDARTNR           PIC S9(9)              COMP-3.           
021810     03  W-DADATTID-X.                                                    
021820         05  W-DADATTID          PIC 9(14)   VALUE ZERO.                  
021830     03  W-ADTRDEST              PIC X(3)    VALUE SPACE.                 
021840     03  W-IDTRPTNR-X.                                                    
021850         05  W-IDTRPTNR          PIC S9(5)              COMP-3.           
021851     03  W-IDTRPTNR-X2.                                                   
021852         05  W-IDTRPTNR2         PIC S9(5)              COMP-3.           
021860*                                                                         
021870     03  W-W6GXKEY-6017-X.                                                
021880         05  W-6017-IDHTYP       PIC X(4)    VALUE '6017'.                
021890         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
021891*                                                                         
021892     03  W-W6GXKEY-6018-X.                                                
021893         05  W-6018-KDSEGKEY     PIC X(1)    VALUE '1'.                   
021894                                                                          
021900                                                                          
028091     EJECT                                                                
028100*    --- STATUS-KOD FRÅN IMS                                              
028130 01  FILLER                      PIC X(16)   VALUE 'STATUS-WS'.           
028200 01  STATUS-WS                   PIC XX.                                  
028300     88  SEGMENT-FINNS                       VALUE '  '.                  
028400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028600     SKIP2                                                                
028700 01  GODK-STATUSKODER.                                                    
028800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028910     EJECT                                                                
028920 01  FILLER                      PIC X(16)   VALUE 'SSA'.                 
028930     SKIP3                                                                
029000 01  SSA1                        PIC X(160).                              
029100 01  SSA2                        PIC X(128).                              
029110 01  SSA3                        PIC X(128).                              
029210     EJECT                                                                
029220***********************************************                           
029230*  PRINTRADER                                 *                           
029240***********************************************                           
029250                                                                          
029260                                                                          
029270 01  L1-RAD1.                                                             
029280     03 FILLER          PIC X(23) VALUE 'VOLVO CUSTOMER SERVICE'.         
029290     03 FILLER          PIC X(18) VALUE SPACE.                            
029291     03 FILLER          PIC X(10) VALUE 'W60152-001'.                     
029292     03 FILLER          PIC X(10) VALUE SPACE.                            
029295     03 FILLER          PIC X(14) VALUE 'LASTNINGSLISTA'.                 
029296     03 FILLER          PIC X(15) VALUE SPACE.                            
029297     03 FILLER          PIC X(5)  VALUE 'DATE '.                          
029298     03 L1-RAD1-DATUM   PIC X(6).                                         
029299     03 FILLER          PIC X(8)  VALUE '   SID  '.                       
029300     03 L1-RAD1-IDSID        PIC ZZ9.                                     
029311                                                                          
029312 01  L1-RAD2.                                                             
029313     03 FILLER               PIC X(121) VALUE SPACE.                      
029314                                                                          
029315 01  L1-RAD3.                                                             
029316     03 FILLER               PIC X(121) VALUE SPACE.                      
029317                                                                          
029318 01  L1-RAD4.                                                             
029319     03 FILLER               PIC X(121) VALUE SPACE.                      
029331                                                                          
029332 01  L1-RAD5.                                                             
029333     03 FILLER               PIC X(121) VALUE SPACE.                      
029334                                                                          
029335 01  L1-RAD6.                                                             
029336     03 FILLER               PIC X(121) VALUE SPACE.                      
029337                                                                          
029338 01  L1-RAD7.                                                             
029339     03 FILLER               PIC X(4)   VALUE '    '.                     
029340     03 FILLER               PIC X(5)   VALUE 'ARTNR'.                    
029341     03 FILLER               PIC X(3)   VALUE SPACE.                      
029342     03 FILLER               PIC X(5)   VALUE 'ANTAL'.                    
029343     03 FILLER               PIC X(2)   VALUE SPACE.                      
029344     03 FILLER               PIC X(11)  VALUE 'PLATS'.                    
029345     03 FILLER               PIC X(2)   VALUE SPACE.                      
029346     03 FILLER               PIC X(3)   VALUE 'FT'.                       
029347     03 FILLER               PIC X(2)   VALUE SPACE.                      
029348     03 FILLER               PIC X(8)   VALUE 'NOTERING'.                 
029380                                                                          
029404 01  L1-RAD8.                                                             
029405     03 L1-RAD8-IDARTNR      PIC Z(8)9.                                   
029406     03 FILLER               PIC X(2)   VALUE SPACE.                      
029407     03 L1-RAD8-KVANTAL      PIC Z(5)9.                                   
029408     03 FILLER               PIC X(2)   VALUE SPACE.                      
029411     03 L1-RAD8-ADLAGOMR     PIC Z9.                                      
029412     03 FILLER               PIC X      VALUE SPACE.                      
029413     03 L1-RAD8-ADGANG       PIC Z9.                                      
029414     03 FILLER               PIC X      VALUE SPACE.                      
029415     03 L1-RAD8-ADPLATS      PIC Z(4)9.                                   
029416     03 FILLER               PIC X(2)   VALUE SPACE.                      
029417     03 L1-RAD8-BEFT         PIC Z9.                                      
029418     03 FILLER               PIC X(2)   VALUE SPACE.                      
029419     03 L1-RAD8-TETRPMED     PIC X(20)  VALUE SPACE.                      
029438                                                                          
029439                                                                          
029440                                                                          
029441 01  L2-RAD1.                                                             
029442     03 FILLER          PIC X(23) VALUE 'VOLVO CUSTOMER SERVICE'.         
029443     03 FILLER          PIC X(18) VALUE SPACE.                            
029444     03 FILLER          PIC X(10) VALUE 'W60152-001'.                     
029445     03 FILLER          PIC X(10) VALUE SPACE.                            
029446     03 FILLER          PIC X(14) VALUE 'TRANSPORTLISTA'.                 
029447     03 FILLER          PIC X(15) VALUE SPACE.                            
029448     03 FILLER          PIC X(5)  VALUE 'DATE '.                          
029449     03 L2-RAD1-DATUM   PIC X(6).                                         
029450     03 FILLER          PIC X(8)  VALUE '   SID  '.                       
029451     03 L2-RAD1-IDSID        PIC ZZ9.                                     
029452                                                                          
029453 01  L2-RAD2.                                                             
029454     03 FILLER               PIC X(121) VALUE SPACE.                      
029455                                                                          
029456 01  L2-RAD3.                                                             
029457     03 FILLER               PIC X(121) VALUE SPACE.                      
029458                                                                          
029459 01  L2-RAD4.                                                             
029462     03 FILLER               PIC X(12)  VALUE 'TRANSPORTID'.              
029463     03 L2-RAD4-TRPTNR       PIC Z(5)9.                                   
029464     03 FILLER               PIC X(103) VALUE SPACE.                      
029466                                                                          
029467 01  L2-RAD5.                                                             
029468     03 FILLER               PIC X(121) VALUE SPACE.                      
029469                                                                          
029470 01  L2-RAD6.                                                             
029471     03 FILLER               PIC X(121) VALUE SPACE.                      
029472                                                                          
029473 01  L2-RAD7.                                                             
029474     03 FILLER               PIC X(4)   VALUE '    '.                     
029475     03 FILLER               PIC X(5)   VALUE 'ARTNR'.                    
029476     03 FILLER               PIC X(3)   VALUE SPACE.                      
029477     03 FILLER               PIC X(5)   VALUE 'ANTAL'.                    
029478     03 FILLER               PIC X(2)   VALUE SPACE.                      
029479     03 FILLER               PIC X(11)  VALUE 'PLATS'.                    
029480     03 FILLER               PIC X(2)   VALUE SPACE.                      
029481     03 FILLER               PIC X(8)   VALUE 'NOTERING'.                 
029482                                                                          
029483 01  L2-RAD8.                                                             
029484     03 L2-RAD8-IDARTNR      PIC Z(8)9.                                   
029485     03 FILLER               PIC X(2)   VALUE SPACE.                      
029486     03 L2-RAD8-KVANTAL      PIC Z(5)9.                                   
029487     03 FILLER               PIC X(2)   VALUE SPACE.                      
029488     03 L2-RAD8-ADLAGOMR     PIC Z9.                                      
029489     03 FILLER               PIC X      VALUE SPACE.                      
029490     03 L2-RAD8-ADGANG       PIC Z9.                                      
029491     03 FILLER               PIC X      VALUE SPACE.                      
029492     03 L2-RAD8-ADPLATS      PIC Z(4)9.                                   
029493     03 FILLER               PIC X(2)   VALUE SPACE.                      
029494     03 L2-RAD8-TETRPMED     PIC X(20)  VALUE SPACE.                      
029495                                                                          
029496*    --- IMS FUNKTIONSKODER                                               
029497*01  -COPY W0003                                                          
029500     EJECT                                                                
029600*    ---  DLI INPUT-OUTPUT AREA                                           
033098     EJECT                                                                
033099                                                                          
033100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
033200 01  DLI-IO-WDK601.                                                       
033201*    03  -COPY WDK601                                                     
033202     EJECT                                                                
033203                                                                          
033204 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
033205 01  DLI-IO-WDK611.                                                       
033206*    03  -COPY WDK611                                                     
033207     EJECT                                                                
033208                                                                          
033209 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM501'.                      
033210 01  DLI-IO-WDM501.                                                       
033211*    03  -COPY WDM501                                                     
033212     EJECT                                                                
033213                                                                          
033214 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM511'.                      
033215 01  DLI-IO-WDM511.                                                       
033216*    03  -COPY WDM511                                                     
033217     EJECT                                                                
033218                                                                          
033219 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM521'.                      
033220 01  DLI-IO-WDM521.                                                       
033221*    03  -COPY WDM521                                                     
033223     EJECT                                                                
033224                                                                          
033225 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM901'.                      
033226 01  DLI-IO-WDM901.                                                       
033227*    03  -COPY WDM901         -PRE WDM9-                                  
033228     EJECT                                                                
033235                                                                          
033236 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDM911'.                      
033237 01  DLI-IO-WDM911.                                                       
033238*    03  -COPY WDM911                                                     
033239     EJECT                                                                
033241                                                                          
033242 01  FILLER         PIC X(24) VALUE 'DLI-IO-W6G110'.                      
033243 01  DLI-IO-W6G110.                                                       
033244*    03  -COPY W6GX6018                                                   
033251     EJECT                                                                
033252                                                                          
033260 LINKAGE SECTION.                                                         
033300                                                                          
033400*01  -COPY W0009   -PRE MSG-                                              
033500     EJECT                                                                
033700*01  -COPY W0009   -PRE ALT-                                              
033800     EJECT                                                                
034200*01  -COPY W0008   -PRE WDP7-                                             
034300     05  FILLER                  PIC X.                                   
034310     EJECT                                                                
034500*01  -COPY W0008  -PRE WDM5-                                              
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
034710*01  -COPY W0008  -PRE WDM5-2-                                            
034720     05  FILLER                  PIC X.                                   
034730     EJECT                                                                
034800*01  -COPY W0008  -PRE WDK6-                                              
034900     05  FILLER                  PIC X.                                   
035000     EJECT                                                                
035100*01  -COPY W0008  -PRE WDM9-                                              
035200     05  FILLER                  PIC X.                                   
035300     EJECT                                                                
035400*01  -COPY W0008  -PRE W6G1-                                              
035500     05  FILLER                  PIC X.                                   
036599     EJECT                                                                
036600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDM5-PCB              
036700                           WDM5-2-PCB WDM9-PCB WDK6-PCB W6G1-PCB.         
036820 MAIN SECTION.                                                            
036900     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDM5-PCB              
036901                           WDM5-2-PCB WDM9-PCB WDK6-PCB W6G1-PCB.         
037200                                                                          
037300     PERFORM IMS-GET-MSG                                                  
037400                                                                          
037500     IF SEGMENT-FINNS                                                     
037600        PERFORM A-INIT                                                    
037700        PERFORM B-KOLLA-NYCKLAR                                           
037800                                                                          
037900        IF NYCKLAR-OK                                                     
038000                                                                          
038100           IF MFS-UPDATE                                                  
038200              PERFORM G-KOLLA-INPUT                                       
038300*                                                                         
038400              IF INDATA-OK                                                
038500                 PERFORM H-UPPDATERA                                      
038600              END-IF                                                      
038700                                                                          
038800           ELSE                                                           
038900              IF MFS-FIRST                                                
039000                 PERFORM C-FOERSTA-SIDA                                   
039200              ELSE                                                        
039300                 IF MFS-NEXT                                              
039400                    PERFORM D-NAESTA-SIDA                                 
039600                 ELSE                                                     
039700                    PERFORM E-SAMMA-SIDA                                  
039900                 END-IF                                                   
040000              END-IF                                                      
040100           END-IF                                                         
040200                                                                          
040400           PERFORM F-LAES-VISA-INFO                                       
040410                                                                          
040500* ---    UPPDATERA MSGI-SPAR-AREA                                         
040510           MOVE '002'           TO MSGI-KDCALL                            
040520           MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                      
040530           MOVE MSG-SIGNON-USERID                                         
040531                                TO MSGI-IDUSER                            
040540           MOVE '6152'          TO MSGI-IDTRANS                           
040550           MOVE WS-MSGI-AREA-6152                                         
040551                                TO MSGI-SPAR-AREA                         
040560           CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                     
040570                                                                          
040600        END-IF                                                            
040700                                                                          
042300        COMPUTE MSG-KVLL = LENGTH OF MOD-W6O15201 + 4                     
042400        PERFORM IMS-INSERT-MSG                                            
042600     END-IF                                                               
042700                                                                          
042800     MOVE ZERO TO RETURN-CODE                                             
042900     GOBACK                                                               
043000     .                                                                    
043100     EJECT                                                                
043200 A-INIT SECTION.                                                          
043300                                                                          
043400     IF MSG-DUBBLA-TRANSKODER                                             
043500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I15201                 
043600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
043700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
043800     ELSE                                                                 
043900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I15201                  
044000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
044100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
044200     END-IF                                                               
044300                                                                          
044400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
044500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
044600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
044700                                                                          
044800     MOVE LOW-VALUE TO MSG-AREA                                           
044900     MOVE 'W6O15201' TO MFS-IDMOD                                         
045000     MOVE '6152' TO MOD-IDTRANS                                           
045100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
045200                                                                          
045300     IF EGEN-MID OR HELP-MID                                              
045400        CONTINUE                                                          
045500     ELSE                                                                 
045600        MOVE SPACE TO MFS-KDTRTYP                                         
045700        MOVE '7'   TO MFS-IDPFK                                           
045800     END-IF                                                               
045900     ACCEPT DAGENS-DATUM FROM DATE                                        
046000     MOVE 'SE '           TO MED-IDSKYLT                                  
046600     .                                                                    
046700     EJECT                                                                
046710 B-KOLLA-NYCKLAR SECTION.                                                 
046720                                                                          
046721     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
046722                                WS-MSGI-SSA-KEY-NEXT                      
046723                                                                          
046724******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
046725******   I SLUTET AV PROGRAMMET                                           
046726     MOVE ALL '+'            TO MSGI-WMSGINIT                             
046727     MOVE '001'              TO MSGI-KDCALL                               
046728     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
046729     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
046730     MOVE '6152'             TO MSGI-IDTRANS                              
046731     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
046740                                                                          
046741     IF EGEN-MID                                                          
046742     AND MSGI-SPAR-AREA(1:4) = '6152'                                     
046743       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-6152                         
046744     END-IF                                                               
046745                                                                          
046746                                                                          
046747     MOVE JA TO NYCKLAR-SW                                                
046748     MOVE MFS-RENSA-FAELT    TO MOD-ADLASTPL-IN                           
046750                                                                          
046760     IF MID-ADLASTPL-IN = ALL '+'                                         
046770       MOVE MID-ADLASTPL-UT  TO WS-ADLASTPL                               
046772       INSPECT WS-ADLASTPL  REPLACING LEADING '+' BY SPACE                
046780     ELSE                                                                 
046790       MOVE MID-ADLASTPL-IN  TO WS-ADLASTPL                               
046791       MOVE '7'              TO MFS-IDPFK                                 
046792       MOVE SPACE            TO MFS-KDTRTYP                               
046793     END-IF                                                               
046795                                                                          
046796     IF WS-ADLASTPL = 'SVS'                                               
046797     OR WS-ADLASTPL = 'CDC'                                               
046798       MOVE WS-ADLASTPL      TO MOD-ADLASTPL-UT                           
046799       IF WS-ADLASTPL = 'SVS'                                             
046800         MOVE 'CDC' TO WS-ADTRDEST                                        
046801       ELSE                                                               
046802         MOVE 'SVS' TO WS-ADTRDEST                                        
046803       END-IF                                                             
046804     ELSE                                                                 
046805       MOVE NEJ TO NYCKLAR-SW                                             
046806     END-IF                                                               
046807                                                                          
046808     MOVE MFS-RENSA-FAELT    TO MOD-KDTRPSTA-IN                           
046809                                MOD-IDARTNR-IN                            
046812                                                                          
046813     IF MID-KDTRPSTA-IN = ALL '+'                                         
046814       MOVE MID-KDTRPSTA-UT  TO WS-KDTRPSTA                               
046815       INSPECT WS-KDTRPSTA  REPLACING LEADING '+' BY SPACE                
046816     ELSE                                                                 
046817       MOVE MID-KDTRPSTA-IN  TO WS-KDTRPSTA                               
046818       MOVE '7'              TO MFS-IDPFK                                 
046819       MOVE SPACE            TO MFS-KDTRTYP                               
046820     END-IF                                                               
046821                                                                          
046822     IF WS-KDTRPSTA = SPACE                                               
046823     OR WS-KDTRPSTA = 'L'                                                 
046824     OR WS-KDTRPSTA = 'P'                                                 
046825       MOVE WS-KDTRPSTA      TO MOD-KDTRPSTA-UT                           
046826     ELSE                                                                 
046827       MOVE NEJ TO NYCKLAR-SW                                             
046828     END-IF                                                               
046829                                                                          
046830     IF MID-IDARTNR-IN = ALL '+'                                          
046831       IF MID-IDARTNR-UT IS NUMERIC                                       
046832         MOVE MID-IDARTNR-UT TO WS-IDARTNR                                
046833       ELSE                                                               
046834         MOVE ZERO           TO WS-IDARTNR                                
046835       END-IF                                                             
046836     ELSE                                                                 
046840       MOVE MID-IDARTNR-IN   TO WS-IDARTNR                                
046850       MOVE '7'              TO MFS-IDPFK                                 
046860       MOVE SPACE            TO MFS-KDTRTYP                               
046870     END-IF                                                               
046880                                                                          
046890     IF WS-IDARTNR IS NUMERIC                                             
047100       MOVE WS-IDARTNR       TO MOD-IDARTNR-UT                            
047110                                WS-IDARTNR-NUM                            
047200     ELSE                                                                 
047300       MOVE NEJ TO NYCKLAR-SW                                             
047400     END-IF                                                               
047500                                                                          
050500     IF NYCKLAR-FEL                                                       
050600        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
050700        CALL WMEDKONV USING MED-WMEDAREA                                  
050800                                                                          
050900        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
051000        PERFORM MFS-RENSA-FAELT-IN                                        
051100        PERFORM MFS-RENSA-FAELT-UT                                        
051200     END-IF                                                               
051300     .                                                                    
051400     EJECT                                                                
051410 C-FOERSTA-SIDA SECTION.                                                  
051412     MOVE 'C-FOERSTA-SIDA    '                                            
051414                             TO WS-SECTION                                
051420                                                                          
051430     MOVE 'SE '           TO MED-IDSKYLT                                  
051440     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
051450     CALL WMEDKONV USING MED-WMEDAREA                                     
051460     MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                                  
051470                                                                          
051480*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
051490     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
051491                                WS-MSGI-SSA-KEY-NEXT                      
051492     PERFORM MFS-RENSA-FAELT-IN                                           
051493     .                                                                    
051494     EJECT                                                                
051495 D-NAESTA-SIDA SECTION.                                                   
051497     MOVE 'D-NAESTA-SIDA     '                                            
051498                             TO WS-SECTION                                
051504     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
051506     .                                                                    
051507     EJECT                                                                
051508 E-SAMMA-SIDA SECTION.                                                    
051510     MOVE 'E-SAMMA-SIDA      '                                            
051511                             TO WS-SECTION                                
051512                                                                          
051514     IF MID-INPUT = ALL '+'                                               
051529       PERFORM MFS-RENSA-FAELT-IN                                         
051530     ELSE                                                                 
051531       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
051532       CALL WMEDKONV USING MED-WMEDAREA                                   
051533       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
051534       PERFORM EA-MID-INDATA-TILL-MOD                                     
051535     END-IF                                                               
051539     .                                                                    
051540     EJECT                                                                
051550 EA-MID-INDATA-TILL-MOD SECTION.                                          
051553     MOVE 'EA-MID-INDATA-TILL-MOD'                                        
051554                             TO WS-SECTION                                
051560                                                                          
051570     MOVE +1 TO INDX                                                      
051580                                                                          
051590     PERFORM UNTIL INDX > RAD-MAX                                         
051600        IF MID-CMD (INDX)         = ALL '+'                               
051700           MOVE MFS-RENSA-FAELT         TO MOD-CMD (INDX)                 
051800        ELSE                                                              
051900           MOVE MID-CMD (INDX)                                            
051910                             TO MOD-CMD (INDX)                            
052200        END-IF                                                            
055800        ADD +1     TO INDX                                                
055900                                                                          
056000     END-PERFORM                                                          
056100                                                                          
056200     IF MID-KVANTAL           = ALL '+'                                   
056300        MOVE MFS-RENSA-FAELT         TO MOD-KVANTAL-NY                    
056400     ELSE                                                                 
056500        MOVE MID-KVANTAL             TO MOD-KVANTAL-NY                    
056700     END-IF                                                               
056710                                                                          
056720     IF MID-LAST-KLAR         = ALL '+'                                   
056730        MOVE MFS-RENSA-FAELT         TO MOD-LAST-KLAR                     
056740     ELSE                                                                 
056750        MOVE MID-LAST-KLAR           TO MOD-LAST-KLAR                     
056760     END-IF                                                               
056770                                                                          
056800     IF MID-PRINTER          = ALL '+'                                    
056900        MOVE MFS-RENSA-FAELT         TO MOD-PRINTER                       
057000     ELSE                                                                 
057100        MOVE MID-PRINTER             TO MOD-PRINTER                       
057300     END-IF                                                               
058000     .                                                                    
058100     EJECT                                                                
084170 F-LAES-VISA-INFO SECTION.                                                
084171     MOVE 'F-LAES-VISA-INFO' TO WS-SECTION                                
084200                                                                          
084204     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
084206     MOVE 99999              TO W-IDTRPTNR                                
084207     PERFORM IMS-GU-M511                                                  
084210                                                                          
084211     IF SEGMENT-FINNS                                                     
084212                                                                          
084213       IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                               
084214          MOVE WS-MSGI-SSA-KEY-ENTER                                      
084215                               TO W-DADATTID                              
084219          PERFORM IMS-GNP-M521-KVAL                                       
084220       ELSE                                                               
084221          IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                             
084222            MOVE WS-MSGI-SSA-KEY-NEXT                                     
084223                               TO W-DADATTID                              
084224            PERFORM IMS-GNP-M521-KVAL                                     
084228                                                                          
084229          ELSE                                                            
084230                                                                          
084234            PERFORM IMS-GNP-M521                                          
084235          END-IF                                                          
084236       END-IF                                                             
084237     END-IF                                                               
084238     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
084239                                WS-MSGI-SSA-KEY-NEXT                      
084240                                                                          
084241     MOVE 1                  TO INDX                                      
084242     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
084243     PERFORM UNTIL INDX > RAD-MAX                                         
084244     OR SEGMENT-SAKNAS                                                    
084245                                                                          
084246       IF  (WS-KDTRPSTA = SPACE                                           
084247       OR   WS-KDTRPSTA = AVG-KDTRPSTA)                                   
084248       AND (WS-IDARTNR-NUM  = ZERO                                        
084249       OR   WS-IDARTNR-NUM  = AVG-IDARTNR)                                
084250         MOVE AVG-IDARTNR    TO MOD-IDARTNR (INDX)                        
084251                                 W-IDARTNR                                
084252         MOVE AVG-KVANTAL    TO MOD-KVANTAL (INDX)                        
084253         MOVE AVG-DADATTID(3:6)                                           
084254                             TO MOD-TIDATUM (INDX)                        
084255         MOVE AVG-KDTRPSTA   TO MOD-KDTRPSTA (INDX)                       
084256         MOVE MFS-ADD-LAES-IN-FAELT                                       
084257                             TO MOD-KDTRPSTA-ATTR (INDX)                  
084258         MOVE AVG-IDUSER-TRP TO MOD-IDUSER (INDX)                         
084259         MOVE AVG-TETRPMED   TO MOD-TETRPMED (INDX)                       
084260         MOVE AVG-DADATTID   TO WS-MSGI-RAD-KEY (INDX)                    
084261                                                                          
084262         PERFORM IMS-GU-K611                                              
084264                                                                          
084265         IF WS-ADLASTPL = 'CDC'                                           
084266           MOVE CLAG-ADLAGOMR                                             
084267                             TO MOD-ADLAGOMR (INDX)                       
084268           MOVE CLAG-ADGANG  TO MOD-ADGANG (INDX)                         
084269           MOVE CLAG-ADPLATS TO MOD-ADPLATS (INDX)                        
084270         END-IF                                                           
084271                                                                          
084272         IF WS-ADLASTPL = 'SVS'                                           
084273           MOVE CLAG-ADLAGOMR-SVS                                         
084274                             TO MOD-ADLAGOMR (INDX)                       
084275           MOVE CLAG-ADGANG-SVS                                           
084276                             TO MOD-ADGANG (INDX)                         
084277           MOVE CLAG-ADPLATS-SVS                                          
084278                             TO MOD-ADPLATS (INDX)                        
084279         END-IF                                                           
084280         MOVE CLAG-BEFT      TO MOD-BEFT (INDX)                           
084281                                                                          
084282         IF INDX = 1                                                      
084283           MOVE AVG-DADATTID TO WS-MSGI-SSA-KEY-ENTER                     
084285         END-IF                                                           
084286                                                                          
084287         ADD 1               TO INDX                                      
084288                                                                          
084289       END-IF                                                             
084290                                                                          
084293       PERFORM IMS-GNP-M521                                               
084294                                                                          
084295     END-PERFORM                                                          
084296                                                                          
084297     IF SEGMENT-FINNS                                                     
084298        MOVE AVG-DADATTID    TO WS-MSGI-SSA-KEY-NEXT                      
084300        MOVE 'SE '           TO MED-IDSKYLT                               
084301        MOVE INF-MORE-INFO-EXISTS                                         
084302                             TO MED-IDMFSFEL                              
084303        CALL WMEDKONV USING MED-WMEDAREA                                  
084304        MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                              
084305     ELSE                                                                 
084306        MOVE SPACE           TO WS-MSGI-SSA-KEY-NEXT                      
084307     END-IF                                                               
084308                                                                          
084309     PERFORM UNTIL INDX > RAD-MAX                                         
084310                                                                          
084318        MOVE MFS-STAENG-FAELT                                             
084319                             TO MOD-CMD-ATTR (INDX)                       
084321        MOVE MFS-RENSA-FAELT TO MOD-CMD (INDX)                            
084322                                MOD-IDARTNR (INDX)                        
084323                                MOD-KVANTAL (INDX)                        
084324                                MOD-ADLAGOMR (INDX)                       
084325                                MOD-ADGANG (INDX)                         
084326                                MOD-ADPLATS (INDX)                        
084327                                MOD-BEFT (INDX)                           
084328                                MOD-TIDATUM (INDX)                        
084329                                MOD-KDTRPSTA (INDX)                       
084330                                MOD-IDUSER (INDX)                         
084331                                MOD-TETRPMED (INDX)                       
084332        MOVE ZERO            TO WS-MSGI-RAD-KEY (INDX)                    
084333                                                                          
084334       ADD 1                 TO INDX                                      
084335                                                                          
084336     END-PERFORM                                                          
094900     .                                                                    
094910     EJECT                                                                
095000 G-KOLLA-INPUT SECTION.                                                   
095001     MOVE 'G-KOLLA-INPUT'    TO WS-SECTION                                
095100                                                                          
095110                                                                          
095198                                                                          
095200     MOVE JA                 TO INDATA-SW                                 
095220     MOVE NEJ                TO UPD-RAD-SW                                
095230                                LAST-KLAR-SW                              
095240                                PRINT-RAD-SW                              
095250                                PRINTER-VAL-SW                            
095300                                                                          
095400     IF  MID-INPUT      = ALL '+'                                         
096800        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
096900        MOVE NEJ TO INDATA-SW                                             
097100     ELSE                                                                 
097110                                                                          
097200        MOVE +1              TO INDX                                      
097300        MOVE NEJ             TO FLANTAL                                   
097400        PERFORM UNTIL INDX   > RAD-MAX                                    
097463                                                                          
097500            IF  MID-CMD (INDX)  NOT = ALL '+'                             
097600            AND MID-CMD (INDX)  NOT = SPACE                               
097610                                                                          
097700                IF  (MID-CMD (INDX) = 'P'                                 
097701                AND  MID-KDTRPSTA (INDX) = SPACE)                         
097702                OR  (MID-CMD (INDX) = 'L'                                 
097703                AND (MID-KDTRPSTA (INDX) = SPACE                          
097704                OR   MID-KDTRPSTA (INDX) = 'P'))                          
097705                OR  (MID-CMD (INDX) = 'X'                                 
097706                AND  (MID-KDTRPSTA (INDX) = 'L' OR 'P'))                  
097707                OR  ((MID-CMD (INDX) = 'C'                                
097708                OR   MID-CMD (INDX) = 'Ä')                                
097709                AND  MID-KDTRPSTA (INDX) = SPACE                          
097710                AND  MID-KVANTAL IS NUMERIC)                              
097711                OR   MID-CMD (INDX) = 'B'                                 
097712                  MOVE JA    TO UPD-RAD-SW                                
097713                  MOVE MFS-ALFA-FAELT-RAETT                               
097714                             TO MOD-CMD-ATTR(INDX)                        
097715                  IF MID-CMD (INDX) = 'P'                                 
097716                    MOVE JA  TO PRINT-RAD-SW                              
097717                  END-IF                                                  
097718                  IF MID-CMD (INDX) = 'C' OR 'Ä'                          
097719                    IF FLANTAL = JA                                       
097720** FÅR BARA UPPDATERA ANTAL PÅ 1 RAD ÅT GÅNGEN                            
097721                      MOVE MFS-ALFA-FAELT-FEL                             
097723                               TO MOD-CMD-ATTR (INDX)                     
097725                      MOVE ERR-CORR-HILITE-FLDS                           
097726                               TO MED-IDMFSFEL                            
097727                      MOVE NEJ TO INDATA-SW                               
097728                    ELSE                                                  
097729                      MOVE JA  TO FLANTAL                                 
097730                    END-IF                                                
097731                  END-IF                                                  
097740                ELSE                                                      
098000                    MOVE MFS-ALFA-FAELT-FEL                               
098010                               TO MOD-CMD-ATTR (INDX)                     
098200                    MOVE ERR-CORR-HILITE-FLDS                             
098300                             TO MED-IDMFSFEL                              
098400                    MOVE NEJ TO INDATA-SW                                 
098600                END-IF                                                    
099500            ELSE                                                          
099600                MOVE MFS-RENSA-FAELT                                      
099610                             TO MOD-CMD-ATTR (INDX)                       
099700            END-IF                                                        
099840                                                                          
099900            ADD 1 TO INDX                                                 
100000         END-PERFORM                                                      
100010                                                                          
100020         IF MID-LAST-KLAR = ALL '+' OR SPACE                              
100030            CONTINUE                                                      
100040         ELSE                                                             
100050           IF MID-LAST-KLAR = JA                                          
100051               MOVE JA       TO LAST-KLAR-SW                              
100060               MOVE MFS-ALFA-FAELT-RAETT                                  
100070                             TO MOD-LAST-KLAR-ATTR                        
100080           ELSE                                                           
100090               MOVE MFS-ALFA-FAELT-FEL                                    
100091                             TO MOD-LAST-KLAR-ATTR                        
100092               MOVE ERR-CORR-HILITE-FLDS                                  
100093                             TO MED-IDMFSFEL                              
100094               MOVE NEJ TO INDATA-SW                                      
100095           END-IF                                                         
100096         END-IF                                                           
100097                                                                          
100099         IF MID-PRINTER = ALL '+' OR SPACE                                
100100            CONTINUE                                                      
100101         ELSE                                                             
100103           MOVE JA           TO PRINTER-VAL-SW                            
100104           PERFORM GA-KOLLA-PRINTER                                       
100116         END-IF                                                           
100120     END-IF                                                               
100121                                                                          
100122     IF INDATA-FEL                                                        
100124        CALL WMEDKONV USING MED-WMEDAREA                                  
100125        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
100126        PERFORM MFS-ROER-EJ-FAELT-UT                                      
100127        PERFORM MFS-ROER-EJ-FAELT-IN                                      
100129     ELSE                                                                 
100131                                                                          
100149         IF UPD-RAD-JA                                                    
100150         AND LAST-KLAR-JA                                                 
100151*****                                                                     
100152*****     INTE MÖJLIGT ATT UPPDATERA BÅDE ENSKILD RAD                     
100160*****     OCH LASTNING SAMTIDIGT                                          
100161*****                                                                     
100170             MOVE NEJ     TO INDATA-SW                                    
100180             MOVE CONFLICT TO MED-IDMFSFEL                                
100190             CALL WMEDKONV USING MED-WMEDAREA                             
100191             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
100204             PERFORM MFS-ROER-EJ-FAELT-IN                                 
100205             PERFORM MFS-ROER-EJ-FAELT-UT                                 
100206         ELSE                                                             
100210                                                                          
100222           IF (PRINT-RAD-JA                                               
100223           OR  LAST-KLAR-JA)                                              
100224           AND PRINTER-VAL-NEJ                                            
100240*****                                                                     
100250*****       MÅSTE ANGE PRINTER OM RADEN ÄR MARKERAD FÖR PRINTNING         
100251*****       ELLER LASTNING KLAR OCH VICE VERSA                            
100270*****                                                                     
100280               MOVE NEJ   TO INDATA-SW                                    
100281               MOVE MFS-ALFA-FAELT-FEL                                    
100282                             TO MOD-PRINTER-ATTR                          
100290               MOVE ERR-FEL-PRINTER TO MED-IDMFSFEL                       
100292               CALL WMEDKONV USING MED-WMEDAREA                           
100293               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
100294               PERFORM MFS-ROER-EJ-FAELT-IN                               
100295               PERFORM MFS-ROER-EJ-FAELT-UT                               
100296           END-IF                                                         
101100         END-IF                                                           
101300     END-IF                                                               
105549     .                                                                    
105550     EJECT                                                                
105560 GA-KOLLA-PRINTER SECTION.                                                
105561     MOVE 'GA-KOLLA-PRINTER' TO WS-SECTION                                
105563                                                                          
105564     MOVE 001                TO PRT-KDCALL                                
105565     MOVE '6M'               TO PRT-IDPRTLST(1:2)                         
105566     MOVE MID-PRINTER        TO PRT-IDPRTLST(3:6)                         
105567     CALL W006PRT USING PRT-W006PRT                                       
105568     IF PRT-KDSVAR = 'F'                                                  
105569       MOVE MFS-ALFA-FAELT-FEL                                            
105570                             TO MOD-PRINTER-ATTR                          
105571       MOVE NEJ TO PRINTER-OK                                             
105572       MOVE NEJ              TO INDATA-SW                                 
105573       MOVE ERR-FEL-PRINTER  TO MED-IDMFSFEL                              
105574       CALL WMEDKONV USING MED-WMEDAREA                                   
105575       MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                              
105576       PERFORM MFS-ROER-EJ-FAELT-UT                                       
105577       PERFORM MFS-ROER-EJ-FAELT-IN                                       
105578     ELSE                                                                 
105579       MOVE PRT-BEPRTLST     TO MOD-TEMFSFEL                              
105580       MOVE PRT-IDPRTLST     TO WS-LIST-PRINTER                           
105581       MOVE MID-PRINTER      TO MOD-PRINTER                               
105582       MOVE JA TO PRINTER-OK                                              
105583       MOVE MFS-ALFA-FAELT-RAETT                                          
105584                             TO MOD-PRINTER-ATTR                          
105585     END-IF                                                               
105586                                                                          
105587     .                                                                    
105588     EJECT                                                                
105590 H-UPPDATERA SECTION.                                                     
105591     MOVE 'H-UPPDATERA'      TO WS-SECTION                                
105600                                                                          
105610     IF PRINT-RAD-JA                                                      
105700       PERFORM S01-PRT-OPEN                                               
105701       PERFORM HE-SKRIV-L1-RAD1-7                                         
105710     END-IF                                                               
105800                                                                          
105860     MOVE +1 TO INDX                                                      
105900     PERFORM UNTIL INDX > RAD-MAX                                         
107137        IF MID-CMD (INDX) = 'P'                                           
107139           PERFORM HF-SKRIV-RAD8-A                                        
107141        END-IF                                                            
107142        IF MID-CMD (INDX) = 'L'                                           
107150           PERFORM HB-LASTA                                               
107160        END-IF                                                            
107170        IF MID-CMD (INDX) = 'X'                                           
107180           PERFORM HC-AENDRA-STATUS                                       
107190        END-IF                                                            
107200        IF MID-CMD (INDX) = 'B'                                           
107300           PERFORM HD-BORTTAG                                             
107400        END-IF                                                            
107410        IF MID-CMD (INDX) = 'C' OR 'Ä'                                    
107420           PERFORM HI-UPD-ANTAL                                           
107430        END-IF                                                            
107500        ADD 1 TO INDX                                                     
107600     END-PERFORM                                                          
107601                                                                          
107602     IF PRINT-RAD-JA                                                      
107603                                                                          
107604       MOVE INF-PRINT-START TO MED-IDMFSINF                               
107605       CALL WMEDKONV USING MED-WMEDAREA                                   
107606       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
107607                                                                          
107608       PERFORM S03-PRT-CLOSE                                              
107609     END-IF                                                               
107610                                                                          
107611     IF MID-LAST-KLAR = JA                                                
107620        PERFORM HA-LAST-KLAR                                              
107630     END-IF                                                               
107700                                                                          
108400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
108500     CALL WMEDKONV USING MED-WMEDAREA                                     
108700     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
108800     PERFORM MFS-RENSA-FAELT-IN                                           
108900     .                                                                    
109000     EJECT                                                                
112100 HA-LAST-KLAR SECTION.                                                    
112101     MOVE 'HA-LAST-KLAR'     TO WS-SECTION                                
112200                                                                          
112206     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
112207     MOVE 99999              TO W-IDTRPTNR                                
112208                                                                          
112210     PERFORM IMS-GU-M511                                                  
112211                                                                          
112214     PERFORM IMS-GHU-W6G110                                               
112215     IF 6018-IDTRPTNR-INTFL = 99998                                       
112216       MOVE 60000            TO 6018-IDTRPTNR-INTFL                       
112217     ELSE                                                                 
112218       ADD 1                 TO 6018-IDTRPTNR-INTFL                       
112219     END-IF                                                               
112220     PERFORM IMS-REPL-W6G110                                              
112221                                                                          
112222     MOVE 6018-IDTRPTNR-INTFL                                             
112223                             TO TRAN-IDTRPTNR                             
112224                                L2-RAD4-TRPTNR                            
112225                                W-IDTRPTNR2                               
112226     PERFORM IMS-ISRT-M511-PCB2                                           
112227                                                                          
112228     MOVE ZERO               TO INDX                                      
112229     PERFORM IMS-GHNP-M521                                                
112230                                                                          
112231     PERFORM UNTIL SEGMENT-SAKNAS                                         
112232       IF AVG-KDTRPSTA = 'L'                                              
112233         ADD 1               TO INDX                                      
112234         MOVE AVG-IDARTNR    TO TAB-IDARTNR  (INDX)                       
112235                                 W-IDARTNR                                
112236         MOVE AVG-KVANTAL    TO TAB-KVANTAL  (INDX)                       
112237         PERFORM IMS-GU-K611                                              
112238         IF WS-ADTRDEST = 'CDC'                                           
112239           MOVE CLAG-ADLAGOMR TO TAB-ADLAGOMR (INDX)                      
112240                                  TAB-ADLAGOMR-SORT (INDX)                
112241           MOVE CLAG-ADGANG  TO TAB-ADGANG   (INDX)                       
112242                                  TAB-ADGANG-SORT (INDX)                  
112243           MOVE CLAG-ADPLATS TO TAB-ADPLATS  (INDX)                       
112244                                  TAB-ADPLATS-SORT (INDX)                 
112245         END-IF                                                           
112246         IF WS-ADTRDEST = 'SVS'                                           
112247           MOVE CLAG-ADLAGOMR-SVS                                         
112248                               TO TAB-ADLAGOMR (INDX)                     
112249                                  TAB-ADLAGOMR-SORT (INDX)                
112250           MOVE CLAG-ADGANG-SVS                                           
112251                               TO TAB-ADGANG (INDX)                       
112252                                  TAB-ADGANG-SORT (INDX)                  
112253           MOVE CLAG-ADPLATS-SVS                                          
112254                               TO TAB-ADPLATS (INDX)                      
112255                                  TAB-ADPLATS-SORT (INDX)                 
112256         END-IF                                                           
112257         MOVE AVG-TETRPMED   TO TAB-TETRPMED (INDX)                       
112258                                                                          
112259         MOVE DAGENS-DATUM   TO AVG-TIUPPDAT                              
112260         MOVE MSGI-IDUSER    TO AVG-IDUSER-TRP                            
112261         PERFORM IMS-ISRT-M521-PCB2                                       
112262         PERFORM IMS-DLET-M521                                            
112263         PERFORM HAA-SKAPA-HIST-POST                                      
112264         IF INDX = 1                                                      
112265           MOVE SPACE        TO WS-MSGI-SSA-KEY-ENTER                     
112266         END-IF                                                           
112267       END-IF                                                             
112268       PERFORM IMS-GHNP-M521                                              
112269     END-PERFORM                                                          
112270     MOVE INDX               TO TAB-MAX                                   
112271                                ANTAL                                     
112272*                                                                         
112273     CALL WINTSOR USING TABELL STEGLANGD ANTAL                            
112274                  TAB-SORT (1) NYCKELLANGD                                
112275                                                                          
112276     PERFORM S01-PRT-OPEN                                                 
112280     PERFORM HG-SKRIV-L2-RAD1-7                                           
112290     MOVE 1                  TO INDX                                      
112291     PERFORM UNTIL INDX > TAB-MAX                                         
112293       PERFORM HH-SKRIV-RAD8-B                                            
112294       ADD 1                 TO INDX                                      
112295     END-PERFORM                                                          
112296     PERFORM S03-PRT-CLOSE                                                
112300     .                                                                    
112310     EJECT                                                                
112320 HAA-SKAPA-HIST-POST SECTION.                                             
112321     MOVE 'HAA-SKAPA-HIST-POST'                                           
112322                             TO WS-SECTION                                
112330                                                                          
112350     MOVE AVG-IDARTNR        TO WDM9-ART-IDARTNR                          
112391     PERFORM IMS-ISRT-M901                                                
112392     MOVE SPACE              TO TRP-WDM911                                
112393     COMPUTE TRP-DADATTID-9KOMPL =                                        
112394             99999999999999 - AVG-DADATTID                                
112404     MOVE WS-ADTRDEST        TO TRP-ADTRDEST                              
112406     MOVE TRAN-IDTRPTNR      TO TRP-IDTRPTNR                              
112407     MOVE AVG-KVANTAL        TO TRP-KVANTAL                               
112408     MOVE ZERO               TO TRP-TITRPMOT                              
112409     MOVE MSGI-IDUSER        TO TRP-IDUSER-TRP                            
112410     MOVE AVG-TETRPMED       TO TRP-TETRPMED                              
112411     PERFORM IMS-ISRT-M911                                                
112412     .                                                                    
112413     EJECT                                                                
112414 HB-LASTA SECTION.                                                        
112415     MOVE 'HB-LASTA'         TO WS-SECTION                                
112416                                                                          
112422     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
112423     MOVE 99999              TO W-IDTRPTNR                                
112424     MOVE WS-MSGI-RAD-KEY (INDX)                                          
112425                             TO W-DADATTID                                
112426     PERFORM IMS-GHU-M521                                                 
112427     MOVE 'L'                TO AVG-KDTRPSTA                              
112428     MOVE DAGENS-DATUM       TO AVG-TIUPPDAT                              
112429     MOVE MSGI-IDUSER        TO AVG-IDUSER-TRP                            
112430     PERFORM IMS-REPL-M521                                                
112431     .                                                                    
112440     EJECT                                                                
112500 HC-AENDRA-STATUS SECTION.                                                
112510     MOVE 'HC-AENDRA-STATUS' TO WS-SECTION                                
112600                                                                          
112760     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
112800     MOVE 99999              TO W-IDTRPTNR                                
112900     MOVE WS-MSGI-RAD-KEY (INDX)                                          
113000                             TO W-DADATTID                                
113110     PERFORM IMS-GHU-M521                                                 
113111     IF MID-KDTRPSTA (INDX) = 'L'                                         
113120       MOVE 'P'              TO AVG-KDTRPSTA                              
113121     ELSE                                                                 
113122       MOVE SPACE            TO AVG-KDTRPSTA                              
113123     END-IF                                                               
113130     MOVE DAGENS-DATUM       TO AVG-TIUPPDAT                              
113140     MOVE MSGI-IDUSER        TO AVG-IDUSER-TRP                            
113200     PERFORM IMS-REPL-M521                                                
113300     .                                                                    
113400     EJECT                                                                
113500 HD-BORTTAG SECTION.                                                      
113501     MOVE 'HD-BORTTAG      ' TO WS-SECTION                                
113600                                                                          
113760     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
113800     MOVE 99999              TO W-IDTRPTNR                                
113900     MOVE WS-MSGI-RAD-KEY (INDX)                                          
114000                             TO W-DADATTID                                
114100     PERFORM IMS-GHU-M521-DLET                                            
114110     IF SEGMENT-FINNS                                                     
114200       PERFORM IMS-DLET-M521                                              
114201     END-IF                                                               
114210     IF INDX = 1                                                          
114220       MOVE SPACE            TO WS-MSGI-SSA-KEY-ENTER                     
114230     END-IF                                                               
114300     .                                                                    
114400     EJECT                                                                
114600                                                                          
114700 HE-SKRIV-L1-RAD1-7 SECTION.                                              
114800     MOVE 'HE-SKRIV-L1-RAD1-7'                                            
114900                             TO WS-SECTION                                
115800                                                                          
115900     IF IDSID-RAKN = +0                                                   
116000        MOVE +1 TO IDSID-RAKN                                             
116100     END-IF                                                               
116200                                                                          
116300     MOVE DAGENS-DATUM       TO L1-RAD1-DATUM                             
116500     MOVE IDSID-RAKN         TO L1-RAD1-IDSID                             
117000     MOVE L1-RAD1            TO WS-LISTRAD                                
117200     MOVE PRT-NYSIDA-RAD4    TO PRT-RADSKIP                               
117300     PERFORM S02-SKRIV-RAD                                                
117400                                                                          
117500     MOVE L1-RAD2            TO WS-LISTRAD                                
117600     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
117700     PERFORM S02-SKRIV-RAD                                                
117800                                                                          
117900     MOVE L1-RAD3            TO WS-LISTRAD                                
118000     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
118100     PERFORM S02-SKRIV-RAD                                                
118200                                                                          
119000     MOVE L1-RAD4            TO WS-LISTRAD                                
119200     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
119300     PERFORM S02-SKRIV-RAD                                                
119400                                                                          
119500     MOVE L1-RAD5            TO WS-LISTRAD                                
119600     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
119700     PERFORM S02-SKRIV-RAD                                                
119800                                                                          
119900     MOVE L1-RAD6            TO WS-LISTRAD                                
120000     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
120100     PERFORM S02-SKRIV-RAD                                                
120200                                                                          
120600     MOVE L1-RAD7            TO WS-LISTRAD                                
120800     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
120900     PERFORM S02-SKRIV-RAD                                                
121000     .                                                                    
121100     EJECT                                                                
121200                                                                          
121320 HF-SKRIV-RAD8-A SECTION.                                                 
121321     MOVE 'HF-SKRIV-RAD8-A   '                                            
121340                             TO WS-SECTION                                
121400                                                                          
121426     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
121430     MOVE 99999              TO W-IDTRPTNR                                
121440     MOVE WS-MSGI-RAD-KEY (INDX)                                          
121450                             TO W-DADATTID                                
121451     PERFORM IMS-GHU-M521                                                 
121452     MOVE 'P'                TO AVG-KDTRPSTA                              
121453     MOVE DAGENS-DATUM       TO AVG-TIUPPDAT                              
121454     MOVE MSGI-IDUSER        TO AVG-IDUSER-TRP                            
121455     PERFORM IMS-REPL-M521                                                
121460                                                                          
121500     IF RAD-IX = +0                                                       
121600       ADD +1 TO RAD-IX                                                   
121700       MOVE JA TO FOERSTA-RADEN                                           
121800     ELSE                                                                 
121900       MOVE NEJ TO FOERSTA-RADEN                                          
122000     END-IF                                                               
122100                                                                          
122200     MOVE AVG-IDARTNR        TO  L1-RAD8-IDARTNR                          
122300                                 W-IDARTNR                                
122400     MOVE AVG-KVANTAL        TO  L1-RAD8-KVANTAL                          
122705     PERFORM IMS-GU-K611                                                  
122706                                                                          
122910     IF WS-ADLASTPL = 'CDC'                                               
122920       MOVE CLAG-ADLAGOMR    TO L1-RAD8-ADLAGOMR                          
122930       MOVE CLAG-ADGANG      TO L1-RAD8-ADGANG                            
122940       MOVE CLAG-ADPLATS     TO L1-RAD8-ADPLATS                           
122960     END-IF                                                               
122980     IF WS-ADLASTPL = 'SVS'                                               
122984       MOVE CLAG-ADLAGOMR-SVS                                             
122985                             TO L1-RAD8-ADLAGOMR                          
122986       MOVE CLAG-ADGANG-SVS  TO L1-RAD8-ADGANG                            
122987       MOVE CLAG-ADPLATS-SVS TO L1-RAD8-ADPLATS                           
122999     END-IF                                                               
123000     MOVE CLAG-BEFT          TO L1-RAD8-BEFT                              
123100     MOVE AVG-TETRPMED       TO L1-RAD8-TETRPMED                          
132110     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
132120     ADD +2                TO RAD-IX                                      
132200                                                                          
132300     MOVE L1-RAD8          TO WS-LISTRAD                                  
132400     PERFORM S02-SKRIV-RAD                                                
136600     .                                                                    
136700     EJECT                                                                
136800 HG-SKRIV-L2-RAD1-7 SECTION.                                              
136810     MOVE 'HG-SKRIV-L2-RAD1-7'                                            
136820                             TO WS-SECTION                                
136900                                                                          
137000     IF IDSID-RAKN = +0                                                   
137100        MOVE +1 TO IDSID-RAKN                                             
137200     END-IF                                                               
137300                                                                          
137400     MOVE DAGENS-DATUM       TO L2-RAD1-DATUM                             
137500     MOVE IDSID-RAKN         TO L2-RAD1-IDSID                             
137600     MOVE L2-RAD1            TO WS-LISTRAD                                
137700     MOVE PRT-NYSIDA-RAD4    TO PRT-RADSKIP                               
137800     PERFORM S02-SKRIV-RAD                                                
137900                                                                          
138000     MOVE L2-RAD2            TO WS-LISTRAD                                
138100     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
138200     PERFORM S02-SKRIV-RAD                                                
138300                                                                          
138400     MOVE L2-RAD3            TO WS-LISTRAD                                
138500     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
138600     PERFORM S02-SKRIV-RAD                                                
138700                                                                          
138800     MOVE L2-RAD4            TO WS-LISTRAD                                
138900     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
139000     PERFORM S02-SKRIV-RAD                                                
139100                                                                          
139200     MOVE L2-RAD5            TO WS-LISTRAD                                
139300     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
139400     PERFORM S02-SKRIV-RAD                                                
139500                                                                          
139600     MOVE L2-RAD6            TO WS-LISTRAD                                
139700     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
139800     PERFORM S02-SKRIV-RAD                                                
139900                                                                          
140000     MOVE L2-RAD7            TO WS-LISTRAD                                
140100     MOVE PRT-AFTER-1        TO PRT-RADSKIP                               
140200     PERFORM S02-SKRIV-RAD                                                
140300     .                                                                    
140400     EJECT                                                                
140500                                                                          
140610 HH-SKRIV-RAD8-B SECTION.                                                 
140611     MOVE 'HH-SKRIV-RAD8-B   '                                            
140630                             TO WS-SECTION                                
140700                                                                          
141400     IF RAD-IX = +0                                                       
141500       ADD +1 TO RAD-IX                                                   
141600       MOVE JA TO FOERSTA-RADEN                                           
141700     ELSE                                                                 
141800       MOVE NEJ TO FOERSTA-RADEN                                          
141900     END-IF                                                               
142000                                                                          
142300     MOVE TAB-IDARTNR (INDX) TO  L2-RAD8-IDARTNR                          
142400     MOVE TAB-KVANTAL (INDX) TO  L2-RAD8-KVANTAL                          
142600     MOVE TAB-ADLAGOMR (INDX)                                             
142610                             TO L2-RAD8-ADLAGOMR                          
142700     MOVE TAB-ADGANG (INDX)  TO L2-RAD8-ADGANG                            
142800     MOVE TAB-ADPLATS (INDX) TO L2-RAD8-ADPLATS                           
143700     MOVE TAB-TETRPMED (INDX)                                             
143710                             TO L2-RAD8-TETRPMED                          
144410     MOVE PRT-AFTER-2      TO PRT-RADSKIP                                 
144420     ADD +2                TO RAD-IX                                      
144500                                                                          
144600     MOVE L2-RAD8          TO WS-LISTRAD                                  
144700     PERFORM S02-SKRIV-RAD                                                
144800     .                                                                    
147300     EJECT                                                                
147400 HI-UPD-ANTAL SECTION.                                                    
147500     MOVE 'HI-UPD-ANTAL    ' TO WS-SECTION                                
147600                                                                          
147700     MOVE WS-ADTRDEST        TO W-ADTRDEST                                
147800     MOVE 99999              TO W-IDTRPTNR                                
147900     MOVE WS-MSGI-RAD-KEY (INDX)                                          
148000                             TO W-DADATTID                                
148100     PERFORM IMS-GHU-M521                                                 
148110     MOVE MID-KVANTAL        TO AVG-KVANTAL                               
148200     PERFORM IMS-REPL-M521                                                
148300     IF INDX = 1                                                          
148400       MOVE SPACE            TO WS-MSGI-SSA-KEY-ENTER                     
148500     END-IF                                                               
148600     .                                                                    
148700     EJECT                                                                
156370 MFS-RENSA-FAELT-UT SECTION.                                              
156400                                                                          
156500*    --- ALLA UTDATA-FÄLT                                                 
156700     MOVE MFS-RENSA-FAELT    TO MOD-ADLASTPL-UT                           
156800                                MOD-KDTRPSTA-UT                           
156801                                MOD-IDARTNR-UT                            
156810     MOVE +1 TO INDX                                                      
156820     PERFORM UNTIL INDX > RAD-MAX                                         
156830       MOVE MFS-RENSA-FAELT                                               
156900                             TO MOD-IDARTNR (INDX)                        
157000                                MOD-KVANTAL (INDX)                        
157100                                MOD-ADLAGOMR(INDX)                        
157200                                MOD-ADGANG (INDX)                         
157300                                MOD-ADPLATS (INDX)                        
157400                                MOD-BEFT (INDX)                           
157410                                MOD-TIDATUM (INDX)                        
157411                                MOD-KDTRPSTA(INDX)                        
157420                                MOD-IDUSER (INDX)                         
157430                                MOD-TETRPMED(INDX)                        
157440       ADD +1 TO INDX                                                     
157450     END-PERFORM                                                          
157500     .                                                                    
159610     EJECT                                                                
159620 MFS-RENSA-FAELT-IN SECTION.                                              
159630                                                                          
159640*    --- ALLA INDATA-FÄLT                                                 
159650     MOVE MFS-RENSA-FAELT    TO MOD-ADLASTPL-IN                           
159660                                MOD-KDTRPSTA-IN                           
159661                                MOD-IDARTNR-IN                            
159662                                MOD-KVANTAL-NY                            
159670                                MOD-LAST-KLAR                             
159680                                MOD-PRINTER                               
159690     MOVE +1 TO INDX                                                      
159691     PERFORM UNTIL INDX > RAD-MAX                                         
159692       MOVE MFS-RENSA-FAELT  TO MOD-CMD (INDX)                            
159693       ADD +1                TO INDX                                      
159694     END-PERFORM                                                          
159695     .                                                                    
159696     EJECT                                                                
159700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
159800                                                                          
159900*    --- ALLA UTDATA-FÄLT                                                 
159910     MOVE MFS-ROER-EJ-FAELT  TO MOD-ADLASTPL-UT                           
159920                                MOD-KDTRPSTA-UT                           
159922                                MOD-IDARTNR-UT                            
159930     MOVE +1 TO INDX                                                      
159940     PERFORM UNTIL INDX > RAD-MAX                                         
159941       MOVE MFS-ROER-EJ-FAELT                                             
159960                             TO MOD-IDARTNR (INDX)                        
159970                                MOD-KVANTAL (INDX)                        
159980                                MOD-ADLAGOMR(INDX)                        
159990                                MOD-ADGANG (INDX)                         
159991                                MOD-ADPLATS (INDX)                        
159992                                MOD-BEFT (INDX)                           
159993                                MOD-TIDATUM (INDX)                        
159994                                MOD-KDTRPSTA(INDX)                        
159995                                MOD-IDUSER (INDX)                         
159996                                MOD-TETRPMED(INDX)                        
159997       ADD +1 TO INDX                                                     
159998     END-PERFORM                                                          
161500     .                                                                    
161600     EJECT                                                                
163100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
163200                                                                          
163300*    --- ALLA INDATA-FÄLT                                                 
163310     MOVE MFS-ROER-EJ-FAELT  TO MOD-ADLASTPL-IN                           
163320                                MOD-KDTRPSTA-IN                           
163321                                MOD-IDARTNR-IN                            
163322                                MOD-KVANTAL-NY                            
163330                                MOD-LAST-KLAR                             
163340                                MOD-PRINTER                               
163350     MOVE +1 TO INDX                                                      
163360     PERFORM UNTIL INDX > RAD-MAX                                         
163361       MOVE MFS-ROER-EJ-FAELT                                             
163370                             TO MOD-CMD (INDX)                            
163380       ADD +1                TO INDX                                      
163390     END-PERFORM                                                          
163500     .                                                                    
163600     EJECT                                                                
163620 S01-PRT-OPEN SECTION.                                                    
163630                                                                          
163640     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
163650                          PRT-OPEN                                        
163660                          WS-LIST-PRINTER                                 
163670                          ALT-PCB                                         
163680                          WS-DUMMY                                        
163690                          WS-DUMMY                                        
163691     .                                                                    
163692     EJECT                                                                
163693 S02-SKRIV-RAD SECTION.                                                   
163694                                                                          
163695     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
163696                          PRT-WRITE                                       
163697                          WS-LIST-PRINTER                                 
163698                          ALT-PCB                                         
163699                          PRT-RADSKIP                                     
163700                          WS-RAD                                          
163701     .                                                                    
163702     EJECT                                                                
163703 S03-PRT-CLOSE SECTION.                                                   
163704                                                                          
163705     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
163706                          PRT-CLOSE                                       
163707                          WS-LIST-PRINTER                                 
163708                          ALT-PCB                                         
163709                          WS-DUMMY                                        
163710                          WS-DUMMY                                        
163711     .                                                                    
163712     EJECT                                                                
163720* --- IMS SEKTIONER ---                                                   
163800     SKIP3                                                                
163900 IMS-GET-MSG SECTION.                                                     
164100     MOVE '  QC' TO GODK-STATUSKODER                                      
164200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
164300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
164400     PERFORM IMS-STATUSKONTROLL                                           
164500     .                                                                    
164600     SKIP2                                                                
164700 IMS-INSERT-MSG SECTION.                                                  
164810     IF SWEDISH-TEXT                                                      
164820        IF MSGI-IDLAND-SPR NOT = 'GB'                                     
164830           MOVE '0' TO MFS-KDHUVOMR                                       
164840        END-IF                                                            
164850     END-IF                                                               
165300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
165400     MOVE SPACE TO GODK-STATUSKODER                                       
165500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
165700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
165800     PERFORM IMS-STATUSKONTROLL                                           
165900     .                                                                    
166080     EJECT                                                                
183902 IMS-GU-K611 SECTION.                                                     
183903                                                                          
183904     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
183905          DELIMITED BY SIZE INTO SSA1                                     
183906     MOVE 'WDK611  '         TO SSA2                                      
183909     MOVE SPACE  TO GODK-STATUSKODER                                      
183910     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
183911     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
183912     PERFORM IMS-STATUSKONTROLL                                           
183920     .                                                                    
183930     EJECT                                                                
183962 IMS-GU-M511   SECTION.                                                   
183963                                                                          
183964     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
183965          DELIMITED BY SIZE INTO SSA1                                     
183966     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
183967          DELIMITED BY SIZE INTO SSA2                                     
183968     MOVE '  GE' TO GODK-STATUSKODER                                      
183970     CALL CBLTDLI USING GU  WDM5-PCB DLI-IO-WDM511 SSA1 SSA2              
183971     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
183972     PERFORM IMS-STATUSKONTROLL                                           
183973     .                                                                    
183974     EJECT                                                                
183975 IMS-ISRT-M511-PCB2 SECTION.                                              
183976                                                                          
183977     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
183978          DELIMITED BY SIZE INTO SSA1                                     
183979     STRING 'WDM511    '                                                  
183980          DELIMITED BY SIZE INTO SSA2                                     
183981     MOVE '  ' TO GODK-STATUSKODER                                        
183982     CALL CBLTDLI USING ISRT WDM5-2-PCB                                   
183984                             DLI-IO-WDM511 SSA1 SSA2                      
183985     MOVE WDM5-2-STATUS-CODE TO STATUS-WS                                 
183986     PERFORM IMS-STATUSKONTROLL                                           
183987     .                                                                    
183989     EJECT                                                                
183990 IMS-ISRT-M521-PCB2 SECTION.                                              
183991                                                                          
183992     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
183993          DELIMITED BY SIZE INTO SSA1                                     
183994     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X2 ')'                       
183995          DELIMITED BY SIZE INTO SSA2                                     
183996     STRING 'WDM521    '                                                  
183997          DELIMITED BY SIZE INTO SSA3                                     
183998     MOVE '  ' TO GODK-STATUSKODER                                        
184000     CALL CBLTDLI USING ISRT WDM5-2-PCB                                   
184001                             DLI-IO-WDM521 SSA1 SSA2 SSA3                 
184002     MOVE WDM5-2-STATUS-CODE TO STATUS-WS                                 
184004     PERFORM IMS-STATUSKONTROLL                                           
184005     .                                                                    
184006     EJECT                                                                
184007 IMS-GNP-M521 SECTION.                                                    
184008                                                                          
184009     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
184010          DELIMITED BY SIZE INTO SSA1                                     
184011     STRING 'WDM521     '                                                 
184012          DELIMITED BY SIZE INTO SSA2                                     
184013     MOVE '  GE' TO GODK-STATUSKODER                                      
184014     CALL CBLTDLI USING GNP WDM5-PCB DLI-IO-WDM521 SSA1 SSA2              
184015     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
184016     PERFORM IMS-STATUSKONTROLL                                           
184017     .                                                                    
184018     EJECT                                                                
184019 IMS-GHNP-M521 SECTION.                                                   
184020                                                                          
184021     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
184022          DELIMITED BY SIZE INTO SSA1                                     
184023     STRING 'WDM521     '                                                 
184024          DELIMITED BY SIZE INTO SSA2                                     
184025     MOVE '  GE' TO GODK-STATUSKODER                                      
184026     CALL CBLTDLI USING GHNP WDM5-PCB DLI-IO-WDM521 SSA1 SSA2             
184027     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
184028     PERFORM IMS-STATUSKONTROLL                                           
184029     .                                                                    
184030     EJECT                                                                
184100 IMS-GNP-M521-KVAL   SECTION.                                             
184101                                                                          
184102     STRING 'WDM521  (DADATTID =' W-DADATTID-X ')'                        
184103          DELIMITED BY SIZE INTO SSA1                                     
184104     MOVE '  GE' TO GODK-STATUSKODER                                      
184106     CALL CBLTDLI USING GNP WDM5-PCB DLI-IO-WDM521 SSA1                   
184107     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
184108     PERFORM IMS-STATUSKONTROLL                                           
184109     .                                                                    
184110     EJECT                                                                
184111 IMS-GHU-M521   SECTION.                                                  
184112                                                                          
184113     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
184114          DELIMITED BY SIZE INTO SSA1                                     
184115     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
184116          DELIMITED BY SIZE INTO SSA2                                     
184117     STRING 'WDM521  (DADATTID =' W-DADATTID-X ')'                        
184118          DELIMITED BY SIZE INTO SSA3                                     
184119     MOVE SPACE  TO GODK-STATUSKODER                                      
184120     CALL CBLTDLI USING GHU WDM5-PCB DLI-IO-WDM521 SSA1 SSA2 SSA3         
184121     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
184122     PERFORM IMS-STATUSKONTROLL                                           
184123     .                                                                    
184124     EJECT                                                                
184125 IMS-GHU-M521-DLET  SECTION.                                              
184126                                                                          
184127     STRING 'WDM501  (ADTRDEST =' W-ADTRDEST ')'                          
184128          DELIMITED BY SIZE INTO SSA1                                     
184129     STRING 'WDM511  (IDTRPTNR =' W-IDTRPTNR-X ')'                        
184130          DELIMITED BY SIZE INTO SSA2                                     
184131     STRING 'WDM521  (DADATTID =' W-DADATTID-X ')'                        
184132          DELIMITED BY SIZE INTO SSA3                                     
184133     MOVE '  GE' TO GODK-STATUSKODER                                      
184134     CALL CBLTDLI USING GHU WDM5-PCB DLI-IO-WDM521 SSA1 SSA2 SSA3         
184135     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
184136     PERFORM IMS-STATUSKONTROLL                                           
184137     .                                                                    
184138     EJECT                                                                
184139 IMS-REPL-M521 SECTION.                                                   
184140                                                                          
184141     MOVE '  ' TO GODK-STATUSKODER                                        
184142     CALL CBLTDLI USING REPL WDM5-PCB DLI-IO-WDM521                       
184143     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
184144     PERFORM IMS-STATUSKONTROLL                                           
184145     .                                                                    
184146     EJECT                                                                
184147 IMS-DLET-M521 SECTION.                                                   
184148                                                                          
184149     MOVE '  ' TO GODK-STATUSKODER                                        
184150     CALL CBLTDLI USING DLET WDM5-PCB DLI-IO-WDM521                       
184151     MOVE WDM5-STATUS-CODE TO STATUS-WS                                   
184152     PERFORM IMS-STATUSKONTROLL                                           
184153     .                                                                    
184154     EJECT                                                                
184155 IMS-ISRT-M901 SECTION.                                                   
184156                                                                          
184157     STRING 'WDM901    '                                                  
184158          DELIMITED BY SIZE INTO SSA1                                     
184159     MOVE '  II' TO GODK-STATUSKODER                                      
184160     CALL CBLTDLI USING ISRT WDM9-PCB                                     
184161                             DLI-IO-WDM901 SSA1                           
184162     MOVE WDM9-STATUS-CODE TO STATUS-WS                                   
184163     PERFORM IMS-STATUSKONTROLL                                           
184164     .                                                                    
184165     EJECT                                                                
184166 IMS-ISRT-M911 SECTION.                                                   
184167                                                                          
184168     STRING 'WDM901  (IDARTNR  =' W-IDARTNR-X ')'                         
184169          DELIMITED BY SIZE INTO SSA1                                     
184170     STRING 'WDM911    '                                                  
184171          DELIMITED BY SIZE INTO SSA2                                     
184172     MOVE '  II' TO GODK-STATUSKODER                                      
184173     CALL CBLTDLI USING ISRT WDM9-PCB                                     
184174                             DLI-IO-WDM911 SSA1 SSA2                      
184175     MOVE WDM9-STATUS-CODE TO STATUS-WS                                   
184176     PERFORM IMS-STATUSKONTROLL                                           
184177     .                                                                    
184178     EJECT                                                                
184179 IMS-GHU-W6G110 SECTION.                                                  
184180     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
184181          DELIMITED BY SIZE INTO SSA1                                     
184182     STRING 'W6G110  (KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
184183          DELIMITED BY SIZE INTO SSA2                                     
184184     MOVE '    ' TO GODK-STATUSKODER                                      
184185     CALL CBLTDLI USING GHU W6G1-PCB DLI-IO-W6G110 SSA1 SSA2              
184186     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
184187     PERFORM IMS-STATUSKONTROLL                                           
184188     .                                                                    
184189     EJECT                                                                
184190 IMS-REPL-W6G110 SECTION.                                                 
184191     MOVE '    ' TO GODK-STATUSKODER                                      
184192     CALL CBLTDLI USING REPL W6G1-PCB DLI-IO-W6G110                       
184193     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
184194     PERFORM IMS-STATUSKONTROLL                                           
184195     .                                                                    
184196     EJECT                                                                
184197 IMS-STATUSKONTROLL SECTION.                                              
184198     SET STATUS-IX TO 1                                                   
184200     SEARCH GODK-STATUS                                                   
184300       AT END                                                             
184400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
184500         DELIMITED BY SIZE INTO FELTEXT                                   
184600         CALL FELLOG                                                      
184700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
184800         CONTINUE                                                         
184900     END-SEARCH                                                           
185000     .                                                                    
