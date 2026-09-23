000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6019600.                                                
000300 AUTHOR.         LARS THELL.                                              
000400 DATE-WRITTEN.   92/04/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BAKGRUNDS MPP SOM SKRIVER UT ARBETSRAPPORT ELLER                 
000900*        CLEARINGRAPPORT BEROENDE PÅ KDINL.                               
001000*        PROGRAMMET STARTAS FRÅN BILD 6109 ELLER                          
001100*        BILD 6115. STARTAS ÄVEN AV BMP PGM W61102 (RETURER).             
001200*                                                                         
001300*        THIS PROGRAM PRINTS RECEIVING REPORT OR CLEARING REPORT.         
001400*        DEPENDING ON DATA ITEM KDINL.                                    
001500*                                                                         
001600*        PROGRAMMET          UPPDATERAR WLLISB (WDG8)                     
001700*        PROGRAMMET          LÄSER      W6INLA (W6D1)                     
001800*        PROGRAMMET          LÄSER      W6PLAA (W6G1)                     
001900*        PROGRAMMET          LÄSER      WLARTC (WDK6)                     
002000*        PROGRAMMET          LÄSER      WDD5                              
002100*        PROGRAMMET          LÄSER      WDK7                              
002200*        PROGRAMMET          LÄSER      WLARTD (WDD8)                     
002300*        PROGRAMMET          LÄSER      WDF5                              
002400*        PROGRAMMET          LÄSER      WLUSEA (WDP8)                     
002500*    SUB PROGRAMMET 611ADR   LÄSER      W6INLA (W6D1)                     
002600*                            LÄSER      W6PLAA (W6G1)                     
002700*                            LÄSER      W6HANA (W6G1)                     
002800*                                                                         
002900*    INDATA.                                                              
003000*        TRANSAKTION: W6T109                                              
003100*        MID:         W6I19601                                            
003200*        MID:         W6I19602                                            
003300*                                                                         
003400*    UTDATA.                                                              
003500*        MOD:         W6O10901                                            
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900                                                                          
004000 DATA DIVISION.                                                           
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(08)   VALUE 'W6019600'.            
004600                                                                          
004700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004900                                                                          
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +144  COMP SYNC.        
005400 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
005500 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
005600 77  MAX-TAB-IX-VCOM             PIC S9(4)  VALUE +42   COMP SYNC.        
005700 77  MAX-TAB-IX-LIGGANDE         PIC S9(4)  VALUE +114  COMP SYNC.        
005800 77  MAX-TAB-IX-STAENDE          PIC S9(4)  VALUE +112  COMP SYNC.        
005900 77  MAX-ZEBRA-IX                PIC S9(4)  VALUE +102  COMP SYNC.        
006000 77  MAX-LASER-IX                PIC S9(4)  VALUE +48   COMP SYNC.        
006200 77  MAX-LASER-IX-OLD            PIC S9(4)  VALUE +50   COMP SYNC.        
006300 77  MAX-LASER-IX-RR             PIC S9(4)  VALUE +47   COMP SYNC.        
006400 77  RR-IX-ZEBRA                 PIC S9(4)  VALUE +0    COMP SYNC.        
006500 77  AR-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
006600 77  AR-IX-VCOM                  PIC S9(4)  VALUE +0    COMP SYNC.        
006700 77  AR-LASER-IX                 PIC S9(4)  VALUE +0    COMP SYNC.        
006800 77  OLD-LASER-IX                PIC S9(4)  VALUE +0    COMP SYNC.        
006900 77  RR-LASER-IX                 PIC S9(4)  VALUE +0    COMP SYNC.        
007000 77  MID-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
007100                                                                          
007200 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
007300 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
007400                                                                          
007500 01  WS-IDDC-LOCAL.                                                       
007600     03  FILLER                  PIC X(5)    VALUE 'WIDDC'.               
007700     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
007800     03  FILLER                  PIC X(1)    VALUE SPACE.                 
007900 77  W-ADLAGOMR                  PIC 9(2)    VALUE ZERO.                  
008000 77  W-ADBUFFOMR                 PIC 9(2)    VALUE ZERO.                  
008100 77  ANT-ADBUFF                  PIC S9      VALUE ZERO COMP-3.           
008200 77  W-SPAR-IDLBBET              PIC X(12)   VALUE SPACE.                 
008300 77  WS-KVROS                    PIC S9(6)   VALUE ZERO.                  
008400                                                                          
008500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008600     88  ALLT-OK                             VALUE 'J'.                   
008700                                                                          
008800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008900     88  INDATA-OK                           VALUE 'J'.                   
009000     88  INDATA-FEL                          VALUE 'N'.                   
009100                                                                          
009200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009300     88  NYCKLAR-OK                          VALUE 'J'.                   
009400     88  NYCKLAR-FEL                         VALUE 'N'.                   
009500                                                                          
009600 77  INL-SEGMENT-SW              PIC X       VALUE 'J'.                   
009700     88  INL-SEGMENT-SAKNAS                  VALUE 'N'.                   
009800                                                                          
009900 77  LASER-SW                    PIC X       VALUE 'N'.                   
010000     88  LASER                               VALUE 'J'.                   
010100                                                                          
010200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010300     88  EGEN-MID                            VALUE '6196'.                
010400     88  SVARA-MOD                           VALUE '6109'.                
010500     EJECT                                                                
010600*      --- VALID IDDC CODES                                               
010700*                                                                         
010800*01    -COPY WWDC99 -PRE SW-                                              
010900       EJECT                                                              
011000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011100 01  GENERELLA-SUBPROGRAM.                                                
011200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011600     03  W611ADR                 PIC X(8)    VALUE 'W611ADR '.            
011700     03  W006PRC1                PIC X(8)    VALUE 'W006PRC1'.            
011800     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
011900     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
012000     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
012100     EJECT                                                                
012200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012300*01 -COPY WMEDAREA                                                        
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012600 01  FILLER                      PIC X(8)    VALUE 'W005INIT'.            
012700*01 -COPY WMSGINIT                                                        
012800     EJECT                                                                
012900*    --- PARAMETRAR FÖR W006PRT                                           
013000 01  FILLER                      PIC X(8)    VALUE 'W006PRT '.            
013100*01 -COPY W006PRT                                                         
013200     EJECT                                                                
013300*    --- COPYTEXT  FÖR W611ADR                                            
013400 01  FILLER                      PIC X(8)    VALUE 'W611ADR '.            
013500*01 -COPY W611ADR                                                         
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL W006PRC1 OCH W006PRS1                            
013800 01  FILLER                      PIC X(8)    VALUE 'W006PRAR'.            
013900*01 -COPY W006PRAR                                                        
014000     EJECT                                                                
014100 01  FILLER                      PIC X(8)    VALUE 'W006PRVC'.            
014200*01 -COPY W006PRVC                                                        
014300     EJECT                                                                
014400*    --- PARAMETRAR TILL SUBPROGRAM W400ARTU                              
014500 01  FILLER                      PIC X(8)    VALUE 'W400ARTU'.            
014600*01 -COPY W400ARTU                                                        
014700     EJECT                                                                
014800* -COPY WWOMVAND                                                          
014900*                                                                         
015000 01  WS-RAPP-AREA.                                                        
015100     03  WS-RAPP-LISTID          PIC X(8).                                
015200     03  WS-RAPP-LISTRAD.                                                 
015300         05  FILLER              PIC X(2)    VALUE SPACE.                 
015400         05  WS-RAPP-RAD         PIC X(114).                              
015500     03  WS-DUMMY                PIC X(1).                                
015600     03  WS-RAPP-PRINTER         PIC X(8).                                
015700 01  AR-LIST-RAD                 PIC X(133) VALUE SPACE.                  
015800*                                                                         
015900 01  MESSAGE-CODES.                                                       
016000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016100*                                                                         
016200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016300*                                                                         
016400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016500     SKIP3                                                                
016600 01  MID-W6I19601.                                                        
016700*                      --- HÄR ÄR MIDDEN FRÅN 6109-TRANSEN                
016800*  03 -COPY W6I19601 -PRE MOD6109-                                        
016900     EJECT                                                                
017000 01  MID-W6I19602.                                                        
017100*                      --- HÄR ÄR MIDDEN FRÅN ÖVRIGA TRANSAR              
017200*  03 -COPY W6I19602 -PRE MOD-                                            
017300     EJECT                                                                
017400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017500     SKIP3                                                                
017600*01  -COPY WMSGAREA                                                       
017700     EJECT                                                                
017800     03  MOD REDEFINES MSG-AREA.                                          
017900*      05  -COPY W6O19601                                                 
018000     EJECT                                                                
018100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018200     SKIP3                                                                
018300*01  -COPY WMFSAREA                                                       
018400     EJECT                                                                
018500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018600*                                                                         
018700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018800     SKIP3                                                                
018900 01  NYCKLAR-TILL-DLI.                                                    
019000                                                                          
019100     03  W-IDLOPNRM-X.                                                    
019200         05  W-IDLOPNRM               PIC S9(9) COMP-3 VALUE ZERO.        
019300     03  W-W6D101KY-X.                                                    
019400         05  W-D101KY-IDDC       PIC  X(2)    VALUE SPACE.                
019500         05  W-D101KY-IDLEVNR    PIC  X(5)    VALUE SPACE.                
019600         05  W-D101KY-IDFS       PIC  X(8)    VALUE SPACE.                
019700         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
019800                                                                          
019900     03  W-IDRADNR-INL-X.                                                 
020000         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
020100                                                                          
020200     03  W-W6GXKEY-6005-X.                                                
020300         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
020400         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
020500         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
020600                                                                          
020700     03  W-W6GXKEY-6006-X.                                                
020800         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
020900         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
021000                                                                          
021100     03  W-IDARTNR-X.                                                     
021200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021300                                                                          
021400     03  W-KDSEGKEY-X.                                                    
021500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
021600                                                                          
021700     03  W-IDLEVNR-X.                                                     
021800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
021900                                                                          
022000     03  W-IDDC-X.                                                        
022100         05  W-IDDC              PIC X(2)    VALUE '11'.                  
022200                                                                          
022210     03  W-IDLAND.                                                        
022220         05  W-IDLAND-US-CN      PIC  X(2)   VALUE '  '.                  
022230                                                                          
022300     SKIP2                                                                
022400*    --- STATUS-KOD FRÅN IMS                                              
022500 01  STATUS-WS                   PIC XX.                                  
022600     88  SEGMENT-FINNS                       VALUE '  '.                  
022700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022900     SKIP2                                                                
023000 01  GODK-STATUSKODER.                                                    
023100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023200     SKIP3                                                                
023300 01  SSA1                        PIC X(128).                              
023400 01  SSA2                        PIC X(64).                               
023500 01  SSA3                        PIC X(64).                               
023600     EJECT                                                                
023700*    --- IMS FUNKTIONSKODER                                               
023800*01  -COPY W0003                                                          
023900     EJECT                                                                
024000*    ---  DLI INPUT-OUTPUT AREA                                           
024100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
024200     SKIP3                                                                
024300 01  DLI-IO-AREA.                                                         
024400     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
024500     SKIP3                                                                
024600     03  W6INLA01 REDEFINES IO-AREA.                                      
024700*        05  -COPY W6D101                                                 
024800     EJECT                                                                
024900     03  W6INLC01 REDEFINES IO-AREA.                                      
025000*        05  -COPY W6D1B1                                                 
025100     EJECT                                                                
025200     03  W6INLA11 REDEFINES IO-AREA.                                      
025300*        05  -COPY W6D111                                                 
025400     EJECT                                                                
025500     03  W6PLAA11 REDEFINES IO-AREA.                                      
025600*        05  -COPY W6GX6006 -PRE PLAA-                                    
025700     EJECT                                                                
025800     03  WLARTC11 REDEFINES IO-AREA.                                      
025900*        05  -COPY WDK611  -PRE ARTC11-                                   
026000     EJECT                                                                
026100     03  WLARTD11 REDEFINES IO-AREA.                                      
026200*        05  -COPY WDD811                                                 
026300     EJECT                                                                
026400     03  WDF502   REDEFINES IO-AREA.                                      
026500*        05  -COPY WDF502                                                 
026600     EJECT                                                                
026700 01  DLI-IO-AREA-WDK7.                                                    
026800     03  WDK711.                                                          
026900*        05  -COPY WDK711                                                 
027000     EJECT                                                                
027010 01  DLI-IO-WDK712.                                                       
027020     03  WDK712.                                                          
027030*        05  -COPY WDK712                                                 
027040     EJECT                                                                
027100 01  DLI-IO-AREA-WDD5.                                                    
027200     03  WDD501.                                                          
027300*        05  -COPY WDD501  -PRE WDD5-                                     
027400     EJECT                                                                
027500                                                                          
027600 01      FILLER                  PIC X(16)   VALUE 'AR-TAB-VCOM'.         
027700 01      AR-TAB-VCOM.                                                     
027800   03    FILLER                  PIC X(80)   VALUE                        
027900         '!M "ARB"                                             '.         
028000   03    FILLER.                                                          
028100      05 AR-TAB-IDLOPNRM-VCOM    PIC 9(8)    VALUE ZERO.                  
028200      05 FILLER                  PIC X(72)   VALUE SPACE.                 
028300   03    FILLER.                                                          
028400      05 AR-TAB-IDLEVNR-VCOM     PIC X(5)    VALUE SPACE.                 
028500      05 FILLER                  PIC X(75)   VALUE SPACE.                 
028600   03    FILLER.                                                          
028700      05 AR-TAB-VKART-VCOM       PIC Z(6)9   VALUE ZERO.                  
028800      05 FILLER                  PIC X(73)   VALUE SPACE.                 
028900   03    FILLER.                                                          
029000      05 AR-TAB-ADLAGOMR-VCOM    PIC 9(2)    VALUE ZERO.                  
029100      05 FILLER                  PIC X       VALUE SPACE.                 
029200      05 AR-TAB-ADGANG-VCOM      PIC Z(2)    VALUE ZERO.                  
029300      05 FILLER                  PIC X       VALUE SPACE.                 
029400      05 AR-TAB-ADPLATS-VCOM     PIC Z(4)9   VALUE ZERO.                  
029500      05 FILLER                  PIC X(69)   VALUE SPACE.                 
029600   03    FILLER.                                                          
029700      05 AR-TAB-ADBUFFOMR-1-VCOM PIC 9(2).                                
029800      05 FILLER                  PIC X       VALUE SPACE.                 
029900      05 AR-TAB-ADBUFFGANG-1-VCOM PIC Z(2).                               
030000      05 FILLER                  PIC X       VALUE SPACE.                 
030100      05 AR-TAB-ADBUFFPL-1-VCOM  PIC Z(4)9.                               
030200      05 FILLER                  PIC X(69)   VALUE SPACE.                 
030300   03    FILLER.                                                          
030400      05 AR-TAB-ADBUFFOMR-2-VCOM PIC Z(2).                                
030500      05 FILLER                  PIC X       VALUE SPACE.                 
030600      05 AR-TAB-ADBUFFGANG-2-VCOM PIC Z(2).                               
030700      05 FILLER                  PIC X       VALUE SPACE.                 
030800      05 AR-TAB-ADBUFFPL-2-VCOM  PIC Z(5).                                
030900      05 FILLER                  PIC X(69)   VALUE SPACE.                 
031000   03    FILLER.                                                          
031100      05 AR-TAB-ADBUFFOMR-3-VCOM PIC Z(2).                                
031200      05 FILLER                  PIC X       VALUE SPACE.                 
031300      05 AR-TAB-ADBUFFGANG-3-VCOM PIC Z(2).                               
031400      05 FILLER                  PIC X       VALUE SPACE.                 
031500      05 AR-TAB-ADBUFFPL-3-VCOM  PIC Z(5).                                
031600      05 FILLER                  PIC X(69)   VALUE SPACE.                 
031700   03    FILLER.                                                          
031800      05 AR-TAB-BELEV1-VCOM      PIC X(23)   VALUE SPACE.                 
031900      05 FILLER                  PIC X(57)   VALUE SPACE.                 
032000   03    FILLER.                                                          
032100      05 AR-TAB-BELEV2-VCOM      PIC X(23)   VALUE SPACE.                 
032200      05 FILLER                  PIC X(57)   VALUE SPACE.                 
032300   03    FILLER.                                                          
032400      05 AR-TAB-IDFS-VCOM        PIC X(8)    VALUE SPACE.                 
032500      05 FILLER                  PIC X(72)   VALUE SPACE.                 
032600   03    FILLER.                                                          
032700      05 AR-TAB-VLARTNTO-VCOM    PIC Z(7)9.9 VALUE ZERO.                  
032800      05 FILLER                  PIC X(71)   VALUE SPACE.                 
032900   03    FILLER.                                                          
033000      05 AR-TAB-KVQPACK-3-VCOM   PIC Z(6)9   VALUE ZERO.                  
033100      05 FILLER                  PIC X(73)   VALUE SPACE.                 
033200   03    FILLER.                                                          
033300      05 AR-TAB-IDANSK-VCOM      PIC Z(2)9   VALUE ZERO.                  
033400      05 FILLER                  PIC X(77)   VALUE SPACE.                 
033500   03    FILLER.                                                          
033600      05 AR-TAB-BEARTURS-VCOM    PIC X(15)   VALUE SPACE.                 
033700      05 FILLER                  PIC X(65)   VALUE SPACE.                 
033800   03    FILLER.                                                          
033900      05 AR-TAB-KDSORT-VCOM      PIC X(2)    VALUE SPACE.                 
034000      05 FILLER                  PIC X(78)   VALUE SPACE.                 
034100   03    FILLER.                                                          
034200      05 AR-TAB-TIAVIDAT-VCOM    PIC 9(6)    VALUE ZERO.                  
034300      05 FILLER                  PIC X(74)   VALUE SPACE.                 
034400   03    FILLER.                                                          
034500      05 AR-TAB-BEFT-VCOM        PIC 9(2)    VALUE ZERO.                  
034600      05 FILLER                  PIC X(78)   VALUE SPACE.                 
034700   03    FILLER.                                                          
034800      05 AR-TAB-KDLAGEMB-VCOM    PIC X(4)    VALUE SPACE.                 
034900      05 FILLER                  PIC X(76)   VALUE SPACE.                 
035000   03    FILLER.                                                          
035100      05 AR-TAB-KDFARLIG-TEXT-L-VCOM  PIC X(10)   VALUE SPACE.            
035200      05 FILLER                  PIC X(70)   VALUE SPACE.                 
035300   03    FILLER.                                                          
035400      05 AR-TAB-KDFARLIG-TEXT-T-VCOM  PIC X(10)   VALUE SPACE.            
035500      05 FILLER                  PIC X(70)   VALUE SPACE.                 
035600   03    FILLER.                                                          
035700      05 AR-TAB-BEART-VCOM       PIC X(25)   VALUE SPACE.                 
035800      05 FILLER                  PIC X(55)   VALUE SPACE.                 
035900   03    FILLER.                                                          
036000      05 AR-TAB-IDLBBET-VCOM     PIC X(12)   VALUE SPACE.                 
036100      05 FILLER                  PIC X(68)   VALUE SPACE.                 
036200   03    FILLER.                                                          
036300      05 AR-TAB-KDRT-VCOM        PIC 9(2)    VALUE ZERO.                  
036400      05 FILLER                  PIC X(78)   VALUE SPACE.                 
036500   03    FILLER.                                                          
036600      05 AR-TAB-TIINLMOT-VCOM    PIC 9(6)    VALUE ZERO.                  
036700      05 FILLER                  PIC X(74)   VALUE SPACE.                 
036800   03    FILLER.                                                          
036900      05 AR-TAB-KDLORAPP-TEXT-VCOM PIC X(3)    VALUE SPACE.               
037000      05 FILLER                  PIC X(77)   VALUE SPACE.                 
037100   03    FILLER.                                                          
037200      05 AR-TAB-FLKVAANT-TEXT-VCOM PIC X(3)    VALUE SPACE.               
037300      05 FILLER                  PIC X(77)   VALUE SPACE.                 
037400   03    FILLER.                                                          
037500      05 AR-TAB-KDKONTR-TEXT-VCOM  PIC X(22)   VALUE SPACE.               
037600      05 FILLER                  PIC X(58)   VALUE SPACE.                 
037700   03    FILLER.                                                          
037800      05 AR-TAB-ADINLOMR-NXT1-VCOM PIC X(4)    VALUE SPACE.               
037900      05 FILLER                  PIC X       VALUE SPACE.                 
038000      05 AR-TAB-ADINLOMR-NXT2-VCOM PIC X(4)    VALUE SPACE.               
038100      05 FILLER                  PIC X       VALUE SPACE.                 
038200      05 AR-TAB-ADINLOMR-NXT3-VCOM PIC X(4)    VALUE SPACE.               
038300      05 FILLER                  PIC X       VALUE SPACE.                 
038400      05 AR-TAB-ADINLOMR-NXT4-VCOM PIC X(4)    VALUE SPACE.               
038500      05 FILLER                  PIC X       VALUE SPACE.                 
038600      05 AR-TAB-ADINLOMR-NXT5-VCOM PIC X(4)    VALUE SPACE.               
038700      05 FILLER                  PIC X       VALUE SPACE.                 
038800      05 AR-TAB-ADINLOMR-NXT6-VCOM PIC X(4)    VALUE SPACE.               
038900      05 FILLER                  PIC X(51)   VALUE SPACE.                 
039000   03    FILLER.                                                          
039100      05 AR-TAB-KVAVIS-VCOM      PIC Z(5)9   VALUE ZERO.                  
039200      05 FILLER                  PIC X(74)   VALUE SPACE.                 
039300   03    FILLER.                                                          
039400      05 AR-TAB-KVAVIS-PRIO-VCOM PIC Z(5)9   VALUE ZERO.                  
039500      05 FILLER                  PIC X(74)   VALUE SPACE.                 
039600   03    FILLER.                                                          
039700      05 AR-TAB-KVAVIS-KIT-VCOM   PIC Z(5)9  VALUE ZERO.                  
039800      05 FILLER                   PIC X(1)   VALUE '/'.                   
039900      05 AR-TAB-ADTRDEST-KIT-VCOM PIC X(3)   VALUE SPACE.                 
040000      05 FILLER                   PIC X(70)  VALUE SPACE.                 
040100   03    FILLER.                                                          
040200      05 AR-TAB-KVKVAPRIM-BER-VCOM PIC Z(5)9   VALUE ZERO.                
040300      05 FILLER                  PIC X(74)   VALUE SPACE.                 
040400   03    FILLER.                                                          
040500      05 AR-TAB-KVKVASEK-BER-VCOM PIC Z(5)9   VALUE ZERO.                 
040600      05 FILLER                  PIC X(74)   VALUE SPACE.                 
040700   03    FILLER.                                                          
040800      05 AR-TAB-IDARTNR-VCOM     PIC Z(8)    VALUE ZERO.                  
040900      05 FILLER                  PIC X(72)   VALUE SPACE.                 
041000   03    FILLER.                                                          
041100      05 AR-TAB-HH-VCOM          PIC 9(2)    VALUE ZERO.                  
041200      05 FILLER                  PIC X       VALUE '.'.                   
041300      05 AR-TAB-MM-VCOM          PIC 9(2)    VALUE ZERO.                  
041400      05 FILLER                  PIC X       VALUE '.'.                   
041500      05 AR-TAB-SS-VCOM          PIC 9(2)    VALUE ZERO.                  
041600      05 FILLER                  PIC X(72)   VALUE SPACE.                 
041700   03    FILLER.                                                          
041800      05 AR-TAB-KVROS-VCOM       PIC Z(6)    VALUE ZERO.                  
041900      05 FILLER                  PIC X(74)   VALUE SPACE.                 
042000   03    FILLER.                                                          
042100      05 AR-TAB-KDERS-VCOM       PIC 9(2)    VALUE ZERO.                  
042200      05 FILLER                  PIC X(78)   VALUE SPACE.                 
042300   03    FILLER.                                                          
042400      05 AR-TAB-KDKVAINL-VCOM    PIC X(2)    VALUE SPACE.                 
042500      05 FILLER                  PIC X(78)   VALUE SPACE.                 
042600   03    FILLER.                                                          
042700      05 AR-TAB-KDFGPRIO-VCOM    PIC Z(3)    VALUE ZERO.                  
042800      05 FILLER                  PIC X(77)   VALUE SPACE.                 
042900   03    FILLER                  PIC X(80)   VALUE                        
043000         '!P                                                   '.         
043100   03    FILLER                  PIC X(80)   VALUE                        
043200         '!R                                                   '.         
043300 01  FILLER REDEFINES AR-TAB-VCOM.                                        
043400   03  FILLER OCCURS  44.                                                 
043500     05  AR-RAD-VCOM          PIC X(80).                                  
043600     EJECT                                                                
043700******  Stående ArbetsRapport ******                                      
043800*NOVA-TAB                                                                 
043900*NOVA-TAB                                                                 
044000*NOVA-TAB                                                                 
044100*NOVA-TAB                                                                 
044200 01      FILLER                  PIC X(16)   VALUE 'NOVA-TAB'.            
044300 01      NOVA-TAB.                                                        
044400   03    FILLER                  PIC X(80)   VALUE                        
044500         '!C                                                   '.         
044600   03    FILLER                  PIC X(80)   VALUE                        
044700         '!C                                                   '.         
044800   03    FILLER                  PIC X(80)   VALUE                        
044900         '!F T E 1470   20 L 2 2 3 "VOLVO"                     '.         
045000   03    FILLER                  PIC X(80)   VALUE                        
045100         '!F T E 1420   20 L 1 1 3 "Car Parts     "            '.         
045200   03    FILLER                  PIC X(80)   VALUE                        
045300         '!F T E 1350   20 L 1 1 3 "Partinummer                '.         
045400   03    FILLER                  PIC X(80)   VALUE                        
045500         '!F T E  960   20 L 1 1 3 "Lastbärare"                '.         
045600   03    FILLER                  PIC X(80)   VALUE                        
045700         '!F T E 1160  910 L 1 1 3 "Levnr"                     '.         
045800   03    FILLER                  PIC X(80)   VALUE                        
045900         '!F T E  750   20 L 1 1 3 "Vikt"                      '.         
046000   03    FILLER                  PIC X(80)   VALUE                        
046100         '!F T E  650   20 L 1 1 3 "Lagerplats"                '.         
046200   03    FILLER                  PIC X(80)   VALUE                        
046300         '!F T E  550   20 L 1 1 3 "Buffertplats"              '.         
046400   03    FILLER                  PIC X(80)   VALUE                        
046500         '!F T E  350   20 L 1 1 3 "Leverantörens artikelnummer"'.        
046600   03    FILLER                  PIC X(80)   VALUE                        
046700         '!F T E 1160 1180 L 1 1 3 "Följesedel"                 '.        
046800   03    FILLER                  PIC X(80)   VALUE                        
046900         '!F T E  750  320 L 1 1 3 "Volym"                      '.        
047000   03    FILLER                  PIC X(80)   VALUE                        
047100         '!F T E  650  320 L 1 1 3 "Q3-kvant"                   '.        
047200   03    FILLER                  PIC X(80)   VALUE                        
047300         '!F T E  550  320 L 1 1 3 "Anskaffare"                 '.        
047400   03    FILLER                  PIC X(80)   VALUE                        
047500         '!F T E  460  320 L 1 1 3 "Ursprung"                   '.        
047600   03    FILLER                  PIC X(80)   VALUE                        
047700         '!F T E  960  620 L 1 1 3 "Enhet"                      '.        
047800   03    FILLER                  PIC X(80)   VALUE                        
047900         '!F T E 1160 1420 L 1 1 3 "Avidatum "                  '.        
048000   03    FILLER                  PIC X(80)   VALUE                        
048100         '!F T E  750  620 L 1 1 3 "Förp.typ"                   '.        
048200   03    FILLER                  PIC X(80)   VALUE                        
048300         '!F T E  650  620 L 1 1 3 "Emballage"                  '.        
048400   03    FILLER                  PIC X(80)   VALUE                        
048500         '!F T E  550  620 L 1 1 3 "L farligt gods"             '.        
048600   03    FILLER                  PIC X(80)   VALUE                        
048700         '!F T E  450  620 L 1 1 3 "T farligt gods"             '.        
048800   03    FILLER                  PIC X(80)   VALUE                        
048900         '!F T E 1420  910 L 2 2 3 "ARBETSRAPPORT"              '.        
049000   03    FILLER                  PIC X(80)   VALUE                        
049100         '!F T E 1350  910 L 1 1 3 "Benämning"                  '.        
049200   03    FILLER                  PIC X(80)   VALUE                        
049300         '!F T E 1050  910 L 1 1 3 "RT"                         '.        
049400   03    FILLER                  PIC X(80)   VALUE                        
049500         '!F T E  960  910 L 1 1 3 "AR med gods"                '.        
049600   03    FILLER                  PIC X(80)   VALUE                        
049700         '!F T E  850  910 L 1 1 3 "Restorder Antal"            '.        
049800   03    FILLER                  PIC X(80)   VALUE                        
049900         '!F T E  750  910 L 1 1 3 "Ersättningskod"             '.        
050000   03    FILLER                  PIC X(80)   VALUE                        
050100         '!F T E  650  910 L 1 1 3 "Skickas till"               '.        
050200   03    FILLER                  PIC X(80)   VALUE                        
050300         '!F T E  350 1420 L 1 1 3 "Mot.dat"                    '.        
050400   03    FILLER                  PIC X(80)   VALUE                        
050500         '!F T E  550  910 L 1 1 3 "Antalskontroll"             '.        
050600   03    FILLER                  PIC X(80)   VALUE                        
050700         '!F T E  450  910 L 1 1 3 "Kontrollkod"                '.        
050800   03    FILLER                  PIC X(80)   VALUE                        
050900         '!F T E  350  910 L 1 1 3 "Möjliga adresser"           '.        
051000   03    FILLER                  PIC X(80)   VALUE                        
051100         '!F T E 1050 1420 L 1 1 3 "Antal avis"                 '.        
051200   03    FILLER                  PIC X(80)   VALUE                        
051300         '!F T E  960 1420 L 1 1 3 "Antal prio"                 '.        
051400   03    FILLER                  PIC X(80)   VALUE                        
051500         '!F T E  850 1420 L 1 1 3 "Antal till sats"            '.        
051600   03    FILLER                  PIC X(80)   VALUE                        
051700         '!F T E  750 1420 L 1 1 3 "Antal PRIM"                 '.        
051800   03    FILLER                  PIC X(80)   VALUE                        
051900         '!F T E  650 1420 L 1 1 3 "Antal SEK"                  '.        
052000   03    FILLER                  PIC X(80)   VALUE                        
052100         '!F T E  550 1420 L 1 1 3 "Kval anm"                   '.        
052200   03    FILLER                  PIC X(80)   VALUE                        
052300         '!F T E  450 1410 L 1 1 3 "FIFO antal veckor"          '.        
052400   03    FILLER                  PIC X(80)   VALUE                        
052500         '!F T E 1350 1710 L 1 1 3 "Artnr"                      '.        
052600   03    FILLER                  PIC X(80)   VALUE                        
052700         '!F T E 1160 1710 L 1 1 3 "Partinr"                    '.        
052800   03    FILLER                  PIC X(80)   VALUE                        
052900         '!F T E 1050 1710 L 1 1 3 "Mottaget"                   '.        
053000   03    FILLER                  PIC X(80)   VALUE                        
053100         '!F T E  960 1710 L 1 1 3 "Antal prio"                 '.        
053200   03    FILLER                  PIC X(80)   VALUE                        
053300         '!F T E  850 1710 L 1 1 3 "Antal till sats"            '.        
053400   03    FILLER                  PIC X(80)   VALUE                        
053500         '!F T E  750 1710 L 1 1 3 "Antal PRIM"                 '.        
053600   03    FILLER                  PIC X(80)   VALUE                        
053700         '!F T E  650 1710 L 1 1 3 "Antal SEK"                  '.        
053800   03    FILLER                  PIC X(80)   VALUE                        
053900         '!F T E  550 1710 L 1 1 3 "Antal kvalf"                '.        
054000   03    FILLER                  PIC X(80)   VALUE                        
054100         '!F T E  450 1710 L 1 1 3 "Antal inlagt"               '.        
054200   03    FILLER                  PIC X(80)   VALUE                        
054300         '!F T E  350 1710 L 1 1 3 "Tid"                        '.        
054400   03    FILLER.                                                          
054500      05 FILLER                  PIC X(26)   VALUE                        
054600         '!F T E 1290   20 L 2 1 3 "'.                                    
054700      05 NOVA-TAB-IDLOPNRM       PIC Z(08).                               
054800      05 FILLER                  PIC X(46)   VALUE                        
054900         '"                         '.                                    
055000   03    FILLER.                                                          
055100      05 FILLER                  PIC X(26)   VALUE                        
055200         '!F T E 1100  920 L 2 1 3 "'.                                    
055300      05 NOVA-TAB-IDLEVNR        PIC Z(5).                                
055400      05 FILLER                  PIC X(49)   VALUE                        
055500         '"                         '.                                    
055600   03    FILLER.                                                          
055700      05 FILLER                  PIC X(26)   VALUE                        
055800         '!F T E  700   20 L 2 1 3 "'.                                    
055900      05 NOVA-TAB-VKART          PIC Z(06)9.                              
056000      05 FILLER                  PIC X(47)   VALUE                        
056100         '"                         '.                                    
056200   03    FILLER.                                                          
056300      05 FILLER                  PIC X(26)   VALUE                        
056400         '!F T E  600   20 L 2 1 3 "'.                                    
056500      05 NOVA-TAB-ADLAGOMR       PIC 9(02).                               
056600      05 FILLER                  PIC X       VALUE SPACE.                 
056700      05 NOVA-TAB-ADGANG         PIC Z(02).                               
056800      05 FILLER                  PIC X       VALUE SPACE.                 
056900      05 NOVA-TAB-ADPLATS        PIC Z(05).                               
057000      05 FILLER                  PIC X(43)   VALUE                        
057100         '"                         '.                                    
057200   03    FILLER.                                                          
057300      05 FILLER                  PIC X(26)   VALUE                        
057400         '!F T E  500   20 L 2 1 3 "'.                                    
057500      05 NOVA-TAB-ADBUFFOMR-1    PIC 9(02).                               
057600      05 FILLER                  PIC X       VALUE SPACE.                 
057700      05 NOVA-TAB-ADBUFFGANG-1   PIC Z(02).                               
057800      05 FILLER                  PIC X       VALUE SPACE.                 
057900      05 NOVA-TAB-ADBUFFPL-1     PIC Z(04)9.                              
058000      05 FILLER                  PIC X(43)   VALUE                        
058100         '"                         '.                                    
058200   03    FILLER.                                                          
058300      05 FILLER                  PIC X(26)   VALUE                        
058400         '!F T E  450   20 L 2 1 3 "'.                                    
058500      05 NOVA-TAB-ADBUFFOMR-2    PIC 9(02).                               
058600      05 FILLER                  PIC X       VALUE SPACE.                 
058700      05 NOVA-TAB-ADBUFFGANG-2   PIC Z(02).                               
058800      05 FILLER                  PIC X       VALUE SPACE.                 
058900      05 NOVA-TAB-ADBUFFPL-2     PIC Z(04)9.                              
059000      05 FILLER                  PIC X(43)   VALUE                        
059100         '"                         '.                                    
059200   03    FILLER.                                                          
059300      05 FILLER                  PIC X(26)   VALUE                        
059400         '!F T E  400   20 L 2 1 3 "'.                                    
059500      05 NOVA-TAB-ADBUFFOMR-3    PIC 9(02).                               
059600      05 FILLER                  PIC X       VALUE SPACE.                 
059700      05 NOVA-TAB-ADBUFFGANG-3   PIC Z(02).                               
059800      05 FILLER                  PIC X       VALUE SPACE.                 
059900      05 NOVA-TAB-ADBUFFPL-3     PIC Z(04)9.                              
060000      05 FILLER                  PIC X(43)   VALUE                        
060100         '"                         '.                                    
060200   03    FILLER.                                                          
060300      05 FILLER                  PIC X(26)   VALUE                        
060400         '!F T E  300   20 L 2 1 3 "'.                                    
060500      05 NOVA-TAB-BELEV1         PIC X(23).                               
060600      05 FILLER                  PIC X(31)   VALUE                        
060700         '"                         '.                                    
060800   03    FILLER.                                                          
060900      05 FILLER                  PIC X(26)   VALUE                        
061000         '!F T E  250   20 L 2 1 3 "'.                                    
061100      05 NOVA-TAB-BELEV2         PIC X(23).                               
061200      05 FILLER                  PIC X(31)   VALUE                        
061300         '"                         '.                                    
061400   03    FILLER.                                                          
061500      05 FILLER                  PIC X(26)   VALUE                        
061600         '!F T E 1100 1190 L 2 1 3 "'.                                    
061700      05 NOVA-TAB-IDFS           PIC Z(8).                                
061800      05 FILLER                  PIC X(46)   VALUE                        
061900         '"                         '.                                    
062000   03    FILLER.                                                          
062100      05 FILLER                  PIC X(26)   VALUE                        
062200         '!F T E  700  350 L 2 1 3 "'.                                    
062300      05 NOVA-TAB-VLARTNTO       PIC Z(7)9.9.                             
062400      05 FILLER                  PIC X(44)   VALUE                        
062500         '"                         '.                                    
062600   03    FILLER.                                                          
062700      05 FILLER                  PIC X(26)   VALUE                        
062800         '!F T E  600  350 L 2 1 3 "'.                                    
062900      05 NOVA-TAB-KVQPACK-3      PIC Z(6)9.                               
063000      05 FILLER                  PIC X(47)   VALUE                        
063100         '"                         '.                                    
063200   03    FILLER.                                                          
063300      05 FILLER                  PIC X(26)   VALUE                        
063400         '!F T E  500  350 L 2 1 3 "'.                                    
063500      05 NOVA-TAB-IDANSK         PIC Z(2)9.                               
063600      05 FILLER                  PIC X(51)   VALUE                        
063700         '"                         '.                                    
063800   03    FILLER.                                                          
063900      05 FILLER                  PIC X(26)   VALUE                        
064000         '!F T E  410  350 L 2 1 3 "'.                                    
064100      05 NOVA-TAB-BEARTURS       PIC X(15).                               
064200      05 FILLER                  PIC X(39)   VALUE                        
064300         '"                         '.                                    
064400   03    FILLER.                                                          
064500      05 FILLER                  PIC X(26)   VALUE                        
064600         '!F T E  900  800 L 1 1 6 "'.                                    
064700      05 NOVA-TAB-KDSORT         PIC X(02).                               
064800      05 FILLER                  PIC X(52)   VALUE                        
064900         '"                         '.                                    
065000   03    FILLER.                                                          
065100      05 FILLER                  PIC X(26)   VALUE                        
065200         '!F T E 1100 1420 L 2 1 3 "'.                                    
065300      05 NOVA-TAB-TIAVIDAT       PIC 9(06).                               
065400      05 FILLER                  PIC X(48)   VALUE                        
065500         '"                         '.                                    
065600   03    FILLER.                                                          
065700      05 FILLER                  PIC X(26)   VALUE                        
065800         '!F T E  700  620 L 2 1 3 "'.                                    
065900      05 NOVA-TAB-BEFT           PIC 9(02).                               
066000      05 FILLER                  PIC X(52)   VALUE                        
066100         '"                         '.                                    
066200   03    FILLER.                                                          
066300      05 FILLER                  PIC X(26)   VALUE                        
066400         '!F T E  600  620 L 2 1 3 "'.                                    
066500      05 NOVA-TAB-KDLAGEMB       PIC X(04).                               
066600      05 FILLER                  PIC X(50)   VALUE                        
066700         '"                         '.                                    
066800   03    FILLER.                                                          
066900      05 FILLER                  PIC X(26)   VALUE                        
067000         '!F T E  500  620 L 2 1 3 "'.                                    
067100      05 NOVA-TAB-KDFARLIG-TEXT-L PIC X(10).                              
067200      05 FILLER                  PIC X(44)   VALUE                        
067300         '"                         '.                                    
067400   03    FILLER.                                                          
067500      05 FILLER                  PIC X(26)   VALUE                        
067600         '!F T E  400  620 L 2 1 3 "'.                                    
067700      05 NOVA-TAB-KDFARLIG-TEXT-T PIC X(10).                              
067800      05 FILLER                  PIC X(44)   VALUE                        
067900         '"                         '.                                    
068000   03    FILLER.                                                          
068100      05 FILLER                  PIC X(26)   VALUE                        
068200         '!F T E 1290  920 L 2 1 3 "'.                                    
068300      05 NOVA-TAB-BEART          PIC X(25).                               
068400      05 FILLER                  PIC X(29)   VALUE                        
068500         '"                         '.                                    
068600   03    FILLER.                                                          
068700      05 FILLER                  PIC X(26)   VALUE                        
068800         '!F T E  900   20 L 2 1 3 "'.                                    
068900      05 NOVA-TAB-IDLBBET        PIC X(12).                               
069000      05 FILLER                  PIC X(42)   VALUE                        
069100         '"                         '.                                    
069200   03    FILLER.                                                          
069300      05 FILLER                  PIC X(26)   VALUE                        
069400         '!F T E 1000  920 L 2 1 3 "'.                                    
069500      05 NOVA-TAB-KDRT           PIC 9(02).                               
069600      05 FILLER                  PIC X(52)   VALUE                        
069700         '"                         '.                                    
069800   03    FILLER.                                                          
069900      05 FILLER                  PIC X(26)   VALUE                        
070000         '!F T E  300 1420 L 2 1 3 "'.                                    
070100      05 NOVA-TAB-TIINLMOT       PIC 9(06).                               
070200      05 FILLER                  PIC X(48)   VALUE                        
070300         '"                         '.                                    
070400   03    FILLER.                                                          
070500      05 FILLER                  PIC X(26)   VALUE                        
070600         '!F T E  900  920 L 2 1 3 "'.                                    
070700      05 NOVA-TAB-KDLORAPP-TEXT  PIC X(03).                               
070800      05 FILLER                  PIC X(51)   VALUE                        
070900         '"                         '.                                    
071000   03    FILLER.                                                          
071100      05 FILLER                  PIC X(26)   VALUE                        
071200         '!F T E  800 1220 L 1 1 6 "'.                                    
071300      05 NOVA-TAB-KVROS          PIC Z(6).                                
071400      05 FILLER                  PIC X(48)   VALUE                        
071500         '"                         '.                                    
071600   03    FILLER.                                                          
071700      05 FILLER                  PIC X(26)   VALUE                        
071800         '!F T E  700 1220 L 1 1 6 "'.                                    
071900      05 NOVA-TAB-KDERS          PIC 9(2).                                
072000      05 FILLER                  PIC X(52)   VALUE                        
072100         '"                         '.                                    
072200   03    FILLER.                                                          
072300      05 FILLER                  PIC X(26)   VALUE                        
072400         '!F T E  600  920 L 2 1 3 "'.                                    
072500      05 NOVA-TAB-KDKVAINL       PIC X(2).                                
072600      05 FILLER                  PIC X(52)   VALUE                        
072700         '"                         '.                                    
072800   03    FILLER.                                                          
072900      05 FILLER                  PIC X(26)   VALUE                        
073000         '!F T E  500  920 L 2 1 3 "'.                                    
073100      05 NOVA-TAB-FLKVAANT-TEXT  PIC X(03).                               
073200      05 FILLER                  PIC X(51)   VALUE                        
073300         '"                         '.                                    
073400   03    FILLER.                                                          
073500      05 FILLER                  PIC X(26)   VALUE                        
073600         '!F T E  400  920 L 2 1 3 "'.                                    
073700      05 NOVA-TAB-KDKONTR-TEXT   PIC X(22).                               
073800      05 FILLER                  PIC X(32)   VALUE                        
073900         '"                         '.                                    
074000   03    FILLER.                                                          
074100      05 FILLER                  PIC X(26)   VALUE                        
074200         '!F T E  300  920 L 2 1 3 "'.                                    
074300      05 NOVA-TAB-ADINLOMR-NXT1  PIC X(04).                               
074400      05 FILLER                  PIC X(01)   VALUE SPACE.                 
074500      05 NOVA-TAB-ADINLOMR-NXT2  PIC X(04).                               
074600      05 FILLER                  PIC X(01)   VALUE SPACE.                 
074700      05 NOVA-TAB-ADINLOMR-NXT3  PIC X(04).                               
074800      05 FILLER                  PIC X(01)   VALUE SPACE.                 
074900      05 NOVA-TAB-ADINLOMR-NXT4  PIC X(04).                               
075000      05 FILLER                  PIC X(01)   VALUE SPACE.                 
075100      05 NOVA-TAB-ADINLOMR-NXT5  PIC X(04).                               
075200      05 FILLER                  PIC X(01)   VALUE SPACE.                 
075300      05 NOVA-TAB-ADINLOMR-NXT6  PIC X(04).                               
075400      05 FILLER                  PIC X(25)   VALUE                        
075500         '"                       '.                                      
075600   03    FILLER.                                                          
075700      05 FILLER                  PIC X(26)   VALUE                        
075800         '!F T E 1000 1440 L 2 1 3 "'.                                    
075900      05 NOVA-TAB-KVAVIS         PIC Z(5)9.                               
076000      05 FILLER                  PIC X(48)   VALUE                        
076100         '"                         '.                                    
076200   03    FILLER.                                                          
076300      05 FILLER                  PIC X(26)   VALUE                        
076400         '!F T E  900 1440 L 2 1 3 "'.                                    
076500      05 NOVA-TAB-KVAVIS-PRIO    PIC Z(5)9.                               
076600      05 FILLER                  PIC X(48)   VALUE                        
076700         '"                         '.                                    
076800   03    FILLER.                                                          
076900      05 FILLER                  PIC X(26)   VALUE                        
077000         '!F T E  800 1420 L 2 1 3 "'.                                    
077100      05 NOVA-TAB-KVAVIS-KIT     PIC Z(5)9.                               
077200      05 FILLER                  PIC X(1)   VALUE '/'.                    
077300      05 NOVA-TAB-ADTRDEST-KIT   PIC X(3).                                
077400      05 FILLER                  PIC X(44)   VALUE                        
077500         '"                         '.                                    
077600   03    FILLER.                                                          
077700      05 FILLER                  PIC X(26)   VALUE                        
077800         '!F T E  700 1440 L 2 1 3 "'.                                    
077900      05 NOVA-TAB-KVKVAPRIM-BER  PIC Z(5)9.                               
078000      05 FILLER                  PIC X(48)   VALUE                        
078100         '"                         '.                                    
078200   03    FILLER.                                                          
078300      05 FILLER                  PIC X(26)   VALUE                        
078400         '!F T E  600 1440 L 2 1 3 "'.                                    
078500      05 NOVA-TAB-KVKVASEK-BER   PIC Z(5)9.                               
078600      05 FILLER                  PIC X(48)   VALUE                        
078700         '"                         '.                                    
078800   03    FILLER.                                                          
078900      05 FILLER                  PIC X(26)   VALUE                        
079000         '!F T E  400 1480 L 2 1 3 "'.                                    
079100      05 NOVA-TAB-KDFGPRIO       PIC Z(3).                                
079200      05 FILLER                  PIC X(51)   VALUE                        
079300         '"                         '.                                    
079400   03    FILLER.                                                          
079500      05 FILLER                  PIC X(26)   VALUE                        
079600         '!F T E 1290 1720 L 2 1 3 "'.                                    
079700      05 NOVA-TAB-IDARTNR        PIC Z(08).                               
079800      05 FILLER                  PIC X(46)   VALUE                        
079900         '"                         '.                                    
080000   03    FILLER.                                                          
080100      05 FILLER                  PIC X(26)   VALUE                        
080200         '!F T E  300 1750 L 2 1 3 "'.                                    
080300      05 NOVA-TAB-HH             PIC 9(02).                               
080400      05 FILLER                  PIC X       VALUE ':'.                   
080500      05 NOVA-TAB-MM             PIC 9(02).                               
080600      05 FILLER                  PIC X       VALUE ':'.                   
080700      05 NOVA-TAB-SS             PIC 9(02).                               
080800      05 FILLER                  PIC X(46)   VALUE                        
080900         '"                         '.                                    
081000   03    FILLER.                                                          
081100      05 FILLER                  PIC X(26)   VALUE                        
081200         '!F T E 1100 1720 L 2 1 3 "'.                                    
081300      05 NOVA-TAB-IDLOPNRM-2     PIC Z(08).                               
081400      05 FILLER                  PIC X(46)   VALUE                        
081500         '"                         '.                                    
081600   03    FILLER.                                                          
081700      05 FILLER                  PIC X(30)   VALUE                        
081800         '!F C E 1080  125 L 130 3 12 "N'.                                
081900      05 NOVA-TAB-IDLOPNRM-STRK  PIC 9(08).                               
082000      05 FILLER                  PIC X(42)   VALUE                        
082100         '"                         '.                                    
082200   03    FILLER                  PIC X(80)   VALUE                        
082300         '!F B E 1380    2 L 5 1980                             '.        
082400   03    FILLER                  PIC X(80)   VALUE                        
082500         '!F B E 1187  895 L 5 1090                             '.        
082600   03    FILLER                  PIC X(80)   VALUE                        
082700         '!F B E 1080  895 L 5 1090                             '.        
082800   03    FILLER                  PIC X(80)   VALUE                        
082900         '!F B E  990    2 L 5 1980                             '.        
083000   03    FILLER                  PIC X(80)   VALUE                        
083100         '!F B E  880    2 L 5 1980                             '.        
083200   03    FILLER                  PIC X(80)   VALUE                        
083300         '!F B E  780    2 L 5 1980                             '.        
083400   03    FILLER                  PIC X(80)   VALUE                        
083500         '!F B E  680    2 L 5 1980                             '.        
083600   03    FILLER                  PIC X(80)   VALUE                        
083700         '!F B E  580    2 L 5 1980                             '.        
083800   03    FILLER                  PIC X(80)   VALUE                        
083900         '!F B E  480  300 L 5 1690                             '.        
084000   03    FILLER                  PIC X(80)   VALUE                        
084100         '!F B E  380    2 L 5 1980                             '.        
084200   03    FILLER                  PIC X(80)   VALUE                        
084300         '!F B E  230    2 L 5 1980                             '.        
084400   03    FILLER                  PIC X(80)   VALUE                        
084500         '!F B E  380  300 L 500 5                              '.        
084600   03    FILLER                  PIC X(80)   VALUE                        
084700         '!F B E  380  600 L 610 5                              '.        
084800   03    FILLER                  PIC X(80)   VALUE                        
084900         '!F B E  990 1150 L 200 5                              '.        
085000   03    FILLER                  PIC X(80)   VALUE                        
085100         '!F B E  230 1400 L 960 5                              '.        
085200   03    FILLER                  PIC X(80)   VALUE                        
085300         '!F B E  230 1980 L 1150 5                             '.        
085400   03    FILLER                  PIC X(80)   VALUE                        
085500         '!F B E  230 1690 L 1150 5                             '.        
085600   03    FILLER                  PIC X(80)   VALUE                        
085700         '!F B E  230  890 L 1150 5                             '.        
085800   03    FILLER                  PIC X(80)   VALUE                        
085900         '!F B E  230    2 L 1150 5                             '.        
086000   03    FILLER                  PIC X(80)   VALUE                        
086100         '!P                                                    '.        
086200   03    FILLER                  PIC X(80)   VALUE                        
086300         '!R                                                   '.         
086400 01  FILLER REDEFINES NOVA-TAB.                                           
086500   03  FILLER OCCURS  114.                                                
086600     05  NOVA-RAD             PIC X(80).                                  
086700     EJECT                                                                
086800****** Liggande ArbetsRapport ******                                      
086900*AR-TAB                                                                   
087000*AR-TAB                                                                   
087100*AR-TAB                                                                   
087200*AR-TAB                                                                   
087300 01      FILLER                  PIC X(16)   VALUE 'AR-TAB'.              
087400 01      AR-TAB.                                                          
087500   03    FILLER                  PIC X(80)   VALUE                        
087600         '!C                                                   '.         
087700   03    FILLER                  PIC X(80)   VALUE                        
087800         '!C                                                   '.         
087900   03    FILLER                  PIC X(80)   VALUE                        
088000         '!Y 89 1                                              '.         
088100   03    FILLER                  PIC X(80)   VALUE                        
088200         '!K 125                                               '.         
088300   03    FILLER                  PIC X(80)   VALUE                        
088400         '!F T S 1300 2145 L 2 2 3 "VOLVO         "            '.         
088500   03    FILLER                  PIC X(80)   VALUE                        
088600         '!F T S 1250 2145 L 1 1 3 "Car Parts     "            '.         
088700   03    FILLER                  PIC X(80)   VALUE                        
088800         '!F T S 1170 2145 L 1 1 3 "Partinummer   "            '.         
088900   03    FILLER                  PIC X(80)   VALUE                        
089000         '!F T S  770 2145 L 1 1 3 "Lastbärare"                '.         
089100   03    FILLER                  PIC X(80)   VALUE                        
089200         '!F T S  970 1245 L 1 1 3 "Levnr"                     '.         
089300   03    FILLER                  PIC X(80)   VALUE                        
089400         '!F T S  570 2145 L 1 1 3 "Vikt"                      '.         
089500   03    FILLER                  PIC X(80)   VALUE                        
089600         '!F T S  470 2145 L 1 1 3 "Lagerplats"                '.         
089700   03    FILLER                  PIC X(80)   VALUE                        
089800         '!F T S  370 2145 L 1 1 3 "Buffertplats"              '.         
089900   03    FILLER                  PIC X(80)   VALUE                        
090000         '!F T S  170 2145 L 1 1 3 "Leverantörens artikelnummer"'.        
090100   03    FILLER                  PIC X(80)   VALUE                        
090200         '!F T S  970 1045 L 1 1 3 "Följesedel"                 '.        
090300   03    FILLER                  PIC X(80)   VALUE                        
090400         '!F T S  570 1845 L 1 1 3 "Volym"                      '.        
090500   03    FILLER                  PIC X(80)   VALUE                        
090600         '!F T S  470 1845 L 1 1 3 "Q3-kvant"                   '.        
090700   03    FILLER                  PIC X(80)   VALUE                        
090800         '!F T S  370 1845 L 1 1 3 "Anskaffare"                 '.        
090900   03    FILLER                  PIC X(80)   VALUE                        
091000         '!F T S  270 1845 L 1 1 3 "Ursprung"                   '.        
091100   03    FILLER                  PIC X(80)   VALUE                        
091200         '!F T S  770 1545 L 1 1 3 "Enhet"                      '.        
091300   03    FILLER                  PIC X(80)   VALUE                        
091400         '!F T S  970  745 L 1 1 3 "Avidatum "                  '.        
091500   03    FILLER                  PIC X(80)   VALUE                        
091600         '!F T S  570 1545 L 1 1 3 "Förp.typ"                   '.        
091700   03    FILLER                  PIC X(80)   VALUE                        
091800         '!F T S  470 1545 L 1 1 3 "Emballage"                  '.        
091900   03    FILLER                  PIC X(80)   VALUE                        
092000         '!F T S  370 1545 L 1 1 3 "L farligt gods"             '.        
092100   03    FILLER                  PIC X(80)   VALUE                        
092200         '!F T S  270 1545 L 1 1 3 "T farligt gods"             '.        
092300   03    FILLER                  PIC X(80)   VALUE                        
092400         '!F T S 1300 1245 L 2 2 3 "ARBETSRAPPORT"              '.        
092500   03    FILLER                  PIC X(80)   VALUE                        
092600         '!F T S 1170 1245 L 1 1 3 "Benämning"                  '.        
092700   03    FILLER                  PIC X(80)   VALUE                        
092800         '!F T S  770 1245 L 1 1 3 "AR med gods"                '.        
092900   03    FILLER                  PIC X(80)   VALUE                        
093000         '!F T S  670 1245 L 1 1 3 "Restorder Antal"            '.        
093100   03    FILLER                  PIC X(80)   VALUE                        
093200         '!F T S  570 1245 L 1 1 3 "Ersättningskod"             '.        
093300   03    FILLER                  PIC X(80)   VALUE                        
093400         '!F T S  470 1245 L 1 1 3 "Skickas till"               '.        
093500   03    FILLER                  PIC X(80)   VALUE                        
093600         '!F T S  870 1245 L 1 1 3 "RT"                         '.        
093700   03    FILLER                  PIC X(80)   VALUE                        
093800         '!F T S  170 745  L 1 1 3 "Mot.dat"                    '.        
093900   03    FILLER                  PIC X(80)   VALUE                        
094000         '!F T S  370 1245 L 1 1 3 "Antalskontroll"             '.        
094100   03    FILLER                  PIC X(80)   VALUE                        
094200         '!F T S  270 1245 L 1 1 3 "Kontrollkod"                '.        
094300   03    FILLER                  PIC X(80)   VALUE                        
094400         '!F T S  170 1245 L 1 1 3 "Möjliga adresser"           '.        
094500   03    FILLER                  PIC X(80)   VALUE                        
094600         '!F T S  870  745 L 1 1 3 "Antal avis"                 '.        
094700   03    FILLER                  PIC X(80)   VALUE                        
094800         '!F T S  770  745 L 1 1 3 "Antal prio"                 '.        
094900   03    FILLER                  PIC X(80)   VALUE                        
095000         '!F T S  670  745 L 1 1 3 "Antal till sats"            '.        
095100   03    FILLER                  PIC X(80)   VALUE                        
095200         '!F T S  570  745 L 1 1 3 "Antal PRIM"                 '.        
095300   03    FILLER                  PIC X(80)   VALUE                        
095400         '!F T S  470  745 L 1 1 3 "Antal SEK"                  '.        
095500   03    FILLER                  PIC X(80)   VALUE                        
095600         '!F T S  370  745 L 1 1 3 "Kval anm"                   '.        
095700   03    FILLER                  PIC X(80)   VALUE                        
095800         '!F T S  270  745 L 1 1 3 "FIFO antal veckor"          '.        
095900   03    FILLER                  PIC X(80)   VALUE                        
096000         '!F T S 1170  445 L 1 1 3 "Artnr"                      '.        
096100   03    FILLER                  PIC X(80)   VALUE                        
096200         '!F T S  970  445 L 1 1 3 "Partinr"                    '.        
096300   03    FILLER                  PIC X(80)   VALUE                        
096400         '!F T S  270  445 L 1 1 3 "Antal inlagt"               '.        
096500   03    FILLER                  PIC X(80)   VALUE                        
096600         '!F T S  170  445 L 1 1 3 "Tid"                        '.        
096700   03    FILLER                  PIC X(80)   VALUE                        
096800         '!F T S  870  445 L 1 1 3 "Mottaget"                   '.        
096900   03    FILLER                  PIC X(80)   VALUE                        
097000         '!F T S  770  445 L 1 1 3 "Antal prio"                 '.        
097100   03    FILLER                  PIC X(80)   VALUE                        
097200         '!F T S  670  445 L 1 1 3 "Antal till sats"            '.        
097300   03    FILLER                  PIC X(80)   VALUE                        
097400         '!F T S  570  445 L 1 1 3 "Antal PRIM"                 '.        
097500   03    FILLER                  PIC X(80)   VALUE                        
097600         '!F T S  470  445 L 1 1 3 "Antal SEK"                  '.        
097700   03    FILLER                  PIC X(80)   VALUE                        
097800         '!F T S  370  445 L 1 1 3 "Antal kvalf"                '.        
097900   03    FILLER.                                                          
098000      05 FILLER                  PIC X(26)   VALUE                        
098100         '!F T S 1120 2145 L 2 1 3 "'.                                    
098200      05 AR-TAB-IDLOPNRM         PIC 9(08).                               
098300      05 FILLER                  PIC X(46)   VALUE                        
098400         '"                         '.                                    
098500   03    FILLER.                                                          
098600      05 FILLER                  PIC X(26)   VALUE                        
098700         '!F T S  920 1245 L 2 1 3 "'.                                    
098800      05 AR-TAB-IDLEVNR          PIC X(05).                               
098900      05 FILLER                  PIC X(49)   VALUE                        
099000         '"                         '.                                    
099100   03    FILLER.                                                          
099200      05 FILLER                  PIC X(26)   VALUE                        
099300         '!F T S  520 2145 L 2 1 3 "'.                                    
099400      05 AR-TAB-VKART            PIC Z(06)9.                              
099500      05 FILLER                  PIC X(47)   VALUE                        
099600         '"                         '.                                    
099700   03    FILLER.                                                          
099800      05 FILLER                  PIC X(26)   VALUE                        
099900         '!F T S  420 2145 L 2 1 3 "'.                                    
100000      05 AR-TAB-ADLAGOMR         PIC 9(02).                               
100100      05 FILLER                  PIC X       VALUE SPACE.                 
100200      05 AR-TAB-ADGANG           PIC Z(02).                               
100300      05 FILLER                  PIC X       VALUE SPACE.                 
100400      05 AR-TAB-ADPLATS          PIC Z(05).                               
100500      05 FILLER                  PIC X(43)   VALUE                        
100600         '"                         '.                                    
100700   03    FILLER.                                                          
100800      05 FILLER                  PIC X(26)   VALUE                        
100900         '!F T S  320 2145 L 2 1 3 "'.                                    
101000      05 AR-TAB-ADBUFFOMR-1      PIC 9(02).                               
101100      05 FILLER                  PIC X       VALUE SPACE.                 
101200      05 AR-TAB-ADBUFFGANG-1     PIC Z(02).                               
101300      05 FILLER                  PIC X       VALUE SPACE.                 
101400      05 AR-TAB-ADBUFFPL-1       PIC Z(04)9.                              
101500      05 FILLER                  PIC X(43)   VALUE                        
101600         '"                         '.                                    
101700   03    FILLER.                                                          
101800      05 FILLER                  PIC X(26)   VALUE                        
101900         '!F T S  270 2145 L 2 1 3 "'.                                    
102000      05 AR-TAB-ADBUFFOMR-2      PIC 9(02).                               
102100      05 FILLER                  PIC X       VALUE SPACE.                 
102200      05 AR-TAB-ADBUFFGANG-2     PIC Z(02).                               
102300      05 FILLER                  PIC X       VALUE SPACE.                 
102400      05 AR-TAB-ADBUFFPL-2       PIC Z(04)9.                              
102500      05 FILLER                  PIC X(43)   VALUE                        
102600         '"                         '.                                    
102700   03    FILLER.                                                          
102800      05 FILLER                  PIC X(26)   VALUE                        
102900         '!F T S  220 2145 L 2 1 3 "'.                                    
103000      05 AR-TAB-ADBUFFOMR-3      PIC 9(02).                               
103100      05 FILLER                  PIC X       VALUE SPACE.                 
103200      05 AR-TAB-ADBUFFGANG-3     PIC Z(02).                               
103300      05 FILLER                  PIC X       VALUE SPACE.                 
103400      05 AR-TAB-ADBUFFPL-3       PIC Z(04)9.                              
103500      05 FILLER                  PIC X(43)   VALUE                        
103600         '"                         '.                                    
103700   03    FILLER.                                                          
103800      05 FILLER                  PIC X(26)   VALUE                        
103900         '!F T S  120 2145 L 2 1 3 "'.                                    
104000      05 AR-TAB-BELEV1           PIC X(23).                               
104100      05 FILLER                  PIC X(31)   VALUE                        
104200         '"                         '.                                    
104300   03    FILLER.                                                          
104400      05 FILLER                  PIC X(26)   VALUE                        
104500         '!F T S  65  2145 L 2 1 3 "'.                                    
104600      05 AR-TAB-BELEV2           PIC X(23).                               
104700      05 FILLER                  PIC X(31)   VALUE                        
104800         '"                         '.                                    
104900   03    FILLER.                                                          
105000      05 FILLER                  PIC X(26)   VALUE                        
105100         '!F T S  920 1045 L 2 1 3 "'.                                    
105200      05 AR-TAB-IDFS             PIC X(08).                               
105300      05 FILLER                  PIC X(46)   VALUE                        
105400         '"                         '.                                    
105500   03    FILLER.                                                          
105600      05 FILLER                  PIC X(26)   VALUE                        
105700         '!F T S  520 1845 L 2 1 3 "'.                                    
105800      05 AR-TAB-VLARTNTO         PIC Z(7)9.9.                             
105900      05 FILLER                  PIC X(44)   VALUE                        
106000         '"                         '.                                    
106100   03    FILLER.                                                          
106200      05 FILLER                  PIC X(26)   VALUE                        
106300         '!F T S  420 1845 L 2 1 3 "'.                                    
106400      05 AR-TAB-KVQPACK-3        PIC Z(6)9.                               
106500      05 FILLER                  PIC X(47)   VALUE                        
106600         '"                         '.                                    
106700   03    FILLER.                                                          
106800      05 FILLER                  PIC X(26)   VALUE                        
106900         '!F T S  320 1845 L 2 1 3 "'.                                    
107000      05 AR-TAB-IDANSK           PIC Z(2)9.                               
107100      05 FILLER                  PIC X(51)   VALUE                        
107200         '"                         '.                                    
107300   03    FILLER.                                                          
107400      05 FILLER                  PIC X(26)   VALUE                        
107500         '!F T S  220 1845 L 2 1 3 "'.                                    
107600      05 AR-TAB-BEARTURS         PIC X(15).                               
107700      05 FILLER                  PIC X(39)   VALUE                        
107800         '"                         '.                                    
108000   03    FILLER.                                                          
108100      05 FILLER                  PIC X(26)   VALUE                        
108200         '!F T S  720 1400 L 1 1 6 "'.                                    
108300      05 AR-TAB-KDSORT           PIC X(02).                               
108400      05 FILLER                  PIC X(52)   VALUE                        
108500         '"                         '.                                    
108600   03    FILLER.                                                          
108700      05 FILLER                  PIC X(26)   VALUE                        
108800         '!F T S  920  745 L 2 1 3 "'.                                    
108900      05 AR-TAB-TIAVIDAT         PIC 9(06).                               
109000      05 FILLER                  PIC X(48)   VALUE                        
109100         '"                         '.                                    
109200   03    FILLER.                                                          
109300      05 FILLER                  PIC X(26)   VALUE                        
109400         '!F T S  520 1545 L 2 1 3 "'.                                    
109500      05 AR-TAB-BEFT             PIC 9(02).                               
109600      05 FILLER                  PIC X(52)   VALUE                        
109700         '"                         '.                                    
109800   03    FILLER.                                                          
109900      05 FILLER                  PIC X(26)   VALUE                        
110000         '!F T S  420 1545 L 2 1 3 "'.                                    
110100      05 AR-TAB-KDLAGEMB         PIC X(04).                               
110200      05 FILLER                  PIC X(50)   VALUE                        
110300         '"                         '.                                    
110400   03    FILLER.                                                          
110500      05 FILLER                  PIC X(26)   VALUE                        
110600         '!F T S  320 1545 L 2 1 3 "'.                                    
110700      05 AR-TAB-KDFARLIG-TEXT-L  PIC X(10).                               
110800      05 FILLER                  PIC X(44)   VALUE                        
110900         '"                         '.                                    
111000   03    FILLER.                                                          
111100      05 FILLER                  PIC X(26)   VALUE                        
111200         '!F T S  220 1545 L 2 1 3 "'.                                    
111300      05 AR-TAB-KDFARLIG-TEXT-T  PIC X(10).                               
111400      05 FILLER                  PIC X(44)   VALUE                        
111500         '"                         '.                                    
111600   03    FILLER.                                                          
111700      05 FILLER                  PIC X(26)   VALUE                        
111800         '!F T S 1120 1245 L 2 1 3 "'.                                    
111900      05 AR-TAB-BEART            PIC X(25).                               
112000      05 FILLER                  PIC X(29)   VALUE                        
112100         '"                         '.                                    
112200   03    FILLER.                                                          
112300      05 FILLER                  PIC X(26)   VALUE                        
112400         '!F T S  720 2145 L 2 1 3 "'.                                    
112500      05 AR-TAB-IDLBBET          PIC X(12).                               
112600      05 FILLER                  PIC X(42)   VALUE                        
112700         '"                         '.                                    
112800   03    FILLER.                                                          
112900      05 FILLER                  PIC X(26)   VALUE                        
113000         '!F T S  820 1245 L 2 1 3 "'.                                    
113100      05 AR-TAB-KDRT             PIC 9(02).                               
113200      05 FILLER                  PIC X(52)   VALUE                        
113300         '"                         '.                                    
113400   03    FILLER.                                                          
113500      05 FILLER                  PIC X(26)   VALUE                        
113600         '!F T S  120  745 L 2 1 3 "'.                                    
113700      05 AR-TAB-TIINLMOT         PIC 9(06).                               
113800      05 FILLER                  PIC X(48)   VALUE                        
113900         '"                         '.                                    
114000   03    FILLER.                                                          
114100      05 FILLER                  PIC X(26)   VALUE                        
114200         '!F T S  720 1245 L 2 1 3 "'.                                    
114300      05 AR-TAB-KDLORAPP-TEXT    PIC X(03).                               
114400      05 FILLER                  PIC X(51)   VALUE                        
114500         '"                         '.                                    
114600   03    FILLER.                                                          
114700      05 FILLER                  PIC X(26)   VALUE                        
114800         '!F T S  620 1000 L 1 1 6 "'.                                    
114900      05 AR-TAB-KVROS            PIC Z(5)9.                               
115000      05 FILLER                  PIC X(48)   VALUE                        
115100         '"                         '.                                    
115200   03    FILLER.                                                          
115300      05 FILLER                  PIC X(26)   VALUE                        
115400         '!F T S  520 1000 L 1 1 6 "'.                                    
115500      05 AR-TAB-KDERS            PIC 9(2).                                
115600      05 FILLER                  PIC X(52)   VALUE                        
115700         '"                         '.                                    
115800   03    FILLER.                                                          
115900      05 FILLER                  PIC X(26)   VALUE                        
116000         '!F T S  420 1245 L 2 1 3 "'.                                    
116100      05 AR-TAB-KDKVAINL         PIC X(2).                                
116200      05 FILLER                  PIC X(52)   VALUE                        
116300         '"                         '.                                    
116400   03    FILLER.                                                          
116500      05 FILLER                  PIC X(26)   VALUE                        
116600         '!F T S  320 1245 L 2 1 3 "'.                                    
116700      05 AR-TAB-FLKVAANT-TEXT    PIC X(03).                               
116800      05 FILLER                  PIC X(51)   VALUE                        
116900         '"                         '.                                    
117000   03    FILLER.                                                          
117100      05 FILLER                  PIC X(26)   VALUE                        
117200         '!F T S  220 1245 L 2 1 3 "'.                                    
117300      05 AR-TAB-KDKONTR-TEXT     PIC X(22).                               
117400      05 FILLER                  PIC X(32)   VALUE                        
117500         '"                         '.                                    
117600   03    FILLER.                                                          
117700      05 FILLER                  PIC X(26)   VALUE                        
117800         '!F T S  120 1245 L 2 1 3 "'.                                    
117900      05 AR-TAB-ADINLOMR-NXT1    PIC X(04).                               
118000      05 FILLER                  PIC X(01)   VALUE SPACE.                 
118100      05 AR-TAB-ADINLOMR-NXT2    PIC X(04).                               
118200      05 FILLER                  PIC X(01)   VALUE SPACE.                 
118300      05 AR-TAB-ADINLOMR-NXT3    PIC X(04).                               
118400      05 FILLER                  PIC X(01)   VALUE SPACE.                 
118500      05 AR-TAB-ADINLOMR-NXT4    PIC X(04).                               
118600      05 FILLER                  PIC X(01)   VALUE SPACE.                 
118700      05 AR-TAB-ADINLOMR-NXT5    PIC X(04).                               
118800      05 FILLER                  PIC X(01)   VALUE SPACE.                 
118900      05 AR-TAB-ADINLOMR-NXT6    PIC X(04).                               
119000      05 FILLER                  PIC X(25)   VALUE                        
119100         '"                       '.                                      
119200   03    FILLER.                                                          
119300      05 FILLER                  PIC X(26)   VALUE                        
119400         '!F T S  820  745 L 2 1 3 "'.                                    
119500      05 AR-TAB-KVAVIS           PIC Z(5)9.                               
119600      05 FILLER                  PIC X(48)   VALUE                        
119700         '"                         '.                                    
119800   03    FILLER.                                                          
119900      05 FILLER                  PIC X(26)   VALUE                        
120000         '!F T S  720  745 L 2 1 3 "'.                                    
120100      05 AR-TAB-KVAVIS-PRIO      PIC Z(5)9.                               
120200      05 FILLER                  PIC X(48)   VALUE                        
120300         '"                         '.                                    
120400   03    FILLER.                                                          
120500      05 FILLER                  PIC X(26)   VALUE                        
120600         '!F T S  620  745 L 2 1 3 "'.                                    
120700      05 AR-TAB-KVAVIS-KIT       PIC Z(5)9.                               
120800      05 FILLER                  PIC X(1)   VALUE '/'.                    
120900      05 AR-TAB-ADTRDEST-KIT     PIC X(3).                                
121000      05 FILLER                  PIC X(44)   VALUE                        
121100         '"                         '.                                    
121200   03    FILLER.                                                          
121300      05 FILLER                  PIC X(26)   VALUE                        
121400         '!F T S  520  745 L 2 1 3 "'.                                    
121500      05 AR-TAB-KVKVAPRIM-BER    PIC Z(5)9.                               
121600      05 FILLER                  PIC X(48)   VALUE                        
121700         '"                         '.                                    
121800   03    FILLER.                                                          
121900      05 FILLER                  PIC X(26)   VALUE                        
122000         '!F T S  420  745 L 2 1 3 "'.                                    
122100      05 AR-TAB-KVKVASEK-BER     PIC Z(5)9.                               
122200      05 FILLER                  PIC X(48)   VALUE                        
122300         '"                         '.                                    
122400   03    FILLER.                                                          
122500      05 FILLER                  PIC X(26)   VALUE                        
122600         '!F T S  220  700 L 2 1 3 "'.                                    
122700      05 AR-TAB-KDFGPRIO         PIC Z(3).                                
122800      05 FILLER                  PIC X(51)   VALUE                        
122900         '"                         '.                                    
123000   03    FILLER.                                                          
123100      05 FILLER                  PIC X(26)   VALUE                        
123200         '!F T S 1120  445 L 2 1 3 "'.                                    
123300      05 AR-TAB-IDARTNR          PIC Z(08).                               
123400      05 FILLER                  PIC X(46)   VALUE                        
123500         '"                         '.                                    
123600   03    FILLER.                                                          
123700      05 FILLER                  PIC X(26)   VALUE                        
123800         '!F T S  120  445 L 2 1 3 "'.                                    
123900      05 AR-TAB-HH               PIC 9(02).                               
124000      05 FILLER                  PIC X       VALUE SPACE.                 
124100      05 AR-TAB-MM               PIC 9(02).                               
124200      05 FILLER                  PIC X       VALUE SPACE.                 
124300      05 AR-TAB-SS               PIC 9(02).                               
124400      05 FILLER                  PIC X(46)   VALUE                        
124500         '"                         '.                                    
124600   03    FILLER.                                                          
124700      05 FILLER                  PIC X(26)   VALUE                        
124800         '!F T S  920  445 L 2 1 3 "'.                                    
124900      05 AR-TAB-IDLOPNRM-2       PIC 9(08).                               
125000      05 FILLER                  PIC X(46)   VALUE                        
125100         '"                         '.                                    
125200   03    FILLER.                                                          
125300      05 FILLER                  PIC X(30)   VALUE                        
125400         '!F C S  900 2045 L 130 3 12 "N'.                                
125500      05 AR-TAB-IDLOPNRM-STRK    PIC 9(08).                               
125600      05 FILLER                  PIC X(42)   VALUE                        
125700         '"                         '.                                    
125800   03    FILLER                  PIC X(80)   VALUE                        
125900         '!F B S 1200 2160 L 5 2010                             '.        
126000   03    FILLER                  PIC X(80)   VALUE                        
126100         '!F B S 1000 1260 L 5 1110                             '.        
126200   03    FILLER                  PIC X(80)   VALUE                        
126300         '!F B S  900 1260 L 5 1110                             '.        
126400   03    FILLER                  PIC X(80)   VALUE                        
126500         '!F B S  800 2160 L 5 2010                             '.        
126600   03    FILLER                  PIC X(80)   VALUE                        
126700         '!F B S  700 2160 L 5 2010                             '.        
126800   03    FILLER                  PIC X(80)   VALUE                        
126900         '!F B S  600 2160 L 5 2010                             '.        
127000   03    FILLER                  PIC X(80)   VALUE                        
127100         '!F B S  500 2160 L 5 2010                             '.        
127200   03    FILLER                  PIC X(80)   VALUE                        
127300         '!F B S  400 2160 L 5 2010                             '.        
127400   03    FILLER                  PIC X(80)   VALUE                        
127500         '!F B S  300 1860 L 5 1710                             '.        
127600   03    FILLER                  PIC X(80)   VALUE                        
127700         '!F B S  200 2160 L 5 2010                             '.        
127800   03    FILLER                  PIC X(80)   VALUE                        
127900         '!F B S   50 2160 L 5 2010                             '.        
128000   03    FILLER                  PIC X(80)   VALUE                        
128100         '!F B S   50 2160 L 1150 5                             '.        
128200   03    FILLER                  PIC X(80)   VALUE                        
128300         '!F B S  200 1860 L 500 5                              '.        
128400   03    FILLER                  PIC X(80)   VALUE                        
128500         '!F B S  200 1560 L 600 5                              '.        
128600   03    FILLER                  PIC X(80)   VALUE                        
128700         '!F B S   50 1260 L 1150 5                             '.        
128800   03    FILLER                  PIC X(80)   VALUE                        
128900         '!F B S  800 1060 L 200 5                              '.        
129000   03    FILLER                  PIC X(80)   VALUE                        
129100         '!F B S   50  760 L 950 5                              '.        
129200   03    FILLER                  PIC X(80)   VALUE                        
129300         '!F B S   50  460 L 1150 5                             '.        
129400   03    FILLER                  PIC X(80)   VALUE                        
129500         '!F B S   50  150 L 1150 5                             '.        
129600   03    FILLER                  PIC X(80)   VALUE                        
129700         '!P                                                    '.        
129800   03    FILLER                  PIC X(80)   VALUE                        
129900         '!R                                                   '.         
130000 01  FILLER REDEFINES AR-TAB.                                             
130100   03  FILLER OCCURS  114.                                                
130200     05  AR-RAD               PIC X(80).                                  
130300     EJECT                                                                
130400*OLD-LASER-TAB                                                            
130500*OLD-LASER-TAB                                                            
130600*OLD-LASER-TAB                                                            
130700*OLD-LASER-TAB                                                            
130800 01      FILLER                  PIC X(16)  VALUE 'OLD-LASER-TAB'.        
130900 01      OLD-LASER-TAB.                                                   
131000   03    FILLER                  PIC X(80)   VALUE                        
131100         '¤&l0L                                                '.         
131200   03    FILLER                  PIC X(80)   VALUE                        
131300         '¤&f1Y                                                '.         
131400   03    FILLER                  PIC X(80)   VALUE                        
131500         '¤&f4X                                                '.         
131600   03    FILLER                  PIC X(80)   VALUE                        
131700         '&&??%%P                                              '.         
131800   03    FILLER                  PIC X(80)   VALUE                        
131900         '%P                                                   '.         
132000   03    FILLER                  PIC X(80)   VALUE                        
132100         '=211,1,2,3,1,5                                       '.         
132200   03    FILLER.                                                          
132300      05 FILLER                  PIC X(7)    VALUE                        
132400         '=212,"P'.                                                       
132500      05 OLD-LASER-IDLOPNRM-STRK PIC 9(8).                                
132600      05 FILLER                  PIC X(65)   VALUE '"  '.                 
132700   03    FILLER                  PIC X(80)   VALUE                        
132800         '%                                                    '.         
132900   03    FILLER.                                                          
133000      05 FILLER                  PIC X(14)   VALUE                        
133100         '¤*p0221y0005xL'.                                                
133200      05 OLD-LASER-IDLOPNRM-1    PIC Z(8).                                
133300      05 FILLER                  PIC X(58)   VALUE SPACE.                 
133400   03    FILLER.                                                          
133500      05 FILLER                  PIC X(14)   VALUE                        
133600         '¤*p0666y0005xL'.                                                
133700      05 OLD-LASER-IDLBBET       PIC X(12).                               
133800      05 FILLER                  PIC X(54)   VALUE SPACE.                 
133900   03    FILLER.                                                          
134000      05 FILLER                  PIC X(14)   VALUE                        
134100         '¤*p0902y0005xL'.                                                
134200      05 OLD-LASER-VKART         PIC Z(6)9.                               
134300      05 FILLER                  PIC X(59)   VALUE SPACE.                 
134400   03    FILLER.                                                          
134500      05 FILLER                  PIC X(14)   VALUE                        
134600         '¤*p1020y0005xL'.                                                
134700      05 OLD-LASER-ADLAGOMR      PIC 9(2).                                
134800      05 FILLER                  PIC X(1)    VALUE SPACE.                 
134900      05 OLD-LASER-ADGANG        PIC Z(2).                                
135000      05 FILLER                  PIC X(1)    VALUE SPACE.                 
135100      05 OLD-LASER-ADPLATS       PIC Z(4)9.                               
135200      05 FILLER                  PIC X(55)   VALUE SPACE.                 
135300   03    FILLER.                                                          
135400      05 FILLER                  PIC X(14)   VALUE                        
135500         '¤*p1138y0005xL'.                                                
135600      05 OLD-LASER-ADBUFFOMR-1   PIC 9(2).                                
135700      05 FILLER                  PIC X(1)    VALUE SPACE.                 
135800      05 OLD-LASER-ADBUFFGANG-1  PIC Z(2).                                
135900      05 FILLER                  PIC X(1)    VALUE SPACE.                 
136000      05 OLD-LASER-ADBUFFPL-1    PIC Z(4)9.                               
136100      05 FILLER                  PIC X(55)   VALUE SPACE.                 
136200   03    FILLER.                                                          
136300      05 FILLER                  PIC X(14)   VALUE                        
136400         '¤*p1188y0005xL'.                                                
136500      05 OLD-LASER-ADBUFFOMR-2   PIC Z(2).                                
136600      05 FILLER                  PIC X(1)    VALUE SPACE.                 
136700      05 OLD-LASER-ADBUFFGANG-2  PIC Z(2).                                
136800      05 FILLER                  PIC X(1)    VALUE SPACE.                 
136900      05 OLD-LASER-ADBUFFPL-2    PIC Z(5).                                
137000      05 FILLER                  PIC X(55)   VALUE SPACE.                 
137100   03    FILLER.                                                          
137200      05 FILLER                  PIC X(14)   VALUE                        
137300         '¤*p1238y0005xL'.                                                
137400      05 OLD-LASER-ADBUFFOMR-3   PIC Z(2).                                
137500      05 FILLER                  PIC X(1)    VALUE SPACE.                 
137600      05 OLD-LASER-ADBUFFGANG-3  PIC Z(2).                                
137700      05 FILLER                  PIC X(1)    VALUE SPACE.                 
137800      05 OLD-LASER-ADBUFFPL-3    PIC Z(5).                                
137900      05 FILLER                  PIC X(55)   VALUE SPACE.                 
138000   03    FILLER.                                                          
138100      05 FILLER                  PIC X(14)   VALUE                        
138200         '¤*p1374y0005xL'.                                                
138300      05 OLD-LASER-BELEV1        PIC X(23).                               
138400      05 FILLER                  PIC X(43)   VALUE SPACE.                 
138500   03    FILLER.                                                          
138600      05 FILLER                  PIC X(14)   VALUE                        
138700         '¤*p1424y0005xL'.                                                
138800      05 OLD-LASER-BELEV2        PIC X(23).                               
138900      05 FILLER                  PIC X(43)   VALUE SPACE.                 
139000   03    FILLER.                                                          
139100      05 FILLER                  PIC X(14)   VALUE                        
139200         '¤*p0902y0358xL'.                                                
139300      05 OLD-LASER-VLARTNTO      PIC Z(7)9.9.                             
139400      05 FILLER                  PIC X(56)   VALUE SPACE.                 
139500   03    FILLER.                                                          
139600      05 FILLER                  PIC X(14)   VALUE                        
139700         '¤*p1020y0358xL'.                                                
139800      05 OLD-LASER-KVQPACK-3     PIC Z(6)9.                               
139900      05 FILLER                  PIC X(59)   VALUE SPACE.                 
140000   03    FILLER.                                                          
140100      05 FILLER                  PIC X(14)   VALUE                        
140200         '¤*p1138y0358xL'.                                                
140300      05 OLD-LASER-IDANSK        PIC Z(2)9.                               
140400      05 FILLER                  PIC X(63)   VALUE SPACE.                 
140500   03    FILLER.                                                          
140600      05 FILLER                  PIC X(14)   VALUE                        
140700         '¤*p1266y0358xL'.                                                
140800      05 OLD-LASER-BEARTURS      PIC X(11).                               
140900      05 FILLER                  PIC X(55)   VALUE SPACE.                 
141000   03    FILLER.                                                          
141100      05 FILLER                  PIC X(14)   VALUE                        
141200         '¤*p0666y0711xL'.                                                
141300      05 OLD-LASER-KDSORT        PIC X(2).                                
141400      05 FILLER                  PIC X(64)   VALUE SPACE.                 
141500   03    FILLER.                                                          
141600      05 FILLER                  PIC X(14)   VALUE                        
141700         '¤*p0902y0711xL'.                                                
141800      05 OLD-LASER-BEFT          PIC 9(2).                                
141900      05 FILLER                  PIC X(64)   VALUE SPACE.                 
142000   03    FILLER.                                                          
142100      05 FILLER                  PIC X(14)   VALUE                        
142200         '¤*p1020y0711xL'.                                                
142300      05 OLD-LASER-KDLAGEMB      PIC X(4).                                
142400      05 FILLER                  PIC X(62)   VALUE SPACE.                 
142500   03    FILLER.                                                          
142600      05 FILLER                  PIC X(14)   VALUE                        
142700         '¤*p1138y0711xL'.                                                
142800      05 OLD-LASER-KDFARLIG-TEXT-L PIC X(10).                             
142900      05 FILLER                  PIC X(56)   VALUE SPACE.                 
143000   03    FILLER.                                                          
143100      05 FILLER                  PIC X(14)   VALUE                        
143200         '¤*p1266y0711xL'.                                                
143300      05 OLD-LASER-KDFARLIG-TEXT-T PIC X(10).                             
143400      05 FILLER                  PIC X(56)   VALUE SPACE.                 
143500   03    FILLER.                                                          
143600      05 FILLER                  PIC X(14)   VALUE                        
143700         '¤*p0221y1063xL'.                                                
143800      05 OLD-LASER-BEART         PIC X(25).                               
143900      05 FILLER                  PIC X(41)   VALUE SPACE.                 
144000   03    FILLER.                                                          
144100      05 FILLER                  PIC X(14)   VALUE                        
144200         '¤*p0430y1063xL'.                                                
144300      05 OLD-LASER-IDLEVNR       PIC X(5).                                
144400      05 FILLER                  PIC X(61)   VALUE SPACE.                 
144500   03    FILLER.                                                          
144600      05 FILLER                  PIC X(14)   VALUE                        
144700         '¤*p0548y1063xL'.                                                
144800      05 OLD-LASER-KDRT          PIC 9(2).                                
144900      05 FILLER                  PIC X(64)   VALUE SPACE.                 
145000   03    FILLER.                                                          
145100      05 FILLER                  PIC X(14)   VALUE                        
145200         '¤*p0666y1063xL'.                                                
145300      05 OLD-LASER-KDLORAPP-TEXT PIC X(3).                                
145400      05 FILLER                  PIC X(63)   VALUE SPACE.                 
145500   03    FILLER.                                                          
145600      05 FILLER                  PIC X(14)   VALUE                        
145700         '¤*p0784y1063xL'.                                                
145800      05 OLD-LASER-KVROS         PIC Z(6).                                
145900      05 FILLER                  PIC X(60)   VALUE SPACE.                 
146000   03    FILLER.                                                          
146100      05 FILLER                  PIC X(14)   VALUE                        
146200         '¤*p0902y1063xL'.                                                
146300      05 OLD-LASER-KDERS         PIC 9(2).                                
146400      05 FILLER                  PIC X(64)   VALUE SPACE.                 
146500   03    FILLER.                                                          
146600      05 FILLER                  PIC X(14)   VALUE                        
146700         '¤*p1020y1063xL'.                                                
146800      05 OLD-LASER-KDKVAINL      PIC X(2).                                
146900      05 FILLER                  PIC X(64)   VALUE SPACE.                 
147000   03    FILLER.                                                          
147100      05 FILLER                  PIC X(14)   VALUE                        
147200         '¤*p1138y1063xL'.                                                
147300      05 OLD-LASER-FLKVAANT-TEXT PIC X(3).                                
147400      05 FILLER                  PIC X(63)   VALUE SPACE.                 
147500   03    FILLER.                                                          
147600      05 FILLER                  PIC X(14)   VALUE                        
147700         '¤*p1266y1063xL'.                                                
147800      05 OLD-LASER-KDKONTR-TEXT  PIC X(22).                               
147900      05 FILLER                  PIC X(44)   VALUE SPACE.                 
148000   03    FILLER.                                                          
148100      05 FILLER                  PIC X(14)   VALUE                        
148200         '¤*p1374y1063xL'.                                                
148300      05 OLD-LASER-ADINLOMR-NXT1 PIC X(4).                                
148400      05 FILLER                  PIC X(1)   VALUE SPACE.                  
148500      05 OLD-LASER-ADINLOMR-NXT2 PIC X(4).                                
148600      05 FILLER                  PIC X(1)   VALUE SPACE.                  
148700      05 OLD-LASER-ADINLOMR-NXT3 PIC X(4).                                
148800      05 FILLER                  PIC X(1)   VALUE SPACE.                  
148900      05 OLD-LASER-ADINLOMR-NXT4 PIC X(4).                                
149000      05 FILLER                  PIC X(1)   VALUE SPACE.                  
149100      05 OLD-LASER-ADINLOMR-NXT5 PIC X(4).                                
149200      05 FILLER                  PIC X(1)   VALUE SPACE.                  
149300      05 OLD-LASER-ADINLOMR-NXT6 PIC X(4).                                
149400      05 FILLER                  PIC X(37)  VALUE SPACE.                  
149500   03    FILLER.                                                          
149600      05 FILLER                  PIC X(14)   VALUE                        
149700         '¤*p0430y1298xL'.                                                
149800      05 OLD-LASER-IDFS          PIC X(8).                                
149900      05 FILLER                  PIC X(58)  VALUE SPACE.                  
150000   03    FILLER.                                                          
150100      05 FILLER                  PIC X(14)   VALUE                        
150200         '¤*p0430y1651xL'.                                                
150300      05 OLD-LASER-TIAVIDAT      PIC 9(6).                                
150400      05 FILLER                  PIC X(60)  VALUE SPACE.                  
150500   03    FILLER.                                                          
150600      05 FILLER                  PIC X(14)   VALUE                        
150700         '¤*p0548y1651xL'.                                                
150800      05 OLD-LASER-KVAVIS        PIC Z(5)9.                               
150900      05 FILLER                  PIC X(60)  VALUE SPACE.                  
151000   03    FILLER.                                                          
151100      05 FILLER                  PIC X(14)   VALUE                        
151200         '¤*p0666y1651xL'.                                                
151300      05 OLD-LASER-KVAVIS-PRIO   PIC Z(5)9.                               
151400      05 FILLER                  PIC X(60)  VALUE SPACE.                  
151500   03    FILLER.                                                          
151600      05 FILLER                  PIC X(14)   VALUE                        
151700         '¤*p0784y1651xL'.                                                
151800      05 OLD-LASER-KVAVIS-KIT    PIC Z(5)9.                               
151900      05 FILLER                  PIC X(1)   VALUE '/'.                    
152000      05 OLD-LASER-ADTRDEST-KIT  PIC X(3).                                
152100      05 FILLER                  PIC X(56)  VALUE SPACE.                  
152200   03    FILLER.                                                          
152300      05 FILLER                  PIC X(14)   VALUE                        
152400         '¤*p0902y1651xL'.                                                
152500      05 OLD-LASER-KVKVAPRIM-BER PIC Z(5)9.                               
152600      05 FILLER                  PIC X(60)  VALUE SPACE.                  
152700   03    FILLER.                                                          
152800      05 FILLER                  PIC X(14)   VALUE                        
152900         '¤*p1020y1651xL'.                                                
153000      05 OLD-LASER-KVKVASEK-BER  PIC Z(5)9.                               
153100      05 FILLER                  PIC X(60)  VALUE SPACE.                  
153200   03    FILLER.                                                          
153300      05 FILLER                  PIC X(14)   VALUE                        
153400         '¤*p1266y1730xL'.                                                
153500      05 OLD-LASER-KDFGPRIO      PIC Z(3).                                
153600      05 FILLER                  PIC X(63)   VALUE SPACE.                 
153700   03    FILLER.                                                          
153800      05 FILLER                  PIC X(14)   VALUE                        
153900         '¤*p1374y1651xL'.                                                
154000      05 OLD-LASER-TIINLMOT      PIC 9(6).                                
154100      05 FILLER                  PIC X(60)  VALUE SPACE.                  
154200   03    FILLER.                                                          
154300      05 FILLER                  PIC X(14)   VALUE                        
154400         '¤*p0221y1969xL'.                                                
154500      05 OLD-LASER-IDARTNR       PIC Z(8).                                
154600      05 FILLER                  PIC X(58)  VALUE SPACE.                  
154700   03    FILLER.                                                          
154800      05 FILLER                  PIC X(14)   VALUE                        
154900         '¤*p0430y1969xL'.                                                
155000      05 OLD-LASER-IDLOPNRM-2    PIC Z(8).                                
155100      05 FILLER                  PIC X(58)  VALUE SPACE.                  
155200   03    FILLER.                                                          
155300      05 FILLER                  PIC X(14)   VALUE                        
155400         '¤*p1374y1969xL'.                                                
155500      05 OLD-LASER-HH            PIC 9(2).                                
155600      05 FILLER                  PIC X(1)  VALUE '.'.                     
155700      05 OLD-LASER-MM            PIC 9(2).                                
155800      05 FILLER                  PIC X(1)  VALUE '.'.                     
155900      05 OLD-LASER-SS            PIC 9(2).                                
156000      05 FILLER                  PIC X(58) VALUE SPACE.                   
156100   03    FILLER.                                                          
156200      05 FILLER                  PIC X(80)   VALUE                        
156300         '¤Z                                          '.                  
156400   03    FILLER.                                                          
156500      05 FILLER                  PIC X(80)   VALUE                        
156600         '¤E                                          '.                  
156700 01  FILLER REDEFINES OLD-LASER-TAB.                                      
156800   03  FILLER OCCURS  50.                                                 
156900     05  OLD-LASER-RAD              PIC X(80).                            
157000     EJECT                                                                
157100 01      FILLER                  PIC X(16)   VALUE 'AR-LASER-TAB'.        
157200 01      AR-LASER-TAB.                                                    
157300   03    FILLER                  PIC X(80)   VALUE                        
157400         '¤&l0L                                                '.         
157500   03    FILLER                  PIC X(80)   VALUE                        
157600         '¤&f1Y                                                '.         
157700   03    FILLER                  PIC X(80)   VALUE                        
157800         '¤&f4X                                                '.         
157900   03    FILLER                  PIC X(80)   VALUE                        
158000         '¤&a01L                                               '.         
158100*                                                                         
158200*  här skrivs barcoden ut !                                               
158300*                                                                         
158400* ¤(s inleder statement, #T-talar om språk(24670T=code 39)                
158500* #p=location,#H=font text,#v=bar heights,#b=bar widths                   
158600* #s=space widths                                                         
158700* nästa ¤(s - statement                                                   
158800* sätter tillbaka inställningarna till text istf barcode                  
158900* instruktioner finns på cd: lexmark bar code option                      
159000   03    FILLER.                                                          
159100      05 FILLER                  PIC X(12)   VALUE                        
159200         '¤*p0427y0222'.                                                  
159300      05 FILLER                  PIC X(68)   VALUE SPACE.                 
159400   03    FILLER.                                                          
159500      05 FILLER                  PIC X(1) VALUE 'P'.                      
159600      05 FILLER                  PIC X(38)  VALUE                         
159700         '¤(s4p44v12,24,36,48b12,24,36,48s24670T'.                        
159800      05 FILLER                  PIC X(1) VALUE 'P'.                      
159900      05 AR-LASER-IDLOPNRM-STRK  PIC 9(8).                                
160000      05 FILLER                  PIC X(17)  VALUE                         
160100         '¤(s1p12v0s0b4101T'.                                             
160200      05 FILLER                  PIC X(15) VALUE SPACE.                   
160300                                                                          
160400   03    FILLER                  PIC X(80)   VALUE                        
160500         '¤&a0L                                                '.         
160600   03    FILLER.                                                          
160700      05 FILLER                  PIC X(14)   VALUE                        
160800         '¤*p0221y0005xL'.                                                
160900      05 AR-LASER-IDLOPNRM-1     PIC Z(8).                                
161000      05 FILLER                  PIC X(58)   VALUE SPACE.                 
161100   03    FILLER.                                                          
161200      05 FILLER                  PIC X(14)   VALUE                        
161300         '¤*p0666y0005xL'.                                                
161400      05 AR-LASER-IDLBBET        PIC X(12).                               
161500      05 FILLER                  PIC X(54)   VALUE SPACE.                 
161600   03    FILLER.                                                          
161700      05 FILLER                  PIC X(14)   VALUE                        
161800         '¤*p0902y0005xL'.                                                
161900      05 AR-LASER-VKART          PIC Z(6)9.                               
162000      05 FILLER                  PIC X(59)   VALUE SPACE.                 
162100   03    FILLER.                                                          
162200      05 FILLER                  PIC X(14)   VALUE                        
162300         '¤*p1020y0005xL'.                                                
162400      05 AR-LASER-ADLAGOMR       PIC 9(2).                                
162500      05 FILLER                  PIC X(1)    VALUE SPACE.                 
162600      05 AR-LASER-ADGANG         PIC Z(2).                                
162700      05 FILLER                  PIC X(1)    VALUE SPACE.                 
162800      05 AR-LASER-ADPLATS        PIC Z(4)9.                               
162900      05 FILLER                  PIC X(55)   VALUE SPACE.                 
163000   03    FILLER.                                                          
163100      05 FILLER                  PIC X(14)   VALUE                        
163200         '¤*p1138y0005xL'.                                                
163300      05 AR-LASER-ADBUFFOMR-1    PIC 9(2).                                
163400      05 FILLER                  PIC X(1)    VALUE SPACE.                 
163500      05 AR-LASER-ADBUFFGANG-1   PIC Z(2).                                
163600      05 FILLER                  PIC X(1)    VALUE SPACE.                 
163700      05 AR-LASER-ADBUFFPL-1     PIC Z(4)9.                               
163800      05 FILLER                  PIC X(55)   VALUE SPACE.                 
163900   03    FILLER.                                                          
164000      05 FILLER                  PIC X(14)   VALUE                        
164100         '¤*p1188y0005xL'.                                                
164200      05 AR-LASER-ADBUFFOMR-2    PIC Z(2).                                
164300      05 FILLER                  PIC X(1)    VALUE SPACE.                 
164400      05 AR-LASER-ADBUFFGANG-2   PIC Z(2).                                
164500      05 FILLER                  PIC X(1)    VALUE SPACE.                 
164600      05 AR-LASER-ADBUFFPL-2     PIC Z(5).                                
164700      05 FILLER                  PIC X(55)   VALUE SPACE.                 
164800   03    FILLER.                                                          
164900      05 FILLER                  PIC X(14)   VALUE                        
165000         '¤*p1238y0005xL'.                                                
165100      05 AR-LASER-ADBUFFOMR-3    PIC Z(2).                                
165200      05 FILLER                  PIC X(1)    VALUE SPACE.                 
165300      05 AR-LASER-ADBUFFGANG-3   PIC Z(2).                                
165400      05 FILLER                  PIC X(1)    VALUE SPACE.                 
165500      05 AR-LASER-ADBUFFPL-3     PIC Z(5).                                
165600      05 FILLER                  PIC X(55)   VALUE SPACE.                 
165700   03    FILLER.                                                          
165800      05 FILLER                  PIC X(14)   VALUE                        
165900         '¤*p1374y0005xL'.                                                
166000      05 AR-LASER-BELEV1         PIC X(23).                               
166100      05 FILLER                  PIC X(43)   VALUE SPACE.                 
166200   03    FILLER.                                                          
166300      05 FILLER                  PIC X(14)   VALUE                        
166400         '¤*p1424y0005xL'.                                                
166500      05 AR-LASER-BELEV2         PIC X(23).                               
166600      05 FILLER                  PIC X(43)   VALUE SPACE.                 
166700   03    FILLER.                                                          
166800      05 FILLER                  PIC X(14)   VALUE                        
166900         '¤*p0902y0358xL'.                                                
167000      05 AR-LASER-VLARTNTO       PIC Z(7)9.9.                             
167100      05 FILLER                  PIC X(56)   VALUE SPACE.                 
167200   03    FILLER.                                                          
167300      05 FILLER                  PIC X(14)   VALUE                        
167400         '¤*p1020y0358xL'.                                                
167500      05 AR-LASER-KVQPACK-3      PIC Z(6)9.                               
167600      05 FILLER                  PIC X(59)   VALUE SPACE.                 
167700   03    FILLER.                                                          
167800      05 FILLER                  PIC X(14)   VALUE                        
167900         '¤*p1138y0358xL'.                                                
168000      05 AR-LASER-IDANSK         PIC Z(2)9.                               
168100      05 FILLER                  PIC X(63)   VALUE SPACE.                 
168200   03    FILLER.                                                          
168300      05 FILLER                  PIC X(14)   VALUE                        
168400         '¤*p1266y0358xL'.                                                
168500      05 AR-LASER-BEARTURS       PIC X(11).                               
168600      05 FILLER                  PIC X(55)   VALUE SPACE.                 
168700   03    FILLER.                                                          
168800      05 FILLER                  PIC X(14)   VALUE                        
168900         '¤*p0666y0711xL'.                                                
169000      05 AR-LASER-KDSORT         PIC X(2).                                
169100      05 FILLER                  PIC X(64)   VALUE SPACE.                 
169101*ba   05 FILLER                  PIC X(48)   VALUE SPACE.                 
169110*ba      '¤*p0666y0711xL¤(s1p14.4v0s3b5T'.                                
169200   03    FILLER.                                                          
169300      05 FILLER                  PIC X(14)   VALUE                        
169400         '¤*p0902y0711xL'.                                                
169500      05 AR-LASER-BEFT           PIC 9(2).                                
169600      05 FILLER                  PIC X(64)   VALUE SPACE.                 
169700   03    FILLER.                                                          
169800      05 FILLER                  PIC X(14)   VALUE                        
169900         '¤*p1020y0711xL'.                                                
170000      05 AR-LASER-KDLAGEMB       PIC X(4).                                
170100      05 FILLER                  PIC X(62)   VALUE SPACE.                 
170200   03    FILLER.                                                          
170300      05 FILLER                  PIC X(14)   VALUE                        
170400         '¤*p1138y0711xL'.                                                
170500      05 AR-LASER-KDFARLIG-TEXT-L PIC X(10).                              
170600      05 FILLER                  PIC X(56)   VALUE SPACE.                 
170700   03    FILLER.                                                          
170800      05 FILLER                  PIC X(14)   VALUE                        
170900         '¤*p1266y0711xL'.                                                
171000      05 AR-LASER-KDFARLIG-TEXT-T PIC X(10).                              
171100      05 FILLER                  PIC X(56)   VALUE SPACE.                 
171200   03    FILLER.                                                          
171300      05 FILLER                  PIC X(14)   VALUE                        
171400         '¤*p0221y1063xL'.                                                
171500      05 AR-LASER-BEART          PIC X(25).                               
171600      05 FILLER                  PIC X(41)   VALUE SPACE.                 
171700   03    FILLER.                                                          
171800      05 FILLER                  PIC X(14)   VALUE                        
171900         '¤*p0430y1063xL'.                                                
172000      05 AR-LASER-IDLEVNR        PIC X(5).                                
172100      05 FILLER                  PIC X(61)   VALUE SPACE.                 
172200   03    FILLER.                                                          
172300      05 FILLER                  PIC X(14)   VALUE                        
172400         '¤*p0548y1063xL'.                                                
172500      05 AR-LASER-KDRT           PIC 9(2).                                
172600      05 FILLER                  PIC X(64)   VALUE SPACE.                 
172700   03    FILLER.                                                          
172800      05 FILLER                  PIC X(14)   VALUE                        
172900         '¤*p0666y1063xL'.                                                
173000      05 AR-LASER-KDLORAPP-TEXT  PIC X(3).                                
173100      05 FILLER                  PIC X(63)   VALUE SPACE.                 
173200   03    FILLER.                                                          
173300      05 FILLER                  PIC X(14)   VALUE                        
173400         '¤*p0784y1063xL'.                                                
173500      05 AR-LASER-KVROS          PIC Z(6).                                
173600      05 FILLER                  PIC X(60)   VALUE SPACE.                 
173700   03    FILLER.                                                          
173800      05 FILLER                  PIC X(14)   VALUE                        
173900         '¤*p0902y1063xL'.                                                
174000      05 AR-LASER-KDERS          PIC 9(2).                                
174100      05 FILLER                  PIC X(64)   VALUE SPACE.                 
174200   03    FILLER.                                                          
174300      05 FILLER                  PIC X(14)   VALUE                        
174400         '¤*p1020y1063xL'.                                                
174500      05 AR-LASER-KDKVAINL       PIC X(2).                                
174600      05 FILLER                  PIC X(64)   VALUE SPACE.                 
174700   03    FILLER.                                                          
174800      05 FILLER                  PIC X(14)   VALUE                        
174900         '¤*p1138y1063xL'.                                                
175000      05 AR-LASER-FLKVAANT-TEXT  PIC X(3).                                
175100      05 FILLER                  PIC X(63)   VALUE SPACE.                 
175200   03    FILLER.                                                          
175300      05 FILLER                  PIC X(14)   VALUE                        
175400         '¤*p1266y1063xL'.                                                
175500      05 AR-LASER-KDKONTR-TEXT   PIC X(22).                               
175600      05 FILLER                  PIC X(44)   VALUE SPACE.                 
175700   03    FILLER.                                                          
175800      05 FILLER                  PIC X(14)   VALUE                        
175900         '¤*p1374y1063xL'.                                                
176000      05 AR-LASER-ADINLOMR-NXT1  PIC X(4).                                
176100      05 FILLER                  PIC X(1)   VALUE SPACE.                  
176200      05 AR-LASER-ADINLOMR-NXT2  PIC X(4).                                
176300      05 FILLER                  PIC X(1)   VALUE SPACE.                  
176400      05 AR-LASER-ADINLOMR-NXT3  PIC X(4).                                
176500      05 FILLER                  PIC X(1)   VALUE SPACE.                  
176600      05 AR-LASER-ADINLOMR-NXT4  PIC X(4).                                
176700      05 FILLER                  PIC X(1)   VALUE SPACE.                  
176800      05 AR-LASER-ADINLOMR-NXT5  PIC X(4).                                
176900      05 FILLER                  PIC X(1)   VALUE SPACE.                  
177000      05 AR-LASER-ADINLOMR-NXT6  PIC X(4).                                
177100      05 FILLER                  PIC X(37)  VALUE SPACE.                  
177200   03    FILLER.                                                          
177300      05 FILLER                  PIC X(14)   VALUE                        
177400         '¤*p0430y1298xL'.                                                
177500      05 AR-LASER-IDFS           PIC X(8).                                
177600      05 FILLER                  PIC X(58)  VALUE SPACE.                  
177700   03    FILLER.                                                          
177800      05 FILLER                  PIC X(14)   VALUE                        
177900         '¤*p0430y1651xL'.                                                
178000      05 AR-LASER-TIAVIDAT       PIC 9(6).                                
178100      05 FILLER                  PIC X(60)  VALUE SPACE.                  
178200   03    FILLER.                                                          
178300      05 FILLER                  PIC X(14)   VALUE                        
178400         '¤*p0548y1651xL'.                                                
178500      05 AR-LASER-KVAVIS         PIC Z(5)9.                               
178600      05 FILLER                  PIC X(60)  VALUE SPACE.                  
178700   03    FILLER.                                                          
178800      05 FILLER                  PIC X(14)   VALUE                        
178900         '¤*p0666y1651xL'.                                                
179000      05 AR-LASER-KVAVIS-PRIO    PIC Z(5)9.                               
179100      05 FILLER                  PIC X(60)  VALUE SPACE.                  
179200   03    FILLER.                                                          
179300      05 FILLER                  PIC X(14)   VALUE                        
179400         '¤*p0784y1651xL'.                                                
179500      05 AR-LASER-KVAVIS-KIT     PIC Z(5)9.                               
179600      05 FILLER                  PIC X(1)   VALUE '/'.                    
179700      05 AR-LASER-ADTRDEST-KIT   PIC X(3).                                
179800      05 FILLER                  PIC X(56)  VALUE SPACE.                  
179900   03    FILLER.                                                          
180000      05 FILLER                  PIC X(14)   VALUE                        
180100         '¤*p0902y1651xL'.                                                
180200      05 AR-LASER-KVKVAPRIM-BER  PIC Z(5)9.                               
180300      05 FILLER                  PIC X(60)  VALUE SPACE.                  
180400   03    FILLER.                                                          
180500      05 FILLER                  PIC X(14)   VALUE                        
180600         '¤*p1020y1651xL'.                                                
180700      05 AR-LASER-KVKVASEK-BER   PIC Z(5)9.                               
180800      05 FILLER                  PIC X(60)  VALUE SPACE.                  
180900   03    FILLER.                                                          
181000      05 FILLER                  PIC X(14)   VALUE                        
181100         '¤*p1266y1715xL'.                                                
181200      05 AR-LASER-KDFGPRIO       PIC Z(3).                                
181300      05 FILLER                  PIC X(63)   VALUE SPACE.                 
181400   03    FILLER.                                                          
181500      05 FILLER                  PIC X(14)   VALUE                        
181600         '¤*p1374y1651xL'.                                                
181700      05 AR-LASER-TIINLMOT       PIC 9(6).                                
181800      05 FILLER                  PIC X(60)  VALUE SPACE.                  
181900   03    FILLER.                                                          
182000      05 FILLER                  PIC X(14)   VALUE                        
182100         '¤*p0221y1969xL'.                                                
182200      05 AR-LASER-IDARTNR        PIC Z(8).                                
182300      05 FILLER                  PIC X(58)  VALUE SPACE.                  
182400   03    FILLER.                                                          
182500      05 FILLER                  PIC X(14)   VALUE                        
182600         '¤*p0430y1969xL'.                                                
182700      05 AR-LASER-IDLOPNRM-2     PIC Z(8).                                
182800      05 FILLER                  PIC X(58)  VALUE SPACE.                  
182900   03    FILLER.                                                          
183000      05 FILLER                  PIC X(14)   VALUE                        
183100         '¤*p1374y1969xL'.                                                
183200      05 AR-LASER-HH             PIC 9(2).                                
183300      05 FILLER                  PIC X(1)  VALUE '.'.                     
183400      05 AR-LASER-MM             PIC 9(2).                                
183500      05 FILLER                  PIC X(1)  VALUE '.'.                     
183600      05 AR-LASER-SS             PIC 9(2).                                
183700      05 FILLER                  PIC X(58) VALUE SPACE.                   
183800   03    FILLER.                                                          
183900      05 FILLER                  PIC X(80)   VALUE                        
184000         '¤Z                                          '.                  
184100   03    FILLER.                                                          
184200      05 FILLER                  PIC X(80)   VALUE                        
184300         '¤E                                          '.                  
184400 01  FILLER REDEFINES AR-LASER-TAB.                                       
184500   03  FILLER OCCURS  48.                                                 
184600     05  AR-LASER-RAD               PIC X(80).                            
184700     EJECT                                                                
184800****** Liggande Receiving Report *****                                    
184900*RR-LASER-TAB                                                             
185000*RR-LASER-TAB                                                             
185100*RR-LASER-TAB                                                             
185200*RR-LASER-TAB                                                             
185300 01      FILLER                  PIC X(16)   VALUE 'RR-LASER-TAB'.        
185400 01      RR-LASER-TAB.                                                    
185500   03    FILLER                  PIC X(80)   VALUE                        
185600         '^&l0L                                                '.         
185700   03    FILLER                  PIC X(80)   VALUE                        
185800         '¤R¤ CASS 2; SPO L; EXIT;                             '.         
185900   03    FILLER                  PIC X(80)   VALUE                        
186000         '^&f1Y                                                '.         
186100   03    FILLER                  PIC X(80)   VALUE                        
186200         '^&f4X                                                '.         
186300   03    FILLER.                                                          
186400      05 FILLER                  PIC X(20)   VALUE                        
186500         '^*p0271y0100xL      '.                                          
186600      05 FILLER                  PIC X(60)   VALUE SPACE.                 
186700   03    FILLER.                                                          
186800      05 FILLER                  PIC X(11)   VALUE '¤R¤ BARC 19'.         
186900      05 FILLER                  PIC X(03)   VALUE ',Y,'.                 
187000      05 FILLER                  PIC X(01)   VALUE QUOTE.                 
187100      05 FILLER                  PIC X(01)   VALUE 'P'.                   
187200      05 RR-LASER-IDLOPNRM-STRK  PIC 9(8).                                
187300      05 FILLER                  PIC X(01)   VALUE QUOTE.                 
187400      05 FILLER                  PIC X(55)   VALUE ';EXIT;'.              
187500   03    FILLER.                                                          
187600      05 FILLER                  PIC X(14)   VALUE                        
187700         '^*p0666y0005xL'.                                                
187800      05 RR-LASER-IDLBBET        PIC X(12).                               
187900      05 FILLER                  PIC X(54)   VALUE SPACE.                 
188000   03    FILLER.                                                          
188100      05 FILLER                  PIC X(14)   VALUE                        
188200         '^*p0902y0005xL'.                                                
188300      05 RR-LASER-VKART          PIC Z(6)9.                               
188400      05 FILLER                  PIC X(59)   VALUE SPACE.                 
188500   03    FILLER.                                                          
188600      05 FILLER                  PIC X(14)   VALUE                        
188700         '^*p1020y0005xL'.                                                
188800      05 RR-LASER-ADLAGOMR       PIC 9(2).                                
188900      05 FILLER                  PIC X(1)    VALUE SPACE.                 
189000      05 RR-LASER-ADGANG         PIC Z(2).                                
189100      05 FILLER                  PIC X(1)    VALUE SPACE.                 
189200      05 RR-LASER-ADPLATS        PIC Z(4)9.                               
189300      05 FILLER                  PIC X(55)   VALUE SPACE.                 
189400   03    FILLER.                                                          
189500      05 FILLER                  PIC X(14)   VALUE                        
189600         '^*p1138y0005xL'.                                                
189700      05 RR-LASER-ADBUFFOMR-1    PIC 9(2).                                
189800      05 FILLER                  PIC X(1)    VALUE SPACE.                 
189900      05 RR-LASER-ADBUFFGANG-1   PIC Z(2).                                
190000      05 FILLER                  PIC X(1)    VALUE SPACE.                 
190100      05 RR-LASER-ADBUFFPL-1     PIC Z(4)9.                               
190200      05 FILLER                  PIC X(55)   VALUE SPACE.                 
190300   03    FILLER.                                                          
190400      05 FILLER                  PIC X(14)   VALUE                        
190500         '^*p1188y0005xL'.                                                
190600      05 RR-LASER-ADBUFFOMR-2    PIC Z(2).                                
190700      05 FILLER                  PIC X(1)    VALUE SPACE.                 
190800      05 RR-LASER-ADBUFFGANG-2   PIC Z(2).                                
190900      05 FILLER                  PIC X(1)    VALUE SPACE.                 
191000      05 RR-LASER-ADBUFFPL-2     PIC Z(5).                                
191100      05 FILLER                  PIC X(55)   VALUE SPACE.                 
191200   03    FILLER.                                                          
191300      05 FILLER                  PIC X(14)   VALUE                        
191400         '^*p1238y0005xL'.                                                
191500      05 RR-LASER-ADBUFFOMR-3    PIC Z(2).                                
191600      05 FILLER                  PIC X(1)    VALUE SPACE.                 
191700      05 RR-LASER-ADBUFFGANG-3   PIC Z(2).                                
191800      05 FILLER                  PIC X(1)    VALUE SPACE.                 
191900      05 RR-LASER-ADBUFFPL-3     PIC Z(5).                                
192000      05 FILLER                  PIC X(55)   VALUE SPACE.                 
192100   03    FILLER.                                                          
192200      05 FILLER                  PIC X(14)   VALUE                        
192300         '^*p1374y0005xL'.                                                
192400      05 RR-LASER-BELEV1         PIC X(23).                               
192500      05 FILLER                  PIC X(43)   VALUE SPACE.                 
192600   03    FILLER.                                                          
192700      05 FILLER                  PIC X(14)   VALUE                        
192800         '^*p1424y0005xL'.                                                
192900      05 RR-LASER-BELEV2         PIC X(23).                               
193000      05 FILLER                  PIC X(43)   VALUE SPACE.                 
193100   03    FILLER.                                                          
193200      05 FILLER                  PIC X(14)   VALUE                        
193300         '^*p0902y0358xL'.                                                
193400      05 RR-LASER-VLARTNTO       PIC Z(7)9.9.                             
193500      05 FILLER                  PIC X(56)   VALUE SPACE.                 
193600   03    FILLER.                                                          
193700      05 FILLER                  PIC X(14)   VALUE                        
193800         '^*p1020y0358xL'.                                                
193900      05 RR-LASER-KVQPACK-3      PIC Z(6)9.                               
194000      05 FILLER                  PIC X(59)   VALUE SPACE.                 
194100   03    FILLER.                                                          
194200      05 FILLER                  PIC X(14)   VALUE                        
194300         '^*p1138y0358xL'.                                                
194400      05 RR-LASER-IDANSK         PIC Z(2)9.                               
194500      05 FILLER                  PIC X(63)   VALUE SPACE.                 
194600   03    FILLER.                                                          
194700      05 FILLER                  PIC X(14)   VALUE                        
194800         '^*p1266y0358xL'.                                                
194900      05 RR-LASER-BEARTURS       PIC X(11).                               
195000      05 FILLER                  PIC X(55)   VALUE SPACE.                 
195100   03    FILLER.                                                          
195200      05 FILLER                  PIC X(14)   VALUE                        
195300         '^*p0666y0711xL'.                                                
195400      05 RR-LASER-KDSORT         PIC X(2).                                
195500      05 FILLER                  PIC X(64)   VALUE SPACE.                 
195600   03    FILLER.                                                          
195700      05 FILLER                  PIC X(14)   VALUE                        
195800         '^*p0902y0711xL'.                                                
195900      05 RR-LASER-BEFT           PIC 9(2).                                
196000      05 FILLER                  PIC X(64)   VALUE SPACE.                 
196100   03    FILLER.                                                          
196200      05 FILLER                  PIC X(14)   VALUE                        
196300         '^*p1020y0711xL'.                                                
196400      05 RR-LASER-KDLAGEMB       PIC X(4).                                
196500      05 FILLER                  PIC X(62)   VALUE SPACE.                 
196600   03    FILLER.                                                          
196700      05 FILLER                  PIC X(14)   VALUE                        
196800         '^*p1138y0711xL'.                                                
196900      05 RR-LASER-KDFARLIG-TEXT-L PIC X(10).                              
197000      05 FILLER                  PIC X(56)   VALUE SPACE.                 
197100   03    FILLER.                                                          
197200      05 FILLER                  PIC X(14)   VALUE                        
197300         '^*p1266y0711xL'.                                                
197400      05 RR-LASER-KDFARLIG-TEXT-T PIC X(10).                              
197500      05 FILLER                  PIC X(56)   VALUE SPACE.                 
197600   03    FILLER.                                                          
197700      05 FILLER                  PIC X(14)   VALUE                        
197800         '^*p0221y1063xL'.                                                
197900      05 RR-LASER-BEART          PIC X(25).                               
198000      05 FILLER                  PIC X(41)   VALUE SPACE.                 
198100   03    FILLER.                                                          
198200      05 FILLER                  PIC X(14)   VALUE                        
198300         '^*p0430y1063xL'.                                                
198400      05 RR-LASER-IDLEVNR        PIC X(5).                                
198500      05 FILLER                  PIC X(61)   VALUE SPACE.                 
198600   03    FILLER.                                                          
198700      05 FILLER                  PIC X(14)   VALUE                        
198800         '^*p0548y1063xL'.                                                
198900      05 RR-LASER-KDRT           PIC 9(2).                                
199000      05 FILLER                  PIC X(64)   VALUE SPACE.                 
199100   03    FILLER.                                                          
199200      05 FILLER                  PIC X(14)   VALUE                        
199300         '^*p0666y1063xL'.                                                
199400      05 RR-LASER-KDLORAPP-TEXT  PIC X(3).                                
199500      05 FILLER                  PIC X(63)   VALUE SPACE.                 
199600   03    FILLER.                                                          
199700      05 FILLER                  PIC X(14)   VALUE                        
199800         '^*p0784y1063xL'.                                                
199900      05 RR-LASER-KVROS          PIC Z(6).                                
200000      05 FILLER                  PIC X(60)   VALUE SPACE.                 
200100   03    FILLER.                                                          
200200      05 FILLER                  PIC X(14)   VALUE                        
200300         '^*p0902y1063xL'.                                                
200400      05 RR-LASER-KDERS          PIC 9(2).                                
200500      05 FILLER                  PIC X(64)   VALUE SPACE.                 
200600   03    FILLER.                                                          
200700      05 FILLER                  PIC X(14)   VALUE                        
200800         '^*p1138y1063xL'.                                                
200900      05 RR-LASER-FLKVAANT-TEXT  PIC X(3).                                
201000      05 FILLER                  PIC X(63)   VALUE SPACE.                 
201100   03    FILLER.                                                          
201200      05 FILLER                  PIC X(14)   VALUE                        
201300         '^*p1266y1063xL'.                                                
201400      05 RR-LASER-KDKONTR-TEXT   PIC X(22).                               
201500      05 FILLER                  PIC X(44)   VALUE SPACE.                 
201600   03    FILLER.                                                          
201700      05 FILLER                  PIC X(14)   VALUE                        
201800         '^*p1374y1063xL'.                                                
201900      05 RR-LASER-ADINLOMR-NXT1  PIC X(4).                                
202000      05 FILLER                  PIC X(1)   VALUE SPACE.                  
202100      05 RR-LASER-ADINLOMR-NXT2  PIC X(4).                                
202200      05 FILLER                  PIC X(1)   VALUE SPACE.                  
202300      05 RR-LASER-ADINLOMR-NXT3  PIC X(4).                                
202400      05 FILLER                  PIC X(1)   VALUE SPACE.                  
202500      05 RR-LASER-ADINLOMR-NXT4  PIC X(4).                                
202600      05 FILLER                  PIC X(1)   VALUE SPACE.                  
202700      05 RR-LASER-ADINLOMR-NXT5  PIC X(4).                                
202800      05 FILLER                  PIC X(1)   VALUE SPACE.                  
202900      05 RR-LASER-ADINLOMR-NXT6  PIC X(4).                                
203000      05 FILLER                  PIC X(37)  VALUE SPACE.                  
203100   03    FILLER.                                                          
203200      05 FILLER                  PIC X(14)   VALUE                        
203300         '^*p0430y1298xL'.                                                
203400      05 RR-LASER-IDFS           PIC X(8).                                
203500      05 FILLER                  PIC X(58)  VALUE SPACE.                  
203600   03    FILLER.                                                          
203700      05 FILLER                  PIC X(14)   VALUE                        
203800         '^*p0430y1651xL'.                                                
203900      05 RR-LASER-TIAVIDAT       PIC 9(6).                                
204000      05 FILLER                  PIC X(60)  VALUE SPACE.                  
204100   03    FILLER.                                                          
204200      05 FILLER                  PIC X(14)   VALUE                        
204300         '^*p0548y1651xL'.                                                
204400      05 RR-LASER-KVAVIS         PIC Z(5)9.                               
204500      05 FILLER                  PIC X(60)  VALUE SPACE.                  
204600   03    FILLER.                                                          
204700      05 FILLER                  PIC X(14)   VALUE                        
204800         '^*p0666y1651xL'.                                                
204900      05 RR-LASER-KVAVIS-PRIO    PIC Z(5)9.                               
205000      05 FILLER                  PIC X(60)  VALUE SPACE.                  
205100   03    FILLER.                                                          
205200      05 FILLER                  PIC X(14)   VALUE                        
205300         '^*p0784y1651xL'.                                                
205400      05 RR-LASER-KVAVIS-KIT     PIC Z(5)9.                               
205500      05 FILLER                  PIC X(60)  VALUE SPACE.                  
205600   03    FILLER.                                                          
205700      05 FILLER                  PIC X(14)   VALUE                        
205800         '^*p0902y1651xL'.                                                
205900      05 RR-LASER-KVKVAPRIM-BER  PIC Z(5)9.                               
206000      05 FILLER                  PIC X(60)  VALUE SPACE.                  
206100   03    FILLER.                                                          
206200      05 FILLER                  PIC X(14)   VALUE                        
206300         '^*p1020y1651xL'.                                                
206400      05 RR-LASER-KVKVASEK-BER   PIC Z(5)9.                               
206500      05 FILLER                  PIC X(60)  VALUE SPACE.                  
206600   03    FILLER.                                                          
206700      05 FILLER                  PIC X(14)   VALUE                        
206800         '^*p1266y1691xL'.                                                
206900      05 RR-LASER-KDFGPRIO       PIC Z(3).                                
207000      05 FILLER                  PIC X(63)   VALUE SPACE.                 
207100   03    FILLER.                                                          
207200      05 FILLER                  PIC X(14)   VALUE                        
207300         '^*p1374y1651xL'.                                                
207400      05 RR-LASER-TIINLMOT       PIC 9(6).                                
207500      05 FILLER                  PIC X(60)  VALUE SPACE.                  
207600   03    FILLER.                                                          
207700      05 FILLER                  PIC X(14)   VALUE                        
207800         '^*p0221y1969xL'.                                                
207900      05 RR-LASER-IDARTNR        PIC Z(8).                                
208000      05 FILLER                  PIC X(58)  VALUE SPACE.                  
208100   03    FILLER.                                                          
208200      05 FILLER                  PIC X(14)   VALUE                        
208300         '^*p0430y1969xL'.                                                
208400      05 RR-LASER-IDLOPNRM-2     PIC Z(8).                                
208500      05 FILLER                  PIC X(58)  VALUE SPACE.                  
208600   03    FILLER.                                                          
208700      05 FILLER                  PIC X(14)   VALUE                        
208800         '^*p1374y1969xL'.                                                
208900      05 RR-LASER-HH             PIC 9(2).                                
209000      05 FILLER                  PIC X(1)  VALUE '.'.                     
209100      05 RR-LASER-MM             PIC 9(2).                                
209200      05 FILLER                  PIC X(1)  VALUE '.'.                     
209300      05 RR-LASER-SS             PIC 9(2).                                
209400      05 FILLER                  PIC X(58) VALUE SPACE.                   
209500   03    FILLER.                                                          
209600      05 FILLER                  PIC X(80)   VALUE                        
209700         '^Z                                          '.                  
209800   03    FILLER.                                                          
209900      05 FILLER                  PIC X(80)   VALUE                        
210000         '^E                                          '.                  
210100   03    FILLER                  PIC X(80)   VALUE                        
210200         '¤R¤ CASS 1; SPO P; EXIT, E;                          '.         
210300 01  FILLER REDEFINES RR-LASER-TAB.                                       
210400   03  FILLER OCCURS  47.                                                 
210500     05  RR-LASER-RAD               PIC X(80).                            
210600     EJECT                                                                
210700****** Stående Receiving Report *****                                     
210800*RR-TAB ZEBRA                                                             
210900*RR-TAB ZEBRA                                                             
211000*RR-TAB ZEBRA                                                             
211100*RR-TAB ZEBRA                                                             
211200 01      FILLER                  PIC X(16)   VALUE 'RR-TAB ZEBRA'.        
211300 01      RR-TAB-ZEBRA.                                                    
211400   03    FILLER                  PIC X(80)   VALUE                        
211500                   '^XA^CF0^FWN^FS'.                                      
211600   03    FILLER                  PIC X(80)   VALUE                        
211700                   '^FO0050,0250^GB1500,1,05^FS'.                         
211800   03    FILLER                  PIC X(80)   VALUE                        
211900                   '^FO0750,0400^GB0800,1,05^FS'.                         
212000   03    FILLER                  PIC X(80)   VALUE                        
212100                   '^FO0750,0500^GB0800,1,05^FS'.                         
212200   03    FILLER                  PIC X(80)   VALUE                        
212300                   '^FO0050,0600^GB1500,1,05^FS'.                         
212400   03    FILLER                  PIC X(80)   VALUE                        
212500                   '^FO0050,0700^GB1500,1,05^FS'.                         
212600   03    FILLER                  PIC X(80)   VALUE                        
212700                   '^FO0050,0800^GB1500,1,05^FS'.                         
212800   03    FILLER                  PIC X(80)   VALUE                        
212900                   '^FO0050,0900^GB1500,1,05^FS'.                         
213000   03    FILLER                  PIC X(80)   VALUE                        
213100                   '^FO0050,1000^GB1500,1,05^FS'.                         
213200   03    FILLER                  PIC X(80)   VALUE                        
213300                   '^FO0300,1100^GB1250,1,05^FS'.                         
213400   03    FILLER                  PIC X(80)   VALUE                        
213500                   '^FO0050,1200^GB1500,1,05^FS'.                         
213600   03    FILLER                  PIC X(80)   VALUE                        
213700                   '^FO0050,1300^GB1500,1,05^FS'.                         
213800   03    FILLER                  PIC X(80)   VALUE                        
213900                   '^FO0050,0250^GB1,1050,05^FS'.                         
214000   03    FILLER                  PIC X(80)   VALUE                        
214100                   '^FO0300,0700^GB1,0500,05^FS'.                         
214200   03    FILLER                  PIC X(80)   VALUE                        
214300                   '^FO0500,0600^GB1,0600,05^FS'.                         
214400   03    FILLER                  PIC X(80)   VALUE                        
214500                   '^FO0750,0250^GB1,1050,05^FS'.                         
214600   03    FILLER                  PIC X(80)   VALUE                        
214700                   '^FO0900,0400^GB1,0200,05^FS'.                         
214800   03    FILLER                  PIC X(80)   VALUE                        
214900                   '^FO1100,0400^GB1,0900,05^FS'.                         
215000   03    FILLER                  PIC X(80)   VALUE                        
215100                   '^FO1300,0250^GB1,1050,05^FS'.                         
215200   03    FILLER                  PIC X(80)   VALUE                        
215300                   '^FO1550,0250^GB1,1050,05^FS'.                         
215400   03    FILLER                  PIC X(80)   VALUE                        
215500         '^FO0050,0150^AON,0050,0075^FD VOLVO^FS'.                        
215600   03    FILLER                  PIC X(80)   VALUE                        
215700         '^FO0550,0150^AON,0050,0075^FD RECEIVING REPORT^FS'.             
215800   03    FILLER                  PIC X(80)   VALUE                        
215900         '^FO0060,0260^AON,0020,0015^FD Sequence No.^FS'.                 
216000   03    FILLER                  PIC X(80)   VALUE                        
216100         '^FO0760,0260^AON,0020,0015^FD Description^FS'.                  
216200   03    FILLER                  PIC X(80)   VALUE                        
216300         '^FO1310,0260^AON,0020,0015^FD Part No.^FS'.                     
216400   03    FILLER                  PIC X(80)   VALUE                        
216500         '^FO0760,0410^AON,0020,0015^FD Supplier^FS'.                     
216600   03    FILLER                  PIC X(80)   VALUE                        
216700         '^FO0910,0410^AON,0020,0015^FD Advice note^FS'.                  
216800   03    FILLER                  PIC X(80)   VALUE                        
216900         '^FO1110,0410^AON,0020,0015^FD Adv. Date^FS'.                    
217000   03    FILLER                  PIC X(80)   VALUE                        
217100         '^FO1310,0410^AON,0020,0015^FD Seq. No.^FS'.                     
217200   03    FILLER                  PIC X(80)   VALUE                        
217300         '^FO0760,0510^AON,0020,0015^FD AT^FS'.                           
217400   03    FILLER                  PIC X(80)   VALUE                        
217500         '^FO1110,0510^AON,0020,0015^FD Qty. Adv.^FS'.                    
217600   03    FILLER                  PIC X(80)   VALUE                        
217700         '^FO1310,0510^AON,0020,0015^FD Received^FS'.                     
217800   03    FILLER                  PIC X(80)   VALUE                        
217900         '^FO0060,0610^AON,0020,0015^FD Carrier^FS'.                      
218000   03    FILLER                  PIC X(80)   VALUE                        
218100         '^FO0510,0610^AON,0020,0015^FD Unit^FS'.                         
218200   03    FILLER                  PIC X(80)   VALUE                        
218300         '^FO0760,0610^AON,0020,0015^FD RR W. Goods^FS'.                  
218400   03    FILLER                  PIC X(80)   VALUE                        
218500         '^FO1110,0610^AON,0020,0015^FD Qty. Prio^FS'.                    
218600   03    FILLER                  PIC X(80)   VALUE                        
218700         '^FO1310,0610^AON,0020,0015^FD Qty. Prio^FS'.                    
218800   03    FILLER                  PIC X(80)   VALUE                        
218900         '^FO0760,0710^AON,0020,0015^FD Qty. BO^FS'.                      
219000   03    FILLER                  PIC X(80)   VALUE                        
219100         '^FO1110,0710^AON,0020,0015^FD Qty. Kit^FS'.                     
219200   03    FILLER                  PIC X(80)   VALUE                        
219300         '^FO1310,0710^AON,0020,0015^FD Qty. Kit^FS'.                     
219400   03    FILLER                  PIC X(80)   VALUE                        
219500         '^FO0060,0810^AON,0020,0015^FD Weight^FS'.                       
219600   03    FILLER                  PIC X(80)   VALUE                        
219700         '^FO0310,0810^AON,0020,0015^FD Volume^FS'.                       
219800   03    FILLER                  PIC X(80)   VALUE                        
219900         '^FO0510,0810^AON,0020,0015^FD PT^FS'.                           
220000   03    FILLER                  PIC X(80)   VALUE                        
220100         '^FO1110,0810^AON,0020,0015^FD Qty. PRIM^FS'.                    
220200   03    FILLER                  PIC X(80)   VALUE                        
220300         '^FO1310,0810^AON,0020,0015^FD Qty. PRIM^FS'.                    
220400   03    FILLER                  PIC X(80)   VALUE                        
220500         '^FO0060,0910^AON,0020,0015^FD Storage Area^FS'.                 
220600   03    FILLER                  PIC X(80)   VALUE                        
220700         '^FO0310,0910^AON,0020,0015^FD Q3^FS'.                           
220800   03    FILLER                  PIC X(80)   VALUE                        
220900         '^FO0510,0910^AON,0020,0015^FD T. Pack^FS'.                      
221000   03    FILLER                  PIC X(80)   VALUE                        
221100         '^FO0060,1010^AON,0020,0015^FD Buffer area^FS'.                  
221200   03    FILLER                  PIC X(80)   VALUE                        
221300         '^FO0310,1010^AON,0020,0015^FD Procurer^FS'.                     
221400   03    FILLER                  PIC X(80)   VALUE                        
221500         '^FO0510,1010^AON,0020,0015^FD S danger goods^FS'.               
221600   03    FILLER                  PIC X(80)   VALUE                        
221700         '^FO0760,1010^AON,0020,0015^FD Qty. control^FS'.                 
221800   03    FILLER                  PIC X(80)   VALUE                        
221900         '^FO1310,1010^AON,0020,0015^FD Qty. qual.err^FS'.                
222000   03    FILLER                  PIC X(80)   VALUE                        
222100         '^FO0310,1110^AON,0020,0015^FD C orgin^FS'.                      
222200   03    FILLER                  PIC X(80)   VALUE                        
222300         '^FO0510,1110^AON,0020,0015^FD T danger goods^FS'.               
222400   03    FILLER                  PIC X(80)   VALUE                        
222500         '^FO0760,1110^AON,0020,0015^FD Control code^FS'.                 
222600   03    FILLER                  PIC X(80)   VALUE                        
222700         '^FO1110,1110^AON,0020,0015^FD FIFO week^FS'.                    
222800   03    FILLER                  PIC X(80)   VALUE                        
222900         '^FO1310,1110^AON,0020,0015^FD Qty. Binned^FS'.                  
223000   03    FILLER                  PIC X(80)   VALUE                        
223100         '^FO0060,1210^AON,0020,0015^FD Suppliers partnumber^FS'.         
223200   03    FILLER                  PIC X(80)   VALUE                        
223300         '^FO0760,1210^AON,0020,0015^FD Possible addr^FS'.                
223400   03    FILLER                  PIC X(80)   VALUE                        
223500         '^FO1110,1210^AON,0020,0015^FD Rec. date^FS'.                    
223600   03    FILLER                  PIC X(80)   VALUE                        
223700         '^FO1310,1210^AON,0020,0015^FD Time^FS'.                         
223800   03    FILLER.                                                          
223900     05  FILLER                  PIC X(29)   VALUE                        
224000         '^FO0770,0290^AON,0050,0045^FD'.                                 
224100     05  RR-ZEBRA-BEART          PIC X(25).                               
224200     05  FILLER                  PIC X(26)   VALUE                        
224300         '^FS'.                                                           
224400   03    FILLER.                                                          
224500     05  FILLER                  PIC X(29)   VALUE                        
224600         '^FO1320,0290^AON,0050,0045^FD'.                                 
224700     05  RR-ZEBRA-IDARTNR        PIC Z(08).                               
224800     05  FILLER                  PIC X(43)   VALUE                        
224900         '^FS'.                                                           
225000   03    FILLER.                                                          
225100     05  FILLER                  PIC X(29)   VALUE                        
225200         '^FO0770,0440^AON,0050,0045^FD'.                                 
225300     05  RR-ZEBRA-IDLEVNR        PIC X(05).                               
225400     05  FILLER                  PIC X(46)   VALUE                        
225500         '^FS'.                                                           
225600   03    FILLER.                                                          
225700     05  FILLER                  PIC X(29)   VALUE                        
225800         '^FO0920,0440^AON,0050,0045^FD'.                                 
225900     05  RR-ZEBRA-IDFS           PIC X(08).                               
226000     05  FILLER                  PIC X(43)   VALUE                        
226100         '^FS'.                                                           
226200   03    FILLER.                                                          
226300     05  FILLER                  PIC X(29)   VALUE                        
226400         '^FO1120,0440^AON,0050,0045^FD'.                                 
226500     05  RR-ZEBRA-TIAVIDAT       PIC 9(06).                               
226600     05  FILLER                  PIC X(45)   VALUE                        
226700         '^FS'.                                                           
226800   03    FILLER.                                                          
226900     05  FILLER                  PIC X(29)   VALUE                        
227000         '^FO1320,0440^AON,0050,0045^FD'.                                 
227100     05  RR-ZEBRA-IDLOPNRM       PIC Z(08).                               
227200     05  FILLER                  PIC X(43)   VALUE                        
227300         '^FS'.                                                           
227400   03    FILLER.                                                          
227500     05  FILLER                  PIC X(29)   VALUE                        
227600         '^FO0770,0540^AON,0050,0045^FD'.                                 
227700     05  RR-ZEBRA-KDRT           PIC 9(02).                               
227800     05  FILLER                  PIC X(49)   VALUE                        
227900         '^FS'.                                                           
228000   03    FILLER.                                                          
228100     05  FILLER                  PIC X(29)   VALUE                        
228200         '^FO1120,0540^AON,0050,0045^FD'.                                 
228300     05  RR-ZEBRA-KVAVIS         PIC Z(05)9.                              
228400     05  FILLER                  PIC X(45)   VALUE                        
228500         '^FS'.                                                           
228600   03    FILLER.                                                          
228700     05  FILLER                  PIC X(29)   VALUE                        
228800         '^FO0070,0640^AON,0050,0045^FD'.                                 
228900     05  RR-ZEBRA-IDLBBET        PIC X(12).                               
229000     05  FILLER                  PIC X(39)   VALUE                        
229100         '^FS'.                                                           
229200   03    FILLER.                                                          
229300     05  FILLER                  PIC X(29)   VALUE                        
229400         '^FO0520,0640^AON,0050,0045^FD'.                                 
229500     05  RR-ZEBRA-KDSORT         PIC X(02).                               
229600     05  FILLER                  PIC X(49)   VALUE                        
229700         '^FS'.                                                           
229800   03    FILLER.                                                          
229900     05  FILLER                  PIC X(29)   VALUE                        
230000         '^FO0770,0640^AON,0050,0045^FD'.                                 
230100     05  RR-ZEBRA-KDLORAPP-TEXT  PIC X(03).                               
230200     05  FILLER                  PIC X(48)   VALUE                        
230300         '^FS'.                                                           
230400   03    FILLER.                                                          
230500     05  FILLER                  PIC X(29)   VALUE                        
230600         '^FO1120,0640^AON,0050,0045^FD'.                                 
230700     05  RR-ZEBRA-KVAVIS-PRIO    PIC Z(06).                               
230800     05  FILLER                  PIC X(45)   VALUE                        
230900         '^FS'.                                                           
231000   03    FILLER.                                                          
231100     05  FILLER                  PIC X(29)   VALUE                        
231200         '^FO1120,0740^AON,0050,0045^FD'.                                 
231300     05  RR-ZEBRA-KVAVIS-KIT     PIC Z(06).                               
231400     05  FILLER                  PIC X(45)   VALUE                        
231500         '^FS'.                                                           
231600   03    FILLER.                                                          
231700     05  FILLER                  PIC X(29)   VALUE                        
231800         '^FO0070,0840^AON,0050,0045^FD'.                                 
231900     05  RR-ZEBRA-VKART          PIC Z(06)9.                              
232000     05  FILLER                  PIC X(44)   VALUE                        
232100         '^FS'.                                                           
232200   03    FILLER.                                                          
232300     05  FILLER                  PIC X(29)   VALUE                        
232400         '^FO0320,0840^AON,0050,0045^FD'.                                 
232500     05  RR-ZEBRA-VLARTNTO       PIC Z(07)9.9.                            
232600     05  FILLER                  PIC X(41)   VALUE                        
232700         '^FS'.                                                           
232800   03    FILLER.                                                          
232900     05  FILLER                  PIC X(29)   VALUE                        
233000         '^FO0520,0840^AON,0050,0045^FD'.                                 
233100     05  RR-ZEBRA-BEFT           PIC Z(01)9.                              
233200     05  FILLER                  PIC X(49)   VALUE                        
233300         '^FS'.                                                           
233400   03    FILLER.                                                          
233500     05  FILLER                  PIC X(29)   VALUE                        
233600         '^FO0770,0840^AON,0050,0045^FD'.                                 
233700     05  RR-ZEBRA-KDERS          PIC 9(02).                               
233800     05  FILLER                  PIC X(49)   VALUE                        
233900         '^FS'.                                                           
234000   03    FILLER.                                                          
234100     05  FILLER                  PIC X(29)   VALUE                        
234200         '^FO1120,0840^AON,0050,0045^FD'.                                 
234300     05  RR-ZEBRA-KVKVAPRIM-BER  PIC Z(06).                               
234400     05  FILLER                  PIC X(45)   VALUE                        
234500         '^FS'.                                                           
234600   03    FILLER.                                                          
234700     05  FILLER                  PIC X(29)   VALUE                        
234800         '^FO0070,0940^AON,0050,0045^FD'.                                 
234900     05  RR-ZEBRA-ADLAGOMR       PIC Z(01)9.                              
235000     05  FILLER                  PIC X(01)   VALUE SPACE.                 
235100     05  RR-ZEBRA-ADGANG         PIC Z(01)9.                              
235200     05  FILLER                  PIC X(01)   VALUE SPACE.                 
235300     05  RR-ZEBRA-ADPLATS        PIC Z(04)9.                              
235400     05  FILLER                  PIC X(40)   VALUE                        
235500         '^FS'.                                                           
235600   03    FILLER.                                                          
235700     05  FILLER                  PIC X(29)   VALUE                        
235800         '^FO0320,0940^AON,0050,0045^FD'.                                 
235900     05  RR-ZEBRA-KVQPACK-3      PIC Z(06)9.                              
236000     05  FILLER                  PIC X(44)   VALUE                        
236100         '^FS'.                                                           
236200   03    FILLER.                                                          
236300     05  FILLER                  PIC X(29)   VALUE                        
236400         '^FO0520,0940^AON,0050,0045^FD'.                                 
236500     05  RR-ZEBRA-KDLAGEMB       PIC X(04).                               
236600     05  FILLER                  PIC X(47)   VALUE                        
236700         '^FS'.                                                           
236800   03    FILLER.                                                          
236900     05  FILLER                  PIC X(29)   VALUE                        
237000         '^FO0070,1040^AON,0050,0045^FD'.                                 
237100     05  RR-ZEBRA-ADBUFFOMR-1    PIC Z(01)9.                              
237200     05  FILLER                  PIC X(01)   VALUE SPACE.                 
237300     05  RR-ZEBRA-ADBUFFGANG-1   PIC Z(01)9.                              
237400     05  FILLER                  PIC X(01)   VALUE SPACE.                 
237500     05  RR-ZEBRA-ADBUFFPL-1     PIC Z(04)9.                              
237600     05  FILLER                  PIC X(40)   VALUE                        
237700         '^FS'.                                                           
237800   03    FILLER.                                                          
237900     05  FILLER                  PIC X(29)   VALUE                        
238000         '^FO0070,1100^AON,0050,0045^FD'.                                 
238100     05  RR-ZEBRA-ADBUFFOMR-2    PIC Z(01)9.                              
238200     05  FILLER                  PIC X(01)   VALUE SPACE.                 
238300     05  RR-ZEBRA-ADBUFFGANG-2   PIC Z(01)9.                              
238400     05  FILLER                  PIC X(01)   VALUE SPACE.                 
238500     05  RR-ZEBRA-ADBUFFPL-2     PIC Z(04)9.                              
238600     05  FILLER                  PIC X(40)   VALUE                        
238700         '^FS'.                                                           
238800   03    FILLER.                                                          
238900     05  FILLER                  PIC X(29)   VALUE                        
239000         '^FO0070,1160^AON,0050,0045^FD'.                                 
239100     05  RR-ZEBRA-ADBUFFOMR-3    PIC Z(01)9.                              
239200     05  FILLER                  PIC X(01)   VALUE SPACE.                 
239300     05  RR-ZEBRA-ADBUFFGANG-3   PIC Z(01)9.                              
239400     05  FILLER                  PIC X(01)   VALUE SPACE.                 
239500     05  RR-ZEBRA-ADBUFFPL-3     PIC Z(04)9.                              
239600     05  FILLER                  PIC X(40)   VALUE                        
239700         '^FS'.                                                           
239800   03    FILLER.                                                          
239900     05  FILLER                  PIC X(29)   VALUE                        
240000         '^FO0320,1040^AON,0050,0045^FD'.                                 
240100     05  RR-ZEBRA-IDANSK         PIC Z(02)9.                              
240200     05  FILLER                  PIC X(48)   VALUE                        
240300         '^FS'.                                                           
240400   03    FILLER.                                                          
240500     05  FILLER                  PIC X(29)   VALUE                        
240600         '^FO0520,1040^AON,0050,0045^FD'.                                 
240700     05  RR-ZEBRA-KDFARLIG-TEXT-L PIC X(10).                              
240800     05  FILLER                  PIC X(41)   VALUE                        
240900         '^FS'.                                                           
241000   03    FILLER.                                                          
241100     05  FILLER                  PIC X(29)   VALUE                        
241200         '^FO0770,0740^AON,0050,0045^FD'.                                 
241300     05  RR-ZEBRA-KVROS          PIC Z(06).                               
241400     05  FILLER                  PIC X(45)   VALUE                        
241500         '^FS'.                                                           
241600   03    FILLER.                                                          
241700     05  FILLER                  PIC X(29)   VALUE                        
241800         '^FO0770,1040^AON,0050,0045^FD'.                                 
241900     05  RR-ZEBRA-FLKVAANT-TEXT  PIC X(03).                               
242000     05  FILLER                  PIC X(48)   VALUE                        
242100         '^FS'.                                                           
242200   03    FILLER.                                                          
242300     05  FILLER                  PIC X(29)   VALUE                        
242400         '^FO0320,1140^AON,0050,0045^FD'.                                 
242500     05  RR-ZEBRA-BEARTURS       PIC X(15).                               
242600     05  FILLER                  PIC X(36)   VALUE                        
242700         '^FS'.                                                           
242800   03    FILLER.                                                          
242900     05  FILLER                  PIC X(29)   VALUE                        
243000         '^FO0520,1140^AON,0050,0045^FD'.                                 
243100     05  RR-ZEBRA-KDFARLIG-TEXT-T PIC X(10).                              
243200     05  FILLER                  PIC X(41)   VALUE                        
243300         '^FS'.                                                           
243400   03    FILLER.                                                          
243500     05  FILLER                  PIC X(29)   VALUE                        
243600         '^FO0770,1140^AON,0050,0045^FD'.                                 
243700     05  RR-ZEBRA-KDKONTR-TEXT   PIC X(22).                               
243800     05  FILLER                  PIC X(29)   VALUE                        
243900         '^FS'.                                                           
244000   03    FILLER.                                                          
244100     05  FILLER                  PIC X(29)   VALUE                        
244200         '^FO1150,1140^AON,0050,0045^FD'.                                 
244300     05  RR-ZEBRA-KDFGPRIO       PIC Z(03).                               
244400     05  FILLER                  PIC X(48)   VALUE                        
244500         '^FS'.                                                           
244600   03    FILLER.                                                          
244700     05  FILLER                  PIC X(29)   VALUE                        
244800         '^FO0070,1240^AON,0050,0045^FD'.                                 
244900     05  RR-ZEBRA-BELEV1         PIC X(23).                               
245000     05  FILLER                  PIC X(28)   VALUE                        
245100         '^FS'.                                                           
245200   03    FILLER.                                                          
245300     05  FILLER                  PIC X(29)   VALUE                        
245400         '^FO0070,1300^AON,0050,0045^FD'.                                 
245500     05  RR-ZEBRA-BELEV2         PIC X(23).                               
245600     05  FILLER                  PIC X(28)   VALUE                        
245700         '^FS'.                                                           
245800   03    FILLER.                                                          
245900     05  FILLER                  PIC X(29)   VALUE                        
246000         '^FO0770,1240^AON,0050,0045^FD'.                                 
246100     05  RR-ZEBRA-ADINLOMR-NXT1  PIC X(04).                               
246200     05  FILLER                  PIC X(01)   VALUE SPACE.                 
246300     05  RR-ZEBRA-ADINLOMR-NXT2  PIC X(04).                               
246400     05  FILLER                  PIC X(01)   VALUE SPACE.                 
246500     05  RR-ZEBRA-ADINLOMR-NXT3  PIC X(04).                               
246600     05  FILLER                  PIC X(01)   VALUE SPACE.                 
246700     05  RR-ZEBRA-ADINLOMR-NXT4  PIC X(04).                               
246800     05  FILLER                  PIC X(01)   VALUE SPACE.                 
246900     05  RR-ZEBRA-ADINLOMR-NXT5  PIC X(04).                               
247000     05  FILLER                  PIC X(01)   VALUE SPACE.                 
247100     05  RR-ZEBRA-ADINLOMR-NXT6  PIC X(04).                               
247200     05  FILLER                  PIC X(22)   VALUE                        
247300         '^FS'.                                                           
247400   03    FILLER.                                                          
247500     05  FILLER                  PIC X(29)   VALUE                        
247600         '^FO1120,1240^AON,0050,0045^FD'.                                 
247700     05  RR-ZEBRA-TIINLMOT       PIC 9(06).                               
247800     05  FILLER                  PIC X(45)   VALUE                        
247900         '^FS'.                                                           
248000   03    FILLER.                                                          
248100     05  FILLER                  PIC X(29)   VALUE                        
248200         '^FO1320,1240^AON,0050,0045^FD'.                                 
248300     05  RR-ZEBRA-HH             PIC 9(02).                               
248400     05  FILLER                  PIC X(01)   VALUE '.'.                   
248500     05  RR-ZEBRA-MM             PIC 9(02).                               
248600     05  FILLER                  PIC X(01)   VALUE '.'.                   
248700     05  RR-ZEBRA-SS             PIC 9(02).                               
248800     05  FILLER                  PIC X(43)   VALUE                        
248900         '^FS'.                                                           
249000   03    FILLER                  PIC X(80)   VALUE                        
249100         '^by3,,'.                                                        
249200   03    FILLER.                                                          
249300     05  FILLER                  PIC X(34)   VALUE                        
249400         '^FO0070,0390^B3N,N,100,Y,N^FWN^FDP'.                            
249500     05  RR-ZEBRA-IDLOPNRM-STRK  PIC 9(08).                               
249600     05  FILLER                  PIC X(38)   VALUE                        
249700         '^FS'.                                                           
249800   03    FILLER                  PIC X(80)   VALUE                        
249900         '^XZ'.                                                           
250000 01  FILLER REDEFINES RR-TAB-ZEBRA.                                       
250100   03  FILLER OCCURS 102.                                                 
250200     05  RR-ZEBRA-RAD               PIC X(80).                            
250300     EJECT                                                                
250400 LINKAGE SECTION.                                                         
250500                                                                          
250600*01  -COPY W0009   -PRE MSG-                                              
250700     EJECT                                                                
250800*01  -COPY W0009   -PRE ALT-                                              
250900     EJECT                                                                
251000*01  -COPY W0008  -PRE LISB-                                              
251100     05  FILLER                  PIC X.                                   
251200     EJECT                                                                
251300*01  -COPY W0008  -PRE INLA-                                              
251400     05  FILLER                  PIC X.                                   
251500     EJECT                                                                
251600*01  -COPY W0008  -PRE INLC-                                              
251700     05  FILLER                  PIC X.                                   
251800     EJECT                                                                
251900*01  -COPY W0008  -PRE PLAA-                                              
252000     05  FILLER                  PIC X.                                   
252100     EJECT                                                                
252200*01  -COPY W0008  -PRE ARTC-                                              
252300     05  FILLER                  PIC X.                                   
252400     EJECT                                                                
252500*01  -COPY W0008  -PRE ARTD-                                              
252600     05  FILLER                  PIC X.                                   
252700     EJECT                                                                
252800*01  -COPY W0008  -PRE WDF5-                                              
252900     05  FILLER                  PIC X.                                   
253000     EJECT                                                                
253100*01  -COPY W0008  -PRE USEA-                                              
253200     05  FILLER                  PIC X.                                   
253300     EJECT                                                                
253400*01  -COPY W0008  -PRE WDK7-                                              
253500     05  FILLER                  PIC X.                                   
253600     EJECT                                                                
253700*01  -COPY W0008  -PRE WDD5-                                              
253800     05  FILLER                  PIC X.                                   
253900     EJECT                                                                
254000*   PCB'ER FÖR SUBPGM W611ADR                                             
254100 01  ADR-INLA-PCB                PIC X.                                   
254200                                                                          
254300 01  ADR-INLC-PCB                PIC X.                                   
254400                                                                          
254500 01  ADR-PLAA-PCB                PIC X.                                   
254600                                                                          
254700 01  ADR-WDK6-PCB                PIC X.                                   
254800                                                                          
254900 01  ADR-STYR-HANA-PCB           PIC X.                                   
255000                                                                          
255100 01  ADR-STYR-PLAA-PCB           PIC X.                                   
255200                                                                          
255300     EJECT                                                                
255400 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB LISB-PCB INLA-PCB              
255500       INLC-PCB PLAA-PCB ARTC-PCB ARTD-PCB WDF5-PCB USEA-PCB              
255600         WDK7-PCB WDD5-PCB                                                
255700         ADR-INLA-PCB ADR-INLC-PCB ADR-PLAA-PCB ADR-WDK6-PCB              
255800                ADR-STYR-HANA-PCB ADR-STYR-PLAA-PCB.                      
255900 MAIN SECTION.                                                            
256000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB LISB-PCB INLA-PCB              
256100       INLC-PCB PLAA-PCB ARTC-PCB ARTD-PCB WDF5-PCB USEA-PCB              
256200         WDK7-PCB WDD5-PCB                                                
256300         ADR-INLA-PCB ADR-INLC-PCB ADR-PLAA-PCB ADR-WDK6-PCB              
256400                ADR-STYR-HANA-PCB ADR-STYR-PLAA-PCB.                      
256500                                                                          
256600                                                                          
256700     PERFORM IMS-GET-MSG                                                  
256800     IF SEGMENT-FINNS                                                     
256900       PERFORM A-INIT                                                     
257000       IF SVARA-MOD                                                       
257100**         --- D.V.S. W-IDTRANS = '6109'                                  
257200           PERFORM B-KOLLA-INDATA                                         
257300       END-IF                                                             
257400       IF INDATA-OK AND NYCKLAR-OK                                        
257500           IF W-IDTRANS        =  '6109'                                  
257600               MOVE PRT-IDPRTLST TO WS-RAPP-PRINTER                       
257700               PERFORM S02-OPEN-PRINTER                                   
257800               PERFORM C-SKAPA-RAPPORT                                    
257900               PERFORM S03-CLOSE-PRINTER                                  
258000           ELSE                                                           
258100               MOVE +1                TO MID-IX                           
258200               PERFORM S01-KOLLA-PRINTER                                  
258300               MOVE PRT-IDPRTLST TO WS-RAPP-PRINTER                       
258400               PERFORM S02-OPEN-PRINTER                                   
258500               PERFORM UNTIL MID-IX   > MOD-MID-KVPOST                    
258600                   PERFORM C-SKAPA-RAPPORT                                
258700                   ADD +1             TO MID-IX                           
258800               END-PERFORM                                                
258900               PERFORM S03-CLOSE-PRINTER                                  
259000           END-IF                                                         
259100       END-IF                                                             
259200       IF SVARA-MOD                                                       
259300           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
259400           PERFORM IMS-INSERT-MSG                                         
259500       END-IF                                                             
259600     END-IF                                                               
259700                                                                          
259800     MOVE ZERO TO RETURN-CODE                                             
259900     GOBACK                                                               
260000     .                                                                    
260100     EJECT                                                                
260200 A-INIT SECTION.                                                          
260300                                                                          
260400     MOVE JA                   TO INDATA-SW                               
260500                                  NYCKLAR-SW                              
260600     IF MSG-DUBBLA-TRANSKODER                                             
260700       IF MSG-IDTRANS-2                   =  '6109'                       
260800           MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I19601             
260900        ELSE                                                              
261000           MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I19602             
261100       END-IF                                                             
261200       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
261300       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
261400     ELSE                                                                 
261500       IF MSG-IDTRANS-1                   =  '6109'                       
261600           MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W6I19601               
261700        ELSE                                                              
261800           MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W6I19602               
261900       END-IF                                                             
262000       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
262100       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
262200     END-IF                                                               
262300                                                                          
262400     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
262500     MOVE MSG-IDPFK            TO MFS-IDPFK                               
262600     MOVE MFS-IDTRANS          TO W-IDTRANS                               
262700                                                                          
262800     MOVE LOW-VALUE            TO MSG-AREA                                
262900     MOVE 'W6O10902'           TO MFS-IDMOD                               
263000     MOVE '6109'               TO MOD-IDTRANS                             
263100     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
263200                                                                          
263300     IF ENGLISH-TEXT                                                      
263400       MOVE 'GB '              TO MED-IDSKYLT                             
263500     ELSE                                                                 
263600       MOVE 'S  '              TO MED-IDSKYLT                             
263700     END-IF                                                               
263800*    MOVE 'W601Z1SE'       TO    PRT-IDVCOM                               
263900     MOVE 'W601FLAA'       TO    PRT-IDCPYTXT                             
264000     .                                                                    
264100     EJECT                                                                
264200 B-KOLLA-INDATA SECTION.                                                  
264300                                                                          
264400     PERFORM BA-KOLLA-IDLEVNR                                             
264500     PERFORM BB-KOLLA-IDOKOLLI                                            
264600     PERFORM BC-KOLLA-IDLOPNRM                                            
264700     PERFORM BD-KOLLA-ADINLOMR-PRT                                        
264800     PERFORM BE-KOLLA-IDDC                                                
264900                                                                          
265000     IF NYCKLAR-FEL                                                       
265100         MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                       
265200         CALL WMEDKONV USING MED-WMEDAREA                                 
265300         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
265400         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-IN-ATTR                
265500                                       MOD-IDOKOLLI-IN-ATTR               
265600                                       MOD-IDLOPNRM-IN-ATTR               
265700         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDLEVNR-IN                     
265800                                       MOD-IDOKOLLI-IN                    
265900                                       MOD-IDLOPNRM-IN                    
266000     END-IF                                                               
266100                                                                          
266200     IF INDATA-FEL                                                        
266300         MOVE '772'              TO MED-IDMFSFEL                          
266400         CALL WMEDKONV USING MED-WMEDAREA                                 
266500         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
266600         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-ATTR                 
266700     END-IF                                                               
266800     .                                                                    
266900     EJECT                                                                
267000 BA-KOLLA-IDLEVNR SECTION.                                                
267100                                                                          
267200     IF MOD6109-MID-IDLEVNR-IN  = ALL '+'                                 
267300         MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-IN                          
267400     ELSE                                                                 
267500         MOVE NEJ              TO NYCKLAR-SW                              
267600     END-IF                                                               
267700     .                                                                    
267800     EJECT                                                                
267900 BB-KOLLA-IDOKOLLI SECTION.                                               
268000                                                                          
268100     IF MOD6109-MID-IDOKOLLI-IN  = ALL '+'                                
268200         MOVE MFS-RENSA-FAELT  TO MOD-IDOKOLLI-IN                         
268300     ELSE                                                                 
268400         MOVE NEJ              TO NYCKLAR-SW                              
268500     END-IF                                                               
268600     .                                                                    
268700     EJECT                                                                
268800 BC-KOLLA-IDLOPNRM SECTION.                                               
268900                                                                          
269000     IF  MOD6109-MID-IDLOPNRM-IN      = ALL '+'                           
269100     AND MOD6109-MID-IDLOPNRM-UT  NOT = SPACE                             
269200                                                                          
269300         MOVE MFS-RENSA-FAELT     TO MOD-IDLOPNRM-IN                      
269400     ELSE                                                                 
269500         MOVE NEJ                 TO NYCKLAR-SW                           
269600     END-IF                                                               
269700     .                                                                    
269800     EJECT                                                                
269900 BD-KOLLA-ADINLOMR-PRT SECTION.                                           
270000                                                                          
270100     IF MOD6109-MID-ADINLOMR-PRT         NOT = ALL '+'                    
270200         MOVE MOD6109-MID-ADINLOMR-PRT   TO MOD-ADINLOMR-PRT              
270300         MOVE SPACE                      TO PRT-IDPRTLST                  
270400         MOVE '6L'                       TO PRT-IDPRTLST(1:2)             
270500         MOVE MOD6109-MID-ADINLOMR-PRT   TO PRT-IDPRTLST(3:6)             
270600         MOVE 1                          TO PRT-KDCALL                    
270700         CALL W006PRT  USING PRT-W006PRT                                  
270800         IF PRT-KDSVAR                   = 'F'                            
270900           MOVE SPACE                      TO PRT-IDPRTLST                
271000           MOVE '6F'                       TO PRT-IDPRTLST(1:2)           
271100           MOVE MOD6109-MID-ADINLOMR-PRT   TO PRT-IDPRTLST(3:6)           
271200           MOVE 1                          TO PRT-KDCALL                  
271300           CALL W006PRT  USING PRT-W006PRT                                
271400           IF PRT-KDSVAR                   = 'F'                          
271500             MOVE NEJ                    TO INDATA-SW                     
271600             MOVE MFS-ALFA-FAELT-FEL     TO MOD-ADINLOMR-PRT-ATTR         
271700           ELSE                                                           
271800             MOVE MFS-ALFA-FAELT-RAETT   TO MOD-ADINLOMR-PRT-ATTR         
271900           END-IF                                                         
272000         ELSE                                                             
272100             MOVE JA                     TO LASER-SW                      
272200             MOVE MFS-ALFA-FAELT-RAETT   TO MOD-ADINLOMR-PRT-ATTR         
272300         END-IF                                                           
272400     ELSE                                                                 
272500         MOVE MFS-ALFA-FAELT-FEL         TO MOD-ADINLOMR-PRT-ATTR         
272600         MOVE MFS-RENSA-FAELT            TO MOD-ADINLOMR-PRT              
272700         MOVE NEJ                        TO INDATA-SW                     
272800     END-IF                                                               
272900     .                                                                    
273000     EJECT                                                                
273100 BE-KOLLA-IDDC     SECTION.                                               
273200                                                                          
273300     MOVE MOD6109-MID-IDDC-UT     TO SW-WS-IDDC                           
273400     IF  MOD6109-MID-IDDC-IN      = ALL '+' AND SW-CDC OR SW-NDC          
273500                                                                          
273600         MOVE MFS-RENSA-FAELT     TO MOD-IDDC-IN                          
273700     ELSE                                                                 
273800         MOVE NEJ                 TO NYCKLAR-SW                           
273900     END-IF                                                               
274000     .                                                                    
274100     EJECT                                                                
274200 C-SKAPA-RAPPORT  SECTION.                                                
274300                                                                          
274400     IF W-IDTRANS  = '6109'                                               
274500         PERFORM CB-FLYTTA-6109-MID                                       
274600      ELSE                                                                
274700         PERFORM CC-FLYTTA-ANNAN-MID                                      
274800     END-IF                                                               
274900                                                                          
275000     PERFORM IMS-GU-INLC-INLC01                                           
275100     IF SEGMENT-FINNS                                                     
275200         MOVE SEQB-IDDC         TO W-D101KY-IDDC                          
275300                                   W-6005-IDDC                            
275400                                   WS-IDDC                                
275500                                   W-IDDC                                 
275600         MOVE SEQB-IDLEVNR      TO W-D101KY-IDLEVNR                       
275700                                   W-IDLEVNR                              
275800         MOVE SEQB-IDFS         TO W-D101KY-IDFS                          
275900         MOVE SEQB-TIAVIDAT     TO W-D101KY-TIAVIDAT                      
276000         MOVE SEQB-IDRADNR-INL  TO W-IDRADNR-INL                          
276100                                                                          
276200         PERFORM IMS-GU-INLA-INLA01                                       
276300*------------------------------------------------------                   
276400*** ÄT 96021 - ARBETSRAPPORT FÅR EJ SKRIVAS UT OM GODSET INTE             
276500** ÄR MOTTAGET. ÄNDRAT NOVEMBER -97 /KENT JEBSEN                          
276600         IF INL-TIINLMOT > 0 OR W-IDTRANS NOT = '6109'                    
276700*------------------------------------------------------                   
276800            MOVE INL-IDFS          TO NOVA-TAB-IDFS                       
276900            MOVE INL-IDFS          TO AR-TAB-IDFS                         
277000                                      AR-TAB-IDFS-VCOM                    
277100                                      AR-LASER-IDFS                       
277200                                      OLD-LASER-IDFS                      
277300                                      RR-LASER-IDFS                       
277400                                      RR-ZEBRA-IDFS                       
277500            MOVE INL-TIAVIDAT      TO NOVA-TAB-TIAVIDAT                   
277600            MOVE INL-TIAVIDAT      TO AR-TAB-TIAVIDAT                     
277700                                      AR-TAB-TIAVIDAT-VCOM                
277800                                      AR-LASER-TIAVIDAT                   
277900                                      OLD-LASER-TIAVIDAT                  
278000                                      RR-LASER-TIAVIDAT                   
278100                                      RR-ZEBRA-TIAVIDAT                   
278200            MOVE INL-IDLBBET       TO NOVA-TAB-IDLBBET                    
278300            MOVE INL-IDLBBET       TO AR-TAB-IDLBBET                      
278400                                      AR-TAB-IDLBBET-VCOM                 
278500                                      AR-LASER-IDLBBET                    
278600                                      OLD-LASER-IDLBBET                   
278700                                      RR-LASER-IDLBBET                    
278800                                      RR-ZEBRA-IDLBBET                    
278900            MOVE INL-IDLEVNR       TO NOVA-TAB-IDLEVNR                    
279000            MOVE INL-IDLEVNR       TO AR-TAB-IDLEVNR                      
279100                                      AR-TAB-IDLEVNR-VCOM                 
279200                                      AR-LASER-IDLEVNR                    
279300                                      OLD-LASER-IDLEVNR                   
279400                                      RR-LASER-IDLEVNR                    
279500                                      RR-ZEBRA-IDLEVNR                    
279600            MOVE INL-TIINLMOT      TO NOVA-TAB-TIINLMOT                   
279700            MOVE INL-TIINLMOT      TO AR-TAB-TIINLMOT                     
279800                                      AR-TAB-TIINLMOT-VCOM                
279900                                      AR-LASER-TIINLMOT                   
280000                                      OLD-LASER-TIINLMOT                  
280100                                      RR-LASER-TIINLMOT                   
280200                                      RR-ZEBRA-TIINLMOT                   
280300                                                                          
280400                                                                          
280500            PERFORM IMS-GNP-INLA-INLA11                                   
280600                                                                          
280700            PERFORM CD-FLYTTA-INLA11-INFO                                 
280800                                                                          
280900            PERFORM CE-HAEMTA-ADRESSER                                    
281000                                                                          
281100            PERFORM CF-FLYTTA-ARTIKEL-INFO                                
281200                                                                          
281300            PERFORM CH-HAEMTA-KDFGPRIO                                    
281400                                                                          
281500            MOVE SPACE            TO W-6006-ADINLOMR                      
281600            MOVE W-ADLAGOMR       TO W-6006-ADINLOMR                      
281700            PERFORM IMS-GU-PLAA-PLAA11                                    
281800            IF SEGMENT-FINNS                                              
281900              IF PLAA-6006-KDLORAPP = 2                                   
282000                MOVE 'JA '        TO NOVA-TAB-KDLORAPP-TEXT               
282100                MOVE 'JA '        TO AR-TAB-KDLORAPP-TEXT                 
282200                                 AR-TAB-KDLORAPP-TEXT-VCOM                
282300                                 AR-LASER-KDLORAPP-TEXT                   
282400                                 OLD-LASER-KDLORAPP-TEXT                  
282500                MOVE 'YES'        TO RR-LASER-KDLORAPP-TEXT               
282600                                 RR-ZEBRA-KDLORAPP-TEXT                   
282700              ELSE                                                        
282800                MOVE 'NEJ'        TO NOVA-TAB-KDLORAPP-TEXT               
282900                MOVE 'NEJ'        TO AR-TAB-KDLORAPP-TEXT                 
283000                                 AR-TAB-KDLORAPP-TEXT-VCOM                
283100                                 AR-LASER-KDLORAPP-TEXT                   
283200                                 OLD-LASER-KDLORAPP-TEXT                  
283300                MOVE 'NO '        TO RR-LASER-KDLORAPP-TEXT               
283400                                 RR-ZEBRA-KDLORAPP-TEXT                   
283500              END-IF                                                      
283600            END-IF                                                        
283700                                                                          
283800            MOVE ALL '+'         TO MSGI-WMSGINIT                         
283900            MOVE '011'           TO MSGI-KDCALL                           
284000            MOVE WS-IDDC         TO WS-IDDC-LOCAL-DATE                    
284100                                                                          
284200            MOVE WS-IDDC-LOCAL   TO MSGI-IDUSER                           
284300            MOVE FUNCTION CURRENT-DATE(9:4) TO MSGI-TILOKTID              
284400            MOVE RR-ZEBRA-TIINLMOT          TO MSGI-TILOKDAT              
284500                                                                          
284600            CALL W005INIT  USING  MSGI-WMSGINIT USEA-PCB                  
284700                                                                          
284800            MOVE MSGI-TILOKTID(1:2)  TO NOVA-TAB-HH                       
284900            MOVE MSGI-TILOKTID(1:2)  TO AR-TAB-HH                         
285000                                        AR-TAB-HH-VCOM                    
285100                                        AR-LASER-HH                       
285200                                        OLD-LASER-HH                      
285300                                        RR-LASER-HH                       
285400                                        RR-ZEBRA-HH                       
285500            MOVE MSGI-TILOKTID(3:2)  TO NOVA-TAB-MM                       
285600            MOVE MSGI-TILOKTID(3:2)  TO AR-TAB-MM                         
285700                                        AR-TAB-MM-VCOM                    
285800                                        AR-LASER-MM                       
285900                                        OLD-LASER-MM                      
286000                                        RR-LASER-MM                       
286100                                        RR-ZEBRA-MM                       
286200            MOVE FUNCTION CURRENT-DATE(13:2) TO                           
286300                                      NOVA-TAB-SS                         
286400                                        AR-TAB-SS                         
286500                                        AR-TAB-SS-VCOM                    
286600                                        AR-LASER-SS                       
286700                                        OLD-LASER-SS                      
286800                                        RR-LASER-SS                       
286900                                        RR-ZEBRA-SS                       
287000            MOVE MSGI-TILOKDAT     TO NOVA-TAB-TIINLMOT                   
287100            MOVE MSGI-TILOKDAT     TO AR-TAB-TIINLMOT                     
287200                                      AR-TAB-TIINLMOT-VCOM                
287300                                      AR-LASER-TIINLMOT                   
287400                                      OLD-LASER-TIINLMOT                  
287500                                      RR-LASER-TIINLMOT                   
287600                                      RR-ZEBRA-TIINLMOT                   
287700                                                                          
287800                                                                          
287900            PERFORM CG-SKRIV-RAPPORT                                      
288000                                                                          
288100            MOVE '118'            TO MED-IDMFSINF                         
288200            CALL WMEDKONV USING MED-WMEDAREA                              
288300            MOVE MED-MFSINF       TO MOD-TEMFSINF                         
288400            MOVE PRT-BEPRTLST     TO MOD-TEMFSFEL                         
288500*-----------------------------------------------------------              
288600         ELSE                                                             
288700            MOVE 'PARTIET EJ MOTTAGET' TO MOD-TEMFSFEL                    
288800         END-IF                                                           
288900*-----------------------------------------------------------              
289000     ELSE                                                                 
289100         MOVE '010'            TO MED-IDMFSFEL                            
289200         CALL WMEDKONV USING MED-WMEDAREA                                 
289300         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
289400     END-IF                                                               
289500     .                                                                    
289600     EJECT                                                                
289700 CB-FLYTTA-6109-MID   SECTION.                                            
289800                                                                          
289900     MOVE MOD6109-MID-IDDC-UT      TO WS-IDDC                             
290000     INSPECT                                                              
290100          MOD6109-MID-IDLOPNRM-UT REPLACING LEADING SPACE BY ZERO         
290200     MOVE MOD6109-MID-IDLOPNRM-UT  TO W-IDLOPNRM                          
290300     .                                                                    
290400     EJECT                                                                
290500 CC-FLYTTA-ANNAN-MID   SECTION.                                           
290600                                                                          
290700     MOVE MOD-MID-IDDC             TO WS-IDDC                             
290800                                                                          
290900     MOVE MOD-MID-IDLOPNRM(MID-IX) TO W-IDLOPNRM                          
291000     .                                                                    
291100     EJECT                                                                
291200 CD-FLYTTA-INLA11-INFO  SECTION.                                          
291300                                                                          
291400     MOVE ART-ADLAGOMR         TO NOVA-TAB-ADLAGOMR                       
291500     MOVE ART-ADLAGOMR         TO AR-TAB-ADLAGOMR                         
291600                                  AR-TAB-ADLAGOMR-VCOM                    
291700                                  AR-LASER-ADLAGOMR                       
291800                                  OLD-LASER-ADLAGOMR                      
291900                                  RR-LASER-ADLAGOMR                       
292000                                  RR-ZEBRA-ADLAGOMR                       
292100     MOVE ART-ADGANG           TO NOVA-TAB-ADGANG                         
292200     MOVE ART-ADGANG           TO AR-TAB-ADGANG                           
292300                                  AR-TAB-ADGANG-VCOM                      
292400                                  AR-LASER-ADGANG                         
292500                                  OLD-LASER-ADGANG                        
292600                                  RR-LASER-ADGANG                         
292700                                  RR-ZEBRA-ADGANG                         
292800     MOVE ART-ADPLATS          TO NOVA-TAB-ADPLATS                        
292900     MOVE ART-ADPLATS          TO AR-TAB-ADPLATS                          
293000                                  AR-TAB-ADPLATS-VCOM                     
293100                                  AR-LASER-ADPLATS                        
293200                                  OLD-LASER-ADPLATS                       
293300                                  RR-LASER-ADPLATS                        
293400                                  RR-ZEBRA-ADPLATS                        
293500     MOVE ART-BEART            TO NOVA-TAB-BEART                          
293600     MOVE ART-BEART            TO AR-TAB-BEART                            
293700                                  AR-TAB-BEART-VCOM                       
293800                                  AR-LASER-BEART                          
293900                                  OLD-LASER-BEART                         
294000                                  RR-LASER-BEART                          
294100                                  RR-ZEBRA-BEART                          
294200     MOVE ART-BEFT             TO NOVA-TAB-BEFT                           
294300     MOVE ART-BEFT             TO AR-TAB-BEFT                             
294400                                  AR-TAB-BEFT-VCOM                        
294500                                  AR-LASER-BEFT                           
294600                                  OLD-LASER-BEFT                          
294700                                  RR-LASER-BEFT                           
294800                                  RR-ZEBRA-BEFT                           
294900     IF ART-KDKVAANT           > ZERO                                     
295000       MOVE 'JA '              TO NOVA-TAB-FLKVAANT-TEXT                  
295100       MOVE 'JA '              TO AR-TAB-FLKVAANT-TEXT                    
295200                                  AR-TAB-FLKVAANT-TEXT-VCOM               
295300                                  AR-LASER-FLKVAANT-TEXT                  
295400                                  OLD-LASER-FLKVAANT-TEXT                 
295500       MOVE 'YES'              TO RR-LASER-FLKVAANT-TEXT                  
295600                                  RR-ZEBRA-FLKVAANT-TEXT                  
295700     ELSE                                                                 
295800       MOVE 'NEJ'              TO NOVA-TAB-FLKVAANT-TEXT                  
295900       MOVE 'NEJ'              TO AR-TAB-FLKVAANT-TEXT                    
296000                                  AR-TAB-FLKVAANT-TEXT-VCOM               
296100                                  AR-LASER-FLKVAANT-TEXT                  
296200                                  OLD-LASER-FLKVAANT-TEXT                 
296300       MOVE 'NO '              TO RR-LASER-FLKVAANT-TEXT                  
296400                                  RR-ZEBRA-FLKVAANT-TEXT                  
296500     END-IF                                                               
296600     MOVE ART-IDARTNR          TO NOVA-TAB-IDARTNR                        
296700     MOVE ART-IDARTNR          TO AR-TAB-IDARTNR                          
296800                                  AR-TAB-IDARTNR-VCOM                     
296900                                  AR-LASER-IDARTNR                        
297000                                  OLD-LASER-IDARTNR                       
297100                                  RR-LASER-IDARTNR                        
297200                                  RR-ZEBRA-IDARTNR                        
297300                                  W-IDARTNR                               
297400     MOVE ART-IDLOPNRM         TO NOVA-TAB-IDLOPNRM                       
297500     MOVE ART-IDLOPNRM         TO NOVA-TAB-IDLOPNRM-2                     
297600     MOVE ART-IDLOPNRM         TO NOVA-TAB-IDLOPNRM-STRK                  
297700     MOVE ART-IDLOPNRM         TO AR-TAB-IDLOPNRM                         
297800                                  AR-TAB-IDLOPNRM-VCOM                    
297900                                  AR-TAB-IDLOPNRM-2                       
298000                                  AR-TAB-IDLOPNRM-STRK                    
298100                                  AR-LASER-IDLOPNRM-1                     
298200                                  OLD-LASER-IDLOPNRM-1                    
298300                                  AR-LASER-IDLOPNRM-2                     
298400                                  OLD-LASER-IDLOPNRM-2                    
298500                                  RR-LASER-IDLOPNRM-2                     
298600                                  RR-ZEBRA-IDLOPNRM                       
298700                                  AR-LASER-IDLOPNRM-STRK                  
298800                                  OLD-LASER-IDLOPNRM-STRK                 
298900                                  RR-LASER-IDLOPNRM-STRK                  
299000                                  RR-ZEBRA-IDLOPNRM-STRK                  
299100     MOVE ART-KDLAGEMB         TO NOVA-TAB-KDLAGEMB                       
299200     MOVE ART-KDLAGEMB         TO AR-TAB-KDLAGEMB                         
299300                                  AR-TAB-KDLAGEMB-VCOM                    
299400                                  AR-LASER-KDLAGEMB                       
299500                                  OLD-LASER-KDLAGEMB                      
299600                                  RR-LASER-KDLAGEMB                       
299700                                  RR-ZEBRA-KDLAGEMB                       
299800     MOVE ART-KDRT             TO NOVA-TAB-KDRT                           
299900     MOVE ART-KDRT             TO AR-TAB-KDRT                             
300000                                  AR-TAB-KDRT-VCOM                        
300100                                  AR-LASER-KDRT                           
300200                                  OLD-LASER-KDRT                          
300300                                  RR-LASER-KDRT                           
300400                                  RR-ZEBRA-KDRT                           
300500     MOVE ART-KDSORT           TO NOVA-TAB-KDSORT                         
300600     MOVE ART-KDSORT           TO AR-TAB-KDSORT                           
300700                                  AR-TAB-KDSORT-VCOM                      
300800                                  AR-LASER-KDSORT                         
300900                                  OLD-LASER-KDSORT                        
301000                                  RR-LASER-KDSORT                         
301100                                  RR-ZEBRA-KDSORT                         
301200     MOVE ART-KVAVIS           TO NOVA-TAB-KVAVIS                         
301300     MOVE ART-KVAVIS           TO AR-TAB-KVAVIS                           
301400                                  AR-TAB-KVAVIS-VCOM                      
301500                                  AR-LASER-KVAVIS                         
301600                                  OLD-LASER-KVAVIS                        
301700                                  RR-LASER-KVAVIS                         
301800                                  RR-ZEBRA-KVAVIS                         
301900     MOVE ART-KVAVIS-KIT       TO NOVA-TAB-KVAVIS-KIT                     
302000     MOVE ART-KVAVIS-KIT       TO AR-TAB-KVAVIS-KIT                       
302100                                  AR-TAB-KVAVIS-KIT-VCOM                  
302200                                  AR-LASER-KVAVIS-KIT                     
302300                                  OLD-LASER-KVAVIS-KIT                    
302400                                  RR-LASER-KVAVIS-KIT                     
302500                                  RR-ZEBRA-KVAVIS-KIT                     
302600     MOVE ART-ADTRDEST-KIT     TO NOVA-TAB-ADTRDEST-KIT                   
302700     MOVE ART-ADTRDEST-KIT     TO AR-TAB-ADTRDEST-KIT                     
302800                                  AR-TAB-ADTRDEST-KIT-VCOM                
302900                                  AR-LASER-ADTRDEST-KIT                   
303000                                  OLD-LASER-ADTRDEST-KIT                  
303100     MOVE ART-VKART            TO NOVA-TAB-VKART                          
303200     MOVE ART-VKART            TO AR-TAB-VKART                            
303300                                  AR-TAB-VKART-VCOM                       
303400                                  AR-LASER-VKART                          
303500                                  OLD-LASER-VKART                         
303600                                  RR-LASER-VKART                          
303700     COMPUTE RR-ZEBRA-VKART = ART-VKART * CONV-GR-TO-OZ                   
303800                                                                          
303900     MOVE ART-VLARTNTO         TO NOVA-TAB-VLARTNTO                       
304000     MOVE ART-VLARTNTO         TO AR-TAB-VLARTNTO                         
304100                                  AR-TAB-VLARTNTO-VCOM                    
304200                                  AR-LASER-VLARTNTO                       
304300                                  OLD-LASER-VLARTNTO                      
304400                                  RR-LASER-VLARTNTO                       
304500     COMPUTE RR-ZEBRA-VLARTNTO = ART-VLARTNTO * CONV-CM3-TO-IN3           
304600     MOVE ART-KVAVIS-PRIO      TO NOVA-TAB-KVAVIS-PRIO                    
304700     MOVE ART-KVAVIS-PRIO      TO AR-TAB-KVAVIS-PRIO                      
304800                                  AR-TAB-KVAVIS-PRIO-VCOM                 
304900                                  AR-LASER-KVAVIS-PRIO                    
305000                                  OLD-LASER-KVAVIS-PRIO                   
305100                                  RR-LASER-KVAVIS-PRIO                    
305200                                  RR-ZEBRA-KVAVIS-PRIO                    
305300     MOVE ART-KVKVAPRIM-BER    TO NOVA-TAB-KVKVAPRIM-BER                  
305400     MOVE ART-KVKVAPRIM-BER    TO AR-TAB-KVKVAPRIM-BER                    
305500                                  AR-TAB-KVKVAPRIM-BER-VCOM               
305600                                  AR-LASER-KVKVAPRIM-BER                  
305700                                  OLD-LASER-KVKVAPRIM-BER                 
305800                                  RR-LASER-KVKVAPRIM-BER                  
305900                                  RR-ZEBRA-KVKVAPRIM-BER                  
306000     MOVE ART-KVKVASEK-BER     TO NOVA-TAB-KVKVASEK-BER                   
306100     MOVE ART-KVKVASEK-BER     TO AR-TAB-KVKVASEK-BER                     
306200                                  AR-TAB-KVKVASEK-BER-VCOM                
306300                                  AR-LASER-KVKVASEK-BER                   
306400                                  OLD-LASER-KVKVASEK-BER                  
306500                                  RR-LASER-KVKVASEK-BER                   
306600                                                                          
306700     MOVE SPACE                TO NOVA-TAB-KDFARLIG-TEXT-T                
306800     MOVE SPACE                TO AR-TAB-KDFARLIG-TEXT-T                  
306900                                  AR-TAB-KDFARLIG-TEXT-T-VCOM             
307000                                  AR-LASER-KDFARLIG-TEXT-T                
307100                                  OLD-LASER-KDFARLIG-TEXT-T               
307200                                  RR-LASER-KDFARLIG-TEXT-T                
307300                                  RR-ZEBRA-KDFARLIG-TEXT-T                
307400                                  AR-TAB-KDFARLIG-TEXT-L                  
307500                                  AR-TAB-KDFARLIG-TEXT-L-VCOM             
307600                                  AR-LASER-KDFARLIG-TEXT-L                
307700                                  OLD-LASER-KDFARLIG-TEXT-L               
307800                                  RR-LASER-KDFARLIG-TEXT-L                
307900                                  RR-ZEBRA-KDFARLIG-TEXT-L                
308000     MOVE ART-KDKVAINL         TO NOVA-TAB-KDKVAINL                       
308100     MOVE ART-KDKVAINL         TO AR-TAB-KDKVAINL                         
308200                                  AR-TAB-KDKVAINL-VCOM                    
308300                                  AR-LASER-KDKVAINL                       
308400                                  OLD-LASER-KDKVAINL                      
308500     MOVE ART-IDDC         TO SW-WS-IDDC                                  
308600     EVALUATE ART-KDFARLIG                                                
308700       WHEN 4                                                             
308800         IF SW-CDC-SE                                                     
308900           MOVE JA               TO NOVA-TAB-KDFARLIG-TEXT-T              
309000           MOVE JA               TO AR-TAB-KDFARLIG-TEXT-T                
309100                                    AR-TAB-KDFARLIG-TEXT-T-VCOM           
309200                                    AR-LASER-KDFARLIG-TEXT-T              
309300                                    OLD-LASER-KDFARLIG-TEXT-T             
309400         ELSE                                                             
309500           MOVE 'YES'            TO NOVA-TAB-KDFARLIG-TEXT-T              
309600           MOVE 'YES'            TO AR-TAB-KDFARLIG-TEXT-T                
309700                                    AR-TAB-KDFARLIG-TEXT-T-VCOM           
309800                                    RR-LASER-KDFARLIG-TEXT-T              
309900                                    RR-ZEBRA-KDFARLIG-TEXT-T              
310000         END-IF                                                           
310100       WHEN 5                                                             
310200         IF SW-CDC-SE                                                     
310300           MOVE 'ASBEST'         TO NOVA-TAB-KDFARLIG-TEXT-L              
310400           MOVE 'ASBEST'         TO AR-TAB-KDFARLIG-TEXT-L                
310500                                    AR-TAB-KDFARLIG-TEXT-L-VCOM           
310600                                    AR-LASER-KDFARLIG-TEXT-L              
310700                                    OLD-LASER-KDFARLIG-TEXT-L             
310800         ELSE                                                             
310900           MOVE 'ASBEST'         TO NOVA-TAB-KDFARLIG-TEXT-L              
311000           MOVE 'ASBEST'         TO AR-TAB-KDFARLIG-TEXT-L                
311100                                    AR-TAB-KDFARLIG-TEXT-L-VCOM           
311200                                    RR-LASER-KDFARLIG-TEXT-L              
311300                                    RR-ZEBRA-KDFARLIG-TEXT-L              
311400         END-IF                                                           
311500       WHEN 6                                                             
311600         IF SW-CDC-SE                                                     
311700           MOVE 'KEMIKALIER'     TO NOVA-TAB-KDFARLIG-TEXT-L              
311800           MOVE 'KEMIKALIER'     TO AR-TAB-KDFARLIG-TEXT-L                
311900                                    AR-TAB-KDFARLIG-TEXT-L-VCOM           
312000                                    AR-LASER-KDFARLIG-TEXT-L              
312100                                    OLD-LASER-KDFARLIG-TEXT-L             
312200         ELSE                                                             
312300           MOVE 'CHEMICALS '     TO NOVA-TAB-KDFARLIG-TEXT-L              
312400           MOVE 'CHEMICALS '     TO AR-TAB-KDFARLIG-TEXT-L                
312500                                    AR-TAB-KDFARLIG-TEXT-L-VCOM           
312600                                    RR-LASER-KDFARLIG-TEXT-L              
312700                                    RR-ZEBRA-KDFARLIG-TEXT-L              
312800         END-IF                                                           
312900       WHEN 7                                                             
313000         IF SW-CDC-SE                                                     
313100           MOVE JA               TO NOVA-TAB-KDFARLIG-TEXT-T              
313200           MOVE JA               TO AR-TAB-KDFARLIG-TEXT-T                
313300                                    AR-TAB-KDFARLIG-TEXT-T-VCOM           
313400                                    AR-LASER-KDFARLIG-TEXT-T              
313500                                    OLD-LASER-KDFARLIG-TEXT-T             
313600         ELSE                                                             
313700           MOVE 'YES'            TO NOVA-TAB-KDFARLIG-TEXT-T              
313800           MOVE 'YES'            TO AR-TAB-KDFARLIG-TEXT-T                
313900                                    AR-TAB-KDFARLIG-TEXT-T-VCOM           
314000                                    RR-LASER-KDFARLIG-TEXT-T              
314100                                    RR-ZEBRA-KDFARLIG-TEXT-T              
314200         END-IF                                                           
314300     END-EVALUATE                                                         
314400                                                                          
314500     IF (ART-KVKVAPRIM-BER     > +0 OR                                    
314600         ART-KVKVASEK-BER      > +0)                                      
314700       IF ART-FLKVAKAR = JA                                               
314800         MOVE 'KARANTÄN              ' TO NOVA-TAB-KDKONTR-TEXT           
314900         MOVE 'KARANTÄN              ' TO AR-TAB-KDKONTR-TEXT             
315000                                          AR-TAB-KDKONTR-TEXT-VCOM        
315100                                          AR-LASER-KDKONTR-TEXT           
315200                                          OLD-LASER-KDKONTR-TEXT          
315300         MOVE 'QUARANTINE            ' TO RR-LASER-KDKONTR-TEXT           
315400                                          RR-ZEBRA-KDKONTR-TEXT           
315500       ELSE                                                               
315600         IF LASER                                                         
315700           MOVE WS-IDDC        TO SW-WS-IDDC                              
315800           IF SW-CDC-SE                                                   
315900             MOVE 'KONTROLL, SE 6139' TO OLD-LASER-KDKONTR-TEXT           
316000           ELSE                                                           
316100           MOVE 'KONTROLL, SE BILD 6139' TO NOVA-TAB-KDKONTR-TEXT         
316200           MOVE 'KONTROLL, SE BILD 6139' TO AR-TAB-KDKONTR-TEXT           
316300                                        AR-TAB-KDKONTR-TEXT-VCOM          
316400                                            AR-LASER-KDKONTR-TEXT         
316500                                            OLD-LASER-KDKONTR-TEXT        
316600           MOVE 'CONTR, SEE SCREEN 6139' TO RR-LASER-KDKONTR-TEXT         
316700                                            RR-ZEBRA-KDKONTR-TEXT         
316800           END-IF                                                         
316900         END-IF                                                           
317000       END-IF                                                             
317100     ELSE                                                                 
317200         MOVE SPACE                    TO NOVA-TAB-KDKONTR-TEXT           
317300         MOVE SPACE                    TO AR-TAB-KDKONTR-TEXT             
317400                                          AR-TAB-KDKONTR-TEXT-VCOM        
317500                                          AR-LASER-KDKONTR-TEXT           
317600                                          OLD-LASER-KDKONTR-TEXT          
317700                                          RR-LASER-KDKONTR-TEXT           
317800                                          RR-ZEBRA-KDKONTR-TEXT           
317900     END-IF                                                               
318000     .                                                                    
318100     EJECT                                                                
318200 CE-HAEMTA-ADRESSER      SECTION.                                         
318300                                                                          
318400     IF ART-IDLOPNRM           >  ZERO                                    
318500         MOVE ART-IDLOPNRM     TO ADR-IDLOPNRM                            
318600                                                                          
318700         CALL W611ADR USING ADR-W611ADR ADR-INLA-PCB ADR-INLC-PCB         
318800                                        ADR-PLAA-PCB ADR-WDK6-PCB         
318900                                        ADR-STYR-HANA-PCB                 
319000                                        ADR-STYR-PLAA-PCB                 
319100                                                                          
319200         MOVE ADR-ADINLOMR-NXT1 TO NOVA-TAB-ADINLOMR-NXT1                 
319300         MOVE ADR-ADINLOMR-NXT1 TO AR-TAB-ADINLOMR-NXT1                   
319400                                   AR-TAB-ADINLOMR-NXT1-VCOM              
319500                                   AR-LASER-ADINLOMR-NXT1                 
319600                                   OLD-LASER-ADINLOMR-NXT1                
319700                                   RR-LASER-ADINLOMR-NXT1                 
319800                                   RR-ZEBRA-ADINLOMR-NXT1                 
319900         MOVE ADR-ADINLOMR-NXT2 TO NOVA-TAB-ADINLOMR-NXT2                 
320000         MOVE ADR-ADINLOMR-NXT2 TO AR-TAB-ADINLOMR-NXT2                   
320100                                   AR-TAB-ADINLOMR-NXT2-VCOM              
320200                                   AR-LASER-ADINLOMR-NXT2                 
320300                                   OLD-LASER-ADINLOMR-NXT2                
320400                                   RR-LASER-ADINLOMR-NXT2                 
320500                                   RR-ZEBRA-ADINLOMR-NXT2                 
320600         MOVE ADR-ADINLOMR-NXT3 TO NOVA-TAB-ADINLOMR-NXT3                 
320700         MOVE ADR-ADINLOMR-NXT3 TO AR-TAB-ADINLOMR-NXT3                   
320800                                   AR-TAB-ADINLOMR-NXT3-VCOM              
320900                                   AR-LASER-ADINLOMR-NXT3                 
321000                                   OLD-LASER-ADINLOMR-NXT3                
321100                                   RR-LASER-ADINLOMR-NXT3                 
321200                                   RR-ZEBRA-ADINLOMR-NXT3                 
321300         MOVE ADR-ADINLOMR-NXT4 TO NOVA-TAB-ADINLOMR-NXT4                 
321400         MOVE ADR-ADINLOMR-NXT4 TO AR-TAB-ADINLOMR-NXT4                   
321500                                   AR-TAB-ADINLOMR-NXT4-VCOM              
321600                                   AR-LASER-ADINLOMR-NXT4                 
321700                                   OLD-LASER-ADINLOMR-NXT4                
321800                                   RR-LASER-ADINLOMR-NXT4                 
321900                                   RR-ZEBRA-ADINLOMR-NXT4                 
322000         MOVE ADR-ADINLOMR-NXT5 TO NOVA-TAB-ADINLOMR-NXT5                 
322100         MOVE ADR-ADINLOMR-NXT5 TO AR-TAB-ADINLOMR-NXT5                   
322200                                   AR-TAB-ADINLOMR-NXT5-VCOM              
322300                                   AR-LASER-ADINLOMR-NXT5                 
322400                                   OLD-LASER-ADINLOMR-NXT5                
322500                                   RR-LASER-ADINLOMR-NXT5                 
322600                                   RR-ZEBRA-ADINLOMR-NXT5                 
322700         MOVE ADR-ADINLOMR-NXT6 TO NOVA-TAB-ADINLOMR-NXT6                 
322800                                   AR-TAB-ADINLOMR-NXT6-VCOM              
322900                                   AR-LASER-ADINLOMR-NXT6                 
323000                                   OLD-LASER-ADINLOMR-NXT6                
323100                                   RR-LASER-ADINLOMR-NXT6                 
323200                                   RR-ZEBRA-ADINLOMR-NXT6                 
323300     END-IF                                                               
323400     .                                                                    
323500     EJECT                                                                
323600 CF-FLYTTA-ARTIKEL-INFO  SECTION.                                         
323700                                                                          
323800     PERFORM IMS-GU-ARTC-ARTC11                                           
323900     IF SEGMENT-FINNS                                                     
324000         PERFORM CFA-CALL-W400ARTU                                        
324100                                                                          
324200         MOVE ARTC11-CLAG-IDANSK   TO NOVA-TAB-IDANSK                     
324300         MOVE ARTC11-CLAG-IDANSK   TO AR-TAB-IDANSK                       
324400                                      AR-TAB-IDANSK-VCOM                  
324500                                      AR-LASER-IDANSK                     
324600                                      OLD-LASER-IDANSK                    
324700                                      RR-LASER-IDANSK                     
324800                                      RR-ZEBRA-IDANSK                     
324900                                                                          
324910         IF SW-NDC-US OR SW-NDC-CN                                        
324920            IF SW-NDC-US                                                  
324930               MOVE 'US' TO W-IDLAND-US-CN                                
324940            ELSE                                                          
324950               MOVE 'CN' TO W-IDLAND-US-CN                                
324960            END-IF                                                        
324970                                                                          
324980            PERFORM IMS-GU-WDK712                                         
324990            IF SEGMENT-FINNS                                              
324991               IF LART-KVQPACK-3 > 0                                      
324992                  MOVE LART-KVQPACK-3 TO ARTC11-CLAG-KVQPACK-3            
324993               END-IF                                                     
324994            END-IF                                                        
324995         END-IF                                                           
324996                                                                          
325000         MOVE ARTC11-CLAG-KVQPACK-3 TO NOVA-TAB-KVQPACK-3                 
325100         MOVE ARTC11-CLAG-KVQPACK-3 TO AR-TAB-KVQPACK-3                   
325200                                  AR-TAB-KVQPACK-3-VCOM                   
325300                                  AR-LASER-KVQPACK-3                      
325400                                  OLD-LASER-KVQPACK-3                     
325500                                  RR-LASER-KVQPACK-3                      
325600                                  RR-ZEBRA-KVQPACK-3                      
325700                                                                          
325810         MOVE ARTC11-CLAG-KDERS   TO NOVA-TAB-KDERS                       
325900         MOVE ARTC11-CLAG-KDERS   TO AR-TAB-KDERS                         
326000                                     AR-TAB-KDERS-VCOM                    
326100                                     AR-LASER-KDERS                       
326200                                     OLD-LASER-KDERS                      
326300                                     RR-LASER-KDERS                       
326400                                     RR-ZEBRA-KDERS                       
326500         IF SW-CDC                                                        
326600           MOVE ARTC11-CLAG-KVROS TO NOVA-TAB-KVROS                       
326700           MOVE ARTC11-CLAG-KVROS TO AR-TAB-KVROS                         
326800                                     AR-TAB-KVROS-VCOM                    
326900                                     AR-LASER-KVROS                       
327000                                     OLD-LASER-KVROS                      
327100                                     RR-LASER-KVROS                       
327200                                     RR-ZEBRA-KVROS                       
327300         ELSE                                                             
327400           PERFORM IMS-GU-WDK711                                          
327500           IF SEGMENT-FINNS                                               
327600             COMPUTE WS-KVROS = SLAG-KVROS-DAG + SLAG-KVROS-BULK          
327700             MOVE WS-KVROS       TO NOVA-TAB-KVROS                        
327800                                    AR-TAB-KVROS                          
327900                                    AR-TAB-KVROS-VCOM                     
328000                                    AR-LASER-KVROS                        
328100                                    OLD-LASER-KVROS                       
328200                                    RR-LASER-KVROS                        
328300                                    RR-ZEBRA-KVROS                        
328400           END-IF                                                         
328500         END-IF                                                           
328600                                                                          
328700         MOVE ARTU-BEARTURS-SVE            TO NOVA-TAB-BEARTURS           
328800         MOVE ARTU-BEARTURS-SVE            TO AR-TAB-BEARTURS             
328900                                              AR-TAB-BEARTURS-VCOM        
329000                                              AR-LASER-BEARTURS           
329100                                              OLD-LASER-BEARTURS          
329200         MOVE ARTU-BEARTURS-ENG                                           
329300                                           TO RR-LASER-BEARTURS           
329400                                              RR-ZEBRA-BEARTURS           
329500     END-IF                                                               
329600                                                                          
329700     PERFORM CFB-BEHANDLA-ARTD                                            
329800                                                                          
329900     PERFORM IMS-GU-WDF502                                                
330000     IF SEGMENT-FINNS                                                     
330100         MOVE XLEV-BELEVART    TO NOVA-TAB-BELEV1                         
330200                                  AR-TAB-BELEV1                           
330300                                  AR-TAB-BELEV1-VCOM                      
330400                                  AR-LASER-BELEV1                         
330500                                  OLD-LASER-BELEV1                        
330600                                  RR-LASER-BELEV1                         
330700                                  RR-ZEBRA-BELEV1                         
330800       PERFORM IMS-GNP-WDF502                                             
330900       IF SEGMENT-FINNS                                                   
331000           MOVE XLEV-BELEVART    TO NOVA-TAB-BELEV2                       
331100                                    AR-TAB-BELEV2                         
331200                                    AR-TAB-BELEV2-VCOM                    
331300                                    AR-LASER-BELEV2                       
331400                                    OLD-LASER-BELEV2                      
331500                                    RR-LASER-BELEV2                       
331600                                    RR-ZEBRA-BELEV2                       
331700       ELSE                                                               
331800           MOVE SPACE            TO NOVA-TAB-BELEV2                       
331900           MOVE SPACE            TO AR-TAB-BELEV2                         
332000                                    AR-TAB-BELEV2-VCOM                    
332100                                    AR-LASER-BELEV2                       
332200                                    OLD-LASER-BELEV2                      
332300                                    RR-LASER-BELEV2                       
332400                                    RR-ZEBRA-BELEV2                       
332500       END-IF                                                             
332600     ELSE                                                                 
332700         MOVE SPACE            TO NOVA-TAB-BELEV1                         
332800         MOVE SPACE            TO AR-TAB-BELEV1                           
332900                                  AR-TAB-BELEV1-VCOM                      
333000                                  AR-LASER-BELEV1                         
333100                                  OLD-LASER-BELEV1                        
333200                                  RR-LASER-BELEV1                         
333300                                  RR-ZEBRA-BELEV1                         
333400                                  NOVA-TAB-BELEV2                         
333500                                  AR-TAB-BELEV2-VCOM                      
333600                                  AR-LASER-BELEV2                         
333700                                  OLD-LASER-BELEV2                        
333800                                  RR-LASER-BELEV2                         
333900                                  RR-ZEBRA-BELEV2                         
334000     END-IF                                                               
334100     .                                                                    
334200     EJECT                                                                
334300 CFA-CALL-W400ARTU       SECTION.                                         
334400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
334500* ÖVERSÄTTER EN ARTIKELS URSPRUNGSKOD TILL KLARTEXT             *         
334600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
334700     SKIP2                                                                
334800                                                                          
334900     MOVE ARTC11-CLAG-KDARTURS                                            
335000                         TO ARTU-KDARTURS                                 
335100     MOVE ZERO           TO ARTU-IDDISTR                                  
335200     MOVE WS-IDDC        TO ARTU-IDDC                                     
335300                                                                          
335400     CALL W400ARTU USING ARTU-W400ARTU                                    
335500                                                                          
335600     .                                                                    
335700     EJECT                                                                
335800                                                                          
335900                                                                          
336000 CFB-BEHANDLA-ARTD       SECTION.                                         
336100                                                                          
336200     MOVE ZERO             TO NOVA-TAB-ADBUFFOMR-1                        
336300                              NOVA-TAB-ADBUFFGANG-1                       
336400                              NOVA-TAB-ADBUFFPL-1                         
336500                              NOVA-TAB-ADBUFFOMR-2                        
336600                              NOVA-TAB-ADBUFFGANG-2                       
336700                              NOVA-TAB-ADBUFFPL-2                         
336800                              NOVA-TAB-ADBUFFOMR-3                        
336900                              NOVA-TAB-ADBUFFGANG-3                       
337000                              NOVA-TAB-ADBUFFPL-3                         
337100     MOVE ZERO             TO AR-TAB-ADBUFFOMR-1                          
337200                              AR-TAB-ADBUFFGANG-1                         
337300                              AR-TAB-ADBUFFPL-1                           
337400                              AR-TAB-ADBUFFOMR-2                          
337500                              AR-TAB-ADBUFFGANG-2                         
337600                              AR-TAB-ADBUFFPL-2                           
337700                              AR-TAB-ADBUFFOMR-3                          
337800                              AR-TAB-ADBUFFGANG-3                         
337900                              AR-TAB-ADBUFFPL-3                           
338000                              AR-TAB-ADBUFFOMR-1-VCOM                     
338100                              AR-TAB-ADBUFFGANG-1-VCOM                    
338200                              AR-TAB-ADBUFFPL-1-VCOM                      
338300                              AR-TAB-ADBUFFOMR-2-VCOM                     
338400                              AR-TAB-ADBUFFGANG-2-VCOM                    
338500                              AR-TAB-ADBUFFPL-2-VCOM                      
338600                              AR-TAB-ADBUFFOMR-3-VCOM                     
338700                              AR-TAB-ADBUFFGANG-3-VCOM                    
338800                              AR-TAB-ADBUFFPL-3-VCOM                      
338900                              AR-LASER-ADBUFFOMR-1                        
339000                              OLD-LASER-ADBUFFOMR-1                       
339100                              AR-LASER-ADBUFFGANG-1                       
339200                              OLD-LASER-ADBUFFGANG-1                      
339300                              AR-LASER-ADBUFFPL-1                         
339400                              OLD-LASER-ADBUFFPL-1                        
339500                              AR-LASER-ADBUFFOMR-2                        
339600                              OLD-LASER-ADBUFFOMR-2                       
339700                              AR-LASER-ADBUFFGANG-2                       
339800                              OLD-LASER-ADBUFFGANG-2                      
339900                              AR-LASER-ADBUFFPL-2                         
340000                              OLD-LASER-ADBUFFPL-2                        
340100                              AR-LASER-ADBUFFOMR-3                        
340200                              OLD-LASER-ADBUFFOMR-3                       
340300                              AR-LASER-ADBUFFGANG-3                       
340400                              OLD-LASER-ADBUFFGANG-3                      
340500                              AR-LASER-ADBUFFPL-3                         
340600                              OLD-LASER-ADBUFFPL-3                        
340700                              RR-LASER-ADBUFFOMR-1                        
340800                              RR-LASER-ADBUFFGANG-1                       
340900                              RR-LASER-ADBUFFPL-1                         
341000                              RR-LASER-ADBUFFOMR-2                        
341100                              RR-LASER-ADBUFFGANG-2                       
341200                              RR-LASER-ADBUFFPL-2                         
341300                              RR-LASER-ADBUFFOMR-3                        
341400                              RR-LASER-ADBUFFGANG-3                       
341500                              RR-LASER-ADBUFFPL-3                         
341600                              RR-ZEBRA-ADBUFFOMR-1                        
341700                              RR-ZEBRA-ADBUFFGANG-1                       
341800                              RR-ZEBRA-ADBUFFPL-1                         
341900                              RR-ZEBRA-ADBUFFOMR-2                        
342000                              RR-ZEBRA-ADBUFFGANG-2                       
342100                              RR-ZEBRA-ADBUFFPL-2                         
342200                              RR-ZEBRA-ADBUFFOMR-3                        
342300                              RR-ZEBRA-ADBUFFGANG-3                       
342400                              RR-ZEBRA-ADBUFFPL-3                         
342500     MOVE 1                        TO ANT-ADBUFF                          
342600     PERFORM IMS-GU-ARTD-ARTD11                                           
342700     IF SEGMENT-FINNS                                                     
342800         PERFORM UNTIL SEGMENT-SAKNAS OR ANT-ADBUFF = 4                   
342900             MOVE SALDO-ADBUFFOMR  TO W-ADBUFFOMR                         
343000             IF W-ADBUFFOMR        =  W-ADLAGOMR                          
343100                 PERFORM IMS-GNP-ARTD-ARTD11                              
343200              ELSE                                                        
343300                 EVALUATE ANT-ADBUFF                                      
343400                   WHEN 1                                                 
343500                    MOVE SALDO-ADBUFFOMR  TO NOVA-TAB-ADBUFFOMR-1         
343600                    MOVE SALDO-ADBUFFOMR  TO AR-TAB-ADBUFFOMR-1           
343700                                           AR-TAB-ADBUFFOMR-1-VCOM        
343800                                             AR-LASER-ADBUFFOMR-1         
343900                                             OLD-LASER-ADBUFFOMR-1        
344000                                             RR-LASER-ADBUFFOMR-1         
344100                                             RR-ZEBRA-ADBUFFOMR-1         
344200                    MOVE SALDO-ADBUFFGANG TO NOVA-TAB-ADBUFFGANG-1        
344300                    MOVE SALDO-ADBUFFGANG TO AR-TAB-ADBUFFGANG-1          
344400                                          AR-TAB-ADBUFFGANG-1-VCOM        
344500                                             AR-LASER-ADBUFFGANG-1        
344600                                            OLD-LASER-ADBUFFGANG-1        
344700                                             RR-LASER-ADBUFFGANG-1        
344800                                             RR-ZEBRA-ADBUFFGANG-1        
344900                    MOVE SALDO-ADBUFFPL   TO NOVA-TAB-ADBUFFPL-1          
345000                    MOVE SALDO-ADBUFFPL   TO AR-TAB-ADBUFFPL-1            
345100                                          AR-TAB-ADBUFFPL-1-VCOM          
345200                                             AR-LASER-ADBUFFPL-1          
345300                                             OLD-LASER-ADBUFFPL-1         
345400                                             RR-LASER-ADBUFFPL-1          
345500                                             RR-ZEBRA-ADBUFFPL-1          
345600                   WHEN 2                                                 
345700                    MOVE SALDO-ADBUFFOMR  TO NOVA-TAB-ADBUFFOMR-2         
345800                    MOVE SALDO-ADBUFFOMR  TO AR-TAB-ADBUFFOMR-2           
345900                                           AR-TAB-ADBUFFOMR-2-VCOM        
346000                                             AR-LASER-ADBUFFOMR-2         
346100                                             OLD-LASER-ADBUFFOMR-2        
346200                                             RR-LASER-ADBUFFOMR-2         
346300                                             RR-ZEBRA-ADBUFFOMR-2         
346400                    MOVE SALDO-ADBUFFGANG TO NOVA-TAB-ADBUFFGANG-2        
346500                    MOVE SALDO-ADBUFFGANG TO AR-TAB-ADBUFFGANG-2          
346600                                          AR-TAB-ADBUFFGANG-2-VCOM        
346700                                             AR-LASER-ADBUFFGANG-2        
346800                                           OLD-LASER-ADBUFFGANG-2         
346900                                             RR-LASER-ADBUFFGANG-2        
347000                                             RR-ZEBRA-ADBUFFGANG-2        
347100                    MOVE SALDO-ADBUFFPL   TO NOVA-TAB-ADBUFFPL-2          
347200                    MOVE SALDO-ADBUFFPL   TO AR-TAB-ADBUFFPL-2            
347300                                          AR-TAB-ADBUFFPL-2-VCOM          
347400                                             AR-LASER-ADBUFFPL-2          
347500                                             OLD-LASER-ADBUFFPL-2         
347600                                             RR-LASER-ADBUFFPL-2          
347700                                             RR-ZEBRA-ADBUFFPL-2          
347800                   WHEN 3                                                 
347900                    MOVE SALDO-ADBUFFOMR  TO NOVA-TAB-ADBUFFOMR-3         
348000                    MOVE SALDO-ADBUFFOMR  TO AR-TAB-ADBUFFOMR-3           
348100                                          AR-TAB-ADBUFFOMR-3-VCOM         
348200                                             AR-LASER-ADBUFFOMR-3         
348300                                             OLD-LASER-ADBUFFOMR-3        
348400                                             RR-LASER-ADBUFFOMR-3         
348500                                             RR-ZEBRA-ADBUFFOMR-3         
348600                    MOVE SALDO-ADBUFFGANG TO NOVA-TAB-ADBUFFGANG-3        
348700                    MOVE SALDO-ADBUFFGANG TO AR-TAB-ADBUFFGANG-3          
348800                                          AR-TAB-ADBUFFGANG-3-VCOM        
348900                                             AR-LASER-ADBUFFGANG-3        
349000                                           OLD-LASER-ADBUFFGANG-3         
349100                                             RR-LASER-ADBUFFGANG-3        
349200                                             RR-ZEBRA-ADBUFFGANG-3        
349300                    MOVE SALDO-ADBUFFPL   TO NOVA-TAB-ADBUFFPL-3          
349400                    MOVE SALDO-ADBUFFPL   TO AR-TAB-ADBUFFPL-3            
349500                                          AR-TAB-ADBUFFPL-3-VCOM          
349600                                             AR-LASER-ADBUFFPL-3          
349700                                             OLD-LASER-ADBUFFPL-3         
349800                                             RR-LASER-ADBUFFPL-3          
349900                                             RR-ZEBRA-ADBUFFPL-3          
350000                 END-EVALUATE                                             
350100                 PERFORM IMS-GNP-ARTD-ARTD11                              
350200                 ADD +1                TO ANT-ADBUFF                      
350300             END-IF                                                       
350400         END-PERFORM                                                      
350500         IF ANT-ADBUFF         = 1                                        
350600             PERFORM IMS-GNP-ARTD-ARTD11-FIRST                            
350700             IF SEGMENT-FINNS                                             
350800                 MOVE SALDO-ADBUFFOMR  TO NOVA-TAB-ADBUFFOMR-1            
350900                 MOVE SALDO-ADBUFFOMR  TO AR-TAB-ADBUFFOMR-1              
351000                                       AR-TAB-ADBUFFOMR-1-VCOM            
351100                                          AR-LASER-ADBUFFOMR-1            
351200                                          OLD-LASER-ADBUFFOMR-1           
351300                                          RR-LASER-ADBUFFOMR-1            
351400                                          RR-ZEBRA-ADBUFFOMR-1            
351500                 MOVE SALDO-ADBUFFGANG TO NOVA-TAB-ADBUFFGANG-1           
351600                 MOVE SALDO-ADBUFFGANG TO AR-TAB-ADBUFFGANG-1             
351700                                       AR-TAB-ADBUFFGANG-1-VCOM           
351800                                          AR-LASER-ADBUFFGANG-1           
351900                                          OLD-LASER-ADBUFFGANG-1          
352000                                          RR-LASER-ADBUFFGANG-1           
352100                                          RR-ZEBRA-ADBUFFGANG-1           
352200                 MOVE SALDO-ADBUFFPL   TO NOVA-TAB-ADBUFFPL-1             
352300                 MOVE SALDO-ADBUFFPL   TO AR-TAB-ADBUFFPL-1               
352400                                       AR-TAB-ADBUFFPL-1-VCOM             
352500                                          AR-LASER-ADBUFFPL-1             
352600                                          OLD-LASER-ADBUFFPL-1            
352700                                          RR-LASER-ADBUFFPL-1             
352800                                          RR-ZEBRA-ADBUFFPL-1             
352900             END-IF                                                       
353000         END-IF                                                           
353100     END-IF                                                               
353200     .                                                                    
353300     EJECT                                                                
353400 CG-SKRIV-RAPPORT        SECTION.                                         
353500                                                                          
353600     MOVE PRT-IDPRTLST         TO WS-RAPP-PRINTER                         
353700                                                                          
353800     IF LASER                                                             
353900       MOVE WS-IDDC            TO SW-WS-IDDC                              
354000       IF SW-CDC-SE                                                       
354100         IF PRT-IDPRTLST(3:6) = 'GM3   ' OR 'GM5   ' OR                   
354200                                '5691  '                                  
354300           MOVE +1 TO AR-LASER-IX                                         
354400           MOVE PRT-NYSIDA-RAD1    TO PRT-RADSKIP                         
354500           PERFORM UNTIL AR-LASER-IX > MAX-LASER-IX                       
354600             MOVE AR-LASER-RAD (AR-LASER-IX) TO AR-LIST-RAD               
354700             CALL W006PRS1 USING       PRT-SPOOL-OVR                      
354800                                         PRT-WRITE                        
354900                                         WS-RAPP-PRINTER                  
355000                                         ALT-PCB                          
355100                                         PRT-RADSKIP                      
355200                                         AR-LIST-RAD                      
355300             MOVE PRT-AFTER-1        TO PRT-RADSKIP                       
355400             ADD +1 TO AR-LASER-IX                                        
355500           END-PERFORM                                                    
355600         ELSE                                                             
355700           MOVE +1 TO  OLD-LASER-IX                                       
355800           MOVE PRT-NYSIDA-RAD1    TO PRT-RADSKIP                         
355900           PERFORM UNTIL OLD-LASER-IX > MAX-LASER-IX-OLD                  
356000             MOVE OLD-LASER-RAD (OLD-LASER-IX) TO AR-LIST-RAD             
356100             CALL W006PRS1 USING       PRT-SPOOL-OVR                      
356200                                         PRT-WRITE                        
356300                                         WS-RAPP-PRINTER                  
356400                                         ALT-PCB                          
356500                                         PRT-RADSKIP                      
356600                                         AR-LIST-RAD                      
356700             MOVE PRT-AFTER-1        TO PRT-RADSKIP                       
356800             ADD +1 TO OLD-LASER-IX                                       
356900           END-PERFORM                                                    
357000         END-IF                                                           
357100       ELSE                                                               
357200         MOVE +1 TO RR-LASER-IX                                           
357300         MOVE PRT-NYSIDA-RAD1      TO PRT-RADSKIP                         
357400         PERFORM UNTIL RR-LASER-IX > MAX-LASER-IX-RR                      
357500           MOVE RR-LASER-RAD (RR-LASER-IX) TO AR-LIST-RAD                 
357600           CALL W006PRS1 USING         PRT-SPOOL-OVR                      
357700                                       PRT-WRITE                          
357800                                       WS-RAPP-PRINTER                    
357900                                       ALT-PCB                            
358000                                       PRT-RADSKIP                        
358100                                       AR-LIST-RAD                        
358200           MOVE PRT-AFTER-1          TO PRT-RADSKIP                       
358300           ADD +1 TO RR-LASER-IX                                          
358400         END-PERFORM                                                      
358500       END-IF                                                             
358600     ELSE                                                                 
358700       MOVE WS-IDDC        TO SW-WS-IDDC                                  
358800       IF SW-CDC-SE                                                       
358900         IF PRT-BEPRTLST(1:4) = 'VCOM'                                    
358910           CONTINUE                                                       
359000                                                                          
359100*          MOVE +1 TO AR-IX-VCOM                                          
359200*          MOVE PRT-NYSIDA-RAD1      TO PRT-RADSKIP                       
359300*          PERFORM UNTIL AR-IX-VCOM > MAX-TAB-IX-VCOM                     
359400*            MOVE AR-RAD-VCOM (AR-IX-VCOM) TO PRC1-DATA                   
359500*            CALL W006PRC1 USING         PRT-VCOM                         
359600*                                        PRT-WRITE                        
359700*                                        WS-RAPP-PRINTER                  
359800*                                        ALT-PCB                          
359900*                                        PRC1-W006PRVC                    
360000*            MOVE PRT-AFTER-1          TO PRT-RADSKIP                     
360100*            ADD +1 TO AR-IX-VCOM                                         
360200*          END-PERFORM                                                    
360300         ELSE                                                             
360400                                                                          
360500           IF PRT-BEPRTLST(1:4) = 'NOVA'                                  
360600             MOVE +1 TO AR-IX                                             
360700             MOVE PRT-NYSIDA-RAD1    TO PRT-RADSKIP                       
360800             PERFORM UNTIL AR-IX > MAX-TAB-IX-STAENDE                     
360900               MOVE NOVA-RAD (AR-IX) TO AR-LIST-RAD                       
361000               CALL W006PRS1 USING       PRT-SPOOL-OVR                    
361100                                         PRT-WRITE                        
361200                                         WS-RAPP-PRINTER                  
361300                                         ALT-PCB                          
361400                                         PRT-RADSKIP                      
361500                                         AR-LIST-RAD                      
361600               MOVE PRT-AFTER-1        TO PRT-RADSKIP                     
361700               ADD +1 TO AR-IX                                            
361800             END-PERFORM                                                  
361900           ELSE                                                           
362000                                                                          
362100             MOVE +1 TO AR-IX                                             
362200             MOVE PRT-NYSIDA-RAD1    TO PRT-RADSKIP                       
362300             PERFORM UNTIL AR-IX > MAX-TAB-IX-LIGGANDE                    
362400               MOVE AR-RAD (AR-IX) TO AR-LIST-RAD                         
362500               CALL W006PRS1 USING       PRT-SPOOL-OVR                    
362600                                           PRT-WRITE                      
362700                                           WS-RAPP-PRINTER                
362800                                           ALT-PCB                        
362900                                           PRT-RADSKIP                    
363000                                           AR-LIST-RAD                    
363100               MOVE PRT-AFTER-1        TO PRT-RADSKIP                     
363200               ADD +1 TO AR-IX                                            
363300             END-PERFORM                                                  
363400           END-IF                                                         
363500         END-IF                                                           
363600       ELSE                                                               
363700         MOVE +1 TO RR-IX-ZEBRA                                           
363800         MOVE PRT-NYSIDA-RAD1      TO PRT-RADSKIP                         
363900         PERFORM UNTIL RR-IX-ZEBRA > MAX-ZEBRA-IX                         
364000           MOVE RR-ZEBRA-RAD (RR-IX-ZEBRA) TO AR-LIST-RAD                 
364100           CALL W006PRS1 USING         PRT-SPOOL-OVR                      
364200                                       PRT-WRITE                          
364300                                       WS-RAPP-PRINTER                    
364400                                       ALT-PCB                            
364500                                       PRT-RADSKIP                        
364600                                       AR-LIST-RAD                        
364700           MOVE PRT-AFTER-1          TO PRT-RADSKIP                       
364800           ADD +1 TO RR-IX-ZEBRA                                          
364900         END-PERFORM                                                      
365000       END-IF                                                             
365100     END-IF                                                               
365200     .                                                                    
365300     EJECT                                                                
365400                                                                          
365500 CH-HAEMTA-KDFGPRIO      SECTION.                                         
365600                                                                          
365700     PERFORM IMS-GU-WDD501                                                
365800     IF SEGMENT-FINNS                                                     
365900        MOVE WDD5-ART-KDFGPRIO       TO NOVA-TAB-KDFGPRIO                 
366000                                        AR-TAB-KDFGPRIO                   
366100                                        AR-TAB-KDFGPRIO-VCOM              
366200                                        AR-LASER-KDFGPRIO                 
366300                                        OLD-LASER-KDFGPRIO                
366400                                        RR-LASER-KDFGPRIO                 
366500                                        RR-ZEBRA-KDFGPRIO                 
366600     ELSE                                                                 
366700        MOVE ZERO                    TO NOVA-TAB-KDFGPRIO                 
366800                                        AR-TAB-KDFGPRIO                   
366900                                        AR-TAB-KDFGPRIO-VCOM              
367000                                        AR-LASER-KDFGPRIO                 
367100                                        OLD-LASER-KDFGPRIO                
367200                                        RR-LASER-KDFGPRIO                 
367300                                        RR-ZEBRA-KDFGPRIO                 
367400     END-IF                                                               
367500     .                                                                    
367600     EJECT                                                                
367700 S01-KOLLA-PRINTER         SECTION.                                       
367800     SKIP3                                                                
367900     MOVE SPACE                          TO PRT-IDPRTLST                  
368000     MOVE '6L'                           TO PRT-IDPRTLST(1:2)             
368100     MOVE MOD-MID-ADINLOMR-PRT           TO PRT-IDPRTLST(3:6)             
368200     MOVE 1                              TO PRT-KDCALL                    
368300     CALL W006PRT  USING PRT-W006PRT                                      
368400     IF PRT-KDSVAR                       = 'F'                            
368500        MOVE SPACE                       TO PRT-IDPRTLST                  
368600        MOVE '6F'                        TO PRT-IDPRTLST(1:2)             
368700        MOVE MOD-MID-ADINLOMR-PRT        TO PRT-IDPRTLST(3:6)             
368800        MOVE 1                           TO PRT-KDCALL                    
368900        CALL W006PRT  USING PRT-W006PRT                                   
369000        IF PRT-KDSVAR                    = 'F'                            
369100           STRING  'SKRIVARE SAKNAS '  PRT-IDPRTLST                       
369200           DELIMITED BY SIZE INTO FELTEXT                                 
369300           CALL FELLOG                                                    
369400*          DET HÄR BORDE INTE FÅ FÖREKOMMA                                
369500*          PRINTERN KOLLAD I ONLINE-BILDEN                                
369600        END-IF                                                            
369700     ELSE                                                                 
369800        MOVE JA               TO LASER-SW                                 
369900     END-IF                                                               
370000     .                                                                    
370100     EJECT                                                                
370200 S02-OPEN-PRINTER         SECTION.                                        
370300                                                                          
370400     IF PRT-BEPRTLST(1:4) = 'VCOM'                                        
370410       CONTINUE                                                           
370500*      MOVE +120                TO PRC1-KVLRECL                           
370600*      MOVE 'W006ASCI'          TO PRC1-IDVCINIT                          
370700*      MOVE 'W006PRT '          TO PRC1-TEVCOMST                          
370800*      MOVE SPACE               TO PRC1-DATA                              
370900*      CALL W006PRC1 USING         PRT-VCOM                               
371000*                                  PRT-OPEN                               
371100*                                  WS-RAPP-PRINTER                        
371200*                                  ALT-PCB                                
371300*                                  PRC1-W006PRVC                          
371400     ELSE                                                                 
371500       CALL W006PRS1 USING         PRT-SPOOL-OVR                          
371600                                   PRT-OPEN                               
371700                                   WS-RAPP-PRINTER                        
371800                                   ALT-PCB                                
371900                                   WS-DUMMY                               
372000                                   WS-DUMMY                               
372100     END-IF                                                               
372200     .                                                                    
372300     EJECT                                                                
372400 S03-CLOSE-PRINTER         SECTION.                                       
372500                                                                          
372600     IF PRT-BEPRTLST(1:4) = 'VCOM'                                        
372610       CONTINUE                                                           
372700*      CALL W006PRC1 USING         PRT-VCOM                               
372800*                                  PRT-CLOSE                              
372900*                                  WS-RAPP-PRINTER                        
373000*                                  ALT-PCB                                
373100*                                  PRC1-W006PRVC                          
373200     ELSE                                                                 
373300       CALL W006PRS1 USING         PRT-SPOOL-OVR                          
373400                                   PRT-CLOSE                              
373500                                   WS-RAPP-PRINTER                        
373600                                   ALT-PCB                                
373700                                   WS-DUMMY                               
373800                                   WS-DUMMY                               
373900     END-IF                                                               
374000     .                                                                    
374100     EJECT                                                                
374200* --- IMS SEKTIONER ---                                                   
374300     SKIP3                                                                
374400 IMS-GET-MSG SECTION.                                                     
374500                                                                          
374600     MOVE '  QC' TO GODK-STATUSKODER                                      
374700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
374800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
374900     PERFORM IMS-STATUSKONTROLL                                           
375000     .                                                                    
375100     SKIP3                                                                
375200 IMS-INSERT-MSG SECTION.                                                  
375300                                                                          
375400     IF ENGLISH-TEXT                                                      
375500       MOVE 'N' TO MFS-KDHUVOMR                                           
375600     END-IF                                                               
375700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
375800     MOVE SPACE TO GODK-STATUSKODER                                       
375900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
376000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
376100     PERFORM IMS-STATUSKONTROLL                                           
376200     .                                                                    
376300     EJECT                                                                
376400 IMS-GU-INLA-INLA01 SECTION.                                              
376500     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
376600          DELIMITED BY SIZE INTO SSA1                                     
376700     MOVE '  GE' TO GODK-STATUSKODER                                      
376800     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA SSA1                      
376900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
377000     PERFORM IMS-STATUSKONTROLL                                           
377100     .                                                                    
377200     SKIP3                                                                
377300 IMS-GNP-INLA-INLA11 SECTION.                                             
377400     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
377500          DELIMITED BY SIZE INTO SSA2                                     
377600     MOVE '  GE' TO GODK-STATUSKODER                                      
377700     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA SSA1 SSA2                
377800     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
377900     PERFORM IMS-STATUSKONTROLL                                           
378000     .                                                                    
378100     SKIP3                                                                
378200 IMS-GU-INLC-INLC01 SECTION.                                              
378300     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
378400          DELIMITED BY SIZE INTO SSA1                                     
378500     MOVE '  GE' TO GODK-STATUSKODER                                      
378600     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA  SSA1                     
378700     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
378800     PERFORM IMS-STATUSKONTROLL                                           
378900     .                                                                    
379000     SKIP3                                                                
379100 IMS-GU-PLAA-PLAA11 SECTION.                                              
379200     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
379300          DELIMITED BY SIZE INTO SSA1                                     
379400     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
379500          DELIMITED BY SIZE INTO SSA2                                     
379600     MOVE '  GE' TO GODK-STATUSKODER                                      
379700     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
379800     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
379900     PERFORM IMS-STATUSKONTROLL                                           
380000     .                                                                    
380100     EJECT                                                                
380200 IMS-GU-ARTC-ARTC11 SECTION.                                              
380300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
380400          DELIMITED BY SIZE INTO SSA1                                     
380500     MOVE 'WLARTC11 ' TO SSA2                                             
380600     MOVE '  ' TO GODK-STATUSKODER                                        
380700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
380800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
380900     PERFORM IMS-STATUSKONTROLL                                           
381000     .                                                                    
381100     SKIP3                                                                
381200 IMS-GU-ARTD-ARTD11 SECTION.                                              
381300     STRING 'WLARTD01*P(IDARTNR  =' W-IDARTNR-X ')'                       
381400          DELIMITED BY SIZE INTO SSA1                                     
381500     STRING 'WLARTD11(IDDC     =' W-IDDC-X ')'                            
381600          DELIMITED BY SIZE INTO SSA2                                     
381700     MOVE '  GE' TO GODK-STATUSKODER                                      
381800     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA SSA1 SSA2                 
381900     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
382000     PERFORM IMS-STATUSKONTROLL                                           
382100     .                                                                    
382200     SKIP3                                                                
382300 IMS-GNP-ARTD-ARTD11 SECTION.                                             
382400     STRING 'WLARTD11(IDDC     =' W-IDDC-X ')'                            
382500          DELIMITED BY SIZE INTO SSA1                                     
382600     MOVE '  GE' TO GODK-STATUSKODER                                      
382700     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
382800     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
382900     PERFORM IMS-STATUSKONTROLL                                           
383000     .                                                                    
383100     SKIP3                                                                
383200 IMS-GNP-ARTD-ARTD11-FIRST SECTION.                                       
383300     STRING 'WLARTD11*F(IDDC     =' W-IDDC-X ')'                          
383400          DELIMITED BY SIZE INTO SSA1                                     
383500     MOVE '  GE' TO GODK-STATUSKODER                                      
383600     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
383700     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
383800     PERFORM IMS-STATUSKONTROLL                                           
383900     .                                                                    
384000     EJECT                                                                
384100 IMS-GU-WDF502      SECTION.                                              
384200     STRING 'WDF501  *P(IDARTNR  =' W-IDARTNR-X ')'                       
384300          DELIMITED BY SIZE INTO SSA1                                     
384400     STRING 'WDF502  (IDLEVNR  =' W-IDLEVNR-X ')'                         
384500          DELIMITED BY SIZE INTO SSA2                                     
384600     MOVE '  GE' TO GODK-STATUSKODER                                      
384700     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-AREA SSA1 SSA2                 
384800     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
384900     PERFORM IMS-STATUSKONTROLL                                           
385000     .                                                                    
385100     EJECT                                                                
385200 IMS-GNP-WDF502      SECTION.                                             
385300     STRING 'WDF502  (IDLEVNR  =' W-IDLEVNR-X ')'                         
385400          DELIMITED BY SIZE INTO SSA1                                     
385500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
385600     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-AREA SSA1                     
385700     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
385800     PERFORM IMS-STATUSKONTROLL                                           
385900     .                                                                    
386000     EJECT                                                                
386100 IMS-GU-WDK711 SECTION.                                                   
386200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
386300          DELIMITED BY SIZE INTO SSA1                                     
386400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
386500          DELIMITED BY SIZE INTO SSA2                                     
386600     MOVE '  GE' TO GODK-STATUSKODER                                      
386700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2            
386800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
386900     PERFORM IMS-STATUSKONTROLL                                           
387000     .                                                                    
387100     EJECT                                                                
387110 IMS-GU-WDK712 SECTION.                                                   
387120     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
387130          DELIMITED BY SIZE INTO SSA1                                     
387140     STRING 'WDK712  (IDLAND   =' W-IDLAND ')'                            
387150          DELIMITED BY SIZE INTO SSA2                                     
387160     MOVE '  GE' TO GODK-STATUSKODER                                      
387170     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
387180     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
387190     PERFORM IMS-STATUSKONTROLL                                           
387191     .                                                                    
387192     EJECT                                                                
387200 IMS-GU-WDD501 SECTION.                                                   
387300     STRING 'WDD501  (IDARTNR  =' W-IDARTNR-X ')'                         
387400          DELIMITED BY SIZE INTO SSA1                                     
387500     MOVE '  GE' TO GODK-STATUSKODER                                      
387600     CALL CBLTDLI USING GU WDD5-PCB DLI-IO-AREA-WDD5 SSA1                 
387700     MOVE WDD5-STATUS-CODE TO STATUS-WS                                   
387800     PERFORM IMS-STATUSKONTROLL                                           
387900     .                                                                    
388000 IMS-STATUSKONTROLL SECTION.                                              
388100                                                                          
388200     SET STATUS-IX TO 1                                                   
388300     SEARCH GODK-STATUS                                                   
388400       AT END                                                             
388500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
388600         DELIMITED BY SIZE INTO FELTEXT                                   
388700         CALL FELLOG                                                      
388800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
388900         CONTINUE                                                         
389000     END-SEARCH                                                           
390000     .                                                                    
