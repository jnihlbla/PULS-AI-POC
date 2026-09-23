000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W611REG.                                                 
000500*AUTHOR.         LARS THELL.                                              
000600*DATE-WRITTEN.   92/02/19.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        SUBPROGRAM SOM UPPDATERAR INLEVERANSREGISTRET (W6D1).            
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001400*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
001500*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001600*        PROGRAMMET LÄSER      WDK7                                       
001700*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001800*        PROGRAMMET LÄSER      WLINLB (WDD9)                              
001900*        PROGRAMMET LÄSER      WDB6                                       
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800     SKIP2                                                                
003900 77  IDPGM                       PIC X(8)    VALUE 'W611REG'.             
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004300   88 ALLT-OK                                VALUE 'J'.                   
004400 77  IDARTNR-SW                  PIC X       VALUE 'J'.                   
004500   88 IDARTNR-FEL                            VALUE 'N'.                   
004600   88 IDARTNR-OK                             VALUE 'J'.                   
004700 77  KDRT-SW                     PIC X       VALUE 'J'.                   
004800   88 KDRT-FEL                               VALUE 'N'.                   
004900 77  TIAVIDAT-SW                 PIC X       VALUE 'J'.                   
005000   88 TIAVIDAT-FEL                           VALUE 'N'.                   
005100 77  FS-AKTIVERAD-SW             PIC X       VALUE 'J'.                   
005200   88 FS-AKTIVERAD                           VALUE 'J'.                   
005300 77  FLFEL-AENDRAD-SW            PIC X       VALUE 'N'.                   
005400   88 FLFEL-AENDRAD                          VALUE 'J'.                   
005500 77  SLUT-SW                     PIC X       VALUE 'N'.                   
005600   88 SLUT                                   VALUE 'J'.                   
005700 77  PRIS-FINNS-SW               PIC X       VALUE 'J'.                   
005800   88 PRIS-FINNS                             VALUE 'J'.                   
005900   88 PRIS-FINNS-INTE                        VALUE 'N'.                   
006000 77  KDVALISO-FINNS-SW           PIC X       VALUE 'J'.                   
006100   88 KDVALISO-FINNS                         VALUE 'J'.                   
006200   88 KDVALISO-FINNS-INTE                    VALUE 'N'.                   
006300                                                                          
006400*01    -COPY WWDCKONS                                                     
006500*      --- VALID IDDC CODES                                               
006600*                                                                         
006700*01    -COPY WWLNDKON                                                     
006800*      --- VALID COUNTRY CODES                                            
006900*                                                                         
007000*01    -COPY WWDC99                                                       
007100       EJECT                                                              
007200*      --- EMBALLAGE-KODER MED ÖVERSÄTTNINGAR                             
007300*                                                                         
007400*01    -COPY W611EMB3                                                     
007500       EJECT                                                              
007600                                                                          
007700*                                                                         
007800*01    -COPY WWPRODSL                                                     
007900       EJECT                                                              
008000                                                                          
008100 77  IDMFSFEL                    PIC X(3)    VALUE SPACE.                 
008200                                                                          
008300 77  INDX                        PIC S9(9)  VALUE ZERO COMP SYNC.         
008400 77  PRIS-INDX-MAX               PIC S9(9)  VALUE +5   COMP SYNC.         
008500 77  EMB-IX                      PIC S9(9)  VALUE ZERO COMP SYNC.         
008600 77  TAB-IX                      PIC S9(9)  VALUE ZERO COMP SYNC.         
008700 77  REG-IX                      PIC S9(9)  VALUE ZERO COMP SYNC.         
008800 77  REG1-IX                     PIC S9(9)  VALUE ZERO COMP SYNC.         
008900 77  REG2-IX                     PIC S9(9)  VALUE ZERO COMP SYNC.         
009000 77  REG3-IX                     PIC S9(9)  VALUE ZERO COMP SYNC.         
009100 77  REG4-IX                     PIC S9(9)  VALUE ZERO COMP SYNC.         
009200 77  REG6-IX                     PIC S9(9)  VALUE ZERO COMP SYNC.         
009300                                                                          
009400*    MAX-REG1, REG4 OCH REG6 SÄTTS AV ANROPANDE PROGRAM                   
009500 77  MAX-REG                     PIC S9(4)  VALUE +500 COMP SYNC.         
009600 77  MAX-REG1                    PIC S9(4)             COMP SYNC.         
009700 77  MAX-REG2                    PIC S9(4)  VALUE +24  COMP SYNC.         
009800 77  MAX-REG3                    PIC S9(4)  VALUE +12  COMP SYNC.         
009900 77  MAX-REG4                    PIC S9(4)             COMP SYNC.         
010000 77  MAX-REG6                    PIC S9(4)             COMP SYNC.         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(10)   VALUE 'REG AREA'.            
010300 01  REG-AREA-TABELL.                                                     
010400  05 REG-IDTRANS                 PIC X(4).                                
010500  05 REG-IDDC                    PIC X(2).                                
010600  05 REG-IDLBBET                 PIC X(12).                               
010700  05 REG-FLGODK                  PIC X(1).                                
010800  05 REG-FLGODK-IDFS             PIC X(1).                                
010900  05 REG-FLGODK-IDARTNR          PIC X(1).                                
011000  05 REG-IDFTG                   PIC S9(3)   COMP-3.                      
011100  05 REG-IDKONTO                 PIC 9(10).                               
011200  05 REG-IDANALYS                PIC X(12).                               
011300  05 REG-IDKST                   PIC X(10).                               
011400  05 REG-RAD           OCCURS 500.                                        
011500   10 REG-IDARTNR-OK             PIC X.                                   
011600   10 REG-IDMFSFEL               PIC X(3).                                
011700   10 REG-IDFS-OK                PIC X.                                   
011800   10 REG-KDBEH                  PIC S9(1).                               
011900   10 REG-IDLEVNR                PIC  X(5).                               
012000   10 REG-IDARTNR-NY             PIC S9(9)   COMP-3.                      
012100   10 REG-IDARTNR-GAMMAL         PIC S9(9)   COMP-3.                      
012200   10 REG-IDRADNR-INL-GAMMAL     PIC S9(5)   COMP-3.                      
012300   10 REG-IDFS                   PIC X(8).                                
012400   10 REG-KVAVIS                 PIC S9(7)   COMP-3.                      
012500   10 REG-KDRT                   PIC S9(3)   COMP-3.                      
012600   10 REG-TIAVIDAT               PIC S9(7).                               
012700   10 REG-VKART                  PIC S9(7)   COMP-3.                      
012800   10 REG-VLARTNTO               PIC S9(8)V9(1) COMP-3.                   
012900   10 REG-ADLAGOMR               PIC S9(3)      COMP-3.                   
013000   10 REG-ADGANG                 PIC S9(3)      COMP-3.                   
013100   10 REG-ADPLATS                PIC S9(5)      COMP-3.                   
013200   10 REG-BEFT                   PIC S9(3)      COMP-3.                   
013300   10 REG-IDFKNGRP               PIC S9(5)      COMP-3.                   
013400   10 REG-FLFSP                  PIC  X(1).                               
013500   10 REG-PRARTSTD               PIC S9(7)V9(2) COMP-3.                   
013600   10 REG-BEART                  PIC X(25).                               
013700   10 REG-KDFARLIG               PIC S9(1)      COMP-3.                   
013800   10 REG-KDSORT                 PIC  X(2).                               
013900   10 REG-KVMP                   PIC S9(7)      COMP-3.                   
014000   10 REG-CL1-KDLAGEMB           PIC X(4).                                
014100   10 REG-KDARTURS               PIC X(2).                                
014200                                                                          
014300 77  W-SPAR-IDLEVNR           PIC  X(5)      VALUE SPACE.                 
014400 77  W-SPAR-IDRADNR-INL       PIC S9(5)      COMP-3 VALUE ZERO.           
014500 77  W-KDRT                   PIC S9(3)      COMP-3 VALUE ZERO.           
014600 77  W-CL1-IDARTNR-EMBQ3      PIC S9(9)   VALUE ZERO COMP-3.              
014700 77  W-CL1-ART-EMBQ3          PIC 9(9).                                   
014800 77  W-RAD-KVINLART           PIC S9(7)      COMP-3 VALUE ZERO.           
014900 77  W-IDFTG                  PIC  9(2)      VALUE ZERO.                  
015000 77  WS-IDARTNR               PIC S9(9)   VALUE ZERO COMP-3.              
015100 77  WS-TIPRLIST              PIC 9(6)    VALUE ZERO.                     
015200 77  WS-KVAVROP               PIC S9(7)   COMP-3 VALUE ZERO.              
015300*     -- DAGENS-DATUM MED SEKEL-SIFFRA                                    
015400 01      WS-DAGENS-DATUM         PIC 9(8)    VALUE ZERO.                  
015500 01      FILLER REDEFINES WS-DAGENS-DATUM.                                
015600   03    WS-DAGENS-SEKEL         PIC 9(2).                                
015700   03    WS-IDAG                 PIC 9(6).                                
015800 EJECT                                                                    
015900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
016000 01  FILLER REDEFINES DAGENS-DATUM.                                       
016100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
016200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
016300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
016400                                                                          
016500 01  TEST-DATUM-1                PIC 9(6)    VALUE ZERO.                  
016600                                                                          
016700 01  TEST-DATUM-2                PIC 9(6)    VALUE ZERO.                  
016800 01  FILLER REDEFINES TEST-DATUM-2.                                       
016900     03  TEST-DATUM2-AAR         PIC 9(2).                                
017000     03  TEST-DATUM2-MAANAD      PIC 9(2).                                
017100     03  TEST-DATUM2-DAG         PIC 9(2).                                
017200     EJECT                                                                
017300* -COPY WY2000W1                                                          
017400 01  TMP1-YYMMDD-TEST            PIC 9(6).                                
017500 01  FILLER REDEFINES TMP1-YYMMDD-TEST.                                   
017600   03  TMP1-YY                   PIC 9(2).                                
017700   03  TMP1-MM                   PIC 9(2).                                
017800   03  TMP1-MM                   PIC 9(2).                                
017900     EJECT                                                                
018000 01  DYNAMISKA-SUBPROGRAM.                                                
018100*                                                                         
018200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
018300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
018600     03  WDAGKONV                PIC X(8)    VALUE 'WDATKONV'.            
018700     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
018800     SKIP2                                                                
018900*    --- COPY TEXT TILL WDATKONV                                          
019000*01  -COPY WDATAREA                                                       
019100     EJECT                                                                
019200*    --- COPY TEXT TILL WDAGKONV                                          
019300*01  -COPY WDAGAREA                                                       
019400     EJECT                                                                
019500*01  -COPY W510CURR                                                       
019600     EJECT                                                                
019700*    --- PARAMETRAR TILL ABEND                                            
019800                                                                          
019900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
020000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
020100     SKIP2                                                                
020200 01  FELTEXT.                                                             
020300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
020400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
020500     EJECT                                                                
020600                                                                          
020700 01  FILLER                      PIC X(16)   VALUE 'LAENK-AREOR'.         
020800*01  -COPY W611REG1                                                       
020900     EJECT                                                                
021000*01  -COPY W611REG2                                                       
021100     EJECT                                                                
021200*01  -COPY W611REG3                                                       
021300     EJECT                                                                
021400*01  -COPY W611REG4                                                       
021500     EJECT                                                                
021600*01  -COPY W611REG6                                                       
021700     EJECT                                                                
021800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021900*                                                                         
022000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022100     SKIP3                                                                
022200 01  NYCKLAR-TILL-DLI.                                                    
022300     03  W-W6D101KY-X.                                                    
022400         05  W-D101KY-IDDC       PIC X(2)     VALUE SPACE.                
022500         05  W-D101KY-IDLEVNR    PIC X(5)     VALUE SPACE.                
022600         05  W-D101KY-IDFS       PIC X(8)     VALUE SPACE.                
022700         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
022800                                                                          
022900     03  W-W6D1ASEQ-MIN-X.                                                
023000         05  W-D1ASEQ-IDDC-MIN      PIC X(2)  VALUE SPACE.                
023100         05  W-D1ASEQ-IDLEVNR-MIN   PIC X(5)  VALUE SPACE.                
023200         05  W-D1ASEQ-IDFS-MIN      PIC X(8)  VALUE SPACE.                
023300         05  W-D1ASEQ-TIAVIDAT-MIN  PIC S9(7) COMP-3 VALUE ZERO.          
023400         05  W-D1ASEQ-IDLBBET-MIN   PIC X(12) VALUE SPACE.                
023500                                                                          
023600     03  W-W6D1ASEQ-MAX-X.                                                
023700         05  W-D1ASEQ-IDDC-MAX      PIC X(2)  VALUE SPACE.                
023800         05  W-D1ASEQ-IDLEVNR-MAX   PIC X(5)  VALUE SPACE.                
023900         05  W-D1ASEQ-IDFS-MAX      PIC X(8)  VALUE SPACE.                
024000         05  W-D1ASEQ-TIAVIDAT-MAX  PIC S9(7) COMP-3 VALUE ZERO.          
024100         05  W-D1ASEQ-IDLBBET-MAX   PIC X(12) VALUE SPACE.                
024200                                                                          
024300     03  W-IDRADNR-INL-X.                                                 
024400         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
024500     03  W-IDRADNR-X.                                                     
024600         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
024700     03  W-IDARTNR-X.                                                     
024800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
024900     03  W-IDARTNR-K7-X.                                                  
025000         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
025100     03  W-WDD901KY-X.                                                    
025200         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
025300         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
025400     03  W-IDLEVNR-X.                                                     
025500         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
025600     03  W-IDLEVNR-21-X.                                                  
025700         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
025800     03  W-DAPRLIST-21-N.                                                 
025900         05  W-DAPRLIST-21       PIC 9(8)    VALUE ZERO.                  
026000     03  W-DAPRLIST-K7-N.                                                 
026100         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
026200     03  W-IDLEVNR-PR-X.                                                  
026300         05  W-IDLEVNR-PR        PIC X(5)    VALUE ZERO.                  
026400     03  W-KDSEGKEY-X.                                                    
026500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
026600     03  W-IDSKYLT-X.                                                     
026700         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
026800     03  W-IDDC-X.                                                        
026900         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
027000     03  W-KDAVROP-X.                                                     
027100         05  W-KDAVROP           PIC S9(1)    VALUE ZERO COMP-3.          
027200     03  W-IDLAND-X.                                                      
027300         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
027400     SKIP2                                                                
027500*    --- STATUS-KOD FRÅN IMS                                              
027600 01  STATUS-WS                   PIC XX.                                  
027700     88  SEGMENT-FINNS                       VALUE '  '.                  
027800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
028100     SKIP2                                                                
028200 01  GODK-STATUSKODER.                                                    
028300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028400     SKIP3                                                                
028500 01  SSA1                        PIC X(128).                              
028600 01  SSA2                        PIC X(64).                               
028700 01  SSA3                        PIC X(64).                               
028800     EJECT                                                                
028900*    --- IMS FUNKTIONSKODER                                               
029000*01  -COPY W0003                                                          
029100     EJECT                                                                
029200*    ---  DLI INPUT-OUTPUT AREA                                           
029300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
029400     SKIP3                                                                
029500 01  DLI-IO-AREA.                                                         
029600     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
029700     SKIP3                                                                
029800     03  W6INLA01 REDEFINES IO-AREA.                                      
029900*        05  -COPY W6D101                                                 
030000     EJECT                                                                
030100     03  W6INLA11 REDEFINES IO-AREA.                                      
030200*        05  -COPY W6D111                                                 
030300     EJECT                                                                
030400     03  W6INLA21 REDEFINES IO-AREA.                                      
030500*        05  -COPY W6D121                                                 
030600     EJECT                                                                
030700     03  WLLEVA01 REDEFINES IO-AREA.                                      
030800*        05  -COPY WDF101  -PRE LEVA01-                                   
030900     EJECT                                                                
031000     03  WLARTC01 REDEFINES IO-AREA.                                      
031100*        05  -COPY WDK601  -PRE ARTC-                                     
031200     EJECT                                                                
031300     03  WLARTC11 REDEFINES IO-AREA.                                      
031400*        05  -COPY WDK611  -PRE ARTC-                                     
031500     EJECT                                                                
031600     03  WLBENA11 REDEFINES IO-AREA.                                      
031700*        05  -COPY WDD311  -PRE BENA11-                                   
031800     EJECT                                                                
031900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
032000 01  DLI-IO-AREA-WDK711.                                                  
032100     03  WDK711.                                                          
032200*        05  -COPY WDK711                                                 
032300                                                                          
032400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK712'.         
032500 01  DLI-IO-AREA-WDK712.                                                  
032600     03  WDK712.                                                          
032700*        05  -COPY WDK712                                                 
032800                                                                          
032900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK724'.             
033000 01  DLI-IO-WDK724.                                                       
033100*        05  -COPY WDK724                                                 
033200                                                                          
033300     EJECT                                                                
033400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ARTC21'.         
033500     SKIP3                                                                
033600 01  DLI-IO-AREA-ARTC21.                                                  
033700     03  WLARTC21.                                                        
033800*        05  -COPY WDK621 -PRE ARTC-                                      
033900     EJECT                                                                
034000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD9'.           
034100     SKIP3                                                                
034200 01  DLI-IO-WDD9.                                                         
034300     03  WDD9.                                                            
034400*        05  -COPY WDD905                                                 
034500     SKIP3                                                                
034600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB6'.           
034700 01  DLI-IO-AREA-B601.                                                    
034800     03  WDB6.                                                            
034900*        05  -COPY WDB601                                                 
035000     EJECT                                                                
035100 LINKAGE SECTION.                                                         
035200                                                                          
035300*01  -COPY W611REG0                                                       
035400     SKIP3                                                                
035500*01  -COPY W0008  -PRE INLA1-                                             
035600     05  FILLER                  PIC X.                                   
035700     EJECT                                                                
035800*01  -COPY W0008  -PRE INLA2-                                             
035900     05  FILLER                  PIC X.                                   
036000     EJECT                                                                
036100*01  -COPY W0008  -PRE INLA3-                                             
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400*01  -COPY W0008  -PRE LEVA-                                              
036500     05  FILLER                  PIC X.                                   
036600     EJECT                                                                
036700*01  -COPY W0008  -PRE ARTC-                                              
036800     05  FILLER                  PIC X.                                   
036900     EJECT                                                                
037000*01  -COPY W0008  -PRE BENA-                                              
037100     05  FILLER                  PIC X.                                   
037200     EJECT                                                                
037300*01  -COPY W0008  -PRE WDD9-                                              
037400     05  FILLER                  PIC X.                                   
037500     EJECT                                                                
037600*01  -COPY W0008  -PRE WDK7-                                              
037700     05  FILLER                  PIC X.                                   
037800     EJECT                                                                
037900*01  -COPY W0008  -PRE WDB6-                                              
038000     05  FILLER                  PIC X.                                   
038100     EJECT                                                                
038200*01  -COPY W0008  -PRE 9305-                                              
038300     05  FILLER                  PIC X(30).                               
038400     EJECT                                                                
038500 PROCEDURE DIVISION  USING LAENK-W611REG0 INLA1-PCB INLA2-PCB             
038600                   INLA3-PCB LEVA-PCB ARTC-PCB BENA-PCB                   
038700                   WDD9-PCB  WDK7-PCB WDB6-PCB 9305-PCB.                  
038800*------------------------                                                 
038900     SKIP2                                                                
039000     PERFORM A-INIT                                                       
039100     PERFORM B-FLYTTA-LAENKAREA                                           
039200     PERFORM C-KOLLA-HUVUD                                                
039300     PERFORM D-KOLLA-RADER                                                
039400     IF ALLT-OK                                                           
039500         PERFORM E-UPPDATERA-INLA                                         
039600     END-IF                                                               
039700                                                                          
039800     PERFORM F-FLYTTA-LAENKAREA                                           
039900                                                                          
040000     MOVE ZERO TO RETURN-CODE                                             
040100     GOBACK                                                               
040200     .                                                                    
040300     EJECT                                                                
040400 A-INIT SECTION.                                                          
040500     SKIP2                                                                
040600     MOVE SPACE                TO REG-IDLBBET                             
040700                                  REG-FLGODK                              
040800                                  REG-FLGODK-IDFS                         
040900                                  REG-FLGODK-IDARTNR                      
041000                                  REG-IDTRANS                             
041100                                  REG-IDDC                                
041200                                  REG-IDANALYS                            
041300                                  REG-IDKST                               
041400     MOVE ZERO                 TO REG-IDFTG                               
041500                                  REG-IDKONTO                             
041600     MOVE +1                   TO REG-IX                                  
041700     PERFORM UNTIL REG-IX      >  MAX-REG                                 
041800         MOVE JA               TO REG-IDARTNR-OK     (REG-IX)             
041900                                  REG-IDFS           (REG-IX)             
042000         MOVE ZERO             TO REG-KDBEH          (REG-IX)             
042100                                  REG-IDARTNR-NY     (REG-IX)             
042200                                  REG-IDARTNR-GAMMAL (REG-IX)             
042300                                  REG-IDRADNR-INL-GAMMAL (REG-IX)         
042400                                  REG-KVAVIS         (REG-IX)             
042500                                  REG-KDRT           (REG-IX)             
042600                                  REG-TIAVIDAT       (REG-IX)             
042700                                  REG-VKART          (REG-IX)             
042800                                  REG-VLARTNTO       (REG-IX)             
042900                                  REG-ADLAGOMR       (REG-IX)             
043000                                  REG-ADGANG         (REG-IX)             
043100                                  REG-ADPLATS        (REG-IX)             
043200                                  REG-BEFT           (REG-IX)             
043300                                  REG-IDFKNGRP       (REG-IX)             
043400                                  REG-PRARTSTD       (REG-IX)             
043500                                  REG-KDFARLIG       (REG-IX)             
043600                                  REG-KVMP           (REG-IX)             
043700         MOVE SPACE            TO REG-IDFS           (REG-IX)             
043800                                  REG-IDLEVNR        (REG-IX)             
043900                                  REG-FLFSP          (REG-IX)             
044000                                  REG-BEART          (REG-IX)             
044100                                  REG-KDSORT         (REG-IX)             
044200                                  REG-CL1-KDLAGEMB   (REG-IX)             
044300                                  REG-IDMFSFEL       (REG-IX)             
044400                                  REG-KDARTURS       (REG-IX)             
044500         ADD +1                TO REG-IX                                  
044600     END-PERFORM                                                          
044700                                                                          
044800     MOVE JA                   TO TIAVIDAT-SW                             
044900     MOVE NEJ                  TO FS-AKTIVERAD-SW                         
045000                                  FLFEL-AENDRAD-SW                        
045100     ACCEPT DAGENS-DATUM  FROM DATE                                       
045200     .                                                                    
045300     EJECT                                                                
045400 B-FLYTTA-LAENKAREA  SECTION.                                             
045500                                                                          
045600     EVALUATE LAENK-IDTRANS                                               
045700                                                                          
045800         WHEN '6111'                                                      
045900            PERFORM BA-FLYTTA-W6111-AREA                                  
046000                                                                          
046100         WHEN '6112'                                                      
046200            PERFORM BB-FLYTTA-W6112-AREA                                  
046300                                                                          
046400         WHEN '6113'                                                      
046500            PERFORM BC-FLYTTA-W6113-AREA                                  
046600                                                                          
046700         WHEN '6114'                                                      
046800            PERFORM BD-FLYTTA-W6114-AREA                                  
046900                                                                          
047000         WHEN '6116'                                                      
047100            PERFORM BE-FLYTTA-W6116-AREA                                  
047200                                                                          
047300     END-EVALUATE                                                         
047400*    --- ORDNINGSTÄLLER  RÄTT DC FÖR ALLA IMS-LÄSNINGAR                   
047500                                                                          
047600     MOVE REG-IDDC  TO W-D1ASEQ-IDDC-MIN                                  
047700                       W-D1ASEQ-IDDC-MAX                                  
047800                       W-D101KY-IDDC                                      
047900                       WS-IDDC                                            
048000     .                                                                    
048100     EJECT                                                                
048200 BA-FLYTTA-W6111-AREA  SECTION.                                           
048300                                                                          
048400     MOVE LAENK-W611REG0             TO REG1-W611REG1                     
048500     MOVE REG1-KVRADER-MAX           TO MAX-REG1                          
048600                                                                          
048700     MOVE REG1-IDTRANS               TO REG-IDTRANS                       
048800     MOVE REG1-IDDC                  TO REG-IDDC                          
048900     MOVE REG1-IDLBBET               TO REG-IDLBBET                       
049000     MOVE REG1-FLGODK                TO REG-FLGODK                        
049100     MOVE +1                         TO REG-IX                            
049200                                        REG1-IX                           
049300     PERFORM UNTIL REG1-IX           >  MAX-REG1                          
049400         MOVE REG1-IDLEVNR           TO REG-IDLEVNR       (REG-IX)        
049500         MOVE REG1-IDARTNR (REG1-IX) TO REG-IDARTNR-NY    (REG-IX)        
049600         MOVE REG1-KVAVIS  (REG1-IX) TO REG-KVAVIS        (REG-IX)        
049700         MOVE REG1-TIAVIDAT          TO REG-TIAVIDAT      (REG-IX)        
049800         MOVE REG1-KDRT              TO REG-KDRT          (REG-IX)        
049900         MOVE REG1-IDFS              TO REG-IDFS          (REG-IX)        
050000         ADD +1                      TO REG-IX                            
050100                                        REG1-IX                           
050200     END-PERFORM                                                          
050300     .                                                                    
050400     EJECT                                                                
050500 BB-FLYTTA-W6112-AREA  SECTION.                                           
050600                                                                          
050700     MOVE LAENK-W611REG0             TO REG2-W611REG2                     
050800                                                                          
050900     MOVE REG2-IDTRANS               TO REG-IDTRANS                       
051000     MOVE REG2-IDDC                  TO REG-IDDC                          
051100     MOVE REG2-IDLBBET               TO REG-IDLBBET                       
051200     MOVE REG2-FLGODK                TO REG-FLGODK                        
051300     MOVE +1                         TO REG-IX                            
051400                                        REG2-IX                           
051500     PERFORM UNTIL REG2-IX           >  MAX-REG2                          
051600         MOVE REG2-IDLEVNR           TO REG-IDLEVNR       (REG-IX)        
051700         MOVE REG2-IDARTNR (REG2-IX) TO REG-IDARTNR-NY    (REG-IX)        
051800         MOVE REG2-KVAVIS  (REG2-IX) TO REG-KVAVIS        (REG-IX)        
051900         MOVE REG2-KDRT              TO REG-KDRT          (REG-IX)        
052000         MOVE REG2-TIAVIDAT          TO REG-TIAVIDAT      (REG-IX)        
052100         MOVE REG2-IDFS    (REG2-IX) TO REG-IDFS          (REG-IX)        
052200         ADD +1                      TO REG-IX                            
052300                                        REG2-IX                           
052400     END-PERFORM                                                          
052500     .                                                                    
052600     EJECT                                                                
052700 BC-FLYTTA-W6113-AREA  SECTION.                                           
052800                                                                          
052900     MOVE LAENK-W611REG0             TO REG3-W611REG3                     
053000                                                                          
053100     MOVE REG3-IDTRANS               TO REG-IDTRANS                       
053200     MOVE REG3-IDDC                  TO REG-IDDC                          
053300     MOVE REG3-IDLBBET               TO REG-IDLBBET                       
053400     MOVE REG3-FLGODK                TO REG-FLGODK                        
053500     MOVE +1                         TO REG-IX                            
053600                                        REG3-IX                           
053700     PERFORM UNTIL REG3-IX           >  MAX-REG3                          
053800         MOVE REG3-IDLEVNR (REG3-IX) TO REG-IDLEVNR       (REG-IX)        
053900         MOVE REG3-TIAVIDAT(REG3-IX) TO REG-TIAVIDAT      (REG-IX)        
054000         MOVE REG3-IDARTNR (REG3-IX) TO REG-IDARTNR-NY    (REG-IX)        
054100         MOVE REG3-KVAVIS  (REG3-IX) TO REG-KVAVIS        (REG-IX)        
054200         MOVE REG3-KDRT    (REG3-IX) TO REG-KDRT          (REG-IX)        
054300         MOVE REG3-IDFS    (REG3-IX) TO REG-IDFS          (REG-IX)        
054400         ADD +1                      TO REG-IX                            
054500                                        REG3-IX                           
054600     END-PERFORM                                                          
054700     .                                                                    
054800     EJECT                                                                
054900 BD-FLYTTA-W6114-AREA  SECTION.                                           
055000                                                                          
055100     MOVE LAENK-W611REG0             TO REG4-W611REG4                     
055200     MOVE REG4-KVRADER-MAX           TO MAX-REG4                          
055300                                                                          
055400     MOVE REG4-IDTRANS               TO REG-IDTRANS                       
055500     MOVE REG4-IDDC                  TO REG-IDDC                          
055600     MOVE SPACE                      TO REG-IDLBBET                       
055700                                        REG-FLGODK-IDFS                   
055800     MOVE REG4-IDFTG                 TO REG-IDFTG                         
055900     MOVE REG4-IDKONTO               TO REG-IDKONTO                       
056000     MOVE REG4-IDANALYS              TO REG-IDANALYS                      
056100     MOVE REG4-IDKST                 TO REG-IDKST                         
056200     MOVE +1                         TO REG-IX                            
056300                                        REG4-IX                           
056400     PERFORM UNTIL REG4-IX           >  MAX-REG4                          
056500         MOVE REG4-KDBEH (REG-IX)    TO REG-KDBEH         (REG-IX)        
056600         MOVE REG4-IDLEVNR           TO REG-IDLEVNR       (REG-IX)        
056700         MOVE REG4-TIAVIDAT          TO REG-TIAVIDAT      (REG-IX)        
056800         MOVE REG4-IDFS              TO REG-IDFS          (REG-IX)        
056900         MOVE REG4-IDARTNR-NY (REG4-IX)                                   
057000                                     TO REG-IDARTNR-NY    (REG-IX)        
057100         MOVE REG4-IDARTNR-GAMMAL (REG4-IX)                               
057200                                     TO REG-IDARTNR-GAMMAL(REG-IX)        
057300         MOVE REG4-IDRADNR-INL-GAMMAL (REG4-IX)                           
057400                                 TO REG-IDRADNR-INL-GAMMAL(REG-IX)        
057500         MOVE REG4-KVAVIS  (REG4-IX) TO REG-KVAVIS        (REG-IX)        
057600         MOVE REG4-KDRT    (REG4-IX) TO REG-KDRT          (REG-IX)        
057700         ADD +1                      TO REG-IX                            
057800                                        REG4-IX                           
057900     END-PERFORM                                                          
058000     .                                                                    
058100     EJECT                                                                
058200 BE-FLYTTA-W6116-AREA  SECTION.                                           
058300                                                                          
058400     MOVE LAENK-W611REG0             TO REG6-W611REG6                     
058500     MOVE REG6-KVRADER-MAX           TO MAX-REG6                          
058600                                                                          
058700     MOVE REG6-IDTRANS               TO REG-IDTRANS                       
058800     MOVE REG6-IDDC                  TO REG-IDDC                          
058900     MOVE REG6-IDLBBET               TO REG-IDLBBET                       
059000     MOVE REG6-FLGODK                TO REG-FLGODK                        
059100     MOVE REG6-IDFTG                 TO REG-IDFTG                         
059200     MOVE REG6-IDKONTO               TO REG-IDKONTO                       
059300     MOVE REG6-IDANALYS              TO REG-IDANALYS                      
059400     MOVE REG6-IDKST                 TO REG-IDKST                         
059500     MOVE +1                         TO REG-IX                            
059600                                        REG6-IX                           
059700     PERFORM UNTIL REG6-IX           >  MAX-REG6                          
059800         MOVE REG6-IDLEVNR           TO REG-IDLEVNR       (REG-IX)        
059900         MOVE REG6-IDARTNR (REG6-IX) TO REG-IDARTNR-NY    (REG-IX)        
060000         MOVE REG6-IDFS              TO REG-IDFS          (REG-IX)        
060100         MOVE REG6-KVAVIS  (REG6-IX) TO REG-KVAVIS        (REG-IX)        
060200         MOVE REG6-TIAVIDAT          TO REG-TIAVIDAT      (REG-IX)        
060300         MOVE REG6-KDRT              TO REG-KDRT          (REG-IX)        
060400         ADD +1                      TO REG-IX                            
060500                                        REG6-IX                           
060600     END-PERFORM                                                          
060700     .                                                                    
060800     EJECT                                                                
060900 C-KOLLA-HUVUD        SECTION.                                            
061000                                                                          
061100     IF REG-IDTRANS            = '6114'                                   
061200         CONTINUE                                                         
061300      ELSE                                                                
061400         PERFORM CA-KOLLA-IDLEVNR                                         
061500         PERFORM CB-KOLLA-IDFS                                            
061600         PERFORM CC-KOLLA-TIAVIDAT                                        
061700         PERFORM CD-KOLLA-IDLBBET                                         
061800     END-IF                                                               
061900     PERFORM CE-KOLLA-KDRT                                                
062000     .                                                                    
062100     EJECT                                                                
062200 CA-KOLLA-IDLEVNR     SECTION.                                            
062300                                                                          
062400     EVALUATE REG-IDTRANS                                                 
062500       WHEN '6111'                                                        
062600           MOVE REG-IDLEVNR (1) TO  W-IDLEVNR                             
062700           PERFORM S01-KOLLA-IDLEVNR                                      
062800           IF SEGMENT-SAKNAS OR W-IDLEVNR = '1002 ' OR '9998 '            
062900               MOVE NEJ         TO ALLT-SW                                
063000                                   REG1-IDLEVNR-OK                        
063100           END-IF                                                         
063200                                                                          
063300       WHEN '6112'                                                        
063400           MOVE REG-IDLEVNR (1) TO  W-IDLEVNR                             
063500           PERFORM S01-KOLLA-IDLEVNR                                      
063600           IF SEGMENT-SAKNAS OR W-IDLEVNR = '1002 ' OR '9998 '            
063700               MOVE NEJ         TO ALLT-SW                                
063800                                   REG2-IDLEVNR-OK                        
063900           END-IF                                                         
064000                                                                          
064100       WHEN '6113'                                                        
064200           MOVE +1                            TO REG-IX                   
064300           PERFORM UNTIL REG-IX               > MAX-REG3                  
064400               IF REG-IDARTNR-NY (REG-IX)     > ZERO                      
064500                   MOVE REG-IDLEVNR  (REG-IX) TO  W-IDLEVNR               
064600                   PERFORM S01-KOLLA-IDLEVNR                              
064700                   IF SEGMENT-SAKNAS                                      
064800                   OR W-IDLEVNR = '1002 ' OR '9998 '                      
064900                       MOVE NEJ    TO ALLT-SW                             
065000                                      REG3-IDLEVNR-OK (REG-IX)            
065100                   END-IF                                                 
065200               END-IF                                                     
065300               ADD +1          TO REG-IX                                  
065400           END-PERFORM                                                    
065500                                                                          
065600       WHEN '6116'                                                        
065700           MOVE REG-IDLEVNR (1) TO  W-IDLEVNR                             
065800           PERFORM S01-KOLLA-IDLEVNR                                      
065900           IF SEGMENT-SAKNAS OR W-IDLEVNR = '1002 ' OR '9998 '            
066000               MOVE NEJ         TO ALLT-SW                                
066100                                   REG6-IDLEVNR-OK                        
066200           END-IF                                                         
066300     END-EVALUATE                                                         
066400     .                                                                    
066500     EJECT                                                                
066600 CB-KOLLA-IDFS        SECTION.                                            
066700                                                                          
066800     EVALUATE REG-IDTRANS                                                 
066900        WHEN '6111'                                                       
067000           IF REG-IDFS (1)     =  SPACE                                   
067100               MOVE NEJ        TO REG1-IDFS-OK                            
067200                                  ALLT-SW                                 
067300            ELSE                                                          
067400               MOVE +1           TO REG-IX                                
067500               PERFORM S02-KOLLA-INLAREG                                  
067600               IF SEGMENT-FINNS                                           
067700                   MOVE NEJ      TO REG1-IDFS-OK                          
067800                                    REG-IDFS-OK (1)                       
067900                   MOVE JA       TO REG-FLGODK-IDFS                       
068000                   IF REG-FLGODK =  SPACE                                 
068100                       MOVE NEJ  TO ALLT-SW                               
068200                   END-IF                                                 
068300               END-IF                                                     
068400            END-IF                                                        
068500                                                                          
068600        WHEN '6112'                                                       
068700           PERFORM CBBA-KOLLA-6112-IDFS                                   
068800                                                                          
068900        WHEN '6113'                                                       
069000           PERFORM CBBB-KOLLA-6113-IDFS                                   
069100                                                                          
069200        WHEN '6116'                                                       
069300           IF REG-IDFS (1)     =  SPACE                                   
069400               MOVE NEJ        TO REG6-IDFS-OK                            
069500                                  ALLT-SW                                 
069600            ELSE                                                          
069700               MOVE +1           TO REG-IX                                
069800               PERFORM S02-KOLLA-INLAREG                                  
069900               IF SEGMENT-FINNS                                           
070000                   MOVE NEJ      TO REG6-IDFS-OK                          
070100                                    REG-IDFS-OK (1)                       
070200                   MOVE JA       TO REG-FLGODK-IDFS                       
070300                   IF REG-FLGODK =  SPACE                                 
070400                       MOVE NEJ  TO ALLT-SW                               
070500                   END-IF                                                 
070600               END-IF                                                     
070700            END-IF                                                        
070800     END-EVALUATE                                                         
070900                                                                          
071000     IF FS-AKTIVERAD                                                      
071100         MOVE NEJ              TO ALLT-SW                                 
071200         MOVE NEJ              TO REG-FLGODK-IDFS                         
071300     END-IF                                                               
071400     .                                                                    
071500     EJECT                                                                
071600 CBBA-KOLLA-6112-IDFS  SECTION.                                           
071700                                                                          
071800     MOVE +1                            TO REG-IX                         
071900     PERFORM UNTIL REG-IX               >  MAX-REG2                       
072000         IF  REG-IDARTNR-NY (REG-IX)    >  ZERO                           
072100             IF REG-IDFS (REG-IX)       =  SPACE                          
072200                 MOVE NEJ               TO REG2-IDFS-OK (REG-IX)          
072300                                           ALLT-SW                        
072400              ELSE                                                        
072500                 PERFORM S02-KOLLA-INLAREG                                
072600                 IF SEGMENT-FINNS                                         
072700                     MOVE NEJ           TO REG2-IDFS-OK (REG-IX)          
072800                                           REG-IDFS-OK  (REG-IX)          
072900                     MOVE JA            TO REG-FLGODK-IDFS                
073000                     IF REG-FLGODK      =  SPACE                          
073100                         MOVE NEJ       TO ALLT-SW                        
073200                     END-IF                                               
073300                 END-IF                                                   
073400             END-IF                                                       
073500         END-IF                                                           
073600         ADD +1                TO REG-IX                                  
073700     END-PERFORM                                                          
073800     .                                                                    
073900     EJECT                                                                
074000 CBBB-KOLLA-6113-IDFS  SECTION.                                           
074100                                                                          
074200     MOVE +1                            TO REG-IX                         
074300     PERFORM UNTIL REG-IX               >  MAX-REG3                       
074400         IF  REG-IDARTNR-NY (REG-IX)    >  ZERO                           
074500             IF REG-IDFS (REG-IX)       =  SPACE                          
074600                 MOVE NEJ               TO REG3-IDFS (REG-IX)             
074700                                           ALLT-SW                        
074800              ELSE                                                        
074900                 PERFORM S02-KOLLA-INLAREG                                
075000                 IF SEGMENT-FINNS                                         
075100                     MOVE NEJ           TO REG-IDFS-OK  (REG-IX)          
075200                                           REG3-IDFS-OK (REG-IX)          
075300                     MOVE JA            TO REG-FLGODK-IDFS                
075400                     IF REG-FLGODK      =  SPACE                          
075500                         MOVE NEJ       TO ALLT-SW                        
075600                     END-IF                                               
075700                 END-IF                                                   
075800             END-IF                                                       
075900         END-IF                                                           
076000         ADD +1                TO REG-IX                                  
076100     END-PERFORM                                                          
076200     .                                                                    
076300     EJECT                                                                
076400 CC-KOLLA-TIAVIDAT    SECTION.                                            
076500*   I FÄLT TMP1-YYMMDD   LIGGER DAGENS-DATUM                              
076600*          TMP2-YYMMDD   LIGGER DAGENS-DATUM MINUS 365 DAGAR              
076700*                        DVS ETT ÅR BAKÅT                                 
076800*          TMP3-YYMMDD   LIGGER STANSAD DATUM (TIAVIDAT) FRÅN             
076900*                        BILDEN                                           
077000                                                                          
077100     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
077200                                                                          
077300     MOVE DAGENS-DATUM         TO TMP1-YYMMDD                             
077400                                                                          
077500     MOVE REG-TIAVIDAT (1)     TO TMP3-YYMMDD                             
077600                                                                          
077700     EVALUATE REG-IDTRANS                                                 
077800        WHEN '6111'                                                       
077900            PERFORM  WY2000Q1                                             
078000            COMPUTE TMP2-YYMMDD = TMP1-YYMMDD - 10000                     
078100            END-COMPUTE                                                   
078200            MOVE REG-TIAVIDAT (1)  TO DAT-I-TIDATUM                       
078300            PERFORM S06-CALL-WDATKONV                                     
078400            IF (TMP3-YYMMDD     > TMP1-YYMMDD)    OR                      
078500               (TMP3-YYMMDD     < TMP2-YYMMDD)    OR                      
078600               TIAVIDAT-FEL                                               
078700                MOVE NEJ       TO ALLT-SW                                 
078800                                  REG1-TIAVIDAT-OK                        
078900            END-IF                                                        
079000                                                                          
079100        WHEN '6112'                                                       
079200            PERFORM WY2000Q1                                              
079300            COMPUTE TMP2-YYMMDD = TMP1-YYMMDD - 10000                     
079400            END-COMPUTE                                                   
079500            MOVE REG-TIAVIDAT (1)  TO DAT-I-TIDATUM                       
079600            PERFORM S06-CALL-WDATKONV                                     
079700            IF TMP3-YYMMDD     > TMP1-YYMMDD      OR                      
079800               TMP3-YYMMDD     < TMP2-YYMMDD      OR                      
079900               TIAVIDAT-FEL                                               
080000                MOVE NEJ       TO ALLT-SW                                 
080100                                  REG2-TIAVIDAT-OK                        
080200            END-IF                                                        
080300                                                                          
080400        WHEN '6113'                                                       
080500            PERFORM CCXA-KOLLA-6113-TIAVIDAT                              
080600                                                                          
080700        WHEN '6116'                                                       
080800            PERFORM WY2000Q1                                              
080900            COMPUTE TMP2-YYMMDD = TMP1-YYMMDD - 10000                     
081000            END-COMPUTE                                                   
081100            MOVE REG-TIAVIDAT (1)  TO DAT-I-TIDATUM                       
081200            PERFORM S06-CALL-WDATKONV                                     
081300            IF TMP3-YYMMDD     > TMP1-YYMMDD      OR                      
081400               TMP3-YYMMDD     < TMP2-YYMMDD      OR                      
081500               TIAVIDAT-FEL                                               
081600                MOVE NEJ       TO ALLT-SW                                 
081700                                  REG6-TIAVIDAT-OK                        
081800            END-IF                                                        
081900     END-EVALUATE                                                         
082000     .                                                                    
082100     EJECT                                                                
082200 CCXA-KOLLA-6113-TIAVIDAT    SECTION.                                     
082300                                                                          
082400     MOVE +1                            TO REG-IX                         
082500     PERFORM UNTIL REG-IX               >  MAX-REG3                       
082600         IF REG-IDARTNR-NY (REG-IX)     >  ZERO                           
082700             MOVE REG-TIAVIDAT (REG-IX) TO DAT-I-TIDATUM                  
082800                                           TMP3-YYMMDD                    
082900             MOVE DAGENS-DATUM          TO TMP1-YYMMDD                    
083000             PERFORM WY2000Q1                                             
083100             COMPUTE TMP2-YYMMDD = TMP1-YYMMDD - 10000                    
083200             END-COMPUTE                                                  
083300             PERFORM S06-CALL-WDATKONV                                    
083400             IF TMP3-YYMMDD             >  TMP1-YYMMDD          OR        
083500                TMP3-YYMMDD             <  TMP2-YYMMDD          OR        
083600                TIAVIDAT-FEL                                              
083700                 MOVE NEJ               TO ALLT-SW                        
083800                                          REG3-TIAVIDAT-OK(REG-IX)        
083900             END-IF                                                       
084000         END-IF                                                           
084100         ADD +1                TO REG-IX                                  
084200     END-PERFORM                                                          
084300     .                                                                    
084400     EJECT                                                                
084500 CD-KOLLA-IDLBBET     SECTION.                                            
084600                                                                          
084700     IF REG-IDLBBET            = SPACE                                    
084800        IF NDC                                                            
084900           MOVE 'TRPC3'            TO REG-IDLBBET                         
085000        ELSE                                                              
085100           IF CDC-TR                                                      
085200             MOVE 'TRPC2'          TO REG-IDLBBET                         
085300           ELSE                                                           
085400             MOVE 'TRPC1'          TO REG-IDLBBET                         
085500           END-IF                                                         
085600        END-IF                                                            
085700     END-IF                                                               
085800     .                                                                    
085900     EJECT                                                                
086000 CE-KOLLA-KDRT        SECTION.                                            
086100                                                                          
086200     MOVE JA                   TO KDRT-SW                                 
086300     EVALUATE REG-IDTRANS                                                 
086400       WHEN '6111'                                                        
086500         PERFORM CEA-KOLLA-KDRT-6111                                      
086600                                                                          
086700       WHEN '6112'                                                        
086800         PERFORM CEB-KOLLA-KDRT-6112                                      
086900                                                                          
087000       WHEN '6113'                                                        
087100         PERFORM CEC-KOLLA-KDRT-6113                                      
087200                                                                          
087300       WHEN '6114'                                                        
087400         PERFORM CED-KOLLA-KDRT-6114                                      
087500                                                                          
087600       WHEN '6116'                                                        
087700         PERFORM CEE-KOLLA-KDRT-6116                                      
087800     END-EVALUATE                                                         
087900                                                                          
088000     IF KDRT-FEL                                                          
088100         MOVE NEJ              TO ALLT-SW                                 
088200     END-IF                                                               
088300     .                                                                    
088400     EJECT                                                                
088500 CEA-KOLLA-KDRT-6111  SECTION.                                            
088600                                                                          
088700     MOVE REG1-KDRT            TO W-KDRT                                  
088800     MOVE REG1-IDLEVNR         TO W-SPAR-IDLEVNR                          
088900     PERFORM S03-KOLLA-KDRT-IDLEVNR                                       
089000     IF KDRT-FEL                                                          
089100         MOVE NEJ              TO REG1-KDRT-OK                            
089200     END-IF                                                               
089300     .                                                                    
089400     EJECT                                                                
089500 CEB-KOLLA-KDRT-6112  SECTION.                                            
089600                                                                          
089700     MOVE REG2-KDRT            TO W-KDRT                                  
089800     MOVE REG2-IDLEVNR         TO W-SPAR-IDLEVNR                          
089900     PERFORM S03-KOLLA-KDRT-IDLEVNR                                       
090000     IF KDRT-FEL                                                          
090100         MOVE NEJ              TO REG2-KDRT-OK                            
090200     END-IF                                                               
090300     .                                                                    
090400     EJECT                                                                
090500 CEC-KOLLA-KDRT-6113  SECTION.                                            
090600                                                                          
090700     MOVE +1                           TO REG-IX                          
090800     PERFORM UNTIL REG-IX              >  MAX-REG3                        
090900         IF REG-IDARTNR-NY(REG-IX)     > ZERO                             
091000             MOVE REG-KDRT(REG-IX)     TO W-KDRT                          
091100             MOVE REG-IDLEVNR(REG-IX)  TO W-SPAR-IDLEVNR                  
091200             PERFORM S03-KOLLA-KDRT-IDLEVNR                               
091300             IF KDRT-FEL                                                  
091400                 MOVE NEJ              TO REG3-KDRT-OK(REG-IX)            
091500             END-IF                                                       
091600         END-IF                                                           
091700         ADD +1                        TO REG-IX                          
091800     END-PERFORM                                                          
091900     .                                                                    
092000     EJECT                                                                
092100 CED-KOLLA-KDRT-6114  SECTION.                                            
092200                                                                          
092300     MOVE +1                           TO REG-IX                          
092400     PERFORM UNTIL REG-IX              >  MAX-REG4                        
092500         IF REG-IDARTNR-NY(REG-IX)     > ZERO AND                         
092600            REG-KDRT      (REG-IX)     NOT = 99                           
092700             MOVE REG-KDRT(REG-IX)     TO W-KDRT                          
092800             MOVE REG-IDLEVNR(REG-IX)  TO W-SPAR-IDLEVNR                  
092900             PERFORM CEDA-KOLLA-KDRT-IDLEVNR                              
093000             IF KDRT-FEL                                                  
093100                 MOVE NEJ              TO REG4-KDRT-OK(REG-IX)            
093200             END-IF                                                       
093300         END-IF                                                           
093400         ADD +1                        TO REG-IX                          
093500     END-PERFORM                                                          
093600     .                                                                    
093700     EJECT                                                                
093800 CEDA-KOLLA-KDRT-IDLEVNR  SECTION.                                        
093900                                                                          
094000     IF REG-IDKONTO            > ZERO OR                                  
094100        REG-IDANALYS           > ZERO OR                                  
094200        REG-IDKST              > SPACE                                    
094300         PERFORM S04-KOLLA-KDRT-IDLEVNR                                   
094400      ELSE                                                                
094500         PERFORM S03-KOLLA-KDRT-IDLEVNR                                   
094600     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900 CEE-KOLLA-KDRT-6116  SECTION.                                            
095000                                                                          
095100     MOVE REG6-KDRT            TO W-KDRT                                  
095200     MOVE REG6-IDLEVNR         TO W-SPAR-IDLEVNR                          
095300     PERFORM S04-KOLLA-KDRT-IDLEVNR                                       
095400     IF KDRT-FEL                                                          
095500         MOVE NEJ              TO REG6-KDRT-OK                            
095600     END-IF                                                               
095700     .                                                                    
095800     EJECT                                                                
095900 D-KOLLA-RADER        SECTION.                                            
096000                                                                          
096100     MOVE ZERO                 TO W-IDFTG                                 
096200                                                                          
096300     MOVE +1                   TO REG-IX                                  
096400     PERFORM UNTIL             REG-IX > MAX-REG                           
096500         IF REG-IDARTNR-NY(REG-IX)    = ZERO OR                           
096600            REG-KDBEH     (REG-IX)    = 2                                 
096700             CONTINUE                                                     
096800          ELSE                                                            
096900             MOVE REG-IDARTNR-NY (REG-IX)  TO W-IDARTNR                   
097000                                              W-IDARTNR-K7                
097100                                                                          
097200             PERFORM DB-KOLLA-ARTIKEL-REGISTER                            
097300             IF (REG-IDARTNR-NY     (REG-IX) =                            
097400                 REG-IDARTNR-GAMMAL (REG-IX))                             
097500                 CONTINUE                                                 
097600              ELSE                                                        
097700                 IF IDMFSFEL = SPACE                                      
097800                   PERFORM DC-KOLLA-INLEV-REGISTER                        
097900                 END-IF                                                   
098000             END-IF                                                       
098100         END-IF                                                           
098200         ADD +1                TO REG-IX                                  
098300     END-PERFORM                                                          
098400     .                                                                    
098500     EJECT                                                                
098600 DB-KOLLA-ARTIKEL-REGISTER    SECTION.                                    
098700                                                                          
098800     MOVE JA                   TO IDARTNR-SW                              
098900     MOVE SPACE                TO IDMFSFEL                                
099000                                                                          
099100     IF NDC                                                               
099200         MOVE REG-IDDC  TO W-IDDC                                         
099300         PERFORM IMS-GU-WDK711                                            
099400         IF SEGMENT-SAKNAS                                                
099500             MOVE NEJ     TO IDARTNR-SW                                   
099600             MOVE '017'   TO IDMFSFEL                                     
099700         END-IF                                                           
099800     END-IF                                                               
099900                                                                          
100000     IF IDARTNR-OK                                                        
100100       PERFORM DBB-KOLLA-WLARTC                                           
100200       IF IDARTNR-OK                                                      
100300         PERFORM DBC-KOLLA-WLBENA                                         
100400       END-IF                                                             
100500     END-IF                                                               
100600                                                                          
100700     IF IDARTNR-FEL                                                       
100800         EVALUATE REG-IDTRANS                                             
100900           WHEN '6111'                                                    
101000             MOVE NEJ          TO REG1-IDARTNR-OK (REG-IX)                
101100             MOVE IDMFSFEL     TO REG1-IDMFSFEL   (REG-IX)                
101200                                                                          
101300           WHEN '6112'                                                    
101400             MOVE NEJ          TO REG2-IDARTNR-OK (REG-IX)                
101500             MOVE IDMFSFEL     TO REG2-IDMFSFEL   (REG-IX)                
101600                                                                          
101700           WHEN '6113'                                                    
101800             MOVE NEJ          TO REG3-IDARTNR-OK (REG-IX)                
101900             MOVE IDMFSFEL     TO REG3-IDMFSFEL   (REG-IX)                
102000                                                                          
102100           WHEN '6114'                                                    
102200             MOVE NEJ          TO REG4-IDARTNR-OK (REG-IX)                
102300             MOVE IDMFSFEL     TO REG4-IDMFSFEL   (REG-IX)                
102400                                                                          
102500           WHEN '6116'                                                    
102600             MOVE NEJ          TO REG6-IDARTNR-OK (REG-IX)                
102700             MOVE IDMFSFEL     TO REG6-IDMFSFEL   (REG-IX)                
102800                                                                          
102900         END-EVALUATE                                                     
103000         MOVE NEJ              TO ALLT-SW                                 
103100         MOVE NEJ              TO REG-FLGODK-IDARTNR                      
103200                                  REG-FLGODK-IDFS                         
103300     END-IF                                                               
103400     .                                                                    
103500     EJECT                                                                
103600 DBB-KOLLA-WLARTC           SECTION.                                      
103700                                                                          
103800     PERFORM IMS-GU-ARTC01                                                
103900     IF SEGMENT-FINNS                                                     
104000         IF W-IDFTG                    =  ZERO                            
104100             MOVE ARTC-ART-IDFTG         TO W-IDFTG                       
104200             MOVE ARTC-ART-IDARTNR       TO WS-IDARTNR                    
104300             MOVE ARTC-ART-KDPRODSL      TO TEST-KDPRODSL                 
104400             MOVE REG-IDLEVNR(REG-IX)  TO W-D101KY-IDLEVNR                
104500             MOVE REG-IDFS (REG-IX)    TO W-D101KY-IDFS                   
104600             MOVE REG-TIAVIDAT(REG-IX) TO W-D101KY-TIAVIDAT               
104700             PERFORM IMS-GU-INLA3-INLA01                                  
104800             IF SEGMENT-FINNS                                             
104900               PERFORM IMS-GNP-INLA3-INLA11-OKVAL                         
105000***FÖRSTA ARTIKEL PÅ FS BESTÄMMER IDFTG                                   
105100***FÖLJANDE FÖR ATT KOLLA ATT FÖRSTA ARTIKEL SJÄLV ÄR OK                  
105200               IF SEGMENT-FINNS                                           
105300                   PERFORM UNTIL (ART-IDARTNR > ZERO AND                  
105400                      ART-IDARTNR NOT = W-IDARTNR) OR                     
105500                      SEGMENT-SAKNAS                                      
105600                     PERFORM IMS-GNP-INLA3-INLA11-OKVAL                   
105700                   END-PERFORM                                            
105800                   IF SEGMENT-FINNS                                       
105900                     MOVE ART-IDARTNR TO W-IDARTNR                        
106000                     PERFORM IMS-GU-ARTC01                                
106100                     IF SEGMENT-FINNS                                     
106200                       IF W-IDFTG NOT = ARTC-ART-IDFTG                    
106300                           MOVE NEJ      TO IDARTNR-SW                    
106400                           MOVE '088'    TO IDMFSFEL                      
106500                       END-IF                                             
106600                     ELSE                                                 
106700                       PERFORM UNTIL SLUT                                 
106800                         PERFORM IMS-GNP-INLA3-INLA11-OKVAL               
106900                         IF SEGMENT-FINNS                                 
107000                           MOVE ART-IDARTNR TO W-IDARTNR                  
107100                           PERFORM IMS-GU-ARTC01                          
107200                           IF SEGMENT-FINNS                               
107300                             IF W-IDFTG NOT = ARTC-ART-IDFTG              
107400                                 MOVE NEJ      TO IDARTNR-SW              
107500                                 MOVE '088'    TO IDMFSFEL                
107600                                 MOVE JA       TO SLUT-SW                 
107700                             END-IF                                       
107800                           END-IF                                         
107900                         ELSE                                             
108000                           MOVE JA TO SLUT-SW                             
108100                         END-IF                                           
108200                       END-PERFORM                                        
108300                     END-IF                                               
108400                   END-IF                                                 
108500                   MOVE WS-IDARTNR   TO W-IDARTNR                         
108600                   PERFORM IMS-GU-ARTC01                                  
108700               ELSE                                                       
108800                 MOVE WS-IDARTNR   TO W-IDARTNR                           
108900                 PERFORM IMS-GU-ARTC01                                    
109000               END-IF                                                     
109100             END-IF                                                       
109200          ELSE                                                            
109300             IF ARTC-ART-IDFTG   NOT = W-IDFTG                            
109400                 MOVE NEJ      TO IDARTNR-SW                              
109500                 MOVE '088'    TO IDMFSFEL                                
109600             END-IF                                                       
109700         END-IF                                                           
109800         MOVE ARTC-ART-IDFKNGRP  TO REG-IDFKNGRP(REG-IX)                  
109900         MOVE ARTC-ART-KDSORT    TO REG-KDSORT     (REG-IX)               
110000                                                                          
110100         PERFORM IMS-GNP-ARTC11                                           
110200         IF SEGMENT-FINNS                                                 
110300             MOVE ARTC-CLAG-KVMP      TO REG-KVMP     (REG-IX)            
110400             MOVE ARTC-CLAG-PRARTSTD  TO REG-PRARTSTD (REG-IX)            
110500             MOVE ARTC-CLAG-KDFARLIG  TO REG-KDFARLIG (REG-IX)            
110600             MOVE ARTC-CLAG-VKART     TO REG-VKART    (REG-IX)            
110700             MOVE ARTC-CLAG-VLARTNTO  TO REG-VLARTNTO (REG-IX)            
110800             MOVE ARTC-CLAG-BEFT      TO REG-BEFT     (REG-IX)            
110900             MOVE ARTC-CLAG-IDARTNR-EMBQ3                                 
111000                                      TO W-CL1-IDARTNR-EMBQ3              
111100             MOVE ARTC-CLAG-KDARTURS  TO REG-KDARTURS (REG-IX)            
111200             IF CDC                                                       
111300               MOVE ARTC-CLAG-ADLAGOMR TO REG-ADLAGOMR(REG-IX)            
111400               MOVE ARTC-CLAG-ADGANG   TO REG-ADGANG  (REG-IX)            
111500               MOVE ARTC-CLAG-ADPLATS  TO REG-ADPLATS (REG-IX)            
111600             ELSE                                                         
111700               MOVE SLAG-ADLAGOMR      TO REG-ADLAGOMR(REG-IX)            
111800               MOVE SLAG-ADGANG        TO REG-ADGANG  (REG-IX)            
111900               MOVE SLAG-ADPLATS       TO REG-ADPLATS (REG-IX)            
112000                                                                          
112100**             GET VALUES FOM WDK712 FOR CHINSE DC'S                      
112200               IF NDC-CN OR NDC-US                                        
112300                 IF NDC-CN                                                
112400                   MOVE WC-LAND-CN     TO W-IDLAND                        
112500                 ELSE                                                     
112600                   MOVE WC-LAND-US     TO W-IDLAND                        
112700                 END-IF                                                   
112800                 PERFORM IMS-GU-WDK712                                    
112900                 MOVE LART-KDARTURS    TO REG-KDARTURS(REG-IX)            
113000*                                                                         
113100                 IF LART-VKART > 0                                        
113200                   MOVE LART-VKART     TO REG-VKART   (REG-IX)            
113300                 END-IF                                                   
113400*                                                                         
113500                 IF LART-VLARTNTO > 0                                     
113600                   MOVE LART-VLARTNTO  TO REG-VLARTNTO(REG-IX)            
113700                 END-IF                                                   
113800*                                                                         
113900                 IF LART-BEFT > 0                                         
114000                   MOVE LART-BEFT      TO REG-BEFT    (REG-IX)            
114100                 END-IF                                                   
114200               END-IF                                                     
114300             END-IF                                                       
114400                                                                          
114500             PERFORM DBBA-HAEMTA-KDLAGEMB                                 
114600                                                                          
114700** ARTIKLAR SOM HAR MÄRKNING 100% DIREKTLEVERANS FÅR EJ TAS EMOT          
114800** AV DC11                                                                
114900             IF ARTC-CLAG-REDIRLEV = 1.00 AND CDC                         
115000               IF REG-IDLEVNR(REG-IX) = '6492 ' OR 'BZFFA'                
115100** NYCKLAR FRÅN KONRAD SLÄPPS IGENOM TROTS 100% DIRLEV                    
115200                 CONTINUE                                                 
115300               ELSE                                                       
115400                 IF REG-IDLEVNR(REG-IX) = ('BP8BA' OR 'BP3EA' OR          
115500                        'BP7YA') AND CDC-TR                               
115600** SLÄPPER IGENOM BUMPRAR FRÅN PLASTAL TILL DC 12                         
115700** SE ETRACKER: 8435072                                                   
115800                   CONTINUE                                               
115900                 ELSE                                                     
116000                   MOVE NEJ              TO IDARTNR-SW                    
116100                   MOVE '306'            TO IDMFSFEL                      
116200                 END-IF                                                   
116300               END-IF                                                     
116400             END-IF                                                       
116500                                                                          
116600             IF ARTC-CLAG-PRARTSTD = ZERO                                 
116700                 MOVE NEJ              TO IDARTNR-SW                      
116800                 MOVE '301'            TO IDMFSFEL                        
116900             END-IF                                                       
117000                                                                          
117100* FELFLAGGAR OM AVROP SAKNAS(TILLÄGG SAMBAND MED EVEREST)                 
117200             IF CDC OR NDC-CN OR NDC-US                                   
117300               MOVE +2                    TO W-KDAVROP                    
117400               MOVE REG-IDLEVNR  (REG-IX) TO  W-IDLEVNR                   
117500               IF W-IDLEVNR = '     ' OR '0    ' OR '9998 ' OR            
117600                              '9999 ' OR                                  
117700                              '8888 ' OR '16466' OR '6492 ' OR            
117800                              '1003 ' OR '1304 ' OR                       
117900                              'BWLAA' OR 'BZFFA' OR                       
118000                              'BP2TH' OR 'BKMJA' OR 'S5S2A' OR            
118100                              '12054' OR 'BP2TF'                          
118200                                                                          
118300                 CONTINUE                                                 
118400               ELSE                                                       
118500                 IF (REG-FLGODK = JA OR                                   
118600                       LAENK-IDTRANS = '6114')                            
118700                 AND PRIS-FINNS                                           
118800                     CONTINUE                                             
118900                 ELSE                                                     
119000                   MOVE W-IDARTNR            TO W-IDARTNR-D9              
119100                   MOVE WS-IDDC              TO W-IDDC-D9                 
119200                   PERFORM IMS-GET-WDD902                                 
119300                   IF SEGMENT-FINNS                                       
119400                     PERFORM IMS-GNP-WDD905                               
119500                     IF SEGMENT-FINNS                                     
119600                       IF KVAVROP < REG-KVAVIS(REG-IX)                    
119700                         MOVE ZERO TO WS-KVAVROP                          
119800                         PERFORM UNTIL SEGMENT-SAKNAS OR                  
119900                         (WS-KVAVROP NOT < REG-KVAVIS(REG-IX))            
120000                           ADD KVAVROP TO WS-KVAVROP                      
120100                           PERFORM IMS-GNP-WDD905                         
120200                         END-PERFORM                                      
120300                         IF WS-KVAVROP < REG-KVAVIS(REG-IX)               
120400                           MOVE '285'             TO IDMFSFEL             
120500                           MOVE NEJ               TO IDARTNR-SW           
120600                         END-IF                                           
120700                       END-IF                                             
120800                     ELSE                                                 
120900                       MOVE '285'             TO IDMFSFEL                 
121000                       MOVE NEJ               TO IDARTNR-SW               
121100                     END-IF                                               
121200                   ELSE                                                   
121300                     MOVE '285'             TO IDMFSFEL                   
121400                     MOVE NEJ               TO IDARTNR-SW                 
121500                   END-IF                                                 
121600                 END-IF                                                   
121700               END-IF                                                     
121800             END-IF                                                       
121900                                                                          
122000             IF ARTC-CLAG-KDERS > 20                                      
122100               IF (REG-FLGODK = JA OR LAENK-IDTRANS = '6114')             
122200               AND PRIS-FINNS                                             
122300                 CONTINUE                                                 
122400               ELSE                                                       
122500                 MOVE '220'             TO IDMFSFEL                       
122600                 MOVE NEJ               TO IDARTNR-SW                     
122700               END-IF                                                     
122800             END-IF                                                       
122900                                                                          
123000             IF NDC                                                       
123100               IF NDC-CN OR NDC-US                                        
123200                 MOVE NEJ TO PRIS-FINNS-SW                                
123300                 PERFORM DBBC-KOLLA-PRIS-FINNS-CN-US                      
123400                 IF PRIS-FINNS-INTE                                       
123500                     MOVE NEJ            TO IDARTNR-SW                    
123600                     MOVE '301'          TO IDMFSFEL                      
123700                 END-IF                                                   
123800               ELSE                                                       
123900                 MOVE NEJ TO PRIS-FINNS-SW                                
124000                 MOVE NEJ TO KDVALISO-FINNS-SW                            
124100                 PERFORM DBBB-KOLLA-PRIS-FINNS                            
124200                 IF PRIS-FINNS-INTE                                       
124300                     MOVE NEJ            TO IDARTNR-SW                    
124400                     MOVE '301'          TO IDMFSFEL                      
124500                 END-IF                                                   
124600               END-IF                                                     
124700             END-IF                                                       
124800         ELSE                                                             
124900           MOVE NEJ              TO IDARTNR-SW                            
125000           MOVE '017'            TO IDMFSFEL                              
125100           MOVE +0               TO REG-KVMP (REG-IX)                     
125200                                    REG-PRARTSTD(REG-IX)                  
125300                                    REG-KDFARLIG   (REG-IX)               
125400                                    REG-VKART      (REG-IX)               
125500                                    REG-VLARTNTO   (REG-IX)               
125600                                    REG-BEFT    (REG-IX)                  
125700                                    W-CL1-IDARTNR-EMBQ3                   
125800                                    REG-ADLAGOMR(REG-IX)                  
125900                                    REG-ADGANG  (REG-IX)                  
126000                                    REG-ADPLATS (REG-IX)                  
126100           MOVE SPACE            TO REG-KDARTURS(REG-IX)                  
126200         END-IF                                                           
126300      ELSE                                                                
126400         MOVE NEJ              TO IDARTNR-SW                              
126500         MOVE '017'            TO IDMFSFEL                                
126600     END-IF                                                               
126700     .                                                                    
126800     EJECT                                                                
126900 DBBA-HAEMTA-KDLAGEMB   SECTION.                                          
127000                                                                          
127100     MOVE W-CL1-IDARTNR-EMBQ3  TO W-CL1-ART-EMBQ3                         
127200                                                                          
127300     MOVE SPACE                 TO REG-CL1-KDLAGEMB (REG-IX)              
127400                                                                          
127500     MOVE 1                     TO EMB-IX                                 
127600     PERFORM UNTIL EMB-IX       >  TAB-EMBQ3-MAX OR                       
127700          TAB-KOD (EMB-IX)      = W-CL1-ART-EMBQ3                         
127800       ADD 1                    TO EMB-IX                                 
127900     END-PERFORM                                                          
128000                                                                          
128100     IF EMB-IX                  > TAB-EMBQ3-MAX                           
128200         CONTINUE                                                         
128300      ELSE                                                                
128400         IF TAB-KOD (EMB-IX)        =  W-CL1-ART-EMBQ3                    
128500             MOVE TAB-TEXT (EMB-IX) TO REG-CL1-KDLAGEMB(REG-IX)           
128600         END-IF                                                           
128700     END-IF                                                               
128800     .                                                                    
128900     EJECT                                                                
129000 DBBB-KOLLA-PRIS-FINNS SECTION.                                           
129100     SKIP2                                                                
129200     MOVE REG-TIAVIDAT (REG-IX) TO WS-IDAG                                
129300     MOVE WS-IDAG           TO DAT-I-TIDATUM                              
129400     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
129500     CALL WDATKONV USING       DAT-KDDATFORM                              
129600                               DAT-I-TIDATUM                              
129700                               DAT-O-TIDATUM                              
129800                               DAT-KDSVAR                                 
129900     IF DAT-KDSVAR-FEL                                                    
130000       MOVE NEJ TO PRIS-FINNS-SW                                          
130100       MOVE NEJ TO KDVALISO-FINNS-SW                                      
130200     ELSE                                                                 
130300                                                                          
130400       MOVE DAT-TISEKEL TO WS-DAGENS-SEKEL                                
130500                                                                          
130600       MOVE NEJ                  TO PRIS-FINNS-SW                         
130700       MOVE NEJ                  TO KDVALISO-FINNS-SW                     
130800       MOVE REG-IDLEVNR (REG-IX) TO W-IDLEVNR                             
130900                                    W-IDLEVNR-21                          
131000       COMPUTE W-DAPRLIST-21 = 99999999 - WS-DAGENS-DATUM                 
131100       PERFORM IMS-GNP-ARTC21                                             
131200                                                                          
131300       PERFORM UNTIL SEGMENT-SAKNAS OR PRIS-FINNS                         
131400         IF ARTC-PRL-KDSTATUS-PR = +1                                     
131500           MOVE JA               TO PRIS-FINNS-SW                         
131600           EVALUATE REG-IDTRANS                                           
131700              WHEN '6111'                                                 
131800              WHEN '6116'                                                 
131900                  IF KDPRODSL-LOCAL                                       
132000                     MOVE REG-IDDC         TO W-IDDC                      
132100                     PERFORM IMS-GU-WDB601                                
132200                     IF SEGMENT-FINNS                                     
132300                        MOVE DCS-KDVALISO       TO                        
132400                             CURR-KDVALISO-HUV                            
132500                        IF W-IDDC    = '61'                               
132600                                    OR '62'                               
132700                                    OR '6A'                               
132800                           MOVE 'SEK'           TO                        
132900                        CURR-KDVALISO-HUV                                 
133000                        END-IF                                            
133100                        MOVE ARTC-PRL-KDVALISO  TO                        
133200                             CURR-KDVALISO-ROW                            
133300                        MOVE DAT-TIAAMMDD(1:4)  TO CURR-TIAAMM            
133400                             CURR-TIAAMM                                  
133500                        MOVE 'M'                TO CURR-KDVALTYP          
133600                        CALL W510CURR USING                               
133700                                      CURR-W510CURR 9305-PCB              
133800                        IF CURR-KDSVAR = ' '                              
133900                           MOVE JA              TO                        
134000                                KDVALISO-FINNS-SW                         
134100                        ELSE                                              
134200                           MOVE NEJ             TO IDARTNR-SW             
134300                           MOVE '151'           TO IDMFSFEL               
134400                        END-IF                                            
134500                     END-IF                                               
134600                  END-IF                                                  
134700           END-EVALUATE                                                   
134800         END-IF                                                           
134900         PERFORM IMS-GNP-ARTC21                                           
135000       END-PERFORM                                                        
135100     END-IF                                                               
135200     .                                                                    
135300     EJECT                                                                
135400                                                                          
135500 DBBC-KOLLA-PRIS-FINNS-CN-US SECTION.                                     
135600     SKIP2                                                                
135700     MOVE REG-TIAVIDAT (REG-IX) TO WS-IDAG                                
135800     MOVE WS-IDAG           TO DAT-I-TIDATUM                              
135900     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
136000     CALL WDATKONV USING       DAT-KDDATFORM                              
136100                               DAT-I-TIDATUM                              
136200                               DAT-O-TIDATUM                              
136300                               DAT-KDSVAR                                 
136400     IF DAT-KDSVAR-FEL                                                    
136500       MOVE NEJ TO PRIS-FINNS-SW                                          
136600     ELSE                                                                 
136700                                                                          
136800       MOVE DAT-TISEKEL TO WS-DAGENS-SEKEL                                
136900                                                                          
137000       MOVE NEJ                  TO PRIS-FINNS-SW                         
137100       MOVE REG-IDLEVNR (REG-IX) TO W-IDLEVNR                             
137200                                    W-IDLEVNR-21                          
137300                                    W-IDLEVNR-PR                          
137400       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAGENS-DATUM                 
137500                                                                          
137600       PERFORM IMS-GU-WDK711                                              
137700       IF SEGMENT-FINNS                                                   
137800                                                                          
137900*    -- WDK724                                                            
138000         PERFORM IMS-GNP-WDK724                                           
138100                                                                          
138200         IF SEGMENT-FINNS                                                 
138300           MOVE JA                TO PRIS-FINNS-SW                        
138400                                                                          
138500         END-IF                                                           
138600       END-IF                                                             
138700     END-IF                                                               
138800     .                                                                    
138900     EJECT                                                                
139000 DBC-KOLLA-WLBENA       SECTION.                                          
139100                                                                          
139200     MOVE REG-IDARTNR-NY(REG-IX) TO W-IDARTNR                             
139300     IF CDC-SE                                                            
139400       MOVE 'S  '                TO W-IDSKYLT                             
139500     ELSE                                                                 
139600       MOVE 'GB '                TO W-IDSKYLT                             
139700     END-IF                                                               
139800     PERFORM IMS-GU-BENA11                                                
139900     IF SEGMENT-FINNS                                                     
140000         MOVE BENA11-TEXT-BEART TO REG-BEART(REG-IX)                      
140100      ELSE                                                                
140200         MOVE NEJ               TO IDARTNR-SW                             
140300         MOVE '017'             TO IDMFSFEL                               
140400     END-IF                                                               
140500     .                                                                    
140600     EJECT                                                                
140700 DC-KOLLA-INLEV-REGISTER    SECTION.                                      
140800                                                                          
140900     MOVE JA                          TO IDARTNR-SW                       
141000     IF REG-IDTRANS NOT = '6114'                                          
141100       PERFORM DCA-LAES-INLEV-REGISTER                                    
141200     END-IF                                                               
141300     IF IDARTNR-FEL                                                       
141400         EVALUATE REG-IDTRANS                                             
141500           WHEN '6111'                                                    
141600             MOVE NEJ                 TO REG1-IDARTNR-OK (REG-IX)         
141700             MOVE IDMFSFEL            TO REG1-IDMFSFEL   (REG-IX)         
141800                                                                          
141900           WHEN '6112'                                                    
142000             MOVE NEJ                 TO REG2-IDARTNR-OK (REG-IX)         
142100             MOVE IDMFSFEL            TO REG2-IDMFSFEL   (REG-IX)         
142200                                                                          
142300           WHEN '6113'                                                    
142400             MOVE NEJ                 TO REG3-IDARTNR-OK (REG-IX)         
142500             MOVE IDMFSFEL            TO REG3-IDMFSFEL   (REG-IX)         
142600                                                                          
142700           WHEN '6114'                                                    
142800             MOVE NEJ                 TO REG4-IDARTNR-OK (REG-IX)         
142900             MOVE IDMFSFEL            TO REG4-IDMFSFEL   (REG-IX)         
143000                                                                          
143100           WHEN '6116'                                                    
143200             MOVE NEJ                 TO REG6-IDARTNR-OK (REG-IX)         
143300             MOVE IDMFSFEL            TO REG6-IDMFSFEL   (REG-IX)         
143400                                                                          
143500         END-EVALUATE                                                     
143600         IF REG-FLGODK         = SPACE                                    
143700             MOVE NEJ          TO ALLT-SW                                 
143800         END-IF                                                           
143900     END-IF                                                               
144000     .                                                                    
144100     EJECT                                                                
144200 DCA-LAES-INLEV-REGISTER SECTION.                                         
144300                                                                          
144400     MOVE LOW-VALUE        TO  W-W6D1ASEQ-MIN-X                           
144500     MOVE HIGH-VALUE       TO  W-W6D1ASEQ-MAX-X                           
144600     MOVE REG-IDDC            TO W-D1ASEQ-IDDC-MIN                        
144700                                 W-D1ASEQ-IDDC-MAX                        
144800     MOVE REG-IDLEVNR(REG-IX) TO W-D1ASEQ-IDLEVNR-MIN                     
144900                                 W-D1ASEQ-IDLEVNR-MAX                     
145000     PERFORM IMS-GU-INLA1-INLA11-OKVAL                                    
145100     IF SEGMENT-FINNS                                                     
145200         IF REG-FLGODK-IDARTNR = NEJ                                      
145300             CONTINUE                                                     
145400          ELSE                                                            
145500             MOVE JA           TO REG-FLGODK-IDARTNR                      
145600             MOVE NEJ          TO REG-IDARTNR-OK (REG-IX)                 
145700         END-IF                                                           
145800         MOVE NEJ              TO IDARTNR-SW                              
145900         IF IDMFSFEL = SPACE                                              
146000           MOVE '201'            TO IDMFSFEL                              
146100         END-IF                                                           
146200     END-IF                                                               
146300     .                                                                    
146400     EJECT                                                                
146500 E-UPPDATERA-INLA     SECTION.                                            
146600                                                                          
146700     MOVE +1                          TO REG-IX                           
146800     PERFORM UNTIL REG-IX             >  MAX-REG                          
146900         IF (REG-IDARTNR-NY  (REG-IX) = ZERO) OR                          
147000            ((REG-IDFS-OK    (REG-IX) = NEJ OR                            
147100              REG-IDARTNR-OK (REG-IX) = NEJ) AND                          
147200              REG-FLGODK              = NEJ)                              
147300             CONTINUE                                                     
147400          ELSE                                                            
147500             MOVE REG-IDLEVNR (REG-IX) TO W-D101KY-IDLEVNR                
147600             MOVE REG-IDFS    (REG-IX) TO W-D101KY-IDFS                   
147700             MOVE REG-TIAVIDAT(REG-IX) TO W-D101KY-TIAVIDAT               
147800             IF REG-IDTRANS    = '6111' OR '6112' OR                      
147900                                 '6113' OR '6116'                         
148000                 PERFORM EA-SKAPA-INLA01                                  
148100             END-IF                                                       
148200                                                                          
148300             IF (REG-IDTRANS   =  '6111' OR '6112' OR '6113' OR           
148400                                          '6116') OR                      
148500                (REG-IDTRANS   =  '6114' AND                              
148600                 REG-KDBEH(REG-IX)  = 1)                                  
148700                 IF REG-IDTRANS = '6114'                                  
148800                   PERFORM IMS-GU-INLA2-INLA11-LAST                       
148900                   MOVE ART-IDRADNR-INL TO W-SPAR-IDRADNR-INL             
149000                 END-IF                                                   
149100                 PERFORM EB-SKAPA-INLA11                                  
149200                 MOVE REG-IDARTNR-NY(REG-IX) TO W-IDARTNR                 
149300                 PERFORM S05-SKAPA-INLA21                                 
149400              ELSE                                                        
149500                 IF REG-IDTRANS = '6114' AND                              
149600                   (REG-KDBEH(REG-IX) = 2 OR 3)                           
149700                     PERFORM EC-UPPDATERA-INLA11-21                       
149800                 END-IF                                                   
149900             END-IF                                                       
150000         END-IF                                                           
150100         ADD +1                TO REG-IX                                  
150200     END-PERFORM                                                          
150300                                                                          
150400     IF REG-IDTRANS            = '6114' AND                               
150500        FLFEL-AENDRAD                                                     
150600         PERFORM ED-UPPDATERA-EV-INLA01                                   
150700     END-IF                                                               
150800     .                                                                    
150900     EJECT                                                                
151000 EA-SKAPA-INLA01  SECTION.                                                
151100                                                                          
151200     MOVE REG-IDDC             TO INL-IDDC                                
151300                                  W-IDDC                                  
151400     MOVE REG-IDLEVNR (REG-IX) TO INL-IDLEVNR                             
151500                                  W-D101KY-IDLEVNR                        
151600     MOVE REG-IDFS    (REG-IX) TO INL-IDFS                                
151700                                  W-D101KY-IDFS                           
151800     MOVE REG-TIAVIDAT(REG-IX) TO INL-TIAVIDAT                            
151900                                  W-D101KY-TIAVIDAT                       
152000     PERFORM IMS-GU-WDB601                                                
152100     MOVE DCS-IDFTG            TO INL-IDFTG                               
152200                                                                          
152300     MOVE REG-IDKONTO          TO INL-IDKONTO                             
152400     MOVE REG-IDANALYS         TO INL-IDANALYS                            
152500     MOVE REG-IDKST            TO INL-IDKST                               
152600     MOVE REG-IDLBBET          TO INL-IDLBBET                             
152700     MOVE 'R31'                TO INL-KDINL                               
152800     MOVE ZERO                 TO INL-TIANKDAG                            
152900                                  INL-TIINLMOT                            
153000                                  INL-IDARTNR                             
153100                                  INL-IDSHIPM                             
153200     MOVE NEJ                  TO INL-FLFEL                               
153300                                                                          
153400     PERFORM IMS-ISRT-INLA2-INLA01                                        
153500     IF SEGMENT-FINNS-REDAN                                               
153600       PERFORM IMS-GU-INLA2-INLA11-LAST                                   
153700       IF SEGMENT-FINNS                                                   
153800         MOVE ART-IDRADNR-INL TO W-SPAR-IDRADNR-INL                       
153900       ELSE                                                               
154000         MOVE +0              TO W-SPAR-IDRADNR-INL                       
154100       END-IF                                                             
154200     ELSE                                                                 
154300       MOVE +0              TO W-SPAR-IDRADNR-INL                         
154400     END-IF                                                               
154500     .                                                                    
154600     EJECT                                                                
154700 EB-SKAPA-INLA11    SECTION.                                              
154800                                                                          
154900     ADD  +1                       TO  W-SPAR-IDRADNR-INL                 
155000     MOVE W-SPAR-IDRADNR-INL       TO  ART-IDRADNR-INL                    
155100                                       W-IDRADNR-INL                      
155200     MOVE REG-IDARTNR-NY(REG-IX)   TO  ART-IDARTNR                        
155300     MOVE REG-ADGANG      (REG-IX) TO  ART-ADGANG                         
155400     MOVE REG-ADLAGOMR    (REG-IX) TO  ART-ADLAGOMR                       
155500     MOVE REG-ADPLATS     (REG-IX) TO  ART-ADPLATS                        
155600     MOVE REG-BEART       (REG-IX) TO  ART-BEART                          
155700     MOVE REG-BEFT        (REG-IX) TO  ART-BEFT                           
155800     MOVE SPACE                    TO  ART-ADTRDEST-KIT                   
155900     MOVE NEJ                      TO  ART-FLETIKETT                      
156000     MOVE NEJ                      TO  ART-FLFEL                          
156100     MOVE NEJ                      TO  ART-FLKLAR                         
156200                                       ART-FLKVAFEL                       
156300                                       ART-FLKVAKAR                       
156400                                       ART-FLANNULL                       
156500     MOVE REG-IDFKNGRP    (REG-IX) TO  ART-IDFKNGRP                       
156600     MOVE ZERO                     TO  ART-IDLOPNRM                       
156700     MOVE REG-IDDC                 TO  ART-IDDC                           
156800     MOVE REG-KDFARLIG(REG-IX)     TO  ART-KDFARLIG                       
156900     MOVE ZERO                     TO  ART-KDINLPRIO                      
157000                                       ART-KDKVAANT                       
157100     MOVE REG-CL1-KDLAGEMB(REG-IX) TO  ART-KDLAGEMB                       
157200     MOVE REG-KDRT    (REG-IX)    TO  ART-KDRT                            
157300     MOVE REG-KDSORT  (REG-IX)    TO  ART-KDSORT                          
157400     MOVE REG-KVAVIS  (REG-IX)    TO  ART-KVAVIS                          
157500                                      W-RAD-KVINLART                      
157600     MOVE ZERO                    TO  ART-KVAVIS-KIT                      
157700                                      ART-KVAVIS-PRIO                     
157800                                      ART-KVKVAPRIM-BER                   
157900                                      ART-KVKVAPRIM-VER                   
158000                                      ART-KVKVASEK-BER                    
158100                                      ART-KVKVASEK-VER                    
158200                                      ART-TIUPPDAT                        
158300     MOVE REG-KVMP     (REG-IX)   TO  ART-KVMP                            
158400     MOVE REG-PRARTSTD (REG-IX)   TO  ART-PRARTSTD                        
158500     MOVE REG-VKART    (REG-IX)   TO  ART-VKART                           
158600     MOVE REG-VLARTNTO (REG-IX)   TO  ART-VLARTNTO                        
158700     MOVE REG-KDARTURS (REG-IX)   TO  ART-KDARTURS                        
158800                                                                          
158900     MOVE NEJ                     TO  ART-FLSPLPART                       
159000     MOVE SPACE                   TO  ART-ADTRDEST                        
159100     MOVE SPACE                   TO  ART-KDKVAINL                        
159200                                                                          
159300     PERFORM IMS-ISRT-INLA2-INLA11                                        
159400     .                                                                    
159500     EJECT                                                                
159600 EC-UPPDATERA-INLA11-21   SECTION.                                        
159700                                                                          
159800     MOVE REG-IDARTNR-GAMMAL (REG-IX)                                     
159900                                   TO W-IDARTNR                           
160000     MOVE REG-IDRADNR-INL-GAMMAL    (REG-IX)                              
160100                                   TO W-IDRADNR-INL                       
160200     PERFORM IMS-GHU-INLA3-INLA11                                         
160300     IF REG-KDBEH (REG-IX)                    =  3                        
160400         IF REG-IDARTNR-NY      (REG-IX)      NOT =                       
160500            REG-IDARTNR-GAMMAL  (REG-IX)                                  
160600             PERFORM IMS-DLET-INLA3-INLA11                                
160700             MOVE ART-IDRADNR-INL TO W-SPAR-IDRADNR-INL                   
160800             IF ART-FLFEL                     = JA                        
160900                 MOVE NEJ                     TO ART-FLFEL                
161000                 MOVE JA                      TO FLFEL-AENDRAD-SW         
161100             END-IF                                                       
161200             MOVE REG-IDARTNR-NY(REG-IX)      TO ART-IDARTNR              
161300                                                 W-IDARTNR                
161400             IF REG-KVAVIS (REG-IX)           >  ZERO                     
161500                 MOVE REG-KVAVIS(REG-IX)      TO ART-KVAVIS               
161600                                                 W-RAD-KVINLART           
161700              ELSE                                                        
161800                 MOVE ART-KVAVIS              TO W-RAD-KVINLART           
161900             END-IF                                                       
162000                                                                          
162100             IF REG-KDRT      (REG-IX)        NOT = 99                    
162200                 MOVE REG-KDRT(REG-IX)        TO ART-KDRT                 
162300             END-IF                                                       
162400             MOVE REG-KVMP      (REG-IX)      TO ART-KVMP                 
162500             MOVE REG-PRARTSTD  (REG-IX)      TO ART-PRARTSTD             
162600             MOVE REG-KDSORT    (REG-IX)      TO ART-KDSORT               
162700             MOVE REG-KDFARLIG  (REG-IX)      TO ART-KDFARLIG             
162800             MOVE REG-IDFKNGRP  (REG-IX)      TO ART-IDFKNGRP             
162900             MOVE REG-BEFT      (REG-IX)      TO ART-BEFT                 
163000             MOVE REG-BEART     (REG-IX)      TO ART-BEART                
163100             MOVE REG-ADPLATS   (REG-IX)      TO ART-ADPLATS              
163200             MOVE REG-ADGANG    (REG-IX)      TO ART-ADGANG               
163300             MOVE REG-ADLAGOMR  (REG-IX)      TO ART-ADLAGOMR             
163400             MOVE REG-VKART     (REG-IX)      TO ART-VKART                
163500             MOVE REG-VLARTNTO  (REG-IX)      TO ART-VLARTNTO             
163600             MOVE REG-KDARTURS  (REG-IX)      TO ART-KDARTURS             
163700             MOVE W-SPAR-IDRADNR-INL          TO ART-IDRADNR-INL          
163800                                                 W-IDRADNR-INL            
163900             MOVE SPACE                       TO ART-KDKVAINL             
164000             PERFORM IMS-ISRT-INLA2-INLA11                                
164100          ELSE                                                            
164200             MOVE REG-KVAVIS  (REG-IX)       TO ART-KVAVIS                
164300                                                W-RAD-KVINLART            
164400             IF REG-KDRT      (REG-IX)       NOT = 99                     
164500                 MOVE REG-KDRT(REG-IX)       TO ART-KDRT                  
164600             END-IF                                                       
164700             IF ART-FLFEL                    = JA                         
164800                 MOVE NEJ                    TO ART-FLFEL                 
164900                 MOVE JA                     TO FLFEL-AENDRAD-SW          
165000             END-IF                                                       
165100             PERFORM IMS-REPL-INLA3-INLA11                                
165200             PERFORM ECA-TA-BORT-INLA21                                   
165300             MOVE REG-IDARTNR-GAMMAL (REG-IX) TO W-IDARTNR                
165400         END-IF                                                           
165500                                                                          
165600         PERFORM S05-SKAPA-INLA21                                         
165700      ELSE                                                                
165800         MOVE REG-KDRT (REG-IX) TO ART-KDRT                               
165900         PERFORM IMS-REPL-INLA3-INLA11                                    
166000     END-IF                                                               
166100     .                                                                    
166200     EJECT                                                                
166300 ECA-TA-BORT-INLA21       SECTION.                                        
166400                                                                          
166500     PERFORM IMS-GHNP-INLA3-INLA21                                        
166600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
166700         PERFORM IMS-DLET-INLA3-INLA21                                    
166800         PERFORM IMS-GHNP-INLA3-INLA21                                    
166900     END-PERFORM                                                          
167000     .                                                                    
167100     EJECT                                                                
167200 ED-UPPDATERA-EV-INLA01   SECTION.                                        
167300                                                                          
167400     PERFORM IMS-GU-INLA3-INLA11-OKVAL                                    
167500     PERFORM UNTIL SEGMENT-SAKNAS OR ART-FLFEL = JA                       
167600         PERFORM IMS-GNP-INLA3-INLA11                                     
167700     END-PERFORM                                                          
167800                                                                          
167900     IF ART-FLFEL              = NEJ                                      
168000         PERFORM IMS-GHU-INLA3-INLA01                                     
168100         MOVE NEJ              TO INL-FLFEL                               
168200         PERFORM IMS-REPL-INLA3-INLA01                                    
168300     END-IF                                                               
168400     .                                                                    
168500     EJECT                                                                
168600 F-FLYTTA-LAENKAREA  SECTION.                                             
168700                                                                          
168800     EVALUATE LAENK-IDTRANS                                               
168900                                                                          
169000         WHEN '6111'                                                      
169100            MOVE REG-FLGODK-IDFS     TO REG1-FLGODK-IDFS                  
169200            MOVE REG-FLGODK-IDARTNR  TO REG1-FLGODK-IDARTNR               
169300            MOVE REG1-W611REG1       TO LAENK-W611REG0                    
169400                                                                          
169500         WHEN '6112'                                                      
169600            MOVE REG-FLGODK-IDFS     TO REG2-FLGODK-IDFS                  
169700            MOVE REG-FLGODK-IDARTNR  TO REG2-FLGODK-IDARTNR               
169800            MOVE REG2-W611REG2       TO LAENK-W611REG0                    
169900                                                                          
170000         WHEN '6113'                                                      
170100            MOVE REG-FLGODK-IDFS     TO REG3-FLGODK-IDFS                  
170200            MOVE REG-FLGODK-IDARTNR  TO REG3-FLGODK-IDARTNR               
170300            MOVE REG3-W611REG3       TO LAENK-W611REG0                    
170400                                                                          
170500         WHEN '6114'                                                      
170600            MOVE REG4-W611REG4       TO LAENK-W611REG0                    
170700                                                                          
170800         WHEN '6116'                                                      
170900            MOVE REG-FLGODK-IDFS     TO REG6-FLGODK-IDFS                  
171000            MOVE REG-FLGODK-IDARTNR  TO REG6-FLGODK-IDARTNR               
171100            MOVE REG6-W611REG6       TO LAENK-W611REG0                    
171200                                                                          
171300     END-EVALUATE                                                         
171400     .                                                                    
171500     EJECT                                                                
171600 S01-KOLLA-IDLEVNR   SECTION.                                             
171700                                                                          
171800     IF W-IDLEVNR              = SPACE  OR '0    '                        
171900     OR W-IDLEVNR              = '9999 '                                  
172000         MOVE SPACE            TO STATUS-WS                               
172100      ELSE                                                                
172200         PERFORM IMS-GU-LEVA01                                            
172300     END-IF                                                               
172400     .                                                                    
172500     EJECT                                                                
172600 S02-KOLLA-INLAREG    SECTION.                                            
172700                                                                          
172800     MOVE REG-IDLEVNR(REG-IX)  TO  W-D101KY-IDLEVNR                       
172900     MOVE REG-IDFS(REG-IX)     TO  W-D101KY-IDFS                          
173000     MOVE REG-TIAVIDAT(REG-IX) TO  W-D101KY-TIAVIDAT                      
173100     PERFORM  IMS-GU-INLA2-INLA01                                         
173200     IF SEGMENT-FINNS                                                     
173300         IF INL-TIINLMOT       >   ZERO                                   
173400             MOVE JA           TO  FS-AKTIVERAD-SW                        
173500         END-IF                                                           
173600      ELSE                                                                
173700         MOVE LOW-VALUE           TO W-W6D1ASEQ-MIN-X                     
173800         MOVE HIGH-VALUE          TO W-W6D1ASEQ-MAX-X                     
173900         MOVE REG-IDDC            TO W-D1ASEQ-IDDC-MIN                    
174000                                     W-D1ASEQ-IDDC-MAX                    
174100         MOVE REG-IDLEVNR(REG-IX) TO W-D1ASEQ-IDLEVNR-MIN                 
174200                                     W-D1ASEQ-IDLEVNR-MAX                 
174300         MOVE REG-IDFS(REG-IX)    TO W-D1ASEQ-IDFS-MIN                    
174400                                     W-D1ASEQ-IDFS-MAX                    
174500                                                                          
174600         PERFORM IMS-GU-INLA1-INLA01                                      
174700     END-IF                                                               
174800     .                                                                    
174900     EJECT                                                                
175000 S03-KOLLA-KDRT-IDLEVNR  SECTION.                                         
175100                                                                          
175200     IF W-KDRT                 = 99                                       
175300         CONTINUE                                                         
175400      ELSE                                                                
175500                                                                          
175600       EVALUATE TRUE                                                      
175700       WHEN W-KDRT             = ZERO OR 10                               
175800         IF W-SPAR-IDLEVNR     = SPACE OR '1002 ' OR '1013 ' OR           
175900                                 '1064 ' OR '19539' OR '1225 ' OR         
176000                                 '1229 ' OR '12054' OR 'CBGKA' OR         
176100                                 '13450' OR '13456' OR '16466' OR         
176200                                 'BP2TC' OR 'BP2TG' OR 'BP2TE' OR         
176300                                 'BP2TF' OR 'BW5JA' OR 'DV8VE' OR         
176400                                 'CBKGA' OR 'S5S2A'                       
176500             MOVE NEJ          TO KDRT-SW                                 
176600         END-IF                                                           
176700                                                                          
176800       WHEN W-KDRT             = 1                                        
176900         IF W-SPAR-IDLEVNR     = '1001 ' OR 'BL3YA'                       
177000             CONTINUE                                                     
177100          ELSE                                                            
177200             MOVE NEJ          TO KDRT-SW                                 
177300         END-IF                                                           
177400                                                                          
177500       WHEN W-KDRT             = 2                                        
177600         IF W-SPAR-IDLEVNR     = '1012 ' OR                               
177700                                 '1021 ' OR '1064 '                       
177800             CONTINUE                                                     
177900         ELSE                                                             
178000             MOVE NEJ          TO KDRT-SW                                 
178100         END-IF                                                           
178200                                                                          
178300                                                                          
178400       WHEN W-KDRT             = 9                                        
178500         IF W-SPAR-IDLEVNR     = SPACE OR '1000 ' OR '1001 ' OR           
178600                                 '1002 ' OR '1003 ' OR '1004 ' OR         
178700*1003 INLAGT 930514 PÅ ORDER AV SUSANNE                AE                 
178800*1004 INLAGT 931013 PÅ ORDER AV TUULA                                     
178900                                 '1012 ' OR '1013 ' OR                    
179000                                 '1021 ' OR '1064 ' OR                    
179100*1812 OCH 2120 INLAGT 930915 PÅ ORDER AV SUSANNE MS                       
179200                                 '1618 ' OR '1619 ' OR                    
179300                                 '1621 ' OR '1812 ' OR                    
179400                                 '2120 ' OR '4509 ' OR                    
179500                                 'BL3YA' OR 'BP2TH' OR                    
179600                                 'BP2TD' OR 'BP2TC' OR                    
179700                                 'C7CUL' OR 'BS8CA'                       
179800             CONTINUE                                                     
179900         ELSE                                                             
180000             MOVE NEJ          TO KDRT-SW                                 
180100         END-IF                                                           
180200                                                                          
180300       WHEN OTHER                                                         
180400             MOVE NEJ          TO KDRT-SW                                 
180500       END-EVALUATE                                                       
180600     END-IF                                                               
180700     .                                                                    
180800     EJECT                                                                
180900 S04-KOLLA-KDRT-IDLEVNR        SECTION.                                   
181000                                                                          
181100     EVALUATE TRUE                                                        
181200                                                                          
181300       WHEN W-KDRT             = 4                                        
181400         IF W-SPAR-IDLEVNR     = '1000 '                                  
181500             CONTINUE                                                     
181600          ELSE                                                            
181700             MOVE NEJ          TO KDRT-SW                                 
181800         END-IF                                                           
181900                                                                          
182000       WHEN W-KDRT             = 5                                        
182100         IF W-SPAR-IDLEVNR     = '1005 ' OR '1021 ' OR '1064 ' OR         
182200                                 'BMJGA'                                  
182300             CONTINUE                                                     
182400          ELSE                                                            
182500             MOVE NEJ          TO KDRT-SW                                 
182600         END-IF                                                           
182700                                                                          
182800       WHEN W-KDRT             = 6                                        
182900         IF W-SPAR-IDLEVNR     = '0    ' OR '1013 ' OR '8265 ' OR         
183000                                 '9999 ' OR '13456' OR '19539' OR         
183100                                 '12054' OR '13450' OR 'CBGKA'            
183200                                 OR '1225 ' OR '1229 ' OR '16466'         
183300                                 OR 'BP2TC' OR 'BP2TG' OR 'BP2TE'         
183400                                 OR 'BP2TF' OR 'BW5JA' OR 'DV8VE'         
183500                                 OR 'CBKGA' OR 'S5S2A' OR 'BP2TU'         
183600                                                                          
183700             CONTINUE                                                     
183800          ELSE                                                            
183900             MOVE NEJ          TO KDRT-SW                                 
184000         END-IF                                                           
184100                                                                          
184200       WHEN OTHER                                                         
184300             MOVE NEJ          TO KDRT-SW                                 
184400     END-EVALUATE                                                         
184500     .                                                                    
184600     EJECT                                                                
184700 S05-SKAPA-INLA21   SECTION.                                              
184800                                                                          
184900     MOVE +1                   TO RAD-IDRADNR                             
185000     MOVE SPACE                TO RAD-ADINLOMR                            
185100     MOVE SPACE                TO RAD-ADINLOMR-NXT                        
185200                                  RAD-IDLEVNR-KOLLI                       
185300     MOVE NEJ                  TO RAD-FLDIVKLI                            
185400                                  RAD-FLKVAANT                            
185500                                  RAD-FLPRIO                              
185600                                  RAD-FLSATS                              
185700                                  RAD-FLINLFB                             
185800                                  RAD-FLINLFP                             
185900                                  RAD-FLSVSLS                             
186000     MOVE ZERO                 TO RAD-IDANSTNR                            
186100                                  RAD-IDILIRAD                            
186200                                  RAD-IDILIST                             
186300                                  RAD-IDINLVGN                            
186400                                  RAD-IDOKOLLI                            
186500                                  RAD-KDINLPRIO                           
186600     MOVE 'REG'                TO RAD-KDINLSTA                            
186700     MOVE W-RAD-KVINLART       TO RAD-KVINLART                            
186800     MOVE ZERO                 TO RAD-TIUPPDAT                            
186900                                                                          
187000     PERFORM IMS-ISRT-INLA2-INLA21                                        
187100     .                                                                    
187200     EJECT                                                                
187300 S06-CALL-WDATKONV SECTION.                                               
187400                                                                          
187500     CALL WDATKONV USING DAT-KDDATFORM,                                   
187600                         DAT-I-TIDATUM,                                   
187700                         DAT-O-TIDATUM,                                   
187800                         DAT-KDSVAR                                       
187900     IF DAT-KDSVAR-OK                                                     
188000         CONTINUE                                                         
188100      ELSE                                                                
188200         MOVE NEJ              TO TIAVIDAT-SW                             
188300     END-IF                                                               
188400     .                                                                    
188500     EJECT                                                                
188600                                                                          
188700* -COPY WY2000Q1                                                          
188800     EJECT                                                                
188900* --- IMS SEKTIONER ---                                                   
189000     SKIP3                                                                
189100 IMS-GU-INLA1-INLA01 SECTION.                                             
189200     STRING 'W6INLA01(W6D1ASEQ>=' W-W6D1ASEQ-MIN-X                        
189300                    '&W6D1ASEQ<=' W-W6D1ASEQ-MAX-X ')'                    
189400          DELIMITED BY SIZE INTO SSA1                                     
189500     MOVE '  GE' TO GODK-STATUSKODER                                      
189600     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA SSA1                     
189700     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
189800     PERFORM IMS-STATUSKONTROLL                                           
189900     .                                                                    
190000     SKIP3                                                                
190100 IMS-GU-INLA1-INLA11-OKVAL SECTION.                                       
190200                                                                          
190300     STRING 'W6INLA01(W6D1ASEQ>=' W-W6D1ASEQ-MIN-X                        
190400                    '&W6D1ASEQ<=' W-W6D1ASEQ-MAX-X ')'                    
190500          DELIMITED BY SIZE INTO SSA1                                     
190600     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
190700          DELIMITED BY SIZE INTO SSA2                                     
190800     MOVE '  GE' TO GODK-STATUSKODER                                      
190900     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA SSA1 SSA2                
191000     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
191100     PERFORM IMS-STATUSKONTROLL                                           
191200     .                                                                    
191300     SKIP3                                                                
191400 IMS-GU-INLA2-INLA01 SECTION.                                             
191500                                                                          
191600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
191700          DELIMITED BY SIZE INTO SSA1                                     
191800     MOVE '  GE' TO GODK-STATUSKODER                                      
191900     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA SSA1                     
192000     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
192100     PERFORM IMS-STATUSKONTROLL                                           
192200     .                                                                    
192300     EJECT                                                                
192400 IMS-GU-INLA2-INLA11-LAST SECTION.                                        
192500                                                                          
192600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
192700          DELIMITED BY SIZE INTO SSA1                                     
192800     MOVE 'W6INLA11*L' TO SSA2                                            
192900     MOVE '  GE' TO GODK-STATUSKODER                                      
193000     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA SSA1 SSA2                
193100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
193200     PERFORM IMS-STATUSKONTROLL                                           
193300     .                                                                    
193400     EJECT                                                                
193500 IMS-ISRT-INLA2-INLA01 SECTION.                                           
193600                                                                          
193700     MOVE 'W6INLA01 ' TO SSA1                                             
193800     MOVE '  II' TO GODK-STATUSKODER                                      
193900     CALL CBLTDLI USING ISRT INLA2-PCB DLI-IO-AREA SSA1                   
194000     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
194100     PERFORM IMS-STATUSKONTROLL                                           
194200     .                                                                    
194300     SKIP3                                                                
194400 IMS-ISRT-INLA2-INLA11 SECTION.                                           
194500                                                                          
194600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
194700          DELIMITED BY SIZE INTO SSA1                                     
194800     MOVE 'W6INLA11 ' TO SSA2                                             
194900     MOVE '  II' TO GODK-STATUSKODER                                      
195000     CALL CBLTDLI USING ISRT INLA2-PCB DLI-IO-AREA SSA1 SSA2              
195100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
195200     PERFORM IMS-STATUSKONTROLL                                           
195300     .                                                                    
195400     EJECT                                                                
195500 IMS-ISRT-INLA2-INLA21 SECTION.                                           
195600                                                                          
195700     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
195800          DELIMITED BY SIZE INTO SSA1                                     
195900     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
196000          DELIMITED BY SIZE INTO SSA2                                     
196100     MOVE 'W6INLA21 ' TO SSA3                                             
196200     MOVE '  ' TO GODK-STATUSKODER                                        
196300     CALL CBLTDLI USING ISRT INLA2-PCB DLI-IO-AREA SSA1 SSA2 SSA3         
196400     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
196500     PERFORM IMS-STATUSKONTROLL                                           
196600     .                                                                    
196700     EJECT                                                                
196800 IMS-GHU-INLA3-INLA01    SECTION.                                         
196900                                                                          
197000     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
197100          DELIMITED BY SIZE INTO SSA1                                     
197200     MOVE '    ' TO GODK-STATUSKODER                                      
197300     CALL CBLTDLI USING GHU INLA3-PCB DLI-IO-AREA SSA1                    
197400     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
197500     PERFORM IMS-STATUSKONTROLL                                           
197600     .                                                                    
197700     SKIP3                                                                
197800 IMS-REPL-INLA3-INLA01  SECTION.                                          
197900                                                                          
198000     MOVE '    ' TO GODK-STATUSKODER                                      
198100     CALL CBLTDLI USING REPL INLA3-PCB DLI-IO-AREA                        
198200     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
198300     PERFORM IMS-STATUSKONTROLL                                           
198400     .                                                                    
198500     SKIP3                                                                
198600 IMS-GU-INLA3-INLA11-OKVAL  SECTION.                                      
198700                                                                          
198800     STRING 'W6INLA01*P(W6D101KY =' W-W6D101KY-X ')'                      
198900          DELIMITED BY SIZE INTO SSA1                                     
199000     MOVE 'W6INLA11'     TO SSA2                                          
199100     MOVE '  GE' TO GODK-STATUSKODER                                      
199200     CALL CBLTDLI USING GU INLA3-PCB DLI-IO-AREA SSA1 SSA2                
199300     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
199400     PERFORM IMS-STATUSKONTROLL                                           
199500     .                                                                    
199600     SKIP3                                                                
199700 IMS-GU-INLA3-INLA01  SECTION.                                            
199800                                                                          
199900     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
200000          DELIMITED BY SIZE INTO SSA1                                     
200100     MOVE '  GE' TO GODK-STATUSKODER                                      
200200     CALL CBLTDLI USING GU INLA3-PCB DLI-IO-AREA SSA1                     
200300     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
200400     PERFORM IMS-STATUSKONTROLL                                           
200500     .                                                                    
200600     SKIP3                                                                
200700 IMS-GNP-INLA3-INLA11-OKVAL  SECTION.                                     
200800                                                                          
200900     MOVE 'W6INLA11'     TO SSA1                                          
201000     MOVE '  GE' TO GODK-STATUSKODER                                      
201100     CALL CBLTDLI USING GNP INLA3-PCB DLI-IO-AREA SSA1                    
201200     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
201300     PERFORM IMS-STATUSKONTROLL                                           
201400     .                                                                    
201500     SKIP3                                                                
201600 IMS-GNP-INLA3-INLA11    SECTION.                                         
201700                                                                          
201800     MOVE 'W6INLA11'     TO SSA1                                          
201900     MOVE '  GE' TO GODK-STATUSKODER                                      
202000     CALL CBLTDLI USING GNP INLA3-PCB DLI-IO-AREA SSA1                    
202100     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
202200     PERFORM IMS-STATUSKONTROLL                                           
202300     .                                                                    
202400     SKIP3                                                                
202500                                                                          
202600 IMS-GHU-INLA3-INLA11 SECTION.                                            
202700                                                                          
202800     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
202900          DELIMITED BY SIZE INTO SSA1                                     
203000     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
203100          DELIMITED BY SIZE INTO SSA2                                     
203200     MOVE '  GE' TO GODK-STATUSKODER                                      
203300     CALL CBLTDLI USING GHU INLA3-PCB DLI-IO-AREA SSA1 SSA2               
203400     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
203500     PERFORM IMS-STATUSKONTROLL                                           
203600     .                                                                    
203700     SKIP3                                                                
203800 IMS-REPL-INLA3-INLA11 SECTION.                                           
203900                                                                          
204000     MOVE '    ' TO GODK-STATUSKODER                                      
204100     CALL CBLTDLI USING REPL INLA3-PCB DLI-IO-AREA                        
204200     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
204300     PERFORM IMS-STATUSKONTROLL                                           
204400     .                                                                    
204500     EJECT                                                                
204600 IMS-DLET-INLA3-INLA11 SECTION.                                           
204700                                                                          
204800     MOVE '    ' TO GODK-STATUSKODER                                      
204900     CALL CBLTDLI USING DLET INLA3-PCB DLI-IO-AREA                        
205000     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
205100     PERFORM IMS-STATUSKONTROLL                                           
205200     .                                                                    
205300     EJECT                                                                
205400 IMS-GHNP-INLA3-INLA21 SECTION.                                           
205500                                                                          
205600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
205700          DELIMITED BY SIZE INTO SSA1                                     
205800     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
205900          DELIMITED BY SIZE INTO SSA2                                     
206000     MOVE 'W6INLA21' TO SSA3                                              
206100     MOVE '  GE' TO GODK-STATUSKODER                                      
206200     CALL CBLTDLI USING GHNP INLA3-PCB DLI-IO-AREA SSA1 SSA2 SSA3         
206300     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
206400     PERFORM IMS-STATUSKONTROLL                                           
206500     .                                                                    
206600     EJECT                                                                
206700 IMS-DLET-INLA3-INLA21 SECTION.                                           
206800                                                                          
206900     MOVE '    ' TO GODK-STATUSKODER                                      
207000     CALL CBLTDLI USING DLET INLA3-PCB DLI-IO-AREA                        
207100     MOVE INLA3-STATUS-CODE TO STATUS-WS                                  
207200     PERFORM IMS-STATUSKONTROLL                                           
207300     .                                                                    
207400     EJECT                                                                
207500 IMS-GU-LEVA01 SECTION.                                                   
207600     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
207700          DELIMITED BY SIZE INTO SSA1                                     
207800     MOVE '  GE' TO GODK-STATUSKODER                                      
207900     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA SSA1                      
208000     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
208100     PERFORM IMS-STATUSKONTROLL                                           
208200     .                                                                    
208300     SKIP3                                                                
208400 IMS-GU-ARTC01 SECTION.                                                   
208500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
208600          DELIMITED BY SIZE INTO SSA1                                     
208700     MOVE '  GE' TO GODK-STATUSKODER                                      
208800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
208900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
209000     PERFORM IMS-STATUSKONTROLL                                           
209100     .                                                                    
209200     SKIP3                                                                
209300 IMS-GNP-ARTC11 SECTION.                                                  
209400     MOVE 'WLARTC11 ' TO SSA1                                             
209500     MOVE '  GE' TO GODK-STATUSKODER                                      
209600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
209700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
209800     PERFORM IMS-STATUSKONTROLL                                           
209900     .                                                                    
210000     SKIP3                                                                
210100 IMS-GNP-ARTC21 SECTION.                                                  
210200                                                                          
210300     STRING 'WLARTC21(DAPRLIST>=' W-DAPRLIST-21-N                         
210400                    '&IDLEVNR  =' W-IDLEVNR-21-X ')'                      
210500          DELIMITED BY SIZE INTO SSA1                                     
210600     MOVE '  GE' TO GODK-STATUSKODER                                      
210700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC21 SSA1              
210800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
210900     PERFORM IMS-STATUSKONTROLL                                           
211000     .                                                                    
211100     SKIP3                                                                
211200 IMS-GU-WDK711 SECTION.                                                   
211300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
211400          DELIMITED BY SIZE INTO SSA1                                     
211500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
211600          DELIMITED BY SIZE INTO SSA2                                     
211700     MOVE '  GE' TO GODK-STATUSKODER                                      
211800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
211900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
212000     PERFORM IMS-STATUSKONTROLL                                           
212100     .                                                                    
212200     SKIP3                                                                
212300 IMS-GU-WDK712 SECTION.                                                   
212400     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
212500          DELIMITED BY SIZE INTO SSA1                                     
212600     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
212700          DELIMITED BY SIZE INTO SSA2                                     
212800     MOVE '  ' TO GODK-STATUSKODER                                        
212900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
213000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
213100     PERFORM IMS-STATUSKONTROLL                                           
213200     .                                                                    
213300     EJECT                                                                
213400 IMS-GNP-WDK724 SECTION.                                                  
213500     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
213600                    '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                      
213700          DELIMITED BY SIZE INTO SSA1                                     
213800     MOVE '  GE' TO GODK-STATUSKODER                                      
213900     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
214000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
214100     PERFORM IMS-STATUSKONTROLL                                           
214200     .                                                                    
214300     SKIP3                                                                
214400 IMS-GU-BENA11 SECTION.                                                   
214500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
214600          DELIMITED BY SIZE INTO SSA1                                     
214700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
214800          DELIMITED BY SIZE INTO SSA2                                     
214900     MOVE '  GE' TO GODK-STATUSKODER                                      
215000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
215100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
215200     PERFORM IMS-STATUSKONTROLL                                           
215300     .                                                                    
215400     SKIP3                                                                
215500 IMS-GET-WDD902 SECTION.                                                  
215600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
215700          DELIMITED BY SIZE INTO SSA1                                     
215800     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
215900          DELIMITED BY SIZE INTO SSA2                                     
216000     MOVE '  GE' TO GODK-STATUSKODER                                      
216100     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD9 SSA1 SSA2                 
216200     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
216300     PERFORM IMS-STATUSKONTROLL                                           
216400     .                                                                    
216500     SKIP3                                                                
216600 IMS-GNP-WDD905 SECTION.                                                  
216700     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
216800          DELIMITED BY SIZE INTO SSA1                                     
216900     MOVE '  GE' TO GODK-STATUSKODER                                      
217000     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD9 SSA1                     
217100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
217200     PERFORM IMS-STATUSKONTROLL                                           
217300     .                                                                    
217400     SKIP3                                                                
217500                                                                          
217600 IMS-GU-WDB601    SECTION.                                                
217700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
217800          DELIMITED BY SIZE INTO SSA1                                     
217900     MOVE '    ' TO GODK-STATUSKODER                                      
218000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
218100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
218200     PERFORM IMS-STATUSKONTROLL                                           
218300     IF SEGMENT-SAKNAS                                                    
218400        MOVE SPACE TO DCS-KDDC                                            
218500     END-IF                                                               
218600     .                                                                    
218700 IMS-STATUSKONTROLL SECTION.                                              
218800     SKIP2                                                                
218900     SET STATUS-IX TO 1                                                   
219000     SEARCH GODK-STATUS                                                   
219100       AT END                                                             
219200         MOVE 'FEL VID IMS CALL' TO FELTEXT-STR                           
219300         CALL FELLOG                                                      
219400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
219500         CONTINUE                                                         
219600     END-SEARCH                                                           
219700     .                                                                    
