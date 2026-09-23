000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2714100.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   94/11/07.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        AKTIVERAR ELLER SKAPAR ARTIKLAR PÅ WDK7.                         
001100*        KONTROLL GÖRS MOT STYRTABELL FÖR AKTIVERING PÅ WDB614.           
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDK7                                       
001400*                                                                         
001500*        KAN KÖRAS OM FRÅN BÖRJAN EFTER ABEND. INGET ÅTERSTARTS           
001600*        SEGMENT ANVÄNDS.                                                 
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  SVAR FRÅN WDATKONV EJ OK                                
002000*              -  "ÖVERSÄTTNING" AV DC TILL DC-INDX SAKNAS                
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300*****************************************************************         
002400****                      C H A N G E L O G                               
002500*****************************************************************         
002600*                                                                         
002700* 2011-05-02  SO  E-TRACKER 7936414  STOCK STEERING IN PULS               
002800*                                                                         
002900* 2011-09-01  SO  E-TRACKER 10147523 RÄTTA FEL I SPEC CR 7936414          
003000*                 ENLIGT PATRIK L. ANTAL PUBW BILD 2367 NY KOLL.          
003100*                                                                         
003200* 2015-10-05  RR  E-TRACKER 10197037 ACTIVATE 9-CODED PARTS.              
003300*                                                                         
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP2                                                                
003800 INPUT-OUTPUT SECTION.                                                    
003900                                                                          
004000 FILE-CONTROL.                                                            
004100     SKIP2                                                                
004200*          --- ARTIKLAR SOM SKALL AKTIVERAS                               
004300     SELECT W27140                     ASSIGN TO W27141D1.                
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP3                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
004900 FD  W27140                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  -COPY W27140      -L.                                                
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600     SKIP2                                                                
005700*    -COPY WY2000W2                                                       
005800     SKIP3                                                                
005900*    -COPY WY2000W3                                                       
006000     SKIP3                                                                
006100 77  IDPGM                       PIC X(8)    VALUE 'W2714100'.            
006200 01  CHKP-VAR.                                                            
006300 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
006400 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
006500 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
006600 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
006700 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
006800 03  CHKP-MAX                    PIC S9(3)   VALUE +10.                   
006900 77  JA                          PIC X       VALUE 'J'.                   
007000 77  NEJ                         PIC X       VALUE 'N'.                   
007100 77  STOPP                       PIC X       VALUE 'S'.                   
007200                                                                          
007300 77 CURRENT-SECTION              PIC X(30)   VALUE SPACE.                 
007400                                                                          
007500*--------------------------------------------------------------           
007600 01  WS-SUM-KVPB-TOT             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
007700 01  WS-KVPB-REF-TOT             PIC S9(6)V9(1) VALUE ZERO COMP-3.        
007800 01  WS-IDLEVNR                  PIC X(5)       VALUE SPACE.              
007900                                                                          
008000 01  WS-SUM-KVPB-JFR             PIC 9(6)V9(1) VALUE ZERO.                
008100 01  FILLER REDEFINES WS-SUM-KVPB-JFR.                                    
008200     03  WS-SUM-KVPB-HELTAL      PIC 9(6).                                
008300     03  WS-SUM-KVPB-DECIMAL     PIC 9(1).                                
008400                                                                          
008500 01  WS-VLARTNTO-JFR             PIC 9(8)V9(1) VALUE ZERO.                
008600 01  FILLER REDEFINES WS-VLARTNTO-JFR.                                    
008700     03  WS-VLARTNTO-HELTAL      PIC 9(8).                                
008800     03  WS-VLARTNTO-DECIMAL     PIC 9(1).                                
008900                                                                          
009000 01  WS-PRARTSTD-JFR             PIC 9(7)V9(2) VALUE ZERO.                
009100 01  FILLER REDEFINES WS-PRARTSTD-JFR.                                    
009200     03  WS-PRARTSTD-HELTAL      PIC 9(7).                                
009300     03  WS-PRARTSTD-DECIMAL     PIC 9(2).                                
009400                                                                          
009500 01  WS-KVVECKOR-PUBV            PIC 9(5)    VALUE ZERO.                  
009600                                                                          
009700 01  WS-TIFINLV-AAVVD            PIC 9(5)    VALUE ZERO.                  
009800 01  FILLER REDEFINES WS-TIFINLV-AAVVD.                                   
009900     03 WS-TIFINLV-AAVV          PIC 9(4).                                
010000     03 WS-TIFINLV-D             PIC 9(1).                                
010100                                                                          
010200 01  WS-INNEVARANDE-TIVV         PIC 9(2)   VALUE ZERO.                   
010300                                                                          
010400 01  WS-TIREFSTA-6               PIC 9(6)    VALUE ZERO.                  
010500                                                                          
010510 01  WS-TODAY-DATE-30            PIC  9(6)   VALUE ZERO.                  
010520                                                                          
010600 01  WS-TIREFSTA-JFR             PIC 9(4)    VALUE ZERO.                  
010700 01  FILLER REDEFINES WS-TIREFSTA-JFR.                                    
010800     03 WS-TIREFSTA-JFR-AA       PIC 9(2).                                
010900     03 WS-TIREFSTA-JFR-VV       PIC 9(2).                                
011000                                                                          
011100 01  WS-TIKVOT-JFR               PIC 9(4)    VALUE ZERO.                  
011200 01  FILLER REDEFINES WS-TIKVOT-JFR.                                      
011300     03 WS-TIKVOT-TIAA           PIC 9(2).                                
011400     03 WS-TIKVOT-TIVV           PIC 9(2).                                
011500                                                                          
011600 01  DATUM-OMVANDLING.                                                    
011700     03  WS-INNEV-TIVV           PIC 9(2) VALUE ZERO.                     
011800     03  WS-FIRST-TIVV           PIC 9(2) VALUE ZERO.                     
011900     03  WS-INNEV-TIAARP         PIC 9(4) VALUE ZERO.                     
012000     03  WS-INNEV-TIRP           PIC 9(2) VALUE ZERO.                     
012100     03  WS-KVVIPER              PIC 9(1) VALUE ZERO.                     
012200                                                                          
012300 01  VECKA-I-PERIOD-TABELL.                                               
012400     03  VECKA-I-INNEV-PERIOD    OCCURS 5.                                
012500         05 AKTUELL-VECKA        PIC 9(2).                                
012600*--------------------------------------------------------------           
012700 01  WS-NO-OF-MONTHS             PIC S9(7) COMP-3 VALUE ZERO.             
012800 01  WS-CDC-ASSETS               PIC S9(7) COMP-3 VALUE ZERO.             
012900 01  WS-CDC-PROGNOSIS            PIC S9(6)V9(1) COMP-3 VALUE ZERO.        
013000 01  WS-KVAVIS                   PIC S9(7) COMP-3 VALUE ZERO.             
013100 01  WS-KVOKS                    PIC S9(7) COMP-3 VALUE ZERO.             
013200 01  WS-KVAVROP                  PIC S9(7) COMP-3 VALUE ZERO.             
013300 01  WS-KVBEART                  PIC S9(7) COMP-3 VALUE ZERO.             
013400 01  WS-KVPB-REF                 PIC S9(6)V9(1) COMP-3 VALUE ZERO.        
013500 01  WS-KVPBREOI                 PIC S9(6)V9(1) COMP-3 VALUE ZERO.        
013600*--------------------------------------------------------------           
013700                                                                          
013800*01  -COPY WWPRODSL                                                       
013900                                                                          
014000*01  -COPY WWDCKONS                                                       
014100*01  -COPY WWDC99                                                         
014200                                                                          
014300 77  VECKOSLUT-SW                PIC X       VALUE 'N'.                   
014400     88  VECKOSLUT                           VALUE 'J'.                   
014500                                                                          
014600 77  LOCAL-SOURCED-SW            PIC X       VALUE 'N'.                   
014700     88  LOCAL-SOURCED                       VALUE 'J'.                   
014800                                                                          
014900 77  PB-SEP-SW                   PIC X       VALUE 'N'.                   
015000     88  EXKLUD-PB-SEP                       VALUE 'J'.                   
015100                                                                          
015200 77  PG-SW                       PIC X       VALUE 'N'.                   
015300     88  PG-FINNS                            VALUE 'J'.                   
015400                                                                          
015500 77  AKTIVERING-OK-SW            PIC X       VALUE 'N'.                   
015600     88  AKTIVERA-ART-OK                     VALUE 'J'.                   
015700                                                                          
015800*    --- ARBETSFÄLT                                                       
015900 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT'.          
016000 01  ARBETSFAELT.                                                         
016100     03  WS-PERIODTABELL    OCCURS 12.                                    
016200         05 WS-TIAARP            PIC  9(4)   VALUE ZERO.                  
016300         05 WS-FORSTA-TIVV       PIC  9(2)   VALUE ZERO.                  
016400         05 WS-SISTA-TIVV        PIC  9(2)   VALUE ZERO.                  
016500                                                                          
016600     03  DAGENS-TIAARP           PIC  9(4)   VALUE ZERO.                  
016700     03  FILLER REDEFINES DAGENS-TIAARP.                                  
016800         05 DAGENS-TIAA          PIC  9(2).                               
016900         05 DAGENS-TIRP          PIC  9(2).                               
017000                                                                          
017100     03  DAGENS-TIVV             PIC  9(4)   VALUE ZERO.                  
017200                                                                          
017300     03  DAGENS-VECKA            PIC 9(5)    VALUE ZERO.                  
017400     03  FILLER REDEFINES DAGENS-VECKA.                                   
017500         05 DAGENS-VECKA-AAVV    PIC 9(4).                                
017600         05 DAGENS-VECKA-D       PIC 9(1).                                
017700                                                                          
017800     03  DAGENS-TIAAVVD-MINUS-25V PIC 9(5)   VALUE ZERO.                  
017900     03  FILLER REDEFINES DAGENS-TIAAVVD-MINUS-25V.                       
018000         05 WS-TIAAVV-MINUS-25V  PIC  9(4).                               
018100         05 WS-TID-MINUS-25V     PIC  9(1).                               
018200                                                                          
018300     03  WS-TIAARP-UTRAEKNING    PIC  9(4)   VALUE ZERO.                  
018400     03  FILLER REDEFINES WS-TIAARP-UTRAEKNING.                           
018500         05 WS-TIAA              PIC  9(2).                               
018600         05 WS-TIRP              PIC  9(2).                               
018700                                                                          
018800     03  WS-KVOT-TOT             PIC S9(11)     VALUE ZERO COMP-3.        
018900     03  WS-KVOT-AKT             PIC S9(11)     VALUE ZERO COMP-3.        
019000     03  WS-KVDISP-CDC           PIC S9(7)      VALUE ZERO COMP-3.        
019100     03  WS-KVPB-INNEV-8V-                                                
019200-               CDC-ALL-SDC      PIC S9(6)V9(2) VALUE ZERO COMP-3.        
019300                                                                          
019400     03 WS-ATTA-VECKOR           PIC  9(5)      VALUE ZERO.               
019500     03 WS-ATTA-VECKOR-AAVVD REDEFINES WS-ATTA-VECKOR.                    
019600        05 WS-ATTA-VECKOR-AAVV   PIC  9(4).                               
019700        05 WS-ATTA-VECKOR-D      PIC  9(1).                               
019800     03 WS-FLKTRL-AKT            PIC  X(1)   VALUE SPACE.                 
019900     03 WS-SUM-KVAVROP           PIC S9(7)           COMP-3.              
020000     03 WS-IDDC-PREV             PIC  X(2)   VALUE SPACE.                 
020100     03 WS-IDDC-SPAR             PIC  X(2)   VALUE SPACE.                 
020200     03 WS-IDDC-REF              PIC  X(2)   VALUE SPACE.                 
020300     03 WS-TISTOREF              PIC S9(5) VALUE ZERO COMP-3.             
020400     03 WS-PRIS                  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
020500     SKIP2                                                                
020600 77  W-W27140-KVPOST-IN          PIC S9(7)   VALUE ZERO COMP-3.           
020700 77  SPAR-TIAARP                 PIC 9(4)    VALUE ZERO.                  
020800 77  AKTIV                       PIC X(1)    VALUE 'A'.                   
020900 77  PASSIV                      PIC X(1)    VALUE 'P'.                   
021000 77  IX                          PIC 9(2)    VALUE ZERO.                  
021100 77  IX0                         PIC 9(2)    VALUE ZERO.                  
021200 77  IX1                         PIC 9(2)    VALUE ZERO.                  
021300 77  IX2                         PIC 9(2)    VALUE ZERO.                  
021400 77  MAX-IX1                     PIC 9(2)    VALUE 13.                    
021500 77  INDX                        PIC 9(2)    VALUE ZERO.                  
021600 77  INDX1                       PIC 9(3)    VALUE ZERO.                  
021700 77  PG-IX                       PIC 9(2)    VALUE ZERO.                  
021800 77  PG-IX-MAX                   PIC 9(2)    VALUE 11.                    
021900 77  DC-INDX                     PIC 9(2)    VALUE ZERO.                  
022000 77  RP-INDX                     PIC 9(2)    VALUE ZERO.                  
022100 77  MAX-RP-INDX                 PIC 9(2)    VALUE 12.                    
022200 77  ANTAL-ISRT-K711             PIC 9(5)    VALUE ZERO.                  
022300 77  ANTAL-REPL-K711             PIC 9(5)    VALUE ZERO.                  
022400                                                                          
022500 77  VV-INDX                     PIC 9(2)    VALUE ZERO.                  
022600 77  MAX-VV-INDX                 PIC 9(2)    VALUE 53.                    
022700                                                                          
022800 77  WKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
022900 77  WKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
023000       EJECT                                                              
023100 01  FELTEXT.                                                             
023200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
023300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
023400                                                                          
023500 77  W27140-EOF-SW               PIC X       VALUE 'N'.                   
023600     88  END-OF-W27140                       VALUE 'J'.                   
023700     EJECT                                                                
023800*    --- PARAMETRAR TILL DATKORT                                          
023900*                                                                         
024000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27141'.              
024100     SKIP2                                                                
024200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
024300     SKIP2                                                                
024400*01  -COPY WDATKORT                                                       
024500     EJECT                                                                
024600 01  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
024700     SKIP2                                                                
024800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
024900 01  FILLER REDEFINES DAGENS-DATUM.                                       
025000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
025100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
025200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
025300     EJECT                                                                
025400                                                                          
025500 01  WS-TABLE-WDB614.                                                     
025600     03 WS-TAB-IDDC              PIC X(2).                                
025700     03 WS-TAB-WDB614 OCCURS 13.                                          
025800        05 WS-TAB-KVAKT          PIC S9(3) COMP-3.                        
025900        05 WS-TAB-KVVECKOR-PUBV  PIC S9(3) COMP-3.                        
026000        05 WS-TAB-PRARTSTD       PIC 9(7).                                
026100        05 WS-TAB-VLARTNTO       PIC 9(8).                                
026200        05 WS-TAB-KVPB-SEP-REF   PIC S9(7) COMP-3.                        
026300        05 WS-TAB-KDPRODSL OCCURS 11 TIMES PIC S9(3) COMP-3.              
026400                                                                          
026500                                                                          
026600     EJECT                                                                
026700 01  DYNAMISKA-SUBPROGRAM.                                                
026800*                                                                         
026900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
027000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
027100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
027200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
027300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
027400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
027500     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
027600     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
027700     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
027800     EJECT                                                                
027900*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
028000 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
028100*   -COPY W005WDK7                                                        
028200     EJECT                                                                
028300*    --- PARAMETRAR TILL POSTSUM                                          
028400*                                                                         
028500*01  -COPY W0005   -PRE  POSTSUM-                                         
028600     EJECT                                                                
028700*01  -COPY WDATAREA                                                       
028800     EJECT                                                                
028900*01  -COPY W271AKT   -PRE AKT-                                            
029000     EJECT                                                                
029100*    --- PARAMETRAR TILL VECKOADD                                         
029200                                                                          
029300 01  W009VADD-AREA.                                                       
029400     03 VADD-DATUM-AAVV          PIC S9(5) VALUE ZERO COMP-3.             
029500     03 VADD-ANTAL               PIC S9(3) VALUE ZERO COMP-3.             
029600                                                                          
029700     EJECT                                                                
029800*    --- PARAMETRAR TILL WZ20DAYS                                         
029900*    -COPY WZ20DAYS                                                       
030000                                                                          
030100     EJECT                                                                
030200 01  IN-AREA-START               PIC X(24)   VALUE                        
030300                                             'IN-AREA-START'.             
030400     SKIP2                                                                
030500                                                                          
030600*01  AREA -COPY W27140     -PRE IN-                                       
030700*                                                                         
030800     EJECT                                                                
030900*                                                                         
031000*01    -COPY WWBYT03                                                      
031100     EJECT                                                                
031200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031300     SKIP3                                                                
031400 01  NYCKLAR-TILL-DLI.                                                    
031500     03  W-IDARTNR-X.                                                     
031600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
031700     03  W-IDDC-X.                                                        
031800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
031900     03  W-IDLAND-X.                                                      
032000         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
032100     03  W-IDDC-B6-X.                                                     
032200         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
032300                                                                          
032400     03  W-WDB614KY-X.                                                    
032500         05  W-AKT-KVAKT             PIC S9(3) VALUE 0 COMP-3.            
032600         05  W-AKT-KVVECKOR-PUBV     PIC S9(3) VALUE 0 COMP-3.            
032700         05  W-AKT-PRARTSTD          PIC 9(7)  VALUE 0.                   
032800         05  W-AKT-VLARTNTO          PIC 9(8)  VALUE 0.                   
032900         05  W-AKT-KVPB-SEP-REF      PIC S9(7) VALUE 0 COMP-3.            
033000         05  W-AKT-KDPRODSL          PIC S9(3) VALUE 0 COMP-3.            
033100                                                                          
033200                                                                          
033300     03  W-KDSEGKEY-X.                                                    
033400         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
033500                                                                          
033600     03  W-DAINLEV-X.                                                     
033700         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
033800                                                                          
033900     03  W-IDPTYP-X.                                                      
034000         05  W-IDPTYP            PIC X(3)    VALUE '310'.                 
034100                                                                          
034200     03  W-WDD901KY-X.                                                    
034300         05  W-IDARTNR-WDD9      PIC S9(9) VALUE 0 COMP-3.                
034400         05  W-IDDC-WDD9         PIC X(2)  VALUE SPACE.                   
034500     03  W-IDLEVNR-X.                                                     
034600         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
034700     03  W-KDAVROP-X.                                                     
034800         05  W-KDAVROP           PIC S9  COMP-3 VALUE 2.                  
034900                                                                          
035000     SKIP2                                                                
035100*    --- STATUS-KOD FRÅN IMS                                              
035200 01  STATUS-WS                   PIC XX.                                  
035300     88  SEGMENT-FINNS                       VALUE '  '.                  
035400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
035500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
035600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
035700     88  IMS-EJ-OK                           VALUE 'XD'.                  
035800     SKIP2                                                                
035900 01  GODK-STATUSKODER.                                                    
036000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036100     SKIP3                                                                
036200 01  SSA1                        PIC X(64).                               
036300 01  SSA2                        PIC X(64).                               
036400 01  SSA3                        PIC X(64).                               
036500     EJECT                                                                
036600*    --- IMS FUNKTIONSKODER                                               
036700*01  -COPY W0003                                                          
036800     EJECT                                                                
036900*    ---  DLI INPUT-OUTPUT AREA                                           
037000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
037100     SKIP3                                                                
037200 01  DLI-IO-AREA-WDK601.                                                  
037300*        05  -COPY WDK601                                                 
037400     EJECT                                                                
037500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
037600     SKIP3                                                                
037700 01  DLI-IO-AREA-WDK611.                                                  
037800*        05  -COPY WDK611                                                 
037900     EJECT                                                                
038000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK61129'.           
038100     SKIP3                                                                
038200 01  DLI-IO-AREA-WDK61129.                                                
038300*        05  -COPY WDK611  -PRE K6-                                       
038400*        05  -COPY WDK629  -PRE K6-                                       
038500     EJECT                                                                
038600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
038700     SKIP3                                                                
038800 01  DLI-IO-AREA-WDK701.                                                  
038900*        05  -COPY WDK701                                                 
039000     EJECT                                                                
039100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
039200     SKIP3                                                                
039300 01  DLI-IO-AREA-WDK711.                                                  
039400*        05  -COPY WDK711                                                 
039500                                                                          
039600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK712'.             
039700     SKIP3                                                                
039800 01  DLI-IO-AREA-WDK712.                                                  
039900*        05  -COPY WDK712                                                 
040000                                                                          
040100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK901'.             
040200 01  DLI-IO-WDK901.                                                       
040300*    03  -COPY WDK901   -PRE WDK9-                                        
040400     EJECT                                                                
040500                                                                          
040600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD704'.             
040700 01  DLI-IO-WDD704.                                                       
040800*    03  -COPY WDD704   -PRE WDD704-                                      
040900     EJECT                                                                
041000                                                                          
041100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
041200 01   DLI-IO-AREA-B601.                                                   
041300*     03  -COPY WDB601                                                    
041400     EJECT                                                                
041500                                                                          
041600 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDB614'.              
041700 01  DLI-IO-WDB614.                                                       
041800*    03  -COPY WDB614   -PRE WDB614-                                      
041900                                                                          
042000 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA11'.                      
042100 01  DLI-IO-OIGA11.                                                       
042200*    03  -COPY WDL711                                                     
042300     EJECT                                                                
042400                                                                          
042500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL2'.               
042600 01  DLI-IO-WDL2             PIC X(150) VALUE SPACE.                      
042700*01  -COPY WDL201       -PRE WDL2-  -RED DLI-IO-WDL2.                     
042800*01  -COPY WDL211       -PRE WDL2-  -RED DLI-IO-WDL2.                     
042900*01  -COPY WDL221       -PRE WDL2-  -RED DLI-IO-WDL2.                     
043000     EJECT                                                                
043100                                                                          
043200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD9'.               
043300 01  DLI-IO-WDD9             PIC X(100) VALUE SPACE.                      
043400*01  -COPY WDD901       -PRE WDD9-  -RED DLI-IO-WDD9.                     
043500*01  -COPY WDD902       -PRE WDD9-  -RED DLI-IO-WDD9.                     
043600*01  -COPY WDD905       -PRE WDD9-  -RED DLI-IO-WDD9.                     
043700     EJECT                                                                
043800                                                                          
043900 LINKAGE SECTION.                                                         
044000                                                                          
044100*01  -COPY W0009   -PRE MSG-                                              
044200     EJECT                                                                
044300*01  -COPY W0008  -PRE WDK7-1-                                            
044400     05  FILLER                  PIC X.                                   
044500*01  -COPY W0008  -PRE WDK6-                                              
044600     05  FILLER                  PIC X.                                   
044700*01  -COPY W0008  -PRE WDB6-                                              
044800     05  FILLER                  PIC X.                                   
044900*01  -COPY W0008  -PRE 2501-                                              
045000     05  FILLER                  PIC X.                                   
045100*01  -COPY W0008  -PRE WDK9-                                              
045200     05  FILLER                  PIC X.                                   
045300*01  -COPY W0008  -PRE WDD7-                                              
045400     05  FILLER                  PIC X.                                   
045500*01  -COPY W0008  -PRE WDK7-2-                                            
045600     05  FILLER                  PIC X.                                   
045700*01  -COPY W0008  -PRE OIGA-                                              
045800     05  FILLER                  PIC X.                                   
045900*01  -COPY W0008  -PRE WDL2-                                              
046000     05  FILLER                  PIC X.                                   
046100*01  -COPY W0008  -PRE WDD9-                                              
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
046400 PROCEDURE DIVISION  USING MSG-PCB WDK7-1-PCB WDK6-PCB                    
046500                                   WDB6-PCB 2501-PCB                      
046600                                   WDK9-PCB WDD7-PCB                      
046700                                   WDK7-2-PCB OIGA-PCB                    
046800                                   WDL2-PCB WDD9-PCB.                     
046900     ENTRY 'DLITCBL' USING MSG-PCB WDK7-1-PCB WDK6-PCB                    
047000                                   WDB6-PCB 2501-PCB                      
047100                                   WDK9-PCB WDD7-PCB                      
047200                                   WDK7-2-PCB OIGA-PCB                    
047300                                   WDL2-PCB WDD9-PCB.                     
047400                                                                          
047500     PERFORM A-INIT                                                       
047600     PERFORM S01-LAES-W27140                                              
047700     PERFORM UNTIL END-OF-W27140                                          
047800       IF CHKP-ANT > CHKP-MAX                                             
047900         PERFORM X-TAG-CHECKPOINT                                         
048000       END-IF                                                             
048100                                                                          
048200       MOVE IN-IDARTNR          TO W-IDARTNR                              
048300       MOVE IN-IDDC             TO W-IDDC                                 
048400                                   WS-IDDC                                
048500       PERFORM IMS-GU-WDK711                                              
048600       IF SEGMENT-SAKNAS                                                  
048700         MOVE SPACE         TO WS-IDDC-REF                                
048800         PERFORM B-KOLLA-OM-AKTIVERING                                    
048900       ELSE                                                               
049000***TILLAGT FÖR PROBLEM MED AKTIVERADE  IF SLAG-KDREFSTA = 'P'             
049100***SOM BLIR AKTIVERADE IGEN 190322.                                       
049200         IF SLAG-KDREFSTA = 'P'                                           
049300           IF SLAG-IDDC-REF = SPACE AND                                   
049400              (NDC-CN OR NDC-US)                                          
049500              CONTINUE                                                    
049600           ELSE                                                           
049700              MOVE SLAG-IDDC-REF TO WS-IDDC-REF                           
049800              PERFORM B-KOLLA-OM-AKTIVERING                               
049900           END-IF                                                         
050000         END-IF                                                           
050100       END-IF                                                             
050200                                                                          
050300       PERFORM S01-LAES-W27140                                            
050400     END-PERFORM                                                          
050500                                                                          
050600     DISPLAY 'ISRT-711:' ANTAL-ISRT-K711                                  
050700     DISPLAY 'REPL-711:' ANTAL-REPL-K711                                  
050800     PERFORM Z-FINIT                                                      
050900                                                                          
051000     MOVE ZERO TO RETURN-CODE                                             
051100     GOBACK                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 A-INIT SECTION.                                                          
051500     SKIP2                                                                
051600                                                                          
051700     PERFORM IMS-RESTART                                                  
051800                                                                          
051900     OPEN INPUT W27140                                                    
052000                                                                          
052100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
052200                                                                          
052300     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-SEKEL                
052400                                                                          
052500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
052600     MOVE D-AAR        TO DAGENS-DATUM-AAR                                
052700     MOVE D-MAANAD     TO DAGENS-DATUM-MAANAD                             
052800     MOVE D-DAG        TO DAGENS-DATUM-DAG                                
052900     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
053000                                                                          
053100     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
053200     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
053300                                                                          
053400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
053500                         DAT-O-TIDATUM DAT-KDSVAR                         
053600                                                                          
053700     IF DAT-KDSVAR-OK                                                     
053800       MOVE DAT-TIAARP  TO DAGENS-TIAARP                                  
053900       MOVE DAT-TIVV    TO DAGENS-TIVV                                    
054000       MOVE DAT-TIAAVVD TO DAGENS-TIAAVVD-MINUS-25V                       
054100                           DAGENS-VECKA                                   
054200     ELSE                                                                 
054300       MOVE 'SVAR 1 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
054400       DISPLAY FELTEXT                                                    
054500       PERFORM S99-ABEND                                                  
054600     END-IF                                                               
054700                                                                          
054800     IF DAGENS-VECKA-D > 4                                                
054900        MOVE JA TO VECKOSLUT-SW                                           
055000     END-IF                                                               
055010* TODAYS DATE MINUS 30 DAYS                                               
055020     MOVE  'YYMMDD'            TO DAYS-KDDATFMT1                          
055030     MOVE  'YYMMDD'            TO DAYS-KDDATFMT2                          
055040     MOVE  DAGENS-DATUM        TO DAYS-TIDATE1                            
055050     MOVE  -30                 TO DAYS-KVDAYS                             
055060     MOVE  SPACE               TO DAYS-TIDATE2                            
055070                                  DAYS-IDCALEND                           
055080     CALL  WZ20DAYS USING DAYS-WZ20DAYS                                   
055090                                                                          
055091     MOVE  DAYS-TIDATE2(1:6)   TO WS-TODAY-DATE-30                        
055100                                                                          
055200*    BERÄKNA VECKA 25 VECKOR BAKÅT                                        
055300                                                                          
055400     MOVE WS-TIAAVV-MINUS-25V TO VADD-DATUM-AAVV                          
055500     MOVE -25                 TO VADD-ANTAL                               
055600                                                                          
055700     CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL                       
055800                                                                          
055900     MOVE VADD-DATUM-AAVV   TO WS-TIAAVV-MINUS-25V                        
056000                                                                          
056100     PERFORM AA-INITERA-PERIODTABELL                                      
056200     .                                                                    
056300     EJECT                                                                
056400 AA-INITERA-PERIODTABELL SECTION.                                         
056500     MOVE 'AA-INITERA-PERIODTABELL '  TO CURRENT-SECTION                  
056600*--------------------------------------------------------------*          
056700* HÄR INITIERAS PERIODTABELLEN. INDX 1 MOTSVARAR SENASTE HELA  *          
056800* PERIOD, INDX 2 PERIODEN DESSFÖRINNAN OSV. TABELLEN INITIERAS *          
056900* MED PERIOD + START/SLUTVECKA FÖR ATT KUNNA UTFÖRA SUMMERINGAR*          
057000*--------------------------------------------------------------*          
057100                                                                          
057200     MOVE +1                       TO RP-INDX                             
057300                                                                          
057400     PERFORM UNTIL RP-INDX         >  MAX-RP-INDX                         
057500       IF RP-INDX                  =  1                                   
057600*------- BERÄKNA SENASTE HELA PERIOD                                      
057700         MOVE DAGENS-TIAARP        TO WS-TIAARP-UTRAEKNING                
057800         SUBTRACT 1                FROM WS-TIRP                           
057900         IF WS-TIRP                =  ZERO                                
058000           MOVE 12                 TO WS-TIRP                             
058100           IF WS-TIAA              =  ZERO                                
058200             MOVE 99               TO WS-TIAA                             
058300           ELSE                                                           
058400             SUBTRACT 1            FROM WS-TIAA                           
058500           END-IF                                                         
058600         END-IF                                                           
058700       ELSE                                                               
058800*------- BERÄKNA FÖREGÅENDE PERIOD                                        
058900         SUBTRACT 1                FROM WS-TIRP                           
059000         IF WS-TIRP                = ZERO                                 
059100           MOVE 12                 TO WS-TIRP                             
059200           IF WS-TIAA              =  ZERO                                
059300             MOVE 99               TO WS-TIAA                             
059400           ELSE                                                           
059500             SUBTRACT              1 FROM WS-TIAA                         
059600           END-IF                                                         
059700         END-IF                                                           
059800       END-IF                                                             
059900                                                                          
060000*----- TA REDA PÅ STARTVECKA FÖR PERIODEN                                 
060100       MOVE 'AARP  '               TO DAT-KDDATFORM                       
060200       MOVE WS-TIAARP-UTRAEKNING   TO DAT-I-TIDATUM                       
060300       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
060400                           DAT-O-TIDATUM DAT-KDSVAR                       
060500       IF DAT-KDSVAR-OK                                                   
060600         MOVE WS-TIAARP-UTRAEKNING TO WS-TIAARP      (RP-INDX)            
060700         MOVE DAT-TIVV             TO WS-FORSTA-TIVV (RP-INDX)            
060800       ELSE                                                               
060900         MOVE 'SVAR 2 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                 
061000         DISPLAY FELTEXT                                                  
061100         PERFORM S99-ABEND                                                
061200       END-IF                                                             
061300                                                                          
061400       IF RP-INDX                  = 1                                    
061500*------- TA REDA PÅ SISTA VECKAN FÖR INNEVARANDE PERIOD                   
061600*------- GENOM ATT ANVÄNDA FÖRSTA VECKAN I NÄSTA PERIOD                   
061700         MOVE WS-TIAARP-UTRAEKNING TO SPAR-TIAARP                         
061800         ADD 1                     TO WS-TIRP                             
061900         IF WS-TIRP                >  12                                  
062000           ADD  1                  TO WS-TIAA                             
062100           MOVE 1                  TO WS-TIRP                             
062200         END-IF                                                           
062300         MOVE WS-TIAARP-UTRAEKNING TO DAT-I-TIDATUM                       
062400         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
062500                             DAT-O-TIDATUM DAT-KDSVAR                     
062600         IF DAT-KDSVAR-OK                                                 
062700           MOVE DAT-TIVV        TO WS-SISTA-TIVV (1)                      
062800           SUBTRACT 1           FROM WS-SISTA-TIVV (1)                    
062900           IF WS-SISTA-TIVV (1) = ZERO                                    
063000             MOVE 53            TO WS-SISTA-TIVV (1)                      
063100           END-IF                                                         
063200         ELSE                                                             
063300           MOVE 'SVAR 3 FRÅN WDATKONV EJ OK' TO FELTEXT-STR               
063400           DISPLAY FELTEXT                                                
063500           PERFORM S99-ABEND                                              
063600         END-IF                                                           
063700                                                                          
063800         MOVE SPAR-TIAARP           TO WS-TIAARP-UTRAEKNING               
063900                                                                          
064000       END-IF                                                             
064100                                                                          
064200       IF RP-INDX         < 12                                            
064300*------- FÖRSTA VECKAN FÖR BERÄKNAD PERIOD                                
064400*------- ANVÄNDS FÖR ATT FÅ FRAM SISTA VECKAN                             
064500*------- FÖR PERIODEN DESSFÖRINNAN                                        
064600         MOVE WS-FORSTA-TIVV (RP-INDX) TO                                 
064700                                 WS-SISTA-TIVV (RP-INDX + 1)              
064800         SUBTRACT 1              FROM WS-SISTA-TIVV (RP-INDX + 1)         
064900         IF WS-SISTA-TIVV (RP-INDX + 1) = ZERO                            
065000           MOVE 53               TO WS-SISTA-TIVV (RP-INDX + 1)           
065100         END-IF                                                           
065200       END-IF                                                             
065300                                                                          
065400*----- SE TILL ATT FÖRSTA VECKAN ALLTID ÄR 01                             
065500*----- FÖR 'INLEDNINGSPERIODEN' VARJE ÅR                                  
065600       IF WS-TIAARP (RP-INDX) (3:2) = 01                                  
065700         MOVE 1                     TO WS-FORSTA-TIVV (RP-INDX)           
065800       END-IF                                                             
065900                                                                          
066000       ADD +1                       TO RP-INDX                            
066100     END-PERFORM                                                          
066200                                                                          
066300*--- OM VECKOR I FÖRSTA (INDX=1) OCH SISTA (INDX=12) PERIODEN             
066400*--- ÖVERLAPPAR VARANDRA, RÄTTA SISTA (ÄLDSTA) PERIODEN                   
066500*--- DVS ELIMINERA DE GEMENSAMMA VECKORNA.                                
066600                                                                          
066700*--- EX. FÖRSTA PERIODEN = 9210 MED VECKORNA  40 - 44                     
066800*---     SISTA  PERIODEN = 9111 MED VECKORNA  44 - 48                     
066900*---     REDUCERA SISTA PERIODENS VECKOR TILL 45 - 48                     
067000                                                                          
067100*--- RÄTTNING GÖRS AV MAX 2 ÖVERLAPPANDE VECKOR                           
067200                                                                          
067300     IF WS-SISTA-TIVV(1)          = WS-FORSTA-TIVV(12)  OR                
067400        WS-SISTA-TIVV(1)          = WS-FORSTA-TIVV(12) + 1                
067500       COMPUTE WS-FORSTA-TIVV(12) = WS-SISTA-TIVV(1) + 1                  
067600     END-IF                                                               
067700                                                                          
067800     .                                                                    
067900     EJECT                                                                
068000 B-KOLLA-OM-AKTIVERING SECTION.                                           
068100     MOVE 'B-KOLLA-OM-AKTIVERING'  TO CURRENT-SECTION                     
068200                                                                          
068300     MOVE IN-IDDC TO W-IDDC-B6                                            
068400     IF IN-IDDC NOT = WS-IDDC-PREV                                        
068500       MOVE IN-IDDC             TO WS-IDDC-PREV                           
068600       PERFORM IMS-GU-WDB601                                              
068700       IF DCS-DDC                                                         
068800         CONTINUE                                                         
068900       ELSE                                                               
069000         PERFORM C-FYLL-WDB614-TABELL                                     
069100       END-IF                                                             
069200     END-IF                                                               
069300                                                                          
069400     IF DCS-KDDC > SPACE AND NOT DCS-DDC                                  
069500                                                                          
069600       MOVE IN-IDARTNR          TO W-IDARTNR                              
069700       MOVE IN-IDDC             TO W-IDDC                                 
069800                                                                          
069900       MOVE JA                  TO WS-FLKTRL-AKT                          
070000       PERFORM IMS-GU-WDK601                                              
070100       PERFORM IMS-GNP-WDK611                                             
070200                                                                          
070300       IF CLAG-KDERS > ZERO                                               
070400         PERFORM BE-KTRL-AKTIVERING                                       
070500       END-IF                                                             
070600                                                                          
070700       MOVE W-IDARTNR           TO BYT03-IDARTNR                          
070800                                                                          
070900       MOVE ART-KDPRODSL        TO TEST-KDPRODSL                          
071000       IF (WS-FLKTRL-AKT = JA                                             
071310       AND ( DCS-NDC                                                      
071340       OR ( DCS-SDC                                                       
071400            AND CLAG-FLREFILL = JA)                                       
071500*            DCS-FLEXCP1-REFBER = JA ERSÄTTER LDC 1A                      
071600       OR (DCS-FLEXCP1-REFBER = JA                                        
071700       AND (KDPRODSL-BIMA                                                 
071800       AND ART-IDFKNGRP NOT = 1969)                                       
071900       AND NOT (BYT03-OBJEKT                                              
072000       OR       CLAG-FLLSRDEL = NEJ))))                                   
072100       AND ART-KDSORT NOT = 'SW'                                          
072200                                                                          
072300         PERFORM BB-BERAEKNA-KVOT                                         
072400                                                                          
072500         PERFORM BA-KOLLA-REGLER-AKTIVERING                               
072600                                                                          
072700         IF AKTIVERING-OK-SW  = JA                                        
072800            PERFORM IMS-GHU-WDK711                                        
072900                                                                          
073000            IF SEGMENT-FINNS                                              
073100               IF SLAG-FLPB-FLYTT = 'N'                                   
073200                  PERFORM BC-AKTIVERING-WDK7                              
073300               END-IF                                                     
073400            ELSE                                                          
073500               PERFORM BD-SKAPA-WDK7                                      
073600            END-IF                                                        
073700         END-IF                                                           
073800       END-IF                                                             
073900     END-IF                                                               
074000                                                                          
074100     .                                                                    
074200     EJECT                                                                
074300 BA-KOLLA-REGLER-AKTIVERING     SECTION.                                  
074400     MOVE 'BA-KOLLA-REGLER-AKTIVERING' TO CURRENT-SECTION                 
074500                                                                          
074600     PERFORM BAF-GET-WDK712-INFO                                          
074700                                                                          
074800     MOVE NEJ      TO AKTIVERING-OK-SW                                    
074900     MOVE +1       TO IX1                                                 
075000                                                                          
075100     IF WS-TAB-KVAKT(IX1) > ZERO                                          
075200       PERFORM UNTIL IX1 > MAX-IX1 OR (AKTIVERING-OK-SW = JA)             
075300                  OR (WS-TAB-KVAKT(IX1) = ZERO)                           
075400                                                                          
075500         IF WS-KVOT-TOT >= WS-TAB-KVAKT(IX1)                              
075600           MOVE JA     TO AKTIVERING-OK-SW                                
075700                                                                          
075800           IF WS-TAB-KVVECKOR-PUBV(IX1) > ZERO                            
075900             PERFORM BAA-KOLLA-PUBW                                       
076000           END-IF                                                         
076100                                                                          
076200           IF WS-TAB-PRARTSTD(IX1) > ZERO                                 
076300             PERFORM BAB-KOLLA-STD-PRIS                                   
076400           END-IF                                                         
076500                                                                          
076600           IF WS-TAB-VLARTNTO(IX1) > ZERO                                 
076700             PERFORM BAC-KOLLA-VOLYM                                      
076800           END-IF                                                         
076900                                                                          
077000           IF WS-TAB-KVPB-SEP-REF(IX1) > ZERO                             
077100             PERFORM BAD-KOLLA-GLOBAL-F-C                                 
077200           END-IF                                                         
077300                                                                          
077400           IF WS-TAB-KDPRODSL(IX1 1) > ZERO                               
077500             PERFORM BAE-KOLLA-PG                                         
077600           END-IF                                                         
077700                                                                          
077800         END-IF                                                           
077900         ADD +1   TO IX1                                                  
078000       END-PERFORM                                                        
078100     ELSE                                                                 
078200       MOVE NEJ      TO AKTIVERING-OK-SW                                  
078300     END-IF                                                               
078400                                                                          
078500     .                                                                    
078600     EJECT                                                                
078700 BAA-KOLLA-PUBW   SECTION.                                                
078800     MOVE 'BAA-KOLLA-PUBW'   TO CURRENT-SECTION                           
078900                                                                          
079000     IF WS-KVVECKOR-PUBV < WS-TAB-KVVECKOR-PUBV(IX1)                      
079100       CONTINUE                                                           
079200     ELSE                                                                 
079300       MOVE NEJ        TO AKTIVERING-OK-SW                                
079400     END-IF                                                               
079500                                                                          
079600     .                                                                    
079700     EJECT                                                                
079800 BAB-KOLLA-STD-PRIS  SECTION.                                             
079900     MOVE 'BAB-KOLLA-STD-PRIS'  TO CURRENT-SECTION                        
080000                                                                          
080100     IF DCS-CHINA                                                         
080200     OR DCS-NDC-NA                                                        
080300       MOVE WS-PRIS      TO WS-PRARTSTD-JFR                               
080400     ELSE                                                                 
080500       MOVE CLAG-PRARTSTD TO WS-PRARTSTD-JFR                              
080600     END-IF                                                               
080700                                                                          
080800     IF WS-PRARTSTD-JFR < WS-TAB-PRARTSTD(IX1)                            
080900       CONTINUE                                                           
081000     ELSE                                                                 
081100       MOVE NEJ      TO AKTIVERING-OK-SW                                  
081200     END-IF                                                               
081300                                                                          
081400     .                                                                    
081500     EJECT                                                                
081600 BAC-KOLLA-VOLYM   SECTION.                                               
081700     MOVE 'BAC-KOLLA-VOLYM '  TO CURRENT-SECTION                          
081800                                                                          
081900     IF WS-VLARTNTO-HELTAL < WS-TAB-VLARTNTO(IX1)                         
082000       CONTINUE                                                           
082100     ELSE                                                                 
082200       MOVE NEJ      TO AKTIVERING-OK-SW                                  
082300     END-IF                                                               
082400                                                                          
082500     .                                                                    
082600     EJECT                                                                
082700 BAD-KOLLA-GLOBAL-F-C  SECTION.                                           
082800     MOVE 'BAD-KOLLA-GLOBAL-F-C '  TO CURRENT-SECTION                     
082900                                                                          
083000     MOVE ZERO TO WS-KVPB-REF-TOT                                         
083100                                                                          
083200     MOVE NEJ            TO LOCAL-SOURCED-SW                              
083300     MOVE NEJ            TO PB-SEP-SW                                     
083400     PERFORM IMS-GU-WDK701-2                                              
083500     IF SEGMENT-FINNS                                                     
083600       PERFORM IMS-GNP-WDK711                                             
083700       PERFORM UNTIL SEGMENT-SAKNAS                                       
083800         IF NDC-CN OR NDC-US                                              
083900           IF SLAG-IDDC-REF = SPACE                                       
084000****KOLLA OM ARTIKEL ÄR LOKALT ANSKAFFAD I LANDET                         
084100             MOVE JA      TO LOCAL-SOURCED-SW                             
084200           END-IF                                                         
084300         END-IF                                                           
084400         PERFORM IMS-GNP-WDK711                                           
084500       END-PERFORM                                                        
084600     END-IF                                                               
084700                                                                          
084800     IF LOCAL-SOURCED                                                     
084900       PERFORM IMS-GU-WDK701-2                                            
085000       IF NOT DCS-CHINA                                                   
085100       OR NOT DCS-USA                                                     
085200         IF SEGMENT-FINNS                                                 
085300           PERFORM IMS-GNP-WDK711                                         
085400           PERFORM UNTIL SEGMENT-SAKNAS                                   
085500             IF SLAG-KVPB-REF > ZERO                                      
085600             AND (NDC-CN OR NDC-US)                                       
085700               COMPUTE WS-KVPB-REF-TOT =                                  
085800                       WS-KVPB-REF-TOT + SLAG-KVPB-REF                    
085900               END-COMPUTE                                                
086000               MOVE JA    TO PB-SEP-SW                                    
086100             END-IF                                                       
086200             PERFORM IMS-GNP-WDK711                                       
086300           END-PERFORM                                                    
086400         END-IF                                                           
086500       ELSE                                                               
086600         IF SEGMENT-FINNS                                                 
086700           PERFORM IMS-GNP-WDK711                                         
086800           IF WS-IDDC-REF (1:1) = '7'                                     
086900             PERFORM UNTIL SEGMENT-SAKNAS                                 
087000               IF SLAG-KVPB-REF > ZERO                                    
087100               AND NDC-CN                                                 
087200                 COMPUTE WS-KVPB-REF-TOT =                                
087300                         WS-KVPB-REF-TOT + SLAG-KVPB-REF                  
087400                 END-COMPUTE                                              
087500               END-IF                                                     
087600               PERFORM IMS-GNP-WDK711                                     
087700             END-PERFORM                                                  
087800           END-IF                                                         
087900           IF WS-IDDC-REF (1:1) = '4' OR '5'                              
088000             PERFORM UNTIL SEGMENT-SAKNAS                                 
088100               IF SLAG-KVPB-REF > ZERO                                    
088200               AND NDC-NA                                                 
088300                 COMPUTE WS-KVPB-REF-TOT =                                
088400                         WS-KVPB-REF-TOT + SLAG-KVPB-REF                  
088500                 END-COMPUTE                                              
088600               END-IF                                                     
088700               PERFORM IMS-GNP-WDK711                                     
088800             END-PERFORM                                                  
088900           END-IF                                                         
089000           IF NOT (NDC-CN OR NDC-US)                                      
089100             PERFORM UNTIL SEGMENT-SAKNAS                                 
089200               IF SLAG-KVPB-REF > ZERO                                    
089300               AND NOT (NDC-CN OR NDC-US)                                 
089400                 COMPUTE WS-KVPB-REF-TOT =                                
089500                         WS-KVPB-REF-TOT + SLAG-KVPB-REF                  
089600                 END-COMPUTE                                              
089700               END-IF                                                     
089800               PERFORM IMS-GNP-WDK711                                     
089900             END-PERFORM                                                  
090000           END-IF                                                         
090100         END-IF                                                           
090200       END-IF                                                             
090300     ELSE                                                                 
090400       PERFORM IMS-GU-WDK701-2                                            
090500       IF SEGMENT-FINNS                                                   
090600         PERFORM IMS-GNP-WDK711                                           
090700         PERFORM UNTIL SEGMENT-SAKNAS                                     
090800           IF SLAG-KVPB-REF > ZERO                                        
090900             COMPUTE WS-KVPB-REF-TOT =                                    
091000                     WS-KVPB-REF-TOT + SLAG-KVPB-REF                      
091100             END-COMPUTE                                                  
091200           END-IF                                                         
091300           PERFORM IMS-GNP-WDK711                                         
091400         END-PERFORM                                                      
091500       END-IF                                                             
091600     END-IF                                                               
091700     IF EXKLUD-PB-SEP                                                     
091800       MOVE WS-KVPB-REF-TOT    TO WS-SUM-KVPB-TOT                         
091900     ELSE                                                                 
092000       COMPUTE WS-SUM-KVPB-TOT =                                          
092100               CLAG-KVPB-SEP + WS-KVPB-REF-TOT                            
092200       END-COMPUTE                                                        
092300     END-IF                                                               
092400                                                                          
092500     IF NDC-CN OR NDC-US                                                  
092600        PERFORM BADA-GET-WDK6-PBPLAN                                      
092700     END-IF                                                               
092800                                                                          
092900     MOVE WS-SUM-KVPB-TOT  TO WS-SUM-KVPB-JFR                             
093000                                                                          
093100     IF WS-SUM-KVPB-HELTAL > WS-TAB-KVPB-SEP-REF(IX1)                     
093200       CONTINUE                                                           
093300     ELSE                                                                 
093400       MOVE NEJ      TO AKTIVERING-OK-SW                                  
093500     END-IF                                                               
093600                                                                          
093700     .                                                                    
093800     EJECT                                                                
093900 BADA-GET-WDK6-PBPLAN  SECTION.                                           
094000                                                                          
094100     PERFORM IMS-GU-WDK61129                                              
094200     IF SEGMENT-FINNS                                                     
094300        IF K6-CLAG-DAPBPLAN       >= DAGENS-DATUM-SEKEL                   
094400           COMPUTE WS-SUM-KVPB-TOT =                                      
094500                       WS-SUM-KVPB-TOT + K6-CLAG-KVPB-PLAN                
094600        ELSE                                                              
094700           COMPUTE WS-SUM-KVPB-TOT =                                      
094800                       WS-SUM-KVPB-TOT + K6-CREF-KVPB-PLAN                
094900        END-IF                                                            
095000     END-IF                                                               
095100     .                                                                    
095200     EJECT                                                                
095300 BAE-KOLLA-PG  SECTION.                                                   
095400     MOVE 'BAE-KOLLA-PG '  TO CURRENT-SECTION                             
095500                                                                          
095600     MOVE NEJ  TO PG-SW                                                   
095700                                                                          
095800     MOVE +1 TO PG-IX                                                     
095900     PERFORM UNTIL PG-IX > PG-IX-MAX OR (PG-SW = JA)                      
096000       IF ART-KDPRODSL = WS-TAB-KDPRODSL(IX1 PG-IX)                       
096100         MOVE JA  TO PG-SW                                                
096200       END-IF                                                             
096300                                                                          
096400       ADD +1 TO PG-IX                                                    
096500     END-PERFORM                                                          
096600                                                                          
096700     IF PG-SW = NEJ                                                       
096800       MOVE NEJ      TO AKTIVERING-OK-SW                                  
096900     END-IF                                                               
097000                                                                          
097100     .                                                                    
097200     EJECT                                                                
097300 BAF-GET-WDK712-INFO   SECTION.                                           
097400     MOVE 'BAF-GET-WDK712  '  TO CURRENT-SECTION                          
097500                                                                          
097600     MOVE DCS-IDLANDX2          TO W-IDLAND                               
097700     PERFORM IMS-GU-WDK712                                                
097800     IF SEGMENT-FINNS                                                     
097900       MOVE LART-PRMATRL        TO WS-PRIS                                
098000       IF LART-DAPUBL > 0                                                 
098100                                                                          
098200         MOVE 'YYYYMMDD'        TO DAYS-KDDATFMT1                         
098300         MOVE LART-DAPUBL       TO DAYS-TIDATE1                           
098400         PERFORM S03-HAMTA-VECKOR-WZ20DAYS                                
098500       ELSE                                                               
098600         MOVE 'YYWWD'           TO DAYS-KDDATFMT1                         
098700         MOVE IN-TIFINLV        TO WS-TIFINLV-AAVVD                       
098800         MOVE WS-TIFINLV-AAVVD  TO DAYS-TIDATE1                           
098900         PERFORM S03-HAMTA-VECKOR-WZ20DAYS                                
099000       END-IF                                                             
099100       IF NDC                                                             
099200         IF LART-VLARTNTO >  0                                            
099300           MOVE LART-VLARTNTO   TO WS-VLARTNTO-JFR                        
099400         ELSE                                                             
099500           MOVE CLAG-VLARTNTO   TO WS-VLARTNTO-JFR                        
099600         END-IF                                                           
099700       ELSE                                                               
099800         MOVE CLAG-VLARTNTO     TO WS-VLARTNTO-JFR                        
099900       END-IF                                                             
100000     ELSE                                                                 
100100       MOVE CLAG-VLARTNTO       TO WS-VLARTNTO-JFR                        
100200       MOVE 'YYWWD'             TO DAYS-KDDATFMT1                         
100300       MOVE IN-TIFINLV          TO WS-TIFINLV-AAVVD                       
100400       MOVE WS-TIFINLV-AAVVD    TO DAYS-TIDATE1                           
100500       PERFORM S03-HAMTA-VECKOR-WZ20DAYS                                  
100600     END-IF                                                               
100700                                                                          
100800     .                                                                    
100900     EJECT                                                                
101000 BB-BERAEKNA-KVOT    SECTION.                                             
101100     MOVE 'BB-BERAEKNA-KVOT '   TO CURRENT-SECTION                        
101200                                                                          
101300*--- RÄKNAR FRAM VECKAN FÖR ARTIKELNS PASSIVISERINGSDATUM.                
101400*--- ENDAST ORDERTRÄFFAR SOM INKOMMIT EFTER DETTA DATUM TAS MED.          
101500                                                                          
101600     PERFORM IMS-GU-WDK711                                                
101700     IF SEGMENT-FINNS                                                     
101800       MOVE SLAG-TIREFSTA   TO WS-TIREFSTA-6                              
101900       MOVE WS-TIREFSTA-6   TO DAT-I-TIDATUM                              
102000       MOVE 'AAMMDD'        TO DAT-KDDATFORM                              
102100       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
102200                           DAT-O-TIDATUM DAT-KDSVAR                       
102300       IF DAT-KDSVAR-OK                                                   
102400         MOVE DAT-TIAA-VECKA  TO WS-TIREFSTA-JFR-AA                       
102500         MOVE DAT-TIVV        TO WS-TIREFSTA-JFR-VV                       
102600       ELSE                                                               
102700         MOVE ZERO            TO WS-TIREFSTA-JFR                          
102800       END-IF                                                             
102900     ELSE                                                                 
103000       MOVE ZERO              TO WS-TIREFSTA-JFR                          
103100     END-IF                                                               
103200                                                                          
103300                                                                          
103400*--- SUMMERA ALLA ORDERTRÄFFAR DE SENASTE 12 PERIODERNA                   
103500     MOVE +1                        TO INDX                               
103600     MOVE ZERO                      TO WS-KVOT-TOT                        
103700     PERFORM UNTIL INDX             > 12                                  
103800       MOVE WS-FORSTA-TIVV(INDX) TO VV-INDX                               
103900       PERFORM UNTIL VV-INDX        > WS-SISTA-TIVV(INDX)                 
104000                                                                          
104100         MOVE WS-TIAARP(INDX) (1:2) TO WS-TIKVOT-TIAA                     
104200         MOVE VV-INDX                TO WS-TIKVOT-TIVV                    
104300         MOVE WS-TIKVOT-JFR          TO TMP1-YYWW                         
104400         MOVE WS-TIREFSTA-JFR        TO TMP2-YYWW                         
104500         PERFORM WY2000P3                                                 
104600         IF TMP1-YYWW >= TMP2-YYWW                                        
104700*** I IN-KVOT-RULL ÄR BÅDE KUND + REFILLORDERTRÄFFAR INKLUDERADE          
104800*** FRÅN W2714000                                                         
104900           ADD IN-KVOT-RULL (VV-INDX) TO WS-KVOT-TOT                      
105000         END-IF                                                           
105100                                                                          
105200         ADD +1                     TO VV-INDX                            
105300       END-PERFORM                                                        
105400       ADD +1                       TO INDX                               
105500     END-PERFORM                                                          
105600                                                                          
105700                                                                          
105800*--- HÄMTAR FÖRSTA VECKAN FÖR INNEVARANDE PERIOD.                         
105900     MOVE 'AARP  '                 TO DAT-KDDATFORM                       
106000     MOVE DAGENS-TIAARP            TO DAT-I-TIDATUM                       
106100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
106200                         DAT-O-TIDATUM DAT-KDSVAR                         
106300     IF DAT-KDSVAR-OK                                                     
106400       MOVE DAT-TIVV               TO WS-INNEVARANDE-TIVV                 
106500     ELSE                                                                 
106600       MOVE 'SVAR 5 FRÅN WDATKONV EJ OK' TO FELTEXT-STR                   
106700       DISPLAY FELTEXT                                                    
106800       PERFORM S99-ABEND                                                  
106900     END-IF                                                               
107000                                                                          
107100                                                                          
107200*--- ADDERA ORDERTRÄFFAR INNEVARANDE PERIOD                               
107300     MOVE +1                      TO INDX                                 
107400     PERFORM UNTIL INDX           > 5                                     
107500                                                                          
107600       MOVE DAGENS-TIAA             TO WS-TIKVOT-TIAA                     
107700       MOVE WS-INNEVARANDE-TIVV     TO WS-TIKVOT-TIVV                     
107800       MOVE WS-TIKVOT-JFR           TO TMP1-YYWW                          
107900       MOVE WS-TIREFSTA-JFR         TO TMP2-YYWW                          
108000       PERFORM WY2000P3                                                   
108100       IF TMP1-YYWW >= TMP2-YYWW                                          
108200*** I IN-KVOT-INNEV ÄR BÅDE KUND + REFILLORDERTRÄFFAR INKLUDERADE         
108300*** FRÅN W2714000                                                         
108400         ADD IN-KVOT-INNEV (INDX) TO WS-KVOT-TOT                          
108500       END-IF                                                             
108600                                                                          
108700       ADD +1    TO INDX                                                  
108800       ADD 1     TO WS-INNEVARANDE-TIVV                                   
108900     END-PERFORM                                                          
109000                                                                          
109100     .                                                                    
109200     EJECT                                                                
109300 BC-AKTIVERING-WDK7  SECTION.                                             
109400     MOVE 'BC-AKTIVERING-WDK7' TO CURRENT-SECTION                         
109500                                                                          
109600     MOVE AKTIV              TO SLAG-KDREFSTA                             
109700     MOVE DAGENS-DATUM       TO SLAG-TIREFSTA                             
109800     IF NDC                                                               
109900       MOVE 0.2              TO SLAG-KVPB-REF                             
110000     ELSE                                                                 
110100       MOVE 0.3              TO SLAG-KVPB-REF                             
110200     END-IF                                                               
110300*                                                                         
110400*        OM SLAG-FLREFBEO = S, SKA DEN FÖRBLI SÅ                          
110500*                                                                         
110600     IF SLAG-FLREFBEO = JA                                                
110700       MOVE NEJ              TO SLAG-FLREFBEO                             
110800     END-IF                                                               
110900                                                                          
111000     IF  CLAG-KDERS = 09                                                  
111100     AND DCS-KDDC  = 'S'                                                  
111200       IF SLAG-FLREFBEO = JA OR NEJ                                       
111300          MOVE STOPP         TO SLAG-FLREFBEO                             
111400       END-IF                                                             
111500     END-IF                                                               
111600                                                                          
111710     MOVE WS-TODAY-DATE-30    TO SLAG-TIREFMPB                            
111800                                                                          
111900     PERFORM IMS-REPL-WDK711                                              
112000     DISPLAY 'REPL :' IN-IDARTNR                                          
112100     DISPLAY 'REPL :' IN-IDDC                                             
112200     ADD +1 TO ANTAL-REPL-K711                                            
112300     ADD +1                  TO CHKP-ANT                                  
112400     .                                                                    
112500     EJECT                                                                
112600 BD-SKAPA-WDK7  SECTION.                                                  
112700     MOVE 'BD-SKAPA-WDK7'  TO CURRENT-SECTION                             
112800                                                                          
112900     MOVE ALL '+'      TO WDK7-W005WDK7                                   
113000     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
113100     MOVE IN-IDARTNR   TO WDK7-IDARTNR-KFB                                
113200     MOVE IN-IDDC      TO WDK7-IDDC-KFB                                   
113300                          WDK7-IDDC                                       
113400                                                                          
113500     MOVE AKTIV        TO WDK7-KDREFSTA                                   
113600     IF NDC                                                               
113700       MOVE 0.2              TO WDK7-KVPB-REF                             
113800     ELSE                                                                 
113900       MOVE 0.3              TO WDK7-KVPB-REF                             
114000     END-IF                                                               
114200     MOVE WS-TODAY-DATE-30    TO WDK7-TIREFMPB                            
114300                                                                          
114400     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                  
114500                                       WDK7-1-PCB                         
114600     DISPLAY 'ISRT :' IN-IDARTNR                                          
114700     DISPLAY 'ISRT :' IN-IDDC                                             
114800     ADD +1           TO ANTAL-ISRT-K711                                  
114900     ADD +2           TO CHKP-ANT                                         
115000     .                                                                    
115100     EJECT                                                                
115200                                                                          
115300 BE-KTRL-AKTIVERING SECTION.                                              
115400     MOVE 'BE-KTRL-AKTIVERING'  TO CURRENT-SECTION                        
115500     IF SLAG-FLPB-FLYTT = JA                                              
115510       MOVE NEJ           TO WS-FLKTRL-AKT                                
115511**IF FORECAST IS MOVED NO ACTIVATION OF THE PARTNO                        
115520     ELSE                                                                 
115600       IF CLAG-KDERS = 01 OR 02 OR 03 OR 05 OR 06 OR 07                   
115700         MOVE CLAG-TISTOREF       TO WS-TISTOREF                          
115800         PERFORM BEB-ERS-EFT-INLEV-EL-VISS-TIDP                           
115900       ELSE                                                               
116000         IF CLAG-KDERS = 09                                               
116100           PERFORM BEC-CHECK-9-CODED-PARTS                                
116200         ELSE                                                             
116300           IF CLAG-KDERS = 04 OR 08                                       
116400             MOVE NEJ           TO WS-FLKTRL-AKT                          
116500           END-IF                                                         
116600         END-IF                                                           
116700       END-IF                                                             
116710     END-IF                                                               
116800                                                                          
116900     .                                                                    
117000     EJECT                                                                
117100                                                                          
117200 BEB-ERS-EFT-INLEV-EL-VISS-TIDP SECTION.                                  
117300     MOVE 'BEB-ERS-EFT-INLEV-EL-VISS-TIDP' TO CURRENT-SECTION             
117400                                                                          
117500*8 VECKOR FRAMÅT                                                          
117600                                                                          
117700     MOVE DAGENS-VECKA-D TO WS-ATTA-VECKOR-D                              
117800                                                                          
117900     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
118000     MOVE +8                   TO VADD-ANTAL                              
118100                                                                          
118200     CALL W009VADD         USING VADD-DATUM-AAVV                          
118300                                 VADD-ANTAL                               
118400     MOVE VADD-DATUM-AAVV  TO WS-ATTA-VECKOR-AAVV                         
118500                                                                          
118600     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
118700     MOVE WS-ATTA-VECKOR     TO DAT-I-TIDATUM                             
118800                                                                          
118900     IF WS-TISTOREF > WS-ATTA-VECKOR                                      
119000       CONTINUE                                                           
119100     ELSE                                                                 
119200       MOVE NEJ                 TO WS-FLKTRL-AKT                          
119300     END-IF                                                               
119400     .                                                                    
119500     EJECT                                                                
119600 BEC-CHECK-9-CODED-PARTS SECTION.                                         
119700     MOVE 'BEC-CHECK-9-CODED-PARTS'  TO CURRENT-SECTION                   
119800                                                                          
119900     MOVE ZERO                       TO WS-CDC-ASSETS                     
120000                                        WS-CDC-PROGNOSIS                  
120100                                        WS-NO-OF-MONTHS                   
120200                                        WS-KVAVIS                         
120300                                        WS-KVOKS                          
120400                                        WS-KVAVROP                        
120500                                        WS-KVBEART                        
120600                                        WS-KVPB-REF                       
120700                                        WS-KVPBREOI                       
120800                                                                          
120900     PERFORM IMS-GU-WDL201                                                
121000     IF SEGMENT-FINNS                                                     
121100       PERFORM IMS-GNP-WDL211                                             
121200       PERFORM UNTIL SEGMENT-SAKNAS                                       
121300         MOVE WDL2-INL-DAINLEV       TO W-DAINLEV                         
121400         PERFORM IMS-GNP-WDL221                                           
121500         IF SEGMENT-FINNS                                                 
121600           IF WDL2-MOT-KDRT = 07                                          
121700             COMPUTE WS-KVAVIS = WS-KVAVIS + WDL2-MOT-KVAVIS              
121800           END-IF                                                         
121900         END-IF                                                           
122000         PERFORM IMS-GNP-WDL211                                           
122100       END-PERFORM                                                        
122200     END-IF                                                               
122300                                                                          
122400     PERFORM IMS-GU-WDK901                                                
122500     IF SEGMENT-FINNS                                                     
122600       COMPUTE WS-KVOKS = WDK9-ART-KVOKS-BULK +                           
122700                          WDK9-ART-KVOKS-DAG  +                           
122800                          WDK9-ART-KVOKS-VOR                              
122900     END-IF                                                               
123000                                                                          
123100     MOVE IN-IDARTNR             TO W-IDARTNR-WDD9                        
123200     MOVE WC-CDC-SE              TO W-IDDC-WDD9                           
123300     MOVE ART-IDLEVNR            TO W-IDLEVNR                             
123400     PERFORM IMS-GU-WDD902                                                
123500     IF SEGMENT-FINNS                                                     
123600       PERFORM IMS-GNP-WDD905                                             
123700       PERFORM UNTIL SEGMENT-SAKNAS                                       
123800         COMPUTE WS-KVAVROP = WS-KVAVROP + WDD9-KVAVROP                   
123900         PERFORM IMS-GNP-WDD905                                           
124000       END-PERFORM                                                        
124100     END-IF                                                               
124200                                                                          
124300     PERFORM IMS-GU-WDK701-2                                              
124400     IF SEGMENT-FINNS                                                     
124500       PERFORM IMS-GNP-WDK711                                             
124600       PERFORM UNTIL SEGMENT-SAKNAS                                       
124700         IF SLAG-IDLEVNR = '1441'                                         
124800           COMPUTE WS-KVBEART  = WS-KVBEART  + SLAG-KVBEART               
124900           COMPUTE WS-KVPB-REF = WS-KVPB-REF + SLAG-KVPB-REF              
125000           COMPUTE WS-KVPBREOI = WS-KVPBREOI + SLAG-KVPBREOI              
125100         END-IF                                                           
125200         PERFORM IMS-GNP-WDK711                                           
125300       END-PERFORM                                                        
125400     END-IF                                                               
125500                                                                          
125600     COMPUTE WS-CDC-ASSETS = CLAG-KVLS      +                             
125700                             CLAG-KVAKS-PAV +                             
125800                             CLAG-KVAKS-T   +                             
125900                             CLAG-KVAKS-CDC +                             
126000                             WS-KVAVROP     -                             
126100                             WS-KVBEART     -                             
126200                             WS-KVAVIS      -                             
126300                             WS-KVOKS       -                             
126400                             CLAG-KVROS     -                             
126500                             CLAG-KVRESS    -                             
126600                             CLAG-KVSPARR-KVAL                            
126700                                                                          
126800     COMPUTE WS-CDC-PROGNOSIS = CLAG-KVPB-SEP  +                          
126900                                CLAG-KVPB-SATS +                          
127000                                CLAG-KVPB-TPO  +                          
127100                                WS-KVPB-REF    +                          
127200                                WS-KVPBREOI                               
127300                                                                          
127400     IF WS-CDC-PROGNOSIS > 0                                              
127500       COMPUTE WS-NO-OF-MONTHS = WS-CDC-ASSETS / WS-CDC-PROGNOSIS         
127600     END-IF                                                               
127700                                                                          
127800     IF WS-NO-OF-MONTHS > 12                                              
127900       CONTINUE                                                           
128000     ELSE                                                                 
128100       MOVE NEJ                      TO WS-FLKTRL-AKT                     
128200     END-IF                                                               
128300                                                                          
128400     .                                                                    
128500     EJECT                                                                
128600 C-FYLL-WDB614-TABELL  SECTION.                                           
128700     MOVE 'C-FYLL-WDB614-TABELL '    TO CURRENT-SECTION                   
128800                                                                          
128900*** LÄGG ÖVER WDB614 SEGMENTEN I INTERNTABELL FÖR ATT INTE TYNGA          
129000*** NER LÄSNINGARNA.                                                      
129100                                                                          
129200     PERFORM CA-NOLLA-WDB614-TABELL                                       
129300                                                                          
129400     MOVE W-IDDC-B6     TO  WS-TAB-IDDC                                   
129500                                                                          
129600     PERFORM IMS-GNP-WDB614                                               
129700     MOVE +1   TO IX1                                                     
129800     IF SEGMENT-FINNS                                                     
129900       PERFORM UNTIL SEGMENT-SAKNAS                                       
130000         MOVE WDB614-AKT-KVAKT      TO WS-TAB-KVAKT(IX1)                  
130100         MOVE WDB614-AKT-KVVECKOR-PUBV TO                                 
130200                                       WS-TAB-KVVECKOR-PUBV(IX1)          
130300         MOVE WDB614-AKT-PRARTSTD   TO WS-TAB-PRARTSTD(IX1)               
130400         MOVE WDB614-AKT-VLARTNTO   TO WS-TAB-VLARTNTO(IX1)               
130500         MOVE WDB614-AKT-KVPB-SEP-REF TO                                  
130600                                       WS-TAB-KVPB-SEP-REF(IX1)           
130700         MOVE +1   TO PG-IX                                               
130800         PERFORM UNTIL PG-IX > PG-IX-MAX                                  
130900           MOVE WDB614-AKT-KDPRODSL(PG-IX) TO                             
131000                                       WS-TAB-KDPRODSL(IX1 PG-IX)         
131100           ADD +1  TO PG-IX                                               
131200         END-PERFORM                                                      
131300                                                                          
131400         PERFORM IMS-GNP-WDB614                                           
131500         ADD +1   TO IX1                                                  
131600       END-PERFORM                                                        
131700     END-IF                                                               
131800                                                                          
131900     .                                                                    
132000     EJECT                                                                
132100 CA-NOLLA-WDB614-TABELL  SECTION.                                         
132200     MOVE 'CA-NOLLA-WDB614-TABELL '    TO CURRENT-SECTION                 
132300                                                                          
132400     MOVE +1   TO IX1                                                     
132500     PERFORM UNTIL IX1 > MAX-IX1                                          
132600       MOVE ZERO            TO WS-TAB-KVAKT(IX1)                          
132700       MOVE ZERO            TO WS-TAB-KVVECKOR-PUBV(IX1)                  
132800       MOVE ZERO            TO WS-TAB-PRARTSTD(IX1)                       
132900       MOVE ZERO            TO WS-TAB-VLARTNTO(IX1)                       
133000       MOVE ZERO            TO WS-TAB-KVPB-SEP-REF(IX1)                   
133100       MOVE +1   TO PG-IX                                                 
133200       PERFORM UNTIL PG-IX > PG-IX-MAX                                    
133300         MOVE ZERO          TO WS-TAB-KDPRODSL(IX1 PG-IX)                 
133400         ADD +1  TO PG-IX                                                 
133500       END-PERFORM                                                        
133600                                                                          
133700       ADD +1   TO IX1                                                    
133800     END-PERFORM                                                          
133900                                                                          
134000     .                                                                    
134100     EJECT                                                                
134200                                                                          
134300 Z-FINIT SECTION.                                                         
134400                                                                          
134500                                                                          
134600     CLOSE W27140                                                         
134700     SKIP2                                                                
134800     MOVE 'S' TO POSTSUM-OPKOD                                            
134900     CALL POSTSUM USING POSTSUM-PARM                                      
135000                                                                          
135100     .                                                                    
135200     EJECT                                                                
135300 S01-LAES-W27140  SECTION.                                                
135400     MOVE 'S01-LAES-W27140'  TO CURRENT-SECTION                           
135500     SKIP2                                                                
135600     READ W27140 INTO IN-AREA                                             
135700     AT END                                                               
135800        SET END-OF-W27140   TO TRUE                                       
135900                                                                          
136000     NOT AT END                                                           
136100        MOVE 'W27140'       TO POSTSUM-FDNAMN                             
136200        MOVE 'W27141D1'     TO POSTSUM-DDNAMN2                            
136300        CALL POSTSUM USING POSTSUM-PARM                                   
136400                                                                          
136500        ADD 1 TO W-W27140-KVPOST-IN                                       
136600     END-READ                                                             
136700     .                                                                    
136800     EJECT                                                                
139600 S03-HAMTA-VECKOR-WZ20DAYS SECTION.                                       
139700     MOVE 'S03-HAMTA-VECKOR-WZ20DAYS '  TO CURRENT-SECTION                
139800                                                                          
139900     MOVE ZERO           TO DAYS-KVDAYS                                   
140000     MOVE 'YYMMDD'       TO DAYS-KDDATFMT2                                
140100     MOVE DAGENS-DATUM   TO DAYS-TIDATE2                                  
140200                                                                          
140300     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
140400*                                                                         
140500     IF DAYS-KDRC = +0                                                    
140600       COMPUTE WS-KVVECKOR-PUBV = DAYS-KVDAYS / 7                         
140700     ELSE                                                                 
140800       MOVE 'ERROR FROM WZ20DAYS MODULE' TO FELTEXT-STR                   
140900       DISPLAY FELTEXT                                                    
141000       DISPLAY DAYS-KDRC                                                  
141100       PERFORM S99-ABEND                                                  
141200     END-IF                                                               
141300     .                                                                    
141400     EJECT                                                                
141500 S99-ABEND SECTION.                                                       
141600                                                                          
141700     MOVE 'S' TO POSTSUM-OPKOD                                            
141800     CALL POSTSUM USING POSTSUM-PARM                                      
141900     CALL ABEND USING WKOD-ABEND-UTAN-DUMP                                
142000     .                                                                    
142100     EJECT                                                                
142200 X-TAG-CHECKPOINT   SECTION.                                              
142300                                                                          
142400* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
142500* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
142600     PERFORM IMS-CHECKPOINT                                               
142700     MOVE ZERO TO CHKP-ANT                                                
142800* --- LÄS OM DATABAS OM DET BEHÖVS                                        
142900     .                                                                    
143000     EJECT                                                                
143100* --- IMS SEKTIONER ---                                                   
143200                                                                          
143300 IMS-GU-WDK701-1 SECTION.                                                 
143400                                                                          
143500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
143600          DELIMITED BY SIZE INTO SSA1                                     
143700     MOVE '  GE' TO GODK-STATUSKODER                                      
143800     CALL CBLTDLI USING GU WDK7-1-PCB DLI-IO-AREA-WDK701 SSA1             
143900     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
144000     PERFORM IMS-STATUSKONTROLL                                           
144100     .                                                                    
144200     SKIP3                                                                
144300 IMS-GU-WDK711 SECTION.                                                   
144400                                                                          
144500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
144600          DELIMITED BY SIZE INTO SSA1                                     
144700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
144800          DELIMITED BY SIZE INTO SSA2                                     
144900     MOVE '  GE' TO GODK-STATUSKODER                                      
145000     CALL CBLTDLI USING GU WDK7-1-PCB DLI-IO-AREA-WDK711 SSA1 SSA2        
145100     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
145200     PERFORM IMS-STATUSKONTROLL                                           
145300     .                                                                    
145400     SKIP3                                                                
145500 IMS-GHU-WDK711 SECTION.                                                  
145600                                                                          
145700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
145800          DELIMITED BY SIZE INTO SSA1                                     
145900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
146000          DELIMITED BY SIZE INTO SSA2                                     
146100     MOVE '  GE' TO GODK-STATUSKODER                                      
146200     CALL CBLTDLI USING GHU WDK7-1-PCB DLI-IO-AREA-WDK711                 
146300                            SSA1 SSA2                                     
146400     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
146500     PERFORM IMS-STATUSKONTROLL                                           
146600     .                                                                    
146700     SKIP3                                                                
146800 IMS-GU-WDK712 SECTION.                                                   
146900                                                                          
147000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
147100          DELIMITED BY SIZE INTO SSA1                                     
147200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
147300          DELIMITED BY SIZE INTO SSA2                                     
147400     MOVE '  GE' TO GODK-STATUSKODER                                      
147500     CALL CBLTDLI USING GU WDK7-1-PCB DLI-IO-AREA-WDK712 SSA1 SSA2        
147600     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
147700     PERFORM IMS-STATUSKONTROLL                                           
147800     .                                                                    
147900     SKIP3                                                                
148000 IMS-REPL-WDK711 SECTION.                                                 
148100                                                                          
148200     MOVE '  ' TO GODK-STATUSKODER                                        
148300     CALL CBLTDLI USING REPL WDK7-1-PCB DLI-IO-AREA-WDK711                
148400     MOVE WDK7-1-STATUS-CODE TO STATUS-WS                                 
148500     PERFORM IMS-STATUSKONTROLL                                           
148600     .                                                                    
148700     EJECT                                                                
148800     SKIP2                                                                
148900 IMS-GU-WDK601 SECTION.                                                   
149000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
149100          DELIMITED BY SIZE INTO SSA1                                     
149200     MOVE '  ' TO GODK-STATUSKODER                                        
149300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
149400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
149500     PERFORM IMS-STATUSKONTROLL                                           
149600     .                                                                    
149700     SKIP3                                                                
149800 IMS-GNP-WDK611 SECTION.                                                  
149900                                                                          
150000     MOVE 'WDK611   ' TO SSA1                                             
150100     MOVE '  ' TO GODK-STATUSKODER                                        
150200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
150300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
150400     PERFORM IMS-STATUSKONTROLL                                           
150500     .                                                                    
150600     SKIP3                                                                
150700 IMS-GU-WDK61129   SECTION.                                               
150800                                                                          
150900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
151000          DELIMITED BY SIZE INTO SSA1                                     
151100     STRING 'WDK611  *D(KDSEGKEY =' W-KDSEGKEY-X ')'                      
151200          DELIMITED BY SIZE INTO SSA2                                     
151300     MOVE 'WDK629  '       TO SSA3                                        
151400     MOVE '  GE' TO GODK-STATUSKODER                                      
151500     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-AREA-WDK61129                 
151600                            SSA1 SSA2 SSA3                                
151700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
151800     PERFORM IMS-STATUSKONTROLL                                           
151900     .                                                                    
152000     SKIP3                                                                
152100                                                                          
152200 IMS-GU-WDK901 SECTION.                                                   
152300                                                                          
152400     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
152500          DELIMITED BY SIZE INTO SSA1                                     
152600     MOVE '  GE' TO GODK-STATUSKODER                                      
152700     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
152800     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
152900     PERFORM IMS-STATUSKONTROLL                                           
153000     .                                                                    
153100     EJECT                                                                
153200                                                                          
153300 IMS-GU-D704 SECTION.                                                     
153400                                                                          
153500     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
153600          DELIMITED BY SIZE INTO SSA1                                     
153700     MOVE 'WDD704   ' TO SSA2                                             
153800     MOVE '  ' TO GODK-STATUSKODER                                        
153900     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD704 SSA1 SSA2               
154000     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
154100     PERFORM IMS-STATUSKONTROLL                                           
154200     .                                                                    
154300     EJECT                                                                
154400                                                                          
154500 IMS-GU-WDB601    SECTION.                                                
154600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
154700          DELIMITED BY SIZE INTO SSA1                                     
154800     MOVE '  ' TO GODK-STATUSKODER                                        
154900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
155000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
155100     PERFORM IMS-STATUSKONTROLL                                           
155200     .                                                                    
155300     EJECT                                                                
155400                                                                          
155500 IMS-GNP-WDB614 SECTION.                                                  
155600     MOVE 'WDB614   ' TO SSA1                                             
155700     MOVE '  GE' TO GODK-STATUSKODER                                      
155800     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB614  SSA1                  
155900     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
156000     PERFORM IMS-STATUSKONTROLL                                           
156100     .                                                                    
156200     EJECT                                                                
156300                                                                          
156400 IMS-GU-WDK701-2 SECTION.                                                 
156500     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
156600          DELIMITED BY SIZE INTO SSA1                                     
156700     MOVE '  GE' TO GODK-STATUSKODER                                      
156800     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-AREA-WDK701  SSA1            
156900     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
157000     PERFORM IMS-STATUSKONTROLL                                           
157100     .                                                                    
157200     SKIP3                                                                
157300 IMS-GNP-WDK711 SECTION.                                                  
157400     MOVE 'WDK711   ' TO SSA1                                             
157500     MOVE '  GE' TO GODK-STATUSKODER                                      
157600     CALL CBLTDLI USING GNP WDK7-2-PCB DLI-IO-AREA-WDK711  SSA1           
157700     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
157800     PERFORM IMS-STATUSKONTROLL                                           
157900     .                                                                    
158000     SKIP3                                                                
158100 IMS-GHU-DC71-WDL711 SECTION.                                             
158200                                                                          
158300     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
158400          DELIMITED BY SIZE INTO SSA1                                     
158500     STRING 'WLOIGA11(IDDC     =' W-IDDC-X ')'                            
158600          DELIMITED BY SIZE INTO SSA2                                     
158700     MOVE '  GE' TO GODK-STATUSKODER                                      
158800     CALL CBLTDLI USING GHU OIGA-PCB DLI-IO-OIGA11 SSA1 SSA2              
158900     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
159000     PERFORM IMS-STATUSKONTROLL                                           
159100     .                                                                    
159200     SKIP3                                                                
159300 IMS-GU-WDL201 SECTION.                                                   
159400     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
159500           DELIMITED BY SIZE INTO SSA1                                    
159600     MOVE '  GE' TO GODK-STATUSKODER                                      
159700     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL2 SSA1                      
159800     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
159900     PERFORM IMS-STATUSKONTROLL                                           
160000     .                                                                    
160100     SKIP3                                                                
160200 IMS-GNP-WDL211 SECTION.                                                  
160300     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
160400             DELIMITED BY SIZE INTO SSA1                                  
160500     STRING 'WDL211  (DAINLEV =>' W-DAINLEV-X ')'                         
160600             DELIMITED BY SIZE INTO SSA2                                  
160700     MOVE '  GE' TO GODK-STATUSKODER                                      
160800     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL2 SSA1 SSA2                
160900     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
161000     PERFORM IMS-STATUSKONTROLL                                           
161100     .                                                                    
161200     EJECT                                                                
161300 IMS-GNP-WDL221 SECTION.                                                  
161400     STRING 'WDL211  (DAINLEV  =' W-DAINLEV-X ')'                         
161500             DELIMITED BY SIZE INTO SSA1                                  
161600     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
161700             DELIMITED BY SIZE INTO SSA2                                  
161800     MOVE '  GE' TO GODK-STATUSKODER                                      
161900     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL2 SSA1 SSA2                
162000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
162100     PERFORM IMS-STATUSKONTROLL                                           
162200     .                                                                    
162300     EJECT                                                                
162400 IMS-GU-WDD902 SECTION.                                                   
162500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
162600           DELIMITED BY SIZE INTO SSA1                                    
162700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
162800           DELIMITED BY SIZE INTO SSA2                                    
162900     MOVE '  GE' TO GODK-STATUSKODER                                      
163000     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD9 SSA1                      
163100     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
163200     PERFORM IMS-STATUSKONTROLL                                           
163300     .                                                                    
163400     SKIP3                                                                
163500 IMS-GNP-WDD905 SECTION.                                                  
163600     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
163700           DELIMITED BY SIZE INTO SSA1                                    
163800     MOVE '  GE' TO GODK-STATUSKODER                                      
163900     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD9 SSA1                     
164000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
164100     PERFORM IMS-STATUSKONTROLL                                           
164200     .                                                                    
164300     EJECT                                                                
164400 IMS-RESTART SECTION.                                                     
164500     SKIP2                                                                
164600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
164700     MOVE '  ' TO GODK-STATUSKODER                                        
164800     CALL CBLTDLI USING XRST MSG-PCB                                      
164900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
165000                        CHKP-AREA-LENGTH CHKP-AREA                        
165100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
165200     PERFORM IMS-STATUSKONTROLL                                           
165300     .                                                                    
165400     EJECT                                                                
165500 IMS-CHECKPOINT SECTION.                                                  
165600     SKIP2                                                                
165700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
165800     MOVE '  XD' TO GODK-STATUSKODER                                      
165900     CALL CBLTDLI USING CHKP MSG-PCB                                      
166000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
166100                        CHKP-AREA-LENGTH CHKP-AREA                        
166200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
166300     PERFORM IMS-STATUSKONTROLL                                           
166400                                                                          
166500     IF IMS-EJ-OK                                                         
166600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
166700       DISPLAY FELTEXT                                                    
166800       CALL FELLOG                                                        
166900     END-IF                                                               
167000     .                                                                    
167100     EJECT                                                                
167200 IMS-STATUSKONTROLL SECTION.                                              
167300     SKIP2                                                                
167400     SET STATUS-IX TO 1                                                   
167500     SEARCH GODK-STATUS                                                   
167600       AT END                                                             
167700         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
167800         DISPLAY FELTEXT                                                  
167900         CALL FELLOG                                                      
168000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
168100         CONTINUE                                                         
168200     END-SEARCH                                                           
168300     .                                                                    
168400     EJECT                                                                
168500*    -COPY WY2000P2                                                       
168600     EJECT                                                                
168700*    -COPY WY2000P3                                                       
