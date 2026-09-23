000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1011700.                                                
000400 AUTHOR.         KENT HELLQVIST.                                          
000500 DATE-WRITTEN.   MARS 1989.                                               
000600     REMARKS.                                                             
000700*    ***********************************************************          
000800*    *                                                         *          
000900*    *       1117      N Y P O N  - S Y S T E M E T            *          
001000*    *                                                         *          
001100*    *          *ÄNDRINGSBILD FÖR BEREDNINGSDATA.*             *          
001200*    *                                                         *          
001300*    ***********************************************************          
001400*    FUNKTION.                                                            
001500*         INDATA.                                                         
001600*             TRANSAKTION: W1T117                                         
001700*                          W1T117U                                        
001710*                          W1T117X                                        
001800*                                                                         
001900*                     MID: W1I11701                                       
002000*         UTDATA.                                                         
002100*                     MOD: W1O11701                                       
002110*             TRANSACTION: W0T693X                                        
002200*         SUBPROGRAM.                                                     
002300*               DYNAMISKA: FELLOG                                         
002400*                          CBLTDLI                                        
002500*                          WDATKONV                                       
002600*    ***********************************************************          
002700*    ÄNDRINGAR.                                                           
002800*      FÄLTEN ART-FLRITB OCH ART-TIRITB  HAR UTGÅTT                       
002900*       ( SE KOMMENTAR I W1140200)                                        
003000*      DESSA FÄLT INITIERAS MED NOLL OCH SPACE  TILLSV.                   
003100*                                                                         
003200*    ÄNDRING:                                                             
003300*        2005-FEB  ETRACKER=1476814.  VISA KAMPANJ-INFO                   
003400*                                     TILLAGT DB2-LÄSNING  /C.E.          
003500*                                                                         
003600*    ÄNDRING:                                                             
003700*        2006-JAN  ETRACKER=1986420. SKAPA LEV-PLANELARM VID              
003800*                  FÖRÄNDRING AV TIFINLV OCH NÄR ARTIKELN SAM-            
003900*                  TIDIGT ÄR ERSÄTTANDE I ERS MED EK 01-09.               
004000*                  BÅDE ERSATT OCH ERSÄTTANDE ARTIKEL SKALL LARMAS        
004100*                  MED ORSAK=09.     TILLÄGG AV WLXXBJ11-ISRT FÖR         
004200*                                    LARM PÅ 2204 HTR          /CE        
004300*                                                                         
004400*    ÄNDRING:                                                             
004500*        2006-JAN. TILLÄGG AV EKONOMISK HÄNDELSE KDEKOHT='M21'            
004600*                  FÖR US OCH CA. /M.A.                                   
004700*                                                                         
004800*    ÄNDRING:                                                             
004900*        2006-FEB. RENSNING AV KOD FÖR HÄNDELSER PÅ XXAU OCH XXAV         
005000*                  FÖR UTGÅENDE SYSTEM "TIKO". /BL                        
005100*                                                                         
005200*    ÄNDRING:                                                             
005300*        2012-JAN. E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1           
005400*                                                                         
005500*    ÄNDRING:                                                             
005600*        2012-MAJ. LOCAL SOURCING NYCKEL WDD901 UTÖKAD MED IDDC           
005700*                                                                         
005800*    ÄNDRING:                                                             
005900*        2014-SOMMAR  TISOP TILLAGD PÅ BILDEN                             
006000*                                                                         
006100*    ÄNDRING:                                                             
006200*        2015-FEB. E-TRACKER 10249871 PRODUCTGROUP RESTRICTIONS           
006300*                                                                         
006400*                                                                         
006410*    ÄNDRING:                                                             
006420*        2025-JUL. X TRANS COMES FROM DISPACTHER FROM PGM W11813          
006421*                  FOR TCPLM EXCHANGE PART UPDATE                         
006430*                                                                         
006500     EJECT                                                                
006600 ENVIRONMENT DIVISION.                                                    
006700     SKIP3                                                                
006800 DATA DIVISION.                                                           
006900 WORKING-STORAGE SECTION.                                                 
007000*    -COPY WY2000W1                                                       
007100     SKIP3                                                                
007200*    -COPY WY2000W3                                                       
007300     SKIP3                                                                
007400*                                                                         
007500******************************************************************        
007600*          W O R K I N G  S T O R A G E  S E C T I O N           *        
007700******************************************************************        
007800*                                                                         
007900 77  PROGRAM-NAMN                PIC X(08)  VALUE 'W1011700'.             
008000 77  JA                          PIC X(01)  VALUE 'J'.                    
008100 77  NEJ                         PIC X(01)  VALUE 'N'.                    
008200 77  LEV05-IX                    PIC S9(3)  VALUE ZERO COMP-3.            
008300 77  WS-IDARTNR                  PIC X(09).                               
008400 77  WS-IDARTNR-OPACKAT          PIC 9(09)  VALUE ZERO.                   
008500 77  WS-IDLEVNR                  PIC X(05).                               
008600 77  WS-TVA-AAR                  PIC S9(05)  VALUE +10000.                
008700 77  WS-GAMMAL-ART               PIC S9(07)  VALUE  ZERO.                 
008800 77  WS-KDRESBED                 PIC X(1)    VALUE  SPACE.                
008900 77  WS-KDPRODSL-LEV             PIC S9(3)   VALUE  ZERO COMP-3.          
009000 77  WS-KDPRODSL                 PIC S9(3)   VALUE  ZERO COMP-3.          
009100 77  WS-KDPSLLOC-OLD             PIC 9(2)    VALUE  ZERO.                 
009200 77  W-IDSEKVNR                  PIC S9(3)   VALUE  ZERO COMP-3.          
009300 77  WS-A17-KDPRODSL             PIC 9(2)    VALUE  ZERO.                 
009310 77  WS-ERROR-UPDX               PIC X(3)    VALUE SPACE.                 
009400                                                                          
009500 01  WS-TEST-IDFKNGRP            PIC 9(4)    VALUE  ZERO.                 
009600 01  FILLER REDEFINES WS-TEST-IDFKNGRP.                                   
009700     03  FILLER                  PIC 9(3).                                
009800     03  WS-SISTA-SIFFRAN        PIC 9(1).                                
009900                                                                          
010000 01  ARBETSAREOR.                                                         
010100     03 FILLER                   PIC X(16)   VALUE                        
010200                                             'WS-DB2-SEKTION'.            
010300     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
010400                                                                          
010500     03  WS-DAGENS-AAAAMMDD      PIC 9(8).                                
010600     03  WS-JMFR-AAAAMMDD        PIC 9(8).                                
010700     03  FILLER REDEFINES WS-JMFR-AAAAMMDD.                               
010800        05 FILLER                PIC 9(2).                                
010900        05 WS-JMFR-AA            PIC 9(2).                                
011000        05 FILLER                PIC 9(4).                                
011100                                                                          
011200     03  DAGENS-TIAAAAMMDD       PIC 9(8).                                
011300     03  WS-HHMMSSTH             PIC 9(8)    VALUE ZERO.                  
011400                                                                          
011500     03  WS-FLAGGA-Q-KAMP        PIC X(1)    VALUE SPACE.                 
011600     03  WS-FLAGGA-W-S-KAMP      PIC X(1)    VALUE SPACE.                 
011700     03  WS-KVLS-REM             PIC S9(7)   VALUE ZERO COMP-3.           
011800     03  WS-ANTAL-KAMP           PIC 9(7)    VALUE ZERO.                  
011900                                                                          
012000     03  WS-TEMFSINF             PIC X(42)   VALUE SPACE.                 
012100     03  WS-TEMFSINF-KAMP        PIC X(10)   VALUE SPACE.                 
012200     03  WS-TEMFSINF-SPLIT       PIC X(3)    VALUE '-- '.                 
012300                                                                          
012400     03 WS-YYWWD                 PIC 9(5).                                
012500     03 FILLER REDEFINES WS-YYWWD.                                        
012600         05 WS-YY                PIC 9(2).                                
012700         05 WS-WW                PIC 9(2).                                
012800         05 WS-D                 PIC 9(1).                                
012900                                                                          
013000*      --- VALID IDDC CODES                                               
013100*                                                                         
013200*01    -COPY WWDC99                                                       
013300*01    -COPY WWDCKONS                                                     
013400*01    -COPY WWLEV05                                                      
013500*                                                                         
013600*01    -COPY WWPRODSL                                                     
013700*01    -COPY WWPRODSL -PRE WS-                                            
013800       EJECT                                                              
013900 01  WS-IDTRANS                  PIC X(04).                               
014000     88  EGEN-BILD                          VALUE '1117'.                 
014100                                                                          
014200     EJECT                                                                
014300                                                                          
014400 01  WS-KDBPSR-GODK              PIC 9(01).                               
014500     88 KDBPSR-GODK                         VALUE 1                       
014600                                                  2                       
014700                                                  3                       
014800                                                  4                       
014900                                                  5                       
015000                                                  6                       
015100                                                  7                       
015200                                                  8.                      
015300     EJECT                                                                
015400 01  WS-KDSORT-GODK              PIC X(02).                               
015500     88 KDSORT-GODK                         VALUE 'ST'                    
015600                                                  'PA'                    
015700                                                  'SA'                    
015800                                                  'KG'                    
015900                                                  'M '                    
016000                                                  ' M'                    
016100                                                  'L '                    
016200                                                  ' L'                    
016300                                                  'MM'                    
016400                                                  'G '                    
016500                                                  ' G'                    
016600                                                  'C2'                    
016700                                                  'M2'                    
016800                                                  'ML'                    
016900                                                  'SW'                    
017000                                                  'TM'                    
017100                                                  'HW'.                   
017200                                                                          
017300 77  WS-KDP-KDUART               PIC X      VALUE SPACE.                  
017400                                                                          
017500 01  WS-KDUART-GODK              PIC X.                                   
017600     88 KDUART-GODK                         VALUE 'A'                     
017700                                                  'M'                     
017800                                                  'S'                     
017900                                                  'P'                     
018000                                                  'K'                     
018100                                                  'B'                     
018200                                                  ' '.                    
018300                                                                          
018400     EJECT                                                                
018500*                                                                         
018600******************************************************************        
018700*                     S W I T C H A R                            *        
018800******************************************************************        
018900*                                                                         
019000 01  SWITCHAR.                                                            
019100     05  SW-INPUT-RAETT              PIC X(01)  VALUE 'J'.                
019200     05  SW-ARTIKEL-FINNS-PA-NYPON   PIC X(01)  VALUE 'N'.                
019300     05  SW-TRAFF                    PIC X(01)  VALUE 'N'.                
019400     05  SW-GODK-IDPROJK             PIC X(01)  VALUE 'N'.                
019500     05  SW-ANSK-FINNS               PIC X(01)  VALUE 'N'.                
019600     05  SW-TISERLEV-FINNS           PIC X(01)  VALUE 'N'.                
019700     05  SW-REPL-NYPON               PIC X(01)  VALUE 'N'.                
019800     05  SW-DLET-ISRT-NYPON          PIC X(01)  VALUE 'N'.                
019900     05  SW-ISRT-TRANS-TILL-KDP      PIC X(01)  VALUE 'N'.                
020000     05  SW-KOLLA-KDPSLLOC           PIC X(01)  VALUE 'N'.                
020100     05  SW-SUPPLIER-OK              PIC X(01)  VALUE 'N'.                
020200                                                                          
020300 01  SW-PROJ-GODK                    PIC X(01).                           
020400     88  PROJ-GODK                              VALUE 'J'.                
020500                                                                          
020600 01  SW-KDSORT-AENDRAD-TILL-FRAN-SA  PIC X(01).                           
020700     88  KDSORT-AENDRAD-TILL-FRAN-SATS          VALUE 'J'.                
020800                                                                          
020900*                                                                         
021000******************************************************************        
021100*               D I V E R S E  S P A R F Ä L T                   *        
021200******************************************************************        
021300*                                                                         
021400 01  SPAR-DIVERSE.                                                        
021500                                                                          
021600                                                                          
021700     05  SPAR-TEXT-IND               PIC 9(01)  VALUE ZERO.               
021800     05  SPAR-TISERLEV-IND           PIC 9(01)  VALUE ZERO.               
021900     05  SPAR-TISERLEV-IND-MAX       PIC 9(01)  VALUE 5.                  
022000     05  SPAR-TILL-IND               PIC 9(01)  VALUE ZERO.               
022100     05  SPAR-FRAN-IND               PIC 9(01)  VALUE ZERO.               
022200     05  SPAR-IDPROJK-IND            PIC 9(02)  VALUE ZERO.               
022300     05  SPAR-IDPROJK-IND-MAX        PIC 9(02)  VALUE 75.                 
022400     05  SPAR-DAGENS-DATUM           PIC 9(07)  VALUE ZERO.               
022500     05  SPAR-DAGENS-AAVV.                                                
022600        10  SPAR-DAGENS-AA           PIC 9(02)  VALUE ZERO.               
022700        10  SPAR-DAGENS-VV           PIC 9(02)  VALUE ZERO.               
022800     05  SPAR-DAGENS-AAVV-R  REDEFINES  SPAR-DAGENS-AAVV                  
022900                                     PIC 9(04).                           
023000     05  SPAR-FLERS                  PIC X(01)  VALUE 'N'.                
023100     05  SPAR-FLIART                 PIC X(01)  VALUE 'N'.                
023200     05  SPAR-KDERS                  PIC 9(02)  VALUE ZERO.               
023300     05  SPAR-IDANSK                 PIC 9(03)  VALUE ZERO.               
023400     05  SPAR-IDRITN                 PIC X(08)  VALUE SPACE.              
023500     05  SPAR-FLPISK                 PIC X(01)  VALUE 'N'.                
023600     05  SPAR-KVPROG                 PIC 9(7)   VALUE ZERO.               
023700     05  SPAR-FLLSRDEL-OLD           PIC X(01)  VALUE SPACE.              
023800     05  SPAR-IDPROJK-TEST           PIC X(04)  VALUE SPACE.              
023900     05  SPAR-IDPROJK-OLD            PIC X(04)  VALUE SPACE.              
024000     05  SPAR-IDPROJK                PIC X(04)  VALUE SPACE.              
024100     05  SPAR-IDPROJOBJ              PIC X(04)  VALUE SPACE.              
024200     05  SPAR-KDSORT                 PIC X(02)  VALUE SPACE.              
024300     05  WS-KDSORT-OLD               PIC X(2)   VALUE SPACE.              
024400     05  SPAR-IDFKNGRP               PIC 9(05)  VALUE ZERO.               
024500     05  WS-IDFKNGRP-OLD             PIC 9(4)   VALUE ZERO.               
024600     05  SPAR-KDAGE                  PIC X      VALUE SPACE.              
024700     05  SPAR-FLRELSP                PIC X      VALUE SPACE.              
024800     05  SPAR-KDPRODSL               PIC 9(02)  VALUE ZERO.               
024900     05  SPAR-KDPRODSL-OLD           PIC 9(02)  VALUE ZERO.               
025000     05  SPAR-IDAO1                  PIC X(10)  VALUE SPACE.              
025100     05  SPAR-IDBERED                PIC 9(02)  VALUE ZERO.               
025200     05  SPAR-IDBERED-OLD            PIC 9(02)  VALUE ZERO.               
025300     05  SPAR-IDARTNR-MOTSV          PIC 9(08)  VALUE ZERO.               
025400     05  SPAR-FLBYTES                PIC X(01)  VALUE 'N'.                
025500     05  SPAR-TEORSAK                PIC X(50)  VALUE SPACE.              
025600     05  SPAR-KDRESBED               PIC X(01)  VALUE SPACE.              
025700     05  SPAR-KDBPSR                 PIC X(01)  VALUE SPACE.              
025800     05  SPAR-KDBPSR-OLD             PIC X(01)  VALUE SPACE.              
025900     05  SPAR-KDUART                 PIC X      VALUE SPACE.              
026000     05  SPAR-KDUART-OLD             PIC X      VALUE SPACE.              
026100     05  SPAR-IDLEVNR                PIC X(05)  VALUE ZERO.               
026200     05  SPAR-IDPROJ                 PIC X(04)  VALUE SPACE.              
026300     05  SPAR-IDPROJ-OLD             PIC X(04)  VALUE SPACE.              
026400     05  SPAR-RAD-IND                PIC 9(02)  VALUE ZERO.               
026500     05  SPAR-RAD-IND-MAX            PIC 9(02)  VALUE 12.                 
026600     05  SPAR-IDPROENH-IND-MAX       PIC 9(01)  VALUE 3.                  
026700     05  SPAR-IDKAT-IND-MAX          PIC 9(01)  VALUE 3.                  
026800     05  SPAR-IDAO-IND-MAX           PIC 9(01)  VALUE 5.                  
026900                                                                          
027000     05  SPAR-FLLSRDEL               PIC X(01)  VALUE 'J'.                
027100                                                                          
027200     05  SPAR-PRARTSTD               PIC 9(7)V9(2) VALUE ZERO.            
027300                                                                          
027400     05  SPAR-IDPROENH1              PIC X(08)  VALUE SPACE.              
027500     05  SPAR-IDPROENH-AREA.                                              
027600         10  SPAR-IDPROENH           PIC X(8) OCCURS 3                    
027700                           INDEXED BY SPAR-IDPROENH-IND.                  
027800                                                                          
027900  01  SPAR-DASOP-AAAAMMDD         PIC 9(08)  VALUE ZERO.                  
028000                                                                          
028100  01  SPAR-TISOP-AAVVD            PIC 9(05)  VALUE ZERO.                  
028200                                                                          
028300  01  SPAR-TISOP-AAVV.                                                    
028400    10  SPAR-TISOP-AA             PIC 9(02)  VALUE ZERO.                  
028500    10  SPAR-TISOP-VV             PIC 9(02)  VALUE ZERO.                  
028600  01  SPAR-TISOP-AAVV-R  REDEFINES  SPAR-TISOP-AAVV                       
028700                                  PIC 9(04).                              
028800                                                                          
028900  01  XX-TISOP                    PIC X(05)  VALUE SPACE.                 
029000  01  FILLER  REDEFINES  XX-TISOP.                                        
029100     05  XX-AAR-SOP               PIC X(02).                              
029200     05  XX-VV-SOP                PIC X(02).                              
029300     05  XX-DAG-SOP               PIC X(01).                              
029400                                                                          
029500  01  SPAR-TIFINLV-AAVVD.                                                 
029600    10  SPAR-TIFINLV-AAVV         PIC 9(04)  VALUE ZERO.                  
029700    10  SPAR-TIFINLV-D            PIC 9(01)  VALUE ZERO.                  
029800  01  SPAR-TIFINLV-AAVVD-R  REDEFINES  SPAR-TIFINLV-AAVVD                 
029900                                  PIC 9(05).                              
030000     EJECT                                                                
030100 01  TABELL-1.                                                            
030200     03  FILLER                  PIC 9(3)    VALUE 011.                   
030300     03  FILLER                  PIC 9(4)    VALUE 3111.                  
030400     03  FILLER                  PIC 9(4)    VALUE 3111.                  
030500     03  FILLER                  PIC 9(2)    VALUE 018.                   
030600     03  FILLER                  PIC 9(3)    VALUE 011.                   
030700     03  FILLER                  PIC 9(4)    VALUE 7702.                  
030800     03  FILLER                  PIC 9(4)    VALUE 7702.                  
030900     03  FILLER                  PIC 9(2)    VALUE 96.                    
031000     03  FILLER                  PIC 9(3)    VALUE 011.                   
031100     03  FILLER                  PIC 9(4)    VALUE 7724.                  
031200     03  FILLER                  PIC 9(4)    VALUE 7724.                  
031300     03  FILLER                  PIC 9(2)    VALUE 96.                    
031400     03  FILLER                  PIC 9(3)    VALUE 011.                   
031500     03  FILLER                  PIC 9(4)    VALUE 7726.                  
031600     03  FILLER                  PIC 9(4)    VALUE 7726.                  
031700     03  FILLER                  PIC 9(2)    VALUE 96.                    
031800     03  FILLER                  PIC 9(3)    VALUE 014.                   
031900     03  FILLER                  PIC 9(4)    VALUE 3471.                  
032000     03  FILLER                  PIC 9(4)    VALUE 3471.                  
032100     03  FILLER                  PIC 9(2)    VALUE 15.                    
032200     03  FILLER                  PIC 9(3)    VALUE 015.                   
032300     03  FILLER                  PIC 9(4)    VALUE 2750.                  
032400     03  FILLER                  PIC 9(4)    VALUE 2759.                  
032500     03  FILLER                  PIC 9(2)    VALUE 54.                    
032600     03  FILLER                  PIC 9(3)    VALUE 016.                   
032700     03  FILLER                  PIC 9(4)    VALUE 7702.                  
032800     03  FILLER                  PIC 9(4)    VALUE 7702.                  
032900     03  FILLER                  PIC 9(2)    VALUE 97.                    
033000     03  FILLER                  PIC 9(3)    VALUE 025.                   
033100     03  FILLER                  PIC 9(4)    VALUE 9910.                  
033200     03  FILLER                  PIC 9(4)    VALUE 9919.                  
033300     03  FILLER                  PIC 9(2)    VALUE 25.                    
033400     03  FILLER                  PIC 9(3)    VALUE 016.                   
033500     03  FILLER                  PIC 9(4)    VALUE 7724.                  
033600     03  FILLER                  PIC 9(4)    VALUE 7724.                  
033700     03  FILLER                  PIC 9(2)    VALUE 96.                    
033800     03  FILLER                  PIC 9(3)    VALUE 016.                   
033900     03  FILLER                  PIC 9(4)    VALUE 3900.                  
034000     03  FILLER                  PIC 9(4)    VALUE 3999.                  
034100     03  FILLER                  PIC 9(2)    VALUE 73.                    
034200     03  FILLER                  PIC 9(3)    VALUE 021.                   
034300     03  FILLER                  PIC 9(4)    VALUE 2841.                  
034400     03  FILLER                  PIC 9(4)    VALUE 2841.                  
034500     03  FILLER                  PIC 9(2)    VALUE 13.                    
034600     03  FILLER                  PIC 9(3)    VALUE 091.                   
034700     03  FILLER                  PIC 9(4)    VALUE 3111.                  
034800     03  FILLER                  PIC 9(4)    VALUE 3111.                  
034900     03  FILLER                  PIC 9(2)    VALUE 11.                    
035000     03  FILLER                  PIC 9(3)    VALUE 091.                   
035100     03  FILLER                  PIC 9(4)    VALUE 7702.                  
035200     03  FILLER                  PIC 9(4)    VALUE 7702.                  
035300     03  FILLER                  PIC 9(2)    VALUE 97.                    
035400     03  FILLER                  PIC 9(3)    VALUE 091.                   
035500     03  FILLER                  PIC 9(4)    VALUE 7724.                  
035600     03  FILLER                  PIC 9(4)    VALUE 7724.                  
035700     03  FILLER                  PIC 9(2)    VALUE 97.                    
035800     03  FILLER                  PIC 9(3)    VALUE 094.                   
035900     03  FILLER                  PIC 9(4)    VALUE 3471.                  
036000     03  FILLER                  PIC 9(4)    VALUE 3471.                  
036100     03  FILLER                  PIC 9(2)    VALUE 16.                    
036200     03  FILLER                  PIC 9(3)    VALUE 095.                   
036300     03  FILLER                  PIC 9(4)    VALUE 3900.                  
036400     03  FILLER                  PIC 9(4)    VALUE 3999.                  
036500     03  FILLER                  PIC 9(2)    VALUE 55.                    
036600     03  FILLER                  PIC 9(3)    VALUE 096.                   
036700     03  FILLER                  PIC 9(4)    VALUE 3900.                  
036800     03  FILLER                  PIC 9(4)    VALUE 3999.                  
036900     03  FILLER                  PIC 9(2)    VALUE 74.                    
037000******************************************************************        
037100*           D Y N A M I S K A  S U B P R O G R A M               *        
037200******************************************************************        
037300*                                                                         
037400 01  DYNAMISKA-SUBPROGRAM.                                                
037500     05  WDATKONV                PIC X(08)  VALUE 'WDATKONV'.             
037600     05  CBLTDLI                 PIC X(08)  VALUE 'CBLTDLI '.             
037700     05  FELLOG                  PIC X(08)  VALUE 'FELLOG  '.             
037800     05  WKPSKONV                PIC X(08)  VALUE 'WKPSKONV'.             
037900     05  W005INIT                PIC X(08)  VALUE 'W005INIT'.             
038000     05  WZ20DAYS                PIC X(08)  VALUE 'WZ20DAYS'.             
038100     03  W100LPC                 PIC X(08)  VALUE 'W100LPC'.              
038200                                                                          
038300     EJECT                                                                
038400 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
038500*   -COPY WZ20DAYS                                                        
038600     EJECT                                                                
038700                                                                          
038800 01  FILLER                      PIC X(16)   VALUE 'W100LPC-AREA'.        
038900*01  LPC-AREA  -COPY W100LPC                                              
039000     EJECT                                                                
039100******************************************************************        
039200*    F E L M E D D E L A N D E N                                          
039300******************************************************************        
039400*                                                                         
039500 01  MEDDELANDE.                                                          
039600     03 W-FEL-1.                                                          
039700        05  FILLER                  PIC X(32) VALUE                       
039800            'ARTIKELNUMMER EJ NUMERISKT      '.                           
039900        05  FILLER                  PIC X(32) VALUE                       
040000            'PARTNUMBER NOT NUMERIC          '.                           
040100     03  FILLER  REDEFINES  W-FEL-1.                                      
040200        05  FEL-1                   PIC X(32)  OCCURS 2.                  
040300                                                                          
040400     03 W-FEL-2.                                                          
040500        05  FILLER                  PIC X(32) VALUE                       
040600            'UPPLYSTA FÄLT FEL               '.                           
040700        05  FILLER                  PIC X(32) VALUE                       
040800            'CORRECT HIGH LIGHTED FIELDS     '.                           
040900     03  FILLER  REDEFINES  W-FEL-2.                                      
041000        05  FEL-2                   PIC X(32)  OCCURS 2.                  
041100                                                                          
041200     03 W-FEL-3.                                                          
041300        05  FILLER                  PIC X(32) VALUE                       
041400            'ARTIKELNUMMER SAKNAS PÅ ARTREG  '.                           
041500        05  FILLER                  PIC X(32) VALUE                       
041600            'THIS PART IS NOT IN THE DATABASE'.                           
041700     03  FILLER  REDEFINES  W-FEL-3.                                      
041800        05  FEL-3                   PIC X(32)  OCCURS 2.                  
041900                                                                          
042000     03 W-FEL-4.                                                          
042100        05  FILLER                  PIC X(32) VALUE                       
042200            'ARTIKEL ERSATT, -RENSAD         '.                           
042300        05  FILLER                  PIC X(32) VALUE                       
042400            'PARTNUMBER SUPERSEEDED, -DELETED'.                           
042500     03  FILLER  REDEFINES  W-FEL-4.                                      
042600        05  FEL-4                   PIC X(32)  OCCURS 2.                  
042700                                                                          
042800     03 W-FEL-5.                                                          
042900        05  FILLER                  PIC X(40) VALUE                       
043000            'SORT ÄNDRAD TILL/FRÅN SATS, KOLLA RASA  '.                   
043100        05  FILLER                  PIC X(40) VALUE                       
043200            'SORT CHANGED TO/FROM KIT, CHECK RASA    '.                   
043300     03  FILLER  REDEFINES  W-FEL-5.                                      
043400        05  FEL-5                   PIC X(40)  OCCURS 2.                  
043500                                                                          
043600                                                                          
043700                                                                          
043800                                                                          
043900     03 W-MED-1.                                                          
044000        05  FILLER                  PIC X(32) VALUE                       
044100            'TRYCK PF11 FÖR UPPDATERING      '.                           
044200        05  FILLER                  PIC X(32) VALUE                       
044300            'PRESS PF11 FOR UPDATING         '.                           
044400     03  FILLER  REDEFINES  W-MED-1.                                      
044500        05  MED-1                   PIC X(32)  OCCURS 2.                  
044600                                                                          
044700     03 W-MED-2.                                                          
044800        05  FILLER                  PIC X(32) VALUE                       
044900            'UPPDATERING UTFÖRD '.                                        
045000        05  FILLER                  PIC X(32) VALUE                       
045100            'UPDATED                         '.                           
045200     03  FILLER  REDEFINES  W-MED-2.                                      
045300        05  MED-2                   PIC X(32)  OCCURS 2.                  
045400                                                                          
045500     03 W-MED-5.                                                          
045600        05  FILLER                  PIC X(32) VALUE                       
045700            'ARTIKEL ERSATT                '.                             
045800        05  FILLER                  PIC X(32) VALUE                       
045900            'PARTNUMBER SUPERSEEDED        '.                             
046000     03  FILLER  REDEFINES  W-MED-5.                                      
046100        05  MED-5                   PIC X(32)  OCCURS 2.                  
046200                                                                          
046300     03 W-MED-6.                                                          
046400        05  FILLER                  PIC X(32) VALUE                       
046500            'ARTIKEL AVSLAGEN              '.                             
046600        05  FILLER                  PIC X(32) VALUE                       
046700            'REJECTED PARTNUMBER             '.                           
046800     03  FILLER  REDEFINES  W-MED-6.                                      
046900        05  MED-6                   PIC X(32)  OCCURS 2.                  
047000                                                                          
047100     03 W-MED-7.                                                          
047200        05  FILLER                  PIC X(35) VALUE                       
047300            'ARTIKEL INGÅR I 1002-SATS         '.                         
047400        05  FILLER                  PIC X(35) VALUE                       
047500            'PART NO IS INCLUDED IN A 1002-KIT '.                         
047600     03  FILLER  REDEFINES  W-MED-7.                                      
047700        05  MED-7                   PIC X(35)  OCCURS 2.                  
047800                                                                          
047900     03 W-MED-8.                                                          
048000        05  FILLER                  PIC X(35) VALUE                       
048100            'UPPDATERING EJ TILLÅTEN           '.                         
048200        05  FILLER                  PIC X(35) VALUE                       
048300            'UPDATE NOT ALLOWED                '.                         
048400     03  FILLER  REDEFINES  W-MED-8.                                      
048500        05  MED-8                   PIC X(35)  OCCURS 2.                  
048600     03 W-MED-9.                                                          
048700        05  FILLER                  PIC X(35) VALUE                       
048800            'PS SKICKADE TILL VECKOUPPDAT-BAS '.                          
048900        05  FILLER                  PIC X(35) VALUE                       
049000            'PS SENT TO WEEKLY UPDATE BASE  '.                            
049100     03  FILLER  REDEFINES  W-MED-9.                                      
049200        05  MED-9                   PIC X(35)  OCCURS 2.                  
049300                                                                          
049400                                                                          
049500                                                                          
049600****           MED-91 KAN KOMBINERAS MED MEDDELANDENA                     
049700****                  MED-2, MED-16, MED-18, MED-30 VID LÄS-TRANS         
049800     03    W-MED-91.                                                      
049900         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
050000         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
050100     03    FILLER REDEFINES W-MED-91.                                     
050200         05    MED-91        PIC X(10) OCCURS 2.                          
050300                                                                          
050400****           MED-92 KAN KOMBINERAS MED MEDDELANDENA                     
050500****                  MED-2, MED-16, MED-18, MED-30 VID LÄS-TRANS         
050600     03    W-MED-92.                                                      
050700         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
050800         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
050900     03    FILLER REDEFINES W-MED-92.                                     
051000         05    MED-92        PIC X(10) OCCURS 2.                          
051100                                                                          
051200                                                                          
051300     EJECT                                                                
051400*                                                                         
051500******************************************************************        
051600*   C O P Y T E X T E R  F Ö R   K D P  - T R A N S E N          *        
051700******************************************************************        
051800*                                                                         
051900 01  KDP-FILLER.                                                          
052000     03  FILLER                  PIC X(16)   VALUE 'KDP-COPY '.           
052100     SKIP3                                                                
052200*01  -COPY W10111                                                         
052300     EJECT                                                                
052400*01  AREA -COPY W092W001       -PRE W092-                                 
052500     EJECT                                                                
052600*                                                                         
052700******************************************************************        
052800*   C O P Y T E X T   NYPON-ROT SPARAS EVENTUELLT                *        
052900******************************************************************        
053000*                                                                         
053100 01  IMS-WS-0.                                                            
053200     03  FILLER                  PIC X(16)   VALUE 'NYPONCOPY'.           
053300     SKIP3                                                                
053400*01  AREA -COPY WDD201          -PRE SPAR-                                
053500     EJECT                                                                
053600*                                                                         
053700******************************************************************        
053800*                    C O P Y T E X T E R    (DYNAMISKA ANROP)    *        
053900******************************************************************        
054000*                                                                         
054100 01  IMS-WS-1.                                                            
054200     03  FILLER                  PIC X(16)   VALUE 'RDAT-AREA'.           
054300     SKIP3                                                                
054400*01  -COPY WDATAREA                                                       
054500     EJECT                                                                
054600******************************************************************        
054700 01  IMS-WS-2.                                                            
054800     03  FILLER                  PIC X(16)   VALUE 'RKPS-AREA'.           
054900     SKIP3                                                                
055000*01  -COPY WKPSAREA                                                       
055100     EJECT                                                                
055200******************************************************************        
055300 01  IMS-WS-3.                                                            
055400*                    ****   PARAMETRAR TILL W005INIT                      
055500     03  FILLER                  PIC X(16)   VALUE 'WMSGINIT '.           
055600*01  -COPY WMSGINIT                                                       
055700     EJECT                                                                
055800******************************************************************        
055900*              N Y C K L A R  T I L L  D L I                     *        
056000******************************************************************        
056100*                                                                         
056200 01  NYCKLAR-TILL-DLI.                                                    
056300     03  W-IDARTNR-X.                                                     
056400         05  W-IDARTNR            PIC S9(09) COMP-3 VALUE ZERO.           
056500                                                                          
056600     03  W-KDANSKQ-X.                                                     
056700         05  W-KDANSKQ            PIC  X(01)        VALUE '1'.            
056800                                                                          
056900     03  W-KDNOTTYP-X.                                                    
057000         05  W-KDNOTTYP           PIC  S9(01)  COMP-3 VALUE ZERO.         
057100                                                                          
057200     03  W-IDLEVNR-X.                                                     
057300         05  W-IDLEVNR            PIC X(05)  VALUE SPACE.                 
057400                                                                          
057500     03  W-IDSKYLT-X.                                                     
057600         05  W-IDSKYLT            PIC X(03)  VALUE SPACE.                 
057700                                                                          
057800     03  W-IDDC-X.                                                        
057900         05  W-IDDC               PIC X(2)   VALUE SPACE.                 
058000                                                                          
058100     03  W-IDLOGLOP-X.                                                    
058200         05  W-IDLOGLOP           PIC S9(01) COMP-3 VALUE ZERO.           
058300                                                                          
058400     03  W-WDD7A1KY-MIN.                                                  
058500         05  W-IDARTNR-MIN7       PIC S9(9)  COMP-3 VALUE ZERO.           
058600         05  FILLER               PIC X(7)   VALUE LOW-VALUE.             
058700                                                                          
058800     03  W-WDD7A1KY-MAX.                                                  
058900         05  W-IDARTNR-MAX7       PIC S9(9)  COMP-3                       
059000                                  VALUE ZERO.                             
059100         05  FILLER               PIC X(7)   VALUE HIGH-VALUE.            
059200                                                                          
059300     03  W-1123KEY-X.                                                     
059400         05  FILLER               PIC X(04)  VALUE '1123'.                
059500         05  W-KDPRODSL-1123      PIC S9(3)  VALUE 11   COMP-3.           
059600         05  W-IDPROJ-1123        PIC X(04)  VALUE SPACE.                 
059700         05  FILLER               PIC X(20)  VALUE LOW-VALUE.             
059800                                                                          
059900     03  W-1131KEY-X.                                                     
060000         05  FILLER               PIC X(04)  VALUE '1131'.                
060100         05  W-KDPRODSL1          PIC S9(3)  VALUE ZERO COMP-3.           
060200         05  FILLER               PIC X(24)  VALUE LOW-VALUE.             
060300                                                                          
060400     03  W-1132KEY-X.                                                     
060500         05  W-IDPROJK            PIC X(04)  VALUE SPACE.                 
060600         05  W-IDPROJOBJ          PIC X(04)  VALUE SPACE.                 
060700         05  W-IDPROJ             PIC X(04)  VALUE SPACE.                 
060800         05  FILLER               PIC X(03)  VALUE LOW-VALUE.             
060900                                                                          
061000     03  W-1137KEY-X.                                                     
061100         05  FILLER               PIC X(04)  VALUE '1137'.                
061200         05  W-KDPRODSL2          PIC S9(3)  VALUE ZERO COMP-3.           
061300         05  FILLER               PIC X(24)  VALUE LOW-VALUE.             
061400                                                                          
061500     03  W-1138KEY-X.                                                     
061600         05  W-IDUSER             PIC X(08)  VALUE SPACE.                 
061700                                                                          
061800     03  W-1139KEY-X.                                                     
061900         05  FILLER               PIC X(04)  VALUE '1139'.                
062000         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
062100                                                                          
062200     03  W-111701-KEY.                                                    
062300         05  FILLER               PIC X(04)  VALUE '1117'.                
062400         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
062500                                                                          
062600     03  W-111711-KEY.                                                    
062700         05  W-IDARTNR-1117       PIC S9(9) VALUE ZERO COMP-3.            
062800                                                                          
062900     03  W-KDAVROP-X.                                                     
063000         05  W-KDAVROP           PIC S9(1)   VALUE +2   COMP-3.           
063100                                                                          
063200     03  W-WDGX2224-X.                                                    
063300         05  W-TISENBEK-DAG-2224 PIC S9(7)    VALUE ZERO COMP-3.          
063400         05  W-TISENBEK-KL-2224  PIC S9(7)    VALUE ZERO COMP-3.          
063500         05  W-KDLARM-2224       PIC S9(3)    VALUE ZERO COMP-3.          
063600                                                                          
063700     03  W-WDGX2231-X.                                                    
063800         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
063900         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
064000                                                                          
064100     03  W-WDGX2232-X.                                                    
064200         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
064300         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
064400                                                                          
064500     03  W-B6-IDDC-MIN-X.                                                 
064600         05  W-B6-IDDC-MIN        PIC X(2) VALUE LOW-VALUE.               
064700                                                                          
064800     03  W-B6-IDDC-MAX-X.                                                 
064900         05  W-B6-IDDC-MAX        PIC X(2) VALUE HIGH-VALUE.              
065000                                                                          
065100     03  W-WDGX2263-X.                                                    
065200         05  W-IDHTYP-2263       PIC X(4)    VALUE '2263'.                
065300         05  W-FILLER            PIC X(26)   VALUE LOW-VALUE.             
065400     03  W-TISOP-O-X.                                                     
065500         05  W-TISOP-2264-O       PIC S9(5)           COMP-3.             
065600     03  W-W2266KY-MIN-O-X.                                               
065700         05  W-IDARTNR-2266-O-MIN PIC S9(9)           COMP-3.             
065800         05  W-IDDC-2266-O-MIN    PIC X(2)    VALUE LOW-VALUE.            
065900     03  W-W2266KY-MAX-O-X.                                               
066000         05  W-IDARTNR-2266-O-MAX PIC S9(9)           COMP-3.             
066100         05  W-IDDC-2266-O-MAX    PIC X(2)    VALUE HIGH-VALUE.           
066200     EJECT                                                                
066300*                                                                         
066400******************************************************************        
066500*                    M I D-C O P Y T E X T                       *        
066600******************************************************************        
066700*                                                                         
066800*                        ****    MFS OCH SKÄRMHANTERING                   
066900 01  IMS-WS-3.                                                            
067000     03  FILLER                  PIC X(16)   VALUE 'MFS-WS'.              
067100     SKIP3                                                                
067200*01  MID -COPY W1I11701                                                   
067300     EJECT                                                                
067400*                                                                         
067410 01  MSG-KOM-MESSAGE-CODES.                                               
067420     03  FEL-ERR-FIELD           PIC X(3)    VALUE '020'.                 
067430     03  FEL-ERR-IDDC            PIC X(3)    VALUE '028'.                 
067440     03  FEL-ERR-IDARTNR         PIC X(3)    VALUE '768'.                 
067450     03  FEL-ERR-IDBERED         PIC X(3)    VALUE '96A'.                 
067460     03  FEL-ERR-KDPRODSL        PIC X(3)    VALUE '96B'.                 
067470     03  FEL-ERR-KDSORT          PIC X(3)    VALUE '96C'.                 
067480     03  FEL-ERR-IDPROENH        PIC X(3)    VALUE '96D'.                 
067490     03  FEL-ERR-KDYTBEH         PIC X(3)    VALUE '96E'.                 
067491     03  FEL-ERR-IDPROJ          PIC X(3)    VALUE '96F'.                 
067492     03  FEL-ERR-KDFARLIG        PIC X(3)    VALUE '96G'.                 
067493     03  FEL-ERR-KDBPSR          PIC X(3)    VALUE '96H'.                 
067494     03  FEL-ERR-KDUART          PIC X(3)    VALUE '96I'.                 
067495     03  FEL-ERR-IDPROJK         PIC X(3)    VALUE '96J'.                 
067496     03  FEL-ERR-FLPISK          PIC X(3)    VALUE '96K'.                 
067497     03  FEL-ERR-IDAO            PIC X(3)    VALUE '96L'.                 
067498     03  FEL-ERR-TISOP           PIC X(3)    VALUE '96M'.                 
067499     03  FEL-ERR-IDSKYLT         PIC X(3)    VALUE '96N'.                 
067500     03  FEL-ERR-FLLSRDEL        PIC X(3)    VALUE '96O'.                 
067501     03  FEL-ERR-IDPROJUP        PIC X(3)    VALUE '96P'.                 
067502     03  FEL-ERR-BEART           PIC X(3)    VALUE '96Q'.                 
067503     03  FEL-ERR-FLRSBEART       PIC X(3)    VALUE '96R'.                 
067504     03  FEL-ERR-IDFKNGRP        PIC X(3)    VALUE '96S'.                 
067505     03  FEL-ERR-TEORSAK-1       PIC X(3)    VALUE '96T'.                 
067506     03  FEL-ERR-TEARTNOT        PIC X(3)    VALUE '96U'.                 
067507     03  FEL-ERR-IDRITN          PIC X(3)    VALUE '96V'.                 
067508     03  FEL-ERR-IDPSN           PIC X(3)    VALUE '96W'.                 
067509     03  FEL-ERR-KDARTHNT        PIC X(3)    VALUE '96X'.                 
067510     03  FEL-ERR-KDEMBKOD-2      PIC X(3)    VALUE '96Z'.                 
067511     03  FEL-ERR-VLFG            PIC X(3)    VALUE '97A'.                 
067512     03  FEL-ERR-KDSORT-VLFG     PIC X(3)    VALUE '97B'.                 
067513     03  FEL-ERR-VKFORSFG        PIC X(3)    VALUE '97C'.                 
067514     03  FEL-ERR-IDCDS           PIC X(3)    VALUE '97D'.                 
067515     03  FEL-ERR-KDARTSYS        PIC X(3)    VALUE '97E'.                 
067516     03  FEL-ERR-FLAGMART        PIC X(3)    VALUE '97F'.                 
067517     03  FEL-ERR-KVPROG          PIC X(3)    VALUE '97G'.                 
067517     03  FEL-ERR-TEVARNOT        PIC X(3)    VALUE '97H'.                 
067518     03  FEL-ERR-KVARTVAGN       PIC X(3)    VALUE '181'.                 
067519     03  FEL-ERR-BELEV           PIC X(3)    VALUE '092'.                 
067520     03  FEL-ERR-FLBYTES         PIC X(3)    VALUE '358'.                 
067521     03  FEL-ERR-IDFTG           PIC X(3)    VALUE '950'.                 
067522     03  OK-GODKANT-FEL          PIC X(3)    VALUE '114'.                 
067523     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
067524     EJECT                                                                
067530******************************************************************        
067600*                    M S G - A R E A                             *        
067700******************************************************************        
067800*                                                                         
067813*    --- GENERAL  IO-COMMUNICATION  FOR DISPATCHER                        
067820*01    -COPY WMSGKOM                                                      
067830     EJECT                                                                
067840*                                                                         
067900 01  IMS-WS-4.                                                            
068000     03  FILLER                  PIC X(16)   VALUE 'MSG-AREA'.            
068100     SKIP3                                                                
068200*01  -COPY WMSGAREA                                                       
068300     EJECT                                                                
068400*                                                                         
068500******************************************************************        
068600*                    M O D-C O P Y T E X T                       *        
068700******************************************************************        
068800*                                                                         
068900*    03  MOD -COPY W1O11701  -RED MSG-AREA.                               
069000     EJECT                                                                
069100*                                                                         
069200******************************************************************        
069300*                    M F S - A R E A                             *        
069400******************************************************************        
069500*                                                                         
069600 01  IMS-WS-6.                                                            
069700     03  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.            
069800     SKIP3                                                                
069900*01  -COPY WMFSAREA.                                                      
070000     EJECT                                                                
070100*                                                                         
070200******************************************************************        
070300*        A R B E T S- AREOR TILL  D B 2 -SEKTIONERNA                      
070400******************************************************************        
070500 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
070600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
070700                                                                          
070800 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
070900 01  DB2-WS.                                                              
071000     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
071100         88  CURSOR-OK                      VALUE 000.                    
071200         88  LINES-FOUND                    VALUE 000.                    
071300         88  LINES-MISSING                  VALUE 100.                    
071400         88  RESOURCE-WRONG                 VALUE 904.                    
071500     03  GOOD-SQLCODECODES.                                               
071600         05  GOOD-SQLCODE OCCURS 5                                        
071700             INDEXED BY SQLCODE-IX PIC 9(3).                              
071800     EJECT                                                                
071900*                                                                         
072000******************************************************************        
072100*    A R B E T S A R E O R  I M S - S E K T I O N E R N A        *        
072200******************************************************************        
072300*                                                                         
072400 01  IMS-WS-7.                                                            
072500     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
072600     SKIP3                                                                
072700*****                    **** STATUS-KOD FRÅN IMS                         
072800     03  STATUS-WS               PIC X(2).                                
072900         88  SEGMENT-FINNS                   VALUE '  '.                  
073000         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
073100         88  SEGMENT-SLUT                    VALUE 'GB'.                  
073200         88  SEGMENT-FINNS-REDAN             VALUE 'II'.                  
073300     SKIP3                                                                
073400     03  GODK-STATUSKODER.                                                
073500         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
073600     SKIP3                                                                
073700 01  IMS-WS-8.                                                            
073800     03  FILLER                  PIC X(09)   VALUE 'SSA:ER   '.           
073900     SKIP3                                                                
074000 01  SSA1                        PIC X(128).                              
074100 01  SSA2                        PIC X(128).                              
074200 01  SSA3                        PIC X(128).                              
074300     EJECT                                                                
074400*                                                                         
074500******************************************************************        
074600*            I M S  F U N K T I O N S K O D E R                  *        
074700******************************************************************        
074800*                                                                         
074900*                                                                         
075000 01  IMS-WS-9.                                                            
075100     03  FILLER                  PIC X(16)   VALUE ' IMS-FUNK'.           
075200     SKIP3                                                                
075300*01  -COPY W0003                                                          
075400     EJECT                                                                
075500*                                                                         
075600******************************************************************        
075700*            D L I  I N P U T-O U T P U T A R E A                *        
075800******************************************************************        
075900*                                                                         
076000******************************************************************        
076100*            S E G M E N T C O P Y T E X T E R                   *        
076200******************************************************************        
076300 01  IMS-WS-10.                                                           
076400     03  FILLER                 PIC X(16) VALUE 'DLI-IO-AREA'.            
076500     SKIP3                                                                
076600 01  DLI-IO-ARTC.                                                         
076700     03  IO-ARTC                   PIC X(900) VALUE SPACE.                
076800*    03  ARTC -COPY WDK601                   -RED IO-ARTC.                
076900     EJECT                                                                
077000*    03  ARTC -COPY WDK611                   -RED IO-ARTC.                
077100     EJECT                                                                
077200*    03  ARTC -COPY WDK625                   -RED IO-ARTC.                
077300     EJECT                                                                
077400                                                                          
077500 01  DLI-IO-WDD3.                                                         
077600     03  IO-WDD3                   PIC X(200) VALUE SPACE.                
077700*    03  BENA -COPY WDD311      -PRE BENA11-  -RED IO-WDD3.               
077800     EJECT                                                                
077900                                                                          
078000 01  DLI-IO-AREA1.                                                        
078100     03  IO-AREA1                  PIC X(300) VALUE SPACE.                
078200*    03  ERSB -COPY WDD7A1      -PRE ERSB01- -RED IO-AREA1.               
078300     EJECT                                                                
078400*    03  LEVA -COPY WDF101      -PRE LEVA01- -RED IO-AREA1.               
078500     EJECT                                                                
078600*    03  XXAQ -COPY WDGX1132    -PRE XXAQ-  -RED IO-AREA1.                
078700     EJECT                                                                
078800*    03  XXAT -COPY WDGX1138    -PRE XXAT-  -RED IO-AREA1.                
078900     EJECT                                                                
079000*    03  XXAQ -COPY WDGX1123    -PRE XXAP-  -RED IO-AREA1.                
079100     EJECT                                                                
079200*    03  XXAP -COPY WDGX1124    -PRE XXAP-  -RED IO-AREA1.                
079300     EJECT                                                                
079400 01  DLI-IO-WDGZ01.                                                       
079500*    03  ZZAC -COPY WDGZ01      -PRE ZZAC-.                               
079600     EJECT                                                                
079700 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2264-O'.                  
079800 01  DLI-IO-WDGX2264-O.                                                   
079900*    03  -COPY WDGX2264       -PRE OLD-                                   
080000     EJECT                                                                
080100 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2266-O'.                  
080200 01  DLI-IO-WDGX2266-O.                                                   
080300*    03  -COPY WDGX2266      -PRE OLD-                                    
080400     EJECT                                                                
080500 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2264-N'.                  
080600 01  DLI-IO-WDGX2264-N.                                                   
080700*    03  -COPY WDGX2264       -PRE NEW-                                   
080800     EJECT                                                                
080900 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2266-N'.                  
081000 01  DLI-IO-WDGX2266-N.                                                   
081100*    03  -COPY WDGX2266      -PRE NEW-                                    
081200     EJECT                                                                
081300*                                                                         
081400******************************************************************        
081500*            D L I  I N P U T-O U T P U T A R E A -2             *        
081600******************************************************************        
081700*                                                                         
081800 01  IMS-WS-16.                                                           
081900     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA2'.           
082000     SKIP3                                                                
082100 01  DLI-IO-AREA2.                                                        
082200     03  IO-AREA2                  PIC X(550)   VALUE SPACE.              
082300     SKIP3                                                                
082400*    03  ARTG -COPY WDD201        -PRE NYPON-  -RED IO-AREA2.             
082500     EJECT                                                                
082600*                                                                         
082700******************************************************************        
082800*            D L I  I N P U T-O U T P U T A R E A -3             *        
082900******************************************************************        
083000*                                                                         
083100 01  IMS-WS-93.                                                           
083200     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA3'.           
083300     SKIP3                                                                
083400 01  DLI-IO-AREA3.                                                        
083500     03  IO-AREA3                  PIC X(550)   VALUE SPACE.              
083600     SKIP3                                                                
083700*    03  1117 -COPY WDGX1118    -PRE PSUPD-  -RED IO-AREA3.               
083800     EJECT                                                                
083900*                                                                         
084000******************************************************************        
084100*            D L I  I N P U T-O U T P U T A R E A -4             *        
084200******************************************************************        
084300*                                                                         
084400 01  IMS-WS-17.                                                           
084500     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA4'.           
084600     SKIP3                                                                
084700 01  DLI-IO-AREA4.                                                        
084800     03  IO-AREA4                  PIC X(1500)  VALUE SPACE.              
084900     SKIP3                                                                
085000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
085100 01  DLI-IO-WDK701.                                                       
085200*    03  -COPY WDK701                                                     
085300     SKIP3                                                                
085400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
085500 01  DLI-IO-WDK711.                                                       
085600*    03  -COPY WDK711                                                     
085700     SKIP3                                                                
085800     EJECT                                                                
085900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR220'.                      
086000 01  DLI-IO-AREA-2232.                                                    
086100*    03  -COPY WDGX2232   -PRE WDR220-                                    
086200     EJECT                                                                
086300 01  FILLER                      PIC X(16)   VALUE 'WDR801-AREA'.         
086400 01  DLI-IO-WDR801.                                                       
086500*    03  -COPY WDR801                                                     
086600     EJECT                                                                
086700 01  FILLER                      PIC X(16)   VALUE 'A17-TRANS'.           
086800**   ---- A17-TRANS                                                       
086900 01  -COPY W510A17    -PRE A17-                                           
087000                                                                          
087100     EJECT                                                                
087200 01  FILLER                      PIC X(16)   VALUE 'WDB601-AREA'.         
087300 01  DLI-IO-WDB601.                                                       
087400*    03  -COPY WDB601                                                     
087500     EJECT                                                                
087600*                                                                         
087700******************************************************************        
087800*             D B 2  INPUT-OUTPUT   A R E A                               
087900******************************************************************        
088000                                                                          
088100 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
088200*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
088300     EJECT                                                                
088400 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
088500*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
088600     EJECT                                                                
088700     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
088800     EJECT                                                                
088900     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
089000     EJECT                                                                
089100                                                                          
089200*                                                                         
089300******************************************************************        
089400*            L I N K A G E  S E C T I O N                        *        
089500******************************************************************        
089600*                                                                         
089700 LINKAGE SECTION.                                                         
089800     SKIP2                                                                
089900*01  -COPY W0009     -PRE MSG-                                            
090000     EJECT                                                                
090010*01  -COPY W0008     -PRE MSGKOM-                                         
090020         05  FILLER              PIC X.                                   
090030     EJECT                                                                
090100*01  -COPY W0008     -PRE USEA-                                           
090200         05  FILLER              PIC X.                                   
090330     EJECT                                                                
090400*01  -COPY W0008     -PRE ARTC-                                           
090500         05  FILLER              PIC X.                                   
090600     EJECT                                                                
090700*01  -COPY W0008     -PRE BENA-                                           
090800         05  FILLER              PIC X.                                   
090900     EJECT                                                                
091000*01  -COPY W0008     -PRE ERSB-                                           
091100         05  FILLER              PIC X.                                   
091200     EJECT                                                                
091300*01  -COPY W0008     -PRE ARTG-                                           
091400         05  FILLER              PIC X.                                   
091500     EJECT                                                                
091600*01  -COPY W0008     -PRE LEVA-                                           
091700         05  FILLER              PIC X.                                   
091800     EJECT                                                                
091900*01  -COPY W0008     -PRE XXAQ-                                           
092000         05  FILLER              PIC X.                                   
092100     EJECT                                                                
092200*01  -COPY W0008     -PRE XXAT-                                           
092300         05  FILLER              PIC X.                                   
092400     EJECT                                                                
092500*01  -COPY W0008     -PRE XXAP-                                           
092600         05  FILLER              PIC X.                                   
092700     EJECT                                                                
092800*01  -COPY W0008     -PRE ZZAC-                                           
092900         05  FILLER              PIC X.                                   
093000     EJECT                                                                
093100*01  -COPY W0008     -PRE 1117-                                           
093200         05  FILLER              PIC X.                                   
093300     EJECT                                                                
093400*01  -COPY W0008     -PRE WDK7-                                           
093500         05  FILLER              PIC X.                                   
093600     EJECT                                                                
093700*01  -COPY W0008     -PRE WDR2-                                           
093800         05  FILLER              PIC X.                                   
093900     EJECT                                                                
094000*01  -COPY W0008     -PRE WDR8-                                           
094100         05  FILLER              PIC X.                                   
094200     EJECT                                                                
094300*01  -COPY W0008     -PRE WDB6-                                           
094400         05  FILLER              PIC X.                                   
094500     EJECT                                                                
094600*01    -COPY W0008     -PRE WDR2-O-                                       
094700     05  FILLER                  PIC X.                                   
094800     EJECT                                                                
094900*01    -COPY W0008     -PRE WDR2-N-                                       
095000     05  FILLER                  PIC X.                                   
095100     EJECT                                                                
095200 PROCEDURE DIVISION USING MSG-PCB MSGKOM-PCB USEA-PCB                     
095300                                  ARTC-PCB BENA-PCB                       
095400     ERSB-PCB ARTG-PCB LEVA-PCB                                           
095500     XXAQ-PCB XXAT-PCB XXAP-PCB ZZAC-PCB                                  
095600     1117-PCB WDK7-PCB WDR2-PCB                                           
095700     WDR8-PCB WDB6-PCB WDR2-O-PCB WDR2-N-PCB.                             
095800     SKIP1                                                                
095900     ENTRY 'DLITCBL' USING MSG-PCB MSGKOM-PCB USEA-PCB                    
096000     ARTC-PCB BENA-PCB ERSB-PCB ARTG-PCB LEVA-PCB                         
096100     XXAQ-PCB XXAT-PCB XXAP-PCB ZZAC-PCB                                  
096200     1117-PCB WDK7-PCB WDR2-PCB                                           
096300     WDR8-PCB WDB6-PCB WDR2-O-PCB WDR2-N-PCB.                             
096400                                                                          
096500     PERFORM IMS-GET-MSG                                                  
096600     IF SEGMENT-FINNS                                                     
096700        PERFORM IMS-GET-WMSGKOM-MSG                                       
096710        PERFORM A-INIT-SPARA-INPUT                                        
096800        IF WS-IDARTNR NUMERIC                                             
096900           MOVE WS-IDARTNR  TO W-IDARTNR                                  
097000           PERFORM IMS-GHU-ARTC01                                         
097100           IF SEGMENT-FINNS                                               
097200              IF ART-KDERS-UTG > ZERO                                     
097300                 MOVE FEL-4 (SPAR-TEXT-IND) TO MOD-TEMFSFEL               
097400              ELSE                                                        
097500                 PERFORM IMS-GHU-ARTG01-MED-GE                            
097600                 IF SEGMENT-FINNS                                         
097700                    MOVE JA      TO SW-ARTIKEL-FINNS-PA-NYPON             
097800                    MOVE NYPON-ART-IDPROJOBJ TO SPAR-IDPROJOBJ            
097900                 END-IF                                                   
098000                 IF MFS-UPDATE OR MFS-UPD-X                               
098100                    PERFORM C-KOLLA-INPUT                                 
098200                    IF SW-INPUT-RAETT = JA                                
098300                       PERFORM D-RELATIONSKOLL                            
098400                       IF SW-INPUT-RAETT = JA                             
098500                          PERFORM E-UPPDATERA-OCH-VISA-BILD               
098600                       ELSE                                               
098700                          MOVE FEL-2 (SPAR-TEXT-IND)                      
098800                                     TO MOD-TEMFSFEL                      
098900                       END-IF                                             
099000                    ELSE                                                  
099100                       MOVE FEL-2 (SPAR-TEXT-IND)                         
099200                                   TO MOD-TEMFSFEL                        
099300                    END-IF                                                
099400                 ELSE                                                     
099500                    MOVE ART-KDPRODSL TO WS-KDPRODSL-LEV                  
099600                    PERFORM S11-CHECK-SUPPLIER                            
099700                    IF SW-INPUT-RAETT = JA                                
099800                       PERFORM B-VISA-BILD                                
099900                    END-IF                                                
100000                 END-IF                                                   
100100              END-IF                                                      
100200           ELSE                                                           
100300              MOVE FEL-3 (SPAR-TEXT-IND)    TO MOD-TEMFSFEL               
100400           END-IF                                                         
100500        ELSE                                                              
100600           MOVE FEL-1 (SPAR-TEXT-IND) TO MOD-TEMFSFEL                     
100700        END-IF                                                            
100800                                                                          
100810        IF MFS-UPD-X                                                      
100820*          X-TRANS FRÅN DISPATCHERN SKALL INTE SVARA EN SKÄRM             
100830           IF  WS-ERROR-UPDX  = SPACE                                     
100841             MOVE OK-BEHANDLAD TO MSG-KOM-IDMFSMED                        
100842             PERFORM IMS-INSERT-WMSGKOM-MSG                               
100850           ELSE                                                           
100860             MOVE WS-ERROR-UPDX TO MSG-KOM-IDMFSMED                       
100880             MOVE '1'         TO MSG-KOM-KDSVAR                           
100890             PERFORM IMS-INSERT-WMSGKOM-MSG                               
100891           END-IF                                                         
100892        ELSE                                                              
100893           COMPUTE MSG-KVLL = LENGTH OF MOD-W1O11701 + 4                  
100894           PERFORM IMS-INSERT-MSG                                         
100895        END-IF                                                            
101100     END-IF                                                               
101200                                                                          
101300     MOVE ZERO TO RETURN-CODE                                             
101400     GOBACK.                                                              
101500     EJECT                                                                
101600 A-INIT-SPARA-INPUT SECTION.                                              
101700     SKIP2                                                                
101800     IF MSG-DUBBLA-TRANSKODER                                             
101900         MOVE MSG-IDTRANS-2                 TO                            
102000                                         MFS-IDTRANS WS-IDTRANS           
102100         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I11701               
102200         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
102400         MOVE MSG-IDPFK                     TO MFS-IDPFK                  
102500     ELSE                                                                 
102600         MOVE MSG-IDTRANS-1                 TO                            
102700                                          MFS-IDTRANS WS-IDTRANS          
102800         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W1I11701               
102900         MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR               
103100                                               MFS-IDPFK                  
103200     END-IF                                                               
103300                                                                          
103400     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
103400     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
103400     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
103400     IF EGEN-BILD OR MFS-UPD-X                                            
103500        CONTINUE                                                          
103600     ELSE                                                                 
103700        MOVE SPACE TO MFS-KDTRTYP                                         
103800                      MFS-IDPFK                                           
103900     END-IF                                                               
104000     EJECT                                                                
104100     MOVE LOW-VALUE         TO MOD-W1O11701                               
104200     MOVE 'W1O117N1'        TO MFS-IDMOD                                  
104300     MOVE '1117'            TO MOD-IDTRANS                                
104400                                                                          
104500     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL                               
104600                               MOD-TEMFSINF                               
104700                               MOD-IDARTNR-IN                             
104800     MOVE SPACE TO             WS-TEMFSINF                                
104900                               WS-TEMFSINF-KAMP                           
105000                                                                          
105030                                                                          
105100     ACCEPT SPAR-DAGENS-DATUM FROM DATE                                   
105200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
105300                                                                          
105400     MOVE SPAR-DAGENS-DATUM     TO DAT-I-TIDATUM                          
105500     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
105600     PERFORM S99-WDATKONV                                                 
105700                                                                          
105800     IF DAT-KDSVAR-OK                                                     
105900        MOVE DAT-TIAA-VECKA     TO SPAR-DAGENS-AA                         
106000        MOVE DAT-TIVV           TO SPAR-DAGENS-VV                         
106100     END-IF                                                               
106200                                                                          
106210     IF MFS-UPD-X                                                         
106220****************  DISPATCHER CALL                                         
106240       IF MID-IDARTNR-IN NUMERIC AND MID-IDARTNR-IN > ZERO                
106260          MOVE MID-IDARTNR-UT TO WS-IDARTNR                               
106270          MOVE WS-IDARTNR  TO MOD-IDARTNR-UT                              
106271          INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE          
106280       END-IF                                                             
106270       MOVE '11'        TO WS-IDDC                                        
106290     ELSE                                                                 
106300       MOVE ALL '+' TO MSGI-WMSGINIT                                      
106400       MOVE '001'           TO MSGI-KDCALL                                
106500       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
106600       MOVE MSG-LTERM-NAME  TO MSGI-IDLTERM-USER                          
106700       MOVE '1117'          TO MSGI-IDTRANS                               
106800       IF MFS-IDTRANS = '1117'                                            
106900       OR (MID-IDARTNR-IN NUMERIC                                         
107000       AND MID-IDARTNR-IN > ZERO)                                         
107100           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
107200       END-IF                                                             
107300       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
107400       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
107500       MOVE MSGI-IDDC  TO WS-IDDC                                         
107600       INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                     
107700                                                                          
107800       IF MID-IDARTNR-IN = ALL '+'                                        
107900          CONTINUE                                                        
108000       ELSE                                                               
108100          MOVE SPACE        TO MFS-KDTRTYP                                
108200                                 MFS-IDPFK                                
108300       END-IF                                                             
108400                                                                          
108500       IF MFS-UPDATE                                                      
108600          IF MID-INDEL  = ALL '+'                                         
108700             MOVE SPACE     TO MFS-KDTRTYP                                
108800          END-IF                                                          
108900       END-IF                                                             
109000                                                                          
109100       MOVE WS-IDARTNR      TO MOD-IDARTNR-UT                             
109200       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
109300                                                                          
109310     END-IF                                                               
109400     IF MSGI-IDLAND-SPR = 'GB'                                            
109500        MOVE 2                   TO SPAR-TEXT-IND                         
109600     ELSE                                                                 
109700        MOVE 1                   TO SPAR-TEXT-IND                         
109800     END-IF.                                                              
109900                                                                          
110000     EJECT                                                                
110100 B-VISA-BILD SECTION.                                                     
110200     SKIP2                                                                
110300***** I SLUTET AV DENNA SEKTION SÄTTS MOD-TEMFSINF IHOP FRÅN              
110400*****                       WS-TEMFSINF-KAMP   OCH   WS-TEMFSINF          
110500     PERFORM BA-LAS-VISA-ARTC                                             
110600     PERFORM BC-LAS-VISA-BENA                                             
110700                                                                          
110800     IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                    
110900        PERFORM BD-VISA-ARTG                                              
111000     ELSE                                                                 
111100        PERFORM BE-RENSA-NYPON-MODFAELT                                   
111200     END-IF                                                               
111300     PERFORM BG-KOLLA-KAMPANJ                                             
111400                                                                          
111500     IF MID-IDARTNR-IN = ALL '+'  AND  EGEN-BILD                          
111600        IF MID-INDEL   = ALL '+'                                          
111700*          GAMMAL NYCKEL, INGENTING INMATAT                               
111800           PERFORM S03-RENSA-MOD-INMATNINGSFAELT                          
111900        ELSE                                                              
112000           MOVE MED-1 (SPAR-TEXT-IND)    TO WS-TEMFSINF                   
112100           PERFORM BF-KOLLA-INMATADE-FAELT                                
112200        END-IF                                                            
112300     ELSE                                                                 
112400        PERFORM S03-RENSA-MOD-INMATNINGSFAELT                             
112500     END-IF                                                               
112600                                                                          
112700*    ---- SÄTT IHOP EV. MEDDELANDEN PÅ RAD 23 ------------                
112800     IF WS-TEMFSINF = SPACE  AND WS-TEMFSINF-KAMP = SPACE                 
112900         CONTINUE                                                         
113000     ELSE                                                                 
113100       IF WS-TEMFSINF = SPACE                                             
113200           MOVE WS-TEMFSINF-KAMP TO MOD-TEMFSINF                          
113300       ELSE                                                               
113400         IF WS-TEMFSINF-KAMP = SPACE                                      
113500             MOVE WS-TEMFSINF TO MOD-TEMFSINF                             
113600         ELSE                                                             
113700*            --- OBS MAX. 55 TECKEN                                       
113800             STRING WS-TEMFSINF-KAMP  DELIMITED BY SIZE                   
113900                    WS-TEMFSINF-SPLIT DELIMITED BY SIZE                   
114000                    WS-TEMFSINF       DELIMITED BY SIZE                   
114100             INTO MOD-TEMFSINF                                            
114200         END-IF                                                           
114300       END-IF                                                             
114400     END-IF                                                               
114500     .                                                                    
114600     EJECT                                                                
114700 BA-LAS-VISA-ARTC SECTION.                                                
114800     SKIP2                                                                
114900     MOVE ART-TIFINLV                TO MOD-TIFINLV                       
115000     MOVE ART-TISOP                  TO MOD-TISOP                         
115100                                                                          
115200     MOVE ART-IDAO (1)               TO MOD-IDAO (1)                      
115300     MOVE ART-IDAO (2)               TO MOD-IDAO (2)                      
115400     MOVE ART-IDAO (3)               TO MOD-IDAO (3)                      
115500     MOVE ART-IDAO (4)               TO MOD-IDAO (4)                      
115600     MOVE ART-IDAO (5)               TO MOD-IDAO (5)                      
115700                                                                          
115800     MOVE ART-KDPRODSL               TO MOD-KDPRODSL                      
115900     MOVE ART-IDFKNGRP               TO MOD-IDFKNGRP                      
116000     MOVE ART-KDSORT                 TO MOD-KDSORT                        
116100                                                                          
116200*WDK611                                                                   
116300     PERFORM IMS-GHNP-ARTC11                                              
116400     MOVE CLAG-IDBERED               TO MOD-IDBERED                       
116500     MOVE CLAG-IDPROJ                TO MOD-IDPROJ                        
116600     MOVE CLAG-IDKAT (1)             TO MOD-IDKAT (1)                     
116700     MOVE CLAG-IDKAT (2)             TO MOD-IDKAT (2)                     
116800     MOVE CLAG-IDKAT (3)             TO MOD-IDKAT (3)                     
116900     MOVE CLAG-IDPROJUP              TO MOD-IDPROJUP                      
117000     MOVE CLAG-IDRITN                TO MOD-IDRITN                        
117100     MOVE CLAG-IDPROENH(1)           TO MOD-IDPROENH(1)                   
117200     INSPECT MOD-IDPROENH(1) REPLACING LEADING ZERO BY SPACE              
117300     MOVE CLAG-IDPROENH(2)           TO MOD-IDPROENH(2)                   
117400     INSPECT MOD-IDPROENH(2) REPLACING LEADING ZERO BY SPACE              
117500     MOVE CLAG-IDPROENH(3)           TO MOD-IDPROENH(3)                   
117600     INSPECT MOD-IDPROENH(3) REPLACING LEADING ZERO BY SPACE              
117700     MOVE CLAG-KDAGE                 TO MOD-KDAGE                         
117800     MOVE CLAG-KDUART                TO MOD-KDUART                        
117900     MOVE CLAG-FLLSRDEL              TO MOD-FLLSRDEL                      
118000     MOVE CLAG-KDBPSR                TO MOD-KDBPSR                        
118100                                                                          
118200     IF CLAG-KDERS > ZERO                                                 
118300        MOVE MED-5 (SPAR-TEXT-IND)   TO WS-TEMFSINF                       
118400     END-IF                                                               
118500                                                                          
118600     MOVE +6 TO W-KDNOTTYP                                                
118700     PERFORM IMS-GHNP-ARTC25                                              
118800     IF SEGMENT-FINNS                                                     
118900        MOVE NOT-TEARTNOT            TO MOD-TEVARNOT-IN-UT                
119000     ELSE                                                                 
119100        MOVE MFS-RENSA-FAELT         TO MOD-TEVARNOT-IN-UT                
119200     END-IF                                                               
119300     .                                                                    
119400     EJECT                                                                
119500 BC-LAS-VISA-BENA SECTION.                                                
119600     SKIP2                                                                
119700*WDD311                                                                   
119800     IF MSGI-IDLAND-SPR = 'GB'                                            
119900        MOVE 'GB '                   TO W-IDSKYLT                         
120000     ELSE                                                                 
120100        MOVE 'S  '                   TO W-IDSKYLT                         
120200     END-IF                                                               
120300                                                                          
120400     PERFORM IMS-GU-BENA11-BSEQ                                           
120500     MOVE BENA11-TEXT-BEART          TO MOD-BEART.                        
120600                                                                          
120700     EJECT                                                                
120800 BD-VISA-ARTG    SECTION.                                                 
120900     SKIP2                                                                
121000     MOVE NYPON-ART-FLPISK           TO MOD-FLPISK                        
121100     MOVE NYPON-ART-IDPROJK          TO MOD-IDPROJK                       
121200     MOVE NYPON-ART-IDARTNR-MOTSV    TO MOD-IDARTNR-MOTSV                 
121300     MOVE NYPON-ART-FLBYTES          TO MOD-FLBYTES                       
121400     MOVE NYPON-ART-KVARTVAGN        TO MOD-KVARTVAGN                     
121500     MOVE NYPON-ART-TEORSAK          TO MOD-TEORSAK-IN-UT                 
121600     MOVE NYPON-ART-KVPROG           TO MOD-KVPROG                        
121700                                                                          
121800     IF NYPON-ART-KDRESBED = '-'                                          
121900        MOVE MED-6 (SPAR-TEXT-IND)   TO WS-TEMFSINF                       
122000     END-IF.                                                              
122100     EJECT                                                                
122200 BE-RENSA-NYPON-MODFAELT SECTION.                                         
122300     SKIP2                                                                
122400     MOVE MFS-RENSA-FAELT            TO MOD-FLPISK                        
122500                                        MOD-IDPROJK                       
122600                                        MOD-IDARTNR-MOTSV                 
122700                                        MOD-KVARTVAGN                     
122800                                        MOD-FLBYTES                       
122900                                        MOD-TEORSAK-IN-UT                 
123000                                        MOD-KVPROG.                       
123100     EJECT                                                                
123200 BF-KOLLA-INMATADE-FAELT SECTION.                                         
123300     SKIP2                                                                
123400     IF MID-IDBERED       = ALL '+'                                       
123500        MOVE MFS-RENSA-FAELT      TO MOD-IDBERED-IN                       
123600     ELSE                                                                 
123700        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDBERED-IN                       
123800        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDBERED-IN-ATTR                  
123900     END-IF                                                               
124000                                                                          
124100     IF MID-KDPRODSL      = ALL '+'                                       
124200        MOVE MFS-RENSA-FAELT      TO MOD-KDPRODSL-IN                      
124300     ELSE                                                                 
124400        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDPRODSL-IN                      
124500        MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDPRODSL-IN-ATTR                 
124600     END-IF                                                               
124700                                                                          
124800     IF MID-KDSORT        = ALL '+'                                       
124900        MOVE MFS-RENSA-FAELT      TO MOD-KDSORT-IN                        
125000     ELSE                                                                 
125100        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDSORT-IN                        
125200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-IN-ATTR                   
125300     END-IF                                                               
125400                                                                          
125500     IF MID-IDPROENH (1)  = ALL '+'                                       
125600        MOVE MFS-RENSA-FAELT      TO MOD-IDPROENH-IN       (1)            
125700     ELSE                                                                 
125800        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPROENH-IN-ATTR  (1)            
125900        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROENH-IN       (1)            
126000     END-IF                                                               
126100                                                                          
126200     IF MID-IDPROENH (2)  = ALL '+'                                       
126300        MOVE MFS-RENSA-FAELT      TO MOD-IDPROENH-IN       (2)            
126400     ELSE                                                                 
126500        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPROENH-IN-ATTR  (2)            
126600        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROENH-IN       (2)            
126700     END-IF                                                               
126800                                                                          
126900     IF MID-IDPROENH (3)  = ALL '+'                                       
127000        MOVE MFS-RENSA-FAELT      TO MOD-IDPROENH-IN       (3)            
127100     ELSE                                                                 
127200        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPROENH-IN-ATTR  (3)            
127300        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROENH-IN       (3)            
127400     END-IF                                                               
127500                                                                          
127600                                                                          
127700     IF MID-KDUART        = ALL '+'                                       
127800        MOVE MFS-RENSA-FAELT      TO MOD-KDUART-IN                        
127900     ELSE                                                                 
128000        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDUART-IN                        
128100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDUART-IN-ATTR                   
128200     END-IF                                                               
128300                                                                          
128400     IF MID-IDPROJ        = ALL '+'                                       
128500        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJ-IN                        
128600     ELSE                                                                 
128700        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROJ-IN                        
128800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR                   
128900     END-IF                                                               
129000                                                                          
129100     IF MID-FLPISK        = ALL '+'                                       
129200        MOVE MFS-RENSA-FAELT      TO MOD-FLPISK-IN                        
129300     ELSE                                                                 
129400        MOVE MFS-ROER-EJ-FAELT    TO MOD-FLPISK-IN                        
129500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPISK-IN-ATTR                   
129600     END-IF                                                               
129700                                                                          
129800     IF MID-IDKAT (1)     = ALL '+'                                       
129900        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (1)               
130000     ELSE                                                                 
130100        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (1)               
130200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-IN-ATTR  (1)               
130300     END-IF                                                               
130400                                                                          
130500     IF MID-IDKAT (2)     = ALL '+'                                       
130600        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (2)               
130700     ELSE                                                                 
130800        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (2)               
130900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-IN-ATTR  (2)               
131000     END-IF                                                               
131100                                                                          
131200     IF MID-IDKAT (3)     = ALL '+'                                       
131300        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (3)               
131400     ELSE                                                                 
131500        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (3)               
131600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-IN-ATTR  (3)               
131700     END-IF                                                               
131800                                                                          
131900     IF MID-FLLSRDEL      = ALL '+'                                       
132000        MOVE MFS-RENSA-FAELT      TO MOD-FLLSRDEL-IN                      
132100     ELSE                                                                 
132200        MOVE MFS-ROER-EJ-FAELT    TO MOD-FLLSRDEL-IN                      
132300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLSRDEL-IN-ATTR                 
132400     END-IF                                                               
132500                                                                          
132600     IF MID-IDPROJK       = ALL '+'                                       
132700        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJK-IN                       
132800     ELSE                                                                 
132900        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROJK-IN                       
133000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR                  
133100     END-IF                                                               
133200                                                                          
133300     IF MID-KDBPSR        = ALL '+'                                       
133400        MOVE MFS-RENSA-FAELT      TO MOD-KDBPSR-IN                        
133500     ELSE                                                                 
133600        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDBPSR-IN                        
133700        MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDBPSR-IN-ATTR                   
133800     END-IF                                                               
133900                                                                          
134000     IF MID-TISOP         = ALL '+'                                       
134100        MOVE MFS-RENSA-FAELT      TO MOD-TISOP-IN                         
134200     ELSE                                                                 
134300        MOVE MFS-ROER-EJ-FAELT    TO MOD-TISOP-IN                         
134400        MOVE MFS-NUM-FAELT-RAETT  TO MOD-TISOP-IN-ATTR                    
134500     END-IF                                                               
134600                                                                          
134700     IF MID-IDFKNGRP      = ALL '+'                                       
134800        MOVE MFS-RENSA-FAELT      TO MOD-IDFKNGRP-IN                      
134900     ELSE                                                                 
135000        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDFKNGRP-IN                      
135100        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDFKNGRP-IN-ATTR                 
135200     END-IF                                                               
135300                                                                          
135400     IF MID-IDPROJUP      = ALL '+'                                       
135500        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJUP-IN                      
135600     ELSE                                                                 
135700        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROJUP-IN                      
135800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJUP-IN-ATTR                 
135900     END-IF                                                               
136000                                                                          
136100     IF MID-IDARTNR-MOTSV = ALL '+'                                       
136200        MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-MOTSV-IN                 
136300     ELSE                                                                 
136400        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDARTNR-MOTSV-IN                 
136500        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-MOTSV-IN-ATTR            
136600     END-IF                                                               
136700                                                                          
136800     IF MID-FLBYTES       = ALL '+'                                       
136900        MOVE MFS-RENSA-FAELT      TO MOD-FLBYTES-IN                       
137000     ELSE                                                                 
137100        MOVE MFS-ROER-EJ-FAELT    TO MOD-FLBYTES-IN                       
137200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBYTES-IN-ATTR                  
137300     END-IF                                                               
137400                                                                          
137500     IF MID-IDRITN        = ALL '+'                                       
137600        MOVE MFS-RENSA-FAELT      TO MOD-IDRITN-IN                        
137700     ELSE                                                                 
137800        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDRITN-IN                        
137900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-IN-ATTR                   
138000     END-IF                                                               
138100                                                                          
138200     IF MID-KVARTVAGN     = ALL '+'                                       
138300        MOVE MFS-RENSA-FAELT      TO MOD-KVARTVAGN-IN                     
138400     ELSE                                                                 
138500        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVARTVAGN-IN                     
138600        MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVARTVAGN-IN-ATTR                
138700     END-IF                                                               
138800                                                                          
138900     IF MID-IDAO  (1)     = ALL '+'                                       
139000        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (1)               
139100     ELSE                                                                 
139200        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (1)               
139300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (1)               
139400     END-IF                                                               
139500                                                                          
139600     IF MID-IDAO  (2)     = ALL '+'                                       
139700        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (2)               
139800     ELSE                                                                 
139900        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (2)               
140000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (2)               
140100     END-IF                                                               
140200                                                                          
140300     IF MID-IDAO  (3)     = ALL '+'                                       
140400        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (3)               
140500     ELSE                                                                 
140600        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (3)               
140700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (3)               
140800     END-IF                                                               
140900                                                                          
141000     IF MID-IDAO  (4)     = ALL '+'                                       
141100        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (4)               
141200     ELSE                                                                 
141300        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (4)               
141400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (4)               
141500     END-IF                                                               
141600                                                                          
141700     IF MID-IDAO  (5)     = ALL '+'                                       
141800        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (5)               
141900     ELSE                                                                 
142000        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (5)               
142100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (5)               
142200     END-IF                                                               
142300                                                                          
142400     IF MID-TEORSAK       = ALL '+'                                       
142500        CONTINUE                                                          
142600     ELSE                                                                 
142700        MOVE MFS-ROER-EJ-FAELT    TO MOD-TEORSAK-IN-UT                    
142800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEORSAK-IN-UT-ATTR               
142900     END-IF                                                               
143000                                                                          
143100     IF MID-KVPROG        = ALL '+'                                       
143200        CONTINUE                                                          
143300     ELSE                                                                 
143400        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPROG-IN                        
143500        MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVPROG-IN-ATTR                   
143600     END-IF                                                               
143700                                                                          
143800     IF MID-KDAGE         = ALL '+'                                       
143900        CONTINUE                                                          
144000     ELSE                                                                 
144100        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDAGE-IN                         
144200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAGE-IN-ATTR                    
144300     END-IF                                                               
144400                                                                          
144500     IF MID-TEVARNOT      = ALL '+'                                       
144600        CONTINUE                                                          
144700     ELSE                                                                 
144800        MOVE MFS-ROER-EJ-FAELT    TO MOD-TEVARNOT-IN-UT                   
144900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEVARNOT-IN-UT-ATTR              
145000     END-IF                                                               
145100     .                                                                    
145200     EJECT                                                                
145300 BG-KOLLA-KAMPANJ   SECTION.                                              
145400     SKIP2                                                                
145500     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
145600     IF SQLCODE-WS = ZERO                                                 
145700       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
145800     END-IF                                                               
145900                                                                          
146000     MOVE ZERO               TO WS-ANTAL-KAMP                             
146100     MOVE NEJ                TO WS-FLAGGA-Q-KAMP                          
146200                                WS-FLAGGA-W-S-KAMP                        
146300     PERFORM UNTIL SQLCODE > ZERO                                         
146400       IF TP1KAMP-TISTODAT-KAMP > ZERO                                    
146500         MOVE TP1KAMP-TISTODAT-KAMP                                       
146600                             TO WS-JMFR-AAAAMMDD                          
146700       ELSE                                                               
146800         MOVE TP1KAMP-TISTADAT-KAMP                                       
146900                             TO WS-JMFR-AAAAMMDD                          
147000       END-IF                                                             
147100       IF WS-JMFR-AA > 50                                                 
147200         MOVE 19             TO WS-JMFR-AAAAMMDD (1:2)                    
147300       ELSE                                                               
147400         MOVE 20             TO WS-JMFR-AAAAMMDD (1:2)                    
147500       END-IF                                                             
147600       IF TP1KAMP-TISTODAT-KAMP = ZERO                                    
147700*    LÄGG TILL 5 ÅR                                                       
147800         ADD 50000           TO WS-JMFR-AAAAMMDD                          
147900       END-IF                                                             
148000       IF WS-JMFR-AAAAMMDD >= WS-DAGENS-AAAAMMDD                          
148100         IF TP1KAMP-KDKAMP = 'Q'                                          
148200           MOVE JA           TO WS-FLAGGA-Q-KAMP                          
148300         END-IF                                                           
148400         IF TP1KAMP-KDKAMP = 'W'                                          
148500         OR TP1KAMP-KDKAMP = 'S'                                          
148600           MOVE JA           TO WS-FLAGGA-W-S-KAMP                        
148700         END-IF                                                           
148800       END-IF                                                             
148900       ADD 1                 TO WS-ANTAL-KAMP                             
149000       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
149100     END-PERFORM                                                          
149200                                                                          
149300     IF  WS-FLAGGA-Q-KAMP   = JA                                          
149400     AND WS-FLAGGA-W-S-KAMP = NEJ                                         
149500       MOVE MED-92 (SPAR-TEXT-IND) TO WS-TEMFSINF-KAMP                    
149600*            SM ETC                                                       
149700     ELSE                                                                 
149800       IF WS-FLAGGA-W-S-KAMP = JA                                         
149900       MOVE MED-91 (SPAR-TEXT-IND) TO WS-TEMFSINF-KAMP                    
150000*            CAMPAIGN                                                     
150100       END-IF                                                             
150200     END-IF                                                               
150300*    MOVE WS-ANTAL-KAMP      TO WS-TEMFSINF-KAMP                          
150400     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
150500     .                                                                    
150600     EJECT                                                                
150700 C-KOLLA-INPUT SECTION.                                                   
150800     SKIP2                                                                
150900     MOVE JA                      TO SW-INPUT-RAETT                       
151000                                                                          
151100     MOVE ART-IDLEVNR             TO SPAR-IDLEVNR                         
151200     MOVE ART-FLERS               TO SPAR-FLERS                           
151300     MOVE ART-FLIART              TO SPAR-FLIART                          
151400     MOVE ART-KDPRODSL            TO WS-KDPRODSL                          
151500                                     WS-A17-KDPRODSL                      
151600                                     TEST-KDPRODSL                        
151700     MOVE ART-KDSORT              TO WS-KDSORT-OLD                        
151800     MOVE ART-IDFKNGRP            TO WS-IDFKNGRP-OLD                      
151900                                                                          
152000     IF KDPRODSL-VOLVO-BIMA                                               
152100        IF CDC OR SDC                                                     
152200           CONTINUE                                                       
152300        ELSE                                                              
152400           MOVE NEJ TO SW-INPUT-RAETT                                     
152500           MOVE MED-8 (SPAR-TEXT-IND) TO MOD-TEMFSINF                     
152510           MOVE FEL-ERR-IDARTNR    TO WS-ERROR-UPDX                       
152600        END-IF                                                            
152700     END-IF                                                               
152800     MOVE ZERO TO WS-GAMMAL-ART                                           
152900     PERFORM CA-KOLLA-TISOP                                               
153000                                                                          
153100     PERFORM IMS-GHNP-ARTC11                                              
153200     MOVE CLAG-IDPROJ             TO SPAR-IDPROJ                          
153300     MOVE CLAG-PRARTSTD           TO SPAR-PRARTSTD                        
153400     MOVE CLAG-KDUART             TO SPAR-KDUART                          
153500     PERFORM CC-KOLLA-INPUT-2                                             
153600     MOVE TEST-KDPRODSL           TO WS-KDPRODSL-LEV                      
153700     PERFORM S11-CHECK-SUPPLIER                                           
153800                                                                          
153900     .                                                                    
154000     EJECT                                                                
154100 CA-KOLLA-TISOP   SECTION.                                                
154200                                                                          
154300     MOVE MID-TISOP               TO XX-TISOP                             
154400     MOVE '+'                     TO XX-DAG-SOP                           
154500     IF XX-TISOP = ALL '+'                                                
154600        MOVE MFS-RENSA-FAELT      TO MOD-TISOP-IN                         
154700        IF ART-TISOP = 99999                                              
154800           MOVE 99999999          TO SPAR-DASOP-AAAAMMDD                  
154900           MOVE 99999             TO SPAR-TISOP-AAVVD                     
155000        ELSE                                                              
155100           MOVE 'AAVVD'           TO DAT-KDDATFORM                        
155200           MOVE ART-TISOP         TO DAT-I-TIDATUM                        
155300                                     SPAR-TISOP-AAVVD                     
155400           PERFORM S99-WDATKONV                                           
155500           IF DAT-KDSVAR-OK                                               
155600              MOVE DAT-TIAAMMDD   TO SPAR-DASOP-AAAAMMDD                  
155700              MOVE DAT-TISEKEL    TO SPAR-DASOP-AAAAMMDD (1:2)            
155800                                                                          
155900              MOVE SPAR-DAGENS-DATUM   TO TMP1-YYMMDD                     
156000              MOVE SPAR-DASOP-AAAAMMDD (3:6) TO TMP2-YYMMDD               
156100              PERFORM WY2000P1                                            
156200              COMPUTE                                                     
156300                WS-GAMMAL-ART = TMP1-YYMMDD - TMP2-YYMMDD                 
156400           END-IF                                                         
156500        END-IF                                                            
156600     ELSE                                                                 
156700        MOVE 1                    TO XX-DAG-SOP                           
156800        MOVE XX-TISOP             TO SPAR-TISOP-AAVVD                     
156900        IF SPAR-TISOP-AAVVD = 99991                                       
157000           MOVE MFS-NUM-FAELT-RAETT                                       
157100                                  TO MOD-TISOP-IN-ATTR                    
157200           MOVE 99999999          TO SPAR-DASOP-AAAAMMDD                  
157300           MOVE 99999             TO SPAR-TISOP-AAVVD                     
157400        ELSE                                                              
157500           MOVE 'AAVVD '          TO DAT-KDDATFORM                        
157600           MOVE SPAR-TISOP-AAVVD  TO DAT-I-TIDATUM                        
157700           PERFORM S99-WDATKONV                                           
157800           IF DAT-KDSVAR-OK                                               
157900              MOVE DAT-TIAA-VECKA  TO SPAR-TISOP-AA                       
158000              MOVE DAT-TIVV        TO SPAR-TISOP-VV                       
158100              MOVE MFS-NUM-FAELT-RAETT                                    
158200                                   TO MOD-TISOP-IN-ATTR                   
158300              MOVE DAT-TIAAMMDD    TO SPAR-DASOP-AAAAMMDD                 
158400              MOVE DAT-TISEKEL     TO SPAR-DASOP-AAAAMMDD (1:2)           
158500                                                                          
158600              MOVE SPAR-DAGENS-DATUM         TO TMP1-YYMMDD               
158700              MOVE SPAR-DASOP-AAAAMMDD (3:6) TO TMP2-YYMMDD               
158800              PERFORM WY2000P1                                            
158900              COMPUTE                                                     
159000                 WS-GAMMAL-ART = TMP1-YYMMDD - TMP2-YYMMDD                
159100                                                                          
159200              MOVE SPAR-TISOP-AAVV-R  TO TMP1-YYWW                        
159300              MOVE SPAR-DAGENS-AAVV-R TO TMP2-YYWW                        
159400              PERFORM WY2000P1                                            
159500              COMPUTE TMP2-YYWW = TMP2-YYWW + 0200                        
159600                 IF TMP1-YYWW < TMP2-YYWW                                 
159700                    MOVE MFS-NUM-FAELT-RAETT                              
159800                                  TO MOD-TISOP-IN-ATTR                    
159900                 ELSE                                                     
160000                    MOVE MFS-NUM-FAELT-FEL                                
160100                                  TO MOD-TISOP-IN-ATTR                    
160200                    MOVE NEJ      TO SW-INPUT-RAETT                       
160200                    MOVE FEL-ERR-TISOP TO WS-ERROR-UPDX                   
160300                 END-IF                                                   
160400           ELSE                                                           
160500              MOVE MFS-NUM-FAELT-FEL                                      
160600                                  TO MOD-TISOP-IN-ATTR                    
160700              MOVE NEJ            TO SW-INPUT-RAETT                       
160200              MOVE FEL-ERR-TISOP  TO WS-ERROR-UPDX                        
160800           END-IF                                                         
160900        END-IF                                                            
161000        MOVE MFS-ROER-EJ-FAELT    TO MOD-TISOP-IN                         
161100     END-IF                                                               
161200     .                                                                    
161300     EJECT                                                                
161400 CC-KOLLA-INPUT-2 SECTION.                                                
161500     MOVE SPACE TO SPAR-IDPROENH-AREA                                     
161600     SET SPAR-IDPROENH-IND TO 1                                           
161700     PERFORM UNTIL SPAR-IDPROENH-IND > SPAR-IDPROENH-IND-MAX              
161800        MOVE CLAG-IDPROENH (SPAR-IDPROENH-IND)                            
161900           TO SPAR-IDPROENH (SPAR-IDPROENH-IND)                           
162000        SET SPAR-IDPROENH-IND UP BY 1                                     
162100     END-PERFORM                                                          
162200                                                                          
162300     IF MID-IDBERED = ALL '+'                                             
162400        MOVE MFS-RENSA-FAELT      TO MOD-IDBERED-IN                       
162500     ELSE                                                                 
162600        IF MID-IDBERED NUMERIC                                            
162700           IF MID-IDBERED > ZERO                                          
162800              MOVE MFS-NUM-FAELT-RAETT                                    
162900                                  TO MOD-IDBERED-IN-ATTR                  
163000           ELSE                                                           
163100              MOVE MFS-NUM-FAELT-FEL                                      
163200                                  TO MOD-IDBERED-IN-ATTR                  
163300              MOVE NEJ            TO SW-INPUT-RAETT                       
163310              MOVE FEL-ERR-IDBERED TO WS-ERROR-UPDX                       
163400           END-IF                                                         
163500        ELSE                                                              
163600           MOVE MFS-NUM-FAELT-FEL                                         
163700                                  TO MOD-IDBERED-IN-ATTR                  
163800           MOVE NEJ               TO SW-INPUT-RAETT                       
163810           MOVE FEL-ERR-IDBERED   TO WS-ERROR-UPDX                        
163900        END-IF                                                            
164000        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDBERED-IN                       
164100     END-IF                                                               
164200                                                                          
164300     MOVE ZERO                    TO SPAR-KVPROG                          
164400     IF MID-KVPROG = ALL '+'                                              
164500        MOVE MFS-RENSA-FAELT      TO MOD-KVPROG-IN                        
164600     ELSE                                                                 
164700        IF MID-KVPROG NUMERIC                                             
164800              MOVE MFS-NUM-FAELT-RAETT                                    
164900                                  TO MOD-KVPROG-IN-ATTR                   
165000              MOVE MID-KVPROG     TO SPAR-KVPROG                          
165100        ELSE                                                              
165200           MOVE MFS-NUM-FAELT-FEL                                         
165300                                  TO MOD-KVPROG-IN-ATTR                   
165400           MOVE NEJ               TO SW-INPUT-RAETT                       
165410           MOVE FEL-ERR-FIELD     TO WS-ERROR-UPDX                        
165500        END-IF                                                            
165600        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPROG-IN                        
165700     END-IF                                                               
165800                                                                          
165900                                                                          
166000     MOVE SPACE                   TO SPAR-KDAGE                           
166100     IF MID-KDAGE = ALL '+'                                               
166200        MOVE MFS-RENSA-FAELT      TO MOD-KDAGE-IN                         
166300     ELSE                                                                 
166400        MOVE MFS-ALFA-FAELT-RAETT                                         
166500                                  TO MOD-KDAGE-IN-ATTR                    
166600        MOVE MID-KDAGE            TO SPAR-KDAGE                           
166700        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDAGE-IN                         
166800     END-IF                                                               
166900                                                                          
167000     IF MID-KDPRODSL = ALL '+'                                            
167100        MOVE MFS-RENSA-FAELT      TO MOD-KDPRODSL-IN                      
167200        MOVE WS-KDPRODSL          TO TEST-KDPRODSL                        
167300                                     W-KDPRODSL1                          
167400     ELSE                                                                 
167500        IF MID-KDPRODSL NUMERIC                                           
167600           MOVE MID-KDPRODSL      TO TEST-KDPRODSL                        
167700                                     W-KDPRODSL1                          
167800           MOVE WS-KDPRODSL       TO WS-TEST-KDPRODSL                     
167900           IF GOOD-KDPRODSL                                               
168000              MOVE MFS-NUM-FAELT-RAETT                                    
168100                                  TO MOD-KDPRODSL-IN-ATTR                 
168200                                                                          
168300              MOVE 002            TO KPS-KDCALL                           
168400              MOVE MID-KDPRODSL   TO KPS-KDPRODSL                         
168500              CALL WKPSKONV    USING KPS-WKPSAREA                         
168600              IF KPS-KDSVAR = 'F'                                         
168700                 MOVE MFS-NUM-FAELT-FEL                                   
168800                                  TO MOD-KDPRODSL-IN-ATTR                 
168900                 MOVE NEJ         TO SW-INPUT-RAETT                       
168910                 MOVE FEL-ERR-KDPRODSL  TO WS-ERROR-UPDX                  
169000              END-IF                                                      
170000              IF (KDPRODSL-BIMA AND (NOT WS-KDPRODSL-BIMA)) OR            
170100                 (WS-KDPRODSL-BIMA AND (NOT KDPRODSL-BIMA))               
170200                 MOVE MFS-NUM-FAELT-FEL                                   
170300                                  TO MOD-KDPRODSL-IN-ATTR                 
170400                 MOVE NEJ         TO SW-INPUT-RAETT                       
170410                 MOVE FEL-ERR-KDPRODSL  TO WS-ERROR-UPDX                  
170500              END-IF                                                      
170600           ELSE                                                           
170700              MOVE MFS-NUM-FAELT-FEL                                      
170800                                  TO MOD-KDPRODSL-IN-ATTR                 
170900              MOVE NEJ            TO SW-INPUT-RAETT                       
170910              MOVE FEL-ERR-KDPRODSL  TO WS-ERROR-UPDX                     
171000           END-IF                                                         
171100        ELSE                                                              
171200           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                 
171300                                                                          
171400           MOVE NEJ               TO SW-INPUT-RAETT                       
171410           MOVE FEL-ERR-KDPRODSL  TO WS-ERROR-UPDX                        
171500        END-IF                                                            
171600        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDPRODSL-IN                      
171700     END-IF                                                               
171800                                                                          
171900     IF MID-KDSORT = ALL '+'                                              
172000        MOVE MFS-RENSA-FAELT      TO MOD-KDSORT-IN                        
172100     ELSE                                                                 
172200        MOVE MID-KDSORT TO WS-KDSORT-GODK                                 
172300        IF KDSORT-GODK                                                    
172400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-IN-ATTR                
172500        ELSE                                                              
172600           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR                  
172700           MOVE NEJ TO SW-INPUT-RAETT                                     
172710           MOVE FEL-ERR-KDSORT    TO WS-ERROR-UPDX                        
172800        END-IF                                                            
172900        MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT-IN                           
173000     END-IF                                                               
173100                                                                          
173200     IF MID-IDPROENH (1)  = ALL '+'                                       
173300        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-IN(1)                        
173400     ELSE                                                                 
173500        IF MID-IDPROENH(1) NUMERIC                                        
173600           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-IN-ATTR (1)           
173700        ELSE                                                              
173800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPROENH-IN-ATTR (1)             
173900           MOVE NEJ TO SW-INPUT-RAETT                                     
173910           MOVE FEL-ERR-IDPROENH  TO WS-ERROR-UPDX                        
174000        END-IF                                                            
174100        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROENH-IN(1)                      
174200     END-IF                                                               
174300                                                                          
174400     IF MID-IDPROENH(2) = ALL '+'                                         
174500        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-IN(2)                        
174600     ELSE                                                                 
174700        IF MID-IDPROENH (2) NUMERIC                                       
174800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-IN-ATTR(2)            
174900        ELSE                                                              
175000           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPROENH-IN-ATTR(2)              
175100           MOVE NEJ TO SW-INPUT-RAETT                                     
175110           MOVE FEL-ERR-IDPROENH  TO WS-ERROR-UPDX                        
175200        END-IF                                                            
175300        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROENH-IN(2)                      
175400     END-IF                                                               
175500                                                                          
175600     IF MID-IDPROENH(3) = ALL '+'                                         
175700        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-IN(3)                        
175800     ELSE                                                                 
175900        IF MID-IDPROENH(3) NUMERIC                                        
176000           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-IN-ATTR(3)            
176100        ELSE                                                              
176200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPROENH-IN-ATTR(3)              
176300           MOVE NEJ TO SW-INPUT-RAETT                                     
176310           MOVE FEL-ERR-IDPROENH  TO WS-ERROR-UPDX                        
176400        END-IF                                                            
176500        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROENH-IN(3)                      
176600     END-IF                                                               
176700                                                                          
176800     IF MID-KDUART = ALL '+'                                              
176900        MOVE MFS-RENSA-FAELT TO MOD-KDUART-IN                             
177000     ELSE                                                                 
177100        MOVE MID-KDUART TO WS-KDUART-GODK                                 
177200                                                                          
177300        IF MID-KDUART = ' '                                               
177400            IF SPAR-KDUART = 'L'                                          
177500                MOVE '*'          TO WS-KDUART-GODK                       
177600****    FÖR ATT ANGE DENNA KOMBINATION MED KDUART EJ ÄR GODKÄND           
177700            END-IF                                                        
177800        END-IF                                                            
177900                                                                          
178000        IF KDUART-GODK                                                    
178100           IF WS-KDUART-GODK = 'P' AND SPAR-FLIART = JA                   
178200              MOVE MFS-ALFA-FAELT-FEL                                     
178300                                     TO MOD-KDUART-IN-ATTR                
178400              MOVE NEJ               TO SW-INPUT-RAETT                    
178410              MOVE FEL-ERR-KDUART    TO WS-ERROR-UPDX                     
178500              MOVE MED-7 (SPAR-TEXT-IND)   TO MOD-TEMFSINF                
178600           ELSE                                                           
178700              MOVE MFS-ALFA-FAELT-RAETT                                   
178800                                     TO MOD-KDUART-IN-ATTR                
178900           END-IF                                                         
179000        ELSE                                                              
179100           MOVE MFS-ALFA-FAELT-FEL                                        
179200                                  TO MOD-KDUART-IN-ATTR                   
179300           MOVE NEJ               TO SW-INPUT-RAETT                       
179310           MOVE FEL-ERR-KDUART    TO WS-ERROR-UPDX                        
179400        END-IF                                                            
179500        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDUART-IN                        
179600     END-IF                                                               
179700                                                                          
179800     IF MID-FLPISK = ALL '+'                                              
179900        MOVE MFS-RENSA-FAELT      TO MOD-FLPISK-IN                        
180000     ELSE                                                                 
180100        IF MID-FLPISK = JA OR NEJ                                         
180200           MOVE MFS-ALFA-FAELT-RAETT                                      
180300                                  TO MOD-FLPISK-IN-ATTR                   
180400           MOVE MID-FLPISK        TO SPAR-FLPISK                          
180500        ELSE                                                              
180600           MOVE MFS-ALFA-FAELT-FEL                                        
180700                                  TO MOD-FLPISK-IN-ATTR                   
180800           MOVE NEJ               TO SW-INPUT-RAETT                       
180810           MOVE FEL-ERR-FLPISK    TO WS-ERROR-UPDX                        
180900        END-IF                                                            
181000        MOVE MFS-ROER-EJ-FAELT    TO MOD-FLPISK-IN                        
181100     END-IF                                                               
181200                                                                          
181300     IF MID-IDKAT (1) = ALL '+'                                           
181400        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (1)               
181500     ELSE                                                                 
181600        MOVE MFS-ALFA-FAELT-RAETT                                         
181700                                  TO MOD-IDKAT-IN-ATTR  (1)               
181800        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (1)               
181900     END-IF                                                               
182000                                                                          
182100     IF MID-IDKAT (2) = ALL '+'                                           
182200        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (2)               
182300     ELSE                                                                 
182400        MOVE MFS-ALFA-FAELT-RAETT                                         
182500                                  TO MOD-IDKAT-IN-ATTR  (2)               
182600        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (2)               
182700     END-IF                                                               
182800                                                                          
182900     IF MID-IDKAT (3) = ALL '+'                                           
183000        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (3)               
183100     ELSE                                                                 
183200        MOVE MFS-ALFA-FAELT-RAETT                                         
183300                                  TO MOD-IDKAT-IN-ATTR  (3)               
183400        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (3)               
183500     END-IF                                                               
183600                                                                          
183700     IF MID-FLLSRDEL = ALL '+'                                            
183800        MOVE MFS-RENSA-FAELT      TO MOD-FLLSRDEL-IN                      
183900     ELSE                                                                 
184000        IF MID-FLLSRDEL = JA OR NEJ                                       
184100           MOVE MFS-ALFA-FAELT-RAETT                                      
184200                                  TO MOD-FLLSRDEL-IN-ATTR                 
184300        ELSE                                                              
184400           MOVE MFS-ALFA-FAELT-FEL                                        
184500                                  TO MOD-FLLSRDEL-IN-ATTR                 
184600           MOVE NEJ               TO SW-INPUT-RAETT                       
184610           MOVE FEL-ERR-FLLSRDEL  TO WS-ERROR-UPDX                        
184700        END-IF                                                            
184800        MOVE MFS-ROER-EJ-FAELT    TO MOD-FLLSRDEL-IN                      
184900     END-IF                                                               
185000                                                                          
185100     IF MID-KDBPSR = ALL '+'                                              
185200        MOVE MFS-RENSA-FAELT      TO MOD-KDBPSR-IN                        
185300     ELSE                                                                 
185400        MOVE MID-KDBPSR           TO WS-KDBPSR-GODK                       
185500        IF KDBPSR-GODK                                                    
185600           MOVE MFS-NUM-FAELT-RAETT                                       
185700                                  TO MOD-KDBPSR-IN-ATTR                   
185800        ELSE                                                              
185900           MOVE MFS-NUM-FAELT-FEL                                         
186000                                  TO MOD-KDBPSR-IN-ATTR                   
186100           MOVE NEJ               TO SW-INPUT-RAETT                       
186110           MOVE FEL-ERR-KDBPSR    TO WS-ERROR-UPDX                        
186200        END-IF                                                            
186300        MOVE MFS-ROER-EJ-FAELT    TO MOD-KDBPSR-IN                        
186400     END-IF                                                               
186500                                                                          
186600*    ************************************************                     
186700                                                                          
186800     IF MID-IDPROJK = ALL '+'                                             
186900        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJK-IN                       
187000        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
187100           MOVE NYPON-ART-IDPROJK TO SPAR-IDPROJK                         
187200        ELSE                                                              
187300           MOVE SPACE             TO SPAR-IDPROJK                         
187400        END-IF                                                            
187500     ELSE                                                                 
187600        MOVE MID-IDPROJK       TO SPAR-IDPROJK                            
187700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR                  
187800        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROJK-IN                          
187900     END-IF                                                               
188000                                                                          
188100                                                                          
188200                                                                          
188300     IF MID-IDPROJ = ALL '+'                                              
188400        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJ-IN                        
188500*       *******************************************    *                  
188600**      ARTC11-IDPROJ FINNS INFLYTTAT I SPAR-IDPROJ   **                  
188700*       *******************************************    *                  
188800     ELSE                                                                 
188900        MOVE MID-IDPROJ              TO SPAR-IDPROJ                       
189000        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDPROJ-IN                     
189100     END-IF                                                               
189200                                                                          
189300     IF WS-GAMMAL-ART > WS-TVA-AAR                                        
189400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR                   
189500     ELSE                                                                 
189600        IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV OR KDPRODSL-LOCAL           
189700           PERFORM CCA-KTR-PROJ-GODK                                      
189800           IF PROJ-GODK                                                   
189900              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR             
190000           ELSE                                                           
190100              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-IN-ATTR               
190200              MOVE NEJ  TO SW-INPUT-RAETT                                 
190210              MOVE FEL-ERR-IDPROJ    TO WS-ERROR-UPDX                     
190300           END-IF                                                         
190400        ELSE                                                              
190500           IF KDPRODSL-EMB                                                
190600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR             
190700           ELSE                                                           
190800              IF SPAR-IDPROJ = SPACE                                      
190900                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-IN-ATTR            
191000                 MOVE NEJ TO SW-INPUT-RAETT                               
191010                 MOVE FEL-ERR-IDPROJ    TO WS-ERROR-UPDX                  
191100              ELSE                                                        
191200                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR          
191300              END-IF                                                      
191400           END-IF                                                         
191500        END-IF                                                            
191600     END-IF                                                               
191700                                                                          
191800     IF MID-IDFKNGRP = ALL '+'                                            
191900        MOVE MFS-RENSA-FAELT      TO MOD-IDFKNGRP-IN                      
192000     ELSE                                                                 
192100        IF MID-IDFKNGRP NUMERIC                                           
192200           IF MID-IDFKNGRP > ZERO                                         
192300              MOVE MFS-NUM-FAELT-RAETT                                    
192400                                  TO MOD-IDFKNGRP-IN-ATTR                 
192500           ELSE                                                           
192600              MOVE MFS-NUM-FAELT-FEL                                      
192700                                  TO MOD-IDFKNGRP-IN-ATTR                 
192800              MOVE NEJ            TO SW-INPUT-RAETT                       
192810              MOVE FEL-ERR-IDFKNGRP  TO WS-ERROR-UPDX                     
192900           END-IF                                                         
193000        ELSE                                                              
193100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR                 
193200           MOVE NEJ               TO SW-INPUT-RAETT                       
193210           MOVE FEL-ERR-IDFKNGRP  TO WS-ERROR-UPDX                        
193300        END-IF                                                            
193400        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDFKNGRP-IN                      
193500     END-IF                                                               
193600                                                                          
193700     IF MID-IDPROJUP = ALL '+'                                            
193800        MOVE MFS-RENSA-FAELT      TO MOD-IDPROJUP-IN                      
193900     ELSE                                                                 
194000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJUP-IN-ATTR                 
194100        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROJUP-IN                      
194200     END-IF                                                               
194300                                                                          
194400     IF MID-IDARTNR-MOTSV = ALL '+'                                       
194500        MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-MOTSV-IN                 
194600     ELSE                                                                 
194700        IF MID-IDARTNR-MOTSV NUMERIC                                      
194800           MOVE MFS-NUM-FAELT-RAETT                                       
194900                                  TO MOD-IDARTNR-MOTSV-IN-ATTR            
195000           MOVE MID-IDARTNR-MOTSV TO SPAR-IDARTNR-MOTSV                   
195100        ELSE                                                              
195200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-MOTSV-IN-ATTR            
195300           MOVE NEJ               TO SW-INPUT-RAETT                       
195310           MOVE FEL-ERR-IDARTNR   TO WS-ERROR-UPDX                        
195400        END-IF                                                            
195500        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDARTNR-MOTSV-IN                 
195600     END-IF                                                               
195700                                                                          
195800     IF MID-FLBYTES = ALL '+'                                             
195900        MOVE MFS-RENSA-FAELT      TO MOD-FLBYTES-IN                       
196000     ELSE                                                                 
196100        IF MID-FLBYTES = JA OR NEJ                                        
196200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBYTES-IN-ATTR               
196300           MOVE MID-FLBYTES       TO SPAR-FLBYTES                         
196400        ELSE                                                              
196500           MOVE MFS-ALFA-FAELT-FEL                                        
196600                                   TO MOD-FLBYTES-IN-ATTR                 
196700           MOVE NEJ               TO SW-INPUT-RAETT                       
196710           MOVE FEL-ERR-FLBYTES   TO WS-ERROR-UPDX                        
196800        END-IF                                                            
196900        MOVE MFS-ROER-EJ-FAELT    TO MOD-FLBYTES-IN                       
197000     END-IF                                                               
197100                                                                          
197200     IF MID-IDRITN = ALL '+'                                              
197300        MOVE MFS-RENSA-FAELT      TO MOD-IDRITN-IN                        
197400     ELSE                                                                 
197500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-IN-ATTR                   
197600        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDRITN-IN                        
197700     END-IF                                                               
197800                                                                          
197900     IF MID-KVARTVAGN = ALL '+'                                           
198000        MOVE MFS-RENSA-FAELT      TO MOD-KVARTVAGN-IN                     
198100     ELSE                                                                 
198200        IF MID-KVARTVAGN NUMERIC                                          
198300           MOVE MFS-NUM-FAELT-RAETT                                       
198400                                  TO MOD-KVARTVAGN-IN-ATTR                
198500        ELSE                                                              
198600           MOVE MFS-NUM-FAELT-FEL                                         
198700                                  TO MOD-KVARTVAGN-IN-ATTR                
198800           MOVE NEJ               TO SW-INPUT-RAETT                       
198810           MOVE FEL-ERR-KVARTVAGN TO WS-ERROR-UPDX                        
198900        END-IF                                                            
199000        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVARTVAGN-IN                     
199100     END-IF                                                               
199200                                                                          
199300     IF MID-IDAO (1) = ALL '+'                                            
199400        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (1)               
199500     ELSE                                                                 
199600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (1)               
199700        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (1)               
199800     END-IF                                                               
199900                                                                          
200000     IF MID-IDAO (2) = ALL '+'                                            
200100        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (2)               
200200     ELSE                                                                 
200300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (2)               
200400        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (2)               
200500     END-IF                                                               
200600                                                                          
200700     IF MID-IDAO (3) = ALL '+'                                            
200800        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (3)               
200900     ELSE                                                                 
201000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (3)               
201100        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (3)               
201200     END-IF                                                               
201300                                                                          
201400     IF MID-IDAO (4) = ALL '+'                                            
201500        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (4)               
201600     ELSE                                                                 
201700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (4)               
201800        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (4)               
201900     END-IF                                                               
202000                                                                          
202100     IF MID-IDAO (5) = ALL '+'                                            
202200        MOVE MFS-RENSA-FAELT      TO MOD-IDAO-IN        (5)               
202300     ELSE                                                                 
202400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (5)               
202500        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDAO-IN        (5)               
202600     END-IF                                                               
202700                                                                          
202800     IF MID-TEORSAK = ALL '+'                                             
202900        MOVE MFS-RENSA-FAELT      TO MOD-TEORSAK-IN-UT                    
203000     ELSE                                                                 
203100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEORSAK-IN-UT-ATTR               
203200        MOVE MFS-ROER-EJ-FAELT    TO MOD-TEORSAK-IN-UT                    
203300        MOVE MID-TEORSAK          TO SPAR-TEORSAK                         
203400     END-IF                                                               
203500                                                                          
203600     IF MID-TEVARNOT = ALL '+'                                            
203700        MOVE MFS-RENSA-FAELT      TO MOD-TEVARNOT-IN-UT                   
203800     ELSE                                                                 
203900        IF MID-KDPRODSL = ALL '+'                                         
204000           IF KDPRODSL-VOLVO-EMB                                          
204100              MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEVARNOT-IN-UT-ATTR        
204200           ELSE                                                           
204300              MOVE MFS-ALFA-FAELT-FEL                                     
204400                                     TO MOD-TEVARNOT-IN-UT-ATTR           
204500              MOVE NEJ               TO SW-INPUT-RAETT                    
204510              MOVE FEL-ERR-TEVARNOT  TO WS-ERROR-UPDX                     
204600           END-IF                                                         
204700           MOVE MFS-ROER-EJ-FAELT    TO MOD-TEVARNOT-IN-UT                
204800        ELSE                                                              
204900           IF MID-KDPRODSL NUMERIC                                        
205000              IF KDPRODSL-VOLVO-EMB                                       
205100                 MOVE MFS-ALFA-FAELT-RAETT                                
205200                     TO MOD-TEVARNOT-IN-UT-ATTR                           
205300              ELSE                                                        
205400                 MOVE MFS-ALFA-FAELT-FEL                                  
205500                                        TO MOD-TEVARNOT-IN-UT-ATTR        
205600                 MOVE NEJ               TO SW-INPUT-RAETT                 
205610                 MOVE FEL-ERR-TEVARNOT  TO WS-ERROR-UPDX                  
205700              END-IF                                                      
205800           END-IF                                                         
205900           MOVE MFS-ROER-EJ-FAELT    TO MOD-TEVARNOT-IN-UT                
206000        END-IF                                                            
206100     END-IF                                                               
206200                                                                          
206300     IF SW-INPUT-RAETT = JA                                               
206400        IF MID-KDSORT = ALL '+'                                           
206500        AND MID-IDFKNGRP = ALL '+'                                        
206600           CONTINUE                                                       
206700        ELSE                                                              
206800           PERFORM CCB-KOLLA-SOFTWARE                                     
206900        END-IF                                                            
207000     END-IF                                                               
207100                                                                          
207200     IF SW-INPUT-RAETT = JA                                               
207300        CONTINUE                                                          
207400     ELSE                                                                 
207500        PERFORM S02-ROER-EJ-VISADE-FAELT                                  
207600     END-IF                                                               
207700     .                                                                    
207800     EJECT                                                                
207900 CCA-KTR-PROJ-GODK SECTION.                                               
208000     SKIP2                                                                
208100     MOVE NEJ TO SW-PROJ-GODK                                             
208200                                                                          
208300     PERFORM IMS-GU-WLXXAQ01-UNIK                                         
208400                                                                          
208500     IF SEGMENT-FINNS                                                     
208600        MOVE SPAR-IDPROJ TO W-IDPROJ                                      
208700        PERFORM IMS-GNP-WLXXAQ11-PROJ                                     
208800        IF SEGMENT-FINNS                                                  
208900           MOVE JA TO SW-PROJ-GODK                                        
209000        END-IF                                                            
209100     END-IF                                                               
209200     .                                                                    
209300     EJECT                                                                
209400 CCB-KOLLA-SOFTWARE SECTION.                                              
209500                                                                          
209600     IF MID-IDFKNGRP = ALL '+'                                            
209700        MOVE WS-IDFKNGRP-OLD TO WS-TEST-IDFKNGRP                          
209800     ELSE                                                                 
209900        MOVE MID-IDFKNGRP    TO WS-TEST-IDFKNGRP                          
210000     END-IF                                                               
210100                                                                          
210200     IF MID-KDSORT = ALL '+'                                              
210300        CONTINUE                                                          
210400     ELSE                                                                 
210500        IF WS-KDSORT-GODK = 'SW'                                          
210600           IF WS-SISTA-SIFFRAN = 8                                        
210700              CONTINUE                                                    
210800           ELSE                                                           
210900              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR               
211000              MOVE NEJ TO SW-INPUT-RAETT                                  
211010              MOVE FEL-ERR-KDSORT    TO WS-ERROR-UPDX                     
211100           END-IF                                                         
211200        ELSE                                                              
211300           IF WS-SISTA-SIFFRAN = 8                                        
211400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR               
211500              MOVE NEJ TO SW-INPUT-RAETT                                  
211510              MOVE FEL-ERR-KDSORT    TO WS-ERROR-UPDX                     
211600           END-IF                                                         
211700        END-IF                                                            
211800     END-IF                                                               
211900                                                                          
212000     IF MID-IDFKNGRP = ALL '+'                                            
212100        CONTINUE                                                          
212200     ELSE                                                                 
212300        IF WS-SISTA-SIFFRAN = 8                                           
212400           IF MID-KDSORT = ALL '+'                                        
212500              IF WS-KDSORT-OLD = 'SW'                                     
212600                 CONTINUE                                                 
212700              ELSE                                                        
212800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR           
212900                 MOVE NEJ TO SW-INPUT-RAETT                               
212910                 MOVE FEL-ERR-IDFKNGRP  TO WS-ERROR-UPDX                  
213000              END-IF                                                      
213100           ELSE                                                           
213200              IF WS-KDSORT-GODK = 'SW'                                    
213300                 CONTINUE                                                 
213400              ELSE                                                        
213500                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR           
213600                 MOVE NEJ TO SW-INPUT-RAETT                               
213610                 MOVE FEL-ERR-KDSORT    TO WS-ERROR-UPDX                  
213700              END-IF                                                      
213800           END-IF                                                         
213900        ELSE                                                              
214000           IF MID-KDSORT = ALL '+'                                        
214100              IF WS-KDSORT-OLD = 'SW'                                     
214200                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR           
214300                 MOVE NEJ TO SW-INPUT-RAETT                               
214310                 MOVE FEL-ERR-KDSORT    TO WS-ERROR-UPDX                  
214400              END-IF                                                      
214500           ELSE                                                           
214600              IF WS-KDSORT-GODK = 'SW'                                    
214700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR           
214800                 MOVE NEJ TO SW-INPUT-RAETT                               
214810                 MOVE FEL-ERR-KDSORT    TO WS-ERROR-UPDX                  
214900              END-IF                                                      
215000           END-IF                                                         
215100        END-IF                                                            
215200     END-IF                                                               
215300     .                                                                    
215400     EJECT                                                                
215500                                                                          
215600*           *********************************************                 
215700 D-RELATIONSKOLL SECTION.                                                 
215800     SKIP2                                                                
215900     IF SPAR-DASOP-AAAAMMDD = 99999999                                    
216000        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
216100           PERFORM DA-TESTA-RSUNIK-TIKOART                                
216200        ELSE                                                              
216300           MOVE MFS-NUM-FAELT-FEL   TO MOD-TISOP-IN-ATTR                  
216400           MOVE NEJ                 TO SW-INPUT-RAETT                     
216400           MOVE FEL-ERR-FIELD       TO WS-ERROR-UPDX                      
216500        END-IF                                                            
216600     END-IF                                                               
216700                                                                          
216800     PERFORM DB-TESTA-NYPON-FAELT                                         
216900                                                                          
217000     IF SW-INPUT-RAETT = JA                                               
217100        CONTINUE                                                          
217200     ELSE                                                                 
217300        PERFORM S02-ROER-EJ-VISADE-FAELT                                  
217400     END-IF                                                               
217500     .                                                                    
217600     EJECT                                                                
217700 DA-TESTA-RSUNIK-TIKOART SECTION.                                         
217800     SKIP2                                                                
217900     IF NYPON-ART-TIREGDAT = ZERO                                         
218000*       RS-UNIKT REGISTRERAD                                              
218100        MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-IN-ATTR                       
218200        MOVE NEJ               TO SW-INPUT-RAETT                          
218210        MOVE FEL-ERR-FIELD     TO WS-ERROR-UPDX                           
218300*       TISOP FÅR EJ VARA 999999, VI MÅSTE VETA                           
218400*       VILKEN TISOP ARTIKELN SKALL HA.....                               
218500     ELSE                                                                 
218600*       TIKO-ARTIKEL                                                      
218700        IF NYPON-ART-IDLEVNR    = '9998 '   OR                            
218800           NYPON-ART-FLUNIKRD   = JA     OR                               
218900           NYPON-ART-KDPRODSL   = 15                                      
219000           MOVE MFS-NUM-FAELT-FEL                                         
219100                               TO MOD-TISOP-IN-ATTR                       
219200           MOVE NEJ            TO SW-INPUT-RAETT                          
219210           MOVE FEL-ERR-FIELD  TO WS-ERROR-UPDX                           
219300*          TISOP FÅR EJ VARA 999999,VI FÅR ALDRIG                         
219400*          TISOP FRÅN TIKO......                                          
219500        ELSE                                                              
219600           PERFORM DAA-XXAUIDPROJK-TISERLEV                               
219700        END-IF                                                            
219800     END-IF                                                               
219900     .                                                                    
220000     EJECT                                                                
220100 DAA-XXAUIDPROJK-TISERLEV SECTION.                                        
220200     SKIP2                                                                
220300     PERFORM IMS-GHU-ARTC01                                               
220400     MOVE ART-KDPRODSL            TO SPAR-KDPRODSL                        
220500     MOVE SPAR-KDPRODSL           TO W-KDPRODSL1                          
220600     PERFORM IMS-GU-WLXXAQ01-UNIK                                         
220700     IF SEGMENT-FINNS                                                     
220800        MOVE SPAR-IDPROJ          TO W-IDPROJ                             
220900        MOVE SPAR-IDPROJOBJ       TO W-IDPROJOBJ                          
221000        MOVE SPAR-IDPROJK         TO W-IDPROJK                            
221100        PERFORM IMS-GNP-WLXXAQ11-UNIK                                     
221200        IF SEGMENT-FINNS                                                  
221300           IF XXAQ-1132-TIPRODSTA = 111111                                
221400*             LÖPANDE IDPROJK                                             
221500              PERFORM DAAA-TESTA-TISERLEV                                 
221600              IF SW-TISERLEV-FINNS = JA                                   
221700*                ART HAR SERIELEVERANSVECKA, 999999 EJ TILLÅTET           
221800                 MOVE MFS-NUM-FAELT-FEL                                   
221900                                     TO MOD-TISOP-IN-ATTR                 
222000                 MOVE NEJ            TO SW-INPUT-RAETT                    
222010                 MOVE FEL-ERR-TISOP   TO WS-ERROR-UPDX                    
222100              END-IF                                                      
222200           ELSE                                                           
222300              MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-IN-ATTR                 
222400              MOVE NEJ               TO SW-INPUT-RAETT                    
222410              MOVE FEL-ERR-TISOP     TO WS-ERROR-UPDX                     
222500           END-IF                                                         
222600        ELSE                                                              
222700           MOVE MFS-NUM-FAELT-FEL    TO MOD-TISOP-IN-ATTR                 
222800           MOVE NEJ                  TO SW-INPUT-RAETT                    
222810           MOVE FEL-ERR-TISOP        TO WS-ERROR-UPDX                     
222900        END-IF                                                            
223000     ELSE                                                                 
223100        MOVE MFS-NUM-FAELT-FEL       TO MOD-TISOP-IN-ATTR                 
223200        MOVE NEJ                     TO SW-INPUT-RAETT                    
223210        MOVE FEL-ERR-TISOP           TO WS-ERROR-UPDX                     
223300     END-IF                                                               
223400     .                                                                    
223500     EJECT                                                                
223600 DAAA-TESTA-TISERLEV SECTION.                                             
223700     SKIP2                                                                
223800     MOVE 1             TO SPAR-TISERLEV-IND                              
223900                                                                          
224000     PERFORM UNTIL SPAR-TISERLEV-IND > SPAR-TISERLEV-IND-MAX              
224100             OR    SW-TISERLEV-FINNS = JA                                 
224200        IF NYPON-ART-TISERLEV (SPAR-TISERLEV-IND) = ZERO                  
224300           CONTINUE                                                       
224400        ELSE                                                              
224500           MOVE JA      TO SW-TISERLEV-FINNS                              
224600        END-IF                                                            
224700                                                                          
224800        ADD 1           TO SPAR-TISERLEV-IND                              
224900                                                                          
225000     END-PERFORM                                                          
225100     .                                                                    
225200     EJECT                                                                
225300 DB-TESTA-NYPON-FAELT SECTION.                                            
225400     SKIP2                                                                
225500     IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                    
225600        IF  WS-GAMMAL-ART > WS-TVA-AAR                                    
225700            MOVE MFS-ALFA-FAELT-RAETT                                     
225800                 TO MOD-IDPROJK-IN-ATTR                                   
225900        ELSE                                                              
226000           PERFORM DBA-KTR-GODK-XXAQPROJK                                 
226100        END-IF                                                            
226200     ELSE                                                                 
226300        IF SPAR-PRARTSTD = ZERO                                           
226400           IF  WS-GAMMAL-ART > WS-TVA-AAR                                 
226500               MOVE MFS-ALFA-FAELT-RAETT                                  
226600                    TO MOD-IDPROJK-IN-ATTR                                
226700           ELSE                                                           
226800              PERFORM DBA-KTR-GODK-XXAQPROJK                              
226900           END-IF                                                         
227000        ELSE                                                              
227100           IF MID-IDPROJK       = ALL '+'   AND                           
227200              MID-FLPISK        = ALL '+'   AND                           
227300              MID-IDARTNR-MOTSV = ALL '+'   AND                           
227400              MID-FLBYTES       = ALL '+'   AND                           
227500              MID-KVARTVAGN     = ALL '+'   AND                           
227600              MID-TEORSAK       = ALL '+'   AND                           
227700              MID-KVPROG        = ALL '+'                                 
227800              CONTINUE                                                    
227900           ELSE                                                           
228000*          ARTIKEL FINNS INTE PÅ NYPON, ISRT KOMMER INTE ATT SKE          
228100*          ÄNDÅ HAR MAN MATAT IN PÅ FÄLT SOM BARA FINNS PÅ NYPON          
228200              IF MID-IDPROJK = ALL '+'                                    
228300                 CONTINUE                                                 
228400              ELSE                                                        
228500                 MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDPROJK-IN-ATTR          
228600                 MOVE NEJ                 TO SW-INPUT-RAETT               
228610                 MOVE FEL-ERR-IDPROJK     TO WS-ERROR-UPDX                
228700              END-IF                                                      
228800                                                                          
228900              IF MID-FLPISK = ALL '+'                                     
229000                 CONTINUE                                                 
229100              ELSE                                                        
229200                 MOVE MFS-ALFA-FAELT-FEL  TO  MOD-FLPISK-IN-ATTR          
229300                 MOVE NEJ                 TO SW-INPUT-RAETT               
229310                 MOVE FEL-ERR-FLPISK      TO WS-ERROR-UPDX                
229400              END-IF                                                      
229500                                                                          
229600              IF MID-IDARTNR-MOTSV = ALL '+'                              
229700                 CONTINUE                                                 
229800              ELSE                                                        
229900                 MOVE MFS-NUM-FAELT-FEL   TO                              
230000                                   MOD-IDARTNR-MOTSV-IN-ATTR              
230100                 MOVE NEJ                 TO SW-INPUT-RAETT               
230110                 MOVE FEL-ERR-IDARTNR     TO WS-ERROR-UPDX                
230200              END-IF                                                      
230300                                                                          
230400              IF MID-FLBYTES       = ALL '+'                              
230500                 CONTINUE                                                 
230600              ELSE                                                        
230700                 MOVE MFS-ALFA-FAELT-FEL  TO  MOD-FLBYTES-IN-ATTR         
230800                 MOVE NEJ                 TO SW-INPUT-RAETT               
230810                 MOVE FEL-ERR-FLBYTES     TO WS-ERROR-UPDX                
230900              END-IF                                                      
231000                                                                          
231100              IF MID-KVARTVAGN     = ALL '+'                              
231200                 CONTINUE                                                 
231300              ELSE                                                        
231400                 MOVE MFS-NUM-FAELT-FEL   TO MOD-KVARTVAGN-IN-ATTR        
231500                 MOVE NEJ                 TO SW-INPUT-RAETT               
231510                 MOVE FEL-ERR-KVARTVAGN   TO WS-ERROR-UPDX                
231600              END-IF                                                      
231700                                                                          
231800              IF MID-TEORSAK       = ALL '+'                              
231900                 CONTINUE                                                 
232000              ELSE                                                        
232100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-TEORSAK-IN-UT-ATTR        
232200                 MOVE NEJ                TO SW-INPUT-RAETT                
232210                 MOVE FEL-ERR-TEORSAK-1   TO WS-ERROR-UPDX                
232300              END-IF                                                      
232400                                                                          
232500              IF MID-KVPROG        = ALL '+'                              
232600                 CONTINUE                                                 
232700              ELSE                                                        
232800                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVPROG-IN-ATTR             
232900                 MOVE NEJ                TO SW-INPUT-RAETT                
232910                 MOVE FEL-ERR-KVPROG      TO WS-ERROR-UPDX                
233000              END-IF                                                      
233100           END-IF                                                         
233200        END-IF                                                            
233300     END-IF.                                                              
233400     EJECT                                                                
233500 DBA-KTR-GODK-XXAQPROJK SECTION.                                          
233600     SKIP2                                                                
233700******************************************************************        
233800***                                                                       
233900*** HÄR KONTROLLERAS ATT PROJK ÄR UPPLAGT PÅ BILD 1153 AV BEREDNIN        
234000*** GÄLLER ENDAST PRODUKTSLAG PV-BASLAGER                                 
234100******************************************************************        
234200     SKIP2                                                                
234300     IF KDPRODSL-UTAN-EMB OR KDPRODSL-LOCAL                               
234400*** *** KONTROLL PÅ PROJK SKA SKE ***  ***  ***  ***  *** *** ***         
234500        PERFORM IMS-GU-WLXXAQ01-UNIK                                      
234600        IF SEGMENT-FINNS                                                  
234700           MOVE SPAR-IDPROJK TO W-IDPROJK                                 
234800           PERFORM IMS-GNP-WLXXAQ11-PROJK                                 
234900           IF SEGMENT-FINNS                                               
235000              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR            
235100           ELSE                                                           
235200              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJK-IN-ATTR              
235300              MOVE NEJ TO SW-INPUT-RAETT                                  
235310              MOVE FEL-ERR-KDPRODSL    TO WS-ERROR-UPDX                   
235400           END-IF                                                         
235500        END-IF                                                            
235600     ELSE                                                                 
235700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR                  
235800     END-IF                                                               
235900                                                                          
236000     IF SW-INPUT-RAETT = JA                                               
236100        IF MID-IDPROJK = ALL '+'                                          
236200           CONTINUE                                                       
236300        ELSE                                                              
236400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPROJK-ATTR                 
236500        END-IF                                                            
236600     END-IF                                                               
236700     .                                                                    
236800     EJECT                                                                
236900 E-UPPDATERA-OCH-VISA-BILD SECTION.                                       
237000     SKIP2                                                                
237100     MOVE NEJ TO SW-KDSORT-AENDRAD-TILL-FRAN-SA                           
237200                                                                          
237300     PERFORM IMS-GHU-ARTC01                                               
237400                                                                          
237500     IF MID-TISOP    = ALL '+'  AND                                       
237600        MID-IDAO (1) = ALL '+'  AND                                       
237700        MID-IDAO (2) = ALL '+'  AND                                       
237800        MID-IDAO (3) = ALL '+'  AND                                       
237900        MID-IDAO (4) = ALL '+'  AND                                       
238000        MID-IDAO (5) = ALL '+'  AND                                       
238100        MID-KDPRODSL = ALL '+'  AND                                       
238200        MID-IDFKNGRP = ALL '+'  AND                                       
238300        MID-KDSORT   = ALL '+'                                            
238400        CONTINUE                                                          
238500     ELSE                                                                 
238600        PERFORM EA-UPPDATERING-ARTC01                                     
238700        PERFORM IMS-REPL-ARTC                                             
238800     END-IF                                                               
238900                                                                          
239000     MOVE ART-TIFINLV                TO MOD-TIFINLV                       
239100     MOVE ART-TISOP                  TO MOD-TISOP                         
239200     MOVE ART-IDAO (1)               TO MOD-IDAO (1)                      
239300                                        SPAR-IDAO1                        
239400     MOVE ART-IDAO (2)               TO MOD-IDAO (2)                      
239500     MOVE ART-IDAO (3)               TO MOD-IDAO (3)                      
239600     MOVE ART-IDAO (4)               TO MOD-IDAO (4)                      
239700     MOVE ART-IDAO (5)               TO MOD-IDAO (5)                      
239800                                                                          
239900     MOVE ART-KDPRODSL               TO MOD-KDPRODSL                      
240000                                        SPAR-KDPRODSL                     
240100     MOVE ART-IDFKNGRP               TO MOD-IDFKNGRP                      
240200                                        SPAR-IDFKNGRP                     
240300     MOVE ART-KDSORT                 TO MOD-KDSORT                        
240400                                        SPAR-KDSORT                       
240500                                                                          
240600*WDK611                                                                   
240700     PERFORM IMS-GHNP-ARTC11                                              
240800     MOVE CLAG-KDERS TO SPAR-KDERS                                        
240900     IF MID-IDBERED       = ALL '+'  AND                                  
241000        MID-IDPROJ        = ALL '+'  AND                                  
241100        MID-IDKAT (1)     = ALL '+'  AND                                  
241200        MID-IDKAT (2)     = ALL '+'  AND                                  
241300        MID-IDKAT (3)     = ALL '+'  AND                                  
241400        MID-IDPROJUP      = ALL '+'  AND                                  
241500        MID-IDRITN        = ALL '+'  AND                                  
241600        MID-KDAGE         = ALL '+'  AND                                  
241700        MID-KDUART        = ALL '+'  AND                                  
241800        MID-FLLSRDEL     = ALL '+'   AND                                  
241900        MID-KDBPSR       = ALL '+'   AND                                  
242000        MID-IDPROENH (1) = ALL '+'   AND                                  
242100        MID-IDPROENH (2) = ALL '+'   AND                                  
242200        MID-IDPROENH (3) = ALL '+'   AND                                  
242300        SW-KOLLA-KDPSLLOC = NEJ                                           
242400        CONTINUE                                                          
242500     ELSE                                                                 
242600        PERFORM EB-UPPDATERING-ARTC11                                     
242700        PERFORM IMS-REPL-ARTC                                             
242800     END-IF                                                               
242900                                                                          
243000     MOVE CLAG-IDBERED               TO MOD-IDBERED                       
243100                                        SPAR-IDBERED                      
243200     MOVE CLAG-IDPROJ                TO MOD-IDPROJ                        
243300                                        SPAR-IDPROJ                       
243400                                                                          
243500     MOVE CLAG-IDKAT (1)             TO MOD-IDKAT (1)                     
243600     MOVE CLAG-IDKAT (2)             TO MOD-IDKAT (2)                     
243700     MOVE CLAG-IDKAT (3)             TO MOD-IDKAT (3)                     
243800                                                                          
243900     MOVE CLAG-IDPROJUP              TO MOD-IDPROJUP                      
244000     MOVE CLAG-IDRITN                TO MOD-IDRITN                        
244100                                        SPAR-IDRITN                       
244200     MOVE CLAG-KDAGE                 TO MOD-KDAGE                         
244300     MOVE CLAG-FLLSRDEL              TO MOD-FLLSRDEL                      
244400                                        SPAR-FLLSRDEL                     
244500     MOVE CLAG-KDUART                TO MOD-KDUART                        
244600                                        SPAR-KDUART                       
244700     MOVE CLAG-KDBPSR                TO MOD-KDBPSR                        
244800                                        SPAR-KDBPSR                       
244900                                                                          
245000     MOVE CLAG-IDPROENH(1)           TO MOD-IDPROENH(1)                   
245100                                        SPAR-IDPROENH1                    
245200     INSPECT MOD-IDPROENH (1) REPLACING LEADING ZERO BY SPACE             
245300                                                                          
245400     MOVE CLAG-IDPROENH(2)           TO MOD-IDPROENH(2)                   
245500     INSPECT MOD-IDPROENH (2) REPLACING LEADING ZERO BY SPACE             
245600                                                                          
245700     MOVE CLAG-IDPROENH(3)           TO MOD-IDPROENH(3)                   
245800     INSPECT MOD-IDPROENH (3) REPLACING LEADING ZERO BY SPACE             
245900                                                                          
246000*WD6625                                                                   
246100                                                                          
246200     MOVE +6                         TO W-KDNOTTYP                        
246300     PERFORM IMS-GHNP-ARTC25                                              
246400     IF MID-TEVARNOT = ALL '+'                                            
246500        IF SEGMENT-FINNS                                                  
246600           MOVE NOT-TEARTNOT         TO MOD-TEVARNOT-IN-UT                
246700        END-IF                                                            
246800     ELSE                                                                 
246900        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-TEVARNOT-IN-UT-ATTR           
247000        IF MID-TEVARNOT = SPACE                                           
247100           IF SEGMENT-FINNS                                               
247200              PERFORM IMS-DLET-ARTC                                       
247300              MOVE SPACE             TO MOD-TEVARNOT-IN-UT                
247400           END-IF                                                         
247500        ELSE                                                              
247600           MOVE MID-TEVARNOT         TO NOT-TEARTNOT                      
247700                                        MOD-TEVARNOT-IN-UT                
247800           MOVE +6                   TO NOT-KDNOTTYP                      
247900           IF SEGMENT-FINNS                                               
248000              PERFORM IMS-REPL-ARTC                                       
248100           ELSE                                                           
248200              PERFORM IMS-ISRT-ARTC25                                     
248300           END-IF                                                         
248400        END-IF                                                            
248500     END-IF                                                               
248600                                                                          
248700*WDD311                                                                   
248800     IF MSGI-IDLAND-SPR = 'GB'                                            
248900        MOVE 'GB '   TO W-IDSKYLT                                         
249000     ELSE                                                                 
249100        MOVE 'S  '   TO W-IDSKYLT                                         
249200     END-IF                                                               
249300                                                                          
249400*WDK7                                                                     
249500                                                                          
249600     PERFORM IMS-GU-BENA11-BSEQ                                           
249700     MOVE BENA11-TEXT-BEART   TO MOD-BEART                                
249800                                                                          
249900     IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                    
250000                                                                          
250100        IF  MID-FLPISK        = ALL '+'                                   
250200        AND MID-IDPROJK       = ALL '+'                                   
250300        AND MID-IDARTNR-MOTSV = ALL '+'                                   
250400        AND MID-FLBYTES       = ALL '+'                                   
250500        AND MID-KVARTVAGN     = ALL '+'                                   
250600        AND MID-TEORSAK       = ALL '+'                                   
250700        AND MID-IDAO (1)      = ALL '+'                                   
250800        AND MID-TISOP         = ALL '+'                                   
250900        AND MID-IDPROJ        = ALL '+'                                   
251000        AND MID-KVPROG        = ALL '+'                                   
251100           CONTINUE                                                       
251200        ELSE                                                              
251300           PERFORM EF-UPDATNYPON-OCH-TRANSKDP                             
251400           MOVE JA  TO SW-REPL-NYPON                                      
251500        END-IF                                                            
251600                                                                          
251700        PERFORM EH-KOLLA-OM-ANSKQ-BERORS                                  
251800        PERFORM EG-KOLLA-OM-BASL-BERORS                                   
251900                                                                          
252000        IF SW-DLET-ISRT-NYPON = JA                                        
252100           PERFORM EJ-DLET-ISRT-NYPON                                     
252200        ELSE                                                              
252300           IF SW-REPL-NYPON = JA                                          
252400              PERFORM IMS-REPL-NYPON                                      
252500           END-IF                                                         
252600        END-IF                                                            
252700                                                                          
252800        PERFORM EM-FLYTTA-NYPON-TILL-MOD                                  
252900     END-IF                                                               
253000                                                                          
253100     PERFORM S03-RENSA-MOD-INMATNINGSFAELT                                
253200     PERFORM S05-FORMATETS-ATTRIBUT                                       
253300                                                                          
253400     MOVE MED-2 (SPAR-TEXT-IND)   TO MOD-TEMFSINF                         
253500     IF KDSORT-AENDRAD-TILL-FRAN-SATS                                     
253600*****  VARNING ATT SORT ÄNDRAD TILL/FRÅN SATS                             
253700       MOVE FEL-5 (SPAR-TEXT-IND) TO MOD-TEMFSFEL                         
253800     END-IF                                                               
253900     .                                                                    
254000     EJECT                                                                
254100 EA-UPPDATERING-ARTC01 SECTION.                                           
254200     SKIP2                                                                
254300     MOVE NEJ    TO SW-KOLLA-KDPSLLOC                                     
254400                                                                          
254500                                                                          
254600     IF MID-TISOP = ALL '+'                                               
254700        CONTINUE                                                          
254800     ELSE                                                                 
254900        PERFORM EAC-UPDATE-WDGX2264-2266                                  
255000        MOVE SPAR-TISOP-AAVVD      TO ART-TISOP                           
255100        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISOP-ATTR                      
255200        PERFORM EAB-JUSTERA-TIFINLV                                       
255300     END-IF                                                               
255400                                                                          
255500     IF  MID-IDAO (1) = ALL '+'                                           
255600     AND MID-IDAO (2) = ALL '+'                                           
255700     AND MID-IDAO (3) = ALL '+'                                           
255800     AND MID-IDAO (4) = ALL '+'                                           
255900     AND MID-IDAO (5) = ALL '+'                                           
256000        CONTINUE                                                          
256100     ELSE                                                                 
256200        IF MID-IDAO (1) = ALL '+'                                         
256300           CONTINUE                                                       
256400        ELSE                                                              
256500           MOVE MID-IDAO (1)          TO ART-IDAO (1)                     
256600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAO-ATTR (1)                
256700        END-IF                                                            
256800                                                                          
256900        IF MID-IDAO (2) = ALL '+'                                         
257000           CONTINUE                                                       
257100        ELSE                                                              
257200           MOVE MID-IDAO (2)          TO ART-IDAO (2)                     
257300           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAO-ATTR (2)                
257400        END-IF                                                            
257500                                                                          
257600        IF MID-IDAO (3) = ALL '+'                                         
257700           CONTINUE                                                       
257800        ELSE                                                              
257900           MOVE MID-IDAO (3)          TO ART-IDAO (3)                     
258000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAO-ATTR (3)                
258100        END-IF                                                            
258200                                                                          
258300        IF MID-IDAO (4) = ALL '+'                                         
258400           CONTINUE                                                       
258500        ELSE                                                              
258600           MOVE MID-IDAO (4)          TO ART-IDAO (4)                     
258700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAO-ATTR (4)                
258800        END-IF                                                            
258900                                                                          
259000        IF MID-IDAO (5) = ALL '+'                                         
259100           CONTINUE                                                       
259200        ELSE                                                              
259300           MOVE MID-IDAO (5)          TO ART-IDAO (5)                     
259400           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAO-ATTR (5)                
259500        END-IF                                                            
259600                                                                          
259700        MOVE 1    TO SPAR-TILL-IND                                        
259800                     SPAR-FRAN-IND                                        
259900                                                                          
260000        PERFORM UNTIL SPAR-FRAN-IND > 5                                   
260100           IF ART-IDAO (SPAR-FRAN-IND) = SPACE                            
260200              CONTINUE                                                    
260300           ELSE                                                           
260400              MOVE ART-IDAO (SPAR-FRAN-IND) TO                            
260500                              ART-IDAO (SPAR-TILL-IND)                    
260600              ADD 1   TO SPAR-TILL-IND                                    
260700           END-IF                                                         
260800           ADD 1      TO SPAR-FRAN-IND                                    
260900        END-PERFORM                                                       
261000                                                                          
261100        IF SPAR-TILL-IND < 6                                              
261200           PERFORM UNTIL SPAR-TILL-IND > 5                                
261300              MOVE SPACE          TO ART-IDAO (SPAR-TILL-IND)             
261400              ADD 1               TO SPAR-TILL-IND                        
261500           END-PERFORM                                                    
261600        END-IF                                                            
261700     END-IF                                                               
261800                                                                          
261900     IF MID-KDPRODSL = ALL '+'                                            
262000        CONTINUE                                                          
262100     ELSE                                                                 
262200        IF SPAR-PRARTSTD > 0                                              
262300            PERFORM EAA-PS-1117                                           
262400            MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDPRODSL-ATTR               
262500        ELSE                                                              
262600            MOVE JA TO SW-KOLLA-KDPSLLOC                                  
262700            MOVE ART-KDPRODSL            TO SPAR-KDPRODSL-OLD             
262800            MOVE MID-KDPRODSL            TO ART-KDPRODSL                  
262900                                            WS-A17-KDPRODSL               
263000            MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KDPRODSL-ATTR             
263100                                                                          
263200            MOVE KPS-IDFTG               TO ART-IDFTG                     
263300        END-IF                                                            
263400     END-IF                                                               
263500                                                                          
263600     IF MID-IDFKNGRP = ALL '+'                                            
263700        CONTINUE                                                          
263800     ELSE                                                                 
263900        MOVE JA TO SW-KOLLA-KDPSLLOC                                      
264000        MOVE MID-IDFKNGRP            TO ART-IDFKNGRP                      
264100        MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDFKNGRP-ATTR                 
264200     END-IF                                                               
264300                                                                          
264400     IF MID-KDSORT   = ALL '+'                                            
264500        CONTINUE                                                          
264600     ELSE                                                                 
264700        IF MID-KDSORT = 'SA' OR ART-KDSORT = 'SA'                         
264800          MOVE JA TO SW-KDSORT-AENDRAD-TILL-FRAN-SA                       
264900        END-IF                                                            
265000        IF MID-KDSORT = 'TM' OR ART-KDSORT = 'TM'                         
265100          MOVE JA TO SW-KDSORT-AENDRAD-TILL-FRAN-SA                       
265200        END-IF                                                            
265300        MOVE MID-KDSORT             TO ART-KDSORT                         
265400        MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KDSORT-ATTR                    
265500     END-IF                                                               
265600                                                                          
265700     .                                                                    
265800     EJECT                                                                
265900*           *********************************************                 
266000 EAA-PS-1117 SECTION.                                                     
266100     MOVE MID-KDPRODSL   TO TEST-KDPRODSL                                 
266200                            PSUPD-1118-KDPRODSL                           
266300     MOVE   WS-IDARTNR TO PSUPD-1118-IDARTNR                              
266400                          W-IDARTNR-1117                                  
266500     PERFORM IMS-ISRT-WEEK                                                
266600     IF STATUS-WS =  'II'                                                 
266700         PERFORM IMS-GHU-WEEK                                             
266800         IF WS-KDPRODSL =  TEST-KDPRODSL                                  
266900             PERFORM IMS-DLET-WEEK                                        
267000         ELSE                                                             
267100             MOVE  MID-KDPRODSL TO PSUPD-1118-KDPRODSL                    
267200             PERFORM IMS-REPL-WEEK                                        
267300         END-IF                                                           
267400     END-IF                                                               
267500*               **********NOLLSTÄLL FÄLT *************                    
267600     MOVE MFS-RENSA-FAELT      TO MOD-KDPRODSL-IN                         
267700     MOVE '++'                 TO MID-KDPRODSL                            
267800     MOVE MED-9 (SPAR-TEXT-IND) TO MOD-TEMFSINF                           
267900     MOVE MED-9 (SPAR-TEXT-IND) TO MOD-TEMFSFEL                           
268000     CONTINUE                                                             
268100     .                                                                    
268200     EJECT                                                                
268300 EAB-JUSTERA-TIFINLV   SECTION.                                           
268400                                                                          
268500     IF SPAR-TISOP-AAVVD(1:4) > SPAR-DAGENS-AAVV                          
268600        MOVE SPAR-TISOP-AAVVD     TO ART-TIFINLV                          
268700     ELSE                                                                 
268800        MOVE 'YYWWD'              TO DAYS-KDDATFMT1                       
268900        MOVE 'YYWWD'              TO DAYS-KDDATFMT2                       
269000        MOVE SPAR-TISOP-AAVVD     TO DAYS-TIDATE1                         
269100        MOVE 7                    TO DAYS-KVDAYS                          
269200        MOVE SPACE                TO DAYS-TIDATE2                         
269300                                     DAYS-IDCALEND                        
269400        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
269500                                                                          
269600        MOVE  DAYS-TIDATE2(1:5)   TO ART-TIFINLV                          
269700     END-IF                                                               
269800     .                                                                    
269900     EJECT                                                                
270000 EAC-UPDATE-WDGX2264-2266 SECTION.                                        
270100                                                                          
270200     IF ART-TISOP NOT = SPAR-TISOP-AAVVD                                  
270300        MOVE ART-TISOP                   TO W-TISOP-2264-O                
270400        PERFORM IMS-GHU-WDGX2264-OLD                                      
270500        IF SEGMENT-FINNS                                                  
270600           MOVE WS-IDARTNR               TO W-IDARTNR-2266-O-MIN          
270700                                            W-IDARTNR-2266-O-MAX          
270800           PERFORM IMS-GHNP-WDGX2266-OLD                                  
270900           IF SEGMENT-FINNS                                               
271000              MOVE SPAR-TISOP-AAVVD      TO NEW-2264-TISOP                
271100              PERFORM IMS-ISRT-WDGX2264-NEW                               
271200           END-IF                                                         
271300                                                                          
271400           PERFORM UNTIL SEGMENT-SAKNAS                                   
271500              MOVE OLD-2266-WDGX2266     TO NEW-2266-WDGX2266             
271600              PERFORM IMS-ISRT-WDGX2266-NEW                               
271700              PERFORM IMS-DLET-WDGX2266-OLD                               
271800                                                                          
271900              PERFORM IMS-GHNP-WDGX2266-OLD                               
272000           END-PERFORM                                                    
272100                                                                          
272200           PERFORM IMS-GHU-WDGX2264-OLD                                   
272300           IF SEGMENT-FINNS                                               
272400              PERFORM IMS-GNP-WDGX2266                                    
272500              IF SEGMENT-SAKNAS                                           
272600                 PERFORM IMS-GHU-WDGX2264-OLD                             
272700                 PERFORM IMS-DLET-WDGX2264-OLD                            
272800              END-IF                                                      
272900           END-IF                                                         
273000        END-IF                                                            
273100     END-IF                                                               
273200     .                                                                    
273300     EJECT                                                                
273400 EB-UPPDATERING-ARTC11 SECTION.                                           
273500                                                                          
273600     IF MID-IDBERED  = ALL '+'                                            
273700        CONTINUE                                                          
273800     ELSE                                                                 
273900        MOVE CLAG-IDBERED         TO SPAR-IDBERED-OLD                     
274000        MOVE MID-IDBERED          TO CLAG-IDBERED                         
274100                                     SPAR-IDBERED                         
274200        MOVE MFS-ADD-LYS-UPP-FAELT                                        
274300                                 TO MOD-IDBERED-ATTR                      
274400     END-IF                                                               
274500                                                                          
274600     IF MID-IDPROJ   = ALL '+'                                            
274700        CONTINUE                                                          
274800     ELSE                                                                 
274900        MOVE MID-IDPROJ           TO CLAG-IDPROJ                          
275000        MOVE MFS-ADD-LYS-UPP-FAELT                                        
275100                                 TO MOD-IDPROJ-ATTR                       
275200     END-IF                                                               
275300                                                                          
275400     IF  MID-IDKAT (1) = ALL '+'                                          
275500     AND MID-IDKAT (2) = ALL '+'                                          
275600     AND MID-IDKAT (3) = ALL '+'                                          
275700        CONTINUE                                                          
275800     ELSE                                                                 
275900        IF MID-IDKAT (1) = ALL '+'                                        
276000           CONTINUE                                                       
276100        ELSE                                                              
276200           MOVE MID-IDKAT (1)          TO CLAG-IDKAT (1)                  
276300           MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-IDKAT-ATTR  (1)             
276400        END-IF                                                            
276500                                                                          
276600        IF MID-IDKAT (2) = ALL '+'                                        
276700           CONTINUE                                                       
276800        ELSE                                                              
276900           MOVE MID-IDKAT (2)         TO CLAG-IDKAT (2)                   
277000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKAT-ATTR  (2)              
277100        END-IF                                                            
277200                                                                          
277300        IF MID-IDKAT (3) = ALL '+'                                        
277400           CONTINUE                                                       
277500        ELSE                                                              
277600           MOVE MID-IDKAT (3)         TO CLAG-IDKAT (3)                   
277700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDKAT-ATTR  (3)              
277800        END-IF                                                            
277900                                                                          
278000        MOVE 1   TO SPAR-TILL-IND                                         
278100                    SPAR-FRAN-IND                                         
278200                                                                          
278300        PERFORM UNTIL SPAR-FRAN-IND > 3                                   
278400           IF CLAG-IDKAT (SPAR-FRAN-IND) = SPACE                          
278500              CONTINUE                                                    
278600           ELSE                                                           
278700              MOVE CLAG-IDKAT (SPAR-FRAN-IND) TO                          
278800                              CLAG-IDKAT (SPAR-TILL-IND)                  
278900              ADD 1  TO SPAR-TILL-IND                                     
279000           END-IF                                                         
279100           ADD 1   TO SPAR-FRAN-IND                                       
279200        END-PERFORM                                                       
279300                                                                          
279400        IF SPAR-TILL-IND < 4                                              
279500           PERFORM UNTIL SPAR-TILL-IND > 3                                
279600              MOVE SPACE   TO CLAG-IDKAT (SPAR-TILL-IND)                  
279700              ADD 1        TO SPAR-TILL-IND                               
279800           END-PERFORM                                                    
279900        END-IF                                                            
280000     END-IF                                                               
280100                                                                          
280200     IF MID-IDPROJUP = ALL '+'                                            
280300        CONTINUE                                                          
280400     ELSE                                                                 
280500        MOVE MID-IDPROJUP          TO CLAG-IDPROJUP                       
280600        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPROJUP-ATTR                   
280700     END-IF                                                               
280800                                                                          
280900     IF MID-IDRITN = ALL '+'                                              
281000        CONTINUE                                                          
281100     ELSE                                                                 
281200        MOVE MID-IDRITN            TO CLAG-IDRITN                         
281300        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDRITN-ATTR                     
281400     END-IF                                                               
281500                                                                          
281600     IF MID-KDAGE = ALL '+'                                               
281700        CONTINUE                                                          
281800     ELSE                                                                 
281900       MOVE SPAR-KDAGE            TO CLAG-KDAGE                           
282000       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDAGE-ATTR                       
282100     END-IF                                                               
282200                                                                          
282300     IF MID-KDUART = ALL '+'                                              
282400        CONTINUE                                                          
282500     ELSE                                                                 
282600        MOVE CLAG-KDUART           TO SPAR-KDUART-OLD                     
282700        MOVE MID-KDUART            TO CLAG-KDUART                         
282800        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDUART-ATTR                     
282900     END-IF                                                               
283000                                                                          
283100     IF MID-FLLSRDEL = ALL '+'                                            
283200        CONTINUE                                                          
283300     ELSE                                                                 
283400        MOVE CLAG-FLLSRDEL          TO SPAR-FLLSRDEL-OLD                  
283500        MOVE MID-FLLSRDEL           TO CLAG-FLLSRDEL                      
283600        MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-FLLSRDEL-ATTR                  
283700*********                                                                 
283800        MOVE 'J'                    TO CLAG-FLREFILL                      
283900        MOVE SPAR-KDPRODSL          TO TEST-KDPRODSL                      
284000        IF KDPRODSL-BIMA                                                  
284100           MOVE 'N'     TO CLAG-FLREFILL                                  
284200        ELSE                                                              
284300           IF KDPRODSL-WHEELS AND SPAR-IDFKNGRP = 3955                    
284400              MOVE 'N'  TO CLAG-FLREFILL                                  
284500           ELSE                                                           
284600              IF CLAG-FLLSRDEL = 'N'                                      
284700                 MOVE 'N' TO CLAG-FLREFILL                                
284800              END-IF                                                      
284900           END-IF                                                         
285000        END-IF                                                            
285100*********                                                                 
285200     END-IF                                                               
285300                                                                          
285400     IF MID-KDBPSR   = ALL '+'                                            
285500        CONTINUE                                                          
285600     ELSE                                                                 
285700        MOVE MID-KDBPSR             TO CLAG-KDBPSR                        
285800        MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KDBPSR-ATTR                    
285900     END-IF                                                               
286000                                                                          
286100     IF MID-IDPROENH (1) = ALL '+'                                        
286200        CONTINUE                                                          
286300     ELSE                                                                 
286400        MOVE MID-IDPROENH (1) TO CLAG-IDPROENH(1)                         
286500        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPROENH-ATTR(1)                
286600     END-IF                                                               
286700                                                                          
286800     IF MID-IDPROENH (2) = ALL '+'                                        
286900        CONTINUE                                                          
287000     ELSE                                                                 
287100        MOVE MID-IDPROENH (2) TO CLAG-IDPROENH(2)                         
287200        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPROENH-ATTR(2)                
287300     END-IF                                                               
287400                                                                          
287500     IF MID-IDPROENH (3) = ALL '+'                                        
287600        CONTINUE                                                          
287700     ELSE                                                                 
287800        MOVE MID-IDPROENH (3) TO CLAG-IDPROENH(3)                         
287900        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPROENH-ATTR(3)                
288000     END-IF                                                               
288100                                                                          
288200     IF SW-KOLLA-KDPSLLOC = JA                                            
288300        PERFORM EBA-KOLLA-KDPSLLOC                                        
288400     END-IF                                                               
288500     .                                                                    
288600     EJECT                                                                
288700 EBA-KOLLA-KDPSLLOC SECTION.                                              
288800                                                                          
288900     MOVE CLAG-KDPSLLOC              TO WS-KDPSLLOC-OLD                   
289000                                                                          
289100     MOVE W-IDARTNR                  TO LPC-IDARTNR-IN                    
289200     MOVE SPAR-IDFKNGRP              TO LPC-IDFKNGRP-IN                   
289300     MOVE SPAR-KDPRODSL              TO LPC-KDPRODSL-IN                   
289400     MOVE ZERO                       TO LPC-KDPSLLOC-UT                   
289500     CALL W100LPC  USING LPC-AREA                                         
289600                                                                          
289700     MOVE LPC-KDPSLLOC-UT            TO CLAG-KDPSLLOC                     
289800                                                                          
289900     PERFORM EBAA-FYLL-WDR8-A17                                           
290000     .                                                                    
290100     EJECT                                                                
290200 EBAA-FYLL-WDR8-A17 SECTION.                                              
290300     MOVE SPACE TO A17-W510A17                                            
290400     PERFORM IMS-GU-WDK701                                                
290500     IF SEGMENT-FINNS                                                     
290600      PERFORM IMS-GNP-WDK711                                              
290700      IF SEGMENT-FINNS                                                    
290800       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
290900         MOVE SLAG-IDDC TO W-IDDC                                         
291000         PERFORM IMS-GU-WDB601                                            
291100         IF DCS-NDC-NA                                                    
291200           IF DCS-USA                                                     
291300               MOVE SLAG-PRAVCOST  TO A17-PRAVCOST                        
291400               MOVE SLAG-KVLS      TO A17-KVLS                            
291500               MOVE SLAG-KVEFRS    TO A17-KVEFRS                          
291600               MOVE '53'           TO A17-IDFTG                           
291700               PERFORM EBAAA-SKAPA-A17                                    
291800           END-IF                                                         
291900                                                                          
292000           IF DCS-CANADA                                                  
292100               MOVE SLAG-PRAVCOST  TO A17-PRAVCOST                        
292200               MOVE SLAG-KVLS      TO A17-KVLS                            
292300               MOVE SLAG-KVEFRS    TO A17-KVEFRS                          
292400               MOVE '54'           TO A17-IDFTG                           
292500               PERFORM EBAAA-SKAPA-A17                                    
292600           END-IF                                                         
292700         END-IF                                                           
292800         PERFORM IMS-GNP-WDK711                                           
292900       END-PERFORM                                                        
293000      END-IF                                                              
293100     END-IF                                                               
293200     .                                                                    
293300     EJECT                                                                
293400 EBAAA-SKAPA-A17 SECTION.                                                 
293500                                                                          
293600     MOVE 'A17'                  TO A17-IDPTYP                            
293700     MOVE 'M21'                  TO A17-KDEKOHT                           
293800     MOVE W-IDDC                 TO A17-IDDC-REC                          
293900     MOVE MSGI-IDDC              TO A17-IDDC-SEND                         
294000     MOVE WS-A17-KDPRODSL        TO A17-KDPRODSL                          
294100     MOVE W-IDARTNR              TO A17-IDARTNR                           
294200     MOVE WS-KDPSLLOC-OLD        TO A17-KDPSLLOC-OLD                      
294300     MOVE CLAG-KDPSLLOC          TO A17-KDPSLLOC-NEW                      
294400                                                                          
294500     MOVE 'W1011700'             TO FIL-IDPGM                             
294600     MOVE FUNCTION CURRENT-DATE (1:8) TO                                  
294700     DAGENS-TIAAAAMMDD                                                    
294800     ACCEPT WS-HHMMSSTH FROM TIME                                         
294900     MOVE WS-HHMMSSTH            TO FIL-TIKLOCK                           
295000                                                                          
295100     MOVE DAGENS-TIAAAAMMDD      TO FIL-TIREGDAT                          
295200     MOVE DAGENS-TIAAAAMMDD      TO A17-DAJUSTDA                          
295300     ADD +1                      TO W-IDSEKVNR                            
295400     MOVE W-IDSEKVNR             TO FIL-IDSEKVNR                          
295500     MOVE 'W510A17 '             TO FIL-IDCPYTXT                          
295600     MOVE A17-W510A17            TO FIL-WDR801-DATA                       
295700                                                                          
295800     PERFORM IMS-ISRT-WDR801                                              
295900     IF SEGMENT-FINNS-REDAN                                               
296000       PERFORM UNTIL SEGMENT-FINNS                                        
296100         ADD +1                      TO W-IDSEKVNR                        
296200         MOVE W-IDSEKVNR             TO FIL-IDSEKVNR                      
296300         PERFORM IMS-ISRT-WDR801                                          
296400       END-PERFORM                                                        
296500     END-IF                                                               
296600     .                                                                    
296700     EJECT                                                                
296800 EF-UPDATNYPON-OCH-TRANSKDP     SECTION.                                  
296900     SKIP3                                                                
297000     IF MID-FLPISK = ALL '+'                                              
297100        CONTINUE                                                          
297200     ELSE                                                                 
297300        MOVE MID-FLPISK             TO NYPON-ART-FLPISK                   
297400        MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-FLPISK-ATTR                    
297500     END-IF                                                               
297600                                                                          
297700     IF MID-KVPROG = ALL '+'                                              
297800        CONTINUE                                                          
297900     ELSE                                                                 
298000        MOVE SPAR-KVPROG            TO NYPON-ART-KVPROG                   
298100        MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVPROG-ATTR                    
298200     END-IF                                                               
298300                                                                          
298400     IF MID-IDPROJK = ALL '+'                                             
298500        CONTINUE                                                          
298600     ELSE                                                                 
298700        MOVE MID-IDPROJK         TO NYPON-ART-IDPROJK                     
298800        MOVE MFS-ADD-LYS-UPP-FAELT                                        
298900                                 TO MOD-IDPROJK-ATTR                      
299000     END-IF                                                               
299100                                                                          
299200     IF MID-IDARTNR-MOTSV = ALL '+'                                       
299300        CONTINUE                                                          
299400     ELSE                                                                 
299500        MOVE MID-IDARTNR-MOTSV     TO NYPON-ART-IDARTNR-MOTSV             
299600        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-MOTSV-ATTR              
299700     END-IF                                                               
299800                                                                          
299900     IF MID-FLBYTES  = ALL '+'                                            
300000        CONTINUE                                                          
300100     ELSE                                                                 
300200        MOVE MID-FLBYTES           TO NYPON-ART-FLBYTES                   
300300        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLBYTES-ATTR                    
300400     END-IF                                                               
300500                                                                          
300600     IF MID-KVARTVAGN     = ALL '+'                                       
300700        CONTINUE                                                          
300800     ELSE                                                                 
300900        MOVE MID-KVARTVAGN         TO NYPON-ART-KVARTVAGN                 
301000        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVARTVAGN-ATTR                  
301100     END-IF                                                               
301200                                                                          
301300     IF MID-TEORSAK       = ALL '+'                                       
301400        CONTINUE                                                          
301500     ELSE                                                                 
301600        MOVE MID-TEORSAK           TO NYPON-ART-TEORSAK                   
301700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEORSAK-IN-UT-ATTR              
301800     END-IF                                                               
301900                                                                          
302000     IF MID-IDAO (1)      = ALL '+'                                       
302100        CONTINUE                                                          
302200     ELSE                                                                 
302300        MOVE MID-IDAO (1)            TO NYPON-ART-IDAO                    
302400     END-IF                                                               
302500                                                                          
302600     IF MID-IDPROJ        = ALL '+'                                       
302700        CONTINUE                                                          
302800     ELSE                                                                 
302900        MOVE MID-IDPROJ              TO NYPON-ART-IDPROJ                  
303000     END-IF                                                               
303100                                                                          
303200     IF NYPON-ART-KDRESBED = 'E' OR 'U'                                   
303300        CONTINUE                                                          
303400     ELSE                                                                 
303500        PERFORM EFA-UPPDATERA-KDRESBED                                    
303600        IF SW-ISRT-TRANS-TILL-KDP     = JA                                
303700           PERFORM EFB-SKICKA-TRANS-TILL-KDP                              
303800        END-IF                                                            
303900     END-IF                                                               
304000     .                                                                    
304100     EJECT                                                                
304200 EFA-UPPDATERA-KDRESBED SECTION.                                          
304300     SKIP2                                                                
304400     IF SPAR-KDBPSR = 8  AND  SPAR-FLLSRDEL = NEJ                         
304500        IF NYPON-ART-KDRESBED = '-'                                       
304600           CONTINUE                                                       
304700        ELSE                                                              
304800           MOVE '-'               TO NYPON-ART-KDRESBED                   
304900                                     WS-KDRESBED                          
305000           MOVE JA                TO SW-ISRT-TRANS-TILL-KDP               
305100        END-IF                                                            
305200     ELSE                                                                 
305300        IF NYPON-ART-KDRESBED = 'R'                                       
305400           CONTINUE                                                       
305500        ELSE                                                              
305600           MOVE 'R'               TO NYPON-ART-KDRESBED                   
305700                                     WS-KDRESBED                          
305800           MOVE JA                TO SW-ISRT-TRANS-TILL-KDP               
305900        END-IF                                                            
306000     END-IF                                                               
306100     IF MID-KDPRODSL = ALL '+'                                            
306200        MOVE SPAR-KDPRODSL        TO TEST-KDPRODSL                        
306300        IF KDPRODSL-VCBV                                                  
306400           MOVE NEJ               TO SW-ISRT-TRANS-TILL-KDP               
306500        END-IF                                                            
306600     ELSE                                                                 
306700        MOVE SPAR-KDPRODSL-OLD    TO TEST-KDPRODSL                        
306800        IF KDPRODSL-VCBV                                                  
306900           MOVE SPAR-KDPRODSL     TO TEST-KDPRODSL                        
307000           IF KDPRODSL-VCBV                                               
307100              MOVE NEJ            TO SW-ISRT-TRANS-TILL-KDP               
307200           ELSE                                                           
307300              CONTINUE                                                    
307400           END-IF                                                         
307500        ELSE                                                              
307600           MOVE SPAR-KDPRODSL     TO TEST-KDPRODSL                        
307700           IF KDPRODSL-VCBV                                               
307800              MOVE NEJ            TO SW-ISRT-TRANS-TILL-KDP               
307900           ELSE                                                           
308000              CONTINUE                                                    
308100           END-IF                                                         
308200        END-IF                                                            
308300     END-IF                                                               
308400     .                                                                    
308500     EJECT                                                                
308600 EFB-SKICKA-TRANS-TILL-KDP     SECTION.                                   
308700     SKIP3                                                                
308800     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
308900     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
309000******************************************************************        
309100*    IDLOGLOP= 3, FÖR ATT SKILJA TRANSAR FRÅN 1113,1115,1117,1142         
309200******************************************************************        
309300     MOVE 3                          TO W-IDLOGLOP                        
309400     MOVE W-IDLOGLOP                 TO ZZAC-IDLOGLOP                     
309500                                                                          
309600     MOVE 'RZU'                      TO KDP-IDPTYP                        
309700                                                                          
309800     MOVE WS-KDRESBED                TO KDP-KDUART                        
309900                                                                          
310000     MOVE W-IDARTNR                  TO WS-IDARTNR-OPACKAT                
310100     MOVE WS-IDARTNR-OPACKAT         TO KDP-IDARTNR                       
310200                                        W092-SORTBGP                      
310300                                                                          
310400     MOVE KDP-W10111                 TO ZZAC-LOGGPOST                     
310500     MOVE W092-AREA                  TO ZZAC-SORTPOST                     
310600     PERFORM IMS-ISRT-ZZAC.                                               
310700                                                                          
310800     EJECT                                                                
310900 EG-KOLLA-OM-BASL-BERORS SECTION.                                         
311000     SKIP3                                                                
311100*****************************************************************         
311200*  ÄT NOV 92  ARTIKEL MED KDUART SKA INTE TILL BASLAGER         *         
311300*****************************************************************         
311400                                                                          
311500     PERFORM EGA-TESTA-OM-KDPRODSL-BYTE                                   
311600     IF SW-DLET-ISRT-NYPON = JA                                           
311700        CONTINUE                                                          
311800     ELSE                                                                 
311900        PERFORM EGB-TESTA-OM-IDPROJ-BYTE                                  
312000        IF SW-DLET-ISRT-NYPON = JA                                        
312100           CONTINUE                                                       
312200        ELSE                                                              
312300           PERFORM EGC-TESTA-OM-FLLSRDEL-BYTE                             
312400           IF SW-DLET-ISRT-NYPON = JA                                     
312500              CONTINUE                                                    
312600           ELSE                                                           
312700              PERFORM EGE-TESTA-OM-KDUART-BYTE                            
312800           END-IF                                                         
312900        END-IF                                                            
313000     END-IF                                                               
313100     .                                                                    
313200     EJECT                                                                
313300 EGA-TESTA-OM-KDPRODSL-BYTE SECTION.                                      
313400     SKIP3                                                                
313500     IF MID-KDPRODSL = ALL '+'                                            
313600        CONTINUE                                                          
313700     ELSE                                                                 
313800        IF MID-KDPRODSL = SPAR-KDPRODSL-OLD                               
313900           CONTINUE                                                       
314000        ELSE                                                              
314100           PERFORM EGAA-KDPRODSL-BYTE-UTFORT                              
314200        END-IF                                                            
314300     END-IF.                                                              
314400     EJECT                                                                
314500 EGAA-KDPRODSL-BYTE-UTFORT SECTION.                                       
314600     SKIP2                                                                
314700                                                                          
314800*    TEST OM PRODUKTSLAGSBYTET PÅVERKAR   B A S L A G E R                 
314900                                                                          
315000     MOVE MID-KDPRODSL                TO TEST-KDPRODSL                    
315100     MOVE SPAR-KDPRODSL-OLD           TO WS-TEST-KDPRODSL                 
315200     IF (KDPRODSL-CHEMICAL OR KDPRODSL-X5-X7)  AND                        
315300        (WS-KDPRODSL-PARTS-BYTES)                                         
315400                                                                          
315500*       PRODUKTSLAGSBYTE FRÅN 11/14 ---> 15/16/17                         
315600                                                                          
315700        IF NYPON-ART-TISTOMREG > ZERO                                     
315800           MOVE NYPON-ART-IDPROJ      TO W-IDPROJ-1123                    
315900           PERFORM IMS-GU-WLXXAP01-UNIK                                   
316000*          LÄSER ALLTID MED PRODUKTSLAG 11                                
316100           IF SEGMENT-FINNS                                               
316200              PERFORM IMS-GNP-WLXXAP11                                    
316300              IF SEGMENT-FINNS                                            
316400                 MOVE XXAP-1124-TIPROJSTO                                 
316500                                      TO NYPON-ART-TISTOMREG              
316600                 MOVE JA              TO SW-REPL-NYPON                    
316700              END-IF                                                      
316800           END-IF                                                         
316900        END-IF                                                            
317000     ELSE                                                                 
317100        MOVE MID-KDPRODSL               TO TEST-KDPRODSL                  
317200        IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV                             
317300           MOVE SPAR-KDPRODSL-OLD       TO TEST-KDPRODSL                  
317400           IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV                          
317500              CONTINUE                                                    
317600           ELSE                                                           
317700*             PRODUKTSLAGSBYTE FRÅN ANNAT ----> 11/14/15/16/17/18         
317800*                                               21/24/25/26/27/28/        
317900*                                               29                        
318000              IF SPAR-FLLSRDEL       = JA                                 
318100              AND SPAR-KDUART        = SPACE                              
318200                 MOVE 1                 TO NYPON-ART-DABASL               
318300                 MOVE JA                TO SW-REPL-NYPON                  
318400              END-IF                                                      
318500           END-IF                                                         
318600        ELSE                                                              
318700           MOVE SPAR-KDPRODSL-OLD       TO TEST-KDPRODSL                  
318800           IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV                          
318900                                                                          
319000*             PRODUKTSLAGSBYTE FRÅN 11/14/15/16/17/18                     
319100*                                   21/24/25/26/27/28/29                  
319200*                                                  ----> ANNAT            
319300                                                                          
319400              MOVE ZERO                 TO  NYPON-ART-DABASL              
319500              MOVE JA                   TO  SW-DLET-ISRT-NYPON            
319600           END-IF                                                         
319700        END-IF                                                            
319800     END-IF.                                                              
319900     EJECT                                                                
320000 EGB-TESTA-OM-IDPROJ-BYTE SECTION.                                        
320100     SKIP3                                                                
320200     IF MID-IDPROJ = ALL '+'                                              
320300        CONTINUE                                                          
320400     ELSE                                                                 
320500        IF MID-IDPROJ = SPAR-IDPROJ-OLD                                   
320600           CONTINUE                                                       
320700        ELSE                                                              
320800           PERFORM EGBA-IDPROJ-BYTE-UTFORT                                
320900        END-IF                                                            
321000     END-IF.                                                              
321100     EJECT                                                                
321200 EGBA-IDPROJ-BYTE-UTFORT SECTION.                                         
321300     SKIP3                                                                
321400     IF NYPON-ART-DABASL > 1                                              
321500        MOVE SPAR-KDPRODSL           TO TEST-KDPRODSL                     
321600        IF (KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV)                           
321700        AND SPAR-FLLSRDEL = JA                                            
321800        AND SPAR-KDUART   = SPACE                                         
321900           MOVE 1                 TO NYPON-ART-DABASL                     
322000        ELSE                                                              
322100           MOVE ZERO              TO NYPON-ART-DABASL                     
322200        END-IF                                                            
322300        MOVE JA              TO SW-DLET-ISRT-NYPON                        
322400     END-IF.                                                              
322500     EJECT                                                                
322600 EGC-TESTA-OM-FLLSRDEL-BYTE SECTION.                                      
322700     SKIP3                                                                
322800     IF MID-FLLSRDEL = ALL '+'                                            
322900        CONTINUE                                                          
323000     ELSE                                                                 
323100        IF MID-FLLSRDEL = SPAR-FLLSRDEL-OLD                               
323200           CONTINUE                                                       
323300        ELSE                                                              
323400           PERFORM EGCA-FLLSRDEL-BYTE-UTFORT                              
323500        END-IF                                                            
323600     END-IF.                                                              
323700     EJECT                                                                
323800 EGCA-FLLSRDEL-BYTE-UTFORT SECTION.                                       
323900     SKIP3                                                                
324000     IF  MID-FLLSRDEL = JA                                                
324100     AND SPAR-FLLSRDEL-OLD = NEJ                                          
324200*       A R T I K E L  S K A L L  T I L L  B A S L A G E R                
324300        MOVE SPAR-KDPRODSL        TO TEST-KDPRODSL                        
324400        IF (KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV)                           
324500        AND SPAR-KDUART        = SPACE                                    
324600           MOVE 1                 TO NYPON-ART-DABASL                     
324700           MOVE JA                TO SW-REPL-NYPON                        
324800        END-IF                                                            
324900     ELSE                                                                 
325000        IF NYPON-ART-DABASL > 1                                           
325100           MOVE ZERO              TO NYPON-ART-DABASL                     
325200           MOVE JA                TO SW-DLET-ISRT-NYPON                   
325300        END-IF                                                            
325400     END-IF                                                               
325500     .                                                                    
325600     EJECT                                                                
325700 EGE-TESTA-OM-KDUART-BYTE SECTION.                                        
325800     SKIP3                                                                
325900*    G Ä L L E R    B A S L A G E R B E H A N D L I N G                   
326000     IF MID-KDUART = ALL '+'                                              
326100        CONTINUE                                                          
326200     ELSE                                                                 
326300        IF MID-KDUART = SPAR-KDUART-OLD                                   
326400           CONTINUE                                                       
326500        ELSE                                                              
326600           IF MID-KDUART > SPACE AND SPAR-KDUART-OLD > SPACE              
326700              CONTINUE                                                    
326800           ELSE                                                           
326900              PERFORM EGEA-KDUART-BYTE-UTFORT                             
327000           END-IF                                                         
327100        END-IF                                                            
327200     END-IF                                                               
327300     .                                                                    
327400     EJECT                                                                
327500 EGEA-KDUART-BYTE-UTFORT SECTION.                                         
327600     SKIP3                                                                
327700     IF  MID-KDUART > SPACE                                               
327800     AND SPAR-KDUART-OLD = SPACE                                          
327900        IF NYPON-ART-DABASL > 1                                           
328000           MOVE ZERO              TO NYPON-ART-DABASL                     
328100           MOVE JA                TO SW-DLET-ISRT-NYPON                   
328200        END-IF                                                            
328300     ELSE                                                                 
328400        MOVE SPAR-KDPRODSL        TO TEST-KDPRODSL                        
328500******** ARTIKEL SKA TILL BASLAGER                                        
328600                                                                          
328700        IF (KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV)                           
328800           AND SPAR-FLLSRDEL = JA                                         
328900           MOVE 1                 TO NYPON-ART-DABASL                     
329000           MOVE JA                TO SW-REPL-NYPON                        
329100        END-IF                                                            
329200     END-IF                                                               
329300     .                                                                    
329400     EJECT                                                                
329500 EH-KOLLA-OM-ANSKQ-BERORS SECTION.                                        
329600     SKIP3                                                                
329700     IF SPAR-PRARTSTD = ZERO                                              
329800        IF (SPAR-KDBPSR = 8  AND  SPAR-FLLSRDEL = NEJ)  OR                
329900           (SPAR-KDERS         > 20   )                                   
330000           IF NYPON-ART-KDANSKQ = ZERO                                    
330100              CONTINUE                                                    
330200           ELSE                                                           
330300*             A R T I K E L  S K A L L  E J  T I L L  A N S K K Ö         
330400              MOVE ZERO              TO NYPON-ART-KDANSKQ                 
330500              MOVE JA                TO SW-REPL-NYPON                     
330600           END-IF                                                         
330700        ELSE                                                              
330800           IF NYPON-ART-KDANSKQ = 1                                       
330900              CONTINUE                                                    
331000           ELSE                                                           
331100*             A R T I K E L  S K A L L  T I L L  A N S K K Ö              
331200              IF SPAR-KDERS > 10                                          
331300                 CONTINUE                                                 
331400              ELSE                                                        
331500                 MOVE JA                TO SW-REPL-NYPON                  
331600                 MOVE 1                 TO NYPON-ART-KDANSKQ              
331700                                                                          
331800                 IF SPAR-IDANSK = ZERO                                    
331900                    MOVE SPAR-KDPRODSL TO TEST-KDPRODSL                   
332000                    IF KDPRODSL-SPARE-PARTS                               
332100                       IF NYPON-ART-IDLEVNR = SPACE OR '9996 '            
332200                                         OR '9997 ' OR '9998 '            
332300                                         OR '9999 '                       
332400                          PERFORM S10-HAEMTA-ANSK-XXAT                    
332500                       ELSE                                               
332600                          MOVE NYPON-ART-IDLEVNR TO W-IDLEVNR             
332700                          PERFORM IMS-GU-LEVA01                           
332800                          IF SEGMENT-FINNS                                
332900                             MOVE LEVA01-LEV-IDANSK-PG(1)                 
333000                             TO NYPON-ART-IDANSK                          
333100                          ELSE                                            
333200                             PERFORM S10-HAEMTA-ANSK-XXAT                 
333300                          END-IF                                          
333400                       END-IF                                             
333500                    ELSE                                                  
333600                       PERFORM S10-HAEMTA-ANSK-XXAT                       
333700                    END-IF                                                
333800                 END-IF                                                   
333900              END-IF                                                      
334000           END-IF                                                         
334100        END-IF                                                            
334200     END-IF.                                                              
334300     EJECT                                                                
334400 EJ-DLET-ISRT-NYPON SECTION.                                              
334500     SKIP2                                                                
334600*    ÄNDRING SOM ORSAKAR RENSNING AV BASLAGER UTFÖRD                      
334700*                                                                         
334800     MOVE NYPON-ART-WDD201      TO SPAR-ART-WDD201                        
334900     PERFORM IMS-DLET-NYPON                                               
335000     MOVE ZERO                  TO  SPAR-ART-KVBASL                       
335100                                    SPAR-ART-TISTOMREG                    
335200     MOVE SPACE                  TO SPAR-ART-TEARTNOT-BASL                
335300                                    SPAR-ART-FLBASL                       
335400     MOVE SPAR-ART-WDD201        TO NYPON-ART-WDD201                      
335500                                                                          
335600     PERFORM IMS-ISRT-NYPON                                               
335700     PERFORM IMS-GHU-ARTG01-UTAN-GE                                       
335800                                                                          
335900     EJECT                                                                
336000     .                                                                    
336100 EM-FLYTTA-NYPON-TILL-MOD SECTION.                                        
336200     SKIP3                                                                
336300     MOVE NYPON-ART-FLPISK           TO MOD-FLPISK                        
336400     MOVE NYPON-ART-IDPROJK          TO MOD-IDPROJK                       
336500     MOVE NYPON-ART-IDARTNR-MOTSV    TO MOD-IDARTNR-MOTSV                 
336600     MOVE NYPON-ART-FLBYTES          TO MOD-FLBYTES                       
336700     MOVE NYPON-ART-KVARTVAGN        TO MOD-KVARTVAGN                     
336800     MOVE NYPON-ART-TEORSAK          TO MOD-TEORSAK-IN-UT                 
336900     MOVE NYPON-ART-KVPROG           TO MOD-KVPROG                        
337000     .                                                                    
337100     EJECT                                                                
337200 S02-ROER-EJ-VISADE-FAELT SECTION.                                        
337300     SKIP3                                                                
337400     MOVE MFS-ROER-EJ-FAELT          TO MOD-BEART                         
337500                                        MOD-IDBERED                       
337600                                        MOD-KDPRODSL                      
337700                                        MOD-KDSORT                        
337800                                        MOD-IDPROENH (1)                  
337900                                        MOD-IDPROENH (2)                  
338000                                        MOD-IDPROENH (3)                  
338100                                        MOD-KDUART                        
338200                                        MOD-IDPROJ                        
338300                                        MOD-FLPISK                        
338400                                        MOD-IDKAT  (1)                    
338500                                        MOD-IDKAT  (2)                    
338600                                        MOD-IDKAT  (3)                    
338700                                        MOD-FLLSRDEL                      
338800                                        MOD-IDPROJK                       
338900                                        MOD-KDBPSR                        
339000                                        MOD-TIFINLV                       
339100                                        MOD-TISOP                         
339200                                        MOD-IDFKNGRP                      
339300                                        MOD-IDPROJUP                      
339400                                        MOD-IDARTNR-MOTSV                 
339500                                        MOD-FLBYTES                       
339600                                        MOD-IDRITN                        
339700                                        MOD-KVARTVAGN                     
339800                                        MOD-IDAO (1)                      
339900                                        MOD-IDAO (2)                      
340000                                        MOD-IDAO (3)                      
340100                                        MOD-IDAO (4)                      
340200                                        MOD-IDAO (5)                      
340300                                        MOD-TEORSAK-IN-UT                 
340400                                        MOD-TEVARNOT-IN-UT                
340500                                        MOD-KVPROG                        
340600                                        MOD-KDAGE.                        
340700                                                                          
340800     EJECT                                                                
340900 S03-RENSA-MOD-INMATNINGSFAELT SECTION.                                   
341000     SKIP3                                                                
341100     MOVE MFS-RENSA-FAELT             TO MOD-IDBERED-IN                   
341200                                         MOD-KDPRODSL-IN                  
341300                                         MOD-KDSORT-IN                    
341400                                         MOD-IDPROENH-IN (1)              
341500                                         MOD-IDPROENH-IN (2)              
341600                                         MOD-IDPROENH-IN (3)              
341700                                         MOD-KDUART-IN                    
341800                                         MOD-IDPROJ-IN                    
341900                                         MOD-FLPISK-IN                    
342000                                         MOD-IDKAT-IN (1)                 
342100                                         MOD-IDKAT-IN (2)                 
342200                                         MOD-IDKAT-IN (3)                 
342300                                         MOD-FLLSRDEL-IN                  
342400                                         MOD-IDPROJK-IN                   
342500                                         MOD-KDBPSR-IN                    
342600                                         MOD-TISOP-IN                     
342700                                         MOD-IDFKNGRP-IN                  
342800                                         MOD-IDPROJUP-IN                  
342900                                         MOD-IDARTNR-MOTSV-IN             
343000                                         MOD-FLBYTES-IN                   
343100                                         MOD-IDRITN-IN                    
343200                                         MOD-KVARTVAGN-IN                 
343300                                         MOD-IDAO-IN (1)                  
343400                                         MOD-IDAO-IN (2)                  
343500                                         MOD-IDAO-IN (3)                  
343600                                         MOD-IDAO-IN (4)                  
343700                                         MOD-IDAO-IN (5)                  
343800                                         MOD-KDAGE-IN                     
343900                                         MOD-KVPROG-IN.                   
344000     EJECT                                                                
344100 S05-FORMATETS-ATTRIBUT SECTION.                                          
344200     SKIP3                                                                
344300     MOVE MFS-FORMATETS-ATTR         TO MOD-IDBERED-IN-ATTR               
344400                                        MOD-KDPRODSL-IN-ATTR              
344500                                        MOD-KDSORT-IN-ATTR                
344600                                        MOD-IDPROENH-IN-ATTR (1)          
344700                                        MOD-IDPROENH-IN-ATTR (2)          
344800                                        MOD-IDPROENH-IN-ATTR (3)          
344900                                        MOD-KDUART-IN-ATTR                
345000                                        MOD-IDPROJ-IN-ATTR                
345100                                        MOD-FLPISK-IN-ATTR                
345200                                        MOD-KVPROG-IN-ATTR                
345300                                        MOD-KDAGE-IN-ATTR                 
345400                                        MOD-IDKAT-IN-ATTR (1)             
345500                                        MOD-IDKAT-IN-ATTR (2)             
345600                                        MOD-IDKAT-IN-ATTR (3)             
345700                                        MOD-FLLSRDEL-IN-ATTR              
345800                                        MOD-IDPROJK-IN-ATTR               
345900                                        MOD-KDBPSR-IN-ATTR                
346000                                        MOD-TISOP-IN-ATTR                 
346100                                        MOD-IDFKNGRP-IN-ATTR              
346200                                        MOD-IDPROJUP-IN-ATTR              
346300                                        MOD-IDARTNR-MOTSV-IN-ATTR         
346400                                        MOD-FLBYTES-IN-ATTR               
346500                                        MOD-IDRITN-IN-ATTR                
346600                                        MOD-KVARTVAGN-IN-ATTR             
346700                                        MOD-IDAO-IN-ATTR  (1)             
346800                                        MOD-IDAO-IN-ATTR  (2)             
346900                                        MOD-IDAO-IN-ATTR  (3)             
347000                                        MOD-IDAO-IN-ATTR  (4)             
347100                                        MOD-IDAO-IN-ATTR  (5).            
347200     EJECT                                                                
347300 S10-HAEMTA-ANSK-XXAT SECTION.                                            
347400     SKIP2                                                                
347500     MOVE NEJ TO SW-ANSK-FINNS                                            
347600     MOVE SPAR-KDPRODSL TO W-KDPRODSL2                                    
347700     PERFORM IMS-GU-WLXXAT01                                              
347800     IF SEGMENT-FINNS                                                     
347900        PERFORM IMS-GNP-WLXXAT11                                          
348000        PERFORM UNTIL SEGMENT-SAKNAS OR SW-ANSK-FINNS = JA                
348100           IF (NYPON-ART-IDFKNGRP = XXAT-1138-IDFKNGRP-FOM                
348200               OR > XXAT-1138-IDFKNGRP-FOM) AND                           
348300               (NYPON-ART-IDFKNGRP = XXAT-1138-IDFKNGRP-TOM               
348400               OR < XXAT-1138-IDFKNGRP-TOM)                               
348500              MOVE XXAT-1138-IDANSK TO NYPON-ART-IDANSK                   
348600              MOVE JA TO SW-ANSK-FINNS                                    
348700           ELSE                                                           
348800              PERFORM IMS-GNP-WLXXAT11                                    
348900           END-IF                                                         
349000        END-PERFORM                                                       
349100     END-IF.                                                              
349200     EJECT                                                                
349300 S11-CHECK-SUPPLIER SECTION.                                              
349400     MOVE JA                  TO SW-SUPPLIER-OK                           
349500                                                                          
349600     MOVE 1                   TO LEV05-IX                                 
349700     PERFORM UNTIL LEV05-IX > 2                                           
349800       IF MSGI-KDARBTYP-SEC-IDLEV = TAB-KDARBTYP-LEV (LEV05-IX)           
349900         MOVE NEJ             TO SW-SUPPLIER-OK                           
350000         IF WS-KDPRODSL-LEV = TAB-KDPRODSL (LEV05-IX)                     
350100           MOVE JA            TO SW-SUPPLIER-OK                           
350200           MOVE 2             TO LEV05-IX                                 
350300         END-IF                                                           
350400       END-IF                                                             
350500       ADD 1                  TO LEV05-IX                                 
350600     END-PERFORM                                                          
350700                                                                          
350800     IF SW-SUPPLIER-OK = NEJ                                              
350900       MOVE NEJ                   TO SW-INPUT-RAETT                       
351000       MOVE MFS-NUM-FAELT-FEL     TO MOD-KDPRODSL-IN-ATTR                 
351100       MOVE MED-8 (SPAR-TEXT-IND) TO MOD-TEMFSINF                         
351110       MOVE FEL-ERR-FIELD       TO WS-ERROR-UPDX                          
351200     END-IF                                                               
351300     .                                                                    
351400     EJECT                                                                
351500 S99-WDATKONV SECTION.                                                    
351600     SKIP2                                                                
351700     CALL WDATKONV USING DAT-KDDATFORM                                    
351800                         DAT-I-TIDATUM                                    
351900                         DAT-O-TIDATUM                                    
352000                         DAT-KDSVAR.                                      
352100     EJECT                                                                
352200* IMS SEKTIONER                                                           
352300     SKIP3                                                                
352400 IMS-GET-MSG SECTION.                                                     
352500     SKIP2                                                                
352600     MOVE '  QC' TO GODK-STATUSKODER                                      
352700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
352800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
352900     PERFORM IMS-STATUS-KONTROLL.                                         
353000     SKIP3                                                                
353010 IMS-GET-WMSGKOM-MSG SECTION.                                             
353020                                                                          
353030     MOVE '  QD'   TO GODK-STATUSKODER                                    
353040     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
353050     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
353060     PERFORM IMS-STATUS-KONTROLL.                                         
353080     SKIP3                                                                
353100 IMS-INSERT-MSG SECTION.                                                  
353200     SKIP2                                                                
353300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
353400        MOVE '0'             TO MFS-KDHUVOMR                              
353500     END-IF                                                               
353600                                                                          
353700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
353800     MOVE SPACE TO GODK-STATUSKODER                                       
353900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
354000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
354100     PERFORM IMS-STATUS-KONTROLL.                                         
354200     EJECT                                                                
354210 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
354220                                                                          
354230     MOVE '  '  TO GODK-STATUSKODER                                       
354240     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
354250     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
354260     PERFORM IMS-STATUS-KONTROLL.                                         
354270     .                                                                    
354280     EJECT                                                                
354300 IMS-GU-LEVA01 SECTION.                                                   
354400     SKIP2                                                                
354500     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
354600             DELIMITED BY SIZE INTO SSA1                                  
354700     MOVE '  GE' TO GODK-STATUSKODER                                      
354800     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA1 SSA1                     
354900     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
355000     PERFORM IMS-STATUS-KONTROLL.                                         
355100     EJECT                                                                
355200 IMS-GHU-ARTC01 SECTION.                                                  
355300     SKIP2                                                                
355400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
355500             DELIMITED BY SIZE INTO SSA1                                  
355600     MOVE '  GE' TO GODK-STATUSKODER                                      
355700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC   SSA1                   
355800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
355900     PERFORM IMS-STATUS-KONTROLL.                                         
356000     SKIP3                                                                
356100 IMS-GHNP-ARTC11 SECTION.                                                 
356200     SKIP2                                                                
356300     MOVE 'WLARTC11 ' TO SSA1                                             
356400     MOVE '  ' TO GODK-STATUSKODER                                        
356500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-ARTC   SSA1                  
356600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
356700     PERFORM IMS-STATUS-KONTROLL.                                         
356800     EJECT                                                                
356900 IMS-GHNP-ARTC25 SECTION.                                                 
357000     SKIP2                                                                
357100     MOVE 'WLARTC11 ' TO SSA1                                             
357200     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
357300             DELIMITED BY SIZE INTO SSA2                                  
357400     MOVE '  GE' TO GODK-STATUSKODER                                      
357500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-ARTC   SSA1 SSA2             
357600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
357700     PERFORM IMS-STATUS-KONTROLL.                                         
357800     SKIP3                                                                
357900 IMS-DLET-ARTC SECTION.                                                   
358000     SKIP2                                                                
358100     MOVE '  '   TO GODK-STATUSKODER                                      
358200     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-ARTC                         
358300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
358400     PERFORM IMS-STATUS-KONTROLL.                                         
358500     SKIP3                                                                
358600 IMS-ISRT-ARTC25 SECTION.                                                 
358700     SKIP2                                                                
358800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
358900             DELIMITED BY SIZE INTO SSA1                                  
359000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
359100     MOVE 'WLARTC25 ' TO SSA3                                             
359200     MOVE '  ' TO GODK-STATUSKODER                                        
359300     CALL CBLTDLI USING ISRT ARTC-PCB DLI-IO-ARTC  SSA1 SSA2 SSA3         
359400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
359500     PERFORM IMS-STATUS-KONTROLL.                                         
359600     SKIP3                                                                
359700 IMS-GU-BENA11-BSEQ SECTION.                                              
359800     SKIP2                                                                
359900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
360000             DELIMITED BY SIZE INTO SSA1                                  
360100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
360200             DELIMITED BY SIZE INTO SSA2                                  
360300     MOVE '  ' TO GODK-STATUSKODER                                        
360400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD3  SSA1 SSA2                
360500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
360600     PERFORM IMS-STATUS-KONTROLL.                                         
360700     EJECT                                                                
360800 IMS-GN-ERSB01 SECTION.                                                   
360900     SKIP2                                                                
361000     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
361100                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
361200             DELIMITED BY SIZE INTO SSA1                                  
361300     MOVE '  GE' TO GODK-STATUSKODER                                      
361400     CALL CBLTDLI USING GN ERSB-PCB DLI-IO-AREA1 SSA1                     
361500     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
361600     PERFORM IMS-STATUS-KONTROLL.                                         
361700     SKIP2                                                                
361800 IMS-GHU-ARTG01-MED-GE SECTION.                                           
361900     SKIP2                                                                
362000     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
362100             DELIMITED BY SIZE INTO SSA1                                  
362200     MOVE '  GE' TO GODK-STATUSKODER                                      
362300     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA2 SSA1                    
362400     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
362500     PERFORM IMS-STATUS-KONTROLL.                                         
362600     SKIP2                                                                
362700 IMS-GHU-ARTG01-UTAN-GE SECTION.                                          
362800     SKIP2                                                                
362900     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
363000             DELIMITED BY SIZE INTO SSA1                                  
363100     MOVE '  ' TO GODK-STATUSKODER                                        
363200     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA2 SSA1                    
363300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
363400     PERFORM IMS-STATUS-KONTROLL.                                         
363500     EJECT                                                                
363600 IMS-GU-WLXXAQ01-UNIK SECTION.                                            
363700     SKIP2                                                                
363800     STRING 'WLXXAQ01(WDGXKEY  =' W-1131KEY-X ')'                         
363900             DELIMITED BY SIZE INTO SSA1                                  
364000     MOVE '  GE' TO GODK-STATUSKODER                                      
364100     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA1 SSA1                     
364200     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
364300     PERFORM IMS-STATUS-KONTROLL.                                         
364400     SKIP3                                                                
364500 IMS-GNP-WLXXAQ11-UNIK  SECTION.                                          
364600     SKIP2                                                                
364700     STRING 'WLXXAQ11(WDGXKEY  =' W-1132KEY-X ')'                         
364800             DELIMITED BY SIZE INTO SSA1                                  
364900     MOVE '  GE' TO GODK-STATUSKODER                                      
365000     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA1 SSA1                    
365100     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
365200     PERFORM IMS-STATUS-KONTROLL.                                         
365300     SKIP3                                                                
365400 IMS-GNP-WLXXAQ11-PROJK SECTION.                                          
365500     SKIP2                                                                
365600     STRING 'WLXXAQ11(IDPROJK  =' W-IDPROJK ')'                           
365700             DELIMITED BY SIZE INTO SSA1                                  
365800     MOVE '  GE' TO GODK-STATUSKODER                                      
365900     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA1 SSA1                    
366000     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
366100     PERFORM IMS-STATUS-KONTROLL.                                         
366200     SKIP2                                                                
366300 IMS-GNP-WLXXAQ11-PROJ SECTION.                                           
366400     SKIP2                                                                
366500     STRING 'WLXXAQ11(IDPROJ   =' W-IDPROJ ')'                            
366600             DELIMITED BY SIZE INTO SSA1                                  
366700     MOVE '  GE' TO GODK-STATUSKODER                                      
366800     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA1 SSA1                    
366900     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
367000     PERFORM IMS-STATUS-KONTROLL.                                         
367100     EJECT                                                                
367200 IMS-GU-WLXXAP01-UNIK SECTION.                                            
367300     SKIP2                                                                
367400     STRING 'WLXXAP01(WDGXKEY  =' W-1123KEY-X ')'                         
367500             DELIMITED BY SIZE INTO SSA1                                  
367600     MOVE '  GE' TO GODK-STATUSKODER                                      
367700     CALL CBLTDLI USING GU XXAP-PCB DLI-IO-AREA1 SSA1                     
367800     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
367900     PERFORM IMS-STATUS-KONTROLL.                                         
368000     SKIP3                                                                
368100 IMS-GNP-WLXXAP11 SECTION.                                                
368200     SKIP2                                                                
368300     MOVE 'WLXXAP11 ' TO  SSA1                                            
368400     MOVE '  GE' TO GODK-STATUSKODER                                      
368500     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA1 SSA1                    
368600     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
368700     PERFORM IMS-STATUS-KONTROLL.                                         
368800     EJECT                                                                
368900 IMS-GU-WLXXAT01 SECTION.                                                 
369000     SKIP2                                                                
369100     STRING 'WLXXAT01(WDGXKEY  =' W-1137KEY-X ')'                         
369200             DELIMITED BY SIZE INTO SSA1                                  
369300     MOVE '  GE' TO GODK-STATUSKODER                                      
369400     CALL CBLTDLI USING GU XXAT-PCB DLI-IO-AREA1 SSA1                     
369500     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
369600     PERFORM IMS-STATUS-KONTROLL.                                         
369700     SKIP2                                                                
369800 IMS-GNP-WLXXAT11 SECTION.                                                
369900     SKIP2                                                                
370000     MOVE 'WLXXAT11 '  TO SSA1                                            
370100     MOVE '  GE' TO GODK-STATUSKODER                                      
370200     CALL CBLTDLI USING GNP XXAT-PCB DLI-IO-AREA1 SSA1                    
370300     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
370400     PERFORM IMS-STATUS-KONTROLL.                                         
370500     EJECT                                                                
370600 IMS-REPL-ARTC SECTION.                                                   
370700     SKIP2                                                                
370800     MOVE '  '   TO GODK-STATUSKODER                                      
370900     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC                         
371000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
371100     PERFORM IMS-STATUS-KONTROLL.                                         
371200     SKIP3                                                                
371300 IMS-REPL-NYPON SECTION.                                                  
371400     SKIP2                                                                
371500     MOVE '  '   TO GODK-STATUSKODER                                      
371600     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA2                        
371700     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
371800     PERFORM IMS-STATUS-KONTROLL.                                         
371900     EJECT                                                                
372000 IMS-DLET-NYPON SECTION.                                                  
372100     SKIP2                                                                
372200     MOVE '  '   TO GODK-STATUSKODER                                      
372300     CALL CBLTDLI USING DLET ARTG-PCB DLI-IO-AREA2                        
372400     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
372500     PERFORM IMS-STATUS-KONTROLL.                                         
372600     SKIP3                                                                
372700 IMS-ISRT-NYPON SECTION.                                                  
372800     SKIP2                                                                
372900     MOVE 'WLARTG01 '     TO SSA1                                         
373000     MOVE '  '   TO GODK-STATUSKODER                                      
373100     CALL CBLTDLI USING ISRT ARTG-PCB DLI-IO-AREA2 SSA1                   
373200     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
373300     PERFORM IMS-STATUS-KONTROLL.                                         
373400     SKIP3                                                                
373500 IMS-ISRT-ZZAC SECTION.                                                   
373600     SKIP2                                                                
373700     MOVE 'WLZZAC01 '     TO SSA1                                         
373800     MOVE '  '   TO GODK-STATUSKODER                                      
373900     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-WDGZ01 SSA1                  
374000     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
374100     PERFORM IMS-STATUS-KONTROLL.                                         
374200     EJECT                                                                
374300 IMS-ISRT-WEEK SECTION.                                                   
374400     SKIP2                                                                
374500     STRING 'WL111701(WDGXKEY  =' W-111701-KEY ')'                        
374600          DELIMITED BY SIZE INTO SSA1                                     
374700     MOVE 'WL111711 ' TO SSA2                                             
374800     MOVE 'II  ' TO GODK-STATUSKODER                                      
374900     CALL CBLTDLI USING ISRT 1117-PCB DLI-IO-AREA3 SSA1 SSA2              
375000     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
375100     PERFORM IMS-STATUS-KONTROLL                                          
375200     .                                                                    
375300     SKIP3                                                                
375400 IMS-GHU-WEEK SECTION.                                                    
375500     SKIP2                                                                
375600     STRING 'WL111701(WDGXKEY  =' W-111701-KEY ')'                        
375700             DELIMITED BY SIZE INTO SSA1                                  
375800     STRING 'WL111711(IDARTNR  =' W-111711-KEY ')'                        
375900             DELIMITED BY SIZE INTO SSA2                                  
376000     MOVE '    ' TO GODK-STATUSKODER                                      
376100     CALL CBLTDLI USING GHU 1117-PCB DLI-IO-AREA3 SSA1 SSA2               
376200     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
376300     PERFORM IMS-STATUS-KONTROLL.                                         
376400     EJECT                                                                
376500 IMS-DLET-WEEK SECTION.                                                   
376600     SKIP2                                                                
376700     MOVE '  '   TO GODK-STATUSKODER                                      
376800     CALL CBLTDLI USING DLET 1117-PCB DLI-IO-AREA3                        
376900     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
377000     PERFORM IMS-STATUS-KONTROLL.                                         
377100     SKIP3                                                                
377200 IMS-REPL-WEEK SECTION.                                                   
377300     SKIP2                                                                
377400     MOVE '  '   TO GODK-STATUSKODER                                      
377500     CALL CBLTDLI USING REPL 1117-PCB DLI-IO-AREA3                        
377600     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
377700     PERFORM IMS-STATUS-KONTROLL.                                         
377800     EJECT                                                                
377900 IMS-GU-WDK701   SECTION.                                                 
378000     SKIP2                                                                
378100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
378200             DELIMITED BY SIZE INTO SSA1                                  
378300     MOVE '  GE' TO GODK-STATUSKODER                                      
378400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
378500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
378600     PERFORM IMS-STATUS-KONTROLL.                                         
378700     SKIP2                                                                
378800 IMS-GNP-WDK711 SECTION.                                                  
378900     MOVE   'WDK711   '  TO SSA1                                          
379000     MOVE '  GE' TO GODK-STATUSKODER                                      
379100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
379200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
379300     PERFORM IMS-STATUS-KONTROLL.                                         
379400     SKIP2                                                                
379500 IMS-GU-R220 SECTION.                                                     
379600                                                                          
379700     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
379800          DELIMITED BY SIZE INTO SSA1                                     
379900     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
380000          DELIMITED BY SIZE INTO SSA2                                     
380100     MOVE '  GE' TO GODK-STATUSKODER                                      
380200     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-2232 SSA1 SSA2            
380300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
380400     PERFORM IMS-STATUS-KONTROLL                                          
380500     .                                                                    
380600     SKIP2                                                                
380700 IMS-ISRT-WDR801   SECTION.                                               
380800                                                                          
380900     MOVE 'WDR801   ' TO SSA1                                             
381000     MOVE '  II' TO GODK-STATUSKODER                                      
381100     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
381200     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
381300     PERFORM IMS-STATUS-KONTROLL                                          
381400     .                                                                    
381500     EJECT                                                                
381600 IMS-GU-WDB601 SECTION.                                                   
381700                                                                          
381800     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
381900                    DELIMITED BY SIZE INTO SSA1                           
382000     MOVE '  GE' TO GODK-STATUSKODER                                      
382100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
382200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
382300     PERFORM IMS-STATUS-KONTROLL                                          
382400     .                                                                    
382500     EJECT                                                                
382600 IMS-ISRT-WDGX2264-NEW SECTION.                                           
382700                                                                          
382800     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
382900          DELIMITED BY SIZE INTO SSA1                                     
383000     MOVE 'WDGX2264 '      TO SSA2                                        
383100     MOVE '  II'           TO GODK-STATUSKODER                            
383200     CALL CBLTDLI USING ISRT WDR2-N-PCB DLI-IO-WDGX2264-N SSA1            
383300                                                          SSA2            
383400     MOVE WDR2-N-STATUS-CODE TO STATUS-WS                                 
383500     PERFORM IMS-STATUS-KONTROLL                                          
383600     .                                                                    
383700                                                                          
383800 IMS-ISRT-WDGX2266-NEW SECTION.                                           
383900                                                                          
384000     MOVE 'WDGX2266 '      TO SSA1                                        
384100     MOVE '  '             TO GODK-STATUSKODER                            
384200     CALL CBLTDLI USING ISRT WDR2-N-PCB DLI-IO-WDGX2266-N SSA1            
384300     MOVE WDR2-N-STATUS-CODE TO STATUS-WS                                 
384400     PERFORM IMS-STATUS-KONTROLL                                          
384500     .                                                                    
384600     EJECT                                                                
384700 IMS-GHU-WDGX2264-OLD SECTION.                                            
384800                                                                          
384900     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
385000          DELIMITED BY SIZE INTO SSA1                                     
385100     STRING 'WDGX2264(TISOP    =' W-TISOP-O-X ')'                         
385200          DELIMITED BY SIZE INTO SSA2                                     
385300     MOVE '  GE'              TO GODK-STATUSKODER                         
385400     CALL CBLTDLI USING GHU WDR2-O-PCB DLI-IO-WDGX2264-O SSA1 SSA2        
385500     MOVE WDR2-O-STATUS-CODE    TO STATUS-WS                              
385600     PERFORM IMS-STATUS-KONTROLL                                          
385700     .                                                                    
385800     EJECT                                                                
385900 IMS-GHNP-WDGX2266-OLD SECTION.                                           
386000                                                                          
386100     STRING 'WDGX2266(KY2266  >=' W-W2266KY-MIN-O-X                       
386200                    '&KY2266  <=' W-W2266KY-MAX-O-X ')'                   
386300          DELIMITED BY SIZE INTO SSA1                                     
386400     MOVE '  GE'              TO GODK-STATUSKODER                         
386500     CALL CBLTDLI USING GHNP WDR2-O-PCB DLI-IO-WDGX2266-O SSA1            
386600     MOVE WDR2-O-STATUS-CODE    TO STATUS-WS                              
386700     PERFORM IMS-STATUS-KONTROLL                                          
386800     .                                                                    
386900 IMS-GNP-WDGX2266     SECTION.                                            
387000                                                                          
387100     MOVE 'WDGX2266 '           TO  SSA1                                  
387200     MOVE '  GE'              TO GODK-STATUSKODER                         
387300     CALL CBLTDLI USING GNP WDR2-O-PCB DLI-IO-WDGX2266-O SSA1             
387400     MOVE WDR2-O-STATUS-CODE    TO STATUS-WS                              
387500     PERFORM IMS-STATUS-KONTROLL                                          
387600     .                                                                    
387700     EJECT                                                                
387800 IMS-DLET-WDGX2266-OLD SECTION.                                           
387900                                                                          
388000     MOVE '  '       TO GODK-STATUSKODER                                  
388100     CALL CBLTDLI USING DLET WDR2-O-PCB DLI-IO-WDGX2266-O                 
388200     MOVE WDR2-O-STATUS-CODE TO STATUS-WS                                 
388300     PERFORM IMS-STATUS-KONTROLL                                          
388400     .                                                                    
388500     EJECT                                                                
388600 IMS-DLET-WDGX2264-OLD SECTION.                                           
388700                                                                          
388800     MOVE '  '       TO GODK-STATUSKODER                                  
388900     CALL CBLTDLI USING DLET WDR2-O-PCB DLI-IO-WDGX2264-O                 
389000     MOVE WDR2-O-STATUS-CODE TO STATUS-WS                                 
389100     PERFORM IMS-STATUS-KONTROLL                                          
389200     .                                                                    
389300     EJECT                                                                
389400 IMS-STATUS-KONTROLL SECTION.                                             
389500     SET STATUS-IX TO 1                                                   
389600     SEARCH GODK-STATUS AT END CALL FELLOG                                
389700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
389800     END-SEARCH.                                                          
389900     EJECT                                                                
390000                                                                          
390100                                                                          
390200* DB2 SEKTIONER                                                           
390300     SKIP3                                                                
390400                                                                          
390500 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
390600     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
390700                                                                          
390800     MOVE 000100  TO GOOD-SQLCODECODES                                    
390900                                                                          
391000     EXEC SQL                                                             
391100         DECLARE TP1ARTK-CRS CURSOR FOR                                   
391200           SELECT  A.IDKAMP                                               
391300                  ,A.IDARTNR                                              
391400                  ,B.TISTADAT_KAMP                                        
391500                  ,B.TISTODAT_KAMP                                        
391600                  ,B.KDKAMP                                               
391700                                                                          
391800           FROM    TP1ARTK A                                              
391900                  ,TP1KAMP B                                              
392000                                                                          
392100           WHERE   A.IDARTNR = :W-IDARTNR                                 
392200               AND A.IDKAMP  =  B.IDKAMP                                  
392300                                                                          
392400           ORDER BY A.IDARTNR                                             
392500     END-EXEC                                                             
392600                                                                          
392700     MOVE 000100  TO GOOD-SQLCODECODES                                    
392800     MOVE SQLCODE TO SQLCODE-WS                                           
392900     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
393000     .                                                                    
393100     SKIP3                                                                
393200 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
393300     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
393400     SKIP2                                                                
393500     MOVE 000100  TO GOOD-SQLCODECODES                                    
393600     EXEC SQL                                                             
393700         FETCH TP1ARTK-CRS INTO                                           
393800                    :TP1KAMP-IDKAMP                                       
393900                   ,:TP1ARTK-IDARTNR                                      
394000                   ,:TP1KAMP-TISTADAT-KAMP                                
394100                   ,:TP1KAMP-TISTODAT-KAMP                                
394200                   ,:TP1KAMP-KDKAMP                                       
394300     END-EXEC                                                             
394400                                                                          
394500     MOVE SQLCODE TO SQLCODE-WS                                           
394600     PERFORM DB2-STATUS-CHECK                                             
394700     .                                                                    
394800     SKIP3                                                                
394900 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
395000     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
395100                                                                          
395200     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
395300     .                                                                    
395400     EJECT                                                                
395500 DB2-STATUS-CHECK  SECTION.                                               
395600                                                                          
395700     SET SQLCODE-IX TO 1                                                  
395800     SEARCH GOOD-SQLCODE                                                  
395900       AT END                                                             
396000*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
396100*         DELIMITED BY SIZE INTO ERROR-TEXT                               
396200          CALL FELLOG                                                     
396300       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
396400     END-SEARCH                                                           
396500     .                                                                    
396600     EJECT                                                                
396700                                                                          
396800*    -COPY WY2000P1                                                       
396900     EJECT                                                                
397000*    -COPY WY2000P3                                                       
