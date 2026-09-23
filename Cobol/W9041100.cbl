000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W9041100.                                                
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
001600*             TRANSAKTION: W90411T                                        
001700*                          W90411U                                        
001800*                                                                         
001900*                     MID: W90411I1                                       
002000*         UTDATA.                                                         
002100*                     MOD: W90411O1                                       
002200*         SUBPROGRAM.                                                     
002300*               DYNAMISKA: FELLOG                                         
002400*                          CBLTDLI                                        
002500*                          WDATKONV                                       
002600*   ************************************************************          
002700*   *  THIS PGM IS A COPY OF PGM W1011700 FOR USE              *          
002800*   *  BY SPIE2. THE COPY WAS MADE MARS-04 BY                  *          
002900*   *  JOHAN NIHLBLAD. SOME PIECES OF THE PGM HAS              *          
003000*   *  BEEN REMOVED WHEN COPIED TO THIS PGM NAME.              *          
003100*   ************************************************************          
003200*                                                                         
003300*    ÄNDRINGAR.                                                           
003400*      FÄLTEN ART-FLRITB OCH ART-TIRITB  HAR UTGÅTT                       
003500*       ( SE KOMMENTAR I W1140200)                                        
003600*      DESSA FÄLT INITIERAS MED NOLL OCH SPACE  TILLSV.                   
003700*                                                                         
003800*    ÄNDRING:                                                             
003900*        2005-FEB  ETRACKER=1476814.  VISA KAMPANJ-INFO                   
004000*                                     TILLAGT DB2-LÄSNING  /C.E.          
004100*                                                                         
004200*    ÄNDRING:                                                             
004300*        2006-JAN  ETRACKER=1986420. SKAPA LEV-PLANELARM VID              
004400*                  FÖRÄNDRING AV TIFINLV OCH NÄR ARTIKELN SAM-            
004500*                  TIDIGT ÄR ERSÄTTANDE I ERS MED EK 01-09.               
004600*                  BÅDE ERSATT OCH ERSÄTTANDE ARTIKEL SKALL LARMAS        
004700*                  MED ORSAK=09.     TILLÄGG AV WLXXBJ11-ISRT FÖR         
004800*                                    LARM PÅ 2204 HTR          /CE        
004900*                                                                         
005000*    ÄNDRING:                                                             
005100*        2006-JAN. TILLÄGG AV EKONOMISK HÄNDELSE KDEKOHT='M21'            
005200*                  FÖR US OCH CA. /MA                                     
005300*                                                                         
005400*    ÄNDRING:                                                             
005500*        2006-FEB. RENSNING AV KOD FÖR HÄNDELSER PÅ XXAU OCH XXAV         
005600*                  FÖR UTGÅENDE SYSTEM "TIKO". /BL                        
005700*                                                                         
005800*    ÄNDRING:                                                             
005900*        2012-JAN  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1           
006000*                                                                         
006100*    ÄNDRING:                                                             
006200*        2014-SOMMAR ETRACKER 10150487 CHANGE PUBWEEK                     
006300*        BYTE MID/MOD-TIFINLV TILL TISOP                                  
006400*                                                                         
006500                                                                          
006600     EJECT                                                                
006700 ENVIRONMENT DIVISION.                                                    
006800     SKIP3                                                                
006900 DATA DIVISION.                                                           
007000 WORKING-STORAGE SECTION.                                                 
007100*    -COPY WY2000W1                                                       
007200     SKIP3                                                                
007300*    -COPY WY2000W3                                                       
007400     SKIP3                                                                
007500*                                                                         
007600******************************************************************        
007700*          W O R K I N G  S T O R A G E  S E C T I O N           *        
007800******************************************************************        
007900*                                                                         
008000 77  PROGRAM-NAMN                PIC X(08)  VALUE 'W9041100'.             
008100 77  JA                          PIC X(01)  VALUE 'J'.                    
008200 77  NEJ                         PIC X(01)  VALUE 'N'.                    
008300 77  WS-IDARTNR                  PIC X(09).                               
008400 77  WS-IDARTNR-OPACKAT          PIC 9(09)  VALUE ZERO.                   
008500 77  WS-IDLEVNR                  PIC X(05).                               
008600 77  WS-TVA-AAR                  PIC S9(05)  VALUE +10000.                
008700 77  WS-GAMMAL-ART               PIC S9(07)  VALUE  ZERO.                 
008800 77  WS-KDRESBED                 PIC X(1)    VALUE  SPACE.                
008900 77  WS-KDPRODSL                 PIC S9(3)   VALUE  ZERO COMP-3.          
009000 77  WS-KDPSLLOC-OLD             PIC 9(2)    VALUE  ZERO.                 
009100 77  W-IDSEKVNR                  PIC S9(3)   VALUE  ZERO COMP-3.          
009200 77  WS-A17-KDPRODSL             PIC  9(2)   VALUE  ZERO.                 
009300                                                                          
009400 01  WS-TEST-IDFKNGRP            PIC 9(4)    VALUE  ZERO.                 
009500 01  FILLER REDEFINES WS-TEST-IDFKNGRP.                                   
009600     03  FILLER                  PIC 9(3).                                
009700     03  WS-SISTA-SIFFRAN        PIC 9(1).                                
009800                                                                          
009900 01  ARBETSAREOR.                                                         
010000     03 FILLER                   PIC X(16)   VALUE                        
010100                                             'WS-DB2-SEKTION'.            
010200     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
010300                                                                          
010400     03  WS-DAGENS-AAAAMMDD      PIC 9(8).                                
010500     03  WS-JMFR-AAAAMMDD        PIC 9(8).                                
010600     03  FILLER REDEFINES WS-JMFR-AAAAMMDD.                               
010700        05 FILLER                PIC 9(2).                                
010800        05 WS-JMFR-AA            PIC 9(2).                                
010900        05 FILLER                PIC 9(4).                                
011000                                                                          
011100     03  DAGENS-TIAAAAMMDD       PIC 9(8).                                
011200     03  WS-HHMMSSTH             PIC 9(8)    VALUE ZERO.                  
011300                                                                          
011400     03  WS-FLAGGA-Q-KAMP        PIC X(1)    VALUE SPACE.                 
011500     03  WS-FLAGGA-W-S-KAMP      PIC X(1)    VALUE SPACE.                 
011600     03  WS-KVLS-REM             PIC S9(7)   VALUE ZERO COMP-3.           
011700     03  WS-ANTAL-KAMP           PIC 9(7)    VALUE ZERO.                  
011800                                                                          
011900     03  WS-TEMFSINF             PIC X(42)   VALUE SPACE.                 
012000     03  WS-TEMFSINF-KAMP        PIC X(10)   VALUE SPACE.                 
012100     03  WS-TEMFSINF-SPLIT       PIC X(3)    VALUE '-- '.                 
012200                                                                          
012300*                                                                         
012400*01    -COPY WWPRODSL                                                     
012500*01    -COPY WWPRODSL -PRE WS-                                            
012600*      --- VALID IDDC CODES                                               
012700*                                                                         
012800*01    -COPY WWDC99                                                       
012900*01    -COPY WWDCKONS                                                     
013000       EJECT                                                              
013100 01  WS-IDTRANS                  PIC X(04).                               
013200     88  EGEN-BILD                          VALUE '9411'.                 
013300                                                                          
013400     EJECT                                                                
013500                                                                          
013600 01  WS-KDBPSR-GODK              PIC 9(01).                               
013700     88 KDBPSR-GODK                         VALUE 1                       
013800                                                  2                       
013900                                                  3                       
014000                                                  4                       
014100                                                  5                       
014200                                                  6                       
014300                                                  7                       
014400                                                  8.                      
014500     EJECT                                                                
014600 01  WS-KDSORT-GODK              PIC X(02).                               
014700     88 KDSORT-GODK                         VALUE 'ST'                    
014800                                                  'PA'                    
014900                                                  'SA'                    
015000                                                  'KG'                    
015100                                                  'M '                    
015200                                                  ' M'                    
015300                                                  'L '                    
015400                                                  ' L'                    
015500                                                  'MM'                    
015600                                                  'G '                    
015700                                                  ' G'                    
015800                                                  'C2'                    
015900                                                  'M2'                    
016000                                                  'ML'                    
016100                                                  'SW'                    
016200                                                  'TM'                    
016300                                                  'PA'                    
016400                                                  'HW'.                   
016500                                                                          
016600 77  WS-KDP-KDUART               PIC X      VALUE SPACE.                  
016700                                                                          
016800 01  WS-KDUART-GODK              PIC X.                                   
016900     88 KDUART-GODK                         VALUE 'A'                     
017000                                                  'M'                     
017100                                                  'S'                     
017200                                                  'P'                     
017300                                                  'K'                     
017400                                                  'B'                     
017500                                                  ' '.                    
017600                                                                          
017700     EJECT                                                                
017800*                                                                         
017900******************************************************************        
018000*                     S W I T C H A R                            *        
018100******************************************************************        
018200*                                                                         
018300 01  SWITCHAR.                                                            
018400     05  SW-INPUT-RAETT              PIC X(01)  VALUE 'J'.                
018500     05  SW-ARTIKEL-FINNS-PA-NYPON   PIC X(01)  VALUE 'N'.                
018600     05  SW-TRAFF                    PIC X(01)  VALUE 'N'.                
018700     05  SW-GODK-IDPROJK             PIC X(01)  VALUE 'N'.                
018800     05  SW-ANSK-FINNS               PIC X(01)  VALUE 'N'.                
018900     05  SW-TISERLEV-FINNS           PIC X(01)  VALUE 'N'.                
019000     05  SW-REPL-NYPON               PIC X(01)  VALUE 'N'.                
019100     05  SW-DLET-ISRT-NYPON          PIC X(01)  VALUE 'N'.                
019200     05  SW-ISRT-TRANS-TILL-KDP      PIC X(01)  VALUE 'N'.                
019300     05  SW-KOLLA-KDPSLLOC           PIC X(01)  VALUE 'N'.                
019400     05  SW-KOLLA-TISOP              PIC X(01)  VALUE 'N'.                
019500     05  SW-LARMA                    PIC X(01)  VALUE 'N'.                
019600     05  SW-LARM-09                  PIC X(01)  VALUE 'N'.                
019700                                                                          
019800 01  SW-PROJ-GODK                    PIC X(01).                           
019900     88  PROJ-GODK                              VALUE 'J'.                
020000                                                                          
020100 01  HANDELSE-SW                     PIC X(01).                           
020200     88  HANDELSE-OK                            VALUE 'J'.                
020300     88  HANDELSE-EJ-OK                         VALUE 'N'.                
020400                                                                          
020500 01  SW-KDSORT-AENDRAD-TILL-FRAN-SA  PIC X(01).                           
020600     88  KDSORT-AENDRAD-TILL-FRAN-SATS          VALUE 'J'.                
020700                                                                          
020800*                                                                         
020900******************************************************************        
021000*               D I V E R S E  S P A R F Ä L T                   *        
021100******************************************************************        
021200*                                                                         
021300 01  SPAR-DIVERSE.                                                        
021400                                                                          
021500                                                                          
021600     05  SPAR-TEXT-IND               PIC 9(01)  VALUE ZERO.               
021700     05  SPAR-TISERLEV-IND           PIC 9(01)  VALUE ZERO.               
021800     05  SPAR-TISERLEV-IND-MAX       PIC 9(01)  VALUE 5.                  
021900     05  SPAR-TILL-IND               PIC 9(01)  VALUE ZERO.               
022000     05  SPAR-FRAN-IND               PIC 9(01)  VALUE ZERO.               
022100     05  SPAR-IDPROJK-IND            PIC 9(02)  VALUE ZERO.               
022200     05  SPAR-IDPROJK-IND-MAX        PIC 9(02)  VALUE 75.                 
022300     05  SPAR-DAGENS-DATUM           PIC 9(07)  VALUE ZERO.               
022400     05  SPAR-DAGENS-AAVV.                                                
022500        10  SPAR-DAGENS-AA           PIC 9(02)  VALUE ZERO.               
022600        10  SPAR-DAGENS-VV           PIC 9(02)  VALUE ZERO.               
022700     05  SPAR-DAGENS-AAVV-R  REDEFINES  SPAR-DAGENS-AAVV                  
022800                                     PIC 9(04).                           
022900     05  SPAR-FLERS                  PIC X(01)  VALUE 'N'.                
023000     05  SPAR-FLIART                 PIC X(01)  VALUE 'N'.                
023100     05  SPAR-KDERS                  PIC 9(02)  VALUE ZERO.               
023200     05  SPAR-IDANSK                 PIC 9(03)  VALUE ZERO.               
023300     05  SPAR-IDRITN                 PIC X(08)  VALUE SPACE.              
023400     05  SPAR-FLPISK                 PIC X(01)  VALUE 'N'.                
023500     05  SPAR-KVPROG                 PIC 9(7)   VALUE ZERO.               
023600     05  SPAR-FLLSRDEL-OLD           PIC X(01)  VALUE SPACE.              
023700     05  SPAR-IDPROJK-TEST           PIC X(04)  VALUE SPACE.              
023800     05  SPAR-IDPROJK-OLD            PIC X(04)  VALUE SPACE.              
023900     05  SPAR-IDPROJK                PIC X(04)  VALUE SPACE.              
024000     05  SPAR-IDPROJOBJ              PIC X(04)  VALUE SPACE.              
024100     05  SPAR-KDSORT                 PIC X(02)  VALUE SPACE.              
024200     05  WS-KDSORT-OLD               PIC X(2)   VALUE SPACE.              
024300     05  SPAR-IDFKNGRP               PIC 9(05)  VALUE ZERO.               
024400     05  WS-IDFKNGRP-OLD             PIC 9(4)   VALUE ZERO.               
024500     05  SPAR-KDAGE                  PIC X      VALUE SPACE.              
024600     05  SPAR-FLRELSP                PIC X      VALUE SPACE.              
024700     05  SPAR-KDPRODSL               PIC 9(02)  VALUE ZERO.               
024800     05  SPAR-KDPRODSL-OLD           PIC 9(02)  VALUE ZERO.               
024900     05  SPAR-IDAO1                  PIC X(10)  VALUE SPACE.              
025000     05  SPAR-IDBERED                PIC 9(02)  VALUE ZERO.               
025100     05  SPAR-IDBERED-OLD            PIC 9(02)  VALUE ZERO.               
025200     05  SPAR-IDARTNR-MOTSV          PIC 9(08)  VALUE ZERO.               
025300     05  SPAR-FLBYTES                PIC X(01)  VALUE 'N'.                
025400     05  SPAR-TEORSAK                PIC X(50)  VALUE SPACE.              
025500     05  SPAR-KDRESBED               PIC X(01)  VALUE SPACE.              
025600     05  SPAR-KDBPSR                 PIC X(01)  VALUE SPACE.              
025700     05  SPAR-KDBPSR-OLD             PIC X(01)  VALUE SPACE.              
025800     05  SPAR-KDUART                 PIC X      VALUE SPACE.              
025900     05  SPAR-KDUART-OLD             PIC X      VALUE SPACE.              
026000     05  SPAR-IDLEVNR                PIC X(05)  VALUE ZERO.               
026100     05  SPAR-IDPROJ                 PIC X(04)  VALUE SPACE.              
026200     05  SPAR-IDPROJ-OLD             PIC X(04)  VALUE SPACE.              
026300     05  SPAR-RAD-IND                PIC 9(02)  VALUE ZERO.               
026400     05  SPAR-RAD-IND-MAX            PIC 9(02)  VALUE 12.                 
026500     05  SPAR-IDPROENH-IND-MAX       PIC 9(01)  VALUE 3.                  
026600     05  SPAR-IDKAT-IND-MAX          PIC 9(01)  VALUE 3.                  
026700     05  SPAR-IDAO-IND-MAX           PIC 9(01)  VALUE 5.                  
026800                                                                          
026900     05  SPAR-FLLSRDEL               PIC X(01)  VALUE 'J'.                
027000                                                                          
027100     05  SPAR-PRARTSTD               PIC 9(7)V9(2) VALUE ZERO.            
027200                                                                          
027300     05  SPAR-IDPROENH1              PIC X(08)  VALUE SPACE.              
027400     05  SPAR-IDPROENH-AREA.                                              
027500         10  SPAR-IDPROENH           PIC X(8) OCCURS 3                    
027600                           INDEXED BY SPAR-IDPROENH-IND.                  
027700                                                                          
027800  01  SPAR-DASOP-AAAAMMDD         PIC 9(08)  VALUE ZERO.                  
027900                                                                          
028000  01  SPAR-TISOP-AAVVD            PIC 9(05)  VALUE ZERO.                  
028100                                                                          
028200  01  SPAR-TISOP-AAVV.                                                    
028300    10  SPAR-TISOP-AA             PIC 9(02)  VALUE ZERO.                  
028400    10  SPAR-TISOP-VV             PIC 9(02)  VALUE ZERO.                  
028500  01  SPAR-TISOP-AAVV-R    REDEFINES  SPAR-TISOP-AAVV                     
028600                                  PIC 9(04).                              
028700                                                                          
028800  01  XX-TISOP                    PIC X(05)  VALUE SPACE.                 
028900  01  FILLER  REDEFINES  XX-TISOP.                                        
029000     05  XX-AAR                   PIC X(02).                              
029100     05  XX-VV                    PIC X(02).                              
029200     05  XX-DAG                   PIC X(01).                              
029300                                                                          
029400******************************************************************        
029500*           D Y N A M I S K A  S U B P R O G R A M               *        
029600******************************************************************        
029700*                                                                         
029800 01  DYNAMISKA-SUBPROGRAM.                                                
029900     05  WDATKONV                PIC X(08)  VALUE 'WDATKONV'.             
030000     05  CBLTDLI                 PIC X(08)  VALUE 'CBLTDLI '.             
030100     05  FELLOG                  PIC X(08)  VALUE 'FELLOG  '.             
030200     05  WKPSKONV                PIC X(08)  VALUE 'WKPSKONV'.             
030300     05  W005INIT                PIC X(08)  VALUE 'W005INIT'.             
030400     03  W100LPC                 PIC X(08)  VALUE 'W100LPC'.              
030500                                                                          
030600     EJECT                                                                
030700                                                                          
030800 01  FILLER                      PIC X(16)   VALUE 'W100LPC-AREA'.        
030900*01  LPC-AREA  -COPY W100LPC                                              
031000     EJECT                                                                
031100******************************************************************        
031200*    F E L M E D D E L A N D E N                                          
031300******************************************************************        
031400*                                                                         
031500 01  MEDDELANDE.                                                          
031600     03 W-FEL-1.                                                          
031700        05  FILLER                  PIC X(32) VALUE                       
031800            'ARTIKELNUMMER EJ NUMERISKT      '.                           
031900        05  FILLER                  PIC X(32) VALUE                       
032000            'PARTNUMBER NOT NUMERIC          '.                           
032100     03  FILLER  REDEFINES  W-FEL-1.                                      
032200        05  FEL-1                   PIC X(32)  OCCURS 2.                  
032300                                                                          
032400     03 W-FEL-2.                                                          
032500        05  FILLER                  PIC X(32) VALUE                       
032600            'UPPLYSTA FÄLT FEL               '.                           
032700        05  FILLER                  PIC X(32) VALUE                       
032800            'CORRECT HIGH LIGHTED FIELDS     '.                           
032900     03  FILLER  REDEFINES  W-FEL-2.                                      
033000        05  FEL-2                   PIC X(32)  OCCURS 2.                  
033100                                                                          
033200     03 W-FEL-3.                                                          
033300        05  FILLER                  PIC X(32) VALUE                       
033400            'ARTIKELNUMMER SAKNAS PÅ ARTREG  '.                           
033500        05  FILLER                  PIC X(32) VALUE                       
033600            'THIS PART IS NOT IN THE DATABASE'.                           
033700     03  FILLER  REDEFINES  W-FEL-3.                                      
033800        05  FEL-3                   PIC X(32)  OCCURS 2.                  
033900                                                                          
034000     03 W-FEL-4.                                                          
034100        05  FILLER                  PIC X(32) VALUE                       
034200            'ARTIKEL ERSATT, -RENSAD         '.                           
034300        05  FILLER                  PIC X(32) VALUE                       
034400            'PARTNUMBER SUPERSEEDED, -DELETED'.                           
034500     03  FILLER  REDEFINES  W-FEL-4.                                      
034600        05  FEL-4                   PIC X(32)  OCCURS 2.                  
034700                                                                          
034800     03 W-FEL-5.                                                          
034900        05  FILLER                  PIC X(40) VALUE                       
035000            'SORT ÄNDRAD TILL/FRÅN SATS, KOLLA RASA  '.                   
035100        05  FILLER                  PIC X(40) VALUE                       
035200            'SORT CHANGED TO/FROM KIT, CHECK RASA    '.                   
035300     03  FILLER  REDEFINES  W-FEL-5.                                      
035400        05  FEL-5                   PIC X(40)  OCCURS 2.                  
035500                                                                          
035600     03 W-MED-1.                                                          
035700        05  FILLER                  PIC X(32) VALUE                       
035800            'TRYCK PF11 FÖR UPPDATERING      '.                           
035900        05  FILLER                  PIC X(32) VALUE                       
036000            'PRESS PF11 FOR UPDATING         '.                           
036100     03  FILLER  REDEFINES  W-MED-1.                                      
036200        05  MED-1                   PIC X(32)  OCCURS 2.                  
036300                                                                          
036400     03 W-MED-2.                                                          
036500        05  FILLER                  PIC X(32) VALUE                       
036600            'UPPDATERING UTFÖRD '.                                        
036700        05  FILLER                  PIC X(32) VALUE                       
036800            'UPDATED                         '.                           
036900     03  FILLER  REDEFINES  W-MED-2.                                      
037000        05  MED-2                   PIC X(32)  OCCURS 2.                  
037100                                                                          
037200     03 W-MED-5.                                                          
037300        05  FILLER                  PIC X(32) VALUE                       
037400            'ARTIKEL ERSATT                '.                             
037500        05  FILLER                  PIC X(32) VALUE                       
037600            'PARTNUMBER SUPERSEEDED        '.                             
037700     03  FILLER  REDEFINES  W-MED-5.                                      
037800        05  MED-5                   PIC X(32)  OCCURS 2.                  
037900                                                                          
038000     03 W-MED-6.                                                          
038100        05  FILLER                  PIC X(32) VALUE                       
038200            'ARTIKEL AVSLAGEN              '.                             
038300        05  FILLER                  PIC X(32) VALUE                       
038400            'REJECTED PARTNUMBER             '.                           
038500     03  FILLER  REDEFINES  W-MED-6.                                      
038600        05  MED-6                   PIC X(32)  OCCURS 2.                  
038700                                                                          
038800     03 W-MED-7.                                                          
038900        05  FILLER                  PIC X(35) VALUE                       
039000            'ARTIKEL INGÅR I 1002-SATS         '.                         
039100        05  FILLER                  PIC X(35) VALUE                       
039200            'PART NO IS INCLUDED IN A 1002-KIT '.                         
039300     03  FILLER  REDEFINES  W-MED-7.                                      
039400        05  MED-7                   PIC X(35)  OCCURS 2.                  
039500                                                                          
039600     03 W-MED-8.                                                          
039700        05  FILLER                  PIC X(35) VALUE                       
039800            'UPPDATERING EJ TILLÅTEN           '.                         
039900        05  FILLER                  PIC X(35) VALUE                       
040000            'UPDATE NOT ALLOWED                '.                         
040100     03  FILLER  REDEFINES  W-MED-8.                                      
040200        05  MED-8                   PIC X(35)  OCCURS 2.                  
040300     03 W-MED-9.                                                          
040400        05  FILLER                  PIC X(35) VALUE                       
040500            'PS SKICKADE TILL VECKOUPPDAT-BAS '.                          
040600        05  FILLER                  PIC X(35) VALUE                       
040700            'PS SENT TO WEEKLY UPDATE BASE  '.                            
040800     03  FILLER  REDEFINES  W-MED-9.                                      
040900        05  MED-9                   PIC X(35)  OCCURS 2.                  
041000                                                                          
041100                                                                          
041200                                                                          
041300****           MED-91 KAN KOMBINERAS MED MEDDELANDENA                     
041400****                  MED-2, MED-16, MED-18, MED-30 VID LÄS-TRANS         
041500     03    W-MED-91.                                                      
041600         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
041700         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
041800     03    FILLER REDEFINES W-MED-91.                                     
041900         05    MED-91        PIC X(10) OCCURS 2.                          
042000                                                                          
042100****           MED-92 KAN KOMBINERAS MED MEDDELANDENA                     
042200****                  MED-2, MED-16, MED-18, MED-30 VID LÄS-TRANS         
042300     03    W-MED-92.                                                      
042400         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
042500         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
042600     03    FILLER REDEFINES W-MED-92.                                     
042700         05    MED-92        PIC X(10) OCCURS 2.                          
042800                                                                          
042900                                                                          
043000     EJECT                                                                
043100*                                                                         
043200******************************************************************        
043300*   C O P Y T E X T E R  F Ö R   K D P  - T R A N S E N          *        
043400******************************************************************        
043500*                                                                         
043600 01  KDP-FILLER.                                                          
043700     03  FILLER                  PIC X(16)   VALUE 'KDP-COPY '.           
043800     SKIP3                                                                
043900*01  -COPY W10111                                                         
044000     EJECT                                                                
044100*01  AREA -COPY W092W001       -PRE W092-                                 
044200     EJECT                                                                
044300*                                                                         
044400******************************************************************        
044500*   C O P Y T E X T   NYPON-ROT SPARAS EVENTUELLT                *        
044600******************************************************************        
044700*                                                                         
044800 01  IMS-WS-0.                                                            
044900     03  FILLER                  PIC X(16)   VALUE 'NYPONCOPY'.           
045000     SKIP3                                                                
045100*01  AREA -COPY WDD201          -PRE SPAR-                                
045200     EJECT                                                                
045300*                                                                         
045400******************************************************************        
045500*                    C O P Y T E X T E R    (DYNAMISKA ANROP)    *        
045600******************************************************************        
045700*                                                                         
045800 01  IMS-WS-1.                                                            
045900     03  FILLER                  PIC X(16)   VALUE 'RDAT-AREA'.           
046000     SKIP3                                                                
046100*01  -COPY WDATAREA                                                       
046200     EJECT                                                                
046300******************************************************************        
046400 01  IMS-WS-2.                                                            
046500     03  FILLER                  PIC X(16)   VALUE 'RKPS-AREA'.           
046600     SKIP3                                                                
046700*01  -COPY WKPSAREA                                                       
046800     EJECT                                                                
046900******************************************************************        
047000 01  IMS-WS-3.                                                            
047100*                    ****   PARAMETRAR TILL W005INIT                      
047200     03  FILLER                  PIC X(16)   VALUE 'WMSGINIT '.           
047300*01  -COPY WMSGINIT                                                       
047400     EJECT                                                                
047500******************************************************************        
047600*              N Y C K L A R  T I L L  D L I                     *        
047700******************************************************************        
047800*                                                                         
047900 01  NYCKLAR-TILL-DLI.                                                    
048000     03  W-IDARTNR-X.                                                     
048100         05  W-IDARTNR            PIC S9(09) COMP-3 VALUE ZERO.           
048200                                                                          
048300     03  W-KDANSKQ-X.                                                     
048400         05  W-KDANSKQ            PIC  X(01)        VALUE '1'.            
048500                                                                          
048600     03  W-KDNOTTYP-X.                                                    
048700         05  W-KDNOTTYP           PIC  S9(01)  COMP-3 VALUE ZERO.         
048800                                                                          
048900     03  W-IDLEVNR-X.                                                     
049000         05  W-IDLEVNR            PIC X(05)  VALUE SPACE.                 
049100                                                                          
049200     03  W-WDD901KY-X.                                                    
049300         05  W-IDARTNR-D9         PIC S9(09) COMP-3 VALUE ZERO.           
049400         05  W-IDDC-D9            PIC X(2)   VALUE SPACE.                 
049500                                                                          
049600     03  W-IDSKYLT-X.                                                     
049700         05  W-IDSKYLT            PIC X(03)  VALUE SPACE.                 
049800                                                                          
049900     03  W-IDDC-X.                                                        
050000         05  W-IDDC               PIC X(2)   VALUE SPACE.                 
050100                                                                          
050200     03  W-IDDC-A17-X.                                                    
050300         05  W-IDDC-A17           PIC X(2)   VALUE SPACE.                 
050400                                                                          
050500     03  W-IDLAND-X.                                                      
050600         05  W-IDLAND             PIC X(2)   VALUE SPACE.                 
050700                                                                          
050800     03  W-IDLOGLOP-X.                                                    
050900         05  W-IDLOGLOP           PIC S9(01) COMP-3 VALUE ZERO.           
051000                                                                          
051100     03  W-WDD7A1KY-MIN.                                                  
051200         05  W-IDARTNR-MIN7       PIC S9(9)  COMP-3 VALUE ZERO.           
051300         05  FILLER               PIC X(7)   VALUE LOW-VALUE.             
051400                                                                          
051500     03  W-WDD7A1KY-MAX.                                                  
051600         05  W-IDARTNR-MAX7       PIC S9(9)  COMP-3                       
051700                                  VALUE ZERO.                             
051800         05  FILLER               PIC X(7)   VALUE HIGH-VALUE.            
051900                                                                          
052000     03  W-1123KEY-X.                                                     
052100         05  FILLER               PIC X(04)  VALUE '1123'.                
052200         05  W-KDPRODSL-1123      PIC S9(3)  VALUE 11   COMP-3.           
052300         05  W-IDPROJ-1123        PIC X(04)  VALUE SPACE.                 
052400         05  FILLER               PIC X(20)  VALUE LOW-VALUE.             
052500                                                                          
052600     03  W-1131KEY-X.                                                     
052700         05  FILLER               PIC X(04)  VALUE '1131'.                
052800         05  W-KDPRODSL1          PIC S9(3)  VALUE ZERO COMP-3.           
052900         05  FILLER               PIC X(24)  VALUE LOW-VALUE.             
053000                                                                          
053100     03  W-1132KEY-X.                                                     
053200         05  W-IDPROJK            PIC X(04)  VALUE SPACE.                 
053300         05  W-IDPROJOBJ          PIC X(04)  VALUE SPACE.                 
053400         05  W-IDPROJ             PIC X(04)  VALUE SPACE.                 
053500         05  FILLER               PIC X(03)  VALUE LOW-VALUE.             
053600                                                                          
053700     03  W-1137KEY-X.                                                     
053800         05  FILLER               PIC X(04)  VALUE '1137'.                
053900         05  W-KDPRODSL2          PIC S9(3)  VALUE ZERO COMP-3.           
054000         05  FILLER               PIC X(24)  VALUE LOW-VALUE.             
054100                                                                          
054200     03  W-1138KEY-X.                                                     
054300         05  W-IDUSER             PIC X(08)  VALUE SPACE.                 
054400                                                                          
054500     03  W-1139KEY-X.                                                     
054600         05  FILLER               PIC X(04)  VALUE '1139'.                
054700         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
054800                                                                          
054900     03  W-2227KEY-X.                                                     
055000         05  FILLER               PIC X(04)  VALUE '2227'.                
055100         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
055200                                                                          
055300     03  W-111701-KEY.                                                    
055400         05  FILLER               PIC X(04)  VALUE '1117'.                
055500         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
055600                                                                          
055700     03  W-111711-KEY.                                                    
055800         05  W-IDARTNR-1117       PIC S9(9) VALUE ZERO COMP-3.            
055900                                                                          
056000     03  W-KDAVROP-X.                                                     
056100         05  W-KDAVROP           PIC S9(1)   VALUE +2   COMP-3.           
056200                                                                          
056300     03  W-WDGX2223-X.                                                    
056400         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
056500         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
056600         05  W-VALFRI-2225       PIC X(24)    VALUE LOW-VALUE.            
056700                                                                          
056800     03  W-WDGX2224-X.                                                    
056900         05  W-TISENBEK-DAG-2224 PIC S9(7)    VALUE ZERO COMP-3.          
057000         05  W-TISENBEK-KL-2224  PIC S9(7)    VALUE ZERO COMP-3.          
057100         05  W-KDLARM-2224       PIC S9(3)    VALUE ZERO COMP-3.          
057200                                                                          
057300     03  W-WDGX2231-X.                                                    
057400         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
057500         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
057600                                                                          
057700     03  W-WDGX2232-X.                                                    
057800         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
057900         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
058000                                                                          
058100     03  W-WDGXKEY-2203-X.                                                
058200         05  FILLER              PIC X(4)    VALUE '2203'.                
058300         05  W-IDDC-2203         PIC X(2)    VALUE '  '.                  
058400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
058500                                                                          
058600     03  W-B6-IDDC-MIN-X.                                                 
058700         05  W-B6-IDDC-MIN        PIC X(2) VALUE LOW-VALUE.               
058800                                                                          
058900     03  W-B6-IDDC-MAX-X.                                                 
059000         05  W-B6-IDDC-MAX        PIC X(2) VALUE HIGH-VALUE.              
059100                                                                          
059200     03  W-WDGX2263-X.                                                    
059300         05  W-IDHTYP-2263       PIC X(4)    VALUE '2263'.                
059400         05  W-FILLER            PIC X(26)   VALUE LOW-VALUE.             
059500     03  W-TISOP-O-X.                                                     
059600         05  W-TISOP-2264-O       PIC S9(5)           COMP-3.             
059700     03  W-W2266KY-MIN-O-X.                                               
059800         05  W-IDARTNR-2266-O-MIN PIC S9(9)           COMP-3.             
059900         05  W-IDDC-2266-O-MIN    PIC X(2)    VALUE LOW-VALUE.            
060000     03  W-W2266KY-MAX-O-X.                                               
060100         05  W-IDARTNR-2266-O-MAX PIC S9(9)           COMP-3.             
060200         05  W-IDDC-2266-O-MAX    PIC X(2)    VALUE HIGH-VALUE.           
060300     EJECT                                                                
060400*                                                                         
060500******************************************************************        
060600*                    M I D-C O P Y T E X T                       *        
060700******************************************************************        
060800*                                                                         
060900*                        ****    MFS OCH SKÄRMHANTERING                   
061000 01  IMS-WS-3.                                                            
061100     03  FILLER                  PIC X(16)   VALUE 'MFS-WS'.              
061200     SKIP3                                                                
061300*01  MID -COPY W90411I1                                                   
061400     EJECT                                                                
061500*                                                                         
061600******************************************************************        
061700*                    M S G - A R E A                             *        
061800******************************************************************        
061900*                                                                         
062000 01  IMS-WS-4.                                                            
062100     03  FILLER                  PIC X(16)   VALUE 'MSG-AREA'.            
062200     SKIP3                                                                
062300*01  -COPY WMSGAREA                                                       
062400     EJECT                                                                
062500*                                                                         
062600******************************************************************        
062700*                    M O D-C O P Y T E X T                       *        
062800******************************************************************        
062900*                                                                         
063000*    03  MOD -COPY W90411O1  -RED MSG-AREA.                               
063100     EJECT                                                                
063200*                                                                         
063300******************************************************************        
063400*                    M F S - A R E A                             *        
063500******************************************************************        
063600*                                                                         
063700 01  IMS-WS-6.                                                            
063800     03  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.            
063900     SKIP3                                                                
064000*01  -COPY WMFSAREA.                                                      
064100     EJECT                                                                
064200*                                                                         
064300******************************************************************        
064400*        A R B E T S- AREOR TILL  D B 2 -SEKTIONERNA                      
064500******************************************************************        
064600 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
064700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
064800                                                                          
064900 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
065000 01  DB2-WS.                                                              
065100     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
065200         88  CURSOR-OK                      VALUE 000.                    
065300         88  LINES-FOUND                    VALUE 000.                    
065400         88  LINES-MISSING                  VALUE 100.                    
065500         88  RESOURCE-WRONG                 VALUE 904.                    
065600     03  GOOD-SQLCODECODES.                                               
065700         05  GOOD-SQLCODE OCCURS 5                                        
065800             INDEXED BY SQLCODE-IX PIC 9(3).                              
065900     EJECT                                                                
066000*                                                                         
066100******************************************************************        
066200*    A R B E T S A R E O R  I M S - S E K T I O N E R N A        *        
066300******************************************************************        
066400*                                                                         
066500 01  IMS-WS-7.                                                            
066600     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
066700     SKIP3                                                                
066800*****                    **** STATUS-KOD FRÅN IMS                         
066900     03  STATUS-WS               PIC X(2).                                
067000         88  SEGMENT-FINNS                   VALUE '  '.                  
067100         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
067200         88  SEGMENT-SLUT                    VALUE 'GB'.                  
067300         88  SEGMENT-FINNS-REDAN             VALUE 'II'.                  
067400     SKIP3                                                                
067500     03  GODK-STATUSKODER.                                                
067600         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
067700     SKIP3                                                                
067800 01  IMS-WS-8.                                                            
067900     03  FILLER                  PIC X(09)   VALUE 'SSA:ER   '.           
068000     SKIP3                                                                
068100 01  SSA1                        PIC X(128).                              
068200 01  SSA2                        PIC X(128).                              
068300 01  SSA3                        PIC X(128).                              
068400     EJECT                                                                
068500*                                                                         
068600******************************************************************        
068700*            I M S  F U N K T I O N S K O D E R                  *        
068800******************************************************************        
068900*                                                                         
069000*                                                                         
069100 01  IMS-WS-9.                                                            
069200     03  FILLER                  PIC X(16)   VALUE ' IMS-FUNK'.           
069300     SKIP3                                                                
069400*01  -COPY W0003                                                          
069500     EJECT                                                                
069600*                                                                         
069700******************************************************************        
069800*            D L I  I N P U T-O U T P U T A R E A                *        
069900******************************************************************        
070000*                                                                         
070100******************************************************************        
070200*            S E G M E N T C O P Y T E X T E R                   *        
070300******************************************************************        
070400 01  IMS-WS-10.                                                           
070500     03  FILLER                 PIC X(16) VALUE 'DLI-IO-AREA'.            
070600     SKIP3                                                                
070700 01  DLI-IO-ARTC.                                                         
070800     03  IO-ARTC                   PIC X(900) VALUE SPACE.                
070900*    03  ARTC -COPY WDK601                   -RED IO-ARTC.                
071000     EJECT                                                                
071100*    03  ARTC -COPY WDK611                   -RED IO-ARTC.                
071200     EJECT                                                                
071300*    03  ARTC -COPY WDK625                   -RED IO-ARTC.                
071400     EJECT                                                                
071500                                                                          
071600 01  DLI-IO-AREA1.                                                        
071700     03  IO-AREA1                  PIC X(300) VALUE SPACE.                
071800*    03  ERSB -COPY WDD7A1      -PRE ERSB01- -RED IO-AREA1.               
071900     EJECT                                                                
072000*    03  LEVA -COPY WDF101      -PRE LEVA01- -RED IO-AREA1.               
072100     EJECT                                                                
072200*    03  XXAQ -COPY WDGX1132    -PRE XXAQ-  -RED IO-AREA1.                
072300     EJECT                                                                
072400*    03  XXAT -COPY WDGX1138    -PRE XXAT-  -RED IO-AREA1.                
072500     EJECT                                                                
072600*    03  XXAQ -COPY WDGX1123    -PRE XXAP-  -RED IO-AREA1.                
072700     EJECT                                                                
072800*    03  XXAP -COPY WDGX1124    -PRE XXAP-  -RED IO-AREA1.                
072900     EJECT                                                                
073000                                                                          
073100 01  DLI-IO-WDGZ01.                                                       
073200*    03  ZZAC -COPY WDGZ01      -PRE ZZAC-.                               
073300     EJECT                                                                
073400 01  DLI-IO-WDGX2228.                                                     
073500*    03  XXBW -COPY WDGX2228    -PRE XXBW-.                               
073600     EJECT                                                                
073700*                                                                         
073800******************************************************************        
073900*            D L I  I N P U T-O U T P U T A R E A -K6  PCB2      *        
074000******************************************************************        
074100*     DENNA ANVÄNDS ENDAST FÖR KONTROLLÄSNING UTANFÖR UPPDAT-PCB          
074200 01  DLI-IO-AREA-K6.                                                      
074300     03  IO-AREA-K6                PIC X(900) VALUE SPACE.                
074400     SKIP3                                                                
074500*    03  ARTC -COPY WDK601 -PRE PCB2-        -RED IO-AREA-K6.             
074600     EJECT                                                                
074700*    03  ARTC -COPY WDK611 -PRE PCB2-        -RED IO-AREA-K6.             
074800     EJECT                                                                
074900*                                                                         
075000*                                                                         
075100******************************************************************        
075200*            D L I  I N P U T-O U T P U T A R E A -2             *        
075300******************************************************************        
075400*                                                                         
075500 01  IMS-WS-16.                                                           
075600     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA2'.           
075700     SKIP3                                                                
075800 01  DLI-IO-AREA2.                                                        
075900     03  IO-AREA2                  PIC X(550)   VALUE SPACE.              
076000     SKIP3                                                                
076100*    03  ARTG -COPY WDD201        -PRE NYPON-  -RED IO-AREA2.             
076200     EJECT                                                                
076300*                                                                         
076400******************************************************************        
076500*            D L I  I N P U T-O U T P U T A R E A -3             *        
076600******************************************************************        
076700*                                                                         
076800 01  IMS-WS-93.                                                           
076900     03  FILLER                  PIC X(16)   VALUE 'DLI-AREA3'.           
077000     SKIP3                                                                
077100 01  DLI-IO-AREA3.                                                        
077200     03  IO-AREA3                  PIC X(550)   VALUE SPACE.              
077300     SKIP3                                                                
077400*    03  1117 -COPY WDGX1118    -PRE PSUPD-  -RED IO-AREA3.               
077500     EJECT                                                                
077600*                                                                         
077700*                                                                         
077800******************************************************************        
077900*            D L I  I N P U T-O U T P U T A R E A -5             *        
078000******************************************************************        
078100*                                                                         
078200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
078300 01  DLI-IO-WDK701.                                                       
078400*    03  -COPY WDK701                                                     
078500     SKIP3                                                                
078600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
078700 01  DLI-IO-WDK711.                                                       
078800*    03  -COPY WDK711                                                     
078900     SKIP3                                                                
079000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712  '.                    
079100 01  DLI-IO-WDK712.                                                       
079200*    03  -COPY WDK712                                                     
079300     SKIP3                                                                
079400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
079500 01  DLI-IO-WDD901.                                                       
079600*    03  -COPY WDD901  -PRE WDD901-                                       
079700     SKIP3                                                                
079800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
079900 01  DLI-IO-WDD902.                                                       
080000*    03  -COPY WDD902  -PRE WDD902-                                       
080100     EJECT                                                                
080200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
080300 01  DLI-IO-WDD905.                                                       
080400*    03  -COPY WDD905  -PRE WDD905-                                       
080500     EJECT                                                                
080600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR220'.                      
080700 01  DLI-IO-AREA-2232.                                                    
080800*    03  -COPY WDGX2232   -PRE WDR220-                                    
080900     EJECT                                                                
081000 01  FILLER                      PIC X(16)   VALUE 'WDR501-AREA'.         
081100 01  DLI-IO-AREA-2223.                                                    
081200*    03  -COPY WDGX2223   -PRE WDR501-                                    
081300     EJECT                                                                
081400 01  FILLER                      PIC X(16)   VALUE 'WDR550-AREA'.         
081500 01  DLI-IO-AREA-2224.                                                    
081600*    03  -COPY WDGX2224   -PRE WDR550-                                    
081700     EJECT                                                                
081800 01  FILLER                      PIC X(16)   VALUE 'WDG3-AREA'.           
081900 01  DLI-IO-AREA-2204.                                                    
082000*    03  -COPY WDGX2204   -PRE XXBJ11-                                    
082100     03  FILLER                  PIC X(3)  VALUE SPACE.                   
082200     EJECT                                                                
082300 01  FILLER                      PIC X(16)   VALUE 'WDR801-AREA'.         
082400 01  DLI-IO-WDR801.                                                       
082500*    03  -COPY WDR801                                                     
082600     EJECT                                                                
082700 01  FILLER                      PIC X(16)   VALUE 'A17-TRANS'.           
082800**   ---- A17-TRANS                                                       
082900 01  -COPY W510A17    -PRE A17-                                           
083000                                                                          
083100     EJECT                                                                
083200 01  FILLER                      PIC X(16)   VALUE 'WDB601-AREA'.         
083300 01  DLI-IO-WDB601.                                                       
083400*    03  -COPY WDB601                                                     
083500     EJECT                                                                
083600 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2264-O'.                  
083700 01  DLI-IO-WDGX2264-O.                                                   
083800*    03  -COPY WDGX2264       -PRE OLD-                                   
083900     EJECT                                                                
084000 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2266-O'.                  
084100 01  DLI-IO-WDGX2266-O.                                                   
084200*    03  -COPY WDGX2266      -PRE OLD-                                    
084300     EJECT                                                                
084400 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2264-N'.                  
084500 01  DLI-IO-WDGX2264-N.                                                   
084600*    03  -COPY WDGX2264       -PRE NEW-                                   
084700     EJECT                                                                
084800 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDGX2266-N'.                  
084900 01  DLI-IO-WDGX2266-N.                                                   
085000*    03  -COPY WDGX2266      -PRE NEW-                                    
085100     EJECT                                                                
085200*                                                                         
085300*                                                                         
085400******************************************************************        
085500*             D B 2  INPUT-OUTPUT   A R E A                               
085600******************************************************************        
085700                                                                          
085800 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
085900*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
086000     EJECT                                                                
086100 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
086200*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
086300     EJECT                                                                
086400     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
086500     EJECT                                                                
086600     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
086700     EJECT                                                                
086800                                                                          
086900*                                                                         
087000******************************************************************        
087100*            L I N K A G E  S E C T I O N                        *        
087200******************************************************************        
087300*                                                                         
087400 LINKAGE SECTION.                                                         
087500     SKIP2                                                                
087600*01  -COPY W0009     -PRE MSG-                                            
087700     EJECT                                                                
087800*01  -COPY W0008     -PRE USEA-                                           
087900         05  FILLER              PIC X.                                   
088000     EJECT                                                                
088100*01  -COPY W0008     -PRE ARTC-                                           
088200         05  FILLER              PIC X.                                   
088300     EJECT                                                                
088400*01  -COPY W0008     -PRE ERSB-                                           
088500         05  FILLER              PIC X.                                   
088600     EJECT                                                                
088700*01  -COPY W0008     -PRE ARTG-                                           
088800         05  FILLER              PIC X.                                   
088900     EJECT                                                                
089000*01  -COPY W0008     -PRE LEVA-                                           
089100         05  FILLER              PIC X.                                   
089200     EJECT                                                                
089300*01  -COPY W0008     -PRE XXAQ-                                           
089400         05  FILLER              PIC X.                                   
089500     EJECT                                                                
089600*01  -COPY W0008     -PRE XXAT-                                           
089700         05  FILLER              PIC X.                                   
089800     EJECT                                                                
089900*01  -COPY W0008     -PRE XXAP-                                           
090000         05  FILLER              PIC X.                                   
090100     EJECT                                                                
090200*01  -COPY W0008     -PRE ZZAC-                                           
090300         05  FILLER              PIC X.                                   
090400     EJECT                                                                
090500*01  -COPY W0008     -PRE XXBW-                                           
090600         05  FILLER              PIC X.                                   
090700     EJECT                                                                
090800*01  -COPY W0008     -PRE 1117-                                           
090900         05  FILLER              PIC X.                                   
091000     EJECT                                                                
091100*01  -COPY W0008     -PRE WDD9-                                           
091200         05  FILLER              PIC X.                                   
091300     EJECT                                                                
091400*01  -COPY W0008     -PRE WDR2-                                           
091500         05  FILLER              PIC X.                                   
091600     EJECT                                                                
091700*01  -COPY W0008     -PRE WDR5-                                           
091800         05  FILLER              PIC X.                                   
091900     EJECT                                                                
092000*01  -COPY W0008     -PRE WDK6-                                           
092100         05  FILLER              PIC X.                                   
092200     EJECT                                                                
092300*01  -COPY W0008     -PRE XXBJ-                                           
092400         05  FILLER              PIC X.                                   
092500     EJECT                                                                
092600*01  -COPY W0008     -PRE WDR8-                                           
092700         05  FILLER              PIC X.                                   
092800     EJECT                                                                
092900*01  -COPY W0008     -PRE WDB6-                                           
093000         05  FILLER              PIC X.                                   
093100     EJECT                                                                
093200*01  -COPY W0008     -PRE WDK7-                                           
093300         05  FILLER              PIC X.                                   
093400     EJECT                                                                
093500*01    -COPY W0008     -PRE WDR2-O-                                       
093600     05  FILLER                  PIC X.                                   
093700     EJECT                                                                
093800*01    -COPY W0008     -PRE WDR2-N-                                       
093900     05  FILLER                  PIC X.                                   
094000     EJECT                                                                
094100 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
094200                                  ARTC-PCB                                
094300     ERSB-PCB ARTG-PCB LEVA-PCB                                           
094400     XXAQ-PCB XXAT-PCB XXAP-PCB ZZAC-PCB                                  
094500     XXBW-PCB 1117-PCB WDD9-PCB WDR2-PCB WDR5-PCB                         
094600     WDK6-PCB XXBJ-PCB                                                    
094700     WDR8-PCB WDB6-PCB WDR2-O-PCB WDR2-N-PCB WDK7-PCB.                    
094800     SKIP1                                                                
094900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
095000     ARTC-PCB          ERSB-PCB ARTG-PCB LEVA-PCB                         
095100     XXAQ-PCB XXAT-PCB XXAP-PCB ZZAC-PCB                                  
095200     XXBW-PCB 1117-PCB WDD9-PCB WDR2-PCB WDR5-PCB                         
095300     WDK6-PCB XXBJ-PCB                                                    
095400     WDR8-PCB WDB6-PCB WDR2-O-PCB WDR2-N-PCB WDK7-PCB.                    
095500                                                                          
095600     PERFORM IMS-GET-MSG                                                  
095700     IF SEGMENT-FINNS                                                     
095800        PERFORM A-INIT-SPARA-INPUT                                        
095900        IF WS-IDARTNR NUMERIC                                             
096000           MOVE WS-IDARTNR  TO W-IDARTNR                                  
096100           PERFORM IMS-GHU-ARTC01                                         
096200           IF SEGMENT-FINNS                                               
096300              IF ART-KDERS-UTG > ZERO                                     
096400                 MOVE FEL-4 (SPAR-TEXT-IND) TO MOD-TEMFSFEL               
096500              ELSE                                                        
096600                 PERFORM IMS-GHU-ARTG01-MED-GE                            
096700                 IF SEGMENT-FINNS                                         
096800                    MOVE JA      TO SW-ARTIKEL-FINNS-PA-NYPON             
096900                    MOVE NYPON-ART-IDPROJOBJ TO SPAR-IDPROJOBJ            
097000                 END-IF                                                   
097100                 IF MFS-UPDATE                                            
097200                    PERFORM C-KOLLA-INPUT                                 
097300                    IF SW-INPUT-RAETT = JA                                
097400                       PERFORM D-RELATIONSKOLL                            
097500                       IF SW-INPUT-RAETT = JA                             
097600                          PERFORM E-UPPDATERA-OCH-VISA-BILD               
097700                       ELSE                                               
097800                          MOVE FEL-2 (SPAR-TEXT-IND)                      
097900                                     TO MOD-TEMFSFEL                      
098000                       END-IF                                             
098100                    ELSE                                                  
098200                       MOVE FEL-2 (SPAR-TEXT-IND)                         
098300                                   TO MOD-TEMFSFEL                        
098400                    END-IF                                                
098500                 ELSE                                                     
098600                    PERFORM B-VISA-BILD                                   
098700                 END-IF                                                   
098800              END-IF                                                      
098900           ELSE                                                           
099000              MOVE FEL-3 (SPAR-TEXT-IND)    TO MOD-TEMFSFEL               
099100           END-IF                                                         
099200        ELSE                                                              
099300           MOVE FEL-1 (SPAR-TEXT-IND) TO MOD-TEMFSFEL                     
099400        END-IF                                                            
099500                                                                          
099600        COMPUTE MSG-KVLL = LENGTH OF MOD-W90411O1 + 4                     
099700        PERFORM IMS-INSERT-MSG                                            
099800     END-IF                                                               
099900                                                                          
100000     MOVE ZERO TO RETURN-CODE                                             
100100     GOBACK.                                                              
100200     EJECT                                                                
100300 A-INIT-SPARA-INPUT SECTION.                                              
100400     SKIP2                                                                
100500     IF MSG-DUBBLA-TRANSKODER                                             
100600         MOVE MSG-IDTRANS-2                 TO                            
100700                                         MFS-IDTRANS WS-IDTRANS           
100800         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90411I1               
100900         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
101000         MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                
101100         MOVE MSG-IDPFK                     TO MFS-IDPFK                  
101200     ELSE                                                                 
101300         MOVE MSG-IDTRANS-1                 TO                            
101400                                          MFS-IDTRANS WS-IDTRANS          
101500         MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W90411I1               
101600         MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR               
101700         MOVE SPACE                         TO MFS-KDTRTYP                
101800                                               MFS-IDPFK                  
101900     END-IF                                                               
102000                                                                          
102100     IF EGEN-BILD                                                         
102200        CONTINUE                                                          
102300     ELSE                                                                 
102400        MOVE SPACE TO MFS-KDTRTYP                                         
102500                      MFS-IDPFK                                           
102600     END-IF                                                               
102700     EJECT                                                                
102800     MOVE LOW-VALUE         TO MOD-W90411O1                               
102900     MOVE 'W90411O1'        TO MFS-IDMOD                                  
103000     MOVE '9411'            TO MOD-IDTRANS                                
103100                                                                          
103200     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL                               
103300                               MOD-TEMFSINF                               
103400     MOVE SPACE TO             WS-TEMFSINF                                
103500                               WS-TEMFSINF-KAMP                           
103600                                                                          
103700     ACCEPT SPAR-DAGENS-DATUM FROM DATE                                   
103800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
103900                                                                          
104000     MOVE SPAR-DAGENS-DATUM     TO DAT-I-TIDATUM                          
104100     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
104200     PERFORM S99-WDATKONV                                                 
104300                                                                          
104400     IF DAT-KDSVAR-OK                                                     
104500        MOVE DAT-TIAA-VECKA     TO SPAR-DAGENS-AA                         
104600        MOVE DAT-TIVV           TO SPAR-DAGENS-VV                         
104700     END-IF                                                               
104800                                                                          
104900     MOVE ALL '+' TO MSGI-WMSGINIT                                        
105000     MOVE '001'             TO MSGI-KDCALL                                
105100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
105200                               MSGI-IDLTERM-USER                          
105300     MOVE '9411'            TO MSGI-IDTRANS                               
105400     IF MFS-IDTRANS = '9411'                                              
105500     OR (MID-IDARTNR-IN NUMERIC                                           
105600     AND MID-IDARTNR-IN > ZERO)                                           
105700         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
105800     END-IF                                                               
105900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
106000     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
106100     MOVE MSGI-IDDC    TO WS-IDDC                                         
106200     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
106300                                                                          
106400     IF MID-IDARTNR-IN = ALL '+'                                          
106500        CONTINUE                                                          
106600     ELSE                                                                 
106700        MOVE SPACE          TO MFS-KDTRTYP                                
106800                               MFS-IDPFK                                  
106900     END-IF                                                               
107000                                                                          
107100     IF MFS-UPDATE                                                        
107200        IF MID-INDEL    = ALL '+'                                         
107300           MOVE SPACE       TO MFS-KDTRTYP                                
107400        END-IF                                                            
107500     END-IF                                                               
107600                                                                          
107700     IF MSGI-IDLAND-SPR = 'GB'                                            
107800        MOVE 2                   TO SPAR-TEXT-IND                         
107900     ELSE                                                                 
108000        MOVE 1                   TO SPAR-TEXT-IND                         
108100     END-IF.                                                              
108200                                                                          
108300     EJECT                                                                
108400 B-VISA-BILD SECTION.                                                     
108500     SKIP2                                                                
108600***** I SLUTET AV DENNA SEKTION SÄTTS MOD-TEMFSINF IHOP FRÅN              
108700*****                       WS-TEMFSINF-KAMP   OCH   WS-TEMFSINF          
108800     PERFORM BA-LAS-VISA-ARTC                                             
108900     IF MSGI-IDLAND-SPR = 'GB'                                            
109000        MOVE 'GB '                   TO W-IDSKYLT                         
109100     ELSE                                                                 
109200        MOVE 'S  '                   TO W-IDSKYLT                         
109300     END-IF                                                               
109400                                                                          
109500     IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                    
109600        PERFORM BD-VISA-ARTG                                              
109700     END-IF                                                               
109800     PERFORM BG-KOLLA-KAMPANJ                                             
109900                                                                          
110000     IF MID-IDARTNR-IN = ALL '+'  AND  EGEN-BILD                          
110100        IF MID-INDEL   = ALL '+'                                          
110200*          GAMMAL NYCKEL, INGENTING INMATAT                               
110300           PERFORM S03-RENSA-MOD-INMATNINGSFAELT                          
110400        ELSE                                                              
110500           MOVE MED-1 (SPAR-TEXT-IND)    TO WS-TEMFSINF                   
110600           PERFORM BF-KOLLA-INMATADE-FAELT                                
110700        END-IF                                                            
110800     ELSE                                                                 
110900        PERFORM S03-RENSA-MOD-INMATNINGSFAELT                             
111000     END-IF                                                               
111100                                                                          
111200*    ---- SÄTT IHOP EV. MEDDELANDEN PÅ RAD 23 ------------                
111300     IF WS-TEMFSINF = SPACE  AND WS-TEMFSINF-KAMP = SPACE                 
111400         CONTINUE                                                         
111500     ELSE                                                                 
111600       IF WS-TEMFSINF = SPACE                                             
111700           MOVE WS-TEMFSINF-KAMP TO MOD-TEMFSINF                          
111800       ELSE                                                               
111900         IF WS-TEMFSINF-KAMP = SPACE                                      
112000             MOVE WS-TEMFSINF TO MOD-TEMFSINF                             
112100         ELSE                                                             
112200*            --- OBS MAX. 55 TECKEN                                       
112300             STRING WS-TEMFSINF-KAMP  DELIMITED BY SIZE                   
112400                    WS-TEMFSINF-SPLIT DELIMITED BY SIZE                   
112500                    WS-TEMFSINF       DELIMITED BY SIZE                   
112600             INTO MOD-TEMFSINF                                            
112700         END-IF                                                           
112800       END-IF                                                             
112900     END-IF                                                               
113000     .                                                                    
113100     EJECT                                                                
113200 BA-LAS-VISA-ARTC SECTION.                                                
113300     SKIP2                                                                
113400                                                                          
113500*WDK611                                                                   
113600     PERFORM IMS-GHNP-ARTC11                                              
113700     MOVE CLAG-KDAGE                 TO MOD-KDAGE                         
113800                                                                          
113900     IF CLAG-KDERS > ZERO                                                 
114000        MOVE MED-5 (SPAR-TEXT-IND)   TO WS-TEMFSINF                       
114100     END-IF                                                               
114200                                                                          
114300     MOVE +6 TO W-KDNOTTYP                                                
114400     PERFORM IMS-GHNP-ARTC25                                              
114500     .                                                                    
114600     EJECT                                                                
114700 BD-VISA-ARTG    SECTION.                                                 
114800     SKIP2                                                                
114900                                                                          
115000     IF NYPON-ART-KDRESBED = '-'                                          
115100        MOVE MED-6 (SPAR-TEXT-IND)   TO WS-TEMFSINF                       
115200     END-IF.                                                              
115300     EJECT                                                                
115400 BF-KOLLA-INMATADE-FAELT SECTION.                                         
115500     SKIP2                                                                
115600     IF MID-IDBERED       = ALL '+'                                       
115700        CONTINUE                                                          
115800     ELSE                                                                 
115900        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDBERED-IN-ATTR                  
116000     END-IF                                                               
116100                                                                          
116200     IF MID-KDPRODSL      = ALL '+'                                       
116300        CONTINUE                                                          
116400     ELSE                                                                 
116500        MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDPRODSL-IN-ATTR                 
116600     END-IF                                                               
116700                                                                          
116800     IF MID-KDSORT        = ALL '+'                                       
116900        CONTINUE                                                          
117000     ELSE                                                                 
117100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-IN-ATTR                   
117200     END-IF                                                               
117300                                                                          
117400     IF MID-IDPROENH (1)  = ALL '+'                                       
117500        MOVE MFS-RENSA-FAELT      TO MOD-IDPROENH-IN       (1)            
117600     ELSE                                                                 
117700        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPROENH-IN-ATTR  (1)            
117800        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROENH-IN       (1)            
117900     END-IF                                                               
118000                                                                          
118100     IF MID-IDPROENH (2)  = ALL '+'                                       
118200        MOVE MFS-RENSA-FAELT      TO MOD-IDPROENH-IN       (2)            
118300     ELSE                                                                 
118400        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPROENH-IN-ATTR  (2)            
118500        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROENH-IN       (2)            
118600     END-IF                                                               
118700                                                                          
118800     IF MID-IDPROENH (3)  = ALL '+'                                       
118900        MOVE MFS-RENSA-FAELT      TO MOD-IDPROENH-IN       (3)            
119000     ELSE                                                                 
119100        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDPROENH-IN-ATTR  (3)            
119200        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROENH-IN       (3)            
119300     END-IF                                                               
119400                                                                          
119500                                                                          
119600     IF MID-KDUART        = ALL '+'                                       
119700        CONTINUE                                                          
119800     ELSE                                                                 
119900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDUART-IN-ATTR                   
120000     END-IF                                                               
120100                                                                          
120200     IF MID-IDPROJ        = ALL '+'                                       
120300        CONTINUE                                                          
120400     ELSE                                                                 
120500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR                   
120600     END-IF                                                               
120700                                                                          
120800     IF MID-FLPISK        = ALL '+'                                       
120900        CONTINUE                                                          
121000     ELSE                                                                 
121100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPISK-IN-ATTR                   
121200     END-IF                                                               
121300                                                                          
121400     IF MID-IDKAT (1)     = ALL '+'                                       
121500        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (1)               
121600     ELSE                                                                 
121700        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (1)               
121800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-IN-ATTR  (1)               
121900     END-IF                                                               
122000                                                                          
122100     IF MID-IDKAT (2)     = ALL '+'                                       
122200        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (2)               
122300     ELSE                                                                 
122400        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (2)               
122500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-IN-ATTR  (2)               
122600     END-IF                                                               
122700                                                                          
122800     IF MID-IDKAT (3)     = ALL '+'                                       
122900        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (3)               
123000     ELSE                                                                 
123100        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (3)               
123200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKAT-IN-ATTR  (3)               
123300     END-IF                                                               
123400                                                                          
123500     IF MID-IDPROJK       = ALL '+'                                       
123600        CONTINUE                                                          
123700     ELSE                                                                 
123800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR                  
123900     END-IF                                                               
124000                                                                          
124100     IF MID-KDBPSR        = ALL '+'                                       
124200        CONTINUE                                                          
124300     ELSE                                                                 
124400        MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDBPSR-IN-ATTR                   
124500     END-IF                                                               
124600                                                                          
124700     IF MID-TISOP         = ALL '+'                                       
124800        CONTINUE                                                          
124900     ELSE                                                                 
125000        MOVE MFS-NUM-FAELT-RAETT  TO MOD-TISOP-IN-ATTR                    
125100     END-IF                                                               
125200                                                                          
125300     IF MID-IDFKNGRP      = ALL '+'                                       
125400        CONTINUE                                                          
125500     ELSE                                                                 
125600        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDFKNGRP-IN-ATTR                 
125700     END-IF                                                               
125800                                                                          
125900     IF MID-IDPROJUP      = ALL '+'                                       
126000        CONTINUE                                                          
126100     ELSE                                                                 
126200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJUP-IN-ATTR                 
126300     END-IF                                                               
126400                                                                          
126500     IF MID-IDARTNR-MOTSV = ALL '+'                                       
126600        CONTINUE                                                          
126700     ELSE                                                                 
126800        MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDARTNR-MOTSV-IN-ATTR            
126900     END-IF                                                               
127000                                                                          
127100     IF MID-IDRITN        = ALL '+'                                       
127200        CONTINUE                                                          
127300     ELSE                                                                 
127400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-IN-ATTR                   
127500     END-IF                                                               
127600                                                                          
127700     IF MID-KVARTVAGN     = ALL '+'                                       
127800        CONTINUE                                                          
127900     ELSE                                                                 
128000        MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVARTVAGN-IN-ATTR                
128100     END-IF                                                               
128200                                                                          
128300     IF MID-IDAO  (1)     = ALL '+'                                       
128400        CONTINUE                                                          
128500     ELSE                                                                 
128600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (1)               
128700     END-IF                                                               
128800                                                                          
128900     IF MID-IDAO  (2)     = ALL '+'                                       
129000        CONTINUE                                                          
129100     ELSE                                                                 
129200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (2)               
129300     END-IF                                                               
129400                                                                          
129500     IF MID-IDAO  (3)     = ALL '+'                                       
129600        CONTINUE                                                          
129700     ELSE                                                                 
129800        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (3)               
129900     END-IF                                                               
130000                                                                          
130100     IF MID-IDAO  (4)     = ALL '+'                                       
130200        CONTINUE                                                          
130300     ELSE                                                                 
130400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (4)               
130500     END-IF                                                               
130600                                                                          
130700     IF MID-IDAO  (5)     = ALL '+'                                       
130800        CONTINUE                                                          
130900     ELSE                                                                 
131000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (5)               
131100     END-IF                                                               
131200                                                                          
131300     IF MID-TEORSAK       = ALL '+'                                       
131400        CONTINUE                                                          
131500     ELSE                                                                 
131600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEORSAK-IN-UT-ATTR               
131700     END-IF                                                               
131800                                                                          
131900     IF MID-KDAGE         = ALL '+'                                       
132000        CONTINUE                                                          
132100     ELSE                                                                 
132200        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDAGE-IN-ATTR                    
132300     END-IF                                                               
132400     .                                                                    
132500     EJECT                                                                
132600 BG-KOLLA-KAMPANJ   SECTION.                                              
132700     SKIP2                                                                
132800     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
132900     IF SQLCODE-WS = ZERO                                                 
133000       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
133100     END-IF                                                               
133200                                                                          
133300     MOVE ZERO               TO WS-ANTAL-KAMP                             
133400     MOVE NEJ                TO WS-FLAGGA-Q-KAMP                          
133500                                WS-FLAGGA-W-S-KAMP                        
133600     PERFORM UNTIL SQLCODE > ZERO                                         
133700       IF TP1KAMP-TISTODAT-KAMP > ZERO                                    
133800         MOVE TP1KAMP-TISTODAT-KAMP                                       
133900                             TO WS-JMFR-AAAAMMDD                          
134000       ELSE                                                               
134100         MOVE TP1KAMP-TISTADAT-KAMP                                       
134200                             TO WS-JMFR-AAAAMMDD                          
134300       END-IF                                                             
134400       IF WS-JMFR-AA > 50                                                 
134500         MOVE 19             TO WS-JMFR-AAAAMMDD (1:2)                    
134600       ELSE                                                               
134700         MOVE 20             TO WS-JMFR-AAAAMMDD (1:2)                    
134800       END-IF                                                             
134900       IF TP1KAMP-TISTODAT-KAMP = ZERO                                    
135000*    LÄGG TILL 5 ÅR                                                       
135100         ADD 50000           TO WS-JMFR-AAAAMMDD                          
135200       END-IF                                                             
135300       IF WS-JMFR-AAAAMMDD >= WS-DAGENS-AAAAMMDD                          
135400         IF TP1KAMP-KDKAMP = 'Q'                                          
135500           MOVE JA           TO WS-FLAGGA-Q-KAMP                          
135600         END-IF                                                           
135700         IF TP1KAMP-KDKAMP = 'W'                                          
135800         OR TP1KAMP-KDKAMP = 'S'                                          
135900           MOVE JA           TO WS-FLAGGA-W-S-KAMP                        
136000         END-IF                                                           
136100       END-IF                                                             
136200       ADD 1                 TO WS-ANTAL-KAMP                             
136300       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
136400     END-PERFORM                                                          
136500                                                                          
136600     IF  WS-FLAGGA-Q-KAMP   = JA                                          
136700     AND WS-FLAGGA-W-S-KAMP = NEJ                                         
136800       MOVE MED-92 (SPAR-TEXT-IND) TO WS-TEMFSINF-KAMP                    
136900*            SM ETC                                                       
137000     ELSE                                                                 
137100       IF WS-FLAGGA-W-S-KAMP = JA                                         
137200       MOVE MED-91 (SPAR-TEXT-IND) TO WS-TEMFSINF-KAMP                    
137300*            CAMPAIGN                                                     
137400       END-IF                                                             
137500     END-IF                                                               
137600     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
137700     .                                                                    
137800     EJECT                                                                
137900 C-KOLLA-INPUT SECTION.                                                   
138000     SKIP2                                                                
138100     MOVE JA                      TO SW-INPUT-RAETT                       
138200                                                                          
138300     MOVE ART-IDLEVNR             TO SPAR-IDLEVNR                         
138400     MOVE ART-FLERS               TO SPAR-FLERS                           
138500     MOVE ART-FLIART              TO SPAR-FLIART                          
138600     MOVE ART-KDPRODSL            TO WS-KDPRODSL                          
138700                                     WS-A17-KDPRODSL                      
138800                                     TEST-KDPRODSL                        
138900     MOVE ART-KDSORT              TO WS-KDSORT-OLD                        
139000     MOVE ART-IDFKNGRP            TO WS-IDFKNGRP-OLD                      
139100                                                                          
139200     IF KDPRODSL-VOLVO-BIMA                                               
139300        IF CDC OR SDC                                                     
139400           CONTINUE                                                       
139500        ELSE                                                              
139600           MOVE NEJ TO SW-INPUT-RAETT                                     
139700           MOVE MED-8 (SPAR-TEXT-IND) TO MOD-TEMFSINF                     
139800        END-IF                                                            
139900     END-IF                                                               
140000     MOVE ZERO TO WS-GAMMAL-ART                                           
140100     MOVE MID-TISOP               TO XX-TISOP                             
140200     MOVE '+'                     TO XX-DAG                               
140300     IF XX-TISOP = ALL '+'                                                
140400        IF ART-TISOP = 99999                                              
140500           MOVE 99999999          TO SPAR-DASOP-AAAAMMDD                  
140600           MOVE 99999             TO SPAR-TISOP-AAVVD                     
140700        ELSE                                                              
140800           MOVE 'AAVVD'           TO DAT-KDDATFORM                        
140900           MOVE ART-TISOP         TO DAT-I-TIDATUM                        
141000                                     SPAR-TISOP-AAVVD                     
141100           PERFORM S99-WDATKONV                                           
141200           IF DAT-KDSVAR-OK                                               
141300              MOVE DAT-TIAAMMDD   TO SPAR-DASOP-AAAAMMDD                  
141400              MOVE DAT-TISEKEL    TO SPAR-DASOP-AAAAMMDD (1:2)            
141500                                                                          
141600              MOVE SPAR-DAGENS-DATUM   TO TMP1-YYMMDD                     
141700              MOVE SPAR-DASOP-AAAAMMDD (3:6) TO TMP2-YYMMDD               
141800              PERFORM WY2000P1                                            
141900              COMPUTE                                                     
142000                WS-GAMMAL-ART = TMP1-YYMMDD - TMP2-YYMMDD                 
142100           END-IF                                                         
142200        END-IF                                                            
142300     ELSE                                                                 
142400        MOVE 1                    TO XX-DAG                               
142500        MOVE XX-TISOP             TO SPAR-TISOP-AAVVD                     
142600        IF SPAR-TISOP-AAVVD = 99991                                       
142700           MOVE MFS-NUM-FAELT-RAETT                                       
142800                                  TO MOD-TISOP-IN-ATTR                    
142900           MOVE 99999999          TO SPAR-DASOP-AAAAMMDD                  
143000           MOVE 99999             TO SPAR-TISOP-AAVVD                     
143100        ELSE                                                              
143200           MOVE 'AAVVD '          TO DAT-KDDATFORM                        
143300           MOVE SPAR-TISOP-AAVVD  TO DAT-I-TIDATUM                        
143400           PERFORM S99-WDATKONV                                           
143500           IF DAT-KDSVAR-OK                                               
143600              MOVE DAT-TIAA-VECKA  TO SPAR-TISOP-AA                       
143700              MOVE DAT-TIVV        TO SPAR-TISOP-VV                       
143800              MOVE SPAR-TISOP-AAVV-R   TO TMP1-YYWW                       
143900              MOVE SPAR-DAGENS-AAVV-R  TO TMP2-YYWW                       
144000              PERFORM WY2000P3                                            
144100              MOVE MFS-NUM-FAELT-RAETT                                    
144200                               TO MOD-TISOP-IN-ATTR                       
144300              MOVE DAT-TIAAMMDD TO SPAR-DASOP-AAAAMMDD                    
144400              MOVE DAT-TISEKEL     TO SPAR-DASOP-AAAAMMDD (1:2)           
144500                                                                          
144600              MOVE SPAR-DAGENS-DATUM      TO TMP1-YYMMDD                  
144700              MOVE SPAR-DASOP-AAAAMMDD (3:6) TO TMP2-YYMMDD               
144800              PERFORM WY2000P1                                            
144900              COMPUTE                                                     
145000                WS-GAMMAL-ART = TMP1-YYMMDD - TMP2-YYMMDD                 
145100           ELSE                                                           
145200              MOVE MFS-NUM-FAELT-FEL                                      
145300                                  TO MOD-TISOP-IN-ATTR                    
145400              MOVE NEJ            TO SW-INPUT-RAETT                       
145500           END-IF                                                         
145600        END-IF                                                            
145700     END-IF                                                               
145800                                                                          
145900     PERFORM IMS-GHNP-ARTC11                                              
146000     MOVE CLAG-IDPROJ             TO SPAR-IDPROJ                          
146100     MOVE CLAG-PRARTSTD           TO SPAR-PRARTSTD                        
146200     MOVE CLAG-KDUART             TO SPAR-KDUART                          
146300     PERFORM CE-KOLLA-INPUT-2                                             
146400                                                                          
146500     .                                                                    
146600     EJECT                                                                
146700 CA-KTR-PROJ-GODK SECTION.                                                
146800     SKIP2                                                                
146900     MOVE NEJ TO SW-PROJ-GODK                                             
147000                                                                          
147100     PERFORM IMS-GU-WLXXAQ01-UNIK                                         
147200                                                                          
147300     IF SEGMENT-FINNS                                                     
147400        MOVE SPAR-IDPROJ TO W-IDPROJ                                      
147500        PERFORM IMS-GNP-WLXXAQ11-PROJ                                     
147600        IF SEGMENT-FINNS                                                  
147700           MOVE JA TO SW-PROJ-GODK                                        
147800        END-IF                                                            
147900     END-IF                                                               
148000     .                                                                    
148100     EJECT                                                                
148200 CB-KTR-TIPRODSTA SECTION.                                                
148300*****************************************************************         
148400* ÄT OKT 95  BASLAGER                                           *         
148500*            TIFINLV PÅ ARTIKLAR PÅ ICKE LÖPANDE PROJEKT        *         
148600*            MED TIGENORD FRAMÅT I TIDEN FÅR ÄNDRAS OM TIFINLV  *         
148700*            INFALLER FÖRE TIGENORD                             *         
148800*****************************************************************         
148900     SKIP2                                                                
149000     MOVE SPAR-IDPROJ    TO W-IDPROJ                                      
149100     MOVE SPAR-IDPROJK   TO W-IDPROJK                                     
149200     MOVE SPAR-IDPROJOBJ TO W-IDPROJOBJ                                   
149300                                                                          
149400     PERFORM IMS-GU-WLXXAQ11                                              
149500     IF SEGMENT-FINNS                                                     
149600        IF XXAQ-1132-TIPRODSTA = 111111                                   
149700*     LÖPANDE PROJEKT                                                     
149800           CONTINUE                                                       
149900        ELSE                                                              
150000           MOVE W-IDPROJ TO W-IDPROJ-1123                                 
150100           PERFORM IMS-GU-WLXXAP01-UNIK                                   
150200           IF SEGMENT-FINNS                                               
150300              PERFORM IMS-GNP-WLXXAP11                                    
150400              IF SEGMENT-FINNS                                            
150500                 MOVE XXAP-1124-TIGENORD   TO TMP1-YYMMDD                 
150600                 MOVE SPAR-DAGENS-DATUM    TO TMP2-YYMMDD                 
150700                 PERFORM WY2000P1                                         
150800                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
150900                  MOVE SPAR-DASOP-AAAAMMDD (1:2) TO TMP1-YYMMDD           
151000                    MOVE XXAP-1124-TIGENORD      TO TMP2-YYMMDD           
151100                    PERFORM WY2000P1                                      
151200                    IF TMP1-YYMMDD >= TMP2-YYMMDD                         
151300                      MOVE MFS-NUM-FAELT-FEL                              
151400                              TO MOD-TISOP-IN-ATTR                        
151500                      MOVE NEJ TO SW-INPUT-RAETT                          
151600                    END-IF                                                
151700                 END-IF                                                   
151800              END-IF                                                      
151900           END-IF                                                         
152000        END-IF                                                            
152100     END-IF                                                               
152200     .                                                                    
152300     EJECT                                                                
152400 CC-KOLLA-SOFTWARE SECTION.                                               
152500                                                                          
152600     IF MID-IDFKNGRP = ALL '+'                                            
152700        MOVE WS-IDFKNGRP-OLD TO WS-TEST-IDFKNGRP                          
152800     ELSE                                                                 
152900        MOVE MID-IDFKNGRP    TO WS-TEST-IDFKNGRP                          
153000     END-IF                                                               
153100                                                                          
153200     IF MID-KDSORT = ALL '+'                                              
153300        CONTINUE                                                          
153400     ELSE                                                                 
153500        IF WS-KDSORT-GODK = 'SW'                                          
153600           IF WS-SISTA-SIFFRAN = 8                                        
153700              CONTINUE                                                    
153800           ELSE                                                           
153900              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR               
154000              MOVE NEJ TO SW-INPUT-RAETT                                  
154100           END-IF                                                         
154200        ELSE                                                              
154300           IF WS-SISTA-SIFFRAN = 8                                        
154400              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR               
154500              MOVE NEJ TO SW-INPUT-RAETT                                  
154600           END-IF                                                         
154700        END-IF                                                            
154800     END-IF                                                               
154900                                                                          
155000     IF MID-IDFKNGRP = ALL '+'                                            
155100        CONTINUE                                                          
155200     ELSE                                                                 
155300        IF WS-SISTA-SIFFRAN = 8                                           
155400           IF MID-KDSORT = ALL '+'                                        
155500              IF WS-KDSORT-OLD = 'SW'                                     
155600                 CONTINUE                                                 
155700              ELSE                                                        
155800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR           
155900                 MOVE NEJ TO SW-INPUT-RAETT                               
156000              END-IF                                                      
156100           ELSE                                                           
156200              IF WS-KDSORT-GODK = 'SW'                                    
156300                 CONTINUE                                                 
156400              ELSE                                                        
156500                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR           
156600                 MOVE NEJ TO SW-INPUT-RAETT                               
156700              END-IF                                                      
156800           END-IF                                                         
156900        ELSE                                                              
157000           IF MID-KDSORT = ALL '+'                                        
157100              IF WS-KDSORT-OLD = 'SW'                                     
157200                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR           
157300                 MOVE NEJ TO SW-INPUT-RAETT                               
157400              END-IF                                                      
157500           ELSE                                                           
157600              IF WS-KDSORT-GODK = 'SW'                                    
157700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR           
157800                 MOVE NEJ TO SW-INPUT-RAETT                               
157900              END-IF                                                      
158000           END-IF                                                         
158100        END-IF                                                            
158200     END-IF                                                               
158300     .                                                                    
158400     EJECT                                                                
158500 CE-KOLLA-INPUT-2 SECTION.                                                
158600     MOVE SPACE TO SPAR-IDPROENH-AREA                                     
158700     SET SPAR-IDPROENH-IND TO 1                                           
158800     PERFORM UNTIL SPAR-IDPROENH-IND > SPAR-IDPROENH-IND-MAX              
158900        MOVE CLAG-IDPROENH (SPAR-IDPROENH-IND)                            
159000           TO SPAR-IDPROENH (SPAR-IDPROENH-IND)                           
159100        SET SPAR-IDPROENH-IND UP BY 1                                     
159200     END-PERFORM                                                          
159300                                                                          
159400     IF MID-IDBERED = ALL '+'                                             
159500        CONTINUE                                                          
159600     ELSE                                                                 
159700        IF MID-IDBERED NUMERIC                                            
159800           IF MID-IDBERED > ZERO                                          
159900              MOVE MFS-NUM-FAELT-RAETT                                    
160000                                  TO MOD-IDBERED-IN-ATTR                  
160100           ELSE                                                           
160200              MOVE MFS-NUM-FAELT-FEL                                      
160300                                  TO MOD-IDBERED-IN-ATTR                  
160400              MOVE NEJ            TO SW-INPUT-RAETT                       
160500           END-IF                                                         
160600        ELSE                                                              
160700           MOVE MFS-NUM-FAELT-FEL                                         
160800                                  TO MOD-IDBERED-IN-ATTR                  
160900           MOVE NEJ               TO SW-INPUT-RAETT                       
161000        END-IF                                                            
161100     END-IF                                                               
161200                                                                          
161300     MOVE ZERO                    TO SPAR-KVPROG                          
161400                                                                          
161500     MOVE SPACE                   TO SPAR-KDAGE                           
161600     IF MID-KDAGE = ALL '+'                                               
161700        CONTINUE                                                          
161800     ELSE                                                                 
161900        MOVE MFS-ALFA-FAELT-RAETT                                         
162000                                  TO MOD-KDAGE-IN-ATTR                    
162100        MOVE MID-KDAGE            TO SPAR-KDAGE                           
162200     END-IF                                                               
162300                                                                          
162400     IF MID-KDPRODSL = ALL '+'                                            
162500        MOVE WS-KDPRODSL         TO TEST-KDPRODSL                         
162600                                     W-KDPRODSL1                          
162700     ELSE                                                                 
162800        IF MID-KDPRODSL NUMERIC                                           
162900           MOVE MID-KDPRODSL     TO TEST-KDPRODSL                         
163000                                    W-KDPRODSL1                           
163100           MOVE WS-KDPRODSL      TO WS-TEST-KDPRODSL                      
163200                                                                          
163300           IF GOOD-KDPRODSL                                               
163400              MOVE MFS-NUM-FAELT-RAETT                                    
163500                                 TO MOD-KDPRODSL-IN-ATTR                  
163600                                                                          
163700              MOVE 002           TO KPS-KDCALL                            
163800              MOVE MID-KDPRODSL  TO KPS-KDPRODSL                          
163900              CALL WKPSKONV USING KPS-WKPSAREA                            
164000              IF KPS-KDSVAR = 'F'                                         
164100                 MOVE MFS-NUM-FAELT-FEL                                   
164200                                 TO MOD-KDPRODSL-IN-ATTR                  
164300                 MOVE NEJ        TO SW-INPUT-RAETT                        
164400              END-IF                                                      
165400              IF (KDPRODSL-BIMA AND (NOT WS-KDPRODSL-BIMA)) OR            
165500                 (WS-KDPRODSL-BIMA AND (NOT KDPRODSL-BIMA))               
165600                 MOVE MFS-NUM-FAELT-FEL                                   
165700                                 TO MOD-KDPRODSL-IN-ATTR                  
165800                 MOVE NEJ        TO SW-INPUT-RAETT                        
165900              END-IF                                                      
166000           ELSE                                                           
166100              MOVE MFS-NUM-FAELT-FEL                                      
166200                                 TO MOD-KDPRODSL-IN-ATTR                  
166300              MOVE NEJ           TO SW-INPUT-RAETT                        
166400           END-IF                                                         
166500        ELSE                                                              
166600           MOVE MFS-NUM-FAELT-FEL                                         
166700                                 TO MOD-KDPRODSL-IN-ATTR                  
166800           MOVE NEJ              TO SW-INPUT-RAETT                        
166900        END-IF                                                            
167000     END-IF                                                               
167100                                                                          
167200     IF MID-KDSORT = ALL '+'                                              
167300        CONTINUE                                                          
167400     ELSE                                                                 
167500        MOVE MID-KDSORT TO WS-KDSORT-GODK                                 
167600        IF KDSORT-GODK                                                    
167700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-IN-ATTR                
167800        ELSE                                                              
167900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-IN-ATTR                  
168000           MOVE NEJ TO SW-INPUT-RAETT                                     
168100        END-IF                                                            
168200     END-IF                                                               
168300                                                                          
168400     IF MID-IDPROENH (1)  = ALL '+'                                       
168500        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-IN(1)                        
168600     ELSE                                                                 
168700        IF MID-IDPROENH(1) NUMERIC                                        
168800           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-IN-ATTR (1)           
168900        ELSE                                                              
169000           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPROENH-IN-ATTR (1)             
169100           MOVE NEJ TO SW-INPUT-RAETT                                     
169200        END-IF                                                            
169300        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROENH-IN(1)                      
169400     END-IF                                                               
169500                                                                          
169600     IF MID-IDPROENH(2) = ALL '+'                                         
169700        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-IN(2)                        
169800     ELSE                                                                 
169900        IF MID-IDPROENH (2) NUMERIC                                       
170000           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-IN-ATTR(2)            
170100        ELSE                                                              
170200           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPROENH-IN-ATTR(2)              
170300           MOVE NEJ TO SW-INPUT-RAETT                                     
170400        END-IF                                                            
170500        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROENH-IN(2)                      
170600     END-IF                                                               
170700                                                                          
170800     IF MID-IDPROENH(3) = ALL '+'                                         
170900        MOVE MFS-RENSA-FAELT TO MOD-IDPROENH-IN(3)                        
171000     ELSE                                                                 
171100        IF MID-IDPROENH(3) NUMERIC                                        
171200           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPROENH-IN-ATTR(3)            
171300        ELSE                                                              
171400           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPROENH-IN-ATTR(3)              
171500           MOVE NEJ TO SW-INPUT-RAETT                                     
171600        END-IF                                                            
171700        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROENH-IN(3)                      
171800     END-IF                                                               
171900                                                                          
172000     IF MID-KDUART = ALL '+'                                              
172100        CONTINUE                                                          
172200     ELSE                                                                 
172300        MOVE MID-KDUART TO WS-KDUART-GODK                                 
172400                                                                          
172500        IF MID-KDUART = ' '                                               
172600            IF SPAR-KDUART = 'L'                                          
172700                MOVE '*'          TO WS-KDUART-GODK                       
172800****    FÖR ATT ANGE DENNA KOMBINATION MED KDUART EJ ÄR GODKÄND           
172900            END-IF                                                        
173000        END-IF                                                            
173100                                                                          
173200        IF KDUART-GODK                                                    
173300           IF WS-KDUART-GODK = 'P' AND SPAR-FLIART = JA                   
173400              MOVE MFS-ALFA-FAELT-FEL                                     
173500                                     TO MOD-KDUART-IN-ATTR                
173600              MOVE NEJ               TO SW-INPUT-RAETT                    
173700              MOVE MED-7 (SPAR-TEXT-IND)   TO MOD-TEMFSINF                
173800           ELSE                                                           
173900              MOVE MFS-ALFA-FAELT-RAETT                                   
174000                                     TO MOD-KDUART-IN-ATTR                
174100           END-IF                                                         
174200        ELSE                                                              
174300           MOVE MFS-ALFA-FAELT-FEL                                        
174400                                  TO MOD-KDUART-IN-ATTR                   
174500           MOVE NEJ               TO SW-INPUT-RAETT                       
174600        END-IF                                                            
174700     END-IF                                                               
174800                                                                          
174900     IF MID-FLPISK = ALL '+'                                              
175000        CONTINUE                                                          
175100     ELSE                                                                 
175200        IF MID-FLPISK = JA OR NEJ                                         
175300           MOVE MFS-ALFA-FAELT-RAETT                                      
175400                                  TO MOD-FLPISK-IN-ATTR                   
175500           MOVE MID-FLPISK        TO SPAR-FLPISK                          
175600        ELSE                                                              
175700           MOVE MFS-ALFA-FAELT-FEL                                        
175800                                  TO MOD-FLPISK-IN-ATTR                   
175900           MOVE NEJ               TO SW-INPUT-RAETT                       
176000        END-IF                                                            
176100     END-IF                                                               
176200                                                                          
176300     IF MID-IDKAT (1) = ALL '+'                                           
176400        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (1)               
176500     ELSE                                                                 
176600        MOVE MFS-ALFA-FAELT-RAETT                                         
176700                                  TO MOD-IDKAT-IN-ATTR  (1)               
176800        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (1)               
176900     END-IF                                                               
177000                                                                          
177100     IF MID-IDKAT (2) = ALL '+'                                           
177200        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (2)               
177300     ELSE                                                                 
177400        MOVE MFS-ALFA-FAELT-RAETT                                         
177500                                  TO MOD-IDKAT-IN-ATTR  (2)               
177600        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (2)               
177700     END-IF                                                               
177800                                                                          
177900     IF MID-IDKAT (3) = ALL '+'                                           
178000        MOVE MFS-RENSA-FAELT      TO MOD-IDKAT-IN       (3)               
178100     ELSE                                                                 
178200        MOVE MFS-ALFA-FAELT-RAETT                                         
178300                                  TO MOD-IDKAT-IN-ATTR  (3)               
178400        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDKAT-IN       (3)               
178500     END-IF                                                               
178600                                                                          
178700     IF MID-KDBPSR = ALL '+'                                              
178800        CONTINUE                                                          
178900     ELSE                                                                 
179000        MOVE MID-KDBPSR           TO WS-KDBPSR-GODK                       
179100        IF KDBPSR-GODK                                                    
179200           MOVE MFS-NUM-FAELT-RAETT                                       
179300                                  TO MOD-KDBPSR-IN-ATTR                   
179400        ELSE                                                              
179500           MOVE MFS-NUM-FAELT-FEL                                         
179600                                  TO MOD-KDBPSR-IN-ATTR                   
179700           MOVE NEJ               TO SW-INPUT-RAETT                       
179800        END-IF                                                            
179900     END-IF                                                               
180000                                                                          
180100*    ************************************************                     
180200                                                                          
180300     IF MID-IDPROJK = ALL '+'                                             
180400        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
180500           MOVE NYPON-ART-IDPROJK TO SPAR-IDPROJK                         
180600        ELSE                                                              
180700           MOVE SPACE             TO SPAR-IDPROJK                         
180800        END-IF                                                            
180900     ELSE                                                                 
181000        MOVE MID-IDPROJK       TO SPAR-IDPROJK                            
181100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR                  
181200     END-IF                                                               
181300                                                                          
181400                                                                          
181500                                                                          
181600     IF MID-IDPROJ = ALL '+'                                              
181700        CONTINUE                                                          
181800*       *******************************************    *                  
181900**      ARTC11-IDPROJ FINNS INFLYTTAT I SPAR-IDPROJ   **                  
182000*       *******************************************    *                  
182100     ELSE                                                                 
182200        MOVE MID-IDPROJ              TO SPAR-IDPROJ                       
182300     END-IF                                                               
182400                                                                          
182500     IF WS-GAMMAL-ART > WS-TVA-AAR                                        
182600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR                   
182700     ELSE                                                                 
182800        IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV OR KDPRODSL-LOCAL           
182900           PERFORM CA-KTR-PROJ-GODK                                       
183000           IF PROJ-GODK                                                   
183100              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR             
183200           ELSE                                                           
183300              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-IN-ATTR               
183400              MOVE NEJ  TO SW-INPUT-RAETT                                 
183500           END-IF                                                         
183600        ELSE                                                              
183700           IF W-KDPRODSL1 = 19                                            
183800              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR             
183900           ELSE                                                           
184000              IF SPAR-IDPROJ = SPACE                                      
184100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJ-IN-ATTR            
184200                 MOVE NEJ TO SW-INPUT-RAETT                               
184300              ELSE                                                        
184400                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJ-IN-ATTR          
184500              END-IF                                                      
184600           END-IF                                                         
184700        END-IF                                                            
184800     END-IF                                                               
184900                                                                          
185000*****************************************************************         
185100*****************************************************************         
185200* ÄT DEC 92  BASLAGERARTIKLAR                                   *         
185300*            KONTROLL TIPRODSTA PÅ PROJEKTET VID UPPDAT TIFINLV *         
185400*****************************************************************         
185500     IF MID-TISOP = ALL '+'                                               
185600        CONTINUE                                                          
185700     ELSE                                                                 
185800        IF SW-INPUT-RAETT = JA                                            
185900           IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV                          
186000              PERFORM CB-KTR-TIPRODSTA                                    
186100           END-IF                                                         
186200        END-IF                                                            
186300     END-IF                                                               
186400                                                                          
186500*****************************************************************         
186600     IF MID-IDFKNGRP = ALL '+'                                            
186700        CONTINUE                                                          
186800     ELSE                                                                 
186900        IF MID-IDFKNGRP NUMERIC                                           
187000           IF MID-IDFKNGRP > ZERO                                         
187100              MOVE MFS-NUM-FAELT-RAETT                                    
187200                                  TO MOD-IDFKNGRP-IN-ATTR                 
187300           ELSE                                                           
187400              MOVE MFS-NUM-FAELT-FEL                                      
187500                                  TO MOD-IDFKNGRP-IN-ATTR                 
187600              MOVE NEJ            TO SW-INPUT-RAETT                       
187700           END-IF                                                         
187800        ELSE                                                              
187900           MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-IN-ATTR                 
188000           MOVE NEJ               TO SW-INPUT-RAETT                       
188100        END-IF                                                            
188200     END-IF                                                               
188300                                                                          
188400     IF MID-IDPROJUP = ALL '+'                                            
188500        CONTINUE                                                          
188600     ELSE                                                                 
188700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJUP-IN-ATTR                 
188800     END-IF                                                               
188900                                                                          
189000     IF MID-IDARTNR-MOTSV = ALL '+'                                       
189100        CONTINUE                                                          
189200     ELSE                                                                 
189300        IF MID-IDARTNR-MOTSV NUMERIC                                      
189400           MOVE MFS-NUM-FAELT-RAETT                                       
189500                                  TO MOD-IDARTNR-MOTSV-IN-ATTR            
189600           MOVE MID-IDARTNR-MOTSV TO SPAR-IDARTNR-MOTSV                   
189700        ELSE                                                              
189800           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-MOTSV-IN-ATTR            
189900           MOVE NEJ               TO SW-INPUT-RAETT                       
190000        END-IF                                                            
190100     END-IF                                                               
190200                                                                          
190300     IF MID-IDRITN = ALL '+'                                              
190400        CONTINUE                                                          
190500     ELSE                                                                 
190600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRITN-IN-ATTR                   
190700     END-IF                                                               
190800                                                                          
190900     IF MID-KVARTVAGN = ALL '+'                                           
191000        CONTINUE                                                          
191100     ELSE                                                                 
191200        IF MID-KVARTVAGN NUMERIC                                          
191300           MOVE MFS-NUM-FAELT-RAETT                                       
191400                                  TO MOD-KVARTVAGN-IN-ATTR                
191500        ELSE                                                              
191600           MOVE MFS-NUM-FAELT-FEL                                         
191700                                  TO MOD-KVARTVAGN-IN-ATTR                
191800           MOVE NEJ               TO SW-INPUT-RAETT                       
191900        END-IF                                                            
192000     END-IF                                                               
192100                                                                          
192200     IF MID-IDAO (1) = ALL '+'                                            
192300        CONTINUE                                                          
192400     ELSE                                                                 
192500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (1)               
192600     END-IF                                                               
192700                                                                          
192800     IF MID-IDAO (2) = ALL '+'                                            
192900        CONTINUE                                                          
193000     ELSE                                                                 
193100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (2)               
193200     END-IF                                                               
193300                                                                          
193400     IF MID-IDAO (3) = ALL '+'                                            
193500        CONTINUE                                                          
193600     ELSE                                                                 
193700        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (3)               
193800     END-IF                                                               
193900                                                                          
194000     IF MID-IDAO (4) = ALL '+'                                            
194100        CONTINUE                                                          
194200     ELSE                                                                 
194300        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (4)               
194400     END-IF                                                               
194500                                                                          
194600     IF MID-IDAO (5) = ALL '+'                                            
194700        CONTINUE                                                          
194800     ELSE                                                                 
194900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-IN-ATTR   (5)               
195000     END-IF                                                               
195100                                                                          
195200     IF MID-TEORSAK = ALL '+'                                             
195300        CONTINUE                                                          
195400     ELSE                                                                 
195500        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEORSAK-IN-UT-ATTR               
195600        MOVE MID-TEORSAK          TO SPAR-TEORSAK                         
195700     END-IF                                                               
195800                                                                          
195900     IF SW-INPUT-RAETT = JA                                               
196000        IF MID-KDSORT = ALL '+'                                           
196100        AND MID-IDFKNGRP = ALL '+'                                        
196200           CONTINUE                                                       
196300        ELSE                                                              
196400           PERFORM CC-KOLLA-SOFTWARE                                      
196500        END-IF                                                            
196600     END-IF                                                               
196700                                                                          
196800     IF SW-INPUT-RAETT = JA                                               
196900        CONTINUE                                                          
197000     ELSE                                                                 
197100        PERFORM S02-ROER-EJ-VISADE-FAELT                                  
197200     END-IF                                                               
197300     .                                                                    
197400     EJECT                                                                
197500*           *********************************************                 
197600 D-RELATIONSKOLL SECTION.                                                 
197700     SKIP2                                                                
197800     IF SPAR-DASOP-AAAAMMDD = 99999999                                    
197900        IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                 
198000           PERFORM DA-TESTA-RSUNIK-TIKOART                                
198100        ELSE                                                              
198200           MOVE MFS-NUM-FAELT-FEL   TO MOD-TISOP-IN-ATTR                  
198300           MOVE NEJ                 TO SW-INPUT-RAETT                     
198400        END-IF                                                            
198500     END-IF                                                               
198600                                                                          
198700     PERFORM DB-TESTA-NYPON-FAELT                                         
198800                                                                          
198900     IF SW-INPUT-RAETT = JA                                               
199000        CONTINUE                                                          
199100     ELSE                                                                 
199200        PERFORM S02-ROER-EJ-VISADE-FAELT                                  
199300     END-IF                                                               
199400     .                                                                    
199500     EJECT                                                                
199600 DA-TESTA-RSUNIK-TIKOART SECTION.                                         
199700     SKIP2                                                                
199800     IF NYPON-ART-TIREGDAT = ZERO                                         
199900        MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-IN-ATTR                       
200000        MOVE NEJ               TO SW-INPUT-RAETT                          
200100*       TISOP FÅR EJ VARA 999999, VI MÅSTE VETA                           
200200*       VILKEN TISOP ARTIKELN SKALL HA.....                               
200300     ELSE                                                                 
200400*       TIKO-ARTIKEL                                                      
200500        IF NYPON-ART-IDLEVNR    = '9998 '   OR                            
200600           NYPON-ART-FLUNIKRD   = JA     OR                               
200700           NYPON-ART-KDPRODSL   = 15                                      
200800           MOVE MFS-NUM-FAELT-FEL                                         
200900                               TO MOD-TISOP-IN-ATTR                       
201000           MOVE NEJ            TO SW-INPUT-RAETT                          
201100*          TISOP FÅR EJ VARA 999999,VI FÅR ALDRIG                         
201200*          TISOP FRÅN TIKO......                                          
201300        ELSE                                                              
201400           PERFORM DAA-XXAUIDPROJK-TISERLEV                               
201500        END-IF                                                            
201600     END-IF                                                               
201700     .                                                                    
201800     EJECT                                                                
201900 DAA-XXAUIDPROJK-TISERLEV SECTION.                                        
202000     SKIP2                                                                
202100     PERFORM IMS-GHU-ARTC01                                               
202200     MOVE ART-KDPRODSL            TO SPAR-KDPRODSL                        
202300     MOVE SPAR-KDPRODSL           TO W-KDPRODSL1                          
202400     PERFORM IMS-GU-WLXXAQ01-UNIK                                         
202500     IF SEGMENT-FINNS                                                     
202600        MOVE SPAR-IDPROJ          TO W-IDPROJ                             
202700        MOVE SPAR-IDPROJOBJ       TO W-IDPROJOBJ                          
202800        MOVE SPAR-IDPROJK         TO W-IDPROJK                            
202900        PERFORM IMS-GNP-WLXXAQ11-UNIK                                     
203000        IF SEGMENT-FINNS                                                  
203100           IF XXAQ-1132-TIPRODSTA = 111111                                
203200*             LÖPANDE IDPROJK                                             
203300              PERFORM DAAA-TESTA-TISERLEV                                 
203400              IF SW-TISERLEV-FINNS = JA                                   
203500*                ART HAR SERIELEVERANSVECKA, 999999 EJ TILLÅTET           
203600                 MOVE MFS-NUM-FAELT-FEL                                   
203700                                     TO MOD-TISOP-IN-ATTR                 
203800                 MOVE NEJ            TO SW-INPUT-RAETT                    
203900              END-IF                                                      
204000           ELSE                                                           
204100              MOVE MFS-NUM-FAELT-FEL TO MOD-TISOP-IN-ATTR                 
204200              MOVE NEJ               TO SW-INPUT-RAETT                    
204300           END-IF                                                         
204400        ELSE                                                              
204500           MOVE MFS-NUM-FAELT-FEL    TO MOD-TISOP-IN-ATTR                 
204600           MOVE NEJ                  TO SW-INPUT-RAETT                    
204700        END-IF                                                            
204800     ELSE                                                                 
204900        MOVE MFS-NUM-FAELT-FEL       TO MOD-TISOP-IN-ATTR                 
205000        MOVE NEJ                     TO SW-INPUT-RAETT                    
205100     END-IF                                                               
205200     .                                                                    
205300     EJECT                                                                
205400 DAAA-TESTA-TISERLEV SECTION.                                             
205500     SKIP2                                                                
205600     MOVE 1             TO SPAR-TISERLEV-IND                              
205700                                                                          
205800     PERFORM UNTIL SPAR-TISERLEV-IND > SPAR-TISERLEV-IND-MAX              
205900             OR    SW-TISERLEV-FINNS = JA                                 
206000        IF NYPON-ART-TISERLEV (SPAR-TISERLEV-IND) = ZERO                  
206100           CONTINUE                                                       
206200        ELSE                                                              
206300           MOVE JA      TO SW-TISERLEV-FINNS                              
206400        END-IF                                                            
206500                                                                          
206600        ADD 1           TO SPAR-TISERLEV-IND                              
206700                                                                          
206800     END-PERFORM                                                          
206900     .                                                                    
207000     EJECT                                                                
207100 DB-TESTA-NYPON-FAELT SECTION.                                            
207200     SKIP2                                                                
207300     IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                    
207400        IF  WS-GAMMAL-ART > WS-TVA-AAR                                    
207500            MOVE MFS-ALFA-FAELT-RAETT                                     
207600                 TO MOD-IDPROJK-IN-ATTR                                   
207700        ELSE                                                              
207800           PERFORM DBA-KTR-GODK-XXAQPROJK                                 
207900        END-IF                                                            
208000     ELSE                                                                 
208100        IF SPAR-PRARTSTD = ZERO                                           
208200           IF  WS-GAMMAL-ART > WS-TVA-AAR                                 
208300               MOVE MFS-ALFA-FAELT-RAETT                                  
208400                    TO MOD-IDPROJK-IN-ATTR                                
208500           ELSE                                                           
208600              PERFORM DBA-KTR-GODK-XXAQPROJK                              
208700           END-IF                                                         
208800        ELSE                                                              
208900           IF MID-IDPROJK       = ALL '+'   AND                           
209000              MID-FLPISK        = ALL '+'   AND                           
209100              MID-IDARTNR-MOTSV = ALL '+'   AND                           
209200              MID-KVARTVAGN     = ALL '+'   AND                           
209300              MID-TEORSAK       = ALL '+'                                 
209400              CONTINUE                                                    
209500           ELSE                                                           
209600*          ARTIKEL FINNS INTE PÅ NYPON, ISRT KOMMER INTE ATT SKE          
209700*          ÄNDÅ HAR MAN MATAT IN PÅ FÄLT SOM BARA FINNS PÅ NYPON          
209800              IF MID-IDPROJK = ALL '+'                                    
209900                 CONTINUE                                                 
210000              ELSE                                                        
210100                 MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDPROJK-IN-ATTR          
210200                 MOVE NEJ                 TO SW-INPUT-RAETT               
210300              END-IF                                                      
210400                                                                          
210500              IF MID-FLPISK = ALL '+'                                     
210600                 CONTINUE                                                 
210700              ELSE                                                        
210800                 MOVE MFS-ALFA-FAELT-FEL  TO  MOD-FLPISK-IN-ATTR          
210900                 MOVE NEJ                 TO SW-INPUT-RAETT               
211000              END-IF                                                      
211100                                                                          
211200              IF MID-IDARTNR-MOTSV = ALL '+'                              
211300                 CONTINUE                                                 
211400              ELSE                                                        
211500                 MOVE MFS-NUM-FAELT-FEL   TO                              
211600                                   MOD-IDARTNR-MOTSV-IN-ATTR              
211700                 MOVE NEJ                 TO SW-INPUT-RAETT               
211800              END-IF                                                      
211900                                                                          
212000              IF MID-KVARTVAGN     = ALL '+'                              
212100                 CONTINUE                                                 
212200              ELSE                                                        
212300                 MOVE MFS-NUM-FAELT-FEL   TO MOD-KVARTVAGN-IN-ATTR        
212400                 MOVE NEJ                 TO SW-INPUT-RAETT               
212500              END-IF                                                      
212600                                                                          
212700              IF MID-TEORSAK       = ALL '+'                              
212800                 CONTINUE                                                 
212900              ELSE                                                        
213000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-TEORSAK-IN-UT-ATTR        
213100                 MOVE NEJ                TO SW-INPUT-RAETT                
213200              END-IF                                                      
213300           END-IF                                                         
213400        END-IF                                                            
213500     END-IF.                                                              
213600     EJECT                                                                
213700 DBA-KTR-GODK-XXAQPROJK SECTION.                                          
213800     SKIP2                                                                
213900******************************************************************        
214000***                                                                       
214100*** HÄR KONTROLLERAS ATT PROJK ÄR UPPLAGT PÅ BILD 1153 AV BEREDNIN        
214200*** GÄLLER ENDAST PRODUKTSLAG PV-BASLAGER                                 
214300******************************************************************        
214400     SKIP2                                                                
214500     IF KDPRODSL-UTAN-EMB OR KDPRODSL-LOCAL                               
214600*** *** KONTROLL PÅ PROJK SKA SKE ***  ***  ***  ***  *** *** ***         
214700        PERFORM IMS-GU-WLXXAQ01-UNIK                                      
214800        IF SEGMENT-FINNS                                                  
214900           MOVE SPAR-IDPROJK TO W-IDPROJK                                 
215000           PERFORM IMS-GNP-WLXXAQ11-PROJK                                 
215100           IF SEGMENT-FINNS                                               
215200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR            
215300           ELSE                                                           
215400              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPROJK-IN-ATTR              
215500              MOVE NEJ TO SW-INPUT-RAETT                                  
215600           END-IF                                                         
215700        END-IF                                                            
215800     ELSE                                                                 
215900        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROJK-IN-ATTR                  
216000     END-IF                                                               
216100     .                                                                    
216200     EJECT                                                                
216300 E-UPPDATERA-OCH-VISA-BILD SECTION.                                       
216400     SKIP2                                                                
216500     MOVE NEJ TO SW-KDSORT-AENDRAD-TILL-FRAN-SA                           
216600                 SW-LARMA                                                 
216700                                                                          
216800     PERFORM IMS-GHU-ARTC01                                               
216900                                                                          
217000     IF MID-TISOP    = ALL '+'  AND                                       
217100        MID-IDAO (1) = ALL '+'  AND                                       
217200        MID-IDAO (2) = ALL '+'  AND                                       
217300        MID-IDAO (3) = ALL '+'  AND                                       
217400        MID-IDAO (4) = ALL '+'  AND                                       
217500        MID-IDAO (5) = ALL '+'  AND                                       
217600        MID-KDPRODSL = ALL '+'  AND                                       
217700        MID-IDFKNGRP = ALL '+'  AND                                       
217800        MID-KDSORT   = ALL '+'                                            
217900        CONTINUE                                                          
218000     ELSE                                                                 
218100        PERFORM EA-UPPDATERING-ARTC01                                     
218200        PERFORM IMS-REPL-ARTC                                             
218300     END-IF                                                               
218400                                                                          
218500     MOVE ART-IDAO (1)               TO SPAR-IDAO1                        
218600                                                                          
218700     MOVE ART-KDPRODSL               TO SPAR-KDPRODSL                     
218800     MOVE ART-IDFKNGRP               TO SPAR-IDFKNGRP                     
218900     MOVE ART-KDSORT                 TO SPAR-KDSORT                       
219000                                                                          
219100*WDK611                                                                   
219200     PERFORM IMS-GHNP-ARTC11                                              
219300     MOVE CLAG-KDERS TO SPAR-KDERS                                        
219400     IF SW-LARMA = JA                                                     
219500       PERFORM EC-LAGG-UPP-LARM                                           
219600     END-IF                                                               
219700                                                                          
219800     IF MID-IDBERED       = ALL '+'  AND                                  
219900        MID-IDPROJ        = ALL '+'  AND                                  
220000        MID-IDKAT (1)     = ALL '+'  AND                                  
220100        MID-IDKAT (2)     = ALL '+'  AND                                  
220200        MID-IDKAT (3)     = ALL '+'  AND                                  
220300        MID-IDPROJUP      = ALL '+'  AND                                  
220400        MID-IDRITN        = ALL '+'  AND                                  
220500        MID-KDAGE         = ALL '+'  AND                                  
220600        MID-KDUART        = ALL '+'  AND                                  
220700        MID-FLLSRDEL     = ALL '+'   AND                                  
220800        MID-KDBPSR       = ALL '+'   AND                                  
220900        MID-IDPROENH (1) = ALL '+'   AND                                  
221000        MID-IDPROENH (2) = ALL '+'   AND                                  
221100        MID-IDPROENH (3) = ALL '+'   AND                                  
221200        SW-KOLLA-KDPSLLOC = NEJ      AND                                  
221300        SW-KOLLA-TISOP    = NEJ                                           
221400        CONTINUE                                                          
221500     ELSE                                                                 
221600        PERFORM EB-UPPDATERING-ARTC11                                     
221700        PERFORM IMS-REPL-ARTC                                             
221800     END-IF                                                               
221900                                                                          
222000     MOVE CLAG-IDBERED               TO SPAR-IDBERED                      
222100     MOVE CLAG-IDPROJ                TO SPAR-IDPROJ                       
222200                                                                          
222300                                                                          
222400     MOVE CLAG-IDRITN                TO SPAR-IDRITN                       
222500     MOVE CLAG-KDAGE                 TO MOD-KDAGE                         
222600     MOVE CLAG-FLLSRDEL              TO SPAR-FLLSRDEL                     
222700     MOVE CLAG-KDUART                TO SPAR-KDUART                       
222800     MOVE CLAG-KDBPSR                TO SPAR-KDBPSR                       
222900                                                                          
223000*WD6625                                                                   
223100                                                                          
223200     MOVE +6                         TO W-KDNOTTYP                        
223300     PERFORM IMS-GHNP-ARTC25                                              
223400     IF HANDELSE-OK                                                       
223500*WDR530 LÄGGER UPP PÅ HÄNDELSEBAS                                         
223600        MOVE W-IDARTNR         TO XXBW-2228-IDARTNR                       
223700        MOVE LOW-VALUE         TO XXBW-2228-LOW-VALUE                     
223800        MOVE SPACE             TO XXBW-2228-FILLER                        
223900        PERFORM IMS-ISRT-XXBW                                             
224000     END-IF                                                               
224100                                                                          
224200*WDD311                                                                   
224300     IF MSGI-IDLAND-SPR = 'GB'                                            
224400        MOVE 'GB '   TO W-IDSKYLT                                         
224500     ELSE                                                                 
224600        MOVE 'S  '   TO W-IDSKYLT                                         
224700     END-IF                                                               
224800                                                                          
224900*WDK7                                                                     
225000                                                                          
225100     IF SW-KOLLA-TISOP = JA                                               
225200        PERFORM EO-KOLLA-UPPDAT-WDK7                                      
225300     END-IF                                                               
225400                                                                          
225500     IF SW-ARTIKEL-FINNS-PA-NYPON = JA                                    
225600                                                                          
225700        IF  MID-FLPISK        = ALL '+'                                   
225800        AND MID-IDPROJK       = ALL '+'                                   
225900        AND MID-IDARTNR-MOTSV = ALL '+'                                   
226000        AND MID-KVARTVAGN     = ALL '+'                                   
226100        AND MID-TEORSAK       = ALL '+'                                   
226200        AND MID-IDAO (1)      = ALL '+'                                   
226300        AND MID-TISOP         = ALL '+'                                   
226400        AND MID-IDPROJ        = ALL '+'                                   
226500           CONTINUE                                                       
226600        ELSE                                                              
226700           PERFORM EF-UPDATNYPON-OCH-TRANSKDP                             
226800           MOVE JA  TO SW-REPL-NYPON                                      
226900        END-IF                                                            
227000                                                                          
227100        PERFORM EH-KOLLA-OM-ANSKQ-BERORS                                  
227200        PERFORM EG-KOLLA-OM-BASL-BERORS                                   
227300                                                                          
227400        IF SW-DLET-ISRT-NYPON = JA                                        
227500           PERFORM EJ-DLET-ISRT-NYPON                                     
227600        ELSE                                                              
227700           IF SW-REPL-NYPON = JA                                          
227800              PERFORM IMS-REPL-NYPON                                      
227900           END-IF                                                         
228000        END-IF                                                            
228100     ELSE                                                                 
228200        IF  SPAR-PRARTSTD = ZERO                                          
228300           PERFORM EN-NYUPPLAGG-AV-ARTIKEL                                
228400        END-IF                                                            
228500     END-IF                                                               
228600                                                                          
228700     PERFORM S03-RENSA-MOD-INMATNINGSFAELT                                
228800     PERFORM S05-FORMATETS-ATTRIBUT                                       
228900                                                                          
229000     MOVE MED-2 (SPAR-TEXT-IND)   TO MOD-TEMFSINF                         
229100     IF KDSORT-AENDRAD-TILL-FRAN-SATS                                     
229200*****  VARNING ATT SORT ÄNDRAD TILL/FRÅN SATS                             
229300       MOVE FEL-5 (SPAR-TEXT-IND) TO MOD-TEMFSFEL                         
229400     END-IF                                                               
229500     .                                                                    
229600     EJECT                                                                
229700 EA-UPPDATERING-ARTC01 SECTION.                                           
229800     SKIP2                                                                
229900     MOVE NEJ    TO SW-KOLLA-KDPSLLOC                                     
230000                    SW-KOLLA-TISOP                                        
230100     IF MID-TISOP = ALL '+'                                               
230200        MOVE NEJ TO HANDELSE-SW                                           
230300     ELSE                                                                 
230400        PERFORM EAB-EV-SKAPA-LARM                                         
230500        PERFORM EAC-UPDATE-WDGX2264-2266                                  
230600        MOVE SPAR-TISOP-AAVVD      TO ART-TISOP                           
230700                                      ART-TIFINLV                         
230800        MOVE JA                    TO HANDELSE-SW                         
230900                                      SW-KOLLA-TISOP                      
231000     END-IF                                                               
231100                                                                          
231200     IF  MID-IDAO (1) = ALL '+'                                           
231300     AND MID-IDAO (2) = ALL '+'                                           
231400     AND MID-IDAO (3) = ALL '+'                                           
231500     AND MID-IDAO (4) = ALL '+'                                           
231600     AND MID-IDAO (5) = ALL '+'                                           
231700        CONTINUE                                                          
231800     ELSE                                                                 
231900        IF MID-IDAO (1) = ALL '+'                                         
232000           CONTINUE                                                       
232100        ELSE                                                              
232200           MOVE MID-IDAO (1)          TO ART-IDAO (1)                     
232300        END-IF                                                            
232400                                                                          
232500        IF MID-IDAO (2) = ALL '+'                                         
232600           CONTINUE                                                       
232700        ELSE                                                              
232800           MOVE MID-IDAO (2)          TO ART-IDAO (2)                     
232900        END-IF                                                            
233000                                                                          
233100        IF MID-IDAO (3) = ALL '+'                                         
233200           CONTINUE                                                       
233300        ELSE                                                              
233400           MOVE MID-IDAO (3)          TO ART-IDAO (3)                     
233500        END-IF                                                            
233600                                                                          
233700        IF MID-IDAO (4) = ALL '+'                                         
233800           CONTINUE                                                       
233900        ELSE                                                              
234000           MOVE MID-IDAO (4)          TO ART-IDAO (4)                     
234100        END-IF                                                            
234200                                                                          
234300        IF MID-IDAO (5) = ALL '+'                                         
234400           CONTINUE                                                       
234500        ELSE                                                              
234600           MOVE MID-IDAO (5)          TO ART-IDAO (5)                     
234700        END-IF                                                            
234800                                                                          
234900        MOVE 1    TO SPAR-TILL-IND                                        
235000                     SPAR-FRAN-IND                                        
235100                                                                          
235200        PERFORM UNTIL SPAR-FRAN-IND > 5                                   
235300           IF ART-IDAO (SPAR-FRAN-IND) = SPACE                            
235400              CONTINUE                                                    
235500           ELSE                                                           
235600              MOVE ART-IDAO (SPAR-FRAN-IND) TO                            
235700                              ART-IDAO (SPAR-TILL-IND)                    
235800              ADD 1   TO SPAR-TILL-IND                                    
235900           END-IF                                                         
236000           ADD 1      TO SPAR-FRAN-IND                                    
236100        END-PERFORM                                                       
236200                                                                          
236300        IF SPAR-TILL-IND < 6                                              
236400           PERFORM UNTIL SPAR-TILL-IND > 5                                
236500              MOVE SPACE          TO ART-IDAO (SPAR-TILL-IND)             
236600              ADD 1               TO SPAR-TILL-IND                        
236700           END-PERFORM                                                    
236800        END-IF                                                            
236900     END-IF                                                               
237000                                                                          
237100     IF MID-KDPRODSL = ALL '+'                                            
237200        CONTINUE                                                          
237300     ELSE                                                                 
237400        IF SPAR-PRARTSTD > 0                                              
237500            PERFORM EAA-PS-1117                                           
237600        ELSE                                                              
237700            MOVE JA TO SW-KOLLA-KDPSLLOC                                  
237800            MOVE ART-KDPRODSL            TO SPAR-KDPRODSL-OLD             
237900            MOVE MID-KDPRODSL            TO ART-KDPRODSL                  
238000                                            WS-A17-KDPRODSL               
238100                                                                          
238200            MOVE KPS-IDFTG               TO ART-IDFTG                     
238300        END-IF                                                            
238400     END-IF                                                               
238500                                                                          
238600     IF MID-IDFKNGRP = ALL '+'                                            
238700        CONTINUE                                                          
238800     ELSE                                                                 
238900        MOVE JA TO SW-KOLLA-KDPSLLOC                                      
239000        MOVE MID-IDFKNGRP            TO ART-IDFKNGRP                      
239100     END-IF                                                               
239200                                                                          
239300     IF MID-KDSORT   = ALL '+'                                            
239400        CONTINUE                                                          
239500     ELSE                                                                 
239600        IF MID-KDSORT = 'SA' OR ART-KDSORT = 'SA'                         
239700          MOVE JA TO SW-KDSORT-AENDRAD-TILL-FRAN-SA                       
239800        END-IF                                                            
239900        IF MID-KDSORT = 'TM' OR ART-KDSORT = 'TM'                         
240000          MOVE JA TO SW-KDSORT-AENDRAD-TILL-FRAN-SA                       
240100        END-IF                                                            
240200        MOVE MID-KDSORT             TO ART-KDSORT                         
240300     END-IF                                                               
240400                                                                          
240500     .                                                                    
240600     EJECT                                                                
240700*           *********************************************                 
240800 EAA-PS-1117 SECTION.                                                     
240900     MOVE MID-KDPRODSL   TO TEST-KDPRODSL                                 
241000                            PSUPD-1118-KDPRODSL                           
241100     MOVE   WS-IDARTNR TO PSUPD-1118-IDARTNR                              
241200                          W-IDARTNR-1117                                  
241300     PERFORM IMS-ISRT-WEEK                                                
241400     IF STATUS-WS =  'II'                                                 
241500         PERFORM IMS-GHU-WEEK                                             
241600         IF WS-KDPRODSL =  TEST-KDPRODSL                                  
241700             PERFORM IMS-DLET-WEEK                                        
241800         ELSE                                                             
241900             MOVE  MID-KDPRODSL TO PSUPD-1118-KDPRODSL                    
242000             PERFORM IMS-REPL-WEEK                                        
242100         END-IF                                                           
242200     END-IF                                                               
242300*               **********NOLLSTÄLL FÄLT *************                    
242400     MOVE '++'                 TO MID-KDPRODSL                            
242500     MOVE MED-9 (SPAR-TEXT-IND) TO MOD-TEMFSINF                           
242600     MOVE MED-9 (SPAR-TEXT-IND) TO MOD-TEMFSFEL                           
242700     CONTINUE                                                             
242800     .                                                                    
242900     EJECT                                                                
243000 EAB-EV-SKAPA-LARM SECTION.                                               
243100     SKIP2                                                                
243200     MOVE NEJ  TO SW-LARMA                                                
243300                  SW-LARM-09                                              
243400     IF ART-FLERS = JA                                                    
243500        MOVE W-IDARTNR         TO W-IDARTNR-MIN7                          
243600                                  W-IDARTNR-MAX7                          
243700        PERFORM IMS-GN-ERSB01                                             
243800                                                                          
243900        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
244000        OR SW-LARMA = JA                                                  
244100           MOVE ERSB01-ERS-IDARTNR  TO W-IDARTNR                          
244200*          --- ANVÄNDER LÄS-PCB FÖR ATT EJ PÅVERKA UPPDAT-PCB             
244300           PERFORM IMS-GU-WDK601-PCB2                                     
244400           IF SEGMENT-FINNS AND PCB2-ART-KDERS-UTG = ZERO                 
244500              PERFORM IMS-GNP-WDK611-PCB2                                 
244600*******ÅTERSTÄLL NYCKELN FÖR LEV.PLAN-LÄSNINGEN                           
244700              MOVE WS-IDARTNR  TO W-IDARTNR                               
244800              IF PCB2-CLAG-KDERS < 20                                     
244900                 PERFORM EABA-LAS-LEVERANSPLANER                          
245000              END-IF                                                      
245100           END-IF                                                         
245200           PERFORM IMS-GN-ERSB01                                          
245300        END-PERFORM                                                       
245400*                                                                         
245500*       --- KOLLA NU OM LARM-09 SKALL SKAPAS                              
245600        MOVE WS-IDARTNR         TO W-IDARTNR-MIN7                         
245700                                   W-IDARTNR-MAX7                         
245800*       --- LÄSER ERSB MED IDARTNR-TILLK                                  
245900        PERFORM IMS-GU-ERSB01                                             
246000        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
246100*          --- KOLLA IFALL ERSATT ARTIKEL SKALL LARMAS                    
246200           MOVE ERSB01-ERS-IDARTNR  TO W-IDARTNR                          
246300           PERFORM IMS-GU-WDK601-PCB2                                     
246400           IF SEGMENT-FINNS AND PCB2-ART-KDERS-UTG = ZERO                 
246500              PERFORM IMS-GNP-WDK611-PCB2                                 
246600              IF PCB2-CLAG-KDERS = +01 OR +02 OR +03                      
246700                                OR +04 OR +05 OR +06                      
246800                                OR +07 OR +08                             
246900                 PERFORM EABB-SKAPA-LARM-ORSAK-09                         
247000                 MOVE JA TO SW-LARM-09                                    
247100              END-IF                                                      
247200           END-IF                                                         
247300           PERFORM IMS-GN-ERSB01                                          
247400        END-PERFORM                                                       
247500*       --- ÅTERSTÄLLER NYCKELN TILL IDARTNR-TILLK                        
247600        MOVE WS-IDARTNR          TO W-IDARTNR                             
247700                                                                          
247800        IF SW-LARM-09 = JA                                                
247900*          --- ERSATTA ARTIKLAR ÄR LARMADE.                               
248000*          --- LARMA DÄRFÖR NU ÄVEN DENNA ERSÄTTANDE ARTIKELN             
248100           PERFORM EABB-SKAPA-LARM-ORSAK-09                               
248200        END-IF                                                            
248300     END-IF                                                               
248400     CONTINUE                                                             
248500     .                                                                    
248600     EJECT                                                                
248700 EABA-LAS-LEVERANSPLANER SECTION.                                         
248800     MOVE W-IDARTNR          TO W-IDARTNR-D9                              
248900     MOVE WC-CDC-SE          TO W-IDDC-D9                                 
249000     PERFORM IMS-GU-D901                                                  
249100     MOVE NEJ                TO SW-LARMA                                  
249200                                                                          
249300     IF SEGMENT-FINNS                                                     
249400       PERFORM IMS-GNP-D902                                               
249500     END-IF                                                               
249600                                                                          
249700     PERFORM UNTIL SEGMENT-SAKNAS                                         
249800     OR SW-LARMA = JA                                                     
249900                                                                          
250000       IF WDD902-KVBR > ZERO                                              
250100         MOVE JA             TO SW-LARMA                                  
250200       END-IF                                                             
250300       PERFORM IMS-GNP-D905                                               
250400       PERFORM UNTIL SEGMENT-SAKNAS                                       
250500       OR SW-LARMA = JA                                                   
250600         IF  WDD905-KDAVROP = 2                                           
250700         AND WDD905-KVAVROP > ZERO                                        
250800           MOVE JA           TO SW-LARMA                                  
250900         END-IF                                                           
251000         PERFORM IMS-GNP-D905                                             
251100       END-PERFORM                                                        
251200       PERFORM IMS-GNP-D902                                               
251300     END-PERFORM                                                          
251400     CONTINUE                                                             
251500     .                                                                    
251600     EJECT                                                                
251700 EABB-SKAPA-LARM-ORSAK-09 SECTION.                                        
251800     SKIP2                                                                
251900     MOVE WC-CDC-SE  TO W-IDDC-2203                                       
252000     MOVE W-IDARTNR  TO XXBJ11-2204-IDARTNR                               
252100     MOVE +09        TO XXBJ11-2204-KDLPORS                               
252200     PERFORM IMS-ISRT-XXBJ-2204                                           
252300     .                                                                    
252400     EJECT                                                                
252500 EAC-UPDATE-WDGX2264-2266 SECTION.                                        
252600                                                                          
252700     IF ART-TISOP NOT = SPAR-TISOP-AAVVD                                  
252800        MOVE ART-TISOP                 TO W-TISOP-2264-O                  
252900        PERFORM IMS-GHU-WDGX2264-OLD                                      
253000        IF SEGMENT-FINNS                                                  
253100           MOVE WS-IDARTNR             TO W-IDARTNR-2266-O-MIN            
253200                                          W-IDARTNR-2266-O-MAX            
253300           PERFORM IMS-GHNP-WDGX2266-OLD                                  
253400           IF SEGMENT-FINNS                                               
253500              MOVE SPAR-TISOP-AAVVD    TO NEW-2264-TISOP                  
253600              PERFORM IMS-ISRT-WDGX2264-NEW                               
253700           END-IF                                                         
253800           PERFORM UNTIL SEGMENT-SAKNAS                                   
253900              MOVE OLD-2266-WDGX2266   TO NEW-2266-WDGX2266               
254000              PERFORM IMS-ISRT-WDGX2266-NEW                               
254100              PERFORM IMS-DLET-WDGX2266-OLD                               
254200                                                                          
254300              PERFORM IMS-GHNP-WDGX2266-OLD                               
254400           END-PERFORM                                                    
254500                                                                          
254600           PERFORM IMS-GHU-WDGX2264-OLD                                   
254700           IF SEGMENT-FINNS                                               
254800              PERFORM IMS-GNP-WDGX2266                                    
254900              IF SEGMENT-SAKNAS                                           
255000                 PERFORM IMS-GHU-WDGX2264-OLD                             
255100                 PERFORM IMS-DLET-WDGX2264-OLD                            
255200              END-IF                                                      
255300           END-IF                                                         
255400        END-IF                                                            
255500     END-IF                                                               
255600     .                                                                    
255700     EJECT                                                                
255800 EB-UPPDATERING-ARTC11 SECTION.                                           
255900                                                                          
256000     IF MID-IDBERED  = ALL '+'                                            
256100        CONTINUE                                                          
256200     ELSE                                                                 
256300        MOVE CLAG-IDBERED         TO SPAR-IDBERED-OLD                     
256400        MOVE MID-IDBERED          TO CLAG-IDBERED                         
256500                                     SPAR-IDBERED                         
256600                                                                          
256700     END-IF                                                               
256800                                                                          
256900                                                                          
257000     IF MID-IDPROJ   = ALL '+'                                            
257100        CONTINUE                                                          
257200     ELSE                                                                 
257300        MOVE MID-IDPROJ           TO CLAG-IDPROJ                          
257400     END-IF                                                               
257500                                                                          
257600     IF  MID-IDKAT (1) = ALL '+'                                          
257700     AND MID-IDKAT (2) = ALL '+'                                          
257800     AND MID-IDKAT (3) = ALL '+'                                          
257900        CONTINUE                                                          
258000     ELSE                                                                 
258100        IF MID-IDKAT (1) = ALL '+'                                        
258200           CONTINUE                                                       
258300        ELSE                                                              
258400           MOVE MID-IDKAT (1)          TO CLAG-IDKAT (1)                  
258500        END-IF                                                            
258600                                                                          
258700        IF MID-IDKAT (2) = ALL '+'                                        
258800           CONTINUE                                                       
258900        ELSE                                                              
259000           MOVE MID-IDKAT (2)         TO CLAG-IDKAT (2)                   
259100        END-IF                                                            
259200                                                                          
259300        IF MID-IDKAT (3) = ALL '+'                                        
259400           CONTINUE                                                       
259500        ELSE                                                              
259600           MOVE MID-IDKAT (3)         TO CLAG-IDKAT (3)                   
259700        END-IF                                                            
259800                                                                          
259900        MOVE 1   TO SPAR-TILL-IND                                         
260000                    SPAR-FRAN-IND                                         
260100                                                                          
260200        PERFORM UNTIL SPAR-FRAN-IND > 3                                   
260300           IF CLAG-IDKAT (SPAR-FRAN-IND) = SPACE                          
260400              CONTINUE                                                    
260500           ELSE                                                           
260600              MOVE CLAG-IDKAT (SPAR-FRAN-IND) TO                          
260700                              CLAG-IDKAT (SPAR-TILL-IND)                  
260800              ADD 1  TO SPAR-TILL-IND                                     
260900           END-IF                                                         
261000           ADD 1   TO SPAR-FRAN-IND                                       
261100        END-PERFORM                                                       
261200                                                                          
261300        IF SPAR-TILL-IND < 4                                              
261400           PERFORM UNTIL SPAR-TILL-IND > 3                                
261500              MOVE SPACE   TO CLAG-IDKAT (SPAR-TILL-IND)                  
261600              ADD 1        TO SPAR-TILL-IND                               
261700           END-PERFORM                                                    
261800        END-IF                                                            
261900     END-IF                                                               
262000                                                                          
262100     IF MID-IDPROJUP = ALL '+'                                            
262200        CONTINUE                                                          
262300     ELSE                                                                 
262400        MOVE MID-IDPROJUP          TO CLAG-IDPROJUP                       
262500     END-IF                                                               
262600                                                                          
262700     IF MID-IDRITN = ALL '+'                                              
262800        CONTINUE                                                          
262900     ELSE                                                                 
263000        MOVE MID-IDRITN            TO CLAG-IDRITN                         
263100     END-IF                                                               
263200                                                                          
263300     IF MID-KDAGE = ALL '+'                                               
263400        CONTINUE                                                          
263500     ELSE                                                                 
263600       MOVE SPAR-KDAGE            TO CLAG-KDAGE                           
263700     END-IF                                                               
263800                                                                          
263900     IF MID-KDUART = ALL '+'                                              
264000        CONTINUE                                                          
264100     ELSE                                                                 
264200        MOVE CLAG-KDUART           TO SPAR-KDUART-OLD                     
264300        MOVE MID-KDUART            TO CLAG-KDUART                         
264400     END-IF                                                               
264500                                                                          
264600     IF MID-FLLSRDEL = ALL '+'                                            
264700        CONTINUE                                                          
264800     ELSE                                                                 
264900        MOVE CLAG-FLLSRDEL          TO SPAR-FLLSRDEL-OLD                  
265000        MOVE MID-FLLSRDEL           TO CLAG-FLLSRDEL                      
265100        MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-FLLSRDEL-ATTR                  
265200*********                                                                 
265300        MOVE 'J'                    TO CLAG-FLREFILL                      
265400        IF SPAR-KDPRODSL > 70 AND < 75                                    
265500           MOVE 'N'     TO CLAG-FLREFILL                                  
265600        ELSE                                                              
265700           IF SPAR-KDPRODSL = 16 AND SPAR-IDFKNGRP = 3955                 
265800              MOVE 'N'  TO CLAG-FLREFILL                                  
265900           ELSE                                                           
266000              IF CLAG-FLLSRDEL = 'N'                                      
266100                 MOVE 'N' TO CLAG-FLREFILL                                
266200              END-IF                                                      
266300           END-IF                                                         
266400        END-IF                                                            
266500*********                                                                 
266600     END-IF                                                               
266700                                                                          
266800     IF MID-KDBPSR   = ALL '+'                                            
266900        CONTINUE                                                          
267000     ELSE                                                                 
267100        MOVE MID-KDBPSR             TO CLAG-KDBPSR                        
267200     END-IF                                                               
267300                                                                          
267400     IF MID-IDPROENH (1) = ALL '+'                                        
267500        CONTINUE                                                          
267600     ELSE                                                                 
267700        MOVE MID-IDPROENH (1) TO CLAG-IDPROENH(1)                         
267800     END-IF                                                               
267900                                                                          
268000     IF MID-IDPROENH (2) = ALL '+'                                        
268100        CONTINUE                                                          
268200     ELSE                                                                 
268300        MOVE MID-IDPROENH (2) TO CLAG-IDPROENH(2)                         
268400     END-IF                                                               
268500                                                                          
268600     IF MID-IDPROENH (3) = ALL '+'                                        
268700        CONTINUE                                                          
268800     ELSE                                                                 
268900        MOVE MID-IDPROENH (3) TO CLAG-IDPROENH(3)                         
269000     END-IF                                                               
269100                                                                          
269200     IF SW-KOLLA-KDPSLLOC = JA                                            
269300        PERFORM EBA-KOLLA-KDPSLLOC                                        
269400     END-IF                                                               
269500     .                                                                    
269600     EJECT                                                                
269700 EBA-KOLLA-KDPSLLOC SECTION.                                              
269800                                                                          
269900     MOVE CLAG-KDPSLLOC              TO WS-KDPSLLOC-OLD                   
270000                                                                          
270100     MOVE W-IDARTNR                  TO LPC-IDARTNR-IN                    
270200     MOVE SPAR-IDFKNGRP              TO LPC-IDFKNGRP-IN                   
270300     MOVE SPAR-KDPRODSL              TO LPC-KDPRODSL-IN                   
270400     MOVE ZERO                       TO LPC-KDPSLLOC-UT                   
270500     CALL W100LPC  USING LPC-AREA                                         
270600                                                                          
270700     MOVE LPC-KDPSLLOC-UT            TO CLAG-KDPSLLOC                     
270800                                                                          
270900     PERFORM EBAA-FYLL-WDR8-A17                                           
271000     .                                                                    
271100     EJECT                                                                
271200 EBAA-FYLL-WDR8-A17 SECTION.                                              
271300                                                                          
271400     PERFORM IMS-GN-WDK711                                                
271500     IF SEGMENT-FINNS                                                     
271600       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
271700         MOVE SLAG-IDDC TO W-IDDC-A17                                     
271800         PERFORM IMS-GU-WDB601                                            
271900         IF DCS-NDC-NA                                                    
272000           IF DCS-USA                                                     
272100               MOVE SLAG-PRAVCOST  TO A17-PRAVCOST                        
272200               MOVE SLAG-KVLS      TO A17-KVLS                            
272300               MOVE SLAG-KVEFRS    TO A17-KVEFRS                          
272400               MOVE '53'           TO A17-IDFTG                           
272500               PERFORM EBAAA-SKAPA-A17                                    
272600           END-IF                                                         
272700                                                                          
272800           IF DCS-CANADA                                                  
272900               MOVE SLAG-PRAVCOST  TO A17-PRAVCOST                        
273000               MOVE SLAG-KVLS      TO A17-KVLS                            
273100               MOVE SLAG-KVEFRS    TO A17-KVEFRS                          
273200               MOVE '54'           TO A17-IDFTG                           
273300               PERFORM EBAAA-SKAPA-A17                                    
273400           END-IF                                                         
273500         END-IF                                                           
273600         PERFORM IMS-GN-WDK711                                            
273700       END-PERFORM                                                        
273800     END-IF                                                               
273900     .                                                                    
274000     EJECT                                                                
274100 EBAAA-SKAPA-A17 SECTION.                                                 
274200                                                                          
274300     MOVE 'A17'                  TO A17-IDPTYP                            
274400     MOVE 'M21'                  TO A17-KDEKOHT                           
274500     MOVE W-IDDC-A17             TO A17-IDDC-REC                          
274600     MOVE MSGI-IDDC              TO A17-IDDC-SEND                         
274700     MOVE WS-A17-KDPRODSL        TO A17-KDPRODSL                          
274800     MOVE W-IDARTNR              TO A17-IDARTNR                           
274900     MOVE WS-KDPSLLOC-OLD        TO A17-KDPSLLOC-OLD                      
275000     MOVE CLAG-KDPSLLOC          TO A17-KDPSLLOC-NEW                      
275100                                                                          
275200     MOVE 'W9041100'             TO FIL-IDPGM                             
275300     MOVE FUNCTION CURRENT-DATE (1:8) TO                                  
275400     DAGENS-TIAAAAMMDD                                                    
275500     ACCEPT WS-HHMMSSTH FROM TIME                                         
275600     MOVE WS-HHMMSSTH            TO FIL-TIKLOCK                           
275700                                                                          
275800     MOVE DAGENS-TIAAAAMMDD      TO FIL-TIREGDAT                          
275900     MOVE DAGENS-TIAAAAMMDD      TO A17-DAJUSTDA                          
276000     ADD +1                      TO W-IDSEKVNR                            
276100     MOVE W-IDSEKVNR             TO FIL-IDSEKVNR                          
276200     MOVE 'W510A17 '             TO FIL-IDCPYTXT                          
276300     MOVE A17-W510A17            TO FIL-WDR801-DATA                       
276400                                                                          
276500     PERFORM IMS-ISRT-WDR801                                              
276600     IF SEGMENT-FINNS-REDAN                                               
276700       PERFORM UNTIL SEGMENT-FINNS                                        
276800         ADD +1                      TO W-IDSEKVNR                        
276900         MOVE W-IDSEKVNR             TO FIL-IDSEKVNR                      
277000         PERFORM IMS-ISRT-WDR801                                          
277100       END-PERFORM                                                        
277200     END-IF                                                               
277300     .                                                                    
277400     EJECT                                                                
277500 EC-LAGG-UPP-LARM SECTION.                                                
277600                                                                          
277700     MOVE CLAG-IDANSK TO W-IDANSK-2232                                    
277800     PERFORM IMS-GU-R220                                                  
277900     IF SEGMENT-FINNS                                                     
278000       MOVE WDR220-2232-IDANSK-LARM TO W-IDANSK-2223                      
278100     ELSE                                                                 
278200       MOVE ZERO TO W-IDANSK-2223                                         
278300     END-IF                                                               
278400     MOVE '2223'              TO WDR501-2223-IDHTYP                       
278500     MOVE W-IDANSK-2223       TO WDR501-2223-IDANSK                       
278600     MOVE LOW-VALUE           TO WDR501-2223-LOW-VALUE                    
278700     PERFORM IMS-ISRT-R501                                                
278800     PERFORM IMS-GHU-R501                                                 
278900     MOVE FUNCTION CURRENT-DATE(3:6)                                      
279000                              TO WDR550-2224-TISENBEK-DAG                 
279100     MOVE FUNCTION CURRENT-DATE(11:6)                                     
279200                              TO WDR550-2224-TISENBEK-KL                  
279300     MOVE 600                 TO WDR550-2224-KDLARM                       
279400     MOVE W-IDARTNR           TO WDR550-2224-IDARTNR                      
279500     MOVE WC-CDC-SE           TO WDR550-2224-IDDC                         
279600     MOVE JA                  TO WDR550-2224-FLNYLARM                     
279700     MOVE ZERO                TO WDR550-2224-IDDISTR                      
279800                                 WDR550-2224-IDKUNDNR                     
279900     MOVE '0000000   '        TO WDR550-2224-IDKUNDRF                     
280000     MOVE 1                   TO WDR550-2224-IDLOPNR                      
280100     MOVE SPAR-DAGENS-DATUM   TO WDR550-2224-TIREGDAT                     
280200     MOVE SPACE               TO WDR550-2224-IDTRANS                      
280300                                 WDR550-2224-KDMFSFOR                     
280400     MOVE ZERO                TO WDR550-2224-IDKR                         
280500     MOVE SPACE               TO WDR550-2224-IDLEVNR                      
280600                                                                          
280700     PERFORM IMS-ISRT-R550                                                
280800                                                                          
280900     .                                                                    
281000     EJECT                                                                
281100 EF-UPDATNYPON-OCH-TRANSKDP     SECTION.                                  
281200     SKIP3                                                                
281300     IF MID-FLPISK = ALL '+'                                              
281400        CONTINUE                                                          
281500     ELSE                                                                 
281600        MOVE MID-FLPISK             TO NYPON-ART-FLPISK                   
281700     END-IF                                                               
281800                                                                          
281900     IF MID-IDPROJK = ALL '+'                                             
282000        CONTINUE                                                          
282100     ELSE                                                                 
282200        MOVE MID-IDPROJK         TO NYPON-ART-IDPROJK                     
282300     END-IF                                                               
282400                                                                          
282500     IF MID-IDARTNR-MOTSV = ALL '+'                                       
282600        CONTINUE                                                          
282700     ELSE                                                                 
282800        MOVE MID-IDARTNR-MOTSV     TO NYPON-ART-IDARTNR-MOTSV             
282900     END-IF                                                               
283000                                                                          
283100                                                                          
283200     IF MID-KVARTVAGN     = ALL '+'                                       
283300        CONTINUE                                                          
283400     ELSE                                                                 
283500        MOVE MID-KVARTVAGN         TO NYPON-ART-KVARTVAGN                 
283600     END-IF                                                               
283700                                                                          
283800     IF MID-TEORSAK       = ALL '+'                                       
283900        CONTINUE                                                          
284000     ELSE                                                                 
284100        MOVE MID-TEORSAK           TO NYPON-ART-TEORSAK                   
284200        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEORSAK-IN-UT-ATTR              
284300     END-IF                                                               
284400                                                                          
284500     IF MID-IDAO (1)      = ALL '+'                                       
284600        CONTINUE                                                          
284700     ELSE                                                                 
284800        MOVE MID-IDAO (1)            TO NYPON-ART-IDAO                    
284900     END-IF                                                               
285000                                                                          
285100     IF MID-TISOP         = ALL '+'                                       
285200        CONTINUE                                                          
285300     ELSE                                                                 
285400        MOVE SPAR-DASOP-AAAAMMDD     TO NYPON-ART-DAFINLEV                
285500     END-IF                                                               
285600                                                                          
285700     IF MID-IDPROJ        = ALL '+'                                       
285800        CONTINUE                                                          
285900     ELSE                                                                 
286000        MOVE MID-IDPROJ              TO NYPON-ART-IDPROJ                  
286100     END-IF                                                               
286200                                                                          
286300     IF NYPON-ART-KDRESBED = 'E' OR 'U'                                   
286400        CONTINUE                                                          
286500     ELSE                                                                 
286600        PERFORM EFA-UPPDATERA-KDRESBED                                    
286700        IF SW-ISRT-TRANS-TILL-KDP     = JA                                
286800           PERFORM EFB-SKICKA-TRANS-TILL-KDP                              
286900        END-IF                                                            
287000     END-IF                                                               
287100     .                                                                    
287200     EJECT                                                                
287300 EFA-UPPDATERA-KDRESBED SECTION.                                          
287400     SKIP2                                                                
287500     IF SPAR-KDBPSR = 8  AND  SPAR-FLLSRDEL = NEJ                         
287600        IF NYPON-ART-KDRESBED = '-'                                       
287700           CONTINUE                                                       
287800        ELSE                                                              
287900           MOVE '-'               TO NYPON-ART-KDRESBED                   
288000                                     WS-KDRESBED                          
288100           MOVE JA                TO SW-ISRT-TRANS-TILL-KDP               
288200        END-IF                                                            
288300     ELSE                                                                 
288400        IF NYPON-ART-KDRESBED = 'R'                                       
288500           CONTINUE                                                       
288600        ELSE                                                              
288700           MOVE 'R'               TO NYPON-ART-KDRESBED                   
288800                                     WS-KDRESBED                          
288900           MOVE JA                TO SW-ISRT-TRANS-TILL-KDP               
289000        END-IF                                                            
289100     END-IF                                                               
289200     IF MID-KDPRODSL = ALL '+'                                            
289300        MOVE SPAR-KDPRODSL        TO TEST-KDPRODSL                        
289400        IF KDPRODSL-VCBV                                                  
289500           MOVE NEJ               TO SW-ISRT-TRANS-TILL-KDP               
289600        END-IF                                                            
289700     ELSE                                                                 
289800        MOVE SPAR-KDPRODSL-OLD    TO TEST-KDPRODSL                        
289900        IF KDPRODSL-VCBV                                                  
290000           MOVE SPAR-KDPRODSL     TO TEST-KDPRODSL                        
290100           IF KDPRODSL-VCBV                                               
290200              MOVE NEJ            TO SW-ISRT-TRANS-TILL-KDP               
290300           ELSE                                                           
290400              CONTINUE                                                    
290500           END-IF                                                         
290600        ELSE                                                              
290700           MOVE SPAR-KDPRODSL     TO TEST-KDPRODSL                        
290800           IF KDPRODSL-VCBV                                               
290900              MOVE NEJ            TO SW-ISRT-TRANS-TILL-KDP               
291000           ELSE                                                           
291100              CONTINUE                                                    
291200           END-IF                                                         
291300        END-IF                                                            
291400     END-IF                                                               
291500     .                                                                    
291600     EJECT                                                                
291700 EFB-SKICKA-TRANS-TILL-KDP     SECTION.                                   
291800     SKIP3                                                                
291900     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
292000     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
292100******************************************************************        
292200*    IDLOGLOP= 3, FÖR ATT SKILJA TRANSAR FRÅN 1113,1115,1117,1142         
292300******************************************************************        
292400     MOVE 3                          TO W-IDLOGLOP                        
292500     MOVE W-IDLOGLOP                 TO ZZAC-IDLOGLOP                     
292600                                                                          
292700     MOVE 'RZU'                      TO KDP-IDPTYP                        
292800                                                                          
292900     MOVE WS-KDRESBED                TO KDP-KDUART                        
293000                                                                          
293100     MOVE W-IDARTNR                  TO WS-IDARTNR-OPACKAT                
293200     MOVE WS-IDARTNR-OPACKAT         TO KDP-IDARTNR                       
293300                                        W092-SORTBGP                      
293400                                                                          
293500     MOVE KDP-W10111                 TO ZZAC-LOGGPOST                     
293600     MOVE W092-AREA                  TO ZZAC-SORTPOST                     
293700     PERFORM IMS-ISRT-ZZAC.                                               
293800                                                                          
293900     EJECT                                                                
294000 EG-KOLLA-OM-BASL-BERORS SECTION.                                         
294100     SKIP3                                                                
294200*****************************************************************         
294300*  ÄT NOV 92  ARTIKEL MED KDUART SKA INTE TILL BASLAGER         *         
294400*****************************************************************         
294500                                                                          
294600     PERFORM EGA-TESTA-OM-KDPRODSL-BYTE                                   
294700     IF SW-DLET-ISRT-NYPON = JA                                           
294800        CONTINUE                                                          
294900     ELSE                                                                 
295000        PERFORM EGB-TESTA-OM-IDPROJ-BYTE                                  
295100        IF SW-DLET-ISRT-NYPON = JA                                        
295200           CONTINUE                                                       
295300        ELSE                                                              
295400           PERFORM EGC-TESTA-OM-FLLSRDEL-BYTE                             
295500           IF SW-DLET-ISRT-NYPON = JA                                     
295600              CONTINUE                                                    
295700           ELSE                                                           
295800              PERFORM EGE-TESTA-OM-KDUART-BYTE                            
295900           END-IF                                                         
296000        END-IF                                                            
296100     END-IF                                                               
296200     .                                                                    
296300     EJECT                                                                
296400 EGA-TESTA-OM-KDPRODSL-BYTE SECTION.                                      
296500     SKIP3                                                                
296600     IF MID-KDPRODSL = ALL '+'                                            
296700        CONTINUE                                                          
296800     ELSE                                                                 
296900        IF MID-KDPRODSL = SPAR-KDPRODSL-OLD                               
297000           CONTINUE                                                       
297100        ELSE                                                              
297200           PERFORM EGAA-KDPRODSL-BYTE-UTFORT                              
297300        END-IF                                                            
297400     END-IF.                                                              
297500     EJECT                                                                
297600 EGAA-KDPRODSL-BYTE-UTFORT SECTION.                                       
297700     SKIP2                                                                
297800                                                                          
297900*    TEST OM PRODUKTSLAGSBYTET PÅVERKAR   B A S L A G E R                 
298000                                                                          
298100     IF (MID-KDPRODSL           = 15 OR 16 OR 17 OR 13)  AND              
298200        (SPAR-KDPRODSL-OLD      = 11 OR 14)                               
298300                                                                          
298400*       PRODUKTSLAGSBYTE FRÅN 11/14 ---> 15/16/17                         
298500                                                                          
298600        IF NYPON-ART-TISTOMREG > ZERO                                     
298700           MOVE NYPON-ART-IDPROJ      TO W-IDPROJ-1123                    
298800           PERFORM IMS-GU-WLXXAP01-UNIK                                   
298900*          LÄSER ALLTID MED PRODUKTSLAG 11                                
299000           IF SEGMENT-FINNS                                               
299100              PERFORM IMS-GNP-WLXXAP11                                    
299200              IF SEGMENT-FINNS                                            
299300                 MOVE XXAP-1124-TIPROJSTO                                 
299400                                      TO NYPON-ART-TISTOMREG              
299500                 MOVE JA              TO SW-REPL-NYPON                    
299600              END-IF                                                      
299700           END-IF                                                         
299800        END-IF                                                            
299900     ELSE                                                                 
300000        MOVE MID-KDPRODSL               TO TEST-KDPRODSL                  
300100        IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV                             
300200           MOVE SPAR-KDPRODSL-OLD       TO TEST-KDPRODSL                  
300300           IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV                          
300400              CONTINUE                                                    
300500           ELSE                                                           
300600*             PRODUKTSLAGSBYTE FRÅN ANNAT ----> 11/14/15/16/17/18         
300700*                                               21/24/25/26/27/28/        
300800*                                               29                        
300900              IF SPAR-FLLSRDEL       = JA                                 
301000              AND SPAR-KDUART        = SPACE                              
301100                 MOVE 1                 TO NYPON-ART-DABASL               
301200                 MOVE JA                TO SW-REPL-NYPON                  
301300              END-IF                                                      
301400           END-IF                                                         
301500        ELSE                                                              
301600           MOVE SPAR-KDPRODSL-OLD       TO TEST-KDPRODSL                  
301700           IF KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV                          
301800                                                                          
301900*             PRODUKTSLAGSBYTE FRÅN 11/14/15/16/17/18                     
302000*                                   21/24/25/26/27/28/29                  
302100*                                                  ----> ANNAT            
302200                                                                          
302300              MOVE ZERO                 TO  NYPON-ART-DABASL              
302400              MOVE JA                   TO  SW-DLET-ISRT-NYPON            
302500           END-IF                                                         
302600        END-IF                                                            
302700     END-IF.                                                              
302800     EJECT                                                                
302900 EGB-TESTA-OM-IDPROJ-BYTE SECTION.                                        
303000     SKIP3                                                                
303100     IF MID-IDPROJ = ALL '+'                                              
303200        CONTINUE                                                          
303300     ELSE                                                                 
303400        IF MID-IDPROJ = SPAR-IDPROJ-OLD                                   
303500           CONTINUE                                                       
303600        ELSE                                                              
303700           PERFORM EGBA-IDPROJ-BYTE-UTFORT                                
303800        END-IF                                                            
303900     END-IF.                                                              
304000     EJECT                                                                
304100 EGBA-IDPROJ-BYTE-UTFORT SECTION.                                         
304200     SKIP3                                                                
304300     IF NYPON-ART-DABASL > 1                                              
304400        MOVE SPAR-KDPRODSL           TO TEST-KDPRODSL                     
304500        IF (KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV)                           
304600        AND SPAR-FLLSRDEL = JA                                            
304700        AND SPAR-KDUART   = SPACE                                         
304800           MOVE 1                 TO NYPON-ART-DABASL                     
304900        ELSE                                                              
305000           MOVE ZERO              TO NYPON-ART-DABASL                     
305100        END-IF                                                            
305200        MOVE JA              TO SW-DLET-ISRT-NYPON                        
305300     END-IF.                                                              
305400     EJECT                                                                
305500 EGC-TESTA-OM-FLLSRDEL-BYTE SECTION.                                      
305600     SKIP3                                                                
305700     IF MID-FLLSRDEL = ALL '+'                                            
305800        CONTINUE                                                          
305900     ELSE                                                                 
306000        IF MID-FLLSRDEL = SPAR-FLLSRDEL-OLD                               
306100           CONTINUE                                                       
306200        ELSE                                                              
306300           PERFORM EGCA-FLLSRDEL-BYTE-UTFORT                              
306400        END-IF                                                            
306500     END-IF.                                                              
306600     EJECT                                                                
306700 EGCA-FLLSRDEL-BYTE-UTFORT SECTION.                                       
306800     SKIP3                                                                
306900     IF  MID-FLLSRDEL = JA                                                
307000     AND SPAR-FLLSRDEL-OLD = NEJ                                          
307100*       A R T I K E L  S K A L L  T I L L  B A S L A G E R                
307200        MOVE SPAR-KDPRODSL        TO TEST-KDPRODSL                        
307300        IF (KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV)                           
307400        AND SPAR-KDUART        = SPACE                                    
307500           MOVE 1                 TO NYPON-ART-DABASL                     
307600           MOVE JA                TO SW-REPL-NYPON                        
307700        END-IF                                                            
307800     ELSE                                                                 
307900        IF NYPON-ART-DABASL > 1                                           
308000           MOVE ZERO              TO NYPON-ART-DABASL                     
308100           MOVE JA                TO SW-DLET-ISRT-NYPON                   
308200        END-IF                                                            
308300     END-IF                                                               
308400     .                                                                    
308500     EJECT                                                                
308600 EGE-TESTA-OM-KDUART-BYTE SECTION.                                        
308700     SKIP3                                                                
308800*    G Ä L L E R    B A S L A G E R B E H A N D L I N G                   
308900     IF MID-KDUART = ALL '+'                                              
309000        CONTINUE                                                          
309100     ELSE                                                                 
309200        IF MID-KDUART = SPAR-KDUART-OLD                                   
309300           CONTINUE                                                       
309400        ELSE                                                              
309500           IF MID-KDUART > SPACE AND SPAR-KDUART-OLD > SPACE              
309600              CONTINUE                                                    
309700           ELSE                                                           
309800              PERFORM EGEA-KDUART-BYTE-UTFORT                             
309900           END-IF                                                         
310000        END-IF                                                            
310100     END-IF                                                               
310200     .                                                                    
310300     EJECT                                                                
310400 EGEA-KDUART-BYTE-UTFORT SECTION.                                         
310500     SKIP3                                                                
310600     IF  MID-KDUART > SPACE                                               
310700     AND SPAR-KDUART-OLD = SPACE                                          
310800        IF NYPON-ART-DABASL > 1                                           
310900           MOVE ZERO              TO NYPON-ART-DABASL                     
311000           MOVE JA                TO SW-DLET-ISRT-NYPON                   
311100        END-IF                                                            
311200     ELSE                                                                 
311300        MOVE SPAR-KDPRODSL        TO TEST-KDPRODSL                        
311400******** ARTIKEL SKA TILL BASLAGER                                        
311500                                                                          
311600        IF (KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV)                           
311700           AND SPAR-FLLSRDEL = JA                                         
311800           MOVE 1                 TO NYPON-ART-DABASL                     
311900           MOVE JA                TO SW-REPL-NYPON                        
312000        END-IF                                                            
312100     END-IF                                                               
312200     .                                                                    
312300     EJECT                                                                
312400 EH-KOLLA-OM-ANSKQ-BERORS SECTION.                                        
312500     SKIP3                                                                
312600     IF SPAR-PRARTSTD = ZERO                                              
312700        IF (SPAR-KDBPSR = 8  AND  SPAR-FLLSRDEL = NEJ)  OR                
312800           (SPAR-KDERS         > 20   )                                   
312900           IF NYPON-ART-KDANSKQ = ZERO                                    
313000              CONTINUE                                                    
313100           ELSE                                                           
313200*             A R T I K E L  S K A L L  E J  T I L L  A N S K K Ö         
313300              MOVE ZERO              TO NYPON-ART-KDANSKQ                 
313400              MOVE JA                TO SW-REPL-NYPON                     
313500           END-IF                                                         
313600        ELSE                                                              
313700           IF NYPON-ART-KDANSKQ = 1                                       
313800              CONTINUE                                                    
313900           ELSE                                                           
314000*             A R T I K E L  S K A L L  T I L L  A N S K K Ö              
314100              IF SPAR-KDERS > 10                                          
314200                 CONTINUE                                                 
314300              ELSE                                                        
314400                 MOVE JA                TO SW-REPL-NYPON                  
314500                 MOVE 1                 TO NYPON-ART-KDANSKQ              
314600                                                                          
314700                 IF SPAR-IDANSK = ZERO                                    
314800                    IF SPAR-KDPRODSL = 11                                 
314900                       IF NYPON-ART-IDLEVNR = SPACE OR '9996 '            
315000                                         OR '9997 ' OR '9998 '            
315100                                         OR '9999 '                       
315200                          PERFORM S10-HAEMTA-ANSK-XXAT                    
315300                       ELSE                                               
315400                          MOVE NYPON-ART-IDLEVNR TO W-IDLEVNR             
315500                          PERFORM IMS-GU-LEVA01                           
315600                          IF SEGMENT-FINNS                                
315700                             MOVE LEVA01-LEV-IDANSK-PG(1)                 
315800                             TO NYPON-ART-IDANSK                          
315900                          ELSE                                            
316000                             PERFORM S10-HAEMTA-ANSK-XXAT                 
316100                          END-IF                                          
316200                       END-IF                                             
316300                    ELSE                                                  
316400                       PERFORM S10-HAEMTA-ANSK-XXAT                       
316500                    END-IF                                                
316600                 END-IF                                                   
316700              END-IF                                                      
316800           END-IF                                                         
316900        END-IF                                                            
317000     END-IF.                                                              
317100     EJECT                                                                
317200 EJ-DLET-ISRT-NYPON SECTION.                                              
317300     SKIP2                                                                
317400*    ÄNDRING SOM ORSAKAR RENSNING AV BASLAGER UTFÖRD                      
317500*                                                                         
317600     MOVE NYPON-ART-WDD201      TO SPAR-ART-WDD201                        
317700     PERFORM IMS-DLET-NYPON                                               
317800     MOVE ZERO                  TO  SPAR-ART-KVBASL                       
317900                                    SPAR-ART-TISTOMREG                    
318000     MOVE SPACE                  TO SPAR-ART-TEARTNOT-BASL                
318100                                    SPAR-ART-FLBASL                       
318200     MOVE SPAR-ART-WDD201        TO NYPON-ART-WDD201                      
318300                                                                          
318400     PERFORM IMS-ISRT-NYPON                                               
318500     PERFORM IMS-GHU-ARTG01-UTAN-GE                                       
318600                                                                          
318700     EJECT                                                                
318800     .                                                                    
318900 EN-NYUPPLAGG-AV-ARTIKEL SECTION.                                         
319000     SKIP2                                                                
319100     MOVE W-IDARTNR                     TO NYPON-ART-IDARTNR              
319200     MOVE SPAR-KDSORT                   TO NYPON-ART-KDSORT               
319300     MOVE SPAR-IDFKNGRP                 TO NYPON-ART-IDFKNGRP             
319400     MOVE SPAR-KDPRODSL                 TO NYPON-ART-KDPRODSL             
319500                                           TEST-KDPRODSL                  
319600                                                                          
319700     MOVE SPAR-DASOP-AAAAMMDD           TO NYPON-ART-DAFINLEV             
319800     MOVE SPAR-IDAO1                    TO NYPON-ART-IDAO                 
319900     MOVE SPAR-IDBERED                  TO NYPON-ART-IDBERED              
320000     MOVE SPAR-IDPROJ                   TO NYPON-ART-IDPROJ               
320100     MOVE SPAR-IDRITN                   TO NYPON-ART-IDRITN               
320200     MOVE SPAR-IDPROENH1                TO NYPON-ART-IDPROENH             
320300                                                                          
320400     MOVE SPAR-FLPISK                   TO NYPON-ART-FLPISK               
320500     MOVE SPAR-KVPROG                   TO NYPON-ART-KVPROG               
320600     MOVE SPAR-IDPROJK                  TO NYPON-ART-IDPROJK              
320700     MOVE SPAR-IDARTNR-MOTSV            TO NYPON-ART-IDARTNR-MOTSV        
320800     MOVE SPAR-FLBYTES                  TO NYPON-ART-FLBYTES              
320900     MOVE SPAR-TEORSAK                  TO NYPON-ART-TEORSAK              
321000                                                                          
321100     IF SPAR-KDERS = 09 OR 19 OR 29 OR 52                                 
321200        MOVE 'U'                        TO NYPON-ART-KDRESBED             
321300     ELSE                                                                 
321400        IF SPAR-KDERS > ZERO                                              
321500           MOVE 'E'                     TO NYPON-ART-KDRESBED             
321600        ELSE                                                              
321700           IF SPAR-KDBPSR = 8  AND  SPAR-FLLSRDEL = NEJ                   
321800              MOVE '-'                  TO NYPON-ART-KDRESBED             
321900           ELSE                                                           
322000              MOVE 'R'                  TO NYPON-ART-KDRESBED             
322100           END-IF                                                         
322200        END-IF                                                            
322300     END-IF                                                               
322400                                                                          
322500     IF (KDPRODSL-UTAN-EMB OR KDPRODSL-VCBV) AND                          
322600        SPAR-FLLSRDEL = JA                                                
322700*       A R T I K E L   S K A L L   T I L L  B A S L A G E R              
322800        MOVE 1                          TO NYPON-ART-DABASL               
322900     ELSE                                                                 
323000        MOVE ZERO                       TO NYPON-ART-DABASL               
323100     END-IF                                                               
323200                                                                          
323300     IF ( SPAR-KDBPSR = 8  AND  SPAR-FLLSRDEL = NEJ ) OR                  
323400        ( SPAR-KDERS         > 20   )                                     
323500        MOVE ZERO                       TO NYPON-ART-KDANSKQ              
323600     ELSE                                                                 
323700        IF SPAR-KDERS > 10                                                
323800           MOVE ZERO                    TO NYPON-ART-KDANSKQ              
323900        ELSE                                                              
324000*******    A R T I K E L   S K A L L   T I L L  A N S K K Ö               
324100           MOVE 1                          TO NYPON-ART-KDANSKQ           
324200           IF SPAR-FLERS = JA                                             
324300*******       T I L L K O M M A N D E  A R T I K E L                      
324400              MOVE W-IDARTNR         TO W-IDARTNR-MIN7                    
324500                                        W-IDARTNR-MAX7                    
324600              PERFORM IMS-GN-ERSB01                                       
324700              IF SEGMENT-FINNS                                            
324800                 MOVE ERSB01-ERS-IDARTNR                                  
324900                                     TO W-IDARTNR                         
325000                 PERFORM IMS-GHU-ARTC01                                   
325100                 IF SEGMENT-FINNS AND ART-KDERS-UTG = ZERO                
325200                    PERFORM IMS-GHNP-ARTC11                               
325300                    MOVE CLAG-IDANSK TO SPAR-IDANSK                       
325400                 END-IF                                                   
325500*******          ÅTERSTÄLL NYCKELN                                        
325600                 MOVE WS-IDARTNR     TO W-IDARTNR                         
325700              END-IF                                                      
325800           END-IF                                                         
325900        END-IF                                                            
326000                                                                          
326100        IF SPAR-IDANSK = ZERO                                             
326200           PERFORM S10-HAEMTA-ANSK-XXAT                                   
326300        END-IF                                                            
326400     END-IF                                                               
326500                                                                          
326600     PERFORM ENA-INITIERA-OVRIGA-NYPONFAELT                               
326700                                                                          
326800     PERFORM IMS-ISRT-NYPON                                               
326900     .                                                                    
327000     EJECT                                                                
327100 ENA-INITIERA-OVRIGA-NYPONFAELT SECTION.                                  
327200     SKIP3                                                                
327300     MOVE NEJ                        TO NYPON-ART-FLAENDR                 
327400                                        NYPON-ART-FLBERQ                  
327500                                        NYPON-ART-FLRITC                  
327600                                        NYPON-ART-FLRITP                  
327700                                        NYPON-ART-FLUNIKRD                
327800                                                                          
327900     MOVE SPACE                      TO NYPON-ART-BEART-SVE               
328000                                        NYPON-ART-FLUPB                   
328100                                        NYPON-ART-FLPLAKOP                
328200                                        NYPON-ART-FLBASL                  
328300                                        NYPON-ART-FLUPG                   
328400                                        NYPON-ART-FLRITB                  
328500                                        NYPON-ART-IDMATKTO                
328600                                        NYPON-ART-IDPROJOBJ               
328700                                        NYPON-ART-IDRITUTG                
328800                                        NYPON-ART-KDARTTYP                
328900                                        NYPON-ART-KDARTUTG                
329000                                        NYPON-ART-TEANSINK                
329100                                        NYPON-ART-TEARTNOT                
329200                                        NYPON-ART-TEARTNOT-BASL           
329300                                        NYPON-ART-TETEKNIK                
329400                                        NYPON-ART-KDKOPTYP                
329500                                        NYPON-ART-IDLEVNR                 
329600                                        NYPON-ART-IDLEVNR-FORB (1)        
329700                                        NYPON-ART-IDLEVNR-FORB (2)        
329800                                        NYPON-ART-IDLEVNR-FORB (3)        
329900                                        NYPON-ART-IDLEVNR-FORB (4)        
330000                                        NYPON-ART-IDLEVNR-FORB (5)        
330100                                                                          
330200     MOVE ZERO                       TO NYPON-ART-IDANSK-REG              
330300                                        NYPON-ART-KDSTAINK                
330400                                        NYPON-ART-IDAVD                   
330500                                        NYPON-ART-IDINK                   
330600                                        NYPON-ART-KVARTAR1                
330700                                        NYPON-ART-KVARTAR2                
330800                                        NYPON-ART-KVARTAR3                
330900                                        NYPON-ART-KVARTVAGN               
331000                                        NYPON-ART-KVBASL                  
331100                                        NYPON-ART-KVLEVBEG                
331200                                        NYPON-ART-KVUPB                   
331300                                        NYPON-ART-PRARTBES                
331400                                        NYPON-ART-TIANSKREG               
331500                                        NYPON-ART-TIINKOP                 
331600                                        NYPON-ART-TILEVBEG                
331700                                        NYPON-ART-TINEDBRY                
331800                                        NYPON-ART-TIPLAKOP                
331900                                        NYPON-ART-TIREGDAT                
332000                                        NYPON-ART-TIRITB                  
332100                                        NYPON-ART-TIRITC                  
332200                                        NYPON-ART-TIRITP                  
332300                                        NYPON-ART-TISERLEV  (1)           
332400                                        NYPON-ART-TISERLEV  (2)           
332500                                        NYPON-ART-TISERLEV  (3)           
332600                                        NYPON-ART-TISERLEV  (4)           
332700                                        NYPON-ART-TISERLEV  (5)           
332800                                        NYPON-ART-TISLUBER                
332900                                        NYPON-ART-TISTABER                
333000                                        NYPON-ART-TISTOMREG               
333100                                        NYPON-ART-TIUPB                   
333200                                        NYPON-ART-TIUPG                   
333300                                        NYPON-ART-TIUPPDAT                
333400                                        NYPON-ART-IDINKTEK                
333500     .                                                                    
333600     EJECT                                                                
333700 EO-KOLLA-UPPDAT-WDK7 SECTION.                                            
333800                                                                          
333900     PERFORM IMS-GHU-WDK701                                               
334000     IF SEGMENT-FINNS                                                     
334100       PERFORM IMS-GHNP-WDK712                                            
334200       PERFORM UNTIL SEGMENT-SAKNAS                                       
334300         IF SPAR-DASOP-AAAAMMDD > LART-DAPUBL                             
334400           MOVE ZERO TO LART-DAPUBL                                       
334500           PERFORM IMS-REPL-WDK712                                        
334600         END-IF                                                           
334700         PERFORM IMS-GHNP-WDK712                                          
334800       END-PERFORM                                                        
334900     END-IF                                                               
335000     .                                                                    
335100     EJECT                                                                
335200 S02-ROER-EJ-VISADE-FAELT SECTION.                                        
335300     SKIP3                                                                
335400     MOVE MFS-ROER-EJ-FAELT          TO                                   
335500                                        MOD-KDAGE.                        
335600                                                                          
335700     EJECT                                                                
335800                                                                          
335900 S03-RENSA-MOD-INMATNINGSFAELT SECTION.                                   
336000     SKIP3                                                                
336100     MOVE MFS-RENSA-FAELT             TO                                  
336200                                         MOD-IDPROENH-IN (1)              
336300                                         MOD-IDPROENH-IN (2)              
336400                                         MOD-IDPROENH-IN (3)              
336500                                         MOD-IDKAT-IN (1)                 
336600                                         MOD-IDKAT-IN (2)                 
336700                                         MOD-IDKAT-IN (3)                 
336800     .                                                                    
336900     EJECT                                                                
337000 S05-FORMATETS-ATTRIBUT SECTION.                                          
337100     SKIP3                                                                
337200     MOVE MFS-FORMATETS-ATTR         TO                                   
337300                                        MOD-IDBERED-IN-ATTR               
337400                                        MOD-KDPRODSL-IN-ATTR              
337500                                        MOD-KDSORT-IN-ATTR                
337600                                        MOD-IDPROENH-IN-ATTR (1)          
337700                                        MOD-IDPROENH-IN-ATTR (2)          
337800                                        MOD-IDPROENH-IN-ATTR (3)          
337900                                        MOD-KDUART-IN-ATTR                
338000                                        MOD-IDPROJ-IN-ATTR                
338100                                        MOD-FLPISK-IN-ATTR                
338200                                        MOD-KDAGE-IN-ATTR                 
338300                                        MOD-IDKAT-IN-ATTR (1)             
338400                                        MOD-IDKAT-IN-ATTR (2)             
338500                                        MOD-IDKAT-IN-ATTR (3)             
338600                                        MOD-IDPROJK-IN-ATTR               
338700                                        MOD-KDBPSR-IN-ATTR                
338800                                        MOD-TISOP-IN-ATTR                 
338900                                        MOD-IDFKNGRP-IN-ATTR              
339000                                        MOD-IDPROJUP-IN-ATTR              
339100                                        MOD-IDARTNR-MOTSV-IN-ATTR         
339200                                        MOD-IDRITN-IN-ATTR                
339300                                        MOD-KVARTVAGN-IN-ATTR             
339400                                        MOD-IDAO-IN-ATTR  (1)             
339500                                        MOD-IDAO-IN-ATTR  (2)             
339600                                        MOD-IDAO-IN-ATTR  (3)             
339700                                        MOD-IDAO-IN-ATTR  (4)             
339800                                        MOD-IDAO-IN-ATTR  (5).            
339900     EJECT                                                                
340000 S10-HAEMTA-ANSK-XXAT SECTION.                                            
340100     SKIP2                                                                
340200     MOVE NEJ TO SW-ANSK-FINNS                                            
340300     MOVE SPAR-KDPRODSL TO W-KDPRODSL2                                    
340400     PERFORM IMS-GU-WLXXAT01                                              
340500     IF SEGMENT-FINNS                                                     
340600        PERFORM IMS-GNP-WLXXAT11                                          
340700        PERFORM UNTIL SEGMENT-SAKNAS OR SW-ANSK-FINNS = JA                
340800           IF (NYPON-ART-IDFKNGRP = XXAT-1138-IDFKNGRP-FOM                
340900               OR > XXAT-1138-IDFKNGRP-FOM) AND                           
341000               (NYPON-ART-IDFKNGRP = XXAT-1138-IDFKNGRP-TOM               
341100               OR < XXAT-1138-IDFKNGRP-TOM)                               
341200              MOVE XXAT-1138-IDANSK TO NYPON-ART-IDANSK                   
341300              MOVE JA TO SW-ANSK-FINNS                                    
341400           ELSE                                                           
341500              PERFORM IMS-GNP-WLXXAT11                                    
341600           END-IF                                                         
341700        END-PERFORM                                                       
341800     END-IF.                                                              
341900     EJECT                                                                
342000 S99-WDATKONV SECTION.                                                    
342100     SKIP2                                                                
342200     CALL WDATKONV USING DAT-KDDATFORM                                    
342300                         DAT-I-TIDATUM                                    
342400                         DAT-O-TIDATUM                                    
342500                         DAT-KDSVAR.                                      
342600     EJECT                                                                
342700* IMS SEKTIONER                                                           
342800     SKIP3                                                                
342900 IMS-GET-MSG SECTION.                                                     
343000     SKIP2                                                                
343100     MOVE '  QC' TO GODK-STATUSKODER                                      
343200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
343300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
343400     PERFORM IMS-STATUS-KONTROLL.                                         
343500     SKIP3                                                                
343600 IMS-INSERT-MSG SECTION.                                                  
343700     SKIP2                                                                
343800                                                                          
343900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
344000     MOVE SPACE TO GODK-STATUSKODER                                       
344100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
344200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
344300     PERFORM IMS-STATUS-KONTROLL.                                         
344400     EJECT                                                                
344500 IMS-GU-LEVA01 SECTION.                                                   
344600     SKIP2                                                                
344700     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
344800             DELIMITED BY SIZE INTO SSA1                                  
344900     MOVE '  GE' TO GODK-STATUSKODER                                      
345000     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA1 SSA1                     
345100     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
345200     PERFORM IMS-STATUS-KONTROLL.                                         
345300     EJECT                                                                
345400 IMS-GHU-ARTC01 SECTION.                                                  
345500     SKIP2                                                                
345600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
345700             DELIMITED BY SIZE INTO SSA1                                  
345800     MOVE '  GE' TO GODK-STATUSKODER                                      
345900     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC  SSA1                    
346000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
346100     PERFORM IMS-STATUS-KONTROLL.                                         
346200     SKIP3                                                                
346300 IMS-GHNP-ARTC11 SECTION.                                                 
346400     SKIP2                                                                
346500     MOVE 'WLARTC11 ' TO SSA1                                             
346600     MOVE '  ' TO GODK-STATUSKODER                                        
346700     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-ARTC  SSA1                   
346800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
346900     PERFORM IMS-STATUS-KONTROLL.                                         
347000     EJECT                                                                
347100 IMS-GU-WDK601-PCB2 SECTION.                                              
347200     SKIP2                                                                
347300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
347400             DELIMITED BY SIZE INTO SSA1                                  
347500     MOVE '  GE' TO GODK-STATUSKODER                                      
347600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K6 SSA1                   
347700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
347800     PERFORM IMS-STATUS-KONTROLL.                                         
347900     SKIP3                                                                
348000 IMS-GNP-WDK611-PCB2 SECTION.                                             
348100     SKIP2                                                                
348200     MOVE 'WDK611   ' TO SSA1                                             
348300     MOVE '  ' TO GODK-STATUSKODER                                        
348400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-K6 SSA1                  
348500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
348600     PERFORM IMS-STATUS-KONTROLL.                                         
348700     EJECT                                                                
348800 IMS-GHNP-ARTC25 SECTION.                                                 
348900     SKIP2                                                                
349000     MOVE 'WLARTC11 ' TO SSA1                                             
349100     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
349200             DELIMITED BY SIZE INTO SSA2                                  
349300     MOVE '  GE' TO GODK-STATUSKODER                                      
349400     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-ARTC  SSA1 SSA2              
349500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
349600     PERFORM IMS-STATUS-KONTROLL.                                         
349700     SKIP3                                                                
349800 IMS-GN-ERSB01 SECTION.                                                   
349900     SKIP2                                                                
350000     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
350100                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
350200             DELIMITED BY SIZE INTO SSA1                                  
350300     MOVE '  GE' TO GODK-STATUSKODER                                      
350400     CALL CBLTDLI USING GN ERSB-PCB DLI-IO-AREA1 SSA1                     
350500     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
350600     PERFORM IMS-STATUS-KONTROLL.                                         
350700     SKIP2                                                                
350800 IMS-GU-ERSB01 SECTION.                                                   
350900     SKIP2                                                                
351000     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
351100                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
351200             DELIMITED BY SIZE INTO SSA1                                  
351300     MOVE '  GE' TO GODK-STATUSKODER                                      
351400     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA1 SSA1                     
351500     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
351600     PERFORM IMS-STATUS-KONTROLL.                                         
351700     SKIP2                                                                
351800 IMS-GHU-ARTG01-MED-GE SECTION.                                           
351900     SKIP2                                                                
352000     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
352100             DELIMITED BY SIZE INTO SSA1                                  
352200     MOVE '  GE' TO GODK-STATUSKODER                                      
352300     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA2 SSA1                    
352400     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
352500     PERFORM IMS-STATUS-KONTROLL.                                         
352600     SKIP2                                                                
352700 IMS-GHU-ARTG01-UTAN-GE SECTION.                                          
352800     SKIP2                                                                
352900     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
353000             DELIMITED BY SIZE INTO SSA1                                  
353100     MOVE '  ' TO GODK-STATUSKODER                                        
353200     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA2 SSA1                    
353300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
353400     PERFORM IMS-STATUS-KONTROLL.                                         
353500     EJECT                                                                
353600 IMS-GU-WLXXAQ01-UNIK SECTION.                                            
353700     SKIP2                                                                
353800     STRING 'WLXXAQ01(WDGXKEY  =' W-1131KEY-X ')'                         
353900             DELIMITED BY SIZE INTO SSA1                                  
354000     MOVE '  GE' TO GODK-STATUSKODER                                      
354100     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA1 SSA1                     
354200     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
354300     PERFORM IMS-STATUS-KONTROLL.                                         
354400     SKIP3                                                                
354500 IMS-GNP-WLXXAQ11-UNIK  SECTION.                                          
354600     SKIP2                                                                
354700     STRING 'WLXXAQ11(WDGXKEY  =' W-1132KEY-X ')'                         
354800             DELIMITED BY SIZE INTO SSA1                                  
354900     MOVE '  GE' TO GODK-STATUSKODER                                      
355000     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA1 SSA1                    
355100     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
355200     PERFORM IMS-STATUS-KONTROLL.                                         
355300     SKIP3                                                                
355400 IMS-GU-WLXXAQ11 SECTION.                                                 
355500     SKIP2                                                                
355600     STRING 'WLXXAQ01(WDGXKEY  =' W-1131KEY-X ')'                         
355700             DELIMITED BY SIZE INTO SSA1                                  
355800     STRING 'WLXXAQ11(WDGXKEY  =' W-1132KEY-X ')'                         
355900             DELIMITED BY SIZE INTO SSA2                                  
356000     MOVE '  GE' TO GODK-STATUSKODER                                      
356100     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-AREA1 SSA1 SSA2                
356200     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
356300     PERFORM IMS-STATUS-KONTROLL.                                         
356400     EJECT                                                                
356500 IMS-GNP-WLXXAQ11-PROJK SECTION.                                          
356600     SKIP2                                                                
356700     STRING 'WLXXAQ11(IDPROJK  =' W-IDPROJK ')'                           
356800             DELIMITED BY SIZE INTO SSA1                                  
356900     MOVE '  GE' TO GODK-STATUSKODER                                      
357000     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA1 SSA1                    
357100     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
357200     PERFORM IMS-STATUS-KONTROLL.                                         
357300     SKIP2                                                                
357400 IMS-GNP-WLXXAQ11-PROJ SECTION.                                           
357500     SKIP2                                                                
357600     STRING 'WLXXAQ11(IDPROJ   =' W-IDPROJ ')'                            
357700             DELIMITED BY SIZE INTO SSA1                                  
357800     MOVE '  GE' TO GODK-STATUSKODER                                      
357900     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-AREA1 SSA1                    
358000     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
358100     PERFORM IMS-STATUS-KONTROLL.                                         
358200     EJECT                                                                
358300 IMS-GU-WLXXAP01-UNIK SECTION.                                            
358400     SKIP2                                                                
358500     STRING 'WLXXAP01(WDGXKEY  =' W-1123KEY-X ')'                         
358600             DELIMITED BY SIZE INTO SSA1                                  
358700     MOVE '  GE' TO GODK-STATUSKODER                                      
358800     CALL CBLTDLI USING GU XXAP-PCB DLI-IO-AREA1 SSA1                     
358900     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
359000     PERFORM IMS-STATUS-KONTROLL.                                         
359100     SKIP3                                                                
359200 IMS-GNP-WLXXAP11 SECTION.                                                
359300     SKIP2                                                                
359400     MOVE 'WLXXAP11 ' TO  SSA1                                            
359500     MOVE '  GE' TO GODK-STATUSKODER                                      
359600     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA1 SSA1                    
359700     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
359800     PERFORM IMS-STATUS-KONTROLL.                                         
359900     EJECT                                                                
360000 IMS-GU-WLXXAT01 SECTION.                                                 
360100     SKIP2                                                                
360200     STRING 'WLXXAT01(WDGXKEY  =' W-1137KEY-X ')'                         
360300             DELIMITED BY SIZE INTO SSA1                                  
360400     MOVE '  GE' TO GODK-STATUSKODER                                      
360500     CALL CBLTDLI USING GU XXAT-PCB DLI-IO-AREA1 SSA1                     
360600     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
360700     PERFORM IMS-STATUS-KONTROLL.                                         
360800     SKIP2                                                                
360900 IMS-GNP-WLXXAT11 SECTION.                                                
361000     SKIP2                                                                
361100     MOVE 'WLXXAT11 '  TO SSA1                                            
361200     MOVE '  GE' TO GODK-STATUSKODER                                      
361300     CALL CBLTDLI USING GNP XXAT-PCB DLI-IO-AREA1 SSA1                    
361400     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
361500     PERFORM IMS-STATUS-KONTROLL.                                         
361600     EJECT                                                                
361700 IMS-ISRT-XXBJ-2204 SECTION.                                              
361800     SKIP2                                                                
361900     STRING 'WLXXBJ01(WDG3KEY  =' W-WDGXKEY-2203-X ')'                    
362000          DELIMITED BY SIZE INTO SSA1                                     
362100     MOVE 'WLXXBJ11 ' TO SSA2                                             
362200     MOVE '  ' TO GODK-STATUSKODER                                        
362300     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA-2204 SSA1 SSA2          
362400     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
362500     PERFORM IMS-STATUS-KONTROLL                                          
362600     .                                                                    
362700 IMS-REPL-ARTC SECTION.                                                   
362800     SKIP2                                                                
362900     MOVE '  '   TO GODK-STATUSKODER                                      
363000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC                         
363100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
363200     PERFORM IMS-STATUS-KONTROLL.                                         
363300     SKIP3                                                                
363400 IMS-REPL-NYPON SECTION.                                                  
363500     SKIP2                                                                
363600     MOVE '  '   TO GODK-STATUSKODER                                      
363700     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA2                        
363800     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
363900     PERFORM IMS-STATUS-KONTROLL.                                         
364000     EJECT                                                                
364100 IMS-DLET-NYPON SECTION.                                                  
364200     SKIP2                                                                
364300     MOVE '  '   TO GODK-STATUSKODER                                      
364400     CALL CBLTDLI USING DLET ARTG-PCB DLI-IO-AREA2                        
364500     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
364600     PERFORM IMS-STATUS-KONTROLL.                                         
364700     SKIP3                                                                
364800 IMS-ISRT-NYPON SECTION.                                                  
364900     SKIP2                                                                
365000     MOVE 'WLARTG01 '     TO SSA1                                         
365100     MOVE '  '   TO GODK-STATUSKODER                                      
365200     CALL CBLTDLI USING ISRT ARTG-PCB DLI-IO-AREA2 SSA1                   
365300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
365400     PERFORM IMS-STATUS-KONTROLL.                                         
365500     SKIP3                                                                
365600 IMS-ISRT-ZZAC SECTION.                                                   
365700     SKIP2                                                                
365800     MOVE 'WLZZAC01 '     TO SSA1                                         
365900     MOVE '  '   TO GODK-STATUSKODER                                      
366000     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-WDGZ01 SSA1                  
366100     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
366200     PERFORM IMS-STATUS-KONTROLL.                                         
366300     EJECT                                                                
366400 IMS-ISRT-XXBW SECTION.                                                   
366500     SKIP2                                                                
366600     STRING 'WLXXBW01(WDGXKEY  =' W-2227KEY-X ')'                         
366700          DELIMITED BY SIZE INTO SSA1                                     
366800     MOVE 'WLXXBW11 ' TO SSA2                                             
366900     MOVE 'II  ' TO GODK-STATUSKODER                                      
367000     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-WDGX2228 SSA1 SSA2           
367100     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
367200     PERFORM IMS-STATUS-KONTROLL                                          
367300     .                                                                    
367400     SKIP3                                                                
367500 IMS-ISRT-WEEK SECTION.                                                   
367600     SKIP2                                                                
367700     STRING 'WL111701(WDGXKEY  =' W-111701-KEY ')'                        
367800          DELIMITED BY SIZE INTO SSA1                                     
367900     MOVE 'WL111711 ' TO SSA2                                             
368000     MOVE 'II  ' TO GODK-STATUSKODER                                      
368100     CALL CBLTDLI USING ISRT 1117-PCB DLI-IO-AREA3 SSA1 SSA2              
368200     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
368300     PERFORM IMS-STATUS-KONTROLL                                          
368400     .                                                                    
368500     SKIP3                                                                
368600 IMS-GHU-WEEK SECTION.                                                    
368700     SKIP2                                                                
368800     STRING 'WL111701(WDGXKEY  =' W-111701-KEY ')'                        
368900             DELIMITED BY SIZE INTO SSA1                                  
369000     STRING 'WL111711(IDARTNR  =' W-111711-KEY ')'                        
369100             DELIMITED BY SIZE INTO SSA2                                  
369200     MOVE '    ' TO GODK-STATUSKODER                                      
369300     CALL CBLTDLI USING GHU 1117-PCB DLI-IO-AREA3 SSA1 SSA2               
369400     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
369500     PERFORM IMS-STATUS-KONTROLL.                                         
369600     EJECT                                                                
369700 IMS-DLET-WEEK SECTION.                                                   
369800     SKIP2                                                                
369900     MOVE '  '   TO GODK-STATUSKODER                                      
370000     CALL CBLTDLI USING DLET 1117-PCB DLI-IO-AREA3                        
370100     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
370200     PERFORM IMS-STATUS-KONTROLL.                                         
370300     SKIP3                                                                
370400 IMS-REPL-WEEK SECTION.                                                   
370500     SKIP2                                                                
370600     MOVE '  '   TO GODK-STATUSKODER                                      
370700     CALL CBLTDLI USING REPL 1117-PCB DLI-IO-AREA3                        
370800     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
370900     PERFORM IMS-STATUS-KONTROLL.                                         
371000     EJECT                                                                
371100 IMS-GN-WDK711     SECTION.                                               
371200     SKIP2                                                                
371300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
371400             DELIMITED BY SIZE INTO SSA1                                  
371500     MOVE   'WDK711   '  TO SSA2                                          
371600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
371700     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
371800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
371900     PERFORM IMS-STATUS-KONTROLL.                                         
372000     SKIP2                                                                
372100 IMS-GHU-WDK701    SECTION.                                               
372200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
372300             DELIMITED BY SIZE INTO SSA1                                  
372400     MOVE '  GE' TO GODK-STATUSKODER                                      
372500     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK701   SSA1                 
372600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
372700     PERFORM IMS-STATUS-KONTROLL.                                         
372800     SKIP2                                                                
372900 IMS-GHNP-WDK712    SECTION.                                              
373000     MOVE 'WDK712  ' TO SSA1                                              
373100     MOVE '  GE' TO GODK-STATUSKODER                                      
373200     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK712  SSA1                 
373300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
373400     PERFORM IMS-STATUS-KONTROLL.                                         
373500     SKIP2                                                                
373600 IMS-REPL-WDK712 SECTION.                                                 
373700     SKIP2                                                                
373800     MOVE '  '   TO GODK-STATUSKODER                                      
373900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
374000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
374100     PERFORM IMS-STATUS-KONTROLL.                                         
374200     EJECT                                                                
374300 IMS-GU-D901 SECTION.                                                     
374400                                                                          
374500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
374600          DELIMITED BY SIZE INTO SSA1                                     
374700     MOVE '  GE' TO GODK-STATUSKODER                                      
374800     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
374900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
375000     PERFORM IMS-STATUS-KONTROLL                                          
375100     .                                                                    
375200     EJECT                                                                
375300 IMS-GNP-D902 SECTION.                                                    
375400                                                                          
375500     STRING 'WDD902    '                                                  
375600          DELIMITED BY SIZE INTO SSA1                                     
375700     MOVE '  GE' TO GODK-STATUSKODER                                      
375800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
375900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
376000     PERFORM IMS-STATUS-KONTROLL                                          
376100     .                                                                    
376200     EJECT                                                                
376300 IMS-GNP-D905 SECTION.                                                    
376400                                                                          
376500     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
376600          DELIMITED BY SIZE INTO SSA1                                     
376700     MOVE '  GE' TO GODK-STATUSKODER                                      
376800     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
376900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
377000     PERFORM IMS-STATUS-KONTROLL                                          
377100     .                                                                    
377200     EJECT                                                                
377300 IMS-GU-R220 SECTION.                                                     
377400                                                                          
377500     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
377600          DELIMITED BY SIZE INTO SSA1                                     
377700     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
377800          DELIMITED BY SIZE INTO SSA2                                     
377900     MOVE '  GE' TO GODK-STATUSKODER                                      
378000     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-2232 SSA1 SSA2            
378100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
378200     PERFORM IMS-STATUS-KONTROLL                                          
378300     .                                                                    
378400     SKIP2                                                                
378500 IMS-GHU-R501 SECTION.                                                    
378600     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
378700          DELIMITED BY SIZE INTO SSA1                                     
378800     MOVE '  GE'           TO GODK-STATUSKODER                            
378900     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-AREA-2223 SSA1                
379000     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
379100     PERFORM IMS-STATUS-KONTROLL                                          
379200     .                                                                    
379300                                                                          
379400 IMS-ISRT-R501 SECTION.                                                   
379500     MOVE 'WDR501   '      TO SSA1                                        
379600     MOVE '  II'           TO GODK-STATUSKODER                            
379700     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA-2223 SSA1               
379800     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
379900     PERFORM IMS-STATUS-KONTROLL                                          
380000     .                                                                    
380100                                                                          
380200 IMS-ISRT-R550 SECTION.                                                   
380300     MOVE 'WDR550   '      TO SSA1                                        
380400     MOVE '  II'           TO GODK-STATUSKODER                            
380500     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA-2224 SSA1               
380600     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
380700     PERFORM IMS-STATUS-KONTROLL                                          
380800     .                                                                    
380900     EJECT                                                                
381000 IMS-ISRT-WDR801   SECTION.                                               
381100                                                                          
381200     MOVE 'WDR801   ' TO SSA1                                             
381300     MOVE '  II' TO GODK-STATUSKODER                                      
381400     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801   SSA1                
381500     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
381600     PERFORM IMS-STATUS-KONTROLL                                          
381700     .                                                                    
381800     EJECT                                                                
381900 IMS-GU-WDB601 SECTION.                                                   
382000                                                                          
382100     STRING 'WDB601  (IDDC     =' W-IDDC-A17-X ')'                        
382200                    DELIMITED BY SIZE INTO SSA1                           
382300     MOVE '  GE' TO GODK-STATUSKODER                                      
382400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601   SSA1                  
382500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
382600     PERFORM IMS-STATUS-KONTROLL                                          
382700     .                                                                    
382800     EJECT                                                                
382900 IMS-ISRT-WDGX2264-NEW SECTION.                                           
383000                                                                          
383100     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
383200          DELIMITED BY SIZE INTO SSA1                                     
383300     MOVE 'WDGX2264 '      TO SSA2                                        
383400     MOVE '  II'           TO GODK-STATUSKODER                            
383500     CALL CBLTDLI USING ISRT WDR2-N-PCB DLI-IO-WDGX2264-N SSA1            
383600                                                          SSA2            
383700     MOVE WDR2-N-STATUS-CODE TO STATUS-WS                                 
383800     PERFORM IMS-STATUS-KONTROLL                                          
383900     .                                                                    
384000                                                                          
384100 IMS-ISRT-WDGX2266-NEW SECTION.                                           
384200                                                                          
384300     MOVE 'WDGX2266 '      TO SSA1                                        
384400     MOVE '  '             TO GODK-STATUSKODER                            
384500     CALL CBLTDLI USING ISRT WDR2-N-PCB DLI-IO-WDGX2266-N SSA1            
384600     MOVE WDR2-N-STATUS-CODE TO STATUS-WS                                 
384700     PERFORM IMS-STATUS-KONTROLL                                          
384800     .                                                                    
384900     EJECT                                                                
385000 IMS-GHU-WDGX2264-OLD SECTION.                                            
385100                                                                          
385200     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
385300          DELIMITED BY SIZE INTO SSA1                                     
385400     STRING 'WDGX2264(TISOP    =' W-TISOP-O-X ')'                         
385500          DELIMITED BY SIZE INTO SSA2                                     
385600     MOVE '  GE'              TO GODK-STATUSKODER                         
385700     CALL CBLTDLI USING GHU WDR2-O-PCB DLI-IO-WDGX2264-O SSA1 SSA2        
385800     MOVE WDR2-O-STATUS-CODE    TO STATUS-WS                              
385900     PERFORM IMS-STATUS-KONTROLL                                          
386000     .                                                                    
386100     EJECT                                                                
386200 IMS-GHNP-WDGX2266-OLD SECTION.                                           
386300                                                                          
386400     STRING 'WDGX2266(KY2266  >=' W-W2266KY-MIN-O-X                       
386500                    '&KY2266  <=' W-W2266KY-MAX-O-X ')'                   
386600          DELIMITED BY SIZE INTO SSA1                                     
386700     MOVE '  GE'              TO GODK-STATUSKODER                         
386800     CALL CBLTDLI USING GHNP WDR2-O-PCB DLI-IO-WDGX2266-O SSA1            
386900     MOVE WDR2-O-STATUS-CODE    TO STATUS-WS                              
387000     PERFORM IMS-STATUS-KONTROLL                                          
387100     .                                                                    
387200 IMS-GNP-WDGX2266     SECTION.                                            
387300                                                                          
387400     MOVE 'WDGX2266 '           TO  SSA1                                  
387500     MOVE '  GE'              TO GODK-STATUSKODER                         
387600     CALL CBLTDLI USING GNP WDR2-O-PCB DLI-IO-WDGX2266-O SSA1             
387700     MOVE WDR2-O-STATUS-CODE    TO STATUS-WS                              
387800     PERFORM IMS-STATUS-KONTROLL                                          
387900     .                                                                    
388000     EJECT                                                                
388100 IMS-DLET-WDGX2266-OLD SECTION.                                           
388200                                                                          
388300     MOVE '  '       TO GODK-STATUSKODER                                  
388400     CALL CBLTDLI USING DLET WDR2-O-PCB DLI-IO-WDGX2266-O                 
388500     MOVE WDR2-O-STATUS-CODE TO STATUS-WS                                 
388600     PERFORM IMS-STATUS-KONTROLL                                          
388700     .                                                                    
388800     EJECT                                                                
388900 IMS-DLET-WDGX2264-OLD SECTION.                                           
389000                                                                          
389100     MOVE '  '       TO GODK-STATUSKODER                                  
389200     CALL CBLTDLI USING DLET WDR2-O-PCB DLI-IO-WDGX2264-O                 
389300     MOVE WDR2-O-STATUS-CODE TO STATUS-WS                                 
389400     PERFORM IMS-STATUS-KONTROLL                                          
389500     .                                                                    
389600     EJECT                                                                
389700 IMS-STATUS-KONTROLL SECTION.                                             
389800     SET STATUS-IX TO 1                                                   
389900     SEARCH GODK-STATUS AT END CALL FELLOG                                
390000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
390100     END-SEARCH.                                                          
390200     EJECT                                                                
390300                                                                          
390400                                                                          
390500* DB2 SEKTIONER                                                           
390600     SKIP3                                                                
390700                                                                          
390800 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
390900     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
391000                                                                          
391100     MOVE 000100  TO GOOD-SQLCODECODES                                    
391200                                                                          
391300     EXEC SQL                                                             
391400         DECLARE TP1ARTK-CRS CURSOR FOR                                   
391500           SELECT  A.IDKAMP                                               
391600                  ,A.IDARTNR                                              
391700                  ,B.TISTADAT_KAMP                                        
391800                  ,B.TISTODAT_KAMP                                        
391900                  ,B.KDKAMP                                               
392000                                                                          
392100           FROM    TP1ARTK A                                              
392200                  ,TP1KAMP B                                              
392300                                                                          
392400           WHERE   A.IDARTNR = :W-IDARTNR                                 
392500               AND A.IDKAMP  =  B.IDKAMP                                  
392600                                                                          
392700           ORDER BY A.IDARTNR                                             
392800     END-EXEC                                                             
392900                                                                          
393000     MOVE 000100  TO GOOD-SQLCODECODES                                    
393100     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
393200     .                                                                    
393300     SKIP3                                                                
393400 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
393500     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
393600     SKIP2                                                                
393700     MOVE 000100  TO GOOD-SQLCODECODES                                    
393800     EXEC SQL                                                             
393900         FETCH TP1ARTK-CRS INTO                                           
394000                    :TP1KAMP-IDKAMP                                       
394100                   ,:TP1ARTK-IDARTNR                                      
394200                   ,:TP1KAMP-TISTADAT-KAMP                                
394300                   ,:TP1KAMP-TISTODAT-KAMP                                
394400                   ,:TP1KAMP-KDKAMP                                       
394500     END-EXEC                                                             
394600                                                                          
394700     MOVE SQLCODE TO SQLCODE-WS                                           
394800     PERFORM DB2-STATUS-CHECK                                             
394900     .                                                                    
395000     SKIP3                                                                
395100 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
395200     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
395300                                                                          
395400     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
395500     .                                                                    
395600     EJECT                                                                
395700 DB2-STATUS-CHECK  SECTION.                                               
395800                                                                          
395900     SET SQLCODE-IX TO 1                                                  
396000     SEARCH GOOD-SQLCODE                                                  
396100       AT END                                                             
396200*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
396300*         DELIMITED BY SIZE INTO ERROR-TEXT                               
396400          CALL FELLOG                                                     
396500       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
396600     END-SEARCH                                                           
396700     .                                                                    
396800     EJECT                                                                
396900                                                                          
397000*    -COPY WY2000P1                                                       
397100     EJECT                                                                
397200*    -COPY WY2000P3                                                       
