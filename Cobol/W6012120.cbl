000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W6012120.                                                
000500*AUTHOR.         ANNELIE ENGLUND                                          
000600*DATE-WRITTEN.   92/10/14.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W6012100                       
001200*        SOM SKRIVER UT LISTA FRÅN LOSSNINGSBILDEN                        
001300*                                                                         
001400*        PROGRAMMET LÄSER      W6LASA (W6G2)                              
001500*        CPY-TEXT              W6LASA01  (W6GX01)                         
001600*                                    11  (W6GX6108)                       
001700*                                    21  (W6GX6110)                       
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100                                                                          
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400                                                                          
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700                                                                          
003800 WORKING-STORAGE SECTION.                                                 
003900     SKIP2                                                                
003901                                                                          
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'W6012120'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300     SKIP2                                                                
004400                                                                          
004500 77  TRAEFF-SW                   PIC X       VALUE 'J'.                   
004600     88 TRAEFF-JA                            VALUE 'J'.                   
004700     88 TRAEFF-NEJ                           VALUE 'N'.                   
004800                                                                          
004810 77  NY-SW                       PIC X       VALUE 'N'.                   
004820     88 NY-ART                               VALUE 'J'.                   
004830                                                                          
004900 77  PRINTER-OK                  PIC X       VALUE 'J'.                   
005000 77  UTSKRIFT-OK                 PIC X       VALUE 'N'.                   
005100                                                                          
005110 77  WS-KVKOLLI                  PIC 9(4)    VALUE ZERO.                  
005111 77  WS-KVAVIS                   PIC 9(6)    VALUE ZERO.                  
005112 77  WS-KDLAGEMB                 PIC X(4)    VALUE ZERO.                  
005113 77  WS-BEFT                     PIC 9(2)    VALUE ZERO.                  
005114 77  WS-ADINLOMR-FB              PIC X(4)    VALUE 'FADR'.                
005115 77  WS-FLKVAKAR                 PIC X(1)    VALUE 'N'.                   
005116 77  WS-ADTRDEST-KIT             PIC X(3)    VALUE SPACE.                 
005117 77  WS-ADTRDEST                 PIC X(3)    VALUE SPACE.                 
005119 01  WS-KOLLIF.                                                           
005120    03  WS-RAD-KOLLIF            OCCURS 4 TIMES                           
005121                                 PIC 9(9)    VALUE ZERO.                  
005122 01  WS-KOLLIT.                                                           
005123    03  WS-RAD-KOLLIT            OCCURS 4 TIMES                           
005124                                 PIC 9(9)    VALUE ZERO.                  
005125 01  W-ADLAGOMR-HELP.                                                     
005126    03  W-ADLAGOMR               OCCURS 4 TIMES                           
005127                                 PIC X(2)    VALUE SPACE.                 
005128 77  WS-ADLAGOMR                 PIC 9(2)    VALUE ZERO.                  
005129 77  WS-FLSPLPART                PIC X(1)    VALUE 'N'.                   
005130 77  HELP-IX                     PIC S9(5)   VALUE ZERO COMP-3.           
005131 77  WS-IDRADNR-INL              PIC S9(5)   VALUE ZERO COMP-3.           
005140                                                                          
005200 01  WORK-SPAR-AREA.                                                      
005260                                                                          
005270*FÖREGÅENDE RAD (SKRIVEN RAD)                                             
005280     03  WS-RADEN.                                                        
005290         05  WS-IDLEVNR           PIC  X(5)   VALUE SPACE.                
005291         05  WS-IDFS              PIC X(8)    VALUE SPACE.                
005292         05  WS-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.          
005500                                                                          
005600     03  W-SIDNR                 PIC S9(3)    COMP-3.                     
005700     03  W-RADNR                 PIC S9(3)    COMP-3.                     
006300                                                                          
006310*DIVERSE                                                                  
006330     03 W-ADINLOMR-LPL           PIC X(4)    VALUE SPACE.                 
006400***********************************************                           
006500*  PRINTRADER                                 *                           
006600***********************************************                           
006700 01  BLANK-RAD.                                                           
006800     03 FILLER               PIC X(121) VALUE SPACE.                      
006900                                                                          
007000 01  RAD4.                                                                
007100     03 FILLER          PIC X(22) VALUE ' VCCS                 '.         
007300     03 FILLER          PIC X(18) VALUE SPACE.                            
007400     03 FILLER          PIC X(10) VALUE 'W60121-002'.                     
007500     03 FILLER          PIC X(3) VALUE SPACE.                             
007700     03 FILLER          PIC X(44) VALUE                                   
007710                'ARBETSLISTA VID LOSSNING - OLOSSADE PARTIER'.            
007800     03 FILLER          PIC X(3) VALUE SPACE.                             
008100     03 RAD4-DATUM      PIC X(6).                                         
008200     03 FILLER          PIC X(5)  VALUE ' SID '.                          
008300     03 RAD4-SIDNR      PIC ZZ9.                                          
008500                                                                          
008510 01  RAD4-ENG.                                                            
008520     03 FILLER          PIC X(22) VALUE ' VCCS                 '.         
008530     03 FILLER          PIC X(18) VALUE SPACE.                            
008540     03 FILLER          PIC X(10) VALUE 'W60121-002'.                     
008550     03 FILLER          PIC X(2) VALUE SPACE.                             
008560     03 FILLER          PIC X(45) VALUE                                   
008570                'WORKING LIST UNLOADING - NOT UNLOADED BATCHES'.          
008580     03 FILLER          PIC X(2) VALUE SPACE.                             
008590     03 RAD4-DATUM-ENG  PIC X(6).                                         
008591     03 FILLER          PIC X(6)  VALUE ' PAGE '.                         
008592     03 RAD4-SIDNR-ENG  PIC ZZ9.                                          
008593                                                                          
008600 01  RAD7.                                                                
008700     03 FILLER               PIC X(13)  VALUE ' LASTBÄRARE  '.            
008800     03 RAD7-IDLBBET         PIC X(12).                                   
008900     03 FILLER               PIC X(09)  VALUE '    LPL  '.                
009000     03 RAD7-ADINLOMR-LPL    PIC X(4).                                    
009100     03 FILLER               PIC X(83) VALUE SPACE.                       
009200                                                                          
009210 01  RAD7-ENG.                                                            
009220     03 FILLER               PIC X(13)  VALUE ' CARRIER     '.            
009230     03 RAD7-IDLBBET-ENG     PIC X(12).                                   
009240     03 FILLER               PIC X(09)  VALUE '    UNL  '.                
009250     03 RAD7-ADINLOMR-LPL-ENG PIC X(4).                                   
009260     03 FILLER               PIC X(83) VALUE SPACE.                       
009270                                                                          
009300 01  RAD10.                                                               
009400     03 FILLER               PIC X(1)   VALUE SPACE.                      
009500     03 FILLER               PIC X(5)   VALUE 'LEVNR'.                    
009600     03 FILLER               PIC X(1)   VALUE SPACE.                      
009700     03 FILLER               PIC X(4)   VALUE 'FSNR'.                     
009800     03 FILLER               PIC X(5)   VALUE SPACE.                      
009900     03 FILLER               PIC X(5)   VALUE 'ARTNR'.                    
010000     03 FILLER               PIC X(4)   VALUE SPACE.                      
010100     03 FILLER               PIC X(5)   VALUE 'ANTAL'.                    
010200     03 FILLER               PIC X(2)   VALUE SPACE.                      
010210     03 FILLER               PIC X(5)   VALUE 'KOLLI'.                    
010220     03 FILLER               PIC X(1)   VALUE SPACE.                      
010230     03 FILLER               PIC X(2)   VALUE 'FB'.                       
010240     03 FILLER               PIC X(3)   VALUE SPACE.                      
010241     03 FILLER               PIC X(3)   VALUE 'EMB'.                      
010242     03 FILLER               PIC X(3)   VALUE SPACE.                      
010243     03 FILLER               PIC X(2)   VALUE 'FT'.                       
010244     03 FILLER               PIC X(3)   VALUE SPACE.                      
010250     03 FILLER               PIC X(3)   VALUE 'ADR'.                      
010260     03 FILLER               PIC X(2)   VALUE SPACE.                      
010270     03 FILLER               PIC X(5)   VALUE 'ANTAL'.                    
010280     03 FILLER               PIC X(2)   VALUE SPACE.                      
010290     03 FILLER               PIC X(4)   VALUE 'PRIO'.                     
010291     03 FILLER               PIC X(3)   VALUE SPACE.                      
010292     03 FILLER               PIC X(10)  VALUE 'TILL SATS '.               
010293     03 FILLER               PIC X(1)   VALUE SPACE.                      
010294     03 FILLER               PIC X(4)   VALUE 'INFO'.                     
010295     03 FILLER               PIC X(6)   VALUE SPACE.                      
010296     03 FILLER               PIC X(2)   VALUE 'FG'.                       
010297     03 FILLER               PIC X(1)   VALUE SPACE.                      
010298     03 FILLER               PIC X(2)   VALUE 'RO'.                       
010299     03 FILLER               PIC X(1)   VALUE SPACE.                      
010300     03 FILLER               PIC X(9)   VALUE 'KOLLINR F'.                
010301     03 FILLER               PIC X(1)   VALUE SPACE.                      
010302     03 FILLER               PIC X(9)   VALUE 'KOLLINR T'.                
010303                                                                          
010310 01  RAD10-ENG.                                                           
010320     03 FILLER               PIC X(1)   VALUE SPACE.                      
010330     03 FILLER               PIC X(5)   VALUE 'SUPPL'.                    
010340     03 FILLER               PIC X(1)   VALUE SPACE.                      
010350     03 FILLER               PIC X(4)   VALUE 'ADNO'.                     
010360     03 FILLER               PIC X(4)   VALUE SPACE.                      
010370     03 FILLER               PIC X(6)   VALUE 'PARTNO'.                   
010380     03 FILLER               PIC X(4)   VALUE SPACE.                      
010390     03 FILLER               PIC X(5)   VALUE 'QTY  '.                    
010391     03 FILLER               PIC X(2)   VALUE SPACE.                      
010392     03 FILLER               PIC X(5)   VALUE 'CASE '.                    
010393     03 FILLER               PIC X(1)   VALUE SPACE.                      
010394     03 FILLER               PIC X(2)   VALUE 'PG'.                       
010395     03 FILLER               PIC X(3)   VALUE SPACE.                      
010396     03 FILLER               PIC X(6)   VALUE 'P-TYPE'.                   
010398     03 FILLER               PIC X(1)   VALUE SPACE.                      
010399     03 FILLER               PIC X(2)   VALUE 'PT'.                       
010400     03 FILLER               PIC X(2)   VALUE SPACE.                      
010401     03 FILLER               PIC X(4)   VALUE 'ADDR'.                     
010402     03 FILLER               PIC X(2)   VALUE SPACE.                      
010403     03 FILLER               PIC X(5)   VALUE 'QTY  '.                    
010404     03 FILLER               PIC X(2)   VALUE SPACE.                      
010405     03 FILLER               PIC X(4)   VALUE 'PRIO'.                     
010406     03 FILLER               PIC X(3)   VALUE SPACE.                      
010407     03 FILLER               PIC X(10)  VALUE 'TO KIT    '.               
010408     03 FILLER               PIC X(1)   VALUE SPACE.                      
010409     03 FILLER               PIC X(4)   VALUE 'INFO'.                     
010411     03 FILLER               PIC X(31)  VALUE SPACE.                      
010414                                                                          
010420 01  DETALJ-RAD.                                                          
010500     03 FILLER               PIC X(1)   VALUE SPACE.                      
010600     03 RAD-IDLEVNR          PIC X(5).                                    
010700     03 FILLER               PIC X(1)   VALUE SPACE.                      
010800     03 RAD-IDFS             PIC X(8).                                    
010900     03 FILLER               PIC X(1)   VALUE SPACE.                      
011000     03 RAD-IDARTNR          PIC Z(8).                                    
011100     03 FILLER               PIC X(1)   VALUE SPACE.                      
011200     03 RAD-TOT-ANT          PIC Z(6).                                    
011300     03 FILLER               PIC X(1)   VALUE SPACE.                      
011400     03 RAD-KVKOLLI          PIC Z(4).                                    
011500     03 FILLER               PIC X(2)   VALUE SPACE.                      
011501     03 RAD-ADINLOMR-FB      PIC X(4).                                    
011510     03 FILLER               PIC X(1)   VALUE SPACE.                      
011600     03 RAD-KDLAGEMB         PIC X(4).                                    
011610     03 FILLER               PIC X(2)   VALUE SPACE.                      
011620     03 RAD-BEFT             PIC Z(2).                                    
011700     03 FILLER               PIC X(3)   VALUE SPACE.                      
011800     03 RAD-ADINLOMR         PIC X(4).                                    
011900     03 FILLER               PIC X(1)   VALUE SPACE.                      
012000     03 RAD-KVAVIS           PIC Z(6).                                    
012100     03 FILLER               PIC X(1)   VALUE SPACE.                      
012200     03 RAD-KVAVIS-PRIO      PIC Z(6).                                    
012300     03 FILLER               PIC X(1)   VALUE SPACE.                      
012310     03 RAD-KVAVIS-KIT       PIC Z(6).                                    
012320     03 FILLER               PIC X(1)   VALUE SPACE.                      
012321     03 RAD-ADTRDEST-KIT     PIC X(3).                                    
012322     03 FILLER               PIC X(1)   VALUE SPACE.                      
012330     03 RAD-INFO             PIC X(10).                                   
012340     03 FILLER               PIC X(1)   VALUE SPACE.                      
012350     03 RAD-FLFARLIG         PIC X(1).                                    
012351     03 FILLER               PIC X(1)   VALUE SPACE.                      
012352     03 RAD-ROS              PIC X(3).                                    
012360     03 FILLER               PIC X(1)   VALUE SPACE.                      
012370     03 RAD-KOLLIF           PIC Z(8).                                    
012380     03 FILLER               PIC X(1)   VALUE SPACE.                      
012390     03 RAD-KOLLIT           PIC Z(8).                                    
012400                                                                          
012410 01  SISTA-RAD.                                                           
012420     03 FILLER               PIC X(1)   VALUE SPACE.                      
012421     03 FILLER               PIC X(20)  VALUE                             
012422                             'FLER PARTIER FINNS'.                        
012430                                                                          
012440 01  SISTA-RAD-ENG.                                                       
012450     03 FILLER               PIC X(1)   VALUE SPACE.                      
012460     03 FILLER               PIC X(20)  VALUE                             
012470                             'MORE BATCHES EXIST'.                        
012480                                                                          
012500 01  FELTEXT.                                                             
012600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012800     EJECT                                                                
012900                                                                          
013000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013100 01  FILLER REDEFINES DAGENS-DATUM.                                       
013200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013500     EJECT                                                                
013600                                                                          
013700 01  DYNAMISKA-SUBPROGRAM.                                                
013800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014100     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
014110     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
014200     EJECT                                                                
014210*      --- VALID IDDC CODES                                               
014220*                                                                         
014230*01    -COPY WWDC99                                                       
014240       EJECT                                                              
014300                                                                          
014400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014500*01 -COPY WMEDAREA                                                        
014600     SKIP3                                                                
014700*    ---AREA FÖR SUBPGM W006PRS1                                          
014800 01 FILLER                      PIC X(16)   VALUE 'W006PRS1'.             
014900                                                                          
015000*01 -COPY W006PRAR                                                        
015100                                                                          
015110*    ---AREA FÖR SUBPGM W611STYR                                          
015120 01 FILLER                      PIC X(16)   VALUE 'W611STYR'.             
015130                                                                          
015140*01 -COPY W611STYR                                                        
015150                                                                          
015200 01 WS-PRINTER-PARM.                                                      
015300     03 WS-IDPRT.                                                         
015310       05 FILLER                 PIC X(8).                                
015400     03 WS-RAD.                                                           
015500       05 WS-FILLER              PIC X(1).                                
015600       05 WS-LISTRAD             PIC X(132).                              
015700     03 WS-DUMMY                 PIC X(1).                                
015710     03 WS-RAPP-PRINTER          PIC X(8).                                
015800                                                                          
015900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016000     SKIP3                                                                
016100 01  NYCKLAR-TILL-DLI.                                                    
016200*----FYSISK NYCKEL TILL LASA                                              
016300     03  W-W6GX01KY-X.                                                    
016400         05  W-IDHTYP            PIC X(4)    VALUE SPACE.                 
016410         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016500         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016600                                                                          
016700     03  W-W6GX11KY-X.                                                    
016800         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
016900         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
017000                                                                          
017100     03  W-W6GX21KY-X.                                                    
017200         05  WL-IDLEVNR           PIC X(5)    VALUE SPACE.                
017300         05  WL-IDFS              PIC X(8)    VALUE SPACE.                
017400         05  WL-IDARTNR           PIC S9(9)   COMP-3.                     
017500         05  WL-KDSORT1           PIC S9      COMP-3.                     
018000                                                                          
018001*----FYSISK NYCKEL TILL INLA                                              
018002     03  W-W6D101KY-X.                                                    
018003         05  W-IDDC-D1           PIC X(2)    VALUE SPACE.                 
018004         05  W-IDLEVNR-D1        PIC X(5)    VALUE SPACE.                 
018005         05  W-IDFS-D1           PIC X(8)    VALUE SPACE.                 
018006         05  W-TIAVIDAT-D1       PIC S9(7)   COMP-3.                      
018007                                                                          
018008     03  W-IDRADNR-INL-X.                                                 
018009         05  W-IDRADNR-INL       PIC S9(5)   COMP-3.                      
018010                                                                          
018020     03  W-IDARTNR-X.                                                     
018030         05  W-IDARTNR-D1        PIC S9(9)   COMP-3.                      
018031                                                                          
018032     03  W-KDSEGKEY-X.                                                    
018033         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
018040                                                                          
018100     EJECT                                                                
018200*    --- STATUS-KOD FRÅN IMS                                              
018300 01  STATUS-WS                   PIC XX.                                  
018400     88  SEGMENT-FINNS                       VALUE '  '.                  
018500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018800     88  IMS-EJ-OK                           VALUE 'XD'.                  
018900     EJECT                                                                
019000                                                                          
019100     SKIP2                                                                
019200 01  GODK-STATUSKODER.                                                    
019300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019400     EJECT                                                                
019500                                                                          
019600     SKIP3                                                                
019700 01  SSA1                        PIC X(90).                               
019800 01  SSA2                        PIC X(64).                               
019900 01  SSA3                        PIC X(64).                               
020000     EJECT                                                                
020100                                                                          
020200*    --- IMS FUNKTIONSKODER                                               
020300*01  -COPY W0003                                                          
020400     EJECT                                                                
020500                                                                          
020600*    ---  DLI INPUT-OUTPUT AREA                                           
020700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
020800     SKIP3                                                                
020900 01  DLI-IO-AREA-1.                                                       
021000     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
021100                                                                          
021200     03  W6LASA01 REDEFINES IO-AREA-1.                                    
021300*        05  -COPY W6GX01                                                 
021400     EJECT                                                                
021500                                                                          
021600     03  W6LASA11 REDEFINES IO-AREA-1.                                    
021700*        05  -COPY W6GX6108                                               
021800     EJECT                                                                
021900                                                                          
022000     03  W6LASA21 REDEFINES IO-AREA-1.                                    
022100*        05  -COPY W6GX6110                                               
022400     EJECT                                                                
022500                                                                          
022501 01  DLI-IO-AREA-2.                                                       
022502     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
022503                                                                          
022504     03  W6INLA11 REDEFINES IO-AREA-2.                                    
022505*        05  -COPY W6D111   -PRE INLA-                                    
022506     EJECT                                                                
022507                                                                          
022508     03  W6INLA21 REDEFINES IO-AREA-2.                                    
022509*        05  -COPY W6D121   -PRE INLA-                                    
022510     EJECT                                                                
022511                                                                          
022541 01  FILLER        PIC X(16) VALUE 'DLI-IO-WDK6'.                         
022542 01  DLI-IO-WDK6.                                                         
022560*    03  -COPY WDK611                                                     
022570     EJECT                                                                
022600                                                                          
022700 LINKAGE SECTION.                                                         
022800                                                                          
022810*01  -COPY W0009  -PRE ALT-                                               
022830     EJECT                                                                
022870                                                                          
022900*01  -COPY W6012120                                                       
023100     EJECT                                                                
023200                                                                          
023300*01  -COPY W0008  -PRE LASA-W6012120-                                     
023400     05  FILLER                  PIC X.                                   
023500     EJECT                                                                
023501                                                                          
023502*01  -COPY W0008  -PRE INLA-W6012120-                                     
023503     05  FILLER                  PIC X.                                   
023504     EJECT                                                                
023505                                                                          
023506*01  -COPY W0008  -PRE WDK6-W6012120-                                     
023507     05  FILLER                  PIC X.                                   
023508     EJECT                                                                
023510                                                                          
023520*01  -COPY W0008  -PRE STYR-HANA-                                         
023530     05  FILLER                  PIC X.                                   
023540     EJECT                                                                
023541*01  -COPY W0008  -PRE STYR-PLAA-                                         
023542     05  FILLER                  PIC X.                                   
023543     EJECT                                                                
023800                                                                          
023900 PROCEDURE DIVISION  USING ALT-PCB                                        
023920                           W601-W6012120                                  
024000                           LASA-W6012120-PCB                              
024010                           INLA-W6012120-PCB                              
024011                           WDK6-W6012120-PCB                              
024020                           STYR-HANA-PCB                                  
024030                           STYR-PLAA-PCB.                                 
024100                                                                          
024300     PERFORM A-INIT                                                       
024500     PERFORM B-BEARB                                                      
024600     PERFORM C-AVSLUTA                                                    
024700                                                                          
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200******************************************************************        
025300 A-INIT SECTION.                                                          
025400     SKIP2                                                                
025500                                                                          
025600     ACCEPT DAGENS-DATUM     FROM DATE                                    
025700                                                                          
026100*-----------------------------------                                      
026200*----INITIERA INFÖR LÄSNING AV LASA                                       
026300*-----------------------------------                                      
026400     MOVE '6107'               TO W-IDHTYP                                
026500     MOVE W601-IDDC            TO W-IDDC                                  
026501                                  WS-IDDC                                 
026510     MOVE W601-IDLBBET         TO W-IDLBBET                               
026600                                                                          
026700*-----------------------------------                                      
026800*----INIT PRINTERID                                                       
026900*-----------------------------------                                      
026910     MOVE W601-IDPRTLST             TO WS-RAPP-PRINTER                    
027600                                                                          
028000*-----------------------------------                                      
028001*----ÖPPNA PRINTER                                                        
028002*-----------------------------------                                      
028010     PERFORM S01-PRT-OPEN                                                 
028011                                                                          
028012*-----------------------------------                                      
028013*----INITIERA RÄKNARE                                                     
028014*-----------------------------------                                      
028020     MOVE +1                   TO W-SIDNR                                 
028021     MOVE +6                   TO W-RADNR                                 
028200     .                                                                    
028300     EJECT                                                                
029700******************************************************************        
029800*  BEARBETA LÄSA LASA/REDIGERA LISTA                             *        
029900******************************************************************        
030000 B-BEARB SECTION.                                                         
030100                                                                          
030400*----------------------------------------------------------------*        
030500*--- LÄSNING AV W6LASA                                        ---*        
030600*--- MED ENBART LASTBÄRARE SOM NYCKEL                         ---*        
030700*----------------------------------------------------------------*        
030800                                                                          
030900     PERFORM IMS-GU-LASA-G111                                             
030910     MOVE 6108-ADINLOMR-LPL        TO W-ADINLOMR-LPL                      
031000     PERFORM BA-SKRIV-HUVUD                                               
031100                                                                          
031200     PERFORM IMS-GNP-LASA-G121                                            
031201     IF 6110-FLKLAR = NEJ                                                 
031210       PERFORM BC-SPARA-FRAN-INLA                                         
031220     END-IF                                                               
031300                                                                          
031400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR W-SIDNR > 25         
031410       IF 6110-FLKLAR = NEJ                                               
031500         IF W-RADNR = 42 OR (NY-ART AND W-RADNR >= 35)                    
031600            MOVE +6       TO W-RADNR                                      
031700            ADD +1        TO W-SIDNR                                      
031710            IF W-SIDNR > 25                                               
031720              PERFORM BD-SKRIV-SISTA-RAD                                  
031730            ELSE                                                          
031800              PERFORM BA-SKRIV-HUVUD                                      
031810            END-IF                                                        
031900         END-IF                                                           
032000         IF W-SIDNR > 25                                                  
032100           CONTINUE                                                       
032200         ELSE                                                             
032600           PERFORM BB-SKRIV-RAD                                           
032700           ADD 1                TO W-RADNR                                
032710           MOVE 6110-IDLEVNR    TO WS-IDLEVNR                             
032720           MOVE 6110-IDFS       TO WS-IDFS                                
033100           MOVE 6110-IDARTNR    TO WS-IDARTNR                             
033200           MOVE NEJ             TO NY-SW                                  
033210         END-IF                                                           
033300       END-IF                                                             
034000       PERFORM IMS-GNP-LASA-G121                                          
034001       IF 6110-FLKLAR = NEJ                                               
034011         IF 6110-IDARTNR = WS-IDARTNR AND                                 
034012            6110-IDFS    = WS-IDFS    AND                                 
034013            6110-IDLEVNR = WS-IDLEVNR                                     
034014            CONTINUE                                                      
034020         ELSE                                                             
034021***NYTT PARTI                                                             
034022           MOVE JA TO NY-SW                                               
034023           PERFORM BC-SPARA-FRAN-INLA                                     
034024         END-IF                                                           
034030       END-IF                                                             
034100     END-PERFORM                                                          
034901                                                                          
035300     .                                                                    
035400     EJECT                                                                
035500******************************************************************        
035600*  HUVUD                                                         *        
035610*  REDIGERA RUBRIKER                                             *        
035700******************************************************************        
035800 BA-SKRIV-HUVUD        SECTION.                                           
036010                                                                          
036050     MOVE DAGENS-DATUM     TO RAD4-DATUM                                  
036051                              RAD4-DATUM-ENG                              
036052     MOVE W-SIDNR          TO RAD4-SIDNR                                  
036053                              RAD4-SIDNR-ENG                              
036054     IF CDC-SE                                                            
036060       MOVE RAD4             TO WS-LISTRAD                                
036061     ELSE                                                                 
036062       MOVE RAD4-ENG         TO WS-LISTRAD                                
036063     END-IF                                                               
036070     MOVE PRT-NYSIDA-RAD4  TO PRT-RADSKIP                                 
036080     PERFORM S02-SKRIV-RAD                                                
036090                                                                          
036092     MOVE W601-IDLBBET     TO RAD7-IDLBBET                                
036093                              RAD7-IDLBBET-ENG                            
036094     MOVE W-ADINLOMR-LPL   TO RAD7-ADINLOMR-LPL                           
036095                              RAD7-ADINLOMR-LPL-ENG                       
036096     IF CDC-SE                                                            
036097       MOVE RAD7             TO WS-LISTRAD                                
036098     ELSE                                                                 
036099       MOVE RAD7-ENG         TO WS-LISTRAD                                
036100     END-IF                                                               
036101     MOVE PRT-AFTER-3      TO PRT-RADSKIP                                 
036102     PERFORM S02-SKRIV-RAD                                                
036103                                                                          
036104     IF CDC-SE                                                            
036105       MOVE RAD10            TO WS-LISTRAD                                
036106     ELSE                                                                 
036107       MOVE RAD10-ENG        TO WS-LISTRAD                                
036108     END-IF                                                               
036109     MOVE PRT-AFTER-3      TO PRT-RADSKIP                                 
036110     PERFORM S02-SKRIV-RAD                                                
036200     .                                                                    
036300     EJECT                                                                
036400******************************************************************        
036500*  RADER                                                         *        
036600******************************************************************        
036700 BB-SKRIV-RAD          SECTION.                                           
036800                                                                          
036903*----WS-...=  GAMLA                                                       
036911                                                                          
036912     MOVE SPACE            TO DETALJ-RAD                                  
036913     MOVE PRT-AFTER-1      TO PRT-RADSKIP                                 
036914                                                                          
036915     PERFORM IMS-GU-WDK611                                                
036916     MOVE '   ' TO RAD-ROS                                                
036918     IF CLAG-KVROS > 0                                                    
036919       IF CDC-SE                                                          
036920         MOVE 'JA '  TO RAD-ROS                                           
036922       END-IF                                                             
036923     END-IF                                                               
036924     MOVE ' ' TO RAD-FLFARLIG                                             
036925     IF CLAG-KDFARLIG = 4                                                 
036926     OR CLAG-KDFARLIG = 7                                                 
036927       MOVE 'J'  TO RAD-FLFARLIG                                          
036930     END-IF                                                               
036931                                                                          
036932     IF 6110-IDLEVNR = WS-IDLEVNR                                         
036933        IF W-RADNR = 1                                                    
036934          MOVE 6110-IDLEVNR TO RAD-IDLEVNR                                
036935        ELSE                                                              
036936          MOVE SPACE        TO RAD-IDLEVNR                                
036937        END-IF                                                            
036938        IF 6110-IDFS = WS-IDFS                                            
036939          IF W-RADNR = 1                                                  
036940            MOVE 6110-IDFS        TO RAD-IDFS                             
036941          ELSE                                                            
036942            MOVE SPACE TO RAD-IDFS                                        
036943          END-IF                                                          
036944          IF 6110-IDARTNR = WS-IDARTNR                                    
036945            IF W-RADNR = 1                                                
036946              MOVE 6110-IDARTNR TO RAD-IDARTNR                            
036947              MOVE WS-KVAVIS    TO RAD-TOT-ANT                            
036948              MOVE WS-KVKOLLI   TO RAD-KVKOLLI                            
036949              MOVE WS-KDLAGEMB  TO RAD-KDLAGEMB                           
036950              MOVE WS-BEFT      TO RAD-BEFT                               
036951              MOVE WS-ADINLOMR-FB  TO RAD-ADINLOMR-FB                     
036952              MOVE WS-ADTRDEST-KIT TO RAD-ADTRDEST-KIT                    
036953              IF WS-ADTRDEST(1:2) = 'CD'                                  
036954                MOVE 1 TO HELP-IX                                         
036955                PERFORM UNTIL (6110-ADINLOMR(1:2) =                       
036956                  W-ADLAGOMR(HELP-IX)) OR HELP-IX > 3                     
036957                  ADD 1 TO HELP-IX                                        
036958                END-PERFORM                                               
036959                IF 6110-ADINLOMR(1:2) = W-ADLAGOMR(HELP-IX)               
036960                  MOVE WS-RAD-KOLLIF(HELP-IX) TO RAD-KOLLIF               
036961                  MOVE WS-RAD-KOLLIT(HELP-IX) TO RAD-KOLLIT               
036962                ELSE                                                      
036963                  MOVE ZERO                   TO RAD-KOLLIF               
036964                                                 RAD-KOLLIT               
036965                END-IF                                                    
036966              ELSE                                                        
036967                MOVE ZERO                   TO RAD-KOLLIF                 
036968                                               RAD-KOLLIT                 
036969              END-IF                                                      
036970              IF WS-FLKVAKAR = JA                                         
036971                IF CDC-SE                                                 
036972                  MOVE 'KARANTÄN' TO RAD-INFO                             
036973                ELSE                                                      
036974                  MOVE 'Q-PROBLEMS' TO RAD-INFO                           
036975                END-IF                                                    
036976              ELSE                                                        
036980                MOVE SPACE      TO RAD-INFO                               
036982              END-IF                                                      
036983            ELSE                                                          
036984              MOVE ZERO  TO RAD-IDARTNR                                   
036985              MOVE ZERO  TO RAD-KVKOLLI                                   
036986              MOVE ZERO  TO RAD-BEFT                                      
036987              MOVE SPACE TO RAD-KDLAGEMB                                  
036988                                                                          
036989              MOVE SPACE TO RAD-ADINLOMR-FB                               
036990              MOVE SPACE TO RAD-INFO                                      
036991              MOVE SPACE TO RAD-ADTRDEST-KIT                              
036992                                                                          
036993              IF WS-ADTRDEST(1:2) = 'CD'                                  
036994                MOVE 1 TO HELP-IX                                         
036995                PERFORM UNTIL (6110-ADINLOMR(1:2) =                       
036996                  W-ADLAGOMR(HELP-IX)) OR HELP-IX > 3                     
036997                  ADD 1 TO HELP-IX                                        
036998                END-PERFORM                                               
036999                IF 6110-ADINLOMR(1:2) = W-ADLAGOMR(HELP-IX)               
037000                  MOVE WS-RAD-KOLLIF(HELP-IX) TO RAD-KOLLIF               
037001                  MOVE WS-RAD-KOLLIT(HELP-IX) TO RAD-KOLLIT               
037002                ELSE                                                      
037003                  MOVE ZERO                   TO RAD-KOLLIF               
037004                                                 RAD-KOLLIT               
037005                END-IF                                                    
037006              ELSE                                                        
037007                MOVE ZERO                   TO RAD-KOLLIF                 
037008                                               RAD-KOLLIT                 
037009              END-IF                                                      
037010            END-IF                                                        
037015          ELSE                                                            
037016            MOVE 6110-IDARTNR TO RAD-IDARTNR                              
037017            MOVE WS-KVAVIS    TO RAD-TOT-ANT                              
037018            MOVE WS-KVKOLLI   TO RAD-KVKOLLI                              
037019            MOVE WS-KDLAGEMB  TO RAD-KDLAGEMB                             
037020            MOVE WS-BEFT      TO RAD-BEFT                                 
037021            MOVE WS-ADINLOMR-FB  TO RAD-ADINLOMR-FB                       
037022            MOVE WS-ADTRDEST-KIT TO RAD-ADTRDEST-KIT                      
037023            IF WS-ADTRDEST(1:2) = 'CD'                                    
037024              MOVE 1 TO HELP-IX                                           
037025              PERFORM UNTIL (6110-ADINLOMR(1:2) =                         
037026                W-ADLAGOMR(HELP-IX)) OR HELP-IX > 3                       
037027                ADD 1 TO HELP-IX                                          
037028              END-PERFORM                                                 
037029              IF 6110-ADINLOMR(1:2) = W-ADLAGOMR(HELP-IX)                 
037030                MOVE WS-RAD-KOLLIF(HELP-IX) TO RAD-KOLLIF                 
037031                MOVE WS-RAD-KOLLIT(HELP-IX) TO RAD-KOLLIT                 
037032              ELSE                                                        
037033                MOVE ZERO                   TO RAD-KOLLIF                 
037034                                               RAD-KOLLIT                 
037035              END-IF                                                      
037036            ELSE                                                          
037037              MOVE ZERO                   TO RAD-KOLLIF                   
037038                                             RAD-KOLLIT                   
037039            END-IF                                                        
037049            IF WS-FLKVAKAR = JA                                           
037050              IF CDC-SE                                                   
037051                MOVE 'KARANTÄN' TO RAD-INFO                               
037052              ELSE                                                        
037053                MOVE 'Q-PROBLEMS' TO RAD-INFO                             
037054              END-IF                                                      
037055            ELSE                                                          
037056              MOVE SPACE      TO RAD-INFO                                 
037057            END-IF                                                        
037058          END-IF                                                          
037059        ELSE                                                              
037060          MOVE 6110-IDFS        TO RAD-IDFS                               
037061          MOVE 6110-IDARTNR     TO RAD-IDARTNR                            
037062          MOVE WS-KVAVIS        TO RAD-TOT-ANT                            
037063          MOVE WS-KVKOLLI       TO RAD-KVKOLLI                            
037064          MOVE WS-KDLAGEMB      TO RAD-KDLAGEMB                           
037065          MOVE WS-BEFT          TO RAD-BEFT                               
037066          MOVE WS-ADINLOMR-FB   TO RAD-ADINLOMR-FB                        
037067          MOVE WS-ADTRDEST-KIT  TO RAD-ADTRDEST-KIT                       
037068          IF WS-ADTRDEST(1:2) = 'CD'                                      
037069            MOVE 1 TO HELP-IX                                             
037070            PERFORM UNTIL (6110-ADINLOMR(1:2) =                           
037071              W-ADLAGOMR(HELP-IX)) OR HELP-IX > 3                         
037072              ADD 1 TO HELP-IX                                            
037073            END-PERFORM                                                   
037074            IF 6110-ADINLOMR(1:2) = W-ADLAGOMR(HELP-IX)                   
037075              MOVE WS-RAD-KOLLIF(HELP-IX) TO RAD-KOLLIF                   
037076              MOVE WS-RAD-KOLLIT(HELP-IX) TO RAD-KOLLIT                   
037077            ELSE                                                          
037078              MOVE ZERO                   TO RAD-KOLLIF                   
037079                                             RAD-KOLLIT                   
037080            END-IF                                                        
037081          ELSE                                                            
037082            MOVE ZERO                   TO RAD-KOLLIF                     
037083                                           RAD-KOLLIT                     
037084          END-IF                                                          
037094          IF WS-FLKVAKAR = JA                                             
037095            IF CDC-SE                                                     
037096              MOVE 'KARANTÄN' TO RAD-INFO                                 
037097            ELSE                                                          
037098              MOVE 'Q-PROBLEMS' TO RAD-INFO                               
037099            END-IF                                                        
037100          ELSE                                                            
037101            MOVE SPACE      TO RAD-INFO                                   
037102          END-IF                                                          
037103        END-IF                                                            
037104     ELSE                                                                 
037105        IF W-RADNR > 1                                                    
037106           MOVE PRT-AFTER-3 TO PRT-RADSKIP                                
037107           ADD +2           TO W-RADNR                                    
037108        END-IF                                                            
037109        MOVE 6110-IDLEVNR     TO RAD-IDLEVNR                              
037110        MOVE 6110-IDFS        TO RAD-IDFS                                 
037111        MOVE 6110-IDARTNR     TO RAD-IDARTNR                              
037112        MOVE WS-KVAVIS        TO RAD-TOT-ANT                              
037113        MOVE WS-KVKOLLI       TO RAD-KVKOLLI                              
037114        MOVE WS-KDLAGEMB      TO RAD-KDLAGEMB                             
037115        MOVE WS-BEFT          TO RAD-BEFT                                 
037116        MOVE WS-ADINLOMR-FB   TO RAD-ADINLOMR-FB                          
037117        MOVE WS-ADTRDEST-KIT  TO RAD-ADTRDEST-KIT                         
037118        IF WS-ADTRDEST(1:2) = 'CD'                                        
037119          MOVE 1 TO HELP-IX                                               
037120          PERFORM UNTIL (6110-ADINLOMR(1:2) =                             
037121            W-ADLAGOMR(HELP-IX)) OR HELP-IX > 3                           
037122            ADD 1 TO HELP-IX                                              
037123          END-PERFORM                                                     
037124          IF 6110-ADINLOMR(1:2) = W-ADLAGOMR(HELP-IX)                     
037125            MOVE WS-RAD-KOLLIF(HELP-IX) TO RAD-KOLLIF                     
037126            MOVE WS-RAD-KOLLIT(HELP-IX) TO RAD-KOLLIT                     
037127          ELSE                                                            
037128            MOVE ZERO                   TO RAD-KOLLIF                     
037129                                           RAD-KOLLIT                     
037130          END-IF                                                          
037131        ELSE                                                              
037132          MOVE ZERO                   TO RAD-KOLLIF                       
037133                                         RAD-KOLLIT                       
037134        END-IF                                                            
037144        IF WS-FLKVAKAR = JA                                               
037145          IF CDC-SE                                                       
037146            MOVE 'KARANTÄN' TO RAD-INFO                                   
037147          ELSE                                                            
037148            MOVE 'Q-PROBLEMS' TO RAD-INFO                                 
037149          END-IF                                                          
037150        ELSE                                                              
037151          MOVE SPACE      TO RAD-INFO                                     
037152        END-IF                                                            
037153     END-IF                                                               
037154                                                                          
037155     MOVE 6110-ADINLOMR    TO RAD-ADINLOMR                                
037156     MOVE 6110-KVAVIS      TO RAD-KVAVIS                                  
037157     MOVE 6110-KVAVIS-PRIO TO RAD-KVAVIS-PRIO                             
037158     MOVE 6110-KVAVIS-KIT  TO RAD-KVAVIS-KIT                              
037159                                                                          
037160     MOVE DETALJ-RAD       TO WS-LISTRAD                                  
037161     PERFORM S02-SKRIV-RAD                                                
037162                                                                          
037170     .                                                                    
037200     EJECT                                                                
037201 BC-SPARA-FRAN-INLA SECTION.                                              
037202                                                                          
037203     MOVE ZERO TO WS-KVKOLLI                                              
037204     MOVE 1 TO HELP-IX                                                    
037205     PERFORM UNTIL HELP-IX > 4                                            
037206       MOVE ZERO  TO WS-RAD-KOLLIF(HELP-IX)                               
037207                     WS-RAD-KOLLIT(HELP-IX)                               
037208       MOVE SPACE TO W-ADLAGOMR   (HELP-IX)                               
037209       ADD +1 TO HELP-IX                                                  
037210     END-PERFORM                                                          
037211     MOVE NEJ  TO WS-FLSPLPART                                            
037212     IF SEGMENT-FINNS                                                     
037213       MOVE W601-IDDC     TO W-IDDC-D1                                    
037214       MOVE 6110-IDLEVNR  TO W-IDLEVNR-D1                                 
037215                             STYR-IDLEVNR                                 
037216       MOVE 6110-IDFS     TO W-IDFS-D1                                    
037217       MOVE 6110-TIAVIDAT TO W-TIAVIDAT-D1                                
037218       MOVE 6110-IDARTNR  TO W-IDARTNR-D1                                 
037219                             STYR-IDARTNR                                 
037220       PERFORM IMS-GU-INLA-W6D101                                         
037221       PERFORM IMS-GNP-INLA-W6D111-ARTNR                                  
037222       MOVE INLA-ART-IDDC         TO STYR-IDDC                            
037223       MOVE INLA-ART-IDRADNR-INL  TO W-IDRADNR-INL                        
037224       MOVE INLA-ART-KVAVIS       TO WS-KVAVIS                            
037225       MOVE INLA-ART-KDLAGEMB     TO WS-KDLAGEMB                          
037226       MOVE INLA-ART-BEFT         TO WS-BEFT                              
037227       MOVE INLA-ART-FLKVAKAR     TO WS-FLKVAKAR                          
037228       MOVE INLA-ART-ADTRDEST-KIT TO WS-ADTRDEST-KIT                      
037229       MOVE INLA-ART-ADTRDEST     TO WS-ADTRDEST                          
037230       MOVE INLA-ART-IDFKNGRP     TO STYR-IDFKNGRP                        
037231       MOVE INLA-ART-FLSPLPART    TO WS-FLSPLPART                         
037232       MOVE INLA-ART-IDRADNR-INL  TO WS-IDRADNR-INL                       
037233       MOVE INLA-ART-ADLAGOMR     TO WS-ADLAGOMR                          
037234       MOVE +0                    TO STYR-BEFT                            
037235       CALL W611STYR USING STYR-W611STYR STYR-HANA-PCB                    
037236                                         STYR-PLAA-PCB                    
037237       IF STYR-KDSVAR-OK                                                  
037238         MOVE STYR-ADINLOMR-FB TO WS-ADINLOMR-FB                          
037239       ELSE                                                               
037240         MOVE SPACE            TO WS-ADINLOMR-FB                          
037241       END-IF                                                             
037242       PERFORM UNTIL SEGMENT-SAKNAS                                       
037243         PERFORM IMS-GNP-INLA-W6D121-RADNR-INL                            
037244         IF SEGMENT-FINNS                                                 
037245           IF WS-ADTRDEST(1:2) = 'CD'                                     
037246             MOVE WS-ADTRDEST(3:1) TO HELP-IX                             
037247             MOVE WS-ADLAGOMR       TO W-ADLAGOMR(HELP-IX)                
037248             MOVE INLA-RAD-IDOKOLLI TO WS-RAD-KOLLIF(HELP-IX)             
037250           END-IF                                                         
037254         END-IF                                                           
037255         PERFORM UNTIL SEGMENT-SAKNAS                                     
037256           IF INLA-RAD-IDOKOLLI > ZERO                                    
037257             ADD +1 TO WS-KVKOLLI                                         
037258           END-IF                                                         
037259           PERFORM IMS-GNP-INLA-W6D121-RADNR-INL                          
037260           IF INLA-RAD-IDOKOLLI > ZERO                                    
037261             IF WS-ADTRDEST(1:2) = 'CD'                                   
037262               MOVE WS-ADTRDEST(3:1) TO HELP-IX                           
037263               MOVE WS-ADLAGOMR       TO W-ADLAGOMR(HELP-IX)              
037264               MOVE INLA-RAD-IDOKOLLI TO WS-RAD-KOLLIT(HELP-IX)           
037265             END-IF                                                       
037268           END-IF                                                         
037269         END-PERFORM                                                      
037270         PERFORM IMS-GNP-INLA-W6D111-ARTNR                                
037271         IF SEGMENT-FINNS                                                 
037272           MOVE INLA-ART-IDRADNR-INL TO W-IDRADNR-INL                     
037273           ADD INLA-ART-KVAVIS       TO WS-KVAVIS                         
037274           MOVE INLA-ART-IDRADNR-INL  TO WS-IDRADNR-INL                   
037275           MOVE INLA-ART-ADTRDEST     TO WS-ADTRDEST                      
037276           MOVE INLA-ART-ADLAGOMR     TO WS-ADLAGOMR                      
037277         END-IF                                                           
037278       END-PERFORM                                                        
037279       MOVE SPACE TO STATUS-WS                                            
037280     END-IF                                                               
037281                                                                          
037290     .                                                                    
037300     EJECT                                                                
037400 BD-SKRIV-SISTA-RAD SECTION.                                              
037500                                                                          
037600     MOVE PRT-AFTER-2     TO PRT-RADSKIP                                  
037601     IF CDC-SE                                                            
037602       MOVE SISTA-RAD       TO WS-LISTRAD                                 
037603     ELSE                                                                 
037604       MOVE SISTA-RAD-ENG   TO WS-LISTRAD                                 
037605     END-IF                                                               
037700     PERFORM S02-SKRIV-RAD                                                
037800                                                                          
037900     .                                                                    
038000     EJECT                                                                
039800******************************************************************        
039900*                                                                *        
040000******************************************************************        
040100 C-AVSLUTA       SECTION.                                                 
040200                                                                          
040314*-----------------------------------                                      
040315*----STÄNG PRINTER                                                        
040316*-----------------------------------                                      
040320     PERFORM S03-PRT-CLOSE                                                
040500     .                                                                    
040600     EJECT                                                                
040700******************************************************************        
040800******************************************************************        
040900*  IMS-SECTIONER                                                 *        
041000******************************************************************        
041100******************************************************************        
041200                                                                          
043219******************************************************************        
043220*    LASA-PCB                                                    *        
043230******************************************************************        
043240     SKIP3                                                                
043250*----------------------------------------------------------------*        
043260 IMS-GU-LASA-G111      SECTION.                                           
043270     STRING 'W6LASA01(W6GXKEY  =' W-W6GX01KY-X ')'                        
043280          DELIMITED BY SIZE INTO SSA1                                     
043290     STRING 'W6LASA11(W6GXKEY  =' W-W6GX11KY-X ')'                        
043291          DELIMITED BY SIZE INTO SSA2                                     
043292     MOVE '  GE'            TO GODK-STATUSKODER                           
043293     CALL CBLTDLI USING GU  LASA-W6012120-PCB                             
043295                            DLI-IO-AREA-1                                 
043296                            SSA1                                          
043297                            SSA2                                          
043298     MOVE LASA-W6012120-STATUS-CODE  TO STATUS-WS                         
043299     PERFORM IMS-STATUSKONTROLL                                           
043300     .                                                                    
043301     EJECT                                                                
043310*----------------------------------------------------------------*        
043400 IMS-GNP-LASA-G121       SECTION.                                         
043500     STRING 'W6LASA01(W6GXKEY  =' W-W6GX01KY-X ')'                        
043600          DELIMITED BY SIZE INTO SSA1                                     
043700     STRING 'W6LASA11(W6GXKEY  =' W-W6GX11KY-X ')'                        
043800          DELIMITED BY SIZE INTO SSA2                                     
043900     MOVE 'W6LASA21 '            TO SSA3                                  
044000     MOVE '  GE'            TO GODK-STATUSKODER                           
044100     CALL CBLTDLI USING GNP LASA-W6012120-PCB                             
044200                            DLI-IO-AREA-1                                 
044300                            SSA1                                          
044400                            SSA2                                          
044500                            SSA3                                          
044600     MOVE LASA-W6012120-STATUS-CODE  TO STATUS-WS                         
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     .                                                                    
044900     EJECT                                                                
045241******************************************************************        
045242*    INLA-PCB                                                    *        
045243******************************************************************        
045246 IMS-GU-INLA-W6D101    SECTION.                                           
045247                                                                          
045248     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
045249          DELIMITED BY SIZE INTO SSA1                                     
045252     MOVE '  '            TO GODK-STATUSKODER                             
045253     CALL CBLTDLI USING GU  INLA-W6012120-PCB                             
045254                            DLI-IO-AREA-2                                 
045255                            SSA1                                          
045257     MOVE INLA-W6012120-STATUS-CODE  TO STATUS-WS                         
045258     PERFORM IMS-STATUSKONTROLL                                           
045259     .                                                                    
045260     EJECT                                                                
045261     SKIP3                                                                
045262*----------------------------------------------------------------*        
045263 IMS-GNP-INLA-W6D111-ARTNR  SECTION.                                      
045264                                                                          
045265     STRING 'W6INLA11(IDARTNR  =' W-IDARTNR-X ')'                         
045266          DELIMITED BY SIZE INTO SSA1                                     
045267     MOVE '  GE'          TO GODK-STATUSKODER                             
045268     CALL CBLTDLI USING GNP INLA-W6012120-PCB                             
045269                            DLI-IO-AREA-2                                 
045270                            SSA1                                          
045271     MOVE INLA-W6012120-STATUS-CODE  TO STATUS-WS                         
045272     PERFORM IMS-STATUSKONTROLL                                           
045273     .                                                                    
045274     EJECT                                                                
045275     SKIP3                                                                
045276*----------------------------------------------------------------*        
045277 IMS-GNP-INLA-W6D121-RADNR-INL  SECTION.                                  
045278                                                                          
045279     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
045280          DELIMITED BY SIZE INTO SSA1                                     
045281     MOVE 'W6INLA21'      TO SSA2                                         
045282     MOVE '  GE'          TO GODK-STATUSKODER                             
045283     CALL CBLTDLI USING GNP INLA-W6012120-PCB                             
045284                            DLI-IO-AREA-2                                 
045285                            SSA1                                          
045286                            SSA2                                          
045287     MOVE INLA-W6012120-STATUS-CODE  TO STATUS-WS                         
045288     PERFORM IMS-STATUSKONTROLL                                           
045289     .                                                                    
045290     EJECT                                                                
045291     SKIP3                                                                
045322******************************************************************        
045323*    WDK6-PCB                                                    *        
045324******************************************************************        
045325 IMS-GU-WDK611    SECTION.                                                
045326                                                                          
045327     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
045328          DELIMITED BY SIZE INTO SSA1                                     
045329     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
045330          DELIMITED BY SIZE INTO SSA2                                     
045332     MOVE '  '          TO GODK-STATUSKODER                               
045333     CALL CBLTDLI USING GU  WDK6-W6012120-PCB                             
045334                            DLI-IO-WDK6                                   
045335                            SSA1                                          
045336                            SSA2                                          
045337     MOVE WDK6-W6012120-STATUS-CODE  TO STATUS-WS                         
045338     PERFORM IMS-STATUSKONTROLL                                           
045339     .                                                                    
045340     EJECT                                                                
045341     SKIP3                                                                
045342*----------------------------------------------------------------*        
045350 IMS-STATUSKONTROLL SECTION.                                              
045400     SKIP2                                                                
045500     SET STATUS-IX TO 1                                                   
045600     SEARCH GODK-STATUS                                                   
045700       AT END                                                             
045800         MOVE 'FEL I W6012120-PGM - W6INLA' TO FELTEXT-STR                
046000         CALL FELLOG                                                      
046100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046200         CONTINUE                                                         
046300     END-SEARCH                                                           
046400     .                                                                    
046500     EJECT                                                                
051800                                                                          
051810*----------------------------------------------------------------*        
051900 S01-PRT-OPEN SECTION.                                                    
051910                                                                          
052010                                                                          
052100     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
052200                          PRT-OPEN                                        
052300                          WS-RAPP-PRINTER                                 
052400                          ALT-PCB                                         
052420                          WS-IDPRT                                        
052500                          WS-DUMMY                                        
052600                          WS-DUMMY                                        
052700     .                                                                    
052800     EJECT                                                                
052810*----------------------------------------------------------------*        
052900 S02-SKRIV-RAD SECTION.                                                   
052910                                                                          
053100                                                                          
053200     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
053300                          PRT-WRITE                                       
053310                          WS-RAPP-PRINTER                                 
053320                          ALT-PCB                                         
053600                          PRT-RADSKIP                                     
053700                          WS-RAD                                          
053800     .                                                                    
053900     EJECT                                                                
053910*----------------------------------------------------------------*        
054000 S03-PRT-CLOSE SECTION.                                                   
054010                                                                          
054110                                                                          
054200     CALL W006PRS1  USING PRT-SPOOL-OVR                                   
054300                          PRT-CLOSE                                       
054310                          WS-RAPP-PRINTER                                 
054320                          ALT-PCB                                         
054400                          WS-IDPRT                                        
054600                          WS-DUMMY                                        
054700                          WS-DUMMY                                        
054800     .                                                                    
054900     EJECT                                                                
