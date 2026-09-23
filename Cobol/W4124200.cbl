000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4124200.                                                
000300 AUTHOR.         GERRY CARMICHAEL..                                       
000400 DATE-WRITTEN.   00/05/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*    LÄGGER UPP TRANSAR PÅ DISPATCHER FÖR CLEARING AV                     
001000*    LDC RADER FRÅN LDC TILL CDC MED HJÄLP AV 4254 OCH                    
001100*    4255 (ANNULLATION OCH TILLÄGG).                                      
001200*                                                                         
001300*    DATABASER: UPPDATERAR   KOMMUNIKATIONS DB                            
001400*                            WLKOMA-(WDP8)                                
001500*               UPPDATERAR   HÄNDELSE REGISTER (CHKPOINT)                 
001600*                            WL4579-(WDR4)                                
001700*                                                                         
001710*    STORY 2375089 / ADD IDSYSTEM VOUI,ECOM                               
001720*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INFIL                                                      
002700     SELECT W41242                     ASSIGN TO W41242D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W41242                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W41239       -L.                                               
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W4124200'.            
004400 01  CHKP-VAR.                                                            
004500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005000     03 CHKP-MAX                 PIC S9(3)   VALUE +3   COMP-3.           
005100 77  POST-ANT                    PIC S9(3)   VALUE +0   COMP-3.           
005200 77  TRAD-IX                     PIC S9(9)   COMP SYNC VALUE ZERO.        
005300 77  TRAD-IX-MAX                 PIC S9(9)   COMP SYNC VALUE +8.          
005400 77  W-KVRADER                   PIC 9(5)     VALUE ZERO.                 
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
005800                                                                          
005900 77  W41242-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W41242                       VALUE 'J'.                   
006010                                                                          
006020 77  SW-BYT-ARTIKEL              PIC X       VALUE 'N'.                   
006030     88  BYT-ARTIKEL                         VALUE 'J'.                   
006040     88  BYT-EJ-ARTIKEL                      VALUE 'N'.                   
006041                                                                          
006050 77  W-DISP                      PIC S9(8)   VALUE ZERO.                  
006060 77  W-KVAKS-SDC                 PIC 9(7)    VALUE ZERO.                  
006070 77  W-KVOKS-DAG                 PIC 9(7)    VALUE ZERO.                  
006080 77  W-KVOKS-BULK                PIC 9(7)    VALUE ZERO.                  
006100     SKIP2                                                                
006200                                                                          
006300 01  W-PRARTNTO                  PIC 9(7)V9(2).                           
006301 01  FILLER REDEFINES W-PRARTNTO.                                         
006302    03  W-PRARTNTO-HEL           PIC 9(7).                                
006303    03  W-PRARTNTO-DEC           PIC 9(2).                                
006304                                                                          
006305*01  -COPY WWDCKONS                                                       
006306                                                                          
006307 01  NYCKLAR-TILL-DLI.                                                    
006308     03 W-PRARTNTO-X.                                                     
006309        05 W-PRARTNTO-X-HEL      PIC X(7).                                
006310        05 FILLER                PIC X(1)    VALUE '.'.                   
006311        05 W-PRARTNTO-X-DEC      PIC X(2).                                
006315                                                                          
006316*PRATRTNTO-LOC  CONVERSION                                                
006317 01  W-PRARTNTO-LOC              PIC 9(7)V9(2).                           
006318 01  FILLER REDEFINES W-PRARTNTO-LOC.                                     
006319    03  W-PRARTNTO-LOC-HEL       PIC 9(7).                                
006320    03  W-PRARTNTO-LOC-DEC       PIC 9(2).                                
006321                                                                          
006323                                                                          
006324 01  NYCKLAR-TILL-DLI-LOC.                                                
006325     03 W-PRARTNTO-LOC-X.                                                 
006326        05 W-PRARTNTO-LOC-X-HEL      PIC X(7).                            
006327        05 FILLER                PIC X(1)    VALUE '.'.                   
006328        05 W-PRARTNTO-LOC-X-DEC      PIC X(2).                            
006329                                                                          
006330 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600*                                                                         
006700 01  W-DATUM                     PIC 9(6)    VALUE ZERO.                  
006800 01  W-TIKLOCK                   PIC 9(8)    VALUE ZERO.                  
006900     EJECT                                                                
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100*                                                                         
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007500     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
007510     03  W009KSIF                PIC X(8)    VALUE 'W009KSIF'.            
007520     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008010*                                                                         
008011*                                                                         
008013 01  KONTROLL-SIFFRA.                                                     
008014     03  REK-IDARTNR             PIC 9(9)    VALUE 0.                     
008015     03  REK-LNGD                PIC 9(1)    VALUE 9.                     
008016     03  REK-REKSIFFR            PIC 9(1)    VALUE 0.                     
008120                                                                          
008121 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
008122*   -COPY W411SPAR                                                        
008123     EJECT                                                                
008124                                                                          
008125                                                                          
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                             'IN-AREA-START'.             
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W41239      -PRE IN-                                      
008700*                                                                         
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009000     SKIP3                                                                
009100 01  FILLER          PIC X(16)   VALUE 'NYCKLAR TILL DLI'.                
009101 01  NYCKLAR-TILL-DLI.                                                    
009110     03  W-IDARTNR-X.                                                     
009120         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009130     03  W-IDDC-X.                                                        
009140         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
009200     SKIP3                                                                
009300*01  -COPY WDGX01                                                         
009400     EJECT                                                                
009500*    --- STATUS-KOD FRÅN IMS                                              
009600 01  STATUS-WS                   PIC XX.                                  
009700     88  SEGMENT-FINNS                       VALUE '  '.                  
009710     88  INSERT-OK                           VALUE '  '.                  
009800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010100     88  IMS-EJ-OK                           VALUE 'XD'.                  
010200     SKIP2                                                                
010300 01  GODK-STATUSKODER.                                                    
010400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010500     SKIP3                                                                
010600 01  SSA1                        PIC X(64).                               
010700 01  SSA2                        PIC X(64).                               
010800     EJECT                                                                
010900*    --- IMS FUNKTIONSKODER                                               
011000*01  -COPY W0003                                                          
011100     EJECT                                                                
011200*    ---  DLI INPUT-OUTPUT AREA                                           
011300                                                                          
011400 01  FILLER         PIC X(16) VALUE '4580-IO-AREA'.                       
011500 01  4580-IO-AREA.                                                        
011600*    03  -COPY WDGX4580                                                   
011793                                                                          
011794 01  FILLER                      PIC X(16)   VALUE 'WDK601-AREA'.         
011795 01  DLI-IO-AREA-WDK601.                                                  
011796*    03  -COPY WDK601                                                     
011810                                                                          
011811 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
011812 01  DLI-IO-AREA-WDK611.                                                  
011813*    03  -COPY WDK611                                                     
011814                                                                          
011815 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
011816 01  DLI-IO-AREA-WDK711.                                                  
011817*    03  -COPY WDK711                                                     
011818                                                                          
011819 01  FILLER                      PIC X(16)   VALUE 'WDD701-AREA'.         
011820 01  DLI-IO-AREA-WDD701.                                                  
011821*    03  -COPY WDD701                                                     
011822                                                                          
011823 01  FILLER                      PIC X(16)   VALUE 'WDD702-AREA'.         
011824 01  DLI-IO-AREA-WDD702.                                                  
011825*    03  -COPY WDD702                                                     
011826                                                                          
011827 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
011828 01  DLI-IO-AREA-WDQ101.                                                  
011829*    03  -COPY WDQ101                                                     
011830     EJECT                                                                
011840                                                                          
011900 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
012000*01  -COPY WMSGKOM                                                        
012100     EJECT                                                                
012200                                                                          
012300 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
012400     SKIP3                                                                
012500*01  -COPY WMSGAREA                                                       
012600     EJECT                                                                
012700*                                                                         
012800*    --- AREOR FÖR W006KOM SUBMODUL                                       
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
013100 01  KOM-IO-AREA.                                                         
013200   03  KOM-AREA                     PIC X(1500) VALUE SPACE.              
013300   03 ARAD REDEFINES KOM-AREA.                                            
013400*    05   -COPY W4I25401 -PRE ARAD-                                       
013500     EJECT                                                                
013600   03 TRAD REDEFINES KOM-AREA.                                            
013700*    05   -COPY W4I25501 -PRE TRAD-                                       
013800     EJECT                                                                
013900                                                                          
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200*01  -COPY W0009   -PRE MSG-                                              
014300                                                                          
014400 01  DISP-PCB                PIC X.                                       
014500 01  KOMA-PCB                PIC X.                                       
014600                                                                          
014700*01  -COPY W0008  -PRE 4579-                                              
014800     05  FILLER              PIC X.                                       
014810                                                                          
014811*01  -COPY W0008   -PRE WDK6-                                             
014830     05  FILLER                  PIC X.                                   
014831                                                                          
014832*01  -COPY W0008   -PRE WDK7-                                             
014833     05  FILLER                  PIC X.                                   
014834                                                                          
014835*01  -COPY W0008   -PRE WDD7-                                             
014836     05  FILLER                  PIC X.                                   
014837                                                                          
014838*01  -COPY W0008   -PRE WDQ1-                                             
014839     05  FILLER                  PIC X.                                   
014840                                                                          
014850 01  SPAR-WDF8-PCB               PIC X.                                   
014860 01  SPAR-WDF8A-PCB              PIC X.                                   
014870 01  SPAR-WDK6-PCB               PIC X.                                   
014900     EJECT                                                                
014910                                                                          
014920                                                                          
015000 PROCEDURE DIVISION  USING MSG-PCB                                        
015100                           DISP-PCB                                       
015200                           KOMA-PCB                                       
015300                           4579-PCB                                       
015320                           WDK6-PCB WDK7-PCB WDD7-PCB                     
015330                           WDQ1-PCB                                       
015350                           SPAR-WDF8-PCB SPAR-WDF8A-PCB                   
015360                           SPAR-WDK6-PCB.                                 
015400 MAIN SECTION.                                                            
015500     ENTRY 'DLITCBL' USING MSG-PCB                                        
015600                           DISP-PCB                                       
015700                           KOMA-PCB                                       
015800                           4579-PCB                                       
015801                           WDK6-PCB WDK7-PCB WDD7-PCB                     
015802                           WDQ1-PCB                                       
015803                           SPAR-WDF8-PCB SPAR-WDF8A-PCB                   
015804                           SPAR-WDK6-PCB.                                 
015900                                                                          
016000     PERFORM A-INITIERA                                                   
016100                                                                          
016200     PERFORM IMS-LAS-ATERSTART                                            
016300                                                                          
016400     IF SEGMENT-SAKNAS                                                    
016500        MOVE SPACE        TO 4580-WDGX4580-CTX                            
016600        MOVE '1'          TO 4580-KDSEGKEY                                
016700        MOVE ZERO         TO 4580-KVPOST                                  
016800        MOVE W-DATUM      TO 4580-TIUPPDAT                                
016900        MOVE W-TIKLOCK    TO 4580-TIUPPTID                                
017000                                                                          
017100        PERFORM IMS-ISRT-ATERSTART                                        
017200        PERFORM IMS-LAS-ATERSTART                                         
017300     END-IF                                                               
017400                                                                          
017500     IF 4580-KVPOST > +0                                                  
017600        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
017700     ELSE                                                                 
017800        PERFORM S01-LAS-W41242                                            
017900     END-IF                                                               
018000                                                                          
018100     IF NOT END-OF-W41242                                                 
018200        PERFORM S03-SKAPA-MSG-KOM-AREA                                    
018300     END-IF                                                               
018400                                                                          
018500     PERFORM UNTIL END-OF-W41242                                          
018600                                                                          
018700        PERFORM C-BEARBETA                                                
018800        PERFORM S01-LAS-W41242                                            
018900                                                                          
019000     END-PERFORM                                                          
019100                                                                          
019200     PERFORM Z-FINIT                                                      
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700                                                                          
019800 A-INITIERA SECTION.                                                      
019900                                                                          
020000     PERFORM IMS-RESTART                                                  
020100                                                                          
020200     OPEN INPUT W41242                                                    
020300                                                                          
020400     MOVE +0                   TO POST-ANT                                
020500                                  CHKP-ANT                                
020600                                  TRAD-IX                                 
020700                                                                          
020800     MOVE SPACE                TO MSG-AREA                                
020900                                                                          
021000     ACCEPT W-DATUM            FROM DATE                                  
021100     ACCEPT W-TIKLOCK          FROM TIME                                  
021200                                                                          
021300     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
021400     .                                                                    
021500     EJECT                                                                
021600 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
021700                                                                          
021800     PERFORM S01-LAS-W41242                                               
021900                                                                          
022000     PERFORM UNTIL END-OF-W41242  OR                                      
022100                     POST-ANT = 4580-KVPOST                               
022200        PERFORM S01-LAS-W41242                                            
022300        ADD +1           TO POST-ANT                                      
022400     END-PERFORM                                                          
022500                                                                          
022600     IF END-OF-W41242                                                     
022700        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
022800                      TO FELTEXT                                          
022900        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300                                                                          
023400 C-BEARBETA SECTION.                                                      
023500                                                                          
023600     PERFORM CA-KOLLA-ARTIKELBYTE                                         
023601     IF BYT-ARTIKEL                                                       
023603        PERFORM CB-KOLLA-SPARRAR                                          
023604     END-IF                                                               
023610     IF BYT-ARTIKEL                                                       
023620        PERFORM CC-KOLLA-TILLK-SALDO                                      
023630     END-IF                                                               
023700     PERFORM CD-SKAPA-TILLAGGRADTRANS                                     
023710     PERFORM CE-SKAPA-ANNULRADTRANS                                       
023800     PERFORM X-TAG-CHECKPOINT                                             
023900                                                                          
023930     PERFORM CF-SKAPA-OBKR                                                
024000     .                                                                    
024100     EJECT                                                                
024200                                                                          
025254 CA-KOLLA-ARTIKELBYTE SECTION.                                            
025255                                                                          
025258     MOVE NEJ        TO SW-BYT-ARTIKEL                                    
025259     MOVE IN-IDARTNR TO W-IDARTNR                                         
025260     PERFORM IMS-GU-WDK611                                                
025263     IF SEGMENT-FINNS AND CLAG-KDERS = 01 OR 11                           
025264        MOVE JA         TO SW-BYT-ARTIKEL                                 
025269                                                                          
025270        PERFORM IMS-GU-WDD701                                             
025271        IF SEGMENT-SAKNAS                                                 
025272           MOVE NEJ TO SW-BYT-ARTIKEL                                     
025273        ELSE                                                              
025274           IF KVKORT NOT = 1                                              
025275              MOVE NEJ TO SW-BYT-ARTIKEL                                  
025276           END-IF                                                         
025277        END-IF                                                            
025279        IF SW-BYT-ARTIKEL = JA                                            
025280           PERFORM IMS-GNP-WDD702                                         
025281           MOVE IDARTNR-TILLK TO W-IDARTNR                                
025282           PERFORM IMS-GU-WDK601                                          
025283           IF SEGMENT-FINNS                                               
025284              PERFORM IMS-GNP-WDK611                                      
025285              IF SEGMENT-FINNS                                            
025286                 IF CLAG-REDIRLEV > ZERO                                  
025287                    MOVE NEJ TO SW-BYT-ARTIKEL                            
025288                 END-IF                                                   
025289              ELSE                                                        
025290                 MOVE NEJ TO SW-BYT-ARTIKEL                               
025291              END-IF                                                      
025292           ELSE                                                           
025293              MOVE NEJ TO SW-BYT-ARTIKEL                                  
025294           END-IF                                                         
025295        END-IF                                                            
025297     ELSE                                                                 
025298        MOVE NEJ TO SW-BYT-ARTIKEL                                        
025299     END-IF                                                               
025300     .                                                                    
025301                                                                          
025302     EJECT                                                                
025303                                                                          
025304 CB-KOLLA-SPARRAR SECTION.                                                
025305                                                                          
025306     MOVE IN-BERADREF        TO SPAR-BERADREF                             
025307     IF IN-IDBIL = SPACE OR IN-IDSYSTEM = 'SOFT'                          
025308       IF IN-IDSYSTEM = 'VDI '                                            
025309         MOVE SPACE          TO SPAR-BEKUNDRF                             
025310       ELSE                                                               
025311         MOVE IN-BEKUNDRF    TO SPAR-BEKUNDRF                             
025312       END-IF                                                             
025313     ELSE                                                                 
025314       MOVE 'SOFTWARE'       TO SPAR-BEKUNDRF                             
025315     END-IF                                                               
025316     MOVE CLAG-FLAVRART      TO SPAR-FLAVRART                             
025317     MOVE IN-FLFORBI         TO SPAR-FLFORBI                              
025318     MOVE CLAG-FLLSRDEL      TO SPAR-FLLSRDEL                             
025319     MOVE CLAG-FLRADREF      TO SPAR-FLRADREF                             
025320     MOVE IN-FLRESTN         TO SPAR-FLRESTN                              
025321     MOVE W-IDARTNR          TO SPAR-IDARTNR                              
025322     MOVE ART-FLIART         TO SPAR-FLIART                               
025323     MOVE CLAG-FLMARKSP      TO SPAR-FLMARKSP                             
025324     MOVE IN-IDDISTR         TO SPAR-IDDISTR                              
025325     MOVE IN-IDKUNDNR        TO SPAR-IDKUNDNR                             
025326     MOVE IN-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                          
025331     MOVE IN-IDDC            TO SPAR-IDDC                                 
025333     MOVE 'LDC '             TO SPAR-IDSYSTEM                             
025334     MOVE ART-KDERS-UTG      TO SPAR-KDERS-UTG                            
025335     MOVE CLAG-KDERS         TO SPAR-KDERS                                
025336     MOVE IN-KDFAKTYP        TO SPAR-KDFAKTYP                             
025337     MOVE CLAG-KDLEVSP       TO SPAR-KDLEVSP                              
025338     MOVE +1                 TO SPAR-KDORDBEH                             
025339     MOVE IN-KDORDKL         TO SPAR-KDORDKL                              
025340     MOVE ART-KDPRODSL       TO SPAR-KDPRODSL                             
025341     MOVE ART-KDSORT         TO SPAR-KDSORT                               
025342     MOVE IN-KDPRTYP         TO SPAR-KDPRTYP                              
025343     MOVE IN-KDTPOTYP        TO SPAR-KDTPOTYP                             
025344     MOVE CLAG-KDUART        TO SPAR-KDUART                               
025345     MOVE CLAG-PRARTSTD      TO SPAR-PRARTSTD                             
025346     MOVE ART-TIFINLV        TO SPAR-TIFINLV                              
025347     MOVE IN-TIRODAT         TO SPAR-TIRODAT                              
025348     MOVE IN-TITPO           TO SPAR-TITPO                                
025349     MOVE IN-FLSDCLEV        TO SPAR-FLSDCLEV                             
025350     MOVE ZERO               TO SPAR-TIREPDAT                             
025351                                                                          
025352     IF IN-IDBIL = SPACE OR IN-IDSYSTEM = 'SOFT'                          
025353       MOVE IN-FLORDSPE      TO SPAR-FLORDSPE                             
025354     ELSE                                                                 
025355       MOVE JA               TO SPAR-FLORDSPE                             
025356     END-IF                                                               
025357                                                                          
025358     MOVE IN-FLOVRLEV        TO SPAR-FLOVRLEV                             
025359     MOVE IN-FLEMBORD        TO SPAR-FLEMBORD                             
025360     MOVE ZERO               TO SPAR-KDORDBEK                             
025361                                                                          
025362     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
025363                                       SPAR-WDF8A-PCB                     
025364                                       SPAR-WDK6-PCB                      
025365                                                                          
025367     IF SPAR-KDORDBEK > ZERO                                              
025368        MOVE NEJ TO SW-BYT-ARTIKEL                                        
025369     END-IF                                                               
025370     .                                                                    
025371     EJECT                                                                
025372                                                                          
025373 CC-KOLLA-TILLK-SALDO SECTION.                                            
025374                                                                          
025375     MOVE IN-IDDC TO W-IDDC                                               
025376     PERFORM IMS-GU-WDK711                                                
025377     IF SEGMENT-FINNS                                                     
025378        IF SLAG-KVAKS-SDC < +0                                            
025379          MOVE +0          TO W-KVAKS-SDC                                 
025380        ELSE                                                              
025381          MOVE SLAG-KVAKS-SDC TO W-KVAKS-SDC                              
025382        END-IF                                                            
025383        IF SLAG-KVOKS-DAG < +0                                            
025384          MOVE +0          TO W-KVOKS-DAG                                 
025385        ELSE                                                              
025386          MOVE SLAG-KVOKS-DAG TO W-KVOKS-DAG                              
025387        END-IF                                                            
025388        IF SLAG-KVOKS-BULK < +0                                           
025389          MOVE +0           TO W-KVOKS-BULK                               
025390        ELSE                                                              
025391          MOVE SLAG-KVOKS-BULK TO W-KVOKS-BULK                            
025392        END-IF                                                            
025393        COMPUTE W-DISP = SLAG-KVLS +                                      
025394                         W-KVAKS-SDC -                                    
025395                         W-KVOKS-DAG -                                    
025396                         W-KVOKS-BULK                                     
025397        IF W-DISP > ZERO                                                  
025398          COMPUTE W-DISP = W-DISP    -                                    
025399                           SLAG-KVUTRS -                                  
025400                           SLAG-KVSPARR-KVAL                              
025401        END-IF                                                            
025402        IF IN-KVBEART > W-DISP                                            
025403           MOVE NEJ TO SW-BYT-ARTIKEL                                     
025404        END-IF                                                            
025405     ELSE                                                                 
025406        MOVE NEJ TO SW-BYT-ARTIKEL                                        
025407     END-IF                                                               
025408     .                                                                    
025410     EJECT                                                                
025500                                                                          
029600 CD-SKAPA-TILLAGGRADTRANS SECTION.                                        
029700     SKIP2                                                                
029800     MOVE +1                     TO TRAD-IX                               
029900     MOVE SPACE                  TO KOM-AREA                              
030000                                                                          
030100     COMPUTE MSG-KVLL = LENGTH OF TRAD-MID-W4I25501-CTX + 17              
030200                                                                          
030300     MOVE LOW-VALUE              TO MSG-KDZ1                              
030400     MOVE LOW-VALUE              TO MSG-KDZ2                              
030500     MOVE 'W4T255X '             TO MSG-KDTRANS-1                         
030600     MOVE '4255'                 TO MSG-IDTRANS-1                         
030700     MOVE '1'                    TO MSG-KDMFSFOR-1                        
030800     MOVE 'W4I25501'             TO MSG-KOM-IDCPYTXT                      
030900     MOVE 'W41240 '              TO MSG-KOM-IDSNDNOD                      
031000     MOVE 'W4124200'             TO MSG-KOM-IDSNDJOB                      
031100     ADD +1                      TO W-TIKLOCK                             
031200     MOVE W-TIKLOCK              TO MSG-KOM-TIKLOCK                       
031300                                                                          
031400*    MOVE IN-IDSYSTEM            TO TRAD-MID-IDSYSTEM                     
031410     IF BYT-ARTIKEL                                                       
031411       IF IN-IDSYSTEM (1:3) = 'LYN'                                       
031412         MOVE 'LYNS'             TO TRAD-MID-IDSYSTEM                     
031413       ELSE                                                               
031414         IF IN-IDSYSTEM (1:3) = 'ECO'                                     
031415           MOVE 'ECOS'           TO TRAD-MID-IDSYSTEM                     
031416         ELSE                                                             
031417           IF IN-IDSYSTEM (1:3) = 'VOU'                                   
031418             MOVE 'VOUS'         TO TRAD-MID-IDSYSTEM                     
031419           ELSE                                                           
031420             IF IN-IDSYSTEM (1:3) = 'TAD'                                 
031421               MOVE 'TADS'       TO TRAD-MID-IDSYSTEM                     
031422             ELSE                                                         
031423               IF IN-IDSYSTEM (1:3) = 'ACC'                               
031424                 MOVE 'ACCS'       TO TRAD-MID-IDSYSTEM                   
031425               ELSE                                                       
031426                 IF IN-IDSYSTEM (1:3) = 'APA'                             
031427                   MOVE 'APAS'       TO TRAD-MID-IDSYSTEM                 
031428                 ELSE                                                     
031429                   IF IN-IDSYSTEM (1:3) = 'APB'                           
031430                     MOVE 'APBS'       TO TRAD-MID-IDSYSTEM               
031431                   ELSE                                                   
031432                     IF IN-IDSYSTEM (1:3) = 'APC'                         
031433                       MOVE 'APCS'       TO TRAD-MID-IDSYSTEM             
031434                     ELSE                                                 
031435                       IF IN-IDSYSTEM (1:3) = 'APD'                       
031436                         MOVE 'APDS'       TO TRAD-MID-IDSYSTEM           
031437                       ELSE                                               
031438                         IF IN-IDSYSTEM (1:3) = 'APE'                     
031439                           MOVE 'APES'       TO TRAD-MID-IDSYSTEM         
031440                         ELSE                                             
031441                           IF IN-IDSYSTEM (1:3) = 'APF'                   
031442                             MOVE 'APFS'                                  
031443                                              TO TRAD-MID-IDSYSTEM        
031444                           ELSE                                           
031445                             IF IN-IDSYSTEM (1:3) = 'APG'                 
031446                               MOVE 'APGS'                                
031447                                              TO TRAD-MID-IDSYSTEM        
031448                             ELSE                                         
031449                               IF IN-IDSYSTEM (1:3) = 'APH'               
031450                                 MOVE 'APHS'                              
031451                                              TO TRAD-MID-IDSYSTEM        
031452                               ELSE                                       
031453                                 IF IN-IDSYSTEM (1:3) = 'API'             
031454                                   MOVE 'APIS'                            
031455                                              TO TRAD-MID-IDSYSTEM        
031456                                 ELSE                                     
031457                                   IF IN-IDSYSTEM (1:3) = 'APJ'           
031458                                     MOVE 'APJS'                          
031459                                              TO TRAD-MID-IDSYSTEM        
031460                                   ELSE                                   
031461                                     MOVE 'LDCS'                          
031462                                              TO TRAD-MID-IDSYSTEM        
031463                                   END-IF                                 
031464                                 END-IF                                   
031465                               END-IF                                     
031466                             END-IF                                       
031467                           END-IF                                         
031468                         END-IF                                           
031469                       END-IF                                             
031470                      END-IF                                              
031471                    END-IF                                                
031472                  END-IF                                                  
031473                END-IF                                                    
031474              END-IF                                                      
031475            END-IF                                                        
031476          END-IF                                                          
031477        END-IF                                                            
031478     ELSE                                                                 
031510       IF IN-IDSYSTEM (1:3) = 'LYN'                                       
031520         MOVE 'LYNC'             TO TRAD-MID-IDSYSTEM                     
031530       ELSE                                                               
031540         IF IN-IDSYSTEM (1:3) = 'ECO'                                     
031550           MOVE 'ECOC'           TO TRAD-MID-IDSYSTEM                     
031560         ELSE                                                             
031570           IF IN-IDSYSTEM (1:3) = 'VOU'                                   
031580             MOVE 'VOUC'         TO TRAD-MID-IDSYSTEM                     
031590           ELSE                                                           
031591             IF IN-IDSYSTEM (1:3) = 'TAD'                                 
031592               MOVE 'TADC'       TO TRAD-MID-IDSYSTEM                     
031593             ELSE                                                         
031594               IF IN-IDSYSTEM (1:3) = 'ACC'                               
031595                 MOVE 'ACCC'       TO TRAD-MID-IDSYSTEM                   
031596               ELSE                                                       
031597                 IF IN-IDSYSTEM (1:3) = 'APA'                             
031598                   MOVE 'APAC'       TO TRAD-MID-IDSYSTEM                 
031599                 ELSE                                                     
031600                   IF IN-IDSYSTEM (1:3) = 'APB'                           
031601                     MOVE 'APBC'       TO TRAD-MID-IDSYSTEM               
031602                   ELSE                                                   
031603                     IF IN-IDSYSTEM (1:3) = 'APC'                         
031604                       MOVE 'APCC'       TO TRAD-MID-IDSYSTEM             
031605                     ELSE                                                 
031606                       IF IN-IDSYSTEM (1:3) = 'APD'                       
031607                         MOVE 'APDC'       TO TRAD-MID-IDSYSTEM           
031608                       ELSE                                               
031609                         IF IN-IDSYSTEM (1:3) = 'APE'                     
031610                           MOVE 'APEC'       TO TRAD-MID-IDSYSTEM         
031611                         ELSE                                             
031612                           IF IN-IDSYSTEM (1:3) = 'APF'                   
031613                             MOVE 'APFC'                                  
031614                                              TO TRAD-MID-IDSYSTEM        
031615                           ELSE                                           
031616                             IF IN-IDSYSTEM (1:3) = 'APG'                 
031617                               MOVE 'APGC'                                
031618                                              TO TRAD-MID-IDSYSTEM        
031619                             ELSE                                         
031620                               IF IN-IDSYSTEM (1:3) = 'APH'               
031621                                 MOVE 'APHC'                              
031622                                              TO TRAD-MID-IDSYSTEM        
031623                               ELSE                                       
031624                                 IF IN-IDSYSTEM (1:3) = 'API'             
031625                                   MOVE 'APIC'                            
031626                                              TO TRAD-MID-IDSYSTEM        
031627                                 ELSE                                     
031628                                   IF IN-IDSYSTEM (1:3) = 'APJ'           
031629                                     MOVE 'APJC'                          
031630                                              TO TRAD-MID-IDSYSTEM        
031631                                   ELSE                                   
031632                                     MOVE 'LDCC'                          
031633                                              TO TRAD-MID-IDSYSTEM        
031634                                   END-IF                                 
031635                                 END-IF                                   
031636                               END-IF                                     
031637                             END-IF                                       
031638                           END-IF                                         
031639                         END-IF                                           
031640                       END-IF                                             
031641                      END-IF                                              
031642                    END-IF                                                
031643                  END-IF                                                  
031644                END-IF                                                    
031645              END-IF                                                      
031646            END-IF                                                        
031647          END-IF                                                          
031648        END-IF                                                            
031649     END-IF                                                               
031650     MOVE IN-IDDISTR             TO TRAD-MID-IDDISTR                      
031700     MOVE IN-IDKUNDNR            TO TRAD-MID-IDKUNDNR                     
031800     MOVE IN-IDORDNR7            TO TRAD-MID-IDORDNR                      
031900     MOVE IN-KDORDKL             TO TRAD-MID-KDORDKL                      
032000     MOVE SPACE                  TO TRAD-MID-BEKUNDRF-001                 
032100     MOVE SPACE                  TO TRAD-MID-IDMFSMED                     
032200     MOVE 'J'                    TO TRAD-MID-FLSLUT                       
032300                                                                          
032400     MOVE IN-IDARTNR             TO TRAD-MID-IDARTNR  (TRAD-IX)           
032500*    MOVE IN-KVBEART             TO TRAD-MID-KVBEART  (TRAD-IX)           
032510     MOVE IN-KVBEART-Q           TO TRAD-MID-KVBEART  (TRAD-IX)           
032600     MOVE IN-KDKVBRYT            TO TRAD-MID-KDKVBRYT (TRAD-IX)           
032700     MOVE IN-BERADREF            TO TRAD-MID-BERADREF (TRAD-IX)           
032800     MOVE IN-KDDSP               TO TRAD-MID-KDDSP    (TRAD-IX)           
032900     MOVE SPACE                  TO TRAD-MID-FLSLATT  (TRAD-IX)           
033000     MOVE IN-IDPRQUES            TO TRAD-MID-IDPRQUES (TRAD-IX)           
033010                                                                          
033100     MOVE IN-PRARTNTO-LOCPREL  TO W-PRARTNTO                              
033200     MOVE W-PRARTNTO-HEL       TO W-PRARTNTO-X-HEL                        
033201     MOVE W-PRARTNTO-DEC       TO W-PRARTNTO-X-DEC                        
033202                                                                          
033210     MOVE W-PRARTNTO-X         TO                                         
033220                               TRAD-MID-PRARTNTO-LOCPREL (TRAD-IX)        
033230                                                                          
033231     IF  IN-IDSYSTEM (1:3) = 'ECO'                                        
033240         MOVE IN-PRARTNTO-LOC      TO W-PRARTNTO-LOC                      
033250         MOVE W-PRARTNTO-LOC-HEL   TO W-PRARTNTO-LOC-X-HEL                
033260         MOVE W-PRARTNTO-LOC-DEC   TO W-PRARTNTO-LOC-X-DEC                
033270                                                                          
033280         MOVE W-PRARTNTO-LOC-X     TO                                     
033290                                   TRAD-MID-PRARTNTO-LOC (TRAD-IX)        
033400     ELSE                                                                 
033410         MOVE SPACE             TO TRAD-MID-PRARTNTO-LOC (TRAD-IX)        
033411     END-IF                                                               
033420     MOVE SPACE                 TO TRAD-MID-PRARTBTO-LOC (TRAD-IX)        
033500                                    TRAD-MID-KDVALISO    (TRAD-IX)        
033600                                    TRAD-MID-KDVAT       (TRAD-IX)        
033700     MOVE ZERO                   TO TRAD-MID-RERAB       (TRAD-IX)        
033800     MOVE SPACE                  TO TRAD-MID-KDRAB       (TRAD-IX)        
033900                                    TRAD-MID-BEART-VIPS  (TRAD-IX)        
034000     MOVE SPACE                  TO TRAD-MID-IDKUNDRF-WIP(TRAD-IX)        
034100                                                                          
034110     IF BYT-ARTIKEL                                                       
034140       IF IN-IDSYSTEM (1:3) = 'LYN'                                       
034141         MOVE 'LYNS'             TO TRAD-MID-IDSYSTEM                     
034142       ELSE                                                               
034143         IF IN-IDSYSTEM (1:3) = 'ECO'                                     
034144           MOVE 'ECOS'           TO TRAD-MID-IDSYSTEM                     
034145         ELSE                                                             
034146           IF IN-IDSYSTEM (1:3) = 'VOU'                                   
034147             MOVE 'VOUS'         TO TRAD-MID-IDSYSTEM                     
034148           ELSE                                                           
034149             IF IN-IDSYSTEM (1:3) = 'TAD'                                 
034150               MOVE 'TADS'       TO TRAD-MID-IDSYSTEM                     
034151             ELSE                                                         
034152               IF IN-IDSYSTEM (1:3) = 'ACC'                               
034153                 MOVE 'ACCS'       TO TRAD-MID-IDSYSTEM                   
034154               ELSE                                                       
034155                 IF IN-IDSYSTEM (1:3) = 'APA'                             
034156                   MOVE 'APAS'       TO TRAD-MID-IDSYSTEM                 
034157                 ELSE                                                     
034158                   IF IN-IDSYSTEM (1:3) = 'APB'                           
034159                     MOVE 'APBS'       TO TRAD-MID-IDSYSTEM               
034160                   ELSE                                                   
034161                     IF IN-IDSYSTEM (1:3) = 'APC'                         
034162                       MOVE 'APCS'       TO TRAD-MID-IDSYSTEM             
034163                     ELSE                                                 
034164                       IF IN-IDSYSTEM (1:3) = 'APD'                       
034165                         MOVE 'APDS'       TO TRAD-MID-IDSYSTEM           
034166                       ELSE                                               
034167                         IF IN-IDSYSTEM (1:3) = 'APE'                     
034168                           MOVE 'APES'       TO TRAD-MID-IDSYSTEM         
034169                         ELSE                                             
034170                           IF IN-IDSYSTEM (1:3) = 'APF'                   
034171                             MOVE 'APFS'                                  
034172                                              TO TRAD-MID-IDSYSTEM        
034173                           ELSE                                           
034174                             IF IN-IDSYSTEM (1:3) = 'APG'                 
034175                               MOVE 'APGS'                                
034176                                              TO TRAD-MID-IDSYSTEM        
034177                             ELSE                                         
034178                               IF IN-IDSYSTEM (1:3) = 'APH'               
034179                                 MOVE 'APHS'                              
034180                                              TO TRAD-MID-IDSYSTEM        
034181                               ELSE                                       
034182                                 IF IN-IDSYSTEM (1:3) = 'API'             
034183                                   MOVE 'APIS'                            
034184                                              TO TRAD-MID-IDSYSTEM        
034185                                 ELSE                                     
034186                                   IF IN-IDSYSTEM (1:3) = 'APJ'           
034187                                     MOVE 'APJS'                          
034188                                              TO TRAD-MID-IDSYSTEM        
034189                                   ELSE                                   
034190                                     MOVE 'LDCS'                          
034191                                              TO TRAD-MID-IDSYSTEM        
034192                                   END-IF                                 
034193                                 END-IF                                   
034194                               END-IF                                     
034195                             END-IF                                       
034196                           END-IF                                         
034197                         END-IF                                           
034198                       END-IF                                             
034199                      END-IF                                              
034200                    END-IF                                                
034201                  END-IF                                                  
034202                END-IF                                                    
034203              END-IF                                                      
034204            END-IF                                                        
034205          END-IF                                                          
034206        END-IF                                                            
034207        MOVE IDARTNR-TILLK       TO TRAD-MID-IDARTNR  (TRAD-IX)           
034208     END-IF                                                               
034210                                                                          
034300                                                                          
034400     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
034500     CALL W006KOM USING MSG-PCB                                           
034600                       DISP-PCB                                           
034700                       KOMA-PCB                                           
034800                       MSG-KOM-WMSGKOM                                    
034900                       MSG-IO-AREA                                        
035000                                                                          
035100     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
035200*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
035300*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
035400        MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W41242 '                    
035500                      TO FELTEXT                                          
035600        DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W41242 '                    
035700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
035800     END-IF                                                               
035900     MOVE SPACE TO KOM-AREA                                               
036100     .                                                                    
036101                                                                          
036102 CE-SKAPA-ANNULRADTRANS SECTION.                                          
036103     SKIP2                                                                
036104     MOVE SPACE                  TO KOM-AREA                              
036105                                                                          
036106     COMPUTE MSG-KVLL = LENGTH OF ARAD-MID-W4I25401-CTX + 17              
036107                                                                          
036108     MOVE LOW-VALUE              TO MSG-KDZ1                              
036109     MOVE LOW-VALUE              TO MSG-KDZ2                              
036110     MOVE 'W4T254X '             TO MSG-KDTRANS-1                         
036111     MOVE '4254'                 TO MSG-IDTRANS-1                         
036112     MOVE '1'                    TO MSG-KDMFSFOR-1                        
036113**   MOVE 'W4I25401'             TO MSG-KOM-IDCPYTXT                      
036114**   MOVE 'W41240 '              TO MSG-KOM-IDSNDNOD                      
036115**   MOVE 'W4124200'             TO MSG-KOM-IDSNDJOB                      
036116**   ADD +1                      TO W-TIKLOCK                             
036117**   MOVE W-TIKLOCK              TO MSG-KOM-TIKLOCK                       
036118                                                                          
036119     PERFORM CEA-SKAPA-ANNULRADTRANS                                      
036120                                                                          
036121     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
036122     CALL W006KOM USING MSG-PCB                                           
036123                        DISP-PCB                                          
036124                        KOMA-PCB                                          
036125                        MSG-KOM-WMSGKOM                                   
036126                        MSG-IO-AREA                                       
036127                                                                          
036128     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
036129*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
036130*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
036131        MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W41242 '                    
036132                      TO FELTEXT                                          
036133        DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W41242 '                    
036134        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
036135     END-IF                                                               
036136                                                                          
036137     MOVE SPACE TO KOM-AREA                                               
036138     .                                                                    
036139     EJECT                                                                
036140                                                                          
036141 CEA-SKAPA-ANNULRADTRANS SECTION.                                         
036142                                                                          
036143*    MOVE IN-IDSYSTEM            TO ARAD-MID-IDSYSTEM                     
036165       IF IN-IDSYSTEM (1:3) = 'LYN'                                       
036166         MOVE 'LYNC'             TO ARAD-MID-IDSYSTEM                     
036167       ELSE                                                               
036168         IF IN-IDSYSTEM (1:3) = 'ECO'                                     
036169           MOVE 'ECOC'           TO ARAD-MID-IDSYSTEM                     
036170         ELSE                                                             
036171           IF IN-IDSYSTEM (1:3) = 'VOU'                                   
036172             MOVE 'VOUC'         TO ARAD-MID-IDSYSTEM                     
036173           ELSE                                                           
036174             IF IN-IDSYSTEM (1:3) = 'TAD'                                 
036175               MOVE 'TADC'       TO ARAD-MID-IDSYSTEM                     
036176             ELSE                                                         
036177               IF IN-IDSYSTEM (1:3) = 'ACC'                               
036178                 MOVE 'ACCC'       TO ARAD-MID-IDSYSTEM                   
036179               ELSE                                                       
036180                 IF IN-IDSYSTEM (1:3) = 'APA'                             
036181                   MOVE 'APAC'       TO ARAD-MID-IDSYSTEM                 
036182                 ELSE                                                     
036183                   IF IN-IDSYSTEM (1:3) = 'APB'                           
036184                     MOVE 'APBC'       TO ARAD-MID-IDSYSTEM               
036185                   ELSE                                                   
036186                     IF IN-IDSYSTEM (1:3) = 'APC'                         
036187                       MOVE 'APCC'       TO ARAD-MID-IDSYSTEM             
036188                     ELSE                                                 
036189                       IF IN-IDSYSTEM (1:3) = 'APD'                       
036190                         MOVE 'APDC'       TO ARAD-MID-IDSYSTEM           
036191                       ELSE                                               
036192                         IF IN-IDSYSTEM (1:3) = 'APE'                     
036193                           MOVE 'APEC'       TO ARAD-MID-IDSYSTEM         
036194                         ELSE                                             
036195                           IF IN-IDSYSTEM (1:3) = 'APF'                   
036196                             MOVE 'APFC'                                  
036197                                              TO ARAD-MID-IDSYSTEM        
036198                           ELSE                                           
036199                             IF IN-IDSYSTEM (1:3) = 'APG'                 
036200                               MOVE 'APGC'                                
036201                                              TO ARAD-MID-IDSYSTEM        
036202                             ELSE                                         
036203                               IF IN-IDSYSTEM (1:3) = 'APH'               
036204                                 MOVE 'APHC'                              
036205                                              TO ARAD-MID-IDSYSTEM        
036206                               ELSE                                       
036207                                 IF IN-IDSYSTEM (1:3) = 'API'             
036208                                   MOVE 'APIC'                            
036209                                              TO ARAD-MID-IDSYSTEM        
036210                                 ELSE                                     
036211                                   IF IN-IDSYSTEM (1:3) = 'APJ'           
036212                                     MOVE 'APJC'                          
036213                                              TO ARAD-MID-IDSYSTEM        
036214                                   ELSE                                   
036215                                     MOVE 'LDCC'                          
036216                                              TO ARAD-MID-IDSYSTEM        
036217                                   END-IF                                 
036218                                 END-IF                                   
036219                               END-IF                                     
036220                             END-IF                                       
036221                           END-IF                                         
036222                         END-IF                                           
036223                       END-IF                                             
036224                      END-IF                                              
036225                    END-IF                                                
036226                  END-IF                                                  
036227                END-IF                                                    
036228              END-IF                                                      
036229            END-IF                                                        
036230          END-IF                                                          
036231        END-IF                                                            
036232     MOVE IN-IDDISTR             TO ARAD-MID-IDDISTR                      
036233     MOVE IN-IDKUNDNR            TO ARAD-MID-IDKUNDNR                     
036234     MOVE IN-IDORDNR7            TO ARAD-MID-IDORDNR                      
036235     MOVE 'J'                    TO ARAD-MID-FLSLUT                       
036236     MOVE IN-IDARTNR             TO ARAD-MID-IDARTNR                      
036237*    MOVE IN-KVBEART             TO ARAD-MID-KVBEART                      
036238     MOVE IN-KVBEART-Q           TO ARAD-MID-KVBEART                      
036239     MOVE IN-IDDC                TO ARAD-MID-IDDC                         
036240     .                                                                    
036241     EJECT                                                                
036242                                                                          
036243 CF-SKAPA-OBKR SECTION.                                                   
036244                                                                          
036245     PERFORM CFA-INITIERA-OBKR                                            
036246     IF BYT-ARTIKEL                                                       
036247        PERFORM CFB-OBKR-ERSATTNING                                       
036248     ELSE                                                                 
036249        PERFORM CFC-OBKR-TRANSFER                                         
036250     END-IF                                                               
036251                                                                          
036252     .                                                                    
036253     EJECT                                                                
036254                                                                          
036255 CFA-INITIERA-OBKR SECTION.                                               
036256                                                                          
036257     MOVE IN-IDORDER     TO OBKR-IDORDER                                  
036258     MOVE IN-IDDISTR     TO OBKR-IDDISTR                                  
036259     MOVE IN-IDKUNDNR    TO OBKR-IDKUNDNR                                 
036260     MOVE IN-IDORDNR7    TO OBKR-IDKUNDRF                                 
036261     MOVE IN-IDARTNR     TO OBKR-IDARTNR                                  
036262     MOVE ZERO           TO OBKR-IDLOPNR                                  
036263     MOVE ZERO           TO OBKR-IDSEKVNR                                 
036264     MOVE IN-IDDC        TO OBKR-IDDC                                     
036265     MOVE ZERO           TO OBKR-KDORDBEK                                 
036266     MOVE SPACE          TO OBKR-BEERS                                    
036267                            OBKR-BEKUNDRF                                 
036268     MOVE IN-BERADREF    TO OBKR-BERADREF                                 
036269     MOVE SPACE          TO OBKR-BEVOLREF                                 
036270     MOVE ZERO           TO OBKR-DIERS-KVOT                               
036271     MOVE SPACE          TO OBKR-FLAKPLOC                                 
036272                            OBKR-FLINVEST                                 
036273                            OBKR-FLOBOK                                   
036274     MOVE JA             TO OBKR-FLOBTRAN                                 
036275     MOVE SPACE          TO OBKR-FLOBPRT                                  
036276                            OBKR-FLPRTILL                                 
036277                            OBKR-FLRESTN                                  
036278                            OBKR-FLSLATT                                  
036279                            OBKR-FLTILLK                                  
036280     MOVE ZERO           TO OBKR-IDARTNR-TILLK                            
036281     MOVE SPACE          TO OBKR-IDDC-RO                                  
036282     MOVE ZERO           TO OBKR-IDKAMPRF                                 
036283     MOVE 'W4124200'     TO OBKR-IDPGM                                    
036284     MOVE SPACE          TO OBKR-IDKUNDRF-RO                              
036285                            OBKR-IDLEVNR                                  
036286     MOVE ZERO           TO OBKR-IDLOPNR-RO                               
036287     IF IN-IDSYSTEM = 'LDCS'                                              
036288        MOVE 'LDC '      TO OBKR-IDSYSTEM                                 
036289     ELSE                                                                 
036311       IF IN-IDSYSTEM (1:3) = 'LYNS'                                      
036312         MOVE 'LYNK'             TO OBKR-IDSYSTEM                         
036313       ELSE                                                               
036314         IF IN-IDSYSTEM (1:3) = 'ECOS'                                    
036315           MOVE 'ECOM'           TO OBKR-IDSYSTEM                         
036316         ELSE                                                             
036317           IF IN-IDSYSTEM (1:3) = 'VOUS'                                  
036318             MOVE 'VOUI'         TO OBKR-IDSYSTEM                         
036319           ELSE                                                           
036320             IF IN-IDSYSTEM (1:3) = 'TADS'                                
036321               MOVE 'TAD '       TO OBKR-IDSYSTEM                         
036322             ELSE                                                         
036323               IF IN-IDSYSTEM (1:3) = 'ACCS'                              
036324                 MOVE 'ACC '       TO OBKR-IDSYSTEM                       
036325               ELSE                                                       
036326                 IF IN-IDSYSTEM (1:3) = 'APAS'                            
036327                   MOVE 'APA '       TO OBKR-IDSYSTEM                     
036328                 ELSE                                                     
036329                   IF IN-IDSYSTEM (1:3) = 'APBS'                          
036330                     MOVE 'APB '       TO OBKR-IDSYSTEM                   
036331                   ELSE                                                   
036332                     IF IN-IDSYSTEM (1:3) = 'APCS'                        
036333                       MOVE 'APC '       TO OBKR-IDSYSTEM                 
036334                     ELSE                                                 
036335                       IF IN-IDSYSTEM (1:3) = 'APDS'                      
036336                         MOVE 'APD '       TO OBKR-IDSYSTEM               
036337                       ELSE                                               
036338                         IF IN-IDSYSTEM (1:3) = 'APES'                    
036339                           MOVE 'APE '       TO OBKR-IDSYSTEM             
036340                         ELSE                                             
036341                           IF IN-IDSYSTEM (1:3) = 'APFS'                  
036342                             MOVE 'APF '                                  
036343                                              TO OBKR-IDSYSTEM            
036344                           ELSE                                           
036345                             IF IN-IDSYSTEM (1:3) = 'APGS'                
036346                               MOVE 'APG '                                
036347                                              TO OBKR-IDSYSTEM            
036348                             ELSE                                         
036349                               IF IN-IDSYSTEM (1:3) = 'APHS'              
036350                                 MOVE 'APH '                              
036351                                              TO OBKR-IDSYSTEM            
036352                               ELSE                                       
036353                                 IF IN-IDSYSTEM (1:3) = 'APIS'            
036354                                   MOVE 'API '                            
036355                                              TO OBKR-IDSYSTEM            
036356                                 ELSE                                     
036357                                   IF IN-IDSYSTEM (1:3) = 'APJS'          
036358                                     MOVE 'APJ '                          
036359                                              TO OBKR-IDSYSTEM            
036360                                   ELSE                                   
036361                                     MOVE IN-IDSYSTEM                     
036362                                              TO OBKR-IDSYSTEM            
036363                                   END-IF                                 
036364                                 END-IF                                   
036365                               END-IF                                     
036366                             END-IF                                       
036367                           END-IF                                         
036368                         END-IF                                           
036369                       END-IF                                             
036370                      END-IF                                              
036371                    END-IF                                                
036372                  END-IF                                                  
036373                END-IF                                                    
036374              END-IF                                                      
036375            END-IF                                                        
036376          END-IF                                                          
036377        END-IF                                                            
036378     END-IF                                                               
036379     MOVE IN-KDDSP       TO OBKR-KDDSP                                    
036380     MOVE ZERO           TO OBKR-KDERS                                    
036381                            OBKR-KDKVBRYT                                 
036382     MOVE SPACE          TO OBKR-KDOI                                     
036383                            OBKR-KDPRTYP                                  
036384     MOVE ZERO           TO OBKR-KDTPOTYP                                 
036385                            OBKR-KDVRINFO                                 
036386                            OBKR-KVANNANT                                 
036387                            OBKR-KVAVBART                                 
036388     MOVE IN-KVBEART     TO OBKR-KVBEART                                  
036389     MOVE IN-KVBEART-Q   TO OBKR-KVBEART-Q                                
036390     MOVE ZERO           TO OBKR-KVBEART-TILLK                            
036391                            OBKR-KVPREAVB                                 
036392                            OBKR-KVPRERO                                  
036393                            OBKR-KVQPACK                                  
036394                            OBKR-KVRO                                     
036395                            OBKR-KVSLATT                                  
036396                            OBKR-PRARTNTO                                 
036397                            OBKR-PRBPRIS                                  
036398                            OBKR-REKSIFFR                                 
036399     MOVE OBKR-IDARTNR   TO REK-IDARTNR                                   
036400     MOVE 9              TO REK-LNGD                                      
036401     MOVE 0              TO REK-REKSIFFR                                  
036402     CALL W009KSIF    USING REK-IDARTNR                                   
036403                            REK-LNGD                                      
036404                            REK-REKSIFFR                                  
036405     MOVE REK-REKSIFFR   TO OBKR-REKSIFFR                                 
036406     MOVE 0              TO OBKR-REKSIFFR-TILLK                           
036407                            OBKR-RERF-RAD                                 
036408                            OBKR-TIDISPIN                                 
036409                            OBKR-TIORDREG                                 
036410                            OBKR-TIPRIS                                   
036411     ACCEPT OBKR-TIREGDAT   FROM DATE                                     
036412     ACCEPT OBKR-TIREGTID   FROM TIME                                     
036413     MOVE 0              TO OBKR-TIRODAT                                  
036414                            OBKR-TITIREGD-9KOMPL                          
036415                            OBKR-TITPO                                    
036416                            OBKR-TITIORDD-9KOMPL                          
036417                            OBKR-KDFRAKT                                  
036418     MOVE IN-KDORDKL     TO OBKR-KDORDKL                                  
036419     MOVE SPACE          TO OBKR-IDBILTYP                                 
036420                            OBKR-TIAAAA                                   
036421                            OBKR-IDCHASSI-PIE                             
036422     MOVE IN-PRARTNTO-LOC     TO OBKR-PRARTNTO-LOC                        
036423     MOVE IN-PRARTNTO-LOCPREL TO OBKR-PRARTNTO-LOCPREL                    
036424     MOVE IN-IDPRQUES    TO OBKR-IDPRQUES                                 
036425     MOVE ZERO           TO OBKR-PRARTBTO-LOC                             
036426     MOVE SPACE          TO OBKR-KDVALISO                                 
036427                            OBKR-KDVAT                                    
036428     MOVE ZERO           TO OBKR-RERAB                                    
036429     MOVE SPACE          TO OBKR-KDRAB                                    
036430                            OBKR-BEART-VIPS                               
036431                            OBKR-KDORDTYP-LDC                             
036432                            OBKR-IDKUNDRF-WIP                             
036433     MOVE ZERO           TO OBKR-TIREPDAT                                 
036434     MOVE SPACE          TO OBKR-CLEARGROUP                               
036435     MOVE ZERO           TO OBKR-TIDLEVDAT                                
036436     MOVE +0             TO OBKR-PRAVCOST                                 
036437     MOVE 'N/A'          TO OBKR-KDVALISO                                 
036438*    DENNA KDVALISO ANVÄNDS ALDRIG                                        
036439*    MEN EFTER GLOBAL EXPORT FÅR DEN INTE VARA SPACE, DÄRFÖR N/A'         
036440     .                                                                    
036441     EJECT                                                                
036442                                                                          
036443                                                                          
036444 CFB-OBKR-ERSATTNING SECTION.                                             
036445                                                                          
036446     MOVE 41               TO OBKR-KDORDBEK                               
036447     MOVE 1                TO OBKR-IDLOPNR                                
036448     MOVE 1                TO OBKR-IDSEKVNR                               
036449     PERFORM IMS-ISRT-WDQ101                                              
036450                                                                          
036451     IF INSERT-OK                                                         
036452        MOVE W-IDARTNR     TO OBKR-IDARTNR-TILLK                          
036453                                 REK-IDARTNR                              
036454        MOVE 9             TO REK-LNGD                                    
036455        MOVE 0             TO REK-REKSIFFR                                
036456        CALL W009KSIF   USING REK-IDARTNR                                 
036457                                 REK-LNGD                                 
036458                                 REK-REKSIFFR                             
036459        MOVE REK-REKSIFFR  TO OBKR-REKSIFFR-TILLK                         
036460        COMPUTE OBKR-KVBEART-TILLK = DIERS-TILLK * OBKR-KVBEART           
036461        MOVE DIERS-TILLK   TO OBKR-DIERS-KVOT                             
036462                                                                          
036463        MOVE 2             TO OBKR-IDSEKVNR                               
036464        PERFORM IMS-ISRT-WDQ101                                           
036465     END-IF                                                               
036466     .                                                                    
036467     EJECT                                                                
036468                                                                          
036469                                                                          
036470 CFC-OBKR-TRANSFER SECTION.                                               
036471                                                                          
036472     MOVE 15               TO OBKR-KDORDBEK                               
036473     MOVE 1                TO OBKR-IDLOPNR                                
036474     MOVE 1                TO OBKR-IDSEKVNR                               
036475     MOVE WC-CDC-SE        TO OBKR-IDDC                                   
036476     PERFORM IMS-ISRT-WDQ101                                              
036477     .                                                                    
036478     EJECT                                                                
036479                                                                          
036480                                                                          
036481     EJECT                                                                
036482 S01-LAS-W41242 SECTION.                                                  
036490     SKIP2                                                                
036500     READ W41242 INTO IN-AREA                                             
036600       AT END                                                             
036700          MOVE JA TO W41242-EOF-SW                                        
036800     END-READ                                                             
036900                                                                          
037000     IF NOT END-OF-W41242                                                 
037100        MOVE 'W41242'       TO POSTSUM-FDNAMN                             
037200        MOVE 'W41242D1'     TO POSTSUM-DDNAMN2                            
037300        MOVE '001'          TO POSTSUM-TRANSTYP                           
037400        CALL POSTSUM USING POSTSUM-PARM                                   
037500     END-IF                                                               
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900 S03-SKAPA-MSG-KOM-AREA SECTION.                                          
038000     SKIP2                                                                
038100     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
038200     MOVE +54                    TO MSG-KOM-KVLL                          
038300     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
038400     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
038500     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
038600     MOVE 'W4I25501'             TO MSG-KOM-IDCPYTXT                      
038700     MOVE 'W41240 '              TO MSG-KOM-IDSNDNOD                      
038800     MOVE 'W4124200'             TO MSG-KOM-IDSNDJOB                      
038900     MOVE W-DATUM                TO MSG-KOM-TIREGDAT                      
039000**   ADD +1                      TO W-TIKLOCK                             
039100     MOVE W-TIKLOCK              TO MSG-KOM-TIKLOCK                       
039200     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
039300     .                                                                    
039400     EJECT                                                                
039500                                                                          
039600 X-TAG-CHECKPOINT SECTION.                                                
039700     SKIP2                                                                
039800*    UPPDATERA ÅTERSTARTREGISTRET                                         
039900     PERFORM IMS-LAS-ATERSTART                                            
040000     ADD +1          TO 4580-KVPOST                                       
040100     ACCEPT 4580-TIUPPDAT FROM DATE                                       
040200     ACCEPT 4580-TIUPPTID FROM TIME                                       
040300                                                                          
040400     PERFORM IMS-REPL-ATERSTART                                           
040500                                                                          
040600*    TAG CHECKPOINT                                                       
040700     PERFORM IMS-CHECKPOINT                                               
040800     .                                                                    
040900     EJECT                                                                
041000                                                                          
041100 Z-FINIT    SECTION.                                                      
041200                                                                          
041300     CLOSE  W41242                                                        
041400                                                                          
041500*    NOLLA ÅTERSTARTINFORMATIONEN                                         
041600     PERFORM IMS-LAS-ATERSTART                                            
041700     MOVE +0         TO 4580-KVPOST                                       
041800     ACCEPT 4580-TIUPPDAT FROM DATE                                       
041900     ACCEPT 4580-TIUPPTID FROM TIME                                       
042000                                                                          
042100     PERFORM IMS-REPL-ATERSTART                                           
042200                                                                          
042300     MOVE 'S'      TO POSTSUM-OPKOD                                       
042400     CALL POSTSUM USING POSTSUM-PARM                                      
042500     .                                                                    
042600     EJECT                                                                
042700                                                                          
042800* IMS SECTIONER                                                           
042900     SKIP3                                                                
043000                                                                          
043100 IMS-RESTART SECTION.                                                     
043200     SKIP2                                                                
043300     MOVE SPACE TO MSG-IO-AREA                                            
043400     MOVE '  ' TO GODK-STATUSKODER                                        
043500     CALL CBLTDLI USING XRST MSG-PCB                                      
043600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
043700                        CHKP-AREA-LENGTH CHKP-AREA                        
043800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043900     PERFORM IMS-STATUSKONTROLL                                           
044000     .                                                                    
044100                                                                          
044200 IMS-CHECKPOINT SECTION.                                                  
044300     MOVE SPACE        TO MSG-IO-AREA                                     
044400     MOVE '  XD'       TO GODK-STATUSKODER                                
044500     CALL CBLTDLI USING CHKP MSG-PCB                                      
044600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
044700                        CHKP-AREA-LENGTH CHKP-AREA                        
044800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044900     PERFORM IMS-STATUSKONTROLL                                           
045000     IF IMS-EJ-OK                                                         
045100       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
045200       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
045300                            TO FELTEXT                                    
045400       CALL FELLOG                                                        
045500     END-IF                                                               
045600     .                                                                    
045700                                                                          
045800 IMS-LAS-ATERSTART SECTION.                                               
045900     SKIP2                                                                
046000     MOVE '4579'         TO IDHTYP                                        
046100     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
046200     MOVE 'W4124200'     TO NYCKEL-VALFRI(1:8)                            
046300     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
046400                    DELIMITED BY SIZE INTO SSA1                           
046500     MOVE 'WL457911 '    TO SSA2                                          
046600     MOVE '  GE'           TO GODK-STATUSKODER                            
046700     CALL CBLTDLI USING GHU 4579-PCB 4580-IO-AREA SSA1 SSA2               
046800     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
046900     PERFORM IMS-STATUSKONTROLL                                           
047000     .                                                                    
047100                                                                          
047200 IMS-ISRT-ATERSTART SECTION.                                              
047300     SKIP2                                                                
047400     MOVE '4579'         TO IDHTYP                                        
047500     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
047600     MOVE 'W4124200'     TO NYCKEL-VALFRI(1:8)                            
047700     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
047800                    DELIMITED BY SIZE INTO SSA1                           
047900     MOVE 'WL457911 '    TO SSA2                                          
048000     MOVE '  '           TO GODK-STATUSKODER                              
048100     CALL CBLTDLI USING ISRT 4579-PCB 4580-IO-AREA SSA1 SSA2              
048200     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
048300     PERFORM IMS-STATUSKONTROLL                                           
048400     .                                                                    
048500                                                                          
048600 IMS-REPL-ATERSTART SECTION.                                              
048700     SKIP2                                                                
048800     MOVE '  '             TO GODK-STATUSKODER                            
048900     CALL CBLTDLI USING REPL 4579-PCB 4580-IO-AREA                        
049000     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
049100     PERFORM IMS-STATUSKONTROLL                                           
049200     .                                                                    
049300                                                                          
049400     EJECT                                                                
049410 IMS-GU-WDK601 SECTION.                                                   
049420                                                                          
049430     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
049440          DELIMITED BY SIZE INTO SSA1                                     
049460     MOVE '  GE'              TO GODK-STATUSKODER                         
049470     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
049480     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
049490     PERFORM IMS-STATUSKONTROLL                                           
049491     .                                                                    
049492                                                                          
049497     EJECT                                                                
049498 IMS-GNP-WDK611 SECTION.                                                  
049499                                                                          
049502     MOVE 'WDK611   '         TO SSA1                                     
049503     MOVE '  GE'              TO GODK-STATUSKODER                         
049504     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1               
049505     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
049506     PERFORM IMS-STATUSKONTROLL                                           
049507     .                                                                    
049508                                                                          
049509     EJECT                                                                
049510 IMS-GU-WDK611 SECTION.                                                   
049511                                                                          
049512     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
049513          DELIMITED BY SIZE INTO SSA1                                     
049514     MOVE 'WDK611   '         TO SSA2                                     
049515     MOVE '  GE'              TO GODK-STATUSKODER                         
049516     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2          
049517     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
049518     PERFORM IMS-STATUSKONTROLL                                           
049519     .                                                                    
049520                                                                          
049521     EJECT                                                                
049522 IMS-GU-WDK711 SECTION.                                                   
049523                                                                          
049524     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
049525          DELIMITED BY SIZE INTO SSA1                                     
049526     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
049527          DELIMITED BY SIZE INTO SSA2                                     
049528     MOVE '  GE'              TO GODK-STATUSKODER                         
049529     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
049530     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
049531     PERFORM IMS-STATUSKONTROLL                                           
049532     .                                                                    
049533                                                                          
049534     EJECT                                                                
049535 IMS-GU-WDD701 SECTION.                                                   
049536                                                                          
049537     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
049538          DELIMITED BY SIZE INTO SSA1                                     
049539     MOVE 'WDD701   '         TO SSA2                                     
049540     MOVE '  GE'              TO GODK-STATUSKODER                         
049541     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-AREA-WDD701 SSA1               
049542     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
049543     PERFORM IMS-STATUSKONTROLL                                           
049544     .                                                                    
049545                                                                          
049546     EJECT                                                                
049547 IMS-GNP-WDD702 SECTION.                                                  
049548                                                                          
049549     MOVE 'WDD702   '         TO SSA1                                     
049550     MOVE '    '              TO GODK-STATUSKODER                         
049551     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-AREA-WDD702 SSA1              
049552     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
049553     PERFORM IMS-STATUSKONTROLL                                           
049554     .                                                                    
049555                                                                          
049556 IMS-ISRT-WDQ101   SECTION.                                               
049557                                                                          
049558     MOVE 'WDQ101   ' TO SSA1                                             
049559     MOVE '  II' TO GODK-STATUSKODER                                      
049560     CALL CBLTDLI USING ISRT WDQ1-PCB DLI-IO-AREA-WDQ101 SSA1             
049561     MOVE WDQ1-STATUS-CODE TO STATUS-WS                                   
049562     PERFORM IMS-STATUSKONTROLL                                           
049563     .                                                                    
049564     EJECT                                                                
049570 IMS-STATUSKONTROLL SECTION.                                              
049600     SET STATUS-IX TO 1                                                   
049700     SEARCH GODK-STATUS                                                   
049800       AT END                                                             
049900         MOVE ' STATUSKOD FRÅN IMS EJ TILLÅTEN '                          
050000                            TO FELTEXT                                    
050100         CALL FELLOG                                                      
050200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
050300         CONTINUE                                                         
050400     END-SEARCH                                                           
050500     .                                                                    
