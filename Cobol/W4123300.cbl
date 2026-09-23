001100 ID DIVISION.                                                             
001200     SKIP2                                                                
001300 PROGRAM-ID.     W4123300.                                                
001400 AUTHOR.         GERRY CARMICHAEL.                                        
001500 DATE-WRITTEN.   91/06/17.                                                
001600                                                                          
001700     REMARKS.                                                             
001800*                                                                         
001900*    FUNKTION:                                                            
002000*        FUNKTION:  PROGRAMMET MATCHAR/PLOCKAR AV WDQ101 FÖR              
002100*        SLUTLEVERERADE ORDER OCH/ELLER NYA ORDER                         
002200*        SAMT SKAPAR PRINTFIL + RENSNINGSFIL                              
002300*                                                                         
002401*        PROGRAMMET UPPATERAR WLORQM (WDQ1)                               
002402*                             WLXXLU (CHECKPOINT)                         
002410*        PROGRAMMET LÄSER     WDE4 + WDE6                                 
002420*                             WLORQI (WDQ2)                               
002430*                             WLPROC (WDE8)                               
002500*                                                                         
002600*    ABENDKODER:                                                          
002700*        U0016 -  . . . .                                                 
002800*        U1000 -  . . . .                                                 
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003701     SKIP2                                                                
003702*          --- STYRFIL                                                    
003703     SELECT W41232                     ASSIGN TO W41233D1.                
003705     SKIP2                                                                
003706*          --- RENSNINGSFIL                                               
003707     SELECT W41233                     ASSIGN TO W41233D2.                
003708     SKIP2                                                                
003709*          --- PRINTFIL                                                   
003710     SELECT W41234                     ASSIGN TO W41233D3.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004301     SKIP3                                                                
004302 FD  W41232                                                               
004304     RECORDING       F                                                    
004305     BLOCK CONTAINS  0.                                                   
004306     SKIP2                                                                
004307*01  IN32-POST -COPY W412031     -L.                                      
004308     SKIP3                                                                
004309 FD  W41233                                                               
004310     RECORDING       F                                                    
004311     BLOCK CONTAINS  0.                                                   
004312     SKIP2                                                                
004313*01  UT33-POST -COPY W412031  -L.                                         
004314     SKIP3                                                                
004315 FD  W41234                                                               
004316     RECORDING       F                                                    
004317     BLOCK CONTAINS  0.                                                   
004318     SKIP2                                                                
004320*01  UT34-POST -COPY W412034  -L.                                         
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W4123300'.            
004800 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
004900 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
005000 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005100 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005200 77  CHKP-ANT                    PIC S9(7)   COMP-3.                      
005210*77  CHKP-ANT-MAX                PIC S9(7)   COMP-3 VALUE +99.            
005220 77  CHKP-ANT-MAX                PIC S9(7)   COMP-3 VALUE +50.            
005300 77  POST-ANT                    PIC S9(7)   COMP-3.                      
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005510 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP SYNC VALUE ZERO.        
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006101                                                                          
006102 77  W41232-EOF-SW               PIC X       VALUE 'N'.                   
006110     88  END-OF-W41232                       VALUE 'J'.                   
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007510     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007511     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
007520     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
007601     EJECT                                                                
007602*    ---- PARAMETRAR TILL DATKORT                                         
007603 01  FILLER                      PIC X(8)    VALUE 'DATKORT'.             
007604 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W41233'.              
007605 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007606*01  -COPY WDATKORT                                                       
007607     EJECT                                                                
007608*    --- PARAMETRAR TILL POSTSUM                                          
007609*                                                                         
007610*01  -COPY W0005   -PRE  POSTSUM-                                         
007901     EJECT                                                                
007902 01  IN32-AREA-START             PIC X(24)   VALUE                        
007903                                             'IN32-AREA-START'.           
007904     SKIP2                                                                
007905                                                                          
007906*01  AREA -COPY W412031    -PRE IN32-                                     
007907     EJECT                                                                
007908 01  UT33-AREA-START             PIC X(24)   VALUE                        
007909                                             'UT33-AREA-START'.           
007910     SKIP2                                                                
007911                                                                          
007912*01  AREA -COPY W412031    -PRE UT33-                                     
007913     EJECT                                                                
007914 01  UT34-AREA-START             PIC X(24)   VALUE                        
007915                                             'UT34-AREA-START'.           
007916     SKIP2                                                                
007917                                                                          
007920*01  AREA -COPY W412034    -PRE UT34-                                     
008000*                                                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI.                                                    
008500*    ---------TILL WDQ101                                                 
008501     03  W-WDQ101KY-MIN-X.                                                
008502         05  W-OBKR-IDORDER-MIN   PIC S9(7)   VALUE ZERO COMP-3.          
008503         05  W-OBKR-IDARTNR-MIN   PIC S9(9)   VALUE ZERO COMP-3.          
008504         05  W-OBKR-IDLOPNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.          
008505         05  W-OBKR-IDSEKVNR-MIN  PIC S9(3)   VALUE ZERO COMP-3.          
008506         05  W-OBKR-IDDC-MIN      PIC  X(2)   VALUE ZERO.                 
008507         05  W-OBKR-KDORDBEK-MIN  PIC 9(2)    VALUE ZERO.                 
008508                                                                          
008509     03  W-WDQ101KY-MAX-X.                                                
008510         05  W-OBKR-IDORDER-MAX   PIC S9(7)   VALUE ZERO COMP-3.          
008511         05  W-OBKR-IDARTNR-MAX   PIC S9(9)   VALUE ZERO COMP-3.          
008512         05  W-OBKR-IDLOPNR-MAX   PIC S9(3)   VALUE ZERO COMP-3.          
008513         05  W-OBKR-IDSEKVNR-MAX  PIC S9(3)   VALUE ZERO COMP-3.          
008514         05  W-OBKR-IDDC-MAX      PIC  X(2)   VALUE ZERO.                 
008515         05  W-OBKR-KDORDBEK-MAX  PIC 9(2)    VALUE ZERO.                 
008516                                                                          
008517     03  W-IDGMTREF-X.                                                    
008518         05  W-IDDISTR            PIC S9(5)   VALUE ZERO COMP-3.          
008519         05  W-IDKUNDNR           PIC S9(7)   VALUE ZERO COMP-3.          
008520         05  W-IDKUNDRF           PIC X(10)   VALUE SPACE.                
008521                                                                          
008522     03  W-IDSYSTEM-X.                                                    
008523         05  W-IDSYSTEM          PIC  X(4)    VALUE 'PROF'.               
008524                                                                          
008528     03  W-TIREGDAT-X.                                                    
008529         05  W-TIREGDAT          PIC  S9(7)   VALUE ZERO COMP-3.          
008530                                                                          
008531     03  W-IDPRODNR-X.                                                    
008532         05  W-IDPRODNR          PIC  S9(7)   VALUE ZERO COMP-3.          
008533                                                                          
008534*    ---------TILL WDE601 VIA WDE4ASEQ                                    
008535     03  W-WDE4ASEQ-X.                                                    
008536         05  W-SEQA-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.          
008537         05  W-SEQA-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.          
008538         05  W-SEQA-IDKUNDRF      PIC X(10)   VALUE SPACE.                
008539                                                                          
008540     03  W-IDHTYP-X.                                                      
008541         05  W-IDHTYP             PIC X(4)    VALUE SPACE.                
008550         05  NYCKEL-VALFRI        PIC X(26).                              
008551                                                                          
008552     03  W-IDORDER-X.                                                     
008553         05  W-IDORDER-WDQ2      PIC S9(7)   VALUE ZERO COMP-3.           
008560                                                                          
008561     03  W-WDE801KY-X.                                                    
008562         05  W-IDDISTR-WDE8      PIC S9(5)   VALUE ZERO COMP-3.           
008563         05  W-IDKUNDNR-WDE8     PIC S9(7)   VALUE ZERO COMP-3.           
008564         05  W-IDKUNDRF-WDE8     PIC  X(10)  VALUE SPACE.                 
008570                                                                          
008600     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     88  BASEN-SLUT                          VALUE 'GB'.                  
009300     88  IMS-EJ-OK                           VALUE 'XD'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(128).                              
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010100*    --- IMS FUNKTIONSKODER                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010700     SKIP3                                                                
010800 01  DLI-IO-AREA-ORQM.                                                    
010900     03  WLORQM01.                                                        
011000*        05  -COPY WDQ101                                                 
011200     EJECT                                                                
011210 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E401'.         
011300 01  DLI-IO-E401.                                                         
011500*    03  -COPY WDE401                                                     
011900     EJECT                                                                
011910 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
011920 01  DLI-IO-E601.                                                         
011930*    03  -COPY WDE601                                                     
011940     EJECT                                                                
011950 01  DLI-IO-AREA-XXLU.                                                    
011960     03  WLXXLU11.                                                        
011970*        05  -COPY WDGX4568                                               
011980     EJECT                                                                
011981 01  DLI-IO-AREA-PROC.                                                    
011985     03  WLPROC01.                                                        
011986*        05  -COPY WDE801                                                 
011988     EJECT                                                                
011989                                                                          
011990 01  DLI-IO-AREA-ORQI.                                                    
011991     03  WLORQI01.                                                        
011992*        05  -COPY WDQ201                                                 
011993     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100                                                                          
012200*01  -COPY W0009   -PRE MSG-                                              
012301     EJECT                                                                
012302*01  -COPY W0008  -PRE ORQM-                                              
012303     05  FILLER                  PIC X.                                   
012304     EJECT                                                                
012305*01  -COPY W0008  -PRE WDE4-                                              
012310     05  FILLER                  PIC X.                                   
012600     EJECT                                                                
012610*01  -COPY W0008  -PRE WDE6-                                              
012620     05  FILLER                  PIC X.                                   
012630     EJECT                                                                
012700*01  -COPY W0008  -PRE XXLU-                                              
012701     05  FILLER                  PIC X.                                   
012702     EJECT                                                                
012703*01  -COPY W0008  -PRE PROC-                                              
012704     05  FILLER                  PIC X.                                   
012705     EJECT                                                                
012706*01  -COPY W0008  -PRE ORQI-                                              
012707     05  FILLER                  PIC X.                                   
012708     EJECT                                                                
012709 PROCEDURE DIVISION  USING MSG-PCB ORQM-PCB WDE4-PCB WDE6-PCB             
012710                          XXLU-PCB PROC-PCB ORQI-PCB.                     
012720     ENTRY 'DLITCBL' USING MSG-PCB ORQM-PCB WDE4-PCB WDE6-PCB             
012721                          XXLU-PCB PROC-PCB ORQI-PCB.                     
012800                                                                          
013000     SKIP2                                                                
013100     PERFORM A-INIT                                                       
013110                                                                          
013200     PERFORM IMS-RESTART                                                  
013201                                                                          
013202     PERFORM IMS-LAS-ATERSTART                                            
013203                                                                          
013205     IF 4568-KVPOST > +0                                                  
013206       PERFORM B-LAS-FRAM-TILL-CHKPOINT                                   
013207     ELSE                                                                 
013210       PERFORM S01-LAES-W41232                                            
013211     END-IF                                                               
013220                                                                          
013300     PERFORM UNTIL END-OF-W41232                                          
013311                                                                          
013400       PERFORM C-BEHANDLA-INDATA                                          
014100                                                                          
014310       PERFORM S01-LAES-W41232                                            
014403                                                                          
014410     END-PERFORM                                                          
014500                                                                          
014600                                                                          
014700     PERFORM Z-FINIT                                                      
014800                                                                          
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300 A-INIT SECTION.                                                          
015400     SKIP2                                                                
015500                                                                          
015810     OPEN INPUT W41232                                                    
015901                                                                          
015902     OPEN OUTPUT W41233                                                   
015903                                                                          
015910                 W41234                                                   
016100                                                                          
016500                                                                          
016501                                                                          
016502     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016503                                                                          
016504     MOVE D-AAR              TO DAGENS-DATUM-AAR                          
016505     MOVE D-MAANAD           TO DAGENS-DATUM-MAANAD                       
016506     MOVE D-DAG              TO DAGENS-DATUM-DAG                          
016507                                                                          
016510     MOVE +0                   TO CHKP-ANT                                
016520                                  POST-ANT                                
016600     MOVE '4567'               TO W-IDHTYP                                
016601     MOVE LOW-VALUE            TO NYCKEL-VALFRI                           
016610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016800     .                                                                    
017000     EJECT                                                                
017001 B-LAS-FRAM-TILL-CHKPOINT SECTION.                                        
017005                                                                          
017006     PERFORM UNTIL END-OF-W41232 OR                                       
017007                       POST-ANT = 4568-KVPOST                             
017016        ADD +1           TO POST-ANT                                      
017017        PERFORM S01-LAES-W41232                                           
017020     END-PERFORM                                                          
017021                                                                          
017022     IF END-OF-W41232                                                     
017023        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
017024                      TO FELTEXT                                          
017025        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
017026     END-IF                                                               
017027     .                                                                    
017028     EJECT                                                                
017029                                                                          
017030 C-BEHANDLA-INDATA SECTION.                                               
017031                                                                          
017032     PERFORM CA-INIT-KEYVALUE                                             
017033                                                                          
017034     IF IN32-IDPTYP = '001'                                               
017035       PERFORM IMS-GET-ORQI-WDQ2                                          
017037       IF SEGMENT-FINNS                                                   
017038          PERFORM CB-LAES-BASER                                           
017039          PERFORM S06-SKRIV-REGISTERPOST                                  
017053       END-IF                                                             
017054     END-IF                                                               
017055                                                                          
017056     IF IN32-IDPTYP = '002'                                               
017057       PERFORM IMS-GET-PROC-WDE8                                          
017058       IF SEGMENT-FINNS                                                   
017059          PERFORM CC-BEHANDLA-PROFORMA-POST                               
017060          PERFORM S06-SKRIV-REGISTERPOST                                  
017068       END-IF                                                             
017069     END-IF                                                               
017070     .                                                                    
017071     EJECT                                                                
017072 CA-INIT-KEYVALUE SECTION.                                                
017073     ADD +1                  TO POST-ANT                                  
017074                                CHKP-ANT                                  
017075                                                                          
017076     IF CHKP-ANT > CHKP-ANT-MAX                                           
017077       PERFORM CAA-TAG-CHECKPOINT                                         
017078       MOVE +0               TO CHKP-ANT                                  
017079     END-IF                                                               
017080                                                                          
017081     MOVE LOW-VALUE          TO W-WDQ101KY-MIN-X                          
017082     MOVE HIGH-VALUE         TO W-WDQ101KY-MAX-X                          
017083     MOVE IN32-IDORDER       TO W-OBKR-IDORDER-MIN                        
017084                                W-OBKR-IDORDER-MAX                        
017085                                W-IDORDER-WDQ2                            
017086     MOVE IN32-IDDISTR       TO W-IDDISTR                                 
017087                                W-SEQA-IDDISTR                            
017088                                W-IDDISTR-WDE8                            
017089     MOVE IN32-IDKUNDNR      TO W-IDKUNDNR                                
017090                                W-SEQA-IDKUNDNR                           
017091                                W-IDKUNDNR-WDE8                           
017092     MOVE IN32-IDKUNDRF      TO W-IDKUNDRF                                
017093                                W-IDKUNDRF-WDE8                           
017094     MOVE IN32-IDKUNDRF (3:5) TO W-SEQA-IDKUNDRF                          
017095     MOVE IN32-TIREGDAT      TO W-TIREGDAT                                
017096     .                                                                    
017097     EJECT                                                                
017098 CAA-TAG-CHECKPOINT SECTION.                                              
017099* --- UPPDATERA ÅTERSTARTSREGISTRET                                       
017100     PERFORM IMS-LAS-ATERSTART                                            
017101     MOVE POST-ANT           TO 4568-KVPOST                               
017102     ACCEPT 4568-TIUPPDAT    FROM DATE                                    
017103     ACCEPT 4568-TIUPPTID    FROM TIME                                    
017104     PERFORM IMS-REPL-ATERSTART                                           
017105     PERFORM IMS-CHECKPOINT                                               
017106     .                                                                    
017107     EJECT                                                                
017108 CB-LAES-BASER SECTION.                                                   
017109                                                                          
017111     PERFORM IMS-GHU-ORQM-ORQM01                                          
017112     IF SEGMENT-FINNS                                                     
017113       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
017114         IF IN32-KDBEKALT = +0 OR                                         
017115            ((IN32-KDBEKALT = +1 OR +2) AND                               
017116            (IN32-KDORDKL = +2 OR +3 OR +4))                              
017117            PERFORM S03-KDBEKALT-012-POST                                 
017118         ELSE                                                             
017119           IF IN32-KDBEKALT = +3                                          
017120              PERFORM S02-KDBEKALT-3-POST                                 
017121           ELSE                                                           
017122             IF IN32-KDBEKALT = +4 OR +5                                  
017123                PERFORM S03-KDBEKALT-012-POST                             
017124                PERFORM S02-KDBEKALT-3-POST                               
017125             END-IF                                                       
017126           END-IF                                                         
017127         END-IF                                                           
017128         PERFORM IMS-GHN-ORQM-ORQM01                                      
017129       END-PERFORM                                                        
017130     ELSE                                                                 
017131       IF IN32-TIREGDAT = DAGENS-DATUM                                    
017132         IF ((IN32-KDBEKALT = +1 ) AND                                    
017133            (IN32-KDORDKL = +2 OR +3 OR +4))                              
017134            PERFORM S07-KDBEKALT-1-EJ-ORDBEK                              
017135         END-IF                                                           
017136       END-IF                                                             
017137     END-IF                                                               
017139     .                                                                    
017140     EJECT                                                                
017141 CC-BEHANDLA-PROFORMA-POST SECTION.                                       
017142                                                                          
017144     PERFORM IMS-GHU-ORQM-ORQM01-PROFORMA                                 
017145     IF SEGMENT-FINNS                                                     
017146       PERFORM UNTIL SEGMENT-SAKNAS                                       
017147         PERFORM S03-KDBEKALT-012-POST                                    
017148         PERFORM IMS-GHN-ORQM-ORQM01-PROFORMA                             
017149       END-PERFORM                                                        
017151     END-IF                                                               
017152     .                                                                    
017153     EJECT                                                                
017160 Z-FINIT SECTION.                                                         
017200                                                                          
017601                                                                          
017602     CLOSE W41232                                                         
017604                                                                          
017605           W41233                                                         
017606                                                                          
017610           W41234                                                         
017801     SKIP2                                                                
017802*--  NOLLA ÅTERSTARTSINFO                                                 
017803                                                                          
017804     PERFORM IMS-LAS-ATERSTART                                            
017805                                                                          
017806     MOVE +0             TO 4568-KVPOST                                   
017807     ACCEPT 4568-TIUPPDAT FROM DATE                                       
017808     ACCEPT 4568-TIUPPTID FROM TIME                                       
017809                                                                          
017810     PERFORM IMS-REPL-ATERSTART                                           
017811                                                                          
017812     MOVE 'S' TO POSTSUM-OPKOD                                            
017820     CALL POSTSUM USING POSTSUM-PARM                                      
017900     .                                                                    
018001     EJECT                                                                
018002 S01-LAES-W41232  SECTION.                                                
018003     SKIP2                                                                
018004     READ W41232 INTO IN32-AREA                                           
018005     AT END                                                               
018006        MOVE JA TO W41232-EOF-SW                                          
018009                                                                          
018010     NOT AT END                                                           
018011        MOVE 'W41232' TO POSTSUM-FDNAMN                                   
018012        MOVE 'W41233D1' TO POSTSUM-DDNAMN2                                
018013        MOVE IN32-IDPTYP TO POSTSUM-TRANSTYP                              
018014        CALL POSTSUM USING POSTSUM-PARM                                   
018015                                                                          
018017     END-READ                                                             
018020     .                                                                    
018101     EJECT                                                                
018136 S02-KDBEKALT-3-POST SECTION.                                             
018137                                                                          
018139     PERFORM IMS-GU-WDE401-ASEQ                                           
018143     IF SEGMENT-FINNS                                                     
018145       MOVE KORD-IDPRODNR TO W-IDPRODNR                                   
018146       PERFORM IMS-GU-WDE601                                              
018147       IF VORD-KDORDSTA < +3                                              
018148         CONTINUE                                                         
018149       ELSE                                                               
018153         PERFORM S03-KDBEKALT-012-POST                                    
018154       END-IF                                                             
018155     END-IF                                                               
018158     .                                                                    
018159     EJECT                                                                
018160 S03-KDBEKALT-012-POST SECTION.                                           
018162                                                                          
018163     IF OBKR-FLOBPRT = NEJ                                                
018165       PERFORM S05-SKAPA-PRINTPOST                                        
018166       MOVE JA               TO OBKR-FLOBPRT                              
018167       PERFORM IMS-REPL-ORQM                                              
018168     END-IF                                                               
018170     .                                                                    
018171     EJECT                                                                
018177 S05-SKAPA-PRINTPOST SECTION.                                             
018178                                                                          
018179     MOVE OBKR-IDDISTR       TO UT34-IDDISTR                              
018180     MOVE OBKR-IDKUNDNR      TO UT34-IDKUNDNR                             
018181     MOVE OBKR-IDKUNDRF      TO UT34-IDKUNDRF                             
018182     MOVE OBKR-IDORDER       TO UT34-IDORDER                              
018183     MOVE OBKR-IDARTNR       TO UT34-IDARTNR                              
018184     MOVE OBKR-IDDC          TO UT34-IDDC                                 
018185     MOVE OBKR-KDORDBEK      TO UT34-KDORDBEK                             
018186     MOVE OBKR-BEERS         TO UT34-BEERS                                
018187     MOVE OBKR-IDBIL         TO UT34-IDBIL                                
018188     MOVE OBKR-BEKUNDRF      TO UT34-BEKUNDRF                             
018189     MOVE OBKR-DIERS-KVOT    TO UT34-DIERS-KVOT                           
018190     MOVE OBKR-IDARTNR-TILLK TO UT34-IDARTNR-TILLK                        
018191     MOVE OBKR-IDKUNDRF-RO   TO UT34-IDKUNDRF-RO                          
018192     MOVE OBKR-KDTPOTYP      TO UT34-KDTPOTYP                             
018193     MOVE OBKR-KVANNANT      TO UT34-KVANNANT                             
018194     MOVE OBKR-KVBEART       TO UT34-KVBEART                              
018195     MOVE OBKR-KVBEART-Q     TO UT34-KVBEART-Q                            
018196     MOVE OBKR-KVBEART-TILLK TO UT34-KVBEART-TILLK                        
018197     MOVE OBKR-KVPRERO       TO UT34-KVPRERO                              
018198     MOVE OBKR-KVQPACK       TO UT34-KVQPACK-1                            
018199     MOVE OBKR-KVRO          TO UT34-KVRO                                 
018200     MOVE OBKR-REKSIFFR      TO UT34-REKSIFFR                             
018201     MOVE OBKR-REKSIFFR-TILLK TO UT34-REKSIFFR-TILLK                      
018202     IF OBKR-TIDISPIN NUMERIC                                             
018203       MOVE OBKR-TIDISPIN    TO UT34-TIDISPIN                             
018204     ELSE                                                                 
018205       MOVE ZERO             TO UT34-TIDISPIN                             
018206     END-IF                                                               
018207     MOVE OBKR-TITPO         TO UT34-TITPO                                
018208     MOVE OBKR-TIORDREG      TO UT34-TIORDREG                             
018209     MOVE IN32-IDPTYP        TO UT34-IDPTYP                               
018210     MOVE IN32-TIREGDAT      TO UT34-TIREGDAT                             
018211     MOVE IN32-KDORDKL       TO UT34-KDORDKL                              
018212     MOVE IN32-KDBEKALT      TO UT34-KDBEKALT                             
018213                                                                          
018214     WRITE UT34-POST FROM UT34-AREA                                       
018215                                                                          
018216     MOVE UT34-IDPTYP TO POSTSUM-TRANSTYP                                 
018217     MOVE 'W41234 ' TO POSTSUM-FDNAMN                                     
018218     MOVE 'W41233D3' TO POSTSUM-DDNAMN2                                   
018219     CALL POSTSUM USING POSTSUM-PARM                                      
018220     .                                                                    
018221     EJECT                                                                
018222 S06-SKRIV-REGISTERPOST  SECTION.                                         
018223                                                                          
018224     MOVE IN32-IDDISTR       TO UT33-IDDISTR                              
018225     MOVE IN32-IDKUNDNR      TO UT33-IDKUNDNR                             
018226     MOVE IN32-IDKUNDRF      TO UT33-IDKUNDRF                             
018227     MOVE IN32-IDORDER       TO UT33-IDORDER                              
018228     MOVE IN32-IDPTYP        TO UT33-IDPTYP                               
018229     MOVE IN32-TIREGDAT      TO UT33-TIREGDAT                             
018230     MOVE IN32-KDORDKL       TO UT33-KDORDKL                              
018232     MOVE IN32-KDBEKALT      TO UT33-KDBEKALT                             
018233                                                                          
018234     WRITE UT33-POST FROM UT33-AREA                                       
018235                                                                          
018236     MOVE UT33-IDPTYP TO POSTSUM-TRANSTYP                                 
018237     MOVE 'W41233 ' TO POSTSUM-FDNAMN                                     
018238     MOVE 'W41233D2' TO POSTSUM-DDNAMN2                                   
018239     CALL POSTSUM USING POSTSUM-PARM                                      
018241     .                                                                    
018242     EJECT                                                                
018250 S07-KDBEKALT-1-EJ-ORDBEK SECTION.                                        
018260                                                                          
018262                                                                          
018270     MOVE IN32-IDDISTR       TO UT34-IDDISTR                              
018280     MOVE IN32-IDKUNDNR      TO UT34-IDKUNDNR                             
018290     MOVE IN32-IDKUNDRF      TO UT34-IDKUNDRF                             
018300     MOVE IN32-IDORDER       TO UT34-IDORDER                              
018400     MOVE ZERO               TO UT34-IDARTNR                              
018500     MOVE SPACE              TO UT34-IDDC                                 
018600     MOVE ZERO               TO UT34-KDORDBEK                             
018700     MOVE SPACE              TO UT34-BEERS                                
018800     MOVE SPACE              TO UT34-BEKUNDRF                             
018900     MOVE ZERO               TO UT34-DIERS-KVOT                           
019000     MOVE ZERO               TO UT34-IDARTNR-TILLK                        
019100     MOVE SPACE              TO UT34-IDKUNDRF-RO                          
019300     MOVE ZERO               TO UT34-KDTPOTYP                             
019310     MOVE ZERO               TO UT34-KVANNANT                             
019320     MOVE ZERO               TO UT34-KVBEART                              
019330     MOVE ZERO               TO UT34-KVBEART-Q                            
019340     MOVE ZERO               TO UT34-KVBEART-TILLK                        
019350     MOVE ZERO               TO UT34-KVPRERO                              
019360     MOVE ZERO               TO UT34-KVQPACK-1                            
019370     MOVE ZERO               TO UT34-KVRO                                 
019380     MOVE ZERO               TO UT34-REKSIFFR                             
019390     MOVE ZERO               TO UT34-REKSIFFR-TILLK                       
019391     MOVE ZERO               TO UT34-TIDISPIN                             
019392     MOVE ZERO               TO UT34-TITPO                                
019393     MOVE ZERO               TO UT34-TIORDREG                             
019394     MOVE IN32-IDPTYP        TO UT34-IDPTYP                               
019396     MOVE IN32-TIREGDAT      TO UT34-TIREGDAT                             
019397     MOVE IN32-KDORDKL       TO UT34-KDORDKL                              
019398     MOVE IN32-KDBEKALT      TO UT34-KDBEKALT                             
019399                                                                          
019400     WRITE UT34-POST FROM UT34-AREA                                       
019401                                                                          
019402     MOVE UT34-IDPTYP TO POSTSUM-TRANSTYP                                 
019403     MOVE 'W41234 ' TO POSTSUM-FDNAMN                                     
019404     MOVE 'W41233D3' TO POSTSUM-DDNAMN2                                   
019405     CALL POSTSUM USING POSTSUM-PARM                                      
019408     .                                                                    
019409     EJECT                                                                
019410* --- IMS SEKTIONER ---                                                   
019500     SKIP3                                                                
019601     EJECT                                                                
019602 IMS-GHU-ORQM-ORQM01 SECTION.                                             
019603     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
019604                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
019605                    '&IDGMTREF =' W-IDGMTREF-X                            
019607                    '&IDSYSTEMNE' W-IDSYSTEM-X ')'                        
019608          DELIMITED BY SIZE INTO SSA1                                     
019609     MOVE '  GE' TO GODK-STATUSKODER                                      
019610     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
019611     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
019612     PERFORM IMS-STATUSKONTROLL                                           
019613     .                                                                    
019614     SKIP3                                                                
019615 IMS-GHN-ORQM-ORQM01 SECTION.                                             
019616     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
019617                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
019618                    '&IDGMTREF =' W-IDGMTREF-X                            
019620                    '&IDSYSTEMNE' W-IDSYSTEM-X ')'                        
019621          DELIMITED BY SIZE INTO SSA1                                     
019622     MOVE '  GBGE' TO GODK-STATUSKODER                                    
019623     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
019624     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
019625     PERFORM IMS-STATUSKONTROLL                                           
019626     .                                                                    
019627     SKIP3                                                                
019628 IMS-GHU-ORQM-ORQM01-PROFORMA  SECTION.                                   
019629     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
019630                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
019631                    '&IDGMTREF =' W-IDGMTREF-X                            
019633                    '&IDSYSTEM =' W-IDSYSTEM-X ')'                        
019634          DELIMITED BY SIZE INTO SSA1                                     
019635     MOVE '  GE' TO GODK-STATUSKODER                                      
019636     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
019637     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
019638     PERFORM IMS-STATUSKONTROLL                                           
019639     .                                                                    
019640     SKIP3                                                                
019641 IMS-GHN-ORQM-ORQM01-PROFORMA  SECTION.                                   
019642     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
019643                    '&WDQ101KY<=' W-WDQ101KY-MAX-X                        
019644                    '&IDGMTREF =' W-IDGMTREF-X                            
019646                    '&IDSYSTEM =' W-IDSYSTEM-X ')'                        
019647          DELIMITED BY SIZE INTO SSA1                                     
019648     MOVE '  GE' TO GODK-STATUSKODER                                      
019649     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
019650     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
019651     PERFORM IMS-STATUSKONTROLL                                           
019652     .                                                                    
019653     SKIP3                                                                
019654 IMS-REPL-ORQM SECTION.                                                   
019655                                                                          
019656     MOVE '  ' TO GODK-STATUSKODER                                        
019657     CALL CBLTDLI USING REPL ORQM-PCB DLI-IO-AREA-ORQM                    
019658     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
019659     PERFORM IMS-STATUSKONTROLL                                           
019660     .                                                                    
019670     EJECT                                                                
019710 IMS-GU-WDE401-ASEQ SECTION.                                              
019720     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
019730          DELIMITED BY SIZE INTO SSA1                                     
019740     MOVE '  GE' TO GODK-STATUSKODER                                      
019750     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
019760     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
019770     PERFORM IMS-STATUSKONTROLL                                           
019780     .                                                                    
019790                                                                          
019800 IMS-GU-WDE601 SECTION.                                                   
019801     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
019802          DELIMITED BY SIZE INTO SSA1                                     
019804     MOVE '    ' TO GODK-STATUSKODER                                      
019805     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
019806     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
019807     PERFORM IMS-STATUSKONTROLL                                           
019808     .                                                                    
019809     EJECT                                                                
019810 IMS-GET-ORQI-WDQ2 SECTION.                                               
019811     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
019812          DELIMITED BY SIZE INTO SSA1                                     
019813     MOVE '  GE' TO GODK-STATUSKODER                                      
019814     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-ORQI SSA1                 
019815     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
019816     PERFORM IMS-STATUSKONTROLL                                           
019817     .                                                                    
019818     EJECT                                                                
019819 IMS-GET-PROC-WDE8 SECTION.                                               
019820     STRING 'WLPROC01(WDE801KY =' W-WDE801KY-X ')'                        
019821          DELIMITED BY SIZE INTO SSA1                                     
019822     MOVE '  GE' TO GODK-STATUSKODER                                      
019823     CALL CBLTDLI USING GU PROC-PCB DLI-IO-AREA-PROC SSA1                 
019824     MOVE PROC-STATUS-CODE TO STATUS-WS                                   
019825     PERFORM IMS-STATUSKONTROLL                                           
019826     .                                                                    
019827     SKIP2                                                                
019830 IMS-RESTART SECTION.                                                     
019900     SKIP2                                                                
020000     MOVE SPACE TO MSG-IO-AREA                                            
020100     MOVE '  ' TO GODK-STATUSKODER                                        
020200     CALL CBLTDLI USING XRST MSG-PCB                                      
020300                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
020400                        CHKP-AREA-LENGTH CHKP-AREA                        
020500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020600     PERFORM IMS-STATUSKONTROLL                                           
020700     .                                                                    
020800     EJECT                                                                
020900 IMS-CHECKPOINT SECTION.                                                  
021000     SKIP2                                                                
021100     MOVE SPACE TO MSG-IO-AREA                                            
021200     MOVE '  XD' TO GODK-STATUSKODER                                      
021300     CALL CBLTDLI USING CHKP MSG-PCB                                      
021400                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
021500                        CHKP-AREA-LENGTH CHKP-AREA                        
021600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021700     PERFORM IMS-STATUSKONTROLL                                           
021800                                                                          
021900     IF IMS-EJ-OK                                                         
022000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022100       DISPLAY FELTEXT                                                    
022200       CALL FELLOG                                                        
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
026400 IMS-LAS-ATERSTART SECTION.                                               
026500     SKIP2                                                                
026600     MOVE '4567'         TO W-IDHTYP                                      
026700     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
026800     STRING 'WLXXLU01(WDGXKEY  =' W-IDHTYP-X ')'                          
026900                    DELIMITED BY SIZE INTO SSA1                           
027000     MOVE 'WLXXLU11 '    TO SSA2                                          
027100     MOVE '  '           TO GODK-STATUSKODER                              
027200     CALL CBLTDLI USING GHU XXLU-PCB DLI-IO-AREA-XXLU SSA1 SSA2           
027300     MOVE XXLU-STATUS-CODE TO STATUS-WS                                   
027400     PERFORM IMS-STATUSKONTROLL                                           
027500     .                                                                    
027600                                                                          
027700 IMS-REPL-ATERSTART SECTION.                                              
027800     SKIP2                                                                
027900     MOVE '  '             TO GODK-STATUSKODER                            
028000     CALL CBLTDLI USING REPL XXLU-PCB DLI-IO-AREA-XXLU                    
028100     MOVE XXLU-STATUS-CODE TO STATUS-WS                                   
028200     PERFORM IMS-STATUSKONTROLL                                           
028300     .                                                                    
028400                                                                          
028500 IMS-STATUSKONTROLL SECTION.                                              
028600     SKIP2                                                                
028700     SET STATUS-IX TO 1                                                   
028800     SEARCH GODK-STATUS                                                   
028900       AT END CALL FELLOG                                                 
029300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
029400     END-SEARCH                                                           
029500     .                                                                    
029600                                                                          
