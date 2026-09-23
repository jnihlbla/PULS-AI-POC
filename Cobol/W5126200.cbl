000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5126200.                                                
000300 AUTHOR.         PRERNA.                                                  
000400 DATE-WRITTEN.   17/12/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SB FOR WDL6                                                      
000900*        EXTRACT R30 FROM WDL6                                            
001000 ENVIRONMENT DIVISION.                                                    
001100 INPUT-OUTPUT SECTION.                                                    
001200 FILE-CONTROL.                                                            
001300     SKIP2                                                                
001400*          --- EXTRACT R30 FROM WDL6 FOR VCC                              
001500     SELECT UTFIL     ASSIGN     TO W51262D1.                             
001501*          --- EXTRACT R30 FROM WDL6 WITH IDLEVNR FOR NON VCC             
001502     SELECT UTFIL1    ASSIGN     TO W51262D2.                             
001510*          --- EXTRACT 310 FROM WDL6 WITH IDLEVNNR 1441 NON VCC           
001520     SELECT UTFIL2    ASSIGN     TO W51262D3.                             
001530*          --- EXTRACT R31 FROM WDL6 WITH IDLEVNR 1441 NON VCC            
001540     SELECT UTFIL3    ASSIGN     TO W51262D4.                             
001550*          --- EXTRACT R31 310 R30 FROM WDL6 AND NOT 1441                 
001560     SELECT UTFIL4    ASSIGN     TO W51262D5.                             
001570*          --- EXTRACT R30 310 R31 NOT 1441 FOR US BUT DISTR>9999         
001580     SELECT UTFIL5    ASSIGN     TO W51262D6.                             
001600     EJECT                                                                
001700 DATA DIVISION.                                                           
001800     SKIP2                                                                
001900 FILE SECTION.                                                            
002000     SKIP3                                                                
002100 FD  UTFIL                                                                
002200     RECORDING       F                                                    
002300     BLOCK CONTAINS  0.                                                   
002400                                                                          
002500*01  POST -COPY W51262 -PRE  UT-  -L.                                     
002600     EJECT                                                                
002610 FD  UTFIL1                                                               
002620     RECORDING       F                                                    
002630     BLOCK CONTAINS  0.                                                   
002640                                                                          
002650*01  POST -COPY W51280 -PRE  UT1- -L.                                     
002660     EJECT                                                                
002670 FD  UTFIL2                                                               
002680     RECORDING       F                                                    
002690     BLOCK CONTAINS  0.                                                   
002691                                                                          
002692*01  POST -COPY W51280 -PRE  UT2- -L.                                     
002693     EJECT                                                                
002694 FD  UTFIL3                                                               
002695     RECORDING       F                                                    
002696     BLOCK CONTAINS  0.                                                   
002697                                                                          
002698*01  POST -COPY W51280 -PRE  UT3- -L.                                     
002699     EJECT                                                                
002700 FD  UTFIL4                                                               
002701     RECORDING       F                                                    
002702     BLOCK CONTAINS  0.                                                   
002703                                                                          
002704*01  POST -COPY W51280 -PRE  UT4- -L.                                     
002705                                                                          
002706 FD  UTFIL5                                                               
002707     RECORDING       F                                                    
002708     BLOCK CONTAINS  0.                                                   
002709                                                                          
002710*01  POST -COPY W51280 -PRE  UT5- -L.                                     
002711     EJECT                                                                
002720 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900 77  IDPGM                       PIC X(8)    VALUE 'W5126200'.            
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003101 77  WS-KDTRADP                  PIC X(4)    VALUE SPACES.                
003110 77  WS-PRARTNTO                 PIC S9(7)V9(2) VALUE ZERO                
003120                                             COMP-3.                      
003130 77  WS-KVAVIS                   PIC S9(7) VALUE ZERO COMP-3.             
003140 77  DAGENS-AAMMDD               PIC 9(06).                               
003150 77  W-RETULF                    PIC S9(3)V9(4) VALUE ZERO                
003160                                             COMP-3.                      
003170 01  WS-DATUM                    PIC X(8).                                
003180 01  WS-DAINLEV                  PIC 9(16).                               
003190 01  WS-DATE-DIFF                PIC 9(8)    VALUE ZERO.                  
003200 01  WS-RUNDATUM-TO              PIC X(8)    VALUE SPACE.                 
003201 01  WS-RUNDATUM-FROM            PIC X(8)    VALUE SPACE.                 
003210 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
003220     EJECT                                                                
003300 01  DYNAMISKA-SUBPROGRAM.                                                
003400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
003710     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
003800                                                                          
003900 01  FELTEXT.                                                             
004000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004200     EJECT                                                                
004300 01  WZ20DAYS PIC X(8) VALUE 'WZ20DAYS'.                                  
004400     SKIP3                                                                
004500*    -COPY WZ20DAYS                                                       
004600     EJECT                                                                
004610                                                                          
004700*01  -COPY W0005   -PRE  POSTSUM-                                         
004800     EJECT                                                                
004820*01  -COPY WWDC99                                                         
004830     EJECT                                                                
004840*01  -COPY WWDIST07                                                       
004850     EJECT                                                                
004900 01  UT-AREA-START               PIC X(24)   VALUE                        
005000                                 'UT-AREA-START  '.                       
005100*01  AREA -COPY W51262     -PRE UT-                                       
005200     EJECT                                                                
005210 01  UT1-AREA-START              PIC X(24)   VALUE                        
005220                                 'UT1-AREA-START  '.                      
005230*01  AREA -COPY W51280     -PRE UT1-                                      
005240     EJECT                                                                
005250 01  UT2-AREA-START              PIC X(24)   VALUE                        
005260                                 'UT2-AREA-START  '.                      
005270*01  AREA -COPY W51280     -PRE UT2-                                      
005280     EJECT                                                                
005290 01  UT3-AREA-START              PIC X(24)   VALUE                        
005291                                 'UT3-AREA-START  '.                      
005292*01  AREA -COPY W51280     -PRE UT3-                                      
005293     EJECT                                                                
005294 01  UT4-AREA-START              PIC X(24)   VALUE                        
005295                                 'UT4-AREA-START  '.                      
005300*01  AREA -COPY W51280     -PRE UT4-                                      
005301     EJECT                                                                
005302 01  UT5-AREA-START              PIC X(24)   VALUE                        
005303                                 'UT5-AREA-START  '.                      
005304*01  AREA -COPY W51280     -PRE UT5-                                      
005305     EJECT                                                                
005310*                                                                         
005400*    --- STATUS-KOD FRÅN IMS                                              
005500 01  STATUS-WS                   PIC XX.                                  
005600     88  SEGMENT-FINNS                       VALUE '  '.                  
005700     88  BASEN-SLUT                          VALUE 'GB'.                  
005710     88  SEGMENT-FOUND                       VALUE '  '.                  
005720     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
005730     88  SEGMENT-MISSING                     VALUE 'GE'.                  
005740     SKIP2                                                                
005800                                                                          
005900 01  GODK-STATUSKODER.                                                    
006000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
006100     SKIP3                                                                
006200 01  SSA1                        PIC X(160).                              
006300     EJECT                                                                
006310 01  SSA2                        PIC X(160).                              
006320     EJECT                                                                
006330 01  SSA3                        PIC X(160).                              
006340     EJECT                                                                
006400*    --- IMS FUNKTIONSKODER                                               
006500*01  -COPY W0003                                                          
006600     EJECT                                                                
006610 01  NYCKLAR-TILL-DLI.                                                    
006620     03  W-IDARTNR-X.                                                     
006630         05  W-IDARTNR       PIC S9(9)   VALUE +0  COMP-3.                
006640     03  W-IDDC-X.                                                        
006650         05  W-IDDC          PIC X(2)    VALUE SPACE.                     
006651     03  W-IDLEVNR-X.                                                     
006652         05  W-IDLEVNR       PIC X(5)    VALUE SPACE.                     
006653     03  W-IDLAND-X.                                                      
006654         05  W-IDLAND        PIC X(2)    VALUE SPACE.                     
006660     EJECT                                                                
006700*    ---  DLI INPUT-OUTPUT AREA                                           
006800 01  FILLER         PIC X(16)    VALUE 'DLI-IO-WDL6'.                     
006900 01  DLI-IO-WDL6.                                                         
007000     03 IO-AREA     PIC X(600)   VALUE SPACE.                             
007100         03 DLI-IO-WDL601     REDEFINES IO-AREA.                          
007200*            05 -COPY WDL601                                              
007300     EJECT                                                                
007400         03 DLI-IO-WDL611     REDEFINES IO-AREA.                          
007500*            05 -COPY WDL611                                              
007530     EJECT                                                                
007540 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
007550 01  DLI-IO-WDK711.                                                       
007560*    03  -COPY WDK711                                                     
007570 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
007580 01  DLI-IO-WDB601.                                                       
007590*    03  -COPY WDB601                                                     
007591 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
007592 01  DLI-IO-WDF101.                                                       
007593*    03  -COPY WDF101                                                     
007594 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF102'.                      
007595 01  DLI-IO-WDF102.                                                       
007596*    03  -COPY WDF102 -PRE LEV-                                           
007597                                                                          
007598     EJECT                                                                
007600 LINKAGE SECTION.                                                         
007700                                                                          
007800*01  -COPY W0008  -PRE WDL6-                                              
007900     05  FILLER                  PIC X.                                   
007910*01  -COPY W0008  -PRE WDK7-                                              
007920     05  FILLER                  PIC X.                                   
007930*01  -COPY W0008  -PRE WDB6-                                              
007940     05  FILLER                  PIC X.                                   
007960*01  -COPY W0008  -PRE WDF1-                                              
007970     05  FILLER                  PIC X.                                   
008000     EJECT                                                                
008100 PROCEDURE DIVISION  USING WDL6-PCB WDK7-PCB WDB6-PCB WDF1-PCB.           
008200 MAIN SECTION.                                                            
008300     ENTRY 'DLITCBL' USING WDL6-PCB WDK7-PCB WDB6-PCB WDF1-PCB.           
008400                                                                          
008500     PERFORM A-INIT                                                       
008600                                                                          
008700     PERFORM IMS-GET-WDL6                                                 
008800     PERFORM UNTIL BASEN-SLUT                                             
008900       EVALUATE WDL6-SEG-NAME-FB                                          
009000         WHEN 'WDL601'                                                    
009100           MOVE ART-IDARTNR      TO UT-IDARTNR                            
009110                                    UT1-IDARTNR                           
009111                                    UT2-IDARTNR                           
009120                                    UT3-IDARTNR                           
009121                                    UT4-IDARTNR                           
009122                                    UT5-IDARTNR                           
009130                                    W-IDARTNR                             
009200         WHEN 'WDL611'                                                    
009201           MOVE INL-IDDISTR          TO DIST07-IDDISTR                    
009202           PERFORM S12-GET-KDTRADP                                        
009210           MOVE INL-IDDC         TO WS-IDDC                               
009220           IF XDC-NON-VCC-OWNED OR LDC-CN OR NDC-US                       
009231              PERFORM B-PROCESS-NON-VCC                                   
009240           ELSE                                                           
009300              PERFORM B-PROCESS-VCC                                       
009310           END-IF                                                         
009400       END-EVALUATE                                                       
009500       PERFORM IMS-GET-WDL6                                               
009600     END-PERFORM                                                          
009700                                                                          
009800     PERFORM Z-FINIT                                                      
009900     MOVE ZERO                   TO RETURN-CODE                           
010000     GOBACK                                                               
010100     .                                                                    
010200     EJECT                                                                
010300                                                                          
010400 A-INIT SECTION.                                                          
010410     MOVE FUNCTION CURRENT-DATE (3:6) TO DAGENS-AAMMDD                    
010420                                                                          
010500     OPEN OUTPUT UTFIL                                                    
010510                 UTFIL1                                                   
010520                 UTFIL2                                                   
010530                 UTFIL3                                                   
010540                 UTFIL4                                                   
010550                 UTFIL5                                                   
010600     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
010700     .                                                                    
010800     EJECT                                                                
010900                                                                          
010910 B-PROCESS-NON-VCC SECTION.                                               
010928     IF INL-IDPTYP = 'R30'                                                
010950       IF INL-IDLEVNR = '1441'                                            
010960         IF INL-TIINLMOT = ZERO                                           
010970           IF XDC-NON-VCC-OWNED OR LDC-CN                                 
010971             PERFORM BA-MOVE-UTFIL1                                       
010972           ELSE                                                           
010973             PERFORM BE-MOVE-UTFIL5                                       
010995           END-IF                                                         
010996         ELSE                                                             
010997           IF XDC-NON-VCC-OWNED OR LDC-CN                                 
010998**** EXCLUDE DISTRICTS GREATER THAN 9999                                  
010999**** THESE ARE NOT VALID DISTRICTS                                        
011000             IF INL-IDDISTR > 9999                                        
011001               PERFORM BE-MOVE-UTFIL5                                     
011002             ELSE                                                         
011004                 PERFORM BB-MOVE-UTFIL2                                   
011006             END-IF                                                       
011007           ELSE                                                           
011008             PERFORM BE-MOVE-UTFIL5                                       
011009           END-IF                                                         
011010         END-IF                                                           
011011       ELSE                                                               
011012         IF XDC-NON-VCC-OWNED OR LDC-CN                                   
011016           PERFORM BD-MOVE-UTFIL4                                         
011017         ELSE                                                             
011018           PERFORM BE-MOVE-UTFIL5                                         
011022         END-IF                                                           
011023       END-IF                                                             
011024     END-IF                                                               
011025                                                                          
011026     IF INL-IDPTYP = '310'                                                
011029       IF INL-IDLEVNR = '1441'                                            
011030         IF XDC-NON-VCC-OWNED OR LDC-CN                                   
011031           PERFORM BB-MOVE-UTFIL2                                         
011032         ELSE                                                             
011033           PERFORM BE-MOVE-UTFIL5                                         
011036         END-IF                                                           
011037       ELSE                                                               
011038         IF XDC-NON-VCC-OWNED OR LDC-CN                                   
011046           PERFORM BD-MOVE-UTFIL4                                         
011047         ELSE                                                             
011048           PERFORM BE-MOVE-UTFIL5                                         
011052         END-IF                                                           
011053       END-IF                                                             
011054     END-IF                                                               
011055                                                                          
011056     IF INL-IDPTYP = 'R31'                                                
011059       IF INL-IDLEVNR = '1441'                                            
011060         IF XDC-NON-VCC-OWNED OR LDC-CN                                   
011061           PERFORM BC-MOVE-UTFIL3                                         
011062         ELSE                                                             
011063           PERFORM BE-MOVE-UTFIL5                                         
011064         END-IF                                                           
011065       ELSE                                                               
011066         IF XDC-NON-VCC-OWNED OR LDC-CN                                   
011068           PERFORM BD-MOVE-UTFIL4                                         
011069         ELSE                                                             
011070           PERFORM BE-MOVE-UTFIL5                                         
011082         END-IF                                                           
011083       END-IF                                                             
011084     END-IF                                                               
011085                                                                          
011086     IF INL-IDPTYP = 'R40'                                                
011100       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                
011101       MOVE WS-CURRENT-DATE (1:8)     TO DAYS-TIDATE2                     
011102       MOVE 'YYYYMMDD'                TO DAYS-KDDATFMT2                   
011103       MOVE 25                        TO DAYS-KVDAYS                      
011104       COMPUTE WS-DAINLEV = 9999999999999999 - INL-DAINLEV                
011106       MOVE SPACE                     TO DAYS-TIDATE1                     
011107       MOVE ' '                       TO DAYS-IDCALEND                    
011108       MOVE 'YYYYMMDD'                TO DAYS-KDDATFMT1                   
011109       CALL WZ20DAYS USING                                                
011110            DAYS-WZ20DAYS                                                 
011111       IF DAYS-KDRC = ZERO                                                
011112         MOVE DAYS-TIDATE1            TO WS-RUNDATUM-FROM                 
011113         MOVE '01'                    TO WS-RUNDATUM-FROM(7:2)            
011114         MOVE WS-CURRENT-DATE         TO WS-RUNDATUM-TO                   
011115         MOVE '01'                    TO WS-RUNDATUM-TO(7:2)              
011116       END-IF                                                             
011123       IF  (WS-DAINLEV (1:8) =  WS-RUNDATUM-FROM                          
011124       OR  WS-DAINLEV (1:8) >  WS-RUNDATUM-FROM)                          
011125       AND WS-DAINLEV (1:8) <  WS-RUNDATUM-TO                             
011127         IF INL-IDLEVNR NOT = '1441'                                      
011128           IF XDC-NON-VCC-OWNED OR LDC-CN                                 
011130             PERFORM BD-MOVE-UTFIL4                                       
011131           ELSE                                                           
011132             PERFORM BE-MOVE-UTFIL5                                       
011133           END-IF                                                         
011134         END-IF                                                           
011135       END-IF                                                             
011136     END-IF                                                               
011137     .                                                                    
011138     EJECT                                                                
011139 BA-MOVE-UTFIL1 SECTION.                                                  
011140     MOVE INL-IDPTYP      TO UT1-IDPTYP                                   
011141     MOVE INL-IDDC        TO UT1-IDDC                                     
011142     MOVE INL-DAINLEV     TO UT1-DAINLEV                                  
011143     MOVE INL-PRARTNTO    TO UT1-PRARTNTO                                 
011144     MOVE INL-KVAVIS      TO UT1-KVAVIS                                   
011145     MOVE INL-KDVALISO    TO UT1-KDVALISO                                 
011146     MOVE WS-KDTRADP      TO UT1-KDTRADP                                  
011147     MOVE INL-IDLEVNR     TO UT1-IDLEVNR                                  
011148     MOVE INL-IDFAKT      TO UT1-IDFAKT                                   
011149     MOVE INL-IDLOPNRM    TO UT1-IDLOPNRM                                 
011150     MOVE ZERO            TO UT1-RETULF                                   
011151     PERFORM S11-SKRIV-UTFIL-1                                            
011152     .                                                                    
011153     EJECT                                                                
011154 BB-MOVE-UTFIL2 SECTION.                                                  
011155     MOVE INL-IDPTYP      TO UT2-IDPTYP                                   
011156     MOVE INL-IDDC        TO UT2-IDDC                                     
011157     MOVE INL-DAINLEV     TO UT2-DAINLEV                                  
011158     MOVE INL-PRARTNTO    TO UT2-PRARTNTO                                 
011159     MOVE INL-KVAVIS      TO UT2-KVAVIS                                   
011160     MOVE INL-KDVALISO    TO UT2-KDVALISO                                 
011161     MOVE WS-KDTRADP      TO UT2-KDTRADP                                  
011162     MOVE INL-IDLEVNR     TO UT2-IDLEVNR                                  
011163     MOVE INL-IDFAKT      TO UT2-IDFAKT                                   
011164     MOVE INL-IDLOPNRM    TO UT2-IDLOPNRM                                 
011165     MOVE ZERO            TO UT2-RETULF                                   
011166     PERFORM S11-SKRIV-UTFIL-2                                            
011167     .                                                                    
011168     EJECT                                                                
011169 BC-MOVE-UTFIL3 SECTION.                                                  
011170**** IF RETURNS TO NON VCC, TAKE AVERAGE COST                             
011171     IF DIST07-NON-VCC-OWNED                                              
011172       PERFORM S13-GET-AVG-COST                                           
011173       MOVE WS-PRARTNTO    TO UT3-PRARTNTO                                
011174     ELSE                                                                 
011175       MOVE INL-PRARTNTO   TO UT3-PRARTNTO                                
011176     END-IF                                                               
011177     MOVE INL-IDPTYP      TO UT3-IDPTYP                                   
011178     MOVE INL-IDDC        TO UT3-IDDC                                     
011179     MOVE INL-DAINLEV     TO UT3-DAINLEV                                  
011180     MOVE INL-KVAVIS      TO UT3-KVAVIS                                   
011181     MOVE INL-KDVALISO    TO UT3-KDVALISO                                 
011182     MOVE WS-KDTRADP      TO UT3-KDTRADP                                  
011183     MOVE INL-IDLEVNR     TO UT3-IDLEVNR                                  
011184     MOVE INL-IDFAKT      TO UT3-IDFAKT                                   
011185     MOVE INL-IDLOPNRM    TO UT3-IDLOPNRM                                 
011186     MOVE ZERO            TO UT3-RETULF                                   
011187     PERFORM S11-SKRIV-UTFIL-3                                            
011188     .                                                                    
011189     EJECT                                                                
011190 BD-MOVE-UTFIL4 SECTION.                                                  
011191     IF INL-IDPTYP = 'R31'                                                
011192       IF INL-KVANTMOT NOT = ZERO                                         
011193         COMPUTE WS-KVAVIS = (INL-KVAVIS - INL-KVANTMOT)                  
011194         MOVE WS-KVAVIS     TO UT4-KVAVIS                                 
011195         MOVE 'P32'         TO UT4-IDPTYP                                 
011196       ELSE                                                               
011197         MOVE INL-IDPTYP      TO UT4-IDPTYP                               
011198         MOVE INL-KVAVIS      TO UT4-KVAVIS                               
011199       END-IF                                                             
011200     ELSE                                                                 
011201       MOVE INL-IDPTYP      TO UT4-IDPTYP                                 
011202       MOVE INL-KVAVIS      TO UT4-KVAVIS                                 
011203     END-IF                                                               
011204     IF INL-PRARTNTO = 0                                                  
011205       PERFORM S13-GET-AVG-COST                                           
011206       MOVE WS-PRARTNTO     TO UT4-PRARTNTO                               
011207     ELSE                                                                 
011208       MOVE INL-PRARTNTO    TO UT4-PRARTNTO                               
011209     END-IF                                                               
011210     IF INL-IDPTYP = 'R40'                                                
011211       MOVE INL-KVRETUR TO UT4-KVAVIS                                     
011212     END-IF                                                               
011213     MOVE INL-IDDC          TO UT4-IDDC                                   
011214     MOVE INL-DAINLEV       TO UT4-DAINLEV                                
011215     MOVE INL-KDVALISO      TO UT4-KDVALISO                               
011216     MOVE WS-KDTRADP        TO UT4-KDTRADP                                
011217     MOVE INL-IDLEVNR       TO UT4-IDLEVNR                                
011218     MOVE INL-IDFAKT        TO UT4-IDFAKT                                 
011219     MOVE INL-IDLOPNRM      TO UT4-IDLOPNRM                               
011220     PERFORM BDA-PROCESS-FREIGHT-FACTOR                                   
011221     MOVE W-RETULF          TO UT4-RETULF                                 
011222     PERFORM S11-SKRIV-UTFIL-4                                            
011223     .                                                                    
011224     EJECT                                                                
011225 BE-MOVE-UTFIL5 SECTION.                                                  
011226     MOVE INL-IDPTYP      TO UT5-IDPTYP                                   
011227     MOVE INL-IDDC        TO UT5-IDDC                                     
011228     MOVE INL-DAINLEV     TO UT5-DAINLEV                                  
011229     MOVE INL-PRARTNTO    TO UT5-PRARTNTO                                 
011230     MOVE INL-KVAVIS      TO UT5-KVAVIS                                   
011231     MOVE INL-KDVALISO    TO UT5-KDVALISO                                 
011232     MOVE WS-KDTRADP      TO UT5-KDTRADP                                  
011233     MOVE INL-IDLEVNR     TO UT5-IDLEVNR                                  
011234     MOVE INL-IDFAKT      TO UT5-IDFAKT                                   
011235     MOVE INL-IDLOPNRM    TO UT5-IDLOPNRM                                 
011236     MOVE ZERO            TO UT5-RETULF                                   
011237     PERFORM S11-SKRIV-UTFIL-5                                            
011238     .                                                                    
011239     EJECT                                                                
011240 BDA-PROCESS-FREIGHT-FACTOR.                                              
011241                                                                          
011242     MOVE INL-IDLEVNR           TO W-IDLEVNR                              
011243     PERFORM IMS-GU-WDF101                                                
011244     IF SEGMENT-MISSING                                                   
011245       MOVE 1                    TO W-RETULF                              
011246     ELSE                                                                 
011247       MOVE DCS-IDLANDX2         TO W-IDLAND                              
011248       PERFORM IMS-GNP-WDF102                                             
011249       IF SEGMENT-FOUND                                                   
011250         IF LEV-TULL-TITULF < DAGENS-AAMMDD                               
011251           MOVE LEV-TULL-RETULF-1 TO W-RETULF                             
011252         ELSE                                                             
011253           MOVE LEV-TULL-RETULF-2 TO W-RETULF                             
011254         END-IF                                                           
011255       ELSE                                                               
011256         MOVE 1                   TO W-RETULF                             
011257       END-IF                                                             
011258     END-IF                                                               
011259     .                                                                    
011260     EJECT                                                                
011261 B-PROCESS-VCC SECTION.                                                   
011262     IF INL-IDPTYP = 'R30'                                                
011270       MOVE INL-IDDC             TO UT-IDDC                               
011300       MOVE INL-DAINLEV          TO UT-DAINLEV                            
011400       MOVE INL-PRARTNTO         TO UT-PRARTNTO                           
011500       MOVE INL-KVAVIS           TO UT-KVAVIS                             
011600       MOVE INL-KDVALISO         TO UT-KDVALISO                           
011700       MOVE INL-IDDISTR          TO UT-IDDISTR                            
011800       MOVE INL-IDLEVNR          TO UT-IDLEVNR                            
011900       MOVE INL-IDKUNDNR         TO UT-IDKUNDNR                           
012000       MOVE INL-IDORDNR5         TO UT-IDORDNR5                           
012100       MOVE INL-IDFAKT           TO UT-IDFAKT                             
012110       MOVE INL-KDFRAKT          TO UT-KDFRAKT                            
012200       MOVE INL-TIBERANK         TO UT-TIBERANK                           
015700       PERFORM S11-WRITE-UTFIL                                            
017500     END-IF                                                               
021100     .                                                                    
021200     EJECT                                                                
021300                                                                          
021400 Z-FINIT SECTION.                                                         
021500     CLOSE UTFIL                                                          
021600           UTFIL1                                                         
021700           UTFIL2                                                         
021710           UTFIL3                                                         
021720           UTFIL4                                                         
021730           UTFIL5                                                         
021800     MOVE 'S'                    TO POSTSUM-OPKOD                         
021900     CALL  POSTSUM            USING POSTSUM-PARM                          
022000     .                                                                    
022100     SKIP3                                                                
022200                                                                          
022300 S11-WRITE-UTFIL SECTION.                                                 
022400     WRITE UT-POST             FROM UT-AREA                               
022500     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
022600     MOVE 'UTFIL'                TO POSTSUM-FDNAMN                        
022700     MOVE 'W51262D1'             TO POSTSUM-DDNAMN2                       
022800     CALL POSTSUM             USING POSTSUM-PARM                          
022900     .                                                                    
023000     EJECT                                                                
024900                                                                          
024910 S11-SKRIV-UTFIL-1 SECTION.                                               
024920     WRITE UT1-POST FROM UT1-AREA                                         
024930     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
024940     MOVE 'UTFIL1'   TO POSTSUM-FDNAMN                                    
024950     MOVE 'W51262D2' TO POSTSUM-DDNAMN2                                   
024960     CALL POSTSUM USING POSTSUM-PARM                                      
024970     .                                                                    
024980     EJECT                                                                
024990                                                                          
024991 S11-SKRIV-UTFIL-2 SECTION.                                               
024992     WRITE UT2-POST FROM UT2-AREA                                         
024993     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
024994     MOVE 'UTFIL2'   TO POSTSUM-FDNAMN                                    
024995     MOVE 'W51262D3' TO POSTSUM-DDNAMN2                                   
024996     CALL POSTSUM USING POSTSUM-PARM                                      
024997     .                                                                    
024998     EJECT                                                                
024999                                                                          
025000 S11-SKRIV-UTFIL-3 SECTION.                                               
025001     WRITE UT3-POST FROM UT3-AREA                                         
025002     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
025003     MOVE 'UTFIL3'   TO POSTSUM-FDNAMN                                    
025004     MOVE 'W51262D4' TO POSTSUM-DDNAMN2                                   
025005     CALL POSTSUM USING POSTSUM-PARM                                      
025006     .                                                                    
025007     EJECT                                                                
025008 S11-SKRIV-UTFIL-4 SECTION.                                               
025009     WRITE UT4-POST FROM UT4-AREA                                         
025010     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
025011     MOVE 'UTFIL4'   TO POSTSUM-FDNAMN                                    
025012     MOVE 'W51262D5' TO POSTSUM-DDNAMN2                                   
025013     CALL POSTSUM USING POSTSUM-PARM                                      
025014     .                                                                    
025015     EJECT                                                                
025016 S11-SKRIV-UTFIL-5 SECTION.                                               
025017     WRITE UT5-POST FROM UT5-AREA                                         
025018     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
025019     MOVE 'UTFIL5'   TO POSTSUM-FDNAMN                                    
025020     MOVE 'W51262D6' TO POSTSUM-DDNAMN2                                   
025021     CALL POSTSUM USING POSTSUM-PARM                                      
025022     .                                                                    
025023     EJECT                                                                
025024 S12-GET-KDTRADP SECTION.                                                 
025025     MOVE INL-IDDC              TO W-IDDC                                 
025026     PERFORM IMS-GU-WDB601                                                
025027     IF SEGMENT-FINNS                                                     
025028       MOVE DCS-KDTRADP         TO WS-KDTRADP                             
025029     ELSE                                                                 
025030       MOVE SPACES              TO WS-KDTRADP                             
025031     END-IF                                                               
025032     .                                                                    
025033     EJECT                                                                
025034 S13-GET-AVG-COST SECTION.                                                
025035                                                                          
025036     MOVE INL-IDDC              TO W-IDDC                                 
025037     PERFORM IMS-GU-WDK711                                                
025038     IF SEGMENT-FINNS                                                     
025039       MOVE SLAG-PRAVCOST       TO WS-PRARTNTO                            
025040     ELSE                                                                 
025041       MOVE ZERO                TO WS-PRARTNTO                            
025042     END-IF                                                               
025043     .                                                                    
025044     EJECT                                                                
025050* --- IMS SECTION------                                                   
025100 IMS-GET-WDL6 SECTION.                                                    
025200     CALL CBLTDLI             USING GN WDL6-PCB DLI-IO-WDL6               
025300     MOVE WDL6-STATUS-CODE       TO STATUS-WS                             
025400     MOVE '  GAGKGB'             TO GODK-STATUSKODER                      
025500     PERFORM IMS-STATUSKONTROLL                                           
025600     .                                                                    
025700     SKIP3                                                                
025800                                                                          
025810 IMS-GU-WDK711 SECTION.                                                   
025820     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
025830                        DELIMITED  BY SIZE INTO SSA1                      
025840     STRING 'WDK711  (IDDC     =' W-IDDC      ')'                         
025850            DELIMITED BY SIZE INTO SSA2                                   
025860     MOVE '  GE'   TO GODK-STATUSKODER                                    
025870     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
025880     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
025890     PERFORM IMS-STATUSKONTROLL                                           
025891     .                                                                    
025892     SKIP3                                                                
025893 IMS-GU-WDB601 SECTION.                                                   
025894     STRING 'WDB601  (IDDC     =' W-IDDC      ')'                         
025895            DELIMITED BY SIZE INTO SSA1                                   
025896     MOVE '  GE'                 TO GODK-STATUSKODER                      
025897     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
025898     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
025899     PERFORM IMS-STATUSKONTROLL                                           
025900     .                                                                    
025901     SKIP3                                                                
025902                                                                          
025903 IMS-GU-WDF101   SECTION.                                                 
025904     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
025905             DELIMITED BY SIZE INTO SSA1                                  
025906     MOVE '  GE'                 TO GODK-STATUSKODER                      
025907     CALL CBLTDLI             USING GU                                    
025908                                    WDF1-PCB                              
025909                                    DLI-IO-WDF101                         
025910                                    SSA1                                  
025911     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
025912     PERFORM IMS-STATUSKONTROLL                                           
025913     .                                                                    
025914     SKIP3                                                                
025915                                                                          
025916 IMS-GNP-WDF102   SECTION.                                                
025917     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
025918             DELIMITED BY SIZE INTO SSA1                                  
025919     MOVE '  GE'                 TO GODK-STATUSKODER                      
025920     CALL CBLTDLI             USING GNP                                   
025921                                    WDF1-PCB                              
025922                                    DLI-IO-WDF102                         
025923                                    SSA1                                  
025924     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
025925     PERFORM IMS-STATUSKONTROLL                                           
025926     .                                                                    
025927     EJECT                                                                
025928                                                                          
025929 IMS-STATUSKONTROLL SECTION.                                              
026000     SET STATUS-IX TO 1                                                   
026100     SEARCH GODK-STATUS                                                   
026200       AT END                                                             
026300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026400           DELIMITED BY SIZE INTO FELTEXT                                 
026500         DISPLAY FELTEXT                                                  
026600         CALL FELLOG                                                      
026700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
026800         CONTINUE                                                         
026900     END-SEARCH                                                           
027000     .                                                                    
