000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5706700.                                                
000301 AUTHOR.         ANDERS HENRIKSSON.                                       
000401 DATE-WRITTEN.   20120103.                                                
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER INFIL, MATCHAR DEN EMOT REGELVERKET OCH SKICKAR            
000900*        EN POST PER KLIENT.                                              
001000*        SKICKAR OCKSÅ EN FIL TILL LAGERVÄRDERINGSLISTOR                  
001100*        SKAPAR OCKSÅ EN FIL FÖR DIREKTLEVERANSLISTA                      
001200*                                                                         
001301*        PROGRAMMET LÄSER      WDH5                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INFIL MED RÄTTA POSTER                                     
002601     SELECT W57066                     ASSIGN TO W57067D1.                
002700     SKIP2                                                                
002800*          --- UTFIL TILL KLIENTER                                        
002905     SELECT W57067                     ASSIGN TO W57067D2.                
003300     SKIP2                                                                
003400*          --- UTFIL FÖR LAGERVÄRDERINGSLISTA                             
003505     SELECT W57069                     ASSIGN TO W57067D3.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000                                                                          
004101 FD  W57066                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004501*01  -COPY WDR801      -L.                                                
004600                                                                          
004701 FD  W57067                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005100 01  RATT-POST.                                                           
005201*    03   -COPY WDR801    -L.                                             
005300     03 FILLER                   PIC X(6).                                
006100                                                                          
006201 FD  W57069                                                               
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006600 01  UT-POST2.                                                            
006701*    03   -COPY W57069    -L.                                             
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
007000                                                                          
007101 77  IDPGM                       PIC X(8)    VALUE 'W5706700'.            
007200 77  JA                          PIC X       VALUE 'J'.                   
007300 77  NEJ                         PIC X       VALUE 'N'.                   
007310 77  W-KDTRADP                   PIC X(4)    VALUE SPACES.                
007320 77  WS-SAVE-KDTRADP             PIC X(4)    VALUE SPACES.                
007400                                                                          
007500 01  W-KLIENT                    PIC X       VALUE 'N'.                   
007600 01  SPAR-IDSYSMOT               PIC X(6)    VALUE SPACE.                 
007700                                                                          
007800                                                                          
007900 01  POST-SW                     PIC X      VALUE 'J'.                    
008000     88 POST-OK                             VALUE 'J'.                    
008100     88 POST-FEL                            VALUE 'N'.                    
008200                                                                          
008301 77  W57066-EOF-SW               PIC X       VALUE 'N'.                   
008401     88  END-OF-W57066                       VALUE 'J'.                   
008500     EJECT                                                                
008501                                                                          
008510 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
008520*01  -COPY WWIDFTG                                                        
008530     EJECT                                                                
008540                                                                          
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009200     SKIP2                                                                
009300*    --- PARAMETRAR TILL ABEND                                            
009400                                                                          
009500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009800     SKIP2                                                                
009900 01  FELTEXT.                                                             
010000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL POSTSUM                                          
010400*                                                                         
010500*01  -COPY W0005   -PRE  POSTSUM-                                         
010600     EJECT                                                                
010700 01  IN-AREA-START               PIC X(24)   VALUE                        
010800                                 'IN-AREA-START  '.                       
010900     SKIP2                                                                
011000                                                                          
011101*01  AREA -COPY WDR801     -PRE IN-                                       
011201*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR801-DATA               
011300     EJECT                                                                
011400 01  RATT-AREA-START             PIC X(24)   VALUE                        
011500                                 'RATT-AREA-START  '.                     
011600     SKIP2                                                                
011700                                                                          
011801*01  AREA -COPY WDR801     -PRE RATT-                                     
011901*    05   -COPY W510EKHA   -PRE RATT- -RED RATT-FIL-WDR801-DATA           
012000     05   RATT-EKH-IDSYSMOT      PIC X(6).                                
012500     EJECT                                                                
012600 01  DIR-AREA-START             PIC X(24)   VALUE                         
012700                                'DIR-AREA START '.                        
012801*01   -COPY W57069                                                        
012900     EJECT                                                                
013000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013100*                                                                         
013200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013300     SKIP3                                                                
013400 01  NYCKLAR-TILL-DLI.                                                    
013501     03  W-WDH501KY-X.                                                    
013601         05  W-IDFTG             PIC 9(2)        VALUE ZERO.              
013700         05  W-KDEKHHT           PIC X(3)        VALUE SPACE.             
013800     03  W-KDEKSHT-X.                                                     
013900         05  W-KDEKSHT           PIC X(3)        VALUE SPACE.             
014000     03  W-KDEKNIVA-X.                                                    
014100         05  W-KDEKNIVA          PIC X(5)        VALUE SPACE.             
014200     03  W-IDSYSMOT-X.                                                    
014300         05  W-IDSYSMOT          PIC X(6)        VALUE SPACE.             
014400     SKIP2                                                                
014500*    --- STATUS-KOD FRÅN IMS                                              
014600 01  STATUS-WS                   PIC XX.                                  
014700     88  SEGMENT-FINNS                       VALUE '  '.                  
014800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014910     88  SEGMENT-END                         VALUE 'GB'.                  
015000     SKIP2                                                                
015100 01  GODK-STATUSKODER.                                                    
015200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015300     SKIP3                                                                
015400 01  SSA1                        PIC X(64).                               
015500 01  SSA2                        PIC X(64).                               
015600 01  SSA3                        PIC X(64).                               
015700     EJECT                                                                
015800*    --- IMS FUNKTIONSKODER                                               
015900*01  -COPY W0003                                                          
016000     EJECT                                                                
016100*    ---  DLI INPUT-OUTPUT AREA                                           
016201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
016301 01  DLI-IO-WDH501.                                                       
016400*    03  -COPY WDH501                                                     
016500     EJECT                                                                
016601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
016701 01  DLI-IO-WDH511.                                                       
016800*    03  -COPY WDH511                                                     
016900     EJECT                                                                
017001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
017101 01  DLI-IO-WDH521.                                                       
017200*    03  -COPY WDH521                                                     
017300     EJECT                                                                
017401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH531'.                      
017501 01  DLI-IO-WDH531.                                                       
017600*    03  -COPY WDH531                                                     
017601     EJECT                                                                
017610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
017620 01  DLI-IO-WDB601.                                                       
017630*    03  -COPY WDB601                                                     
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900                                                                          
018000                                                                          
018101*01  -COPY W0008  -PRE WDH5-                                              
018200     05  FILLER                  PIC X.                                   
018300     EJECT                                                                
018400*01  -COPY W0008  -PRE WDB6-                                              
018401     05  FILLER                  PIC X.                                   
018402     EJECT                                                                
018403 PROCEDURE DIVISION USING WDH5-PCB WDB6-PCB.                              
018500                                                                          
018600 MAIN SECTION.                                                            
018701     ENTRY 'DLITCBL' USING WDH5-PCB WDB6-PCB.                             
018800                                                                          
018900                                                                          
019000     PERFORM A-INIT                                                       
019100                                                                          
019201     PERFORM S01-LAES-W57066                                              
019301     PERFORM UNTIL END-OF-W57066                                          
019400       PERFORM B-FLYTTA-DATA                                              
019500       PERFORM C-MATCHA-POST                                              
019601       PERFORM S01-LAES-W57066                                            
019700     END-PERFORM                                                          
019800                                                                          
019900                                                                          
020000     PERFORM Z-FINIT                                                      
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700                                                                          
020801     OPEN INPUT  W57066                                                   
020900                                                                          
021001     OPEN OUTPUT W57067                                                   
021201                 W57069                                                   
021300                                                                          
021400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021500     .                                                                    
021600     EJECT                                                                
021610                                                                          
021700 B-FLYTTA-DATA SECTION.                                                   
021722                                                                          
021723     IF IN-EKH-KDTRADP = WS-SAVE-KDTRADP                                  
021724       CONTINUE                                                           
021725     ELSE                                                                 
021726       MOVE IN-EKH-KDTRADP   TO W-KDTRADP                                 
021727                                WS-SAVE-KDTRADP                           
021751       PERFORM IMS-GU-WDB601-TRADP                                        
021752       IF SEGMENT-FINNS                                                   
021759         MOVE DCS-IDFTG      TO W-IDFTG                                   
021763       END-IF                                                             
021770     END-IF                                                               
021900     MOVE IN-EKH-KDEKHHT     TO W-KDEKHHT                                 
022000     MOVE IN-EKH-KDEKSHT     TO W-KDEKSHT                                 
022100     MOVE IN-EKH-KDEKNIVA    TO W-KDEKNIVA                                
022200     .                                                                    
022300     EJECT                                                                
022400 C-MATCHA-POST SECTION.                                                   
022500                                                                          
022600     MOVE 'NEJ' TO W-KLIENT                                               
022700     MOVE SPACE  TO SPAR-IDSYSMOT                                         
022801     PERFORM IMS-GET-WDH5-ALL                                             
022900     IF SEGMENT-FINNS                                                     
023001       PERFORM IMS-GNP-WDH531                                             
023100       IF SEGMENT-FINNS                                                   
023200         PERFORM UNTIL SEGMENT-SAKNAS                                     
023300         IF SYST-IDSYSMOT NOT  = SPACE                                    
023400           MOVE SYST-IDSYSMOT TO W-IDSYSMOT                               
023500           IF W-IDSYSMOT NOT   = SPAR-IDSYSMOT                            
023600             MOVE 'JA'        TO W-KLIENT                                 
023700             PERFORM D-SKAPA-RATTPOST                                     
023800             PERFORM F-SKAPA-DIR-POST                                     
024200             MOVE W-IDSYSMOT TO SPAR-IDSYSMOT                             
024300           END-IF                                                         
024400         END-IF                                                           
024501         PERFORM IMS-GNP-WDH531                                           
024600         END-PERFORM                                                      
024700       ELSE                                                               
024800         IF W-KDEKNIVA = 'MOMS' OR 'SUM'                                  
024900           PERFORM D-SKAPA-RATTPOST                                       
025000         END-IF                                                           
025100       END-IF                                                             
025200     END-IF                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 D-SKAPA-RATTPOST SECTION.                                                
025600                                                                          
025700     MOVE IN-AREA        TO RATT-AREA                                     
025800     MOVE W-IDSYSMOT     TO RATT-EKH-IDSYSMOT                             
025900                                                                          
025910     IF ((IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '121'           
025920         AND IN-EKH-KDEKNIVA    = 'DET')                                  
025930     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '122'            
025944         AND IN-EKH-KDEKNIVA    = 'DET'))                                 
025950       MOVE 'TDET'       TO RATT-EKH-KDEKNIVA                             
025960**** THIS IS TO ONLY GET THE DET POST AT THE TOP FOR NEXT PGM             
025980     END-IF                                                               
025990                                                                          
026000     IF ((IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'           
026002         AND IN-EKH-KDEKNIVA    = 'DDI'                                   
026003         AND IN-FIL-CT-IDSYSTEM = 'KR02')                                 
026004     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026005         AND IN-EKH-KDEKNIVA    = 'DDI'                                   
026006         AND IN-FIL-CT-IDSYSTEM = 'MY04')                                 
026010     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026011         AND IN-EKH-KDEKNIVA    = 'DDI'                                   
026012         AND IN-FIL-CT-IDSYSTEM = 'MX10')                                 
026013     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026014         AND IN-EKH-KDEKNIVA    = 'DDI'                                   
026015         AND IN-FIL-CT-IDSYSTEM = 'BR12')                                 
026013     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026014         AND IN-EKH-KDEKNIVA    = 'DDI'                                   
026015         AND IN-FIL-CT-IDSYSTEM = 'ZA04')                                 
026016     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026017         AND IN-EKH-KDEKNIVA    = 'DDI'                                   
026018         AND IN-FIL-CT-IDSYSTEM = 'TW01'))                                
026019       CONTINUE                                                           
026020     ELSE                                                                 
026021       PERFORM S11-SKRIV-W57067                                           
026022     END-IF                                                               
026023                                                                          
026024     IF ((IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'           
026025         AND IN-EKH-KDEKNIVA    = 'SUM'                                   
026026         AND IN-FIL-CT-IDSYSTEM = 'KR02')                                 
026027     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026028         AND IN-EKH-KDEKNIVA    = 'SUM'                                   
026029         AND IN-FIL-CT-IDSYSTEM = 'MY04')                                 
026033     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026034         AND IN-EKH-KDEKNIVA    = 'SUM'                                   
026035         AND IN-FIL-CT-IDSYSTEM = 'MX10')                                 
026036     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026037         AND IN-EKH-KDEKNIVA    = 'SUM'                                   
026038         AND IN-FIL-CT-IDSYSTEM = 'BR12')                                 
026036     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026037         AND IN-EKH-KDEKNIVA    = 'SUM'                                   
026038         AND IN-FIL-CT-IDSYSTEM = 'ZA04')                                 
026039     OR (IN-EKH-KDEKHHT     = '102' AND IN-EKH-KDEKSHT = '125'            
026040         AND IN-EKH-KDEKNIVA    = 'SUM'                                   
026041         AND IN-FIL-CT-IDSYSTEM = 'TW01'))                                
026042       MOVE 'DDI'        TO RATT-EKH-KDEKNIVA                             
026043       MOVE 0.01         TO RATT-EKH-SUBEL                                
026044**** THIS IS TO ONLY GET ONE DDI POST FOR 102-125 TO                      
026045**** W57078 TO BE CALCULATED                                              
026046       PERFORM S11-SKRIV-W57067                                           
026047     END-IF                                                               
026050                                                                          
026100     .                                                                    
029000     EJECT                                                                
029100 F-SKAPA-DIR-POST SECTION.                                                
029200                                                                          
029300                                                                          
029500     IF IN-EKH-FLLSBOK = 'N' AND IN-EKH-KDEKNIVA = 'DET'                  
029600     AND ((IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '301')             
029700     OR   (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '310')             
029800     OR   (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '311')             
029900     OR   (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '314')             
030000     OR   (IN-EKH-KDEKHHT = '303' AND IN-EKH-KDEKSHT = '351'))            
030100       MOVE IN-EKH-DAVERDAT  TO DIR-DAVERDAT                              
030200       MOVE IN-EKH-IDARTNR   TO DIR-IDARTNR                               
030300       MOVE IN-EKH-IDDC-SEND TO DIR-IDDC                                  
030400       MOVE IN-EKH-IDDISTR   TO DIR-IDDISTR                               
030500       MOVE IN-EKH-IDKUNDNR  TO DIR-IDKUNDNR                              
030600       MOVE IN-EKH-IDVERGL   TO DIR-IDVERGL                               
030700       MOVE IN-EKH-KDEKHHT   TO DIR-KDEKHHT                               
030800       MOVE IN-EKH-KDEKSHT   TO DIR-KDEKSHT                               
030900       MOVE IN-EKH-KDPRODSL  TO DIR-KDPRODSL                              
030910       MOVE IN-EKH-KDTRADP   TO DIR-KDTRADP                               
031000       MOVE IN-EKH-KVANTAL   TO DIR-KVANTAL                               
031100       MOVE IN-EKH-PRARTSTD  TO DIR-PRARTSTD                              
031200       MOVE IN-EKH-PRLANDCO  TO DIR-PRLANDCO                              
031300       MOVE IN-EKH-IDORDNR5  TO DIR-IDORDNR5                              
031401       PERFORM S13-SKRIV-W57069                                           
031500     END-IF                                                               
031600     .                                                                    
031700     EJECT                                                                
031800 Z-FINIT SECTION.                                                         
031900                                                                          
032001     CLOSE W57066                                                         
032101           W57067                                                         
032301           W57069                                                         
032400                                                                          
032500     MOVE 'S' TO POSTSUM-OPKOD                                            
032600     CALL POSTSUM USING POSTSUM-PARM                                      
032700     .                                                                    
032800     EJECT                                                                
032901 S01-LAES-W57066  SECTION.                                                
033000                                                                          
033101     READ W57066 INTO IN-AREA                                             
033200     AT END                                                               
033300        MOVE HIGH-VALUE TO IN-AREA                                        
033401        SET END-OF-W57066 TO TRUE                                         
033500                                                                          
033600     NOT AT END                                                           
033701        MOVE 'W57066'   TO POSTSUM-FDNAMN                                 
033801        MOVE 'W57067D1' TO POSTSUM-DDNAMN2                                
033900        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
034000        CALL POSTSUM USING POSTSUM-PARM                                   
034100     END-READ                                                             
034200     .                                                                    
034300     EJECT                                                                
034401 S11-SKRIV-W57067 SECTION.                                                
034500                                                                          
034600     WRITE RATT-POST FROM RATT-AREA                                       
034700                                                                          
034800     MOVE 'RATT'     TO POSTSUM-TRANSTYP                                  
034901     MOVE 'W57067'   TO POSTSUM-FDNAMN                                    
035001     MOVE 'W57067D2' TO POSTSUM-DDNAMN2                                   
035100     CALL POSTSUM USING POSTSUM-PARM                                      
035200     .                                                                    
036300     EJECT                                                                
036401 S13-SKRIV-W57069 SECTION.                                                
036500                                                                          
036601     WRITE UT-POST2 FROM DIR-W57069                                       
036700                                                                          
036800     MOVE 'DIR '     TO POSTSUM-TRANSTYP                                  
036901     MOVE 'W57069'   TO POSTSUM-FDNAMN                                    
037001     MOVE 'W57067D3' TO POSTSUM-DDNAMN2                                   
037100     CALL POSTSUM USING POSTSUM-PARM                                      
037200     .                                                                    
037300     EJECT                                                                
037400* --- IMS SEKTIONER ---                                                   
037500                                                                          
037601 IMS-GET-WDH5-ALL SECTION.                                                
037700                                                                          
037801     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
037900          DELIMITED BY SIZE INTO SSA1                                     
038001     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
038100          DELIMITED BY SIZE INTO SSA2                                     
038201     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
038300          DELIMITED BY SIZE INTO SSA3                                     
038400     MOVE '  GE' TO GODK-STATUSKODER                                      
038501     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH521 SSA1 SSA2 SSA3          
038601     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
038700     PERFORM IMS-STATUSKONTROLL                                           
038800     .                                                                    
038900     EJECT                                                                
039001 IMS-GNP-WDH531   SECTION.                                                
039100                                                                          
039201     STRING 'WDH531   '                                                   
039300          DELIMITED BY SIZE INTO SSA1                                     
039400     MOVE '  GE' TO GODK-STATUSKODER                                      
039501     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH531 SSA1                   
039601     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
039700     PERFORM IMS-STATUSKONTROLL                                           
039800     .                                                                    
039900     EJECT                                                                
040000                                                                          
040117 IMS-GU-WDB601-TRADP SECTION.                                             
040118     STRING 'WDB601  (KDTRADP  =' W-KDTRADP ')'                           
040119            DELIMITED BY SIZE INTO SSA1                                   
040120     MOVE '  GE'                 TO GODK-STATUSKODER                      
040121     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
040122     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
040123     PERFORM IMS-STATUSKONTROLL                                           
040124     .                                                                    
040125     SKIP3                                                                
040127                                                                          
040130 IMS-STATUSKONTROLL SECTION.                                              
040200                                                                          
040300     SET STATUS-IX TO 1                                                   
040400     SEARCH GODK-STATUS                                                   
040500       AT END                                                             
040600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040700           DELIMITED BY SIZE INTO FELTEXT                                 
040800         DISPLAY FELTEXT                                                  
040900         CALL FELLOG                                                      
041000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041100         CONTINUE                                                         
041200     END-SEARCH                                                           
041300     .                                                                    
