000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5106000.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL.                                         
000400 DATE-WRITTEN.   98/04/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER IN ALLA EKONOMISKA HÄNDELSER (PEDAL) FRÅN DIV.             
001000*        FILER OCH KOMPLETTERAR DESSA MED UPPGIFTER FRÅN ART.REG          
001100*                                                                         
001200*        PROGRAMMET LÄSER       WDK6                                      
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- INFIL-EKONOMIFILERNA                                       
002200     SELECT W51060                     ASSIGN TO W51060D1.                
002300     SKIP2                                                                
002400*          --- KOMPL. UTFIL-EKONOMIFILERNA                                
002500     SELECT W51064                     ASSIGN TO W51060D2.                
002600     EJECT                                                                
002610*          --- KOMPL. UTFIL-EKONOMIFILERNA                                
002620     SELECT W51064A                    ASSIGN TO W51060D3.                
002630     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W51060                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY W51060      -L.                                                
003600                                                                          
003700     EJECT                                                                
003800 FD  W51064                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200 01  UT-POST.                                                             
004300*    03   -COPY W51060    -L.                                             
004400     EJECT                                                                
004401                                                                          
004410 FD  W51064A                                                              
004420     RECORDING       F                                                    
004430     BLOCK CONTAINS  0.                                                   
004440                                                                          
004450 01  UT2-POST.                                                            
004460*    03   -COPY W51060    -L.                                             
004470     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                       PIC X(8)    VALUE 'W5106000'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  W51060-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-W51060                       VALUE 'J'.                   
005300                                                                          
005400 01  W-IDANALYS                  PIC X(12).                               
005500 01  W-IDKST                     PIC X(10)   VALUE SPACE.                 
005600 01  W-IDKONTO                   PIC S9(11)  VALUE ZERO.                  
005700                                                                          
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006100                                                                          
006200 01  SPAR-FALT.                                                           
006300     03  W-SPAR-IDARTNR          PIC S9(9)    VALUE ZERO COMP-3.          
006400     03  W-SPAR-IDDC             PIC X(2)    VALUE SPACE.                 
006500     EJECT                                                                
006600                                                                          
006700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006800 01  FILLER REDEFINES DAGENS-DATUM.                                       
006900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007200     EJECT                                                                
007300 01  DYNAMISKA-SUBPROGRAM.                                                
007400*                                                                         
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300 01  IN-AREA-START               PIC X(24)   VALUE                        
008400                                             'IN-AREA-START'.             
008500     SKIP2                                                                
008600                                                                          
008700*    --- INAREA EKONOMIPOST                                               
008800*01  AREA -COPY W51060  -PRE IN-                                          
008900     EJECT                                                                
009000                                                                          
009100*    --- UTAREA EKONOMIPOST                                               
009200*01  AREA -COPY W51060  -PRE UT-                                          
009300     EJECT                                                                
009400                                                                          
009410*    --- UTAREA EKONOMIPOST                                               
009420*01  AREA -COPY W51060  -PRE UT2-                                         
009430     EJECT                                                                
009440                                                                          
009500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009600     SKIP3                                                                
009700 01  NYCKLAR-TILL-DLI.                                                    
009800     03  W-IDARTNR-X.                                                     
009900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010000     03  W-KDSEGKEY-X.                                                    
010100         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
010200     03  W-IDDC-X.                                                        
010300         05 W-IDDC               PIC X(2)        VALUE SPACE.             
010400     SKIP2                                                                
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011100     88  IMS-EJ-OK                           VALUE 'XD'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300                                                                          
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012500 01  DLI-IO-WDK601.                                                       
012600*    03 WDK601   -COPY WDK601                                             
012700     EJECT                                                                
012800                                                                          
012900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
013000 01  DLI-IO-WDK611.                                                       
013100*    03 WDK611   -COPY WDK611                                             
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500*01  -COPY W0008  -PRE WDK6-                                              
013600     05  FILLER                  PIC X.                                   
013700     EJECT                                                                
013800                                                                          
013900 PROCEDURE DIVISION  USING WDK6-PCB.                                      
014000 MAIN SECTION.                                                            
014100     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
014200                                                                          
014300     PERFORM A-INIT                                                       
014400                                                                          
014500     PERFORM S01-LAES-W51060                                              
014600     PERFORM UNTIL END-OF-W51060                                          
014700*---------------------------------------------*                           
014800* STRUKUR PROBLEM I FAKTURERINGEN             *                           
014900* DÄRFÖR DESSA FLYTT TILL W-FÄLT              *                           
015000* FAKURERINGEN KAN INTE HÅLLA NR I HUVUD DELEN*                           
015100* FUNGERAR SÅ LÄNGE SOM FAKTURERINGEN BARA HAR*                           
015200* ETT NR PÅ RADERNA                           *                           
015300*---------------------------------------------*                           
015400       IF IN-EKHT-KDEKHHT = '201' OR '203'                                
015500         IF IN-EKHT-KDEKNIVA = 'DET'                                      
015600           MOVE IN-EKHT-IDKONTO   TO W-IDKONTO                            
015700           MOVE IN-EKHT-IDANALYS  TO W-IDANALYS                           
015800           MOVE IN-EKHT-IDKST     TO W-IDKST                              
015900         END-IF                                                           
016000         IF IN-EKHT-KDEKNIVA = 'SUM'                                      
016100           MOVE W-IDKONTO         TO IN-EKHT-IDKONTO                      
016200           MOVE W-IDANALYS        TO IN-EKHT-IDANALYS                     
016300           MOVE W-IDKST           TO IN-EKHT-IDKST                        
016400         END-IF                                                           
016500                                                                          
016600       END-IF                                                             
016601**** DESSA KOMMER FRÅN EXPORT                                             
016610       IF IN-EKHT-IDCPYTXT = 'W570EKHA'                                   
016611       OR IN-EKHT-IDCPYTXT = 'W561EKHA'                                   
016612       OR IN-EKHT-IDCPYTXT = 'SEPVEKHA'                                   
016613         PERFORM C-BYGG-EKONOMIPOST-102                                   
016614         PERFORM S03-SKRIV-W51064A                                        
016620       ELSE                                                               
016621         PERFORM B-BYGG-EKONOMIPOST                                       
016622         PERFORM S02-SKRIV-W51064                                         
016630       END-IF                                                             
016900       PERFORM S01-LAES-W51060                                            
017000     END-PERFORM                                                          
017100                                                                          
017200     PERFORM Z-FINIT                                                      
017300                                                                          
017400     MOVE ZERO TO RETURN-CODE                                             
017500     GOBACK                                                               
017600     .                                                                    
017700     EJECT                                                                
017800                                                                          
017900 A-INIT SECTION.                                                          
018000     OPEN INPUT  W51060                                                   
018100     OPEN OUTPUT W51064                                                   
018110     OPEN OUTPUT W51064A                                                  
018200                                                                          
018300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018400     .                                                                    
018500     EJECT                                                                
018600 B-BYGG-EKONOMIPOST SECTION.                                              
018700                                                                          
018800     MOVE  IN-EKHT-IDARTNR   TO W-SPAR-IDARTNR                            
018900     MOVE  IN-EKHT-IDARTNR   TO W-IDARTNR                                 
019000     MOVE  IN-EKHT-IDDC-SEND TO W-IDDC                                    
019100                                                                          
019200     MOVE  IN-EKHT-IDPGM     TO UT-EKHT-IDPGM                             
019300     MOVE  IN-EKHT-DAREGDAT  TO UT-EKHT-DAREGDAT                          
019400     MOVE  IN-EKHT-TIKLOCK   TO UT-EKHT-TIKLOCK                           
019500     MOVE  IN-EKHT-IDSEKVNR  TO UT-EKHT-IDSEKVNR                          
019600     MOVE  'W510EKHA'        TO UT-EKHT-IDCPYTXT                          
019700     MOVE  IN-EKHT-BEVAT     TO UT-EKHT-BEVAT                             
019800     MOVE  IN-EKHT-DAVERDAT  TO UT-EKHT-DAVERDAT                          
019900     MOVE  IN-EKHT-FLLSBOK   TO UT-EKHT-FLLSBOK                           
020000     MOVE  IN-EKHT-IDANALYS  TO UT-EKHT-IDANALYS                          
020100     MOVE  IN-EKHT-IDARTNR   TO UT-EKHT-IDARTNR                           
020200     MOVE  IN-EKHT-IDDC-SEND TO UT-EKHT-IDDC-SEND                         
020300     MOVE  IN-EKHT-IDDC-REC  TO UT-EKHT-IDDC-REC                          
020400     MOVE  IN-EKHT-IDDISTR   TO UT-EKHT-IDDISTR                           
020500     MOVE  IN-EKHT-IDKONTO   TO UT-EKHT-IDKONTO                           
020600     MOVE  IN-EKHT-IDKST     TO UT-EKHT-IDKST                             
020700     MOVE  IN-EKHT-IDKUNDNR  TO UT-EKHT-IDKUNDNR                          
020800     MOVE  IN-EKHT-IDTRANS   TO UT-EKHT-IDTRANS                           
020900     MOVE  IN-EKHT-IDVERGL   TO UT-EKHT-IDVERGL                           
021000     MOVE  IN-EKHT-KDANMORS  TO UT-EKHT-KDANMORS                          
021100     MOVE  IN-EKHT-KDEKHHT   TO UT-EKHT-KDEKHHT                           
021200     MOVE  IN-EKHT-KDEKSHT   TO UT-EKHT-KDEKSHT                           
021300     MOVE  IN-EKHT-KDEKNIVA  TO UT-EKHT-KDEKNIVA                          
021400     MOVE  IN-EKHT-KDFRAKT   TO UT-EKHT-KDFRAKT                           
021500     MOVE  IN-EKHT-KDPRODSL  TO UT-EKHT-KDPRODSL                          
021600     MOVE  IN-EKHT-KDPSLLOC  TO UT-EKHT-KDPSLLOC                          
021700     MOVE  IN-EKHT-KDVALISO  TO UT-EKHT-KDVALISO                          
021800     MOVE  IN-EKHT-KVANTAL   TO UT-EKHT-KVANTAL                           
021900     MOVE  IN-EKHT-PRARTNTO  TO UT-EKHT-PRARTNTO                          
022000     MOVE  IN-EKHT-PRARTSJK  TO UT-EKHT-PRARTSJK                          
022100     MOVE  IN-EKHT-PRHEMTAG  TO UT-EKHT-PRHEMTAG                          
022200     MOVE  IN-EKHT-PRARTSTD  TO UT-EKHT-PRARTSTD                          
022300     MOVE  IN-EKHT-PRDIRLON  TO UT-EKHT-PRDIRLON                          
022400     MOVE  IN-EKHT-PRDMTRL   TO UT-EKHT-PRDMTRL                           
022500     MOVE  IN-EKHT-PRINK     TO UT-EKHT-PRINK                             
022600     MOVE  IN-EKHT-PRKURS    TO UT-EKHT-PRKURS                            
022700     MOVE  IN-EKHT-PRLANDCO  TO UT-EKHT-PRLANDCO                          
022800     MOVE  IN-EKHT-PROVRPAL  TO UT-EKHT-PROVRPAL                          
022900     MOVE  IN-EKHT-SUBEL     TO UT-EKHT-SUBEL                             
023000     MOVE  IN-EKHT-SUVAT     TO UT-EKHT-SUVAT                             
023100     MOVE  IN-EKHT-DAAVIDAT  TO UT-EKHT-DAAVIDAT                          
023200     MOVE  IN-EKHT-IDAVINR   TO UT-EKHT-IDAVINR                           
023300     MOVE  IN-EKHT-IDLEVNR   TO UT-EKHT-IDLEVNR                           
023400     MOVE  IN-EKHT-KDAVVTYP  TO UT-EKHT-KDAVVTYP                          
023500     MOVE  IN-EKHT-KDRT      TO UT-EKHT-KDRT                              
023600     MOVE  IN-EKHT-KVANTMOT  TO UT-EKHT-KVANTMOT                          
023700     MOVE  IN-EKHT-KVAVIS    TO UT-EKHT-KVAVIS                            
023800     MOVE  IN-EKHT-KDSORT    TO UT-EKHT-KDSORT                            
023900     MOVE  IN-EKHT-KDTRADP   TO UT-EKHT-KDTRADP                           
024000     MOVE  IN-EKHT-FLOVRLEV  TO UT-EKHT-FLOVRLEV                          
024100     IF IN-EKHT-FLDCET = JA                                               
024200       MOVE JA               TO UT-EKHT-FLDCET                            
024300     ELSE                                                                 
024400       MOVE NEJ              TO UT-EKHT-FLDCET                            
024500     END-IF                                                               
024600     MOVE IN-EKHT-IDKUNDRF   TO UT-EKHT-IDKUNDRF                          
024610     MOVE IN-EKHT-IDFAKT-EXP TO UT-EKHT-IDFAKT-EXP                        
024700     IF IN-EKHT-KDEKHHT = '203' AND IN-EKHT-KDEKNIVA = 'DET'              
024800       MOVE  IN-EKHT-IDORDNR5  TO UT-EKHT-IDORDNR5                        
024900     END-IF                                                               
025000                                                                          
025100     IF IN-EKHT-IDARTNR > 0                                               
025200       PERFORM IMS-GET-WDK601                                             
025300       IF SEGMENT-FINNS                                                   
025400         MOVE ART-KDPRODSL         TO UT-EKHT-KDPRODSL                    
025500         MOVE ART-KDSORT           TO UT-EKHT-KDSORT                      
025600         PERFORM IMS-GET-WDK611                                           
025700         IF SEGMENT-FINNS                                                 
025800           MOVE CLAG-KDPSLLOC      TO UT-EKHT-KDPSLLOC                    
025900           IF IN-EKHT-KDEKHHT = '201' OR '203' OR '404' OR '501'          
026000             MOVE CLAG-PRARTSTD    TO UT-EKHT-PRARTSTD                    
026100           END-IF                                                         
026200           IF IN-EKHT-KDEKHHT = '202' AND                                 
026300             (IN-EKHT-KDEKSHT = '201' OR '204')                           
026400             MOVE CLAG-PRARTSTD    TO UT-EKHT-PRARTSTD                    
026500           END-IF                                                         
026600           IF IN-EKHT-KDEKHHT = '204'                                     
026700             MOVE CLAG-PRARTSJK    TO UT-EKHT-PRARTSJK                    
026800             MOVE CLAG-PRINK       TO UT-EKHT-PRINK                       
026900             MOVE CLAG-PRHEMTAG    TO UT-EKHT-PRHEMTAG                    
027000             MOVE CLAG-PRARTSTD    TO UT-EKHT-PRARTSTD                    
027100             MOVE CLAG-PRDIRLON    TO UT-EKHT-PRDIRLON                    
027200             MOVE CLAG-PRDMTRL     TO UT-EKHT-PRDMTRL                     
027300             MOVE CLAG-PROVRPAL    TO UT-EKHT-PROVRPAL                    
027400           END-IF                                                         
027500           IF IN-EKHT-KDEKHHT = '303'                                     
027600             IF IN-EKHT-IDPGM = 'W4183000'                                
027700               MOVE CLAG-PRARTSTD  TO UT-EKHT-PRARTSTD                    
027800             ELSE                                                         
027900               MOVE CLAG-PRARTSJK  TO UT-EKHT-PRARTSJK                    
028000               MOVE CLAG-PRINK     TO UT-EKHT-PRINK                       
028100               MOVE CLAG-PRHEMTAG  TO UT-EKHT-PRHEMTAG                    
028200               MOVE CLAG-PRARTSTD  TO UT-EKHT-PRARTSTD                    
028300               MOVE CLAG-PRDIRLON  TO UT-EKHT-PRDIRLON                    
028400               MOVE CLAG-PRDMTRL   TO UT-EKHT-PRDMTRL                     
028500               MOVE CLAG-PROVRPAL  TO UT-EKHT-PROVRPAL                    
028600             END-IF                                                       
028700           END-IF                                                         
028800           IF IN-EKHT-KDEKHHT = '406' AND                                 
028900              IN-EKHT-KDEKSHT = '401'                                     
029000             MOVE CLAG-PRARTSTD    TO UT-EKHT-PRARTSTD                    
029100             MOVE CLAG-PRINK       TO UT-EKHT-PRINK                       
029200           END-IF                                                         
029300         END-IF                                                           
029400       END-IF                                                             
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800                                                                          
029810 C-BYGG-EKONOMIPOST-102 SECTION.                                          
029820                                                                          
029830     MOVE  IN-EKHT-IDARTNR   TO W-SPAR-IDARTNR                            
029840     MOVE  IN-EKHT-IDARTNR   TO W-IDARTNR                                 
029850     MOVE  IN-EKHT-IDDC-SEND TO W-IDDC                                    
029860                                                                          
029870     MOVE  IN-EKHT-IDPGM     TO UT2-EKHT-IDPGM                            
029880     MOVE  IN-EKHT-DAREGDAT  TO UT2-EKHT-DAREGDAT                         
029890     MOVE  IN-EKHT-TIKLOCK   TO UT2-EKHT-TIKLOCK                          
029891     MOVE  IN-EKHT-IDSEKVNR  TO UT2-EKHT-IDSEKVNR                         
029892     MOVE  IN-EKHT-IDCPYTXT  TO UT2-EKHT-IDCPYTXT                         
029893     MOVE  IN-EKHT-BEVAT     TO UT2-EKHT-BEVAT                            
029894     MOVE  IN-EKHT-DAVERDAT  TO UT2-EKHT-DAVERDAT                         
029895     MOVE  IN-EKHT-FLLSBOK   TO UT2-EKHT-FLLSBOK                          
029896     MOVE  IN-EKHT-IDANALYS  TO UT2-EKHT-IDANALYS                         
029897     MOVE  IN-EKHT-IDARTNR   TO UT2-EKHT-IDARTNR                          
029898     MOVE  IN-EKHT-IDDC-SEND TO UT2-EKHT-IDDC-SEND                        
029899     MOVE  IN-EKHT-IDDC-REC  TO UT2-EKHT-IDDC-REC                         
029900     MOVE  IN-EKHT-IDDISTR   TO UT2-EKHT-IDDISTR                          
029901     MOVE  IN-EKHT-IDKONTO   TO UT2-EKHT-IDKONTO                          
029902     MOVE  IN-EKHT-IDKST     TO UT2-EKHT-IDKST                            
029903     MOVE  IN-EKHT-IDKUNDNR  TO UT2-EKHT-IDKUNDNR                         
029904     MOVE  IN-EKHT-IDTRANS   TO UT2-EKHT-IDTRANS                          
029905     MOVE  IN-EKHT-IDVERGL   TO UT2-EKHT-IDVERGL                          
029906     MOVE  IN-EKHT-KDANMORS  TO UT2-EKHT-KDANMORS                         
029907     MOVE  IN-EKHT-KDEKHHT   TO UT2-EKHT-KDEKHHT                          
029908     MOVE  IN-EKHT-KDEKSHT   TO UT2-EKHT-KDEKSHT                          
029909     MOVE  IN-EKHT-KDEKNIVA  TO UT2-EKHT-KDEKNIVA                         
029910     MOVE  IN-EKHT-KDFRAKT   TO UT2-EKHT-KDFRAKT                          
029911     MOVE  IN-EKHT-KDPRODSL  TO UT2-EKHT-KDPRODSL                         
029912     MOVE  IN-EKHT-KDPSLLOC  TO UT2-EKHT-KDPSLLOC                         
029913     MOVE  IN-EKHT-KDVALISO  TO UT2-EKHT-KDVALISO                         
029914     MOVE  IN-EKHT-KVANTAL   TO UT2-EKHT-KVANTAL                          
029915     MOVE  IN-EKHT-PRARTNTO  TO UT2-EKHT-PRARTNTO                         
029916     MOVE  IN-EKHT-PRARTSJK  TO UT2-EKHT-PRARTSJK                         
029917     MOVE  IN-EKHT-PRHEMTAG  TO UT2-EKHT-PRHEMTAG                         
029918     MOVE  IN-EKHT-PRARTSTD  TO UT2-EKHT-PRARTSTD                         
029919     MOVE  IN-EKHT-PRDIRLON  TO UT2-EKHT-PRDIRLON                         
029920     MOVE  IN-EKHT-PRDMTRL   TO UT2-EKHT-PRDMTRL                          
029921     MOVE  IN-EKHT-PRINK     TO UT2-EKHT-PRINK                            
029922     MOVE  IN-EKHT-PRKURS    TO UT2-EKHT-PRKURS                           
029923     MOVE  IN-EKHT-PRLANDCO  TO UT2-EKHT-PRLANDCO                         
029924     MOVE  IN-EKHT-PROVRPAL  TO UT2-EKHT-PROVRPAL                         
029925     MOVE  IN-EKHT-SUBEL     TO UT2-EKHT-SUBEL                            
029926     MOVE  IN-EKHT-SUVAT     TO UT2-EKHT-SUVAT                            
029927     MOVE  IN-EKHT-DAAVIDAT  TO UT2-EKHT-DAAVIDAT                         
029928     MOVE  IN-EKHT-IDAVINR   TO UT2-EKHT-IDAVINR                          
029929     MOVE  IN-EKHT-IDLEVNR   TO UT2-EKHT-IDLEVNR                          
029930     MOVE  IN-EKHT-KDAVVTYP  TO UT2-EKHT-KDAVVTYP                         
029931     MOVE  IN-EKHT-KDRT      TO UT2-EKHT-KDRT                             
029932     MOVE  IN-EKHT-KVANTMOT  TO UT2-EKHT-KVANTMOT                         
029933     MOVE  IN-EKHT-KVAVIS    TO UT2-EKHT-KVAVIS                           
029934     MOVE  IN-EKHT-KDSORT    TO UT2-EKHT-KDSORT                           
029935     MOVE  IN-EKHT-KDTRADP   TO UT2-EKHT-KDTRADP                          
029936     MOVE  IN-EKHT-FLOVRLEV  TO UT2-EKHT-FLOVRLEV                         
029937     IF IN-EKHT-FLDCET = JA                                               
029938       MOVE JA               TO UT2-EKHT-FLDCET                           
029939     ELSE                                                                 
029940       MOVE NEJ              TO UT2-EKHT-FLDCET                           
029941     END-IF                                                               
029942     MOVE IN-EKHT-IDKUNDRF   TO UT2-EKHT-IDKUNDRF                         
029943     MOVE IN-EKHT-IDFAKT-EXP TO UT2-EKHT-IDFAKT-EXP                       
029944     IF IN-EKHT-KDEKHHT = '203' AND IN-EKHT-KDEKNIVA = 'DET'              
029945       MOVE  IN-EKHT-IDORDNR5  TO UT2-EKHT-IDORDNR5                       
029946     END-IF                                                               
029947                                                                          
029948     IF IN-EKHT-IDARTNR > 0                                               
029949       PERFORM IMS-GET-WDK601                                             
029950       IF SEGMENT-FINNS                                                   
029951         MOVE ART-KDPRODSL         TO UT2-EKHT-KDPRODSL                   
029952         MOVE ART-KDSORT           TO UT2-EKHT-KDSORT                     
029953         PERFORM IMS-GET-WDK611                                           
029954         IF SEGMENT-FINNS                                                 
029955           MOVE CLAG-KDPSLLOC      TO UT2-EKHT-KDPSLLOC                   
029956           IF IN-EKHT-KDEKHHT = '201' OR '203' OR '404' OR '501'          
029957             MOVE CLAG-PRARTSTD    TO UT2-EKHT-PRARTSTD                   
029958           END-IF                                                         
029959           IF IN-EKHT-KDEKHHT = '202' AND                                 
029960             (IN-EKHT-KDEKSHT = '201' OR '204')                           
029961             MOVE CLAG-PRARTSTD    TO UT2-EKHT-PRARTSTD                   
029962           END-IF                                                         
029963           IF IN-EKHT-KDEKHHT = '204'                                     
029964             MOVE CLAG-PRARTSJK    TO UT2-EKHT-PRARTSJK                   
029965             MOVE CLAG-PRINK       TO UT2-EKHT-PRINK                      
029966             MOVE CLAG-PRHEMTAG    TO UT2-EKHT-PRHEMTAG                   
029967             MOVE CLAG-PRARTSTD    TO UT2-EKHT-PRARTSTD                   
029968             MOVE CLAG-PRDIRLON    TO UT2-EKHT-PRDIRLON                   
029969             MOVE CLAG-PRDMTRL     TO UT2-EKHT-PRDMTRL                    
029970             MOVE CLAG-PROVRPAL    TO UT2-EKHT-PROVRPAL                   
029971           END-IF                                                         
029972           IF IN-EKHT-KDEKHHT = '303'                                     
029973             IF IN-EKHT-IDPGM = 'W4183000'                                
029974               MOVE CLAG-PRARTSTD  TO UT2-EKHT-PRARTSTD                   
029975             ELSE                                                         
029976               MOVE CLAG-PRARTSJK  TO UT2-EKHT-PRARTSJK                   
029977               MOVE CLAG-PRINK     TO UT2-EKHT-PRINK                      
029978               MOVE CLAG-PRHEMTAG  TO UT2-EKHT-PRHEMTAG                   
029979               MOVE CLAG-PRARTSTD  TO UT2-EKHT-PRARTSTD                   
029980               MOVE CLAG-PRDIRLON  TO UT2-EKHT-PRDIRLON                   
029981               MOVE CLAG-PRDMTRL   TO UT2-EKHT-PRDMTRL                    
029982               MOVE CLAG-PROVRPAL  TO UT2-EKHT-PROVRPAL                   
029983             END-IF                                                       
029984           END-IF                                                         
029985           IF IN-EKHT-KDEKHHT = '406' AND                                 
029986              IN-EKHT-KDEKSHT = '401'                                     
029987             MOVE CLAG-PRARTSTD    TO UT2-EKHT-PRARTSTD                   
029988             MOVE CLAG-PRINK       TO UT2-EKHT-PRINK                      
029989           END-IF                                                         
029990         END-IF                                                           
029991       END-IF                                                             
029992     END-IF                                                               
029993     .                                                                    
029994     EJECT                                                                
029995                                                                          
029996 Z-FINIT SECTION.                                                         
030000     CLOSE W51060                                                         
030100           W51064                                                         
030110           W51064A                                                        
030200                                                                          
030300     SKIP2                                                                
030400     MOVE 'S' TO POSTSUM-OPKOD                                            
030500     CALL POSTSUM USING POSTSUM-PARM                                      
030600     .                                                                    
030700     EJECT                                                                
030800                                                                          
030900 S01-LAES-W51060  SECTION.                                                
031000     READ W51060          INTO IN-AREA                                    
031100     AT END                                                               
031200        MOVE HIGH-VALUE   TO IN-AREA                                      
031300        SET END-OF-W51060 TO TRUE                                         
031400                                                                          
031500     NOT AT END                                                           
031600        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
031700        MOVE 'W51060'     TO POSTSUM-FDNAMN                               
031800        MOVE 'W51060D1'   TO POSTSUM-DDNAMN2                              
031900        CALL POSTSUM USING POSTSUM-PARM                                   
032000                                                                          
032100     END-READ                                                             
032200     .                                                                    
032300     EJECT                                                                
032400                                                                          
032500 S02-SKRIV-W51064 SECTION.                                                
032600     WRITE UT-POST   FROM UT-AREA                                         
032700                                                                          
032800     MOVE 'UT- '     TO POSTSUM-TRANSTYP                                  
032900     MOVE 'W51064'   TO POSTSUM-FDNAMN                                    
033000     MOVE 'W51060D2' TO POSTSUM-DDNAMN2                                   
033100     CALL POSTSUM USING POSTSUM-PARM                                      
033200     .                                                                    
033300     EJECT                                                                
033400                                                                          
033410 S03-SKRIV-W51064A SECTION.                                               
033420     WRITE UT2-POST   FROM UT2-AREA                                       
033430                                                                          
033440     MOVE 'UT- '     TO POSTSUM-TRANSTYP                                  
033450     MOVE 'W51064A'  TO POSTSUM-FDNAMN                                    
033460     MOVE 'W51060D3' TO POSTSUM-DDNAMN2                                   
033470     CALL POSTSUM USING POSTSUM-PARM                                      
033480     .                                                                    
033490     EJECT                                                                
033491                                                                          
033500 IMS-GET-WDK601 SECTION.                                                  
033600     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
033700          DELIMITED BY SIZE INTO SSA1                                     
033800     MOVE '  GE'           TO GODK-STATUSKODER                            
033900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
034000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
034100     PERFORM IMS-STATUSKONTROLL                                           
034200     .                                                                    
034300                                                                          
034400 IMS-GET-WDK611 SECTION.                                                  
034500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
034600          DELIMITED BY SIZE INTO SSA1                                     
034700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
034800          DELIMITED BY SIZE INTO SSA2                                     
034900     MOVE '  GE'           TO GODK-STATUSKODER                            
035000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
035100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
035200     PERFORM IMS-STATUSKONTROLL                                           
035300     .                                                                    
035400     EJECT                                                                
035500                                                                          
035600 IMS-STATUSKONTROLL SECTION.                                              
035700                                                                          
035800     SET STATUS-IX TO 1                                                   
035900     SEARCH GODK-STATUS                                                   
036000       AT END                                                             
036100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036200           DELIMITED BY SIZE INTO FELTEXT                                 
036300         DISPLAY FELTEXT                                                  
036400         CALL FELLOG                                                      
036500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036600         CONTINUE                                                         
036700     END-SEARCH                                                           
036800     .                                                                    
