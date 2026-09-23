000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5706000.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   20120103.                                                
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
002200     SELECT W57060                     ASSIGN TO W57060D1.                
002300     SKIP2                                                                
002400*          --- KOMPL. UTFIL-EKONOMIFILERNA                                
002500     SELECT W57061A                    ASSIGN TO W57060D2.                
002600     EJECT                                                                
002700*          --- KOMPL. UTFIL-EKONOMIFILERNA                                
002800     SELECT W57061C                    ASSIGN TO W57060D3.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W57060                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W57060      -L.                                                
003900                                                                          
004000     EJECT                                                                
004100 FD  W57061A                                                              
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500 01  UT-POST.                                                             
004600*    03   -COPY W57060    -L.                                             
004700     EJECT                                                                
004800 FD  W57061C                                                              
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200 01  UT2-POST.                                                            
005300*    03   -COPY W57060    -L.                                             
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W5706000'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  W57060-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W57060                       VALUE 'J'.                   
006200                                                                          
006300 01  W-IDANALYS                  PIC X(12).                               
006400 01  W-IDKST                     PIC X(10)   VALUE SPACE.                 
006500 01  W-IDKONTO                   PIC S9(11)  VALUE ZERO.                  
006600                                                                          
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007000                                                                          
007100 01  SPAR-FALT.                                                           
007200     03  W-SPAR-IDARTNR          PIC S9(9)    VALUE ZERO COMP-3.          
007300     03  W-SPAR-IDDC             PIC X(2)    VALUE SPACE.                 
007400     EJECT                                                                
007500                                                                          
007600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700 01  FILLER REDEFINES DAGENS-DATUM.                                       
007800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008100     EJECT                                                                
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300*                                                                         
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL POSTSUM                                          
008900*                                                                         
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200 01  IN-AREA-START               PIC X(24)   VALUE                        
009300                                             'IN-AREA-START'.             
009400     SKIP2                                                                
009500                                                                          
009600*    --- INAREA EKONOMIPOST                                               
009700*01  AREA -COPY W57060  -PRE IN-                                          
009800     EJECT                                                                
009900                                                                          
010000*    --- UTAREA EKONOMIPOST                                               
010100*01  AREA -COPY W57060  -PRE UT-                                          
010200     EJECT                                                                
010300                                                                          
010400*    --- UTAREA EKONOMIPOST                                               
010500*01  AREA -COPY W57060  -PRE UT2-                                         
010600     EJECT                                                                
010700                                                                          
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  NYCKLAR-TILL-DLI.                                                    
011100     03  W-IDARTNR-X.                                                     
011200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011300     03  W-KDSEGKEY-X.                                                    
011400         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
011500     03  W-IDDC-X.                                                        
011600         05 W-IDDC               PIC X(2)        VALUE SPACE.             
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012400     88  IMS-EJ-OK                           VALUE 'XD'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(64).                               
013000 01  SSA2                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600                                                                          
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
013800 01  DLI-IO-WDK601.                                                       
013900*    03 WDK601   -COPY WDK601                                             
014000     EJECT                                                                
014100                                                                          
014200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
014300 01  DLI-IO-WDK611.                                                       
014400*    03 WDK611   -COPY WDK611                                             
014500     EJECT                                                                
014600 LINKAGE SECTION.                                                         
014700                                                                          
014800*01  -COPY W0008  -PRE WDK6-                                              
014900     05  FILLER                  PIC X.                                   
015000     EJECT                                                                
015100                                                                          
015200 PROCEDURE DIVISION  USING WDK6-PCB.                                      
015300 MAIN SECTION.                                                            
015400     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
015500                                                                          
015600     PERFORM A-INIT                                                       
015700                                                                          
015800     PERFORM S01-LAES-W57060                                              
015900     PERFORM UNTIL END-OF-W57060                                          
016000*---------------------------------------------*                           
016100* STRUKUR PROBLEM I FAKTURERINGEN             *                           
016200* DÄRFÖR DESSA FLYTT TILL W-FÄLT              *                           
016300* FAKURERINGEN KAN INTE HÅLLA NR I HUVUD DELEN*                           
016400* FUNGERAR SÅ LÄNGE SOM FAKTURERINGEN BARA HAR*                           
016500* ETT NR PÅ RADERNA                           *                           
016600*---------------------------------------------*                           
016700       IF IN-EKHT-KDEKHHT = '201' OR '203'                                
016800         IF IN-EKHT-KDEKNIVA = 'DET'                                      
016900           MOVE IN-EKHT-IDKONTO   TO W-IDKONTO                            
017000           MOVE IN-EKHT-IDANALYS  TO W-IDANALYS                           
017100           MOVE IN-EKHT-IDKST     TO W-IDKST                              
017200         END-IF                                                           
017300         IF IN-EKHT-KDEKNIVA = 'SUM'                                      
017400           MOVE W-IDKONTO         TO IN-EKHT-IDKONTO                      
017500           MOVE W-IDANALYS        TO IN-EKHT-IDANALYS                     
017600           MOVE W-IDKST           TO IN-EKHT-IDKST                        
017700         END-IF                                                           
017800                                                                          
017900       END-IF                                                             
018100       IF IN-EKHT-IDCPYTXT(8:1) = 'B'                                     
018200         PERFORM B-BYGG-EKONOMIPOST-102                                   
018300         PERFORM S02-SKRIV-W57061C                                        
018400       ELSE                                                               
018500         PERFORM B-BYGG-EKONOMIPOST                                       
018600         PERFORM S02-SKRIV-W57061A                                        
018700       END-IF                                                             
018800       PERFORM S01-LAES-W57060                                            
018900     END-PERFORM                                                          
019000                                                                          
019100     PERFORM Z-FINIT                                                      
019200                                                                          
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700                                                                          
019800 A-INIT SECTION.                                                          
019900     OPEN INPUT  W57060                                                   
020000     OPEN OUTPUT W57061A                                                  
020100     OPEN OUTPUT W57061C                                                  
020200                                                                          
020300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020400     .                                                                    
020500     EJECT                                                                
020600 B-BYGG-EKONOMIPOST SECTION.                                              
020700                                                                          
020800     MOVE  IN-EKHT-IDARTNR   TO W-SPAR-IDARTNR                            
020900     MOVE  IN-EKHT-IDARTNR   TO W-IDARTNR                                 
021000     MOVE  IN-EKHT-IDDC-SEND TO W-IDDC                                    
021100                                                                          
021200     MOVE  IN-EKHT-IDPGM     TO UT-EKHT-IDPGM                             
021300     MOVE  IN-EKHT-TIREGDAT  TO UT-EKHT-TIREGDAT                          
021400     MOVE  IN-EKHT-TIKLOCK   TO UT-EKHT-TIKLOCK                           
021500     MOVE  IN-EKHT-IDSEKVNR  TO UT-EKHT-IDSEKVNR                          
021600     MOVE  IN-EKHT-IDCPYTXT  TO UT-EKHT-IDCPYTXT                          
021700     MOVE  IN-EKHT-BEVAT     TO UT-EKHT-BEVAT                             
021800     MOVE  IN-EKHT-DAVERDAT  TO UT-EKHT-DAVERDAT                          
021900     MOVE  IN-EKHT-FLLSBOK   TO UT-EKHT-FLLSBOK                           
022000     MOVE  IN-EKHT-IDANALYS  TO UT-EKHT-IDANALYS                          
022100     MOVE  IN-EKHT-IDARTNR   TO UT-EKHT-IDARTNR                           
022200     MOVE  IN-EKHT-IDDC-SEND TO UT-EKHT-IDDC-SEND                         
022300     MOVE  IN-EKHT-IDDC-REC  TO UT-EKHT-IDDC-REC                          
022400     MOVE  IN-EKHT-IDDISTR   TO UT-EKHT-IDDISTR                           
022500     MOVE  IN-EKHT-IDKONTO   TO UT-EKHT-IDKONTO                           
022600     MOVE  IN-EKHT-IDKST     TO UT-EKHT-IDKST                             
022700     MOVE  IN-EKHT-IDKUNDNR  TO UT-EKHT-IDKUNDNR                          
022800     MOVE  IN-EKHT-IDTRANS   TO UT-EKHT-IDTRANS                           
022900     MOVE  IN-EKHT-IDVERGL   TO UT-EKHT-IDVERGL                           
023000     MOVE  IN-EKHT-KDANMORS  TO UT-EKHT-KDANMORS                          
023100     MOVE  IN-EKHT-KDEKHHT   TO UT-EKHT-KDEKHHT                           
023200     MOVE  IN-EKHT-KDEKSHT   TO UT-EKHT-KDEKSHT                           
023300     MOVE  IN-EKHT-KDEKNIVA  TO UT-EKHT-KDEKNIVA                          
023400     MOVE  IN-EKHT-KDFRAKT   TO UT-EKHT-KDFRAKT                           
023500     MOVE  IN-EKHT-KDPRODSL  TO UT-EKHT-KDPRODSL                          
023600     MOVE  IN-EKHT-KDPSLLOC  TO UT-EKHT-KDPSLLOC                          
023700     MOVE  IN-EKHT-KDVALISO  TO UT-EKHT-KDVALISO                          
023800     MOVE  IN-EKHT-KVANTAL   TO UT-EKHT-KVANTAL                           
023900     MOVE  IN-EKHT-PRARTNTO  TO UT-EKHT-PRARTNTO                          
024000     MOVE  IN-EKHT-PRARTSJK  TO UT-EKHT-PRARTSJK                          
024100     MOVE  IN-EKHT-PRHEMTAG  TO UT-EKHT-PRHEMTAG                          
024200     MOVE  IN-EKHT-PRARTSTD  TO UT-EKHT-PRARTSTD                          
024300     MOVE  IN-EKHT-PRDIRLON  TO UT-EKHT-PRDIRLON                          
024400     MOVE  IN-EKHT-PRDMTRL   TO UT-EKHT-PRDMTRL                           
024500     MOVE  IN-EKHT-PRINK     TO UT-EKHT-PRINK                             
024600     MOVE  IN-EKHT-PRKURS    TO UT-EKHT-PRKURS                            
024700     MOVE  IN-EKHT-PRLANDCO  TO UT-EKHT-PRLANDCO                          
024800     MOVE  IN-EKHT-PROVRPAL  TO UT-EKHT-PROVRPAL                          
024900     MOVE  IN-EKHT-SUBEL     TO UT-EKHT-SUBEL                             
025000     MOVE  IN-EKHT-SUVAT     TO UT-EKHT-SUVAT                             
025100     MOVE  IN-EKHT-DAAVIDAT  TO UT-EKHT-DAAVIDAT                          
025200     MOVE  IN-EKHT-IDAVINR   TO UT-EKHT-IDAVINR                           
025300     MOVE  IN-EKHT-IDLEVNR   TO UT-EKHT-IDLEVNR                           
025400     MOVE  IN-EKHT-KDAVVTYP  TO UT-EKHT-KDAVVTYP                          
025500     MOVE  IN-EKHT-KDRT      TO UT-EKHT-KDRT                              
025600     MOVE  IN-EKHT-KVANTMOT  TO UT-EKHT-KVANTMOT                          
025700     MOVE  IN-EKHT-KVAVIS    TO UT-EKHT-KVAVIS                            
025800     MOVE  IN-EKHT-KDSORT    TO UT-EKHT-KDSORT                            
025900     MOVE  IN-EKHT-KDTRADP   TO UT-EKHT-KDTRADP                           
026000     IF IN-EKHT-FLOVRLEV > SPACE                                          
026100       MOVE  IN-EKHT-FLOVRLEV  TO UT-EKHT-FLOVRLEV                        
026200     ELSE                                                                 
026300       MOVE SPACE              TO UT-EKHT-FLOVRLEV                        
026400     END-IF                                                               
026500     IF IN-EKHT-FLDCET = JA                                               
026600       MOVE JA               TO UT-EKHT-FLDCET                            
026700     ELSE                                                                 
026800       MOVE NEJ              TO UT-EKHT-FLDCET                            
026900     END-IF                                                               
027000     MOVE IN-EKHT-IDKUNDRF   TO UT-EKHT-IDKUNDRF                          
027010     MOVE IN-EKHT-IDFAKT-EXP TO UT-EKHT-IDFAKT-EXP                        
027020     MOVE IN-EKHT-CMD        TO UT-EKHT-CMD                               
027100     IF IN-EKHT-KDEKHHT = '203' AND IN-EKHT-KDEKNIVA = 'DET'              
027200       MOVE  IN-EKHT-IDORDNR5  TO UT-EKHT-IDORDNR5                        
027300     END-IF                                                               
027400                                                                          
027500     IF IN-EKHT-IDARTNR > 0                                               
027600       PERFORM IMS-GET-WDK601                                             
027700       IF SEGMENT-FINNS                                                   
027800         IF  IN-EKHT-KDEKHHT = '402'                                      
027900         AND IN-EKHT-KDEKSHT = '401'                                      
028000           CONTINUE                                                       
028100         ELSE                                                             
028200           MOVE ART-KDPRODSL       TO UT-EKHT-KDPRODSL                    
028300         END-IF                                                           
028400         MOVE ART-KDSORT           TO UT-EKHT-KDSORT                      
028500         PERFORM IMS-GET-WDK611                                           
028600         IF SEGMENT-FINNS                                                 
028700           MOVE CLAG-KDPSLLOC      TO UT-EKHT-KDPSLLOC                    
028800         END-IF                                                           
028900       END-IF                                                             
029000     END-IF                                                               
029100     .                                                                    
029200     EJECT                                                                
029300                                                                          
029400 B-BYGG-EKONOMIPOST-102 SECTION.                                          
029500                                                                          
029600     MOVE  IN-EKHT-IDARTNR   TO W-SPAR-IDARTNR                            
029700     MOVE  IN-EKHT-IDARTNR   TO W-IDARTNR                                 
029800     MOVE  IN-EKHT-IDDC-SEND TO W-IDDC                                    
029900                                                                          
030000     MOVE  IN-EKHT-IDPGM     TO UT2-EKHT-IDPGM                            
030100     MOVE  IN-EKHT-TIREGDAT  TO UT2-EKHT-TIREGDAT                         
030200     MOVE  IN-EKHT-TIKLOCK   TO UT2-EKHT-TIKLOCK                          
030300     MOVE  IN-EKHT-IDSEKVNR  TO UT2-EKHT-IDSEKVNR                         
030400     MOVE  IN-EKHT-IDCPYTXT  TO UT2-EKHT-IDCPYTXT                         
030500     MOVE  IN-EKHT-BEVAT     TO UT2-EKHT-BEVAT                            
030600     MOVE  IN-EKHT-DAVERDAT  TO UT2-EKHT-DAVERDAT                         
030700     MOVE  IN-EKHT-FLLSBOK   TO UT2-EKHT-FLLSBOK                          
030800     MOVE  IN-EKHT-IDANALYS  TO UT2-EKHT-IDANALYS                         
030900     MOVE  IN-EKHT-IDARTNR   TO UT2-EKHT-IDARTNR                          
031000     MOVE  IN-EKHT-IDDC-SEND TO UT2-EKHT-IDDC-SEND                        
031100     MOVE  IN-EKHT-IDDC-REC  TO UT2-EKHT-IDDC-REC                         
031200     MOVE  IN-EKHT-IDDISTR   TO UT2-EKHT-IDDISTR                          
031300     MOVE  IN-EKHT-IDKONTO   TO UT2-EKHT-IDKONTO                          
031400     MOVE  IN-EKHT-IDKST     TO UT2-EKHT-IDKST                            
031500     MOVE  IN-EKHT-IDKUNDNR  TO UT2-EKHT-IDKUNDNR                         
031600     MOVE  IN-EKHT-IDTRANS   TO UT2-EKHT-IDTRANS                          
031700     MOVE  IN-EKHT-IDVERGL   TO UT2-EKHT-IDVERGL                          
031800     MOVE  IN-EKHT-KDANMORS  TO UT2-EKHT-KDANMORS                         
031900     MOVE  IN-EKHT-KDEKHHT   TO UT2-EKHT-KDEKHHT                          
032000     MOVE  IN-EKHT-KDEKSHT   TO UT2-EKHT-KDEKSHT                          
032100     MOVE  IN-EKHT-KDEKNIVA  TO UT2-EKHT-KDEKNIVA                         
032200     MOVE  IN-EKHT-KDFRAKT   TO UT2-EKHT-KDFRAKT                          
032300     MOVE  IN-EKHT-KDPRODSL  TO UT2-EKHT-KDPRODSL                         
032400     MOVE  IN-EKHT-KDPSLLOC  TO UT2-EKHT-KDPSLLOC                         
032500     MOVE  IN-EKHT-KDVALISO  TO UT2-EKHT-KDVALISO                         
032600     MOVE  IN-EKHT-KVANTAL   TO UT2-EKHT-KVANTAL                          
032700     MOVE  IN-EKHT-PRARTNTO  TO UT2-EKHT-PRARTNTO                         
032800     MOVE  IN-EKHT-PRARTSJK  TO UT2-EKHT-PRARTSJK                         
032900     MOVE  IN-EKHT-PRHEMTAG  TO UT2-EKHT-PRHEMTAG                         
033000     MOVE  IN-EKHT-PRARTSTD  TO UT2-EKHT-PRARTSTD                         
033100     MOVE  IN-EKHT-PRDIRLON  TO UT2-EKHT-PRDIRLON                         
033200     MOVE  IN-EKHT-PRDMTRL   TO UT2-EKHT-PRDMTRL                          
033300     MOVE  IN-EKHT-PRINK     TO UT2-EKHT-PRINK                            
033400     MOVE  IN-EKHT-PRKURS    TO UT2-EKHT-PRKURS                           
033500     MOVE  IN-EKHT-PRLANDCO  TO UT2-EKHT-PRLANDCO                         
033600     MOVE  IN-EKHT-PROVRPAL  TO UT2-EKHT-PROVRPAL                         
033700     MOVE  IN-EKHT-SUBEL     TO UT2-EKHT-SUBEL                            
033800     MOVE  IN-EKHT-SUVAT     TO UT2-EKHT-SUVAT                            
033900     MOVE  IN-EKHT-DAAVIDAT  TO UT2-EKHT-DAAVIDAT                         
034000     MOVE  IN-EKHT-IDAVINR   TO UT2-EKHT-IDAVINR                          
034100     MOVE  IN-EKHT-IDLEVNR   TO UT2-EKHT-IDLEVNR                          
034200     MOVE  IN-EKHT-KDAVVTYP  TO UT2-EKHT-KDAVVTYP                         
034300     MOVE  IN-EKHT-KDRT      TO UT2-EKHT-KDRT                             
034400     MOVE  IN-EKHT-KVANTMOT  TO UT2-EKHT-KVANTMOT                         
034500     MOVE  IN-EKHT-KVAVIS    TO UT2-EKHT-KVAVIS                           
034600     MOVE  IN-EKHT-KDSORT    TO UT2-EKHT-KDSORT                           
034700     MOVE  IN-EKHT-KDTRADP   TO UT2-EKHT-KDTRADP                          
034800     IF IN-EKHT-FLOVRLEV > SPACE                                          
034900       MOVE  IN-EKHT-FLOVRLEV  TO UT2-EKHT-FLOVRLEV                       
035000     ELSE                                                                 
035100       MOVE SPACE              TO UT2-EKHT-FLOVRLEV                       
035200     END-IF                                                               
035300     IF IN-EKHT-FLDCET = JA                                               
035400       MOVE JA               TO UT2-EKHT-FLDCET                           
035500     ELSE                                                                 
035600       MOVE NEJ              TO UT2-EKHT-FLDCET                           
035700     END-IF                                                               
035800     MOVE IN-EKHT-IDKUNDRF   TO UT2-EKHT-IDKUNDRF                         
035810     MOVE IN-EKHT-IDFAKT-EXP TO UT2-EKHT-IDFAKT-EXP                       
035820     MOVE IN-EKHT-CMD        TO UT-EKHT-CMD                               
035900     IF IN-EKHT-KDEKHHT = '203' AND IN-EKHT-KDEKNIVA = 'DET'              
036000       MOVE  IN-EKHT-IDORDNR5  TO UT2-EKHT-IDORDNR5                       
036100     END-IF                                                               
036200                                                                          
036300     IF IN-EKHT-IDARTNR > 0                                               
036400       PERFORM IMS-GET-WDK601                                             
036500       IF SEGMENT-FINNS                                                   
036600         IF  IN-EKHT-KDEKHHT = '402'                                      
036700         AND IN-EKHT-KDEKSHT = '401'                                      
036800           CONTINUE                                                       
036900         ELSE                                                             
037000           MOVE ART-KDPRODSL       TO UT2-EKHT-KDPRODSL                   
037100         END-IF                                                           
037200         MOVE ART-KDSORT           TO UT2-EKHT-KDSORT                     
037300         PERFORM IMS-GET-WDK611                                           
037400         IF SEGMENT-FINNS                                                 
037500           MOVE CLAG-KDPSLLOC      TO UT2-EKHT-KDPSLLOC                   
037600         END-IF                                                           
037700       END-IF                                                             
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100                                                                          
038200 Z-FINIT SECTION.                                                         
038300     CLOSE W57060                                                         
038400           W57061A                                                        
038500           W57061C                                                        
038600                                                                          
038700     SKIP2                                                                
038800     MOVE 'S' TO POSTSUM-OPKOD                                            
038900     CALL POSTSUM USING POSTSUM-PARM                                      
039000     .                                                                    
039100     EJECT                                                                
039200                                                                          
039300 S01-LAES-W57060  SECTION.                                                
039400     READ W57060          INTO IN-AREA                                    
039500     AT END                                                               
039600        MOVE HIGH-VALUE   TO IN-AREA                                      
039700        SET END-OF-W57060 TO TRUE                                         
039800                                                                          
039900     NOT AT END                                                           
040000        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
040100        MOVE 'W57060'     TO POSTSUM-FDNAMN                               
040200        MOVE 'W57060D1'   TO POSTSUM-DDNAMN2                              
040300        CALL POSTSUM USING POSTSUM-PARM                                   
040400                                                                          
040500     END-READ                                                             
040600     .                                                                    
040700     EJECT                                                                
040800                                                                          
040900 S02-SKRIV-W57061A SECTION.                                               
041000     WRITE UT-POST   FROM UT-AREA                                         
041100                                                                          
041200     MOVE 'UT- '     TO POSTSUM-TRANSTYP                                  
041300     MOVE 'W5706A'   TO POSTSUM-FDNAMN                                    
041400     MOVE 'W57060D2' TO POSTSUM-DDNAMN2                                   
041500     CALL POSTSUM USING POSTSUM-PARM                                      
041600     .                                                                    
041700     EJECT                                                                
041800                                                                          
041900 S02-SKRIV-W57061C SECTION.                                               
042000     WRITE UT2-POST   FROM UT2-AREA                                       
042100                                                                          
042200     MOVE 'UT2-'     TO POSTSUM-TRANSTYP                                  
042300     MOVE 'W5706C'   TO POSTSUM-FDNAMN                                    
042400     MOVE 'W57060D3' TO POSTSUM-DDNAMN2                                   
042500     CALL POSTSUM USING POSTSUM-PARM                                      
042600     .                                                                    
042700     EJECT                                                                
042800                                                                          
042900 IMS-GET-WDK601 SECTION.                                                  
043000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
043100          DELIMITED BY SIZE INTO SSA1                                     
043200     MOVE '  GE'           TO GODK-STATUSKODER                            
043300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
043400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     .                                                                    
043700                                                                          
043800 IMS-GET-WDK611 SECTION.                                                  
043900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
044000          DELIMITED BY SIZE INTO SSA1                                     
044100     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
044200          DELIMITED BY SIZE INTO SSA2                                     
044300     MOVE '  GE'           TO GODK-STATUSKODER                            
044400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
044500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000 IMS-STATUSKONTROLL SECTION.                                              
045100                                                                          
045200     SET STATUS-IX TO 1                                                   
045300     SEARCH GODK-STATUS                                                   
045400       AT END                                                             
045500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
045600           DELIMITED BY SIZE INTO FELTEXT                                 
045700         DISPLAY FELTEXT                                                  
045800         CALL FELLOG                                                      
045900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046000         CONTINUE                                                         
046100     END-SEARCH                                                           
046200     .                                                                    
