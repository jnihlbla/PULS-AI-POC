000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5156000.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   20170821.                                                
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
002200     SELECT W51560                     ASSIGN TO W51560D1.                
002300     SKIP2                                                                
002400*          --- KOMPL. UTFIL-EKONOMIFILERNA                                
002500     SELECT W51561A                    ASSIGN TO W51560D2.                
002600     EJECT                                                                
002610*          --- KOMPL. UTFIL-EKONOMIFILERNA                                
002620     SELECT W51561C                    ASSIGN TO W51560D3.                
002630     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W51560                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY W51560      -L.                                                
003600                                                                          
003700     EJECT                                                                
003800 FD  W51561A                                                              
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200 01  UT-POST.                                                             
004300*    03   -COPY W51560    -L.                                             
004400     EJECT                                                                
004410 FD  W51561C                                                              
004420     RECORDING       F                                                    
004430     BLOCK CONTAINS  0.                                                   
004440                                                                          
004450 01  UT2-POST.                                                            
004460*    03   -COPY W51560    -L.                                             
004470     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W5156000'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  W51560-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W51560                       VALUE 'J'.                   
005200                                                                          
005300 01  W-IDANALYS                  PIC X(12).                               
005400 01  W-IDKST                     PIC X(10)   VALUE SPACE.                 
005500 01  W-IDKONTO                   PIC S9(11)  VALUE ZERO.                  
005600                                                                          
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006000                                                                          
006100 01  SPAR-FALT.                                                           
006200     03  W-SPAR-IDARTNR          PIC S9(9)    VALUE ZERO COMP-3.          
006300     03  W-SPAR-IDDC             PIC X(2)    VALUE SPACE.                 
006400     EJECT                                                                
006500                                                                          
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                             'IN-AREA-START'.             
008400     SKIP2                                                                
008500                                                                          
008600*    --- INAREA EKONOMIPOST                                               
008700*01  AREA -COPY W51560  -PRE IN-                                          
008800     EJECT                                                                
008900                                                                          
009000*    --- UTAREA EKONOMIPOST                                               
009100*01  AREA -COPY W51560  -PRE UT-                                          
009200     EJECT                                                                
009201                                                                          
009210*    --- UTAREA EKONOMIPOST                                               
009220*01  AREA -COPY W51560  -PRE UT2-                                         
009230     EJECT                                                                
009300                                                                          
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500     SKIP3                                                                
009600 01  NYCKLAR-TILL-DLI.                                                    
009700     03  W-IDARTNR-X.                                                     
009800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009900     03  W-KDSEGKEY-X.                                                    
010000         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
010100     03  W-IDDC-X.                                                        
010200         05 W-IDDC               PIC X(2)        VALUE SPACE.             
010300     SKIP2                                                                
010400*    --- STATUS-KOD FRÅN IMS                                              
010500 01  STATUS-WS                   PIC XX.                                  
010600     88  SEGMENT-FINNS                       VALUE '  '.                  
010700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011000     88  IMS-EJ-OK                           VALUE 'XD'.                  
011100     SKIP2                                                                
011200 01  GODK-STATUSKODER.                                                    
011300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011400     SKIP3                                                                
011500 01  SSA1                        PIC X(64).                               
011600 01  SSA2                        PIC X(64).                               
011700     EJECT                                                                
011800*    --- IMS FUNKTIONSKODER                                               
011900*01  -COPY W0003                                                          
012000     EJECT                                                                
012100*    ---  DLI INPUT-OUTPUT AREA                                           
012200                                                                          
012300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
012400 01  DLI-IO-WDK601.                                                       
012500*    03 WDK601   -COPY WDK601                                             
012600     EJECT                                                                
012700                                                                          
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
012900 01  DLI-IO-WDK611.                                                       
013000*    03 WDK611   -COPY WDK611                                             
013100     EJECT                                                                
013200 LINKAGE SECTION.                                                         
013300                                                                          
013400*01  -COPY W0008  -PRE WDK6-                                              
013500     05  FILLER                  PIC X.                                   
013600     EJECT                                                                
013700                                                                          
013800 PROCEDURE DIVISION  USING WDK6-PCB.                                      
013900 MAIN SECTION.                                                            
014000     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
014100                                                                          
014200     PERFORM A-INIT                                                       
014300                                                                          
014400     PERFORM S01-LAES-W51560                                              
014500     PERFORM UNTIL END-OF-W51560                                          
014600*---------------------------------------------*                           
014700* STRUKTUR PROBLEM I FAKTURERINGEN            *                           
014800* DÄRFÖR FLYTTAS DESSA TILL W-FÄLT            *                           
014900* FAKURERINGEN KAN INTE HÅLLA NR I HUVUD DELEN*                           
015000* FUNGERAR SÅ LÄNGE SOM FAKTURERINGEN BARA HAR*                           
015100* ETT NR PÅ RADERNA                           *                           
015200*---------------------------------------------*                           
015300       IF IN-EKHT-KDEKHHT = '201' OR '203'                                
015400         IF IN-EKHT-KDEKNIVA = 'DET'                                      
015500           MOVE IN-EKHT-IDKONTO   TO W-IDKONTO                            
015600           MOVE IN-EKHT-IDANALYS  TO W-IDANALYS                           
015700           MOVE IN-EKHT-IDKST     TO W-IDKST                              
015800         END-IF                                                           
015900         IF IN-EKHT-KDEKNIVA = 'SUM'                                      
016000           MOVE W-IDKONTO         TO IN-EKHT-IDKONTO                      
016100           MOVE W-IDANALYS        TO IN-EKHT-IDANALYS                     
016200           MOVE W-IDKST           TO IN-EKHT-IDKST                        
016300         END-IF                                                           
016400                                                                          
016500       END-IF                                                             
016510       IF IN-EKHT-IDCPYTXT = 'W515EKHB'                                   
016600         PERFORM B-BYGG-EKONOMIPOST-102                                   
016700         PERFORM S02-SKRIV-W51561C                                        
016710       ELSE                                                               
016711         PERFORM B-BYGG-EKONOMIPOST                                       
016712         PERFORM S02-SKRIV-W51561A                                        
016720       END-IF                                                             
016800       PERFORM S01-LAES-W51560                                            
016900     END-PERFORM                                                          
017000                                                                          
017100     PERFORM Z-FINIT                                                      
017200                                                                          
017300     MOVE ZERO TO RETURN-CODE                                             
017400     GOBACK                                                               
017500     .                                                                    
017600     EJECT                                                                
017700                                                                          
017800 A-INIT SECTION.                                                          
017900     OPEN INPUT  W51560                                                   
018000     OPEN OUTPUT W51561A                                                  
018010     OPEN OUTPUT W51561C                                                  
018100                                                                          
018200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018300     .                                                                    
018400     EJECT                                                                
018500 B-BYGG-EKONOMIPOST SECTION.                                              
018600                                                                          
018700     MOVE  IN-EKHT-IDARTNR   TO W-SPAR-IDARTNR                            
018800     MOVE  IN-EKHT-IDARTNR   TO W-IDARTNR                                 
018900     MOVE  IN-EKHT-IDDC-SEND TO W-IDDC                                    
019000                                                                          
019100     MOVE  IN-EKHT-IDPGM     TO UT-EKHT-IDPGM                             
019200     MOVE  IN-EKHT-TIREGDAT  TO UT-EKHT-TIREGDAT                          
019300     MOVE  IN-EKHT-TIKLOCK   TO UT-EKHT-TIKLOCK                           
019400     MOVE  IN-EKHT-IDSEKVNR  TO UT-EKHT-IDSEKVNR                          
019500     MOVE  'W515EKHA'        TO UT-EKHT-IDCPYTXT                          
019600     MOVE  IN-EKHT-BEVAT     TO UT-EKHT-BEVAT                             
019700     MOVE  IN-EKHT-DAVERDAT  TO UT-EKHT-DAVERDAT                          
019800     MOVE  IN-EKHT-FLLSBOK   TO UT-EKHT-FLLSBOK                           
019900     MOVE  IN-EKHT-IDANALYS  TO UT-EKHT-IDANALYS                          
020000     MOVE  IN-EKHT-IDARTNR   TO UT-EKHT-IDARTNR                           
020100     MOVE  IN-EKHT-IDDC-SEND TO UT-EKHT-IDDC-SEND                         
020200     MOVE  IN-EKHT-IDDC-REC  TO UT-EKHT-IDDC-REC                          
020300     MOVE  IN-EKHT-IDDISTR   TO UT-EKHT-IDDISTR                           
020400     MOVE  IN-EKHT-IDKONTO   TO UT-EKHT-IDKONTO                           
020500     MOVE  IN-EKHT-IDKST     TO UT-EKHT-IDKST                             
020600     MOVE  IN-EKHT-IDKUNDNR  TO UT-EKHT-IDKUNDNR                          
020700     MOVE  IN-EKHT-IDTRANS   TO UT-EKHT-IDTRANS                           
020800     MOVE  IN-EKHT-IDVERGL   TO UT-EKHT-IDVERGL                           
020900     MOVE  IN-EKHT-KDANMORS  TO UT-EKHT-KDANMORS                          
021000     MOVE  IN-EKHT-KDEKHHT   TO UT-EKHT-KDEKHHT                           
021100     MOVE  IN-EKHT-KDEKSHT   TO UT-EKHT-KDEKSHT                           
021200     MOVE  IN-EKHT-KDEKNIVA  TO UT-EKHT-KDEKNIVA                          
021300     MOVE  IN-EKHT-KDFRAKT   TO UT-EKHT-KDFRAKT                           
021400     MOVE  IN-EKHT-KDPRODSL  TO UT-EKHT-KDPRODSL                          
021500     MOVE  IN-EKHT-KDPSLLOC  TO UT-EKHT-KDPSLLOC                          
021600     MOVE  IN-EKHT-KDVALISO  TO UT-EKHT-KDVALISO                          
021700     MOVE  IN-EKHT-KVANTAL   TO UT-EKHT-KVANTAL                           
021800     MOVE  IN-EKHT-PRARTNTO  TO UT-EKHT-PRARTNTO                          
021900     MOVE  IN-EKHT-PRARTSJK  TO UT-EKHT-PRARTSJK                          
022000     MOVE  IN-EKHT-PRHEMTAG  TO UT-EKHT-PRHEMTAG                          
022100     MOVE  IN-EKHT-PRARTSTD  TO UT-EKHT-PRARTSTD                          
022200     MOVE  IN-EKHT-PRDIRLON  TO UT-EKHT-PRDIRLON                          
022300     MOVE  IN-EKHT-PRDMTRL   TO UT-EKHT-PRDMTRL                           
022400     MOVE  IN-EKHT-PRINK     TO UT-EKHT-PRINK                             
022500     MOVE  IN-EKHT-PRKURS    TO UT-EKHT-PRKURS                            
022600     MOVE  IN-EKHT-PRLANDCO  TO UT-EKHT-PRLANDCO                          
022700     MOVE  IN-EKHT-PROVRPAL  TO UT-EKHT-PROVRPAL                          
022800     MOVE  IN-EKHT-SUBEL     TO UT-EKHT-SUBEL                             
022900     MOVE  IN-EKHT-SUVAT     TO UT-EKHT-SUVAT                             
023000     MOVE  IN-EKHT-DAAVIDAT  TO UT-EKHT-DAAVIDAT                          
023100     MOVE  IN-EKHT-IDAVINR   TO UT-EKHT-IDAVINR                           
023200     MOVE  IN-EKHT-IDLEVNR   TO UT-EKHT-IDLEVNR                           
023300     MOVE  IN-EKHT-KDAVVTYP  TO UT-EKHT-KDAVVTYP                          
023400     MOVE  IN-EKHT-KDRT      TO UT-EKHT-KDRT                              
023500     MOVE  IN-EKHT-KVANTMOT  TO UT-EKHT-KVANTMOT                          
023600     MOVE  IN-EKHT-KVAVIS    TO UT-EKHT-KVAVIS                            
023700     MOVE  IN-EKHT-KDSORT    TO UT-EKHT-KDSORT                            
023800     MOVE  IN-EKHT-KDTRADP   TO UT-EKHT-KDTRADP                           
023800     MOVE  IN-EKHT-IDFAKT-EXP                                             
023800                             TO UT-EKHT-IDFAKT-EXP                        
023900     IF IN-EKHT-FLOVRLEV > SPACE                                          
024000       MOVE  IN-EKHT-FLOVRLEV  TO UT-EKHT-FLOVRLEV                        
024010     ELSE                                                                 
024020       MOVE SPACE              TO UT-EKHT-FLOVRLEV                        
024030     END-IF                                                               
024100     IF IN-EKHT-FLDCET = JA                                               
024200       MOVE JA               TO UT-EKHT-FLDCET                            
024300     ELSE                                                                 
024400       MOVE NEJ              TO UT-EKHT-FLDCET                            
024500     END-IF                                                               
024520     MOVE IN-EKHT-IDKUNDRF   TO UT-EKHT-IDKUNDRF                          
024600     IF IN-EKHT-KDEKHHT = '203' AND IN-EKHT-KDEKNIVA = 'DET'              
024700       MOVE  IN-EKHT-IDORDNR5  TO UT-EKHT-IDORDNR5                        
024800     END-IF                                                               
024900                                                                          
025000     IF IN-EKHT-IDARTNR > 0                                               
025100       PERFORM IMS-GET-WDK601                                             
025200       IF SEGMENT-FINNS                                                   
025300         MOVE ART-KDPRODSL         TO UT-EKHT-KDPRODSL                    
025400         MOVE ART-KDSORT           TO UT-EKHT-KDSORT                      
025500         PERFORM IMS-GET-WDK611                                           
025600         IF SEGMENT-FINNS                                                 
025700           MOVE CLAG-KDPSLLOC      TO UT-EKHT-KDPSLLOC                    
025800         END-IF                                                           
025900       END-IF                                                             
026000     END-IF                                                               
026100     .                                                                    
026200     EJECT                                                                
026300                                                                          
026310 B-BYGG-EKONOMIPOST-102 SECTION.                                          
026320                                                                          
026330     MOVE  IN-EKHT-IDARTNR   TO W-SPAR-IDARTNR                            
026340     MOVE  IN-EKHT-IDARTNR   TO W-IDARTNR                                 
026350     MOVE  IN-EKHT-IDDC-SEND TO W-IDDC                                    
026360                                                                          
026370     MOVE  IN-EKHT-IDPGM     TO UT2-EKHT-IDPGM                            
026380     MOVE  IN-EKHT-TIREGDAT  TO UT2-EKHT-TIREGDAT                         
026390     MOVE  IN-EKHT-TIKLOCK   TO UT2-EKHT-TIKLOCK                          
026391     MOVE  IN-EKHT-IDSEKVNR  TO UT2-EKHT-IDSEKVNR                         
026392     MOVE  'W515EKHB'        TO UT2-EKHT-IDCPYTXT                         
026393     MOVE  IN-EKHT-BEVAT     TO UT2-EKHT-BEVAT                            
026394     MOVE  IN-EKHT-DAVERDAT  TO UT2-EKHT-DAVERDAT                         
026395     MOVE  IN-EKHT-FLLSBOK   TO UT2-EKHT-FLLSBOK                          
026396     MOVE  IN-EKHT-IDANALYS  TO UT2-EKHT-IDANALYS                         
026397     MOVE  IN-EKHT-IDARTNR   TO UT2-EKHT-IDARTNR                          
026398     MOVE  IN-EKHT-IDDC-SEND TO UT2-EKHT-IDDC-SEND                        
026399     MOVE  IN-EKHT-IDDC-REC  TO UT2-EKHT-IDDC-REC                         
026400     MOVE  IN-EKHT-IDDISTR   TO UT2-EKHT-IDDISTR                          
026401     MOVE  IN-EKHT-IDKONTO   TO UT2-EKHT-IDKONTO                          
026402     MOVE  IN-EKHT-IDKST     TO UT2-EKHT-IDKST                            
026403     MOVE  IN-EKHT-IDKUNDNR  TO UT2-EKHT-IDKUNDNR                         
026404     MOVE  IN-EKHT-IDTRANS   TO UT2-EKHT-IDTRANS                          
026405     MOVE  IN-EKHT-IDVERGL   TO UT2-EKHT-IDVERGL                          
026406     MOVE  IN-EKHT-KDANMORS  TO UT2-EKHT-KDANMORS                         
026407     MOVE  IN-EKHT-KDEKHHT   TO UT2-EKHT-KDEKHHT                          
026408     MOVE  IN-EKHT-KDEKSHT   TO UT2-EKHT-KDEKSHT                          
026409     MOVE  IN-EKHT-KDEKNIVA  TO UT2-EKHT-KDEKNIVA                         
026410     MOVE  IN-EKHT-KDFRAKT   TO UT2-EKHT-KDFRAKT                          
026411     MOVE  IN-EKHT-KDPRODSL  TO UT2-EKHT-KDPRODSL                         
026412     MOVE  IN-EKHT-KDPSLLOC  TO UT2-EKHT-KDPSLLOC                         
026413     MOVE  IN-EKHT-KDVALISO  TO UT2-EKHT-KDVALISO                         
026414     MOVE  IN-EKHT-KVANTAL   TO UT2-EKHT-KVANTAL                          
026415     MOVE  IN-EKHT-PRARTNTO  TO UT2-EKHT-PRARTNTO                         
026416     MOVE  IN-EKHT-PRARTSJK  TO UT2-EKHT-PRARTSJK                         
026417     MOVE  IN-EKHT-PRHEMTAG  TO UT2-EKHT-PRHEMTAG                         
026418     MOVE  IN-EKHT-PRARTSTD  TO UT2-EKHT-PRARTSTD                         
026419     MOVE  IN-EKHT-PRDIRLON  TO UT2-EKHT-PRDIRLON                         
026420     MOVE  IN-EKHT-PRDMTRL   TO UT2-EKHT-PRDMTRL                          
026421     MOVE  IN-EKHT-PRINK     TO UT2-EKHT-PRINK                            
026422     MOVE  IN-EKHT-PRKURS    TO UT2-EKHT-PRKURS                           
026423     MOVE  IN-EKHT-PRLANDCO  TO UT2-EKHT-PRLANDCO                         
026424     MOVE  IN-EKHT-PROVRPAL  TO UT2-EKHT-PROVRPAL                         
026425     MOVE  IN-EKHT-SUBEL     TO UT2-EKHT-SUBEL                            
026426     MOVE  IN-EKHT-SUVAT     TO UT2-EKHT-SUVAT                            
026427     MOVE  IN-EKHT-DAAVIDAT  TO UT2-EKHT-DAAVIDAT                         
026428     MOVE  IN-EKHT-IDAVINR   TO UT2-EKHT-IDAVINR                          
026429     MOVE  IN-EKHT-IDLEVNR   TO UT2-EKHT-IDLEVNR                          
026430     MOVE  IN-EKHT-KDAVVTYP  TO UT2-EKHT-KDAVVTYP                         
026431     MOVE  IN-EKHT-KDRT      TO UT2-EKHT-KDRT                             
026432     MOVE  IN-EKHT-KVANTMOT  TO UT2-EKHT-KVANTMOT                         
026433     MOVE  IN-EKHT-KVAVIS    TO UT2-EKHT-KVAVIS                           
026434     MOVE  IN-EKHT-KDSORT    TO UT2-EKHT-KDSORT                           
026435     MOVE  IN-EKHT-KDTRADP   TO UT2-EKHT-KDTRADP                          
026435     MOVE  IN-EKHT-IDFAKT-EXP                                             
026435                             TO UT2-EKHT-IDFAKT-EXP                       
026436     IF IN-EKHT-FLOVRLEV > SPACE                                          
026437       MOVE  IN-EKHT-FLOVRLEV  TO UT2-EKHT-FLOVRLEV                       
026438     ELSE                                                                 
026439       MOVE SPACE              TO UT2-EKHT-FLOVRLEV                       
026440     END-IF                                                               
026441     IF IN-EKHT-FLDCET = JA                                               
026442       MOVE JA               TO UT2-EKHT-FLDCET                           
026443     ELSE                                                                 
026444       MOVE NEJ              TO UT2-EKHT-FLDCET                           
026445     END-IF                                                               
026446     MOVE IN-EKHT-IDKUNDRF   TO UT2-EKHT-IDKUNDRF                         
026447     IF IN-EKHT-KDEKHHT = '203' AND IN-EKHT-KDEKNIVA = 'DET'              
026448       MOVE  IN-EKHT-IDORDNR5  TO UT2-EKHT-IDORDNR5                       
026449     END-IF                                                               
026450                                                                          
026451     IF IN-EKHT-IDARTNR > 0                                               
026452       PERFORM IMS-GET-WDK601                                             
026453       IF SEGMENT-FINNS                                                   
026454         MOVE ART-KDPRODSL         TO UT2-EKHT-KDPRODSL                   
026455         MOVE ART-KDSORT           TO UT2-EKHT-KDSORT                     
026456         PERFORM IMS-GET-WDK611                                           
026457         IF SEGMENT-FINNS                                                 
026458           MOVE CLAG-KDPSLLOC      TO UT2-EKHT-KDPSLLOC                   
026459         END-IF                                                           
026460       END-IF                                                             
026461     END-IF                                                               
026462     .                                                                    
026463     EJECT                                                                
026464                                                                          
026470 Z-FINIT SECTION.                                                         
026500     CLOSE W51560                                                         
026600           W51561A                                                        
026610           W51561C                                                        
026700                                                                          
026800     SKIP2                                                                
026900     MOVE 'S' TO POSTSUM-OPKOD                                            
027000     CALL POSTSUM USING POSTSUM-PARM                                      
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 S01-LAES-W51560  SECTION.                                                
027500     READ W51560          INTO IN-AREA                                    
027600     AT END                                                               
027700        MOVE HIGH-VALUE   TO IN-AREA                                      
027800        SET END-OF-W51560 TO TRUE                                         
027900                                                                          
028000     NOT AT END                                                           
028100        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
028200        MOVE 'W51560'     TO POSTSUM-FDNAMN                               
028300        MOVE 'W51560D1'   TO POSTSUM-DDNAMN2                              
028400        CALL POSTSUM USING POSTSUM-PARM                                   
028500                                                                          
028600     END-READ                                                             
028700     .                                                                    
028800     EJECT                                                                
028900                                                                          
029000 S02-SKRIV-W51561A SECTION.                                               
029100     WRITE UT-POST   FROM UT-AREA                                         
029200                                                                          
029300     MOVE 'UT- '     TO POSTSUM-TRANSTYP                                  
029400     MOVE 'W5156A'   TO POSTSUM-FDNAMN                                    
029500     MOVE 'W51560D2' TO POSTSUM-DDNAMN2                                   
029600     CALL POSTSUM USING POSTSUM-PARM                                      
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
029910 S02-SKRIV-W51561C SECTION.                                               
029920     WRITE UT2-POST   FROM UT2-AREA                                       
029930                                                                          
029940     MOVE 'UT- '     TO POSTSUM-TRANSTYP                                  
029950     MOVE 'W5156C'   TO POSTSUM-FDNAMN                                    
029960     MOVE 'W51560D3' TO POSTSUM-DDNAMN2                                   
029970     CALL POSTSUM USING POSTSUM-PARM                                      
029980     .                                                                    
029990     EJECT                                                                
029991                                                                          
030000 IMS-GET-WDK601 SECTION.                                                  
030100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
030200          DELIMITED BY SIZE INTO SSA1                                     
030300     MOVE '  GE'           TO GODK-STATUSKODER                            
030400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
030500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
030600     PERFORM IMS-STATUSKONTROLL                                           
030700     .                                                                    
030800                                                                          
030900 IMS-GET-WDK611 SECTION.                                                  
031000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
031100          DELIMITED BY SIZE INTO SSA1                                     
031200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
031300          DELIMITED BY SIZE INTO SSA2                                     
031400     MOVE '  GE'           TO GODK-STATUSKODER                            
031500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
031600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031700     PERFORM IMS-STATUSKONTROLL                                           
031800     .                                                                    
031900     EJECT                                                                
032000                                                                          
032100 IMS-STATUSKONTROLL SECTION.                                              
032200                                                                          
032300     SET STATUS-IX TO 1                                                   
032400     SEARCH GODK-STATUS                                                   
032500       AT END                                                             
032600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032700           DELIMITED BY SIZE INTO FELTEXT                                 
032800         DISPLAY FELTEXT                                                  
032900         CALL FELLOG                                                      
033000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033100         CONTINUE                                                         
033200     END-SEARCH                                                           
033300     .                                                                    
