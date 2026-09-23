000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5616000.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   20171031.                                                
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
002200     SELECT W56160                     ASSIGN TO W56160D1.                
002300     SKIP2                                                                
002400*          --- KOMPL. UTFIL-EKONOMIFILERNA                                
002500     SELECT W56161A                    ASSIGN TO W56160D2.                
002600     EJECT                                                                
002700*          --- KOMPL. UTFIL-EKONOMIFILERNA                                
002800     SELECT W56161C                    ASSIGN TO W56160D3.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W56160                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W56160      -L.                                                
003900                                                                          
004000     EJECT                                                                
004100 FD  W56161A                                                              
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500 01  UT-POST.                                                             
004600*    03   -COPY W56160    -L.                                             
004700     EJECT                                                                
004800 FD  W56161C                                                              
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200 01  UT2-POST.                                                            
005300*    03   -COPY W56160    -L.                                             
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W5616000'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000 77  W56160-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W56160                       VALUE 'J'.                   
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
009700*01  AREA -COPY W56160  -PRE IN-                                          
009800     EJECT                                                                
009900                                                                          
010000*    --- UTAREA EKONOMIPOST                                               
010100*01  AREA -COPY W56160  -PRE UT-                                          
010200     EJECT                                                                
010300                                                                          
010400*    --- UTAREA EKONOMIPOST                                               
010500*01  AREA -COPY W56160  -PRE UT2-                                         
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
015800     PERFORM S01-LAES-W56160                                              
015900     PERFORM UNTIL END-OF-W56160                                          
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
018000       IF IN-EKHT-IDCPYTXT = 'W561EKHB'                                   
018100         PERFORM B-BYGG-EKONOMIPOST-102                                   
018200         PERFORM S02-SKRIV-W56161C                                        
018300       ELSE                                                               
018400         PERFORM B-BYGG-EKONOMIPOST                                       
018500         PERFORM S02-SKRIV-W56161A                                        
018600       END-IF                                                             
018700       PERFORM S01-LAES-W56160                                            
018800     END-PERFORM                                                          
018900                                                                          
019000     PERFORM Z-FINIT                                                      
019100                                                                          
019200     MOVE ZERO TO RETURN-CODE                                             
019300     GOBACK                                                               
019400     .                                                                    
019500     EJECT                                                                
019600                                                                          
019700 A-INIT SECTION.                                                          
019800     OPEN INPUT  W56160                                                   
019900     OPEN OUTPUT W56161A                                                  
020000     OPEN OUTPUT W56161C                                                  
020100                                                                          
020200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020300     .                                                                    
020400     EJECT                                                                
020500 B-BYGG-EKONOMIPOST SECTION.                                              
020600                                                                          
020700     MOVE  IN-EKHT-IDARTNR   TO W-SPAR-IDARTNR                            
020800     MOVE  IN-EKHT-IDARTNR   TO W-IDARTNR                                 
020900     MOVE  IN-EKHT-IDDC-SEND TO W-IDDC                                    
021000                                                                          
021100     MOVE  IN-EKHT-IDPGM     TO UT-EKHT-IDPGM                             
021200     MOVE  IN-EKHT-TIREGDAT  TO UT-EKHT-TIREGDAT                          
021300     MOVE  IN-EKHT-TIKLOCK   TO UT-EKHT-TIKLOCK                           
021400     MOVE  IN-EKHT-IDSEKVNR  TO UT-EKHT-IDSEKVNR                          
021500     MOVE  'W561EKHA'        TO UT-EKHT-IDCPYTXT                          
021600     MOVE  IN-EKHT-BEVAT     TO UT-EKHT-BEVAT                             
021700     MOVE  IN-EKHT-DAVERDAT  TO UT-EKHT-DAVERDAT                          
021800     MOVE  IN-EKHT-FLLSBOK   TO UT-EKHT-FLLSBOK                           
021900     MOVE  IN-EKHT-IDANALYS  TO UT-EKHT-IDANALYS                          
022000     MOVE  IN-EKHT-IDARTNR   TO UT-EKHT-IDARTNR                           
022100     MOVE  IN-EKHT-IDDC-SEND TO UT-EKHT-IDDC-SEND                         
022200     MOVE  IN-EKHT-IDDC-REC  TO UT-EKHT-IDDC-REC                          
022300     MOVE  IN-EKHT-IDDISTR   TO UT-EKHT-IDDISTR                           
022400     MOVE  IN-EKHT-IDKONTO   TO UT-EKHT-IDKONTO                           
022500     MOVE  IN-EKHT-IDKST     TO UT-EKHT-IDKST                             
022600     MOVE  IN-EKHT-IDKUNDNR  TO UT-EKHT-IDKUNDNR                          
022700     MOVE  IN-EKHT-IDTRANS   TO UT-EKHT-IDTRANS                           
022800     MOVE  IN-EKHT-IDVERGL   TO UT-EKHT-IDVERGL                           
022900     MOVE  IN-EKHT-KDANMORS  TO UT-EKHT-KDANMORS                          
023000     MOVE  IN-EKHT-KDEKHHT   TO UT-EKHT-KDEKHHT                           
023100     MOVE  IN-EKHT-KDEKSHT   TO UT-EKHT-KDEKSHT                           
023200     MOVE  IN-EKHT-KDEKNIVA  TO UT-EKHT-KDEKNIVA                          
023300     MOVE  IN-EKHT-KDFRAKT   TO UT-EKHT-KDFRAKT                           
023400     MOVE  IN-EKHT-KDPRODSL  TO UT-EKHT-KDPRODSL                          
023500     MOVE  IN-EKHT-KDPSLLOC  TO UT-EKHT-KDPSLLOC                          
023600     MOVE  IN-EKHT-KDVALISO  TO UT-EKHT-KDVALISO                          
023700     MOVE  IN-EKHT-KVANTAL   TO UT-EKHT-KVANTAL                           
023800     MOVE  IN-EKHT-PRARTNTO  TO UT-EKHT-PRARTNTO                          
023900     MOVE  IN-EKHT-PRARTSJK  TO UT-EKHT-PRARTSJK                          
024000     MOVE  IN-EKHT-PRHEMTAG  TO UT-EKHT-PRHEMTAG                          
024100     MOVE  IN-EKHT-PRARTSTD  TO UT-EKHT-PRARTSTD                          
024200     MOVE  IN-EKHT-PRDIRLON  TO UT-EKHT-PRDIRLON                          
024300     MOVE  IN-EKHT-PRDMTRL   TO UT-EKHT-PRDMTRL                           
024400     MOVE  IN-EKHT-PRINK     TO UT-EKHT-PRINK                             
024500     MOVE  IN-EKHT-PRKURS    TO UT-EKHT-PRKURS                            
024600     MOVE  IN-EKHT-PRLANDCO  TO UT-EKHT-PRLANDCO                          
024700     MOVE  IN-EKHT-PROVRPAL  TO UT-EKHT-PROVRPAL                          
024800     MOVE  IN-EKHT-SUBEL     TO UT-EKHT-SUBEL                             
024900     MOVE  IN-EKHT-SUVAT     TO UT-EKHT-SUVAT                             
025000     MOVE  IN-EKHT-DAAVIDAT  TO UT-EKHT-DAAVIDAT                          
025100     MOVE  IN-EKHT-IDAVINR   TO UT-EKHT-IDAVINR                           
025200     MOVE  IN-EKHT-IDLEVNR   TO UT-EKHT-IDLEVNR                           
025300     MOVE  IN-EKHT-KDAVVTYP  TO UT-EKHT-KDAVVTYP                          
025400     MOVE  IN-EKHT-KDRT      TO UT-EKHT-KDRT                              
025500     MOVE  IN-EKHT-KVANTMOT  TO UT-EKHT-KVANTMOT                          
025600     MOVE  IN-EKHT-KVAVIS    TO UT-EKHT-KVAVIS                            
025700     MOVE  IN-EKHT-KDSORT    TO UT-EKHT-KDSORT                            
025800     MOVE  IN-EKHT-KDTRADP   TO UT-EKHT-KDTRADP                           
025800     MOVE  IN-EKHT-IDFAKT-EXP                                             
025800                             TO UT-EKHT-IDFAKT-EXP                        
025900     IF IN-EKHT-FLOVRLEV > SPACE                                          
026000       MOVE  IN-EKHT-FLOVRLEV  TO UT-EKHT-FLOVRLEV                        
026100     ELSE                                                                 
026200       MOVE SPACE              TO UT-EKHT-FLOVRLEV                        
026300     END-IF                                                               
026400     IF IN-EKHT-FLDCET = JA                                               
026500       MOVE JA               TO UT-EKHT-FLDCET                            
026600     ELSE                                                                 
026700       MOVE NEJ              TO UT-EKHT-FLDCET                            
026800     END-IF                                                               
026900     MOVE IN-EKHT-IDKUNDRF   TO UT-EKHT-IDKUNDRF                          
027000     IF IN-EKHT-KDEKHHT = '203' AND IN-EKHT-KDEKNIVA = 'DET'              
027100       MOVE  IN-EKHT-IDORDNR5  TO UT-EKHT-IDORDNR5                        
027200     END-IF                                                               
027300                                                                          
027400     IF IN-EKHT-IDARTNR > 0                                               
027500       PERFORM IMS-GET-WDK601                                             
027600       IF SEGMENT-FINNS                                                   
027610         IF  IN-EKHT-KDEKHHT = '402'                                      
027620         AND IN-EKHT-KDEKSHT = '401'                                      
027630           CONTINUE                                                       
027640         ELSE                                                             
027700           MOVE ART-KDPRODSL       TO UT-EKHT-KDPRODSL                    
027710         END-IF                                                           
027800         MOVE ART-KDSORT           TO UT-EKHT-KDSORT                      
027900         PERFORM IMS-GET-WDK611                                           
028000         IF SEGMENT-FINNS                                                 
028100           MOVE CLAG-KDPSLLOC      TO UT-EKHT-KDPSLLOC                    
028200         END-IF                                                           
028300       END-IF                                                             
028400     END-IF                                                               
028500     .                                                                    
028600     EJECT                                                                
028700                                                                          
028800 B-BYGG-EKONOMIPOST-102 SECTION.                                          
028900                                                                          
029000     MOVE  IN-EKHT-IDARTNR   TO W-SPAR-IDARTNR                            
029100     MOVE  IN-EKHT-IDARTNR   TO W-IDARTNR                                 
029200     MOVE  IN-EKHT-IDDC-SEND TO W-IDDC                                    
029300                                                                          
029400     MOVE  IN-EKHT-IDPGM     TO UT2-EKHT-IDPGM                            
029500     MOVE  IN-EKHT-TIREGDAT  TO UT2-EKHT-TIREGDAT                         
029600     MOVE  IN-EKHT-TIKLOCK   TO UT2-EKHT-TIKLOCK                          
029700     MOVE  IN-EKHT-IDSEKVNR  TO UT2-EKHT-IDSEKVNR                         
029800     MOVE  'W561EKHB'        TO UT2-EKHT-IDCPYTXT                         
029900     MOVE  IN-EKHT-BEVAT     TO UT2-EKHT-BEVAT                            
030000     MOVE  IN-EKHT-DAVERDAT  TO UT2-EKHT-DAVERDAT                         
030100     MOVE  IN-EKHT-FLLSBOK   TO UT2-EKHT-FLLSBOK                          
030200     MOVE  IN-EKHT-IDANALYS  TO UT2-EKHT-IDANALYS                         
030300     MOVE  IN-EKHT-IDARTNR   TO UT2-EKHT-IDARTNR                          
030400     MOVE  IN-EKHT-IDDC-SEND TO UT2-EKHT-IDDC-SEND                        
030500     MOVE  IN-EKHT-IDDC-REC  TO UT2-EKHT-IDDC-REC                         
030600     MOVE  IN-EKHT-IDDISTR   TO UT2-EKHT-IDDISTR                          
030700     MOVE  IN-EKHT-IDKONTO   TO UT2-EKHT-IDKONTO                          
030800     MOVE  IN-EKHT-IDKST     TO UT2-EKHT-IDKST                            
030900     MOVE  IN-EKHT-IDKUNDNR  TO UT2-EKHT-IDKUNDNR                         
031000     MOVE  IN-EKHT-IDTRANS   TO UT2-EKHT-IDTRANS                          
031100     MOVE  IN-EKHT-IDVERGL   TO UT2-EKHT-IDVERGL                          
031200     MOVE  IN-EKHT-KDANMORS  TO UT2-EKHT-KDANMORS                         
031300     MOVE  IN-EKHT-KDEKHHT   TO UT2-EKHT-KDEKHHT                          
031400     MOVE  IN-EKHT-KDEKSHT   TO UT2-EKHT-KDEKSHT                          
031500     MOVE  IN-EKHT-KDEKNIVA  TO UT2-EKHT-KDEKNIVA                         
031600     MOVE  IN-EKHT-KDFRAKT   TO UT2-EKHT-KDFRAKT                          
031700     MOVE  IN-EKHT-KDPRODSL  TO UT2-EKHT-KDPRODSL                         
031800     MOVE  IN-EKHT-KDPSLLOC  TO UT2-EKHT-KDPSLLOC                         
031900     MOVE  IN-EKHT-KDVALISO  TO UT2-EKHT-KDVALISO                         
032000     MOVE  IN-EKHT-KVANTAL   TO UT2-EKHT-KVANTAL                          
032100     MOVE  IN-EKHT-PRARTNTO  TO UT2-EKHT-PRARTNTO                         
032200     MOVE  IN-EKHT-PRARTSJK  TO UT2-EKHT-PRARTSJK                         
032300     MOVE  IN-EKHT-PRHEMTAG  TO UT2-EKHT-PRHEMTAG                         
032400     MOVE  IN-EKHT-PRARTSTD  TO UT2-EKHT-PRARTSTD                         
032500     MOVE  IN-EKHT-PRDIRLON  TO UT2-EKHT-PRDIRLON                         
032600     MOVE  IN-EKHT-PRDMTRL   TO UT2-EKHT-PRDMTRL                          
032700     MOVE  IN-EKHT-PRINK     TO UT2-EKHT-PRINK                            
032800     MOVE  IN-EKHT-PRKURS    TO UT2-EKHT-PRKURS                           
032900     MOVE  IN-EKHT-PRLANDCO  TO UT2-EKHT-PRLANDCO                         
033000     MOVE  IN-EKHT-PROVRPAL  TO UT2-EKHT-PROVRPAL                         
033100     MOVE  IN-EKHT-SUBEL     TO UT2-EKHT-SUBEL                            
033200     MOVE  IN-EKHT-SUVAT     TO UT2-EKHT-SUVAT                            
033300     MOVE  IN-EKHT-DAAVIDAT  TO UT2-EKHT-DAAVIDAT                         
033400     MOVE  IN-EKHT-IDAVINR   TO UT2-EKHT-IDAVINR                          
033500     MOVE  IN-EKHT-IDLEVNR   TO UT2-EKHT-IDLEVNR                          
033600     MOVE  IN-EKHT-KDAVVTYP  TO UT2-EKHT-KDAVVTYP                         
033700     MOVE  IN-EKHT-KDRT      TO UT2-EKHT-KDRT                             
033800     MOVE  IN-EKHT-KVANTMOT  TO UT2-EKHT-KVANTMOT                         
033900     MOVE  IN-EKHT-KVAVIS    TO UT2-EKHT-KVAVIS                           
034000     MOVE  IN-EKHT-KDSORT    TO UT2-EKHT-KDSORT                           
034100     MOVE  IN-EKHT-KDTRADP   TO UT2-EKHT-KDTRADP                          
034100     MOVE  IN-EKHT-IDFAKT-EXP                                             
034100                             TO UT2-EKHT-IDFAKT-EXP                       
034200     IF IN-EKHT-FLOVRLEV > SPACE                                          
034300       MOVE  IN-EKHT-FLOVRLEV  TO UT2-EKHT-FLOVRLEV                       
034400     ELSE                                                                 
034500       MOVE SPACE              TO UT2-EKHT-FLOVRLEV                       
034600     END-IF                                                               
034700     IF IN-EKHT-FLDCET = JA                                               
034800       MOVE JA               TO UT2-EKHT-FLDCET                           
034900     ELSE                                                                 
035000       MOVE NEJ              TO UT2-EKHT-FLDCET                           
035100     END-IF                                                               
035200     MOVE IN-EKHT-IDKUNDRF   TO UT2-EKHT-IDKUNDRF                         
035300     IF IN-EKHT-KDEKHHT = '203' AND IN-EKHT-KDEKNIVA = 'DET'              
035400       MOVE  IN-EKHT-IDORDNR5  TO UT2-EKHT-IDORDNR5                       
035500     END-IF                                                               
035600                                                                          
035700     IF IN-EKHT-IDARTNR > 0                                               
035800       PERFORM IMS-GET-WDK601                                             
035900       IF SEGMENT-FINNS                                                   
035910         IF  IN-EKHT-KDEKHHT = '402'                                      
035920         AND IN-EKHT-KDEKSHT = '401'                                      
035930           CONTINUE                                                       
035940         ELSE                                                             
036000           MOVE ART-KDPRODSL       TO UT2-EKHT-KDPRODSL                   
036010         END-IF                                                           
036100         MOVE ART-KDSORT           TO UT2-EKHT-KDSORT                     
036200         PERFORM IMS-GET-WDK611                                           
036300         IF SEGMENT-FINNS                                                 
036400           MOVE CLAG-KDPSLLOC      TO UT2-EKHT-KDPSLLOC                   
036500         END-IF                                                           
036600       END-IF                                                             
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000                                                                          
037100 Z-FINIT SECTION.                                                         
037200     CLOSE W56160                                                         
037300           W56161A                                                        
037400           W56161C                                                        
037500                                                                          
037600     SKIP2                                                                
037700     MOVE 'S' TO POSTSUM-OPKOD                                            
037800     CALL POSTSUM USING POSTSUM-PARM                                      
037900     .                                                                    
038000     EJECT                                                                
038100                                                                          
038200 S01-LAES-W56160  SECTION.                                                
038300     READ W56160          INTO IN-AREA                                    
038400     AT END                                                               
038500        MOVE HIGH-VALUE   TO IN-AREA                                      
038600        SET END-OF-W56160 TO TRUE                                         
038700                                                                          
038800     NOT AT END                                                           
038900        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
039000        MOVE 'W56160'     TO POSTSUM-FDNAMN                               
039100        MOVE 'W56160D1'   TO POSTSUM-DDNAMN2                              
039200        CALL POSTSUM USING POSTSUM-PARM                                   
039300                                                                          
039400     END-READ                                                             
039500     .                                                                    
039600     EJECT                                                                
039700                                                                          
039800 S02-SKRIV-W56161A SECTION.                                               
039900     WRITE UT-POST   FROM UT-AREA                                         
040000                                                                          
040100     MOVE 'UT- '     TO POSTSUM-TRANSTYP                                  
040200     MOVE 'W5616A'   TO POSTSUM-FDNAMN                                    
040300     MOVE 'W56160D2' TO POSTSUM-DDNAMN2                                   
040400     CALL POSTSUM USING POSTSUM-PARM                                      
040500     .                                                                    
040600     EJECT                                                                
040700                                                                          
040800 S02-SKRIV-W56161C SECTION.                                               
040900     WRITE UT2-POST   FROM UT2-AREA                                       
041000                                                                          
041100     MOVE 'UT- '     TO POSTSUM-TRANSTYP                                  
041200     MOVE 'W5616C'   TO POSTSUM-FDNAMN                                    
041300     MOVE 'W56160D3' TO POSTSUM-DDNAMN2                                   
041400     CALL POSTSUM USING POSTSUM-PARM                                      
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800 IMS-GET-WDK601 SECTION.                                                  
041900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
042000          DELIMITED BY SIZE INTO SSA1                                     
042100     MOVE '  GE'           TO GODK-STATUSKODER                            
042200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
042300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
042400     PERFORM IMS-STATUSKONTROLL                                           
042500     .                                                                    
042600                                                                          
042700 IMS-GET-WDK611 SECTION.                                                  
042800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
042900          DELIMITED BY SIZE INTO SSA1                                     
043000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
043100          DELIMITED BY SIZE INTO SSA2                                     
043200     MOVE '  GE'           TO GODK-STATUSKODER                            
043300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
043400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     .                                                                    
043700     EJECT                                                                
043800                                                                          
043900 IMS-STATUSKONTROLL SECTION.                                              
044000                                                                          
044100     SET STATUS-IX TO 1                                                   
044200     SEARCH GODK-STATUS                                                   
044300       AT END                                                             
044400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044500           DELIMITED BY SIZE INTO FELTEXT                                 
044600         DISPLAY FELTEXT                                                  
044700         CALL FELLOG                                                      
044800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044900         CONTINUE                                                         
045000     END-SEARCH                                                           
046000     .                                                                    
