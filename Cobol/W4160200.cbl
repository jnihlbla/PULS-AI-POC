000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4160200.                                                
000400 AUTHOR.         KERSTIN MATTIASSON.                                      
000500 DATE-WRITTEN.   90/11/28.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*    KOMPLETTERAD AV GUNILLA JOHANSSON, 1991                              
001100*        PROGRAMMET LÄSER IN SATSORDERKÖN OCH PRIORITERAR DESSA I         
001200*        RÄTT ORDNING. HÄNSYN TAS ÄVEN TILL AK-SALDOT.                    
001300*        CHECKPOINT EFTER 200 BEHANDLADE ORDER.                           
001400*        DÄREFTER BEHANDLAR MAN SATSORDERN PÅ FÖLJANDE VIS.               
001500*                                                                         
001600*        FÖR GAMLA SATSORDER:                                             
001700*            KONTROLLERAR EVENTUELLA TÄCKNINGAR I RESTORDER.              
001800*            KONTROLLERAR ATT SATS-STRUKTUREN STÄMMER ÖVERENS MED         
001900*            RASA. UPPDATERINGSDATUM I WDJ201 OCH RD101 JÄMFÖRS.          
002000*            ÄVEN ÄNDRINGAR SOM SKETT UNDER DAGEN TAS OM HAND.            
002100*                                                                         
002200*            TIUPPDAT ÄNDRAS NÄR EN ÄNDRING BEROENDE PÅ RASA              
002300*            HAR SKETT. ÄVEN VID SPÄRR. TIUPPDAT ÄNDRAS ÄVEN              
002400*            I PROGRAM W20303.                                            
002500*                                                                         
002600*        FÖR NYA SATSORDER:                                               
002700*            SKAPAR RESERVATIONER OCH EV RESTORDER.                       
002800*            SKAPAR ORDERINGÅNGSSTATISTIK                                 
002900*                                                                         
003000*        VID OTYDLIGHETER I ING.ARTIKLAR VID JÄMFÖRELSE MOT RASA          
003100*        SPÄRRAS SAMTLIGA RADER AKTUELLA FÖR ÄNDRING OCH ORDERN           
003200*        SPÄRRAS. ORDERN BLIR EJ BYGGBAR.                                 
003300*                                                                         
003400*        NÄR EN ORDER BLIR BYGGBAR BERÄKNAS VIKT OCH VOLYM OCH            
003500*        PRODUKTIONSTIDEN BERÄKNAS.                                       
003600*                                                                         
003700*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
003800*                              WLORDP (WDA5)                              
003900*                              WLSATG (WDJ2)                              
004000*                                                                         
004100*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
004200*                              WLERSA (WDD7)                              
004300*                              WLORDQ (WDA5)                              
004400*                              WLORDR (WDA5)                              
004500*                              WLSATB (WDJ1)                              
004600*                                                                         
004700*        LÄNKAREOR:                                                       
004800*                                                                         
004900                                                                          
005000     SKIP3                                                                
005100 ENVIRONMENT DIVISION.                                                    
005200     SKIP2                                                                
005300 CONFIGURATION SECTION.                                                   
005400     SKIP2                                                                
005500 INPUT-OUTPUT SECTION.                                                    
005600                                                                          
005700 FILE-CONTROL.                                                            
005800     SKIP2                                                                
005900*          --- FELLISTA                                                   
006000     SELECT FELLISTA                   ASSIGN TO W41602D1.                
006100     EJECT                                                                
006200 DATA DIVISION.                                                           
006300     SKIP3                                                                
006400 FILE SECTION.                                                            
006500     SKIP3                                                                
006600 FD  FELLISTA                                                             
006700     LABEL RECORD    STANDARD                                             
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000     SKIP2                                                                
007100 01  FELPOST                     PIC X(80).                               
007200                                                                          
007300     EJECT                                                                
007400 WORKING-STORAGE SECTION.                                                 
007500     SKIP2                                                                
007600*    -COPY WY2000W1                                                       
007700     SKIP3                                                                
007800 77  IDPGM                       PIC X(8)    VALUE 'W4160200'.            
007900 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
008000 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
008100 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
008200 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
008300 77  JA                          PIC X       VALUE 'J'.                   
008400 77  NEJ                         PIC X       VALUE 'N'.                   
008500 77  RAKNARE                     PIC 9(4)    COMP-3.                      
008600 77  2109-IX                     PIC S9(9)  VALUE ZERO COMP SYNC.         
008700 77  2109-IX-MAX                 PIC S9(9)  VALUE +18  COMP SYNC.         
008800 77  Y2K-IX                      PIC S9(3)  VALUE ZERO COMP SYNC.         
008900*      --- VALID IDDC CODES                                               
009000*                                                                         
009100*01    -COPY WWDCKONS                                                     
009200                                                                          
009210*      --- BYTESARTIKLAR                                                  
009220*                                                                         
009230*01    -COPY WWBYT03                                                      
009240       EJECT                                                              
009300*                                                                         
009400 01  FILLER                      PIC X(11)   VALUE 'ABEND-KODER'.         
009500 01  ABEND-KODER.                                                         
009600     03  ABEND-MED-DUMP          PIC S9(4) COMP SYNC VALUE +1000.         
009700     03  ABEND-UTAN-DUMP         PIC S9(4) COMP SYNC VALUE +16.           
009800*                                                                         
009900 01  FILLER                      PIC X(9)    VALUE 'SPAR-AREA'.           
010000 01  SPAR-AREA.                                                           
010100     03  SPAR-IDARTNR            PIC S9(9)   VALUE ZERO  COMP-3.          
010200     03  SPAR-KVRORAD-9KOMPL     PIC S9(5)    VALUE ZERO COMP-3.          
010300     03  SPAR-RELSKVOT-L         PIC S9(3)V99 VALUE ZERO COMP-3.          
010400     03  SPAR-RELSKVOT-S         PIC S9(3)V99 VALUE ZERO COMP-3.          
010500*                                                                         
010600 01  FILLER                      PIC X(7)    VALUE 'AWBAREA'.             
010700 01  AWBAREA.                                                             
010800     03  WS-FLBYGGB              PIC X       VALUE 'J'.                   
010900     03  WS-TACKT                PIC S9(7)   VALUE ZERO  COMP-3.          
011000     03  WS-BEHOV                PIC S9(7)   VALUE ZERO  COMP-3.          
011100     03  WS-KVRESS-M             PIC S9(7)   VALUE ZERO  COMP-3.          
011200     03  WS-KVSATROS-SUM         PIC S9(7)   VALUE ZERO  COMP-3.          
011300     03  WS-KVSATROS-G           PIC S9(7)   VALUE ZERO  COMP-3.          
011400     03  WS-KVBYGGBAR            PIC S9(7)   VALUE ZERO  COMP-3.          
011500     03  WS-KVBYGGB              PIC S9(7)V99 VALUE ZERO  COMP-3.         
011600     03  WS-KVBYGGB-KMB          PIC S9(7)V99 VALUE ZERO  COMP-3.         
011700     03  WS-REANTPSA             PIC S99V999 VALUE ZERO  COMP-3.          
011800     03  WS-WDD7-REANTPSA        PIC S99V999 VALUE ZERO  COMP-3.          
011900     03  WS-KVSATRES             PIC S9(7)   VALUE ZERO  COMP-3.          
012000     03  WS-REBEART              PIC S9(7)   VALUE ZERO  COMP-3.          
012100     03  WS-REBEART-KOLL         PIC S9(7)   VALUE ZERO  COMP-3.          
012200     03  WS-REBEART-ANNULL       PIC S9(7)   VALUE ZERO  COMP-3.          
012300     03  WS-TOTLS                PIC S9(7)   VALUE ZERO  COMP-3.          
012400     03  WS-TOTAKS               PIC S9(7)   VALUE ZERO  COMP-3.          
012500     03  WS-TOTROS               PIC S9(7)   VALUE ZERO  COMP-3.          
012600     03  WS-TOTPB                PIC S9(6)V9 VALUE ZERO  COMP-3.          
012700     03  WS-RO-RAKN              PIC S9(7)   VALUE ZERO  COMP-3.          
012800     03  WS-RO-JUST              PIC S9(7)   VALUE ZERO  COMP-3.          
012900     03  WS-RELSKVOT             PIC S999V99 VALUE ZERO  COMP-3.          
013000     03  WS-DISPLS               PIC S9(7)   VALUE ZERO  COMP-3.          
013100     03  WS-RO-ANTAL             PIC S9(9)   COMP-3.                      
013200     03  WS-RADANT               PIC S99     VALUE ZERO  COMP-3.          
013300     03  WS-RASA-IDARTNR-RAKN    PIC S99     VALUE ZERO  COMP-3.          
013400     03  WS-VLORDNTO-CM          PIC S9(8)V9 VALUE ZERO COMP-3.           
013500     03  WS-VLORDNTO-M           PIC S9(4)V9(3) VALUE ZERO COMP-3.        
013600     03  WS-VKORDNTO-1DEC        PIC S9(6)V9 VALUE ZERO COMP-3.           
013700     03  WS-VKORDNTO-3DEC        PIC S9(6)V9(3) VALUE ZERO COMP-3.        
013800     03  WS-VKART                PIC S9(7)   VALUE ZERO COMP-3.           
013900     03  WS-VKARTNTO-TOT         PIC S9(4)V999 VALUE ZERO COMP-3.         
014000     03  WS-VLARTNTO-TOT         PIC S9(8)V9 VALUE ZERO COMP-3.           
014100     03  WS-KVRADER              PIC S9(3)   VALUE ZERO COMP-3.           
014200     03  WS-NYREBEART            PIC S9(7)   VALUE ZERO COMP-3.           
014300     03  WS-NYREBEART-KOLL       PIC S9(7)V9 VALUE ZERO COMP-3.           
014400     03  WS-DAT-TIAAP            PIC  9(3).                               
014500     03  WS-IDDISTR-NUM4         PIC  9(4).                               
014600     03  WS-IDKUNDNR-NUM6        PIC  9(6).                               
014700*                                                                         
014800     03  WS-IDORDNST.                                                     
014900         05  WS-IDORDNSB         PIC 9(4)    VALUE ZERO.                  
015000         05  WS-IDORDNSS         PIC 9       VALUE ZERO.                  
015100     03  WS-IDORDNST-NUM  REDEFINES WS-IDORDNST PIC 9(5).                 
015200*                                                                         
015300     03  WS-IDORDNST-AKT.                                                 
015400         05  WS-IDORDNSB-AKT     PIC S9(5)   VALUE ZERO COMP-3.           
015500         05  WS-IDORDNSS-AKT     PIC S9      VALUE ZERO COMP-3.           
015600*                                                                         
015700 01  FILLER                      PIC X(8)    VALUE 'SWITCHAR'.            
015800 01  SWITCHAR.                                                            
015900     03  SW-RASA-SPARR           PIC X       VALUE 'N'.                   
016000     03  SW-RASA-ANDR            PIC X       VALUE 'N'.                   
016100     03  SW-ORDER-ANDR           PIC X       VALUE 'N'.                   
016200     03  SW-SATS-TRAFF           PIC X       VALUE 'N'.                   
016300     03  SW-ARB-E-TRAFF          PIC X       VALUE 'N'.                   
016400     03  SW-WDD7-TRAFF           PIC X       VALUE 'N'.                   
016500     03  SW-SATS-FEL             PIC X       VALUE 'N'.                   
016600     03  SW-ANT-BYGGB-FORSTA     PIC X       VALUE 'J'.                   
016700     03  SW-TACKT-RO             PIC X       VALUE 'N'.                   
016800     03  SW-RORAD-TACKT          PIC X       VALUE 'N'.                   
016900     03  SW-ANV-KOMBKOD-TRAFF    PIC X       VALUE 'N'.                   
017000     03  SW-UTG                  PIC X       VALUE 'N'.                   
017100*                                                                         
017200                                                                          
017300 01  WS-TIME                     PIC X(8).                                
017400                                                                          
017500 01  DAGENS-DATUM-Y2K.                                                    
017600     03  DAGENS-DATUM-AAR-Y2K    PIC 9(4).                                
017700     03  DAGENS-DATUM-MAANAD-Y2K PIC 9(2).                                
017800     03  DAGENS-DATUM-DAG-Y2K    PIC 9(2).                                
017900                                                                          
018000 01  DAGENS-DATUM.                                                        
018100     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
018200     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
018300     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
018400 01  DAGENS-DATUM-NUM REDEFINES DAGENS-DATUM PIC 9(6).                    
018500                                                                          
018600                                                                          
018700 01  GARDAGENS-DATUM.                                                     
018800     03  GARDAGENS-DATUM-AAR     PIC X(2)    VALUE SPACE.                 
018900     03  GARDAGENS-DATUM-MAANAD  PIC X(2)    VALUE SPACE.                 
019000     03  GARDAGENS-DATUM-DAG     PIC X(2)    VALUE SPACE.                 
019100 01  GARDAGENS-DATUM-NUM REDEFINES GARDAGENS-DATUM PIC 9(6).              
019200     EJECT                                                                
019300 01  DYNAMISKA-SUBPROGRAM.                                                
019400*                                                                         
019500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
019900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020000     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
020100     03  W416PTID                PIC X(8)    VALUE 'W416PTID'.            
020200     EJECT                                                                
020300*                                                                         
020400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020500     SKIP3                                                                
020600 01  NYCKLAR-TILL-DLI.                                                    
020700     03  W-WDJ201-IDORDNST-X.                                             
020800         05  W-WDJ201-IDORDNSB   PIC S9(5)   VALUE ZERO COMP-3.           
020900         05  W-WDJ201-IDORDNSS   PIC S9      VALUE ZERO COMP-3.           
021000*                                                                         
021100     03  W-WDJ211-IDARTNR-X.                                              
021200         05  W-WDJ211-IDARTNR    PIC S9(9)   VALUE ZERO COMP-3.           
021300     03  W-WDJ211-KDSATKMB-MIN-X.                                         
021400         05  W-WDJ211-KDSATKMB-MIN   PIC X   VALUE SPACE.                 
021500     03  W-WDJ211-KDSATKMB-MAX-X.                                         
021600         05  W-WDJ211-KDSATKMB-MAX   PIC X   VALUE SPACE.                 
021700*                                                                         
021800     03  W-WDJ2C-MIN-X.                                                   
021900         05 W-WDJ2C-MIN-IDARTNR            PIC S9(9)    COMP-3.           
022000         05 W-WDJ2C-MIN-DAREGDAT           PIC  9(8).                     
022100     03  W-WDJ2C-MAX-X.                                                   
022200         05 W-WDJ2C-MAX-IDARTNR            PIC S9(9)    COMP-3.           
022300         05 W-WDJ2C-MAX-DAREGDAT           PIC  9(8).                     
022400*                                                                         
022500     03  W-WDJ2D-MIN-X.                                                   
022600         05  W-WDJ2D-MIN-KDCLAGER            PIC S9       COMP-3.         
022700         05  W-WDJ2D-MIN-KVRORAD-9KOMPL      PIC S9(5)    COMP-3.         
022800         05  W-WDJ2D-MIN-RELSKVOT-L          PIC S9(3)V99 COMP-3.         
022900         05  W-WDJ2D-MIN-FLSATPRI            PIC X.                       
023000         05  W-WDJ2D-MIN-RELSKVOT-S          PIC S9(3)V99 COMP-3.         
023100         05  W-WDJ2D-MIN-IDARTNR             PIC S9(9)    COMP-3.         
023200         05  W-WDJ2D-MIN-DAREGDAT            PIC  9(8).                   
023300     03  W-WDJ2D-MAX-X.                                                   
023400         05  W-WDJ2D-MAX-KDCLAGER            PIC S9       COMP-3.         
023500         05  W-WDJ2D-MAX-KVRORAD-9KOMPL      PIC S9(5)    COMP-3.         
023600         05  W-WDJ2D-MAX-RELSKVOT-L          PIC S9(3)V99 COMP-3.         
023700         05  W-WDJ2D-MAX-FLSATPRI            PIC X.                       
023800         05  W-WDJ2D-MAX-RELSKVOT-S          PIC S9(3)V99 COMP-3.         
023900         05  W-WDJ2D-MAX-IDARTNR             PIC S9(9)    COMP-3.         
024000         05  W-WDJ2D-MAX-DAREGDAT            PIC  9(8).                   
024100     03  W-WDJ2D-AKT-X.                                                   
024200         05  W-WDJ2D-AKT-KDCLAGER            PIC S9       COMP-3.         
024300         05  W-WDJ2D-AKT.                                                 
024400           07  W-WDJ2D-AKT-KVRORAD-9KOMPL    PIC S9(5)    COMP-3.         
024500           07  W-WDJ2D-AKT-RELSKVOT-L        PIC S9(3)V99 COMP-3.         
024600           07  W-WDJ2D-AKT-FLSATPRI          PIC X.                       
024700           07  W-WDJ2D-AKT-RELSKVOT-S        PIC S9(3)V99 COMP-3.         
024800           07  W-WDJ2D-AKT-IDARTNR           PIC S9(9)    COMP-3.         
024900           07  W-WDJ2D-AKT-DAREGDAT          PIC  9(8).                   
025000     03  W-WDJ1-IDARTNR-X.                                                
025100         05  W-WDJ1-IDARTNR      PIC S9(9)   VALUE ZERO COMP-3.           
025200     03  W-WDJ111KY-X.                                                    
025300         05  W-WDJ111-KDSTRRAD   PIC X       VALUE SPACE.                 
025400         05  W-WDJ111-IDRADNR    PIC S9(5)   VALUE ZERO COMP-3.           
025500     03  W-IDARTNR-X.                                                     
025600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
025700     03  W-WDA501KY-X.                                                    
025800         05  W-WDA501-IDDISTR    PIC S9(5)   VALUE ZERO COMP-3.           
025900         05  W-WDA501-IDKUNDNR   PIC S9(7)   VALUE ZERO COMP-3.           
026000         05  W-WDA501-IDKUNDRF   PIC X(10).                               
026100         05  W-WDA501-IDARTNR    PIC S9(9)   VALUE ZERO COMP-3.           
026200         05  W-WDA501-IDLOPNR    PIC S9(3)   VALUE ZERO COMP-3.           
026300     03  W-WDA501KY-MIN-X.                                                
026400         05  W-WDA501-IDDISTR-MIN    PIC S9(5)   COMP-3.                  
026500         05  W-WDA501-IDKUNDNR-MIN   PIC S9(7)   COMP-3.                  
026600         05  W-WDA501-IDKUNDRF-MIN.                                       
026700           07 W-WDA501-IDORDNR5-MIN  PIC 9(5).                            
026800           07 FILLER                 PIC X(5).                            
026900         05  W-WDA501-IDARTNR-MIN    PIC S9(9)   COMP-3.                  
027000         05  W-WDA501-IDLOPNR-MIN    PIC S9(3)   COMP-3.                  
027100     03  W-WDA501KY-MAX-X.                                                
027200         05  W-WDA501-IDDISTR-MAX    PIC S9(5)   COMP-3.                  
027300         05  W-WDA501-IDKUNDNR-MAX   PIC S9(7)   COMP-3.                  
027400         05  W-WDA501-IDKUNDRF-MAX.                                       
027500           07 W-WDA501-IDORDNR5-MAX  PIC 9(5).                            
027600           07 FILLER                 PIC X(5).                            
027700         05  W-WDA501-IDARTNR-MAX    PIC S9(9)   COMP-3.                  
027800         05  W-WDA501-IDLOPNR-MAX    PIC S9(3)   COMP-3.                  
027900     03  W-WDA5A1KY-MIN-X.                                                
028000         05  W-WDA5A1-IDARTNR-MIN    PIC S9(9)   COMP-3.                  
028100         05  W-WDA5A1-IDDC-MIN       PIC X(2).                            
028200         05  FILLER                  PIC X(33).                           
028800     03  W-WDA5A1KY-MAX-X.                                                
028900         05  W-WDA5A1-IDARTNR-MAX    PIC S9(9)   COMP-3.                  
029000         05  W-WDA5A1-IDDC-MAX       PIC X(2).                            
029010         05  FILLER                  PIC X(33).                           
029700     03  W-WDA5B1KY-MIN-X.                                                
029800         05  W-WDA5B1-IDDISTR-MIN    PIC S9(5)   COMP-3.                  
029900         05  W-WDA5B1-IDKUNDNR-MIN   PIC S9(7)   COMP-3.                  
030000         05  W-WDA5B1-IDDC-MIN       PIC X(2).                            
030100         05  W-WDA5B1-IDARTNR-MIN    PIC S9(9)   COMP-3.                  
030200         05  W-WDA5B1-IDKUNDRF-MIN.                                       
030300           07  W-WDA5B1-IDORDNR5-MIN PIC 9(5).                            
030400           07  FILLER                PIC X(5).                            
030500         05  W-WDA5B1-IDLOPNR-MIN    PIC S9(3)   COMP-3.                  
030600     03  W-WDA5B1KY-MAX-X.                                                
030700         05  W-WDA5B1-IDDISTR-MAX    PIC S9(5)   COMP-3.                  
030800         05  W-WDA5B1-IDKUNDNR-MAX   PIC S9(7)   COMP-3.                  
030900         05  W-WDA5B1-IDDC-MAX       PIC X(2).                            
031000         05  W-WDA5B1-IDARTNR-MAX    PIC S9(9)   COMP-3.                  
031100         05  W-WDA5B1-IDKUNDRF-MAX.                                       
031200           07  W-WDA5B1-IDORDNR5-MAX PIC 9(5).                            
031300           07  FILLER                PIC X(5).                            
031400         05  W-WDA5B1-IDLOPNR-MAX    PIC S9(3)   COMP-3.                  
031500     03  W-KDSTARAD-X            PIC X       VALUE '3'.                   
031600     03  W-KDCLAGER-X.                                                    
031700         05  W-KDCLAGER          PIC S9      VALUE 0    COMP-3.           
031800*                                                                         
031900*----> PRIORITETSSTYRNING FÖR RESTORDER                                   
032000*                                                                         
032100     03  W-4511-IDHTYP-X.                                                 
032200         05  W-4511-IDHTYP       PIC  X(04) VALUE '4511'.                 
032300         05  W-LOW-VALUE         PIC  X(26) VALUE LOW-VALUE.              
032400                                                                          
032500     03  W-4512-KDTPOTYP-X.                                               
032600         05  W-4512-KDTPOTYP     PIC  S9(1) COMP-3.                       
032700     03  W-4512-KDORDKL-X.                                                
032800         05  W-4512-KDORDKL      PIC  S9(1) COMP-3.                       
032900     03  W-4512-IDDISTR-FOM-X.                                            
033000         05  W-4512-IDDISTR-FOM  PIC  S9(5) COMP-3.                       
033100     03  W-4512-IDDISTR-TOM-X.                                            
033200         05  W-4512-IDDISTR-TOM  PIC  S9(5) COMP-3.                       
033300                                                                          
033400 01  FILLER                    PIC X(16) VALUE 'ALT2191-IO-AREA'.         
033500 01  ALT2191-IO-AREA.                                                     
033600  03     ALT2191-LL               PIC S9(4) COMP SYNC.                    
033700  03     ALT2191-Z1               PIC X(1)  VALUE LOW-VALUE.              
033800  03     ALT2191-Z2               PIC X(1)  VALUE LOW-VALUE.              
033900  03     ALT2191-TRANSKOD         PIC X(8)  VALUE 'W2T191X '.             
034000  03     ALT2191-IDTRANS          PIC X(4)  VALUE '4160'.                 
034100  03     ALT2191-SPRAK            PIC X(1).                               
034200* 03     MID -COPY W2I19101   -PRE ALT2191-                               
034300     EJECT                                                                
034400*                                                                         
034500*    --- STATUS-KOD FRÅN IMS                                              
034600 01  STATUS-WS                   PIC XX.                                  
034700     88  SEGMENT-FINNS                       VALUE '  '.                  
034800     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
034900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
035000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
035100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
035200     88  IMS-EJ-OK                           VALUE 'XD'.                  
035300     SKIP2                                                                
035400 01  GODK-STATUSKODER.                                                    
035500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035600     SKIP3                                                                
035700 01  SSA1                        PIC X(128).                              
035800 01  SSA2                        PIC X(96).                               
035900 01  SSA3                        PIC X(64).                               
036000     EJECT                                                                
036100*    --- IMS FUNKTIONSKODER                                               
036200*01  -COPY W0003                                                          
036300     EJECT                                                                
036400*                                                                         
036500 01  FILLER                      PIC X(13)  VALUE 'TABENTRY-PARM'.        
036600 01  TABENTRY-PARM.                                                       
036700     03  RINT-STEGLNGD           PIC S9(9)   COMP.                        
036800     03  RINT-ANTAL              PIC S9(9)   COMP.                        
036900     03  RINT-NKLLNGD            PIC S9(9)   COMP.                        
037000     03  RINT-TABMAX             PIC S9(9)   COMP.                        
037100*                                                                         
037200 01  FILLER                      PIC X(10)  VALUE 'RASA-E-TAB'.           
037300 01  RASA-E-TAB.                                                          
037400     03 RASA-E-RAD        OCCURS 175                                      
037500                          INDEXED BY E-INDX.                              
037600       05 RASA-E-NKL.                                                     
037700         07 RASA-E-KDERS        PIC S9(3)   COMP-3.                       
037800         07 RASA-E-IDARTNR      PIC S9(9)   COMP-3.                       
037900         07 RASA-E-TISTODAT     PIC S9(7)   COMP-3.                       
038000       05 RASA-E-REANTPSA       PIC S99V999 COMP-3.                       
038100       05 RASA-E-KDISATS        PIC X.                                    
038200*                                                                         
038300 01  FILLER                      PIC X(10) VALUE 'RASA-T-TAB'.            
038400 01  RASA-T-TAB.                                                          
038500     03 RASA-T-RAD        OCCURS 175                                      
038600                          INDEXED BY T-INDX.                              
038700       05 RASA-T-NKL.                                                     
038800         07 RASA-T-KDERS        PIC S9(3)   COMP-3.                       
038900         07 RASA-T-IDARTNR      PIC S9(9)   COMP-3.                       
039000         07 RASA-T-TISTADAT     PIC S9(7)   COMP-3.                       
039100       05 RASA-T-REANTPSA       PIC S99V999 COMP-3.                       
039200       05 RASA-T-KDISATS        PIC X.                                    
039300       05 RASA-T-KDSTRRAD       PIC X.                                    
039400       05 RASA-T-ANV            PIC X.                                    
039500*                                                                         
039600 01  FILLER                      PIC X(10)  VALUE 'ARB-E-TAB '.           
039700 01  ARB-E-TAB.                                                           
039800     03 ARB-E-IDARTNR           PIC S9(9)   COMP-3.                       
039900     03 ARB-E-REANTPSA-TOT      PIC S99V999 COMP-3.                       
040000     03 ARB-E-KDERS             PIC S9(3)   COMP-3.                       
040100     03 ARB-E-KDISATS           PIC X.                                    
040200     03 ARB-E-RAD        OCCURS 15                                        
040300                         INDEXED BY ARB-E-IDX.                            
040400       05 ARB-E-REANTPSA        PIC S99V999 COMP-3.                       
040500       05 ARB-E-TISTODAT        PIC S9(7)   COMP-3.                       
040600*                                                                         
040700 01  FILLER                      PIC X(10) VALUE 'ARB-T-TAB '.            
040800*                                                                         
040900 01  ARB-T-TAB.                                                           
041000     03 ARB-T-RAD        OCCURS 15                                        
041100                         INDEXED BY ARB-T-IDX.                            
041200       05 ARB-T-NKL.                                                      
041300         07 ARB-T-KDERS        PIC S9(3)   COMP-3.                        
041400         07 ARB-T-IDARTNR      PIC S9(9)   COMP-3.                        
041500         07 ARB-T-TISTADAT     PIC S9(7)   COMP-3.                        
041600       05 ARB-T-REANTPSA       PIC S99V999 COMP-3.                        
041700       05 ARB-T-KDISATS        PIC X.                                     
041800       05 ARB-T-KDSTRRAD       PIC X.                                     
041900*                                                                         
042000 01  FILLER                      PIC X(8)    VALUE 'SATS-TAB'.            
042100 01  SATS-TAB.                                                            
042200     03 SATS-HUV-KVBEART         PIC S9(7)   COMP-3.                      
042300     03 SATS-HUV-KDSATKMB        PIC X.                                   
042400     03 SATS-RADER.                                                       
042500        05 SATS-RAD          OCCURS 250                                   
042600                             INDEXED BY SATS-IDX.                         
042700           07 SATS-IDARTNR       PIC S9(9)   COMP-3.                      
042800           07 SATS-KDCLAGER      PIC S9      COMP-3.                      
042900           07 SATS-KDSATAND      PIC X.                                   
043000           07 SATS-KDSATKMB      PIC X.                                   
043100           07 SATS-KDERS         PIC S9(3)   COMP-3.                      
043200           07 SATS-KDSTRRAD      PIC X.                                   
043300           07 SATS-KVSATRES      PIC S9(7)   COMP-3.                      
043400           07 SATS-REANTPSA      PIC S99V999 COMP-3.                      
043500           07 SATS-REBEART       PIC S9(7)   COMP-3.                      
043600           07 SATS-NYKDSATAND    PIC X.                                   
043700           07 SATS-NYREANTPSA    PIC S99V999 COMP-3.                      
043800           07 SATS-NYKDSATKMB    PIC X.                                   
043900           07 SATS-NYREBEART     PIC S9(7)   COMP-3.                      
044000           07 SATS-NYKVSATROS    PIC S9(7)   COMP-3.                      
044100           07 SATS-SPARR         PIC X.                                   
044200*                                                                         
044300 01  FILLER                      PIC X(8)    VALUE 'WDD7-TAB'.            
044400 01  WDD7-TAB.                                                            
044500     03 WDD7-DIERS-ERS           PIC S9(4)V999 COMP-3.                    
044600     03 WDD7-RAD          OCCURS 10                                       
044700                          INDEXED BY WDD7-IDX.                            
044800        05 WDD7-IDARTNR          PIC S9(9)     COMP-3.                    
044900        05 WDD7-DIERS-TILLK      PIC S9(4)V999 COMP-3.                    
045000        05 WDD7-ANV              PIC X.                                   
045100*                                                                         
045200 01  FILLER                      PIC X(10)  VALUE 'E-TOM'.                
045300 01  TOM-RASA-E-TAB.                                                      
045400     03 TOM-RASA-E-RAD        OCCURS 175                                  
045500                          INDEXED BY E-TOM.                               
045600       05 TOM-RASA-E-NKL.                                                 
045700         07 TOM-RASA-E-KDERS        PIC S9(3)   COMP-3.                   
045800         07 TOM-RASA-E-IDARTNR      PIC S9(9)   COMP-3.                   
045900         07 TOM-RASA-E-TISTODAT     PIC S9(7)   COMP-3.                   
046000       05 TOM-RASA-E-REANTPSA       PIC S99V999 COMP-3.                   
046100       05 TOM-RASA-E-KDISATS        PIC X.                                
046200*                                                                         
046300 01  FILLER                      PIC X(10) VALUE 'T-TOM'.                 
046400 01  TOM-RASA-T-TAB.                                                      
046500     03 TOM-RASA-T-RAD        OCCURS 175                                  
046600                          INDEXED BY T-TOM.                               
046700       05 TOM-RASA-T-NKL.                                                 
046800         07 TOM-RASA-T-KDERS        PIC S9(3)   COMP-3.                   
046900         07 TOM-RASA-T-IDARTNR      PIC S9(9)   COMP-3.                   
047000         07 TOM-RASA-T-TISTADAT     PIC S9(7)   COMP-3.                   
047100       05 TOM-RASA-T-REANTPSA       PIC S99V999 COMP-3.                   
047200       05 TOM-RASA-T-KDISATS        PIC X.                                
047300       05 TOM-RASA-T-KDSTRRAD       PIC X.                                
047400       05 TOM-RASA-T-ANV            PIC X.                                
047500*                                                                         
047600 01  FILLER                      PIC X(10)  VALUE 'ARB-E-TOM '.           
047700 01  TOM-ARB-E-TAB.                                                       
047800     03 TOM-ARB-E-IDARTNR           PIC S9(9)   COMP-3.                   
047900     03 TOM-ARB-E-REANTPSA-TOT      PIC S99V999 COMP-3.                   
048000     03 TOM-ARB-E-KDERS             PIC S9(3)   COMP-3.                   
048100     03 TOM-ARB-E-KDISATS           PIC X.                                
048200     03 TOM-ARB-E-RAD        OCCURS 15                                    
048300                         INDEXED BY ARB-E-TOM.                            
048400       05 TOM-ARB-E-REANTPSA        PIC S99V999 COMP-3.                   
048500       05 TOM-ARB-E-TISTODAT        PIC S9(7)   COMP-3.                   
048600*                                                                         
048700 01  FILLER                      PIC X(10) VALUE 'ARB-T-TOM '.            
048800*                                                                         
048900 01  TOM-ARB-T-TAB.                                                       
049000     03 TOM-ARB-T-RAD        OCCURS 15                                    
049100                         INDEXED BY ARB-T-TOM.                            
049200       05 TOM-ARB-T-NKL.                                                  
049300         07 TOM-ARB-T-KDERS        PIC S9(3)   COMP-3.                    
049400         07 TOM-ARB-T-IDARTNR      PIC S9(9)   COMP-3.                    
049500         07 TOM-ARB-T-TISTADAT     PIC S9(7)   COMP-3.                    
049600       05 TOM-ARB-T-REANTPSA       PIC S99V999 COMP-3.                    
049700       05 TOM-ARB-T-KDISATS        PIC X.                                 
049800       05 TOM-ARB-T-KDSTRRAD       PIC X.                                 
049900*                                                                         
050000 01  FILLER                      PIC X(8)   VALUE 'SATS-TOM'.             
050100 01  TOM-SATS-TAB.                                                        
050200     03 TOM-SATS-HUV-KVBEART         PIC S9(7)   COMP-3.                  
050300     03 TOM-SATS-HUV-KDSATKMB        PIC X.                               
050400     03 TOM-SATS-RADER.                                                   
050500        05 TOM-SATS-RAD          OCCURS 250                               
050600                             INDEXED BY SATS-TOM.                         
050700           07 TOM-SATS-IDARTNR       PIC S9(9)   COMP-3.                  
050800           07 TOM-SATS-KDCLAGER      PIC S9      COMP-3.                  
050900           07 TOM-SATS-KDSATAND      PIC X.                               
051000           07 TOM-SATS-KDSATKMB      PIC X.                               
051100           07 TOM-SATS-KDERS         PIC S9(3)   COMP-3.                  
051200           07 TOM-SATS-KDSTRRAD      PIC X.                               
051300           07 TOM-SATS-KVSATRES      PIC S9(7)   COMP-3.                  
051400           07 TOM-SATS-REANTPSA      PIC S99V999 COMP-3.                  
051500           07 TOM-SATS-REBEART       PIC S9(7)   COMP-3.                  
051600           07 TOM-SATS-NYKDSATAND    PIC X.                               
051700           07 TOM-SATS-NYREANTPSA    PIC S99V999 COMP-3.                  
051800           07 TOM-SATS-NYKDSATKMB    PIC X.                               
051900           07 TOM-SATS-NYREBEART     PIC S9(7)   COMP-3.                  
052000           07 TOM-SATS-NYKVSATROS    PIC S9(7)   COMP-3.                  
052100           07 TOM-SATS-SPARR         PIC X.                               
052200*                                                                         
052300 01  FILLER                      PIC X(8)    VALUE 'WDD7-TOM'.            
052400 01  TOM-WDD7-TAB.                                                        
052500     03 TOM-WDD7-DIERS-ERS           PIC S9(4)V999 COMP-3.                
052600     03 TOM-WDD7-RAD          OCCURS 10                                   
052700                          INDEXED BY WDD7-TOM.                            
052800        05 TOM-WDD7-IDARTNR          PIC S9(9)     COMP-3.                
052900        05 TOM-WDD7-DIERS-TILLK      PIC S9(4)V999 COMP-3.                
053000        05 TOM-WDD7-ANV              PIC X.                               
053100*                                                                         
053200 01  FILLER                      PIC X(13)  VALUE 'TOM-SPAR-KMB '.        
053300 01  TOM-SPAR-KDSATKMB-TAB.                                               
053400     03  TOM-SPAR-KDSATKMB           OCCURS 26                            
053500                                     INDEXED BY SPAR-KOMB-TOM.            
053600         05  TOM-SPAR-KDSATKMB-KOD   PIC X.                               
053700*                                                                         
053800 01  FILLER                      PIC X(12)   VALUE 'WS-KDSATKOMB'.        
053900 01  WS-KDSATKOMB.                                                        
054000     03  FILLER                  PIC X(13) VALUE 'ABCDEFGHIJKLM'.         
054100     03  FILLER                  PIC X(13) VALUE 'NOPQRSTUVWXYZ'.         
054200 01  FILLLER REDEFINES WS-KDSATKOMB.                                      
054300     03 KOMB-TAB-RAD      OCCURS 26                                       
054400                          INDEXED BY KOMB-IDX.                            
054500        05 KOMB-BOKSTAV   PIC X.                                          
054600*                                                                         
054700 01  FILLER                      PIC X(13)  VALUE 'SPAR-KDSATKMB'.        
054800 01  SPAR-KDSATKMB-TAB.                                                   
054900     03  SPAR-KDSATKMB           OCCURS 26                                
055000                                 INDEXED BY SPAR-KOMB-IDX.                
055100         05  SPAR-KDSATKMB-KOD   PIC X.                                   
055200     EJECT                                                                
055300*    ---  COPY-AREOR TILL SUBPGM                                          
055400 01  DATUMKORT-ID                PIC X(6)   VALUE 'WDATUM'.               
055500*01  -COPY WDATKORT                                                       
055600*                                                                         
055700     SKIP3                                                                
055800 01  FILLER                      PIC X(9)   VALUE 'DATUMKONV'.            
055900*01  -COPY WDATAREA                                                       
056000*                                                                         
056100     SKIP3                                                                
056200 01  FILLER                      PIC X(8)   VALUE 'W416PTID'.             
056300*01  -COPY W416PTID                                                       
056400     EJECT                                                                
056500******************************************************************        
056600*                                                                         
056700*        ARBETS-AREOR TILL IO-AREORNA                                     
056800*                                                                         
056900*    ---  DLI INPUT-OUTPUT AREA 1                                         
057000*    ---  DLI-IO-AREA                                                     
057100                                                                          
057200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-SATG01'.            
057300 01  IO-AREA-SATG01.                                                      
057400*    03  WLSATG01  -COPY WDJ201                                           
057500     EJECT                                                                
057600                                                                          
057700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-SATG11'.            
057800 01  IO-AREA-SATG11.                                                      
057900*    03  WLSATG11  -COPY WDJ211                                           
058000     EJECT                                                                
058100                                                                          
058200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-SATB01'.            
058300 01  IO-AREA-SATB01.                                                      
058400*    03  WLSATB01  -COPY WDJ101  -PRE SATB-                               
058500     EJECT                                                                
058600                                                                          
058700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-SATB11'.            
058800 01  IO-AREA-SATB11.                                                      
058900*    03  WLSATB11  -COPY WDJ111  -PRE SATB-                               
059000     EJECT                                                                
059100                                                                          
059200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ERSA01'.            
059300 01  IO-AREA-ERSA01.                                                      
059400*    03  WLERSA01  -COPY WDD701  -PRE ERSA-                               
059500     EJECT                                                                
059600                                                                          
059700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ERSA11'.            
059800 01  IO-AREA-ERSA11.                                                      
059900*    03  WLERSA11  -COPY WDD702  -PRE ERSA-                               
060000     EJECT                                                                
060100                                                                          
060200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORDP01'.            
060300 01  IO-AREA-ORDP01.                                                      
060400*    03  WLORDP01  -COPY WDA501     -PRE ORDP-                            
060500     EJECT                                                                
060600                                                                          
060700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORDQ01'.            
060800 01  IO-AREA-ORDQ01.                                                      
060900*    03  WLORDQ01  -COPY WDA5A1     -PRE ORDQ-                            
061000     EJECT                                                                
061100                                                                          
061200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ORDR01'.            
061300 01  IO-AREA-ORDR01.                                                      
061400*    03  WLORDR01  -COPY WDA5B1     -PRE ORDR-                            
061500     EJECT                                                                
061600                                                                          
061700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTM01'.            
061800 01  IO-AREA-ARTM01.                                                      
061900*    03  WLARTM01  -COPY WDK901                                           
062000     EJECT                                                                
062100                                                                          
062200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC01'.            
062300 01  IO-AREA-ARTC01.                                                      
062400*    03  WLARTC01  -COPY WDK601                                           
062500     EJECT                                                                
062600                                                                          
062700 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
062800 01  IO-AREA-ARTC11.                                                      
062900*    03  WLARTC11  -COPY WDK611                                           
063000     EJECT                                                                
063100                                                                          
063200 01  FILLER                 PIC X(16)  VALUE 'DLI-J211-SATG1'.            
063300 01  IO-AREA-WDJ211-SATG1.                                                
063400*    03  WLSATG11  -COPY WDJ211     -PRE SATG1-                           
063500     EJECT                                                                
063600 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-XXJN11'.            
063700 01  IO-AREA-XXJN11.                                                      
063800*    03  WLXXJN11  -COPY WDGX4512                                         
063900     EJECT                                                                
064000*    MSG-AREA FÖR HOPP TILL W20109                                        
064100 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
064200 01  W-PROG-TO-PROG-SW-1.                                                 
064300     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
064400     03  2109-Z1                   PIC X.                                 
064500     03  2109-Z2                   PIC X.                                 
064600     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
064700     03  2109-IDTRANS              PIC X(4)  VALUE '416 '.                
064800     03  2109-KDMFSFOR             PIC X     VALUE '1'.                   
064900*    03  -COPY W2I10902    -PRE 2109-                                     
065000     EJECT                                                                
065100 01  RUB1.                                                                
065200     03  FILLER                  PIC X(16)   VALUE SPACE.                 
065300     03  FILLER                  PIC X(16)   VALUE 'FELLISTA'.            
065400     03  FILLER                  PIC X(13)   VALUE SPACE.                 
065500     03  FILLER                  PIC X(7)    VALUE 'DATUM: '.             
065600     03  FEL-DATUM               PIC X(10)   VALUE SPACE.                 
065700     03  FILLER                  PIC X(4)    VALUE SPACE.                 
065800     03  FILLER                  PIC X(6)    VALUE 'SIDA: '.              
065900     03  FEL-SIDA                PIC 9(4).                                
066000     03  FILLER                  PIC X(4)    VALUE SPACE.                 
066100 01  RUB2.                                                                
066200     03  FILLER                  PIC X(5)    VALUE SPACE.                 
066300     03  FILLER                  PIC X(7)    VALUE 'ORDERNR'.             
066400     03  FILLER                  PIC XX      VALUE SPACE.                 
066500     03  FILLER                  PIC X(10)   VALUE 'SATS ARTNR'.          
066600     03  FILLER                  PIC XX      VALUE SPACE.                 
066700     03  FILLER                  PIC X(11)   VALUE 'ING.ARTIKEL'.         
066800     03  FILLER                  PIC X(5)    VALUE SPACE.                 
066900     03  FILLER                  PIC X(7)    VALUE 'FELTEXT'.             
067000     03  FILLER                  PIC X(31)   VALUE SPACE.                 
067100 01  FELRAD.                                                              
067200     03  FILLER                  PIC X(6)    VALUE SPACE.                 
067300     03  FEL-IDORDNSB            PIC ZZZZ9.                               
067400     03  FILLER                  PIC X       VALUE '-'.                   
067500     03  FEL-IDORDNSS            PIC 9.                                   
067600     03  FILLER                  PIC X(2)    VALUE SPACE.                 
067700     03  FEL-SATS-IDARTNR        PIC Z(8)9.                               
067800     03  FILLER                  PIC X(4)    VALUE SPACE.                 
067900     03  FEL-ING-IDARTNR         PIC Z(8)9.                               
068000     03  FILLER                  PIC X(5)    VALUE SPACE.                 
068100     03  FEL-FELTEXT             PIC X(30)   VALUE SPACE.                 
068200     03  FILLER                  PIC X(8)    VALUE SPACE.                 
068300     SKIP3                                                                
068400 01  WS-FELTEXTER.                                                        
068500     03  FELTEXT1                PIC X(35) VALUE                          
068600                         'ING. ARTIKEL SAKNAS I SATSREGISTRET'.           
068700     03  FELTEXT2                PIC X(35) VALUE                          
068800                         'FELAKTIG ERSÄTTNINGSKOD            '.           
068900     03  FELTEXT3                PIC X(35) VALUE                          
069000                         'RESTORDER SAKNASFÖR ING. ARTIKEL   '.           
069100     03  FELTEXT4                PIC X(35) VALUE                          
069200                         'ING. ARTIKEL REDAN BEHANDLAD       '.           
069300 01  FILLER                    PIC X(16) VALUE 'SLUT WS W4160200'.        
069400     EJECT                                                                
069500 LINKAGE SECTION.                                                         
069600                                                                          
069700*01  -COPY W0009      -PRE MSG-                                           
069800     EJECT                                                                
069900*01  -COPY W0009      -PRE ALT2191-                                       
070000     EJECT                                                                
070100*01  -COPY W0009      -PRE 2109-                                          
070200     EJECT                                                                
070300*01  -COPY W0008      -PRE SATG-                                          
070400     05  FILLER                  PIC X.                                   
070500     EJECT                                                                
070600*01  -COPY W0008      -PRE SATG1-                                         
070700     05  FILLER                  PIC X.                                   
070800     EJECT                                                                
070900*01  -COPY W0008      -PRE SATJ-                                          
071000     05  FILLER                  PIC X.                                   
071100     EJECT                                                                
071200*01  -COPY W0008      -PRE SATK-                                          
071300     05  FILLER                  PIC X.                                   
071400     EJECT                                                                
071500*01  -COPY W0008      -PRE SATB-                                          
071600     05  FILLER                  PIC X.                                   
071700     EJECT                                                                
071800*01  -COPY W0008      -PRE ERSA-                                          
071900     05  FILLER                  PIC X.                                   
072000     EJECT                                                                
072100*01  -COPY W0008      -PRE ORDP-                                          
072200     05  FILLER                  PIC X.                                   
072300     EJECT                                                                
072400*01  -COPY W0008      -PRE ORDQ-                                          
072500     05  FILLER                  PIC X.                                   
072600     EJECT                                                                
072700*01  -COPY W0008      -PRE ORDR-                                          
072800     05  FILLER                  PIC X.                                   
072900     EJECT                                                                
073000*01  -COPY W0008      -PRE ARTC-                                          
073100     05  FILLER                  PIC X.                                   
073200     EJECT                                                                
073300*01  -COPY W0008      -PRE ARTM-                                          
073400     05  FILLER                  PIC X.                                   
073500     EJECT                                                                
073600*01  -COPY W0008      -PRE XXJN-                                          
073700     05  FILLER                  PIC X.                                   
073800     EJECT                                                                
073900*    PCB FÖR SUBPROGRAM W416PTID                                          
074000 01  XXKH-PCB                    PIC X.                                   
074100                                                                          
074200 01  XXKI-PCB                    PIC X.                                   
074300     EJECT                                                                
074400 PROCEDURE DIVISION  USING MSG-PCB                                        
074500                           ALT2191-PCB  2109-PCB                          
074600                           SATG-PCB     SATG1-PCB                         
074700                           SATJ-PCB     SATK-PCB   SATB-PCB               
074800                           ERSA-PCB     ORDP-PCB   ORDQ-PCB               
074900                           ORDR-PCB     ARTC-PCB                          
075000                           ARTM-PCB     XXJN-PCB                          
075100                           XXKH-PCB     XXKI-PCB.                         
075200                                                                          
075300     ENTRY 'DLITCBL' USING MSG-PCB                                        
075400                           ALT2191-PCB  2109-PCB                          
075500                           SATG-PCB     SATG1-PCB                         
075600                           SATJ-PCB     SATK-PCB   SATB-PCB               
075700                           ERSA-PCB     ORDP-PCB   ORDQ-PCB               
075800                           ORDR-PCB     ARTC-PCB                          
075900                           ARTM-PCB     XXJN-PCB                          
076000                           XXKH-PCB     XXKI-PCB.                         
076100                                                                          
076200     EJECT                                                                
076300                                                                          
076400     PERFORM A-INIT                                                       
076500     PERFORM B-PRIORITERA-SATSORDERKO                                     
076600     PERFORM IMS-GN-WDJ2-SATK-DSEQ-FIRST                                  
076700                                                                          
076800     PERFORM UNTIL SEGMENT-SLUT   OR                                      
076900                   SEGMENT-SAKNAS                                         
077000* ---  ITERERA PÅ SATSORDER                                               
077100                                                                          
077200       PERFORM C-SPARA-SHUV-NYCKLAR                                       
077300                                                                          
077400       PERFORM D-NOLLSTALL-WS                                             
077500                                                                          
077600       IF SHUV-FLSATNYO = NEJ                                             
077700         IF SHUV-FLSATSPR = NEJ                                           
077800* ---      BEHANDLA GAMMAL EJ SPÄRRAD SATSORDER                           
077900* ---      RADER OCH HUVUD UPPDATERAS                                     
078000           IF SHUV-FLBYGGB = NEJ                                          
078100             PERFORM E-KOLLA-RESTORDERRADER                               
078200           END-IF                                                         
078300                                                                          
078400           MOVE SHUV-IDARTNR TO W-WDJ1-IDARTNR-X                          
078500           PERFORM IMS-GU-WDJ1-SATB                                       
078600           MOVE SATB-STR-TIBORT    TO TMP1-YYMMDD                         
078700           MOVE DAGENS-DATUM-NUM   TO TMP2-YYMMDD                         
078800           PERFORM WY2000Q1                                               
078900           IF SATB-STR-TIBORT >  0 AND                                    
079000              TMP1-YYMMDD     <= TMP2-YYMMDD                              
079100* ---        STRUKTUREN ÄR BORTTAGEN UR RASA                              
079200             MOVE NEJ TO WS-FLBYGGB                                       
079300             MOVE JA  TO SW-RASA-SPARR                                    
079400             MOVE JA  TO SW-ORDER-ANDR                                    
079500           ELSE                                                           
079600             PERFORM F-BEH-BEF-RASA-STRUKTUR                              
079700             IF SW-RASA-ANDR = JA                                         
079800               PERFORM G-LAS-SATS-TILL-TABELL                             
079900               PERFORM H-SORTERA-TABELLER                                 
080000               PERFORM I-RASA-ANDR                                        
080100               PERFORM J-UPPDATERA-SATS                                   
080200             END-IF                                                       
080300           END-IF                                                         
080400           IF SW-ORDER-ANDR  = JA OR                                      
080500              SW-RORAD-TACKT = JA                                         
080600             PERFORM K-BEH-SATS-RASA-SPARR-ELLER-EJ                       
080700           END-IF                                                         
080800         END-IF                                                           
080900       ELSE                                                               
081000* ---    BEHANDLA NY SATSORDER                                            
081100         PERFORM L-BEHANDLA-NY-SATSORDER                                  
081200         PERFORM M-SATSORDER-BYGGBAR-ELLER-EJ                             
081300       END-IF                                                             
081400       PERFORM N-LAS-FRAM-AKT-SATSORDERHUVUVD                             
081500     END-PERFORM                                                          
081600     MOVE    NEJ TO SW-SATS-FEL                                           
081700                    SW-ORDER-ANDR                                         
081800     IF 2109-MID2-KVANTART > ZERO                                         
081900       PERFORM S40X-STARTA-2109                                           
082000     END-IF                                                               
082100                                                                          
082200     PERFORM Z-FINIT                                                      
082300                                                                          
082400     MOVE ZERO TO RETURN-CODE                                             
082500     GOBACK                                                               
082600     .                                                                    
082700     EJECT                                                                
082800 A-INIT SECTION.                                                          
082900                                                                          
083000     PERFORM IMS-RESTART                                                  
083100                                                                          
083200     MOVE    NEJ              TO    SW-SATS-FEL                           
083300     MOVE    JA               TO    WS-FLBYGGB                            
083400     MOVE    JA               TO    SW-ANT-BYGGB-FORSTA                   
083500     MOVE    ZERO             TO    WS-VLORDNTO-CM                        
083600     MOVE    ZERO             TO    WS-VLORDNTO-M                         
083700     MOVE    ZERO             TO    WS-VKORDNTO-1DEC                      
083800     MOVE    ZERO             TO    WS-VKORDNTO-3DEC                      
083900     MOVE    ZERO             TO    RAKNARE                               
084000     MOVE SPACE               TO    2109-MID2-W2I10902                    
084100     MOVE +1                  TO    2109-IX                               
084200                                                                          
084300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
084400                                                                          
084500     ACCEPT  DAGENS-DATUM     FROM  DATE                                  
084600     ACCEPT  WS-TIME          FROM  TIME                                  
084700     MOVE    DAGENS-DATUM-NUM TO    DAT-I-TIDATUM                         
084800     MOVE   'AAMMDD'          TO    DAT-KDDATFORM                         
084900     CALL    WDATKONV         USING DAT-KDDATFORM DAT-I-TIDATUM           
085000                                    DAT-O-TIDATUM DAT-KDSVAR              
085100                                                                          
085200     IF DAT-KDSVAR-OK                                                     
085300       MOVE DAT-TIAAP          TO   WS-DAT-TIAAP                          
085400     ELSE                                                                 
085500       DISPLAY '*** W41602  - FEL I DATKONV ***'                          
085600       DISPLAY '***   SECT A-INIT           ***'                          
085700       CALL    ABEND USING ABEND-UTAN-DUMP                                
085800     END-IF                                                               
085900                                                                          
086000     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
086100                                                                          
086200     MOVE    D-AAR            TO    GARDAGENS-DATUM-AAR                   
086300     MOVE    D-MAANAD         TO    GARDAGENS-DATUM-MAANAD                
086400     MOVE    D-DAG            TO    GARDAGENS-DATUM-DAG                   
086500                                                                          
086600     OPEN    OUTPUT FELLISTA                                              
086700     MOVE    +41              TO    WS-RADANT                             
086800                                                                          
086900     PERFORM S10-SKAPA-TOMMA-TABELLER                                     
087000                                                                          
087100     MOVE LOW-VALUE        TO W-WDJ2D-MIN-X                               
087200     MOVE HIGH-VALUE       TO W-WDJ2D-MAX-X                               
087300     .                                                                    
087400     EJECT                                                                
087500****************************************************************          
087600* B-PRIORITERA-SATSORDERKÖ                                     *          
087700* LÄSER IGENOM HELA SATSORDERKÖN,                              *          
087800* FÖR VARJE SATSORDER:                                         *          
087900*   - KONTROLLERAR RESTORDERANTAL PÅ WDA5                      *          
088000*   - SKAPAR RELSKVOT (LAGERSALDO/PERIODBEHOV)                 *          
088100*   - BYGGER UPP PRIORITERINGSNYCKELN                          *          
088200****************************************************************          
088300 B-PRIORITERA-SATSORDERKO SECTION.                                        
088400                                                                          
088500     MOVE    ZERO       TO SPAR-IDARTNR                                   
088600     MOVE    LOW-VALUE  TO W-WDJ2C-MIN-X                                  
088700     MOVE    HIGH-VALUE TO W-WDJ2C-MAX-X                                  
088800     PERFORM IMS-GN-WDJ2-SATG-CSEQ                                        
088900                                                                          
089000     PERFORM UNTIL SEGMENT-SLUT OR                                        
089100                   SEGMENT-SAKNAS                                         
089200                                                                          
089300       IF SHUV-KDSATSTA = 'R'                                             
089400         MOVE    SHUV-IDORDNSB TO W-WDJ201-IDORDNSB                       
089500         MOVE    SHUV-IDORDNSS TO W-WDJ201-IDORDNSS                       
089600         PERFORM IMS-GHU-WDJ201-SATG                                      
089700                                                                          
089800         IF SHUV-IDARTNR = SPAR-IDARTNR                                   
089900           MOVE    SPAR-KVRORAD-9KOMPL TO SHUV-KVRORAD-9KOMPL             
090000           MOVE    SPAR-RELSKVOT-L     TO SHUV-RELSKVOT-L                 
090100           MOVE    SPAR-RELSKVOT-S     TO SHUV-RELSKVOT-S                 
090200           PERFORM IMS-REPL-WDJ201-SATG                                   
090300           PERFORM S500-KOLLA-CHECKPOINT                                  
090400         ELSE                                                             
090500* ---        NYTT SATSARTIKELNR                                           
090600* ---        BERÄKNA LAGERSALDO                                           
090700           MOVE SPACE        TO SSA1 SSA2 SSA3                            
090800           MOVE ZERO         TO WS-TOTLS                                  
090900                                WS-TOTPB                                  
091000                                WS-TOTAKS                                 
091100                                WS-TOTROS                                 
091200                                                                          
091300           MOVE SHUV-IDARTNR TO SPAR-IDARTNR                              
091400           MOVE SHUV-IDARTNR TO W-IDARTNR-X                               
091500           PERFORM IMS-GU-WDK611                                          
091600           IF SEGMENT-FINNS                                               
091700             PERFORM IMS-GU-WDK9-ARTM                                     
091800           END-IF                                                         
091900                                                                          
092000           IF SEGMENT-FINNS                                               
092100                                                                          
092200             COMPUTE WS-TOTLS  =   CLAG-KVLS                              
092300                                 + CLAG-KVAKS-CDC                         
092400                                 + CLAG-KVAKS-PAV                         
092500                                 + CLAG-KVAKS-T                           
092600                                 - CLAG-KVRESS                            
092700                                 - CLAG-KVSPANT                           
092800                                 - ART-KVOKS-BULK                         
092900                                 - ART-KVOKS-DAG                          
093000                                 - ART-KVOKS-VOR                          
093100             ADD     CLAG-KVROS TO WS-TOTROS                              
093200             COMPUTE WS-TOTAKS = + CLAG-KVAKS-CDC                         
093300                                 + CLAG-KVAKS-PAV                         
093400                                 + CLAG-KVAKS-T                           
093500           END-IF                                                         
093600                                                                          
093700           IF WS-TOTLS = 0                                                
093800             MOVE ZERO TO SHUV-RELSKVOT-L                                 
093900             MOVE ZERO TO SHUV-RELSKVOT-S                                 
094000           ELSE                                                           
094100* ---           HÄMTA PERIODBEHOV                                         
094200                                                                          
094300             COMPUTE WS-TOTPB = CLAG-KVPB-SEP +                           
094400                                CLAG-KVPB-SATS                            
094500             IF WS-TOTPB = 0                                              
094600               MOVE ZERO TO SHUV-RELSKVOT-L                               
094700               MOVE ZERO TO SHUV-RELSKVOT-S                               
094800             ELSE                                                         
094900* ---           BERÄKNA RELSKVOT                                          
095000               COMPUTE WS-RELSKVOT = WS-TOTLS / WS-TOTPB                  
095100               IF WS-RELSKVOT < 0.46                                      
095200                 MOVE WS-RELSKVOT TO SHUV-RELSKVOT-L                      
095300                 MOVE 000.00      TO SHUV-RELSKVOT-S                      
095400               ELSE                                                       
095500                 MOVE 999.99      TO SHUV-RELSKVOT-L                      
095600                 MOVE WS-RELSKVOT TO SHUV-RELSKVOT-S                      
095700               END-IF                                                     
095800             END-IF                                                       
095900           END-IF                                                         
096000*                                                                         
096100*    OM RESTORDERANTALET > ANTAL I AK, SKALL                              
096200*    ANTAL RESTORDERRADER MED STATUS 2 BERÄKNAS                           
096300*                                                                         
096400           IF WS-TOTAKS <  WS-TOTROS                                      
096500              MOVE    ZERO         TO WS-RO-RAKN                          
096600              MOVE    LOW-VALUE    TO W-WDA5A1KY-MIN-X                    
096610              MOVE    HIGH-VALUE   TO W-WDA5A1KY-MAX-X                    
096700              MOVE    SHUV-IDARTNR TO W-WDA5A1-IDARTNR-MIN                
096900                                      W-WDA5A1-IDARTNR-MAX                
097000              MOVE    WC-CDC-SE    TO W-WDA5A1-IDDC-MAX                   
097100                                      W-WDA5A1-IDDC-MIN                   
097200              PERFORM IMS-GU-WDA5-ORDQ                                    
097300                                                                          
097400              PERFORM UNTIL SEGMENT-SLUT OR                               
097500                            SEGMENT-SAKNAS                                
097600                IF ORDQ-SEQA-KDSTARAD = '2'                               
097700                  COMPUTE WS-RO-RAKN = WS-RO-RAKN + 1                     
097800                END-IF                                                    
097900                PERFORM IMS-GN-WDA5-ORDQ                                  
098000              END-PERFORM                                                 
098100                                                                          
098200              COMPUTE SHUV-KVRORAD-9KOMPL = 99999 -                       
098300                                            WS-RO-RAKN                    
098400           ELSE                                                           
098500             MOVE 99999 TO SHUV-KVRORAD-9KOMPL                            
098600           END-IF                                                         
098700                                                                          
098800* --- UPDATERING AV ORDERHUVUD OCH SPAR-AREA                              
098900           PERFORM IMS-REPL-WDJ201-SATG                                   
099000           MOVE    SHUV-KVRORAD-9KOMPL TO SPAR-KVRORAD-9KOMPL             
099100           MOVE    SHUV-RELSKVOT-L     TO SPAR-RELSKVOT-L                 
099200           MOVE    SHUV-RELSKVOT-S     TO SPAR-RELSKVOT-S                 
099300           PERFORM S500-KOLLA-CHECKPOINT                                  
099400         END-IF                                                           
099500       END-IF                                                             
099600                                                                          
099700       PERFORM IMS-GN-WDJ2-SATG-CSEQ                                      
099800                                                                          
099900     END-PERFORM                                                          
100000                                                                          
100100     MOVE ZERO TO RAKNARE                                                 
100200     PERFORM IMS-CHECKPOINT                                               
100300     .                                                                    
100400     EJECT                                                                
100500 C-SPARA-SHUV-NYCKLAR SECTION.                                            
100600                                                                          
100700     DISPLAY 'XXXXXXXX'                                                   
100800     MOVE SHUV-KDCLAGER       TO W-WDJ2D-AKT-KDCLAGER                     
100900     MOVE SHUV-KVRORAD-9KOMPL TO W-WDJ2D-AKT-KVRORAD-9KOMPL               
101000     MOVE SHUV-RELSKVOT-L     TO W-WDJ2D-AKT-RELSKVOT-L                   
101100     MOVE SHUV-FLSATPRI       TO W-WDJ2D-AKT-FLSATPRI                     
101200     MOVE SHUV-RELSKVOT-S     TO W-WDJ2D-AKT-RELSKVOT-S                   
101300     MOVE SHUV-IDARTNR        TO W-WDJ2D-AKT-IDARTNR                      
101400     MOVE SHUV-DAREGDAT       TO W-WDJ2D-AKT-DAREGDAT                     
101500     MOVE SHUV-IDORDNSB       TO WS-IDORDNSB-AKT                          
101600                                 WS-IDORDNSB                              
101700     MOVE SHUV-IDORDNSS       TO WS-IDORDNSS-AKT                          
101800                                 WS-IDORDNSS                              
101900     .                                                                    
102000     EJECT                                                                
102100****************************************************************          
102200* D-NOLLSTALL-WS                                               *          
102300* DENNA SEKTION ANVÄNDS NÄR EN SATSORDER BÖRJAR BEHANDLAS.     *          
102400* ARBETSAREOR OCH FLAGGOR INITIERAS.                           *          
102500****************************************************************          
102600 D-NOLLSTALL-WS SECTION.                                                  
102700                                                                          
102800     MOVE ZERO TO WS-KVBYGGBAR                                            
102900                  WS-KVBYGGB                                              
103000                  WS-KVRADER                                              
103100                  WS-IDORDNST-NUM                                         
103200                  WS-VKORDNTO-1DEC                                        
103300                  WS-VKORDNTO-3DEC                                        
103400                  WS-VLORDNTO-CM                                          
103500                  WS-VLORDNTO-M                                           
103600                  WS-REBEART-ANNULL                                       
103700     MOVE JA   TO WS-FLBYGGB                                              
103800                  SW-ANT-BYGGB-FORSTA                                     
103900     MOVE NEJ  TO SW-ORDER-ANDR                                           
104000                  SW-SATS-FEL                                             
104100                  SW-RASA-SPARR                                           
104200                  SW-RASA-ANDR                                            
104300                  SW-SATS-TRAFF                                           
104400                  SW-ARB-E-TRAFF                                          
104500                  SW-WDD7-TRAFF                                           
104600                  SW-TACKT-RO                                             
104700                  SW-RORAD-TACKT                                          
104800                  SW-ANV-KOMBKOD-TRAFF                                    
104900                  SW-UTG                                                  
105000     .                                                                    
105100     EJECT                                                                
105200****************************************************************          
105300* E-KOLLA-RESTORDERRADER                                       *          
105400* DENNA SEKTION GÄLLER GAMLA SATSORDER                         *          
105500* PÅ DENNA SATSORDER ÄR INTE ALLA ING ART TÄCKTA               *          
105600****************************************************************          
105700 E-KOLLA-RESTORDERRADER SECTION.                                          
105800                                                                          
105900     MOVE    JA  TO WS-FLBYGGB                                            
106000     MOVE    NEJ TO SW-RORAD-TACKT                                        
106100     PERFORM IMS-GNP-WDJ2-SATK11-FIRST                                    
106200                                                                          
106300     PERFORM UNTIL SEGMENT-SLUT OR                                        
106400                   SEGMENT-SAKNAS                                         
106500       IF SRAD-KVSATROS > 0                                               
106600         MOVE SRAD-KVSATROS TO WS-KVSATROS-G                              
106700         PERFORM S125-KOLLA-TACKNING-RO                                   
106800* ---    EFTER TÄCKNING (ALLT ELLER DELVIS) FRÅN RO ÄR                    
106900* ---    SW-RORAD-TACKT = JA                                              
107000         IF SRAD-KVSATROS > 0                                             
107100           MOVE NEJ        TO WS-FLBYGGB                                  
107200         END-IF                                                           
107300       END-IF                                                             
107400                                                                          
107500       PERFORM IMS-GNP-WDJ2-SATK11-REST                                   
107600     END-PERFORM                                                          
107700     .                                                                    
107800     EJECT                                                                
107900 F-BEH-BEF-RASA-STRUKTUR SECTION.                                         
108000                                                                          
108100     MOVE SHUV-TIUPPDAT       TO TMP1-YYMMDD                              
108200     MOVE SATB-STR-TIUPPDAT   TO TMP2-YYMMDD                              
108300     PERFORM WY2000P1                                                     
108400     IF TMP1-YYMMDD < TMP2-YYMMDD                                         
108500* ---  RASA ÄR ÄNDRAD                                                     
108600       MOVE    TOM-RASA-E-TAB TO RASA-E-TAB                               
108700       MOVE    TOM-RASA-T-TAB TO RASA-T-TAB                               
108800       MOVE    TOM-SATS-TAB   TO SATS-TAB                                 
108900       PERFORM FA-LAS-RASA-TILL-TABELL                                    
109000     ELSE                                                                 
109100       IF SHUV-TIUPPDAT      = SATB-STR-TIUPPDAT AND                      
109200          SATB-STR-TIUPPDAT  = GARDAGENS-DATUM-NUM                        
109300         MOVE    TOM-RASA-E-TAB TO RASA-E-TAB                             
109400         MOVE    TOM-RASA-T-TAB TO RASA-T-TAB                             
109500         MOVE    TOM-SATS-TAB   TO SATS-TAB                               
109600         PERFORM FB-LAS-RASA-TILL-TABELL                                  
109700       END-IF                                                             
109800     END-IF                                                               
109900     .                                                                    
110000     EJECT                                                                
110100****************************************************************          
110200* FA-LAS-RASA-TILL-TABELL                                      *          
110300* LÄSER RASA-STRUKTUREN FÖR AKTUELL SATSORDER. ÄNDRADE RADER   *          
110400* LÄGGS I INTERNTABELLER I WORKING-STORAGE                     *          
110500****************************************************************          
110600 FA-LAS-RASA-TILL-TABELL SECTION.                                         
110700                                                                          
110800     MOVE    JA     TO SW-RASA-ANDR                                       
110900     SET     E-INDX                                                       
111000             T-INDX TO +1                                                 
111100     PERFORM IMS-GNP-WDJ1-SATB                                            
111200                                                                          
111300     PERFORM UNTIL SEGMENT-SAKNAS                                         
111400                                                                          
111500       IF E-INDX > 175 OR                                                 
111600          T-INDX > 175                                                    
111700         DISPLAY '*************************************'                  
111800         DISPLAY '** INDEX HAR ÖVERSKRIDIT MAX-ANTAL **'                  
111900         DISPLAY '** I RASA-TABELL                   **'                  
112000         DISPLAY '** SECT- FA-                       **'                  
112100         DISPLAY '*************************************'                  
112200         CALL ABEND USING ABEND-UTAN-DUMP                                 
112300       END-IF                                                             
112400                                                                          
112500       MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                            
112600       MOVE DAGENS-DATUM-NUM    TO TMP2-YYMMDD                            
112700       MOVE SHUV-TIUPPDAT       TO TMP3-YYMMDD                            
112800       MOVE SATB-RAD-TISTADAT   TO TMP4-YYMMDD                            
112900       MOVE GARDAGENS-DATUM-NUM TO TMP5-YYMMDD                            
113000       PERFORM WY2000Q1                                                   
113100       IF   TMP1-YYMMDD       <= TMP2-YYMMDD     AND                      
113200            TMP1-YYMMDD       >  TMP3-YYMMDD     AND                      
113300            TMP1-YYMMDD       >  TMP4-YYMMDD     OR                       
113400            TMP1-YYMMDD       =  TMP5-YYMMDD     AND                      
113500            TMP1-YYMMDD       >= TMP3-YYMMDD     AND                      
113600            TMP1-YYMMDD       >  TMP4-YYMMDD                              
113700                                                                          
113800         IF  SATB-RAD-KDISATS = 'U' OR  SPACE                             
113900             MOVE SATB-RAD-IDARTNR  TO RASA-E-IDARTNR (E-INDX)            
114000             MOVE SATB-RAD-REANTPSA TO RASA-E-REANTPSA(E-INDX)            
114100             MOVE SATB-RAD-TISTODAT TO RASA-E-TISTODAT(E-INDX)            
114200             MOVE SATB-RAD-KDISATS  TO RASA-E-KDISATS (E-INDX)            
114300           IF SATB-RAD-KDISATS = 'U'                                      
114400             MOVE ZERO   TO RASA-E-KDERS(E-INDX)                          
114500             SET  E-INDX UP BY +1                                         
114600           ELSE                                                           
114700* ---        HÄMTA EV ERSÄTTNINGSKOD                                      
114800             MOVE    SATB-RAD-IDARTNR TO W-IDARTNR                        
114900             PERFORM IMS-GU-WDK611                                        
115000             MOVE    CLAG-KDERS TO RASA-E-KDERS(E-INDX)                   
115100             SET     E-INDX UP BY +1                                      
115200           END-IF                                                         
115300         END-IF                                                           
115400       ELSE                                                               
115500         MOVE SATB-RAD-TISTADAT   TO TMP1-YYMMDD                          
115600         MOVE SHUV-TIUPPDAT       TO TMP2-YYMMDD                          
115700         MOVE DAGENS-DATUM-NUM    TO TMP3-YYMMDD                          
115800         MOVE SATB-RAD-TISTODAT   TO TMP4-YYMMDD                          
115900         MOVE GARDAGENS-DATUM-NUM TO TMP5-YYMMDD                          
116000         PERFORM WY2000Q1                                                 
116100         IF TMP1-YYMMDD >  TMP2-YYMMDD     AND                            
116200            TMP1-YYMMDD <= TMP3-YYMMDD     AND                            
116300            TMP4-YYMMDD >= TMP3-YYMMDD     OR                             
116400            TMP1-YYMMDD =  TMP2-YYMMDD     AND                            
116500            TMP1-YYMMDD =  TMP5-YYMMDD     AND                            
116600            TMP4-YYMMDD >= TMP3-YYMMDD                                    
116700           IF SATB-RAD-KDISATS = 'N' OR SPACE                             
116800             MOVE SATB-RAD-IDARTNR  TO RASA-T-IDARTNR  (T-INDX)           
116900             MOVE SATB-RAD-REANTPSA TO RASA-T-REANTPSA (T-INDX)           
117000             MOVE SATB-RAD-TISTADAT TO RASA-T-TISTADAT (T-INDX)           
117100             MOVE SATB-RAD-KDISATS  TO RASA-T-KDISATS  (T-INDX)           
117200             MOVE SATB-RAD-KDSTRRAD TO RASA-T-KDSTRRAD (T-INDX)           
117300             SET  T-INDX            UP BY +1                              
117400           END-IF                                                         
117500         END-IF                                                           
117600       END-IF                                                             
117700       PERFORM IMS-GNP-WDJ1-SATB                                          
117800     END-PERFORM                                                          
117900                                                                          
118000     IF (E-INDX = +1 AND                                                  
118100         T-INDX = +1)                                                     
118200       MOVE NEJ TO SW-RASA-ANDR                                           
118300     END-IF                                                               
118400     .                                                                    
118500     EJECT                                                                
118600****************************************************************          
118700* FB-LAS-RASA-TILL-TABELL                                      *          
118800* LÄSER RASA-STRUKTUREN FÖR AKTUELL SATSORDER. ÄNDRADE RADER   *          
118900* LÄGGS I INTERNTABELLER I WORKING-STORAGE                     *          
119000****************************************************************          
119100 FB-LAS-RASA-TILL-TABELL SECTION.                                         
119200                                                                          
119300     MOVE    JA     TO SW-RASA-ANDR                                       
119400     SET     E-INDX                                                       
119500             T-INDX TO +1                                                 
119600     PERFORM IMS-GNP-WDJ1-SATB                                            
119700                                                                          
119800     PERFORM UNTIL SEGMENT-SAKNAS                                         
119900                                                                          
120000       IF E-INDX > 175 OR                                                 
120100          T-INDX > 175                                                    
120200         DISPLAY '*************************************'                  
120300         DISPLAY '** INDEX HAR ÖVERSKRIDIT MAX-ANTAL **'                  
120400         DISPLAY '** I RASA-TABELL                   **'                  
120500         DISPLAY '** SECT- FB-                       **'                  
120600         DISPLAY '*************************************'                  
120700         CALL ABEND USING ABEND-UTAN-DUMP                                 
120800       END-IF                                                             
120900                                                                          
121000       MOVE SATB-RAD-TISTODAT    TO TMP1-YYMMDD                           
121100       MOVE SATB-RAD-TISTADAT    TO TMP2-YYMMDD                           
121200       MOVE GARDAGENS-DATUM-NUM  TO TMP3-YYMMDD                           
121300       PERFORM WY2000Q1                                                   
121400       IF  TMP1-YYMMDD = TMP3-YYMMDD AND                                  
121500           TMP1-YYMMDD > TMP2-YYMMDD                                      
121600         IF  SATB-RAD-KDISATS = 'U'                                       
121700            MOVE SATB-RAD-IDARTNR  TO RASA-E-IDARTNR(E-INDX)              
121800            MOVE SATB-RAD-REANTPSA TO RASA-E-REANTPSA(E-INDX)             
121900            MOVE SATB-RAD-TISTODAT TO RASA-E-TISTODAT(E-INDX)             
122000            MOVE SATB-RAD-KDISATS  TO RASA-E-KDISATS(E-INDX)              
122100            SET  E-INDX            UP BY +1                               
122200         END-IF                                                           
122300       ELSE                                                               
122400         MOVE SATB-RAD-TISTADAT    TO TMP1-YYMMDD                         
122500         MOVE SATB-RAD-TISTODAT    TO TMP2-YYMMDD                         
122600         MOVE GARDAGENS-DATUM-NUM  TO TMP3-YYMMDD                         
122700         PERFORM WY2000P1                                                 
122800         IF  TMP1-YYMMDD = TMP3-YYMMDD AND                                
122900             TMP1-YYMMDD < TMP2-YYMMDD                                    
123000           IF  SATB-RAD-KDISATS = 'N'                                     
123100             MOVE SATB-RAD-IDARTNR  TO RASA-T-IDARTNR  (T-INDX)           
123200             MOVE SATB-RAD-REANTPSA TO RASA-T-REANTPSA (T-INDX)           
123300             MOVE SATB-RAD-TISTADAT TO RASA-T-TISTADAT (T-INDX)           
123400             MOVE SATB-RAD-KDISATS  TO RASA-T-KDISATS  (T-INDX)           
123500             MOVE SATB-RAD-KDSTRRAD TO RASA-T-KDSTRRAD (T-INDX)           
123600             SET  T-INDX            UP BY +1                              
123700           END-IF                                                         
123800         END-IF                                                           
123900       END-IF                                                             
124000       PERFORM IMS-GNP-WDJ1-SATB                                          
124100     END-PERFORM                                                          
124200                                                                          
124300     IF (E-INDX = +1 AND                                                  
124400         T-INDX = +1)                                                     
124500       MOVE NEJ TO SW-RASA-ANDR                                           
124600     END-IF                                                               
124700     .                                                                    
124800     EJECT                                                                
124900****************************************************************          
125000* G-LAS-SATS-TILL-TABELL                                       *          
125100* LÄSER IN HELA SATS-STRUKTUREN TILL EN INTERNTABELL I         *          
125200* WORKING-STORAGE                                              *          
125300****************************************************************          
125400 G-LAS-SATS-TILL-TABELL SECTION.                                          
125500                                                                          
125600     MOVE SHUV-KVBEART  TO SATS-HUV-KVBEART                               
125700     MOVE SHUV-KDSATKMB TO SATS-HUV-KDSATKMB                              
125800                                                                          
125900     SET SATS-IDX       TO +1                                             
126000     PERFORM IMS-GNP-WDJ2-SATK11-FIRST                                    
126100                                                                          
126200     PERFORM UNTIL SEGMENT-SLUT OR                                        
126300                   SEGMENT-SAKNAS                                         
126400                                                                          
126500       IF SATS-IDX > 250                                                  
126600         DISPLAY '*************************************'                  
126700         DISPLAY '** INDEX HAR ÖVERSKRIDIT MAX-ANTAL **'                  
126800         DISPLAY '** I SATS-TABELL                   **'                  
126900         DISPLAY '** SECT- E-                        **'                  
127000         DISPLAY '*************************************'                  
127100         CALL ABEND USING ABEND-UTAN-DUMP                                 
127200       END-IF                                                             
127300                                                                          
127400       MOVE SRAD-IDARTNR        TO SATS-IDARTNR(SATS-IDX)                 
127500       MOVE SRAD-KDCLAGER       TO SATS-KDCLAGER(SATS-IDX)                
127600       MOVE SRAD-KDSATAND       TO SATS-KDSATAND(SATS-IDX)                
127700       MOVE SRAD-KDSATKMB       TO SATS-KDSATKMB(SATS-IDX)                
127800       MOVE SRAD-KVSATRES       TO SATS-KVSATRES(SATS-IDX)                
127900       MOVE SRAD-REANTPSA       TO SATS-REANTPSA(SATS-IDX)                
128000       MOVE SRAD-REBEART        TO SATS-REBEART(SATS-IDX)                 
128100       MOVE SPACE               TO SATS-NYKDSATAND(SATS-IDX)              
128200       MOVE ZERO                TO SATS-NYREANTPSA(SATS-IDX)              
128300       MOVE ZERO                TO SATS-NYREBEART(SATS-IDX)               
128400       MOVE ZERO                TO SATS-NYKVSATROS(SATS-IDX)              
128500       MOVE SPACE               TO SATS-SPARR(SATS-IDX)                   
128600                                                                          
128700       SET SATS-IDX UP            BY +1                                   
128800       PERFORM IMS-GNP-WDJ2-SATK11-REST                                   
128900     END-PERFORM                                                          
129000     .                                                                    
129100     EJECT                                                                
129200****************************************************************          
129300* H-SORTERA-TABELLER                                           *          
129400* SORTERAR RASA-E-TABELL PÅ:                                   *          
129500*   ERSKOD, IDARTNR, STOPPDAT                                  *          
129600* SORTERAR RASA-T-TABELL PÅ:                                   *          
129700*   ERSKOD, IDARTNR, STARTDAT                                  *          
129800* SORTERAR RASA-TABELL PÅ:                                     *          
129900*   IDARTNR                                                    *          
130000* WORKING-STORAGE                                              *          
130100****************************************************************          
130200 H-SORTERA-TABELLER SECTION.                                              
130300                                                                          
130400     SET E-INDX                                                           
130500         T-INDX                                                           
130600         SATS-IDX            DOWN BY +1                                   
130700                                                                          
130800*********** FIX FÖR ATT KLARA SEKELSKIFTET ******************             
130900*********** ÅR < 50 BLIR ÅR + 50           ******************             
131000*********** ÅR > 50 BLIR ÅR - 50           ******************             
131100     MOVE 1                  TO   Y2K-IX                                  
131200     PERFORM UNTIL Y2K-IX > E-INDX                                        
131300       MOVE RASA-E-TISTODAT (Y2K-IX) TO TMP1-YYMMDD                       
131400       PERFORM WY2000P1                                                   
131500       MOVE TMP1-YYMMDD      TO   RASA-E-TISTODAT (Y2K-IX)                
131600       ADD 1                 TO   Y2K-IX                                  
131700     END-PERFORM                                                          
131800                                                                          
131900* --- SORTERA RASA-E-TABELL                                               
132000     MOVE +15                TO   RINT-STEGLNGD                           
132100     SET  RINT-ANTAL         TO   E-INDX                                  
132200     MOVE +11                TO   RINT-NKLLNGD                            
132300     CALL WINTSOR USING RASA-E-TAB RINT-STEGLNGD RINT-ANTAL               
132400          RASA-E-NKL(1) RINT-NKLLNGD                                      
132500                                                                          
132600*********** FIX FÖR ATT KLARA SEKELSKIFTET ******************             
132700*********** ÅTERSTÄLLER DATUMEN            ******************             
132800     MOVE 1                  TO   Y2K-IX                                  
132900     PERFORM UNTIL Y2K-IX > E-INDX                                        
133000       MOVE RASA-E-TISTODAT (Y2K-IX) TO TMP1-YYMMDD                       
133100       PERFORM WY2000P1                                                   
133200       MOVE TMP1-YYMMDD      TO   RASA-E-TISTODAT (Y2K-IX)                
133300       ADD 1                 TO   Y2K-IX                                  
133400     END-PERFORM                                                          
133500                                                                          
133600*********** FIX FÖR ATT KLARA SEKELSKIFTET ******************             
133700*********** ÅR < 50 BLIR ÅR + 50           ******************             
133800*********** ÅR > 50 BLIR ÅR - 50           ******************             
133900     MOVE 1                  TO   Y2K-IX                                  
134000     PERFORM UNTIL Y2K-IX > T-INDX                                        
134100       MOVE RASA-T-TISTADAT (Y2K-IX) TO TMP1-YYMMDD                       
134200       PERFORM WY2000P1                                                   
134300       MOVE TMP1-YYMMDD      TO   RASA-T-TISTADAT (Y2K-IX)                
134400       ADD 1                 TO   Y2K-IX                                  
134500     END-PERFORM                                                          
134600                                                                          
134700* --- SORTERA RASA-T-TABELL                                               
134800     MOVE +17                TO   RINT-STEGLNGD                           
134900     SET RINT-ANTAL          TO   T-INDX                                  
135000     MOVE +11                TO   RINT-NKLLNGD                            
135100     CALL WINTSOR USING RASA-T-TAB RINT-STEGLNGD RINT-ANTAL               
135200          RASA-T-NKL(1) RINT-NKLLNGD                                      
135300                                                                          
135400*********** FIX FÖR ATT KLARA SEKELSKIFTET ******************             
135500*********** ÅTERSTÄLLER DATUMEN            ******************             
135600     MOVE 1                  TO   Y2K-IX                                  
135700     PERFORM UNTIL Y2K-IX > T-INDX                                        
135800       MOVE RASA-T-TISTADAT (Y2K-IX) TO TMP1-YYMMDD                       
135900       PERFORM WY2000P1                                                   
136000       MOVE TMP1-YYMMDD      TO   RASA-T-TISTADAT (Y2K-IX)                
136100       ADD 1                 TO   Y2K-IX                                  
136200     END-PERFORM                                                          
136300     .                                                                    
136400     EJECT                                                                
136500****************************************************************          
136600* I-RASA-ANDR                                                  *          
136700* DENNA SEKTION GÄLLER GAMLA SATSARTIKLAR, DÄR                 *          
136800* RASA HAR BLIVIT FÖRÄNDRAD EFTER FÖREGÅENDE KÖRNING           *          
136900****************************************************************          
137000 I-RASA-ANDR SECTION.                                                     
137100                                                                          
137200     SET E-INDX TO +1                                                     
137300                                                                          
137400     PERFORM UNTIL E-INDX                 > 175  OR                       
137500                   RASA-E-IDARTNR(E-INDX) = ZERO OR                       
137600                   SW-SATS-FEL            = JA   OR                       
137700                   SW-RASA-SPARR          = JA                            
137800                                                                          
137900       MOVE    TOM-WDD7-TAB          TO WDD7-TAB                          
138000       MOVE    TOM-ARB-T-TAB         TO ARB-T-TAB                         
138100       MOVE    TOM-ARB-E-TAB         TO ARB-E-TAB                         
138200       MOVE    TOM-SPAR-KDSATKMB-TAB TO SPAR-KDSATKMB-TAB                 
138300       SET     ARB-E-IDX             TO +1                                
138400       PERFORM S34-SKAPA-ARB-E-TAB                                        
138500                                                                          
138600       SET     ARB-E-IDX             TO +1                                
138700       PERFORM S50-MATCHA-ARB-E-MOT-SATS                                  
138800                                                                          
138900       IF SW-SATS-TRAFF = JA                                              
139000* ---            UTGÅNGSMARKERAD ING.ARTIKEL                              
139100         IF ARB-E-KDISATS = 'U'                                           
139200           PERFORM S80-UTG-MARKERING                                      
139300         ELSE                                                             
139400* ---            "BLANK"-MARKERAD ING.ARTIKEL                             
139500           EVALUATE TRUE                                                  
139600* ---             ERSÄTTNING BEROR PÅ TILLGÅNG                            
139700             WHEN ARB-E-KDERS = 11 OR 21                                  
139800               PERFORM IA-VAL                                             
139900* ---             ERSÄTTNING OAVSETT TILLGÅNG                             
140000             WHEN ARB-E-KDERS = 22 OR 23                                  
140100               PERFORM S06-OBER-TILLG-MED-WDD7                            
140200* ---             INGEN ERSÄTTNINGSKOD                  ELLER             
140300* ---             ÄNNU EJ RASA-PÅVERKANDE ERSÄTTNINGKOD ELLER             
140400* ---             UTGÅR DIREKT                                            
140500             WHEN ARB-E-KDERS < 10 OR                                     
140600                  ARB-E-KDERS = 52                                        
140700               PERFORM S80-UTG-MARKERING                                  
140800               MOVE    'U' TO ARB-E-KDISATS                               
140900* ---             EJ ENTYDIGA ERSÄTTNINGSKODER                            
141000             WHEN ARB-E-KDERS = 14 OR 24 OR 25 OR                         
141100                          18 OR 26 OR 27 OR 28                            
141200               MOVE JA TO SW-RASA-SPARR                                   
141300* ---             FELAKTIG ERSÄTTNINGSKOD                                 
141400             WHEN RASA-E-KDERS(E-INDX) = 19 OR 29                         
141500               MOVE JA TO SW-RASA-SPARR                                   
141600           END-EVALUATE                                                   
141700         END-IF                                                           
141800       ELSE                                                               
141900         MOVE JA TO SW-RASA-SPARR                                         
142000       END-IF                                                             
142100                                                                          
142200       IF SW-RASA-SPARR = NEJ                                             
142300         SET E-INDX UP       BY +1                                        
142400       END-IF                                                             
142500                                                                          
142600     END-PERFORM                                                          
142700                                                                          
142800     IF SW-RASA-SPARR = NEJ                                               
142900        PERFORM S08-BEH-NYTILLKOMNA                                       
143000     ELSE                                                                 
143100        PERFORM S230-SPARRA-ORDERRADER                                    
143200     END-IF                                                               
143300     .                                                                    
143400     EJECT                                                                
143500****************************************************************          
143600* IA-VAL                                                       *          
143700* ERSÄTTNINGSKODEN ÄR 11 EL 21.                                *          
143800* ÄR HELA ANTALET AV DEN ING ARTIKELN RESERVERAD BOKAS DEN     *          
143900* TILLKOMMANDE ARTIKELN AV, SÅ MAN SER ATT DEN ÄR BEHANDLAD.   *          
144000* ÄR INGET RESERVERAT, BEHANDLAS DEN SOM EN ERSATT ARTIKEL.    *          
144100* ÄR ANTALET DELVIS RESERVERAT BLIR DEN "SPECIALBEHANDLAD".    *          
144200****************************************************************          
144300 IA-VAL SECTION.                                                          
144400                                                                          
144500      IF SATS-KVSATRES(SATS-IDX) > SATS-REBEART(SATS-IDX)                 
144600        MOVE JA TO SW-RASA-SPARR                                          
144700      ELSE                                                                
144800        IF SATS-KVSATRES(SATS-IDX) = SATS-REBEART(SATS-IDX)               
144900          PERFORM IAA-BER-PA-TILLGANG-BYGGB                               
145000        ELSE                                                              
145100          IF SATS-KVSATRES(SATS-IDX) = 0                                  
145200            PERFORM S06-OBER-TILLG-MED-WDD7                               
145300          ELSE                                                            
145400            PERFORM IAB-BER-PA-TILLGANG-EJBYGGB                           
145500          END-IF                                                          
145600        END-IF                                                            
145700      END-IF                                                              
145800      .                                                                   
145900      EJECT                                                               
146000****************************************************************          
146100* IAA-BER-PA-TILLGANG-BYGGB                                    *          
146200* EFTERSOM DENNA SATSRAD ÄR BYGGBAR, SÅ PÅVERKAS INTE          *          
146300* DEN INGÅENDE ARTIKELN AV ERSÄTTNINGEN.                       *          
146400* DÄREMOT MÅSTE DEN TILLKOMMANDE ARTIKELN BOKAS AV, SÅ         *          
146500* MAN SER ATT DEN ÄR BEHANDLAD.                                *          
146600****************************************************************          
146700 IAA-BER-PA-TILLGANG-BYGGB SECTION.                                       
146800                                                                          
146900     MOVE ARB-E-IDARTNR TO W-IDARTNR                                      
147000     PERFORM IMS-GU-WDD701-ERSA                                           
147100     IF SEGMENT-FINNS                                                     
147200       PERFORM S40-LAS-WDD7-TILL-TABELL                                   
147300     ELSE                                                                 
147400       DISPLAY '*******************************************'              
147500       DISPLAY '*** WDD7 SAKNAS FÖR INGÅENDE ARTIKEL    ***'              
147600       DISPLAY '*** ING ARTIKEL: ' ARB-E-IDARTNR                          
147700       DISPLAY '*** SECT IAA-                           ***'              
147800       DISPLAY '*******************************************'              
147900       CALL ABEND USING ABEND-UTAN-DUMP                                   
148000     END-IF                                                               
148100                                                                          
148200     PERFORM S300-MATCHA-ARB-T-TAB                                        
148300                                                                          
148400     IF SW-RASA-SPARR = NEJ                                               
148500       IF SW-UTG     = JA                                                 
148600         PERFORM S80-UTG-MARKERING                                        
148700       END-IF                                                             
148800     END-IF                                                               
148900     .                                                                    
149000     EJECT                                                                
149100****************************************************************          
149200* IAB-BER-PA-TILLGANG-EJBYGGB                                  *          
149300* DETTA ÄR EN EJ BYGGBAR RAD MED ERSÄTTNINGSKOD SOM ÄR BEROENDE*          
149400* AV TILLGÅNG. WDD7 KONTROLLERAS, SÅ ERSÄTTNINGSINFO FINNS.    *          
149500* ARBETSTABELL FÖR TILLKOMMANDE ING ARTIKLAR MOTSVARANDE TN-   *          
149600* TABELLEN SKAPAS FÖR DEN ERSATTA ARTIKELN.                    *          
149700* KOMBINATIONSKOD BERÄKNAS.                                    *          
149800****************************************************************          
149900 IAB-BER-PA-TILLGANG-EJBYGGB SECTION.                                     
150000                                                                          
150100     MOVE ARB-E-IDARTNR TO W-IDARTNR                                      
150200     PERFORM IMS-GU-WDD701-ERSA                                           
150300     IF SEGMENT-FINNS                                                     
150400       PERFORM S40-LAS-WDD7-TILL-TABELL                                   
150500     ELSE                                                                 
150600       DISPLAY '*******************************************'              
150700       DISPLAY '*** WDD7 SAKNAS FÖR INGÅENDE ARTIKEL    ***'              
150800       DISPLAY '*** ING ARTIKEL: ' ARB-E-IDARTNR                          
150900       DISPLAY '*** SECT IAB-                           ***'              
151000       DISPLAY '*******************************************'              
151100       CALL ABEND USING ABEND-UTAN-DUMP                                   
151200     END-IF                                                               
151300                                                                          
151400     PERFORM S300-MATCHA-ARB-T-TAB                                        
151500                                                                          
151600     IF SW-RASA-SPARR = NEJ                                               
151700       IF SW-UTG     = NEJ                                                
151800         PERFORM S95-ERS-MARKERING-BER                                    
151900         IF SW-RASA-SPARR = NEJ                                           
152000           IF ARB-T-IDX > 1                                               
152100             PERFORM S303-SORTERA-ARB-T-TAB                               
152200           END-IF                                                         
152300                                                                          
152400           SET ARB-T-IDX TO +1                                            
152500           PERFORM UNTIL ARB-T-IDX > 15 OR                                
152600                         ARB-T-IDARTNR(ARB-T-IDX) = ZERO                  
152700             PERFORM S55-MATCHA-T-MOT-SATS                                
152800             IF SW-RASA-SPARR = NEJ                                       
152900               IF SW-SATS-TRAFF = JA                                      
153000                 MOVE JA TO SW-RASA-SPARR                                 
153100               ELSE                                                       
153200                 PERFORM S60-HITTA-LEDIG-SATS                             
153300                 IF SW-SATS-TRAFF = JA                                    
153400                   PERFORM S102-TILLK-NY-ART-EJBYGGB                      
153500                 ELSE                                                     
153600                   MOVE JA TO SW-RASA-SPARR                               
153700                 END-IF                                                   
153800               END-IF                                                     
153900             END-IF                                                       
154000             SET ARB-T-IDX UP BY +1                                       
154100            END-PERFORM                                                   
154200          END-IF                                                          
154300        ELSE                                                              
154400          MOVE JA TO SW-RASA-SPARR                                        
154500        END-IF                                                            
154600     END-IF                                                               
154700     .                                                                    
154800     EJECT                                                                
154900 J-UPPDATERA-SATS SECTION.                                                
155000                                                                          
155100     IF SHUV-FLBYGGB = JA OR                                              
155200        WS-FLBYGGB   = JA                                                 
155300       PERFORM JA-UPPDATERA-SATS-BYGGB                                    
155400     ELSE                                                                 
155500       PERFORM JB-UPPDATERA-SATS-EJBYGGB                                  
155600     END-IF                                                               
155700     .                                                                    
155800     EJECT                                                                
155900****************************************************************          
156000* JA-UPPDATERA-SATS-BYGGB                                      *          
156100* UPPDATERA SATSORDER-DATABASEN MED SATSORDER-TABELLEN.        *          
156200****************************************************************          
156300 JA-UPPDATERA-SATS-BYGGB SECTION.                                         
156400                                                                          
156500     MOVE JA TO SW-ORDER-ANDR                                             
156600     MOVE SHUV-IDORDNSB TO W-WDJ201-IDORDNSB                              
156700     MOVE SHUV-IDORDNSS TO W-WDJ201-IDORDNSS                              
156800     PERFORM IMS-GHU-WDJ201-SATG                                          
156900                                                                          
157000     SET SATS-IDX TO +1                                                   
157100     PERFORM UNTIL SATS-IDX > 250 OR                                      
157200                   SATS-IDARTNR(SATS-IDX) = ZERO                          
157300                                                                          
157400       IF SW-RASA-SPARR = NEJ                                             
157500         IF SATS-NYKDSATAND(SATS-IDX) NOT = SPACE                         
157600           MOVE    SATS-IDARTNR(SATS-IDX) TO W-WDJ211-IDARTNR             
157700           PERFORM IMS-GHU-WDJ211-SATG                                    
157800           IF SEGMENT-FINNS                                               
157900             EVALUATE SATS-NYKDSATAND(SATS-IDX)                           
158000               WHEN 'U'                                                   
158100                 PERFORM JAA-UPPDAT-UTGANG                                
158200               WHEN 'E'                                                   
158300                 PERFORM JAB-UPPDAT-ERSATT                                
158400               WHEN 'T'                                                   
158500                 PERFORM JAC-UPPDAT-TILLKOMMANDE-NY                       
158600               WHEN 'R'                                                   
158700                 PERFORM JAC-UPPDAT-TILLKOMMANDE-NY                       
158800             END-EVALUATE                                                 
158900           ELSE                                                           
159000             EVALUATE SATS-NYKDSATAND(SATS-IDX)                           
159100               WHEN 'T'                                                   
159200                 PERFORM JAD-SKAPA-TILLKOMMANDE-NY                        
159300               WHEN 'R'                                                   
159400                 PERFORM JAD-SKAPA-TILLKOMMANDE-NY                        
159500             END-EVALUATE                                                 
159600           END-IF                                                         
159700         END-IF                                                           
159800       ELSE                                                               
159900         IF SATS-SPARR(SATS-IDX) = JA                                     
160000           MOVE SATS-IDARTNR(SATS-IDX) TO W-WDJ211-IDARTNR                
160100           PERFORM IMS-GHU-WDJ211-SATG                                    
160200           IF SEGMENT-FINNS                                               
160300             MOVE    JA TO SRAD-FLSATSPR                                  
160400             PERFORM IMS-REPL-WDJ211-SATG                                 
160500           ELSE                                                           
160600             DISPLAY '*************************************'              
160700             DISPLAY '** ERSATT/UTGÅNGEN ARTIKEL SAKNAS  **'              
160800             DISPLAY '** ' SATS-IDARTNR(SATS-IDX)                         
160900             DISPLAY '** SECT- JA-                       **'              
161000             DISPLAY '*************************************'              
161100             CALL ABEND USING ABEND-UTAN-DUMP                             
161200           END-IF                                                         
161300         END-IF                                                           
161400       END-IF                                                             
161500       SET SATS-IDX UP BY +1                                              
161600     END-PERFORM                                                          
161700     .                                                                    
161800     EJECT                                                                
161900****************************************************************          
162000* JAA-UPPDAT-UTGANG                                            *          
162100* UPPDATERA SATSORDER-DATABASEN MED EN UTGÅNGEN ING.ARTIKEL    *          
162200****************************************************************          
162300 JAA-UPPDAT-UTGANG SECTION.                                               
162400                                                                          
162500     MOVE    ZERO                   TO WS-REANTPSA                        
162600     MOVE    SATS-IDARTNR(SATS-IDX) TO W-IDARTNR                          
162700     PERFORM IMS-GU-WDK601                                                
162800     PERFORM IMS-GHNP-WDK611                                              
162900     COMPUTE WS-REANTPSA = SATS-REANTPSA(SATS-IDX) -                      
163000                           SATS-NYREANTPSA(SATS-IDX)                      
163100     IF WS-REANTPSA = 0                                                   
163200       SUBTRACT SATS-REBEART(SATS-IDX)FROM CLAG-KVRESS                    
163300       MOVE ZERO                      TO   SRAD-KVSATRES                  
163400                                           SRAD-REANTPSA                  
163500                                           SRAD-REBEART                   
163600       MOVE NEJ                       TO   SRAD-FLSATRAS                  
163700       MOVE SATS-NYKDSATAND(SATS-IDX) TO   SRAD-KDSATAND                  
163800     ELSE                                                                 
163900* ---  DEL AV ING.ART ÄR UTGÅNGEN                                         
164000       COMPUTE SATS-NYREBEART(SATS-IDX) =                                 
164100               SATS-NYREANTPSA(SATS-IDX) * SATS-HUV-KVBEART               
164200       SUBTRACT SATS-NYREBEART(SATS-IDX)  FROM CLAG-KVRESS                
164300       SUBTRACT SATS-NYREBEART(SATS-IDX)  FROM SRAD-KVSATRES              
164400       SUBTRACT SATS-NYREBEART(SATS-IDX)  FROM SRAD-REBEART               
164500       SUBTRACT SATS-NYREANTPSA(SATS-IDX) FROM SRAD-REANTPSA              
164600       MOVE     SATS-NYKDSATAND(SATS-IDX) TO   SRAD-KDSATAND              
164700       PERFORM S320-KONTR-FLSATRAS                                        
164800       IF WS-RASA-IDARTNR-RAKN > 1                                        
164900         MOVE JA               TO SRAD-FLSATRAS                           
165000       ELSE                                                               
165100         MOVE NEJ              TO SRAD-FLSATRAS                           
165200       END-IF                                                             
165300     END-IF                                                               
165400                                                                          
165500     PERFORM IMS-REPL-WDK611                                              
165600     PERFORM IMS-REPL-WDJ211-SATG                                         
165700     PERFORM S410-SKAPA-2109-A-TRANS                                      
165800     .                                                                    
165900     EJECT                                                                
166000 JAB-UPPDAT-ERSATT SECTION.                                               
166100****************************************************************          
166200* JAB-UPPDAT-ERSATT                                            *          
166300* UPPDATERA SATSORDER-DATABASEN MED EN ERSATT ING.ARTIKEL      *          
166400* VID ERSÄTTNING SKALL ALLA ARTIKLAR VARA ERSATTA, ANNARS      *          
166500* SKALL ORDERN SPÄRRAS.                                        *          
166600****************************************************************          
166700                                                                          
166800     MOVE    SATS-IDARTNR(SATS-IDX) TO W-IDARTNR                          
166900     PERFORM IMS-GU-WDK601                                                
167000     PERFORM IMS-GHNP-WDK611                                              
167100     COMPUTE WS-REANTPSA = SATS-REANTPSA(SATS-IDX) -                      
167200                           SATS-NYREANTPSA(SATS-IDX)                      
167300     IF WS-REANTPSA = 0                                                   
167400* --- HELA ING.ARTIKELN ÄR ERSATT                                         
167500       SUBTRACT SATS-REBEART(SATS-IDX)    FROM CLAG-KVRESS                
167600       MOVE     ZERO                      TO   SRAD-REANTPSA              
167700       MOVE     ZERO                      TO   SRAD-REBEART               
167800       MOVE     ZERO                      TO   SRAD-KVSATRES              
167900       MOVE     NEJ                       TO   SRAD-FLSATRAS              
168000       MOVE     SATS-NYKDSATAND(SATS-IDX) TO   SRAD-KDSATAND              
168100     ELSE                                                                 
168200       DISPLAY '*************************************'                    
168300       DISPLAY '** ERSATT ANTAL FELAKTIGT          **'                    
168400       DISPLAY '** SECT- JAB-                      **'                    
168500       DISPLAY '*************************************'                    
168600       CALL     ABEND USING ABEND-UTAN-DUMP                               
168700     END-IF                                                               
168800                                                                          
168900     PERFORM IMS-REPL-WDK611                                              
169000     PERFORM IMS-REPL-WDJ211-SATG                                         
169100     PERFORM S410-SKAPA-2109-A-TRANS                                      
169200     .                                                                    
169300     EJECT                                                                
169400 JAC-UPPDAT-TILLKOMMANDE-NY SECTION.                                      
169500****************************************************************          
169600* JAC-UPPDAT-TILLKOMMANDE-NY                                   *          
169700* UPPDATERA SATSORDER-DATABASEN MED EN TILLKOMMANDE ING.ARTIKEL*          
169800* UPPDATERA SATSORDER-DATABASEN MED EN NY ING.ARTIKEL          *          
169900****************************************************************          
170000                                                                          
170100     ADD     SATS-NYREANTPSA(SATS-IDX) TO SRAD-REANTPSA                   
170200     ADD     SATS-NYREBEART (SATS-IDX) TO SRAD-REBEART                    
170300     MOVE    SATS-NYKDSATAND(SATS-IDX) TO SRAD-KDSATAND                   
170400     MOVE    SATS-NYKDSATKMB(SATS-IDX) TO SRAD-KDSATKMB                   
170500     MOVE    JA                        TO SRAD-FLSATRAS                   
170600     MOVE    ZERO                      TO WS-RO-ANTAL                     
170700     MOVE    SRAD-IDARTNR              TO W-IDARTNR                       
170800     PERFORM IMS-GU-WDK601                                                
170900     PERFORM IMS-GHNP-WDK611                                              
171000                                                                          
171100     PERFORM S310-DISP-LAGERSALDO                                         
171200                                                                          
171300     IF WS-DISPLS >= SATS-NYREBEART(SATS-IDX)                             
171400                                                                          
171500* ---  ALLT FINNS I LAGER                                                 
171600       ADD SATS-NYREBEART(SATS-IDX) TO CLAG-KVRESS                        
171700       ADD SATS-NYREBEART(SATS-IDX) TO SRAD-KVSATRES                      
171800     ELSE                                                                 
171900       MOVE SATS-IDARTNR(SATS-IDX)  TO ORDP-RAD-IDARTNR                   
172000       MOVE 1                       TO ORDP-RAD-KDROO                     
172100                                                                          
172200       IF WS-DISPLS > 0                                                   
172300         COMPUTE WS-RO-ANTAL =   SATS-NYREBEART(SATS-IDX)                 
172400                               - WS-DISPLS                                
172500* ---    VISS DEL FINNS I LAGER                                           
172600         COMPUTE WS-KVSATRES =   SATS-NYREBEART(SATS-IDX)                 
172700                               - WS-RO-ANTAL                              
172800         ADD WS-RO-ANTAL    TO SRAD-KVSATROS                              
172900         ADD WS-KVSATRES    TO SRAD-KVSATRES                              
173000         ADD WS-KVSATRES    TO CLAG-KVRESS                                
173100       ELSE                                                               
173200                                                                          
173300* ---    INGET FINNS I LAGER                                              
173400         ADD  SATS-NYREBEART(SATS-IDX)  TO SRAD-KVSATROS                  
173500         MOVE SATS-NYREBEART(SATS-IDX) TO WS-RO-ANTAL                     
173600       END-IF                                                             
173700                                                                          
173800       PERFORM S01-SKAPA-RESTORDER                                        
173900       PERFORM S03-EV-LARM-2191-ANSKAFFN                                  
174000       ADD  WS-RO-ANTAL      TO CLAG-KVROS                                
174100     END-IF                                                               
174200                                                                          
174300     PERFORM IMS-REPL-WDK611                                              
174400     PERFORM IMS-REPL-WDJ211-SATG                                         
174500     PERFORM S420-SKAPA-2109-O-TRANS                                      
174600     .                                                                    
174700     EJECT                                                                
174800 JAD-SKAPA-TILLKOMMANDE-NY SECTION.                                       
174900****************************************************************          
175000* JAD-SKAPA-TILLKOMMANDE-NY                                    *          
175100* SKAPA EN TILLKOMMANDE ING.ARTIKEL PÅ SATSORDERDATABASEN      *          
175200* SKAPA EN NY ING.ARTIKEL PÅ SATSORDERDATABASEN                *          
175300****************************************************************          
175400                                                                          
175500     MOVE    SATS-IDARTNR(SATS-IDX)    TO W-IDARTNR                       
175600     PERFORM IMS-GU-WDK601                                                
175700     PERFORM IMS-GHNP-WDK611                                              
175800     PERFORM S330-REDIGERA-SRAD                                           
175900     MOVE    SATS-NYREANTPSA(SATS-IDX) TO SRAD-REANTPSA                   
176000     MOVE    SATS-NYREBEART(SATS-IDX)  TO SRAD-REBEART                    
176100                                                                          
176200     MOVE    ZERO                      TO WS-RO-ANTAL                     
176300     PERFORM S310-DISP-LAGERSALDO                                         
176400                                                                          
176500     IF WS-DISPLS >= SATS-NYREBEART(SATS-IDX)                             
176600* ---  ALLT FINNS I LAGER                                                 
176700       ADD  SATS-NYREBEART(SATS-IDX) TO CLAG-KVRESS                       
176800       MOVE SATS-NYREBEART(SATS-IDX) TO SRAD-KVSATRES                     
176900     ELSE                                                                 
177000       MOVE SATS-IDARTNR(SATS-IDX) TO ORDP-RAD-IDARTNR                    
177100       MOVE 1                      TO ORDP-RAD-KDROO                      
177200       IF WS-DISPLS > 0                                                   
177300         COMPUTE WS-RO-ANTAL =  SATS-NYREBEART(SATS-IDX)                  
177400                               - WS-DISPLS                                
177500* ---    VISS DEL FINNS I LAGER                                           
177600         COMPUTE WS-KVSATRES = SATS-NYREBEART(SATS-IDX)                   
177700                               - WS-RO-ANTAL                              
177800         MOVE WS-RO-ANTAL   TO SRAD-KVSATROS                              
177900         MOVE WS-KVSATRES   TO SRAD-KVSATRES                              
178000         ADD  WS-KVSATRES   TO CLAG-KVRESS                                
178100       ELSE                                                               
178200                                                                          
178300* ---    INGET FINNS I LAGER                                              
178400         MOVE SATS-NYREBEART(SATS-IDX) TO SRAD-KVSATROS                   
178500                                          WS-RO-ANTAL                     
178600         MOVE 9999999 TO WS-KVBYGGBAR                                     
178700       END-IF                                                             
178800                                                                          
178900       PERFORM S01-SKAPA-RESTORDER                                        
179000       PERFORM S03-EV-LARM-2191-ANSKAFFN                                  
179100       ADD     WS-RO-ANTAL TO CLAG-KVROS                                  
179200     END-IF                                                               
179300                                                                          
179400     PERFORM IMS-REPL-WDK611                                              
179500     MOVE    SATS-IDARTNR(SATS-IDX) TO W-WDJ211-IDARTNR                   
179600     PERFORM IMS-INSERT-WDJ211-SATG                                       
179700     PERFORM S420-SKAPA-2109-O-TRANS                                      
179800     .                                                                    
179900     EJECT                                                                
180000****************************************************************          
180100* JB-UPPDATERA-SATS-EJBYGGB                                    *          
180200* UPPDATERA SATSORDER-DATABASEN MED SATSORDER-TABELLEN.        *          
180300****************************************************************          
180400 JB-UPPDATERA-SATS-EJBYGGB SECTION.                                       
180500                                                                          
180600     MOVE    JA            TO SW-ORDER-ANDR                               
180700     MOVE    SHUV-IDORDNSB TO W-WDJ201-IDORDNSB                           
180800     MOVE    SHUV-IDORDNSS TO W-WDJ201-IDORDNSS                           
180900     PERFORM IMS-GHU-WDJ201-SATG                                          
181000                                                                          
181100     SET SATS-IDX TO +1                                                   
181200     PERFORM UNTIL SATS-IDX > 250 OR                                      
181300                   SATS-IDARTNR(SATS-IDX) = ZERO                          
181400                                                                          
181500       IF SW-RASA-SPARR = NEJ                                             
181600         IF SATS-NYKDSATAND(SATS-IDX) NOT = SPACE                         
181700           MOVE    SATS-IDARTNR(SATS-IDX) TO W-WDJ211-IDARTNR             
181800           PERFORM IMS-GHU-WDJ211-SATG                                    
181900           IF SEGMENT-FINNS                                               
182000             EVALUATE SATS-NYKDSATAND(SATS-IDX)                           
182100               WHEN 'U'                                                   
182200                 PERFORM JBA-UPPDAT-UTGANG                                
182300               WHEN 'E'                                                   
182400                 PERFORM JBB-UPPDAT-ERSATT                                
182500               WHEN 'T'                                                   
182600                 PERFORM JBC-UPPDAT-TILLKOMMANDE-NY                       
182700               WHEN 'R'                                                   
182800                 PERFORM JBC-UPPDAT-TILLKOMMANDE-NY                       
182900             END-EVALUATE                                                 
183000           ELSE                                                           
183100             EVALUATE SATS-NYKDSATAND(SATS-IDX)                           
183200               WHEN 'T'                                                   
183300                 PERFORM JBD-SKAPA-TILLKOMMANDE-NY                        
183400               WHEN 'R'                                                   
183500                 PERFORM JBD-SKAPA-TILLKOMMANDE-NY                        
183600             END-EVALUATE                                                 
183700           END-IF                                                         
183800         END-IF                                                           
183900       ELSE                                                               
184000          IF SATS-SPARR(SATS-IDX) = JA                                    
184100             MOVE SATS-IDARTNR(SATS-IDX) TO W-WDJ211-IDARTNR              
184200             PERFORM IMS-GHU-WDJ211-SATG                                  
184300             IF SEGMENT-FINNS                                             
184400                MOVE JA TO SRAD-FLSATSPR                                  
184500                PERFORM IMS-REPL-WDJ211-SATG                              
184600             ELSE                                                         
184700                DISPLAY '*************************************'           
184800                DISPLAY '** ERSATT/UTGÅNGEN ARTIKEL SAKNAS  **'           
184900                DISPLAY '** ' SATS-IDARTNR(SATS-IDX)                      
185000                DISPLAY '** SECT- JB-                       **'           
185100                DISPLAY '*************************************'           
185200                CALL ABEND USING ABEND-UTAN-DUMP                          
185300             END-IF                                                       
185400          END-IF                                                          
185500       END-IF                                                             
185600       SET SATS-IDX UP BY +1                                              
185700     END-PERFORM                                                          
185800     .                                                                    
185900     EJECT                                                                
186000****************************************************************          
186100* JBA-UPPDAT-UTGANG                                            *          
186200* UPPDATERA SATSORDER-DATABASEN MED EN UTGÅNGEN ING.ARTIKEL    *          
186300* FELUTGÅNG I SEKTION S340 OCH S350 KAN BLI ANROPAD.           *          
186400****************************************************************          
186500 JBA-UPPDAT-UTGANG SECTION.                                               
186600                                                                          
186700     MOVE    ZERO                   TO WS-REANTPSA                        
186800     MOVE    ZERO                   TO WS-REBEART                         
186900     MOVE    ZERO                   TO WS-KVRESS-M                        
187000     MOVE    SATS-IDARTNR(SATS-IDX) TO W-IDARTNR                          
187100     PERFORM IMS-GU-WDK601                                                
187200     PERFORM IMS-GHNP-WDK611                                              
187300     COMPUTE WS-REANTPSA = SATS-REANTPSA(SATS-IDX) -                      
187400                           SATS-NYREANTPSA(SATS-IDX)                      
187500     IF WS-REANTPSA = 0                                                   
187600       SUBTRACT SRAD-KVSATRES FROM CLAG-KVRESS                            
187700       MOVE ZERO TO SRAD-KVSATRES                                         
187800       MOVE ZERO TO SRAD-REANTPSA                                         
187900       MOVE ZERO TO SRAD-REBEART                                          
188000       MOVE NEJ  TO SRAD-FLSATRAS                                         
188100       MOVE SATS-NYKDSATAND(SATS-IDX) TO SRAD-KDSATAND                    
188200                                                                          
188300       IF SRAD-KVSATROS > 0                                               
188400         PERFORM S350-TABORT-RESTORDER                                    
188500         IF WS-KVSATROS-SUM =  SRAD-KVSATROS                              
188600           MOVE ZERO       TO SRAD-KVSATROS                               
188700         ELSE                                                             
188800           DISPLAY '*****************************************'            
188900           DISPLAY '***   RESTORDERANTAL FELAKTIGT        ***'            
189000           DISPLAY '***   ARTIKELNR: ' SATS-IDARTNR(SATS-IDX)             
189100           DISPLAY '***   SECT JBA-                       ***'            
189200           DISPLAY '*****************************************'            
189300           CALL ABEND USING ABEND-UTAN-DUMP                               
189400         END-IF                                                           
189500       END-IF                                                             
189600     ELSE                                                                 
189700* --- DEL AV ING.ART ÄR UTGÅNGEN                                          
189800* --- BERÄKNA KVARVARANDE ANTAL                                           
189900       COMPUTE SATS-NYREBEART(SATS-IDX) =                                 
190000               SATS-NYREANTPSA(SATS-IDX) * SATS-HUV-KVBEART               
190100       COMPUTE WS-REBEART =   SATS-REBEART(SATS-IDX)                      
190200                            - SATS-NYREBEART(SATS-IDX)                    
190300                                                                          
190400       IF WS-REBEART <= SRAD-KVSATRES                                     
190500         IF SRAD-KVSATROS > 0                                             
190600            PERFORM S350-TABORT-RESTORDER                                 
190700           IF WS-KVSATROS-SUM = SRAD-KVSATROS                             
190800             MOVE ZERO      TO SRAD-KVSATROS                              
190900           ELSE                                                           
191000             DISPLAY '*****************************************'          
191100             DISPLAY '***   RESTORDERANTAL FELAKTIGT        ***'          
191200             DISPLAY '***   ARTIKELNR: ' SATS-IDARTNR(SATS-IDX)           
191300             DISPLAY '***   SECT JA-                        ***'          
191400             DISPLAY '*****************************************'          
191500             CALL ABEND USING ABEND-UTAN-DUMP                             
191600           END-IF                                                         
191700         END-IF                                                           
191800         COMPUTE WS-KVRESS-M = SRAD-KVSATRES -                            
191900                               WS-REBEART                                 
192000         SUBTRACT WS-KVRESS-M  FROM CLAG-KVRESS                           
192100         SUBTRACT WS-KVRESS-M  FROM SRAD-KVSATRES                         
192200       ELSE                                                               
192300         IF SRAD-KVSATROS > 0                                             
192400           PERFORM S340-JUSTERA-RESTORDER                                 
192500         ELSE                                                             
192600           DISPLAY '*****************************************'            
192700           DISPLAY '***   RESTORDER SAKNAS FÖR ING.ARTIKEL***'            
192800           DISPLAY '***   ARTIKELNR: ' SATS-IDARTNR(SATS-IDX)             
192900           DISPLAY '***   SECT JA-                        ***'            
193000           DISPLAY '*****************************************'            
193100           CALL ABEND USING ABEND-UTAN-DUMP                               
193200         END-IF                                                           
193300       END-IF                                                             
193400       MOVE WS-REBEART          TO SRAD-REBEART                           
193500       MOVE WS-REANTPSA         TO SRAD-REANTPSA                          
193600       MOVE SATS-NYKDSATAND(SATS-IDX)  TO SRAD-KDSATAND                   
193700                                                                          
193800       PERFORM S320-KONTR-FLSATRAS                                        
193900       IF WS-RASA-IDARTNR-RAKN > 1                                        
194000         MOVE JA                TO SRAD-FLSATRAS                          
194100       ELSE                                                               
194200         MOVE NEJ               TO SRAD-FLSATRAS                          
194300       END-IF                                                             
194400     END-IF                                                               
194500                                                                          
194600     PERFORM IMS-REPL-WDK611                                              
194700     PERFORM IMS-REPL-WDJ211-SATG                                         
194800     PERFORM S410-SKAPA-2109-A-TRANS                                      
194900     .                                                                    
195000     EJECT                                                                
195100****************************************************************          
195200* JBB-UPPDAT-ERSATT                                            *          
195300* UPPDATERA SATSORDER-DATABASEN MED EN ERSATT ING.ARTIKEL      *          
195400* OM SATS-NYREBEART(SATS-IDX) > 0 ÄR DEN HÄR ERSATTA ARTIKELN  *          
195500* TILLGÅNGSBEROENDE. SAMMA HANTERING AV ALLA ARTIKLAR MED SAMMA*          
195600* ARTIKELNR, ANNARS SPÄRRAS ORDERN.                            *          
195700****************************************************************          
195800 JBB-UPPDAT-ERSATT SECTION.                                               
195900                                                                          
196000     MOVE    NEJ                    TO SW-SATS-FEL                        
196100     MOVE    SATS-IDARTNR(SATS-IDX) TO W-IDARTNR                          
196200     PERFORM IMS-GU-WDK601                                                
196300     PERFORM IMS-GHNP-WDK611                                              
196400     COMPUTE WS-REANTPSA =   SATS-REANTPSA(SATS-IDX)                      
196500                           - SATS-NYREANTPSA(SATS-IDX)                    
196600     IF WS-REANTPSA = 0                                                   
196700* ---  HELA ING.ARTIKELN ÄR ERSATT                                        
196800                                                                          
196900       IF SATS-NYKDSATKMB(SATS-IDX) NOT = SPACE                           
197000* ---    TILLGÅNGSBEROENDE ERSÄTTNING                                     
197100         PERFORM JBBA-TILLGANGSBEROENDE-ERS                               
197200                                                                          
197300       ELSE                                                               
197400* ---    ÖVRIGA ERSÄTTNINGAR                                              
197500         PERFORM JBBB-OVRIGA-ERSATTNINGAR                                 
197600                                                                          
197700       END-IF                                                             
197800     ELSE                                                                 
197900       DISPLAY '*************************************'                    
198000       DISPLAY '** ERSATT ANTAL FELAKTIGT          **'                    
198100       DISPLAY '** SECT- JB-                       **'                    
198200       DISPLAY '*************************************'                    
198300       CALL ABEND USING ABEND-UTAN-DUMP                                   
198400     END-IF                                                               
198500                                                                          
198600     PERFORM IMS-REPL-WDK611                                              
198700     PERFORM IMS-REPL-WDJ211-SATG                                         
198800     PERFORM S410-SKAPA-2109-A-TRANS                                      
198900     .                                                                    
199000     EJECT                                                                
199100 JBBA-TILLGANGSBEROENDE-ERS SECTION.                                      
199200                                                                          
199300     IF SATS-KVSATRES(SATS-IDX) = SATS-NYREBEART(SATS-IDX)                
199400       PERFORM S350-TABORT-RESTORDER                                      
199500       IF WS-KVSATROS-SUM = SRAD-KVSATROS                                 
199600         MOVE ZERO      TO SRAD-KVSATROS                                  
199700       ELSE                                                               
199800         DISPLAY '*****************************************'              
199900         DISPLAY '***   RESTORDERANTAL FELAKTIGT        ***'              
200000         DISPLAY '***   ARTIKELNR: ' SATS-IDARTNR(SATS-IDX)               
200100         DISPLAY '***   SECT JBB-                       ***'              
200200         DISPLAY '*****************************************'              
200300         CALL ABEND USING ABEND-UTAN-DUMP                                 
200400       END-IF                                                             
200500     ELSE                                                                 
200600       COMPUTE WS-KVRESS-M =   SATS-KVSATRES(SATS-IDX)                    
200700                             - SATS-NYREBEART(SATS-IDX)                   
200800       SUBTRACT WS-KVRESS-M FROM CLAG-KVRESS                              
200900       PERFORM S350-TABORT-RESTORDER                                      
201000       IF WS-KVSATROS-SUM = SRAD-KVSATROS                                 
201100         MOVE ZERO      TO SRAD-KVSATROS                                  
201200       ELSE                                                               
201300         DISPLAY '*****************************************'              
201400         DISPLAY '***   RESTORDERANTAL FELAKTIGT        ***'              
201500         DISPLAY '***   ARTIKELNR: ' SATS-IDARTNR(SATS-IDX)               
201600         DISPLAY '***   SECT JB-                        ***'              
201700         DISPLAY '*****************************************'              
201800         CALL ABEND USING ABEND-UTAN-DUMP                                 
201900       END-IF                                                             
202000     END-IF                                                               
202100     MOVE SATS-NYREBEART(SATS-IDX)  TO SRAD-REBEART                       
202200                                       SRAD-KVSATRES                      
202300     MOVE SATS-NYKDSATAND(SATS-IDX) TO SRAD-KDSATAND                      
202400     MOVE SATS-NYKDSATKMB(SATS-IDX) TO SRAD-KDSATKMB                      
202500     .                                                                    
202600     EJECT                                                                
202700 JBBB-OVRIGA-ERSATTNINGAR SECTION.                                        
202800                                                                          
202900     IF SRAD-KVSATRES = SATS-REBEART(SATS-IDX)                            
203000       SUBTRACT SRAD-KVSATRES FROM CLAG-KVRESS                            
203100     ELSE                                                                 
203200       IF SRAD-KVSATRES > 0                                               
203300         SUBTRACT SRAD-KVSATRES FROM CLAG-KVRESS                          
203400         PERFORM S350-TABORT-RESTORDER                                    
203500         IF WS-KVSATROS-SUM = SRAD-KVSATROS                               
203600           MOVE ZERO    TO SRAD-KVSATROS                                  
203700         ELSE                                                             
203800           DISPLAY '***************************************'              
203900           DISPLAY '**  RESTORDERANTAL FELAKTIGT         **'              
204000           DISPLAY '**  ARTIKELNR: ' SATS-IDARTNR(SATS-IDX)               
204100           DISPLAY '** SECT- JB-                         **'              
204200           DISPLAY '***************************************'              
204300           CALL ABEND USING ABEND-UTAN-DUMP                               
204400         END-IF                                                           
204500       ELSE                                                               
204600         PERFORM S350-TABORT-RESTORDER                                    
204700         IF WS-KVSATROS-SUM = SRAD-KVSATROS                               
204800           MOVE ZERO    TO SRAD-KVSATROS                                  
204900         ELSE                                                             
205000           DISPLAY '***************************************'              
205100           DISPLAY '**  RESTORDERANTAL FELAKTIGT         **'              
205200           DISPLAY '**  ARTIKELNR: ' SATS-IDARTNR(SATS-IDX)               
205300           DISPLAY '** SECT- JB-                         **'              
205400           DISPLAY '***************************************'              
205500           CALL ABEND USING ABEND-UTAN-DUMP                               
205600         END-IF                                                           
205700       END-IF                                                             
205800     END-IF                                                               
205900     MOVE ZERO TO SRAD-REBEART                                            
206000                  SRAD-KVSATRES                                           
206100                  SRAD-REANTPSA                                           
206200     MOVE NEJ  TO SRAD-FLSATRAS                                           
206300     MOVE SATS-NYKDSATAND(SATS-IDX) TO SRAD-KDSATAND                      
206400     .                                                                    
206500     EJECT                                                                
206600****************************************************************          
206700* JBC-UPPDAT-TILLKOMMANDE-NY                                   *          
206800* UPPDATERA SATSORDER-DATABASEN MED EN TILLKOMMANDE ING.ARTIKEL*          
206900* UPPDATERA SATSORDER-DATABASEN MED EN NY ING.ARTIKEL          *          
207000* ARTIKELN FINNS SEDAN TIDIGARE. KOMBINATIONSKOD EJ MÖJLIG.    *          
207100****************************************************************          
207200 JBC-UPPDAT-TILLKOMMANDE-NY SECTION.                                      
207300                                                                          
207400     ADD     SATS-NYREANTPSA(SATS-IDX) TO SRAD-REANTPSA                   
207500     ADD     SATS-NYREBEART(SATS-IDX)  TO SRAD-REBEART                    
207600     MOVE    SATS-NYKDSATAND(SATS-IDX) TO SRAD-KDSATAND                   
207700     MOVE    SATS-NYKDSATKMB(SATS-IDX) TO SRAD-KDSATKMB                   
207800     MOVE    JA                        TO SRAD-FLSATRAS                   
207900     MOVE    ZERO                      TO WS-RO-ANTAL                     
208000     MOVE    SRAD-IDARTNR              TO W-IDARTNR                       
208100     PERFORM IMS-GU-WDK601                                                
208200     PERFORM IMS-GHNP-WDK611                                              
208300     PERFORM S310-DISP-LAGERSALDO                                         
208400                                                                          
208500     IF WS-DISPLS >= SATS-NYREBEART(SATS-IDX)                             
208600                                                                          
208700* ---  ALLT FINNS I LAGER                                                 
208800       ADD SATS-NYREBEART(SATS-IDX) TO CLAG-KVRESS                        
208900       ADD SATS-NYREBEART(SATS-IDX) TO SRAD-KVSATRES                      
209000                                                                          
209100     ELSE                                                                 
209200       MOVE SATS-IDARTNR(SATS-IDX) TO ORDP-RAD-IDARTNR                    
209300       MOVE 1                      TO ORDP-RAD-KDROO                      
209400                                                                          
209500       IF WS-DISPLS > 0                                                   
209600         COMPUTE WS-RO-ANTAL = SATS-NYREBEART(SATS-IDX)                   
209700                               - WS-DISPLS                                
209800                                                                          
209900* ---    VISS DEL FINNS I LAGER                                           
210000         COMPUTE WS-KVSATRES =   SATS-NYREBEART(SATS-IDX)                 
210100                               - WS-RO-ANTAL                              
210200         ADD WS-RO-ANTAL    TO SRAD-KVSATROS                              
210300         ADD WS-KVSATRES    TO SRAD-KVSATRES                              
210400         ADD WS-KVSATRES    TO CLAG-KVRESS                                
210500       ELSE                                                               
210600                                                                          
210700* ---    INGET FINNS I LAGER                                              
210800         ADD  SATS-NYREBEART(SATS-IDX) TO SRAD-KVSATROS                   
210900         MOVE SATS-NYREBEART(SATS-IDX) TO WS-RO-ANTAL                     
211000         IF SRAD-KVSATRES = 0                                             
211100           MOVE 9999999    TO WS-KVBYGGBAR                                
211200         END-IF                                                           
211300       END-IF                                                             
211400                                                                          
211500       PERFORM S01-SKAPA-RESTORDER                                        
211600       PERFORM S03-EV-LARM-2191-ANSKAFFN                                  
211700       ADD WS-RO-ANTAL       TO CLAG-KVROS                                
211800     END-IF                                                               
211900                                                                          
212000     PERFORM IMS-REPL-WDK611                                              
212100     PERFORM IMS-REPL-WDJ211-SATG                                         
212200     PERFORM S420-SKAPA-2109-O-TRANS                                      
212300     .                                                                    
212400     EJECT                                                                
212500****************************************************************          
212600* JBD-SKAPA-TILLKOMMANDE-NY                                    *          
212700* SKAPA EN TILLKOMMANDE ING.ARTIKEL PÅ SATSORDERDATABASEN      *          
212800* SKAPA EN NY ING.ARTIKEL PÅ SATSORDERDATABASEN                *          
212900****************************************************************          
213000 JBD-SKAPA-TILLKOMMANDE-NY SECTION.                                       
213100                                                                          
213200     MOVE    SATS-IDARTNR(SATS-IDX)    TO W-IDARTNR                       
213300     PERFORM IMS-GU-WDK601                                                
213400     PERFORM IMS-GHNP-WDK611                                              
213500     PERFORM S330-REDIGERA-SRAD                                           
213600     MOVE    SATS-NYREANTPSA(SATS-IDX) TO SRAD-REANTPSA                   
213700     MOVE    SATS-NYREBEART(SATS-IDX)  TO SRAD-REBEART                    
213800     MOVE    ZERO                      TO WS-RO-ANTAL                     
213900     PERFORM S310-DISP-LAGERSALDO                                         
214000                                                                          
214100     IF WS-DISPLS >= SATS-NYREBEART(SATS-IDX)                             
214200* ---  ALLT FINNS I LAGER                                                 
214300       ADD  SATS-NYREBEART(SATS-IDX) TO CLAG-KVRESS                       
214400       MOVE SATS-NYREBEART(SATS-IDX) TO SRAD-KVSATRES                     
214500     ELSE                                                                 
214600       MOVE SATS-IDARTNR(SATS-IDX)   TO ORDP-RAD-IDARTNR                  
214700       MOVE 1                        TO ORDP-RAD-KDROO                    
214800       IF WS-DISPLS > 0                                                   
214900         COMPUTE WS-RO-ANTAL =   SATS-NYREBEART(SATS-IDX)                 
215000                               - WS-DISPLS                                
215100                                                                          
215200* ---    VISS DEL FINNS I LAGER                                           
215300         COMPUTE WS-KVSATRES =   SATS-NYREBEART(SATS-IDX)                 
215400                               - WS-RO-ANTAL                              
215500         MOVE WS-RO-ANTAL            TO SRAD-KVSATROS                     
215600         MOVE WS-KVSATRES            TO SRAD-KVSATRES                     
215700         ADD  WS-KVSATRES            TO CLAG-KVRESS                       
215800       ELSE                                                               
215900                                                                          
216000* --- INGET FINNS I LAGER                                                 
216100         MOVE SATS-NYREBEART(SATS-IDX) TO SRAD-KVSATROS                   
216200                                           WS-RO-ANTAL                    
216300         MOVE 9999999                TO WS-KVBYGGBAR                      
216400       END-IF                                                             
216500                                                                          
216600       PERFORM S01-SKAPA-RESTORDER                                        
216700       PERFORM S03-EV-LARM-2191-ANSKAFFN                                  
216800       ADD     WS-RO-ANTAL           TO CLAG-KVROS                        
216900     END-IF                                                               
217000                                                                          
217100     PERFORM IMS-REPL-WDK611                                              
217200     MOVE SATS-IDARTNR(SATS-IDX) TO W-WDJ211-IDARTNR                      
217300     PERFORM IMS-INSERT-WDJ211-SATG                                       
217400     PERFORM S420-SKAPA-2109-O-TRANS                                      
217500     .                                                                    
217600     EJECT                                                                
217700 K-BEH-SATS-RASA-SPARR-ELLER-EJ SECTION.                                  
217800                                                                          
217900     MOVE    SHUV-IDORDNSB TO W-WDJ201-IDORDNSB                           
218000     MOVE    SHUV-IDORDNSS TO W-WDJ201-IDORDNSS                           
218100     PERFORM IMS-GHU-WDJ201-SATG                                          
218200     IF SW-RASA-SPARR = NEJ                                               
218300       PERFORM KA-BERAKNINGAR                                             
218400       PERFORM IMS-GHU-WDJ201-SATG                                        
218500       IF WS-FLBYGGB = JA                                                 
218600         PERFORM S04-BYGGBAR                                              
218700       ELSE                                                               
218800         PERFORM S05-EJBYGGBAR                                            
218900       END-IF                                                             
219000       IF SW-ORDER-ANDR = JA                                              
219100         MOVE DAGENS-DATUM-NUM  TO SHUV-TIUPPDAT                          
219200         MOVE SATS-HUV-KDSATKMB TO SHUV-KDSATKMB                          
219300       END-IF                                                             
219400     ELSE                                                                 
219500       MOVE NEJ              TO WS-FLBYGGB                                
219600       MOVE DAGENS-DATUM-NUM TO SHUV-TIUPPDAT                             
219700       MOVE JA               TO SHUV-FLSATSPR                             
219800       MOVE +0               TO SHUV-KVBYGGB                              
219900     END-IF                                                               
220000     MOVE    WS-FLBYGGB TO SHUV-FLBYGGB                                   
220100     PERFORM IMS-REPL-WDJ201-SATG                                         
220200     PERFORM IMS-CHECKPOINT                                               
220300     .                                                                    
220400     EJECT                                                                
220500****************************************************************          
220600* KA-BERAKNINGAR                                               *          
220700* WS-KVBYGGBAR KAN HA VÄRDET 999999 - OM RADEN ÄR BEARBETAD    *          
220800* I DEN HÄR KÖRNINGEN OCH RADEN GÅTT HELT I RESTORDER.         *          
220900* WS-KVBYGGBAR KAN ÄVEN HA VÄRDET 0.                           *          
221000****************************************************************          
221100 KA-BERAKNINGAR SECTION.                                                  
221200                                                                          
221300     MOVE    JA                  TO SW-ANT-BYGGB-FORSTA                   
221400                                    WS-FLBYGGB                            
221500     PERFORM IMS-GNP-WDJ2-SATG11-FIRST                                    
221600     IF WS-KVBYGGBAR = 9999999                                            
221700       MOVE NEJ TO WS-FLBYGGB                                             
221800     END-IF                                                               
221900                                                                          
222000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
222100                   SEGMENT-SLUT   OR                                      
222200                   WS-KVBYGGBAR = 9999999                                 
222300                                                                          
222400       IF SRAD-KVSATROS > 0                                               
222500         MOVE NEJ TO WS-FLBYGGB                                           
222600       END-IF                                                             
222700                                                                          
222800       MOVE NEJ TO SW-ANV-KOMBKOD-TRAFF                                   
222900                                                                          
223000       IF SRAD-REBEART > 0                                                
223100         IF SRAD-REANTPSA > 0                                             
223200           IF SRAD-KVSATROS = SRAD-REBEART AND                            
223300              SRAD-KDSATKMB = SPACE                                       
223400             MOVE 9999999      TO WS-KVBYGGBAR                            
223500           ELSE                                                           
223600             IF SRAD-KDSATAND = SPACE OR                                  
223700                SRAD-KDSATAND = 'T'   OR                                  
223800                SRAD-KDSATAND = 'R'   OR                                  
223900                SRAD-KDSATAND = 'N'   OR                                  
224000                SRAD-KDSATAND = 'Ä'                                       
224100               IF SRAD-KVSATRES > 0                                       
224200                 PERFORM S160-RAKNA-ANT-BYGGB                             
224300                 IF WS-FLBYGGB = JA                                       
224400                   IF SRAD-KDSATKMB = SPACE                               
224500                     PERFORM S200-RAKNA-VIKT-VOLYM-RAD                    
224600                   END-IF                                                 
224700                 END-IF                                                   
224800               ELSE                                                       
224900                 MOVE 9999999 TO WS-KVBYGGBAR                             
225000                 MOVE NEJ     TO WS-FLBYGGB                               
225100               END-IF                                                     
225200             END-IF                                                       
225300           END-IF                                                         
225400                                                                          
225500           IF SRAD-KDSATAND = 'U' OR                                      
225600              SRAD-KDSATAND = 'E'                                         
225700             IF SRAD-KVSATRES > 0                                         
225800               PERFORM S160-RAKNA-ANT-BYGGB                               
225900               IF WS-FLBYGGB = JA                                         
226000                 IF SRAD-KDSATKMB = SPACE                                 
226100                   PERFORM S200-RAKNA-VIKT-VOLYM-RAD                      
226200                 END-IF                                                   
226300               END-IF                                                     
226400             ELSE                                                         
226500               MOVE 9999999    TO WS-KVBYGGBAR                            
226600               MOVE NEJ        TO WS-FLBYGGB                              
226700             END-IF                                                       
226800           END-IF                                                         
226900         END-IF                                                           
227000       END-IF                                                             
227100                                                                          
227200       IF SRAD-KDSATAND = 'B'                                             
227300          CONTINUE                                                        
227400       END-IF                                                             
227500                                                                          
227600       PERFORM IMS-GNP-WDJ2-SATG11-REST                                   
227700                                                                          
227800     END-PERFORM                                                          
227900     .                                                                    
228000     EJECT                                                                
228100****************************************************************          
228200* L-BEHANDLA-NY-SATSORDER                                      *          
228300* OM EN INGÅENDE ARTIKEL FÖR SATSORDERN ÄR SPÄRRAD SKICKAS     *          
228400*    DEN TILL RESTORDER                                        *          
228500* ANNARS                                                       *          
228600*    SER MAN OM DET FINNS TÄCKNING I LAGER, SAMT RESERVERAR    *          
228700****************************************************************          
228800 L-BEHANDLA-NY-SATSORDER SECTION.                                         
228900                                                                          
229000     PERFORM IMS-GNP-WDJ2-SATK11-FIRST                                    
229100                                                                          
229200     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
229300                   SEGMENT-SLUT                                           
229400                                                                          
229500       MOVE    SHUV-IDORDNSB TO W-WDJ201-IDORDNSB                         
229600       MOVE    SHUV-IDORDNSS TO W-WDJ201-IDORDNSS                         
229700       MOVE    SRAD-IDARTNR  TO W-WDJ211-IDARTNR                          
229800       PERFORM IMS-GHU-WDJ211-SATG                                        
229900                                                                          
230000       PERFORM S400-SKAPA-2109-TRANS                                      
230100                                                                          
230200       MOVE    SRAD-IDARTNR  TO W-IDARTNR                                 
230300       PERFORM IMS-GU-WDK601                                              
230400       PERFORM IMS-GHNP-WDK611                                            
230500                                                                          
230600       IF CLAG-KDLEVSP = 20 OR                                            
230700          CLAG-KDLEVSP = 21                                               
230800* ---    ING.ARTIKEL SPÄRRAD                                              
230900         PERFORM LA-SPARRA-ING-ART                                        
231000       ELSE                                                               
231100* ---    ING.ARTIKEL EJ SPÄRRAD                                           
231200         PERFORM LB-RESERVERA-ING-ART                                     
231300       END-IF                                                             
231400       IF WS-FLBYGGB = JA                                                 
231500         PERFORM S200-RAKNA-VIKT-VOLYM-RAD                                
231600       END-IF                                                             
231700                                                                          
231800       PERFORM IMS-REPL-WDK611                                            
231900       PERFORM IMS-REPL-WDJ211-SATG                                       
232000                                                                          
232100       PERFORM IMS-GNP-WDJ2-SATK11-REST                                   
232200       MOVE    ZERO TO WS-RO-ANTAL                                        
232300                                                                          
232400     END-PERFORM                                                          
232500     .                                                                    
232600     EJECT                                                                
232700****************************************************************          
232800* LA-SPARRA-ING-ART                                            *          
232900* DETTA GÄLLER NYA SATSARTIKLAR                                *          
233000* DENNA INGÅENDE ARTIKELN ÄR SPÄRRAD, ALLTSÅ SKALL ANTAL       *          
233100* BESTÄLLT SKICKAS TILL RESTORDER.                             *          
233200****************************************************************          
233300 LA-SPARRA-ING-ART SECTION.                                               
233400                                                                          
233500     MOVE    SRAD-IDARTNR           TO ORDP-RAD-IDARTNR                   
233600     MOVE    2                      TO ORDP-RAD-KDROO                     
233700     MOVE    SRAD-REBEART           TO WS-RO-ANTAL                        
233800     PERFORM S01-SKAPA-RESTORDER                                          
233900     PERFORM S03-EV-LARM-2191-ANSKAFFN                                    
234000     MOVE    SRAD-REBEART           TO SRAD-KVSATROS                      
234100     MOVE    ZERO                   TO SRAD-KVSATRES                      
234200     MOVE    SRAD-REBEART           TO SRAD-KVSATROS                      
234300     ADD     SRAD-REBEART           TO CLAG-KVROS                         
234400     MOVE    9999999                TO WS-KVBYGGBAR                       
234500     MOVE    NEJ                    TO WS-FLBYGGB                         
234600                                       SW-ANT-BYGGB-FORSTA                
234700     .                                                                    
234800     EJECT                                                                
234900****************************************************************          
235000* LB-RESERVERA-ING-ART                                         *          
235100* DETTA GÄLLER NYA SATSARTIKLAR                                *          
235200* KOLLA MOT DISPONIBELT LAGERSALDO, OM BESTÄLLT ANTAL FINNS.   *          
235300* OM NÅGOT SAKNAS, SKICKAS DE TILL RESTORDER                   *          
235400****************************************************************          
235500 LB-RESERVERA-ING-ART SECTION.                                            
235600                                                                          
235700     PERFORM S310-DISP-LAGERSALDO                                         
235800                                                                          
235900     IF WS-DISPLS >= SRAD-REBEART                                         
236000* ---  ALLT FINNS I LAGER                                                 
236100       ADD  SRAD-REBEART TO CLAG-KVRESS                                   
236200       ADD  SRAD-REBEART TO SRAD-KVSATRES                                 
236300       MOVE ZERO         TO SRAD-KVSATROS                                 
236400       PERFORM S160-RAKNA-ANT-BYGGB                                       
236500     ELSE                                                                 
236600       MOVE SRAD-IDARTNR TO ORDP-RAD-IDARTNR                              
236700       MOVE 1            TO ORDP-RAD-KDROO                                
236800       MOVE NEJ          TO WS-FLBYGGB                                    
236900       IF WS-DISPLS > 0                                                   
237000         COMPUTE WS-RO-ANTAL = SRAD-REBEART - WS-DISPLS                   
237100* ---    VISS DEL FINNS I LAGER                                           
237200         MOVE WS-RO-ANTAL TO SRAD-KVSATROS                                
237300         COMPUTE SRAD-KVSATRES = SRAD-REBEART -                           
237400                                 WS-RO-ANTAL                              
237500         ADD SRAD-KVSATRES TO CLAG-KVRESS                                 
237600         PERFORM S160-RAKNA-ANT-BYGGB                                     
237700       ELSE                                                               
237800* ---    INGET FINNS I LAGER                                              
237900         MOVE SRAD-REBEART TO WS-RO-ANTAL                                 
238000                              SRAD-KVSATROS                               
238100         ADD  ZERO         TO SRAD-KVSATRES                               
238200         MOVE 9999999 TO WS-KVBYGGBAR                                     
238300         MOVE NEJ     TO SW-ANT-BYGGB-FORSTA                              
238400       END-IF                                                             
238500       PERFORM S01-SKAPA-RESTORDER                                        
238600       PERFORM S03-EV-LARM-2191-ANSKAFFN                                  
238700       ADD     WS-RO-ANTAL TO CLAG-KVROS                                  
238800     END-IF                                                               
238900     .                                                                    
239000     EJECT                                                                
239100 M-SATSORDER-BYGGBAR-ELLER-EJ SECTION.                                    
239200                                                                          
239300     MOVE    SHUV-IDORDNSB TO W-WDJ201-IDORDNSB                           
239400     MOVE    SHUV-IDORDNSS TO W-WDJ201-IDORDNSS                           
239500     PERFORM IMS-GHU-WDJ201-SATG                                          
239600                                                                          
239700     IF WS-FLBYGGB   = JA                                                 
239800       PERFORM S04-BYGGBAR                                                
239900     ELSE                                                                 
240000       PERFORM S05-EJBYGGBAR                                              
240100     END-IF                                                               
240200                                                                          
240300     MOVE    WS-FLBYGGB       TO SHUV-FLBYGGB                             
240400     MOVE    NEJ              TO SHUV-FLSATNYO                            
240500     MOVE    DAGENS-DATUM-NUM TO SHUV-TIUPPDAT                            
240600     PERFORM IMS-REPL-WDJ201-SATG                                         
240700     PERFORM IMS-CHECKPOINT                                               
240800     .                                                                    
240900     EJECT                                                                
241000 N-LAS-FRAM-AKT-SATSORDERHUVUVD SECTION.                                  
241100                                                                          
241200     PERFORM IMS-GU-WDJ2-SATK-DSEQ-AKT                                    
241300     IF SHUV-IDORDNST = WS-IDORDNST-AKT                                   
241400       PERFORM IMS-GN-WDJ2-SATK-DSEQ-AKT                                  
241500     ELSE                                                                 
241600       PERFORM UNTIL SHUV-IDORDNST = WS-IDORDNST-AKT                      
241700         PERFORM IMS-GN-WDJ2-SATK-DSEQ-AKT                                
241800       END-PERFORM                                                        
241900       PERFORM IMS-GN-WDJ2-SATK-DSEQ-AKT                                  
242000     END-IF                                                               
242100     .                                                                    
242200     EJECT                                                                
242300 Z-FINIT SECTION.                                                         
242400                                                                          
242500     CLOSE FELLISTA                                                       
242600     .                                                                    
242700     EJECT                                                                
242800 S01-SKAPA-RESTORDER SECTION.                                             
242900                                                                          
243000     MOVE SHUV-IDDISTR           TO ORDP-RAD-IDDISTR                      
243100     MOVE SHUV-IDKUNDNR          TO ORDP-RAD-IDKUNDNR                     
243200     MOVE SHUV-IDORDNSB          TO WS-IDORDNSB                           
243300     MOVE SHUV-IDORDNSS          TO WS-IDORDNSS                           
243400     MOVE SPACE                  TO ORDP-RAD-IDKUNDRF                     
243500     MOVE WS-IDORDNST-NUM        TO ORDP-RAD-IDORDNR5                     
243600     MOVE 1                      TO ORDP-RAD-IDLOPNR                      
243700     MOVE SPACE                  TO ORDP-RAD-BEKUNDRF                     
243800     MOVE SPACE                  TO ORDP-RAD-BERADREF                     
243900     MOVE NEJ                    TO ORDP-RAD-FLERS                        
244000                                                                          
244100     MOVE ORDP-RAD-IDARTNR       TO W-IDARTNR-X                           
244200     MOVE CLAG-IDANSK            TO ORDP-RAD-IDANSK                       
244300     MOVE SRAD-IDKONTO           TO ORDP-RAD-IDKONTO                      
244400     MOVE SRAD-IDKST             TO ORDP-RAD-IDKST                        
244500     MOVE SHUV-IDANALYS          TO ORDP-RAD-IDANALYS                     
244600     MOVE SPACE                  TO ORDP-RAD-IDKUNDRF-LEV                 
244700     MOVE WC-CDC-SE              TO ORDP-RAD-IDDC                         
244800                                    ORDP-RAD-IDDC-RO                      
244900     MOVE ZERO                   TO ORDP-RAD-KDDSP                        
245000     MOVE SHUV-KDFAKTYP          TO ORDP-RAD-KDFAKTYP                     
245100     MOVE ZERO                   TO ORDP-RAD-KDFRAKT                      
245200     MOVE ZERO                   TO ORDP-RAD-KDKVBRYT                     
245300     MOVE 3                      TO ORDP-RAD-KDORDING                     
245400     MOVE SHUV-KDORDKL           TO ORDP-RAD-KDORDKL                      
245500     MOVE SRAD-KDPRODSL          TO ORDP-RAD-KDPRODSL                     
245600     MOVE WS-RO-ANTAL            TO ORDP-RAD-KVRO                         
245700     MOVE '2'                    TO ORDP-RAD-KDSTARAD                     
245800     MOVE ZERO                   TO ORDP-RAD-KDTPOTYP                     
245900     MOVE ZERO                   TO ORDP-RAD-KDVRINFO                     
246000     MOVE WS-RO-ANTAL            TO ORDP-RAD-KVART                        
246100     MOVE ZERO                   TO ORDP-RAD-PRARTNTO                     
246200     MOVE SRAD-REKSIFFR          TO ORDP-RAD-REKSIFFR                     
246300     MOVE ZERO                   TO ORDP-RAD-TIAVBOKN                     
246400     MOVE SHUV-DAREGDAT (3:6)    TO ORDP-RAD-TIREGDAT                     
246500     MOVE ZERO                   TO ORDP-RAD-TIRES                        
246600     MOVE DAGENS-DATUM-Y2K       TO ORDP-RAD-DARODAT                      
246700     MOVE ZERO                   TO ORDP-RAD-TITPO                        
246800     MOVE SPACE                  TO ORDP-RAD-KDPRTYP                      
246900     MOVE SPACE                  TO ORDP-RAD-BEVOLREF                     
247000     MOVE NEJ                    TO ORDP-RAD-FLINVEST                     
247100     MOVE NEJ                    TO ORDP-RAD-FLPRTILL                     
247200     MOVE JA                     TO ORDP-RAD-FLTPOBEK                     
247300     MOVE ZERO                   TO ORDP-RAD-IDKAMPRF                     
247400     MOVE SHUV-IDLEVNR           TO ORDP-RAD-IDLEVNR                      
247500     MOVE 'SATS'                 TO ORDP-RAD-IDSYSTEM                     
247600     MOVE SRAD-REBEART           TO ORDP-RAD-KVBEART-Q                    
247700     MOVE WS-TIME  (1:6)         TO ORDP-RAD-TIREGTID                     
247800     MOVE 999                    TO ORDP-RAD-DASENDAT                     
247900                                    ORDP-RAD-TISENBEK-KL                  
248000     INITIALIZE                     ORDP-RAD-DEAL-PR-LINE                 
248100                                                                          
248200     MOVE SPACE                  TO ORDP-RAD-KDORDTYP-LDC                 
248300     MOVE ZERO                   TO ORDP-RAD-TIREPDAT                     
248400     MOVE SPACE                  TO ORDP-RAD-IDKUNDRF-WIP                 
248600     MOVE SPACE                  TO ORDP-RAD-CLEARGROUP                   
248610     MOVE +0                     TO ORDP-RAD-PRAVCOST                     
248620     MOVE SPACE                  TO ORDP-RAD-KDROPACK                     
248630     MOVE SPACE                  TO ORDP-RAD-IDARBREF                     
248700                                                                          
248800     PERFORM S01A-HAMTA-PRIORITETSKOD                                     
248900                                                                          
249000     PERFORM IMS-ISRT-WDA5-WLORDP01                                       
249100                                                                          
249200     PERFORM UNTIL SEGMENT-FINNS                                          
249300        ADD     +1 TO ORDP-RAD-IDLOPNR                                    
249400        PERFORM IMS-ISRT-WDA5-WLORDP01                                    
249500     END-PERFORM                                                          
249600     .                                                                    
249700     EJECT                                                                
249800 S01A-HAMTA-PRIORITETSKOD SECTION.                                        
249900                                                                          
250000     MOVE ORDP-RAD-KDTPOTYP      TO W-4512-KDTPOTYP                       
250100     MOVE SHUV-KDORDKL           TO W-4512-KDORDKL                        
250200     MOVE SHUV-IDDISTR           TO W-4512-IDDISTR-FOM                    
250300                                    W-4512-IDDISTR-TOM                    
250400                                                                          
250500     PERFORM IMS-GU-XXJN-WLXXJN11                                         
250600     MOVE 4512-KDRAPRIO          TO ORDP-RAD-KDRAPRIO                     
250700     .                                                                    
250800     EJECT                                                                
250900 S03-EV-LARM-2191-ANSKAFFN SECTION.                                       
251000                                                                          
251100* --- ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS.          
251200     IF CLAG-KVROS = 0                                                    
251300       IF (CLAG-KVAKS-CDC + CLAG-KVAKS-PAV + CLAG-KVAKS-T) = 0            
251400         COMPUTE ALT2191-LL = LENGTH OF ALT2191-MID-W2I19101 + 17         
251500         MOVE W-KDCLAGER         TO ALT2191-MID-KDCLAGER                  
251600         MOVE W-IDARTNR          TO ALT2191-MID-IDARTNR                   
251700         MOVE ZERO               TO ALT2191-MID-TISENBEK-DAG              
251800                                    ALT2191-MID-TISENBEK-KL               
251900         MOVE SPACE              TO ALT2191-MID-IDKR                      
252000         MOVE CLAG-IDANSK        TO ALT2191-MID-IDANSK                    
252100         MOVE 210                TO ALT2191-MID-KDLARM                    
252200         MOVE SHUV-IDDISTR       TO WS-IDDISTR-NUM4                       
252300         MOVE WS-IDDISTR-NUM4    TO ALT2191-MID-IDDISTR                   
252400         MOVE SHUV-IDKUNDNR      TO WS-IDKUNDNR-NUM6                      
252500         MOVE WS-IDKUNDNR-NUM6   TO ALT2191-MID-IDKUNDNR                  
252600         MOVE SHUV-IDORDNSB      TO ALT2191-MID-IDORDNR5(1:4)             
252700         MOVE SHUV-IDORDNSS      TO ALT2191-MID-IDKUNDRF(5:1)             
252800         MOVE 'J'                TO ALT2191-MID-FLNYLARM                  
252900         MOVE WC-CDC-SE          TO ALT2191-MID-IDDC                      
253000         MOVE SPACE              TO ALT2191-MID-IDLEVNR                   
253100                                                                          
253200         PERFORM IMS-PURGE-ALT2191-MSG                                    
253300       END-IF                                                             
253400     END-IF                                                               
253500     .                                                                    
253600     EJECT                                                                
253700****************************************************************          
253800* S04-BYGGBAR                                                  *          
253900* DENNA SEKTION GÄLLER NÄR EN SATSORDER BLIR BYGGBAR           *          
254000* VIKT OCH VOLYM AVRUNDAS. PRODUKTIONSTID HÄMTAS.              *          
254100****************************************************************          
254200 S04-BYGGBAR SECTION.                                                     
254300                                                                          
254400* --- UPPDATERA ELEMENT I ORDERHUVUDET                                    
254500     MOVE SHUV-KVBEART         TO  SHUV-KVBYGGB                           
254600     COMPUTE WS-VKORDNTO-1DEC ROUNDED =                                   
254700                                   WS-VKORDNTO-3DEC                       
254800     MOVE WS-VKORDNTO-1DEC     TO  SHUV-VKORDNTO                          
254900     COMPUTE WS-VLORDNTO-M ROUNDED =                                      
255000                                   WS-VLORDNTO-CM / 1000000               
255100     MOVE WS-VLORDNTO-M        TO  SHUV-VLORDNTO                          
255200* --- BERÄKNA PRODUKTIONSTID                                              
255300     MOVE 'SATS'               TO  PTID-IDSYSTEM                          
255400     MOVE SHUV-IDORDNSB        TO  PTID-IDORDNSB                          
255500     MOVE SHUV-IDORDNSS        TO  PTID-IDORDNSS                          
255600     MOVE SHUV-IDARTNR         TO  PTID-IDARTNR                           
255700     MOVE SHUV-IDPRC           TO  PTID-IDPRC                             
255800     MOVE SHUV-KDCLAGER        TO  PTID-KDCLAGER                          
255900     MOVE SHUV-KVBYGGB         TO  PTID-KVBYGGB                           
256000     MOVE WS-KVRADER           TO  PTID-KVRADER                           
256100                                   PTID-KVANTART                          
256200     MOVE ZERO                 TO  PTID-SUSATPTI                          
256300     MOVE SHUV-VKORDNTO        TO  PTID-VKORDNTO                          
256400     MOVE SHUV-VLORDNTO        TO  PTID-VLORDNTO                          
256500     MOVE SPACE                TO  PTID-KDSVAR                            
256600                                                                          
256700     CALL W416PTID USING PTID-W416PTID XXKH-PCB XXKI-PCB                  
256800                                       SATB-PCB                           
256900                                                                          
257000     IF PTID-KDSVAR            =   ZERO                                   
257100         MOVE PTID-SUSATPTI    TO  SHUV-SUSATPTI                          
257200      ELSE                                                                
257300         MOVE ZERO             TO  SHUV-SUSATPTI                          
257400     END-IF                                                               
257500     .                                                                    
257600     EJECT                                                                
257700****************************************************************          
257800* S05-EJBYGGBAR                                                *          
257900* UPPDATERAR ANTAL BYGGBARA SATSARTIKLAR I SATSORDERN.         *          
258000****************************************************************          
258100 S05-EJBYGGBAR SECTION.                                                   
258200                                                                          
258300     IF WS-KVBYGGBAR NOT = ZERO                                           
258400                                                                          
258500       IF WS-KVBYGGBAR = 9999999                                          
258600         MOVE ZERO             TO SHUV-KVBYGGB                            
258700       ELSE                                                               
258800         IF WS-KVBYGGBAR > SHUV-KVBEART                                   
258900* ---      HAR BLIVIT STÖRRE ÄN, BEROENDE PÅ AVRUNDNINGAR                 
259000           MOVE SHUV-KVBEART  TO SHUV-KVBYGGB                             
259100         ELSE                                                             
259200           MOVE WS-KVBYGGBAR  TO SHUV-KVBYGGB                             
259300         END-IF                                                           
259400       END-IF                                                             
259500                                                                          
259600     END-IF                                                               
259700     .                                                                    
259800     EJECT                                                                
259900****************************************************************          
260000* S06-OBER-TILLG-MED-WDD7                                      *          
260100* ERSÄTTNINGSKODEN ÄR OBEROENDE AV TILLGÅNG (KOD 22 EL 23)     *          
260200* ELLER KOD 11 EL 21 OCH ALLT I RESTORDER                      *          
260300* WDD7 KONTROLLERAS, SÅ ERSÄTTNINGSINFO FINNS.                 *          
260400* ARBETSTABELL FÖR TILLKOMMANDE ING ARTIKLAR MOTSVARANDE TN-   *          
260500* TABELLEN SKAPAS FÖR DEN ERSATTA ARTIKELN.                    *          
260600* FÅR MAN INGEN MATCHNING, TAR MAN POSTEN SOM EN UTGÅNGS-      *          
260700* MARKERAD ING.ART.                                            *          
260800****************************************************************          
260900 S06-OBER-TILLG-MED-WDD7 SECTION.                                         
261000                                                                          
261100     MOVE ARB-E-IDARTNR TO W-IDARTNR                                      
261200     PERFORM IMS-GU-WDD701-ERSA                                           
261300     IF SEGMENT-FINNS                                                     
261400        PERFORM S40-LAS-WDD7-TILL-TABELL                                  
261500     ELSE                                                                 
261600        DISPLAY '*******************************************'             
261700        DISPLAY '*** WDD7 SAKNAS FÖR INGÅENDE ARTIKEL    ***'             
261800        DISPLAY '*** ING ARTIKEL: ' ARB-E-IDARTNR                         
261900        DISPLAY '*** SECT S06-                           ***'             
262000        DISPLAY '*******************************************'             
262100        CALL ABEND USING ABEND-UTAN-DUMP                                  
262200     END-IF                                                               
262300                                                                          
262400     PERFORM S300-MATCHA-ARB-T-TAB                                        
262500                                                                          
262600     IF SW-RASA-SPARR = NEJ                                               
262700        IF SW-UTG     = NEJ                                               
262800           PERFORM S90-ERS-MARKERING                                      
262900           IF SW-RASA-SPARR = NEJ                                         
263000              IF ARB-T-IDX > 1                                            
263100                 PERFORM S303-SORTERA-ARB-T-TAB                           
263200              END-IF                                                      
263300                                                                          
263400              SET ARB-T-IDX TO +1                                         
263500              PERFORM UNTIL ARB-T-IDX > 15 OR                             
263600                            ARB-T-IDARTNR(ARB-T-IDX) = ZERO OR            
263700                            SW-RASA-SPARR = JA                            
263800                 PERFORM S55-MATCHA-T-MOT-SATS                            
263900                 IF SW-RASA-SPARR = NEJ                                   
264000                    IF SW-SATS-TRAFF = JA                                 
264100* ---    RAD MED SAMMA ARTIKELNR FINNS SEDAN GAMMALT I ORDERN             
264200* ---    ELLER HAR REDAN BEARBETATS I DEN HÄR KÖRNINGEN                   
264300                       IF SATS-KDSATAND(SATS-IDX) NOT = SPACE OR          
264400                          SATS-NYKDSATAND(SATS-IDX) NOT =                 
264500                               (SPACE OR 'T')                             
264600                          MOVE JA TO SW-RASA-SPARR                        
264700                       ELSE                                               
264800* ---    DEN TILLKOMMANDE ARTIKELN FINNS REDAN. KONTROLL                  
264900* ---    ATT NYTT OCH GAMMALT ANTAL PÅ RADEN ÄR ENTYDIGT.                 
265000* ---    ANNARS SPÄRRAS ORDERN.                                           
265100                          PERFORM S77-BERAKNA-SATS-ANTAL                  
265200                          IF ((SATS-REBEART(SATS-IDX) = 0 OR              
265300                               SATS-REBEART(SATS-IDX) =                   
265400                               WS-REBEART-KOLL) AND                       
265500                              (SATS-NYREBEART(SATS-IDX) = 0 OR            
265600                               SATS-NYREBEART(SATS-IDX) =                 
265700                               WS-NYREBEART-KOLL))                        
265800                              PERFORM S100-TILLK-GAMMAL-ART               
265900                          ELSE                                            
266000                             MOVE JA TO SW-RASA-SPARR                     
266100                          END-IF                                          
266200                       END-IF                                             
266300                    ELSE                                                  
266400* ---   TILLKOMMANDE FINNS EJ I SATS SEDAN FÖRUT                          
266500                       PERFORM S60-HITTA-LEDIG-SATS                       
266600                       IF SW-SATS-TRAFF = JA                              
266700                          PERFORM S101-TILLK-NY-ART                       
266800                       ELSE                                               
266900                          MOVE JA TO SW-RASA-SPARR                        
267000                       END-IF                                             
267100                    END-IF                                                
267200                                                                          
267300                 END-IF                                                   
267400                 SET ARB-T-IDX UP BY +1                                   
267500              END-PERFORM                                                 
267600           END-IF                                                         
267700        ELSE                                                              
267800           PERFORM S80-UTG-MARKERING                                      
267900        END-IF                                                            
268000     END-IF                                                               
268100     .                                                                    
268200     EJECT                                                                
268300****************************************************************          
268400* S08-BEH-NYTILLKOMNA                                          *          
268500* INGÅENDE ARTIKEL ÄR MARKERAD SOM NY, KDISATS = N             *          
268600* LÄSER IGENOM RASA-T-TABELLEN FÖR ATT HITTA DESSA.            *          
268700****************************************************************          
268800 S08-BEH-NYTILLKOMNA SECTION.                                             
268900                                                                          
269000     SET T-INDX TO +1                                                     
269100                                                                          
269200     PERFORM UNTIL T-INDX                  > 175  OR                      
269300                   RASA-T-IDARTNR (T-INDX) = ZERO OR                      
269400                   SW-RASA-SPARR           = JA                           
269500                                                                          
269600        IF RASA-T-ANV(T-INDX) = NEJ                                       
269700           PERFORM S56-MATCHA-RASA-T-MOT-SATS                             
269800           IF SW-SATS-TRAFF = JA                                          
269900              PERFORM S110-NY-MARKERING                                   
270000              MOVE    JA TO RASA-T-ANV(T-INDX)                            
270100           ELSE                                                           
270200              IF SW-RASA-SPARR = NEJ                                      
270300                 PERFORM S60-HITTA-LEDIG-SATS                             
270400                 PERFORM S110-NY-MARKERING                                
270500                 MOVE    JA TO RASA-T-ANV(T-INDX)                         
270600              END-IF                                                      
270700           END-IF                                                         
270800        END-IF                                                            
270900                                                                          
271000        SET T-INDX UP       BY +1                                         
271100                                                                          
271200     END-PERFORM                                                          
271300     .                                                                    
271400     EJECT                                                                
271500****************************************************************          
271600* S10-SKAPA-TOMMA-TABELLER                                     *          
271700****************************************************************          
271800 S10-SKAPA-TOMMA-TABELLER SECTION.                                        
271900                                                                          
272000     SET E-TOM              TO +1                                         
272100     SET T-TOM              TO +1                                         
272200     PERFORM UNTIL E-TOM > 175 AND                                        
272300                   T-TOM > 175                                            
272400       MOVE ZERO   TO  TOM-RASA-E-KDERS    (E-TOM)                        
272500       MOVE ZERO   TO  TOM-RASA-E-IDARTNR  (E-TOM)                        
272600       MOVE ZERO   TO  TOM-RASA-E-TISTODAT (E-TOM)                        
272700       MOVE ZERO   TO  TOM-RASA-E-REANTPSA (E-TOM)                        
272800       MOVE SPACE  TO  TOM-RASA-E-KDISATS  (E-TOM)                        
272900                                                                          
273000       MOVE ZERO   TO  TOM-RASA-T-KDERS    (T-TOM)                        
273100       MOVE ZERO   TO  TOM-RASA-T-IDARTNR  (T-TOM)                        
273200       MOVE ZERO   TO  TOM-RASA-T-TISTADAT (T-TOM)                        
273300       MOVE ZERO   TO  TOM-RASA-T-REANTPSA (T-TOM)                        
273400       MOVE SPACE  TO  TOM-RASA-T-KDISATS  (T-TOM)                        
273500       MOVE SPACE  TO  TOM-RASA-T-KDSTRRAD (T-TOM)                        
273600       MOVE NEJ    TO  TOM-RASA-T-ANV      (T-TOM)                        
273700       SET  E-TOM UP BY +1                                                
273800       SET  T-TOM UP BY +1                                                
273900     END-PERFORM                                                          
274000                                                                          
274100     SET SATS-TOM            TO +1                                        
274200     PERFORM UNTIL SATS-TOM > 250                                         
274300       MOVE ZERO   TO TOM-SATS-HUV-KVBEART                                
274400       MOVE SPACE  TO TOM-SATS-HUV-KDSATKMB                               
274500       MOVE ZERO   TO TOM-SATS-IDARTNR     (SATS-TOM)                     
274600       MOVE ZERO   TO TOM-SATS-KDCLAGER    (SATS-TOM)                     
274700       MOVE SPACE  TO TOM-SATS-KDSATAND    (SATS-TOM)                     
274800       MOVE SPACE  TO TOM-SATS-KDSATKMB    (SATS-TOM)                     
274900       MOVE ZERO   TO TOM-SATS-KDERS       (SATS-TOM)                     
275000       MOVE SPACE  TO TOM-SATS-KDSTRRAD    (SATS-TOM)                     
275100       MOVE ZERO   TO TOM-SATS-KVSATRES    (SATS-TOM)                     
275200       MOVE ZERO   TO TOM-SATS-REANTPSA    (SATS-TOM)                     
275300       MOVE ZERO   TO TOM-SATS-REBEART     (SATS-TOM)                     
275400       MOVE SPACE  TO TOM-SATS-NYKDSATAND  (SATS-TOM)                     
275500       MOVE ZERO   TO TOM-SATS-NYREANTPSA  (SATS-TOM)                     
275600       MOVE SPACE  TO TOM-SATS-NYKDSATKMB  (SATS-TOM)                     
275700       MOVE ZERO   TO TOM-SATS-NYREBEART   (SATS-TOM)                     
275800       MOVE ZERO   TO TOM-SATS-NYKVSATROS  (SATS-TOM)                     
275900       MOVE SPACE  TO TOM-SATS-SPARR       (SATS-TOM)                     
276000       SET SATS-TOM UP            BY +1                                   
276100     END-PERFORM                                                          
276200                                                                          
276300     SET WDD7-TOM  TO +1                                                  
276400     MOVE ZERO     TO TOM-WDD7-DIERS-ERS                                  
276500     PERFORM UNTIL WDD7-TOM > 10                                          
276600       MOVE ZERO   TO TOM-WDD7-IDARTNR(WDD7-TOM)                          
276700       MOVE ZERO   TO TOM-WDD7-DIERS-TILLK(WDD7-TOM)                      
276800       MOVE NEJ    TO TOM-WDD7-ANV(WDD7-TOM)                              
276900       SET WDD7-TOM UP            BY +1                                   
277000     END-PERFORM                                                          
277100                                                                          
277200     SET ARB-T-TOM      TO +1                                             
277300     PERFORM UNTIL ARB-T-TOM > 15                                         
277400       MOVE ZERO   TO TOM-ARB-T-KDERS   (ARB-T-TOM)                       
277500       MOVE ZERO   TO TOM-ARB-T-IDARTNR (ARB-T-TOM)                       
277600       MOVE ZERO   TO TOM-ARB-T-TISTADAT(ARB-T-TOM)                       
277700       MOVE ZERO   TO TOM-ARB-T-REANTPSA(ARB-T-TOM)                       
277800       MOVE SPACE  TO TOM-ARB-T-KDISATS (ARB-T-TOM)                       
277900       MOVE SPACE  TO TOM-ARB-T-KDSTRRAD(ARB-T-TOM)                       
278000       SET ARB-T-TOM UP BY +1                                             
278100     END-PERFORM                                                          
278200                                                                          
278300     MOVE ZERO     TO TOM-ARB-E-IDARTNR                                   
278400     MOVE ZERO     TO TOM-ARB-E-REANTPSA-TOT                              
278500     MOVE ZERO     TO TOM-ARB-E-KDERS                                     
278600     MOVE SPACE    TO TOM-ARB-E-KDISATS                                   
278700     SET ARB-E-TOM TO +1                                                  
278800                                                                          
278900     PERFORM UNTIL ARB-E-TOM > 15                                         
279000       MOVE ZERO   TO TOM-ARB-E-REANTPSA(ARB-E-TOM)                       
279100       MOVE ZERO   TO TOM-ARB-E-TISTODAT(ARB-E-TOM)                       
279200       SET ARB-E-TOM UP BY +1                                             
279300     END-PERFORM                                                          
279400                                                                          
279500     SET SPAR-KOMB-TOM  TO +1                                             
279600     PERFORM UNTIL SPAR-KOMB-TOM > 26                                     
279700       MOVE SPACE  TO TOM-SPAR-KDSATKMB(SPAR-KOMB-TOM)                    
279800       SET SPAR-KOMB-TOM  UP BY +1                                        
279900     END-PERFORM                                                          
280000     .                                                                    
280100     EJECT                                                                
280200****************************************************************          
280300* S31- S32- AVSLUTA-XXX                                        *          
280400* DESSA SEKTIONER ANVÄNDS FÖR ATT AVSLUTA ARBETSTABELLER       *          
280500****************************************************************          
280600 S30-NASTA-SATSORDER SECTION.                                             
280700                                                                          
280800     PERFORM UNTIL E-INDX           > 175 OR                              
280900             RASA-E-IDARTNR(E-INDX) = ZERO                                
281000       SET E-INDX UP BY +1                                                
281100     END-PERFORM                                                          
281200     .                                                                    
281300     SKIP3                                                                
281400 S31-AVSLUTA-ARB-E SECTION.                                               
281500                                                                          
281600     PERFORM UNTIL ARB-E-IDX > 15                                         
281700       SET ARB-E-IDX UP BY +1                                             
281800     END-PERFORM                                                          
281900     .                                                                    
282000     EJECT                                                                
282100****************************************************************          
282200* S34-SKAPA-ARB-E-TAB                                          *          
282300* DENNA SEKTION SKAPAR EN ARBETSTABELL PER ERSATT ARTIKEL-     *          
282400* NUMMER I RASA MED UTGÅNGSPUNKT FRÅN RASA-E-TABELL.           *          
282500* NÄR ARBETSTABELLEN SKAPATS SÄTTS E-INDX TILLBAKA TILL        *          
282600* SIST ANVÄNDA TABELLRAD.                                      *          
282700****************************************************************          
282800 S34-SKAPA-ARB-E-TAB SECTION.                                             
282900                                                                          
283000     PERFORM UNTIL E-INDX                 > 175   OR                      
283100                   RASA-E-IDARTNR(E-INDX) = ZERO  OR                      
283200                   ARB-E-IDX              > 15    OR                      
283300                   SW-SATS-FEL            = JA    OR                      
283400                   SW-RASA-SPARR          = JA                            
283500                                                                          
283600       IF ARB-E-IDARTNR = 0                                               
283700         PERFORM S35-SKAPA-ARB-E-TAB-FORSTA                               
283800       ELSE                                                               
283900         IF ARB-E-IDARTNR = RASA-E-IDARTNR(E-INDX)                        
284000           IF ARB-E-KDISATS = RASA-E-KDISATS(E-INDX)                      
284100             PERFORM S36-SKAPA-ARB-E-TAB-NASTA                            
284200           ELSE                                                           
284300             MOVE JA TO SW-RASA-SPARR                                     
284400           END-IF                                                         
284500         ELSE                                                             
284600           PERFORM S31-AVSLUTA-ARB-E                                      
284700           SET E-INDX DOWN BY +1                                          
284800         END-IF                                                           
284900       END-IF                                                             
285000                                                                          
285100       SET E-INDX UP BY +1                                                
285200     END-PERFORM                                                          
285300                                                                          
285400     SET E-INDX DOWN BY +1                                                
285500     .                                                                    
285600     EJECT                                                                
285700****************************************************************          
285800* S35-SKAPA-ARB-E-TAB-FORSTA OCH S36-SKAPA-ARB-E-TAB           *          
285900* DESSA SEKTIONER ANVÄNDS FÖR ATT HANTERA FLERA RADER I RASA   *          
286000* MED SAMMA ARTIKELNR. I SATS FINNS ENDAST EN RAD PER ARTIKELNR*          
286100****************************************************************          
286200 S35-SKAPA-ARB-E-TAB-FORSTA SECTION.                                      
286300                                                                          
286400     SET ARB-E-IDX TO +1                                                  
286500     MOVE RASA-E-IDARTNR  (E-INDX) TO ARB-E-IDARTNR                       
286600     MOVE RASA-E-REANTPSA (E-INDX) TO ARB-E-REANTPSA-TOT                  
286700                                      ARB-E-REANTPSA(ARB-E-IDX)           
286800     MOVE RASA-E-TISTODAT (E-INDX) TO ARB-E-TISTODAT(ARB-E-IDX)           
286900     MOVE RASA-E-KDISATS  (E-INDX) TO ARB-E-KDISATS                       
287000     MOVE RASA-E-KDERS    (E-INDX) TO ARB-E-KDERS                         
287100     .                                                                    
287200     EJECT                                                                
287300****************************************************************          
287400* S36-SKAPA-ARB-E-TAB-NASTA                                    *          
287500* DENNA SEKTION ANVÄNDS FÖR ATT HANTERA RADERNA EFTER DEN      *          
287600* FÖRSTA NÄR RASA HAR FLERA RADER MED SAMMA ARTIKELNR          *          
287700****************************************************************          
287800 S36-SKAPA-ARB-E-TAB-NASTA SECTION.                                       
287900                                                                          
288000     SET ARB-E-IDX UP BY +1                                               
288100     ADD  RASA-E-REANTPSA (E-INDX) TO ARB-E-REANTPSA-TOT                  
288200     MOVE RASA-E-REANTPSA (E-INDX) TO ARB-E-REANTPSA(ARB-E-IDX)           
288300     MOVE RASA-E-TISTODAT (E-INDX) TO ARB-E-TISTODAT(ARB-E-IDX)           
288400     .                                                                    
288500     EJECT                                                                
288600****************************************************************          
288700* S40-LAS-WDD7-TILL-TABELL                                     *          
288800****************************************************************          
288900 S40-LAS-WDD7-TILL-TABELL SECTION.                                        
289000                                                                          
289100     MOVE ERSA-DIERS-ERS      TO WDD7-DIERS-ERS                           
289200                                                                          
289300     SET WDD7-IDX TO +1                                                   
289400     PERFORM IMS-GNP-WDD702-ERSA                                          
289500                                                                          
289600     PERFORM UNTIL SEGMENT-SAKNAS                                         
289700                                                                          
289800        IF ERSA-FLTEXT = NEJ                                              
289900          MOVE ERSA-IDARTNR-TILLK  TO WDD7-IDARTNR(WDD7-IDX)              
290000          MOVE ERSA-DIERS-TILLK    TO WDD7-DIERS-TILLK(WDD7-IDX)          
290100          SET WDD7-IDX UP          BY +1                                  
290200        END-IF                                                            
290300                                                                          
290400        PERFORM IMS-GNP-WDD702-ERSA                                       
290500                                                                          
290600     END-PERFORM                                                          
290700     .                                                                    
290800     EJECT                                                                
290900****************************************************************          
291000* S50-MATCHA-ARB-E-MOT-SATS                                    *          
291100* MATCHA RASA:S INGÅENDE ARTIKEL (ERSATT) MED                  *          
291200* SATS INGÅENDE ARTIKEL. KONTROLLERA ANTAL.                    *          
291300* VID UTGÅNGEN AV SEKTIONEN ÄR SW-SATS-TRAFF = JA  ELLER       *          
291400* SW-SATS-TRAFF = NEJ OCH SW-RASA-SPARR = JA                   *          
291500****************************************************************          
291600 S50-MATCHA-ARB-E-MOT-SATS SECTION.                                       
291700                                                                          
291800     MOVE NEJ               TO SW-SATS-TRAFF                              
291900     SET  SATS-IDX          TO +1                                         
292000                                                                          
292100     PERFORM UNTIL SATS-IDX               > 250  OR                       
292200                   SW-SATS-TRAFF          = JA   OR                       
292300                   SW-RASA-SPARR          = JA   OR                       
292400                   SATS-IDARTNR(SATS-IDX) = ZERO                          
292500                                                                          
292600       IF ARB-E-IDARTNR = SATS-IDARTNR(SATS-IDX)                          
292700         IF SATS-KDSATAND(SATS-IDX) = 'B' OR 'N' OR 'Ä'                   
292800                                          OR 'U' OR 'E'                   
292900           MOVE JA TO SW-RASA-SPARR                                       
293000         ELSE                                                             
293100           IF ARB-E-REANTPSA-TOT = SATS-REANTPSA(SATS-IDX)                
293200             MOVE JA TO SW-SATS-TRAFF                                     
293300           ELSE                                                           
293400             MOVE JA TO SW-RASA-SPARR                                     
293500           END-IF                                                         
293600         END-IF                                                           
293700       ELSE                                                               
293800         SET SATS-IDX UP  BY +1                                           
293900       END-IF                                                             
294000                                                                          
294100     END-PERFORM                                                          
294200                                                                          
294300     IF SW-SATS-TRAFF = NEJ                                               
294400       MOVE JA TO SW-RASA-SPARR                                           
294500     END-IF                                                               
294600     .                                                                    
294700     EJECT                                                                
294800****************************************************************          
294900* S55-MATCHA-T-MOT-SATS                                        *          
295000* MATCHA RASA-ARB-T:S INGÅENDE ARTIKEL (TILLKOMMANDE) MED      *          
295100* SATS INGÅENDE ARTIKEL, DVS ARTIKELN KAN FINNAS TIDIGARE.     *          
295200****************************************************************          
295300 S55-MATCHA-T-MOT-SATS SECTION.                                           
295400                                                                          
295500     MOVE NEJ               TO SW-SATS-TRAFF                              
295600     SET  SATS-IDX          TO +1                                         
295700                                                                          
295800     PERFORM UNTIL                                                        
295900               SATS-IDX               > 250                   OR          
296000               SW-SATS-TRAFF          = JA                    OR          
296100               SW-RASA-SPARR          = JA                    OR          
296200               SATS-IDARTNR(SATS-IDX) > ARB-T-IDARTNR(ARB-T-IDX)          
296300                                                                          
296400        IF ARB-T-IDARTNR(ARB-T-IDX) =                                     
296500          SATS-IDARTNR(SATS-IDX)                                          
296600          IF SATS-NYKDSATAND(SATS-IDX)     = 'E' OR 'U' OR 'R' OR         
296700             SATS-KDSATAND  (SATS-IDX) NOT = SPACE                        
296800            MOVE JA TO SW-RASA-SPARR                                      
296900          ELSE                                                            
297000            MOVE JA TO SW-SATS-TRAFF                                      
297100          END-IF                                                          
297200        ELSE                                                              
297300          SET SATS-IDX UP  BY +1                                          
297400        END-IF                                                            
297500                                                                          
297600     END-PERFORM                                                          
297700     .                                                                    
297800     EJECT                                                                
297900****************************************************************          
298000* S56-MATCHA-RASA-T-MOT-SATS                                              
298100* MATCHA RASA:S INGÅENDE ARTIKEL (NY) MED                      *          
298200* SATS INGÅENDE ARTIKEL, DVS ARTIKELN KAN FINNAS TIDIGARE.     *          
298300****************************************************************          
298400 S56-MATCHA-RASA-T-MOT-SATS SECTION.                                      
298500                                                                          
298600     MOVE NEJ               TO SW-SATS-TRAFF                              
298700     SET SATS-IDX           TO +1                                         
298800                                                                          
298900     PERFORM UNTIL                                                        
299000               SATS-IDX               > 250                OR             
299100               SW-SATS-TRAFF          = JA                 OR             
299200               SW-RASA-SPARR          = JA                 OR             
299300               SATS-IDARTNR(SATS-IDX) > RASA-T-IDARTNR(T-INDX)            
299400                                                                          
299500        IF RASA-T-IDARTNR(T-INDX) = SATS-IDARTNR(SATS-IDX)                
299600          IF SATS-NYKDSATAND(SATS-IDX)     = 'E' OR 'U' OR 'T' OR         
299700             SATS-KDSATAND  (SATS-IDX) NOT = SPACE                        
299800            MOVE JA TO SW-RASA-SPARR                                      
299900                       SATS-SPARR(SATS-IDX)                               
300000          ELSE                                                            
300100            MOVE JA TO SW-SATS-TRAFF                                      
300200          END-IF                                                          
300300        ELSE                                                              
300400          SET SATS-IDX UP  BY +1                                          
300500        END-IF                                                            
300600                                                                          
300700     END-PERFORM                                                          
300800     .                                                                    
300900     EJECT                                                                
301000****************************************************************          
301100* S60-HITTA-LEDIG-SATS                                         *          
301200* LETA FRAM FÖRSTA LEDIGA PLATS I SATS, FÖR ATT LÄGGA UPP      *          
301300* EN TILLKOMMANDE RAD SOM INTE FUNNITS FÖRUT                   *          
301400****************************************************************          
301500 S60-HITTA-LEDIG-SATS SECTION.                                            
301600                                                                          
301700     MOVE NEJ               TO SW-SATS-TRAFF                              
301800     SET SATS-IDX           TO +1                                         
301900                                                                          
302000     PERFORM UNTIL SATS-IDX      > 250 OR                                 
302100                   SW-SATS-TRAFF = JA                                     
302200                                                                          
302300        IF SATS-IDARTNR(SATS-IDX) = ZERO                                  
302400          MOVE JA          TO SW-SATS-TRAFF                               
302500        ELSE                                                              
302600          SET SATS-IDX UP  BY +1                                          
302700        END-IF                                                            
302800                                                                          
302900     END-PERFORM                                                          
303000     .                                                                    
303100     EJECT                                                                
303200****************************************************************          
303300* S77-BERÄKNA-SATS-ANTAL                                       *          
303400* MED UTGÅNGSPUNKT FRÅN ANTAL/ST BERÄKNAS VILKET TOTALANTAL AV *          
303500* DEN TILLKOMMANDE ARTIKELN SOM BÖR FINNAS I SATS-TABELLEN.    *          
303600****************************************************************          
303700 S77-BERAKNA-SATS-ANTAL SECTION.                                          
303800                                                                          
303900     MOVE ZERO TO WS-REBEART-KOLL                                         
304000                  WS-NYREBEART-KOLL                                       
304100                                                                          
304200     IF SATS-HUV-KVBEART         > 0 AND                                  
304300        SATS-REANTPSA (SATS-IDX) > 0                                      
304400        COMPUTE WS-REBEART-KOLL = SATS-HUV-KVBEART *                      
304500                SATS-REANTPSA (SATS-IDX)                                  
304600     END-IF                                                               
304700                                                                          
304800     IF SATS-HUV-KVBEART           > 0 AND                                
304900        SATS-NYREANTPSA (SATS-IDX) > 0                                    
305000        COMPUTE WS-NYREBEART-KOLL = SATS-HUV-KVBEART *                    
305100                SATS-NYREANTPSA (SATS-IDX)                                
305200     END-IF                                                               
305300     .                                                                    
305400     EJECT                                                                
305500****************************************************************          
305600* S80-UTG-MARKERING        ORDERN VAR BYGGBAR                  *          
305700* MARKERA 'INGÅENDE ARTIKEL' SOM UTGÅENDE                      *          
305800* HELA ANTALET AV DEN ING ARTIKELN ÄR UTGÅNGET                 *          
305900****************************************************************          
306000 S80-UTG-MARKERING SECTION.                                               
306100                                                                          
306200     IF SATS-NYKDSATAND(SATS-IDX) = SPACE                                 
306300        MOVE 'U'                 TO SATS-NYKDSATAND(SATS-IDX)             
306400        MOVE ARB-E-REANTPSA-TOT  TO SATS-NYREANTPSA(SATS-IDX)             
306500        MOVE NEJ                 TO SATS-SPARR     (SATS-IDX)             
306600     ELSE                                                                 
306700        MOVE JA                  TO SW-RASA-SPARR                         
306800                                    SATS-SPARR     (SATS-IDX)             
306900     END-IF                                                               
307000     .                                                                    
307100     EJECT                                                                
307200****************************************************************          
307300* S90-ERS-MARKERING  (BYGGBAR)                                 *          
307400* MARKERA 'INGÅENDE ARTIKEL' SOM ERSATT                        *          
307500* HELA ANTALET AV DEN ING ARTIKELN ÄR ERSATT                   *          
307600****************************************************************          
307700 S90-ERS-MARKERING SECTION.                                               
307800                                                                          
307900     IF SATS-NYKDSATAND(SATS-IDX) = SPACE                                 
308000        MOVE 'E'                 TO SATS-NYKDSATAND(SATS-IDX)             
308100        MOVE ARB-E-REANTPSA-TOT  TO SATS-NYREANTPSA(SATS-IDX)             
308200        MOVE NEJ                 TO SATS-SPARR     (SATS-IDX)             
308300     ELSE                                                                 
308400        MOVE JA TO SW-RASA-SPARR                                          
308500     END-IF                                                               
308600     .                                                                    
308700     EJECT                                                                
308800****************************************************************          
308900* S95-ERS-MARKERING-BER  (EJ BYGGBAR, BEROR PÅ TILLGÅNG)       *          
309000* MARKERA 'INGÅENDE ARTIKEL' SOM ERSATT                        *          
309100* EN ERSATT ARTIKEL KAN HA ETT FLERTAL TILLKOMMANDE INGÅENDE   *          
309200* ARTIKLAR.                                                    *          
309300****************************************************************          
309400 S95-ERS-MARKERING-BER SECTION.                                           
309500                                                                          
309600     IF SATS-NYKDSATAND(SATS-IDX) = SPACE AND                             
309700        SATS-KDSATKMB(SATS-IDX)   = SPACE                                 
309800        MOVE 'E'                 TO SATS-NYKDSATAND(SATS-IDX)             
309900        MOVE NEJ                 TO SATS-SPARR     (SATS-IDX)             
310000        MOVE ARB-E-REANTPSA-TOT  TO SATS-NYREANTPSA(SATS-IDX)             
310100        MOVE SATS-HUV-KDSATKMB   TO SATS-NYKDSATKMB(SATS-IDX)             
310200     ELSE                                                                 
310300        MOVE JA TO SW-RASA-SPARR                                          
310400     END-IF                                                               
310500                                                                          
310600     IF SW-RASA-SPARR = NEJ                                               
310700        MOVE ZERO                TO WS-TACKT                              
310800        MOVE ZERO                TO WS-BEHOV                              
310900                                                                          
311000* --- RÄKNA UT HUR MÅNGA SATSER SOM KAN BYGGAS MED ERSATT ARTIKEL         
311100* --- MARKERA RESERVERAT ANTAL I NYREBEART (AVRUNDNINGSAVVIKELSER         
311200* --- KAN FÖREKOMMA JÄMFÖRT MED KVSATRES)                                 
311300        COMPUTE WS-TACKT = (SATS-KVSATRES(SATS-IDX) /                     
311400                               SATS-REANTPSA(SATS-IDX))                   
311500        COMPUTE SATS-NYREBEART(SATS-IDX) =                                
311600                WS-TACKT *  SATS-REANTPSA(SATS-IDX) + 0.99                
311700        IF SATS-NYREBEART(SATS-IDX) > SATS-KVSATRES(SATS-IDX)             
311800           MOVE JA TO SW-RASA-SPARR                                       
311900        ELSE                                                              
312000           COMPUTE WS-BEHOV = SATS-HUV-KVBEART - WS-TACKT                 
312100        END-IF                                                            
312200     END-IF                                                               
312300     .                                                                    
312400     EJECT                                                                
312500****************************************************************          
312600* S100-TILLK-GAMMAL-ART    (BYGGBAR)                           *          
312700* MARKERA 'INGÅENDE ARTIKEL' SOM TILLKOMMANDE                  *          
312800* OMRÄKNING AV ANTAL                                           *          
312900****************************************************************          
313000 S100-TILLK-GAMMAL-ART SECTION.                                           
313100                                                                          
313200     ADD ARB-T-REANTPSA(ARB-T-IDX) TO                                     
313300                           SATS-NYREANTPSA(SATS-IDX)                      
313400     COMPUTE SATS-NYREBEART (SATS-IDX) =                                  
313500             SATS-NYREANTPSA(SATS-IDX) * SATS-HUV-KVBEART                 
313600     MOVE 'T' TO SATS-NYKDSATAND(SATS-IDX)                                
313700     MOVE NEJ              TO SATS-SPARR(SATS-IDX)                        
313800     .                                                                    
313900     EJECT                                                                
314000****************************************************************          
314100* S101-TILLK-NYA-ART                                           *          
314200* SKAPA NY TILLKOMMANDE RAD FRÅN ARB-T-TABELLEN                *          
314300****************************************************************          
314400 S101-TILLK-NY-ART SECTION.                                               
314500                                                                          
314600     MOVE ARB-T-IDARTNR (ARB-T-IDX)                                       
314700                           TO SATS-IDARTNR     (SATS-IDX)                 
314800     MOVE W-KDCLAGER       TO SATS-KDCLAGER    (SATS-IDX)                 
314900     MOVE SPACE            TO SATS-KDSATAND    (SATS-IDX)                 
315000     MOVE SPACE            TO SATS-KDSATKMB    (SATS-IDX)                 
315100     MOVE +0               TO SATS-KDERS       (SATS-IDX)                 
315200     MOVE ARB-T-KDSTRRAD (ARB-T-IDX)                                      
315300                           TO SATS-KDSTRRAD    (SATS-IDX)                 
315400     MOVE +0               TO SATS-KVSATRES    (SATS-IDX)                 
315500     MOVE +0               TO SATS-REANTPSA    (SATS-IDX)                 
315600     MOVE +0               TO SATS-REBEART     (SATS-IDX)                 
315700     MOVE SPACE            TO SATS-NYKDSATKMB  (SATS-IDX)                 
315800     MOVE 'T'              TO SATS-NYKDSATAND  (SATS-IDX)                 
315900     MOVE ZERO             TO SATS-NYKVSATROS  (SATS-IDX)                 
316000     MOVE NEJ              TO SATS-SPARR       (SATS-IDX)                 
316100     MOVE ARB-T-REANTPSA(ARB-T-IDX)                                       
316200                           TO SATS-NYREANTPSA  (SATS-IDX)                 
316300     COMPUTE SATS-NYREBEART (SATS-IDX) =                                  
316400             SATS-NYREANTPSA (SATS-IDX) * SATS-HUV-KVBEART                
316500     .                                                                    
316600     EJECT                                                                
316700****************************************************************          
316800* S102-TILLK-NYA-ART    (EJ BYGGBAR)                           *          
316900* SKAPA NY TILLKOMMANDE RAD - KOMBINERAD MED ERSATT RAD        *          
317000****************************************************************          
317100 S102-TILLK-NY-ART-EJBYGGB SECTION.                                       
317200                                                                          
317300     MOVE ARB-T-IDARTNR (ARB-T-IDX)                                       
317400                           TO SATS-IDARTNR     (SATS-IDX)                 
317500     MOVE W-KDCLAGER       TO SATS-KDCLAGER    (SATS-IDX)                 
317600     MOVE SPACE            TO SATS-KDSATAND    (SATS-IDX)                 
317700     MOVE SPACE            TO SATS-KDSATKMB    (SATS-IDX)                 
317800     MOVE +0               TO SATS-KDERS       (SATS-IDX)                 
317900     MOVE ARB-T-KDSTRRAD(ARB-T-IDX)                                       
318000                           TO SATS-KDSTRRAD    (SATS-IDX)                 
318100     MOVE +0               TO SATS-KVSATRES    (SATS-IDX)                 
318200     MOVE +0               TO SATS-REANTPSA    (SATS-IDX)                 
318300     MOVE +0               TO SATS-REBEART     (SATS-IDX)                 
318400     MOVE 'T'              TO SATS-NYKDSATAND  (SATS-IDX)                 
318500     MOVE NEJ              TO SATS-SPARR       (SATS-IDX)                 
318600     MOVE ZERO             TO SATS-NYKVSATROS  (SATS-IDX)                 
318700     MOVE SATS-HUV-KDSATKMB TO SATS-NYKDSATKMB (SATS-IDX)                 
318800     MOVE ARB-T-REANTPSA(ARB-T-IDX)                                       
318900                           TO SATS-NYREANTPSA  (SATS-IDX)                 
319000     COMPUTE SATS-NYREBEART (SATS-IDX) =                                  
319100             SATS-NYREANTPSA (SATS-IDX) * WS-BEHOV + 0.99                 
319200     PERFORM S104-ORDNA-NY-KDSATKMB                                       
319300     .                                                                    
319400     EJECT                                                                
319500****************************************************************          
319600* S104-ORDNA-NY-KDSATKMB                                       *          
319700* ORDNA SÅ ATT NÄSTA SATS-KOMBINATIONSKOD FINNS TILLGÄNGLIG    *          
319800* KDSATKMB = BOKSTÄVER I ALFABETS-ORDNING                      *          
319900****************************************************************          
320000 S104-ORDNA-NY-KDSATKMB SECTION.                                          
320100                                                                          
320200     SET KOMB-IDX            TO +1                                        
320300                                                                          
320400     PERFORM UNTIL KOMB-IDX               > 26             OR             
320500                   KOMB-BOKSTAV(KOMB-IDX) = SATS-HUV-KDSATKMB             
320600                                                                          
320700        IF KOMB-BOKSTAV(KOMB-IDX) = SATS-HUV-KDSATKMB                     
320800          SET  KOMB-IDX UP BY +1                                          
320900          MOVE KOMB-BOKSTAV(KOMB-IDX) TO SATS-HUV-KDSATKMB                
321000        ELSE                                                              
321100          SET  KOMB-IDX UP   BY +1                                        
321200        END-IF                                                            
321300                                                                          
321400     END-PERFORM                                                          
321500        .                                                                 
321600        EJECT                                                             
321700****************************************************************          
321800* S110-NY-MARKERING                                            *          
321900* MARKERA 'INGÅENDE ARTIKEL' SOM NY                            *          
322000****************************************************************          
322100 S110-NY-MARKERING SECTION.                                               
322200                                                                          
322300     MOVE 'R'                       TO SATS-NYKDSATAND (SATS-IDX)         
322400     MOVE RASA-T-IDARTNR  (T-INDX)  TO SATS-IDARTNR    (SATS-IDX)         
322500     ADD  RASA-T-REANTPSA (T-INDX)  TO SATS-NYREANTPSA (SATS-IDX)         
322600* --- RÄKNA FRAM ANTAL NYA SOM SKALL BESTÄLLAS                            
322700     COMPUTE WS-NYREBEART =                                               
322800        (RASA-T-REANTPSA  (T-INDX) * SATS-HUV-KVBEART) + 0.999            
322900     ADD     WS-NYREBEART           TO SATS-NYREBEART  (SATS-IDX)         
323000                                                                          
323100     MOVE RASA-T-KDSTRRAD (T-INDX)  TO SATS-KDSTRRAD   (SATS-IDX)         
323200     .                                                                    
323300     EJECT                                                                
323400****************************************************************          
323500* S125-KOLLA-TACKNING-RO                                       *          
323600* SE EFTER I RESTORDERSYSTEMET OM ING.ARTIKLAR HAR BLIVIT      *          
323700* TÄCKTA. ENDAST TÄCKTA RESTORDERRADER LÄSES (B-NYCKEL).       *          
323800* OM EN RESTORDERRAD PÅ WDA5 BLIR ANVÄND, SKALL DEN DELETAS    *          
323900* FRÅN RESTORDERREGISTRET(WDA501).                             *          
324000****************************************************************          
324100 S125-KOLLA-TACKNING-RO SECTION.                                          
324200                                                                          
324300     MOVE SHUV-IDORDNSB      TO W-WDJ201-IDORDNSB                         
324400                                      WS-IDORDNSB                         
324500                                                                          
324600     MOVE SHUV-IDORDNSS      TO W-WDJ201-IDORDNSS                         
324700                                      WS-IDORDNSS                         
324800                                                                          
324900     MOVE SRAD-IDARTNR       TO W-WDJ211-IDARTNR                          
325000                                                                          
325100     PERFORM IMS-GHU-WDJ211-SATG                                          
325200                                                                          
325300     MOVE LOW-VALUE          TO W-WDA5B1KY-MIN-X                          
325400     MOVE HIGH-VALUE         TO W-WDA5B1KY-MAX-X                          
325500                                                                          
325600     MOVE SHUV-IDDISTR       TO W-WDA5B1-IDDISTR-MIN                      
325700                                W-WDA5B1-IDDISTR-MAX                      
325800                                                                          
325900     MOVE SHUV-IDKUNDNR      TO W-WDA5B1-IDKUNDNR-MIN                     
326000                                W-WDA5B1-IDKUNDNR-MAX                     
326100                                                                          
326200     MOVE SPACE              TO W-WDA5B1-IDKUNDRF-MIN                     
326300                                W-WDA5B1-IDKUNDRF-MAX                     
326400                                                                          
326500     MOVE WS-IDORDNST-NUM    TO W-WDA5B1-IDORDNR5-MIN                     
326600                                W-WDA5B1-IDORDNR5-MAX                     
326700                                                                          
326800     MOVE SRAD-IDARTNR       TO W-WDA5B1-IDARTNR-MIN                      
326900                                W-WDA5B1-IDARTNR-MAX                      
327000                                                                          
327100     MOVE WC-CDC-SE          TO W-WDA5B1-IDDC-MIN                         
327200                                W-WDA5B1-IDDC-MAX                         
327300                                                                          
327400     PERFORM IMS-GU-WDA5-ORDR                                             
327500                                                                          
327600     IF SEGMENT-FINNS                                                     
327700                                                                          
327800        PERFORM UNTIL SEGMENT-SLUT              OR                        
327900                      SEGMENT-SAKNAS            OR                        
328000                      SRAD-REBEART = SRAD-KVSATRES                        
328100                                                                          
328200          ADD     ORDR-SEQB-KVART    TO SRAD-KVSATRES                     
328300          COMPUTE SRAD-KVSATROS =   SRAD-KVSATROS                         
328400                                  - ORDR-SEQB-KVART                       
328500          MOVE    ORDR-SEQB-IDDISTR  TO W-WDA501-IDDISTR                  
328600          MOVE    ORDR-SEQB-IDKUNDNR TO W-WDA501-IDKUNDNR                 
328700          MOVE    ORDR-SEQB-IDKUNDRF TO W-WDA501-IDKUNDRF                 
328800          MOVE    ORDR-SEQB-IDARTNR  TO W-WDA501-IDARTNR                  
328900          MOVE    ORDR-SEQB-IDLOPNR  TO W-WDA501-IDLOPNR                  
329000          PERFORM IMS-GHU-WDA5-ORDP                                       
329100          PERFORM IMS-DLET-WDA5-ORDP                                      
329200                                                                          
329300          PERFORM IMS-GN-WDA5-ORDR                                        
329400                                                                          
329500        END-PERFORM                                                       
329600        PERFORM IMS-REPL-WDJ211-SATG                                      
329700        MOVE JA TO SW-RORAD-TACKT                                         
329800     END-IF                                                               
329900     .                                                                    
330000     EJECT                                                                
330100****************************************************************          
330200* S135-SKRIV-FELLISTA                                          *          
330300* SKRIVER UT EN FELRAD FÖR SATSORDERN                          *          
330400****************************************************************          
330500 S135-SKRIV-FELLISTA SECTION.                                             
330600                                                                          
330700     IF WS-RADANT > 40                                                    
330800        ADD +1                   TO FEL-SIDA                              
330900        MOVE DAGENS-DATUM        TO FEL-DATUM                             
331000        MOVE RUB1                TO FELPOST                               
331100        WRITE FELPOST AFTER ADVANCING PAGE                                
331200        MOVE RUB2                TO FELPOST                               
331300        WRITE FELPOST AFTER ADVANCING 2 LINES                             
331400        MOVE +4                  TO WS-RADANT                             
331500                                                                          
331600        MOVE SHUV-IDORDNSB       TO FEL-IDORDNSB                          
331700        MOVE SHUV-IDORDNSS       TO FEL-IDORDNSS                          
331800        MOVE SHUV-IDARTNR        TO FEL-SATS-IDARTNR                      
331900     ELSE                                                                 
332000        MOVE SHUV-IDORDNSB       TO FEL-IDORDNSB                          
332100        MOVE SHUV-IDORDNSS       TO FEL-IDORDNSS                          
332200        MOVE SHUV-IDARTNR        TO FEL-SATS-IDARTNR                      
332300     END-IF                                                               
332400     MOVE FELRAD              TO FELPOST                                  
332500     WRITE FELPOST AFTER ADVANCING 2 LINES                                
332600     ADD +2                      TO WS-RADANT                             
332700     MOVE JA                     TO SW-SATS-FEL                           
332800     .                                                                    
332900     EJECT                                                                
333000****************************************************************          
333100* S160-RAKNA-ANT-BYGGB                                         *          
333200* RÄKNAR UT HUR MÅNGA SATSORDER MAN KAN BYGGA.                 *          
333300* OM DET NYA ANTALET BYGGBARA ÄR MINDRE ÄN VAD SOM FINNS       *          
333400* SEDAN FÖRUT, SPARA UNDAN DET LÄGSTA.                         *          
333500****************************************************************          
333600 S160-RAKNA-ANT-BYGGB SECTION.                                            
333700                                                                          
333800* --- KOLLA OM KOMBINATIONSKOD FINNS PÅ DEN INGÅENDE ARTIKELN             
333900     IF SRAD-KDSATKMB = SPACE                                             
334000        IF SRAD-REANTPSA > 0                                              
334100           COMPUTE WS-KVBYGGB = SRAD-KVSATRES /                           
334200                                SRAD-REANTPSA                             
334300        END-IF                                                            
334400     ELSE                                                                 
334500        PERFORM S165-KOLLA-OM-ANVAND-KDSATKMB                             
334600        IF SW-ANV-KOMBKOD-TRAFF = NEJ                                     
334700* --- DENNA KOMBINATIONSKODEN ÄR EJ BEHANDLAD                             
334800           PERFORM S170-LETA-KDSATKMB-MATCH                               
334900        END-IF                                                            
335000     END-IF                                                               
335100                                                                          
335200* --- BERÄKNA ANTAL BYGGBARA SATSARTIKLAR                                 
335300     IF SW-ANV-KOMBKOD-TRAFF = NEJ                                        
335400        IF SW-ANT-BYGGB-FORSTA = JA                                       
335500           MOVE WS-KVBYGGB        TO WS-KVBYGGBAR                         
335600           MOVE NEJ               TO SW-ANT-BYGGB-FORSTA                  
335700        ELSE                                                              
335800           IF WS-KVBYGGBAR NOT = 9999999                                  
335900              IF WS-KVBYGGB    < WS-KVBYGGBAR                             
336000                 MOVE WS-KVBYGGB    TO WS-KVBYGGBAR                       
336100              END-IF                                                      
336200           END-IF                                                         
336300        END-IF                                                            
336400     END-IF                                                               
336500                                                                          
336600     IF WS-KVBYGGB < SHUV-KVBEART                                         
336700        MOVE NEJ TO WS-FLBYGGB                                            
336800     END-IF                                                               
336900     .                                                                    
337000     EJECT                                                                
337100****************************************************************          
337200* S165-KOLLA-OM-ANVAND-KDSATKMB                                *          
337300* HÄR LÄSER MAN IGENOM SPAR-KDSATKMB-TABELLEN FÖR ATT SE       *          
337400* OM DEN ANGIVNA KOMBINATIONSKODEN REDAN HAR BLIVIT BE-        *          
337500* HANDLAD. OM DEN VARIT BEARBETAD BETYDER DET ATT KVBYGGB      *          
337600* REDAN HAR BLIVIT BERÄKNAD FÖR DENNA INGÅENDE ARTIKELN .      *          
337700****************************************************************          
337800 S165-KOLLA-OM-ANVAND-KDSATKMB SECTION.                                   
337900                                                                          
338000     SET SPAR-KOMB-IDX        TO +1                                       
338100                                                                          
338200     PERFORM UNTIL SPAR-KOMB-IDX        > 26 OR                           
338300                   SW-ANV-KOMBKOD-TRAFF = JA                              
338400                                                                          
338500        IF SRAD-KDSATKMB = SPAR-KDSATKMB-KOD(SPAR-KOMB-IDX)               
338600          MOVE JA            TO SW-ANV-KOMBKOD-TRAFF                      
338700        ELSE                                                              
338800          SET SPAR-KOMB-IDX  UP BY +1                                     
338900        END-IF                                                            
339000                                                                          
339100     END-PERFORM                                                          
339200                                                                          
339300     IF SW-ANV-KOMBKOD-TRAFF = NEJ                                        
339400* --- DENNA KOMBKOD:EN HAR INTE BLIVIT BEARBETAD, DÄRFÖR LÄGGER           
339500* --- JAG IN DEN PÅ LEDIG PLATS I TABELLEN NU, FÖR ATT MARKERA ATT        
339600* --- DEN ÄR UNDER BEARBETNING.                                           
339700                                                                          
339800        SET SPAR-KOMB-IDX     TO +1                                       
339900        PERFORM UNTIL SPAR-KOMB-IDX                    > 26 OR            
340000                      SPAR-KDSATKMB-KOD(SPAR-KOMB-IDX) = SPACE            
340100          SET SPAR-KOMB-IDX  UP BY +1                                     
340200        END-PERFORM                                                       
340300        MOVE SRAD-KDSATKMB    TO SPAR-KDSATKMB-KOD(SPAR-KOMB-IDX)         
340400                                                                          
340500     END-IF                                                               
340600     .                                                                    
340700     EJECT                                                                
340800****************************************************************          
340900* S170-LETA-KDSATKMB-MATCH                                     *          
341000* HÄR LÄSER MAN IGENOM WDJ2 FÖR ATT HITTA EN ELLER FLER        *          
341100* INGÅENDE ARTIKLAR SOM HAR SAMMA KDSATKOMB.                   *          
341200* MÖJLIGT ANTAL ATT BYGGA AV RESP ARTIKEL BERÄKNAS OCH         *          
341300* SUMMERAS FÖR KOMBINATIONSKODEN.                              *          
341400****************************************************************          
341500 S170-LETA-KDSATKMB-MATCH SECTION.                                        
341600                                                                          
341700     MOVE SRAD-IDARTNR           TO W-WDJ211-IDARTNR                      
341800     MOVE SRAD-KDSATKMB          TO W-WDJ211-KDSATKMB-MIN                 
341900                                    W-WDJ211-KDSATKMB-MAX                 
342000     MOVE ZERO                   TO WS-KVBYGGB                            
342100                                    WS-KVBYGGB-KMB                        
342200                                                                          
342300     IF SRAD-KVSATRES             >  0                                    
342400     AND SRAD-REANTPSA            >  0                                    
342500       COMPUTE WS-KVBYGGB-KMB = SRAD-KVSATRES / SRAD-REANTPSA             
342600       ADD         WS-KVBYGGB-KMB TO WS-KVBYGGB                           
342700       IF WS-FLBYGGB = JA                                                 
342800          PERFORM S200-RAKNA-VIKT-VOLYM-RAD                               
342900       END-IF                                                             
343000     END-IF                                                               
343100                                                                          
343200* ---  HÄMTA NÄSTA MED SAMMA KOMBINATIONSKOD                              
343300     PERFORM IMS-GU-WDJ211-KDSATKMB                                       
343400                                                                          
343500     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
343600                   SEGMENT-SLUT                                           
343700                                                                          
343800        MOVE ZERO                TO WS-KVBYGGB-KMB                        
343900        IF SATG1-SRAD-KVSATRES > 0 AND                                    
344000           SATG1-SRAD-REANTPSA > 0                                        
344100           COMPUTE WS-KVBYGGB-KMB =                                       
344200               SATG1-SRAD-KVSATRES /  SATG1-SRAD-REANTPSA                 
344300           ADD     WS-KVBYGGB-KMB TO WS-KVBYGGB                           
344400           IF WS-FLBYGGB = JA                                             
344500             PERFORM S201-RAKNA-VIKT-VOLYM-RAD                            
344600           END-IF                                                         
344700         END-IF                                                           
344800        PERFORM IMS-GN-WDJ211-KDSATKMB                                    
344900                                                                          
345000     END-PERFORM                                                          
345100     .                                                                    
345200     EJECT                                                                
345300****************************************************************          
345400* S200-RAKNA-VIKT-VOLYM-RAD                                    *          
345500* FÖR VARJE ANVÄND ARTIKELRAD BERÄKNAS VIKT OCH VOLYM          *          
345600* ORDERNS TOTALVIKT OCH VOLYM BERÄKNAS LÖPANDE.                *          
345700* ANTAL ARTIKELRADER BERÄKNAS.                                 *          
345800****************************************************************          
345900 S200-RAKNA-VIKT-VOLYM-RAD SECTION.                                       
346000                                                                          
346100     MOVE +0                    TO WS-VKARTNTO-TOT                        
346200                                   WS-VLARTNTO-TOT                        
346300                                                                          
346400     COMPUTE WS-VKARTNTO-TOT    =                                         
346500                  SRAD-VKARTNTO * SRAD-REBEART                            
346600     ADD     WS-VKARTNTO-TOT    TO WS-VKORDNTO-3DEC                       
346700                                                                          
346800     COMPUTE WS-VLARTNTO-TOT    =                                         
346900                  SRAD-VLARTNTO * SRAD-REBEART                            
347000     ADD     WS-VLARTNTO-TOT    TO WS-VLORDNTO-CM                         
347100                                                                          
347200     ADD +1                     TO WS-KVRADER                             
347300     .                                                                    
347400     EJECT                                                                
347500****************************************************************          
347600* S201-RAKNA-VIKT-VOLYM-RAD                                    *          
347700* SPECIALFALL VID KOMBINATIONSKOD                              *          
347800****************************************************************          
347900 S201-RAKNA-VIKT-VOLYM-RAD SECTION.                                       
348000                                                                          
348100     MOVE +0                    TO WS-VKARTNTO-TOT                        
348200                                   WS-VLARTNTO-TOT                        
348300                                                                          
348400     COMPUTE WS-VKARTNTO-TOT    =                                         
348500             SATG1-SRAD-VKARTNTO * SATG1-SRAD-REBEART                     
348600     ADD     WS-VKARTNTO-TOT    TO WS-VKORDNTO-3DEC                       
348700                                                                          
348800     COMPUTE WS-VLARTNTO-TOT    =                                         
348900             SATG1-SRAD-VLARTNTO * SATG1-SRAD-REBEART                     
349000     ADD     WS-VLARTNTO-TOT    TO WS-VLORDNTO-CM                         
349100                                                                          
349200     ADD +1                     TO WS-KVRADER                             
349300     .                                                                    
349400     EJECT                                                                
349500****************************************************************          
349600* S230-SPÄRRA-ORDERRADER                                       *          
349700* HÄR MATCHAS ALLA ERSATTA/UTGÅNGNA ARTIKLAR MOT RESP RAD I    *          
349800* SATSORDERTABELLEN. SAMTLIGA ERSATTA/UTGÅNGNA SPÄRRAS.        *          
349900* BÅDA TABELLERNA SORTERADE I ARTIKELNUMMERFÖLJD.              *          
350000****************************************************************          
350100 S230-SPARRA-ORDERRADER SECTION.                                          
350200                                                                          
350300     MOVE NEJ TO SW-SATS-TRAFF                                            
350400     SET E-INDX TO +1                                                     
350500     SET SATS-IDX TO +1                                                   
350600     PERFORM UNTIL E-INDX                  > 175 OR                       
350700                   RASA-E-IDARTNR (E-INDX) = ZERO                         
350800                                                                          
350900       PERFORM UNTIL SATS-IDX         > 250   OR                          
351000               SATS-IDARTNR(SATS-IDX) = ZERO  OR                          
351100               SW-SATS-TRAFF          = JA                                
351200                                                                          
351300         IF RASA-E-IDARTNR(E-INDX) = SATS-IDARTNR(SATS-IDX)               
351400           MOVE JA TO SATS-SPARR(SATS-IDX)                                
351500           MOVE JA TO SW-SATS-TRAFF                                       
351600         ELSE                                                             
351700           SET SATS-IDX UP BY +1                                          
351800         END-IF                                                           
351900       END-PERFORM                                                        
352000                                                                          
352100       SET E-INDX UP BY +1                                                
352200       MOVE NEJ TO SW-SATS-TRAFF                                          
352300     END-PERFORM                                                          
352400     .                                                                    
352500     EJECT                                                                
352600****************************************************************          
352700* S300-MATCHA-ARB-T-TAB                                        *          
352800* HÄR BYGGS EN ARBETSTABELL UPP FÖR ATT MATCHA EN ERSATT       *          
352900* ARTIKEL. TEORETISKT ANTAL BERÄKNAS. I TN-TABELLEN BOKAS      *          
353000* ARTIKELN AV SOM ANVÄND.                                      *          
353100* TOLKNING AV MATCHNINGEN:                                     *          
353200* SW-ARB-E-TRAFF = N                 J            N            *          
353300* SW-WDD7-TRAFF  = N                 J            J            *          
353400* INGEN TRÄFF => UTGÅNGEN     OK =>TILLKOMMER  OTYDLIG => SPÄRR*          
353500****************************************************************          
353600 S300-MATCHA-ARB-T-TAB SECTION.                                           
353700                                                                          
353800     SET T-INDX                                                           
353900         WDD7-IDX                                                         
354000         ARB-E-IDX                                                        
354100         ARB-T-IDX  TO +1                                                 
354200                                                                          
354300     PERFORM UNTIL ARB-E-IDX                 >   15 OR                    
354400                   ARB-E-REANTPSA(ARB-E-IDX) = ZERO                       
354500                                                                          
354600       MOVE NEJ TO SW-ARB-E-TRAFF                                         
354700       PERFORM UNTIL WDD7-IDX               >   15 OR                     
354800                     WDD7-IDARTNR(WDD7-IDX) = ZERO                        
354900                                                                          
355000         MOVE NEJ TO SW-WDD7-TRAFF                                        
355100                     SW-ARB-E-TRAFF                                       
355200                                                                          
355300         PERFORM UNTIL T-INDX                 > 175       OR              
355400                       RASA-T-IDARTNR(T-INDX) = ZERO      OR              
355500                       RASA-T-IDARTNR(T-INDX) >                           
355600                                   WDD7-IDARTNR(WDD7-IDX) OR              
355700                       SW-WDD7-TRAFF          = JA                        
355800                                                                          
355900           IF RASA-T-IDARTNR(T-INDX) = WDD7-IDARTNR(WDD7-IDX)             
356000             IF RASA-T-ANV(T-INDX) = NEJ                                  
356100* --- MED UTGÅNGSPUNKT FRÅN WDD7 BERÄKNAS VILKET ANTAL ARTIKLAR           
356200* --- AV DEN TILLKOMMANDE SOM BÖR FINNAS I RASA-T-TABELLEN                
356300                                                                          
356400                MOVE ZERO TO WS-WDD7-REANTPSA                             
356500                COMPUTE WS-WDD7-REANTPSA =                                
356600                   ARB-E-REANTPSA (ARB-E-IDX) *                           
356700                   WDD7-DIERS-TILLK (WDD7-IDX) /                          
356800                   WDD7-DIERS-ERS                                         
356900                                                                          
357000                IF WS-WDD7-REANTPSA = RASA-T-REANTPSA(T-INDX) AND         
357100                   RASA-T-TISTADAT(T-INDX) =                              
357200                                    ARB-E-TISTODAT(ARB-E-IDX)             
357300                  MOVE JA TO RASA-T-ANV (T-INDX)   SW-WDD7-TRAFF          
357400                             WDD7-ANV   (WDD7-IDX) SW-ARB-E-TRAFF         
357500                  IF ARB-T-IDARTNR(ARB-T-IDX) = ZERO                      
357600                    PERFORM S301-SKAPA-ARB-T-TAB-FORSTA                   
357700                  ELSE                                                    
357800                    PERFORM S302-SKAPA-ARB-T-TAB                          
357900                  END-IF                                                  
358000                END-IF                                                    
358100             END-IF                                                       
358200           END-IF                                                         
358300           SET T-INDX UP BY +1                                            
358400         END-PERFORM                                                      
358500                                                                          
358600         SET WDD7-IDX UP BY +1                                            
358700                                                                          
358800       END-PERFORM                                                        
358900                                                                          
359000       SET ARB-E-IDX UP BY +1                                             
359100                                                                          
359200     END-PERFORM                                                          
359300                                                                          
359400     IF SW-ARB-E-TRAFF = NEJ AND                                          
359500        SW-WDD7-TRAFF  = JA                                               
359600        MOVE JA TO SW-RASA-SPARR                                          
359700     END-IF                                                               
359800                                                                          
359900     IF SW-ARB-E-TRAFF = NEJ AND                                          
360000        SW-WDD7-TRAFF  = NEJ                                              
360100        MOVE JA TO SW-UTG                                                 
360200     END-IF                                                               
360300     .                                                                    
360400     EJECT                                                                
360500****************************************************************          
360600* S301-SKAPA-ARB-T-TAB-FORSTA OCH S302-SKAPA-ARB-T-TAB OCH     *          
360700* S303-SORTERA-ARB-T-TAB                                       *          
360800* DESSA SEKTIONER ANVÄNDS FÖR ATT HANTERA ATT EN ERSATT ARTIKEL*          
360900* KAN HA HA FLERA RADER I RASA OCH EN ERSATT ARTIKEL KAN ERSÄT-*          
361000* TAS AV FLERA TILLKOMMANDE ARTIKLAR (ENTYDIGT)                *          
361100****************************************************************          
361200 S301-SKAPA-ARB-T-TAB-FORSTA SECTION.                                     
361300                                                                          
361400     SET  ARB-T-IDX                TO +1                                  
361500     MOVE RASA-T-IDARTNR  (T-INDX) TO ARB-T-IDARTNR  (ARB-T-IDX)          
361600     MOVE RASA-T-REANTPSA (T-INDX) TO ARB-T-REANTPSA (ARB-T-IDX)          
361700     MOVE RASA-T-TISTADAT (T-INDX) TO ARB-T-TISTADAT (ARB-T-IDX)          
361800     MOVE RASA-T-KDISATS  (T-INDX) TO ARB-T-KDISATS  (ARB-T-IDX)          
361900     MOVE RASA-T-KDERS    (T-INDX) TO ARB-T-KDERS    (ARB-T-IDX)          
362000     MOVE RASA-T-KDSTRRAD (T-INDX) TO ARB-T-KDSTRRAD (ARB-T-IDX)          
362100     .                                                                    
362200     EJECT                                                                
362300 S302-SKAPA-ARB-T-TAB SECTION.                                            
362400                                                                          
362500     SET ARB-T-IDX UP BY +1                                               
362600                                                                          
362700     IF ARB-T-IDX > 15                                                    
362800       DISPLAY '********************************************'             
362900       DISPLAY '**  ARBETSTAB FÖR LITEN VID MATCHNING AV  **'             
363000       DISPLAY '**  TILLK ARTIKLAR. ERSATT ING ARTIKEL:   **'             
363100       DISPLAY '**  SECT- S302-                           **'             
363200       DISPLAY '********************************************'             
363300       CALL ABEND USING ABEND-UTAN-DUMP                                   
363400     ELSE                                                                 
363500       MOVE RASA-T-IDARTNR  (T-INDX) TO ARB-T-IDARTNR  (ARB-T-IDX)        
363600       MOVE RASA-T-REANTPSA (T-INDX) TO ARB-T-REANTPSA (ARB-T-IDX)        
363700       MOVE RASA-T-TISTADAT (T-INDX) TO ARB-T-TISTADAT (ARB-T-IDX)        
363800       MOVE RASA-T-KDISATS  (T-INDX) TO ARB-T-KDISATS  (ARB-T-IDX)        
363900       MOVE RASA-T-KDERS    (T-INDX) TO ARB-T-KDERS    (ARB-T-IDX)        
364000       MOVE RASA-T-KDSTRRAD (T-INDX) TO ARB-T-KDSTRRAD (ARB-T-IDX)        
364100     END-IF                                                               
364200     .                                                                    
364300     EJECT                                                                
364400 S303-SORTERA-ARB-T-TAB SECTION.                                          
364500                                                                          
364600     SET  ARB-T-IDX   DOWN  BY +1                                         
364700                                                                          
364800*********** FIX FÖR ATT KLARA SEKELSKIFTET ******************             
364900*********** ÅR < 50 BLIR ÅR + 50           ******************             
365000*********** ÅR > 50 BLIR ÅR - 50           ******************             
365100     MOVE 1                  TO   Y2K-IX                                  
365200     PERFORM UNTIL Y2K-IX > ARB-T-IDX                                     
365300       MOVE ARB-T-TISTADAT (Y2K-IX) TO TMP1-YYMMDD                        
365400       PERFORM WY2000P1                                                   
365500       MOVE TMP1-YYMMDD      TO   ARB-T-TISTADAT (Y2K-IX)                 
365600       ADD 1                 TO   Y2K-IX                                  
365700     END-PERFORM                                                          
365800                                                                          
365900     MOVE +15         TO    RINT-STEGLNGD                                 
366000     SET  RINT-ANTAL  TO    ARB-T-IDX                                     
366100     MOVE +11         TO    RINT-NKLLNGD                                  
366200     CALL WINTSOR     USING ARB-T-TAB RINT-STEGLNGD RINT-ANTAL            
366300          ARB-T-NKL(1) RINT-NKLLNGD                                       
366400                                                                          
366500*********** FIX FÖR ATT KLARA SEKELSKIFTET ******************             
366600*********** ÅTERSTÄLLER DATUMEN            ******************             
366700     MOVE 1                  TO   Y2K-IX                                  
366800     PERFORM UNTIL Y2K-IX > ARB-T-IDX                                     
366900       MOVE ARB-T-TISTADAT (Y2K-IX) TO TMP1-YYMMDD                        
367000       PERFORM WY2000P1                                                   
367100       MOVE TMP1-YYMMDD      TO   ARB-T-TISTADAT (Y2K-IX)                 
367200       ADD 1                 TO   Y2K-IX                                  
367300     END-PERFORM                                                          
367400                                                                          
367500     .                                                                    
367600     EJECT                                                                
367700****************************************************************          
367800* S310-DISP-LAGERSALDO                                         *          
367900* DISPONIBELT LAGERSALDO FÖR RESERVATION BERÄKNAS.             *          
368000* WDK611-SEGMENTET ÄR INLÄST FÖRE DENNA SEKTION.               *          
368100* HÄR LÄSES WDK901-SEGMENTET.                                  *          
368200****************************************************************          
368300 S310-DISP-LAGERSALDO SECTION.                                            
368400                                                                          
368500     PERFORM IMS-GU-WDK9-ARTM                                             
368600     MOVE ZERO               TO WS-DISPLS                                 
368700     IF SEGMENT-FINNS                                                     
368800        COMPUTE WS-DISPLS =   CLAG-KVLS                                   
368900                            - CLAG-KVRESS                                 
369000                            - CLAG-KVSPANT                                
369100                            - CLAG-KVUTRS                                 
369200                            - ART-KVPREAVB-DAG                            
369300                            - ART-KVPREAVB-VOR                            
369400     ELSE                                                                 
369500        COMPUTE WS-DISPLS =   CLAG-KVLS                                   
369600                            - CLAG-KVRESS                                 
369700                            - CLAG-KVSPANT                                
369800                            - CLAG-KVUTRS                                 
369900     END-IF                                                               
370000     .                                                                    
370100     EJECT                                                                
370200****************************************************************          
370300* S320-KONTR-FLSATRAS                                          *          
370400* KONTROLLERA MOT RASA, FÖR ATT SE OM DET FORTFARANDE FINNS    *          
370500* FLER AV SAMMA ING.ARTIKEL, EFTER ATT DEN SOM JAG HÅLLER PÅ   *          
370600* MED ATT BEARBETA BLIVIT UTGÅNGEN.                            *          
370700****************************************************************          
370800 S320-KONTR-FLSATRAS SECTION.                                             
370900                                                                          
371000     MOVE ZERO                   TO WS-RASA-IDARTNR-RAKN                  
371100     MOVE SHUV-IDARTNR           TO W-WDJ1-IDARTNR                        
371200     MOVE SRAD-KDSTRRAD          TO W-WDJ111-KDSTRRAD                     
371300     MOVE ZERO                   TO W-WDJ111-IDRADNR                      
371400     PERFORM IMS-GU-WDJ1-SATB                                             
371500     PERFORM IMS-GNP-WDJ1-SATB                                            
371600                                                                          
371700     PERFORM UNTIL SEGMENT-SLUT OR                                        
371800                   SEGMENT-SAKNAS                                         
371900       IF SATB-RAD-IDARTNR = SRAD-IDARTNR                                 
372000         MOVE SATB-RAD-TISTADAT   TO TMP1-YYMMDD                          
372100         MOVE SATB-RAD-TISTODAT   TO TMP2-YYMMDD                          
372200         MOVE DAGENS-DATUM-NUM    TO TMP3-YYMMDD                          
372300         PERFORM WY2000P1                                                 
372400         IF TMP1-YYMMDD <= TMP3-YYMMDD AND                                
372500            TMP2-YYMMDD >= TMP3-YYMMDD                                    
372600           ADD +1             TO WS-RASA-IDARTNR-RAKN                     
372700         END-IF                                                           
372800       END-IF                                                             
372900       PERFORM IMS-GNP-WDJ1-SATB                                          
373000     END-PERFORM                                                          
373100     .                                                                    
373200     EJECT                                                                
373300****************************************************************          
373400* S330-REDIGERA-SRAD                                           *          
373500* REDIGERA DEN NYA INGÅENDE ARTIKELN TILL SATSORDERBASEN       *          
373600****************************************************************          
373700 S330-REDIGERA-SRAD SECTION.                                              
373800                                                                          
373900     MOVE    SPACE                     TO SRAD-WDJ211                     
374000     MOVE    SATS-IDARTNR(SATS-IDX)    TO SRAD-IDARTNR                    
374100     MOVE    SATS-KDSTRRAD(SATS-IDX)   TO SRAD-KDSTRRAD                   
374200     MOVE    SHUV-KDCLAGER             TO SRAD-KDCLAGER                   
374300     MOVE    SHUV-IDKONTO              TO SRAD-IDKONTO                    
374400     MOVE    SHUV-IDANALYS             TO SRAD-IDANALYS                   
374500     MOVE    SHUV-IDKST                TO SRAD-IDKST                      
374600     MOVE    SATS-NYKDSATAND(SATS-IDX) TO SRAD-KDSATAND                   
374700     MOVE    SATS-NYKDSATKMB(SATS-IDX) TO SRAD-KDSATKMB                   
374800     MOVE    ART-REKSIFFR              TO SRAD-REKSIFFR                   
374900     MOVE    ART-KDSORT                TO SRAD-KDSORT                     
375000     MOVE    ART-KDPRODSL              TO SRAD-KDPRODSL                   
375100     MOVE    CLAG-VLARTNTO             TO SRAD-VLARTNTO                   
375200     MOVE    CLAG-VKART                TO WS-VKART                        
375300     COMPUTE SRAD-VKARTNTO = WS-VKART / 1000                              
375400     MOVE    NEJ                       TO SRAD-FLSATUTS                   
375500     MOVE    NEJ                       TO SRAD-FLSATSPR                   
375600     MOVE    +0                        TO SRAD-KVSATRES                   
375700     MOVE    +0                        TO SRAD-KVSATROS                   
375800     MOVE    +0                        TO SRAD-PRARTSTD                   
375900     MOVE    +0                        TO SRAD-REANTPSA                   
376000     MOVE    +0                        TO SRAD-REBEART                    
376100     PERFORM S320-KONTR-FLSATRAS                                          
376200     IF WS-RASA-IDARTNR-RAKN > 1                                          
376300        MOVE JA                        TO SRAD-FLSATRAS                   
376400     ELSE                                                                 
376500        MOVE NEJ                       TO SRAD-FLSATRAS                   
376600     END-IF                                                               
376700     .                                                                    
376800     EJECT                                                                
376900****************************************************************          
377000* S340-JUSTERA-RESTORDER (EJ BYGGB)                            *          
377100* JUSTERAR RESTORDER I RESTORDERBASEN(WDA5) I OCH MED ATT      *          
377200* DENNA ING.ARTIKEL HAR BLIVIT DELVIS UTGÅNGS-MARKERAD         *          
377300****************************************************************          
377400 S340-JUSTERA-RESTORDER SECTION.                                          
377500                                                                          
377600* --- RÄKNA UT HUR MÅNGA RESTORDER SOM BEHÖVS                             
377700     COMPUTE WS-RO-ANTAL = WS-REBEART - SRAD-KVSATRES                     
377800                                                                          
377900     MOVE NEJ                TO SW-TACKT-RO                               
378000     MOVE NEJ                TO SW-SATS-FEL                               
378100                                                                          
378200     MOVE LOW-VALUE          TO W-WDA501KY-MIN-X                          
378300     MOVE HIGH-VALUE         TO W-WDA501KY-MAX-X                          
378400                                                                          
378500     MOVE SHUV-IDDISTR       TO W-WDA501-IDDISTR-MIN                      
378600                                W-WDA501-IDDISTR-MAX                      
378700                                                                          
378800     MOVE SHUV-IDKUNDNR      TO W-WDA501-IDKUNDNR-MIN                     
378900                                W-WDA501-IDKUNDNR-MAX                     
379000                                                                          
379100     MOVE SHUV-IDORDNSB      TO WS-IDORDNSB                               
379200     MOVE SHUV-IDORDNSS      TO WS-IDORDNSS                               
379300                                                                          
379400     MOVE SPACE              TO W-WDA501-IDKUNDRF-MIN                     
379500                                W-WDA501-IDKUNDRF-MAX                     
379600                                                                          
379700     MOVE WS-IDORDNST-NUM    TO W-WDA501-IDORDNR5-MIN                     
379800                                W-WDA501-IDORDNR5-MAX                     
379900                                                                          
380000     MOVE SRAD-IDARTNR       TO W-WDA501-IDARTNR-MIN                      
380100                                W-WDA501-IDARTNR-MAX                      
380200                                                                          
380300     PERFORM IMS-GHU-WDA501-ORDP                                          
380400                                                                          
380500     IF SEGMENT-FINNS                                                     
380600       PERFORM UNTIL SEGMENT-SLUT        OR                               
380700                     SEGMENT-SAKNAS      OR                               
380800                     SW-SATS-FEL    = JA                                  
380900         IF ORDP-RAD-KDSTARAD = '2'                                       
381000           IF SW-TACKT-RO = NEJ                                           
381100             IF WS-RO-ANTAL < ORDP-RAD-KVART                              
381200               COMPUTE  WS-RO-JUST =   ORDP-RAD-KVART                     
381300                                     - WS-RO-ANTAL                        
381400               SUBTRACT WS-RO-JUST  FROM CLAG-KVROS                       
381500                                         SRAD-KVSATROS                    
381600               MOVE     WS-RO-ANTAL   TO ORDP-RAD-KVRO                    
381700               MOVE     WS-RO-ANTAL   TO ORDP-RAD-KVART                   
381800               PERFORM  IMS-REPL-WDA5-ORDP                                
381900               MOVE     JA            TO SW-TACKT-RO                      
382000             END-IF                                                       
382100             IF WS-RO-ANTAL = ORDP-RAD-KVART                              
382200               MOVE     JA            TO SW-TACKT-RO                      
382300             END-IF                                                       
382400             IF WS-RO-ANTAL > ORDP-RAD-KVART                              
382500               SUBTRACT ORDP-RAD-KVART FROM WS-RO-ANTAL                   
382600               MOVE     NEJ           TO SW-TACKT-RO                      
382700             END-IF                                                       
382800           ELSE                                                           
382900             SUBTRACT ORDP-RAD-KVART FROM CLAG-KVROS                      
383000                                          SRAD-KVSATROS                   
383100             PERFORM IMS-DLET-WDA5-ORDP                                   
383200           END-IF                                                         
383300         ELSE                                                             
383400           MOVE JA TO SW-SATS-FEL                                         
383500         END-IF                                                           
383600         PERFORM IMS-GHN-WDA501-ORDP                                      
383700       END-PERFORM                                                        
383800                                                                          
383900     ELSE                                                                 
384000       MOVE JA TO SW-SATS-FEL                                             
384100     END-IF                                                               
384200                                                                          
384300     IF SW-SATS-FEL = JA                                                  
384400       MOVE    FELTEXT3     TO FEL-FELTEXT                                
384500       MOVE    SRAD-IDARTNR TO FEL-ING-IDARTNR                            
384600       PERFORM S135-SKRIV-FELLISTA                                        
384700       PERFORM S30-NASTA-SATSORDER                                        
384800     END-IF                                                               
384900     .                                                                    
385000     EJECT                                                                
385100****************************************************************          
385200* S350-TABORT-RESTORDER  (EJ BYGGB)                            *          
385300* TAR BORT RESTORDER FRÅN RESTORDERSYSTEMET(WDA5) I OCH MED    *          
385400* ATT DENNA ING.ARTIKELN INTE LÄNGRE SKALL ANVÄNDAS.           *          
385500* DETTA GÄLLER VID ERSÄTTNING(OBER AV TILLG) OCH UTGÅNGS-      *          
385600* MARKERADE ING.ARTIKLAR.                                      *          
385700****************************************************************          
385800 S350-TABORT-RESTORDER SECTION.                                           
385900                                                                          
386000     MOVE NEJ                TO SW-SATS-FEL                               
386100     MOVE ZERO               TO WS-KVSATROS-SUM                           
386200     MOVE LOW-VALUE          TO W-WDA501KY-MIN-X                          
386300     MOVE HIGH-VALUE         TO W-WDA501KY-MAX-X                          
386400                                                                          
386500     MOVE SHUV-IDDISTR       TO W-WDA501-IDDISTR-MIN                      
386600     MOVE SHUV-IDDISTR       TO W-WDA501-IDDISTR-MAX                      
386700                                                                          
386800     MOVE SHUV-IDKUNDNR      TO W-WDA501-IDKUNDNR-MIN                     
386900     MOVE SHUV-IDKUNDNR      TO W-WDA501-IDKUNDNR-MAX                     
387000                                                                          
387100     MOVE SHUV-IDORDNSB      TO WS-IDORDNSB                               
387200     MOVE SHUV-IDORDNSS      TO WS-IDORDNSS                               
387300                                                                          
387400     MOVE SPACE              TO W-WDA501-IDKUNDRF-MIN                     
387500                                W-WDA501-IDKUNDRF-MAX                     
387600                                                                          
387700     MOVE WS-IDORDNST-NUM    TO W-WDA501-IDORDNR5-MIN                     
387800                                W-WDA501-IDORDNR5-MAX                     
387900                                                                          
388000     MOVE SRAD-IDARTNR       TO W-WDA501-IDARTNR-MIN                      
388100     MOVE SRAD-IDARTNR       TO W-WDA501-IDARTNR-MAX                      
388200                                                                          
388300     PERFORM IMS-GHU-WDA501-ORDP                                          
388400                                                                          
388500     IF SEGMENT-FINNS                                                     
388600                                                                          
388700        PERFORM UNTIL SEGMENT-SLUT OR                                     
388800                      SEGMENT-SAKNAS                                      
388900                                                                          
389000          IF ORDP-RAD-KDSTARAD = '2'                                      
389100            SUBTRACT ORDP-RAD-KVART FROM CLAG-KVROS                       
389200            ADD      ORDP-RAD-KVART TO   WS-KVSATROS-SUM                  
389300            PERFORM IMS-DLET-WDA5-ORDP                                    
389400          ELSE                                                            
389500            MOVE JA TO SW-SATS-FEL                                        
389600          END-IF                                                          
389700                                                                          
389800          PERFORM IMS-GHN-WDA501-ORDP                                     
389900       END-PERFORM                                                        
390000     ELSE                                                                 
390100       MOVE JA TO SW-SATS-FEL                                             
390200     END-IF                                                               
390300                                                                          
390400     IF SW-SATS-FEL = JA                                                  
390500       MOVE    FELTEXT3     TO FEL-FELTEXT                                
390600       MOVE    SRAD-IDARTNR TO FEL-ING-IDARTNR                            
390700       PERFORM S135-SKRIV-FELLISTA                                        
390800       PERFORM S30-NASTA-SATSORDER                                        
390900     END-IF                                                               
391000     .                                                                    
391100     EJECT                                                                
391200****************************************************************          
391300* NÄR EN ING ARTIKEL RESERVERAS/RESTNOTERAS/ANNULLERAS PÅVER-  *          
391400* KAS ORDERINGÅNGSSTATISTIKEN.                                 *          
391500* S400-SKAPA-2109-TRANS     VID FÖRSTA KÖRNINGEN (ÖKNING)      *          
391600* S410-SKAPA-2109-A-TRANS   MINSKNING AV ANTAL PÅ ING ARTIKEL  *          
391700* S420-SKAPA-2109-O-TRANS   ÖKNING AV ANTAL PÅ ING ARTIKEL     *          
391800****************************************************************          
391900 S400-SKAPA-2109-TRANS SECTION.                                           
392000                                                                          
392010*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
392020     MOVE SRAD-IDARTNR       TO BYT03-IDARTNR                             
392030     IF NOT BYT03-OBJEKT                                                  
392040                                                                          
392100        MOVE 2109-IX          TO 2109-MID2-KVANTART                       
392200        MOVE SRAD-IDARTNR     TO 2109-MID2-IDARTNR (2109-IX)              
392300        MOVE WC-CDC-SE        TO 2109-MID2-IDDC (2109-IX)                 
392400        MOVE '+'              TO 2109-MID2-KDTECKEN (2109-IX)             
392500        MOVE 'KI'             TO 2109-MID2-KDOI (2109-IX)                 
392600        MOVE SPACE            TO 2109-MID2-CLEARGROUP(2109-IX)            
392700        MOVE SRAD-REBEART     TO 2109-MID2-KVOI (2109-IX)                 
392800        MOVE DAGENS-DATUM-NUM TO 2109-MID2-TIUPPDAT (2109-IX)             
392900                                                                          
393000        ADD +1                TO 2109-IX                                  
393100        IF 2109-IX > 2109-IX-MAX                                          
393200          PERFORM S40X-STARTA-2109                                        
393300        END-IF                                                            
393310     END-IF                                                               
393400     .                                                                    
393500     EJECT                                                                
393600 S410-SKAPA-2109-A-TRANS SECTION.                                         
393700                                                                          
393710*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
393720     MOVE SRAD-IDARTNR       TO BYT03-IDARTNR                             
393730     IF NOT BYT03-OBJEKT                                                  
393740                                                                          
393800       COMPUTE WS-REBEART-ANNULL =                                        
393900               SATS-REBEART(SATS-IDX) - SRAD-REBEART                      
394000                                                                          
394100       MOVE 2109-IX           TO 2109-MID2-KVANTART                       
394200       MOVE SRAD-IDARTNR      TO 2109-MID2-IDARTNR (2109-IX)              
394300       MOVE WC-CDC-SE         TO 2109-MID2-IDDC (2109-IX)                 
394400       MOVE '-'               TO 2109-MID2-KDTECKEN (2109-IX)             
394500       MOVE 'KI'              TO 2109-MID2-KDOI (2109-IX)                 
394600       MOVE SPACE             TO 2109-MID2-CLEARGROUP(2109-IX)            
394700       MOVE WS-REBEART-ANNULL TO 2109-MID2-KVOI (2109-IX)                 
394800       MOVE DAGENS-DATUM-NUM  TO 2109-MID2-TIUPPDAT (2109-IX)             
394900                                                                          
395000       ADD +1                 TO 2109-IX                                  
395100       IF 2109-IX > 2109-IX-MAX                                           
395200         PERFORM S40X-STARTA-2109                                         
395300       END-IF                                                             
395310     END-IF                                                               
395400     .                                                                    
395500     EJECT                                                                
395600 S420-SKAPA-2109-O-TRANS SECTION.                                         
395700                                                                          
395710*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
395720     MOVE SRAD-IDARTNR       TO BYT03-IDARTNR                             
395730     IF NOT BYT03-OBJEKT                                                  
395740                                                                          
395800        MOVE 2109-IX          TO 2109-MID2-KVANTART                       
395900        MOVE SRAD-IDARTNR     TO 2109-MID2-IDARTNR (2109-IX)              
396000        MOVE WC-CDC-SE        TO 2109-MID2-IDDC (2109-IX)                 
396100        MOVE '+'              TO 2109-MID2-KDTECKEN (2109-IX)             
396200        MOVE 'KI'             TO 2109-MID2-KDOI (2109-IX)                 
396300        MOVE SPACE            TO 2109-MID2-CLEARGROUP(2109-IX)            
396400        MOVE SATS-NYREBEART (SATS-IDX)                                    
396500                              TO 2109-MID2-KVOI (2109-IX)                 
396600        MOVE DAGENS-DATUM-NUM TO 2109-MID2-TIUPPDAT (2109-IX)             
396700                                                                          
396800        ADD +1                TO 2109-IX                                  
396900        IF 2109-IX > 2109-IX-MAX                                          
397000          PERFORM S40X-STARTA-2109                                        
397100        END-IF                                                            
397110     END-IF                                                               
397200     .                                                                    
397300     EJECT                                                                
397400 S40X-STARTA-2109 SECTION.                                                
397500                                                                          
397600     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
397700                                                                          
397800     PERFORM IMS-PURG-ALT-MSG-2109                                        
397900                                                                          
398000     MOVE SPACE              TO 2109-MID2-W2I10902                        
398100     MOVE ZERO               TO 2109-MID2-KVANTART                        
398200     MOVE +1                 TO 2109-IX                                   
398300     .                                                                    
398400     EJECT                                                                
398500 S500-KOLLA-CHECKPOINT  SECTION.                                          
398600                                                                          
398700     ADD 1 TO RAKNARE                                                     
398800     IF RAKNARE > 200                                                     
398900        MOVE    SHUV-IDARTNR  TO W-WDJ2C-MIN-IDARTNR                      
399000        MOVE    SHUV-DAREGDAT TO W-WDJ2C-MIN-DAREGDAT                     
399100        MOVE    ZERO          TO RAKNARE                                  
399200        PERFORM IMS-CHECKPOINT                                            
399300     END-IF                                                               
399400     .                                                                    
399500     EJECT                                                                
399600                                                                          
399700 IMS-PURGE-ALT2191-MSG SECTION.                                           
399800                                                                          
399900     MOVE LOW-VALUE TO ALT2191-Z1 ALT2191-Z2                              
400000     MOVE '  '  TO GODK-STATUSKODER                                       
400100     CALL CBLTDLI USING PURG ALT2191-PCB ALT2191-IO-AREA                  
400200     MOVE ALT2191-STATUS-CODE TO STATUS-WS                                
400300     PERFORM IMS-STATUSKONTROLL                                           
400400     .                                                                    
400500     SKIP2                                                                
400600 IMS-PURG-ALT-MSG-2109 SECTION.                                           
400700     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
400800     MOVE SPACE TO GODK-STATUSKODER                                       
400900     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
401000     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
401100     PERFORM IMS-STATUSKONTROLL                                           
401200     .                                                                    
401300     SKIP2                                                                
401400 IMS-GHU-WDJ201-SATG SECTION.                                             
401500                                                                          
401600     STRING 'WLSATG01(IDORDNST =' W-WDJ201-IDORDNST-X ')'                 
401700          DELIMITED BY SIZE INTO SSA1                                     
401800     MOVE '  GE' TO GODK-STATUSKODER                                      
401900     CALL CBLTDLI USING GHU SATG-PCB IO-AREA-SATG01 SSA1                  
402000     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
402100     PERFORM IMS-STATUSKONTROLL                                           
402200     .                                                                    
402300     EJECT                                                                
402400 IMS-GHU-WDJ211-SATG SECTION.                                             
402500                                                                          
402600     STRING 'WLSATG01(IDORDNST =' W-WDJ201-IDORDNST-X ')'                 
402700          DELIMITED BY SIZE INTO SSA1                                     
402800     STRING 'WLSATG11(IDARTNR  =' W-WDJ211-IDARTNR-X ')'                  
402900          DELIMITED BY SIZE INTO SSA2                                     
403000     MOVE '  GE' TO GODK-STATUSKODER                                      
403100     CALL CBLTDLI USING GHU SATG-PCB IO-AREA-SATG11 SSA1 SSA2             
403200     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
403300     PERFORM IMS-STATUSKONTROLL                                           
403400     .                                                                    
403500     SKIP3                                                                
403600 IMS-GU-WDJ211-KDSATKMB SECTION.                                          
403700                                                                          
403800     STRING 'WLSATG01(IDORDNST =' W-WDJ201-IDORDNST-X ')'                 
403900          DELIMITED BY SIZE INTO SSA1                                     
404000     STRING 'WLSATG11(IDARTNR  >' W-WDJ211-IDARTNR-X                      
404100                    '&KDSATKMB>=' W-WDJ211-KDSATKMB-MIN-X                 
404200                    '&KDSATKMB<=' W-WDJ211-KDSATKMB-MAX-X ')'             
404300          DELIMITED BY SIZE INTO SSA2                                     
404400     MOVE '  GE' TO GODK-STATUSKODER                                      
404500     CALL CBLTDLI USING GU SATG1-PCB IO-AREA-WDJ211-SATG1                 
404600                                     SSA1 SSA2                            
404700     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
404800     PERFORM IMS-STATUSKONTROLL                                           
404900     .                                                                    
405000     EJECT                                                                
405100 IMS-GN-WDJ211-KDSATKMB SECTION.                                          
405200                                                                          
405300     STRING 'WLSATG01(IDORDNST =' W-WDJ201-IDORDNST-X ')'                 
405400          DELIMITED BY SIZE INTO SSA1                                     
405500     STRING 'WLSATG11(IDARTNR  >' W-WDJ211-IDARTNR-X                      
405600                    '&KDSATKMB>=' W-WDJ211-KDSATKMB-MIN-X                 
405700                    '&KDSATKMB<=' W-WDJ211-KDSATKMB-MAX-X ')'             
405800          DELIMITED BY SIZE INTO SSA2                                     
405900     MOVE '  GE' TO GODK-STATUSKODER                                      
406000     CALL CBLTDLI USING GN SATG1-PCB IO-AREA-WDJ211-SATG1                 
406100                                     SSA1 SSA2                            
406200     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
406300     PERFORM IMS-STATUSKONTROLL                                           
406400     .                                                                    
406500     SKIP3                                                                
406600 IMS-REPL-WDJ211-SATG SECTION.                                            
406700                                                                          
406800     MOVE '  ' TO GODK-STATUSKODER                                        
406900     CALL CBLTDLI USING REPL SATG-PCB IO-AREA-SATG11                      
407000     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
407100     PERFORM IMS-STATUSKONTROLL                                           
407200     .                                                                    
407300     EJECT                                                                
407400 IMS-INSERT-WDJ211-SATG SECTION.                                          
407500                                                                          
407600     STRING 'WLSATG01(IDORDNST =' W-WDJ201-IDORDNST-X ')'                 
407700          DELIMITED BY SIZE INTO SSA1                                     
407800     MOVE 'WLSATG11 ' TO SSA2                                             
407900     MOVE '  II' TO GODK-STATUSKODER                                      
408000     CALL CBLTDLI USING ISRT SATG-PCB IO-AREA-SATG11 SSA1 SSA2            
408100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
408200     PERFORM IMS-STATUSKONTROLL                                           
408300     .                                                                    
408400     SKIP3                                                                
408500 IMS-REPL-WDJ201-SATG SECTION.                                            
408600                                                                          
408700     MOVE '  ' TO GODK-STATUSKODER                                        
408800     CALL CBLTDLI USING REPL SATG-PCB IO-AREA-SATG01                      
408900     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
409000     PERFORM IMS-STATUSKONTROLL                                           
409100     .                                                                    
409200     EJECT                                                                
409300 IMS-GN-WDJ2-SATG-CSEQ SECTION.                                           
409400                                                                          
409500     STRING 'WLSATG01(WDJ2CSEQ>=' W-WDJ2C-MIN-X                           
409600                    '&WDJ2CSEQ<=' W-WDJ2C-MAX-X ')'                       
409700          DELIMITED BY SIZE INTO SSA1                                     
409800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
409900     CALL CBLTDLI USING GN SATJ-PCB IO-AREA-SATG01 SSA1                   
410000     MOVE SATJ-STATUS-CODE TO STATUS-WS                                   
410100     PERFORM IMS-STATUSKONTROLL                                           
410200     .                                                                    
410300     SKIP3                                                                
410400 IMS-GN-WDJ2-SATK-DSEQ-FIRST SECTION.                                     
410500                                                                          
410600     STRING 'WLSATG01(WDJ2DSEQ>=' W-WDJ2D-MIN-X                           
410700                    '&WDJ2DSEQ<=' W-WDJ2D-MAX-X ')'                       
410800          DELIMITED BY SIZE INTO SSA1                                     
410900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
411000     CALL CBLTDLI USING GN SATK-PCB IO-AREA-SATG01 SSA1                   
411100     MOVE SATK-STATUS-CODE TO STATUS-WS                                   
411200     PERFORM IMS-STATUSKONTROLL                                           
411300     .                                                                    
411400     SKIP3                                                                
411500 IMS-GU-WDJ2-SATK-DSEQ-AKT   SECTION.                                     
411600                                                                          
411700     STRING 'WLSATG01(WDJ2DSEQ =' W-WDJ2D-AKT-X ')'                       
411800          DELIMITED BY SIZE INTO SSA1                                     
411900     MOVE '  ' TO GODK-STATUSKODER                                        
412000     CALL CBLTDLI USING GU SATK-PCB IO-AREA-SATG01 SSA1                   
412100     MOVE SATK-STATUS-CODE TO STATUS-WS                                   
412200     PERFORM IMS-STATUSKONTROLL                                           
412300     .                                                                    
412400     SKIP3                                                                
412500 IMS-GN-WDJ2-SATK-DSEQ-AKT SECTION.                                       
412600                                                                          
412700     STRING 'WLSATG01 '                                                   
412800          DELIMITED BY SIZE INTO SSA1                                     
412900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
413000     CALL CBLTDLI USING GN SATK-PCB IO-AREA-SATG01 SSA1                   
413100     MOVE SATK-STATUS-CODE TO STATUS-WS                                   
413200     PERFORM IMS-STATUSKONTROLL                                           
413300     .                                                                    
413400     EJECT                                                                
413500 IMS-GNP-WDJ2-SATK11-FIRST SECTION.                                       
413600                                                                          
413700     STRING 'WLSATG11*F '                                                 
413800          DELIMITED BY SIZE INTO SSA1                                     
413900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
414000     CALL CBLTDLI USING GNP SATK-PCB IO-AREA-SATG11 SSA1                  
414100     MOVE SATK-STATUS-CODE TO STATUS-WS                                   
414200     PERFORM IMS-STATUSKONTROLL                                           
414300     .                                                                    
414400     SKIP3                                                                
414500 IMS-GNP-WDJ2-SATK11-REST SECTION.                                        
414600                                                                          
414700     STRING 'WLSATG11 '                                                   
414800          DELIMITED BY SIZE INTO SSA1                                     
414900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
415000     CALL CBLTDLI USING GNP SATK-PCB IO-AREA-SATG11 SSA1                  
415100     MOVE SATK-STATUS-CODE TO STATUS-WS                                   
415200     PERFORM IMS-STATUSKONTROLL                                           
415300     .                                                                    
415400     EJECT                                                                
415500 IMS-GNP-WDJ2-SATG11-FIRST SECTION.                                       
415600                                                                          
415700     STRING 'WLSATG11*F '                                                 
415800          DELIMITED BY SIZE INTO SSA1                                     
415900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
416000     CALL CBLTDLI USING GNP SATG-PCB IO-AREA-SATG11 SSA1                  
416100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
416200     PERFORM IMS-STATUSKONTROLL                                           
416300     .                                                                    
416400     SKIP3                                                                
416500 IMS-GNP-WDJ2-SATG11-REST SECTION.                                        
416600                                                                          
416700     STRING 'WLSATG11 '                                                   
416800          DELIMITED BY SIZE INTO SSA1                                     
416900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
417000     CALL CBLTDLI USING GNP SATG-PCB IO-AREA-SATG11 SSA1                  
417100     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
417200     PERFORM IMS-STATUSKONTROLL                                           
417300     .                                                                    
417400     EJECT                                                                
417500 IMS-GU-WDJ1-SATB SECTION.                                                
417600                                                                          
417700     STRING 'WLSATB01(IDARTNR  =' W-WDJ1-IDARTNR-X ')'                    
417800          DELIMITED BY SIZE INTO SSA1                                     
417900     MOVE '  ' TO GODK-STATUSKODER                                        
418000     CALL CBLTDLI USING GU SATB-PCB IO-AREA-SATB01 SSA1                   
418100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
418200     PERFORM IMS-STATUSKONTROLL                                           
418300     .                                                                    
418400     SKIP3                                                                
418500 IMS-GNP-WDJ1-SATB SECTION.                                               
418600                                                                          
418700     STRING 'WLSATB11 '                                                   
418800          DELIMITED BY SIZE INTO SSA1                                     
418900     MOVE '  GE' TO GODK-STATUSKODER                                      
419000     CALL CBLTDLI USING GNP SATB-PCB IO-AREA-SATB11 SSA1                  
419100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
419200     PERFORM IMS-STATUSKONTROLL                                           
419300     .                                                                    
419400     EJECT                                                                
419500 IMS-GU-WDD701-ERSA SECTION.                                              
419600                                                                          
419700     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
419800          DELIMITED BY SIZE INTO SSA1                                     
419900     MOVE '  GE' TO GODK-STATUSKODER                                      
420000     CALL CBLTDLI USING GU ERSA-PCB IO-AREA-ERSA01 SSA1                   
420100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
420200     PERFORM IMS-STATUSKONTROLL                                           
420300     .                                                                    
420400     SKIP3                                                                
420500 IMS-GNP-WDD702-ERSA SECTION.                                             
420600                                                                          
420700     STRING 'WLERSA11 '                                                   
420800          DELIMITED BY SIZE INTO SSA1                                     
420900     MOVE '  GE' TO GODK-STATUSKODER                                      
421000     CALL CBLTDLI USING GNP ERSA-PCB IO-AREA-ERSA11 SSA1                  
421100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
421200     PERFORM IMS-STATUSKONTROLL                                           
421300     .                                                                    
421400     EJECT                                                                
421500 IMS-GHU-WDA5-ORDP SECTION.                                               
421600                                                                          
421700     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
421800          DELIMITED BY SIZE INTO SSA1                                     
421900     MOVE '  GE' TO GODK-STATUSKODER                                      
422000     CALL CBLTDLI USING GHU ORDP-PCB IO-AREA-ORDP01 SSA1                  
422100     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
422200     PERFORM IMS-STATUSKONTROLL                                           
422300     .                                                                    
422400     SKIP3                                                                
422500 IMS-GHU-WDA501-ORDP SECTION.                                             
422600                                                                          
422700     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
422800                    '&WDA501KY<=' W-WDA501KY-MAX-X ')'                    
422900          DELIMITED BY SIZE INTO SSA1                                     
423000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
423100     CALL CBLTDLI USING GHU ORDP-PCB IO-AREA-ORDP01 SSA1                  
423200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
423300     PERFORM IMS-STATUSKONTROLL                                           
423400     .                                                                    
423500     EJECT                                                                
423600 IMS-GHN-WDA501-ORDP SECTION.                                             
423700                                                                          
423800     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN-X                        
423900                    '&WDA501KY<=' W-WDA501KY-MAX-X ')'                    
424000          DELIMITED BY SIZE INTO SSA1                                     
424100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
424200     CALL CBLTDLI USING GHN ORDP-PCB IO-AREA-ORDP01 SSA1                  
424300     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
424400     PERFORM IMS-STATUSKONTROLL                                           
424500     .                                                                    
424600     SKIP3                                                                
424700 IMS-DLET-WDA5-ORDP SECTION.                                              
424800                                                                          
424900     MOVE '  ' TO GODK-STATUSKODER                                        
425000     CALL CBLTDLI USING DLET ORDP-PCB IO-AREA-ORDP01                      
425100     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
425200     PERFORM IMS-STATUSKONTROLL                                           
425300     .                                                                    
425400     EJECT                                                                
425500 IMS-REPL-WDA5-ORDP SECTION.                                              
425600                                                                          
425700     MOVE '  ' TO GODK-STATUSKODER                                        
425800     CALL CBLTDLI USING REPL ORDP-PCB IO-AREA-ORDP01                      
425900     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
426000     PERFORM IMS-STATUSKONTROLL                                           
426100     .                                                                    
426200     SKIP3                                                                
426300 IMS-GU-WDA5-ORDQ SECTION.                                                
426400                                                                          
426500     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
426600                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X ')'                    
426700          DELIMITED BY SIZE INTO SSA1                                     
426800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
426900     CALL CBLTDLI USING GU ORDQ-PCB IO-AREA-ORDQ01 SSA1                   
427000     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
427100     PERFORM IMS-STATUSKONTROLL                                           
427200     .                                                                    
427300     EJECT                                                                
427400 IMS-GN-WDA5-ORDQ SECTION.                                                
427500                                                                          
427600     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
427700                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X ')'                    
427800          DELIMITED BY SIZE INTO SSA1                                     
427900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
428000     CALL CBLTDLI USING GN ORDQ-PCB IO-AREA-ORDQ01 SSA1                   
428100     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
428200     PERFORM IMS-STATUSKONTROLL                                           
428300     .                                                                    
428400     SKIP3                                                                
428500 IMS-GU-WDA5-ORDR SECTION.                                                
428600                                                                          
428700     STRING 'WLORDR01(WDA5B1KY>=' W-WDA5B1KY-MIN-X                        
428800                    '&WDA5B1KY<=' W-WDA5B1KY-MAX-X ')'                    
428900          DELIMITED BY SIZE INTO SSA1                                     
429000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
429100     CALL CBLTDLI USING GU ORDR-PCB IO-AREA-ORDR01 SSA1                   
429200     MOVE ORDR-STATUS-CODE TO STATUS-WS                                   
429300     PERFORM IMS-STATUSKONTROLL                                           
429400     .                                                                    
429500     EJECT                                                                
429600 IMS-GN-WDA5-ORDR SECTION.                                                
429700                                                                          
429800     STRING 'WLORDR01(WDA5B1KY>=' W-WDA5B1KY-MIN-X                        
429900                    '&WDA5B1KY<=' W-WDA5B1KY-MAX-X ')'                    
430000          DELIMITED BY SIZE INTO SSA1                                     
430100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
430200     CALL CBLTDLI USING GN ORDR-PCB IO-AREA-ORDR01 SSA1                   
430300     MOVE ORDR-STATUS-CODE TO STATUS-WS                                   
430400     PERFORM IMS-STATUSKONTROLL                                           
430500     .                                                                    
430600     SKIP3                                                                
430700 IMS-ISRT-WDA5-WLORDP01 SECTION.                                          
430800                                                                          
430900     MOVE 'WLORDP01 ' TO SSA1                                             
431000     MOVE '  II' TO GODK-STATUSKODER                                      
431100     CALL CBLTDLI USING ISRT ORDP-PCB IO-AREA-ORDP01 SSA1                 
431200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
431300     PERFORM IMS-STATUSKONTROLL                                           
431400     .                                                                    
431500     EJECT                                                                
431600 IMS-GU-XXJN-WLXXJN11 SECTION.                                            
431700                                                                          
431800     STRING 'WLXXJN01(WDGXKEY  =' W-4511-IDHTYP-X  ')'                    
431900          DELIMITED BY SIZE INTO SSA1                                     
432000     STRING 'WLXXJN11(KDTPOTYP =' W-4512-KDTPOTYP-X                       
432100                    '&KDORDKL  =' W-4512-KDORDKL-X                        
432200                    '&IDDISTRF<=' W-4512-IDDISTR-FOM-X                    
432300                    '&IDDISTRT>=' W-4512-IDDISTR-TOM-X ')'                
432400          DELIMITED BY SIZE INTO SSA2                                     
432500     MOVE '    ' TO GODK-STATUSKODER                                      
432600     CALL CBLTDLI USING GU XXJN-PCB IO-AREA-XXJN11 SSA1 SSA2              
432700     MOVE XXJN-STATUS-CODE TO STATUS-WS                                   
432800     PERFORM IMS-STATUSKONTROLL                                           
432900     .                                                                    
433000     SKIP3                                                                
433100 IMS-GU-WDK601 SECTION.                                                   
433200                                                                          
433300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
433400          DELIMITED BY SIZE INTO SSA1                                     
433500     MOVE '    ' TO GODK-STATUSKODER                                      
433600     CALL CBLTDLI USING GU ARTC-PCB IO-AREA-ARTC01 SSA1                   
433700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
433800     PERFORM IMS-STATUSKONTROLL                                           
433900     .                                                                    
434000     EJECT                                                                
434100 IMS-GU-WDK611 SECTION.                                                   
434200                                                                          
434300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
434400          DELIMITED BY SIZE INTO SSA1                                     
434500     MOVE 'WLARTC11 ' TO SSA2                                             
434600     MOVE '    ' TO GODK-STATUSKODER                                      
434700     CALL CBLTDLI USING GU   ARTC-PCB IO-AREA-ARTC11 SSA1 SSA2            
434800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
434900     PERFORM IMS-STATUSKONTROLL                                           
435000     .                                                                    
435100     SKIP2                                                                
435200 IMS-GHNP-WDK611 SECTION.                                                 
435300                                                                          
435400     MOVE 'WLARTC11 ' TO SSA1                                             
435500     MOVE '    ' TO GODK-STATUSKODER                                      
435600     CALL CBLTDLI USING GHNP ARTC-PCB IO-AREA-ARTC11 SSA1                 
435700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
435800     PERFORM IMS-STATUSKONTROLL                                           
435900     .                                                                    
436000     EJECT                                                                
436100 IMS-REPL-WDK611 SECTION.                                                 
436200                                                                          
436300     MOVE '  ' TO GODK-STATUSKODER                                        
436400     CALL CBLTDLI USING REPL ARTC-PCB IO-AREA-ARTC11                      
436500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
436600     PERFORM IMS-STATUSKONTROLL                                           
436700     .                                                                    
436800     SKIP3                                                                
436900 IMS-GU-WDK9-ARTM SECTION.                                                
437000                                                                          
437100     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
437200          DELIMITED BY SIZE INTO SSA1                                     
437300     MOVE '  GE' TO GODK-STATUSKODER                                      
437400     CALL CBLTDLI USING GU ARTM-PCB IO-AREA-ARTM01 SSA1                   
437500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
437600     PERFORM IMS-STATUSKONTROLL                                           
437700     .                                                                    
437800     EJECT                                                                
437900 IMS-RESTART SECTION.                                                     
438000                                                                          
438100     MOVE SPACE TO MSG-IO-AREA                                            
438200     MOVE '  ' TO GODK-STATUSKODER                                        
438300     CALL CBLTDLI USING XRST MSG-PCB                                      
438400                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
438500                        CHKP-AREA-LENGTH CHKP-AREA                        
438600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
438700     PERFORM IMS-STATUSKONTROLL                                           
438800     .                                                                    
438900     SKIP3                                                                
439000 IMS-CHECKPOINT SECTION.                                                  
439100                                                                          
439200     MOVE SPACE TO MSG-IO-AREA                                            
439300     MOVE '  XD' TO GODK-STATUSKODER                                      
439400     CALL CBLTDLI USING CHKP MSG-PCB                                      
439500                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
439600                        CHKP-AREA-LENGTH CHKP-AREA                        
439700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
439800     PERFORM IMS-STATUSKONTROLL                                           
439900                                                                          
440000     IF IMS-EJ-OK                                                         
440100       CALL FELLOG                                                        
440200     END-IF                                                               
440300     .                                                                    
440400     SKIP3                                                                
440500 IMS-STATUSKONTROLL SECTION.                                              
440600                                                                          
440700     SET    STATUS-IX TO 1                                                
440800     SEARCH GODK-STATUS                                                   
440900       AT END CALL FELLOG                                                 
441000         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                         
441100         CONTINUE                                                         
441200     END-SEARCH                                                           
441300     .                                                                    
441400     EJECT                                                                
441500*    -COPY WY2000P1                                                       
441600     EJECT                                                                
441700*    -COPY WY2000Q1                                                       
