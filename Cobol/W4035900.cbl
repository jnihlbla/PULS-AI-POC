000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W4035900.                                                
000500 AUTHOR.         CAP GEMINI AB/EP.                                        
000600 DATE-WRITTEN.   JULI-SEPT 86.                                            
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        ANNULLATION AV HELA ORDER, RADINTERVALL ELLER OM RAD-            
001200*        NUMMER ANGES HELA/EL DEL AV RADEN.                               
001300*                                                                         
001400*        SE&O.                                                            
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T359                                              
001800*        MID:         W4I35901                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O35901                                            
002200*        TRANS:       W4T397X                                             
002300* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
002400**                                                                        
002500* ETRACK 5522142 ADD KEYFIELD ORDERPART NUMBER ON SCREEN                  
002600* ETRACK 7450328 2008-HÖST  VOHF                                          
002700* ETRACK 10254592 2015      DECOMISSION VOHF                              
002800* ETRACK 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2             
002900*    SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP3                                                                
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500*    -- CHECKED BY WY2000                                                 
003600     SKIP3                                                                
003700 77    IDPGM                     PIC X(8)    VALUE 'W4035900'.            
003800 77    WS-LOG-AVBROTT            PIC X(32)   VALUE SPACE.                 
003900 01    FILLER                    PIC X(8)    VALUE 'IMSIMSIM'.            
004000 77    WS-IMS-AVBROTT            PIC X(32)   VALUE SPACE.                 
004100 01    FILLER                    PIC X(8)    VALUE 'FELTEXT:'.            
004200 77    FELTEXT                   PIC X(80)   VALUE SPACE.                 
004300 77    JA                        PIC X       VALUE 'J'.                   
004400 77    YES                       PIC X       VALUE 'Y'.                   
004500 77    NEJ                       PIC X       VALUE 'N'.                   
004600                                                                          
004700*01    -COPY WWDCKONS                                                     
004800                                                                          
004900*01    -COPY WWBYT03                                                      
005000                                                                          
005100 77    W-IDDC                    PIC X(2)    VALUE SPACE.                 
005200                                                                          
005300 77    RAETT                     PIC X       VALUE 'R'.                   
005400 77    FEL                       PIC X       VALUE 'F'.                   
005500 77    FINNS-PA-RDE5             PIC X       VALUE 'N'.                   
005600 77    KLAR-PA-WDE6              PIC X       VALUE 'N'.                   
005700 77    WS-FLMANORD               PIC X       VALUE 'N'.                   
005800 01    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
005900 77    2109-IX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006000 77    2109-IX-MAX               PIC S9(9)   VALUE +18  COMP SYNC.        
006100 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
006200 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
006300 77    IX                        PIC S9(9)   VALUE +0   COMP SYNC.        
006400 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +186 COMP SYNC.        
006500 77    4359-LAENGD               PIC S9(4)   VALUE +69  COMP SYNC.        
006600 01    FILLER                    PIC X(8)    VALUE 'BBBBBBBB'.            
006700 77    WS-ANTAL-ANNULL-RADER     PIC S9(4)   VALUE +0    COMP-3.          
006800 77    WS-TOT-ANT-RADER          PIC S9(4)   VALUE +0    COMP-3.          
006900 77    WS-ANT-RADER-INT-PLUS-1   PIC S9(4)   VALUE +0    COMP-3.          
007000 77    WS-ANT-RADER-INT          PIC S9(4)   VALUE +0    COMP-3.          
007100 77    WS-KDORDKL                PIC S9(1)   VALUE +0    COMP-3.          
007200 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
007300 77    WS-DAGENS-DATUM           PIC 9(6)   VALUE ZERO.                   
007400 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
007500 77    WS-IDDISTR-NUM            PIC 9(4)   VALUE ZERO.                   
007600 77    WS-HUV-KVORDRAD           PIC S9(5)  VALUE ZERO COMP-3.            
007700 77    WS-HUV-KVORDRAD-PACK      PIC S9(5)  VALUE ZERO COMP-3.            
007800 77    WS-FLRESTN                PIC X(1)   VALUE SPACE.                  
007900 77    WS-FLDIRLEV               PIC X(1)   VALUE SPACE.                  
008000 77    WS-FLLSBOK                PIC X(1)   VALUE SPACE.                  
008100 77    WS-FLORDSPE               PIC X(1)   VALUE SPACE.                  
008200 77    WS-IDARTNR                PIC 9(9)   VALUE ZERO.                   
008300 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
008400 77    WS-KDFAKTYP               PIC X(1)   VALUE SPACE.                  
008500 77    WS-KDFRAKT                PIC S9(3)  VALUE ZERO.                   
008600 77    WS-IDPRODNR               PIC X(7)   VALUE SPACE.                  
008700 77    WS-IDRADNR                PIC X(4)   VALUE SPACE.                  
008800 01    FILLER                    PIC X(8)    VALUE 'CCCCCCCC'.            
008900 77    WS-IDRADNR-START-NUM      PIC 9(4)   VALUE ZERO.                   
009000 77    WS-IDRADNR-NUM            PIC 9(4)   VALUE ZERO.                   
009100 77    WS-IDRADNR-MAX            PIC 9(4)   VALUE ZERO.                   
009200 77    WS-IDRADNR-FOM            PIC 9(4)   VALUE ZERO.                   
009300 77    WS-IDRADNR-TOM            PIC 9(4)   VALUE ZERO.                   
009400 77    WS-RAD-IDRADNR-ORD-FROM   PIC 9(4)   VALUE ZERO.                   
009500 77    WS-IDRADNR-START          PIC 9(4)   VALUE ZERO.                   
009600 77    WS-IDPLKLST-START         PIC 9(3)   VALUE ZERO.                   
009700 77    WS-IDPLKLST               PIC 9(3)   VALUE ZERO.                   
009800 77    WS-AKTUELL-RAD            PIC 9(4)   VALUE ZERO.                   
009900 77    WS-KVAVBART               PIC 9(6)   VALUE ZERO.                   
010000 77    SPAR-KVAVBART             PIC 9(6)   VALUE ZERO.                   
010100 77    WS-KVART                  PIC 9(6)   VALUE ZERO.                   
010200 77    WS-KVANNANT               PIC 9(6)   VALUE ZERO.                   
010300 77    WS-ORAD-KVANNANT          PIC 9(6)   VALUE ZERO.                   
010400 77    WS-KVPRERO-JUST           PIC S9(7)  VALUE ZERO COMP-3.            
010500 77    WS-RESLATT                PIC S9(3)  VALUE ZERO COMP-3.            
010600 77    WS-KVSLATT                PIC S9(7)  VALUE ZERO COMP-3.            
010700 77    WS-KVSLATTAT              PIC S9(7)  VALUE ZERO COMP-3.            
010800 77    WS-ORAD-SLUTANNULL        PIC X(1).                                
010900 77    WS-FLREFILL               PIC X(1).                                
011000 77    WS-REDIRLEV               PIC S9V9(2) VALUE ZERO COMP-3.           
011100 01    FILLER                    PIC X(8)    VALUE 'DDDDDDDD'.            
011200 77    WS-KVLS                   PIC S9(7)  VALUE ZERO COMP-3.            
011300 77    WS-KVEFRS                 PIC S9(7)  VALUE ZERO COMP-3.            
011400 77    WS-RAD-KDTPOTYP           PIC S9     VALUE ZERO COMP-3.            
011500 77    WS-MINSKA-KVPRERO         PIC S9(7)  VALUE ZERO COMP-3.            
011600 77    MAX-RAD-ANTAL             PIC S9(9)  VALUE +500 COMP SYNC.         
011700 77    TAECKNING                 PIC X      VALUE SPACE.                  
011800 77    DISPONIBELT               PIC S9(7)  VALUE +0   COMP-3.            
011900 77    WS-CLAG-KDLEVSP           PIC S9(3)  VALUE +0   COMP-3.            
012000 77    W-ART-KVLS                PIC S9(7)  VALUE +0   COMP-3.            
012100 77    W-ART-KVUTRS              PIC S9(7)  VALUE +0   COMP-3.            
012200 77    W-ART-KVRESS              PIC S9(7)  VALUE +0   COMP-3.            
012300 77    W-ART-KVSPANT             PIC S9(7)  VALUE +0   COMP-3.            
012400 77    W-ART-KDERS               PIC S9(3)  VALUE +0   COMP-3.            
012500 77    W-ART-KVSLAGER            PIC S9(7)  VALUE +0   COMP-3.            
012600 77    W-ART-PRARTSTD            PIC S9(7)  VALUE +0   COMP-3.            
012700 77    SPAR-IDPURAD              PIC S9(5)  VALUE ZERO COMP-3.            
012800 01    FILLER                    PIC X(8)    VALUE 'EEEEEEEE'.            
012900 77    WS-CLAG-KVQPACK-1         PIC S9(5)  VALUE ZERO COMP-3.            
013000 77    SPAR-BEKUNDRF             PIC X(15)  VALUE ZERO.                   
013100 01    FILLER                    PIC X(8)    VALUE 'HIHIHIHI'.            
013200 77    SPAR-ODEL-TIRFS           PIC S9(11) VALUE ZERO COMP-3.            
013300 77    SPAR-ODEL-IDPRODNR        PIC S9(07) VALUE ZERO COMP-3.            
013400 77    SPAR-ODEL-IDPLKLST        PIC S9(03) VALUE ZERO COMP-3.            
013500 77    WS-TIREGTID               PIC  9(08) VALUE ZERO.                   
013600 77    WS-DATUM-9KOMPL           PIC 9(8).                                
013700 77    DAGENS-DATUM              PIC 9(9).                                
013800 77    WS-TID                    PIC 9(9).                                
013900 77    WS-TISKPTID               PIC 9(6)    VALUE ZERO.                  
014000 77    KDRC-DISP                 PIC 9(4)    VALUE ZERO.                  
014100 77    WS-IDCOM                  PIC S9(9)   VALUE ZERO COMP-3.           
014200 77    W-IDSHIPM                 PIC 9(7)    VALUE ZERO.                  
014300 77  WS-TINUDAT                  PIC S9(7)  COMP-3.                       
014400 77  WS-TINUTID                  PIC S9(9)  COMP-3.                       
014500*                                                                         
014600 77    FILLER                    PIC  X(08) VALUE 'DN WS   '.             
014700 77    WS-DNOT-IDORDER           PIC S9(07) VALUE ZERO COMP-3.            
014800 77    WS-DNOT-IDARTNR           PIC S9(09) VALUE ZERO COMP-3.            
014900 77    WS-DNOT-IDDC              PIC  X(02) VALUE ZERO.                   
015000 77    WS-DNOT-IDKOLLI           PIC S9(05) VALUE ZERO COMP-3.            
015100 77    WS-DNOT-IDPURAD           PIC S9(05) VALUE ZERO COMP-3.            
015200       EJECT                                                              
015300 77    WS-IDTRANS                PIC X(04).                               
015400   88  EGEN-MID                             VALUE '4359'.                 
015500   88  GODK-MID                             VALUE '4359'.                 
015600     SKIP2                                                                
015700 77    WS-INDATA-TEST            PIC X(01).                               
015800   88  WS-INDATA-FEL                        VALUE 'F'.                    
015900   88  WS-INDATA-RATT                       VALUE 'R'.                    
016000     SKIP2                                                                
016100 77    WS-BEHANDLING-TEST        PIC X(01).                               
016200   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
016300   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
016400 01    WS-SATS                   PIC X(1).                                
016500   88  FINNS-I-SATS                         VALUE 'J'.                    
016600   88  SAKNAS-I-SATS                        VALUE 'N'.                    
016700     SKIP2                                                                
016800 01    WS-KLOCKAN.                                                        
016900   03  WS-TIHHMMSS               PIC 9(6).                                
017000   03  FILLER                    PIC X(2).                                
017100     SKIP2                                                                
017200 01    FL-4359-OMSTARTAD         PIC X(01).                               
017300     SKIP2                                                                
017400 77    FL-NYCKLAR                PIC X(01).                               
017500   88  NYA-NYCKLAR                          VALUE 'J'.                    
017600   88  GAMLA-NYCKLAR                        VALUE 'N'.                    
017700     SKIP2                                                                
017800 77    FL-RAD-INOM-INTERVALL     PIC X(01).                               
017900   88  RAD-FINNS-I-INTERVALL                VALUE 'J'.                    
018000     SKIP2                                                                
018100 77  WS-VORD-FARDIGPACKAD        PIC X(1).                                
018200     EJECT                                                                
018300 01    WS-FLAUTFAK               PIC X(01).                               
018400   88  AUT-FAK-SKRIVS-EJ-UT                 VALUE 'N'.                    
018500   88  AUT-FAKTURA-SKRIVS-UT                VALUE 'J'.                    
018600*                                                                         
018700 01    WS-LASNINGSTRANS          PIC X(01).                               
018800   88  LASNINGSTRANS-TAS-EJ-BORT            VALUE 'N'.                    
018900   88  LASNINGSTRANS-TAS-BORT               VALUE 'J'.                    
019000*                                                                         
019100 01    WS-SLINGA-KLAR            PIC X(01).                               
019200   88  SLINGA-KLAR                          VALUE 'J'.                    
019300*                                                                         
019400 01    FIRST-TIME-SW             PIC X(01).                               
019500   88  FIRST-TIME                           VALUE 'J'.                    
019600                                                                          
019700 01    SW-TIKLAR-UPPDATERAD      PIC X(01).                               
019800*                                                                         
019900 01    WS-IDUSER                 PIC X(8).                                
020000*                                                                         
020100 01    FILLER REDEFINES  WS-IDUSER.                                       
020200   03  FILLER                    PIC X(2).                                
020300   03  WS-IDANSTNR               PIC 9(5).                                
020400   03  FILLER                    PIC X(1).                                
020500*                                                                         
020600 01  RKOD-33                     PIC S9(4)  VALUE +33 COMP SYNC.          
020700*                                                                         
020800 01     WS-IDKUNDRF.                                                      
020900   03   WS-IDORDNR              PIC X(5).                                 
021000   03   FILLER                  PIC X(5)    VALUE SPACE.                  
021100     SKIP2                                                                
021200 01     WS-IDKUNDRF-OLD.                                                  
021300   03   WS-IDORDNR5-OLD         PIC X(5).                                 
021400   03   FILLER                  PIC X(5)    VALUE SPACE.                  
021500     SKIP2                                                                
021600 01     WS-IDKUNDRF-NEW.                                                  
021700   03   FILLER                  PIC X(2)    VALUE ZERO.                   
021800   03   WS-IDORDNR7-NEW         PIC X(5).                                 
021900   03   FILLER                  PIC X(3)    VALUE SPACE.                  
022000 01 DB2-LASNING.                                                          
022100     03 FILLER                   PIC X(16)   VALUE                        
022200                                             'WS-DB2-SEKTION'.            
022300     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
022400                                                                          
022500     EJECT                                                                
022600 01 NYCKLAR-TP4TRAN.                                                      
022700     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
022800                                                                          
022900     EJECT                                                                
023000 01     TEST-IDDISTR            PIC 9(5)                 COMP-3.          
023100*01     FILLER   -COPY WWDIST03     -RED TEST-IDDISTR.                    
023200*01     FILLER   -COPY WWDIST07     -RED TEST-IDDISTR.                    
023300*01     FILLER   -COPY WWDIST20     -RED TEST-IDDISTR.                    
023400*01     FILLER   -COPY WWDIST35     -RED TEST-IDDISTR.                    
023500*01     FILLER   -COPY WWDIST79     -RED TEST-IDDISTR.                    
023600*    ----DISTR-DEALER-PRICE----                                           
023700*01    -COPY WWDC99                                                       
023800     EJECT                                                                
023900 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
024000*   -COPY WWDIST57                                                        
024100     EJECT                                                                
024200 01     FILLER                  PIC X(10)   VALUE 'SPAR-AREOR'.           
024300 01     SPAR-AREOR.                                                       
024400   03   SPAR-AREA.                                                        
024500     05 SPAR-KVORDRAD-PACK      PIC  S9(5)        VALUE ZERO.             
024600     05 SPAR-VKORDNTO           PIC  9(6)V9(4)    VALUE ZERO.             
024700     05 SPAR-VLORDNTO           PIC  9(4)V9(7)    VALUE ZERO.             
024800     05 SPAR-SUORDV             PIC  S9(9)V9(2)   VALUE ZERO.             
024900     05 SPAR-SUORDV-LOC         PIC  S9(9)V9(2)   VALUE ZERO.             
025000     05 SPAR-SUORDV-LOCPREL     PIC  S9(9)V9(2)   VALUE ZERO.             
025100     05 SPAR-SUORDV-EXP         PIC  S9(9)V9(2)   VALUE ZERO.             
025200 SKIP1                                                                    
025300 01     FILLER                  PIC X(09)   VALUE 'ACK-AREOR'.            
025400 01     ACKUMULATOR-AREOR.                                                
025500   03   ACK-AREA.                                                         
025600     05 ACK-KVORDRAD-PACK   PIC  S9(5)      COMP-3  VALUE ZERO.           
025700     05 ACK-VKORDNTO        PIC  9(6)V9(4)  COMP-3  VALUE ZERO.           
025800     05 ACK-VLORDNTO        PIC  9(4)V9(7)  COMP-3  VALUE ZERO.           
025900     05 ACK-SUORDV          PIC  S9(9)V9(2) COMP-3  VALUE ZERO.           
026000     05 ACK-SUORDV-EXP      PIC  S9(9)V9(2) COMP-3  VALUE ZERO.           
026100     05 ACK-SUORDV-LOC      PIC  S9(9)V9(2) COMP-3  VALUE ZERO.           
026200     05 ACK-SUORDV-LOCPREL  PIC  S9(9)V9(2) COMP-3  VALUE ZERO.           
026300     05 ACK-SUORDV-LEVPL    PIC  S9(9)V9(2) COMP-3  VALUE ZERO.           
026400     05 ACK-SUORDV-LEVPL-LOC     PIC  S9(9)V9(2) COMP-3                   
026500                                                    VALUE ZERO.           
026600     05 ACK-SUORDV-LEVPL-LOCPREL PIC  S9(9)V9(2) COMP-3                   
026700                                                    VALUE ZERO.           
026800     EJECT                                                                
026900 01    NYCKLAR-TILL-DLI.                                                  
027000*                                                                         
027100   03    W-WDE401KY-X.                                                    
027200     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
027300     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
027400     05    W-401-IDKUNDRF.                                                
027500       07  W-401-IDORDNR         PIC 9(5)    VALUE ZERO.                  
027600       07  FILLER                PIC X(5)    VALUE SPACE.                 
027700     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
027800     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
027900*                                                                         
028000   03    W-IDPLKLST-X.                                                    
028100     05    W-IDPLKLST            PIC S9(3)   VALUE ZERO  COMP-3.          
028200*                                                                         
028300   03    W-WDE411-IDPURAD-X.                                              
028400     05    W-WDE411-IDPURAD      PIC S9(5)   VALUE ZERO  COMP-3.          
028500*                                                                         
028600   03    W-IDPURAD-MIN-X.                                                 
028700     05    W-IDPURAD-MIN         PIC S9(5)   VALUE ZERO  COMP-3.          
028800   03    W-IDPURAD-MAX-X.                                                 
028900     05    W-IDPURAD-MAX         PIC S9(5)   VALUE +9999 COMP-3.          
029000                                                                          
029100   03  W-WDE4E1KY-MIN.                                                    
029200       05  W-IDPRODNR-WDE4E-MIN      PIC S9(7) COMP-3 VALUE ZERO.         
029300       05  W-IDDISTR-WDE4E-MIN       PIC S9(5) COMP-3 VALUE ZERO.         
029400       05  FILLER-MIN                PIC X(16) VALUE LOW-VALUE.           
029500                                                                          
029600   03  W-WDE4E1KY-MAX.                                                    
029700       05  W-IDPRODNR-WDE4E-MAX      PIC S9(7) COMP-3 VALUE ZERO.         
029800       05  W-IDDISTR-WDE4E-MAX       PIC S9(5) COMP-3 VALUE 99999.        
029900       05  FILLER-MAX                PIC X(16) VALUE HIGH-VALUE.          
030000                                                                          
030100     03  W-WDE4F1KY-MIN-X.                                                
030200         05  W-IDPRODNR-MIN      PIC S9(7)   COMP-3 VALUE ZERO.           
030300         05  W-IDKOLLI-MIN       PIC S9(5)   COMP-3 VALUE ZERO.           
030400         05  FILLER              PIC X(22)   VALUE  LOW-VALUE.            
030500                                                                          
030600     03  W-WDE4F1KY-MAX-X.                                                
030700         05  W-IDPRODNR-MAX      PIC S9(7)   COMP-3 VALUE ZERO.           
030800         05  W-IDKOLLI-MAX       PIC S9(5)   COMP-3 VALUE ZERO.           
030900         05  FILLER              PIC X(22)   VALUE  HIGH-VALUE.           
031000                                                                          
031100     03  W-IDKOLLI-E6-X.                                                  
031200         05  W-IDKOLLI-E6        PIC S9(5)   COMP-3 VALUE ZERO.           
031300                                                                          
031400     03  W-IDPRODNR-E6-X.                                                 
031500         05  W-IDPRODNR-E6       PIC S9(7)   COMP-3 VALUE ZERO.           
031600                                                                          
031700     03  W-KDKOLSTA-X.                                                    
031800         05  W-KDKOLSTA          PIC S9(1)   COMP-3 VALUE +0.             
031900*                                                                         
032000   03    W-WDA501KY-X.                                                    
032100     05    W-501-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
032200     05    W-501-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
032300     05    W-501-IDKUNDRF.                                                
032400       07  W-501-IDORDNR         PIC 9(5)    VALUE ZERO.                  
032500       07  FILLER                PIC X(5)    VALUE SPACE.                 
032600     05    W-501-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.          
032700     05    W-501-IDLOPNR         PIC S9(3)   VALUE ZERO  COMP-3.          
032800*                                                                         
032900   03    W-WDK901-IDARTNR-X.                                              
033000     05    W-WDK901-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
033100*                                                                         
033200     03  W-IDORDER-X.                                                     
033300         05  W-IDORDER-Q2        PIC S9(7)   COMP-3.                      
033400*                                                                         
033500   03    W-WDQ301-KEY-X.                                                  
033600     05    W-WDQ301-IDORDER      PIC S9(7)   VALUE ZERO  COMP-3.          
033700     05    W-WDQ301-IDDC         PIC X(2)    VALUE SPACE.                 
033800     05    W-WDQ301-IDPRODNR     PIC S9(7)   VALUE ZERO  COMP-3.          
033900     05    W-WDQ301-IDPLKLST     PIC S9(3)   VALUE ZERO  COMP-3.          
034000                                                                          
034100     03  W-Q301-KEY-MIN-X.                                                
034200         05  W-Q301-MIN-IDORDER  PIC S9(7)   COMP-3.                      
034300         05  W-Q301-MIN-IDDC     PIC X(2).                                
034400         05  W-Q301-MIN-IDPRODNR PIC S9(7)   COMP-3.                      
034500         05  FILLER              PIC X(2)    VALUE LOW-VALUE.             
034600                                                                          
034700     03  W-Q301-KEY-MAX-X.                                                
034800         05  W-Q301-MAX-IDORDER  PIC S9(7)   COMP-3.                      
034900         05  W-Q301-MAX-IDDC     PIC X(2).                                
035000         05  W-Q301-MAX-IDPRODNR PIC S9(7)   COMP-3.                      
035100         05  FILLER              PIC X(2)    VALUE HIGH-VALUE.            
035200*                                                                         
035300   03    W-WDGX-4505-KEY-X.                                               
035400     05    W-IDHTYP-4505         PIC X(04) VALUE '4505'.                  
035500     05    W-4505-IDDC           PIC X(2).                                
035600     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
035700*                                                                         
035800   03    W-4305-X.                                                        
035900     05    FILLER                PIC X(4) VALUE '4305'.                   
036000     05    W-XXDJ-IDDC           PIC X(2).                                
036100     05    FILLER                PIC X(24) VALUE LOW-VALUE.               
036200*                                                                         
036300   03    W-4306-X.                                                        
036400     05    W-XXDJ-IDPRODNR       PIC S9(7) COMP-3.                        
036500     05    FILLER                PIC X(6)  VALUE LOW-VALUE.               
036600*                                                                         
036700   03    W-4308-X.                                                        
036800     05    W-XXDJ-IDRADNR-TOM    PIC S9(5) COMP-3.                        
036900     05    FILLER                PIC X(7)  VALUE LOW-VALUE.               
037000*                                                                         
037100   03    W-4726-X.                                                        
037200     05    FILLER                PIC X(4) VALUE '4726'.                   
037300     05    W-XXDV-FLBATCH        PIC X(1) VALUE SPACE.                    
037400     05    FILLER                PIC X(25) VALUE LOW-VALUE.               
037500*                                                                         
037600   03    W-WDK601-IDARTNR-X.                                              
037700     05    W-WDK601-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
037800*                                                                         
037900   03    W-WDK611-KDSEGKEY-X.                                             
038000     05    W-WDK611-KDSEGKEY     PIC X       VALUE '1'.                   
038100*                                                                         
038200   03    W-WDK701-IDARTNR-X.                                              
038300     05    W-WDK701-IDARTNR      PIC S9(9)   VALUE ZERO  COMP-3.          
038400*                                                                         
038500   03    W-WDK711-IDDC-X.                                                 
038600     05    W-WDK711-IDDC         PIC X(2)    VALUE SPACE.                 
038700*                                                                         
038800   03    W-WDGX11-WDGXKEY-X.                                              
038900     05    W-RDG-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
039000     05    W-RDG-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
039100     05    W-RDG-IDDC            PIC X(2)    VALUE SPACE.                 
039200     05    W-RDG-KDFAKTYP        PIC X(1)    VALUE SPACE.                 
039300*                                                                         
039400   03    W-WDM201-X.                                                      
039500     05    W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.            
039600     05    W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                  
039700                                                                          
039800   03    W-WDM211-X.                                                      
039900     05    W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.            
040000                                                                          
040100   03    W-WDM221-X.                                                      
040200     05    W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.             
040300     05    W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.             
040400     05    W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.             
040500     05    W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.             
040600                                                                          
040700   03    W-WDA601KY-MIN-X.                                                
040800     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
040900     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
041000     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
041100     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
041200     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
041300     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
041400     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
041500     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
041600     SKIP2                                                                
041700   03    W-WDA601KY-MAX-X.                                                
041800     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
041900     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
042000     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
042100     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
042200     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
042300     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
042400     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
042500     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
042600  03    W-IDKUNDRF-RED.                                                   
042700     05    W-IDORDNR                 PIC 9(07) VALUE ZERO.                
042800     05    FILLER                    PIC X(03) VALUE SPACE.               
042900*                                                                         
043000*  03    -COPY WDGX01                                                     
043100                                                                          
043200   03  W-IDDC-B6-X.                                                       
043300     05 W-IDDC-B6                    PIC X(2).                            
043400     EJECT                                                                
043500                                                                          
043600 01    MEDDELANDE.                                                        
043700   03    W-RAETT-1.                                                       
043800     05  FILLER                  PIC X(40)   VALUE                        
043900        'ANNULLATION UTFÖRD                      '.                       
044000     05  FILLER                  PIC X(40)   VALUE                        
044100        'CANCELLATION OK                         '.                       
044200   03    FILLER  REDEFINES  W-RAETT-1.                                    
044300     05  RAETT-1     OCCURS 2    PIC X(40).                               
044400*                                                                         
044500   03    FEL-1.                                                           
044600     05  FILLER                  PIC X(40)   VALUE                        
044700        '701 ORDERN SAKNAS                       '.                       
044800     05  FILLER                  PIC X(40)   VALUE                        
044900        '701 ORDER MISSING                       '.                       
045000   03    FILLER  REDEFINES  FEL-1.                                        
045100     05  FEL-701     OCCURS 2    PIC X(40).                               
045200*                                                                         
045300   03    FEL-2.                                                           
045400     05  FILLER                  PIC X(40)   VALUE                        
045500        '710 ORDERN FÄRDIGPACKAD                 '.                       
045600     05  FILLER                  PIC X(40)   VALUE                        
045700        '710 ORDER ALREADY PACKED                '.                       
045800   03    FILLER  REDEFINES  FEL-2.                                        
045900     05  FEL-710     OCCURS 2    PIC X(40).                               
046000*                                                                         
046100   03    FEL-4.                                                           
046200     05  FILLER                  PIC X(40)   VALUE                        
046300        '725 FEL ANTAL                           '.                       
046400     05  FILLER                  PIC X(40)   VALUE                        
046500        '725 WRONG QUANTITY                      '.                       
046600   03    FILLER  REDEFINES  FEL-4.                                        
046700     05  FEL-725     OCCURS 2    PIC X(40).                               
046800*                                                                         
046900   03    FEL-5.                                                           
047000     05  FILLER                  PIC X(40)   VALUE                        
047100        '748 UPPLYSTA FÄLT FEL                   '.                       
047200     05  FILLER                  PIC X(40)   VALUE                        
047300        '748 HIGH LIGHTED FIELDS WRONG           '.                       
047400   03    FILLER  REDEFINES  FEL-5.                                        
047500     05  FEL-748     OCCURS 2    PIC X(40).                               
047600*                                                                         
047700   03    FEL-6.                                                           
047800     05  FILLER                  PIC X(40)   VALUE                        
047900        '820 ORDERN TILLHÖR ANNAT DISTRIKT       '.                       
048000     05  FILLER                  PIC X(40)   VALUE                        
048100        '820 THE ORDER BELONGS TO ANOTHER DISTR  '.                       
048200   03    FILLER  REDEFINES  FEL-6.                                        
048300     05  FEL-820     OCCURS 2    PIC X(40).                               
048400*                                                                         
048500   03    FEL-7.                                                           
048600     05  FILLER                  PIC X(40)   VALUE                        
048700        '821 RADNUMMER SAKNAS PÅ ORDERDEL        '.                       
048800     05  FILLER                  PIC X(40)   VALUE                        
048900        '821 LINE NO. MISSING ON ORDERPART       '.                       
049000   03    FILLER  REDEFINES  FEL-7.                                        
049100     05  FEL-821     OCCURS 2    PIC X(40).                               
049200*                                                                         
049300   03    FEL-9.                                                           
049400     05  FILLER                  PIC X(40)   VALUE                        
049500        '823 PACKNING PÅBÖRJAD, ANNULL. GÅR EJ   '.                       
049600     05  FILLER                  PIC X(40)   VALUE                        
049700        '823 PACING REP. STARTED, CANCEL. NOT POS'.                       
049800   03    FILLER  REDEFINES  FEL-9.                                        
049900     05  FEL-823     OCCURS 2    PIC X(40).                               
050000*                                                                         
050100   03    FEL-10.                                                          
050200     05  FILLER                  PIC X(40)   VALUE                        
050300        '751 FELAKTIGT RADNUMMER                 '.                       
050400     05  FILLER                  PIC X(40)   VALUE                        
050500        '751 WRONG LINE NUMBER                   '.                       
050600   03    FILLER  REDEFINES  FEL-10.                                       
050700     05  FEL-751     OCCURS 2    PIC X(40).                               
050800*                                                                         
050900   03    FEL-11.                                                          
051000     05  FILLER                  PIC X(40)   VALUE                        
051100        '59X MATA IN ANNULLATIONSUPPGIFTER       '.                       
051200     05  FILLER                  PIC X(40)   VALUE                        
051300        '59X ENTER CANCELLATION INFORMATION      '.                       
051400   03    FILLER  REDEFINES  FEL-11.                                       
051500     05  FEL-59X     OCCURS 2    PIC X(40).                               
051600*                                                                         
051700   03    FEL-12.                                                          
051800     05  FILLER                  PIC X(40)   VALUE                        
051900        '59Y EJ HEL ORDER OCH INTERVALL          '.                       
052000     05  FILLER                  PIC X(40)   VALUE                        
052100        '59Y COMPL ORDER AND INTERVAL NOT ALLOWED'.                       
052200   03    FILLER  REDEFINES  FEL-12.                                       
052300     05  FEL-59Y     OCCURS 2    PIC X(40).                               
052400*                                                                         
052500   03    FEL-13.                                                          
052600     05  FILLER                  PIC X(40)   VALUE                        
052700        '749 FEL NYCKEL                          '.                       
052800     05  FILLER                  PIC X(40)   VALUE                        
052900        '749 WRONG KEY                           '.                       
053000   03    FILLER  REDEFINES  FEL-13.                                       
053100     05  FEL-749     OCCURS 2    PIC X(40).                               
053200*                                                                         
053300   03    FEL-14.                                                          
053400     05  FILLER                  PIC X(40)   VALUE                        
053500        '750 ORDERN EJ KLAR, ANNULL. GÅR EJ      '.                       
053600     05  FILLER                  PIC X(40)   VALUE                        
053700        '750 ORDER NOT FINISHED, CANCEL. NOT POS.'.                       
053800   03    FILLER  REDEFINES  FEL-14.                                       
053900     05  FEL-750     OCCURS 2    PIC X(40).                               
054000*                                                                         
054100   03    FEL-15.                                                          
054200     05  FILLER                  PIC X(40)   VALUE                        
054300        '8221 PACKN STARTAD.ALT RAD(ER) REDAN ANN'.                       
054400     05  FILLER                  PIC X(40)   VALUE                        
054500        '8221 PACKING REP STARTED OR LINES CAN.  '.                       
054600   03    FILLER  REDEFINES  FEL-15.                                       
054700     05  FEL-8221    OCCURS 2    PIC X(40).                               
054800*                                                                         
054900   03    FEL-16.                                                          
055000     05  FILLER                  PIC X(40)   VALUE                        
055100        '8222 PACKN STARTAD.ALT RAD(ER) REDAN ANN'.                       
055200     05  FILLER                  PIC X(40)   VALUE                        
055300        '8222 PACKING REP STARTED OR LINES CAN.  '.                       
055400   03    FILLER  REDEFINES  FEL-16.                                       
055500     05  FEL-8222    OCCURS 2    PIC X(40).                               
055600*                                                                         
055700   03    FEL-17.                                                          
055800     05  FILLER                  PIC X(40)   VALUE                        
055900        '8223 PACKN STARTAD. SÄTT PACKARE TILL 0.'.                       
056000     05  FILLER                  PIC X(40)   VALUE                        
056100        '8223 PACK REP STARTED. SET PACKER TO 0. '.                       
056200   03    FILLER  REDEFINES  FEL-17.                                       
056300     05  FEL-8223    OCCURS 2    PIC X(40).                               
056400*                                                                         
056500   03    FEL-18.                                                          
056600     05  FILLER                  PIC X(40)   VALUE                        
056700        'SOFTWARE ORDER                          '.                       
056800     05  FILLER                  PIC X(40)   VALUE                        
056900        'SOFTWARE ORDER                          '.                       
057000   03    FILLER  REDEFINES  FEL-18.                                       
057100     05  FEL18       OCCURS 2    PIC X(40).                               
057200*                                                                         
057300   03    FEL-19.                                                          
057400     05  FILLER                  PIC X(40)   VALUE                        
057500        '702 ORDERHUVUD SAKNAS                   '.                       
057600     05  FILLER                  PIC X(40)   VALUE                        
057700        '702 ORDER HEAD MISSING                  '.                       
057800   03    FILLER  REDEFINES  FEL-19.                                       
057900     05  FEL-702     OCCURS 2    PIC X(40).                               
058000*                                                                         
058100   03    FEL-20.                                                          
058200     05  FILLER                  PIC X(40)   VALUE                        
058300        '796 ANVÄND BILD 4245                    '.                       
058400     05  FILLER                  PIC X(40)   VALUE                        
058500        '796 USE SCREEN 4245                     '.                       
058600   03    FILLER  REDEFINES  FEL-20.                                       
058700     05  FEL-796     OCCURS 2    PIC X(40).                               
058800*                                                                         
058900   03    FEL-21.                                                          
059000     05  FILLER                  PIC X(40)   VALUE                        
059100        'PLOCKLISTA SAKNAS                       '.                       
059200     05  FILLER                  PIC X(40)   VALUE                        
059300        'OPERATION NO. MISSING                   '.                       
059400   03    FILLER  REDEFINES  FEL-21.                                       
059500     05  FEL21       OCCURS 2    PIC X(40).                               
059600*                                                                         
059700     EJECT                                                                
059800******************************************************************        
059900*                                                                *        
060000*                AREOR FÖR MFS OCH SKÄRMHANTERING                *        
060100*                                                                *        
060200******************************************************************        
060300 01    FILLER                 PIC X(16) VALUE 'MID W4I35901 MID'.         
060400     SKIP3                                                                
060500*01    MID -COPY W4I35901.                                                
060600     EJECT                                                                
060700*01    -COPY WMSGAREA                                                     
060800     EJECT                                                                
060900*  03    MOD -COPY W4O35901.                                              
061000     EJECT                                                                
061100 01    FILLER              PIC X(16)   VALUE 'M0D W40636I1 M0D'.          
061200*01  -COPY W40636I1   -PRE MOD4636-                                       
061300     EJECT                                                                
061400 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
061500     SKIP3                                                                
061600*01    -COPY WMFSAREA                                                     
061700     EJECT                                                                
061800******************************************************************        
061900*                                                                         
062000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
062100*                                                                         
062200 01    IMS-WS.                                                            
062300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
062400     SKIP3                                                                
062500*                        **** STATUS-KOD FRÅN IMS                         
062600   03    STATUS-WS-E411          PIC XX.                                  
062700     88    SEGMENT-SAKNAS-E411               VALUE 'GE'.                  
062800   03    STATUS-WS               PIC XX.                                  
062900     88    SEGMENT-FINNS                     VALUE '  '.                  
063000     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
063100     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
063200     88    BASEN-SLUT                        VALUE 'GB'.                  
063300     SKIP3                                                                
063400   03    GODK-STATUSKODER.                                                
063500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
063600     SKIP3                                                                
063700 01    SSA1                      PIC X(200).                              
063800 01    SSA2                      PIC X(128).                              
063900 01    SSA3                      PIC X(128).                              
064000 01    SSA4                      PIC X(128).                              
064100     EJECT                                                                
064200 01 GENERELLA-SUBPROGRAM.                                                 
064300    03  WMEDKONV                 PIC X(8)   VALUE 'WMEDKONV'.             
064400    03 WDATKONV                  PIC X(8)   VALUE 'WDATKONV'.             
064500    03 CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
064600    03 FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
064700    03 ABEND                     PIC X(8)   VALUE 'ABEND   '.             
064800    03 W005INIT                  PIC X(8)   VALUE 'W005INIT'.             
064900    03 W411DNOT                  PIC X(8)   VALUE 'W411DNOT'.             
065000     SKIP3                                                                
065100*        DATA TILL DEL NOTE NDC                                           
065200     SKIP3                                                                
065300     EJECT                                                                
065400 01  FILLER                      PIC X(08)  VALUE 'W411DNOT'.             
065500*01 -COPY W411DNOT                                                        
065600     EJECT                                                                
065700*01    -COPY WDATAREA                                                     
065800     EJECT                                                                
065900*                                                                         
066000*                                                                         
066100 01  FILLER                     PIC X(16)   VALUE 'W005INIT '.            
066200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
066300*01 -COPY WMSGINIT                                                        
066400     EJECT                                                                
066500     SKIP3                                                                
066600*                            DB2 FUNKTIONSKODER                           
066700 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
066800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
066900                                                                          
067000 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
067100 01  DB2-WS.                                                              
067200     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
067300         88  CURSOR-OK                       VALUE 000.                   
067400         88  RADER-FINNS                     VALUE 000.                   
067500         88  RADER-SAKNAS                    VALUE 100.                   
067600         88  ATKOMST-FEL                     VALUE 904.                   
067700     03  GODK-SQLCODEKODER.                                               
067800         05  GODK-SQLCODE OCCURS 5                                        
067900             INDEXED BY SQLCODE-IX PIC 9(3).                              
068000 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
068100     EJECT                                                                
068200*                            IMS FUNKTIONSKODER                           
068300*01    -COPY W0003                                                        
068400   03    ROLB                PIC X(4)    VALUE 'ROLB'.                    
068500     EJECT                                                                
068600*                            DLI INPUT-OUTPUT AREA                        
068700 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA'.           
068800 01    DLI-IO-AREA.                                                       
068900   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
069000     SKIP3                                                                
069100*  03    WLXXDJ01 -COPY WDGX4305           -RED IO-AREA.                  
069200     EJECT                                                                
069300*  03    WLXXDJ11 -COPY WDGX4306           -RED IO-AREA.                  
069400     EJECT                                                                
069500*  03    WLXXDJ21 -COPY WDGX4308           -RED IO-AREA.                  
069600     EJECT                                                                
069700*  03    WLARTC01 -COPY WDK601             -RED IO-AREA.                  
069800     EJECT                                                                
069900*  03    WLARTC11 -COPY WDK611             -RED IO-AREA.                  
070000     EJECT                                                                
070100*  03    WLARTS11 -COPY WDK711             -RED IO-AREA.                  
070200     EJECT                                                                
070300 01  FILLER         PIC X(16) VALUE 'DLI-IO-E4E1'.                        
070400 01  DLI-IO-E4E1.                                                         
070500*    03  -COPY WDE4E1                                                     
070600     EJECT                                                                
070700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE4F1'.         
070800 01  DLI-IO-WDE4F1.                                                       
070900*    03  -COPY WDE4F1                                                     
071000     EJECT                                                                
071100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E401'.           
071200 01  DLI-IO-E401.                                                         
071300*  03  -COPY WDE401                                                       
071400     EJECT                                                                
071500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E411'.           
071600 01  DLI-IO-E411.                                                         
071700*  03  -COPY WDE411                                                       
071800     EJECT                                                                
071900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E421'.           
072000 01  DLI-IO-E421.                                                         
072100*  03  -COPY WDE421                                                       
072200     EJECT                                                                
072300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E601'.           
072400 01  DLI-IO-E601.                                                         
072500*  03  -COPY WDE601                                                       
072600     EJECT                                                                
072700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE611'.         
072800 01  DLI-IO-WDE611.                                                       
072900*    03  -COPY WDE611                                                     
073000     EJECT                                                                
073100 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
073200*01  WLLOGA01  -COPY WDL901                                               
073300     EJECT                                                                
073400*                            DLI INPUT-OUTPUT AREA2                       
073500 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA2'.          
073600 01    DLI-IO-AREA2.                                                      
073700   03    IO-AREA2                PIC X(300)  VALUE SPACE.                 
073800     SKIP3                                                                
073900     EJECT                                                                
074000*  HTR FÖR AUTOMATFAKTURERING                                             
074100*  03    WDGX4726 -COPY WDGX4726  -PRE 4726-  -RED IO-AREA2.              
074200     SKIP2                                                                
074300*  BARN TILL 4726                                                         
074400*  03    WDGX4727 -COPY WDGX4727  -PRE 4727-  -RED IO-AREA2.              
074500     EJECT                                                                
074600*                            DLI INPUT-OUTPUT AREA3                       
074700 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA3'.          
074800 01    DLI-IO-AREA3.                                                      
074900   03    IO-AREA3                PIC X(200)  VALUE SPACE.                 
075000     SKIP3                                                                
075100*  03    WDGZ01   -COPY WDGZ01  -PRE LOGG- -RED IO-AREA3.                 
075200     EJECT                                                                
075300*                            DLI INPUT-OUTPUT AREA4                       
075400 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA4'.          
075500 01    DLI-IO-AREA4.                                                      
075600   03    IO-AREA4                PIC X(400)  VALUE SPACE.                 
075700     SKIP3                                                                
075800*  03    WLORDP01 -COPY WDA501             -RED IO-AREA4.                 
075900     EJECT                                                                
076000*                            DLI INPUT-OUTPUT AREA5                       
076100 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA5'.          
076200 01    DLI-IO-AREA5.                                                      
076300   03    IO-AREA5                PIC X(50)  VALUE SPACE.                  
076400     SKIP3                                                                
076500*  03    WL450501 -COPY WDGX01    -PRE 4505- -RED IO-AREA5.               
076600     EJECT                                                                
076700*  03    WL450511 -COPY WDGX4506              -RED IO-AREA5.              
076800     EJECT                                                                
076900 01    KOLL-IDDISTR              PIC 9(5)      COMP-3.                    
077000*01      FILLER   -COPY WWDIST19         -RED KOLL-IDDISTR.               
077100     EJECT                                                                
077200*                            DLI INPUT-OUTPUT AREA6                       
077300 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA6'.          
077400 01    DLI-IO-AREA6.                                                      
077500*  03    WLORQM01 -COPY WDQ101                                            
077600     EJECT                                                                
077700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
077800 01  DLI-IO-WDM211.                                                       
077900*    03 -COPY WDM211                                                      
078000     EJECT                                                                
078100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
078200 01  DLI-IO-WDM221.                                                       
078300*    03 -COPY WDM221                                                      
078400     EJECT                                                                
078500*                            DLI INPUT-OUTPUT AREA8                       
078600 01    FILLER                    PIC X(16) VALUE 'DLI-IO-AREA8'.          
078700 01    DLI-IO-AREA8.                                                      
078800   03    IO-AREA8                PIC X(100) VALUE SPACE.                  
078900     SKIP3                                                                
079000*  03    WLARTM01 -COPY WDK901 -PRE ARTM-  -RED IO-AREA8.                 
079100     EJECT                                                                
079200 01    FILLER                 PIC X(17) VALUE 'ANNULLATIONSTRANS'.        
079300     SKIP3                                                                
079400*01      WDGZRY5  -COPY WDGZRY5.                                          
079500     EJECT                                                                
079600     SKIP3                                                                
079700*01      WDGZRY5  -COPY WDGZRY5S.                                         
079800     EJECT                                                                
079900 01    FILLER                 PIC X(17) VALUE 'KLARTRANS-SV4    '.        
080000     SKIP3                                                                
080100*01      WDGZRY6  -COPY WDGZRY6.                                          
080200     EJECT                                                                
080300     SKIP3                                                                
080400*01      WDGZRZD  -COPY WDGZRZD.                                          
080500     EJECT                                                                
080600 01    FILLER                    PIC X(16) VALUE 'DLI-IO-WDQ201'.         
080700 01    DLI-IO-WDQ201.                                                     
080800*  03    WLORQI01 -COPY WDQ201                                            
080900     EJECT                                                                
081000                                                                          
081100 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ORQA'.             
081200 01  DLI-IO-AREA-ORQA.                                                    
081300*    03  WLORQA01   -COPY WDQ301                                          
081400     EJECT                                                                
081500 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
081600 01  DLI-IO-AREA-WDA6.                                                    
081700*  03    -COPY WDA601                                                     
081800     EJECT                                                                
081900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
082000 01   DLI-IO-AREA-B601.                                                   
082100*     03  -COPY WDB601                                                    
082200                                                                          
082300                                                                          
082400 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
082500 01    4397-TRANSAREA.                                                    
082600   03    4397-IDPRODNR           PIC 9(7)    VALUE ZERO.                  
082700   03    4397-IDANSTNR           PIC 9(5)    VALUE ZERO.                  
082800   03    4397-IDPLKLST           PIC 9(3)    VALUE ZERO.                  
082900   03    4397-IDPURAD            PIC 9(5)    VALUE ZERO.                  
083000   03    FILLER                  PIC X(80)   VALUE SPACE.                 
083100     EJECT                                                                
083200 01    FILLER                    PIC X(16)   VALUE '4397-IO-AREA'.        
083300 01    4397-IO-AREA.                                                      
083400   03    4397-LL                 PIC S9(4)   COMP  SYNC.                  
083500   03    4397-Z1                 PIC X(1).                                
083600   03    4397-Z2                 PIC X(1).                                
083700   03    4397-TRANSKOD           PIC X(8)    VALUE SPACE.                 
083800   03    4397-IDTRANS            PIC X(4)    VALUE SPACE.                 
083900   03    4397-KDMFSFOR           PIC X(1)    VALUE SPACE.                 
084000   03    4397-AREA               PIC X(100)  VALUE SPACE.                 
084100     EJECT                                                                
084200*    MSG-AREA FÖR HOPP TILL W20109                                        
084300 01  FILLER            PIC X(16)   VALUE '2109-MSG-IO-AREA'.              
084400 01  W-PROG-TO-PROG-SW-1.                                                 
084500     03  2109-KVLL                 PIC S9(4) COMP SYNC.                   
084600     03  2109-Z1                   PIC X.                                 
084700     03  2109-Z2                   PIC X.                                 
084800     03  2109-TRANSKOD             PIC X(8)  VALUE 'W2T109X '.            
084900     03  2109-IDTRANS              PIC X(4)  VALUE '4359'.                
085000     03  2109-KDMFSFOR             PIC X.                                 
085100*    03  -COPY W2I10902    -PRE 2109-                                     
085200     EJECT                                                                
085300 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
085400                                                                          
085500*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
085600     EJECT                                                                
085700     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
085800     EJECT                                                                
085900 LINKAGE SECTION.                                                         
086000*01    -COPY W0009     -PRE MSG-                                          
086100     EJECT                                                                
086200*01    -COPY W0009     -PRE ALT-                                          
086300     EJECT                                                                
086400*01    -COPY W0009     -PRE 4397-                                         
086500     EJECT                                                                
086600*01    -COPY W0009     -PRE 2109-                                         
086700     EJECT                                                                
086800*01    -COPY W0009     -PRE USEA-                                         
086900     EJECT                                                                
087000*01    -COPY W0008     -PRE WDE4-                                         
087100     05  FILLER                  PIC X.                                   
087200     EJECT                                                                
087300 01    -COPY W0008     -PRE WDE4E-                                        
087400     05  FILLER                  PIC X.                                   
087500     EJECT                                                                
087600 01    -COPY W0008     -PRE WDE4F-                                        
087700     05  FILLER                  PIC X.                                   
087800     EJECT                                                                
087900*01    -COPY W0008     -PRE WDE6-                                         
088000     05  FILLER                  PIC X.                                   
088100     EJECT                                                                
088200*01    -COPY W0008     -PRE ZZAC-                                         
088300     05  FILLER                  PIC X.                                   
088400     EJECT                                                                
088500*01    -COPY W0008     -PRE ORQI-                                         
088600     05  FILLER                  PIC X.                                   
088700     EJECT                                                                
088800*01    -COPY W0008     -PRE ARTC-                                         
088900     05  FILLER                  PIC X.                                   
089000     EJECT                                                                
089100*01    -COPY W0008     -PRE XXDJ-                                         
089200     05  FILLER                  PIC X.                                   
089300     EJECT                                                                
089400*01    -COPY W0008     -PRE XXDV-                                         
089500     05  FILLER                  PIC X.                                   
089600     EJECT                                                                
089700*01    -COPY W0008     -PRE 4505-                                         
089800     05  FILLER                  PIC X.                                   
089900     EJECT                                                                
090000*01    -COPY W0008     -PRE ORDP-                                         
090100     05  FILLER                  PIC X.                                   
090200     EJECT                                                                
090300*01    -COPY W0008     -PRE ORQM-                                         
090400     05  FILLER                  PIC X.                                   
090500     EJECT                                                                
090600*01    -COPY W0008     -PRE ARTM-                                         
090700     05  FILLER                  PIC X.                                   
090800     EJECT                                                                
090900*01    -COPY W0008     -PRE WDM2-                                         
091000     05  FILLER                  PIC X.                                   
091100     EJECT                                                                
091200*01    -COPY W0008     -PRE ORQA-                                         
091300     05  FILLER                  PIC X.                                   
091400     EJECT                                                                
091500*01    -COPY W0008     -PRE ARTS-                                         
091600     05  FILLER                  PIC X.                                   
091700     EJECT                                                                
091800*01    -COPY W0008     -PRE WLLOGA-                                       
091900     05  FILLER                  PIC X.                                   
092000     EJECT                                                                
092100*01    -COPY W0008     -PRE WDA6B-                                        
092200     05  FILLER                  PIC X.                                   
092300     EJECT                                                                
092400*01    -COPY W0008     -PRE WDB6-                                         
092500     05  FILLER                  PIC X.                                   
092600     EJECT                                                                
092700 01  DNOT-ORQP-PCB               PIC X.                                   
092800 01  DNOT-ORQP2-PCB              PIC X.                                   
092900 01  DNOT-ORQP3-PCB              PIC X.                                   
093000 01  DNOT-4013-PCB               PIC X.                                   
093100 01  DNOT-BENA-PCB               PIC X.                                   
093200 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB     4397-PCB  2109-PCB         
093300                          USEA-PCB                                        
093400                          WDE4-PCB WDE4E-PCB WDE4F-PCB                    
093500                          WDE6-PCB  ZZAC-PCB                              
093600                                   ORQI-PCB                               
093700                                   ARTC-PCB    XXDJ-PCB  XXDV-PCB         
093800                                   4505-PCB    ORDP-PCB                   
093900                                   ORQM-PCB    ARTM-PCB                   
094000                                   WDM2-PCB    ORQA-PCB                   
094100                                   ARTS-PCB WLLOGA-PCB WDA6B-PCB          
094200                                   WDB6-PCB                               
094300                                   DNOT-ORQP-PCB DNOT-ORQP2-PCB           
094400                                   DNOT-ORQP3-PCB DNOT-4013-PCB           
094500                                   DNOT-BENA-PCB.                         
094600                                                                          
094700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB     4397-PCB  2109-PCB         
094800                          USEA-PCB                                        
094900                          WDE4-PCB WDE4E-PCB WDE4F-PCB                    
095000                          WDE6-PCB  ZZAC-PCB                              
095100                                   ORQI-PCB                               
095200                                   ARTC-PCB    XXDJ-PCB  XXDV-PCB         
095300                                   4505-PCB    ORDP-PCB                   
095400                                   ORQM-PCB    ARTM-PCB                   
095500                                   WDM2-PCB    ORQA-PCB                   
095600                                   ARTS-PCB WLLOGA-PCB WDA6B-PCB          
095700                                   WDB6-PCB                               
095800                                   DNOT-ORQP-PCB DNOT-ORQP2-PCB           
095900                                   DNOT-ORQP3-PCB DNOT-4013-PCB           
096000                                   DNOT-BENA-PCB.                         
096100*                                                                         
096200     PERFORM IMS-GET-MSG                                                  
096300     IF SEGMENT-FINNS                                                     
096400         PERFORM A-INIT                                                   
096500         IF EGEN-MID                                                      
096600             PERFORM B-GENERELL-KONTROLL                                  
096700*                                                                         
096800             IF WS-INDATA-RATT                                            
096900                 PERFORM C-LAGG-UT-ORDER-INFO                             
097000                 IF GAMLA-NYCKLAR AND WS-INDATA-RATT                      
097100                     PERFORM D-BACKOUT-DEF-CASE                           
097200                     PERFORM E-KOLLA-INFAELT                              
097300                     IF WS-BEHANDLING-RATT                                
097400                         MOVE MFS-ROER-EJ-FAELT TO                        
097500                              MOD-KVORDRAD-PACK                           
097600                         PERFORM D-BEHANDLA-ANNULLERING                   
097700                         PERFORM S02-RENSA-FALT                           
097800                     END-IF                                               
097900                 ELSE                                                     
098000                     MOVE MFS-RENSA-FAELT TO MOD-FLSVAR                   
098100                                             MOD-IDRADNR-FOM              
098200                                             MOD-IDRADNR-TOM              
098300                                             MOD-KVANNANT                 
098400                 END-IF                                                   
098500             END-IF                                                       
098600         ELSE                                                             
098700             IF NOT GODK-MID                                              
098800                 PERFORM F-RENSA-NYCKLAR                                  
098900             END-IF                                                       
099000             MOVE '4359'         TO MFS-IDTRANS                           
099100                                    MOD-IDTRANS                           
099200             MOVE MAX-MOD-LAENGD TO MSG-KVLL                              
099300             MOVE 'W4O35901'     TO MFS-IDMOD                             
099400             MOVE MOD-W4O35901   TO MSG-AREA                              
099500             PERFORM IMS-INSERT-MSG                                       
099600         END-IF                                                           
099700         PERFORM H-AVSLUT                                                 
099800         IF EGEN-MID                                                      
099900             IF INX = MAX-RAD-ANTAL                                       
100000                 MOVE '4359'             TO MSG-IDTRANS-1                 
100100                 MOVE 'W4T359  '         TO MSG-KDTRANS-1                 
100200                 MOVE WS-KDMFSFOR        TO MSG-KDMFSFOR-1                
100300                 PERFORM S03-LADDA-4359-MID                               
100400                 COMPUTE MSG-KVLL = LENGTH OF MID-W4I35901 + 17           
100500                 MOVE MID-W4I35901 TO MSG-INDATA-MINUS-1-TRANSKOD         
100600                 PERFORM IMS-CHANGE-ALTMSG                                
100700                 PERFORM IMS-INSERT-ALTMSG                                
100800             ELSE                                                         
100900                 MOVE MAX-MOD-LAENGD     TO MSG-KVLL                      
101000                 MOVE MOD-W4O35901       TO MSG-AREA                      
101100                 PERFORM IMS-INSERT-MSG                                   
101200             END-IF                                                       
101300         END-IF                                                           
101400         IF 2109-MID2-KVANTART > ZERO                                     
101500           PERFORM S12CA-STARTA-2109                                      
101600         END-IF                                                           
101700     END-IF                                                               
101800     MOVE ZERO TO RETURN-CODE                                             
101900     GOBACK                                                               
102000     .                                                                    
102100     EJECT                                                                
102200 A-INIT             SECTION.                                              
102300                                                                          
102400     IF MSG-DUBBLA-TRANSKODER                                             
102500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I35901                 
102600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
102700       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
102800                                             WS-KDMFSFOR                  
102900                                             2109-KDMFSFOR                
103000       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
103100     ELSE                                                                 
103200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I35901                 
103300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
103400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
103500                                             WS-KDMFSFOR                  
103600                                             2109-KDMFSFOR                
103700       MOVE ' '                           TO MFS-KDTRTYP                  
103800     END-IF                                                               
103900*                                                                         
104000     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
104100     MOVE LOW-VALUE                       TO MSG-AREA                     
104200                                             MOD-W4O35901                 
104300     MOVE 'W4O359N1'                      TO MFS-IDMOD                    
104400     MOVE '4359'                          TO MOD-IDTRANS                  
104500*                                                                         
104600     IF ENGLISH-TEXT                                                      
104700         MOVE +2                          TO INDX                         
104800     ELSE                                                                 
104900         MOVE +1                          TO INDX                         
105000     END-IF                                                               
105100     MOVE NEJ                             TO WS-FLAUTFAK                  
105200                                             WS-LASNINGSTRANS             
105300     MOVE RAETT                           TO WS-INDATA-TEST               
105400                                             WS-BEHANDLING-TEST           
105500     MOVE ZERO                            TO LOGG-IDLOGLOP                
105600*                                                                         
105700     PERFORM AA-FLYTTA-NYCKLAR                                            
105800*                                                                         
105900     ACCEPT WS-DAGENS-DATUM               FROM DATE                       
106000     ACCEPT WS-KLOCKAN                    FROM TIME                       
106100                                                                          
106200     ACCEPT WS-TINUDAT FROM DATE                                          
106300     ACCEPT WS-TINUTID FROM TIME                                          
106400                                                                          
106500     MOVE WS-TIHHMMSS         TO WS-TISKPTID                              
106600                                                                          
106700     IF DCS-NDC OR                                                        
106800       (DCS-SDC AND DCS-ENGLAND)                                          
106900       MOVE ALL '+'           TO MSGI-WMSGINIT                            
107000       MOVE '011'             TO MSGI-KDCALL                              
107100       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
107200                                 MSGI-IDLTERM-USER                        
107300       MOVE WS-DAGENS-DATUM   TO MSGI-TILOKDAT                            
107400       MOVE WS-TIHHMMSS(1:4)  TO MSGI-TILOKTID                            
107500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
107600       MOVE MSGI-TILOKDAT     TO WS-DAGENS-DATUM                          
107700       MOVE MSGI-TILOKTID     TO WS-TIHHMMSS (1:4)                        
107800     END-IF                                                               
107900                                                                          
108000     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
108100                                             MOD-IDDISTR-IN               
108200                                             MOD-IDPRODNR-IN              
108300                                             MOD-IDPLKLST-IN              
108400                                             MOD-IDRADNR-IN               
108500                                             MOD-TEMFSINF                 
108600                                             MOD-FLSVAR                   
108700                                             MOD-IDRADNR-FOM              
108800                                             MOD-IDRADNR-TOM              
108900                                             MOD-KVANNANT                 
109000     IF MID-IDPLKLST-START = ALL '+'   AND                                
109100        MID-IDRADNR-START  = ALL '+'                                      
109200       MOVE NEJ                          TO FL-4359-OMSTARTAD             
109300     ELSE                                                                 
109400       MOVE JA                           TO FL-4359-OMSTARTAD             
109500     END-IF                                                               
109600                                                                          
109700     MOVE +1             TO 2109-IX                                       
109800     MOVE SPACE          TO 2109-MID2-W2I10902                            
109900     .                                                                    
110000     EJECT                                                                
110100 AA-FLYTTA-NYCKLAR  SECTION.                                              
110200                                                                          
110300     MOVE NEJ                             TO FL-NYCKLAR                   
110400                                                                          
110500     IF MID-IDPRODNR-IN = ALL '+'                                         
110600         MOVE MID-IDPRODNR-UT             TO WS-IDPRODNR                  
110700         INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO              
110800     ELSE                                                                 
110900         MOVE MID-IDPRODNR-IN             TO WS-IDPRODNR                  
111000         MOVE JA                          TO FL-NYCKLAR                   
111100     END-IF                                                               
111200                                                                          
111300     IF MID-IDDISTR-IN = ALL '+'                                          
111400         MOVE MID-IDDISTR-UT              TO WS-IDDISTR                   
111500         INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO               
111600         MOVE WS-IDDISTR                  TO WS-IDDISTR-NUM               
111700     ELSE                                                                 
111800         MOVE MID-IDDISTR-IN              TO WS-IDDISTR                   
111900         MOVE JA                          TO FL-NYCKLAR                   
112000     END-IF                                                               
112100                                                                          
112200     IF MID-IDPLKLST-IN = ALL '+'                                         
112300         MOVE MID-IDPLKLST-UT             TO WS-IDPLKLST                  
112400         INSPECT WS-IDPLKLST REPLACING LEADING SPACE BY ZERO              
112500     ELSE                                                                 
112600         MOVE MID-IDPLKLST-IN             TO WS-IDPLKLST                  
112700         MOVE JA                          TO FL-NYCKLAR                   
112800     END-IF                                                               
112900                                                                          
113000     IF MID-IDRADNR-IN = ALL '+'                                          
113100         MOVE MID-IDRADNR-UT              TO WS-IDRADNR                   
113200         INSPECT WS-IDRADNR REPLACING LEADING SPACE BY ZERO               
113300     ELSE                                                                 
113400         MOVE MID-IDRADNR-IN              TO WS-IDRADNR                   
113500         MOVE JA                          TO FL-NYCKLAR                   
113600     END-IF                                                               
113700                                                                          
113800     IF MID-IDDC-IN = ALL '+'                                             
113900       MOVE MID-IDDC-UT                   TO W-IDDC WS-IDDC               
114000     ELSE                                                                 
114100       MOVE JA                            TO FL-NYCKLAR                   
114200       MOVE MID-IDDC-IN                   TO W-IDDC WS-IDDC               
114300     END-IF                                                               
114400                                                                          
114500     MOVE NEJ                           TO WS-SATS                        
114600     MOVE WS-IDPRODNR                   TO MOD-IDPRODNR-UT                
114700     INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE              
114800     MOVE WS-IDDISTR                    TO MOD-IDDISTR-UT                 
114900     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
115000     MOVE WS-IDPLKLST                   TO MOD-IDPLKLST-UT                
115100     INSPECT MOD-IDPLKLST-UT REPLACING LEADING ZERO BY SPACE              
115200     MOVE WS-IDRADNR                    TO MOD-IDRADNR-UT                 
115300     INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE               
115400     MOVE W-IDDC                       TO MOD-IDDC-UT                     
115500                                                                          
115600     MOVE W-IDDC TO W-IDDC-B6                                             
115700     PERFORM IMS-GU-WDB601                                                
115800     .                                                                    
115900     EJECT                                                                
116000 B-GENERELL-KONTROLL  SECTION.                                            
116100                                                                          
116200     IF WS-IDPRODNR NOT NUMERIC                                           
116300         MOVE FEL                       TO WS-INDATA-TEST                 
116400                                           WS-BEHANDLING-TEST             
116500         MOVE FEL-749 (INDX)            TO MOD-TEMFSFEL                   
116600     END-IF                                                               
116700                                                                          
116800     IF WS-IDDISTR NOT NUMERIC                                            
116900         MOVE FEL                       TO WS-INDATA-TEST                 
117000                                           WS-BEHANDLING-TEST             
117100         MOVE FEL-749 (INDX)            TO MOD-TEMFSFEL                   
117200     ELSE                                                                 
117300         MOVE WS-IDDISTR                TO WS-IDDISTR-NUM                 
117400     END-IF                                                               
117500                                                                          
117600     IF WS-IDPLKLST NOT > ZERO                                            
117700         MOVE FEL                       TO WS-INDATA-TEST                 
117800                                           WS-BEHANDLING-TEST             
117900         MOVE FEL-749 (INDX)            TO MOD-TEMFSFEL                   
118000     END-IF                                                               
118100                                                                          
118200     IF WS-IDRADNR NOT NUMERIC                                            
118300         MOVE FEL                       TO WS-INDATA-TEST                 
118400                                           WS-BEHANDLING-TEST             
118500         MOVE FEL-749 (INDX)            TO MOD-TEMFSFEL                   
118600     ELSE                                                                 
118700         MOVE WS-IDRADNR                TO WS-IDRADNR-NUM                 
118800     END-IF                                                               
118900                                                                          
119000     IF W-IDDC IS > SPACE                                                 
119100       IF DCS-DDC                                                         
119200         MOVE FEL-796 (INDX)  TO MOD-TEMFSFEL                             
119300         MOVE FEL                       TO WS-INDATA-TEST                 
119400         PERFORM S02-RENSA-FALT                                           
119500       ELSE                                                               
119600         CONTINUE                                                         
119700       END-IF                                                             
119800     ELSE                                                                 
119900       MOVE FEL                           TO WS-INDATA-TEST               
120000                                             WS-BEHANDLING-TEST           
120100       MOVE FEL-749 (INDX)                TO MOD-TEMFSFEL                 
120200     END-IF                                                               
120300                                                                          
120400     IF  MID-IDRADNR-FOM NOT = ALL '+'                                    
120500     AND MID-IDRADNR-FOM NOT NUMERIC                                      
120600         MOVE FEL                       TO WS-INDATA-TEST                 
120700                                           WS-BEHANDLING-TEST             
120800         MOVE FEL-748 (INDX)            TO MOD-TEMFSFEL                   
120900         MOVE MFS-NUM-FAELT-FEL         TO MOD-IDRADNR-FOM-ATTR           
121000         MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDRADNR-TOM-ATTR           
121100     END-IF                                                               
121200     IF  MID-IDRADNR-TOM NOT = ALL '+'                                    
121300     AND MID-IDRADNR-TOM NOT NUMERIC                                      
121400         MOVE FEL                       TO WS-INDATA-TEST                 
121500                                           WS-BEHANDLING-TEST             
121600         MOVE FEL-748 (INDX)            TO MOD-TEMFSFEL                   
121700         MOVE MFS-NUM-FAELT-FEL         TO MOD-IDRADNR-TOM-ATTR           
121800     END-IF                                                               
121900     IF  MID-KVANNANT    NOT = ALL '+'                                    
122000     AND MID-KVANNANT    NOT NUMERIC                                      
122100         MOVE FEL                       TO WS-INDATA-TEST                 
122200                                           WS-BEHANDLING-TEST             
122300         MOVE FEL-748 (INDX)            TO MOD-TEMFSFEL                   
122400         MOVE MFS-NUM-FAELT-FEL         TO MOD-KVANNANT-ATTR              
122500     END-IF                                                               
122600     IF  MID-FLSVAR      = ALL '+'                                        
122700       CONTINUE                                                           
122800     ELSE                                                                 
122900       IF (MID-FLSVAR    = 'J' OR                                         
123000           MID-FLSVAR    = 'Y')                                           
123100           CONTINUE                                                       
123200       ELSE                                                               
123300           MOVE FEL                     TO WS-INDATA-TEST                 
123400                                           WS-BEHANDLING-TEST             
123500           MOVE FEL-748 (INDX)          TO MOD-TEMFSFEL                   
123600           MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLSVAR-ATTR                
123700       END-IF                                                             
123800     END-IF                                                               
123900     SKIP2                                                                
124000     .                                                                    
124100     EJECT                                                                
124200 D-BACKOUT-DEF-CASE SECTION.                                              
124300*--- DELETE DEFAULT CASE (KDKOLSTA=0) IF THERE IS ANY                     
124400*--- DEFAULT KOLLI FINNS BARA EN PER ORDERDEL SKAPAT PÅ L138.             
124500*--- VID FÖRSTA AVVIKELSE RAPP. PÅ L199 TAS DEN BORT OCH                  
124600*--- DÄREFTER KAN DET INTE FINNAS LÄNGRE DEFAULT KOLLI                    
124700                                                                          
124800     MOVE ZERO          TO W-IDKOLLI-MIN                                  
124900     MOVE 99999         TO W-IDKOLLI-MAX                                  
125000     MOVE WS-IDPLKLST   TO W-IDPLKLST                                     
125100     MOVE WS-IDPRODNR   TO W-IDPRODNR-MIN W-IDPRODNR-MAX                  
125200     PERFORM IMS-GU-WDE4F1-PLK                                            
125300     IF SEGMENT-FINNS                                                     
125400       MOVE SEQF-IDKOLLI  TO W-IDKOLLI-E6                                 
125500       MOVE WS-IDPRODNR   TO W-IDPRODNR-E6                                
125600       PERFORM IMS-GHU-WDE611-DEF                                         
125700       IF SEGMENT-FINNS                                                   
125800* KDKOLSTA CAN BE 0 FOR PACKED CASES FROM API                             
125900* SO ADDED EXTRA DIKOLLI CHECK                                            
125901       IF KOLLI-DIKOLLIL = ZERO                                           
125902         PERFORM IMS-DLET-WDE611                                          
125903         INITIALIZE KOLLI-WDE611                                          
126000         PERFORM IMS-GHU-WDE601                                           
126100         SUBTRACT 1 FROM VORD-KVKOLLI                                     
126200                                                                          
126300         COMPUTE VORD-VLORDBTO = VORD-VLORDBTO                            
126400                               - KOLLI-VLORDBTO-KOLLI                     
126500                                                                          
126600         PERFORM IMS-REPL-WDE601                                          
126700                                                                          
126800*-----WDE401 REDAN LÄST I B-SECTION                                       
126900         MOVE ZERO         TO W-IDPURAD-MIN                               
127000         MOVE 99999        TO W-IDPURAD-MAX                               
127100         PERFORM IMS-GHNP-WDE411-FIRST                                    
127200         PERFORM UNTIL SEGMENT-SAKNAS                                     
127300           MOVE ZERO             TO ORAD-KVLEVART                         
127400           MOVE ORAD-IDARTNR     TO WS-DNOT-IDARTNR                       
127500           MOVE ORAD-IDPURAD     TO WS-DNOT-IDPURAD                       
127600           PERFORM IMS-REPL-WDE411                                        
127700                                                                          
127800           PERFORM IMS-GHNP-WDE421                                        
127900           MOVE KKOLLI-IDKOLLI  TO WS-DNOT-IDKOLLI                        
128000                                                                          
128100           IF NDC-US OR NDC-CA                                            
128200              PERFORM DA-DATA-TILL-DEL-NOTE                               
128300           END-IF                                                         
128400           PERFORM IMS-DLET-WDE421                                        
128500                                                                          
128600           PERFORM IMS-GHNP-WDE411-BACKOUT                                
128700         END-PERFORM                                                      
128800       END-IF                                                             
128810       END-IF                                                             
128900     END-IF                                                               
129000     .                                                                    
129100     EJECT                                                                
129200                                                                          
129300 DA-DATA-TILL-DEL-NOTE SECTION.                                           
129400                                                                          
129500     MOVE VORD-IDDISTR          TO TEST-IDDISTR                           
129600     IF DIST07-USA-RETAILER-DNOTE                                         
129700     OR DIST07-CAN-RETAILER                                               
129800                                                                          
129900        INITIALIZE DNOT-ORDER-INFO                                        
130000                                                                          
130100        MOVE 'WL0199DE'               TO DNOT-IDPGM                       
130200        MOVE WS-DNOT-IDORDER          TO DNOT-IDORDER                     
130300        MOVE WS-DNOT-IDARTNR          TO DNOT-IDARTNR                     
130400        MOVE WS-DNOT-IDDC             TO DNOT-IDDC                        
130500        MOVE WS-DNOT-IDKOLLI          TO DNOT-IDKOLLI-BORT                
130600        MOVE WS-DNOT-IDPURAD          TO DNOT-IDPURAD                     
130700                                                                          
130800        CALL W411DNOT USING DNOT-W411DNOT                                 
130900                            DNOT-ORQP-PCB                                 
131000                            DNOT-ORQP2-PCB                                
131100                            DNOT-ORQP3-PCB                                
131200                            DNOT-4013-PCB                                 
131300                            DNOT-BENA-PCB                                 
131400     END-IF                                                               
131500     .                                                                    
131600     EJECT                                                                
131700                                                                          
131800 C-LAGG-UT-ORDER-INFO SECTION.                                            
131900                                                                          
132000     MOVE WS-IDPRODNR                   TO W-IDPRODNR-E6                  
132100                                           W-IDPRODNR-WDE4E-MIN           
132200                                           W-IDPRODNR-WDE4E-MAX           
132300     PERFORM IMS-GHU-WDE601                                               
132400     IF SEGMENT-FINNS                                                     
132500       IF VORD-IDDC                         =   W-IDDC                    
132600         IF VORD-IDDISTR                    =   WS-IDDISTR-NUM            
132700             IF VORD-KDORDSTA               >   +2                        
132800                 MOVE FEL                   TO WS-INDATA-TEST             
132900                                               WS-BEHANDLING-TEST         
133000                 MOVE FEL-710 (INDX)        TO MOD-TEMFSFEL               
133100             ELSE                                                         
133200                 MOVE VORD-IDDISTR          TO MOD-IDDISTR                
133300                                               W-IDDISTR-WDE4E-MIN        
133400                                               W-IDDISTR-WDE4E-MAX        
133500                 MOVE VORD-IDKUNDNR         TO MOD-IDKUNDNR               
133600                                               WS-IDKUNDNR-NUM            
133700                 MOVE VORD-IDDC             TO MOD-IDDC                   
133800                 MOVE VORD-KDORDKL          TO MOD-KDORDKL                
133900                                               WS-KDORDKL                 
134000                 MOVE VORD-KDFAKTYP         TO WS-KDFAKTYP                
134100                 MOVE VORD-KDFRAKT          TO WS-KDFRAKT                 
134200                 MOVE VORD-FLDIRLEV         TO WS-FLDIRLEV                
134300                 MOVE VORD-FLMANORD         TO WS-FLMANORD                
134400                                                                          
134500                 MOVE WS-IDPLKLST           TO W-IDPLKLST                 
134600                 PERFORM IMS-GU-WDE4E1                                    
134700                 IF SEGMENT-FINNS                                         
134800                                                                          
134900                   MOVE SEQE-IDPRODNR       TO W-401-IDPRODNR             
135000                   MOVE SEQE-IDDISTR        TO W-401-IDDISTR              
135100                   MOVE SEQE-IDKUNDNR       TO W-401-IDKUNDNR             
135200                   MOVE SEQE-IDORDNR5       TO W-401-IDORDNR              
135300                   MOVE SEQE-IDPLKLST       TO W-401-IDPLKLST             
135400                                                                          
135500                   PERFORM IMS-GHU-WDE401                                 
135600                   MOVE KORD-IDORDER        TO W-WDQ301-IDORDER           
135700                   MOVE KORD-IDORDER        TO W-IDORDER-Q2               
135800                   MOVE KORD-IDDC           TO W-WDQ301-IDDC              
135900                   MOVE KORD-IDPRODNR       TO W-WDQ301-IDPRODNR          
136000                   MOVE KORD-IDPLKLST       TO W-WDQ301-IDPLKLST          
136100                   MOVE KORD-KVORDRAD       TO MOD-KVORDRAD               
136200                   MOVE KORD-KVORDRAD-PACK  TO MOD-KVORDRAD-PACK          
136300                   PERFORM IMS-GU-ORQI01-GE                               
136400                   IF SEGMENT-FINNS                                       
136500** SOFTWARE KONTROLL                                                      
136600                     PERFORM IMS-GU-WDQ301                                
136700                     IF (ODEL-IDLEVNR = '1441 ' OR                        
136800                         ODEL-IDLEVNR = 'BP2TW')  AND                     
136900                       ODEL-IDPRC = '9998'                                
137000                       MOVE FEL             TO WS-INDATA-TEST             
137100                                               WS-BEHANDLING-TEST         
137200                       MOVE FEL18 (INDX)    TO MOD-TEMFSFEL               
137300                     ELSE                                                 
137400                       MOVE KORD-IDKUNDRF   TO WS-IDKUNDRF                
137500                       MOVE WS-IDORDNR      TO MOD-IDORDNR5               
137600                     END-IF                                               
137700                   ELSE                                                   
137800                     MOVE FEL               TO WS-INDATA-TEST             
137900                                               WS-BEHANDLING-TEST         
138000                     MOVE FEL-702 (INDX)    TO MOD-TEMFSFEL               
138100                   END-IF                                                 
138200                 ELSE                                                     
138300                     MOVE FEL               TO WS-INDATA-TEST             
138400                                               WS-BEHANDLING-TEST         
138500                     MOVE FEL21 (INDX)      TO MOD-TEMFSFEL               
138600                     MOVE ZERO              TO MOD-IDORDNR5               
138700                 END-IF                                                   
138800                 IF WS-BEHANDLING-RATT                                    
138900                   PERFORM CA-KONTROLLERA-RADNR                           
139000                 END-IF                                                   
139100             END-IF                                                       
139200         ELSE                                                             
139300             MOVE FEL                       TO WS-INDATA-TEST             
139400                                               WS-BEHANDLING-TEST         
139500             MOVE FEL-820 (INDX)            TO MOD-TEMFSFEL               
139600         END-IF                                                           
139700       ELSE                                                               
139800           MOVE FEL                         TO WS-INDATA-TEST             
139900                                               WS-BEHANDLING-TEST         
140000           MOVE FEL-701 (INDX)              TO MOD-TEMFSFEL               
140100       END-IF                                                             
140200     ELSE                                                                 
140300         MOVE FEL                           TO WS-INDATA-TEST             
140400                                               WS-BEHANDLING-TEST         
140500         MOVE FEL-701 (INDX)                TO MOD-TEMFSFEL               
140600     END-IF                                                               
140700     .                                                                    
140800     EJECT                                                                
140900 CA-KONTROLLERA-RADNR     SECTION.                                        
141000                                                                          
141100     IF WS-IDRADNR-NUM > ZERO                                             
141200         MOVE WS-IDRADNR-NUM          TO W-IDPURAD-MIN                    
141300                                         W-IDPURAD-MAX                    
141400         PERFORM IMS-GHNP-WDE411-MIN-MAX                                  
141500                                                                          
141600         IF SEGMENT-FINNS                                                 
141700             MOVE ORAD-KVAVBART       TO MOD-KVAVBART                     
141800                                         WS-KVAVBART                      
141900             MOVE MFS-STAENG-FAELT    TO MOD-FLSVAR-ATTR                  
142000                                         MOD-IDRADNR-FOM-ATTR             
142100                                         MOD-IDRADNR-TOM-ATTR             
142200             MOVE MFS-OEPPNA-NUM-FAELT                                    
142300                                      TO MOD-KVANNANT-ATTR                
142400             MOVE MFS-RENSA-FAELT     TO MOD-FLSVAR                       
142500                                         MOD-IDRADNR-FOM                  
142600                                         MOD-IDRADNR-TOM                  
142700         ELSE                                                             
142800             MOVE FEL                 TO WS-INDATA-TEST                   
142900                                         WS-BEHANDLING-TEST               
143000             MOVE FEL-821 (INDX)      TO MOD-TEMFSFEL                     
143100         END-IF                                                           
143200     ELSE                                                                 
143300         MOVE MFS-STAENG-FAELT        TO MOD-KVANNANT-ATTR                
143400         MOVE MFS-RENSA-FAELT         TO MOD-KVAVBART                     
143500         MOVE MFS-OEPPNA-ALFA-FAELT   TO MOD-FLSVAR-ATTR                  
143600         MOVE MFS-OEPPNA-NUM-FAELT    TO MOD-IDRADNR-FOM-ATTR             
143700                                         MOD-IDRADNR-TOM-ATTR             
143800     END-IF                                                               
143900     .                                                                    
144000     EJECT                                                                
144100 D-BEHANDLA-ANNULLERING    SECTION.                                       
144200     MOVE 'STA D-BEHANDLA-ANNULL'      TO WS-LOG-AVBROTT                  
144300                                                                          
144400     PERFORM DB-KOLLA-PACKARE-NOLL                                        
144500     MOVE JA                           TO FIRST-TIME-SW                   
144600                                                                          
144700     IF WS-BEHANDLING-RATT                                                
144800       MOVE W-IDDC                    TO W-XXDJ-IDDC                      
144900       MOVE WS-IDPRODNR                TO W-XXDJ-IDPRODNR                 
145000       PERFORM IMS-GU-XXDJ-ROT                                            
145100       IF SEGMENT-FINNS                                                   
145200       PERFORM IMS-GHNP-XXDJ-LASNING                                      
145300       IF SEGMENT-FINNS                                                   
145400*-------------------------FINNS LÅST PÅ LÅSNINGSREG?-------------         
145500         IF 4306-KDPACLAS            >   ZERO                             
145600*-------------------------LÅST AV ORDERVIS?----------------------         
145700             IF 4306-KDPACLAS        =   1 OR 2 OR 3                      
145800                 MOVE FEL-823 (INDX) TO MOD-TEMFSFEL                      
145900                 MOVE FEL            TO WS-BEHANDLING-TEST                
146000             END-IF                                                       
146100         END-IF                                                           
146200         IF WS-BEHANDLING-RATT                                            
146300             IF 4306-FLANNULL        =   NEJ                              
146400                 MOVE JA          TO 4306-FLANNULL                        
146500                 PERFORM IMS-REPL-XXDJ-LASNING                            
146600                 PERFORM S05-SKAPA-4308-ANNULL-SEGM                       
146700             ELSE                                                         
146800                 MOVE WS-IDRADNR-FOM TO W-XXDJ-IDRADNR-TOM                
146900                 PERFORM IMS-GHU-XXDJ-4308                                
147000                 IF SEGMENT-FINNS                                         
147100                   EVALUATE TRUE                                          
147200                                                                          
147300                     WHEN WS-IDRADNR-FOM =                                
147400                       4308-IDRADNR-ORD-TOM AND                           
147500                       WS-IDRADNR-TOM = 4308-IDRADNR-ORD-FROM             
147600                                                                          
147700                         IF (WS-KVANNANT  > WS-KVAVBART                   
147800                         OR  WS-KVANNANT  = ZERO)                         
147900                         AND FL-4359-OMSTARTAD = NEJ                      
148000                             MOVE FEL TO WS-BEHANDLING-TEST               
148100                             MOVE FEL-8221 (INDX) TO MOD-TEMFSFEL         
148200                         ELSE                                             
148300                             IF  WS-KVAVBART = WS-KVANNANT                
148400                                 MOVE ZERO       TO 4308-KVANNANT         
148500                             ELSE                                         
148600                                 ADD WS-KVANNANT TO 4308-KVANNANT         
148700                             END-IF                                       
148800                             PERFORM IMS-REPL-XXDJ-LASNING                
148900                         END-IF                                           
149000                                                                          
149100                     WHEN 4308-IDRADNR-ORD-FROM NOT >                     
149200                          WS-IDRADNR-TOM               AND                
149300                          FL-4359-OMSTARTAD = NEJ                         
149400                       MOVE FEL-8222 (INDX) TO MOD-TEMFSFEL               
149500                       MOVE FEL TO WS-BEHANDLING-TEST                     
149600                                                                          
149700                     WHEN OTHER                                           
149800                       PERFORM S05-SKAPA-4308-ANNULL-SEGM                 
149900                   END-EVALUATE                                           
150000                 ELSE                                                     
150100                     PERFORM S05-SKAPA-4308-ANNULL-SEGM                   
150200                 END-IF                                                   
150300             END-IF                                                       
150400         END-IF                                                           
150500       ELSE                                                               
150600         PERFORM S04-SKAPA-4306-LASNINGSSEGM                              
150700         PERFORM S05-SKAPA-4308-ANNULL-SEGM                               
150800       END-IF                                                             
150900       END-IF                                                             
151000     END-IF                                                               
151100                                                                          
151200     IF WS-BEHANDLING-RATT                                                
151300         PERFORM DH-INITIERA-INDEXEN                                      
151400         PERFORM DJ-NOLLST-ACKAR                                          
151500         PERFORM DI-LAS-FORSTA-RADEN                                      
151600                                                                          
151700         PERFORM UNTIL INX = WS-ANT-RADER-INT-PLUS-1 OR                   
151800                       INX = MAX-RAD-ANTAL           OR                   
151900                       SEGMENT-SAKNAS                OR                   
152000                       SEGMENT-SAKNAS-E411           OR                   
152100                       BASEN-SLUT                                         
152200           PERFORM DA-BEHANDLA-RAD-INOM-INTERVALL                         
152300           PERFORM DE-UPPDATERA-ARTIKELREG                                
152400           PERFORM DF-ACKA-TILL-ORDERREG                                  
152500           PERFORM S12-SKAPA-DIV-TRANS                                    
152600           PERFORM IMS-GHNP-WDE411-MIN-MAX                                
152700                                                                          
152800           IF SEGMENT-FINNS                                               
152900             ADD +1                 TO INX                                
153000           END-IF                                                         
153100                                                                          
153200           IF SEGMENT-SAKNAS                     OR                       
153300              BASEN-SLUT                         OR                       
153400              KORD-IDPLKLST NOT = W-401-IDPLKLST OR                       
153500              INX = WS-ANT-RADER-INT-PLUS-1      OR                       
153600              INX               = MAX-RAD-ANTAL                           
153700                                                                          
153800              PERFORM DK-UPPDATERA-KUNDORDER                              
153900              PERFORM DG-EV-STARTA-4397                                   
154000              PERFORM DJ-NOLLST-ACKAR                                     
154100           END-IF                                                         
154200         END-PERFORM                                                      
154300                                                                          
154400         IF INX = MAX-RAD-ANTAL                                           
154500             MOVE ORAD-IDPURAD          TO WS-IDRADNR-START               
154600             MOVE KORD-IDPLKLST         TO WS-IDPLKLST-START              
154700         END-IF                                                           
154800                                                                          
154900         PERFORM S09-UPPDATERA-KOLLIROT                                   
155000     END-IF                                                               
155100     MOVE 'END D-BEHANDLA-ANNULL'      TO WS-LOG-AVBROTT                  
155200     .                                                                    
155300     EJECT                                                                
155400 DA-BEHANDLA-RAD-INOM-INTERVALL SECTION.                                  
155500     MOVE 'STA DA-BEHANDLA-RAD'     TO WS-LOG-AVBROTT                     
155600                                                                          
155700     MOVE ZERO                          TO WS-KVPRERO-JUST                
155800     MOVE NEJ                           TO WS-ORAD-SLUTANNULL             
155900                                                                          
156000     IF WS-KVANNANT                     =   ZERO                          
156100*        * I DETTA LÄGE ÄR HELA RADEN ANNULLERAD PÅ EN GÅNG,              
156200*          DVS EJ ANNULL. I FLERA STEG.                                   
156300                                                                          
156400         MOVE ORAD-KVAVBART             TO SPAR-KVAVBART                  
156500         IF  OHUV-KDORDKL = 0                                             
156600         AND NOT DCS-NDC                                                  
156800           PERFORM S08-BACKA-NYVORKO                                      
156900         END-IF                                                           
157000                                                                          
157100         MOVE ORAD-KVAVBART             TO WS-ORAD-KVANNANT               
157200                                            ORAD-KVANNANT                 
157300         MOVE ZERO                      TO ORAD-KVAVBART                  
157400         MOVE +4                        TO ORAD-KDRADSTA                  
157500         ADD +1                         TO WS-ANTAL-ANNULL-RADER          
157600         COMPUTE WS-KVPRERO-JUST        = ORAD-KVBEART                    
157700                                        - ORAD-KVANNANT                   
157800         END-COMPUTE                                                      
157900*        IF KORD-KDORDKL = +0                                             
158000*           PERFORM DAA-UPPDATERA-VOR-TIKLAR                              
158100*        END-IF                                                           
158200         MOVE JA                        TO WS-ORAD-SLUTANNULL             
158300         MOVE MFS-RENSA-FAELT           TO MOD-KVAVBART                   
158400     ELSE                                                                 
158500*** UTRÄKNING AV SLATTPROCENT                                             
158600         MOVE +0             TO WS-KVSLATT                                
158700                                WS-KVSLATTAT                              
158800                                WS-RESLATT                                
158900         MOVE ORAD-KVAVBART  TO SPAR-KVAVBART                             
159000         IF ORAD-KVSLATT > +0                                             
159100            COMPUTE WS-RESLATT ROUNDED =                                  
159200        (ORAD-KVSLATT * 100) / (ORAD-KVAVBART + ORAD-KVANNANT)            
159300            COMPUTE WS-KVSLATT ROUNDED =                                  
159400             (ORAD-KVBEART * WS-RESLATT) / 100                            
159500         END-IF                                                           
159600*** SLUT                                                                  
159700         COMPUTE ORAD-KVANNANT = ORAD-KVANNANT + WS-KVANNANT              
159800         COMPUTE ORAD-KVAVBART = ORAD-KVAVBART - WS-KVANNANT              
159900*** SKALL ALLT SLATTAS BORT?                                              
160000         IF (ORAD-KVBEART - ORAD-KVAVBART) > WS-KVSLATT AND               
160100            ORAD-KVSLATT > +0                                             
160200                                                                          
160300            MOVE ORAD-KVAVBART          TO WS-KVSLATTAT                   
160400            MOVE +0                     TO ORAD-KVAVBART                  
160500            COMPUTE ORAD-KVSLATT = ORAD-KVBEART - ORAD-KVANNANT           
160600         END-IF                                                           
160700*** SLUT                                                                  
160800         MOVE ORAD-KVAVBART             TO MOD-KVAVBART                   
160900         IF  ORAD-KVAVBART = ZERO                                         
161000                                                                          
161100             IF  OHUV-KDORDKL = 0                                         
161200             AND NOT DCS-NDC                                              
161500               PERFORM S08-BACKA-NYVORKO                                  
161600             END-IF                                                       
161700                                                                          
161800             MOVE +4                    TO ORAD-KDRADSTA                  
161900             ADD +1                     TO WS-ANTAL-ANNULL-RADER          
162000                                                                          
162100             COMPUTE WS-KVPRERO-JUST    = ORAD-KVBEART                    
162200                                        - SPAR-KVAVBART                   
162300             END-COMPUTE                                                  
162400                                                                          
162500*            IF KORD-KDORDKL = +0                                         
162600*               PERFORM DAA-UPPDATERA-VOR-TIKLAR                          
162700*            END-IF                                                       
162800             MOVE JA                    TO WS-ORAD-SLUTANNULL             
162900         END-IF                                                           
163000         MOVE WS-KVANNANT               TO WS-ORAD-KVANNANT               
163100     END-IF                                                               
163200     PERFORM S01-UPPD-SPAR-UPPGIFTER                                      
163300     MOVE ORAD-FLDIRLEV                 TO WS-FLDIRLEV                    
163400     MOVE ORAD-FLRESTN                  TO WS-FLRESTN                     
163500     MOVE ORAD-IDARTNR                  TO WS-IDARTNR                     
163600     PERFORM IMS-REPL-WDE411                                              
163700     MOVE 'END DA-BEHANDLA-RAD'     TO WS-LOG-AVBROTT                     
163800     .                                                                    
163900     EJECT                                                                
164000 DAA-UPPDATERA-VOR-TIKLAR SECTION.                                        
164100     SKIP3                                                                
164200                                                                          
164300     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
164400     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
164500     MOVE KORD-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
164600                                    W-A601KY-MAX-IDDISTR                  
164700     MOVE KORD-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
164800                                    W-A601KY-MAX-IDKUNDNR                 
164900     MOVE KORD-IDORDNR5          TO W-IDORDNR                             
165000     MOVE W-IDKUNDRF-RED         TO W-A601KY-MIN-IDKUNDRF                 
165100                                    W-A601KY-MAX-IDKUNDRF                 
165200                                                                          
165300     MOVE NEJ                    TO SW-TIKLAR-UPPDATERAD                  
165400                                                                          
165500     PERFORM IMS-GHN-WDA6B                                                
165600     PERFORM UNTIL SEGMENT-SAKNAS                                         
165700                OR BASEN-SLUT                                             
165800                OR SW-TIKLAR-UPPDATERAD = JA                              
165900                                                                          
166000         IF  VOR-IDARTNR = ORAD-IDARTNR                                   
166100         AND VOR-TIKLAR = +0                                              
166200                                                                          
166300             MOVE WS-DAGENS-DATUM  TO VOR-TIKLAR                          
166400             MOVE WS-TIHHMMSS      TO VOR-TIKLATID                        
166500             PERFORM IMS-REPL-WDA6B                                       
166600             MOVE JA               TO SW-TIKLAR-UPPDATERAD                
166700         END-IF                                                           
166800                                                                          
166900         PERFORM IMS-GHN-WDA6B                                            
167000     END-PERFORM                                                          
167100     .                                                                    
167200     EJECT                                                                
167300 DB-KOLLA-PACKARE-NOLL   SECTION.                                         
167400     MOVE 'STA DB-KOLLA-PACK'       TO WS-LOG-AVBROTT                     
167500                                                                          
167600     IF KORD-IDUSER > '00000000'                                          
167700       MOVE FEL                        TO WS-BEHANDLING-TEST              
167800       MOVE FEL-8223 (INDX)            TO MOD-TEMFSFEL                    
167900     END-IF                                                               
168000                                                                          
168100     MOVE 'END DB-KOLLA-PACK'       TO WS-LOG-AVBROTT                     
168200     .                                                                    
168300     EJECT                                                                
168400 DE-UPPDATERA-ARTIKELREG         SECTION.                                 
168500     MOVE 'STA DE-UPPDATERA-'       TO WS-LOG-AVBROTT                     
168600                                                                          
168700     IF DCS-CDC                                                           
168800       PERFORM DEA-UPDATE-ART-REG-WDK6-WDK9                               
168900     ELSE                                                                 
169000       PERFORM DEB-UPDATE-ART-REG-WDK7                                    
169100     END-IF                                                               
169200                                                                          
169300     PERFORM DEC-EV-UPDATE-REFILL-WDK7                                    
169400     MOVE 'END DE-UPPDATERA-'       TO WS-LOG-AVBROTT                     
169500     .                                                                    
169600     EJECT                                                                
169700 DEA-UPDATE-ART-REG-WDK6-WDK9     SECTION.                                
169800     MOVE 'STA DEA-UPDATE-ART-REG'  TO WS-LOG-AVBROTT                     
169900                                                                          
170000      MOVE WS-IDARTNR            TO W-WDK601-IDARTNR                      
170100                                                                          
170200      PERFORM IMS-GHU-ARTC01                                              
170300                                                                          
170400     IF ART-FLIART = JA  AND WS-FLDIRLEV = NEJ                            
170500       MOVE JA                   TO WS-SATS                               
170600     END-IF                                                               
170700                                                                          
170800     PERFORM IMS-GHNP-ARTC11                                              
170900     IF   WS-FLDIRLEV            =   NEJ                                  
171000                                                                          
171100       MOVE CLAG-KVQPACK-1       TO WS-CLAG-KVQPACK-1                     
171200       MOVE CLAG-KDLEVSP         TO WS-CLAG-KDLEVSP                       
171300       MOVE CLAG-REDIRLEV        TO WS-REDIRLEV                           
171400       MOVE CLAG-FLREFILL        TO WS-FLREFILL                           
171500       MOVE CLAG-KVLS            TO WS-KVLS                               
171600       MOVE CLAG-KVEFRS          TO WS-KVEFRS                             
171700                                                                          
171800       COMPUTE CLAG-KVLS         =  CLAG-KVLS                             
171900                                 +  WS-ORAD-KVANNANT                      
172000                                 +  WS-KVSLATTAT                          
172100       END-COMPUTE                                                        
172200*SVS FROG                                                                 
172300                                                                          
172400       MOVE WS-IDDISTR         TO DIST20-IDDISTR                          
172500       IF NOT DIST20-EMBALLAGE-SVS                                        
172600         MOVE KORD-IDORDER     TO W-WDQ301-IDORDER                        
172700         MOVE KORD-IDDC        TO W-WDQ301-IDDC                           
172800         MOVE KORD-IDPRODNR    TO W-WDQ301-IDPRODNR                       
172900         MOVE KORD-IDPLKLST    TO W-WDQ301-IDPLKLST                       
173000         PERFORM IMS-GU-WDQ301                                            
173100       END-IF                                                             
173200                                                                          
173300       IF DIST20-EMBALLAGE-SVS                                            
173400       OR ODEL-IDPRC = 2600                                               
173500         COMPUTE CLAG-KVLS-SVS =                                          
173600                 CLAG-KVLS-SVS   +  WS-ORAD-KVANNANT                      
173700                                 +  WS-KVSLATTAT                          
173800         END-COMPUTE                                                      
173900       END-IF                                                             
174000                                                                          
174100       MOVE 1                    TO IX                                    
174200       PERFORM UNTIL IX > 4                                               
174300       OR ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (IX)                           
174400         ADD 1                   TO IX                                    
174500       END-PERFORM                                                        
174600                                                                          
174700       IF IX > 4                                                          
174800         CONTINUE                                                         
174900       ELSE                                                               
175000         ADD WS-ORAD-KVANNANT    TO CLAG-KVLS-CD (IX)                     
175100       END-IF                                                             
175200       COMPUTE CLAG-KVEFRS       =  CLAG-KVEFRS                           
175300                                 -  WS-ORAD-KVANNANT                      
175400                                 -  WS-KVSLATTAT                          
175500       END-COMPUTE                                                        
175600       IF ORAD-IDKAMPRF          >  ZERO                                  
175700         COMPUTE CLAG-KVRESS     =  CLAG-KVRESS                           
175800                                 +  WS-ORAD-KVANNANT                      
175900                                 +  WS-KVSLATTAT                          
176000         END-COMPUTE                                                      
176100       END-IF                                                             
176200       MOVE CLAG-KVLS            TO W-ART-KVLS                            
176300       MOVE CLAG-KVUTRS          TO W-ART-KVUTRS                          
176400       MOVE CLAG-KVRESS          TO W-ART-KVRESS                          
176500       MOVE CLAG-KVSPANT         TO W-ART-KVSPANT                         
176600       MOVE CLAG-KDERS           TO W-ART-KDERS                           
176700       MOVE CLAG-KVSLAGER        TO W-ART-KVSLAGER                        
176800       MOVE CLAG-PRARTSTD        TO W-ART-PRARTSTD                        
176900       PERFORM IMS-REPL-ARTC                                              
177000       PERFORM DEAC-BERAKNA-SALDOLOGG-DATA                                
177100                                                                          
177200       PERFORM DEAA-TAECKNING                                             
177300     END-IF                                                               
177400                                                                          
177500     PERFORM DEAB-EV-MINSKA-PRERO                                         
177600                                                                          
177700*--- RÄKNA NER KVBEART-KUND FÖR KAMPANJRADER                              
177800                                                                          
177900     IF  ORAD-IDKAMPRF > ZERO                                             
178000                                                                          
178100       MOVE ORAD-IDKAMPRF          TO W-KAMP-IDKAMPRF                     
178200       MOVE KORD-IDDC              TO W-KAMP-IDDC                         
178300       MOVE WS-IDARTNR             TO W-KART-IDARTNR                      
178400       PERFORM IMS-GHU-WDM211                                             
178500       IF SEGMENT-FINNS                                                   
178600                                                                          
178700         IF  KART-KVBEART-KUND >= WS-ORAD-KVANNANT                        
178800           SUBTRACT WS-ORAD-KVANNANT                                      
178900                                    FROM KART-KVBEART-KUND                
179000         COMPUTE KART-KVRESS-ART = KART-KVRESS-ART                        
179100                                 + WS-ORAD-KVANNANT                       
179200         ELSE                                                             
179300             MOVE 'WDM211 KART-KVBEART-KUND BLIR NEGATIV'                 
179400                                    TO FELTEXT                            
179500             CALL ABEND USING RKOD-33                                     
179600         END-IF                                                           
179700         PERFORM IMS-REPL-WDM211                                          
179800       END-IF                                                             
179900                                                                          
180000       MOVE KORD-IDDISTR           TO W-KMRK-IDDISTR-FOM                  
180100       MOVE KORD-IDDISTR           TO W-KMRK-IDDISTR-TOM                  
180200       MOVE KORD-IDKUNDNR          TO W-KMRK-IDKUNDNR-FOM                 
180300       MOVE KORD-IDKUNDNR          TO W-KMRK-IDKUNDNR-TOM                 
180400                                                                          
180500       PERFORM S20-FINN-INTERVALL                                         
180600                                                                          
180700       PERFORM IMS-GHU-WDM221                                             
180800                                                                          
180900       IF  SEGMENT-FINNS                                                  
181000         IF  KMRK-KVBEART-KUND >= WS-ORAD-KVANNANT                        
181100           SUBTRACT WS-ORAD-KVANNANT                                      
181200                                  FROM KMRK-KVBEART-KUND                  
181300         ELSE                                                             
181400           MOVE 'WDM221 KMRK-KVBEART-KUND BLIR NEGATIV'                   
181500                                  TO FELTEXT                              
181600           CALL ABEND USING RKOD-33                                       
181700         END-IF                                                           
181800         PERFORM IMS-REPL-WDM221                                          
181900       END-IF                                                             
182000                                                                          
182100     END-IF                                                               
182200     MOVE 'END DEA-UPDATE-ART-REG'  TO WS-LOG-AVBROTT                     
182300     .                                                                    
182400     EJECT                                                                
182500 DEAA-TAECKNING SECTION.                                                  
182600     MOVE 'STA DEAA-TAECKNING'       TO WS-LOG-AVBROTT                    
182700                                                                          
182800     MOVE JA TO TAECKNING                                                 
182900                                                                          
183000     IF DCS-CDC                                                           
183100        IF WS-CLAG-KDLEVSP = 20 OR 21                                     
183200           MOVE NEJ TO TAECKNING                                          
183300        END-IF                                                            
183400     END-IF                                                               
183500                                                                          
183600     IF TAECKNING = JA                                                    
183700                                                                          
183800           IF W-ART-PRARTSTD > 0                                          
183900             COMPUTE DISPONIBELT =                                        
184000                     W-ART-KVLS   - W-ART-KVUTRS -                        
184100                     W-ART-KVRESS - W-ART-KVSPANT                         
184200             IF DISPONIBELT > 0                                           
184300*------------------* HÄNSYN TILL RANS.GRÄNS BORTTAGEN                     
184400                   MOVE W-IDDC     TO W-4505-IDDC                         
184500                   MOVE WS-IDARTNR  TO 4506-IDARTNR                       
184600                   MOVE 15          TO 4506-KDTAKORS                      
184700                   MOVE ZERO        TO 4506-KVANTMOT                      
184800                   PERFORM IMS-ISRT-450511                                
184900             END-IF                                                       
185000           END-IF                                                         
185100     END-IF                                                               
185200     MOVE 'END DEAA-TAECKNING'       TO WS-LOG-AVBROTT                    
185300     .                                                                    
185400     EJECT                                                                
185500 DEAB-EV-MINSKA-PRERO                     SECTION.                        
185600     MOVE 'STA DEAB-EV-MINSKA'       TO WS-LOG-AVBROTT                    
185700                                                                          
185800     IF  WS-KVPRERO-JUST     > ZERO                                       
185900       MOVE WS-IDARTNR         TO W-WDK901-IDARTNR                        
186000       PERFORM IMS-GHU-ARTM-ART                                           
186100                                                                          
186200       EVALUATE ORAD-KDORDKL                                              
186300         WHEN 1                                                           
186400           SUBTRACT WS-KVPRERO-JUST        FROM                           
186500                                                                          
186600             ARTM-ART-KVPRERO-DAG                                         
186700                                                                          
186800         WHEN 2 THRU 4                                                    
186900           SUBTRACT WS-KVPRERO-JUST        FROM                           
187000             ARTM-ART-KVPRERO-BULK                                        
187100       END-EVALUATE                                                       
187200                                                                          
187300       PERFORM IMS-REPL-ARTM                                              
187400     END-IF                                                               
187500     MOVE 'END DEAB-EV-MINSKA'       TO WS-LOG-AVBROTT                    
187600     .                                                                    
187700     EJECT                                                                
187800 DEAC-BERAKNA-SALDOLOGG-DATA             SECTION.                         
187900     MOVE 'STA DEAC-BERAKNA-'       TO WS-LOG-AVBROTT                     
188000                                                                          
188100     PERFORM S14-FLYTTA-SALDOLOGG-DATA                                    
188200*   ---DB-SPECIFIK INFORMATION                                            
188300     MOVE W-WDK601-IDARTNR         TO LOGG-IDARTNR                        
188400     MOVE WC-CDC-SE                TO LOGG-IDDC                           
188500     MOVE CLAG-KVEFRS              TO LOGG-KVEFRS                         
188600     MOVE CLAG-KVLS                TO LOGG-KVLS                           
188700     MOVE CLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
188800     COMPUTE LOGG-KVAKS            =  CLAG-KVAKS-CDC                      
188900                                   +  CLAG-KVAKS-T                        
189000     COMPUTE LOGG-KVART-SALDO      =  WS-ORAD-KVANNANT                    
189100                                   +  WS-KVSLATTAT                        
189200*   ---KOLLAR SALDOFÖRÄNDRINGAR PÅ WDK611 OCH                             
189300*   ---LOGGAR DESSA PÅ WDL9                                               
189400     IF WS-KVEFRS > CLAG-KVEFRS                                           
189500        MOVE '-' TO LOGG-IDTECKEN-KVEFRS                                  
189600     ELSE                                                                 
189700        IF WS-KVEFRS < CLAG-KVEFRS                                        
189800           MOVE '+' TO LOGG-IDTECKEN-KVEFRS                               
189900        ELSE                                                              
190000           MOVE ' ' TO LOGG-IDTECKEN-KVEFRS                               
190100        END-IF                                                            
190200     END-IF                                                               
190300                                                                          
190400     IF WS-KVLS > CLAG-KVLS                                               
190500        MOVE '-' TO LOGG-IDTECKEN-KVLS                                    
190600     ELSE                                                                 
190700        IF WS-KVLS < CLAG-KVLS                                            
190800           MOVE '+' TO LOGG-IDTECKEN-KVLS                                 
190900        ELSE                                                              
191000           MOVE ' ' TO LOGG-IDTECKEN-KVLS                                 
191100        END-IF                                                            
191200     END-IF                                                               
191300     PERFORM S13-ISRT-SALDOLOGG                                           
191400     MOVE 'END DEAC-BERAKNA-'       TO WS-LOG-AVBROTT                     
191500     .                                                                    
191600     EJECT                                                                
191700 DEB-UPDATE-ART-REG-WDK7      SECTION.                                    
191800     MOVE 'STA DEB-UPDATE-ART'      TO WS-LOG-AVBROTT                     
191900                                                                          
192000     MOVE WS-IDARTNR               TO W-WDK701-IDARTNR                    
192100     MOVE W-IDDC                  TO W-WDK711-IDDC                        
192200                                                                          
192300     PERFORM IMS-GHU-ARTS11                                               
192400                                                                          
192500     IF   WS-FLDIRLEV            =   NEJ                                  
192600                                                                          
192700       MOVE SLAG-KVLS              TO WS-KVLS                             
192800       MOVE SLAG-KVEFRS            TO WS-KVEFRS                           
192900                                                                          
193000       COMPUTE SLAG-KVLS           = SLAG-KVLS                            
193100                                     + WS-ORAD-KVANNANT                   
193200                                     + WS-KVSLATTAT                       
193300       COMPUTE SLAG-KVEFRS         = SLAG-KVEFRS                          
193400                                     - WS-ORAD-KVANNANT                   
193500                                     - WS-KVSLATTAT                       
193600       END-COMPUTE                                                        
193700       PERFORM IMS-REPL-ARTS                                              
193800       PERFORM DEBA-BERAKNA-SALDOLOGG-DATA                                
193900     END-IF                                                               
194000     MOVE 'END DEB-UPDATE-ART'      TO WS-LOG-AVBROTT                     
194100     .                                                                    
194200     EJECT                                                                
194300 DEBA-BERAKNA-SALDOLOGG-DATA             SECTION.                         
194400     MOVE 'STA DEBA-BERAKNA-SA'      TO WS-LOG-AVBROTT                    
194500                                                                          
194600     PERFORM S14-FLYTTA-SALDOLOGG-DATA                                    
194700*    ---DB-SPECIFIK INFORMATION                                           
194800     MOVE W-WDK701-IDARTNR         TO LOGG-IDARTNR                        
194900     MOVE W-WDK711-IDDC            TO LOGG-IDDC                           
195000     MOVE SLAG-KVEFRS              TO LOGG-KVEFRS                         
195100     MOVE SLAG-KVLS                TO LOGG-KVLS                           
195200     MOVE SLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
195300     MOVE SLAG-KVAKS-SDC           TO LOGG-KVAKS                          
195400     COMPUTE LOGG-KVART-SALDO      =  WS-ORAD-KVANNANT                    
195500                                   +  WS-KVSLATTAT                        
195600*    ---KOLLAR SALDOFÖRÄNDRINGAR PÅ WDK711 OCH                            
195700*    ---LOGGAR DESSA PÅ WDL9                                              
195800     IF WS-KVEFRS > SLAG-KVEFRS                                           
195900        MOVE '-' TO LOGG-IDTECKEN-KVEFRS                                  
196000     ELSE                                                                 
196100        IF WS-KVEFRS < SLAG-KVEFRS                                        
196200           MOVE '+' TO LOGG-IDTECKEN-KVEFRS                               
196300        ELSE                                                              
196400           MOVE ' ' TO LOGG-IDTECKEN-KVEFRS                               
196500        END-IF                                                            
196600     END-IF                                                               
196700                                                                          
196800     IF WS-KVLS > SLAG-KVLS                                               
196900        MOVE '-' TO LOGG-IDTECKEN-KVLS                                    
197000     ELSE                                                                 
197100        IF WS-KVLS < SLAG-KVLS                                            
197200           MOVE '+' TO LOGG-IDTECKEN-KVLS                                 
197300        ELSE                                                              
197400           MOVE ' ' TO LOGG-IDTECKEN-KVLS                                 
197500        END-IF                                                            
197600     END-IF                                                               
197700     PERFORM S13-ISRT-SALDOLOGG                                           
197800     MOVE 'END DEBA-BERAKNA-SA'      TO WS-LOG-AVBROTT                    
197900     .                                                                    
198000     EJECT                                                                
198100 DEC-EV-UPDATE-REFILL-WDK7 SECTION.                                       
198200     MOVE 'STA DEC-EV-UPDATE-'      TO WS-LOG-AVBROTT                     
198300                                                                          
198400******************************************************************        
198500*                                                                         
198600*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
198700*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
198800*                                                                         
198900******************************************************************        
199000                                                                          
199100     MOVE WS-IDDISTR-NUM       TO W-TP4TRAN-IDDISTR                       
199200                                                                          
199300     PERFORM DB2-SELECT-TP4TRAN                                           
199400                                                                          
199500*REFILLORDER                                                              
199600     MOVE WS-IDARTNR               TO W-WDK701-IDARTNR                    
199700                                      W-WDK601-IDARTNR                    
199800     MOVE WS-IDDISTR-NUM           TO DIST35-IDDISTR                      
199900     IF (DIST35-REFILL                                                    
200000     OR  DIST35-REFILL-INOM-NDC                                           
200100     OR  DIST35-NONVCC-NONVCC-REFILL                                      
200510     OR  DIST35-NONVCC-NONVCC-TRANSFER                                    
200511     OR  DIST35-NA-TRANSFER                                               
200512     OR  DIST35-PACIFIC-TRANSFER                                          
200513     OR  DIST35-REFILL-INOM-JP                                            
200514     OR  DIST35-NA-NDC-RETURNS                                            
200515     OR  DIST35-NONVCC-VCC-REFILL                                         
200516     OR  DIST35-NONVCC-VCC-TRANSFER                                       
200600     OR  RADER-FINNS)                                                     
200700     AND WS-ORAD-KVANNANT > ZERO                                          
200800       IF RADER-FINNS                                                     
200900         MOVE TP4TRAN-IDDC-REC TO W-WDK711-IDDC                           
201000       ELSE                                                               
201100         PERFORM DECA-GET-SDC-IDDC-VALUE                                  
201200       END-IF                                                             
201300                                                                          
201400       PERFORM IMS-GHU-ARTS11                                             
201500       IF  SLAG-KVBEART  >= WS-ORAD-KVANNANT                              
201600         SUBTRACT WS-ORAD-KVANNANT   FROM SLAG-KVBEART                    
201700       ELSE                                                               
201800         MOVE 'WLARTS11 SLAG-KVBEART BLIR NEGATIV'  TO FELTEXT            
201900         CALL ABEND USING RKOD-33                                         
202000       END-IF                                                             
202100                                                                          
202200       PERFORM IMS-REPL-ARTS                                              
202300     ELSE                                                                 
202400        IF DIST35-NONVCC-CDC-REFILL                                       
202500        AND WS-ORAD-KVANNANT > ZERO                                       
202600                                                                          
202700           PERFORM DECA-GET-SDC-IDDC-VALUE                                
202800                                                                          
202900           PERFORM IMS-GHU-ARTC11                                         
203000           IF  CLAG-KVBEART  >= WS-ORAD-KVANNANT                          
203100              SUBTRACT WS-ORAD-KVANNANT   FROM CLAG-KVBEART               
203200           ELSE                                                           
203300              MOVE 'WLARTC11 CLAG-KVBEART BLIR NEGATIV'                   
203400                                TO FELTEXT                                
203500              CALL ABEND USING RKOD-33                                    
203600           END-IF                                                         
203700                                                                          
203800           PERFORM IMS-REPL-ARTC                                          
203900        END-IF                                                            
204000     END-IF                                                               
204100     MOVE 'END DEC-EV-UPDATE-'      TO WS-LOG-AVBROTT                     
204200     .                                                                    
204300     EJECT                                                                
204400 DECA-GET-SDC-IDDC-VALUE            SECTION.                              
204500                                                                          
204600     SEARCH ALL DIST57-REFILL-DC                                          
204700        AT END                                                            
204800           MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                           
204900                            TO FELTEXT                                    
205000           CALL FELLOG                                                    
205100        WHEN DIST57-SOK-IDDISTR(DIST57-IX) = WS-IDDISTR-NUM               
205200           MOVE DIST57-REFILL-TO-DC(DIST57-IX)                            
205300                            TO W-WDK711-IDDC                              
205400     END-SEARCH                                                           
205500     .                                                                    
205600     EJECT                                                                
205700 DF-ACKA-TILL-ORDERREG     SECTION.                                       
205800     MOVE 'STA DF-ACKA-TILL'        TO WS-LOG-AVBROTT                     
205900                                                                          
206000     MOVE KORD-IDUSER               TO WS-IDUSER                          
206100                                                                          
206200     MOVE KORD-KDFAKTYP             TO WS-KDFAKTYP                        
206300     MOVE KORD-FLLSBOK              TO WS-FLLSBOK                         
206400     MOVE KORD-FLORDSPE             TO WS-FLORDSPE                        
206500                                                                          
206600     MOVE KORD-IDORDER              TO W-IDORDER-Q2                       
206700     PERFORM IMS-GU-ORQI01                                                
206800     MOVE OHUV-BEKUNDRF             TO SPAR-BEKUNDRF                      
206900                                                                          
207000     COMPUTE ACK-VKORDNTO = ACK-VKORDNTO + SPAR-VKORDNTO                  
207100     COMPUTE ACK-VLORDNTO = ACK-VLORDNTO + SPAR-VLORDNTO                  
207200     IF WS-FLDIRLEV = JA                                                  
207300       COMPUTE ACK-SUORDV-LEVPL         = ACK-SUORDV-LEVPL                
207400                                        + SPAR-SUORDV                     
207500       COMPUTE ACK-SUORDV-LEVPL-LOC     = ACK-SUORDV-LEVPL-LOC            
207600                                        + SPAR-SUORDV-LOC                 
207700       COMPUTE ACK-SUORDV-LEVPL-LOCPREL = ACK-SUORDV-LEVPL-LOCPREL        
207800                                        + SPAR-SUORDV-LOCPREL             
207900     ELSE                                                                 
208000       COMPUTE ACK-SUORDV         = ACK-SUORDV                            
208100                                  + SPAR-SUORDV                           
208200       COMPUTE ACK-SUORDV-EXP     = ACK-SUORDV-EXP                        
208300                                  + SPAR-SUORDV-EXP                       
208400       COMPUTE ACK-SUORDV-LOC     = ACK-SUORDV-LOC                        
208500                                  + SPAR-SUORDV-LOC                       
208600       COMPUTE ACK-SUORDV-LOCPREL = ACK-SUORDV-LOCPREL                    
208700                                  + SPAR-SUORDV-LOCPREL                   
208800     END-IF                                                               
208900                                                                          
209000                                                                          
209100     IF  WS-ORAD-SLUTANNULL  = JA                                         
209200*      * RAD HAR HELT ANNULLERATS, RÄKNA UPP PACKADE RADER.               
209300       ADD +1                TO ACK-KVORDRAD-PACK                         
209400     END-IF                                                               
209500     MOVE 'END DF-ACKA-TILL'        TO WS-LOG-AVBROTT                     
209600     .                                                                    
209700     EJECT                                                                
209800 DG-EV-STARTA-4397 SECTION.                                               
209900     MOVE 'STA DG-EV-STARTA'        TO WS-LOG-AVBROTT                     
210000     SKIP2                                                                
210100     IF KORD-KVORDRAD-PACK = KORD-KVORDRAD                                
210200                           + KORD-KVORDRAD-LEVPL                          
210300                                                                          
210400        MOVE 'W4T397X '           TO 4397-TRANSKOD                        
210500                                     4397-LTERM-NAME                      
210600        MOVE WS-KDMFSFOR          TO 4397-KDMFSFOR                        
210700        MOVE '4359'               TO 4397-IDTRANS                         
210800        MOVE +117                 TO 4397-LL                              
210900        MOVE WS-IDPRODNR          TO 4397-IDPRODNR                        
211000        MOVE WS-IDANSTNR          TO 4397-IDANSTNR                        
211100        MOVE W-401-IDPLKLST       TO 4397-IDPLKLST                        
211200        MOVE ZERO                 TO 4397-IDPURAD                         
211300        MOVE 4397-TRANSAREA       TO 4397-AREA                            
211400        IF FIRST-TIME                                                     
211500           PERFORM IMS-INSERT-4397-TRANS                                  
211600           MOVE NEJ               TO FIRST-TIME-SW                        
211700        ELSE                                                              
211800           PERFORM IMS-PURG-4397-TRANS                                    
211900        END-IF                                                            
212000     END-IF                                                               
212100     MOVE 'END DG-EV-STARTA'        TO WS-LOG-AVBROTT                     
212200     .                                                                    
212300     EJECT                                                                
212400 DH-INITIERA-INDEXEN                     SECTION.                         
212500     MOVE 'STA DH-INDEXERA-IND'     TO WS-LOG-AVBROTT                     
212600                                                                          
212700     MOVE +1                        TO INX                                
212800                                                                          
212900     IF FL-4359-OMSTARTAD = JA                                            
213000       MOVE MID-IDRADNR-START            TO WS-IDRADNR-START-NUM          
213100                                                                          
213200       COMPUTE WS-ANT-RADER-INT-PLUS-1 =                                  
213300         WS-IDRADNR-TOM - WS-IDRADNR-START-NUM + 2                        
213400       END-COMPUTE                                                        
213500                                                                          
213600     ELSE                                                                 
213700       COMPUTE WS-ANT-RADER-INT-PLUS-1 =                                  
213800         WS-IDRADNR-TOM - WS-IDRADNR-FOM + 2                              
213900       END-COMPUTE                                                        
214000     END-IF                                                               
214100     MOVE 'END DH-INDEXERA-IND'     TO WS-LOG-AVBROTT                     
214200     .                                                                    
214300     EJECT                                                                
214400 DI-LAS-FORSTA-RADEN                     SECTION.                         
214500     MOVE 'STA DI-LAS-FORSTA-RADEN' TO WS-LOG-AVBROTT                     
214600                                                                          
214700     MOVE WS-IDRADNR-FOM                 TO W-IDPURAD-MIN                 
214800     MOVE WS-IDRADNR-TOM                 TO W-IDPURAD-MAX                 
214900                                                                          
215000     IF FL-4359-OMSTARTAD = JA                                            
215100       MOVE MID-IDRADNR-START            TO W-IDPURAD-MIN                 
215200*DENNA IMSLÄSNING GER ABEND. MÅSTE SKRIVAS OM.                            
215300*1. STARTA OM PGM. 2. TAG BORT LÅSNING PÅ R4. 3. KÖR 4359 MED             
215400*FORTS PÅ ANNULLATIONS-INTERVALL. SE AAID FÖR INMATAT INTERVALL.          
215500       PERFORM IMS-GHNP-WDE411-MIN-MAX                                    
215600       CONTINUE                                                           
215700     ELSE                                                                 
215800       PERFORM IMS-GHNP-WDE411-FIRST                                      
215900     END-IF                                                               
216000     MOVE 'END DI-LAS-FORSTA-RADEN' TO WS-LOG-AVBROTT                     
216100     .                                                                    
216200     EJECT                                                                
216300 DJ-NOLLST-ACKAR                         SECTION.                         
216400     MOVE 'STA DJ-NOLLST-ACKAR'     TO WS-LOG-AVBROTT                     
216500                                                                          
216600     MOVE ZERO                TO ACK-VKORDNTO                             
216700                                 ACK-VLORDNTO                             
216800                                 ACK-SUORDV                               
216900                                 ACK-SUORDV-EXP                           
217000                                 ACK-SUORDV-LOC                           
217100                                 ACK-SUORDV-LOCPREL                       
217200                                 ACK-SUORDV-LEVPL                         
217300                                 ACK-SUORDV-LEVPL-LOC                     
217400                                 ACK-SUORDV-LEVPL-LOCPREL                 
217500                                 ACK-KVORDRAD-PACK                        
217600     MOVE 'END DJ-NOLLST-ACKAR'     TO WS-LOG-AVBROTT                     
217700     .                                                                    
217800     EJECT                                                                
217900 DK-UPPDATERA-KUNDORDER                  SECTION.                         
218000     MOVE 'STA DK-UPPDATERA-KUND'    TO WS-LOG-AVBROTT                    
218100                                                                          
218200     MOVE KORD-IDDISTR           TO W-401-IDDISTR                         
218300     MOVE KORD-IDKUNDNR          TO W-401-IDKUNDNR                        
218400     MOVE KORD-IDKUNDRF          TO W-401-IDKUNDRF                        
218500     MOVE KORD-IDPRODNR          TO W-401-IDPRODNR                        
218600     MOVE KORD-IDPLKLST          TO W-401-IDPLKLST                        
218700     PERFORM IMS-GHU-WDE401                                               
218800                                                                          
218900     SUBTRACT ACK-VKORDNTO              FROM KORD-VKORDNTO                
219000     SUBTRACT ACK-VLORDNTO              FROM KORD-VLORDNTO                
219100     SUBTRACT ACK-SUORDV                FROM KORD-SUORDV                  
219200     SUBTRACT ACK-SUORDV-EXP            FROM KORD-SUORDV-EXP              
219300     SUBTRACT ACK-SUORDV-LOC            FROM KORD-SUORDV-LOC              
219400     SUBTRACT ACK-SUORDV-LOCPREL        FROM KORD-SUORDV-LOCPREL          
219500     SUBTRACT ACK-SUORDV-LEVPL          FROM KORD-SUORDV-LEVPL            
219600     SUBTRACT ACK-SUORDV-LEVPL-LOC      FROM KORD-SUORDV-LEVPL-LOC        
219700     SUBTRACT ACK-SUORDV-LEVPL-LOCPREL  FROM                              
219800                                 KORD-SUORDV-LEVPL-LOCPREL                
219900     ADD ACK-KVORDRAD-PACK       TO KORD-KVORDRAD-PACK                    
220000                                                                          
220100     IF KORD-KVORDRAD-PACK = KORD-KVORDRAD                                
220200                           + KORD-KVORDRAD-LEVPL                          
220300*UPPDATERINGEN GÖRS I PGM W40397                                          
220400        CONTINUE                                                          
220500     ELSE                                                                 
220600        PERFORM IMS-REPL-WDE401                                           
220700     END-IF                                                               
220800     MOVE 'END DK-UPPDATERA-KUND'    TO WS-LOG-AVBROTT                    
220900     .                                                                    
221000     EJECT                                                                
221100 E-KOLLA-INFAELT           SECTION.                                       
221200                                                                          
221300     EVALUATE TRUE                                                        
221400                                                                          
221500       WHEN MID-FLSVAR       = ALL '+' AND                                
221600            MID-IDRADNR-FOM  = ALL '+' AND                                
221700            MID-IDRADNR-TOM  = ALL '+' AND                                
221800            MID-KVANNANT     = ALL '+'                                    
221900         MOVE FEL         TO WS-BEHANDLING-TEST                           
222000         MOVE FEL-59X (INDX)                                              
222100                          TO MOD-TEMFSFEL                                 
222200         IF  WS-IDRADNR-NUM > ZERO                                        
222300             MOVE MFS-NUM-FAELT-FEL                                       
222400                          TO MOD-KVANNANT-ATTR                            
222500             MOVE MFS-STAENG-FAELT                                        
222600                          TO MOD-FLSVAR-ATTR                              
222700                             MOD-IDRADNR-FOM-ATTR                         
222800                             MOD-IDRADNR-TOM-ATTR                         
222900         ELSE                                                             
223000             MOVE MFS-ALFA-FAELT-FEL                                      
223100                          TO MOD-FLSVAR-ATTR                              
223200             MOVE MFS-NUM-FAELT-FEL                                       
223300                          TO MOD-IDRADNR-FOM-ATTR                         
223400                             MOD-IDRADNR-TOM-ATTR                         
223500             MOVE MFS-STAENG-FAELT                                        
223600                          TO MOD-KVANNANT-ATTR                            
223700         END-IF                                                           
223800                                                                          
223900       WHEN MID-FLSVAR       NOT = ALL '+' AND                            
224000            (MID-IDRADNR-FOM NOT = ALL '+' OR                             
224100             MID-IDRADNR-TOM NOT = ALL '+')                               
224200         MOVE FEL         TO WS-BEHANDLING-TEST                           
224300         MOVE FEL-59Y (INDX)                                              
224400                          TO MOD-TEMFSFEL                                 
224500         MOVE MFS-ALFA-FAELT-FEL                                          
224600                          TO MOD-FLSVAR-ATTR                              
224700         MOVE MFS-NUM-FAELT-FEL                                           
224800                          TO MOD-IDRADNR-FOM-ATTR                         
224900                             MOD-IDRADNR-TOM-ATTR                         
225000         MOVE MFS-STAENG-FAELT                                            
225100                          TO MOD-KVANNANT-ATTR                            
225200                                                                          
225300       WHEN MID-FLSVAR = JA OR YES                                        
225400         PERFORM IMS-GHNP-WDE411-LAST                                     
225500         MOVE +1                        TO WS-IDRADNR-FOM                 
225600         MOVE ORAD-IDPURAD              TO WS-IDRADNR-TOM                 
225700                                                                          
225800       WHEN MID-IDRADNR-FOM NOT = ALL '+'                                 
225900         MOVE MID-IDRADNR-FOM TO WS-IDRADNR-FOM                           
226000         IF   MID-IDRADNR-TOM NOT NUMERIC                                 
226100              MOVE MID-IDRADNR-FOM TO MID-IDRADNR-TOM                     
226200         END-IF                                                           
226300         MOVE MID-IDRADNR-TOM TO WS-IDRADNR-TOM                           
226400         IF WS-IDRADNR-MAX = ZERO                                         
226500           PERFORM IMS-GHNP-WDE411-LAST                                   
226600           MOVE ORAD-IDPURAD            TO WS-IDRADNR-MAX                 
226700         END-IF                                                           
226800         IF  MID-IDRADNR-TOM > WS-IDRADNR-MAX                             
226900         AND WS-FLMANORD = NEJ                                            
227000            MOVE FEL TO WS-BEHANDLING-TEST                                
227100            MOVE FEL-821 (INDX) TO MOD-TEMFSFEL                           
227200            MOVE MFS-NUM-FAELT-FEL                                        
227300                         TO MOD-IDRADNR-FOM-ATTR                          
227400                            MOD-IDRADNR-TOM-ATTR                          
227500         END-IF                                                           
227600                                                                          
227700         IF  WS-IDRADNR-FOM > WS-IDRADNR-TOM                              
227800            MOVE FEL TO WS-BEHANDLING-TEST                                
227900            MOVE FEL-751 (INDX) TO MOD-TEMFSFEL                           
228000            MOVE MFS-NUM-FAELT-FEL                                        
228100                         TO MOD-IDRADNR-FOM-ATTR                          
228200                            MOD-IDRADNR-TOM-ATTR                          
228300         END-IF                                                           
228400                                                                          
228500         IF  WS-IDRADNR-FOM = 0                                           
228600            MOVE FEL TO WS-BEHANDLING-TEST                                
228700            MOVE FEL-751 (INDX) TO MOD-TEMFSFEL                           
228800            MOVE MFS-NUM-FAELT-FEL                                        
228900                         TO MOD-IDRADNR-FOM-ATTR                          
229000                            MOD-IDRADNR-TOM-ATTR                          
229100         END-IF                                                           
229200                                                                          
229300       WHEN MID-IDRADNR-FOM = ALL '+' AND                                 
229400            WS-IDRADNR = ZERO                                             
229500            MOVE FEL TO WS-BEHANDLING-TEST                                
229600            MOVE FEL-751 (INDX) TO MOD-TEMFSFEL                           
229700            MOVE MFS-NUM-FAELT-FEL                                        
229800                         TO MOD-IDRADNR-FOM-ATTR                          
229900                            MOD-IDRADNR-TOM-ATTR                          
230000                                                                          
230100       WHEN MID-KVANNANT = ALL '+'                                        
230200            MOVE FEL TO WS-BEHANDLING-TEST                                
230300            MOVE FEL-59X (INDX) TO MOD-TEMFSFEL                           
230400            MOVE MFS-NUM-FAELT-FEL                                        
230500                         TO MOD-KVANNANT-ATTR                             
230600            MOVE MFS-STAENG-FAELT                                         
230700                         TO MOD-FLSVAR-ATTR                               
230800                            MOD-IDRADNR-FOM-ATTR                          
230900                            MOD-IDRADNR-TOM-ATTR                          
231000                                                                          
231100       WHEN MID-KVANNANT NOT = ALL '+'                                    
231200         MOVE WS-IDRADNR-NUM TO WS-IDRADNR-FOM                            
231300                                WS-IDRADNR-TOM                            
231400         IF WS-IDRADNR-NUM > ZERO                                         
231500            MOVE MID-KVANNANT TO WS-KVANNANT                              
231600            PERFORM S10-KOLLA-ANTAL                                       
231700         ELSE                                                             
231800            MOVE FEL TO WS-BEHANDLING-TEST                                
231900            MOVE FEL-725 (INDX) TO MOD-TEMFSFEL                           
232000            MOVE MFS-NUM-FAELT-FEL                                        
232100                         TO MOD-KVANNANT-ATTR                             
232200            MOVE MFS-STAENG-FAELT                                         
232300                         TO MOD-FLSVAR-ATTR                               
232400                            MOD-IDRADNR-FOM-ATTR                          
232500                            MOD-IDRADNR-TOM-ATTR                          
232600         END-IF                                                           
232700     END-EVALUATE                                                         
232800     .                                                                    
232900     EJECT                                                                
233000 F-RENSA-NYCKLAR SECTION.                                                 
233100     MOVE MFS-RENSA-FAELT            TO MOD-IDDISTR-UT                    
233200                                        MOD-IDPRODNR-UT                   
233300                                        MOD-IDPLKLST-UT                   
233400                                        MOD-IDRADNR-UT                    
233500     .                                                                    
233600     EJECT                                                                
233700 H-AVSLUT             SECTION.                                            
233800     SKIP3                                                                
233900     MOVE '4359'                          TO MFS-IDTRANS                  
234000     IF EGEN-MID                                                          
234100         IF WS-BEHANDLING-RATT                                            
234200             MOVE WS-IDDISTR-NUM          TO TEST-IDDISTR                 
234300             IF GAMLA-NYCKLAR                                             
234400                 MOVE RAETT-1 (INDX)      TO MOD-TEMFSINF                 
234500                 MOVE MFS-STAENG-FAELT                                    
234600                              TO MOD-FLSVAR-ATTR                          
234700                                 MOD-IDRADNR-FOM-ATTR                     
234800                                 MOD-IDRADNR-TOM-ATTR                     
234900                                 MOD-KVANNANT-ATTR                        
235000                 IF LASNINGSTRANS-TAS-BORT                                
235100                     PERFORM S06-BORTTAG-LASPOST                          
235200                 END-IF                                                   
235300                 IF AUT-FAKTURA-SKRIVS-UT                                 
235400                    PERFORM S07-AUTOMATFAKTURERING                        
235500                    IF SEGMENT-SAKNAS                                     
235600                       MOVE SPACE       TO IO-AREA2                       
235700                       MOVE WS-IDDISTR  TO 4726-AUTFAKT-IDDISTR           
235800                       MOVE WS-IDKUNDNR-NUM                               
235900                                       TO 4726-AUTFAKT-IDKUNDNR           
236000                       MOVE W-IDDC    TO 4726-AUTFAKT-IDDC                
236100                      MOVE WS-KDFAKTYP TO 4726-AUTFAKT-KDFAKTYP           
236200                       PERFORM IMS-ISRT-AUTFAKTURA-ROT                    
236300                       MOVE SPACE      TO IO-AREA2                        
236400                       PERFORM S07-AUTOMATFAKTURERING                     
236500                    END-IF                                                
236600                 END-IF                                                   
236700             END-IF                                                       
236800         ELSE                                                             
236900             MOVE MFS-ROER-EJ-FAELT                                       
237000                                          TO MOD-FLSVAR                   
237100                                             MOD-IDRADNR-FOM              
237200                                             MOD-IDRADNR-TOM              
237300                                             MOD-KVANNANT                 
237400         END-IF                                                           
237500     END-IF                                                               
237600     SKIP3                                                                
237700     .                                                                    
237800 S01-UPPD-SPAR-UPPGIFTER SECTION.                                         
237900     SKIP3                                                                
238000     COMPUTE SPAR-VKORDNTO ROUNDED = SPAR-VKORDNTO +                      
238100                 (ORAD-VKARTNTO * WS-ORAD-KVANNANT)                       
238200*                                                                         
238300     COMPUTE SPAR-VLORDNTO ROUNDED = SPAR-VLORDNTO +                      
238400                 (ORAD-VLARTNTO * WS-ORAD-KVANNANT / 1000000)             
238500*                                                                         
238600     MOVE WS-IDDISTR     TO TEST-IDDISTR                                  
238700                                                                          
238800     IF DIST79-DEALER-PRICE                                               
238900      IF  ORAD-PRARTNTO-LOCPREL > 0                                       
239000       COMPUTE SPAR-SUORDV-LOCPREL ROUNDED = SPAR-SUORDV-LOCPREL +        
239100                 (ORAD-PRARTNTO-LOCPREL * WS-ORAD-KVANNANT)               
239200      ELSE                                                                
239300       COMPUTE SPAR-SUORDV-LOC ROUNDED = SPAR-SUORDV-LOC +                
239400                 (ORAD-PRARTNTO-LOC * WS-ORAD-KVANNANT)                   
239500      END-IF                                                              
239600     ELSE                                                                 
239700       IF DIST79-ECOM-PRICE                                               
239900         COMPUTE SPAR-SUORDV-LOC ROUNDED = SPAR-SUORDV-LOC +              
240000                   (ORAD-PRARTNTO-LOC * WS-ORAD-KVANNANT)                 
240100       ELSE                                                               
240200        COMPUTE SPAR-SUORDV-EXP ROUNDED = SPAR-SUORDV-EXP +               
240300                (ORAD-PRAVCOST * WS-ORAD-KVANNANT)                        
240400        COMPUTE SPAR-SUORDV ROUNDED = SPAR-SUORDV +                       
240500                (ORAD-PRARTNTO * WS-ORAD-KVANNANT)                        
240600       END-IF                                                             
240700     END-IF                                                               
240800     .                                                                    
240900     EJECT                                                                
241000* MFS SEKTION                                                             
241100 S02-RENSA-FALT          SECTION.                                         
241200     SKIP3                                                                
241300     MOVE MFS-RENSA-FAELT             TO MOD-FLSVAR                       
241400                                         MOD-IDRADNR-FOM                  
241500                                         MOD-IDRADNR-TOM                  
241600                                         MOD-KVANNANT                     
241700     MOVE MFS-ROER-EJ-FAELT           TO MOD-IDDISTR                      
241800                                         MOD-IDKUNDNR                     
241900                                         MOD-IDORDNR5                     
242000                                         MOD-KVORDRAD                     
242100                                         MOD-IDDC                         
242200                                         MOD-KDORDKL                      
242300     .                                                                    
242400     EJECT                                                                
242500 S03-LADDA-4359-MID    SECTION.                                           
242600     SKIP3                                                                
242700     MOVE ALL '+'                        TO MID-W4I35901                  
242800     MOVE WS-IDDISTR                     TO MID-IDDISTR-UT                
242900     MOVE WS-IDPRODNR                    TO MID-IDPRODNR-UT               
243000     MOVE WS-IDPLKLST                    TO MID-IDPLKLST-UT               
243100     MOVE ZERO                           TO MID-IDRADNR-UT                
243200     MOVE W-IDDC                        TO MID-IDDC-UT                    
243300     MOVE WS-IDRADNR-FOM                 TO MID-IDRADNR-FOM               
243400     MOVE WS-IDRADNR-TOM                 TO MID-IDRADNR-TOM               
243500     MOVE WS-IDRADNR-START               TO MID-IDRADNR-START             
243600     MOVE WS-IDPLKLST-START              TO MID-IDPLKLST-START            
243700     .                                                                    
243800     EJECT                                                                
243900 S04-SKAPA-4306-LASNINGSSEGM SECTION.                                     
244000     SKIP3                                                                
244100     MOVE WS-IDPRODNR           TO 4306-IDPRODNR                          
244200                                   W-XXDJ-IDPRODNR                        
244300     MOVE LOW-VALUE             TO 4306-LOWVALUE                          
244400     MOVE JA                    TO 4306-FLANNULL                          
244500     MOVE ZERO                  TO 4306-KDPACLAS                          
244600     SKIP2                                                                
244700     PERFORM IMS-ISRT-XXDJ-LASNING                                        
244800     .                                                                    
244900     EJECT                                                                
245000 S05-SKAPA-4308-ANNULL-SEGM  SECTION.                                     
245100                                                                          
245200     IF FL-4359-OMSTARTAD = NEJ                                           
245300       COMPUTE WS-ANT-RADER-INT   = WS-IDRADNR-TOM                        
245400                                  - WS-IDRADNR-FOM                        
245500                                  + 1                                     
245600       END-COMPUTE                                                        
245700                                                                          
245800       MOVE WS-IDRADNR-TOM        TO W-XXDJ-IDRADNR-TOM                   
245900                                     4308-IDRADNR-ORD-TOM                 
246000       MOVE LOW-VALUE             TO 4308-LOWVALUE                        
246100       MOVE WS-IDRADNR-FOM        TO 4308-IDRADNR-ORD-FROM                
246200                                                                          
246300       IF WS-IDRADNR-NUM = ZERO          OR                               
246400          WS-KVAVBART    = WS-KVANNANT                                    
246500         MOVE ZERO                TO 4308-KVANNANT                        
246600       ELSE                                                               
246700         MOVE WS-KVANNANT         TO 4308-KVANNANT                        
246800       END-IF                                                             
246900                                                                          
247000       PERFORM IMS-ISRT-XXDJ-4308                                         
247100     END-IF                                                               
247200     .                                                                    
247300     EJECT                                                                
247400 S06-BORTTAG-LASPOST         SECTION.                                     
247500     SKIP2                                                                
247600     MOVE WS-IDPRODNR           TO W-XXDJ-IDPRODNR                        
247700     MOVE W-IDDC               TO W-XXDJ-IDDC                             
247800     PERFORM IMS-GU-XXDJ-ROT                                              
247900     PERFORM IMS-GHNP-XXDJ-LASNING                                        
248000     IF  SEGMENT-FINNS                                                    
248100         PERFORM IMS-DLET-XXDJ-LASNING                                    
248200     END-IF                                                               
248300     .                                                                    
248400     EJECT                                                                
248500 S07-AUTOMATFAKTURERING      SECTION.                                     
248600     SKIP3                                                                
248700     MOVE WS-IDDISTR                  TO W-RDG-IDDISTR                    
248800     MOVE WS-IDKUNDNR-NUM             TO W-RDG-IDKUNDNR                   
248900     MOVE W-IDDC                     TO W-RDG-IDDC                        
249000     MOVE WS-KDFAKTYP                 TO W-RDG-KDFAKTYP                   
249100     SKIP2                                                                
249200     MOVE WS-IDPRODNR                 TO 4727-AUTFAKT-IDPRODNR            
249300     MOVE ZERO                        TO 4727-AUTFAKT-PRFRAKT             
249400                                         4727-AUTFAKT-IDSKEPPN            
249500*    IF DIST03-SVERIGE                                                    
249600*    AND NOT DIST19-SATS                                                  
249700*        MOVE JA                      TO W-XXDV-FLBATCH                   
249800*    ELSE                                                                 
249900         MOVE NEJ                     TO W-XXDV-FLBATCH                   
250000*    END-IF                                                               
250100     IF DIST03-SVERIGE                                                    
250200         MOVE NEJ                     TO 4727-AUTFAKT-FLLASTA             
250300     ELSE                                                                 
250400         MOVE JA                      TO 4727-AUTFAKT-FLLASTA             
250500     END-IF                                                               
250600     PERFORM IMS-ISRT-AUTFAKTURA                                          
250700     .                                                                    
250800     EJECT                                                                
250900 S08-BACKA-NYVORKO SECTION.                                               
251000                                                                          
251100     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
251200     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
251300     MOVE OHUV-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
251400                                    W-A601KY-MAX-IDDISTR                  
251500     MOVE OHUV-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
251600                                    W-A601KY-MAX-IDKUNDNR                 
251700     MOVE OHUV-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF                 
251800                                    W-A601KY-MAX-IDKUNDRF                 
251900     MOVE OHUV-TIREGDAT          TO W-A601KY-MIN-TIREGDAT                 
252000                                    W-A601KY-MAX-TIREGDAT                 
252100     MOVE ORAD-IDARTNR           TO W-A601KY-MIN-IDARTNR                  
252200                                    W-A601KY-MAX-IDARTNR                  
252300                                                                          
252400     PERFORM IMS-GHN-WDA6B                                                
252500     PERFORM UNTIL SEGMENT-SAKNAS                                         
252600                OR BASEN-SLUT                                             
252700                                                                          
252800         IF  VOR-KDVORATG > '1'                                           
252900         AND VOR-KDVORATG < '6'                                           
253000         AND VOR-KVPREAVB        = SPAR-KVAVBART                          
253100             MOVE '8'            TO VOR-KDVORATG                          
253200             MOVE 0              TO VOR-KVPREAVB                          
253300             IF VOR-TIKLAR = ZERO                                         
253400                MOVE WS-TINUDAT  TO VOR-TIKLAR                            
253500                COMPUTE VOR-TIKLATID = WS-TINUTID                         
253600                                        / 100                             
253700                END-COMPUTE                                               
253800             END-IF                                                       
253900             PERFORM IMS-REPL-WDA6B                                       
254000         END-IF                                                           
254100                                                                          
254200         PERFORM IMS-GHN-WDA6B                                            
254300     END-PERFORM                                                          
254400     .                                                                    
254500     EJECT                                                                
254600 S09-UPPDATERA-KOLLIROT   SECTION.                                        
254700     SKIP3                                                                
254800     PERFORM IMS-GHU-WDE601                                               
254900     ADD WS-ANTAL-ANNULL-RADER       TO VORD-KVORDRAD-PACK                
255000     MOVE KORD-KVORDRAD-PACK         TO MOD-KVORDRAD-PACK                 
255100     SUBTRACT SPAR-SUORDV          FROM VORD-SUORDV                       
255200     SUBTRACT SPAR-SUORDV-EXP      FROM VORD-SUORDV-EXP                   
255300     SUBTRACT SPAR-SUORDV-LOC      FROM VORD-SUORDV-LOC                   
255400     SUBTRACT SPAR-SUORDV-LOCPREL  FROM VORD-SUORDV-LOCPREL               
255500     SUBTRACT SPAR-VKORDNTO        FROM VORD-VKORDNTO                     
255600     SUBTRACT SPAR-VLORDNTO        FROM VORD-VLORDNTO                     
255700                                                                          
255800     PERFORM IMS-REPL-WDE601                                              
255900     .                                                                    
256000 S10-KOLLA-ANTAL          SECTION.                                        
256100     SKIP3                                                                
256200     MOVE WS-IDRADNR-NUM                  TO W-WDE411-IDPURAD             
256300     PERFORM IMS-GHNP-WDE411                                              
256400     MOVE ORAD-KVAVBART                   TO WS-KVAVBART                  
256500     COMPUTE WS-KVART = ORAD-KVAVBART - ORAD-KVLEVART                     
256600     IF WS-KVANNANT > WS-KVART                                            
256700     OR WS-KVANNANT NOT > ZERO                                            
256800         MOVE FEL                         TO WS-BEHANDLING-TEST           
256900         MOVE FEL-725 (INDX)              TO MOD-TEMFSFEL                 
257000         MOVE MFS-NUM-FAELT-FEL           TO MOD-KVANNANT-ATTR            
257100         MOVE MFS-STAENG-FAELT            TO MOD-FLSVAR-ATTR              
257200                                             MOD-IDRADNR-FOM-ATTR         
257300                                             MOD-IDRADNR-TOM-ATTR         
257400     END-IF                                                               
257500     .                                                                    
257600     EJECT                                                                
257700 S12-SKAPA-DIV-TRANS SECTION.                                             
257800                                                                          
257900     PERFORM S12A-GENERERA-ANNULL-TRANS                                   
258000                                                                          
258100     IF NOT DIST19-SATS                                                   
258200       PERFORM S12B-UPDAT-ORDBEK-WDQ1                                     
258300     END-IF                                                               
258400                                                                          
258500     IF ORAD-KDOI NOT = SPACE                                             
258600       PERFORM S12C-GENERERA-2109-TRANS                                   
258700     END-IF                                                               
258800     .                                                                    
258900     EJECT                                                                
259000 S12A-GENERERA-ANNULL-TRANS      SECTION.                                 
259100     MOVE 'STA S12A-GENERERA'             TO WS-LOG-AVBROTT               
259200                                                                          
259300     ACCEPT LOGG-TIAAMMDD                 FROM DATE                       
259400     ACCEPT LOGG-TIKLOCK                  FROM TIME                       
259500     ADD +1                               TO LOGG-IDLOGLOP                
259600     MOVE 'RY5'                           TO RY5-IDPTYP                   
259700                                             LOGG-IDPTYP                  
259800     MOVE ORAD-BERADREF                   TO RY5-BERADREF                 
259900     MOVE ORAD-BEVOLREF                   TO RY5-BEVOLREF                 
260000     MOVE WS-IDKUNDRF                     TO RY5-IDKUNDRF                 
260100     MOVE WS-IDARTNR                      TO RY5-IDARTNR                  
260200     MOVE WS-FLRESTN                      TO RY5-FLRESTN                  
260300     MOVE WS-FLDIRLEV                     TO RY5-FLDIRLEV                 
260400     MOVE WS-FLLSBOK                      TO RY5-FLLSBOK                  
260500     MOVE WS-FLORDSPE                     TO RY5-FLORDSPE                 
260600     MOVE ORAD-IDKUNDRF-RO                TO RY5-IDKUNDRF-RO              
260700                                                                          
260800     IF ORAD-FLTILLK = JA                                                 
260900         MOVE 1                           TO RY5-KDARTERS                 
261000     ELSE                                                                 
261100         MOVE ZERO                        TO RY5-KDARTERS                 
261200     END-IF                                                               
261300                                                                          
261400     MOVE W-IDDC                         TO RY5-IDDC                      
261500     MOVE ORAD-KDDSP                      TO RY5-KDDSP                    
261600     MOVE WS-KDFAKTYP                     TO RY5-KDFAKTYP                 
261700     MOVE WS-KDFRAKT                      TO RY5-KDFRAKT                  
261800     MOVE ORAD-KDORDING                   TO RY5-KDORDING                 
261900     MOVE WS-KDORDKL                      TO RY5-KDORDKL                  
262000     MOVE ORAD-KDORDKL                    TO RY5-KDORDKL-URS              
262100     MOVE ORAD-KDORDTYP                   TO RY5-KDORDTYP                 
262200     MOVE ORAD-KDKVBRYT                   TO RY5-KDKVBRYT                 
262300     MOVE ORAD-KDVRINFO                   TO RY5-KDVRINFO                 
262400     MOVE ORAD-KVBEART                    TO RY5-KVBEART                  
262500     MOVE ORAD-KVAVBART                   TO RY5-KVAVBART                 
262600     MOVE WS-ORAD-KVANNANT                TO RY5-KVANNANT                 
262700                                             RY5-KVAVART                  
262800     MOVE ORAD-REKSIFFR                   TO RY5-REKSIFFR                 
262900     MOVE ORAD-TIUTSKR                    TO RY5-TIORDREG                 
263000     MOVE ORAD-TIRODAT                    TO RY5-TIRODAT                  
263100     MOVE RY5-WDGZRY5                     TO LOGG-LOGGPOST                
263200                                                                          
263300     MOVE KORD-IDORDER                    TO W-IDORDER-Q2                 
263400     PERFORM IMS-GU-ORQI01                                                
263500                                                                          
263600     MOVE WS-IDDISTR                      TO RY5S-IDDISTR                 
263700     MOVE WS-IDKUNDNR-NUM                 TO RY5S-IDKUNDNR                
263800     IF  OHUV-FLVORKO = JA                                                
263900     OR  OHUV-FLVORKO = YES                                               
264000         MOVE JA                          TO RY5S-FLVORKO                 
264100     ELSE                                                                 
264200         MOVE OHUV-FLVORKO                TO RY5S-FLVORKO                 
264300     END-IF                                                               
264400     MOVE OHUV-FLFORBI                    TO RY5S-FLFORBI                 
264500     MOVE OHUV-FLOVRLEV                   TO RY5S-FLOVRLEV                
264600     MOVE ORAD-IDSYSTEM                   TO RY5S-IDSYSTEM                
264700     MOVE ORAD-KVSLATT                    TO RY5S-KVSLATT                 
264800     MOVE ORAD-KDPRODSL                   TO RY5S-KDPRODSL                
264900     MOVE SPACE                           TO RY5S-FILLERX5                
265000                                             RY5S-FILLERX10               
265100     MOVE KORD-IDDISTR                    TO W-501-IDDISTR                
265200     MOVE KORD-IDKUNDNR                   TO W-501-IDKUNDNR               
265300     MOVE ORAD-IDKUNDRF-RO                TO W-501-IDKUNDRF               
265400     MOVE ORAD-IDARTNR                    TO W-501-IDARTNR                
265500     MOVE ORAD-IDLOPNR-RO                 TO W-501-IDLOPNR                
265600*    OM REST-NOTERAD TAG FRÅN WDA5 ANNARS TAG NOLL.                       
265700     PERFORM IMS-GU-ORDP01                                                
265800     IF SEGMENT-FINNS                                                     
265900       MOVE RAD-KDTPOTYP                  TO RY5S-KDTPOTYP                
266000                                             WS-RAD-KDTPOTYP              
266100     ELSE                                                                 
266200       MOVE ZERO                          TO RY5S-KDTPOTYP                
266300     END-IF                                                               
266400     MOVE RY5S-WDGZRY5S-CTX               TO LOGG-SORTPOST                
266500*                                                                         
266600     PERFORM IMS-ISRT-ZZAC01                                              
266700     PERFORM UNTIL SEGMENT-FINNS                                          
266800       IF LOGG-IDLOGLOP = 9                                               
266900         MOVE ZERO              TO LOGG-IDLOGLOP                          
267000         ACCEPT  LOGG-TIKLOCK   FROM TIME                                 
267100       END-IF                                                             
267200       ADD +1                   TO LOGG-IDLOGLOP                          
267300       PERFORM IMS-ISRT-ZZAC01                                            
267400     END-PERFORM                                                          
267500     MOVE 'END S12A-GENERERA'             TO WS-LOG-AVBROTT               
267600     .                                                                    
267700     EJECT                                                                
267800 S12B-UPDAT-ORDBEK-WDQ1 SECTION.                                          
267900*                                                                         
268000     MOVE KORD-IDORDER                    TO OBKR-IDORDER                 
268100     MOVE ORAD-IDARTNR                    TO OBKR-IDARTNR                 
268200     MOVE 1                               TO OBKR-IDLOPNR                 
268300     MOVE 1                               TO OBKR-IDSEKVNR                
268400     MOVE W-IDDC                         TO OBKR-IDDC                     
268500     MOVE IDPGM                           TO OBKR-IDPGM                   
268600     MOVE 83                              TO OBKR-KDORDBEK                
268700     MOVE SPACE                           TO OBKR-BEERS                   
268800     MOVE SPAR-BEKUNDRF                   TO OBKR-BEKUNDRF                
268900     MOVE ORAD-BERADREF                   TO OBKR-BERADREF                
269000     MOVE ORAD-BEVOLREF                   TO OBKR-BEVOLREF                
269100     MOVE ORAD-IDKAMPRF                   TO OBKR-IDKAMPRF                
269200     MOVE 0                               TO OBKR-DIERS-KVOT              
269300     MOVE NEJ                             TO OBKR-FLAKPLOC                
269400     MOVE NEJ                             TO OBKR-FLSLATT                 
269500     MOVE ORAD-FLINVEST                   TO OBKR-FLINVEST                
269600     MOVE JA                              TO OBKR-FLOBOK                  
269700     MOVE NEJ                             TO OBKR-FLOBTRAN                
269800     MOVE NEJ                             TO OBKR-FLOBPRT                 
269900     MOVE ORAD-FLPRTILL                   TO OBKR-FLPRTILL                
270000     MOVE ORAD-FLRESTN                    TO OBKR-FLRESTN                 
270100     MOVE NEJ                             TO OBKR-FLTILLK                 
270200     MOVE 0                               TO OBKR-IDARTNR-TILLK           
270300     MOVE ORAD-IDDC-RO                    TO OBKR-IDDC-RO                 
270400     MOVE KORD-IDDISTR                    TO OBKR-IDDISTR                 
270500     MOVE KORD-IDKUNDNR                   TO OBKR-IDKUNDNR                
270600                                                                          
270700     MOVE KORD-IDKUNDRF                   TO WS-IDKUNDRF-OLD              
270800     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
270900     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF                
271000                                                                          
271100     MOVE ORAD-IDKUNDRF-RO                TO WS-IDKUNDRF-OLD              
271200     MOVE WS-IDORDNR5-OLD                 TO WS-IDORDNR7-NEW              
271300     MOVE WS-IDKUNDRF-NEW                 TO OBKR-IDKUNDRF-RO             
271400                                                                          
271500     MOVE ORAD-IDLEVNR                    TO OBKR-IDLEVNR                 
271600     MOVE ORAD-IDLOPNR-RO                 TO OBKR-IDLOPNR-RO              
271700     MOVE ORAD-IDSYSTEM                   TO OBKR-IDSYSTEM                
271800     MOVE W-IDDC                         TO OBKR-IDDC                     
271900     MOVE ORAD-KDDSP                      TO OBKR-KDDSP                   
272000     MOVE 0                               TO OBKR-KDERS                   
272100     MOVE ORAD-KDOI                       TO OBKR-KDOI                    
272200     MOVE ORAD-CLEARGROUP                 TO OBKR-CLEARGROUP              
272300     MOVE ORAD-KDKVBRYT                   TO OBKR-KDKVBRYT                
272400     MOVE ORAD-KDPRTYP                    TO OBKR-KDPRTYP                 
272500     MOVE 0                               TO OBKR-KDTPOTYP                
272600     MOVE ORAD-KDVRINFO                   TO OBKR-KDVRINFO                
272700                                                                          
272800     MOVE WS-ORAD-KVANNANT                TO OBKR-KVANNANT                
272900     MOVE ORAD-KVAVBART                   TO OBKR-KVAVBART                
273000     MOVE ORAD-KVBEART                    TO OBKR-KVBEART                 
273100                                             OBKR-KVBEART-Q               
273200     MOVE 0                               TO OBKR-KVBEART-TILLK           
273300     MOVE 0                               TO OBKR-KVPREAVB                
273400     MOVE 0                               TO OBKR-KVPRERO                 
273500     IF DCS-CDC                                                           
273600       MOVE WS-CLAG-KVQPACK-1             TO OBKR-KVQPACK                 
273700     ELSE                                                                 
273800       MOVE ZERO                          TO OBKR-KVQPACK                 
273900     END-IF                                                               
274000                                                                          
274100     MOVE 0                               TO OBKR-KVRO                    
274200     MOVE ZERO                            TO OBKR-TIRODAT                 
274300     MOVE ORAD-KVSLATT                    TO OBKR-KVSLATT                 
274400     MOVE ORAD-PRARTNTO                   TO OBKR-PRARTNTO                
274500     MOVE ORAD-DEAL-PR-LINE               TO OBKR-DEAL-PR-LINE            
274600     MOVE 0                               TO OBKR-PRBPRIS                 
274700     MOVE ORAD-REKSIFFR                   TO OBKR-REKSIFFR                
274800     MOVE 0                               TO OBKR-REKSIFFR-TILLK          
274900     MOVE 0                               TO OBKR-RERF-RAD                
275000     MOVE +0                              TO OBKR-TIDISPIN                
275100     MOVE KORD-TIORDREG                   TO OBKR-TIORDREG                
275200                                             WS-DATUM-9KOMPL              
275300     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
275400     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
275500     END-COMPUTE                                                          
275600     MOVE ORAD-TIPRIS                     TO OBKR-TIPRIS                  
275700                                                                          
275800     MOVE MSGI-TILOKDAT                   TO OBKR-TIREGDAT                
275900     MOVE WS-TIHHMMSS                     TO OBKR-TIREGTID                
276000     MOVE KORD-TIORDREG                   TO WS-DATUM-9KOMPL              
276100     MOVE FUNCTION CURRENT-DATE (1:2)     TO WS-DATUM-9KOMPL (1:2)        
276200                                                                          
276300     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
276400     END-COMPUTE                                                          
276500     MOVE 0                                TO OBKR-TITPO                  
276600     MOVE ORAD-KDFRAKT                     TO OBKR-KDFRAKT                
276700     MOVE ORAD-KDORDKL                     TO OBKR-KDORDKL                
276800     MOVE SPACE                            TO OBKR-IDBIL                  
276900                                                                          
277000     MOVE OHUV-KDORDTYP-LDC                TO OBKR-KDORDTYP-LDC           
277100     MOVE OHUV-TIREPDAT                    TO OBKR-TIREPDAT               
277200     MOVE ORAD-IDKUNDRF-WIP                TO OBKR-IDKUNDRF-WIP           
277300     MOVE ZERO                             TO OBKR-TIDLEVDAT              
277400     MOVE ORAD-PRAVCOST                    TO OBKR-PRAVCOST               
277500     MOVE ORAD-KDVALISO                    TO OBKR-KDVALISO               
277600                                                                          
277700     PERFORM IMS-ISRT-ORQM01                                              
277800     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
277900        ADD +1                             TO OBKR-IDLOPNR                
278000        PERFORM IMS-ISRT-ORQM01                                           
278100     END-PERFORM                                                          
278200     .                                                                    
278300     EJECT                                                                
278400 S12C-GENERERA-2109-TRANS  SECTION.                                       
278500*                                                                         
278600*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
278700     MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                             
278800     IF NOT BYT03-OBJEKT                                                  
278900                                                                          
279000        MOVE 2109-IX            TO 2109-MID2-KVANTART                     
279100        MOVE ORAD-IDARTNR       TO 2109-MID2-IDARTNR (2109-IX)            
279200        MOVE OHUV-IDDC-PRIM     TO 2109-MID2-IDDC (2109-IX)               
279300        MOVE ORAD-KDOI          TO 2109-MID2-KDOI (2109-IX)               
279400        MOVE ORAD-CLEARGROUP    TO 2109-MID2-CLEARGROUP(2109-IX)          
279500        MOVE '-'                TO 2109-MID2-KDTECKEN (2109-IX)           
279600        MOVE WS-ORAD-KVANNANT   TO 2109-MID2-KVOI (2109-IX)               
279700        MOVE KORD-TIORDREG      TO 2109-MID2-TIUPPDAT (2109-IX)           
279800                                                                          
279900        ADD +1                  TO 2109-IX                                
280000        IF 2109-IX > 2109-IX-MAX                                          
280100          PERFORM S12CA-STARTA-2109                                       
280200        END-IF                                                            
280300     END-IF                                                               
280400     .                                                                    
280500     EJECT                                                                
280600 S12CA-STARTA-2109 SECTION.                                               
280700                                                                          
280800     COMPUTE 2109-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                
280900                                                                          
281000     PERFORM IMS-PURG-ALT-MSG-2109                                        
281100                                                                          
281200     MOVE SPACE              TO 2109-MID2-W2I10902                        
281300     MOVE +1                 TO 2109-IX                                   
281400     .                                                                    
281500     EJECT                                                                
281600 S13-ISRT-SALDOLOGG SECTION.                                              
281700     PERFORM IMS-ISRT-WDL901                                              
281800     IF SEGMENT-FINNS-REDAN                                               
281900       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
282000         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
282100         PERFORM IMS-ISRT-WDL901                                          
282200       END-PERFORM                                                        
282300     END-IF                                                               
282400     .                                                                    
282500     EJECT                                                                
282600                                                                          
282700 S14-FLYTTA-SALDOLOGG-DATA SECTION.                                       
282800     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
282900     ACCEPT WS-TID                   FROM TIME                            
283000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
283100     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
283200     MOVE 9                   TO LOGG-IDSEKVNR                            
283300     MOVE 'OUTB'              TO LOGG-IDHUVTYP                            
283400     MOVE 'CAN'               TO LOGG-IDSUBTYP                            
283500     MOVE 'W4035900'          TO LOGG-IDPGM                               
283600     MOVE '4359'              TO LOGG-IDTRANS                             
283700     MOVE MSG-SIGNON-USERID   TO LOGG-IDUSER                              
283800     MOVE SPACE               TO LOGG-REF                                 
283900     MOVE WS-IDDISTR          TO LOGG-IDDISTR                             
284000     MOVE WS-IDKUNDNR-NUM     TO LOGG-IDKUNDNR                            
284100     MOVE WS-IDORDNR          TO LOGG-IDORDNR5                            
284200     MOVE WS-IDPRODNR         TO LOGG-IDPRODNR                            
284300     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
284400     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
284500     MOVE '00000000'          TO LOGG-DAREGDAT-LADD                       
284600     .                                                                    
284700     EJECT                                                                
284800 S20-FINN-INTERVALL SECTION.                                              
284900                                                                          
285000     PERFORM IMS-GU-WDM211                                                
285100     IF SEGMENT-FINNS                                                     
285200       PERFORM IMS-GNP-WDM221                                             
285300       PERFORM UNTIL SEGMENT-SAKNAS                                       
285400         IF  KORD-IDDISTR > KMRK-IDDISTR-TOM                              
285500         OR  KORD-IDDISTR < KMRK-IDDISTR-FOM                              
285600           CONTINUE                                                       
285700         ELSE                                                             
285800           IF  KORD-IDDISTR  = KMRK-IDDISTR-TOM                           
285900           AND KORD-IDKUNDNR > KMRK-IDKUNDNR-TOM                          
286000             CONTINUE                                                     
286100           ELSE                                                           
286200             IF  KORD-IDDISTR  = KMRK-IDDISTR-FOM                         
286300             AND KORD-IDKUNDNR < KMRK-IDKUNDNR-FOM                        
286400               CONTINUE                                                   
286500             ELSE                                                         
286600               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
286700               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
286800               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
286900               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
287000             END-IF                                                       
287100           END-IF                                                         
287200         END-IF                                                           
287300         PERFORM IMS-GNP-WDM221                                           
287400       END-PERFORM                                                        
287500     END-IF                                                               
287600     .                                                                    
287700     EJECT                                                                
287800                                                                          
287900* IMS SEKTIONER                                                           
288000     SKIP3                                                                
288100 IMS-GET-MSG SECTION.                                                     
288200                                                                          
288300     MOVE '  QC' TO GODK-STATUSKODER                                      
288400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
288500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
288600     PERFORM IMS-STATUSKONTROLL                                           
288700     SKIP3                                                                
288800     .                                                                    
288900 IMS-INSERT-MSG SECTION.                                                  
289000                                                                          
289100     IF NOT ENGLISH-TEXT                                                  
289200       MOVE '0' TO MFS-KDHUVOMR                                           
289300     END-IF                                                               
289400*                                                                         
289500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
289600     MOVE SPACE TO GODK-STATUSKODER                                       
289700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
289800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
289900     PERFORM IMS-STATUSKONTROLL                                           
290000     SKIP3                                                                
290100     .                                                                    
290200 IMS-CHANGE-ALTMSG       SECTION.                                         
290300                                                                          
290400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
290500     MOVE '  ' TO GODK-STATUSKODER                                        
290600     CALL CBLTDLI USING CHNG                                              
290700                          ALT-PCB                                         
290800                          MSG-KDTRANS-1                                   
290900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
291000     PERFORM IMS-STATUSKONTROLL                                           
291100     .                                                                    
291200     EJECT                                                                
291300 IMS-INSERT-ALTMSG SECTION.                                               
291400     SKIP2                                                                
291500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
291600     MOVE SPACE TO GODK-STATUSKODER                                       
291700     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
291800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
291900     PERFORM IMS-STATUSKONTROLL                                           
292000     SKIP2                                                                
292100     .                                                                    
292200 IMS-INSERT-4397-TRANS SECTION.                                           
292300     SKIP2                                                                
292400*                                                                         
292500     MOVE LOW-VALUE TO 4397-Z1 4397-Z2                                    
292600     MOVE SPACE TO GODK-STATUSKODER                                       
292700     CALL CBLTDLI USING ISRT 4397-PCB 4397-IO-AREA                        
292800     MOVE 4397-STATUS-CODE TO STATUS-WS                                   
292900     PERFORM IMS-STATUSKONTROLL                                           
293000     .                                                                    
293100     SKIP2                                                                
293200 IMS-PURG-4397-TRANS SECTION.                                             
293300                                                                          
293400     MOVE SPACE TO GODK-STATUSKODER                                       
293500     CALL CBLTDLI USING PURG 4397-PCB 4397-IO-AREA                        
293600     MOVE 4397-STATUS-CODE TO STATUS-WS                                   
293700     PERFORM IMS-STATUSKONTROLL                                           
293800     .                                                                    
293900     SKIP2                                                                
294000 IMS-PURG-ALT-MSG-2109 SECTION.                                           
294100     MOVE LOW-VALUE TO 2109-Z1 2109-Z2                                    
294200     MOVE SPACE TO GODK-STATUSKODER                                       
294300     CALL CBLTDLI USING PURG 2109-PCB W-PROG-TO-PROG-SW-1                 
294400     MOVE 2109-STATUS-CODE TO STATUS-WS                                   
294500     PERFORM IMS-STATUSKONTROLL                                           
294600     .                                                                    
294700     EJECT                                                                
294800 IMS-GHU-WDE401 SECTION.                                                  
294900     MOVE 'STA IMS-GHU-WDE401'     TO WS-IMS-AVBROTT                      
295000                                                                          
295100     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
295200            DELIMITED BY SIZE INTO SSA1                                   
295300     MOVE '    ' TO GODK-STATUSKODER                                      
295400     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-E401 SSA1                     
295500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
295600     PERFORM IMS-STATUSKONTROLL                                           
295700     SKIP2                                                                
295800     .                                                                    
295900 IMS-REPL-WDE401 SECTION.                                                 
296000     MOVE 'STA IMS-REPL-WDE401'    TO WS-IMS-AVBROTT                      
296100                                                                          
296200     MOVE '  '   TO GODK-STATUSKODER                                      
296300     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E401                         
296400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
296500     PERFORM IMS-STATUSKONTROLL                                           
296600     SKIP2                                                                
296700     .                                                                    
296800 IMS-GHNP-WDE411-FIRST SECTION.                                           
296900     MOVE 'STA IMS-GHNP-WDE411-F'   TO WS-IMS-AVBROTT                     
297000                                                                          
297100     STRING 'WDE411  *F(IDPURAD >=' W-IDPURAD-MIN-X                       
297200                      '&IDPURAD <=' W-IDPURAD-MAX-X ')'                   
297300            DELIMITED BY SIZE INTO SSA1                                   
297400     MOVE '  GE' TO GODK-STATUSKODER                                      
297500     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E411 SSA1                    
297600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
297700                              STATUS-WS-E411                              
297800     PERFORM IMS-STATUSKONTROLL                                           
297900     SKIP2                                                                
298000     .                                                                    
298100 IMS-GHNP-WDE411-LAST SECTION.                                            
298200     MOVE 'STA IMS-GHNP-WDE411-L'   TO WS-IMS-AVBROTT                     
298300                                                                          
298400     STRING 'WDE411  *L(IDPURAD >=' W-IDPURAD-MIN-X                       
298500                      '&IDPURAD <=' W-IDPURAD-MAX-X ')'                   
298600            DELIMITED BY SIZE INTO SSA1                                   
298700     MOVE '  GE' TO GODK-STATUSKODER                                      
298800     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E411 SSA1                    
298900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
299000                              STATUS-WS-E411                              
299100     PERFORM IMS-STATUSKONTROLL                                           
299200     SKIP2                                                                
299300     .                                                                    
299400 IMS-GHNP-WDE411 SECTION.                                                 
299500     MOVE 'STA IMS-GHNP-WDE411'     TO WS-IMS-AVBROTT                     
299600                                                                          
299700     STRING 'WDE411  (IDPURAD  =' W-WDE411-IDPURAD-X ')'                  
299800            DELIMITED BY SIZE INTO SSA1                                   
299900     MOVE '  GE' TO GODK-STATUSKODER                                      
300000     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E411 SSA1                    
300100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
300200                              STATUS-WS-E411                              
300300     PERFORM IMS-STATUSKONTROLL                                           
300400     SKIP2                                                                
300500     .                                                                    
300600 IMS-GHNP-WDE411-MIN-MAX SECTION.                                         
300700     MOVE 'STA IMS-GHNP-WDE411-MIN-MAX' TO WS-IMS-AVBROTT                 
300800                                                                          
300900     STRING 'WDE411  (IDPURAD >=' W-IDPURAD-MIN-X                         
301000                    '&IDPURAD <=' W-IDPURAD-MAX-X ')'                     
301100            DELIMITED BY SIZE INTO SSA1                                   
301200     MOVE '  GE' TO GODK-STATUSKODER                                      
301300     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E411 SSA1                    
301400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
301500                              STATUS-WS-E411                              
301600     PERFORM IMS-STATUSKONTROLL                                           
301700     SKIP2                                                                
301800     .                                                                    
301900 IMS-GU-WDE4E1 SECTION.                                                   
302000                                                                          
302100     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN                          
302200                    '&WDE4E1KY<=' W-WDE4E1KY-MAX                          
302300                    '&IDPLKLST =' W-IDPLKLST-X ')'                        
302400          DELIMITED BY SIZE INTO SSA1                                     
302500     MOVE '  GE' TO GODK-STATUSKODER                                      
302600     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-E4E1 SSA1                     
302700     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
302800     PERFORM IMS-STATUSKONTROLL                                           
302900     .                                                                    
303000     SKIP3                                                                
303100 IMS-REPL-WDE411  SECTION.                                                
303200     MOVE 'STA IMS-REPL-WDE411'           TO WS-IMS-AVBROTT               
303300                                                                          
303400     MOVE '    ' TO GODK-STATUSKODER                                      
303500     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E411                         
303600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
303700     PERFORM IMS-STATUSKONTROLL                                           
303800     .                                                                    
303900     EJECT                                                                
304000 IMS-GHU-WDE601 SECTION.                                                  
304100     MOVE 'STA IMS-GHU-WDE601'            TO WS-IMS-AVBROTT               
304200                                                                          
304300     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
304400            DELIMITED BY SIZE INTO SSA1                                   
304500     MOVE '  GE' TO GODK-STATUSKODER                                      
304600     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-E601 SSA1                     
304700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
304800     PERFORM IMS-STATUSKONTROLL                                           
304900     SKIP2                                                                
305000     .                                                                    
305100 IMS-REPL-WDE601   SECTION.                                               
305200     MOVE 'STA IMS-REPL-WDE601'           TO WS-IMS-AVBROTT               
305300                                                                          
305400     MOVE '  '   TO GODK-STATUSKODER                                      
305500     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
305600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
305700     PERFORM IMS-STATUSKONTROLL                                           
305800     .                                                                    
305900     EJECT                                                                
306000 IMS-GU-ORQI01 SECTION.                                                   
306100     MOVE 'STA IMS-GU-ORQI01'       TO WS-IMS-AVBROTT                     
306200                                                                          
306300     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
306400            DELIMITED BY SIZE INTO SSA1                                   
306500     MOVE '  '   TO GODK-STATUSKODER                                      
306600     CALL CBLTDLI USING GU      ORQI-PCB DLI-IO-WDQ201 SSA1               
306700     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
306800     PERFORM IMS-STATUSKONTROLL                                           
306900     .                                                                    
307000     SKIP2                                                                
307100 IMS-GU-ORQI01-GE SECTION.                                                
307200     MOVE 'STA IMS-GU-ORQI01'       TO WS-IMS-AVBROTT                     
307300                                                                          
307400     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
307500            DELIMITED BY SIZE INTO SSA1                                   
307600     MOVE '  GE'   TO GODK-STATUSKODER                                    
307700     CALL CBLTDLI USING GU      ORQI-PCB DLI-IO-WDQ201 SSA1               
307800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
307900     PERFORM IMS-STATUSKONTROLL                                           
308000     .                                                                    
308100     SKIP2                                                                
308200                                                                          
308300 IMS-GU-WDQ301   SECTION.                                                 
308400     STRING 'WLORQA01(WDQ301KY =' W-WDQ301-KEY-X ')'                      
308500            DELIMITED BY SIZE INTO SSA1                                   
308600     MOVE '  GE' TO GODK-STATUSKODER                                      
308700     CALL CBLTDLI USING GU    ORQA-PCB ODEL-WDQ301 SSA1                   
308800     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
308900     PERFORM IMS-STATUSKONTROLL                                           
309000     .                                                                    
309100 IMS-GU-ORDP01   SECTION.                                                 
309200     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X ')'                        
309300            DELIMITED BY SIZE INTO SSA1                                   
309400     MOVE '  GE' TO GODK-STATUSKODER                                      
309500     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-AREA4 SSA1                     
309600     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
309700     PERFORM IMS-STATUSKONTROLL                                           
309800     .                                                                    
309900     EJECT                                                                
310000                                                                          
310100 IMS-ISRT-ZZAC01      SECTION.                                            
310200*    WDG601                                                               
310300     MOVE 'WLZZAC01'  TO SSA1                                             
310400     MOVE '  II' TO GODK-STATUSKODER                                      
310500     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA3 SSA1                   
310600     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
310700     PERFORM IMS-STATUSKONTROLL                                           
310800     .                                                                    
310900     EJECT                                                                
311000 IMS-GHU-ARTM-ART SECTION.                                                
311100     STRING 'WLARTM01(IDARTNR  =' W-WDK901-IDARTNR-X ')'                  
311200            DELIMITED BY SIZE INTO SSA1                                   
311300     MOVE '  '   TO GODK-STATUSKODER                                      
311400     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA8 SSA1                    
311500     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
311600     PERFORM IMS-STATUSKONTROLL                                           
311700     .                                                                    
311800     SKIP2                                                                
311900 IMS-REPL-ARTM SECTION.                                                   
312000     MOVE '  '        TO GODK-STATUSKODER                                 
312100     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA8                        
312200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
312300     PERFORM IMS-STATUSKONTROLL                                           
312400     .                                                                    
312500     EJECT                                                                
312600 IMS-GHU-ARTC01 SECTION.                                                  
312700     STRING 'WLARTC01(IDARTNR  =' W-WDK601-IDARTNR-X ')'                  
312800            DELIMITED BY SIZE INTO SSA1                                   
312900     MOVE '  '   TO GODK-STATUSKODER                                      
313000     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
313100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
313200     PERFORM IMS-STATUSKONTROLL                                           
313300     SKIP2                                                                
313400     .                                                                    
313500 IMS-GHNP-ARTC11 SECTION.                                                 
313600     STRING 'WLARTC11(KDSEGKEY =' W-WDK611-KDSEGKEY-X ')'                 
313700            DELIMITED BY SIZE INTO SSA1                                   
313800     MOVE '  GE'   TO GODK-STATUSKODER                                    
313900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
314000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
314100     PERFORM IMS-STATUSKONTROLL                                           
314200     SKIP2                                                                
314300     .                                                                    
314400 IMS-GHU-ARTC11 SECTION.                                                  
314500     STRING 'WLARTC01(IDARTNR  =' W-WDK601-IDARTNR-X ')'                  
314600            DELIMITED BY SIZE INTO SSA1                                   
314700     MOVE 'WLARTC11 '           TO SSA2                                   
314800     MOVE '    '   TO GODK-STATUSKODER                                    
314900     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
315000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
315100     PERFORM IMS-STATUSKONTROLL                                           
315200     SKIP2                                                                
315300     .                                                                    
315400 IMS-REPL-ARTC       SECTION.                                             
315500     MOVE '  '   TO GODK-STATUSKODER                                      
315600     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
315700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
315800     PERFORM IMS-STATUSKONTROLL                                           
315900     .                                                                    
316000     EJECT                                                                
316100 IMS-GHU-ARTS11 SECTION.                                                  
316200     STRING 'WLARTS01(IDARTNR  =' W-WDK701-IDARTNR-X ')'                  
316300            DELIMITED BY SIZE INTO SSA1                                   
316400     STRING 'WLARTS11(IDDC     =' W-WDK711-IDDC-X ')'                     
316500            DELIMITED BY SIZE INTO SSA2                                   
316600     MOVE '  '   TO GODK-STATUSKODER                                      
316700     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA SSA1 SSA2                
316800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
316900     PERFORM IMS-STATUSKONTROLL                                           
317000     SKIP2                                                                
317100     .                                                                    
317200 IMS-REPL-ARTS       SECTION.                                             
317300     MOVE '  '   TO GODK-STATUSKODER                                      
317400     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA                         
317500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
317600     PERFORM IMS-STATUSKONTROLL                                           
317700     .                                                                    
317800     EJECT                                                                
317900 IMS-GU-XXDJ-ROT  SECTION.                                                
318000     SKIP2                                                                
318100     STRING 'WLXXDJ01(WDGXKEY  =' W-4305-X ')'                            
318200            DELIMITED BY SIZE INTO SSA1                                   
318300     MOVE '  GE' TO GODK-STATUSKODER                                      
318400     CALL CBLTDLI USING GU XXDJ-PCB DLI-IO-AREA SSA1                      
318500     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
318600     PERFORM IMS-STATUSKONTROLL                                           
318700     SKIP2                                                                
318800     .                                                                    
318900 IMS-GHNP-XXDJ-LASNING SECTION.                                           
319000     SKIP2                                                                
319100     STRING 'WLXXDJ11(WDGXKEY  =' W-4306-X ')'                            
319200            DELIMITED BY SIZE INTO SSA1                                   
319300     MOVE '  GE' TO GODK-STATUSKODER                                      
319400     CALL CBLTDLI USING GHNP XXDJ-PCB DLI-IO-AREA SSA1                    
319500     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
319600     PERFORM IMS-STATUSKONTROLL                                           
319700     SKIP2                                                                
319800     .                                                                    
319900 IMS-REPL-XXDJ-LASNING SECTION.                                           
320000     SKIP2                                                                
320100     MOVE '  ' TO GODK-STATUSKODER                                        
320200     CALL CBLTDLI USING REPL XXDJ-PCB DLI-IO-AREA                         
320300     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
320400     PERFORM IMS-STATUSKONTROLL                                           
320500     SKIP2                                                                
320600     .                                                                    
320700 IMS-ISRT-XXDJ-LASNING SECTION.                                           
320800     SKIP2                                                                
320900     STRING 'WLXXDJ01(WDGXKEY  =' W-4305-X ')'                            
321000            DELIMITED BY SIZE INTO SSA1                                   
321100     MOVE 'WLXXDJ11 ' TO SSA2                                             
321200     MOVE '  ' TO GODK-STATUSKODER                                        
321300     CALL CBLTDLI USING ISRT XXDJ-PCB DLI-IO-AREA SSA1 SSA2               
321400     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
321500     PERFORM IMS-STATUSKONTROLL                                           
321600     .                                                                    
321700     EJECT                                                                
321800 IMS-GHU-XXDJ-4308     SECTION.                                           
321900     SKIP2                                                                
322000     STRING 'WLXXDJ21(WDGXKEY >=' W-4308-X ')'                            
322100            DELIMITED BY SIZE INTO SSA1                                   
322200     MOVE '  GE' TO GODK-STATUSKODER                                      
322300     CALL CBLTDLI USING GHU    XXDJ-PCB DLI-IO-AREA SSA1                  
322400     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
322500     PERFORM IMS-STATUSKONTROLL                                           
322600     SKIP2                                                                
322700     .                                                                    
322800 IMS-DLET-XXDJ-LASNING SECTION.                                           
322900     SKIP2                                                                
323000     MOVE '  ' TO GODK-STATUSKODER                                        
323100     CALL CBLTDLI USING DLET XXDJ-PCB DLI-IO-AREA                         
323200     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
323300     PERFORM IMS-STATUSKONTROLL                                           
323400     SKIP2                                                                
323500     .                                                                    
323600 IMS-ISRT-XXDJ-4308    SECTION.                                           
323700     SKIP2                                                                
323800     MOVE 'WLXXDJ21 ' TO SSA1                                             
323900     MOVE '  ' TO GODK-STATUSKODER                                        
324000     CALL CBLTDLI USING ISRT XXDJ-PCB DLI-IO-AREA    SSA1                 
324100     MOVE XXDJ-STATUS-CODE TO STATUS-WS                                   
324200     PERFORM IMS-STATUSKONTROLL                                           
324300     SKIP2                                                                
324400     .                                                                    
324500 IMS-ISRT-AUTFAKTURA-ROT SECTION.                                         
324600     STRING 'WLXXDV01(WDGXKEY  =' W-4726-X ')'                            
324700            DELIMITED BY SIZE INTO SSA1                                   
324800     MOVE   'WLXXDV11 '         TO SSA2                                   
324900     MOVE '  '     TO GODK-STATUSKODER                                    
325000     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA2 SSA1 SSA2              
325100     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
325200     PERFORM IMS-STATUSKONTROLL                                           
325300     .                                                                    
325400     EJECT                                                                
325500 IMS-ISRT-AUTFAKTURA     SECTION.                                         
325600     STRING 'WLXXDV01(WDGXKEY  =' W-4726-X ')'                            
325700            DELIMITED BY SIZE INTO SSA1                                   
325800     STRING 'WLXXDV11(WDGXKEY  =' W-WDGX11-WDGXKEY-X ')'                  
325900            DELIMITED BY SIZE INTO SSA2                                   
326000     MOVE 'WLXXDV21 ' TO SSA3                                             
326100     MOVE '  IIGE' TO GODK-STATUSKODER                                    
326200     CALL CBLTDLI USING ISRT XXDV-PCB DLI-IO-AREA2                        
326300                                        SSA1 SSA2 SSA3                    
326400     MOVE XXDV-STATUS-CODE TO STATUS-WS                                   
326500     PERFORM IMS-STATUSKONTROLL                                           
326600     SKIP2                                                                
326700     .                                                                    
326800 IMS-ISRT-450511         SECTION.                                         
326900     STRING 'WL450501(WDGXKEY  =' W-WDGX-4505-KEY-X ')'                   
327000             DELIMITED BY SIZE INTO SSA1                                  
327100     MOVE 'WL450511 '           TO SSA2                                   
327200     MOVE '  ' TO GODK-STATUSKODER                                        
327300     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA5 SSA1 SSA2              
327400     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
327500     PERFORM IMS-STATUSKONTROLL                                           
327600     .                                                                    
327700     SKIP2                                                                
327800 IMS-ISRT-ORQM01         SECTION.                                         
327900     MOVE 'WLORQM01 '           TO SSA1                                   
328000     MOVE '  II' TO GODK-STATUSKODER                                      
328100     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA6 SSA1                   
328200     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
328300     PERFORM IMS-STATUSKONTROLL                                           
328400     .                                                                    
328500     EJECT                                                                
328600                                                                          
328700 IMS-GU-WDM211 SECTION.                                                   
328800                                                                          
328900     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
329000          DELIMITED BY SIZE INTO SSA1                                     
329100     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
329200          DELIMITED BY SIZE INTO SSA2                                     
329300     MOVE '  GE'              TO GODK-STATUSKODER                         
329400     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
329500     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
329600     PERFORM IMS-STATUSKONTROLL                                           
329700     .                                                                    
329800                                                                          
329900 IMS-GNP-WDM221 SECTION.                                                  
330000                                                                          
330100     MOVE 'WDM221 '           TO SSA1                                     
330200     MOVE '    GE'            TO GODK-STATUSKODER                         
330300     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
330400     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
330500     PERFORM IMS-STATUSKONTROLL                                           
330600     .                                                                    
330700                                                                          
330800 IMS-GHU-WDM211 SECTION.                                                  
330900                                                                          
331000     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
331100          DELIMITED BY SIZE INTO SSA1                                     
331200     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
331300          DELIMITED BY SIZE INTO SSA2                                     
331400     MOVE '  GE'              TO GODK-STATUSKODER                         
331500     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
331600     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
331700     PERFORM IMS-STATUSKONTROLL                                           
331800     .                                                                    
331900                                                                          
332000 IMS-REPL-WDM211 SECTION.                                                 
332100                                                                          
332200     MOVE '  '             TO GODK-STATUSKODER                            
332300     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
332400     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
332500     PERFORM IMS-STATUSKONTROLL                                           
332600     .                                                                    
332700     EJECT                                                                
332800 IMS-GHU-WDM221 SECTION.                                                  
332900                                                                          
333000     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
333100          DELIMITED BY SIZE INTO SSA1                                     
333200     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
333300          DELIMITED BY SIZE INTO SSA2                                     
333400     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
333500          DELIMITED BY SIZE INTO SSA3                                     
333600     MOVE '  GE' TO GODK-STATUSKODER                                      
333700     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
333800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
333900     PERFORM IMS-STATUSKONTROLL                                           
334000     .                                                                    
334100                                                                          
334200 IMS-REPL-WDM221 SECTION.                                                 
334300                                                                          
334400     MOVE '  '             TO GODK-STATUSKODER                            
334500     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
334600     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
334700     PERFORM IMS-STATUSKONTROLL                                           
334800     .                                                                    
334900     EJECT                                                                
335000 IMS-ISRT-WDL901 SECTION.                                                 
335100                                                                          
335200     MOVE 'WLLOGA01 ' TO SSA1                                             
335300     MOVE '  II' TO GODK-STATUSKODER                                      
335400     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
335500     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
335600     PERFORM IMS-STATUSKONTROLL                                           
335700     .                                                                    
335800 IMS-GHN-WDA6B SECTION.                                                   
335900     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
336000                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
336100            DELIMITED BY SIZE INTO SSA1                                   
336200     MOVE '  GEGB'               TO GODK-STATUSKODER                      
336300     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-WDA6 SSA1           
336400     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
336500     PERFORM IMS-STATUSKONTROLL                                           
336600     .                                                                    
336700 IMS-REPL-WDA6B SECTION.                                                  
336800     MOVE 'WDA601  '           TO SSA1                                    
336900     MOVE '    '               TO GODK-STATUSKODER                        
337000     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-WDA6 SSA1            
337100     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
337200     PERFORM IMS-STATUSKONTROLL                                           
337300     .                                                                    
337400     EJECT                                                                
337500 IMS-GU-WDB601    SECTION.                                                
337600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
337700          DELIMITED BY SIZE INTO SSA1                                     
337800     MOVE '  GE' TO GODK-STATUSKODER                                      
337900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
338000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
338100     PERFORM IMS-STATUSKONTROLL                                           
338200     IF SEGMENT-SAKNAS                                                    
338300         MOVE SPACE TO DCS-KDDC                                           
338400     END-IF                                                               
338500     .                                                                    
338600     EJECT                                                                
338700 IMS-GU-WDE4F1-PLK SECTION.                                               
338800     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
338900                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X                        
339000                    '&IDPLKLST =' W-IDPLKLST-X ')'                        
339100          DELIMITED BY SIZE INTO SSA1                                     
339200     MOVE '  GE' TO GODK-STATUSKODER                                      
339300     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-WDE4F1 SSA1                   
339400     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
339500     PERFORM IMS-STATUSKONTROLL                                           
339600     .                                                                    
339700     SKIP3                                                                
339800 IMS-GHU-WDE611-DEF SECTION.                                              
339900                                                                          
340000     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
340100          DELIMITED BY SIZE INTO SSA1                                     
340200     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-E6-X                          
340300                    '&KDKOLSTA =' W-KDKOLSTA-X ')'                        
340400          DELIMITED BY SIZE INTO SSA2                                     
340500     MOVE '  GE' TO GODK-STATUSKODER                                      
340600     CALL CBLTDLI USING GHU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2              
340700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
340800     PERFORM IMS-STATUSKONTROLL                                           
340900     .                                                                    
341000     SKIP3                                                                
341100 IMS-DLET-WDE611    SECTION.                                              
341200                                                                          
341300     MOVE '    ' TO GODK-STATUSKODER                                      
341400     CALL CBLTDLI USING DLET WDE6-PCB DLI-IO-WDE611                       
341500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
341600     PERFORM IMS-STATUSKONTROLL                                           
341700*    ADD +2 TO UPDATE-IX                                                  
341800     .                                                                    
341900     SKIP3                                                                
342000 IMS-GHNP-WDE421  SECTION.                                                
342100     MOVE 'WDE421' TO SSA1                                                
342200     MOVE '  '     TO GODK-STATUSKODER                                    
342300     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E421 SSA1                    
342400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
342500     PERFORM IMS-STATUSKONTROLL                                           
342600     SKIP3                                                                
342700     .                                                                    
342800 IMS-DLET-WDE421  SECTION.                                                
342900     MOVE '    '   TO GODK-STATUSKODER                                    
343000     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-E421                         
343100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
343200     PERFORM IMS-STATUSKONTROLL                                           
343300*    ADD +2 TO UPDATE-IX                                                  
343400     .                                                                    
343500     SKIP2                                                                
343600 IMS-GHNP-WDE411-BACKOUT SECTION.                                         
343700     MOVE 'STA IMS-GHNP-WDE411-B'   TO WS-IMS-AVBROTT                     
343800                                                                          
343900     STRING 'WDE411  (IDPURAD >=' W-IDPURAD-MIN-X                         
344000                    '&IDPURAD <=' W-IDPURAD-MAX-X ')'                     
344100          DELIMITED BY SIZE INTO SSA1                                     
344200     MOVE '  GE' TO GODK-STATUSKODER                                      
344300     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E411 SSA1                    
344400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
344500     PERFORM IMS-STATUSKONTROLL                                           
344600     .                                                                    
344700     SKIP3                                                                
344800 DB2-SELECT-TP4TRAN     SECTION.                                          
344900     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
345000                                                                          
345100     MOVE 000100 TO GODK-SQLCODEKODER                                     
345200                                                                          
345300     EXEC SQL                                                             
345400           SELECT  DISTINCT                                               
345500                   IDDC_REC                                               
345600                                                                          
345700           INTO   :TP4TRAN-IDDC-REC                                       
345800                                                                          
345900           FROM    TP4TRAN                                                
346000                                                                          
346100           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
346200     END-EXEC                                                             
346300                                                                          
346400     MOVE SQLCODE TO SQLCODE-WS                                           
346500     PERFORM DB2-STATUSKONTROLL                                           
346600     .                                                                    
346700     EJECT                                                                
346800 IMS-STATUSKONTROLL SECTION.                                              
346900     SET STATUS-IX TO 1                                                   
347000     SEARCH GODK-STATUS                                                   
347100       AT END                                                             
347200         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
347300           DELIMITED BY SIZE INTO FELTEXT                                 
347400         CALL FELLOG                                                      
347500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
347600         CONTINUE                                                         
347700     END-SEARCH.                                                          
347800     EJECT                                                                
347900 DB2-STATUSKONTROLL  SECTION.                                             
348000                                                                          
348100     SET SQLCODE-IX TO 1                                                  
348200     SEARCH GODK-SQLCODE                                                  
348300       AT END                                                             
348400          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
348500          DELIMITED BY SIZE INTO FELTEXT                                  
348600          CALL ABEND USING RKOD-ABEND-DB2                                 
348700       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
348800     END-SEARCH                                                           
348900     .                                                                    
