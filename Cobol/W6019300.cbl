000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6019300.                                                
000300 AUTHOR.         GUNNAR LARSSON IDK.                                      
000400 DATE-WRITTEN.   92/03/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERAR R32-KOPPLINGAR FRÅN INLEVERANSSYSTEMET                
000900*        TILL ANDRA SYSTEM.                                               
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR ÄVEN INLEVERANS AV LOKALA ARTIKLAR         
001200*        FÖR NDC:ERNA. DÅ UPPDATERAS AK-SALDO OCH LAGERSALDO PÅ           
001300*        WDK7 SAMT HISTORIK PÅ WDL6. ÄVEN EKONOMISKA TRANSAR              
001400*        SKAPAS PÅ ANNAT SÄTT.                                            
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001700*        PROGRAMMET LÄSER      W6INLC (W6D1B)                             
001800*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001900*        PROGRAMMET UPPDATERAR WDK7                                       
002000*        PROGRAMMET UPPDATERAR WLINLB (WDD9)                              
002100*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
002200*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
002300*        PROGRAMMET UPPDATERAR WLXXBW (WDR5)                              
002400*        PROGRAMMET UPPDATERAR WL4505                                     
002500*        PROGRAMMET UPPDATERAR WDL6                                       
002600*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
002700*        PROGRAMMET UPPDATERAR WLFILB (WDR8)                              
002800*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
002900*        PROGRAMMET LÄSER      W6KVAE (W6H7)                              
003000*        PROGRAMMET LÄSER      W6KVAI (W6H7C)                             
003100*        PROGRAMMET LÄSER MÅNADSVALUTA WDG2                               
003200*        PROGRAMMET LÄSER DCREGISTER   WDB6                               
003300*        PROGRAMMET LÄSER LEVERANTÖRSINFORMATION WDR2                     
003400*                                 (WDGX2206 WDGX2216)                     
003500*        PROGRAMMET UPPDATERAR WLXXBL (WDR5) WDGX2218                     
003600*        PROGRAMMET UPPDATERAR WDR5  CN/US   WDGX2248                     
003700*                                                                         
003800*    INDATA.                                                              
003900*        TRANSAKTION: W6T193X                                             
004000*        MID:         W6I19301 + WMSGKOM                                  
004100*                                                                         
004200*    UTDATA:                                                              
004300*        TRANSAKTION: W0T693X                                             
004400*        MOD:         WMSGKOM                                             
004500*        TRANSAKTION: W6T202X                                             
004600*        MOD:         W6I20201                                            
004700*                                                                         
004800*    CHANGE LOG:                                                          
004900*      E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                     
005000*                                                                         
005100*      13/11/13 - REDDY RAHUL     - IR CORRECTIONS                        
005200*                                   SCR 3235165                           
005300                                                                          
005400     SKIP3                                                                
005500 ENVIRONMENT DIVISION.                                                    
005600     EJECT                                                                
005700 DATA DIVISION.                                                           
005800 WORKING-STORAGE SECTION.                                                 
005900*    CHECKED BY WY2000                                                    
006000     SKIP3                                                                
006100*    -COPY WY2000W1                                                       
006200     SKIP3                                                                
006300 77  IDPGM                       PIC X(08)   VALUE 'W6019300'.            
006400                                                                          
006500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006700                                                                          
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
007100 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
007200                                                                          
007300                                                                          
007400 01  -COPY WWDCKONS              PIC X       VALUE 'N'.                   
007500                                                                          
007600 01  -COPY WWDC99.                                                        
007700                                                                          
007800 01  -COPY WWDCLAND                                                       
007900                                                                          
008000 77  IX-IDFS                     PIC S9(9)  VALUE +0    COMP SYNC.        
008100                                                                          
008200 77  IX-IDAVINR                  PIC S9(9)  VALUE +0    COMP SYNC.        
008300                                                                          
008400 77  CD-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
008500                                                                          
008600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
008700                                                                          
008800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008900     88  KVALITET-MID                        VALUE '6203'.                
009000                                                                          
009100 77  PRIS-FINNS-SW               PIC X(1)    VALUE 'N'.                   
009200     88  PRIS-FINNS                          VALUE 'J'.                   
009300     88  PRIS-SAKNAS                         VALUE 'N'.                   
009400                                                                          
009500 77  WS-PRAVCOST                 PIC S9(7)V9(2).                          
009600                                                                          
009700 77  W-PRARTBEL-PR               PIC S9(8)V9(5) VALUE ZERO.               
009800 77  W-PRKURS                    PIC S9(6)V9(5) VALUE ZERO.               
009900 77  W-PRKURS-USD                PIC S9(6)V9(5) VALUE ZERO.               
010000 77  W-PRKURS-CAD                PIC S9(6)V9(5) VALUE ZERO.               
010100 77  W-PRKURS-CNY                PIC S9(6)V9(5) VALUE ZERO.               
010200 77  W-PRKURS-INR                PIC S9(6)V9(5) VALUE ZERO.               
010300 77  W-PRKURS-ML                 PIC S9(6)V9(5) VALUE ZERO.               
010400 77  W-REVALUTA                  PIC S9(5) VALUE ZERO.                    
010500 77  W-2216-TISEND-PER           PIC S9(7) VALUE ZERO COMP-3.             
010600 77  WS-SAP-MM-POST              PIC X(1)  VALUE SPACE.                   
010700 77  W-DATE-AAMM                 PIC 9(4)   VALUE ZERO.                   
010800 77  WS-KDVALISO-HUV             PIC X(3)   VALUE 'SEK'.                  
010900 77  WS-KDVALISO-HUV-CN          PIC X(3)   VALUE 'CNY'.                  
011000 77  WS-KDVALISO-HUV-US          PIC X(3)   VALUE 'USD'.                  
011100                                                                          
011200 01  WS-SUDIRLON                 PIC S9(7)V9(2) VALUE +0 COMP-3.          
011300 01  WS-SUDIRMTRL                PIC S9(7)V9(2) VALUE +0 COMP-3.          
011400 01  WS-SUHEMT                   PIC S9(7)V9(2) VALUE +0 COMP-3.          
011500 01  WS-SUARTSTD                 PIC S9(9)V9(2) VALUE +0 COMP-3.          
011600                                                                          
011700 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
011800 77  WS-SAP-IDLOPNRM             PIC 9(9)    VALUE ZERO.                  
011900 77  WS-SAP-X-IDLOPNRM           PIC X(9)    VALUE SPACE.                 
012000 77  FL-RT6-KR                   PIC X       VALUE SPACE.                 
012100 77  WS-TIAA                     PIC 9(2)    VALUE ZERO.                  
012200 77  WS-TIMM                     PIC 9(2)    VALUE ZERO.                  
012300                                                                          
012400*   --- ARBETSFÄLT FÖR SALDOLOGG                                          
012500 77  WS-LOGG-KVAKS               PIC S9(7)   COMP-3.                      
012600                                                                          
012700*   PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                      
012800 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
012900 77  MAX-MOD-LAENGD-6202         PIC S9(4)  VALUE +303  COMP SYNC.        
013000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
013100     EJECT                                                                
013200*    --- ALLMÄNNA ARBETSFÄLT                                              
013300 01  WS-INLEV.                                                            
013400     03  WS-INLEV-KONTO           PIC X(10).                              
013500     03  WS-INLEV-KST             PIC X(10).                              
013600     03  WS-INLEV-ANALYS          PIC X(12).                              
013700                                                                          
013800                                                                          
013900 01  LOG-DAGENS-DATUM          PIC 9(8).                                  
014000 01  TRANS-TID                 PIC 9(9)   VALUE ZERO.                     
014100   EJECT                                                                  
014200 01  FILLER                    PIC X(16) VALUE 'WS**************'.        
014300                                                                          
014400 01  W-PRL-DADAT               PIC 9(8) VALUE ZERO.                       
014500 01  WS-PRKURS                  PIC S9(5)V9(5) VALUE +0   COMP-3.         
014600 01  WS-REVALUTA                PIC S9(5)      VALUE +0   COMP-3.         
014700                                                                          
014800 01  WS.                                                                  
014900                                                                          
015000  02     WS-DAINLEV              PIC 9(16)   VALUE ZERO.                  
015100  02     FILLER REDEFINES WS-DAINLEV.                                     
015200     03  DAGENS-DATUM-Y2K        PIC 9(08).                               
015300     03  DAGENS-TID              PIC 9(08).                               
015400                                                                          
015500*     -- DAAVIDAT TILL SAP                                                
015600  02     WS-DAAVIDAT             PIC 9(8)    VALUE ZERO.                  
015700  02     FILLER REDEFINES WS-DAAVIDAT.                                    
015800   03    WS-DAAVIDAT-SEKEL       PIC 9(2).                                
015900   03    WS-DAAVIDAT-YYMMDD      PIC 9(6).                                
016000*                                                                         
016100  02     WS-KDCLAGER             PIC S9(1)   VALUE ZERO COMP-3.           
016200  02     WS-IDORDNR-WDL612       PIC  X(7)   VALUE ZERO.                  
016300                                                                          
016400  02     WS-ADLAGOMR             PIC S9(3)   VALUE ZERO COMP-3.           
016500  02     WS-ADGANG               PIC S9(3)   VALUE ZERO COMP-3.           
016600  02     WS-ADPLATS              PIC S9(5)   VALUE ZERO COMP-3.           
016700                                                                          
016800  02     WS-TIPRLIST             PIC 9(6)    VALUE ZERO.                  
016900                                                                          
017000*     -- DAGENS-DATUM MED SEKEL-SIFFRA                                    
017100  02     WS-DAGENS-DATUM         PIC 9(8)    VALUE ZERO.                  
017200  02     FILLER REDEFINES WS-DAGENS-DATUM.                                
017300   03    WS-DAGENS-SEKEL         PIC 9(2).                                
017400   03    WS-IDAG                 PIC 9(6).                                
017500                                                                          
017600  02     WS-DATE-YYMMDD          PIC 9(06)   VALUE ZERO.                  
017700  02     WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                          
017800         03  WS-DATE-YYMM        PIC 9(04).                               
017900         03  WS-DATE-DD          PIC 9(02).                               
018000                                                                          
018100*     -- SPARAT FRÅN SEGMENT                                              
018200                                                                          
018300  02     WS-ARTC.                                                         
018400                                                                          
018500   03    WS-ARTC.                                                         
018600    04   WS-ARTC-IDFTG         PIC 9(2)    VALUE ZERO.                    
018700    04   WS-ARTC-IDLEVNR       PIC  X(5)   VALUE SPACE.                   
018800    04   WS-ARTC-KDPRODSL      PIC S9(3)   VALUE ZERO COMP-3.             
018900    04   WS-ARTC-IDFKNGRP      PIC S9(5)   VALUE ZERO COMP-3.             
019000    04   WS-ARTC-BEFT          PIC S9(3)   VALUE ZERO COMP-3.             
019100    04   WS-ARTC23-IDAVTAL     PIC 9(13)   VALUE ZERO.                    
019200                                                                          
019300    04   WS-ARTC-IDANSK        PIC S9(3)   VALUE ZERO COMP-3.             
019400    04   WS-ARTC-IDINK         PIC S9(3)   VALUE ZERO COMP-3.             
019500    04   WS-ARTC-IDINK-X       PIC  X(4)   VALUE SPACE.                   
019600    04   WS-ARTC-KDHF          PIC S9(1)   VALUE ZERO COMP-3.             
019700    04   WS-ARTC-KDVVKL        PIC S9(1)   VALUE ZERO COMP-3.             
019800    04   WS-ARTC-KDLEVSP       PIC S9(1)   VALUE ZERO COMP-3.             
019900    04   WS-ARTC-KDSORT        PIC  X(2)   VALUE SPACE.                   
020000    04   WS-ARTS-KDLEVSP       PIC S9(1)   VALUE ZERO COMP-3.             
020100                                                                          
020200    04   WS-ARTC-KDPSLLOC      PIC 9(2).                                  
020300    04   WS-ARTC-KDTIPPR       PIC S9(1)   VALUE ZERO COMP-3.             
020400    04   WS-ARTC-KDVTH         PIC S9(1)   VALUE ZERO COMP-3.             
020500    04   WS-ARTC-PRARTBES      PIC S9(7)V9(2)                             
020600                                             VALUE ZERO COMP-3.           
020700    04   WS-ARTC-PRARTBEL      PIC S9(7)V9(2)                             
020800                                             VALUE ZERO COMP-3.           
020900    04   WS-ARTC-PRARTSTD      PIC S9(7)V9(2)                             
021000                                             VALUE ZERO COMP-3.           
021100    04   WS-ARTC-PRDIRLON      PIC S9(4)V9(3)                             
021200                                             VALUE ZERO COMP-3.           
021300    04   WS-ARTC-PRDMTRL       PIC S9(6)V9(3)                             
021400                                             VALUE ZERO COMP-3.           
021500    04   WS-ARTC-PRINK         PIC S9(7)V9(2)                             
021600                                             VALUE ZERO COMP-3.           
021700    04   WS-ARTC-PROVRPAL      PIC S9(4)V9(3)                             
021800                                             VALUE ZERO COMP-3.           
021900    04   WS-ARTC-PRHEMTAG      PIC S9(7)V9(2)                             
022000                                             VALUE ZERO COMP-3.           
022100** NOTERA ATT I PROGRAMMET ANVÄNDS DESSA SALDOFÄLT BÅDE FÖR               
022200** CDC-SALDON OCH NDC-SALDON. DETTA FÖR ATT MINSKA ANTALET FÄLT           
022300** ATT HÅLLA REDA PÅ                                                      
022400** EVABL                                                                  
022500    04   WS-ARTC-ARTS-KVAKS-SDC PIC S9(7)   VALUE +0   COMP-3.            
022600    04   WS-ARTC-ARTS-KVROS    PIC S9(7)   VALUE +0   COMP-3.             
022700    04   WS-ARTC-ARTS-KVLS     PIC S9(7)   VALUE +0   COMP-3.             
022800    04   WS-ARTC-ARTS-KVUTRS   PIC S9(7)   VALUE +0   COMP-3.             
022900    04   WS-ARTC-ARTS-KVRESS   PIC S9(7)   VALUE +0   COMP-3.             
023000    04   WS-ARTC-ARTS-KVSPANT  PIC S9(7)   VALUE +0   COMP-3.             
023100    04   WS-ARTC-ARTS-KVSPARR-KVAL PIC S9(7) VALUE ZERO COMP-3.           
023200                                                                          
023300   03    WS-ARTC21.                                                       
023400    04   WS-ARTC21-PRARTBEL-PR   PIC S9(8)V9(5)                           
023500                                             VALUE ZERO COMP-3.           
023600    04   WS-ARTC21-PRARTBEL-SUM  PIC S9(8)V9(5)                           
023700                                             VALUE ZERO COMP-3.           
023800    04   WS-ARTC21-PRARTBES-PR   PIC S9(7)V9(2)                           
023900                                             VALUE ZERO COMP-3.           
024000    04   WS-ARTC21-KDVALISO      PIC X(3)    VALUE SPACE.                 
024100                                                                          
024200  02 SPAR-PRKURS                 PIC S9(5)V9(5) VALUE +0   COMP-3.        
024300                                                                          
024400  02     WS-SLAG.                                                         
024500   03    WS-ARTS-PRAVCOST        PIC S9(7)V9(2) COMP-3 VALUE ZERO.        
024600                                                                          
024700  02     WS-KURS.                                                         
024800   03    W-PRKURS-LEV            PIC S9(5)V9(2) COMP-3 VALUE ZERO.        
024900   03    W-PRKURS-DC             PIC S9(5)V9(2) COMP-3 VALUE ZERO.        
025000                                                                          
025100  02     W-RETULF                PIC S9(3)V9(4) VALUE +0.                 
025200  02     WS-PRARTKALKYL          PIC S9(8)V9(5) VALUE +0.                 
025300                                                                          
025400  02     WS-INLA.                                                         
025500                                                                          
025600   03    WS-INLA-INL.                                                     
025700    04   WS-INLA-INL-IDDC        PIC X(2)    VALUE SPACE.                 
025800    04   WS-INLA-INL-IDLEVNR     PIC  X(5)   VALUE SPACE.                 
025900    04   WS-INLA-INL-IDFS        PIC X(8)    VALUE SPACE.                 
026000    04   WS-INLA-INL-IDANALYS    PIC X(12)   VALUE SPACE.                 
026100    04   WS-INLA-INL-IDKST       PIC X(10)   VALUE SPACE.                 
026200    04   WS-INLA-INL-TIAVIDAT    PIC S9(7)   VALUE ZERO COMP-3.           
026300    04   WS-INLA-INL-IDKONTO     PIC 9(10)   VALUE ZERO.                  
026400    04   WS-INLA-RAD-IDANSTNR    PIC  9(5)   VALUE ZERO.                  
026410    04   WS-INLA-RAD-IDANSTNR-BIN PIC  9(5)  VALUE ZERO.                  
026500                                                                          
026600   03    WS-TOT.                                                          
026700    04   WS-TOT-KVINLART         PIC S9(7)   VALUE ZERO COMP-3.           
026800    04   WS-TOT-KVINLART-SVS     PIC S9(7)   VALUE ZERO COMP-3.           
026900    04   WS-TOT-KVINLART-CD      OCCURS 4 TIMES                           
027000                                 PIC S9(7)   VALUE ZERO COMP-3.           
027100                                                                          
027200  02     WS-INLE.                                                         
027300                                                                          
027400   03    WS-INLE-MOT.                                                     
027500    04   WS-INLE-MOT-KVANTMOT    PIC S9(7)   VALUE ZERO COMP-3.           
027600    04   WS-INLE-MOT-KDAVVANT    PIC S9(1)   VALUE ZERO COMP-3.           
027700    04   WS-INLE-MOT-KDAVVKV     PIC S9(1)   VALUE ZERO COMP-3.           
027800    04   WS-INLE-MOT-KVFORDEL    PIC S9(7)   VALUE ZERO COMP-3.           
027900    04   WS-INLE-MOT-KVRETUR     PIC S9(7)   VALUE ZERO COMP-3.           
028000    04   WS-INLE-KVANTMOT-INNAN  PIC S9(7)   VALUE ZERO COMP-3.           
028100     SKIP2                                                                
028200    04   WS-INLE-MOT-KVANTDELMOT PIC S9(7)   VALUE ZERO COMP-3.           
028300     SKIP2                                                                
028400  02     WS-KVAE.                                                         
028500   03    WS-KVAE-FLANNULL-ANTAL   PIC X(1)   VALUE 'N'.                   
028600   03    WS-KVAE-FLANNULL-TEKNISK PIC X(1)   VALUE 'N'.                   
028700   03    WS-KVAE-FLKRGODK         PIC X(1)   VALUE SPACE.                 
028800   03    WS-KVAE-IDKRFEL          PIC X(2)   VALUE SPACE.                 
028900   03    WS-KVAE-FLKRLIM          PIC X(1)   VALUE SPACE.                 
029000                                                                          
029100     SKIP2                                                                
029200*     -- DAGENS LOKALA AAMMDD                                             
029300      SKIP2                                                               
029400 02  WS-IDDC-LOCAL.                                                       
029500     03  FILLER                  PIC X(4)   VALUE 'IDDC'.                 
029600     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
029700 02  WS-TIAAMMDD-LOCAL           PIC 9(6)   VALUE ZERO.                   
029800                                                                          
029900*     -- DAGENS AAMMDD                                                    
030000  02     WS-TIAAMMDD             PIC 9(6)    VALUE ZERO.                  
030100  02     FILLER                  REDEFINES WS-TIAAMMDD.                   
030200   03    WS-TIAAMMDD-AA          PIC 9(2).                                
030300   03    WS-TIAAMMDD-MM          PIC 9(2).                                
030400   03    WS-TIAAMMDD-DD          PIC 9(2).                                
030500                                                                          
030600*     -- DAGENS AAVVD                                                     
030700  02     WS-TIAAVVD              PIC 9(5)    VALUE ZERO.                  
030800  02     FILLER                  REDEFINES WS-TIAAVVD.                    
030900   03    WS-TIAAVVD-AAVV         PIC 9(4).                                
031000   03    FILLER                  REDEFINES WS-TIAAVVD-AAVV.               
031100    04   WS-TIAAVVD-AA           PIC 9(2).                                
031200    04   WS-TIAAVVD-VV           PIC 9(2).                                
031300   03    WS-TIAAVVD-D            PIC 9(1).                                
031400*     -- DATE + TIME                                                      
031500  02     WS-TIAAMMDDTTMMSSTH     PIC 9(14)   VALUE ZERO.                  
031600  02     FILLER                  REDEFINES WS-TIAAMMDDTTMMSSTH.           
031700   03    WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                               
031800   03    WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                               
031900                                                                          
032000*     -- IDLOPNRM I VALFRI FORM                                           
032100  02     WS-IDLOPNRM-AAVVDLLLLK  PIC 9(10)   VALUE ZERO.                  
032200  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
032300   03    WS-IDLOPNRM-AA          PIC 9(2).                                
032400   03    WS-IDLOPNRM-VVDLLLLK    PIC 9(8).                                
032500   03    FILLER                  REDEFINES WS-IDLOPNRM-VVDLLLLK.          
032600    04   WS-IDLOPNRM-VVDLLLL     PIC 9(7).                                
032700    04   FILLER                  REDEFINES WS-IDLOPNRM-VVDLLLL.           
032800     05  WS-IDLOPNRM-VV          PIC 9(2).                                
032900     05  FILLER                  PIC X(5).                                
033000    04   FILLER                  PIC X(1).                                
033100  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
033200   03    WS-IDLOPNRM-AAVVDLLLL   PIC 9(9).                                
033300   03    FILLER                  PIC X(1).                                
033400*     -- FÖR REDIG. AV IDAVINR FRÅN IDFS                                  
033500  02     WS-IDAVINR            PIC 9(7)      VALUE ZERO.                  
033600  02     FILLER                REDEFINES WS-IDAVINR.                      
033700   03    WS-IDAVINR-TKN        OCCURS 7 PIC 9(1).                         
033800*                                                                         
033900  02     WS-IDFS               PIC X(8)      VALUE SPACE.                 
034000  02     FILLER                REDEFINES WS-IDFS.                         
034100   03    WS-IDFS-TKN           OCCURS 8 PIC X(1).                         
034200*                                                                         
034300*     -- DIFFERANS: KVANTMOT - KVAVIS                                     
034400  02     WS-KVDIFF-MOT-AVIS      PIC S9(7)   VALUE ZERO COMP-3.           
034500*     -- KVANT FÖR BOKNING LEVPLAN                                        
034600  02     WS-KV-LPLAN             PIC S9(7)   VALUE ZERO COMP-3.           
034700*     -- KVANT SOM SKALL BOKAS UPP ELLER NER I LEVPLAN                    
034800  02     WS-KV-OBOK              PIC S9(7)   VALUE ZERO COMP-3.           
034900*     -- AVROP-KVANT MÖJLIG ATT BOKA                                      
035000  02     WS-KVAVROP-BOKBAR       PIC S9(7)   VALUE ZERO COMP-3.           
035100*     -- AVROP-KVANT VERKLIGT BOKAD                                       
035200  02     WS-KVAVROP-BOKAD        PIC S9(7)   VALUE ZERO COMP-3.           
035300*     -- DISPONIBELT SALDO                                                
035400  02     WS-KVDISP               PIC S9(7)   VALUE ZERO COMP-3.           
035500                                                                          
035600*     -- SUMMA INLAGT ANTAL (INL,VOR)                                     
035700  02     WS-KVINLART-INLAGD      PIC S9(7)   VALUE ZERO COMP-3.           
035800*     -- AKTUELL RADS FRD                                                 
035900  02     WS-KVINLART-AKT-FRD     PIC S9(7)   VALUE ZERO COMP-3.           
036000*     -- SUMMA MED KDINLSTA FRD                                           
036100  02     WS-KVINLART-TRP         PIC S9(7)   VALUE ZERO COMP-3.           
036200*     -- SUMMA MED KDINLSTA TRP                                           
036300  02     WS-KVINLART-FRD         PIC S9(7)   VALUE ZERO COMP-3.           
036400*     -- SUMMA MED KDINLSTA KVA                                           
036500  02     WS-KVINLART-KVA         PIC S9(7)   VALUE ZERO COMP-3.           
036600*     -- SUMMA MED KDINLSTA RET                                           
036700  02     WS-KVINLART-RET         PIC S9(7)   VALUE ZERO COMP-3.           
036800*     -- SUMMA MED KDINLSTA ANT                                           
036900  02     WS-KVINLART-ANT         PIC S9(7)   VALUE ZERO COMP-3.           
037000*     -- SUMMA MED KDINLSTA ANT OCH AVV                                   
037100  02     WS-KVINLART-ANT-AVV     PIC S9(7)   VALUE ZERO COMP-3.           
037200*     -- SUMMA MED KDINLSTA AVV                                           
037300  02     WS-KVINLART-AVV         PIC S9(7)   VALUE ZERO COMP-3.           
037400*     -- SUMMA AVVIKELSER TOTALT (ANT,AVV,KVA)                            
037500  02     WS-KVINLART-ANT-AVV-KVA-RET PIC S9(7) VALUE ZERO COMP-3.         
037600*     -- SALDOPÅVERKANDE ANTAL, FRÅN AKTUELL RAD                          
037700  02     WS-KVINLART-AKT-SALDO   PIC S9(7)   VALUE ZERO COMP-3.           
037800*     -- HJÄLPFÄLT FÖR ATT BERÄKNA KVA-AVVIKELSE                          
037900  02     WS-KR-KVART-KVA         PIC S9(7)   VALUE ZERO COMP-3.           
038000*     -- ANTAL KONTROLLRAPPORTER                                          
038100  02     WS-ANT-KR               PIC S9(1)   VALUE ZERO COMP-3.           
038200                                                                          
038300*     -- BIDRAG FRÅN AVVIKELSER TILL SLUTUPPD.                            
038400  02     WS-AVVIK-KVAKS          PIC S9(7)   VALUE ZERO COMP-3.           
038500  02     WS-AVVIK-KVLS           PIC S9(7)   VALUE ZERO COMP-3.           
038600  02     WS-KDAVVKV              PIC S9(1)   VALUE ZERO COMP-3.           
038700  02     WS-KDAVVANT             PIC S9(1)   VALUE ZERO COMP-3.           
038800  02     WS-KR-KVART-RET         PIC S9(7)   VALUE ZERO COMP-3.           
038900                                                                          
039000*     -- FÖR GRUPPERING AV VÄRDEN                                         
039100  02     WS-KDINLSTA             PIC X(3)    VALUE SPACE.                 
039200   88    WS-KDINLSTA-INLAGD      VALUE 'INL', 'VOR'.                      
039300   88    WS-KDINLSTA-FRD         VALUE 'FRD'.                             
039400                                                                          
039500                                                                          
039600*     -- LÖPNUMMER I LOGGPOST FÖR ATT FÖRSÄKRA SIG OM UNIK NKL.           
039700  02     WS-IDLOGLOP             PIC S9(1)   VALUE ZERO COMP-3.           
039800                                                                          
039900*     -- LOGG-TRANSAR                                                     
040000  02     WS-ZZAC01.                                                       
040100   03    WS-ZZAC01-LOGGPOST      PIC X(90)   VALUE SPACE.                 
040200   03    FILLER                  REDEFINES WS-ZZAC01-LOGGPOST.            
040300    04   WS-ZZAC01-LOGGPOST-1-3  PIC X(3).                                
040400    04   WS-ZZAC01-LOGGPOST-4-90 PIC X(87).                               
040500   03    WS-ZZAC01-SORTPOST      PIC X(36)   VALUE SPACE.                 
040600     EJECT                                                                
040700 01      FILLER                PIC X(16) VALUE 'SW-SWITCHAR*****'.        
040800 01      SW-SWITCHAR.                                                     
040900                                                                          
041000*     -- RAPPORTERING ÄR OFULLSTÄNDIG --> SLUTRAPP KAN EJ SKE             
041100  02     SW-RAPP-OFULLST         PIC X(1)    VALUE SPACE.                 
041200*     -- FÖREKOMMST AV KONTROLLRAPPORTER                                  
041300  02     ANTALS-KR-FINNS         PIC X(1)    VALUE SPACE.                 
041400  02     TEKNISK-KR-FINNS        PIC X(1)    VALUE SPACE.                 
041500*     -- FÖREKOMMST AV OLIKA AVVIK-KODER                                  
041600  02     SW-KDINLSTA-KVA         PIC X(1)    VALUE SPACE.                 
041700  02     SW-KDINLSTA-RET         PIC X(1)    VALUE SPACE.                 
041800     SKIP3                                                                
041900*     -- MAX ANTAL KONTROLLRAPPORTER                                      
042000  02     K-MAX-KR                PIC S9(1)   VALUE +2   COMP-3.           
042100     SKIP3                                                                
042200 01  GENERELLA-SUBPROGRAM.                                                
042300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
042400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
042500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
042600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
042700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
042800     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
042900     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
043000     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
043100     SKIP3                                                                
043200 01  MESSAGE-CODES.                                                       
043300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
043400     EJECT                                                                
043500*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
043600 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
043700*   -COPY W005WDK7                                                        
043800     EJECT                                                                
043900*    --- AREA FÖR WDATKONV                                                
044000 01  FILLER                    PIC X(16) VALUE 'WDATAREA********'.        
044100                                                                          
044200*01  -COPY WDATAREA                                                       
044300     EJECT                                                                
044400*    --- AREA FÖR W005INIT                                                
044500 01  FILLER                    PIC X(16) VALUE 'WDATAREA********'.        
044600                                                                          
044700*01  -COPY WMSGINIT                                                       
044800     EJECT                                                                
044900*    --- AREA FÖR W510AVG                                                 
045000 01  FILLER                    PIC X(16) VALUE 'W510AVGAREA*****'.        
045100                                                                          
045200*01  -COPY W510AVG                                                        
045300     EJECT                                                                
045400*    --- AREA FÖR W510CURR                                                
045500*01  -COPY W510CURR                                                       
045600     EJECT                                                                
045700*    --- AREOR FÖR LOGGTRANSAR                                            
045800 01  FILLER                    PIC X(16) VALUE 'W611R32*********'.        
045900                                                                          
046000*01  -COPY W611R32  -PRE W611R32-                                         
046100     EJECT                                                                
046200 01  FILLER                    PIC X(16) VALUE 'W211310*********'.        
046300                                                                          
046400*01  -COPY W211310  -PRE R320-                                            
046500     EJECT                                                                
046600 01  FILLER                    PIC X(16) VALUE 'W211400*********'.        
046700                                                                          
046800*01  -COPY W211400  -PRE W211400-                                         
046900     EJECT                                                                
047000 01  FILLER                    PIC X(16) VALUE 'W211FEL*********'.        
047100                                                                          
047200*01  -COPY W211FEL  -PRE W211FEL-                                         
047300     EJECT                                                                
047400 01  FILLER                    PIC X(16) VALUE 'W211M108********'.        
047500                                                                          
047600*01  -COPY W211M108 -PRE M108-                                            
047700     EJECT                                                                
047800*01  -COPY WWPRODSL                                                       
047900     EJECT                                                                
048000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
048100*                                                                         
048200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
048300     SKIP3                                                                
048400*01  MID -COPY W6I19301                                                   
048500     EJECT                                                                
048600 01  FILLER                      PIC X(16)   VALUE 'WMSGKOM'.             
048700     SKIP3                                                                
048800*01  -COPY WMSGKOM                                                        
048900     EJECT                                                                
049000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
049100     SKIP3                                                                
049200*01  -COPY WMSGAREA                                                       
049300     EJECT                                                                
049400 01      FILLER                  PIC X(16)   VALUE 'P-TO-P-SW'.           
049500     SKIP3                                                                
049600 01      P-TO-P-SW.                                                       
049700                                                                          
049800  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
049900  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
050000  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
050100  02     P-TO-P-KDTRANS          PIC X(8).                                
050200  02     P-TO-P-IDTRANS          PIC X(4).                                
050300  02     P-TO-P-KDMFSFOR         PIC X(1).                                
050400  02     P-TO-P-DATA             PIC X(300).                              
050500     EJECT                                                                
050600 01      FILLER                  PIC X(24)   VALUE                        
050700                                 'MOD6202-MID-W6I19202'.                  
050800     SKIP2                                                                
050900     -COPY W6I20201 -PRE MOD6202-                                         
051000     EJECT                                                                
051100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
051200     SKIP3                                                                
051300*01  -COPY WMFSAREA                                                       
051400     EJECT                                                                
051500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
051600*                                                                         
051700     SKIP2                                                                
051800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
051900     SKIP2                                                                
052000 01  NYCKLAR-TILL-DLI.                                                    
052100                                                                          
052200*    -- W6D1 HUVUDBAS.                                                    
052300     03  W-W6D101KY-X.                                                    
052400         05  W-W6D101KY-IDDC     PIC X(2)    VALUE SPACE.                 
052500         05  W-W6D101KY-IDLEVNR  PIC  X(5)   VALUE SPACE.                 
052600         05  W-W6D101KY-IDFS     PIC X(8)    VALUE SPACE.                 
052700         05  W-W6D101KY-TIAVIDAT PIC S9(7)   VALUE ZERO COMP-3.           
052800     03  W-IDRADNR-INL-X.                                                 
052900         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
053000     03  W-IDRADNR-X.                                                     
053100         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
053200                                                                          
053300*    -- W6D1 INDEXBAS B. (W6INLC) MIN O MAX.                              
053400     03  W-IDARTNR-X.                                                     
053500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
053600     03  W-IDLEVNR-X.                                                     
053700         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
053800     03  W-IDLEVNR-21-X.                                                  
053900         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
054000     03  W-KDCLAGER-X.                                                    
054100         05  W-KDCLAGER          PIC S9(1)   VALUE ZERO COMP-3.           
054200     03  W-IDDC-X.                                                        
054300         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
054400     03  W-IDDC-K7-X.                                                     
054500         05  W-IDDC-K7           PIC  X(2)   VALUE SPACE.                 
054600     03  W-IDLAND-K7-X.                                                   
054700         05    W-IDLAND-K7       PIC X(2)    VALUE SPACE.                 
054800     03  W-IDLANDX2-X.                                                    
054900         05    W-IDLANDX2        PIC X(2)    VALUE SPACE.                 
055000     03  W-DAINLEV-X.                                                     
055100         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
055200     03  W-IDLEVNR-PR-X.                                                  
055300         05  W-IDLEVNR-PR        PIC X(5)    VALUE ZERO.                  
055400     03  W-DAPRLIST-K7-N.                                                 
055500         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
055600     03  W-KDAVROP-X.                                                     
055700         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
055800     03  W-WDD901KY-X.                                                    
055900         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
056000         05  W-IDDC-D9           PIC  X(2)   VALUE SPACE.                 
056100     03  W-WDD905KY-X.                                                    
056200         05  W-DAAVROP-X.                                                 
056300             07  W-DAAVROP       PIC  9(6)    VALUE ZERO.                 
056400         05  W-TILEVDAG-X.                                                
056500             07  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.          
056600     03  W-INLB11-IDLEVNR-X.                                              
056700         05  W-INLB11-IDLEVNR    PIC  X(5)   VALUE SPACE.                 
056800     03  W-INLB31-IDLOPNRM-X.                                             
056900         05  W-INLB31-IDLOPNRM   PIC S9(9)   VALUE ZERO COMP-3.           
057000     03  W-IDLOPNRM-X.                                                    
057100         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
057200     03  W-2227-KEY-X.                                                    
057300         05  FILLER              PIC X(4)    VALUE '2227'.                
057400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
057500     03  W-4505-KEY-X.                                                    
057600         05  FILLER              PIC X(4)    VALUE '4505'.                
057700         05  4505-IDDC           PIC X(2)    VALUE SPACE.                 
057800         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
057900*    -- W6H7 HUVUDBAS.                                                    
058000     03  W-IDKR-X.                                                        
058100         05  W-IDKR              PIC 9(5)    VALUE ZERO.                  
058200                                                                          
058300*    -- W6H7 INDEXBAS C. (W6KVAI) MIN O MAX.                              
058400     03  W-W6H7C1KY-MIN-X.                                                
058500         05  W-W6H7C1KY-MIN-IDLOPNRM                                      
058600                                 PIC S9(9)   VALUE ZERO COMP-3.           
058700         05  W-W6H7C1KY-MIN-DAAVSDAT                                      
058800                                 PIC 9(8)    VALUE ZERO.                  
058900         05  FILLER              PIC 9(5)    VALUE ZERO.                  
059000     03  W-W6H7C1KY-MAX-X.                                                
059100         05  W-W6H7C1KY-MAX-IDLOPNRM                                      
059200                                 PIC S9(9)   VALUE ZERO COMP-3.           
059300         05  W-W6H7C1KY-MAX-DAAVSDAT                                      
059400                                 PIC 9(8)    VALUE ZERO.                  
059500         05  FILLER              PIC 9(5)    VALUE ZERO.                  
059600     03  W-IDDC-B6-X.                                                     
059700         05 W-IDDC-B6                  PIC X(2).                          
059800     03  W-IDLAND-X.                                                      
059900         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
060000                                                                          
060100     03  W-WDGX9305-X.                                                    
060200         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
060300         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
060400         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
060500         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
060600     03  W-KDVALISO-X.                                                    
060700         05  W-KDVALISO-ROW      PIC X(3)    VALUE SPACE.                 
060800     03  W-TISTADA9-X.                                                    
060900         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
061000                                                                          
061100                                                                          
061200     03  W-WDGXKEY-2215-X.                                                
061300         05  W-IDHTYP            PIC X(4)    VALUE '2215'.                
061400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
061500                                                                          
061600     03  W-IDLEVNR-2216-X.                                                
061700         05  W-IDLEVNR-2216      PIC  X(5)   VALUE SPACE.                 
061800                                                                          
061900     03  W-WDGXKEY-2217-X.                                                
062000         05  W-IDHTYP            PIC X(4)    VALUE '2217'.                
062100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
062200                                                                          
062300     03  W-WDGXKEY-2217-P-X.                                              
062400         05  FILLER              PIC X(4)    VALUE '2217'.                
062500         05  FILLER              PIC X(1)    VALUE 'J'.                   
062600         05  FILLER              PIC X(25)   VALUE LOW-VALUE.             
062700                                                                          
062800     03  W-WDGXKEY-2205-X.                                                
062900         05  W-IDHTYP            PIC X(04)    VALUE '2205'.               
063000         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
063100                                                                          
063200     03  W-KY2206-X.                                                      
063300         05  W-IDLEVNR-2206      PIC X(5)     VALUE SPACE.                
063400         05  W-IDDC-2206         PIC X(2)     VALUE SPACE.                
063500                                                                          
063600     03  W-WDGXKEY-2247-X.                                                
063700         05  W-IDHTYP            PIC X(4)    VALUE '2247'.                
063800         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
063900                                                                          
064000     03  W-WDGXKEY-2247-P-X.                                              
064100         05  FILLER              PIC X(4)    VALUE '2247'.                
064200         05  FILLER              PIC X(1)    VALUE 'J'.                   
064300         05  FILLER              PIC X(25)   VALUE LOW-VALUE.             
064400                                                                          
064500     EJECT                                                                
064600*    --- STATUS-KOD FRÅN IMS                                              
064700 01  STATUS-WS                   PIC XX.                                  
064800     88  SEGMENT-FINNS                       VALUE '  '.                  
064900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
065000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
065100     88  BASEN-SLUT                          VALUE 'GB'.                  
065200     SKIP2                                                                
065300 01  GODK-STATUSKODER.                                                    
065400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
065500     SKIP3                                                                
065600 01  SSA1                        PIC X(96).                               
065700 01  SSA2                        PIC X(64).                               
065800 01  SSA3                        PIC X(64).                               
065900     EJECT                                                                
066000*    --- IMS FUNKTIONSKODER                                               
066100*01  -COPY W0003                                                          
066200     EJECT                                                                
066300*    ---  DLI INPUT-OUTPUT AREA                                           
066400                                                                          
066500*         DLI-IO-AREA    W6INLC01                                         
066600*                        WLINLE21,-31                                     
066700*                        WLINLB11,-23                                     
066800*                        WLXXBW11                                         
066900*                        W6KVAI01                                         
067000*                        W6KVAE01                                         
067100                                                                          
067200*         DLI-IO-AREA2   W6INLA01,-11                                     
067300                                                                          
067400*         DLI-IO-AREA3   W6INLA21                                         
067500                                                                          
067600*         DLI-IO-AREA5   WLINLB31                                         
067700                                                                          
067800*         DLI-IO-AREA6   WLINLB23+31 (PATH-CALL)                          
067900                                                                          
068000*         DLI-IO-ARTC    WLARTC01,-11,-21                                 
068100                                                                          
068200*         DLI-IO-AREA-4505                                                
068300     EJECT                                                                
068400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
068500     SKIP3                                                                
068600 01  DLI-IO-AREA.                                                         
068700     03  IO-AREA                 PIC X(550)  VALUE SPACE.                 
068800     SKIP3                                                                
068900     03  W6INLC01 REDEFINES IO-AREA.                                      
069000*        05  -COPY W6D1B1  -PRE INLC-                                     
069100     EJECT                                                                
069200     03  WLINLB11 REDEFINES IO-AREA.                                      
069300*        05  -COPY WDD902  -PRE INLB11-                                   
069400     EJECT                                                                
069500     03  WLINLB23 REDEFINES IO-AREA.                                      
069600*        05  -COPY WDD905  -PRE INLB23-                                   
069700     EJECT                                                                
069800     03  WLINLE01 REDEFINES IO-AREA.                                      
069900*        05  -COPY WDL201  -PRE INLE-                                     
070000     EJECT                                                                
070100     03  WLINLE11 REDEFINES IO-AREA.                                      
070200*        05  -COPY WDL211  -PRE INLE-                                     
070300     EJECT                                                                
070400     03  WLINLE21 REDEFINES IO-AREA.                                      
070500*        05  -COPY WDL221  -PRE INLE-                                     
070600     EJECT                                                                
070700     03  WLINLE31 REDEFINES IO-AREA.                                      
070800*        05  -COPY WDL231  -PRE INLE-                                     
070900     EJECT                                                                
071000     03  WLZZAC01 REDEFINES IO-AREA.                                      
071100*        05  -COPY WDG601  -PRE ZZAC01-                                   
071200     EJECT                                                                
071300     03  WLXXBW11 REDEFINES IO-AREA.                                      
071400*        05  -COPY WDGX2228 -PRE XXBW-                                    
071500     EJECT                                                                
071600     03  W6KVAI01 REDEFINES IO-AREA.                                      
071700*        05  -COPY W6H7C1  -PRE KVAI-                                     
071800     EJECT                                                                
071900     03  W6KVAE01 REDEFINES IO-AREA.                                      
072000*        05  -COPY W6H701  -PRE KVAE-                                     
072100     EJECT                                                                
072200*    ---  DLI INPUT-OUTPUT AREA 2                                         
072300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
072400     SKIP3                                                                
072500 01  DLI-IO-AREA2.                                                        
072600     03  IO-AREA2                PIC X(200)  VALUE SPACE.                 
072700     SKIP3                                                                
072800     03  W6INLA01 REDEFINES IO-AREA2.                                     
072900*        05  -COPY W6D101  -PRE INLA-                                     
073000     EJECT                                                                
073100     03  W6INLA11 REDEFINES IO-AREA2.                                     
073200*        05  -COPY W6D111  -PRE INLA-                                     
073300     EJECT                                                                
073400*    ---  DLI INPUT-OUTPUT AREA 3                                         
073500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
073600     SKIP3                                                                
073700 01  DLI-IO-AREA3.                                                        
073800     03  IO-AREA3                PIC X(100)  VALUE SPACE.                 
073900     SKIP3                                                                
074000     03  W6INLA21 REDEFINES IO-AREA3.                                     
074100*        05  -COPY W6D121  -PRE INLA-                                     
074200     EJECT                                                                
074300*    ---  DLI INPUT-OUTPUT AREA 5                                         
074400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA5'.        
074500     SKIP3                                                                
074600 01  DLI-IO-AREA5.                                                        
074700     03  IO-AREA5                PIC X(100)  VALUE SPACE.                 
074800     SKIP3                                                                
074900     03  WLINLB31 REDEFINES IO-AREA5.                                     
075000*        05  -COPY WDD906  -PRE INLB31-                                   
075100     EJECT                                                                
075200*    ---  DLI INPUT-OUTPUT AREA 6  KONKATINERADE SEGMENT (PATH)           
075300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
075400     SKIP3                                                                
075500 01  DLI-IO-AREA6.                                                        
075600     SKIP3                                                                
075700     03  WLINLB23-PATH.                                                   
075800*        05  -COPY WDD905  -PRE INLB23P-                                  
075900     EJECT                                                                
076000     03  WLINLB31-PATH.                                                   
076100*        05  -COPY WDD906  -PRE INLB31P-                                  
076200     EJECT                                                                
076300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
076400     SKIP3                                                                
076500 01  DLI-IO-WDK711.                                                       
076600     SKIP3                                                                
076700     03  -COPY WDK711                                                     
076800     SKIP3                                                                
076900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK712'.         
077000     SKIP3                                                                
077100 01  DLI-IO-WDK712.                                                       
077200     SKIP3                                                                
077300     03  -COPY WDK712                                                     
077400     SKIP3                                                                
077500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK723'.         
077600 01  DLI-IO-WDK723.                                                       
077700     SKIP3                                                                
077800     03  -COPY WDK723                                                     
077900     EJECT                                                                
078000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK724'.         
078100 01  DLI-IO-WDK724.                                                       
078200     SKIP3                                                                
078300     03  -COPY WDK724                                                     
078400     EJECT                                                                
078500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL601'.         
078600     SKIP3                                                                
078700 01  DLI-IO-AREA-WDL601.                                                  
078800     SKIP3                                                                
078900     03  -COPY WDL601  -PRE INLC-                                         
079000     EJECT                                                                
079100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL611'.         
079200     SKIP3                                                                
079300 01  DLI-IO-AREA-WDL611.                                                  
079400     SKIP3                                                                
079500     03  -COPY WDL611 -PRE INLC-                                          
079600     EJECT                                                                
079700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-INLC21'.         
079800     SKIP3                                                                
079900 01  DLI-IO-AREA-WDL621.                                                  
080000     SKIP3                                                                
080100     03  -COPY WDL621 -PRE INLC-                                          
080200     EJECT                                                                
080300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL612'.         
080400     SKIP3                                                                
080500 01  DLI-IO-AREA-WDL612.                                                  
080600     SKIP3                                                                
080700     03  -COPY WDL612 -PRE INLC-                                          
080800     EJECT                                                                
080900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-ARTC'.         
081000     SKIP3                                                                
081100 01  DLI-IO-ARTC.                                                         
081200     03  IO-ARTC                 PIC X(900)  VALUE SPACE.                 
081300     SKIP3                                                                
081400     03  WLARTC01 REDEFINES IO-ARTC.                                      
081500*        05  -COPY WDK601  -PRE ARTC-                                     
081600     EJECT                                                                
081700     03  WLARTC11 REDEFINES IO-ARTC.                                      
081800*        05  -COPY WDK611  -PRE ARTC-                                     
081900     EJECT                                                                
082000     03  WLARTC21 REDEFINES IO-ARTC.                                      
082100*        05  -COPY WDK621  -PRE ARTC-                                     
082200     EJECT                                                                
082300 01  FILLER                      PIC X(16)  VALUE 'WDK623'.               
082400*01  WLARTC23 -COPY WDK623                                                
082500     EJECT                                                                
082600 01  DLI-IO-AREA-4505.                                                    
082700     03  IO-AREA-4505            PIC X(300)  VALUE SPACE.                 
082800     SKIP3                                                                
082900     03  WL450611 REDEFINES IO-AREA-4505.                                 
083000*        05  -COPY WDGX4506                                               
083100     EJECT                                                                
083200 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
083300*01  WLLOGA01    -COPY WDL901                                             
083400     EJECT                                                                
083500 01  DLI-IO-AREA-FILB01.                                                  
083600*    05  -COPY WDR801       -PRE EKO-                                     
083700       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
083800         09  -COPY W51080   -PRE EKO-                                     
083900       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
084000         09  -COPY W510A11  -PRE LAB-                                     
084100       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
084200         09  -COPY W510EKHA -PRE EKO-                                     
084300     EJECT                                                                
084400 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
084500 01  DLI-IO-WLSAPA01.                                                     
084600*    03  WLSAPA01  -COPY WDR901                                           
084700*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
084800                                                                          
084900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
085000 01   DLI-IO-AREA-B601.                                                   
085100*     03  -COPY WDB601                                                    
085200                                                                          
085300 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
085400 01   DLI-IO-AREA-B617.                                                   
085500*     03  -COPY WDB617                                                    
085600     EJECT                                                                
085700                                                                          
085800 01  FILLER               PIC X(16)   VALUE 'WDB601 NEXT'.                
085900 01   DLI-IO-B601-NEXT.                                                   
086000*     03  -COPY WDB601 -PRE NEXT-                                         
086100     EJECT                                                                
086200                                                                          
086300 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
086400*01  WLLEVA01 -COPY WDF101                                                
086500     EJECT                                                                
086600                                                                          
086700 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
086800*01  WLLEVA11 -COPY WDF102 -PRE LEV-                                      
086900     EJECT                                                                
087000                                                                          
087100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
087200 01  DLI-IO-WDGX9306.                                                     
087300*    03  -COPY WDGX9306                                                   
087400     EJECT                                                                
087500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
087600 01  DLI-IO-WDGX9308.                                                     
087700*    03  -COPY WDGX9308                                                   
087800     EJECT                                                                
087900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2216'.                    
088000 01  DLI-IO-WDGX2216.                                                     
088100*    03  -COPY WDGX2216                                                   
088200     EJECT                                                                
088300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2218'.                    
088400 01  DLI-IO-WDGX2218.                                                     
088500*    03  -COPY WDGX2218                                                   
088600     EJECT                                                                
088700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2206'.                    
088800 01  DLI-IO-WDGX2206.                                                     
088900*    03  -COPY WDGX2206                                                   
089000     EJECT                                                                
089100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2248'.                    
089200 01  DLI-IO-WDGX2248.                                                     
089300*    03  -COPY WDGX2248                                                   
089400     EJECT                                                                
089500                                                                          
089600 LINKAGE SECTION.                                                         
089700                                                                          
089800*01  -COPY W0009   -PRE MSG-                                              
089900     EJECT                                                                
090000*01  -COPY W0009   -PRE DISP-                                             
090100     EJECT                                                                
090200*01  -COPY W0009   -PRE 6202-                                             
090300     EJECT                                                                
090400*01  -COPY W0008  -PRE W6INLC-                                            
090500     05  FILLER                  PIC X.                                   
090600                                                                          
090700*01  -COPY W0008  -PRE W6INLA-                                            
090800     05  FILLER                  PIC X.                                   
090900     EJECT                                                                
091000*01  -COPY W0008  -PRE W6KVAI-                                            
091100     05  FILLER                  PIC X.                                   
091200                                                                          
091300*01  -COPY W0008  -PRE W6KVAE-                                            
091400     05  FILLER                  PIC X.                                   
091500     EJECT                                                                
091600*01  -COPY W0008  -PRE ARTC-                                              
091700     05  FILLER                  PIC X.                                   
091800                                                                          
091900*01  -COPY W0008  -PRE WDK7I-                                             
092000     05  FILLER                  PIC X.                                   
092100     EJECT                                                                
092200*01  -COPY W0008  -PRE WDL6-                                              
092300     05  FILLER                  PIC X.                                   
092400                                                                          
092500*01  -COPY W0008  -PRE INLB-                                              
092600     05  FILLER                  PIC X.                                   
092700     EJECT                                                                
092800*01  -COPY W0008  -PRE INLE-                                              
092900     05  FILLER                  PIC X.                                   
093000                                                                          
093100*01  -COPY W0008  -PRE ZZAC-                                              
093200     05  FILLER                  PIC X.                                   
093300     EJECT                                                                
093400*01  -COPY W0008  -PRE XXBW-                                              
093500     05  FILLER                  PIC X.                                   
093600                                                                          
093700*01  -COPY W0008  -PRE 4505-                                              
093800     05  FILLER                  PIC X.                                   
093900     EJECT                                                                
094000*01  -COPY W0008  -PRE KREA-                                              
094100     05  FILLER                  PIC X.                                   
094200     EJECT                                                                
094300*01  -COPY W0008  -PRE USEA-                                              
094400     05  FILLER                  PIC X.                                   
094500     EJECT                                                                
094600*01  -COPY W0008  -PRE WDG2-                                              
094700     05  FILLER                  PIC X.                                   
094800     EJECT                                                                
094900*01  -COPY W0008  -PRE 9305-AVG-                                          
095000     05  FILLER                  PIC X.                                   
095100     EJECT                                                                
095200*01  -COPY W0008  -PRE AVG-WDB6-                                          
095300     05  FILLER                  PIC X.                                   
095400     EJECT                                                                
095500*01  -COPY W0008  -PRE LOGA-                                              
095600     05  FILLER                  PIC X.                                   
095700     EJECT                                                                
095800*01  -COPY W0008  -PRE FILB-                                              
095900     05  FILLER                  PIC X.                                   
096000     EJECT                                                                
096100*01  -COPY W0008  -PRE SAPA-                                              
096200     05  FILLER                  PIC X.                                   
096300     EJECT                                                                
096400*01  -COPY W0008  -PRE WDK7-                                              
096500     05  FILLER                  PIC X.                                   
096600     EJECT                                                                
096700*01  -COPY W0008  -PRE WDB6-                                              
096800     05  FILLER                  PIC X.                                   
096900     EJECT                                                                
097000*01  -COPY W0008  -PRE WDB6-NEXT-                                         
097100     05  FILLER                  PIC X.                                   
097200     EJECT                                                                
097300*01  -COPY W0008  -PRE LEV-                                               
097400     05  FILLER                  PIC X(5).                                
097500     EJECT                                                                
097600*01  -COPY W0008  -PRE 9305-                                              
097700     05  FILLER                  PIC X.                                   
097800     EJECT                                                                
097900*01  -COPY W0008  -PRE 2215-                                              
098000     05  FILLER                  PIC X.                                   
098100     EJECT                                                                
098200*01  -COPY W0008  -PRE 2217-                                              
098300     05  FILLER                  PIC X.                                   
098400     EJECT                                                                
098500*01  -COPY W0008  -PRE WDR2-                                              
098600     05  FILLER                  PIC X.                                   
098700     EJECT                                                                
098800*01  -COPY W0008  -PRE 2247-                                              
098900     05  FILLER                  PIC X.                                   
099000     EJECT                                                                
099100 PROCEDURE DIVISION  USING                                                
099200                     MSG-PCB DISP-PCB 6202-PCB                            
099300                     W6INLC-PCB W6INLA-PCB                                
099400                     W6KVAI-PCB W6KVAE-PCB                                
099500                     ARTC-PCB WDK7I-PCB WDL6-PCB                          
099600                     INLB-PCB INLE-PCB ZZAC-PCB                           
099700                     XXBW-PCB 4505-PCB KREA-PCB                           
099800                     USEA-PCB WDG2-PCB 9305-AVG-PCB AVG-WDB6-PCB          
099900                     LOGA-PCB FILB-PCB SAPA-PCB                           
100000                     WDK7-PCB WDB6-PCB WDB6-NEXT-PCB                      
100100                     LEV-PCB 9305-PCB 2215-PCB                            
100200                     2217-PCB WDR2-PCB 2247-PCB.                          
100300 MAIN SECTION.                                                            
100400     ENTRY 'DLITCBL' USING                                                
100500                     MSG-PCB DISP-PCB 6202-PCB                            
100600                     W6INLC-PCB W6INLA-PCB                                
100700                     W6KVAI-PCB W6KVAE-PCB                                
100800                     ARTC-PCB WDK7I-PCB WDL6-PCB                          
100900                     INLB-PCB INLE-PCB ZZAC-PCB                           
101000                     XXBW-PCB 4505-PCB KREA-PCB                           
101100                     USEA-PCB WDG2-PCB 9305-AVG-PCB AVG-WDB6-PCB          
101200                     LOGA-PCB FILB-PCB SAPA-PCB                           
101300                     WDK7-PCB WDB6-PCB WDB6-NEXT-PCB                      
101400                     LEV-PCB 9305-PCB 2215-PCB                            
101500                     2217-PCB WDR2-PCB 2247-PCB.                          
101600                                                                          
101700     PERFORM IMS-GET-MSG                                                  
101800     IF SEGMENT-FINNS                                                     
101900       PERFORM IMS-GN-MSG                                                 
102000                                                                          
102100       PERFORM A-INIT                                                     
102200                                                                          
102300       MOVE MID-IDLOPNRM         TO W-IDLOPNRM                            
102400       PERFORM IMS-GU-INLC-SEQB                                           
102500       IF SEGMENT-FINNS                                                   
102600                                                                          
102700       MOVE INLC-SEQB-IDDC     TO W-W6D101KY-IDDC                         
102800       MOVE INLC-SEQB-IDLEVNR  TO W-W6D101KY-IDLEVNR                      
102900       MOVE INLC-SEQB-IDFS     TO W-W6D101KY-IDFS                         
103000       MOVE INLC-SEQB-TIAVIDAT TO W-W6D101KY-TIAVIDAT                     
103100       MOVE INLC-SEQB-IDRADNR-INL TO W-IDRADNR-INL                        
103200                                                                          
103300       PERFORM IMS-GU-INLA-INL                                            
103400       MOVE INLA-INL-IDDC      TO WS-INLA-INL-IDDC                        
103500                                  W-IDDC                                  
103600                                  W-IDDC-K7                               
103610                                  WS-IDDC                                 
103700       MOVE INLA-INL-IDLEVNR   TO WS-INLA-INL-IDLEVNR                     
103800       MOVE INLA-INL-IDFS      TO WS-INLA-INL-IDFS                        
103900       MOVE INLA-INL-IDANALYS   TO WS-INLA-INL-IDANALYS                   
104000       MOVE INLA-INL-IDKST     TO WS-INLA-INL-IDKST                       
104100       MOVE INLA-INL-TIAVIDAT  TO WS-INLA-INL-TIAVIDAT                    
104200       MOVE INLA-INL-IDKONTO   TO WS-INLA-INL-IDKONTO                     
104300       MOVE INLA-INL-IDFS      TO WS-IDFS                                 
104400                                                                          
104500       MOVE INLA-INL-IDDC      TO W-IDDC-B6                               
104600       PERFORM IMS-GU-WDB601                                              
104700       MOVE DCS-KDVALISO       TO WS-ARTC21-KDVALISO                      
104800       IF DCS-CDC                                                         
104900         MOVE +1               TO W-KDCLAGER                              
105000                                  WS-KDCLAGER                             
105100       ELSE                                                               
105200         MOVE +2               TO W-KDCLAGER                              
105300                                  WS-KDCLAGER                             
105400       END-IF                                                             
105500                                                                          
105600*FÖR NDC:ERNAS SKULL HÄMTAR MAN LOKAL TID MHA ETT ANROP TILL              
105700*W005INIT. DETTA SKA KUNNA GÄLLA FÖR SAMTLIGA DC:N ÄVEN CDC               
105800*                                                                         
105900       MOVE ALL '+'         TO MSGI-WMSGINIT                              
106000       MOVE '013'              TO MSGI-KDCALL                             
106100       MOVE WS-INLA-INL-IDDC   TO WS-IDDC-LOCAL-DATE                      
106200                                                                          
106300       MOVE WS-IDDC-LOCAL   TO MSGI-IDUSER                                
106400                                                                          
106500       CALL W005INIT  USING  MSGI-WMSGINIT USEA-PCB                       
106600       MOVE MSGI-TILOKDAT  TO WS-TIAAMMDD-LOCAL                           
106700       MOVE WS-TIAAMMDD-LOCAL(1:2) TO WS-TIAA                             
106800       MOVE WS-TIAAMMDD-LOCAL(3:2) TO WS-TIMM                             
106900                                                                          
107000       PERFORM S04-KONVERTERA-IDFS-IDAVINR                                
107100                                                                          
107200       PERFORM IMS-GNP-INLA-ART                                           
107300       MOVE INLA-ART-IDARTNR   TO W-IDARTNR                               
107400                                                                          
107500       IF  INLA-ART-FLKLAR = NEJ                                          
107600                                                                          
107700         PERFORM S05-RAKNA-FRAM-RAPPORTERAT                               
107800                                                                          
107900         IF  (NOT KVALITET-MID)                                           
108000         AND MID-IDRADNR = ZERO                                           
108100*          -- ALLA RADER I PARTIER ACKUMULERAS                            
108200                                                                          
108300                                                                          
108400           PERFORM IMS-GNP-INLA-RAD                                       
108500                                                                          
108600           PERFORM UNTIL (SEGMENT-SAKNAS)                                 
108700             ADD INLA-RAD-KVINLART TO WS-TOT-KVINLART                     
108800** CD-PARTI                                                               
108900             IF  INLA-ART-ADTRDEST(1:2) = 'CD'                            
109000               MOVE INLA-ART-ADTRDEST(3:1) TO CD-IX                       
109100               ADD INLA-RAD-KVINLART TO WS-TOT-KVINLART-CD(CD-IX)         
109200             END-IF                                                       
109300                                                                          
109400             IF INLA-RAD-FLSVSLS = JA                                     
109500                ADD INLA-RAD-KVINLART                                     
109600                                 TO WS-TOT-KVINLART-SVS                   
109700             END-IF                                                       
109800             PERFORM IMS-GNP-INLA-RAD                                     
109900           END-PERFORM                                                    
110000                                                                          
110100         ELSE                                                             
110200           PERFORM B-SUM-KTRL-RADER                                       
110300                                                                          
110400           MOVE SPACE            TO WS-KDINLSTA                           
110500                                                                          
110600           IF  (NOT KVALITET-MID)                                         
110700             MOVE MID-IDRADNR    TO W-IDRADNR                             
110800             PERFORM IMS-GNP-INLA-RAD-F-KV                                
110900             MOVE INLA-RAD-KDINLSTA TO WS-KDINLSTA                        
111000           END-IF                                                         
111100         END-IF                                                           
111200                                                                          
111300         IF  SW-RAPP-OFULLST = NEJ                                        
111400         OR WS-KDINLSTA-INLAGD                                            
111500         OR WS-KDINLSTA-FRD                                               
111600                                                                          
111700           PERFORM C-LAS-IN-INFO                                          
111800           IF  SW-RAPP-OFULLST = NEJ                                      
111900             PERFORM D-SLUTRAPP                                           
112000           ELSE                                                           
112100             IF  WS-KDINLSTA-INLAGD OR WS-KDINLSTA-FRD                    
112200               PERFORM E-DELRAPP                                          
112300             END-IF                                                       
112400           END-IF                                                         
112500                                                                          
112600*NDC                                                                      
112700           IF INLA-ART-IDDC NOT = DCS-IDDC                                
112800              MOVE INLA-ART-IDDC   TO W-IDDC-B6                           
112900              PERFORM IMS-GU-WDB601                                       
113000              MOVE DCS-KDVALISO    TO WS-ARTC21-KDVALISO                  
113100           END-IF                                                         
113200           IF DCS-CDC OR DCS-NDC-NA OR DCS-AUSTRALIA OR DCS-JAPAN         
113300           OR  (DCS-CDC-TR AND WS-KDINLSTA-FRD)                           
113400           OR DCS-LAND-NON-VCC-OWNED                                      
113500             PERFORM F-EV-RO-TACKNING                                     
113600           END-IF                                                         
113700                                                                          
113800         END-IF                                                           
113900                                                                          
114000                                                                          
114100       ELSE                                                               
114200*        -- FLKLAR = JA: PARTIET TIDIGARE SLUTRAPPORTERAT.                
114300         CONTINUE                                                         
114400       END-IF                                                             
114500     END-IF                                                               
114600     END-IF                                                               
114700                                                                          
114800                                                                          
114900     PERFORM Z-FINIT                                                      
115000                                                                          
115100     MOVE ZERO TO RETURN-CODE                                             
115200     GOBACK                                                               
115300     .                                                                    
115400     EJECT                                                                
115500 A-INIT SECTION.                                                          
115600                                                                          
115700     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I19301                    
115800     MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                               
115900     MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                              
116000                                                                          
116100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
116200     MOVE MSG-IDPFK TO MFS-IDPFK                                          
116300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
116400                                                                          
116500     MOVE LOW-VALUE TO MSG-AREA                                           
116600     MOVE SPACE TO LOGG-WDL901                                            
116700                                                                          
116800*    -- C-LAGER FÖR KONTAKT MED ÖVRIGA SYSTEM                             
116900     MOVE 'IDAG'                 TO DAT-KDDATFORM                         
117000     CALL WDATKONV USING         DAT-KDDATFORM                            
117100                                 DAT-I-TIDATUM                            
117200                                 DAT-O-TIDATUM                            
117300                                 DAT-KDSVAR                               
117400                                                                          
117500     IF  DAT-KDSVAR-OK                                                    
117600       MOVE DAT-TIAAMMDD         TO WS-TIAAMMDD                           
117700       MOVE DAT-TIAAVVD          TO WS-TIAAVVD                            
117800     ELSE                                                                 
117900       STRING 'FEL FRÅN WDATKONV:' DAT-KDSVAR                             
118000         DELIMITED BY SIZE INTO FELTEXT                                   
118100       CALL FELLOG                                                        
118200     END-IF                                                               
118300                                                                          
118400     MOVE FUNCTION CURRENT-DATE (3:4) TO WS-DATE-YYMM                     
118500     MOVE 01                          TO WS-DATE-DD                       
118600                                                                          
118700     MOVE ZERO                   TO WS-IDLOGLOP                           
118800                                    WS-TOT-KVINLART                       
118900                                    WS-TOT-KVINLART-CD(1)                 
119000                                    WS-TOT-KVINLART-CD(2)                 
119100                                    WS-TOT-KVINLART-CD(3)                 
119200                                    WS-TOT-KVINLART-CD(4)                 
119300                                    WS-TOT-KVINLART-SVS                   
119400                                    WS-ANT-KR                             
119500                                    WS-KVINLART-INLAGD                    
119600                                    WS-KVINLART-FRD                       
119700                                    WS-KVINLART-AKT-FRD                   
119800                                    WS-KVINLART-KVA                       
119900                                    WS-KVINLART-ANT                       
120000                                    WS-KVINLART-ANT-AVV                   
120100                                    WS-KVINLART-AVV                       
120200                                    WS-KVINLART-ANT-AVV-KVA-RET           
120300                                    WS-KVINLART-AKT-SALDO                 
120400                                    WS-AVVIK-KVAKS                        
120500                                    WS-AVVIK-KVLS                         
120600                                    WS-KDAVVKV                            
120700                                    WS-KDAVVANT                           
120800                                    WS-KR-KVART-RET                       
120900     MOVE NEJ                    TO SW-KDINLSTA-KVA                       
121000                                    SW-RAPP-OFULLST                       
121100                                    FL-RT6-KR                             
121200     .                                                                    
121300     EJECT                                                                
121400 B-SUM-KTRL-RADER SECTION.                                                
121500                                                                          
121600     PERFORM IMS-GNP-INLA-RAD                                             
121700                                                                          
121800     PERFORM UNTIL (SEGMENT-SAKNAS                                        
121900                OR  SW-RAPP-OFULLST = JA)                                 
122000                                                                          
122010       IF INLA-RAD-IDANSTNR > ZERO                                        
122020          MOVE INLA-RAD-IDANSTNR TO WS-INLA-RAD-IDANSTNR-BIN              
122030       END-IF                                                             
122040                                                                          
122100       EVALUATE TRUE                                                      
122200                                                                          
122300         WHEN INLA-RAD-KDINLSTA = 'INL' OR 'VOR'                          
122400           ADD INLA-RAD-KVINLART TO WS-KVINLART-INLAGD                    
122500           IF  INLA-RAD-IDRADNR = MID-IDRADNR                             
122600             MOVE INLA-RAD-KVINLART TO WS-KVINLART-AKT-SALDO              
122700           END-IF                                                         
122800                                                                          
122900         WHEN INLA-RAD-KDINLSTA = 'FRD'                                   
123000           IF  INLA-RAD-IDRADNR = MID-IDRADNR                             
123100             MOVE INLA-RAD-KVINLART TO WS-KVINLART-AKT-FRD                
123200           END-IF                                                         
123300           ADD INLA-RAD-KVINLART TO WS-KVINLART-FRD                       
123400                                                                          
123500         WHEN INLA-RAD-KDINLSTA = 'KVA'                                   
123600           MOVE JA               TO SW-KDINLSTA-KVA                       
123700           ADD INLA-RAD-KVINLART TO WS-KVINLART-KVA                       
123800           ADD INLA-RAD-KVINLART TO WS-KVINLART-ANT-AVV-KVA-RET           
123900                                                                          
124000         WHEN INLA-RAD-KDINLSTA = 'ANT'                                   
124100           ADD INLA-RAD-KVINLART TO WS-KVINLART-ANT                       
124200           ADD INLA-RAD-KVINLART TO WS-KVINLART-ANT-AVV                   
124300           ADD INLA-RAD-KVINLART TO WS-KVINLART-ANT-AVV-KVA-RET           
124400           MOVE INLA-RAD-IDANSTNR TO WS-INLA-RAD-IDANSTNR                 
124500                                                                          
124600         WHEN INLA-RAD-KDINLSTA = 'AVV'                                   
124700           ADD INLA-RAD-KVINLART TO WS-KVINLART-ANT-AVV                   
124800           ADD INLA-RAD-KVINLART TO WS-KVINLART-AVV                       
124900           ADD INLA-RAD-KVINLART TO WS-KVINLART-ANT-AVV-KVA-RET           
125000           MOVE INLA-RAD-IDANSTNR TO WS-INLA-RAD-IDANSTNR                 
125100                                                                          
125200         WHEN INLA-RAD-KDINLSTA = 'RET'                                   
125300           ADD INLA-RAD-KVINLART TO WS-KVINLART-RET                       
125400           ADD INLA-RAD-KVINLART TO WS-KVINLART-ANT-AVV-KVA-RET           
125500           MOVE JA               TO SW-KDINLSTA-RET                       
125600                                                                          
125700         WHEN INLA-RAD-KDINLSTA = 'TRP'                                   
125800             ADD INLA-RAD-KVINLART TO WS-KVINLART-TRP                     
125900                                                                          
126000         WHEN OTHER                                                       
126100           MOVE JA             TO SW-RAPP-OFULLST                         
126200       END-EVALUATE                                                       
126300                                                                          
126400       IF  SW-RAPP-OFULLST = NEJ                                          
126500         PERFORM IMS-GNP-INLA-RAD                                         
126600       END-IF                                                             
126700                                                                          
126800     END-PERFORM                                                          
126900                                                                          
127000     IF  SW-RAPP-OFULLST = NEJ                                            
127100       IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                 
127200          MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                            
127300          PERFORM IMS-GU-WDB601                                           
127400       END-IF                                                             
127500       IF INLA-ART-KDRT = 6                                               
127600**                                                                        
127700         MOVE LOW-VALUE            TO W-W6H7C1KY-MIN-X                    
127800         MOVE HIGH-VALUE           TO W-W6H7C1KY-MAX-X                    
127900         MOVE INLA-ART-IDLOPNRM    TO W-W6H7C1KY-MIN-IDLOPNRM             
128000                                      W-W6H7C1KY-MAX-IDLOPNRM             
128100         MOVE WS-INLA-INL-TIAVIDAT TO W-W6H7C1KY-MIN-DAAVSDAT             
128200                                      W-W6H7C1KY-MAX-DAAVSDAT             
128300         IF WS-INLA-INL-TIAVIDAT NOT = ZERO                               
128400           IF WS-INLA-INL-TIAVIDAT < 500000                               
128500             MOVE 20               TO W-W6H7C1KY-MIN-DAAVSDAT(1:2)        
128600                                      W-W6H7C1KY-MAX-DAAVSDAT(1:2)        
128700           ELSE                                                           
128800             IF WS-INLA-INL-TIAVIDAT < 999999                             
128900               MOVE 19             TO W-W6H7C1KY-MIN-DAAVSDAT(1:2)        
129000                                      W-W6H7C1KY-MAX-DAAVSDAT(1:2)        
129100             ELSE                                                         
129200               MOVE 99999999       TO W-W6H7C1KY-MIN-DAAVSDAT             
129300                                      W-W6H7C1KY-MAX-DAAVSDAT             
129400             END-IF                                                       
129500           END-IF                                                         
129600         END-IF                                                           
129700         PERFORM IMS-GU-KVAI-SEQC                                         
129800         IF SEGMENT-FINNS                                                 
129900           MOVE KVAI-SEQC-IDKR       TO W-IDKR                            
130000           PERFORM IMS-GU-KVAE-KR                                         
130100           IF SEGMENT-FINNS                                               
130200             IF KVAE-KR-IDKRFEL(1:1) = 'P'                                
130300               MOVE NEJ TO FL-RT6-KR                                      
130400               MOVE KVAE-KR-FLKRGODK     TO WS-KVAE-FLKRGODK              
130500               MOVE KVAE-KR-FLKRLIM      TO WS-KVAE-FLKRLIM               
130600               MOVE KVAE-KR-IDKRFEL      TO WS-KVAE-IDKRFEL               
130700             ELSE                                                         
130800               MOVE JA  TO FL-RT6-KR                                      
130900             END-IF                                                       
131000           END-IF                                                         
131100         END-IF                                                           
131200       END-IF                                                             
131300      IF INLA-ART-KDRT =  8 OR FL-RT6-KR = JA                             
131400      OR DCS-CANADA OR DCS-NDC-PF OR DCS-NDC-OTHERS OR DCS-NDC-SA         
131500*INDIA AND KOREA IS IN NDC-PF                                             
131600*DUBAI IS IN NDC-OTHERS                                                   
131700         IF  WS-INLE-KVANTMOT-INNAN + WS-KVINLART-AKT-SALDO               
131800             = INLA-ART-KVAVIS             -                              
131900               WS-KVINLART-ANT-AVV-KVA-RET -                              
132000               WS-KVINLART-AKT-FRD         -                              
132100               WS-KVINLART-TRP                                            
132200*FIXAR TILL AK-SALDOT FÖR DE SOM INTE SKER GENOM KONTROLL RAPPORT         
132300*DETTA GÖRS ÄVEN FÖR NDC:ERNA                                             
132400           IF WS-KVINLART-ANT-AVV = +0 AND                                
132500              WS-KVINLART-KVA = +0 AND                                    
132600              WS-KVINLART-RET = +0                                        
132700              CONTINUE                                                    
132800           ELSE                                                           
132900             IF WS-KVINLART-ANT-AVV = +0                                  
133000               CONTINUE                                                   
133100             ELSE                                                         
133200               MOVE +1           TO WS-KDAVVANT                           
133300               SUBTRACT WS-KVINLART-ANT-AVV FROM WS-AVVIK-KVAKS           
133400             END-IF                                                       
133500             IF WS-KVINLART-KVA = +0                                      
133600               CONTINUE                                                   
133700             ELSE                                                         
133800               MOVE WS-KVINLART-KVA TO WS-KR-KVART-RET                    
133900               ADD  WS-KVINLART-RET TO WS-KR-KVART-RET                    
134000               SUBTRACT WS-KVINLART-KVA     FROM WS-AVVIK-KVAKS           
134100               SUBTRACT WS-KVINLART-RET     FROM WS-AVVIK-KVAKS           
134200             END-IF                                                       
134300           END-IF                                                         
134400         ELSE                                                             
134500           MOVE JA             TO SW-RAPP-OFULLST                         
134600         END-IF                                                           
134700      ELSE                                                                
134800       IF  WS-INLE-KVANTMOT-INNAN + WS-KVINLART-AKT-SALDO                 
134900           = INLA-ART-KVAVIS             -                                
135000             WS-KVINLART-ANT-AVV-KVA-RET -                                
135100             WS-KVINLART-AKT-FRD         -                                
135200             WS-KVINLART-TRP                                              
135300         PERFORM BA-KTRL-AVVIK                                            
135400       ELSE                                                               
135500         MOVE JA             TO SW-RAPP-OFULLST                           
135600       END-IF                                                             
135700      END-IF                                                              
135800     END-IF                                                               
135900     .                                                                    
136000     EJECT                                                                
136100 BA-KTRL-AVVIK SECTION.                                                   
136200                                                                          
136300     MOVE LOW-VALUE              TO W-W6H7C1KY-MIN-X                      
136400     MOVE HIGH-VALUE             TO W-W6H7C1KY-MAX-X                      
136500     MOVE INLA-ART-IDLOPNRM      TO W-W6H7C1KY-MIN-IDLOPNRM               
136600                                    W-W6H7C1KY-MAX-IDLOPNRM               
136700     MOVE WS-INLA-INL-TIAVIDAT   TO W-W6H7C1KY-MIN-DAAVSDAT               
136800                                    W-W6H7C1KY-MAX-DAAVSDAT               
136900     IF WS-INLA-INL-TIAVIDAT NOT = ZERO                                   
137000       IF WS-INLA-INL-TIAVIDAT < 500000                                   
137100         MOVE 20                 TO W-W6H7C1KY-MIN-DAAVSDAT (1:2)         
137200                                    W-W6H7C1KY-MAX-DAAVSDAT (1:2)         
137300       ELSE                                                               
137400         IF WS-INLA-INL-TIAVIDAT < 999999                                 
137500           MOVE 19               TO W-W6H7C1KY-MIN-DAAVSDAT (1:2)         
137600                                    W-W6H7C1KY-MAX-DAAVSDAT (1:2)         
137700         ELSE                                                             
137800           MOVE 99999999         TO W-W6H7C1KY-MIN-DAAVSDAT               
137900                                    W-W6H7C1KY-MAX-DAAVSDAT               
138000         END-IF                                                           
138100       END-IF                                                             
138200     END-IF                                                               
138300     PERFORM IMS-GU-KVAI-SEQC                                             
138400                                                                          
138500     PERFORM UNTIL ((NOT SEGMENT-FINNS)                                   
138600                OR  WS-ANT-KR >= K-MAX-KR)                                
138700                                                                          
138800       ADD +1                    TO WS-ANT-KR                             
138900                                                                          
139000       MOVE KVAI-SEQC-IDKR       TO W-IDKR                                
139100       PERFORM IMS-GU-KVAE-KR                                             
139200                                                                          
139300       IF KVAE-KR-FLANNULL = JA                                           
139400         IF  KVAE-KR-IDKRFEL     = 'PA' OR 'PB' OR 'K '                   
139500           MOVE KVAE-KR-FLANNULL TO WS-KVAE-FLANNULL-ANTAL                
139600         ELSE                                                             
139700           MOVE KVAE-KR-FLANNULL TO WS-KVAE-FLANNULL-TEKNISK              
139800         END-IF                                                           
139900       ELSE                                                               
140000         IF  KVAE-KR-KDKRSTA       >= 2                                   
140100         AND KVAE-KR-FLKRGODK      = JA                                   
140200                                                                          
140300           ADD KVAE-KR-KVART-RET   TO WS-KR-KVART-RET                     
140400           ADD KVAE-KR-KVART-SKROT TO WS-KR-KVART-RET                     
140500                                                                          
140600           IF  KVAE-KR-IDKRFEL     = 'PA' OR 'PB' OR 'K '                 
140700*            -- ANTAL-AVVIKELSE                                           
140800              MOVE +1              TO WS-KDAVVANT                         
140900              IF TEKNISK-KR-FINNS = JA                                    
141000                SUBTRACT WS-KVINLART-ANT-AVV FROM WS-AVVIK-KVAKS          
141100                                                                          
141200                IF  WS-KVINLART-ANT-AVV NOT =                             
141300                                        KVAE-KR-KVART-AAVV * -1           
141400                  COMPUTE WS-AVVIK-KVLS = WS-AVVIK-KVLS                   
141500                                        + WS-KVINLART-ANT-AVV             
141600                                        + KVAE-KR-KVART-AAVV              
141700                END-IF                                                    
141800                COMPUTE WS-AVVIK-KVLS = WS-AVVIK-KVLS -                   
141900                                        KVAE-KR-KVART-RET                 
142000              ELSE                                                        
142100                SUBTRACT WS-KVINLART-ANT-AVV FROM WS-AVVIK-KVAKS          
142200                SUBTRACT WS-KVINLART-RET     FROM WS-AVVIK-KVAKS          
142300                                                                          
142400                IF  WS-KVINLART-ANT-AVV + WS-KVINLART-RET NOT =           
142500                    (KVAE-KR-KVART-AAVV * -1) + KVAE-KR-KVART-RET         
142600                  COMPUTE WS-AVVIK-KVLS = WS-AVVIK-KVLS                   
142700                                        + WS-KVINLART-ANT-AVV             
142800                                        + WS-KVINLART-RET                 
142900                                        + KVAE-KR-KVART-AAVV              
143000                                        - KVAE-KR-KVART-RET               
143100                END-IF                                                    
143200              END-IF                                                      
143300                                                                          
143400              MOVE JA                 TO ANTALS-KR-FINNS                  
143500                                                                          
143600           ELSE                                                           
143700*          -- KVALITET-AVVIKELSE                                          
143800                                                                          
143900             IF  KVAE-KR-KVART-RET > ZERO OR                              
144000                 KVAE-KR-KVART-SKROT > ZERO                               
144100               MOVE +2             TO WS-KDAVVKV                          
144200             ELSE                                                         
144300               MOVE +1             TO WS-KDAVVKV                          
144400             END-IF                                                       
144500                                                                          
144600             IF ANTALS-KR-FINNS = JA                                      
144700               SUBTRACT WS-KVINLART-KVA FROM WS-AVVIK-KVAKS               
144800               COMPUTE WS-KR-KVART-KVA = KVAE-KR-KVART-RET                
144900                                       + KVAE-KR-KVART-SKROT              
145000                                       - KVAE-KR-KVART-SJUST              
145100                                                                          
145200               IF  WS-KVINLART-KVA   NOT = WS-KR-KVART-KVA                
145300                 COMPUTE WS-AVVIK-KVLS = WS-AVVIK-KVLS                    
145400                                       + WS-KVINLART-KVA                  
145500                                       - WS-KR-KVART-KVA                  
145600               END-IF                                                     
145700             ELSE                                                         
145800               SUBTRACT WS-KVINLART-KVA FROM WS-AVVIK-KVAKS               
145900               SUBTRACT WS-KVINLART-RET FROM WS-AVVIK-KVAKS               
146000               COMPUTE WS-KR-KVART-KVA = KVAE-KR-KVART-RET                
146100                                       + KVAE-KR-KVART-SKROT              
146200                                       - KVAE-KR-KVART-SJUST              
146300                                                                          
146400               IF  WS-KVINLART-KVA + WS-KVINLART-RET  NOT =               
146500                                     WS-KR-KVART-KVA                      
146600                 COMPUTE WS-AVVIK-KVLS = WS-AVVIK-KVLS                    
146700                                       + WS-KVINLART-KVA                  
146800                                       + WS-KVINLART-RET                  
146900                                       - WS-KR-KVART-KVA                  
147000               END-IF                                                     
147100                                                                          
147200               MOVE JA              TO TEKNISK-KR-FINNS                   
147300                                                                          
147400             END-IF                                                       
147500           END-IF                                                         
147600                                                                          
147700         ELSE                                                             
147800           MOVE JA             TO SW-RAPP-OFULLST                         
147900         END-IF                                                           
148000       END-IF                                                             
148100                                                                          
148200       IF KVAE-KR-IDKRFEL(1:1) = 'P'                                      
148300         MOVE KVAE-KR-FLKRGODK     TO WS-KVAE-FLKRGODK                    
148400         MOVE KVAE-KR-FLKRLIM      TO WS-KVAE-FLKRLIM                     
148500         MOVE KVAE-KR-IDKRFEL      TO WS-KVAE-IDKRFEL                     
148600       END-IF                                                             
148700       IF  WS-ANT-KR < K-MAX-KR                                           
148800         PERFORM IMS-GN-KVAI-SEQC                                         
148900       END-IF                                                             
149000                                                                          
149100     END-PERFORM                                                          
149200                                                                          
149300                                                                          
149400     IF  WS-KDAVVANT          = ZERO                                      
149500     AND (WS-KVINLART-AVV NOT = ZERO                                      
149600      OR  WS-KVINLART-ANT NOT = ZERO)                                     
149700*    -- KR FÖR ANT SAKNAS TROTS ATT AVV-DIFF EL ANT-DIFF FINNS            
149800       IF WS-KVAE-FLANNULL-ANTAL = NEJ                                    
149900         PERFORM BAA-SKAPA-TRANS-6202                                     
150000         MOVE JA                   TO SW-RAPP-OFULLST                     
150100       ELSE                                                               
150200         IF WS-INLA-INL-IDDC NOT = DCS-IDDC                               
150300            MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                          
150400            PERFORM IMS-GU-WDB601                                         
150500         END-IF                                                           
150600         IF DCS-CDC-TR                                                    
150700            MOVE JA                TO SW-RAPP-OFULLST                     
150800         ELSE                                                             
150900* FIXAR TILLBAKA SALDONA NÄR INLEV SÄGER AVVIKELSE MEN KVALITE OK         
151000            SUBTRACT WS-KVINLART-ANT-AVV FROM WS-AVVIK-KVAKS              
151100            ADD WS-KVINLART-ANT-AVV      TO   WS-AVVIK-KVLS               
151200          END-IF                                                          
151300       END-IF                                                             
151400     END-IF                                                               
151500                                                                          
151600                                                                          
151700     IF WS-KDAVVKV = +0       AND                                         
151800       (SW-KDINLSTA-KVA = JA  OR SW-KDINLSTA-RET = JA)                    
151900       IF ANTALS-KR-FINNS = JA AND SW-KDINLSTA-KVA = NEJ                  
152000         CONTINUE                                                         
152100       ELSE                                                               
152200         IF WS-KVAE-FLANNULL-TEKNISK = NEJ                                
152300           MOVE JA                   TO SW-RAPP-OFULLST                   
152400         ELSE                                                             
152500* FIXAR TILLBAKA SALDONA NÄR INLEV SÄGER AVVIKELSE MEN KVALITE OK         
152600           IF ANTALS-KR-FINNS = JA                                        
152700             SUBTRACT WS-KVINLART-KVA     FROM WS-AVVIK-KVAKS             
152800             ADD      WS-KVINLART-KVA     TO   WS-AVVIK-KVLS              
152900           ELSE                                                           
153000             SUBTRACT WS-KVINLART-KVA     FROM WS-AVVIK-KVAKS             
153100             SUBTRACT WS-KVINLART-RET     FROM WS-AVVIK-KVAKS             
153200             ADD      WS-KVINLART-KVA     TO   WS-AVVIK-KVLS              
153300             ADD      WS-KVINLART-RET     TO   WS-AVVIK-KVLS              
153400           END-IF                                                         
153500         END-IF                                                           
153600       END-IF                                                             
153700     END-IF                                                               
153800     .                                                                    
153900     EJECT                                                                
154000 BAA-SKAPA-TRANS-6202 SECTION.                                            
154100                                                                          
154200     MOVE ALL '+'                TO MOD6202-MID-W6I20201                  
154300                                                                          
154400     MOVE SPACE                  TO MOD6202-MID-IDKR-UT                   
154500     MOVE INLA-ART-IDLOPNRM      TO MOD6202-MID-IDLOPNRM                  
154600     MOVE WS-INLA-INL-TIAVIDAT   TO MOD6202-MID-TIAVSDAT                  
154700     COMPUTE MOD6202-MID-KVANTMOT = INLA-ART-KVAVIS -                     
154800                                    WS-KVINLART-ANT-AVV                   
154900     MOVE WS-INLA-RAD-IDANSTNR   TO MOD6202-MID-BEKRBEH                   
155000     MOVE '00.5'                 TO MOD6202-MID-KVKRBEH                   
155100                                                                          
155200     COMPUTE P-TO-P-KVLL       = LNG-P-TO-P-PREFIX                        
155300            + LENGTH OF MOD6202-MID-W6I20201                              
155400                                                                          
155500     MOVE 'W6T202X '           TO P-TO-P-KDTRANS                          
155600     MOVE '6193'               TO P-TO-P-IDTRANS                          
155700     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
155800                                                                          
155900     MOVE MOD6202-MID-W6I20201 TO P-TO-P-DATA                             
156000                                                                          
156100     PERFORM IMS-ISRT-ALT-MSG-6202                                        
156200     .                                                                    
156300     EJECT                                                                
156400 C-LAS-IN-INFO SECTION.                                                   
156500                                                                          
156600     MOVE INLA-ART-IDARTNR       TO W-IDARTNR                             
156700     PERFORM IMS-GU-ARTC01                                                
156800     MOVE ARTC-ART-IDFTG           TO WS-ARTC-IDFTG                       
156900     MOVE ARTC-ART-IDLEVNR         TO WS-ARTC-IDLEVNR                     
157000     MOVE ARTC-ART-KDPRODSL        TO WS-ARTC-KDPRODSL                    
157100     MOVE ARTC-ART-IDFKNGRP        TO WS-ARTC-IDFKNGRP                    
157200     MOVE ARTC-ART-KDSORT          TO WS-ARTC-KDSORT                      
157300                                                                          
157400     PERFORM IMS-GHNP-ARTC11                                              
157500******                                                                    
157600     MOVE ARTC-CLAG-IDINK          TO WS-ARTC-IDINK-X                     
157700       IF ARTC-CLAG-IDINK (1:3) NUMERIC                                   
157800          MOVE ARTC-CLAG-IDINK (1:3) TO WS-ARTC-IDINK                     
157900       ELSE                                                               
158000          IF ARTC-CLAG-IDINK (2:3) NUMERIC                                
158100             MOVE ARTC-CLAG-IDINK (2:3) TO WS-ARTC-IDINK                  
158200          ELSE                                                            
158300             MOVE ZERO TO WS-ARTC-IDINK                                   
158400          END-IF                                                          
158500       END-IF                                                             
158600******                                                                    
158700     MOVE ARTC-CLAG-IDANSK          TO WS-ARTC-IDANSK                     
158800     MOVE ARTC-CLAG-KDHF            TO WS-ARTC-KDHF                       
158900     MOVE ARTC-CLAG-KDVVKL          TO WS-ARTC-KDVVKL                     
159000     MOVE ARTC-CLAG-KDLEVSP         TO WS-ARTC-KDLEVSP                    
159100                                                                          
159200     MOVE ARTC-CLAG-KDTIPPR       TO WS-ARTC-KDTIPPR                      
159300     MOVE ARTC-CLAG-KDVTH         TO WS-ARTC-KDVTH                        
159400     MOVE ARTC-CLAG-PRARTSTD      TO WS-ARTC-PRARTSTD                     
159500     MOVE ARTC-CLAG-PRDIRLON      TO WS-ARTC-PRDIRLON                     
159600     MOVE ARTC-CLAG-PRDMTRL       TO WS-ARTC-PRDMTRL                      
159700     MOVE ARTC-CLAG-PRINK         TO WS-ARTC-PRINK                        
159800     MOVE ARTC-CLAG-PRHEMTAG      TO WS-ARTC-PRHEMTAG                     
159900     MOVE ARTC-CLAG-PROVRPAL      TO WS-ARTC-PROVRPAL                     
160000     MOVE ARTC-CLAG-BEFT          TO WS-ARTC-BEFT                         
160100     MOVE ARTC-CLAG-KDPSLLOC      TO WS-ARTC-KDPSLLOC                     
160200                                                                          
160300     PERFORM IMS-GNP-ARTC23                                               
160400     IF SEGMENT-FINNS                                                     
160500       MOVE AVT-IDAVTAL           TO WS-ARTC23-IDAVTAL                    
160600     ELSE                                                                 
160700       MOVE ZERO                  TO WS-ARTC23-IDAVTAL                    
160800     END-IF                                                               
160900                                                                          
161000     MOVE ZERO                    TO WS-ARTC21-PRARTBEL-PR                
161100     MOVE ZERO                    TO WS-ARTC21-PRARTBEL-SUM               
161200     MOVE ZERO                    TO WS-ARTC21-PRARTBES-PR                
161300                                                                          
161400     IF  INLA-ART-KDRT <= 06                                              
161500       PERFORM CA-LAS-PRISINFO                                            
161600     END-IF                                                               
161700                                                                          
161800     MOVE WS-INLA-INL-IDLEVNR   TO W-IDLEVNR                              
161900     PERFORM IMS-GU-WLLEVA01                                              
162000     IF SEGMENT-SAKNAS                                                    
162100       MOVE 1    TO W-RETULF                                              
162200     ELSE                                                                 
162300       MOVE WS-INLA-INL-IDDC TO W-IDDC-B6                                 
162400       PERFORM IMS-GU-WDB601                                              
162500       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
162600       PERFORM IMS-GNP-WLLEVA11                                           
162700       IF SEGMENT-FINNS                                                   
162800         IF LEV-TULL-TITULF < WS-DAGENS-DATUM                             
162900           MOVE LEV-TULL-RETULF-1  TO W-RETULF                            
163000         ELSE                                                             
163100           MOVE LEV-TULL-RETULF-2  TO W-RETULF                            
163200         END-IF                                                           
163300       ELSE                                                               
163400         MOVE 1                    TO W-RETULF                            
163500       END-IF                                                             
163600     END-IF                                                               
163700     .                                                                    
163800     EJECT                                                                
163900                                                                          
164000 CA-LAS-PRISINFO SECTION.                                                 
164100     MOVE MSGI-TILOKDAT     TO DAT-I-TIDATUM                              
164200                               WS-IDAG                                    
164300     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
164400     CALL WDATKONV USING       DAT-KDDATFORM                              
164500                               DAT-I-TIDATUM                              
164600                               DAT-O-TIDATUM                              
164700                               DAT-KDSVAR                                 
164800     IF DAT-KDSVAR-FEL                                                    
164900       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
165000       CALL FELLOG                                                        
165100     END-IF                                                               
165200                                                                          
165300     MOVE DAT-TISEKEL TO WS-DAGENS-SEKEL                                  
165400     MOVE NEJ TO PRIS-FINNS-SW                                            
165500***** NDC THAT HAVE PRICEROWS ON 5206 *****                               
165600     IF DCS-NDC-CN                                                        
165700     OR DCS-USA                                                           
165800       PERFORM CAA-LAS-PRISINFO                                           
165900     ELSE                                                                 
166000       PERFORM CAB-LAS-PRISINFO                                           
166100     END-IF                                                               
166200     .                                                                    
166300     EJECT                                                                
166400                                                                          
166500 CAA-LAS-PRISINFO SECTION.                                                
166600*    -- WDK711                                                            
166700     PERFORM IMS-GU-WDK711                                                
166800     IF SEGMENT-FINNS                                                     
166900                                                                          
167000*    -- WDK724                                                            
167100       MOVE WS-INLA-INL-IDLEVNR   TO W-IDLEVNR-PR                         
167200       MOVE WS-INLA-INL-TIAVIDAT  TO WS-DAAVIDAT-YYMMDD                   
167300       IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                    
167400         MOVE 19                        TO WS-DAAVIDAT-SEKEL              
167500       ELSE                                                               
167600         MOVE 20                        TO WS-DAAVIDAT-SEKEL              
167700       END-IF                                                             
167800       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAAVIDAT                     
167900       PERFORM IMS-GNP-WDK724                                             
168000                                                                          
168100       IF SEGMENT-FINNS                                                   
168200         MOVE JA TO PRIS-FINNS-SW                                         
168300         MOVE SPRL-PRARTBEL-PR    TO WS-ARTC21-PRARTBEL-PR                
168400         MOVE SPRL-PRARTBEL-PR    TO WS-ARTC21-PRARTBEL-SUM               
168500         MOVE SPRL-PRARTBES-PR    TO WS-ARTC21-PRARTBES-PR                
168600         MOVE SPRL-KDVALISO       TO WS-ARTC21-KDVALISO                   
168700       END-IF                                                             
168800     END-IF                                                               
168900     .                                                                    
169000     EJECT                                                                
169100 CAB-LAS-PRISINFO SECTION.                                                
169200*    -- ARTC11                                                            
169300     PERFORM IMS-GHU-ARTC11                                               
169400                                                                          
169500*    -- ARTC21                                                            
169600     MOVE WS-INLA-INL-IDLEVNR   TO W-IDLEVNR-21                           
169700     PERFORM IMS-GNP-ARTC21                                               
169800                                                                          
169900     PERFORM UNTIL SEGMENT-SAKNAS OR PRIS-FINNS                           
170000      COMPUTE W-PRL-DADAT = 99999999 - ARTC-PRL-DAPRLIST-9KOMPL           
170100      IF ARTC-PRL-KDSTATUS-PR = +1 AND                                    
170200        W-PRL-DADAT <= WS-DAGENS-DATUM                                    
170300         MOVE JA TO PRIS-FINNS-SW                                         
170400         MOVE ARTC-PRL-PRARTBEL-PR  TO WS-ARTC21-PRARTBEL-PR              
170500         MOVE ARTC-PRL-PRARTBEL-PR  TO WS-ARTC21-PRARTBEL-SUM             
170600         MOVE ARTC-PRL-PRARTBES-PR  TO WS-ARTC21-PRARTBES-PR              
170700         MOVE ARTC-PRL-KDVALISO     TO WS-ARTC21-KDVALISO                 
170800       ELSE                                                               
170900         PERFORM IMS-GNP-ARTC21                                           
171000       END-IF                                                             
171100     END-PERFORM                                                          
171200     .                                                                    
171300     EJECT                                                                
171400 D-SLUTRAPP SECTION.                                                      
171500                                                                          
171600     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
171700        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
171800        PERFORM IMS-GU-WDB601                                             
171900     END-IF                                                               
172000     IF DCS-CDC-TR                                                        
172100         IF WS-KDINLSTA-INLAGD                                            
172200           PERFORM S07-UPPDATERA-ST-INL                                   
172300         END-IF                                                           
172400     END-IF                                                               
172500                                                                          
172600     PERFORM DB-SLUTUPPD-INLE-INLC-HIST                                   
172700*                                                                         
172800     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
172900        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
173000        PERFORM IMS-GU-WDB601                                             
173100     END-IF                                                               
173200     IF DCS-CDC OR DCS-CDC-TR OR DCS-NDC-CN OR DCS-USA                    
173300       PERFORM DC-SLUTUPPD-INLB-LEVPL                                     
173400                                                                          
173500*--  SKALL LOGGA ALLA R32-RAPP FÖR EDI TILL LEVERANTÖRERNA.               
173600       IF INLA-ART-KDRT = 00                                              
173700         PERFORM DH-LOGGA-EDI-LEV-PLANER                                  
173800       END-IF                                                             
173900     END-IF                                                               
174000                                                                          
174100     PERFORM DD-SLUTUPPD-ARTC-ARTS                                        
174200                                                                          
174300     PERFORM DE-LOGG-ZZAC                                                 
174400                                                                          
174500     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
174600        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
174700        PERFORM IMS-GU-WDB601                                             
174800     END-IF                                                               
174900     IF DCS-CDC OR DCS-CDC-TR                                             
175000       PERFORM DF-HTR-DISPDAT-INL                                         
175100     END-IF                                                               
175200                                                                          
175300     PERFORM DG-SLUTUPPD-INLA                                             
175400     .                                                                    
175500     EJECT                                                                
175600 DB-SLUTUPPD-INLE-INLC-HIST SECTION.                                      
175700                                                                          
175800     IF INLA-ART-IDDC NOT = DCS-IDDC                                      
175900        MOVE INLA-ART-IDDC   TO W-IDDC-B6                                 
176000        PERFORM IMS-GU-WDB601                                             
176100     END-IF                                                               
176200     IF DCS-CDC OR DCS-CDC-TR                                             
176300       PERFORM DBA-SLUTUPPD-INLE-HIST                                     
176400     ELSE                                                                 
176500       PERFORM DBB-SLUTUPPD-INLC-HIST                                     
176600     END-IF                                                               
176700     .                                                                    
176800     EJECT                                                                
176900 DBA-SLUTUPPD-INLE-HIST SECTION.                                          
177000     SKIP2                                                                
177100     PERFORM IMS-GHU-INLE-MOT                                             
177200** FÖR ATT HANTERA ATT FIXADE PARTINUMMER PÅ W6D1 SOM EJ ÄR               
177300** ÄNDRADE PÅ WDL2                                                        
177400     IF SEGMENT-SAKNAS                                                    
177500        SUBTRACT 90000 FROM W-IDLOPNRM                                    
177600        PERFORM IMS-GHU-INLE-MOT                                          
177700        IF SEGMENT-FINNS                                                  
177800          ADD      90000 TO   W-IDLOPNRM                                  
177900        ELSE                                                              
178000          CALL FELLOG                                                     
178100        END-IF                                                            
178200     END-IF                                                               
178300                                                                          
178400     MOVE 'R32'                  TO INLE-MOT-IDPTYP                       
178500     MOVE WS-TIAAMMDD            TO INLE-MOT-TIUPPDAT                     
178600     MOVE INLA-ART-ADLAGOMR      TO INLE-MOT-ADLAGOMR                     
178700     MOVE INLA-ART-ADGANG        TO INLE-MOT-ADGANG                       
178800     MOVE INLA-ART-ADPLATS       TO INLE-MOT-ADPLATS                      
178900     MOVE WS-KDAVVKV             TO INLE-MOT-KDAVVKV                      
179000     MOVE WS-KDAVVANT            TO INLE-MOT-KDAVVANT                     
179100     MOVE WS-KVINLART-FRD        TO INLE-MOT-KVFORDEL                     
179200                                                                          
179300     IF  (NOT KVALITET-MID)                                               
179400     AND MID-IDRADNR = ZERO                                               
179500       ADD WS-TOT-KVINLART       TO INLE-MOT-KVANTMOT                     
179600     ELSE                                                                 
179700       IF  WS-KDINLSTA-INLAGD OR WS-KDINLSTA-FRD                          
179800         ADD INLA-RAD-KVINLART   TO INLE-MOT-KVANTMOT                     
179900       END-IF                                                             
180000       ADD WS-AVVIK-KVLS         TO INLE-MOT-KVANTMOT                     
180100     END-IF                                                               
180200                                                                          
180300     MOVE WS-KR-KVART-RET        TO INLE-MOT-KVRETUR                      
180400     IF   WS-KVINLART-TRP    > ZERO                                       
180500       MOVE +3                   TO INLE-MOT-KDAVVANT                     
180600     END-IF                                                               
180700                                                                          
180800     IF WS-KVAE-FLKRLIM = NEJ AND                                         
180900        WS-KVAE-FLKRGODK = JA AND                                         
181000        WS-KVAE-IDKRFEL(1:1) = 'P' AND                                    
181100        DCS-CDC                                                           
181200       MOVE +1                   TO INLE-MOT-KDAVVANT                     
181300     END-IF                                                               
181400                                                                          
181500     PERFORM IMS-REPL-INLE                                                
181600                                                                          
181700     MOVE INLE-MOT-KDAVVANT      TO WS-INLE-MOT-KDAVVANT                  
181800     MOVE INLE-MOT-KDAVVKV       TO WS-INLE-MOT-KDAVVKV                   
181900     MOVE INLE-MOT-KVFORDEL      TO WS-INLE-MOT-KVFORDEL                  
182000     MOVE INLE-MOT-KVRETUR       TO WS-INLE-MOT-KVRETUR                   
182100     MOVE INLE-MOT-KVANTMOT      TO WS-INLE-MOT-KVANTMOT                  
182200                                                                          
182300     PERFORM IMS-GHNP-INLE-DEL                                            
182400     PERFORM UNTIL (SEGMENT-SAKNAS)                                       
182500       PERFORM IMS-DLET-INLE                                              
182600       PERFORM IMS-GHNP-INLE-DEL                                          
182700     END-PERFORM                                                          
182800                                                                          
182900     COMPUTE WS-KVDIFF-MOT-AVIS  =                                        
183000             WS-INLE-MOT-KVANTMOT   -  INLA-ART-KVAVIS                    
183100                                    +  WS-KVINLART-TRP                    
183200     .                                                                    
183300     EJECT                                                                
183400 DBB-SLUTUPPD-INLC-HIST SECTION.                                          
183500     SKIP2                                                                
183600     PERFORM DBBA-UPPD-WDL611                                             
183700     PERFORM DBBB-RENSA-BORT-WDL612                                       
183800     .                                                                    
183900     EJECT                                                                
184000 DBBA-UPPD-WDL611 SECTION.                                                
184100     SKIP2                                                                
184200     PERFORM IMS-GU-WDL601                                                
184300                                                                          
184400     PERFORM IMS-GHNP-WDL611                                              
184500                                                                          
184600     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
184700             INLA-ART-IDLOPNRM = INLC-INL-IDLOPNRM                        
184800       PERFORM IMS-GHNP-WDL611                                            
184900     END-PERFORM                                                          
185000                                                                          
185100     IF INLA-ART-IDLOPNRM = INLC-INL-IDLOPNRM                             
185200                                                                          
185300       MOVE 'R32'                  TO INLC-INL-IDPTYP                     
185400       MOVE WS-TIAAMMDD-LOCAL      TO INLC-INL-TIINLINL                   
185500       MOVE INLA-ART-ADLAGOMR      TO INLC-INL-ADLAGOMR                   
185600       MOVE INLA-ART-ADGANG        TO INLC-INL-ADGANG                     
185700       MOVE INLA-ART-ADPLATS       TO INLC-INL-ADPLATS                    
185800                                                                          
185900       IF  (NOT KVALITET-MID)                                             
186000       AND MID-IDRADNR = ZERO                                             
186100         ADD WS-TOT-KVINLART       TO INLC-INL-KVANTMOT                   
186200       ELSE                                                               
186300         IF  WS-KDINLSTA-INLAGD OR WS-KDINLSTA-FRD                        
186400           ADD INLA-RAD-KVINLART   TO INLC-INL-KVANTMOT                   
186500         END-IF                                                           
186600         ADD WS-AVVIK-KVLS         TO INLC-INL-KVANTMOT                   
186700       END-IF                                                             
186800                                                                          
186900       MOVE WS-KDAVVANT            TO INLC-INL-KDAVVANT                   
187000       MOVE WS-KR-KVART-RET        TO INLC-INL-KVRETUR                    
187100       IF   WS-KVINLART-TRP    > ZERO                                     
187200         MOVE +3                   TO INLC-INL-KDAVVANT                   
187300       END-IF                                                             
187400                                                                          
187410       IF WS-INLA-RAD-IDANSTNR-BIN > 0                                    
187420          MOVE WS-INLA-RAD-IDANSTNR-BIN TO INLC-INL-IDUSER-003            
187430       END-IF                                                             
187440                                                                          
187500       PERFORM IMS-REPL-WDL611                                            
187600                                                                          
187700       MOVE +0                     TO WS-INLE-MOT-KDAVVANT                
187800       MOVE +0                     TO WS-INLE-MOT-KDAVVKV                 
187900       MOVE +0                     TO WS-INLE-MOT-KVFORDEL                
188000       MOVE +0                     TO WS-INLE-MOT-KVRETUR                 
188100       MOVE INLC-INL-KVANTMOT      TO WS-INLE-MOT-KVANTMOT                
188200       MOVE INLC-INL-DAINLEV       TO W-DAINLEV                           
188300                                                                          
188400       PERFORM IMS-GHNP-WDL621                                            
188500       PERFORM UNTIL (SEGMENT-SAKNAS)                                     
188600         PERFORM IMS-DLET-WDL621                                          
188700         PERFORM IMS-GHNP-WDL621                                          
188800       END-PERFORM                                                        
188900                                                                          
189000       COMPUTE WS-KVDIFF-MOT-AVIS  =                                      
189100               WS-INLE-MOT-KVANTMOT   -  INLA-ART-KVAVIS                  
189200                                      +  WS-KVINLART-TRP                  
189300     ELSE                                                                 
189400       MOVE 'HISTORIK SAKNAS PÅ WDL6' TO FELTEXT                          
189500       CALL FELLOG                                                        
189600     END-IF                                                               
189700     .                                                                    
189800     EJECT                                                                
189900 DBBB-RENSA-BORT-WDL612 SECTION.                                          
190000     SKIP2                                                                
190100     PERFORM IMS-GU-WDL601                                                
190200     PERFORM IMS-GHNP-WDL612                                              
190300                                                                          
190400     PERFORM UNTIL SEGMENT-SAKNAS                                         
190500                                                                          
190600         IF INLA-ART-IDLOPNRM = INLC-ORD-IDLOPNRM                         
190700** SPARAR UNDAN ORDERNUMRET INNAN BORTRENSNING                            
190800           MOVE INLC-ORD-IDKUNDRF  TO WS-IDORDNR-WDL612                   
190900           INSPECT WS-IDORDNR-WDL612 REPLACING                            
191000                                     LEADING SPACE BY ZERO                
191100           IF WS-IDORDNR-WDL612 NUMERIC                                   
191200             CONTINUE                                                     
191300           ELSE                                                           
191400             MOVE '0000000'        TO WS-IDORDNR-WDL612                   
191500           END-IF                                                         
191600           PERFORM IMS-DLET-WDL612                                        
191700         END-IF                                                           
191800         PERFORM IMS-GHNP-WDL612                                          
191900     END-PERFORM                                                          
192000     .                                                                    
192100     EJECT                                                                
192200 DC-SLUTUPPD-INLB-LEVPL SECTION.                                          
192300                                                                          
192400     COMPUTE WS-KV-LPLAN         = INLA-ART-KVAVIS                        
192500                                 - WS-INLE-MOT-KVANTMOT                   
192600                                 - WS-KVINLART-TRP                        
192700                                                                          
192800     MOVE ZERO                   TO WS-KVAVROP-BOKBAR                     
192900                                    WS-KVAVROP-BOKAD                      
193000                                                                          
193100     IF  WS-KV-LPLAN             NOT = ZERO                               
193200                                                                          
193300       IF  INLA-ART-KDRT         = 00                                     
193400       OR  (INLA-ART-KDRT        = 01                                     
193500        AND WS-ARTC-KDHF       > ZERO)                                    
193600       OR  (INLA-ART-KDRT        = 02                                     
193700        AND WS-ARTC-KDHF       > ZERO)                                    
193800       OR  INLA-ART-KDRT         = 03                                     
193900       OR  INLA-ART-KDRT         = 05                                     
194000       OR  (INLA-ART-KDRT        = 06                                     
194100        AND WS-INLA-INL-IDLEVNR  NOT = SPACE                              
194200        AND WS-INLA-INL-IDLEVNR  NOT = '9999 ')                           
194300       OR  INLA-ART-KDRT         = 09                                     
194400       OR  INLA-ART-KDRT         = 10                                     
194500                                                                          
194600         IF  (INLA-ART-KDRT      = 00                                     
194700          OR  INLA-ART-KDRT      = 01                                     
194800          OR  INLA-ART-KDRT      = 02                                     
194900          OR  INLA-ART-KDRT      = 09)                                    
195000         AND WS-ARTC-KDHF      > ZERO                                     
195100           MOVE WS-ARTC-IDLEVNR TO W-INLB11-IDLEVNR                       
195200         ELSE                                                             
195300           MOVE WS-INLA-INL-IDLEVNR TO W-INLB11-IDLEVNR                   
195400         END-IF                                                           
195500                                                                          
195600         MOVE INLA-ART-IDARTNR TO W-IDARTNR-D9                            
195700         MOVE INLA-ART-IDDC    TO W-IDDC-D9                               
195800         PERFORM IMS-GU-INLB11                                            
195900                                                                          
196000         IF  SEGMENT-FINNS                                                
196100                                                                          
196200           MOVE INLA-ART-IDLOPNRM TO WS-IDLOPNRM-VVDLLLLK                 
196300           IF  WS-IDLOPNRM-VV > WS-TIAAVVD-VV                             
196400             IF WS-TIAAVVD-AA = 00                                        
196500                MOVE 99 TO WS-IDLOPNRM-AA                                 
196600             ELSE                                                         
196700                COMPUTE WS-IDLOPNRM-AA = WS-TIAAVVD-AA - 1                
196800             END-IF                                                       
196900           ELSE                                                           
197000             MOVE WS-TIAAVVD-AA  TO WS-IDLOPNRM-AA                        
197100           END-IF                                                         
197200                                                                          
197300           IF  WS-KV-LPLAN       > ZERO                                   
197400             IF  INLA-ART-KDRT   NOT = 10                                 
197500               PERFORM DCA-ATERBOKA-LEVPL-AVROP                           
197600*              -- B.REST ÅTERBOKAS MED ÅTERBOKAD AVROP-KVANT              
197700               MOVE WS-KVAVROP-BOKAD TO WS-KV-OBOK                        
197800             ELSE                                                         
197900*              -- B.REST ÅTERBOKAS MED LEV.PLANENS OBOKADE KVANT          
198000               MOVE WS-KV-LPLAN  TO WS-KV-OBOK                            
198100             END-IF                                                       
198200             PERFORM DCB-ATERBOKA-LEVPL-BREST                             
198300           ELSE                                                           
198400             COMPUTE WS-KV-LPLAN = WS-KV-LPLAN * -1                       
198500             END-COMPUTE                                                  
198600                                                                          
198700             IF  INLA-ART-KDRT   NOT = 10                                 
198800               PERFORM DCC-NEDBOKA-LEVPL-AVROP                            
198900             END-IF                                                       
199000*            -- B.REST BOKAS NER MED LEV.PLANENS OBOKADE KVANT            
199100             MOVE WS-KV-LPLAN    TO WS-KV-OBOK                            
199200             PERFORM DCD-NEDBOKA-LEVPL-BREST                              
199300           END-IF                                                         
199400         END-IF                                                           
199500       END-IF                                                             
199600     END-IF                                                               
199700     .                                                                    
199800     EJECT                                                                
199900 DCA-ATERBOKA-LEVPL-AVROP SECTION.                                        
200000                                                                          
200100     MOVE WS-IDLOPNRM-AAVVDLLLL  TO W-INLB31-IDLOPNRM                     
200200     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
200300                                    WS-KVAVROP-BOKBAR                     
200400                                                                          
200500     PERFORM IMS-GHNP-INLB23-31-PATH                                      
200600                                                                          
200700     PERFORM UNTIL (SEGMENT-SAKNAS                                        
200800                OR  WS-KV-OBOK   = ZERO)                                  
200900                                                                          
201000       IF  INLB31P-KVAVROP-AVB   <= WS-KV-OBOK                            
201100*        -- AVROP ÅTERBOKAS HELT                                          
201200         SUBTRACT INLB31P-KVAVROP-AVB FROM WS-KV-OBOK                     
201300         ADD INLB31P-KVAVROP-AVB TO WS-KVAVROP-BOKAD                      
201400         ADD INLB31P-KVAVROP-AVB TO INLB23P-KVAVROP                       
201500         MOVE +2                 TO INLB23P-KDAVROP                       
201600         PERFORM IMS-REPL-INLB23-NOT-31                                   
201700                                                                          
201800         MOVE INLB23P-DAAVROP-AVS TO W-DAAVROP                            
201900         MOVE INLB23P-TILEVDAG   TO W-TILEVDAG                            
202000         MOVE +2                 TO W-KDAVROP                             
202100         PERFORM IMS-GHNP-INLB31-F-KV                                     
202200         PERFORM IMS-DLET-INLB31                                          
202300                                                                          
202400       ELSE                                                               
202500*        -- AVROP ÅTERBOKAS DELVIS                                        
202600         SUBTRACT WS-KV-OBOK     FROM INLB31P-KVAVROP-AVB                 
202700         ADD WS-KV-OBOK          TO INLB23P-KVAVROP                       
202800         ADD WS-KV-OBOK          TO WS-KVAVROP-BOKAD                      
202900         MOVE +2                 TO INLB23P-KDAVROP                       
203000         PERFORM IMS-REPL-INLB23-31                                       
203100         MOVE ZERO               TO WS-KV-OBOK                            
203200       END-IF                                                             
203300                                                                          
203400       IF  WS-KV-OBOK            > ZERO                                   
203500         PERFORM IMS-GHNP-INLB23-31-PATH                                  
203600       END-IF                                                             
203700     END-PERFORM                                                          
203800     .                                                                    
203900     EJECT                                                                
204000 DCB-ATERBOKA-LEVPL-BREST SECTION.                                        
204100                                                                          
204200     IF  WS-KV-OBOK > ZERO                                                
204300       PERFORM IMS-GHU-INLB11                                             
204400       ADD WS-KV-OBOK            TO INLB11-KVBR                           
204500       PERFORM IMS-REPL-INLB                                              
204600     END-IF                                                               
204700     .                                                                    
204800     EJECT                                                                
204900 DCC-NEDBOKA-LEVPL-AVROP SECTION.                                         
205000                                                                          
205100     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
205200                                    WS-KVAVROP-BOKBAR                     
205300                                                                          
205400     MOVE +2                     TO W-KDAVROP                             
205500     PERFORM IMS-GHNP-INLB23                                              
205600                                                                          
205700     PERFORM UNTIL (SEGMENT-SAKNAS                                        
205800                OR  WS-KV-OBOK   = ZERO)                                  
205900                                                                          
206000       IF  INLB23-KVAVROP        <= WS-KV-OBOK                            
206100*        -- AVROP NEDBOKAS HELT                                           
206200         SUBTRACT INLB23-KVAVROP FROM WS-KV-OBOK                          
206300         ADD INLB23-KVAVROP      TO WS-KVAVROP-BOKAD                      
206400         MOVE INLB23-KVAVROP     TO INLB31-KVAVROP-AVB                    
206500         MOVE ZERO               TO INLB23-KVAVROP                        
206600         MOVE +9                 TO INLB23-KDAVROP                        
206700       ELSE                                                               
206800*        -- AVROP NEDBOKAS DELVIS                                         
206900         SUBTRACT WS-KV-OBOK     FROM INLB23-KVAVROP                      
207000         ADD WS-KV-OBOK          TO WS-KVAVROP-BOKAD                      
207100         MOVE WS-KV-OBOK         TO INLB31-KVAVROP-AVB                    
207200         MOVE ZERO               TO WS-KV-OBOK                            
207300       END-IF                                                             
207400       MOVE WS-IDLOPNRM-AAVVDLLLL TO INLB31-IDLOPNRM-PL                   
207500                                                                          
207600       PERFORM IMS-REPL-INLB                                              
207700       PERFORM IMS-ISRT-INLB31                                            
207800                                                                          
207900       IF  WS-KV-OBOK            > ZERO                                   
208000         PERFORM IMS-GHNP-INLB23                                          
208100       END-IF                                                             
208200     END-PERFORM                                                          
208300     .                                                                    
208400     EJECT                                                                
208500 DCD-NEDBOKA-LEVPL-BREST SECTION.                                         
208600                                                                          
208700     PERFORM IMS-GHU-INLB11                                               
208800                                                                          
208900     IF  INLB11-KVBR             <= WS-KV-OBOK                            
209000       MOVE ZERO                 TO INLB11-KVBR                           
209100     ELSE                                                                 
209200       SUBTRACT WS-KV-OBOK       FROM INLB11-KVBR                         
209300     END-IF                                                               
209400                                                                          
209500     PERFORM IMS-REPL-INLB                                                
209600     .                                                                    
209700     EJECT                                                                
209800 DD-SLUTUPPD-ARTC-ARTS SECTION.                                           
209900                                                                          
210000     IF INLA-ART-IDDC NOT = DCS-IDDC                                      
210100        MOVE INLA-ART-IDDC   TO W-IDDC-B6                                 
210200        PERFORM IMS-GU-WDB601                                             
210300     END-IF                                                               
210400     IF DCS-CDC OR DCS-CDC-TR                                             
210500       PERFORM DDA-SLUTUPPD-ARTC                                          
210600     ELSE                                                                 
210700       PERFORM DDB-SLUTUPPD-ARTS                                          
210800     END-IF                                                               
210900     .                                                                    
211000     EJECT                                                                
211100 DDA-SLUTUPPD-ARTC SECTION.                                               
211200     SKIP2                                                                
211300                                                                          
211400     PERFORM IMS-GHU-ARTC11                                               
211500                                                                          
211600     PERFORM S10-GRUNDDATA-LOGG                                           
211700     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
211800        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
211900        PERFORM IMS-GU-WDB601                                             
212000     END-IF                                                               
212100     IF DCS-CDC                                                           
212200       IF  (NOT KVALITET-MID)                                             
212300       AND MID-IDRADNR = ZERO                                             
212400         SUBTRACT WS-TOT-KVINLART FROM ARTC-CLAG-KVAKS-CDC                
212500         MOVE '-'                 TO LOGG-IDTECKEN-KVAKS                  
212600         ADD WS-TOT-KVINLART      TO ARTC-CLAG-KVLS                       
212700         MOVE 1 TO CD-IX                                                  
212800         PERFORM UNTIL CD-IX > 4                                          
212900           ADD WS-TOT-KVINLART-CD(CD-IX)                                  
213000                                  TO ARTC-CLAG-KVLS-CD(CD-IX)             
213100           ADD 1                  TO CD-IX                                
213200         END-PERFORM                                                      
213300                                                                          
213400         ADD WS-TOT-KVINLART-SVS  TO ARTC-CLAG-KVLS-SVS                   
213500         MOVE '+'                 TO LOGG-IDTECKEN-KVLS                   
213600         MOVE WS-TOT-KVINLART     TO LOGG-KVART-SALDO                     
213700         PERFORM S11-UPPDATERA-LOGG-WDK6                                  
213800         MOVE ' '                 TO LOGG-IDTECKEN-KVLS                   
213900       ELSE                                                               
214000         IF  WS-KDINLSTA-INLAGD                                           
214100           SUBTRACT INLA-RAD-KVINLART FROM ARTC-CLAG-KVAKS-CDC            
214200           MOVE '-'                TO LOGG-IDTECKEN-KVAKS                 
214300           ADD INLA-RAD-KVINLART   TO ARTC-CLAG-KVLS                      
214400           IF INLA-RAD-FLSVSLS = JA                                       
214500              ADD INLA-RAD-KVINLART                                       
214600                                   TO ARTC-CLAG-KVLS-SVS                  
214700           END-IF                                                         
214800           IF INLA-ART-ADTRDEST(1:2) = 'CD'                               
214900              MOVE INLA-ART-ADTRDEST(3:1) TO CD-IX                        
215000              ADD INLA-RAD-KVINLART  TO ARTC-CLAG-KVLS-CD(CD-IX)          
215100           END-IF                                                         
215200                                                                          
215300           MOVE '+'                TO LOGG-IDTECKEN-KVLS                  
215400           MOVE INLA-RAD-KVINLART  TO LOGG-KVART-SALDO                    
215500           PERFORM S11-UPPDATERA-LOGG-WDK6                                
215600           MOVE ' '                TO LOGG-IDTECKEN-KVLS                  
215700         END-IF                                                           
215800                                                                          
215900*      -- EV BIDRAG FRÅN AVVIKELSE PÅVERKAR SALDON                        
216000         ADD WS-AVVIK-KVAKS        TO ARTC-CLAG-KVAKS-CDC                 
216100         IF WS-AVVIK-KVAKS NOT     = ZERO                                 
216200            PERFORM S10-GRUNDDATA-LOGG                                    
216300            MOVE '+'               TO LOGG-IDTECKEN-KVAKS                 
216400            MOVE WS-AVVIK-KVAKS    TO LOGG-KVART-SALDO                    
216500            PERFORM S11-UPPDATERA-LOGG-WDK6                               
216600         END-IF                                                           
216700         SUBTRACT WS-KVINLART-TRP FROM ARTC-CLAG-KVAKS-CDC                
216800         IF WS-KVINLART-TRP NOT   = ZERO                                  
216900            PERFORM S10-GRUNDDATA-LOGG                                    
217000            MOVE '-'              TO LOGG-IDTECKEN-KVAKS                  
217100            MOVE WS-KVINLART-TRP  TO LOGG-KVART-SALDO                     
217200            PERFORM S11-UPPDATERA-LOGG-WDK6                               
217300         END-IF                                                           
217400                                                                          
217500         IF INLA-ART-IDDC NOT = DCS-IDDC                                  
217600            MOVE INLA-ART-IDDC   TO W-IDDC-B6                             
217700            PERFORM IMS-GU-WDB601                                         
217800         END-IF                                                           
217900         IF DCS-CDC                                                       
218000            ADD WS-AVVIK-KVLS     TO ARTC-CLAG-KVLS                       
218100            IF INLA-RAD-FLSVSLS = JA                                      
218200              ADD WS-AVVIK-KVLS     TO ARTC-CLAG-KVLS-SVS                 
218300            END-IF                                                        
218400            IF  INLA-ART-ADTRDEST(1:2) = 'CD'                             
218500              MOVE INLA-ART-ADTRDEST(3:1) TO CD-IX                        
218600              ADD WS-AVVIK-KVLS      TO ARTC-CLAG-KVLS-CD(CD-IX)          
218700            END-IF                                                        
218800                                                                          
218900            IF WS-AVVIK-KVLS NOT  = ZERO                                  
219000               PERFORM S10-GRUNDDATA-LOGG                                 
219100               MOVE '+'           TO LOGG-IDTECKEN-KVLS                   
219200               MOVE ' '           TO LOGG-IDTECKEN-KVAKS                  
219300               MOVE WS-AVVIK-KVLS TO LOGG-KVART-SALDO                     
219400               PERFORM S11-UPPDATERA-LOGG-WDK6                            
219500               MOVE ' '           TO LOGG-IDTECKEN-KVLS                   
219600            END-IF                                                        
219700         END-IF                                                           
219800       END-IF                                                             
219900       MOVE SPACE TO LOGG-IDTECKEN-KVLS                                   
220000                     LOGG-IDTECKEN-KVAKS                                  
220100     ELSE                                                                 
220200       IF  (NOT KVALITET-MID)                                             
220300       AND MID-IDRADNR = ZERO                                             
220400         SUBTRACT WS-TOT-KVINLART FROM ARTC-CLAG-KVAKS-T                  
220500         MOVE '-'                 TO LOGG-IDTECKEN-KVAKS                  
220600         MOVE WS-TOT-KVINLART     TO LOGG-KVART-SALDO                     
220700         PERFORM S11-UPPDATERA-LOGG-WDK6                                  
220800       ELSE                                                               
220900         IF  WS-KDINLSTA-INLAGD                                           
221000           SUBTRACT INLA-RAD-KVINLART FROM ARTC-CLAG-KVAKS-T              
221100           MOVE '-'                 TO LOGG-IDTECKEN-KVAKS                
221200           MOVE INLA-RAD-KVINLART     TO LOGG-KVART-SALDO                 
221300           PERFORM S11-UPPDATERA-LOGG-WDK6                                
221400         END-IF                                                           
221500                                                                          
221600*      -- EV BIDRAG FRÅN AVVIKELSE PÅVERKAR SALDON                        
221700         ADD WS-AVVIK-KVAKS         TO ARTC-CLAG-KVAKS-T                  
221800         IF WS-AVVIK-KVAKS NOT      = ZERO                                
221900            MOVE '+'                TO LOGG-IDTECKEN-KVAKS                
222000            MOVE WS-AVVIK-KVAKS     TO LOGG-KVART-SALDO                   
222100            PERFORM S11-UPPDATERA-LOGG-WDK6                               
222200         END-IF                                                           
222300         SUBTRACT WS-KVINLART-TRP FROM ARTC-CLAG-KVAKS-T                  
222400         IF WS-KVINLART-TRP NOT     = ZERO                                
222500            MOVE '-'                TO LOGG-IDTECKEN-KVAKS                
222600            MOVE WS-KVINLART-TRP    TO LOGG-KVART-SALDO                   
222700            PERFORM S11-UPPDATERA-LOGG-WDK6                               
222800         END-IF                                                           
222900       END-IF                                                             
223000     END-IF                                                               
223100     MOVE ' ' TO LOGG-IDTECKEN-KVLS                                       
223200                 LOGG-IDTECKEN-KVAKS                                      
223300                                                                          
223400     PERFORM IMS-REPL-ARTC                                                
223500                                                                          
223600     MOVE ARTC-CLAG-KVROS         TO WS-ARTC-ARTS-KVROS                   
223700     MOVE ARTC-CLAG-KVLS          TO WS-ARTC-ARTS-KVLS                    
223800     MOVE ARTC-CLAG-KVUTRS        TO WS-ARTC-ARTS-KVUTRS                  
223900     MOVE ARTC-CLAG-KVRESS        TO WS-ARTC-ARTS-KVRESS                  
224000     MOVE ARTC-CLAG-KVSPANT       TO WS-ARTC-ARTS-KVSPANT                 
224100     MOVE ARTC-CLAG-KVSPARR-KVAL  TO WS-ARTC-ARTS-KVSPARR-KVAL            
224200     .                                                                    
224300     EJECT                                                                
224400 DDB-SLUTUPPD-ARTS SECTION.                                               
224500     SKIP2                                                                
224600                                                                          
224700     PERFORM IMS-GHU-WDK711                                               
224800     PERFORM S10-GRUNDDATA-LOGG                                           
224900                                                                          
225000     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
225100       MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                               
225200       PERFORM IMS-GU-WDB601                                              
225300     END-IF                                                               
225400     IF DCS-NDC-NA OR DCS-AUSTRALIA OR DCS-JAPAN                          
225500     OR DCS-LAND-NON-VCC-OWNED                                            
225600       IF  (NOT KVALITET-MID)                                             
225700       AND MID-IDRADNR = ZERO                                             
225800         SUBTRACT WS-TOT-KVINLART FROM SLAG-KVAKS-SDC                     
225900         MOVE '-'                 TO LOGG-IDTECKEN-KVAKS                  
226000         ADD WS-TOT-KVINLART      TO SLAG-KVLS                            
226100         MOVE WS-TOT-KVINLART     TO WS-INLE-MOT-KVANTDELMOT              
226200         MOVE '+'                 TO LOGG-IDTECKEN-KVLS                   
226300         MOVE WS-TOT-KVINLART     TO LOGG-KVART-SALDO                     
226400         PERFORM S12-UPPDATERA-LOGG-WDK7                                  
226500       ELSE                                                               
226600         IF  WS-KDINLSTA-INLAGD                                           
226700           SUBTRACT INLA-RAD-KVINLART FROM SLAG-KVAKS-SDC                 
226800           MOVE '-'               TO LOGG-IDTECKEN-KVAKS                  
226900           ADD INLA-RAD-KVINLART  TO SLAG-KVLS                            
227000           MOVE INLA-RAD-KVINLART   TO WS-INLE-MOT-KVANTDELMOT            
227100           MOVE '+'               TO LOGG-IDTECKEN-KVLS                   
227200           MOVE INLA-RAD-KVINLART TO LOGG-KVART-SALDO                     
227300           PERFORM S12-UPPDATERA-LOGG-WDK7                                
227400         END-IF                                                           
227500                                                                          
227600*        -- EV BIDRAG FRÅN AVVIKELSE PÅVERKAR SALDON                      
227700         ADD WS-AVVIK-KVAKS       TO SLAG-KVAKS-SDC                       
227800         IF WS-AVVIK-KVAKS NOT    = ZERO                                  
227900           PERFORM S10-GRUNDDATA-LOGG                                     
228000           MOVE '+'              TO LOGG-IDTECKEN-KVAKS                   
228100           MOVE ' '              TO LOGG-IDTECKEN-KVLS                    
228200           MOVE WS-AVVIK-KVAKS   TO LOGG-KVART-SALDO                      
228300           PERFORM S12-UPPDATERA-LOGG-WDK7                                
228400         END-IF                                                           
228500         SUBTRACT WS-KVINLART-TRP FROM SLAG-KVAKS-SDC                     
228600         IF WS-KVINLART-TRP NOT   = ZERO                                  
228700           PERFORM S10-GRUNDDATA-LOGG                                     
228800           MOVE '-'              TO LOGG-IDTECKEN-KVAKS                   
228900           MOVE ' '              TO LOGG-IDTECKEN-KVLS                    
229000           MOVE WS-KVINLART-TRP  TO LOGG-KVART-SALDO                      
229100           PERFORM S12-UPPDATERA-LOGG-WDK7                                
229200         END-IF                                                           
229300       END-IF                                                             
229400**CHECK UMESH START                                                       
229500       ADD WS-AVVIK-KVLS     TO SLAG-KVLS                                 
229600       ADD WS-AVVIK-KVLS     TO WS-INLE-MOT-KVANTDELMOT                   
229700       IF WS-AVVIK-KVLS NOT  = ZERO                                       
229800          PERFORM S10-GRUNDDATA-LOGG                                      
229900          MOVE '+'           TO LOGG-IDTECKEN-KVLS                        
230000          MOVE ' '           TO LOGG-IDTECKEN-KVAKS                       
230100          MOVE WS-AVVIK-KVLS TO LOGG-KVART-SALDO                          
230200          PERFORM S12-UPPDATERA-LOGG-WDK7                                 
230300          MOVE ' '           TO LOGG-IDTECKEN-KVLS                        
230400       END-IF                                                             
230500**CHECK UMESH END                                                         
230600       MOVE SPACE TO LOGG-IDTECKEN-KVLS                                   
230700                       LOGG-IDTECKEN-KVAKS                                
230800     END-IF                                                               
230900                                                                          
231000     IF DCS-USA OR DCS-LAND-NON-VCC-OWNED                                 
231100       IF WS-INLE-MOT-KVANTDELMOT < ZERO                                  
231200         PERFORM DDBB-RAEKNA-AVERAGE-COST-BACK                            
231300       ELSE                                                               
231400         PERFORM DDBA-RAEKNA-AVERAGE-COST                                 
231500       END-IF                                                             
231600     ELSE                                                                 
231700       PERFORM DDBA-RAEKNA-AVERAGE-COST                                   
231800     END-IF                                                               
231900                                                                          
232000     MOVE WS-PRAVCOST        TO SLAG-PRAVCOST                             
232100     MOVE WS-TIAAMMDD-LOCAL  TO SLAG-TIAVCOST                             
232200                                                                          
232300     COMPUTE WS-ARTC-ARTS-KVROS = SLAG-KVROS-BULK +                       
232400                                    SLAG-KVROS-DAG                        
232500     MOVE      SLAG-KVLS          TO WS-ARTC-ARTS-KVLS                    
232600     MOVE      SLAG-KVUTRS        TO WS-ARTC-ARTS-KVUTRS                  
232700     MOVE      SLAG-KVRESS        TO WS-ARTC-ARTS-KVRESS                  
232800     MOVE      +0                 TO WS-ARTC-ARTS-KVSPANT                 
232900     MOVE      SLAG-KVSPARR-KVAL  TO WS-ARTC-ARTS-KVSPARR-KVAL            
233000                                                                          
233100     PERFORM IMS-REPL-WDK711                                              
233200                                                                          
233300     IF DCS-NDC-CN                                                        
233400     OR DCS-USA                                                           
233500       PERFORM IMS-GU-WDK711                                              
233600       IF SEGMENT-FINNS                                                   
233700                                                                          
233800*      -- WDK724                                                          
233900         MOVE WS-INLA-INL-TIAVIDAT  TO WS-DAAVIDAT-YYMMDD                 
234000         IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                  
234100           MOVE 19                        TO WS-DAAVIDAT-SEKEL            
234200         ELSE                                                             
234300           MOVE 20                        TO WS-DAAVIDAT-SEKEL            
234400         END-IF                                                           
234500         COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAAVIDAT                   
234600         PERFORM IMS-GHNP-WDK724                                          
234700                                                                          
234800         IF SEGMENT-FINNS                                                 
234900           IF SPRL-SUINLEV-PR = ZERO                                      
235000             MOVE +1               TO SPRL-SUINLEV-PR                     
235100             PERFORM IMS-REPL-WDK724                                      
235200           END-IF                                                         
235300         END-IF                                                           
235400       END-IF                                                             
235500     END-IF                                                               
235600     IF DCS-NDC-NA OR DCS-LAND-NON-VCC-OWNED                              
235700       IF SLAG-PRAVCOST > ZERO                                            
235800          PERFORM S13-UPPDATERA-MATERIALPRIS                              
235900       END-IF                                                             
236000     END-IF                                                               
236100*** ???                                                                   
236200*    CALL FELLOG                                                          
236300*** ???                                                                   
236400     .                                                                    
236500     EJECT                                                                
236600                                                                          
236700 DDBA-RAEKNA-AVERAGE-COST SECTION.                                        
236800     IF DCS-NDC-NA OR DCS-LAND-NON-VCC-OWNED                              
236900       IF DCS-NDC-NA                                                      
237000         MOVE 040                  TO AVG-KDCALL                          
237100       ELSE                                                               
237200         IF DCS-INDIA                                                     
237300           MOVE 042                  TO AVG-KDCALL                        
237400         ELSE                                                             
237500          IF DCS-CHINA                                                    
237600           MOVE 041                  TO AVG-KDCALL                        
237700          ELSE                                                            
237800           MOVE 041                  TO AVG-KDCALL                        
237900          END-IF                                                          
238000         END-IF                                                           
238100       END-IF                                                             
238200**** AVERAGE COST BERÄKNING SKALL TA HÄNSYN TILL EFR                      
238300       COMPUTE AVG-KVLS-OLD = SLAG-KVLS + SLAG-KVEFRS -                   
238400                              WS-INLE-MOT-KVANTDELMOT                     
238500       MOVE W-RETULF             TO AVG-REMARKUP                          
238600                                                                          
238700       MOVE WS-INLA-INL-TIAVIDAT    TO WS-DAAVIDAT-YYMMDD                 
238800       MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                   
238900       MOVE 01                      TO W-DATE-AAMM(3:2)                   
239000       MOVE WS-KDVALISO-HUV         TO CURR-KDVALISO-HUV                  
239100       MOVE DCS-KDVALISO            TO CURR-KDVALISO-ROW                  
239200       MOVE W-DATE-AAMM             TO CURR-TIAAMM                        
239300       MOVE 'A'                     TO CURR-KDVALTYP                      
239400       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
239500       IF CURR-KDSVAR = ' '                                               
239600         MOVE CURR-PRKURS-NEW       TO WS-PRKURS                          
239700         MOVE CURR-REVALUTA-TO      TO WS-REVALUTA                        
239800       ELSE                                                               
239900         MOVE 1                     TO WS-PRKURS                          
240000         MOVE 1                     TO WS-REVALUTA                        
240100       END-IF                                                             
240200       PERFORM IMS-GU-WDB601                                              
240300       MOVE DCS-IDLANDX2 TO W-IDLANDX2                                    
240400       IF SEGMENT-FINNS                                                   
240500         PERFORM IMS-GNP-WDB617                                           
240600         IF SEGMENT-FINNS                                                 
240700           COMPUTE WS-PRARTKALKYL        ROUNDED =                        
240800                  (WS-ARTC-PRDIRLON *                                     
240900                   PROC-REDIRLON * WS-REVALUTA / WS-PRKURS) +             
241000                  (WS-ARTC-PRDMTRL  *                                     
241100                   PROC-REDMTRL  * WS-REVALUTA / WS-PRKURS)               
241200         ELSE                                                             
241300           MOVE ZERO TO WS-PRARTKALKYL                                    
241400         END-IF                                                           
241500       END-IF                                                             
241600       MOVE WS-ARTC21-PRARTBEL-PR   TO AVG-PRARTBEL                       
241700       MOVE WS-PRARTKALKYL          TO AVG-PRARTNTO                       
241800       MOVE WS-INLE-MOT-KVANTDELMOT TO AVG-KVANTMOT                       
241900       MOVE +0                      TO AVG-PRKURS                         
242000     ELSE                                                                 
242100       MOVE 040                  TO AVG-KDCALL                            
242200       COMPUTE AVG-KVLS-OLD = SLAG-KVLS - WS-INLE-MOT-KVANTMOT            
242300       MOVE +0                   TO AVG-REMARKUP                          
242400       MOVE WS-ARTC21-PRARTBEL-PR  TO AVG-PRARTBEL                        
242500       MOVE WS-INLE-MOT-KVANTMOT   TO AVG-KVANTMOT                        
242600       MOVE +0                     TO AVG-PRARTNTO                        
242700       MOVE +0                     TO AVG-PRKURS                          
242800     END-IF                                                               
242900     MOVE SLAG-PRAVCOST          TO AVG-PRAVCOST-OLD                      
243000     MOVE WS-ARTC21-KDVALISO     TO AVG-KDVALISO                          
243100     MOVE +0                     TO AVG-KVLEVART                          
243200     MOVE WS-ARTC-KDPSLLOC       TO AVG-KDPSLLOC                          
243300     MOVE WS-ARTC-KDPRODSL       TO AVG-KDPRODSL                          
243400     MOVE WS-ARTC-IDFKNGRP       TO AVG-IDFKNGRP                          
243500     MOVE W-IDDC                 TO AVG-IDDC                              
243600     MOVE +0                     TO AVG-PRAVCOST-NEW                      
243700     MOVE SPACE                  TO AVG-KDSVAR                            
243800     MOVE WS-TIAA                TO AVG-TIAA                              
243900     MOVE WS-TIMM                TO AVG-TIMM                              
244000                                                                          
244100     CALL W510AVG USING AVG-W510AVG 9305-AVG-PCB                          
244200                        AVG-WDB6-PCB                                      
244300     IF AVG-KDSVAR = SPACE                                                
244400       MOVE AVG-PRAVCOST-NEW     TO WS-PRAVCOST                           
244500     ELSE                                                                 
244600       IF AVG-KDSVAR = '4'                                                
244700         MOVE SLAG-PRAVCOST      TO WS-PRAVCOST                           
244800       ELSE                                                               
244900         STRING 'FEL FRÅN W510AVG ' AVG-KDSVAR                            
245000          DELIMITED BY SIZE INTO FELTEXT                                  
245100           CALL FELLOG                                                    
245200       END-IF                                                             
245300     END-IF                                                               
245400     .                                                                    
245500     EJECT                                                                
245600                                                                          
245700 DDBB-RAEKNA-AVERAGE-COST-BACK SECTION.                                   
245800**** HÄR LÄSER MAN WDK7 IGEN                                              
245900     PERFORM DEFBAA-GET-PRARTBES                                          
246000     PERFORM DDAB-GET-CURRENCY-RATE                                       
246100     IF DCS-NDC-NA                                                        
246200       MOVE 080                  TO AVG-KDCALL                            
246300     ELSE                                                                 
246400       IF DCS-INDIA                                                       
246500         MOVE 082                  TO AVG-KDCALL                          
246600       ELSE                                                               
246700         MOVE 081                  TO AVG-KDCALL                          
246800       END-IF                                                             
246900     END-IF                                                               
247000**** KINAS AVERAGE COST SKALL TA HÄNSYN TILL EFR                          
247100     COMPUTE WS-INLE-MOT-KVANTDELMOT =                                    
247200             WS-INLE-MOT-KVANTDELMOT * -1                                 
247300     COMPUTE AVG-KVLS-OLD = SLAG-KVLS + SLAG-KVEFRS -                     
247400                            WS-INLE-MOT-KVANTDELMOT                       
247500     MOVE W-RETULF               TO AVG-REMARKUP                          
247600                                                                          
247700     MOVE WS-INLA-INL-TIAVIDAT      TO WS-DAAVIDAT-YYMMDD                 
247800     MOVE WS-DAAVIDAT-YYMMDD(1:2)   TO W-DATE-AAMM(1:2)                   
247900     MOVE 01                        TO W-DATE-AAMM(3:2)                   
248000     MOVE WS-KDVALISO-HUV           TO CURR-KDVALISO-HUV                  
248100     MOVE DCS-KDVALISO              TO CURR-KDVALISO-ROW                  
248200     MOVE W-DATE-AAMM               TO CURR-TIAAMM                        
248300     MOVE 'A'                       TO CURR-KDVALTYP                      
248400     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
248500     IF CURR-KDSVAR = ' '                                                 
248600       MOVE CURR-PRKURS-NEW         TO WS-PRKURS                          
248700       MOVE CURR-REVALUTA-TO        TO WS-REVALUTA                        
248800     ELSE                                                                 
248900       MOVE 1                       TO WS-PRKURS                          
249000       MOVE 1                       TO WS-REVALUTA                        
249100     END-IF                                                               
249200     PERFORM IMS-GU-WDB601                                                
249300     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
249400     IF SEGMENT-FINNS                                                     
249500       PERFORM IMS-GNP-WDB617                                             
249600       IF SEGMENT-FINNS                                                   
249700         COMPUTE WS-PRARTKALKYL        ROUNDED =                          
249800                (WS-ARTC-PRDIRLON *                                       
249900                 PROC-REDIRLON * WS-REVALUTA / WS-PRKURS) +               
250000                (WS-ARTC-PRDMTRL    *                                     
250100                 PROC-REDMTRL  * WS-REVALUTA / WS-PRKURS)                 
250200       ELSE                                                               
250300         MOVE ZERO TO WS-PRARTKALKYL                                      
250400       END-IF                                                             
250500     END-IF                                                               
250600     MOVE WS-PRARTKALKYL         TO AVG-PRARTNTO                          
250700     MOVE WS-INLE-MOT-KVANTDELMOT TO AVG-KVANTMOT                         
250800     MOVE SLAG-PRAVCOST          TO AVG-PRAVCOST-OLD                      
250900     MOVE WS-ARTC21-PRARTBEL-PR  TO AVG-PRARTBEL                          
251000     MOVE SPAR-PRKURS            TO AVG-PRKURS                            
251100     MOVE WS-ARTC21-KDVALISO     TO AVG-KDVALISO                          
251200     MOVE +0                     TO AVG-KVLEVART                          
251300     MOVE WS-ARTC-KDPSLLOC       TO AVG-KDPSLLOC                          
251400     MOVE WS-ARTC-KDPRODSL       TO AVG-KDPRODSL                          
251500     MOVE WS-ARTC-IDFKNGRP       TO AVG-IDFKNGRP                          
251600     MOVE W-IDDC                 TO AVG-IDDC                              
251700     MOVE +0                     TO AVG-PRAVCOST-NEW                      
251800     MOVE SPACE                  TO AVG-KDSVAR                            
251900     MOVE WS-TIAA                TO AVG-TIAA                              
252000     MOVE WS-TIMM                TO AVG-TIMM                              
252100                                                                          
252200     CALL W510AVG USING AVG-W510AVG 9305-AVG-PCB                          
252300                        AVG-WDB6-PCB                                      
252400     IF AVG-KDSVAR = SPACE                                                
252500       MOVE AVG-PRAVCOST-NEW     TO WS-PRAVCOST                           
252600     ELSE                                                                 
252700       IF AVG-KDSVAR = '4'                                                
252800         MOVE SLAG-PRAVCOST      TO WS-PRAVCOST                           
252900       ELSE                                                               
253000         STRING 'FEL FRÅN W510AVG ' AVG-KDSVAR                            
253100          DELIMITED BY SIZE INTO FELTEXT                                  
253200           CALL FELLOG                                                    
253300       END-IF                                                             
253400     END-IF                                                               
253500                                                                          
253600     .                                                                    
253700     EJECT                                                                
253800                                                                          
253900 DE-LOGG-ZZAC SECTION.                                                    
254000                                                                          
254100     PERFORM DEA-LOGG-R32                                                 
254200                                                                          
254300     IF  INLA-ART-VKART          = ZERO                                   
254400       PERFORM DEB-LOGG-092-M108                                          
254500     END-IF                                                               
254600                                                                          
254700                                                                          
254800     IF  WS-INLE-MOT-KVRETUR   > ZERO                                     
254900       PERFORM DED-LOGG-400                                               
255000     END-IF                                                               
255100                                                                          
255200     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
255300        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
255400        PERFORM IMS-GU-WDB601                                             
255500     END-IF                                                               
255600     IF DCS-CDC                                                           
255700       PERFORM DEE-LOGG-320                                               
255800     END-IF                                                               
255900**                                                                        
256000     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
256100        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
256200        PERFORM IMS-GU-WDB601                                             
256300     END-IF                                                               
256400     IF DCS-CANADA                                                        
256500       PERFORM DEFA-LOGG-LAB-WDR8                                         
256600     ELSE                                                                 
256700       IF WS-INLE-MOT-KVANTMOT NOT = INLA-ART-KVAVIS                      
256800         IF WS-KVAE-FLKRLIM = NEJ AND                                     
256900            WS-KVAE-IDKRFEL(1:1) = 'P' AND                                
257000            DCS-CDC                                                       
257100           PERFORM DEFC-LOGG-WDR9                                         
257200         ELSE                                                             
257300           IF INLA-ART-KDRT = 0 OR 9                                      
257400             IF WS-INLE-MOT-KDAVVANT = +3                                 
257500** TRANSPORTSKADA                                                         
257600               PERFORM DEG-LOGG-EKO-WDR9                                  
257700             ELSE                                                         
257800               COMPUTE EKO-KVAVIS = WS-INLE-MOT-KVANTMOT -                
257900                       INLA-ART-KVAVIS                                    
258000               END-COMPUTE                                                
258100               IF (EKO-KVAVIS > ZERO) OR (EKO-KVAVIS < ZERO AND           
258200                                    NOT DCS-CDC)                          
258300                 IF EKO-KVAVIS < ZERO                                     
258400**** SHOULD NOT BE DONE FOR MARKETS WITH LOCAL SOURCING                   
258500**** IT IS DONE IN CLAIM STEP                                             
258600                   IF DCS-CHINA OR DCS-USA                                
258700                     CONTINUE                                             
258800                   ELSE                                                   
258900**** SHOULD BE DONE FOR MARKETS THAT IS NOT OWN BY VCC                    
259000                     IF DCS-LAND-NON-VCC-OWNED                            
259100                       PERFORM DEIB-LOGG-R32-ULEV-WDR8                    
259200                     END-IF                                               
259300                   END-IF                                                 
259400                 ELSE                                                     
259500                   PERFORM DEFB-LOGG-EKO-WDR8-WDR9                        
259600                   PERFORM S01-KOLLA-PALAGG                               
259700                 END-IF                                                   
259800               ELSE                                                       
259900                 IF EKO-KVAVIS < ZERO                                     
260000                    PERFORM DEI-LOGG-R32-ULEV-WDR9                        
260100                 END-IF                                                   
260200               END-IF                                                     
260300             END-IF                                                       
260400           ELSE                                                           
260500             IF WS-KVDIFF-MOT-AVIS      NOT = ZERO                        
260600               IF WS-INLA-INL-IDDC NOT = DCS-IDDC                         
260700                  MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                    
260800                  PERFORM IMS-GU-WDB601                                   
260900               END-IF                                                     
261000               IF (DCS-LAND-NON-VCC-OWNED AND INLA-ART-KDRT = 8)          
261100               OR (DCS-USA    AND INLA-ART-KDRT = 8)                      
261200               OR (DCS-CDC AND INLA-ART-KDRT = 8)                         
261300                   PERFORM S08-SKAPA-EK-TRANS                             
261400               END-IF                                                     
261500             END-IF                                                       
261600             IF INLA-ART-KVAVIS NOT = ZERO                                
261700               IF INLA-ART-KDRT = 6                                       
261800                 IF  WS-KVDIFF-MOT-AVIS NOT = ZERO                        
261900                   PERFORM DEK-LOGG-EKO-WDR8-WDR9-RT6-AVV                 
262000                 END-IF                                                   
262100               ELSE                                                       
262200                 PERFORM DEG-LOGG-EKO-WDR9                                
262300               END-IF                                                     
262400             ELSE                                                         
262500               IF INLA-ART-KDRT = 6                                       
262600                 IF  WS-KVDIFF-MOT-AVIS NOT = ZERO                        
262700                   PERFORM DEK-LOGG-EKO-WDR8-WDR9-RT6-AVV                 
262800                 END-IF                                                   
262900               END-IF                                                     
263000             END-IF                                                       
263100             IF INLA-ART-KDRT NOT = 6                                     
263200               PERFORM S01-KOLLA-PALAGG                                   
263300             END-IF                                                       
263400           END-IF                                                         
263500         END-IF                                                           
263600       END-IF                                                             
263700     END-IF                                                               
263800     .                                                                    
263900     EJECT                                                                
264000 DEA-LOGG-R32 SECTION.                                                    
264100                                                                          
264200     MOVE SPACE                  TO W611R32-W611R32                       
264300                                                                          
264400     MOVE 'R32'                  TO W611R32-IDPTYP                        
264500     MOVE ZERO                   TO W611R32-KDSORT2                       
264600     MOVE W-KDCLAGER             TO W611R32-KDCLAGER                      
264700     MOVE INLA-ART-IDARTNR       TO W611R32-IDARTNR                       
264800     MOVE INLA-ART-IDLOPNRM      TO W611R32-IDLOPNRM                      
264900     MOVE WS-INLE-MOT-KDAVVANT   TO W611R32-KDAVVANT                      
265000     MOVE WS-INLE-MOT-KVANTMOT   TO W611R32-KVANTMOT                      
265100     MOVE WS-INLE-MOT-KVFORDEL   TO W611R32-KVFORDEL                      
265200     MOVE WS-INLE-MOT-KDAVVKV    TO W611R32-KDAVVKV                       
265300     MOVE WS-INLE-MOT-KVRETUR    TO W611R32-KVRETUR                       
265400                                                                          
265500     MOVE +0                     TO W611R32-IDKOLLI                       
265600                                                                          
265700     MOVE W611R32-W611R32        TO WS-ZZAC01-LOGGPOST                    
265800     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
265900     PERFORM S02-SKAPA-ZZAC01                                             
266000     .                                                                    
266100     EJECT                                                                
266200 DEB-LOGG-092-M108 SECTION.                                               
266300                                                                          
266400     MOVE INLA-ART-IDLOPNRM      TO M108-IDLOPNRM                         
266500     MOVE INLA-ART-ADLAGOMR      TO M108-ADLAGOMR                         
266600     MOVE INLA-ART-ADGANG        TO M108-ADGANG                           
266700     MOVE INLA-ART-ADPLATS       TO M108-ADPLATS                          
266800                                                                          
266900     PERFORM S03-RED-W211FEL-GNRL                                         
267000     MOVE ZERO                   TO W211FEL-KDORDKL                       
267100     MOVE '108'                  TO W211FEL-IDFELKODX                     
267200     MOVE M108-M108              TO W211FEL-FELMED                        
267300                                                                          
267400     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
267500     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
267600     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
267700     PERFORM S02-SKAPA-ZZAC01                                             
267800     .                                                                    
267900     EJECT                                                                
268000 DED-LOGG-400 SECTION.                                                    
268100                                                                          
268200     MOVE ZERO                   TO W211400-W211400                       
268300                                                                          
268400     MOVE '221'                  TO W211400-IDTTYP                        
268500     MOVE INLA-ART-IDARTNR       TO W211400-IDARTNR-S                     
268600                                    W211400-IDARTNR                       
268700     MOVE W-KDCLAGER             TO W211400-KDCLAGER-S                    
268800                                    W211400-KDCLAGER                      
268900     MOVE 99                     TO W211400-SORTFLT1                      
269000     MOVE 009                    TO W211400-POSTLGD                       
269100     MOVE 400                    TO W211400-IDPTYP                        
269200     MOVE WS-INLE-MOT-KVRETUR    TO W211400-KVRETUR                       
269300     MOVE WS-INLA-INL-IDLEVNR    TO W211400-IDLEVNR-INL                   
269400     MOVE 1                      TO W211400-FLUPPBR                       
269500     MOVE WS-IDAVINR             TO W211400-IDKONTO                       
269600                                                                          
269700     MOVE W211400-W211400        TO WS-ZZAC01-LOGGPOST                    
269800     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
269900     PERFORM S02-SKAPA-ZZAC01                                             
270000     .                                                                    
270100     EJECT                                                                
270200 DEE-LOGG-320 SECTION.                                                    
270300                                                                          
270400     MOVE ZERO                   TO R320-W211310                          
270500                                                                          
270600     MOVE '221'                  TO R320-IDTTYP                           
270700     MOVE INLA-ART-IDARTNR       TO R320-IDARTNR-S                        
270800                                    R320-IDARTNR                          
270900     MOVE W-KDCLAGER             TO R320-KDCLAGER-S                       
271000                                    R320-KDCLAGER                         
271100     MOVE WS-ARTC-IDANSK         TO R320-IDANSKNR                         
271200     MOVE WS-ARTC-PRARTSTD       TO R320-PRARTSTD                         
271300     MOVE INLA-ART-IDLOPNRM      TO R320-IDLOPNR                          
271400     MOVE WS-INLE-MOT-KDAVVANT   TO R320-KDAVVANT                         
271500     MOVE 009                    TO R320-POSTLGD                          
271600     MOVE 320                    TO R320-IDPTYP                           
271700     MOVE WS-INLE-MOT-KVANTMOT   TO R320-KVMOTANT                         
271800     MOVE WS-INLA-INL-IDLEVNR    TO R320-IDLEVNR-INL                      
271900     MOVE WS-INLA-INL-TIAVIDAT   TO R320-TIAVSDAT                         
272000     MOVE INLA-ART-KDRT          TO R320-KDRT                             
272100     MOVE INLA-ART-KVAVIS        TO R320-KVAVIS                           
272200     MOVE WS-IDAVINR             TO R320-IDAVINR                          
272300     MOVE WS-ARTC-IDINK        TO R320-KDPKINR                            
272400                                                                          
272500*    -- OM AVISERING OCH INLÄGGNING SKER UNDER SAMMA VECKA,               
272600*    -- SÄTTS SORTFLT1 TILL 0 ANNARS TILL 1.                              
272700     IF  WS-IDLOPNRM-VV  = WS-TIAAVVD-VV                                  
272800       MOVE ZERO                 TO R320-SORTFLT1                         
272900     ELSE                                                                 
273000       MOVE 1                    TO R320-SORTFLT1                         
273100     END-IF                                                               
273200                                                                          
273300     MOVE R320-W211310           TO WS-ZZAC01-LOGGPOST                    
273400     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
273500     PERFORM S02-SKAPA-ZZAC01                                             
273600     .                                                                    
273700     EJECT                                                                
273800 DEFA-LOGG-LAB-WDR8 SECTION.                                              
273900                                                                          
274000     MOVE SPACE                  TO LAB-W510A11                           
274100                                                                          
274200     MOVE 'A11'                  TO LAB-IDPTYP                            
274300     MOVE 'I10'                  TO LAB-KDEKOHT                           
274400     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
274500        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
274600        PERFORM IMS-GU-WDB601                                             
274700     END-IF                                                               
274800     IF DCS-NDC-NA AND DCS-CANADA                                         
274900       MOVE 54                   TO LAB-IDFTG                             
275000     ELSE                                                                 
275100       MOVE 53                   TO LAB-IDFTG                             
275200     END-IF                                                               
275300     MOVE WS-INLA-INL-IDDC       TO LAB-IDDC-SEND                         
275400                                    LAB-IDDC-REC                          
275500     MOVE WS-INLA-INL-IDLEVNR    TO LAB-IDLEVNR                           
275600     MOVE WS-INLA-INL-IDFS       TO LAB-IDFS                              
275700     MOVE WS-DAGENS-DATUM        TO LAB-DAINLINL                          
275800     MOVE WS-IDORDNR-WDL612      TO LAB-IDORDNR7                          
275900     MOVE INLA-ART-IDARTNR       TO LAB-IDARTNR                           
276000     MOVE WS-ARTC-KDPRODSL       TO LAB-KDPRODSL                          
276100     MOVE WS-ARTC-KDPSLLOC       TO LAB-KDPSLLOC                          
276200     MOVE ZERO                   TO LAB-PRARTBEU                          
276300     MOVE INLA-ART-KVAVIS        TO LAB-KVAVIS                            
276400     MOVE WS-INLE-MOT-KVANTMOT   TO LAB-KVANTMOT                          
276500     MOVE AVG-PRAVCOST-NEW       TO LAB-PRAVCOST                          
276600     MOVE AVG-PRAVCOST-OLD       TO LAB-PRAVCOST-OLD                      
276700     MOVE AVG-KVLS-OLD           TO LAB-KVLS-OLD                          
276800     MOVE AVG-REMARKUP           TO LAB-REMARKUP                          
276900                                                                          
277000     MOVE WS-DATE-YYMM(1:2)      TO W-DATE-AAMM(1:2)                      
277100     MOVE WS-DATE-YYMM(3:2)      TO W-DATE-AAMM(3:2)                      
277200     MOVE W-DATE-AAMM            TO CURR-TIAAMM                           
277300     MOVE WS-KDVALISO-HUV        TO CURR-KDVALISO-HUV                     
277400     MOVE 'M'                    TO CURR-KDVALTYP                         
277500                                                                          
277600     IF WS-ARTC21-PRARTBEL-PR > ZERO                                      
277700       MOVE WS-ARTC21-PRARTBEL-PR  TO W-PRARTBEL-PR                       
277800       MOVE WS-ARTC21-KDVALISO     TO CURR-KDVALISO-ROW                   
277900     ELSE                                                                 
278000       MOVE 0.1                    TO W-PRARTBEL-PR                       
278100       MOVE 'SEK'                  TO CURR-KDVALISO-ROW                   
278200     END-IF                                                               
278300     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
278400        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
278500        PERFORM IMS-GU-WDB601                                             
278600     END-IF                                                               
278700     IF DCS-NDC-NA AND                                                    
278800      ((DCS-USA    AND CURR-KDVALISO-ROW = 'USD') OR                      
278900       (DCS-CANADA AND CURR-KDVALISO-ROW = 'CAD'))                        
279000       MOVE W-PRARTBEL-PR         TO LAB-PRARTBEU                         
279100     ELSE                                                                 
279200       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
279300       IF CURR-KDSVAR = ' '                                               
279400         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
279500         MOVE CURR-REVALUTA-TO    TO W-REVALUTA                           
279600       ELSE                                                               
279700         MOVE 1                   TO W-PRKURS                             
279800                                     W-REVALUTA                           
279900       END-IF                                                             
280000       MOVE 'USD'                 TO CURR-KDVALISO-ROW                    
280100       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
280200       IF CURR-KDSVAR = ' '                                               
280300          MOVE CURR-PRKURS-NEW    TO W-PRKURS-USD                         
280400       ELSE                                                               
280500          MOVE 1                  TO W-PRKURS-USD                         
280600       END-IF                                                             
280700                                                                          
280800       MOVE 'CAD'                 TO CURR-KDVALISO-ROW                    
280900       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
281000       IF CURR-KDSVAR = ' '                                               
281100          MOVE CURR-PRKURS-NEW    TO W-PRKURS-CAD                         
281200       ELSE                                                               
281300          MOVE 1                  TO W-PRKURS-CAD                         
281400       END-IF                                                             
281500       IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                 
281600          MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                            
281700          PERFORM IMS-GU-WDB601                                           
281800       END-IF                                                             
281900       IF DCS-NDC-NA AND DCS-USA                                          
282000         COMPUTE W-PRKURS-ML ROUNDED = W-PRKURS-USD / W-PRKURS            
282100       ELSE                                                               
282200         COMPUTE W-PRKURS-ML ROUNDED = W-PRKURS-CAD / W-PRKURS            
282300       END-IF                                                             
282400       COMPUTE LAB-PRARTBEU ROUNDED = W-PRARTBEL-PR / W-PRKURS-ML         
282500     END-IF                                                               
282600                                                                          
282700     MOVE 'W6019300'        TO EKO-FIL-IDPGM                              
282800     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
282900     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
283000     MOVE 1                 TO EKO-FIL-IDSEKVNR                           
283100     MOVE 'W510'            TO EKO-FIL-CT-IDSYSTEM                        
283200     MOVE 'A11'             TO EKO-FIL-CT-IDPTYP                          
283300     MOVE ' '               TO EKO-FIL-CT-IDVTYP                          
283400                                                                          
283500     PERFORM IMS-ISRT-EKOTRANS                                            
283600                                                                          
283700     PERFORM UNTIL SEGMENT-FINNS                                          
283800       ADD +1 TO EKO-FIL-IDSEKVNR                                         
283900       PERFORM IMS-ISRT-EKOTRANS                                          
284000     END-PERFORM                                                          
284100     .                                                                    
284200     EJECT                                                                
284300 DEFB-LOGG-EKO-WDR8-WDR9 SECTION.                                         
284400     MOVE SPACE                      TO EKO-W51080                        
284500     MOVE SPACE                      TO WS-SAP-MM-POST                    
284600                                                                          
284700     MOVE INLA-ART-IDARTNR           TO EKO-IDARTNR                       
284800     MOVE WS-INLA-INL-IDDC           TO EKO-IDDC                          
284900     MOVE WS-INLA-INL-IDFS           TO EKO-IDFS                          
285000     MOVE WS-ARTC-IDINK-X            TO EKO-IDINK                         
285100     MOVE WS-INLA-INL-IDKONTO        TO EKO-IDKONTO                       
285200     MOVE WS-INLA-INL-IDLEVNR        TO EKO-IDLEVNR                       
285300     MOVE INLA-ART-IDLOPNRM          TO EKO-IDLOPNRM                      
285400     MOVE WS-ARTC-KDPRODSL           TO EKO-KDPRODSL                      
285500     MOVE INLA-ART-KDRT              TO EKO-KDRT                          
285600     MOVE WS-ARTC-KDSORT             TO EKO-KDSORT                        
285700     MOVE WS-ARTC-KDTIPPR            TO EKO-KDTIPPR                       
285800     IF WS-INLA-INL-IDLEVNR = '1441'                                      
285900       MOVE NEJ                      TO WS-SAP-MM-POST                    
286000     END-IF                                                               
286100     IF DCS-CHINA OR DCS-USA                                              
286200       PERFORM DEFBAA-GET-PRARTBES                                        
286300       MOVE WS-ARTC-PRINK            TO EKO-PRINK                         
286400       MOVE WS-ARTC-PRHEMTAG         TO EKO-PRHEMTAG                      
286500       IF DCS-CHINA                                                       
286600         MOVE 60                     TO EKO-IDFTG                         
286700         MOVE WS-KDVALISO-HUV-CN     TO CURR-KDVALISO-HUV                 
286800       ELSE                                                               
286900         MOVE 53                     TO EKO-IDFTG                         
287000         MOVE WS-KDVALISO-HUV-US     TO CURR-KDVALISO-HUV                 
287100       END-IF                                                             
287200       MOVE WS-INLA-INL-TIAVIDAT     TO WS-DAAVIDAT-YYMMDD                
287300       MOVE WS-DAAVIDAT-YYMMDD(1:2)  TO W-DATE-AAMM(1:2)                  
287400       MOVE WS-DAAVIDAT-YYMMDD(3:2)  TO W-DATE-AAMM(3:2)                  
287500       MOVE WS-ARTC21-KDVALISO       TO CURR-KDVALISO-ROW                 
287600       MOVE W-DATE-AAMM              TO CURR-TIAAMM                       
287700       MOVE 'M'                      TO CURR-KDVALTYP                     
287800       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
287900       IF CURR-KDSVAR = ' '                                               
288000         MOVE CURR-PRKURS-NEW        TO WS-PRKURS                         
288100         MOVE CURR-REVALUTA-TO       TO WS-REVALUTA                       
288200       ELSE                                                               
288300         MOVE 1                      TO WS-PRKURS                         
288400         MOVE 1                      TO WS-REVALUTA                       
288500       END-IF                                                             
288600       MOVE WS-ARTC21-KDVALISO       TO EKO-KDVALISO                      
288700**** AGREE PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED                
288800**** TO COUNTRY CURRENCY                                                  
288900       COMPUTE WS-ARTC21-PRARTBES-PR = WS-ARTC21-PRARTBEL-PR              
289000                                     * WS-PRKURS / WS-REVALUTA            
289100       MOVE WS-ARTC21-PRARTBES-PR    TO EKO-PRARTBES                      
289200       IF WS-ARTC21-PRARTBEL-PR  > ZERO                                   
289300         MOVE WS-ARTC21-PRARTBEL-PR  TO EKO-PRARTBEL-PR                   
289400       ELSE                                                               
289500         MOVE 0.1                    TO EKO-PRARTBEL-PR                   
289600       END-IF                                                             
289700     ELSE                                                                 
289800       IF WS-ARTC21-PRARTBES-PR  > ZERO                                   
289900         MOVE WS-ARTC21-PRARTBES-PR  TO EKO-PRARTBES                      
290000       ELSE                                                               
290100         MOVE 0.1                    TO EKO-PRARTBES                      
290200       END-IF                                                             
290300       MOVE WS-ARTC-PRINK            TO EKO-PRINK                         
290400       MOVE WS-ARTC-PRHEMTAG         TO EKO-PRHEMTAG                      
290500       IF WS-ARTC21-PRARTBEL-PR  > ZERO                                   
290600         MOVE WS-ARTC21-PRARTBEL-PR  TO EKO-PRARTBEL-PR                   
290700         MOVE WS-ARTC21-KDVALISO     TO EKO-KDVALISO                      
290800       ELSE                                                               
290900*-- OBS! RADPRIS SAKNAS, KDVALISO=XXX SIGNALERAR DETTA FÖR LEVA1          
291000         MOVE 0.1                    TO EKO-PRARTBEL-PR                   
291100         MOVE 'XXX'                  TO EKO-KDVALISO                      
291200       END-IF                                                             
291300     END-IF                                                               
291400                                                                          
291500     COMPUTE EKO-KVAVIS = WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS          
291600     END-COMPUTE                                                          
291700                                                                          
291800     MOVE ZERO                       TO EKO-RETULF                        
291900     MOVE WS-INLA-INL-TIAVIDAT       TO EKO-TIAVIDAT                      
292000     MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                      
292100     MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                       
292200     MOVE JA                         TO EKO-FLLSBOK                       
292300     MOVE ZERO                       TO EKO-IDDISTR                       
292400     MOVE NEJ                        TO EKO-FLDIRLEV                      
292500                                                                          
292600     MOVE JA                         TO EKO-FLAVVINL                      
292700     MOVE 'A'                        TO EKO-KDINLAVV                      
292800     MOVE WS-ARTC23-IDAVTAL          TO EKO-IDAVTAL                       
292900                                                                          
293000     MOVE 'W6019300'                 TO EKO-FIL-IDPGM                     
293100     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
293200     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
293300     MOVE 1                          TO EKO-FIL-IDSEKVNR                  
293400     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
293500     MOVE '80 '                      TO EKO-FIL-CT-IDPTYP                 
293600     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
293700                                                                          
293800     IF NOT (DCS-LAND-NON-VCC-OWNED OR DCS-USA)                           
293900       IF WS-SAP-MM-POST NOT = NEJ                                        
294000         PERFORM IMS-ISRT-EKOTRANS                                        
294100                                                                          
294200         PERFORM UNTIL SEGMENT-FINNS                                      
294300           ADD +1 TO EKO-FIL-IDSEKVNR                                     
294400           PERFORM IMS-ISRT-EKOTRANS                                      
294500         END-PERFORM                                                      
294600       END-IF                                                             
294700     ELSE                                                                 
294800       IF DCS-CHINA OR DCS-USA                                            
294900         IF WS-SAP-MM-POST NOT = NEJ                                      
295000           PERFORM IMS-ISRT-EKOTRANS                                      
295100                                                                          
295200           PERFORM UNTIL SEGMENT-FINNS                                    
295300             ADD +1 TO EKO-FIL-IDSEKVNR                                   
295400             PERFORM IMS-ISRT-EKOTRANS                                    
295500           END-PERFORM                                                    
295600         END-IF                                                           
295700       END-IF                                                             
295800     END-IF                                                               
295900                                                                          
296000     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
296100       MOVE 'W6019300'                  TO EKO-FIL-IDPGM                  
296200       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
296300       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
296400       MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT               
296500       MOVE +1                          TO EKO-FIL-IDSEKVNR               
296600       MOVE '103'                       TO EKO-EKH-KDEKHHT                
296700       MOVE '102'                       TO EKO-EKH-KDEKSHT                
296800       EVALUATE TRUE                                                      
296900         WHEN DCS-CHINA                                                   
297000           MOVE 'W570'                  TO EKO-FIL-IDCPYTXT(1:4)          
297100         WHEN DCS-INDIA                                                   
297200           MOVE 'W515'                  TO EKO-FIL-IDCPYTXT(1:4)          
297300         WHEN DCS-USA                                                     
297400           MOVE 'W561'                  TO EKO-FIL-IDCPYTXT(1:4)          
297500         WHEN OTHER                                                       
297600           IF DCS-KOREA                                                   
297610           OR DCS-MEXICO                                                  
297620           OR DCS-BRASIL                                                  
297630           OR DCS-SOUTH-AFRICA                                            
297700             MOVE '103'                 TO EKO-EKH-KDEKHHT                
297800             MOVE '107'                 TO EKO-EKH-KDEKSHT                
297900           END-IF                                                         
298000           MOVE DCS-KDTRADP             TO EKO-FIL-IDCPYTXT(1:4)          
298100           MOVE 'OVR'                   TO EKO-EKH-CMD                    
298200       END-EVALUATE                                                       
298300       MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)          
298400       PERFORM DEFBA-LOGG-EKO-WDR8-DET                                    
298500       PERFORM DEFBA-LOGG-EKO-WDR8-HEMT                                   
298600       IF  WS-ARTC-PRINK = WS-ARTC-PRARTSTD                               
298700       AND (INLA-ART-KDRT NOT = 3 AND 7 AND 8 AND 77)                     
298800         CONTINUE                                                         
298900       ELSE                                                               
299000         PERFORM IMS-GU-WDB601                                            
299100         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
299200         IF SEGMENT-FINNS                                                 
299300           PERFORM IMS-GNP-WDB617                                         
299400           IF SEGMENT-FINNS                                               
299500             IF DCS-USA                                                   
299600               CONTINUE                                                   
299700             ELSE                                                         
299800               PERFORM DEFBA-LOGG-EKO-WDR8-KALK                           
299900             END-IF                                                       
300000           END-IF                                                         
300100         END-IF                                                           
300200       END-IF                                                             
300300       IF DCS-USA                                                         
300400         CONTINUE                                                         
300500       ELSE                                                               
300600         PERFORM DEFBA-LOGG-EKO-WDR8-SUM                                  
300700       END-IF                                                             
300800     ELSE                                                                 
300900       MOVE 'W6019300'                  TO FIL-IDPGM                      
301000       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
301100                                           EKH-DAVERDAT                   
301200       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
301300       MOVE +1                          TO FIL-IDSEKVNR                   
301400       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
301500       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
301600       PERFORM DEFBB-LOGG-EKO-WDR9                                        
301700     END-IF                                                               
301800     .                                                                    
301900     EJECT                                                                
302000                                                                          
302100 DEFBA-LOGG-EKO-WDR8-DET SECTION.                                         
302200     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
302300     MOVE WS-INLA-INL-IDDC            TO EKO-EKH-IDDC-SEND                
302400     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
302500     MOVE +0                          TO EKO-EKH-IDDISTR                  
302600                                         EKO-EKH-IDKUNDNR                 
302700*******************************                                           
302800*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
302900     MOVE ZERO TO NOLL-RAKNARE                                            
303000     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
303100     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
303200     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
303300          FOR LEADING ZERO                                                
303400     ADD +1 TO NOLL-RAKNARE                                               
303500     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
303600          WITH POINTER NOLL-RAKNARE                                       
303700*******************************                                           
303800     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
303900     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
304000                                         EKO-EKH-PRARTNTO                 
304100                                         EKO-EKH-PRARTSJK                 
304200                                         EKO-EKH-PRLANDCO                 
304300                                         EKO-EKH-SUBEL                    
304400                                         EKO-EKH-IDORDNR5                 
304500                                         EKO-EKH-PRDIRLON                 
304600                                         EKO-EKH-PRDMTRL                  
304700                                         EKO-EKH-PROVRPAL                 
304800     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
304900     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
305000                                                                          
305100     MOVE 1.00                        TO EKO-EKH-PRKURS                   
305200                                                                          
305300     PERFORM DEFBAA-GET-PRARTBES                                          
305400     PERFORM DDAB-GET-CURRENCY-RATE                                       
305500     IF DCS-LAND-NON-VCC-OWNED                                            
305600       IF DCS-CHINA                                                       
305700         CONTINUE                                                         
305800       ELSE                                                               
305900         PERFORM CAB-LAS-PRISINFO                                         
306000       END-IF                                                             
306100     END-IF                                                               
306200*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
306300*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
306400     IF (XDC-NON-VCC-OWNED OR DCS-LAND-NON-VCC-OWNED)                     
306500     AND NOT (DCS-CHINA OR DCS-USA)                                       
306600       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
306700     END-IF                                                               
306800***                                                                       
306900     IF WS-ARTC21-PRARTBEL-PR > ZERO                                      
307000       MOVE WS-ARTC21-PRARTBEL-PR     TO EKO-EKH-PRARTSTD                 
307100       MOVE WS-ARTC21-KDVALISO        TO EKO-EKH-KDVALISO                 
307200       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
307300     ELSE                                                                 
307400       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
307500       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
307600       MOVE 1.00                      TO EKO-EKH-PRKURS                   
307700     END-IF                                                               
307800     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
307900     MOVE WS-ARTC-PRINK               TO EKO-EKH-PRINK                    
308000     MOVE WS-INLA-INL-IDANALYS        TO EKO-EKH-IDANALYS                 
308100                                                                          
308200     MOVE WS-INLE-MOT-KVANTMOT        TO EKO-EKH-KVANTMOT                 
308300     COMPUTE EKO-EKH-KVANTAL =                                            
308400             WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS                       
308500     COMPUTE WS-SUARTSTD = WS-ARTC21-PRARTBEL-SUM *                       
308600                           EKO-EKH-KVANTAL                                
308700     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
308800     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
308900                                         EKO-EKH-SUVAT                    
309000                                         EKO-EKH-KDFRAKT                  
309100     MOVE SPACE                       TO EKO-EKH-BEVAT                    
309200                                         EKO-EKH-KDANMORS                 
309300                                         EKO-EKH-IDKST                    
309400                                                                          
309500     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
309600     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
309700       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
309800     ELSE                                                                 
309900       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
310000     END-IF                                                               
310100     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
310200     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
310300     MOVE WS-INLA-INL-IDLEVNR         TO EKO-EKH-IDLEVNR                  
310400     IF INLA-ART-KVAVIS > ZERO                                            
310500       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
310600     ELSE                                                                 
310700       MOVE 0                         TO EKO-EKH-KDAVVTYP                 
310800     END-IF                                                               
310900     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
311000     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
311100     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
311200     MOVE SPACE                       TO EKO-EKH-FLDCET                   
311300     MOVE WS-INLA-INL-IDFS            TO EKO-EKH-IDKUNDRF                 
311400     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
311500     PERFORM IMS-ISRT-EKOTRANS                                            
311600                                                                          
311700     PERFORM UNTIL SEGMENT-FINNS                                          
311800       ADD +1 TO EKO-FIL-IDSEKVNR                                         
311900       PERFORM IMS-ISRT-EKOTRANS                                          
312000     END-PERFORM                                                          
312100     .                                                                    
312200     EJECT                                                                
312300                                                                          
312400 DEFBA-LOGG-EKO-WDR8-HEMT SECTION.                                        
312500     MOVE 'HEMT '                     TO EKO-EKH-KDEKNIVA                 
312600     MOVE WS-INLA-INL-IDDC            TO EKO-EKH-IDDC-SEND                
312700     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
312800     MOVE +0                          TO EKO-EKH-IDDISTR                  
312900                                         EKO-EKH-IDKUNDNR                 
313000*******************************                                           
313100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
313200     MOVE ZERO TO NOLL-RAKNARE                                            
313300     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
313400     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
313500     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
313600          FOR LEADING ZERO                                                
313700     ADD +1 TO NOLL-RAKNARE                                               
313800     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
313900          WITH POINTER NOLL-RAKNARE                                       
314000*******************************                                           
314100     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
314200     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
314300                                         EKO-EKH-PRARTNTO                 
314400                                         EKO-EKH-PRARTSJK                 
314500                                         EKO-EKH-PRLANDCO                 
314600                                         EKO-EKH-SUBEL                    
314700                                         EKO-EKH-IDORDNR5                 
314800                                         EKO-EKH-PRDIRLON                 
314900                                         EKO-EKH-PRDMTRL                  
315000                                         EKO-EKH-PROVRPAL                 
315100     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
315200     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
315300                                                                          
315400     PERFORM DEFBAA-GET-PRARTBES                                          
315500     PERFORM DDAB-GET-CURRENCY-RATE                                       
315600     IF DCS-LAND-NON-VCC-OWNED                                            
315700       IF DCS-CHINA                                                       
315800         CONTINUE                                                         
315900       ELSE                                                               
316000         PERFORM CAB-LAS-PRISINFO                                         
316100       END-IF                                                             
316200     END-IF                                                               
316300*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
316400*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
316500     IF (XDC-NON-VCC-OWNED OR DCS-LAND-NON-VCC-OWNED)                     
316600     AND NOT (DCS-CHINA OR DCS-USA)                                       
316700       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
316800     END-IF                                                               
316900***                                                                       
317000     IF WS-ARTC21-PRARTBEL-PR > ZERO                                      
317100       MOVE WS-ARTC21-PRARTBEL-PR     TO EKO-EKH-PRARTSTD                 
317200       MOVE WS-ARTC21-KDVALISO        TO EKO-EKH-KDVALISO                 
317300       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
317400     ELSE                                                                 
317500       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
317600       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
317700       MOVE 1.00                      TO EKO-EKH-PRKURS                   
317800     END-IF                                                               
317900     COMPUTE EKO-EKH-PRHEMTAG ROUNDED = (W-RETULF - 1) *                  
318000                                        EKO-EKH-PRARTSTD *                
318100                  (WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS)                
318200     MOVE EKO-EKH-PRHEMTAG            TO EKO-EKH-SUBEL                    
318300     MOVE EKO-EKH-PRHEMTAG            TO WS-SUHEMT                        
318400     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
318500     MOVE ZERO                        TO EKO-EKH-PRINK                    
318600     MOVE WS-INLA-INL-IDANALYS        TO EKO-EKH-IDANALYS                 
318700                                                                          
318800     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
318900     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
319000     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
319100     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
319200                                         EKO-EKH-SUVAT                    
319300                                         EKO-EKH-KDFRAKT                  
319400     MOVE SPACE                       TO EKO-EKH-BEVAT                    
319500                                         EKO-EKH-IDKST                    
319600                                         EKO-EKH-KDANMORS                 
319700                                                                          
319800     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
319900     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
320000       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
320100     ELSE                                                                 
320200       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
320300     END-IF                                                               
320400     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
320500     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
320600     MOVE WS-INLA-INL-IDLEVNR         TO EKO-EKH-IDLEVNR                  
320700     MOVE 0                           TO EKO-EKH-KDAVVTYP                 
320800     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
320900     MOVE ZERO                        TO EKO-EKH-KVAVIS                   
321000     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
321100     MOVE SPACE                       TO EKO-EKH-FLDCET                   
321200     MOVE WS-INLA-INL-IDFS            TO EKO-EKH-IDKUNDRF                 
321300     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
321400                                                                          
321500     IF EKO-EKH-PRHEMTAG > ZERO                                           
321600       PERFORM IMS-ISRT-EKOTRANS                                          
321700                                                                          
321800       PERFORM UNTIL SEGMENT-FINNS                                        
321900         ADD +1 TO EKO-FIL-IDSEKVNR                                       
322000         PERFORM IMS-ISRT-EKOTRANS                                        
322100       END-PERFORM                                                        
322200     END-IF                                                               
322300     .                                                                    
322400     EJECT                                                                
322500                                                                          
322600 DEFBA-LOGG-EKO-WDR8-KALK SECTION.                                        
322700     MOVE 'KALK '                     TO EKO-EKH-KDEKNIVA                 
322800     MOVE WS-INLA-INL-IDDC            TO EKO-EKH-IDDC-SEND                
322900     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
323000     MOVE +0                          TO EKO-EKH-IDDISTR                  
323100                                         EKO-EKH-IDKUNDNR                 
323200*******************************                                           
323300*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
323400     MOVE ZERO TO NOLL-RAKNARE                                            
323500     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
323600     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
323700     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
323800          FOR LEADING ZERO                                                
323900     ADD +1 TO NOLL-RAKNARE                                               
324000     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
324100          WITH POINTER NOLL-RAKNARE                                       
324200*******************************                                           
324300     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
324400     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
324500                                         EKO-EKH-PRARTNTO                 
324600                                         EKO-EKH-PRARTSJK                 
324700                                         EKO-EKH-PRLANDCO                 
324800                                         EKO-EKH-SUBEL                    
324900                                         EKO-EKH-IDORDNR5                 
325000                                         EKO-EKH-PRDIRLON                 
325100                                         EKO-EKH-PRDMTRL                  
325200                                         EKO-EKH-PROVRPAL                 
325300     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
325400     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
325500                                                                          
325600     MOVE 1.00                        TO EKO-EKH-PRKURS                   
325700                                                                          
325800     MOVE WS-INLA-INL-TIAVIDAT    TO WS-DAAVIDAT-YYMMDD                   
325900     MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                     
326000     MOVE 01                      TO W-DATE-AAMM(3:2)                     
326100     MOVE WS-KDVALISO-HUV         TO CURR-KDVALISO-HUV                    
326200     MOVE DCS-KDVALISO            TO CURR-KDVALISO-ROW                    
326300     MOVE W-DATE-AAMM             TO CURR-TIAAMM                          
326400     MOVE 'A'                     TO CURR-KDVALTYP                        
326500     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
326600     IF CURR-KDSVAR = ' '                                                 
326700       MOVE CURR-PRKURS-NEW       TO WS-PRKURS                            
326800       MOVE CURR-REVALUTA-TO      TO WS-REVALUTA                          
326900     ELSE                                                                 
327000       MOVE 1                     TO WS-PRKURS                            
327100       MOVE 1                     TO WS-REVALUTA                          
327200     END-IF                                                               
327300     COMPUTE EKO-EKH-PRDIRLON ROUNDED = WS-ARTC-PRDIRLON *                
327400                     PROC-REDIRLON * WS-REVALUTA / WS-PRKURS *            
327500                    (WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS)              
327600     COMPUTE EKO-EKH-PRDMTRL  ROUNDED = WS-ARTC-PRDMTRL  *                
327700                     PROC-REDMTRL  * WS-REVALUTA / WS-PRKURS *            
327800                    (WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS)              
327900     MOVE EKO-EKH-PRDMTRL             TO WS-SUDIRMTRL                     
328000     MOVE EKO-EKH-PRDIRLON            TO WS-SUDIRLON                      
328100     COMPUTE EKO-EKH-SUBEL = EKO-EKH-PRDIRLON +                           
328200                             EKO-EKH-PRDMTRL                              
328300     MOVE ZERO                        TO EKO-EKH-PROVRPAL                 
328400     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
328500     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
328600     MOVE ZERO                        TO EKO-EKH-PRINK                    
328700     MOVE WS-INLA-INL-IDANALYS        TO EKO-EKH-IDANALYS                 
328800                                                                          
328900     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
329000     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
329100     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
329200     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
329300                                         EKO-EKH-SUVAT                    
329400                                         EKO-EKH-KDFRAKT                  
329500     MOVE SPACE                       TO EKO-EKH-BEVAT                    
329600                                         EKO-EKH-KDANMORS                 
329700                                         EKO-EKH-IDKST                    
329800                                                                          
329900     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
330000     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
330100       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
330200     ELSE                                                                 
330300       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
330400     END-IF                                                               
330500     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
330600     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
330700     MOVE WS-INLA-INL-IDLEVNR         TO EKO-EKH-IDLEVNR                  
330800     MOVE 0                           TO EKO-EKH-KDAVVTYP                 
330900     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
331000     MOVE ZERO                        TO EKO-EKH-KVAVIS                   
331100     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
331200     MOVE SPACE                       TO EKO-EKH-FLDCET                   
331300     MOVE WS-INLA-INL-IDFS            TO EKO-EKH-IDKUNDRF                 
331400     MOVE WS-ARTC21-KDVALISO          TO EKO-EKH-KDVALISO                 
331500     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
331600                                                                          
331700     IF EKO-EKH-SUBEL > ZERO                                              
331800       PERFORM IMS-ISRT-EKOTRANS                                          
331900                                                                          
332000       PERFORM UNTIL SEGMENT-FINNS                                        
332100         ADD +1 TO EKO-FIL-IDSEKVNR                                       
332200         PERFORM IMS-ISRT-EKOTRANS                                        
332300       END-PERFORM                                                        
332400     END-IF                                                               
332500     .                                                                    
332600     EJECT                                                                
332700                                                                          
332800 DEFBA-LOGG-EKO-WDR8-SUM SECTION.                                         
332900     MOVE 'SUM  '                     TO EKO-EKH-KDEKNIVA                 
333000     MOVE WS-INLA-INL-IDDC            TO EKO-EKH-IDDC-SEND                
333100     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
333200     MOVE +0                          TO EKO-EKH-IDDISTR                  
333300                                         EKO-EKH-IDKUNDNR                 
333400*******************************                                           
333500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
333600     MOVE ZERO TO NOLL-RAKNARE                                            
333700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
333800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
333900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
334000          FOR LEADING ZERO                                                
334100     ADD +1 TO NOLL-RAKNARE                                               
334200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
334300          WITH POINTER NOLL-RAKNARE                                       
334400*******************************                                           
334500     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
334600     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
334700                                         EKO-EKH-PRARTNTO                 
334800                                         EKO-EKH-PRARTSJK                 
334900                                         EKO-EKH-PRLANDCO                 
335000                                         EKO-EKH-SUBEL                    
335100                                         EKO-EKH-IDORDNR5                 
335200                                         EKO-EKH-PRDIRLON                 
335300                                         EKO-EKH-PRDMTRL                  
335400                                         EKO-EKH-PROVRPAL                 
335500     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
335600     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
335700                                                                          
335800     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
335900     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
336000     MOVE ZERO                        TO EKO-EKH-PRINK                    
336100*    COMPUTE EKO-EKH-SUBEL = WS-SUDIRLON + WS-SUDIRMTRL +                 
336200*                            WS-SUHEMT + WS-SUARTSTD                      
336300     MOVE WS-ARTC21-KDVALISO          TO EKO-EKH-KDVALISO                 
336400     MOVE SPAR-PRKURS                 TO EKO-EKH-PRKURS                   
336500     COMPUTE EKO-EKH-SUBEL = WS-SUARTSTD                                  
336600     MOVE WS-INLA-INL-IDANALYS        TO EKO-EKH-IDANALYS                 
336700                                                                          
336800     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
336900     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
337000     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
337100     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
337200                                         EKO-EKH-SUVAT                    
337300                                         EKO-EKH-KDFRAKT                  
337400     MOVE SPACE                       TO EKO-EKH-BEVAT                    
337500                                         EKO-EKH-KDANMORS                 
337600                                         EKO-EKH-IDKST                    
337700                                                                          
337800     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
337900     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
338000       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
338100     ELSE                                                                 
338200       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
338300     END-IF                                                               
338400     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
338500     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
338600     MOVE WS-INLA-INL-IDLEVNR         TO EKO-EKH-IDLEVNR                  
338700     MOVE 0                           TO EKO-EKH-KDAVVTYP                 
338800     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
338900     MOVE ZERO                        TO EKO-EKH-KVAVIS                   
339000     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
339100     MOVE SPACE                       TO EKO-EKH-FLDCET                   
339200     MOVE WS-INLA-INL-IDFS            TO EKO-EKH-IDKUNDRF                 
339300     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
339400                                                                          
339500     PERFORM IMS-ISRT-EKOTRANS                                            
339600                                                                          
339700     PERFORM UNTIL SEGMENT-FINNS                                          
339800       ADD +1 TO EKO-FIL-IDSEKVNR                                         
339900       PERFORM IMS-ISRT-EKOTRANS                                          
340000     END-PERFORM                                                          
340100     .                                                                    
340200     EJECT                                                                
340300                                                                          
340400 DEFBAA-GET-PRARTBES SECTION.                                             
340500     MOVE ZERO              TO WS-ARTC21-PRARTBEL-PR                      
340600     MOVE ZERO              TO WS-ARTC21-PRARTBEL-SUM                     
340700     MOVE ZERO              TO WS-ARTC21-PRARTBES-PR                      
340800                                                                          
340900     MOVE MSGI-TILOKDAT     TO DAT-I-TIDATUM                              
341000                               WS-IDAG                                    
341100     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
341200     CALL WDATKONV USING       DAT-KDDATFORM                              
341300                               DAT-I-TIDATUM                              
341400                               DAT-O-TIDATUM                              
341500                               DAT-KDSVAR                                 
341600     IF DAT-KDSVAR-FEL                                                    
341700       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
341800       CALL FELLOG                                                        
341900     END-IF                                                               
342000                                                                          
342100     MOVE DAT-TISEKEL TO WS-DAGENS-SEKEL                                  
342200     MOVE NEJ TO PRIS-FINNS-SW                                            
342300                                                                          
342400*    -- WDK711                                                            
342500     PERFORM IMS-GU-WDK711                                                
342600     IF SEGMENT-FINNS                                                     
342700                                                                          
342800*    -- WDK723                                                            
342900       PERFORM IMS-GNP-WDK723                                             
343000       IF SEGMENT-FINNS                                                   
343100         MOVE SAVT-IDAVTAL    TO WS-ARTC23-IDAVTAL                        
343200       ELSE                                                               
343300         MOVE ZERO            TO WS-ARTC23-IDAVTAL                        
343400       END-IF                                                             
343500                                                                          
343600*    -- WDK724                                                            
343700       MOVE WS-INLA-INL-TIAVIDAT  TO WS-DAAVIDAT-YYMMDD                   
343800       IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                    
343900         MOVE 19                        TO WS-DAAVIDAT-SEKEL              
344000       ELSE                                                               
344100         MOVE 20                        TO WS-DAAVIDAT-SEKEL              
344200       END-IF                                                             
344300       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAAVIDAT                     
344400       PERFORM IMS-GNP-WDK724                                             
344500                                                                          
344600       IF SEGMENT-FINNS                                                   
344700         MOVE JA                    TO PRIS-FINNS-SW                      
344800         MOVE SPRL-PRARTBEL-PR      TO WS-ARTC21-PRARTBEL-PR              
344900         MOVE SPRL-PRARTBEL-PR      TO WS-ARTC21-PRARTBEL-SUM             
345000         MOVE SPRL-PRARTBES-PR      TO WS-ARTC21-PRARTBES-PR              
345100         MOVE SPRL-KDVALISO         TO WS-ARTC21-KDVALISO                 
345200       END-IF                                                             
345300     END-IF                                                               
345400     .                                                                    
345500     EJECT                                                                
345600                                                                          
345700 DDAB-GET-CURRENCY-RATE SECTION.                                          
345800     MOVE WS-INLA-INL-IDDC  TO W-IDDC-B6                                  
345900     PERFORM IMS-GU-WDB601                                                
346000     MOVE DCS-KDVALISO      TO W-KDVALISO-HUV                             
346100     MOVE WS-ARTC21-KDVALISO TO W-KDVALISO-ROW                            
346200     IF W-KDVALISO-HUV = WS-ARTC21-KDVALISO                               
346300       MOVE 1 TO SPAR-PRKURS                                              
346400       MOVE 1 TO W-REVALUTA                                               
346500     ELSE                                                                 
346600       PERFORM IMS-GU-WDGX9306                                            
346700       IF SEGMENT-SAKNAS                                                  
346800         MOVE 1               TO SPAR-PRKURS                              
346900         MOVE 1               TO W-REVALUTA                               
347000       ELSE                                                               
347100         COMPUTE W-TISTADAT-9KOMPL =                                      
347200                 9999999 - WS-DATE-YYMMDD                                 
347300         PERFORM IMS-GNP-WDGX9308                                         
347400         IF SEGMENT-SAKNAS                                                
347500           PERFORM IMS-GNP-WDGX9308-FIRST                                 
347600           IF SEGMENT-SAKNAS                                              
347700             MOVE 1               TO SPAR-PRKURS                          
347800             MOVE 1               TO W-REVALUTA                           
347900           ELSE                                                           
348000             MOVE 9308-PRKURS   TO SPAR-PRKURS                            
348100             MOVE 9308-REVALUTA-TO TO W-REVALUTA                          
348200           END-IF                                                         
348300         ELSE                                                             
348400           MOVE 9308-PRKURS   TO SPAR-PRKURS                              
348500           MOVE 9308-REVALUTA-TO TO W-REVALUTA                            
348600         END-IF                                                           
348700       END-IF                                                             
348800     END-IF                                                               
348900     .                                                                    
349000     EJECT                                                                
349100                                                                          
349200 DDAC-GET-CURR-RATE-LOCAL SECTION.                                        
349300                                                                          
349400     IF W-KDVALISO-HUV = WS-ARTC21-KDVALISO                               
349500       CONTINUE                                                           
349600     ELSE                                                                 
349700       MOVE WS-ARTC-KDPRODSL           TO TEST-KDPRODSL                   
349800       IF KDPRODSL-LOCAL                                                  
349900         MOVE WS-INLA-INL-TIAVIDAT     TO WS-DAAVIDAT-YYMMDD              
350000         MOVE WS-DAAVIDAT-YYMMDD(1:2)  TO W-DATE-AAMM(1:2)                
350100         MOVE WS-DAAVIDAT-YYMMDD(3:2)  TO W-DATE-AAMM(3:2)                
350200         MOVE W-KDVALISO-HUV           TO CURR-KDVALISO-HUV               
350300         MOVE WS-ARTC21-KDVALISO       TO CURR-KDVALISO-ROW               
350400         MOVE W-DATE-AAMM              TO CURR-TIAAMM                     
350500         MOVE 'M'                      TO CURR-KDVALTYP                   
350600         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
350700         IF CURR-KDSVAR = ' '                                             
350800           MOVE CURR-PRKURS-NEW        TO WS-PRKURS                       
350900                                          SPAR-PRKURS                     
351000           MOVE CURR-REVALUTA-TO       TO WS-REVALUTA                     
351100         ELSE                                                             
351200           MOVE 1                      TO WS-PRKURS                       
351300                                          SPAR-PRKURS                     
351400           MOVE 1                      TO WS-REVALUTA                     
351500         END-IF                                                           
351600**** PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED                      
351700**** TO COUNTRY CURRENCY                                                  
351800         COMPUTE WS-ARTC21-PRARTBEL-PR = WS-ARTC21-PRARTBEL-PR            
351900                                   * WS-PRKURS / WS-REVALUTA              
352000       END-IF                                                             
352100     END-IF                                                               
352200     .                                                                    
352300     EJECT                                                                
352400                                                                          
352500 DEFBB-LOGG-EKO-WDR9 SECTION.                                             
352600     MOVE '103'                       TO EKH-KDEKHHT                      
352700     MOVE '102'                       TO EKH-KDEKSHT                      
352800     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
352900     MOVE WS-INLA-INL-IDDC            TO EKH-IDDC-SEND                    
353000     MOVE SPACE                       TO EKH-IDDC-REC                     
353100     MOVE +0                          TO EKH-IDDISTR                      
353200                                         EKH-IDKUNDNR                     
353300*******************************                                           
353400*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
353500     MOVE ZERO TO NOLL-RAKNARE                                            
353600     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
353700     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
353800     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
353900          FOR LEADING ZERO                                                
354000     ADD +1 TO NOLL-RAKNARE                                               
354100     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
354200          WITH POINTER NOLL-RAKNARE                                       
354300*******************************                                           
354400     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
354500     MOVE ZERO                        TO EKH-KDPSLLOC                     
354600                                         EKH-PRARTNTO                     
354700                                         EKH-PRARTSJK                     
354800                                         EKH-PRLANDCO                     
354900                                         EKH-SUBEL                        
355000                                         EKH-IDORDNR5                     
355100                                         EKH-PRDIRLON                     
355200                                         EKH-PRDMTRL                      
355300                                         EKH-PROVRPAL                     
355400     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
355500     MOVE SPACE                       TO EKH-FLLSBOK                      
355600                                                                          
355700     IF EKO-KDVALISO = 'XXX'                                              
355800       MOVE 'SEK'                     TO EKH-KDVALISO                     
355900     ELSE                                                                 
356000       MOVE EKO-KDVALISO              TO EKH-KDVALISO                     
356100     END-IF                                                               
356200     MOVE 1.00                        TO EKH-PRKURS                       
356300                                                                          
356400     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
356500     IF WS-INLA-INL-IDLEVNR = '1002 '                                     
356600       MOVE ZERO                      TO EKH-PRHEMTAG                     
356700     ELSE                                                                 
356800       MOVE WS-ARTC-PRHEMTAG          TO EKH-PRHEMTAG                     
356900     MOVE WS-ARTC-PRHEMTAG            TO EKH-PRHEMTAG                     
357000     END-IF                                                               
357100     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
357200     MOVE WS-INLA-INL-IDANALYS        TO EKH-IDANALYS                     
357300                                                                          
357400     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
357500     COMPUTE EKH-KVANTAL = WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS         
357600     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
357700     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
357800     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
357900     MOVE ZERO                        TO EKH-IDKONTO                      
358000                                         EKH-SUVAT                        
358100                                         EKH-KDFRAKT                      
358200     MOVE SPACE                       TO EKH-BEVAT                        
358300                                         EKH-IDKST                        
358400                                         EKH-KDANMORS                     
358500                                                                          
358600     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
358700     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
358800       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
358900     ELSE                                                                 
359000       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
359100     END-IF                                                               
359200     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
359300     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
359400     MOVE WS-INLA-INL-IDLEVNR         TO EKH-IDLEVNR                      
359500     IF INLA-ART-KVAVIS > ZERO                                            
359600       MOVE 1                         TO EKH-KDAVVTYP                     
359700     ELSE                                                                 
359800       MOVE 0                         TO EKH-KDAVVTYP                     
359900     END-IF                                                               
360000     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
360100     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
360200     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
360300     MOVE 'SEPV'                      TO EKH-KDTRADP                      
360400     MOVE SPACE                       TO EKH-FLDCET                       
360500     MOVE SPACE                       TO EKH-IDKUNDRF                     
360600     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
360700                                                                          
360800     PERFORM IMS-ISRT-WLSAPA01                                            
360900                                                                          
361000     PERFORM UNTIL SEGMENT-FINNS                                          
361100       ADD +1 TO FIL-IDSEKVNR                                             
361200       PERFORM IMS-ISRT-WLSAPA01                                          
361300     END-PERFORM                                                          
361400     .                                                                    
361500     EJECT                                                                
361600 DEFC-LOGG-WDR9 SECTION.                                                  
361700     MOVE 'W6019300'                  TO FIL-IDPGM                        
361800     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
361900                                         EKH-DAVERDAT                     
362000     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
362100     MOVE +1                          TO FIL-IDSEKVNR                     
362200     MOVE '102'                       TO EKH-KDEKHHT                      
362300     MOVE '109'                       TO EKH-KDEKSHT                      
362400     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
362500     MOVE WS-INLA-INL-IDDC            TO EKH-IDDC-SEND                    
362600     MOVE SPACE                       TO EKH-IDDC-REC                     
362700     MOVE +0                          TO EKH-IDDISTR                      
362800                                         EKH-IDKUNDNR                     
362900*******************************                                           
363000*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
363100     MOVE ZERO TO NOLL-RAKNARE                                            
363200     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
363300     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
363400     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
363500          FOR LEADING ZERO                                                
363600     ADD +1 TO NOLL-RAKNARE                                               
363700     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
363800          WITH POINTER NOLL-RAKNARE                                       
363900*******************************                                           
364000     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
364100     MOVE ZERO                        TO EKH-KDPSLLOC                     
364200                                         EKH-PRARTNTO                     
364300                                         EKH-PRARTSJK                     
364400                                         EKH-PRLANDCO                     
364500                                         EKH-SUBEL                        
364600                                         EKH-IDORDNR5                     
364700                                         EKH-PRDIRLON                     
364800                                         EKH-PRDMTRL                      
364900                                         EKH-PROVRPAL                     
365000     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
365100     MOVE SPACE                       TO EKH-FLLSBOK                      
365200                                                                          
365300     MOVE 'SEK'                       TO EKH-KDVALISO                     
365400     MOVE 1.00                        TO EKH-PRKURS                       
365500                                                                          
365600     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
365700     IF WS-INLA-INL-IDLEVNR = '1002 '                                     
365800       MOVE ZERO                      TO EKH-PRHEMTAG                     
365900     ELSE                                                                 
366000       MOVE WS-ARTC-PRHEMTAG          TO EKH-PRHEMTAG                     
366100     END-IF                                                               
366200     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
366300     MOVE WS-INLA-INL-IDANALYS        TO EKH-IDANALYS                     
366400                                                                          
366500     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
366600     COMPUTE EKH-KVANTAL = WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS         
366700     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
366800     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
366900     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
367000     MOVE ZERO                        TO EKH-IDKONTO                      
367100                                         EKH-SUVAT                        
367200                                         EKH-KDFRAKT                      
367300     MOVE SPACE                       TO EKH-BEVAT                        
367400                                         EKH-IDKST                        
367500                                         EKH-KDANMORS                     
367600                                                                          
367700     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
367800     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
367900       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
368000     ELSE                                                                 
368100       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
368200     END-IF                                                               
368300     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
368400     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
368500     MOVE WS-INLA-INL-IDLEVNR         TO EKH-IDLEVNR                      
368600     IF INLA-ART-KVAVIS > ZERO                                            
368700       MOVE 1                         TO EKH-KDAVVTYP                     
368800     ELSE                                                                 
368900       MOVE 0                         TO EKH-KDAVVTYP                     
369000     END-IF                                                               
369100     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
369200     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
369300     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
369400     MOVE SPACE                       TO EKH-KDTRADP                      
369500     MOVE SPACE                       TO EKH-FLDCET                       
369600     MOVE SPACE                       TO EKH-IDKUNDRF                     
369700     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
369800                                                                          
369900     PERFORM IMS-ISRT-WLSAPA01                                            
370000                                                                          
370100     PERFORM UNTIL SEGMENT-FINNS                                          
370200       ADD +1 TO FIL-IDSEKVNR                                             
370300       PERFORM IMS-ISRT-WLSAPA01                                          
370400     END-PERFORM                                                          
370500     .                                                                    
370600     EJECT                                                                
370700 DEG-LOGG-EKO-WDR9 SECTION.                                               
370800                                                                          
370900     IF INLA-ART-KDRT = 3                                                 
371000** UTTAG SATS                                                             
371100       MOVE 'W6019300'                  TO FIL-IDPGM                      
371200       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
371300                                           EKH-DAVERDAT                   
371400       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
371500       MOVE +1                          TO FIL-IDSEKVNR                   
371600       MOVE '102'                       TO EKH-KDEKHHT                    
371700       MOVE '103'                       TO EKH-KDEKSHT                    
371800       MOVE 'DET  '                     TO EKH-KDEKNIVA                   
371900       MOVE WS-INLA-INL-IDDC            TO EKH-IDDC-SEND                  
372000       MOVE SPACE                       TO EKH-IDDC-REC                   
372100       MOVE +0                          TO EKH-IDDISTR                    
372200                                           EKH-IDKUNDNR                   
372300*******************************                                           
372400*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
372500       MOVE ZERO TO NOLL-RAKNARE                                          
372600       MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                
372700       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
372800       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
372900            FOR LEADING ZERO                                              
373000       ADD +1 TO NOLL-RAKNARE                                             
373100       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                     
373200          WITH POINTER NOLL-RAKNARE                                       
373300*******************************                                           
373400       MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                   
373500       MOVE ZERO                        TO EKH-KDPSLLOC                   
373600                                           EKH-PRARTNTO                   
373700                                           EKH-PRARTSJK                   
373800                                           EKH-PRHEMTAG                   
373900                                           EKH-PRLANDCO                   
374000                                           EKH-PRDIRLON                   
374100                                           EKH-PRDMTRL                    
374200                                           EKH-PROVRPAL                   
374300                                           EKH-SUBEL                      
374400                                           EKH-IDORDNR5                   
374500       MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                    
374600       MOVE SPACE                       TO EKH-FLLSBOK                    
374700                                                                          
374800       MOVE 'SEK'                     TO EKH-KDVALISO                     
374900       MOVE 1.00                      TO EKH-PRKURS                       
375000                                                                          
375100       MOVE WS-ARTC-PRINK               TO EKH-PRINK                      
375200       MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                   
375300                                                                          
375400       MOVE INLA-ART-KVAVIS             TO EKH-KVANTAL                    
375500       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
375600       MOVE W-IDTRANS                   TO EKH-IDTRANS                    
375700       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
375800       MOVE ZERO                        TO EKH-IDKONTO                    
375900                                           EKH-SUVAT                      
376000                                           EKH-KDFRAKT                    
376100       MOVE SPACE                       TO EKH-BEVAT                      
376200                                           EKH-IDKST                      
376300                                           EKH-KDANMORS                   
376400                                           EKH-IDANALYS                   
376500                                                                          
376600       MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD             
376700       IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                    
376800         MOVE 19                        TO WS-DAAVIDAT-SEKEL              
376900       ELSE                                                               
377000         MOVE 20                        TO WS-DAAVIDAT-SEKEL              
377100       END-IF                                                             
377200       MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                   
377300       MOVE WS-IDAVINR                  TO EKH-IDAVINR                    
377400       MOVE WS-INLA-INL-IDLEVNR         TO EKH-IDLEVNR                    
377500       IF INLA-ART-KVAVIS > ZERO                                          
377600         MOVE 1                         TO EKH-KDAVVTYP                   
377700       ELSE                                                               
377800         MOVE 0                         TO EKH-KDAVVTYP                   
377900       END-IF                                                             
378000       MOVE INLA-ART-KDRT               TO EKH-KDRT                       
378100       MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                   
378200       MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                     
378300       MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                     
378400       MOVE SPACE                       TO EKH-KDTRADP                    
378500       MOVE SPACE                       TO EKH-FLDCET                     
378600       MOVE SPACE                       TO EKH-IDKUNDRF                   
378700       MOVE SPACE                       TO EKH-IDFAKT-EXP                 
378800                                                                          
378900       PERFORM IMS-ISRT-WLSAPA01                                          
379000       PERFORM UNTIL SEGMENT-FINNS                                        
379100         ADD +1 TO FIL-IDSEKVNR                                           
379200         PERFORM IMS-ISRT-WLSAPA01                                        
379300       END-PERFORM                                                        
379400     ELSE                                                                 
379500       IF WS-KVINLART-TRP > ZERO                                          
379600          PERFORM S09-SKAPA-TRANSPORT-TRANS                               
379700       END-IF                                                             
379800     END-IF                                                               
379900     .                                                                    
380000     EJECT                                                                
380100 DEHA-PALAGG-WDR9 SECTION.                                                
380200     MOVE 'W6019300'                  TO FIL-IDPGM                        
380300     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
380400                                         EKH-DAVERDAT                     
380500     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
380600     MOVE +1                          TO FIL-IDSEKVNR                     
380700     MOVE '103'                       TO EKH-KDEKHHT                      
380800     MOVE '101'                       TO EKH-KDEKSHT                      
380900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
381000     MOVE WS-INLA-INL-IDDC            TO EKH-IDDC-SEND                    
381100     MOVE SPACE                       TO EKH-IDDC-REC                     
381200     MOVE +0                          TO EKH-IDDISTR                      
381300                                         EKH-IDKUNDNR                     
381400*******************************                                           
381500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
381600     MOVE ZERO TO NOLL-RAKNARE                                            
381700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
381800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
381900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
382000          FOR LEADING ZERO                                                
382100     ADD +1 TO NOLL-RAKNARE                                               
382200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
382300          WITH POINTER NOLL-RAKNARE                                       
382400*******************************                                           
382500     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
382600     MOVE ZERO                        TO EKH-KDPSLLOC                     
382700                                         EKH-PRARTNTO                     
382800                                         EKH-PRARTSJK                     
382900                                         EKH-PRHEMTAG                     
383000                                         EKH-PRLANDCO                     
383100                                         EKH-SUBEL                        
383200                                         EKH-IDORDNR5                     
383300     MOVE WS-ARTC-PRDIRLON            TO EKH-PRDIRLON                     
383400     MOVE WS-ARTC-PRDMTRL             TO EKH-PRDMTRL                      
383500     MOVE WS-ARTC-PROVRPAL            TO EKH-PROVRPAL                     
383600     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
383700     MOVE SPACE                       TO EKH-FLLSBOK                      
383800                                                                          
383900     MOVE 'SEK'                       TO EKH-KDVALISO                     
384000     MOVE 1.00                        TO EKH-PRKURS                       
384100                                                                          
384200     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
384300     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
384400     MOVE WS-INLA-INL-IDANALYS        TO EKH-IDANALYS                     
384500                                                                          
384600     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
384700     COMPUTE EKH-KVANTAL = WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS         
384800     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
384900     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
385000     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
385100     MOVE ZERO                        TO EKH-IDKONTO                      
385200                                         EKH-SUVAT                        
385300                                         EKH-KDFRAKT                      
385400     MOVE SPACE                       TO EKH-BEVAT                        
385500                                         EKH-IDKST                        
385600                                         EKH-KDANMORS                     
385700                                                                          
385800     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
385900     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
386000       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
386100     ELSE                                                                 
386200       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
386300     END-IF                                                               
386400     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
386500     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
386600     MOVE WS-INLA-INL-IDLEVNR         TO EKH-IDLEVNR                      
386700     IF INLA-ART-KVAVIS > ZERO                                            
386800       MOVE 1                         TO EKH-KDAVVTYP                     
386900     ELSE                                                                 
387000       MOVE 0                         TO EKH-KDAVVTYP                     
387100     END-IF                                                               
387200     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
387300     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
387400     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
387500     MOVE SPACE                       TO EKH-KDTRADP                      
387600     MOVE SPACE                       TO EKH-FLDCET                       
387700     MOVE SPACE                       TO EKH-IDKUNDRF                     
387800     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
387900                                                                          
388000     PERFORM IMS-ISRT-WLSAPA01                                            
388100                                                                          
388200     PERFORM UNTIL SEGMENT-FINNS                                          
388300       ADD +1 TO FIL-IDSEKVNR                                             
388400       PERFORM IMS-ISRT-WLSAPA01                                          
388500     END-PERFORM                                                          
388600     .                                                                    
388700     EJECT                                                                
388800 DEI-LOGG-R32-ULEV-WDR9 SECTION.                                          
388900                                                                          
389000     MOVE 'W6019300'                  TO FIL-IDPGM                        
389100     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
389200                                         EKH-DAVERDAT                     
389300     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
389400     MOVE +1                          TO FIL-IDSEKVNR                     
389500     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
389600     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
389700     MOVE '102'                       TO EKH-KDEKHHT                      
389800     MOVE '106'                       TO EKH-KDEKSHT                      
389900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
390000     MOVE WC-CDC-SE                   TO EKH-IDDC-SEND                    
390100     MOVE SPACE                       TO EKH-IDDC-REC                     
390200     MOVE +0                          TO EKH-IDDISTR                      
390300                                         EKH-IDKUNDNR                     
390400*******************************                                           
390500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
390600     MOVE ZERO TO NOLL-RAKNARE                                            
390700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
390800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
390900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
391000          FOR LEADING ZERO                                                
391100     ADD +1 TO NOLL-RAKNARE                                               
391200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
391300          WITH POINTER NOLL-RAKNARE                                       
391400*******************************                                           
391500     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
391600     MOVE ZERO                        TO EKH-KDPSLLOC                     
391700                                         EKH-PRARTNTO                     
391800                                         EKH-PRARTSJK                     
391900                                         EKH-PRHEMTAG                     
392000                                         EKH-PRINK                        
392100                                         EKH-PRLANDCO                     
392200                                         EKH-PRDIRLON                     
392300                                         EKH-PRDMTRL                      
392400                                         EKH-PROVRPAL                     
392500                                         EKH-SUBEL                        
392600                                         EKH-IDORDNR5                     
392700     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
392800     MOVE SPACE                       TO EKH-FLLSBOK                      
392900     MOVE 'SEK'                       TO EKH-KDVALISO                     
393000     MOVE 1.00                        TO EKH-PRKURS                       
393100                                                                          
393200*    VÄNDER PÅ TECKNET PGA ATT DETTA GÖRS IGEN I ETT W510-JOBB I          
393300*    BATCHEN.                                                             
393400     COMPUTE EKH-KVANTAL = EKO-KVAVIS * -1                                
393500                                                                          
393600     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
393700     MOVE '6203'                      TO EKH-IDTRANS                      
393800     MOVE SPACE                       TO EKH-BEVAT                        
393900                                         EKH-IDANALYS                     
394000                                         EKH-KDANMORS                     
394100     MOVE ZERO                        TO EKH-IDKONTO                      
394200                                         EKH-SUVAT                        
394300                                         EKH-KDFRAKT                      
394400                                                                          
394500                                                                          
394600     MOVE FUNCTION CURRENT-DATE (1:8) TO EKH-DAAVIDAT                     
394700     MOVE ZERO                        TO EKH-IDAVINR                      
394800     MOVE WS-INLA-INL-IDLEVNR         TO EKH-IDLEVNR                      
394900     MOVE ZERO                        TO EKH-KDAVVTYP                     
395000                                         EKH-KDRT                         
395100                                         EKH-KVANTMOT                     
395200                                         EKH-KVAVIS                       
395300     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
395400     MOVE SPACE                       TO EKH-KDTRADP                      
395500     MOVE SPACE                       TO EKH-FLDCET                       
395600     MOVE SPACE                       TO EKH-IDKUNDRF                     
395700                                         EKH-IDKST                        
395800     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
395900                                                                          
396000                                                                          
396100     PERFORM IMS-ISRT-WLSAPA01                                            
396200                                                                          
396300     PERFORM UNTIL SEGMENT-FINNS                                          
396400       ADD +1 TO FIL-IDSEKVNR                                             
396500       PERFORM IMS-ISRT-WLSAPA01                                          
396600     END-PERFORM                                                          
396700     .                                                                    
396800     EJECT                                                                
396900                                                                          
397000 DEIB-LOGG-R32-ULEV-WDR8 SECTION.                                         
397100     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
397200       MOVE 'W6019300'                  TO EKO-FIL-IDPGM                  
397300       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
397400       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
397500       MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT               
397600       MOVE +1                          TO EKO-FIL-IDSEKVNR               
397700       MOVE '103'                       TO EKO-EKH-KDEKHHT                
397800       MOVE '102'                       TO EKO-EKH-KDEKSHT                
397900       EVALUATE TRUE                                                      
398000         WHEN DCS-CHINA                                                   
398100           MOVE 'W570'                  TO EKO-FIL-IDCPYTXT(1:4)          
398200         WHEN DCS-INDIA                                                   
398300           MOVE 'W515'                  TO EKO-FIL-IDCPYTXT(1:4)          
398400         WHEN DCS-USA                                                     
398500           MOVE 'W561'                  TO EKO-FIL-IDCPYTXT(1:4)          
398600         WHEN OTHER                                                       
398700           IF DCS-KOREA                                                   
398710           OR DCS-MEXICO                                                  
398720           OR DCS-BRASIL                                                  
398730           OR DCS-SOUTH-AFRICA                                            
398800             MOVE '103'                 TO EKO-EKH-KDEKHHT                
398900             MOVE '106'                 TO EKO-EKH-KDEKSHT                
399000           END-IF                                                         
399100           MOVE DCS-KDTRADP             TO EKO-FIL-IDCPYTXT(1:4)          
399200           MOVE 'SHO'                   TO EKO-EKH-CMD                    
399300       END-EVALUATE                                                       
399400       MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)          
399500       PERFORM DEFBA-LOGG-EKO-WDR8-DET                                    
399600       IF  WS-ARTC-PRINK = WS-ARTC-PRARTSTD                               
399700       AND (INLA-ART-KDRT NOT = 3 AND 7 AND 8 AND 77)                     
399800         CONTINUE                                                         
399900       ELSE                                                               
400000         PERFORM IMS-GU-WDB601                                            
400100         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
400200         IF SEGMENT-FINNS                                                 
400300           PERFORM IMS-GNP-WDB617                                         
400400           IF SEGMENT-FINNS                                               
400500             IF DCS-USA                                                   
400600               CONTINUE                                                   
400700             ELSE                                                         
400800               PERFORM DEFBA-LOGG-EKO-WDR8-KALK                           
400900             END-IF                                                       
401000           END-IF                                                         
401100         END-IF                                                           
401200       END-IF                                                             
401300       IF DCS-USA                                                         
401400         CONTINUE                                                         
401500       ELSE                                                               
401600         PERFORM DEFBA-LOGG-EKO-WDR8-SUM                                  
401700       END-IF                                                             
401800     END-IF                                                               
401900     .                                                                    
402000     EJECT                                                                
402100                                                                          
402200 DEK-LOGG-EKO-WDR8-WDR9-RT6-AVV SECTION.                                  
402300     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
402400       MOVE 'W6019300'                  TO EKO-FIL-IDPGM                  
402500       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
402600       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
402700       MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT               
402800       MOVE +1                          TO EKO-FIL-IDSEKVNR               
402900       PERFORM IMS-GU-WDB601                                              
403000       EVALUATE TRUE                                                      
403100        WHEN DCS-CHINA                                                    
403200          MOVE 'W570'                   TO EKO-FIL-IDCPYTXT(1:4)          
403300        WHEN DCS-INDIA                                                    
403400          MOVE 'W515'                   TO EKO-FIL-IDCPYTXT(1:4)          
403500        WHEN DCS-USA                                                      
403600          MOVE 'W561'                   TO EKO-FIL-IDCPYTXT(1:4)          
403700        WHEN OTHER                                                        
403800          MOVE DCS-KDTRADP              TO EKO-FIL-IDCPYTXT(1:4)          
403900       END-EVALUATE                                                       
404000       MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)          
404100       PERFORM DEKA-LOGG-EKO-WDR8-RT6-AVV                                 
404200     ELSE                                                                 
404300       MOVE 'W6019300'                  TO FIL-IDPGM                      
404400       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
404500                                           EKH-DAVERDAT                   
404600       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
404700       MOVE +1                          TO FIL-IDSEKVNR                   
404800       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
404900       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
405000       PERFORM DEKB-LOGG-EKO-WDR9-RT6-AVV                                 
405100     END-IF                                                               
405200     .                                                                    
405300     EJECT                                                                
405400                                                                          
405500 DEKA-LOGG-EKO-WDR8-RT6-AVV SECTION.                                      
405600     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
405700     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
405800     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
405900     MOVE WS-INLA-INL-IDDC            TO EKO-EKH-IDDC-SEND                
406000     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
406100     MOVE +0                          TO EKO-EKH-IDDISTR                  
406200     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
406300                                                                          
406400*******************************                                           
406500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
406600     MOVE ZERO TO NOLL-RAKNARE                                            
406700     MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                  
406800     MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM                
406900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
407000          FOR LEADING ZERO                                                
407100     ADD +1 TO NOLL-RAKNARE                                               
407200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
407300        WITH POINTER NOLL-RAKNARE                                         
407400*******************************                                           
407500     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
407600                                                                          
407700     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
407800                                         EKO-EKH-PRARTNTO                 
407900                                         EKO-EKH-PRARTSJK                 
408000                                         EKO-EKH-PRHEMTAG                 
408100                                         EKO-EKH-PRLANDCO                 
408200                                         EKO-EKH-PRDIRLON                 
408300                                         EKO-EKH-PRDMTRL                  
408400                                         EKO-EKH-PROVRPAL                 
408500                                         EKO-EKH-SUBEL                    
408600                                         EKO-EKH-IDORDNR5                 
408700     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
408800     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
408900                                                                          
409000     MOVE 1.00                        TO EKO-EKH-PRKURS                   
409100                                                                          
409200     MOVE ZERO                        TO EKO-EKH-PRINK                    
409300     MOVE SLAG-PRAVCOST               TO EKO-EKH-PRARTSTD                 
409400                                                                          
409500     COMPUTE EKO-EKH-KVANTAL =                                            
409600          WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS                          
409700     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
409800     MOVE SPACE                       TO EKO-EKH-BEVAT                    
409900                                         EKO-EKH-KDANMORS                 
410000     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
410100                                         EKO-EKH-SUVAT                    
410200     MOVE WS-INLA-INL-IDANALYS        TO EKO-EKH-IDANALYS                 
410300     MOVE WS-INLA-INL-IDKONTO         TO EKO-EKH-IDKONTO                  
410400     MOVE WS-INLA-INL-IDKST           TO EKO-EKH-IDKST                    
410500     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
410600     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
410700       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
410800     ELSE                                                                 
410900       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
411000     END-IF                                                               
411100     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
411200     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
411300     MOVE WS-INLA-INL-IDLEVNR         TO EKO-EKH-IDLEVNR                  
411400     IF INLA-ART-KVAVIS > ZERO                                            
411500       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
411600     ELSE                                                                 
411700       MOVE 0                         TO EKO-EKH-KDAVVTYP                 
411800     END-IF                                                               
411900     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
412000     MOVE WS-INLE-MOT-KVANTMOT        TO EKO-EKH-KVANTMOT                 
412100     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
412200     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
412300     MOVE SPACE                       TO EKO-EKH-FLDCET                   
412400     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
412500     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
412600     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
412700     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
412800                                                                          
412900     PERFORM IMS-ISRT-EKOTRANS                                            
413000                                                                          
413100     PERFORM UNTIL SEGMENT-FINNS                                          
413200       ADD +1 TO EKO-FIL-IDSEKVNR                                         
413300       PERFORM IMS-ISRT-EKOTRANS                                          
413400     END-PERFORM                                                          
413500     .                                                                    
413600     EJECT                                                                
413700                                                                          
413800 DEKB-LOGG-EKO-WDR9-RT6-AVV SECTION.                                      
413900     MOVE '102'                       TO EKH-KDEKHHT                      
414000     MOVE '102'                       TO EKH-KDEKSHT                      
414100     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
414200     MOVE WS-INLA-INL-IDDC            TO EKH-IDDC-SEND                    
414300     MOVE SPACE                       TO EKH-IDDC-REC                     
414400     MOVE +0                          TO EKH-IDDISTR                      
414500     MOVE +0                          TO EKH-IDKUNDNR                     
414600                                                                          
414700*******************************                                           
414800*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
414900     MOVE ZERO TO NOLL-RAKNARE                                            
415000     MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                  
415100     MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM                
415200     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
415300          FOR LEADING ZERO                                                
415400     ADD +1 TO NOLL-RAKNARE                                               
415500     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
415600        WITH POINTER NOLL-RAKNARE                                         
415700*******************************                                           
415800     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
415900                                                                          
416000     MOVE ZERO                        TO EKH-KDPSLLOC                     
416100                                         EKH-PRARTNTO                     
416200                                         EKH-PRARTSJK                     
416300                                         EKH-PRHEMTAG                     
416400                                         EKH-PRLANDCO                     
416500                                         EKH-PRDIRLON                     
416600                                         EKH-PRDMTRL                      
416700                                         EKH-PROVRPAL                     
416800                                         EKH-SUBEL                        
416900                                         EKH-IDORDNR5                     
417000     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
417100     MOVE SPACE                       TO EKH-FLLSBOK                      
417200                                                                          
417300     MOVE 'SEK'                       TO EKH-KDVALISO                     
417400     MOVE 1.00                        TO EKH-PRKURS                       
417500                                                                          
417600     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
417700     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
417800                                                                          
417900     COMPUTE EKH-KVANTAL =                                                
418000          WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS                          
418100     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
418200     MOVE SPACE                       TO EKH-BEVAT                        
418300                                         EKH-KDANMORS                     
418400     MOVE ZERO                        TO EKH-KDFRAKT                      
418500                                         EKH-SUVAT                        
418600     MOVE WS-INLA-INL-IDANALYS        TO EKH-IDANALYS                     
418700     MOVE WS-INLA-INL-IDKONTO         TO EKH-IDKONTO                      
418800     MOVE SPACE                       TO EKH-IDKST                        
418900     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
419000     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
419100       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
419200     ELSE                                                                 
419300       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
419400     END-IF                                                               
419500     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
419600     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
419700     MOVE WS-INLA-INL-IDLEVNR         TO EKH-IDLEVNR                      
419800     IF INLA-ART-KVAVIS > ZERO                                            
419900       MOVE 1                         TO EKH-KDAVVTYP                     
420000     ELSE                                                                 
420100       MOVE 0                         TO EKH-KDAVVTYP                     
420200     END-IF                                                               
420300     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
420400     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
420500     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
420600     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
420700     MOVE 'SEPV'                      TO EKH-KDTRADP                      
420800     MOVE SPACE                       TO EKH-FLDCET                       
420900     MOVE SPACE                       TO EKH-IDKUNDRF                     
421000     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
421100                                                                          
421200     PERFORM IMS-ISRT-WLSAPA01                                            
421300                                                                          
421400     PERFORM UNTIL SEGMENT-FINNS                                          
421500       ADD +1 TO FIL-IDSEKVNR                                             
421600       PERFORM IMS-ISRT-WLSAPA01                                          
421700     END-PERFORM                                                          
421800     .                                                                    
421900     EJECT                                                                
422000 DF-HTR-DISPDAT-INL SECTION.                                              
422100                                                                          
422200                                                                          
422300     MOVE SPACE                TO XXBW-2228-WDGX2228                      
422400                                                                          
422500     MOVE INLA-ART-IDARTNR     TO XXBW-2228-IDARTNR                       
422600     MOVE LOW-VALUE            TO XXBW-2228-LOW-VALUE                     
422700     MOVE SPACE                TO XXBW-2228-FILLER                        
422800                                                                          
422900     PERFORM IMS-ISRT-XXBW-2228                                           
423000     .                                                                    
423100     EJECT                                                                
423200 DG-SLUTUPPD-INLA SECTION.                                                
423300                                                                          
423400     PERFORM IMS-GHU-INLA-ART                                             
423500     MOVE JA                     TO INLA-ART-FLKLAR                       
423600     MOVE WS-TIAAMMDD-LOCAL      TO INLA-ART-TIUPPDAT                     
423700     PERFORM IMS-REPL-INLA                                                
423800     .                                                                    
423900     EJECT                                                                
424000 DH-LOGGA-EDI-LEV-PLANER  SECTION.                                        
424100     MOVE 'DH-LOGGA-EDI-LEV-PLANER '  TO CURRENT-SECTION                  
424200                                                                          
424300     IF DCS-CDC OR DCS-CDC-TR                                             
424400       PERFORM DHA-LOGGA-CDC-WDGX2218                                     
424500     ELSE                                                                 
424600       PERFORM DHB-LOGGA-NDC-WDGX2248                                     
424700     END-IF                                                               
424800     .                                                                    
424900     EJECT                                                                
425000 DHA-LOGGA-CDC-WDGX2218  SECTION.                                         
425100     MOVE 'DHA-LOGGA-CDC-WDGX2218 ' TO CURRENT-SECTION                    
425200                                                                          
425300     IF WS-ARTC-KDHF = 0                                                  
425400       MOVE WS-INLA-INL-IDLEVNR    TO W-IDLEVNR-2216                      
425500       PERFORM IMS-GU-WDGX2216                                            
425600       IF SEGMENT-FINNS AND 2216-KDEDI NOT = 'T'                          
425700         MOVE INLA-ART-IDARTNR     TO 2218-IDARTNR                        
425800         MOVE WS-INLA-INL-IDLEVNR  TO 2218-IDLEVNR                        
425900         MOVE WS-ARTC-IDANSK       TO 2218-IDANSK                         
426000         IF (2216-KDVECKOSL NOT = 'P')  AND                               
426100            (2218-IDLEVNR     NOT = 'BP8HB')                              
426200            PERFORM IMS-ISRT-WDGX2218                                     
426300         END-IF                                                           
426400                                                                          
426500         MOVE 2216-TISEND-PER  TO W-2216-TISEND-PER                       
426600         IF W-2216-TISEND-PER NOT NUMERIC                                 
426700            MOVE ZERO TO W-2216-TISEND-PER                                
426800         END-IF                                                           
426900                                                                          
427000         MOVE W-2216-TISEND-PER   TO TMP1-YYMMDD                          
427100         MOVE 2216-TISEND-SEN     TO TMP2-YYMMDD                          
427200         PERFORM WY2000P1                                                 
427300         IF 2216-FLLEVPLP = JA OR                                         
427400            2216-FLLEVVB  = JA OR                                         
427500           (W-2216-TISEND-PER > ZERO AND                                  
427600            TMP1-YYMMDD > TMP2-YYMMDD)                                    
427700            PERFORM IMS-ISRT-WDGX2218-PERIOD                              
427800         END-IF                                                           
427900       END-IF                                                             
428000     END-IF                                                               
428100     .                                                                    
428200     EJECT                                                                
428300 DHB-LOGGA-NDC-WDGX2248  SECTION.                                         
428400     MOVE 'DHB-LOGGA-NDC-WDGX2248 '  TO CURRENT-SECTION                   
428500                                                                          
428600     MOVE WS-INLA-INL-IDLEVNR  TO W-IDLEVNR-2206                          
428700     MOVE INLA-ART-IDDC        TO W-IDDC-2206                             
428800     PERFORM IMS-GU-WDGX2206                                              
428900     IF SEGMENT-FINNS                                                     
429000     AND (2206-KDEDI NOT = 'T')                                           
429100       MOVE INLA-ART-IDDC    TO 2248-IDDC                                 
429200       MOVE WS-INLA-INL-IDLEVNR  TO 2248-IDLEVNR                          
429300       MOVE INLA-ART-IDARTNR TO 2248-IDARTNR                              
429400       MOVE ZERO             TO 2248-KVDAGAR                              
429500                                2248-KVBEART                              
429600                                                                          
429700                                                                          
429800       IF 2206-KDVECKOSL NOT = 'P'                                        
429900         PERFORM IMS-ISRT-WDGX2248                                        
430000       END-IF                                                             
430100                                                                          
430200       IF 2206-FLLEVPLP = JA OR                                           
430300          2206-FLLEVVB  = JA                                              
430400                                                                          
430500         PERFORM IMS-ISRT-WDGX2248-PERIOD                                 
430600       END-IF                                                             
430700     END-IF                                                               
430800     .                                                                    
430900     EJECT                                                                
431000 E-DELRAPP SECTION.                                                       
431100                                                                          
431200     PERFORM EB-DELUPPD-INLE-HIST                                         
431300     PERFORM EC-DELUPPD-ARTC                                              
431400     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
431500        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
431600        PERFORM IMS-GU-WDB601                                             
431700     END-IF                                                               
431800     IF DCS-CDC-TR  AND                                                   
431900        WS-KDINLSTA-INLAGD                                                
432000         PERFORM S07-UPPDATERA-ST-INL                                     
432100     END-IF                                                               
432200     .                                                                    
432300     EJECT                                                                
432400 EB-DELUPPD-INLE-HIST SECTION.                                            
432500                                                                          
432600     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
432700        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
432800        PERFORM IMS-GU-WDB601                                             
432900     END-IF                                                               
433000     IF DCS-CDC OR DCS-CDC-TR                                             
433100       PERFORM EBA-DELUPPD-INLE-HIST                                      
433200     ELSE                                                                 
433300       PERFORM EBB-DELUPPD-INLC-HIST                                      
433400     END-IF                                                               
433500                                                                          
433600     .                                                                    
433700     EJECT                                                                
433800 EBA-DELUPPD-INLE-HIST SECTION.                                           
433900     SKIP2                                                                
434000                                                                          
434100     PERFORM IMS-GHU-INLE-MOT                                             
434200     IF SEGMENT-SAKNAS                                                    
434300        SUBTRACT 90000 FROM W-IDLOPNRM                                    
434400        PERFORM IMS-GHU-INLE-MOT                                          
434500        IF SEGMENT-FINNS                                                  
434600          ADD      90000 TO   W-IDLOPNRM                                  
434700        ELSE                                                              
434800          CALL FELLOG                                                     
434900        END-IF                                                            
435000     END-IF                                                               
435100                                                                          
435200     ADD INLA-RAD-KVINLART       TO INLE-MOT-KVANTMOT                     
435300     IF WS-KDINLSTA-FRD                                                   
435400        ADD INLA-RAD-KVINLART    TO INLE-MOT-KVFORDEL                     
435500     END-IF                                                               
435600                                                                          
435700     PERFORM IMS-REPL-INLE                                                
435800                                                                          
435900     MOVE DCS-IDDC               TO INLE-DEL-IDDC                         
436000     MOVE WS-TIAAMMDD            TO INLE-DEL-TIREGDAT                     
436100     MOVE INLA-RAD-KVINLART      TO INLE-DEL-KVRAPP                       
436200                                                                          
436300     PERFORM IMS-ISRT-INLE-DEL                                            
436400     .                                                                    
436500     EJECT                                                                
436600 EBB-DELUPPD-INLC-HIST SECTION.                                           
436700     SKIP2                                                                
436800     PERFORM IMS-GU-WDL601                                                
436900                                                                          
437000     PERFORM IMS-GHNP-WDL611                                              
437100     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
437200                   INLC-INL-IDLOPNRM = W-IDLOPNRM                         
437300       PERFORM IMS-GHNP-WDL611                                            
437400     END-PERFORM                                                          
437500                                                                          
437600     IF INLC-INL-IDLOPNRM = W-IDLOPNRM                                    
437700       ADD INLA-RAD-KVINLART       TO INLC-INL-KVANTMOT                   
437800                                                                          
437810       IF WS-INLA-RAD-IDANSTNR-BIN > 0                                    
437820          MOVE WS-INLA-RAD-IDANSTNR-BIN TO INLC-INL-IDUSER-003            
437830       END-IF                                                             
437850                                                                          
437900       PERFORM IMS-REPL-WDL611                                            
438000     END-IF                                                               
438100                                                                          
438200     MOVE DCS-IDDC               TO INLC-NDEL-IDDC                        
438300     MOVE WS-TIAAMMDD            TO INLC-NDEL-TIREGDAT                    
438400     MOVE INLA-RAD-KVINLART      TO INLC-NDEL-KVRAPP                      
438500                                                                          
438600     PERFORM IMS-ISRT-WDL621                                              
438700     .                                                                    
438800     EJECT                                                                
438900 EC-DELUPPD-ARTC SECTION.                                                 
439000                                                                          
439100     IF INLA-ART-IDDC NOT = DCS-IDDC                                      
439200        MOVE INLA-ART-IDDC   TO W-IDDC-B6                                 
439300        PERFORM IMS-GU-WDB601                                             
439400     END-IF                                                               
439500     IF DCS-CDC OR DCS-CDC-TR                                             
439600       PERFORM ECA-DELUPPD-ARTC                                           
439700     ELSE                                                                 
439800       PERFORM ECB-DELUPPD-ARTS                                           
439900     END-IF                                                               
440000                                                                          
440100     .                                                                    
440200     EJECT                                                                
440300 ECA-DELUPPD-ARTC SECTION.                                                
440400     SKIP2                                                                
440500     MOVE INLA-ART-IDARTNR     TO W-IDARTNR                               
440600                                                                          
440700     PERFORM IMS-GHU-ARTC11                                               
440800                                                                          
440900     MOVE ARTC-CLAG-KDLEVSP         TO WS-ARTC-KDLEVSP                    
441000     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
441100        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
441200        PERFORM IMS-GU-WDB601                                             
441300     END-IF                                                               
441400     IF (DCS-CDC) OR                                                      
441500        (DCS-CDC-TR AND WS-KDINLSTA-INLAGD)                               
441600       IF DCS-CDC-TR AND WS-KDINLSTA-INLAGD                               
441700          SUBTRACT INLA-RAD-KVINLART FROM ARTC-CLAG-KVAKS-T               
441800          MOVE '-' TO LOGG-IDTECKEN-KVAKS                                 
441900       ELSE                                                               
442000          SUBTRACT INLA-RAD-KVINLART FROM ARTC-CLAG-KVAKS-CDC             
442100          MOVE '-' TO LOGG-IDTECKEN-KVAKS                                 
442200          ADD INLA-RAD-KVINLART  TO ARTC-CLAG-KVLS                        
442300          MOVE '+' TO LOGG-IDTECKEN-KVLS                                  
442400          IF INLA-RAD-FLSVSLS = JA                                        
442500             ADD INLA-RAD-KVINLART                                        
442600                                  TO ARTC-CLAG-KVLS-SVS                   
442700          END-IF                                                          
442800          IF  INLA-ART-ADTRDEST(1:2) = 'CD'                               
442900            MOVE INLA-ART-ADTRDEST(3:1) TO CD-IX                          
443000            ADD INLA-RAD-KVINLART  TO ARTC-CLAG-KVLS-CD(CD-IX)            
443100          END-IF                                                          
443200                                                                          
443300       END-IF                                                             
443400       PERFORM IMS-REPL-ARTC                                              
443500       MOVE INLA-RAD-KVINLART TO LOGG-KVART-SALDO                         
443600       PERFORM S10-GRUNDDATA-LOGG                                         
443700       PERFORM S11-UPPDATERA-LOGG-WDK6                                    
443800       MOVE SPACE TO LOGG-IDTECKEN-KVLS                                   
443900                     LOGG-IDTECKEN-KVAKS                                  
444000     END-IF                                                               
444100                                                                          
444200     MOVE ARTC-CLAG-KVROS         TO WS-ARTC-ARTS-KVROS                   
444300     MOVE ARTC-CLAG-KVLS          TO WS-ARTC-ARTS-KVLS                    
444400     MOVE ARTC-CLAG-KVUTRS        TO WS-ARTC-ARTS-KVUTRS                  
444500     MOVE ARTC-CLAG-KVRESS        TO WS-ARTC-ARTS-KVRESS                  
444600     MOVE ARTC-CLAG-KVSPANT       TO WS-ARTC-ARTS-KVSPANT                 
444700     MOVE ARTC-CLAG-KVSPARR-KVAL  TO WS-ARTC-ARTS-KVSPARR-KVAL            
444800     MOVE ARTC-CLAG-PRARTSTD      TO WS-ARTC-PRARTSTD                     
444900                                                                          
445000     .                                                                    
445100     EJECT                                                                
445200 ECB-DELUPPD-ARTS SECTION.                                                
445300     SKIP2                                                                
445400     MOVE WS-INLA-INL-IDDC TO W-IDDC                                      
445500     PERFORM IMS-GHU-WDK711                                               
445600                                                                          
445700     SUBTRACT INLA-RAD-KVINLART FROM SLAG-KVAKS-SDC                       
445800     MOVE '-'                        TO LOGG-IDTECKEN-KVAKS               
445900     ADD INLA-RAD-KVINLART           TO SLAG-KVLS                         
446000     MOVE '+'                        TO LOGG-IDTECKEN-KVLS                
446100     MOVE INLA-RAD-KVINLART          TO LOGG-KVART-SALDO                  
446200                                                                          
446300                                                                          
446400     COMPUTE  WS-ARTC-ARTS-KVROS =      SLAG-KVROS-BULK +                 
446500                                        SLAG-KVROS-DAG                    
446600     MOVE      SLAG-KVAKS-SDC     TO WS-ARTC-ARTS-KVAKS-SDC               
446700     MOVE      SLAG-KVLS          TO WS-ARTC-ARTS-KVLS                    
446800     MOVE      SLAG-KVUTRS        TO WS-ARTC-ARTS-KVUTRS                  
446900     MOVE      SLAG-KVRESS        TO WS-ARTC-ARTS-KVRESS                  
447000     MOVE      +0                 TO WS-ARTC-ARTS-KVSPANT                 
447100     MOVE      SLAG-KVSPARR-KVAL  TO WS-ARTC-ARTS-KVSPARR-KVAL            
447200                                                                          
447300     PERFORM ECBA-RAEKNA-AVERAGE-COST                                     
447400                                                                          
447500     MOVE WS-ARTC-ARTS-KVAKS-SDC TO SLAG-KVAKS-SDC                        
447600     MOVE WS-ARTC-ARTS-KVLS      TO SLAG-KVLS                             
447700                                                                          
447800     PERFORM IMS-REPL-WDK711                                              
447900     PERFORM S10-GRUNDDATA-LOGG                                           
448000     PERFORM S12-UPPDATERA-LOGG-WDK7                                      
448100     MOVE SPACE TO LOGG-IDTECKEN-KVLS                                     
448200                   LOGG-IDTECKEN-KVAKS                                    
448300                                                                          
448400     IF SLAG-PRAVCOST > ZERO                                              
448500     AND (DCS-LAND-NON-VCC-OWNED OR DCS-NDC-NA)                           
448600        PERFORM S13-UPPDATERA-MATERIALPRIS                                
448700     END-IF                                                               
448800     .                                                                    
448900     EJECT                                                                
449000                                                                          
449100 ECBA-RAEKNA-AVERAGE-COST SECTION.                                        
449200     SKIP2                                                                
449300     IF DCS-NDC-NA OR DCS-LAND-NON-VCC-OWNED                              
449400**** HÄR LÄSER MAN WDK7 IGEN                                              
449500       PERFORM DEFBAA-GET-PRARTBES                                        
449600       PERFORM DDAB-GET-CURRENCY-RATE                                     
449700       IF INLA-RAD-KVINLART < ZERO                                        
449800         COMPUTE INLA-RAD-KVINLART = INLA-RAD-KVINLART * -1               
449900         IF DCS-NDC-NA                                                    
450000           MOVE 080              TO AVG-KDCALL                            
450100         ELSE                                                             
450200           IF DCS-INDIA                                                   
450300             MOVE 082              TO AVG-KDCALL                          
450400           ELSE                                                           
450500             MOVE 081              TO AVG-KDCALL                          
450600           END-IF                                                         
450700         END-IF                                                           
450800****   KINAS AVERAGE COST SKALL TA HÄNSYN TILL EFR                        
450900         COMPUTE AVG-KVLS-OLD = SLAG-KVLS + SLAG-KVEFRS -                 
451000                                INLA-RAD-KVINLART                         
451100         MOVE W-RETULF           TO AVG-REMARKUP                          
451200                                                                          
451300         MOVE WS-INLA-INL-TIAVIDAT  TO WS-DAAVIDAT-YYMMDD                 
451400         MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                 
451500         MOVE 01                      TO W-DATE-AAMM(3:2)                 
451600         MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                  
451700         MOVE DCS-KDVALISO          TO CURR-KDVALISO-ROW                  
451800         MOVE W-DATE-AAMM           TO CURR-TIAAMM                        
451900         MOVE 'A'                   TO CURR-KDVALTYP                      
452000         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
452100         IF CURR-KDSVAR = ' '                                             
452200           MOVE CURR-PRKURS-NEW     TO WS-PRKURS                          
452300           MOVE CURR-REVALUTA-TO    TO WS-REVALUTA                        
452400         ELSE                                                             
452500           MOVE 1                   TO WS-PRKURS                          
452600           MOVE 1                   TO WS-REVALUTA                        
452700         END-IF                                                           
452800         PERFORM IMS-GU-WDB601                                            
452900         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
453000         IF SEGMENT-FINNS                                                 
453100           PERFORM IMS-GNP-WDB617                                         
453200           IF SEGMENT-FINNS                                               
453300             COMPUTE WS-PRARTKALKYL      ROUNDED =                        
453400                    (WS-ARTC-PRDIRLON *                                   
453500                     PROC-REDIRLON * WS-REVALUTA / WS-PRKURS) +           
453600                    (WS-ARTC-PRDMTRL *                                    
453700                     PROC-REDMTRL  * WS-REVALUTA / WS-PRKURS)             
453800           ELSE                                                           
453900             MOVE ZERO TO WS-PRARTKALKYL                                  
454000           END-IF                                                         
454100         END-IF                                                           
454200         MOVE WS-ARTC21-PRARTBEL-PR TO AVG-PRARTBEL                       
454300         MOVE INLA-RAD-KVINLART  TO AVG-KVANTMOT                          
454400         MOVE SLAG-PRAVCOST      TO AVG-PRAVCOST-OLD                      
454500         MOVE WS-PRARTKALKYL     TO AVG-PRARTNTO                          
454600         MOVE SPAR-PRKURS        TO AVG-PRKURS                            
454700         MOVE WS-ARTC21-KDVALISO TO AVG-KDVALISO                          
454800         MOVE +0                 TO AVG-KVLEVART                          
454900         MOVE WS-ARTC-KDPSLLOC   TO AVG-KDPSLLOC                          
455000         MOVE WS-ARTC-KDPRODSL   TO AVG-KDPRODSL                          
455100         MOVE WS-ARTC-IDFKNGRP   TO AVG-IDFKNGRP                          
455200         MOVE W-IDDC             TO AVG-IDDC                              
455300         MOVE +0                 TO AVG-PRAVCOST-NEW                      
455400         MOVE SPACE              TO AVG-KDSVAR                            
455500         MOVE WS-DAAVIDAT-YYMMDD(1:2) TO AVG-TIAA                         
455600         MOVE WS-DAAVIDAT-YYMMDD(3:2) TO AVG-TIMM                         
455700                                                                          
455800         CALL W510AVG USING AVG-W510AVG 9305-AVG-PCB                      
455900                            AVG-WDB6-PCB                                  
456000         IF AVG-KDSVAR = SPACE                                            
456100           MOVE AVG-PRAVCOST-NEW TO WS-PRAVCOST                           
456200         ELSE                                                             
456300           IF AVG-KDSVAR = '4'                                            
456400             MOVE SLAG-PRAVCOST  TO WS-PRAVCOST                           
456500           ELSE                                                           
456600             STRING 'FEL FRÅN W510AVG ' AVG-KDSVAR                        
456700              DELIMITED BY SIZE INTO FELTEXT                              
456800               CALL FELLOG                                                
456900           END-IF                                                         
457000         END-IF                                                           
457100       ELSE                                                               
457200         IF DCS-NDC-NA                                                    
457300           MOVE 040              TO AVG-KDCALL                            
457400         ELSE                                                             
457500           IF DCS-INDIA                                                   
457600             MOVE 042              TO AVG-KDCALL                          
457700           ELSE                                                           
457800            IF DCS-CHINA                                                  
457900             MOVE 041              TO AVG-KDCALL                          
458000            ELSE                                                          
458100             MOVE 041              TO AVG-KDCALL                          
458200            END-IF                                                        
458300           END-IF                                                         
458400         END-IF                                                           
458500****   KINAS AVERAGE COST SKALL TA HÄNSYN TILL EFR                        
458600         COMPUTE AVG-KVLS-OLD = SLAG-KVLS + SLAG-KVEFRS                   
458700         MOVE W-RETULF           TO AVG-REMARKUP                          
458800                                                                          
458900         MOVE WS-INLA-INL-TIAVIDAT  TO WS-DAAVIDAT-YYMMDD                 
459000         MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                 
459100         MOVE 01                      TO W-DATE-AAMM(3:2)                 
459200         MOVE WS-KDVALISO-HUV    TO CURR-KDVALISO-HUV                     
459300         MOVE DCS-KDVALISO       TO CURR-KDVALISO-ROW                     
459400         MOVE W-DATE-AAMM        TO CURR-TIAAMM                           
459500         MOVE 'A'                TO CURR-KDVALTYP                         
459600         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
459700         IF CURR-KDSVAR = ' '                                             
459800           MOVE CURR-PRKURS-NEW  TO WS-PRKURS                             
459900           MOVE CURR-REVALUTA-TO TO WS-REVALUTA                           
460000         ELSE                                                             
460100           MOVE 1                TO WS-PRKURS                             
460200           MOVE 1                TO WS-REVALUTA                           
460300         END-IF                                                           
460400         PERFORM IMS-GU-WDB601                                            
460500         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
460600         IF SEGMENT-FINNS                                                 
460700           PERFORM IMS-GNP-WDB617                                         
460800           IF SEGMENT-FINNS                                               
460900             COMPUTE WS-PRARTKALKYL      ROUNDED =                        
461000                    (WS-ARTC-PRDIRLON *                                   
461100                     PROC-REDIRLON * WS-REVALUTA / WS-PRKURS) +           
461200                    (WS-ARTC-PRDMTRL *                                    
461300                     PROC-REDMTRL  * WS-REVALUTA / WS-PRKURS)             
461400           ELSE                                                           
461500             MOVE ZERO TO WS-PRARTKALKYL                                  
461600           END-IF                                                         
461700         END-IF                                                           
461800         MOVE WS-ARTC21-PRARTBEL-PR TO AVG-PRARTBEL                       
461900         MOVE INLA-RAD-KVINLART  TO AVG-KVANTMOT                          
462000         MOVE SLAG-PRAVCOST      TO AVG-PRAVCOST-OLD                      
462100         MOVE WS-PRARTKALKYL     TO AVG-PRARTNTO                          
462200         MOVE +0                 TO AVG-PRKURS                            
462300         MOVE WS-ARTC21-KDVALISO TO AVG-KDVALISO                          
462400         MOVE +0                 TO AVG-KVLEVART                          
462500         MOVE WS-ARTC-KDPSLLOC   TO AVG-KDPSLLOC                          
462600         MOVE WS-ARTC-KDPRODSL   TO AVG-KDPRODSL                          
462700         MOVE WS-ARTC-IDFKNGRP   TO AVG-IDFKNGRP                          
462800         MOVE W-IDDC             TO AVG-IDDC                              
462900         MOVE +0                 TO AVG-PRAVCOST-NEW                      
463000         MOVE SPACE              TO AVG-KDSVAR                            
463100         MOVE WS-DAAVIDAT-YYMMDD(1:2) TO AVG-TIAA                         
463200         MOVE WS-DAAVIDAT-YYMMDD(3:2) TO AVG-TIMM                         
463300                                                                          
463400         CALL W510AVG USING AVG-W510AVG 9305-AVG-PCB                      
463500                            AVG-WDB6-PCB                                  
463600         IF AVG-KDSVAR = SPACE                                            
463700           MOVE AVG-PRAVCOST-NEW TO WS-PRAVCOST                           
463800         ELSE                                                             
463900           IF AVG-KDSVAR = '4'                                            
464000             MOVE SLAG-PRAVCOST  TO WS-PRAVCOST                           
464100           ELSE                                                           
464200             STRING 'FEL FRÅN W510AVG ' AVG-KDSVAR                        
464300              DELIMITED BY SIZE INTO FELTEXT                              
464400               CALL FELLOG                                                
464500           END-IF                                                         
464600         END-IF                                                           
464700       END-IF                                                             
464800                                                                          
464900       MOVE WS-PRAVCOST        TO SLAG-PRAVCOST                           
465000       MOVE WS-TIAAMMDD-LOCAL  TO SLAG-TIAVCOST                           
465100     END-IF                                                               
465200     .                                                                    
465300     EJECT                                                                
465400                                                                          
465500 F-EV-RO-TACKNING SECTION.                                                
465600                                                                          
465700     MOVE SPACE                  TO 4506-WDGX4506                         
465800                                                                          
465900     IF  WS-ARTC-ARTS-KVROS > 0                                           
466000*NDC                                                                      
466100       IF INLA-ART-IDDC NOT = DCS-IDDC                                    
466200          MOVE INLA-ART-IDDC   TO W-IDDC-B6                               
466300          PERFORM IMS-GU-WDB601                                           
466400       END-IF                                                             
466500       IF  ((DCS-CDC OR DCS-CDC-TR)                                       
466600       AND  WS-ARTC-KDLEVSP = ZERO)                                       
466700       OR   ((DCS-NDC-NA OR DCS-LAND-NON-VCC-OWNED                        
466800                         OR DCS-AUSTRALIA OR DCS-JAPAN)                   
466900       AND   WS-ARTC-KDLEVSP = ZERO                                       
467000       AND   WS-ARTS-KDLEVSP = ZERO)                                      
467100                                                                          
467200         IF WS-ARTC-PRARTSTD > 0                                          
467300           MOVE ZERO             TO WS-KVDISP                             
467400           COMPUTE WS-KVDISP     = WS-ARTC-ARTS-KVLS                      
467500                                 - WS-ARTC-ARTS-KVUTRS                    
467600                                 - WS-ARTC-ARTS-KVRESS                    
467700                                 - WS-ARTC-ARTS-KVSPANT                   
467800*NDC                                                                      
467900                                 - WS-ARTC-ARTS-KVSPARR-KVAL              
468000                                                                          
468100           IF  WS-KVDISP > 0                                              
468200             MOVE INLA-ART-IDARTNR TO 4506-IDARTNR                        
468300             MOVE +1               TO 4506-KDTAKORS                       
468400                                                                          
468500             IF  (NOT KVALITET-MID)                                       
468600             AND MID-IDRADNR = ZERO                                       
468700               MOVE WS-TOT-KVINLART TO 4506-KVANTMOT                      
468800                                                                          
468900             ELSE                                                         
469000               IF  WS-KDINLSTA-INLAGD                                     
469100                 MOVE INLA-RAD-KVINLART TO 4506-KVANTMOT                  
469200               ELSE                                                       
469300                 MOVE ZERO              TO 4506-KVANTMOT                  
469400               END-IF                                                     
469500               IF  SW-RAPP-OFULLST = NEJ                                  
469600*                -- EV BIDRAG FRÅN AVVIKELSE PÅVERKAR                     
469700                 ADD WS-AVVIK-KVLS      TO 4506-KVANTMOT                  
469800               END-IF                                                     
469900             END-IF                                                       
470000                                                                          
470100             IF  4506-KVANTMOT > ZERO                                     
470200               MOVE INLA-ART-IDDC      TO 4505-IDDC                       
470300               PERFORM IMS-ISRT-4506                                      
470400             END-IF                                                       
470500           END-IF                                                         
470600         END-IF                                                           
470700       END-IF                                                             
470800     END-IF                                                               
470900     .                                                                    
471000     EJECT                                                                
471100 Z-FINIT SECTION.                                                         
471200                                                                          
471300     MOVE INF-UPDATE-DONE        TO MSG-KOM-IDMFSMED                      
471400                                                                          
471500     PERFORM IMS-ISRT-DISP-MSG                                            
471600     .                                                                    
471700     EJECT                                                                
471800 S01-KOLLA-PALAGG SECTION.                                                
471900** OMKOSTNADSPÅLÄGG LOGGAS PÅ WDR9 (WDR8 FÖR KINA)                        
472000     IF WS-ARTC-PRINK NOT = WS-ARTC-PRARTSTD AND                          
472100       (INLA-ART-KDRT NOT = 3 AND 7 AND 8 AND 77)                         
472200       IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                               
472300         CONTINUE                                                         
472400**** KALKYLPÅLÄGG GÖRS FÖR KINA I 103-102 HÄNDELSEN                       
472500*        PERFORM IMS-GU-WDB601                                            
472600*        MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
472700*        IF SEGMENT-FINNS                                                 
472800*          PERFORM IMS-GNP-WDB617                                         
472900*          IF SEGMENT-FINNS                                               
473000*            PERFORM DEHB-PALAGG-WDR8                                     
473100*          END-IF                                                         
473200*        END-IF                                                           
473300       ELSE                                                               
473400         PERFORM DEHA-PALAGG-WDR9                                         
473500       END-IF                                                             
473600     END-IF                                                               
473700     .                                                                    
473800     EJECT                                                                
473900 S02-SKAPA-ZZAC01 SECTION.                                                
474000                                                                          
474100     ACCEPT ZZAC01-TIAAMMDD      FROM DATE                                
474200     ACCEPT ZZAC01-TIKLOCK       FROM TIME                                
474300                                                                          
474400     ADD +1                      TO WS-IDLOGLOP                           
474500     MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                       
474600                                                                          
474700     MOVE WS-ZZAC01-LOGGPOST     TO ZZAC01-LOGGPOST                       
474800     MOVE WS-ZZAC01-SORTPOST     TO ZZAC01-SORTPOST                       
474900                                                                          
475000     PERFORM IMS-ISRT-ZZAC01                                              
475100     PERFORM UNTIL SEGMENT-FINNS                                          
475200       ACCEPT ZZAC01-TIAAMMDD      FROM DATE                              
475300       ACCEPT ZZAC01-TIKLOCK       FROM TIME                              
475400                                                                          
475500       ADD +1                      TO WS-IDLOGLOP                         
475600       PERFORM IMS-ISRT-ZZAC01                                            
475700     END-PERFORM                                                          
475800     .                                                                    
475900     EJECT                                                                
476000 S03-RED-W211FEL-GNRL SECTION.                                            
476100                                                                          
476200     MOVE ZERO                   TO W211FEL-SORT-FLT                      
476300     MOVE SPACE                  TO W211FEL-FILLER2                       
476400                                                                          
476500     MOVE 'R32'                  TO W211FEL-IDPTYP-S                      
476600     MOVE W-KDCLAGER             TO W211FEL-KDCLAGER-S                    
476700     MOVE INLA-ART-IDARTNR       TO W211FEL-SORTBGP                       
476800     MOVE 1                      TO W211FEL-KDFELMRK                      
476900     .                                                                    
477000     EJECT                                                                
477100 S04-KONVERTERA-IDFS-IDAVINR SECTION.                                     
477200                                                                          
477300     MOVE +8                     TO IX-IDFS                               
477400     MOVE +7                     TO IX-IDAVINR                            
477500                                                                          
477600     PERFORM UNTIL IX-IDFS    = ZERO OR                                   
477700                   IX-IDAVINR = ZERO                                      
477800       IF WS-IDFS-TKN (IX-IDFS) NUMERIC                                   
477900         MOVE WS-IDFS-TKN (IX-IDFS) TO WS-IDAVINR-TKN (IX-IDAVINR)        
478000         SUBTRACT 1               FROM IX-IDAVINR                         
478100       END-IF                                                             
478200       SUBTRACT 1                 FROM IX-IDFS                            
478300     END-PERFORM                                                          
478400     .                                                                    
478500     EJECT                                                                
478600 S05-RAKNA-FRAM-RAPPORTERAT SECTION.                                      
478700                                                                          
478800                                                                          
478900     IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                   
479000        MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                              
479100        PERFORM IMS-GU-WDB601                                             
479200     END-IF                                                               
479300     IF DCS-CDC OR DCS-CDC-TR                                             
479400       PERFORM IMS-GU-WLINLE21                                            
479500       IF SEGMENT-SAKNAS                                                  
479600          SUBTRACT 90000 FROM W-IDLOPNRM                                  
479700          PERFORM IMS-GU-WLINLE21                                         
479800          IF SEGMENT-FINNS                                                
479900            ADD      90000 TO   W-IDLOPNRM                                
480000          ELSE                                                            
480100            CALL FELLOG                                                   
480200          END-IF                                                          
480300       END-IF                                                             
480400                                                                          
480500       MOVE INLE-MOT-KVANTMOT  TO WS-INLE-KVANTMOT-INNAN                  
480600     ELSE                                                                 
480700       PERFORM IMS-GU-WDL601                                              
480800       IF SEGMENT-FINNS                                                   
480900           PERFORM IMS-GNP-WDL611                                         
481000           IF SEGMENT-FINNS                                               
481100             PERFORM UNTIL SEGMENT-SAKNAS OR                              
481200                    INLC-INL-IDLOPNRM = MID-IDLOPNRM                      
481300                 PERFORM IMS-GNP-WDL611                                   
481400                                                                          
481500             END-PERFORM                                                  
481600           END-IF                                                         
481700           IF SEGMENT-FINNS                                               
481800             IF INLC-INL-IDLOPNRM = MID-IDLOPNRM                          
481900                MOVE INLC-INL-KVANTMOT TO WS-INLE-KVANTMOT-INNAN          
482000             END-IF                                                       
482100           END-IF                                                         
482200       END-IF                                                             
482300     END-IF                                                               
482400     .                                                                    
482500     EJECT                                                                
482600 S07-UPPDATERA-ST-INL       SECTION.                                      
482700                                                                          
482800     MOVE WC-SDC-NL-ET           TO W-IDDC                                
482900     PERFORM IMS-GHU-WDK711                                               
483000     IF SEGMENT-FINNS                                                     
483100       MOVE SLAG-ADLAGOMR          TO WS-ADLAGOMR                         
483200       MOVE SLAG-ADGANG            TO WS-ADGANG                           
483300       MOVE SLAG-ADPLATS           TO WS-ADPLATS                          
483400       ADD INLA-RAD-KVINLART       TO SLAG-KVLS                           
483500       PERFORM IMS-REPL-WDK711                                            
483600       MOVE '+' TO LOGG-IDTECKEN-KVLS                                     
483700       MOVE INLA-RAD-KVINLART TO LOGG-KVART-SALDO                         
483800       PERFORM S10-GRUNDDATA-LOGG                                         
483900       PERFORM S12-UPPDATERA-LOGG-WDK7                                    
484000       MOVE SPACE TO LOGG-IDTECKEN-KVLS                                   
484100                     LOGG-IDTECKEN-KVAKS                                  
484200     ELSE                                                                 
484300       PERFORM S07A-SKAPA-WDK7                                            
484400     END-IF                                                               
484500                                                                          
484600     PERFORM S07B-SKAPA-INLC                                              
484700                                                                          
484800** UPPDATERING AV WDR9 (SAPA)                                             
484900     MOVE 'W6019300'                  TO FIL-IDPGM                        
485000     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
485100                                         EKH-DAVERDAT                     
485200     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
485300     MOVE +1                          TO FIL-IDSEKVNR                     
485400     MOVE '502'                       TO EKH-KDEKHHT                      
485500     MOVE '504'                       TO EKH-KDEKSHT                      
485600     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
485700     MOVE WC-CDC-TR                   TO EKH-IDDC-SEND                    
485800     MOVE WC-SDC-NL-ET                TO EKH-IDDC-REC                     
485900     MOVE +0                          TO EKH-IDDISTR                      
486000                                         EKH-IDKUNDNR                     
486100*******************************                                           
486200*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
486300       MOVE ZERO TO NOLL-RAKNARE                                          
486400       MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                
486500       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
486600       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
486700            FOR LEADING ZERO                                              
486800       ADD +1 TO NOLL-RAKNARE                                             
486900       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                     
487000          WITH POINTER NOLL-RAKNARE                                       
487100*******************************                                           
487200     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
487300     MOVE ZERO                        TO EKH-KDPSLLOC                     
487400                                         EKH-PRARTNTO                     
487500                                         EKH-PRARTSJK                     
487600                                         EKH-PRHEMTAG                     
487700                                         EKH-PRINK                        
487800                                         EKH-PRLANDCO                     
487900                                         EKH-PRDIRLON                     
488000                                         EKH-PRDMTRL                      
488100                                         EKH-PROVRPAL                     
488200                                         EKH-SUBEL                        
488300                                         EKH-IDORDNR5                     
488400     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
488500     MOVE SPACE                       TO EKH-FLLSBOK                      
488600     MOVE 'SEK'                       TO EKH-KDVALISO                     
488700     MOVE 1.00                        TO EKH-PRKURS                       
488800     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
488900                                                                          
489000     MOVE INLA-RAD-KVINLART           TO EKH-KVANTAL                      
489100     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
489200     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
489300     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
489400     MOVE ZERO                        TO EKH-IDKONTO                      
489500                                         EKH-SUVAT                        
489600                                         EKH-KDFRAKT                      
489700     MOVE SPACE                       TO EKH-BEVAT                        
489800                                         EKH-IDKST                        
489900                                         EKH-IDANALYS                     
490000                                         EKH-KDANMORS                     
490100                                                                          
490200     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
490300     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
490400       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
490500     ELSE                                                                 
490600       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
490700     END-IF                                                               
490800     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
490900     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
491000     MOVE WS-INLA-INL-IDLEVNR         TO EKH-IDLEVNR                      
491100     IF INLA-ART-KVAVIS > ZERO                                            
491200       MOVE 1                         TO EKH-KDAVVTYP                     
491300     ELSE                                                                 
491400       MOVE 0                         TO EKH-KDAVVTYP                     
491500     END-IF                                                               
491600     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
491700     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
491800     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
491900     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
492000     MOVE SPACE                       TO EKH-KDTRADP                      
492100     MOVE SPACE                       TO EKH-FLDCET                       
492200     MOVE SPACE                       TO EKH-IDKUNDRF                     
492300     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
492400                                                                          
492500     PERFORM IMS-ISRT-WLSAPA01                                            
492600                                                                          
492700     PERFORM UNTIL SEGMENT-FINNS                                          
492800       ADD +1 TO FIL-IDSEKVNR                                             
492900       PERFORM IMS-ISRT-WLSAPA01                                          
493000     END-PERFORM                                                          
493100     .                                                                    
493200     EJECT                                                                
493300 S07A-SKAPA-WDK7        SECTION.                                          
493400                                                                          
493500     MOVE ALL '+'      TO WDK7-W005WDK7                                   
493600     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
493700     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
493800     MOVE WC-SDC-NL-ET TO WDK7-IDDC-KFB                                   
493900                          WDK7-IDDC                                       
494000     MOVE INLA-RAD-KVINLART TO WDK7-KVLS                                  
494100                                                                          
494200     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB WDK7I-PCB        
494300     MOVE WDK7-WDK711  TO SLAG-WDK711                                     
494400                                                                          
494500     MOVE INLA-RAD-KVINLART TO LOGG-KVART-SALDO                           
494600     MOVE '+' TO LOGG-IDTECKEN-KVLS                                       
494700     PERFORM S10-GRUNDDATA-LOGG                                           
494800     PERFORM S12-UPPDATERA-LOGG-WDK7                                      
494900     MOVE SPACE TO LOGG-IDTECKEN-KVLS                                     
495000                   LOGG-IDTECKEN-KVAKS                                    
495100     .                                                                    
495200     EJECT                                                                
495300 S07B-SKAPA-INLC        SECTION.                                          
495400                                                                          
495500     PERFORM IMS-GU-WDL601                                                
495600     IF SEGMENT-SAKNAS                                                    
495700       MOVE W-IDARTNR     TO INLC-ART-IDARTNR                             
495800       PERFORM IMS-ISRT-WDL601                                            
495900     END-IF                                                               
496000                                                                          
496100     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
496200     ACCEPT DAGENS-TID   FROM TIME                                        
496300     COMPUTE INLC-INL-DAINLEV = 9999999999999999 - WS-DAINLEV             
496400                                                                          
496500     MOVE WC-SDC-NL-ET      TO INLC-INL-IDDC                              
496600     MOVE '1441 '           TO INLC-INL-IDLEVNR                           
496700     MOVE INLA-RAD-KVINLART TO INLC-INL-KVANTMOT                          
496800     MOVE WS-ADLAGOMR       TO INLC-INL-ADLAGOMR                          
496900     MOVE WS-ADGANG         TO INLC-INL-ADGANG                            
497000     MOVE WS-ADPLATS        TO INLC-INL-ADPLATS                           
497100     MOVE +0                TO INLC-INL-IDFAKT                            
497200                               INLC-INL-IDDISTR                           
497300                               INLC-INL-IDKUNDNR                          
497400                               INLC-INL-IDKOLLI                           
497500                               INLC-INL-KVAVIS                            
497600                               INLC-INL-IDLOPNRM                          
497700                               INLC-INL-KDFRAKT                           
497800                               INLC-INL-KDRT                              
497900                               INLC-INL-KVART-SKROT                       
498000                               INLC-INL-PRARTNTO                          
498100                               INLC-INL-PRKURS                            
498200                               INLC-INL-TIBERANK                          
498300                               INLC-INL-KVTULRET                          
498400                               INLC-INL-KVRETUR                           
498500                               INLC-INL-KDAVVANT                          
498600     MOVE WS-INLA-INL-IDFS  TO INLC-INL-IDKUNDRF                          
498700     MOVE SPACE             TO INLC-INL-KDKOLLI                           
498800                               INLC-INL-KDVALISO                          
498900     MOVE 'R32'             TO INLC-INL-IDPTYP                            
499000*    MOVE DAGENS-DATUM      TO INLC-INL-TIINLMOT                          
499100     MOVE WS-TIAAMMDD-LOCAL TO INLC-INL-TIINLMOT                          
499200                               INLC-INL-TIINLINL                          
499300     MOVE 'N'               TO INLC-INL-FLPRIO                            
499400                               INLC-INL-FLMAKUL                           
499500                               INLC-INL-FLTULLST                          
499600                               INLC-INL-FLSKAKOL                          
499700     MOVE +0                TO INLC-INL-TIINLMTI                          
499800                               INLC-INL-TIINLITI                          
499900     MOVE SPACE                TO INLC-INL-ADINLOMR                       
500000                                  INLC-INL-IDUSER-003                     
500100                                  INLC-INL-IDDC-LEV                       
500200     MOVE WS-INLA-INL-IDANALYS TO INLC-INL-IDANALYS                       
500300     MOVE WS-INLA-INL-IDKONTO  TO INLC-INL-IDKONTO                        
500400     MOVE WS-INLA-INL-IDKST    TO INLC-INL-IDKST                          
500500     MOVE WS-INLA-INL-TIAVIDAT TO INLC-INL-TIAVIDAT                       
500600                                                                          
500700     PERFORM IMS-ISRT-WDL611                                              
500800     .                                                                    
500900     EJECT                                                                
501000 S08-SKAPA-EK-TRANS     SECTION.                                          
501100                                                                          
501200     IF WS-INLA-INL-IDLEVNR = '3324 '                                     
501300       MOVE WC-CDC-TR      TO W-IDDC                                      
501400     ELSE                                                                 
501500       PERFORM IMS-GN-WDB601                                              
501600       PERFORM UNTIL BASEN-SLUT                                           
501700                  OR SEGMENT-SAKNAS                                       
501800                  OR NEXT-DCS-IDLEVNR-DC = WS-INLA-INL-IDLEVNR            
501900          PERFORM IMS-GN-WDB601                                           
502000       END-PERFORM                                                        
502100     END-IF                                                               
502200     IF BASEN-SLUT OR SEGMENT-SAKNAS                                      
502300        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
502400     ELSE                                                                 
502500        MOVE NEXT-DCS-IDDC TO W-IDDC                                      
502600     END-IF                                                               
502700     IF WS-INLA-INL-IDLEVNR = '3324 '                                     
502800       CONTINUE                                                           
502900     ELSE                                                                 
503000       PERFORM IMS-GHU-WDK711                                             
503100       COMPUTE SLAG-KVLS = SLAG-KVLS + (WS-KVDIFF-MOT-AVIS * -1)          
503200       COMPUTE LOGG-KVART-SALDO = WS-KVDIFF-MOT-AVIS * -1                 
503300       PERFORM IMS-REPL-WDK711                                            
503400                                                                          
503500       MOVE '+' TO LOGG-IDTECKEN-KVLS                                     
503600       PERFORM S10-GRUNDDATA-LOGG                                         
503700       MOVE 'MISC' TO LOGG-IDHUVTYP                                       
503800       MOVE 'R34'  TO LOGG-IDSUBTYP                                       
503900       PERFORM S12-UPPDATERA-LOGG-WDK7                                    
504000       MOVE SPACE TO LOGG-IDTECKEN-KVLS                                   
504100                     LOGG-IDTECKEN-KVAKS                                  
504200       PERFORM S08A-SKAPA-INLC                                            
504300     END-IF                                                               
504400                                                                          
504500     IF DCS-LAND-NON-VCC-OWNED                                            
504600     OR DCS-USA                                                           
504700       MOVE 'W6019300'                  TO EKO-FIL-IDPGM                  
504800       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
504900       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
505000       MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT               
505100       MOVE +1                          TO EKO-FIL-IDSEKVNR               
505200       EVALUATE TRUE                                                      
505300         WHEN DCS-CHINA                                                   
505400           MOVE 'W570'                  TO EKO-FIL-IDCPYTXT(1:4)          
505500         WHEN DCS-INDIA                                                   
505600           MOVE 'W515'                  TO EKO-FIL-IDCPYTXT(1:4)          
505700         WHEN DCS-USA                                                     
505800           MOVE 'W561'                  TO EKO-FIL-IDCPYTXT(1:4)          
505900         WHEN OTHER                                                       
506000           MOVE DCS-KDTRADP             TO EKO-FIL-IDCPYTXT(1:4)          
506100       END-EVALUATE                                                       
506200       MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)          
506300       PERFORM S08B-SKAPA-EK-TRANS                                        
506400     ELSE                                                                 
506500       MOVE 'W6019300'                  TO FIL-IDPGM                      
506600       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
506700                                           EKH-DAVERDAT                   
506800       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
506900       MOVE +1                          TO FIL-IDSEKVNR                   
507000       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
507100       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
507200       PERFORM S08C-SKAPA-EK-TRANS                                        
507300     END-IF                                                               
507400     .                                                                    
507500     EJECT                                                                
507600 S08A-SKAPA-INLC        SECTION.                                          
507700                                                                          
507800     PERFORM IMS-GU-WDL601                                                
507900     IF SEGMENT-SAKNAS                                                    
508000       MOVE W-IDARTNR     TO INLC-ART-IDARTNR                             
508100       PERFORM IMS-ISRT-WDL601                                            
508200     END-IF                                                               
508300                                                                          
508400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
508500     ACCEPT DAGENS-TID   FROM TIME                                        
508600     COMPUTE INLC-INL-DAINLEV = 9999999999999999 - WS-DAINLEV             
508700                                                                          
508800     MOVE W-IDDC            TO INLC-INL-IDDC                              
508900     MOVE '1441 '           TO INLC-INL-IDLEVNR                           
509000     COMPUTE INLC-INL-KVANTMOT = WS-KVDIFF-MOT-AVIS * -1                  
509100     MOVE +0                TO INLC-INL-ADLAGOMR                          
509200                               INLC-INL-ADGANG                            
509300                               INLC-INL-ADPLATS                           
509400     MOVE WS-IDAVINR        TO INLC-INL-IDFAKT                            
509500     MOVE +0                TO INLC-INL-IDDISTR                           
509600                               INLC-INL-IDKUNDNR                          
509700                               INLC-INL-IDKOLLI                           
509800                               INLC-INL-KVAVIS                            
509900                               INLC-INL-IDLOPNRM                          
510000                               INLC-INL-KDFRAKT                           
510100                               INLC-INL-KDRT                              
510200                               INLC-INL-KVART-SKROT                       
510300                               INLC-INL-PRARTNTO                          
510400                               INLC-INL-PRKURS                            
510500                               INLC-INL-TIBERANK                          
510600     MOVE WS-INLA-INL-IDFS  TO INLC-INL-IDKUNDRF                          
510700     MOVE SPACE             TO INLC-INL-KDKOLLI                           
510800                               INLC-INL-KDVALISO                          
510900     MOVE 'R34'             TO INLC-INL-IDPTYP                            
511000*    MOVE DAGENS-DATUM      TO INLC-INL-TIINLMOT                          
511100     MOVE WS-TIAAMMDD-LOCAL TO INLC-INL-TIINLMOT                          
511200                               INLC-INL-TIINLINL                          
511300     MOVE 'N'               TO INLC-INL-FLPRIO                            
511400                               INLC-INL-FLMAKUL                           
511500                               INLC-INL-FLSKAKOL                          
511600                               INLC-INL-FLTULLST                          
511700     MOVE +0                TO INLC-INL-TIINLMTI                          
511800                               INLC-INL-TIINLITI                          
511900                               INLC-INL-TIAVIDAT                          
512000                               INLC-INL-KVTULRET                          
512100                               INLC-INL-KVRETUR                           
512200                               INLC-INL-KDAVVANT                          
512300     MOVE SPACE                TO INLC-INL-ADINLOMR                       
512400                                  INLC-INL-IDUSER-003                     
512500                                  INLC-INL-IDDC-LEV                       
512600     MOVE WS-INLA-INL-IDANALYS TO INLC-INL-IDANALYS                       
512700     MOVE WS-INLA-INL-IDKONTO  TO INLC-INL-IDKONTO                        
512800     MOVE WS-INLA-INL-IDKST    TO INLC-INL-IDKST                          
512900                                                                          
513000     PERFORM IMS-ISRT-WDL611                                              
513100     .                                                                    
513200     EJECT                                                                
513300                                                                          
513400 S08B-SKAPA-EK-TRANS     SECTION.                                         
513500     IF WS-KVDIFF-MOT-AVIS > +0                                           
513600       MOVE WS-KVDIFF-MOT-AVIS TO EKO-EKH-KVANTAL                         
513700       MOVE '502'              TO EKO-EKH-KDEKHHT                         
513800       MOVE '502'              TO EKO-EKH-KDEKSHT                         
513900     ELSE                                                                 
514000       MOVE WS-KVDIFF-MOT-AVIS TO EKO-EKH-KVANTAL                         
514100       MOVE '502'              TO EKO-EKH-KDEKHHT                         
514200       MOVE '503'              TO EKO-EKH-KDEKSHT                         
514300     END-IF                                                               
514400     MOVE NEXT-DCS-IDDC        TO EKO-EKH-IDDC-SEND                       
514500                                                                          
514600     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
514700     MOVE WS-INLA-INL-IDDC            TO EKO-EKH-IDDC-REC                 
514800     MOVE +0                          TO EKO-EKH-IDDISTR                  
514900                                         EKO-EKH-IDKUNDNR                 
515000*******************************                                           
515100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
515200       MOVE ZERO TO NOLL-RAKNARE                                          
515300       MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                
515400       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
515500       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
515600            FOR LEADING ZERO                                              
515700       ADD +1 TO NOLL-RAKNARE                                             
515800       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                 
515900          WITH POINTER NOLL-RAKNARE                                       
516000*******************************                                           
516100     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
516200     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
516300                                         EKO-EKH-PRARTNTO                 
516400                                         EKO-EKH-PRARTSJK                 
516500                                         EKO-EKH-PRHEMTAG                 
516600                                         EKO-EKH-PRINK                    
516700                                         EKO-EKH-PRLANDCO                 
516800                                         EKO-EKH-PRDIRLON                 
516900                                         EKO-EKH-PRDMTRL                  
517000                                         EKO-EKH-PROVRPAL                 
517100                                         EKO-EKH-SUBEL                    
517200                                         EKO-EKH-IDORDNR5                 
517300     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
517400     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
517500     MOVE 1.00                        TO EKO-EKH-PRKURS                   
517600     MOVE SLAG-PRAVCOST               TO EKO-EKH-PRARTSTD                 
517700     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
517800     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
517900                                         EKO-EKH-SUVAT                    
518000                                         EKO-EKH-KDFRAKT                  
518100     MOVE SPACE                       TO EKO-EKH-BEVAT                    
518200                                         EKO-EKH-IDKST                    
518300                                         EKO-EKH-KDANMORS                 
518400                                         EKO-EKH-IDANALYS                 
518500                                                                          
518600     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
518700     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
518800       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
518900     ELSE                                                                 
519000       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
519100     END-IF                                                               
519200     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
519300     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
519400     MOVE WS-INLA-INL-IDLEVNR         TO EKO-EKH-IDLEVNR                  
519500     IF INLA-ART-KVAVIS > ZERO                                            
519600       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
519700     ELSE                                                                 
519800       MOVE 0                         TO EKO-EKH-KDAVVTYP                 
519900     END-IF                                                               
520000     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
520100     MOVE WS-INLE-MOT-KVANTMOT        TO EKO-EKH-KVANTMOT                 
520200     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
520300     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
520400     MOVE SPACE                       TO EKO-EKH-FLDCET                   
520500     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
520600     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
520700     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
520800     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
520900                                                                          
521000     PERFORM IMS-ISRT-EKOTRANS                                            
521100                                                                          
521200     PERFORM UNTIL SEGMENT-FINNS                                          
521300       ADD +1 TO EKO-FIL-IDSEKVNR                                         
521400       PERFORM IMS-ISRT-EKOTRANS                                          
521500     END-PERFORM                                                          
521600     .                                                                    
521700     EJECT                                                                
521800                                                                          
521900 S08C-SKAPA-EK-TRANS     SECTION.                                         
522000     IF WS-INLA-INL-IDLEVNR = '3324 '                                     
522100       MOVE WC-CDC-TR      TO EKH-IDDC-SEND                               
522200       IF WS-KVDIFF-MOT-AVIS > +0                                         
522300         MOVE WS-KVDIFF-MOT-AVIS TO EKH-KVANTAL                           
522400         MOVE '101'              TO EKH-KDEKHHT                           
522500         MOVE '106'              TO EKH-KDEKSHT                           
522600       ELSE                                                               
522700         COMPUTE EKH-KVANTAL = WS-KVDIFF-MOT-AVIS * -1                    
522800         MOVE '101'              TO EKH-KDEKHHT                           
522900         MOVE '107'              TO EKH-KDEKSHT                           
523000       END-IF                                                             
523100       MOVE +0                   TO EKH-IDDISTR                           
523200     ELSE                                                                 
523300       IF WS-KVDIFF-MOT-AVIS > +0                                         
523400         MOVE WS-KVDIFF-MOT-AVIS TO EKH-KVANTAL                           
523500         MOVE '502'              TO EKH-KDEKHHT                           
523600         MOVE '502'              TO EKH-KDEKSHT                           
523700       ELSE                                                               
523800         MOVE WS-KVDIFF-MOT-AVIS TO EKH-KVANTAL                           
523900         MOVE '502'              TO EKH-KDEKHHT                           
524000         MOVE '503'              TO EKH-KDEKSHT                           
524100       END-IF                                                             
524200       MOVE NEXT-DCS-IDDC        TO EKH-IDDC-SEND                         
524300*** FOR RETURNS DEVIATION (OVERLEVERANS/UNDERLEVERANS) FOR VCCS,          
524400*** THE RETURNS DISTRICT FROM WDB6 WILL BE USED. THIS IS NEEDED           
524500*** IN W51068 PROGRAM WHEN UPDATING THE VAT CODES FOR RETURNS             
524600*** DEVIATION                                                             
524700       MOVE NEXT-DCS-IDDISTR-RETUR                                        
524800                                 TO EKH-IDDISTR                           
524900     END-IF                                                               
525000                                                                          
525100     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
525200     MOVE WC-CDC-SE                   TO EKH-IDDC-REC                     
525300     MOVE +0                          TO EKH-IDKUNDNR                     
525400*******************************                                           
525500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
525600       MOVE ZERO TO NOLL-RAKNARE                                          
525700       MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                
525800       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
525900       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
526000            FOR LEADING ZERO                                              
526100       ADD +1 TO NOLL-RAKNARE                                             
526200       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                     
526300          WITH POINTER NOLL-RAKNARE                                       
526400*******************************                                           
526500     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
526600     MOVE ZERO                        TO EKH-KDPSLLOC                     
526700                                         EKH-PRARTNTO                     
526800                                         EKH-PRARTSJK                     
526900                                         EKH-PRHEMTAG                     
527000                                         EKH-PRINK                        
527100                                         EKH-PRLANDCO                     
527200                                         EKH-PRDIRLON                     
527300                                         EKH-PRDMTRL                      
527400                                         EKH-PROVRPAL                     
527500                                         EKH-SUBEL                        
527600                                         EKH-IDORDNR5                     
527700     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
527800     MOVE SPACE                       TO EKH-FLLSBOK                      
527900     MOVE 'SEK'                       TO EKH-KDVALISO                     
528000     MOVE 1.00                        TO EKH-PRKURS                       
528100     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
528200     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
528300     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
528400     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
528500     MOVE ZERO                        TO EKH-IDKONTO                      
528600                                         EKH-SUVAT                        
528700                                         EKH-KDFRAKT                      
528800     MOVE SPACE                       TO EKH-BEVAT                        
528900                                         EKH-IDKST                        
529000                                         EKH-KDANMORS                     
529100                                         EKH-IDANALYS                     
529200                                                                          
529300     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
529400     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
529500       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
529600     ELSE                                                                 
529700       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
529800     END-IF                                                               
529900     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
530000     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
530100     MOVE WS-INLA-INL-IDLEVNR         TO EKH-IDLEVNR                      
530200     IF INLA-ART-KVAVIS > ZERO                                            
530300       MOVE 1                         TO EKH-KDAVVTYP                     
530400     ELSE                                                                 
530500       MOVE 0                         TO EKH-KDAVVTYP                     
530600     END-IF                                                               
530700     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
530800     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
530900     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
531000     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
531100     MOVE 'SEPV'                      TO EKH-KDTRADP                      
531200     MOVE SPACE                       TO EKH-FLDCET                       
531300     MOVE SPACE                       TO EKH-IDKUNDRF                     
531400     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
531500                                                                          
531600     PERFORM IMS-ISRT-WLSAPA01                                            
531700                                                                          
531800     PERFORM UNTIL SEGMENT-FINNS                                          
531900       ADD +1 TO FIL-IDSEKVNR                                             
532000       PERFORM IMS-ISRT-WLSAPA01                                          
532100     END-PERFORM                                                          
532200     .                                                                    
532300     EJECT                                                                
532400                                                                          
532500 S09-SKAPA-TRANSPORT-TRANS SECTION.                                       
532600     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
532700       MOVE 'W6019300'                  TO EKO-FIL-IDPGM                  
532800       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
532900       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
533000       MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT               
533100       MOVE +1                          TO EKO-FIL-IDSEKVNR               
533200       EVALUATE TRUE                                                      
533300         WHEN DCS-CHINA                                                   
533400           MOVE 'W570'                  TO EKO-FIL-IDCPYTXT(1:4)          
533500         WHEN DCS-INDIA                                                   
533600           MOVE 'W515'                  TO EKO-FIL-IDCPYTXT(1:4)          
533700         WHEN DCS-USA                                                     
533800           MOVE 'W561'                  TO EKO-FIL-IDCPYTXT(1:4)          
533900         WHEN OTHER                                                       
534000           MOVE DCS-KDTRADP             TO EKO-FIL-IDCPYTXT(1:4)          
534100       END-EVALUATE                                                       
534200       MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)          
534300       PERFORM S09A-SKAPA-TRANSPORT-TRANS                                 
534400     ELSE                                                                 
534500       MOVE 'W6019300'                  TO FIL-IDPGM                      
534600       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
534700                                           EKH-DAVERDAT                   
534800       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
534900       MOVE +1                          TO FIL-IDSEKVNR                   
535000       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
535100       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
535200       PERFORM S09B-SKAPA-TRANSPORT-TRANS                                 
535300     END-IF                                                               
535400     .                                                                    
535500     EJECT                                                                
535600                                                                          
535700 S09A-SKAPA-TRANSPORT-TRANS SECTION.                                      
535800** UPPDATERING AV WDR8 (FILB)                                             
535900     IF NEXT-DCS-IDLEVNR-DC NOT = WS-INLA-INL-IDLEVNR                     
536000       PERFORM IMS-GN-WDB601                                              
536100       PERFORM UNTIL BASEN-SLUT                                           
536200                  OR SEGMENT-SAKNAS                                       
536300                  OR NEXT-DCS-IDLEVNR-DC = WS-INLA-INL-IDLEVNR            
536400          PERFORM IMS-GN-WDB601                                           
536500       END-PERFORM                                                        
536600     END-IF                                                               
536700     IF BASEN-SLUT OR SEGMENT-SAKNAS                                      
536800       IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                 
536900          MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                            
537000          PERFORM IMS-GU-WDB601                                           
537100       END-IF                                                             
537200       IF DCS-CDC                                                         
537300         MOVE WC-CDC-SE               TO EKO-EKH-IDDC-REC                 
537400                                         EKO-EKH-IDDC-SEND                
537500       ELSE                                                               
537600         MOVE WC-CDC-TR               TO EKO-EKH-IDDC-REC                 
537700                                         EKO-EKH-IDDC-SEND                
537800       END-IF                                                             
537900     ELSE                                                                 
538000       MOVE NEXT-DCS-IDDC             TO EKO-EKH-IDDC-SEND                
538100       MOVE WC-CDC-SE                 TO EKO-EKH-IDDC-REC                 
538200     END-IF                                                               
538300     MOVE '502'                       TO EKO-EKH-KDEKHHT                  
538400     MOVE '501'                       TO EKO-EKH-KDEKSHT                  
538500                                                                          
538600     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
538700     MOVE +0                          TO EKO-EKH-IDDISTR                  
538800                                         EKO-EKH-IDKUNDNR                 
538900*******************************                                           
539000*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
539100       MOVE ZERO TO NOLL-RAKNARE                                          
539200       MOVE WS-IDAVINR                  TO WS-SAP-IDLOPNRM                
539300       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
539400       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
539500            FOR LEADING ZERO                                              
539600       ADD +1 TO NOLL-RAKNARE                                             
539700       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                 
539800          WITH POINTER NOLL-RAKNARE                                       
539900*******************************                                           
540000     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
540100     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
540200                                         EKO-EKH-PRARTNTO                 
540300                                         EKO-EKH-PRARTSJK                 
540400                                         EKO-EKH-PRHEMTAG                 
540500                                         EKO-EKH-PRINK                    
540600                                         EKO-EKH-PRLANDCO                 
540700                                         EKO-EKH-PRDIRLON                 
540800                                         EKO-EKH-PRDMTRL                  
540900                                         EKO-EKH-PROVRPAL                 
541000                                         EKO-EKH-SUBEL                    
541100                                         EKO-EKH-IDORDNR5                 
541200     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
541300     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
541400     MOVE 1.00                        TO EKO-EKH-PRKURS                   
541500     MOVE SLAG-PRAVCOST               TO EKO-EKH-PRARTSTD                 
541600                                                                          
541700     COMPUTE EKO-EKH-KVANTAL = WS-KVINLART-TRP * -1                       
541800     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
541900     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
542000                                         EKO-EKH-SUVAT                    
542100                                         EKO-EKH-KDFRAKT                  
542200     MOVE SPACE                       TO EKO-EKH-BEVAT                    
542300                                         EKO-EKH-KDANMORS                 
542400                                         EKO-EKH-IDKST                    
542500                                         EKO-EKH-IDANALYS                 
542600                                                                          
542700     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
542800     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
542900       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
543000     ELSE                                                                 
543100       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
543200     END-IF                                                               
543300     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
543400     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
543500     MOVE WS-INLA-INL-IDLEVNR         TO EKO-EKH-IDLEVNR                  
543600     IF INLA-ART-KVAVIS > ZERO                                            
543700       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
543800     ELSE                                                                 
543900       MOVE 0                         TO EKO-EKH-KDAVVTYP                 
544000     END-IF                                                               
544100     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
544200     MOVE WS-INLE-MOT-KVANTMOT        TO EKO-EKH-KVANTMOT                 
544300     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
544400     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
544500     MOVE SPACE                       TO EKO-EKH-FLDCET                   
544600     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
544700     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
544800     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
544900     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
545000                                                                          
545100     PERFORM IMS-ISRT-EKOTRANS                                            
545200                                                                          
545300     PERFORM UNTIL SEGMENT-FINNS                                          
545400       ADD +1 TO EKO-FIL-IDSEKVNR                                         
545500       PERFORM IMS-ISRT-EKOTRANS                                          
545600     END-PERFORM                                                          
545700     .                                                                    
545800     EJECT                                                                
545900                                                                          
546000 S09B-SKAPA-TRANSPORT-TRANS SECTION.                                      
546100** UPPDATERING AV WDR9 (SAPA)                                             
546200     IF NEXT-DCS-IDLEVNR-DC NOT = WS-INLA-INL-IDLEVNR                     
546300       PERFORM IMS-GN-WDB601                                              
546400       PERFORM UNTIL BASEN-SLUT                                           
546500                  OR SEGMENT-SAKNAS                                       
546600                  OR NEXT-DCS-IDLEVNR-DC = WS-INLA-INL-IDLEVNR            
546700          PERFORM IMS-GN-WDB601                                           
546800       END-PERFORM                                                        
546900     END-IF                                                               
547000     IF BASEN-SLUT OR SEGMENT-SAKNAS                                      
547100       IF WS-INLA-INL-IDDC NOT = DCS-IDDC                                 
547200          MOVE WS-INLA-INL-IDDC   TO W-IDDC-B6                            
547300          PERFORM IMS-GU-WDB601                                           
547400       END-IF                                                             
547500       IF DCS-CDC                                                         
547600         MOVE WC-CDC-SE               TO EKH-IDDC-REC                     
547700                                         EKH-IDDC-SEND                    
547800       ELSE                                                               
547900         MOVE WC-CDC-TR               TO EKH-IDDC-REC                     
548000                                         EKH-IDDC-SEND                    
548100       END-IF                                                             
548200     ELSE                                                                 
548300       MOVE NEXT-DCS-IDDC             TO EKH-IDDC-SEND                    
548400       MOVE WC-CDC-SE                 TO EKH-IDDC-REC                     
548500     END-IF                                                               
548600     MOVE '502'                       TO EKH-KDEKHHT                      
548700     MOVE '501'                       TO EKH-KDEKSHT                      
548800                                                                          
548900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
549000     MOVE +0                          TO EKH-IDDISTR                      
549100                                         EKH-IDKUNDNR                     
549200*******************************                                           
549300*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
549400       MOVE ZERO TO NOLL-RAKNARE                                          
549500       MOVE WS-IDAVINR                  TO WS-SAP-IDLOPNRM                
549600       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
549700       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
549800            FOR LEADING ZERO                                              
549900       ADD +1 TO NOLL-RAKNARE                                             
550000       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                     
550100          WITH POINTER NOLL-RAKNARE                                       
550200*******************************                                           
550300     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
550400     MOVE ZERO                        TO EKH-KDPSLLOC                     
550500                                         EKH-PRARTNTO                     
550600                                         EKH-PRARTSJK                     
550700                                         EKH-PRHEMTAG                     
550800                                         EKH-PRINK                        
550900                                         EKH-PRLANDCO                     
551000                                         EKH-PRDIRLON                     
551100                                         EKH-PRDMTRL                      
551200                                         EKH-PROVRPAL                     
551300                                         EKH-SUBEL                        
551400                                         EKH-IDORDNR5                     
551500     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
551600     MOVE SPACE                       TO EKH-FLLSBOK                      
551700     MOVE 'SEK'                       TO EKH-KDVALISO                     
551800     MOVE 1.00                        TO EKH-PRKURS                       
551900     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
552000                                                                          
552100     COMPUTE EKH-KVANTAL = WS-KVINLART-TRP * -1                           
552200     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
552300     MOVE ZERO                        TO EKH-IDKONTO                      
552400                                         EKH-SUVAT                        
552500                                         EKH-KDFRAKT                      
552600     MOVE SPACE                       TO EKH-BEVAT                        
552700                                         EKH-IDKST                        
552800                                         EKH-KDANMORS                     
552900                                         EKH-IDANALYS                     
553000                                                                          
553100     MOVE WS-INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD               
553200     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
553300       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
553400     ELSE                                                                 
553500       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
553600     END-IF                                                               
553700     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
553800     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
553900     MOVE WS-INLA-INL-IDLEVNR         TO EKH-IDLEVNR                      
554000     IF INLA-ART-KVAVIS > ZERO                                            
554100       MOVE 1                         TO EKH-KDAVVTYP                     
554200     ELSE                                                                 
554300       MOVE 0                         TO EKH-KDAVVTYP                     
554400     END-IF                                                               
554500     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
554600     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
554700     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
554800     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
554900     MOVE 'SEPV'                      TO EKH-KDTRADP                      
555000     MOVE SPACE                       TO EKH-FLDCET                       
555100     MOVE SPACE                       TO EKH-IDKUNDRF                     
555200     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
555300                                                                          
555400     PERFORM IMS-ISRT-WLSAPA01                                            
555500                                                                          
555600     PERFORM UNTIL SEGMENT-FINNS                                          
555700       ADD +1 TO FIL-IDSEKVNR                                             
555800       PERFORM IMS-ISRT-WLSAPA01                                          
555900     END-PERFORM                                                          
556000     .                                                                    
556100     EJECT                                                                
556200* --- IMS SEKTIONER ---                                                   
556300     SKIP3                                                                
556400 S10-GRUNDDATA-LOGG SECTION.                                              
556500* LÄGGER UPP SALDOLOGG I WDL9 (GRUNDDATA)                                 
556600     MOVE W-IDARTNR               TO LOGG-IDARTNR                         
556700     ACCEPT TRANS-TID FROM TIME                                           
556800     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
556900     MOVE FUNCTION CURRENT-DATE (1:8) TO LOG-DAGENS-DATUM                 
557000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - LOG-DAGENS-DATUM           
557100     MOVE 9                       TO LOGG-IDSEKVNR                        
557200     MOVE 'INBO'                  TO LOGG-IDHUVTYP                        
557300     MOVE 'R32'                   TO LOGG-IDSUBTYP                        
557400     MOVE IDPGM                   TO LOGG-IDPGM                           
557500     MOVE W-IDTRANS               TO LOGG-IDTRANS                         
557600     MOVE MSG-SIGNON-USERID       TO LOGG-IDUSER                          
557700     MOVE SPACE                   TO LOGG-REF                             
557800     MOVE W-IDLOPNRM              TO LOGG-IDLOPNRM                        
557900     MOVE WS-INLA-INL-IDLEVNR     TO LOGG-IDLEVNR                         
558000     MOVE WS-INLA-INL-IDFS        TO LOGG-IDFS                            
558100     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
558200                                                                          
558300     .                                                                    
558400     EJECT                                                                
558500 S11-UPPDATERA-LOGG-WDK6 SECTION.                                         
558600                                                                          
558700     MOVE WC-CDC-SE                  TO LOGG-IDDC                         
558800     MOVE ARTC-CLAG-KVLS             TO LOGG-KVLS                         
558900     MOVE ARTC-CLAG-KVAKS-PAV        TO LOGG-KVAKS-PAV                    
559000     MOVE ARTC-CLAG-KVEFRS           TO LOGG-KVEFRS                       
559100     COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                           
559200                          ARTC-CLAG-KVAKS-T                               
559300     PERFORM IMS-ISRT-WDL901                                              
559400     IF SEGMENT-FINNS-REDAN                                               
559500        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
559600          ADD -1 TO LOGG-IDSEKVNR                                         
559700          PERFORM IMS-ISRT-WDL901                                         
559800        END-PERFORM                                                       
559900     END-IF                                                               
560000     .                                                                    
560100     EJECT                                                                
560200 S12-UPPDATERA-LOGG-WDK7 SECTION.                                         
560300                                                                          
560400     IF W-IDDC NOT = DCS-IDDC                                             
560500        MOVE W-IDDC   TO W-IDDC-B6                                        
560600        PERFORM IMS-GU-WDB601                                             
560700     END-IF                                                               
560800     IF DCS-CDC-TR                                                        
560900       MOVE WC-CDC-SE           TO LOGG-IDDC                              
561000     ELSE                                                                 
561100       MOVE W-IDDC              TO LOGG-IDDC                              
561200     END-IF                                                               
561300     MOVE SLAG-KVLS             TO LOGG-KVLS                              
561400     MOVE SLAG-KVAKS-PAV        TO LOGG-KVAKS-PAV                         
561500     MOVE SLAG-KVEFRS           TO LOGG-KVEFRS                            
561600     MOVE SLAG-KVAKS-SDC        TO LOGG-KVAKS                             
561700                                                                          
561800     PERFORM IMS-ISRT-WDL901                                              
561900     IF SEGMENT-FINNS-REDAN                                               
562000        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
562100          ADD -1 TO LOGG-IDSEKVNR                                         
562200          PERFORM IMS-ISRT-WDL901                                         
562300        END-PERFORM                                                       
562400     END-IF                                                               
562500                                                                          
562600     MOVE SPACE TO LOGG-WDL901                                            
562700                                                                          
562800     .                                                                    
562900     EJECT                                                                
563000 S13-UPPDATERA-MATERIALPRIS SECTION.                                      
563100                                                                          
563200     PERFORM IMS-GU-WDK711                                                
563300     IF  SEGMENT-FINNS                                                    
563400     AND SLAG-PRAVCOST > ZERO                                             
563500        SEARCH ALL DC-LAND                                                
563600        AT END                                                            
563700           MOVE 'EJ TRÄFF I TAB DCLAND'                                   
563800                                TO FELTEXT                                
563900           CALL FELLOG                                                    
564000        WHEN DCLAND-IDDC (DCLAND-IX) = W-IDDC-K7                          
564100           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
564200                                TO W-IDLAND-K7                            
564300        END-SEARCH                                                        
564400                                                                          
564500        PERFORM IMS-GHU-WDK712                                            
564600        IF  SEGMENT-FINNS                                                 
564700        AND LART-KDMATRPR = '2'                                           
564800*  HÄMTA VALUTAKURS FÖR CNY, AVGCO ÄR I CNY SKALL OMVANDLAS               
564900*  TILL SEK                                                               
565000           MOVE WS-TIAAMMDD-LOCAL(1:2)                                    
565100                                TO W-DATE-AAMM(1:2)                       
565200           MOVE 01              TO W-DATE-AAMM(3:2)                       
565300           MOVE WS-KDVALISO-HUV TO CURR-KDVALISO-HUV                      
565400           MOVE DCS-KDVALISO    TO CURR-KDVALISO-ROW                      
565500           MOVE W-DATE-AAMM     TO CURR-TIAAMM                            
565600           MOVE 'A'             TO CURR-KDVALTYP                          
565700           CALL W510CURR USING CURR-W510CURR 9305-PCB                     
565800           IF CURR-KDSVAR = ' '                                           
565900             MOVE CURR-PRKURS-NEW  TO WS-PRKURS                           
566000             MOVE CURR-REVALUTA-TO TO WS-REVALUTA                         
566100           ELSE                                                           
566200             MOVE 1             TO WS-PRKURS                              
566300             MOVE 1             TO WS-REVALUTA                            
566400           END-IF                                                         
566500           COMPUTE LART-PRMATRL ROUNDED =                                 
566600                   SLAG-PRAVCOST * WS-PRKURS / WS-REVALUTA                
566700           MOVE '1'             TO LART-KDMATRPR                          
566800           PERFORM IMS-REPL-WDK712                                        
566900        END-IF                                                            
567000     END-IF                                                               
567100     .                                                                    
567200     EJECT                                                                
567300 IMS-GET-MSG SECTION.                                                     
567400                                                                          
567500     MOVE '  QC' TO GODK-STATUSKODER                                      
567600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
567700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
567800     PERFORM IMS-STATUSKONTROLL                                           
567900     .                                                                    
568000     SKIP3                                                                
568100 IMS-GN-MSG SECTION.                                                      
568200                                                                          
568300     MOVE '  ' TO GODK-STATUSKODER                                        
568400     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
568500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
568600     PERFORM IMS-STATUSKONTROLL                                           
568700     .                                                                    
568800     EJECT                                                                
568900 IMS-ISRT-DISP-MSG SECTION.                                               
569000                                                                          
569100     MOVE    '  '             TO GODK-STATUSKODER                         
569200     CALL    CBLTDLI          USING ISRT DISP-PCB MSG-KOM-WMSGKOM         
569300     MOVE    DISP-STATUS-CODE TO STATUS-WS                                
569400     PERFORM IMS-STATUSKONTROLL                                           
569500     .                                                                    
569600     SKIP3                                                                
569700 IMS-ISRT-ALT-MSG-6202 SECTION.                                           
569800                                                                          
569900     MOVE    LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2               
570000     MOVE    '  '             TO    GODK-STATUSKODER                      
570100     CALL    CBLTDLI          USING ISRT 6202-PCB P-TO-P-SW               
570200     MOVE    6202-STATUS-CODE TO    STATUS-WS                             
570300     PERFORM IMS-STATUSKONTROLL                                           
570400     .                                                                    
570500     EJECT                                                                
570600 IMS-GU-INLC-SEQB SECTION.                                                
570700     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X ')'                        
570800          DELIMITED BY SIZE INTO SSA1                                     
570900     MOVE '  GE' TO GODK-STATUSKODER                                      
571000     CALL CBLTDLI USING GU W6INLC-PCB DLI-IO-AREA SSA1                    
571100     MOVE W6INLC-STATUS-CODE TO STATUS-WS                                 
571200     PERFORM IMS-STATUSKONTROLL                                           
571300     .                                                                    
571400     SKIP3                                                                
571500 IMS-GU-INLA-INL SECTION.                                                 
571600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
571700          DELIMITED BY SIZE INTO SSA1                                     
571800     MOVE '  ' TO GODK-STATUSKODER                                        
571900     CALL CBLTDLI USING GU W6INLA-PCB DLI-IO-AREA2 SSA1                   
572000     MOVE W6INLA-STATUS-CODE TO STATUS-WS                                 
572100     PERFORM IMS-STATUSKONTROLL                                           
572200     .                                                                    
572300     SKIP3                                                                
572400 IMS-GNP-INLA-ART SECTION.                                                
572500     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
572600          DELIMITED BY SIZE INTO SSA1                                     
572700     MOVE '  ' TO GODK-STATUSKODER                                        
572800     CALL CBLTDLI USING GNP W6INLA-PCB DLI-IO-AREA2 SSA1                  
572900     MOVE W6INLA-STATUS-CODE TO STATUS-WS                                 
573000     PERFORM IMS-STATUSKONTROLL                                           
573100     .                                                                    
573200     EJECT                                                                
573300 IMS-GNP-INLA-RAD SECTION.                                                
573400     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
573500          DELIMITED BY SIZE INTO SSA1                                     
573600     MOVE 'W6INLA21 ' TO SSA2                                             
573700     MOVE '  GE' TO GODK-STATUSKODER                                      
573800     CALL CBLTDLI USING GNP W6INLA-PCB DLI-IO-AREA3 SSA1 SSA2             
573900     MOVE W6INLA-STATUS-CODE TO STATUS-WS                                 
574000     PERFORM IMS-STATUSKONTROLL                                           
574100     .                                                                    
574200     SKIP3                                                                
574300 IMS-GNP-INLA-RAD-F-KV SECTION.                                           
574400     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
574500          DELIMITED BY SIZE INTO SSA1                                     
574600     STRING 'W6INLA21*F(IDRADNR  =' W-IDRADNR-X ')'                       
574700          DELIMITED BY SIZE INTO SSA2                                     
574800     MOVE '  ' TO GODK-STATUSKODER                                        
574900     CALL CBLTDLI USING GNP W6INLA-PCB DLI-IO-AREA3 SSA1 SSA2             
575000     MOVE W6INLA-STATUS-CODE TO STATUS-WS                                 
575100     PERFORM IMS-STATUSKONTROLL                                           
575200     .                                                                    
575300     EJECT                                                                
575400 IMS-GHU-INLA-ART SECTION.                                                
575500     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
575600          DELIMITED BY SIZE INTO SSA1                                     
575700     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
575800          DELIMITED BY SIZE INTO SSA2                                     
575900     MOVE '  ' TO GODK-STATUSKODER                                        
576000     CALL CBLTDLI USING GHU W6INLA-PCB DLI-IO-AREA2 SSA1 SSA2             
576100     MOVE W6INLA-STATUS-CODE TO STATUS-WS                                 
576200     PERFORM IMS-STATUSKONTROLL                                           
576300     .                                                                    
576400     SKIP3                                                                
576500 IMS-REPL-INLA     SECTION.                                               
576600     MOVE '  ' TO GODK-STATUSKODER                                        
576700     CALL CBLTDLI USING REPL W6INLA-PCB DLI-IO-AREA2                      
576800     MOVE W6INLA-STATUS-CODE TO STATUS-WS                                 
576900     PERFORM IMS-STATUSKONTROLL                                           
577000     .                                                                    
577100     EJECT                                                                
577200 IMS-GU-KVAI-SEQC SECTION.                                                
577300     STRING 'W6KVAI01(W6H7C1KY>=' W-W6H7C1KY-MIN-X                        
577400                    '&W6H7C1KY<=' W-W6H7C1KY-MAX-X ')'                    
577500          DELIMITED BY SIZE INTO SSA1                                     
577600     MOVE '  GE' TO GODK-STATUSKODER                                      
577700     CALL CBLTDLI USING GU W6KVAI-PCB DLI-IO-AREA SSA1                    
577800     MOVE W6KVAI-STATUS-CODE TO STATUS-WS                                 
577900     PERFORM IMS-STATUSKONTROLL                                           
578000     .                                                                    
578100     SKIP3                                                                
578200 IMS-GN-KVAI-SEQC SECTION.                                                
578300     STRING 'W6KVAI01(W6H7C1KY>=' W-W6H7C1KY-MIN-X                        
578400                    '&W6H7C1KY<=' W-W6H7C1KY-MAX-X ')'                    
578500          DELIMITED BY SIZE INTO SSA1                                     
578600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
578700     CALL CBLTDLI USING GN W6KVAI-PCB DLI-IO-AREA SSA1                    
578800     MOVE W6KVAI-STATUS-CODE TO STATUS-WS                                 
578900     PERFORM IMS-STATUSKONTROLL                                           
579000     .                                                                    
579100     SKIP3                                                                
579200 IMS-GU-KVAE-KR SECTION.                                                  
579300     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
579400          DELIMITED BY SIZE INTO SSA1                                     
579500     MOVE '  ' TO GODK-STATUSKODER                                        
579600     CALL CBLTDLI USING GU W6KVAE-PCB DLI-IO-AREA SSA1                    
579700     MOVE W6KVAE-STATUS-CODE TO STATUS-WS                                 
579800     PERFORM IMS-STATUSKONTROLL                                           
579900     .                                                                    
580000     EJECT                                                                
580100 IMS-GU-ARTC01 SECTION.                                                   
580200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
580300          DELIMITED BY SIZE INTO SSA1                                     
580400     MOVE '  ' TO GODK-STATUSKODER                                        
580500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC SSA1                      
580600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
580700     PERFORM IMS-STATUSKONTROLL                                           
580800     .                                                                    
580900     SKIP3                                                                
581000 IMS-GHU-ARTC11 SECTION.                                                  
581100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
581200          DELIMITED BY SIZE INTO SSA1                                     
581300     MOVE 'WLARTC11 ' TO SSA2                                             
581400     MOVE '  ' TO GODK-STATUSKODER                                        
581500     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC SSA1 SSA2                
581600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
581700     PERFORM IMS-STATUSKONTROLL                                           
581800     .                                                                    
581900     SKIP3                                                                
582000 IMS-GHNP-ARTC11 SECTION.                                                 
582100     MOVE 'WLARTC11 ' TO SSA1                                             
582200     MOVE '  ' TO GODK-STATUSKODER                                        
582300     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-ARTC SSA1                    
582400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
582500     PERFORM IMS-STATUSKONTROLL                                           
582600     .                                                                    
582700     SKIP3                                                                
582800 IMS-GNP-ARTC21 SECTION.                                                  
582900                                                                          
583000     STRING 'WLARTC21(IDLEVNR  =' W-IDLEVNR-21-X ')'                      
583100          DELIMITED BY SIZE INTO SSA1                                     
583200     MOVE '  GE' TO GODK-STATUSKODER                                      
583300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC SSA1                     
583400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
583500     PERFORM IMS-STATUSKONTROLL                                           
583600     .                                                                    
583700     SKIP3                                                                
583800 IMS-GNP-ARTC23 SECTION.                                                  
583900                                                                          
584000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
584100     MOVE 'WLARTC23'              TO SSA2                                 
584200     MOVE '  GE' TO GODK-STATUSKODER                                      
584300     CALL CBLTDLI USING GNP ARTC-PCB WLARTC23 SSA1 SSA2                   
584400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
584500     PERFORM IMS-STATUSKONTROLL                                           
584600     .                                                                    
584700     SKIP3                                                                
584800 IMS-REPL-ARTC SECTION.                                                   
584900                                                                          
585000     MOVE '  ' TO GODK-STATUSKODER                                        
585100     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC                         
585200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
585300     PERFORM IMS-STATUSKONTROLL                                           
585400     .                                                                    
585500     EJECT                                                                
585600 IMS-GHU-WDK711 SECTION.                                                  
585700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
585800          DELIMITED BY SIZE INTO SSA1                                     
585900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
586000          DELIMITED BY SIZE INTO SSA2                                     
586100     MOVE '  GE' TO GODK-STATUSKODER                                      
586200     CALL CBLTDLI USING GHU WDK7I-PCB DLI-IO-WDK711 SSA1 SSA2             
586300     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
586400     PERFORM IMS-STATUSKONTROLL                                           
586500     .                                                                    
586600     SKIP3                                                                
586700 IMS-REPL-WDK711 SECTION.                                                 
586800                                                                          
586900     MOVE '  ' TO GODK-STATUSKODER                                        
587000     CALL CBLTDLI USING REPL WDK7I-PCB DLI-IO-WDK711                      
587100     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
587200     PERFORM IMS-STATUSKONTROLL                                           
587300     .                                                                    
587400     EJECT                                                                
587500 IMS-GU-WDL601 SECTION.                                                   
587600     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
587700          DELIMITED BY SIZE INTO SSA1                                     
587800     MOVE '  GE' TO GODK-STATUSKODER                                      
587900     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-WDL601 SSA1               
588000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
588100     PERFORM IMS-STATUSKONTROLL                                           
588200     .                                                                    
588300     SKIP3                                                                
588400 IMS-ISRT-WDL601 SECTION.                                                 
588500                                                                          
588600     MOVE 'WDL601   ' TO SSA1                                             
588700     MOVE '  ' TO GODK-STATUSKODER                                        
588800     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-AREA-WDL601 SSA1             
588900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
589000     PERFORM IMS-STATUSKONTROLL                                           
589100     .                                                                    
589200     EJECT                                                                
589300 IMS-ISRT-WDL611 SECTION.                                                 
589400                                                                          
589500     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
589600          DELIMITED BY SIZE INTO SSA1                                     
589700     MOVE 'WDL611   ' TO SSA2                                             
589800     MOVE '  ' TO GODK-STATUSKODER                                        
589900     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-AREA-WDL611 SSA1 SSA2        
590000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
590100     PERFORM IMS-STATUSKONTROLL                                           
590200     .                                                                    
590300     EJECT                                                                
590400 IMS-GHNP-WDL611 SECTION.                                                 
590500                                                                          
590600     MOVE 'WDL611   ' TO SSA1                                             
590700     MOVE '  GE' TO GODK-STATUSKODER                                      
590800     CALL CBLTDLI USING GHNP WDL6-PCB DLI-IO-AREA-WDL611 SSA1             
590900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
591000     PERFORM IMS-STATUSKONTROLL                                           
591100     .                                                                    
591200     EJECT                                                                
591300 IMS-GNP-WDL611 SECTION.                                                  
591400                                                                          
591500     MOVE 'WDL611   ' TO SSA1                                             
591600     MOVE '  GE' TO GODK-STATUSKODER                                      
591700     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-AREA-WDL611 SSA1              
591800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
591900     PERFORM IMS-STATUSKONTROLL                                           
592000     .                                                                    
592100     EJECT                                                                
592200 IMS-REPL-WDL611 SECTION.                                                 
592300                                                                          
592400     STRING 'WDL611   '                                                   
592500          DELIMITED BY SIZE INTO SSA1                                     
592600     MOVE '  ' TO GODK-STATUSKODER                                        
592700     CALL CBLTDLI USING REPL WDL6-PCB DLI-IO-AREA-WDL611 SSA1             
592800     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
592900     PERFORM IMS-STATUSKONTROLL                                           
593000     .                                                                    
593100     EJECT                                                                
593200 IMS-GHNP-WDL612 SECTION.                                                 
593300                                                                          
593400     MOVE 'WDL612   ' TO SSA1                                             
593500     MOVE '  GE' TO GODK-STATUSKODER                                      
593600     CALL CBLTDLI USING GHNP WDL6-PCB DLI-IO-AREA-WDL612 SSA1             
593700     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
593800     PERFORM IMS-STATUSKONTROLL                                           
593900     .                                                                    
594000     EJECT                                                                
594100 IMS-DLET-WDL612  SECTION.                                                
594200     MOVE '  ' TO GODK-STATUSKODER                                        
594300     CALL CBLTDLI USING DLET WDL6-PCB DLI-IO-AREA-WDL612                  
594400     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
594500     PERFORM IMS-STATUSKONTROLL                                           
594600     .                                                                    
594700     EJECT                                                                
594800 IMS-GU-INLB11 SECTION.                                                   
594900     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
595000          DELIMITED BY SIZE INTO SSA1                                     
595100     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
595200          DELIMITED BY SIZE INTO SSA2                                     
595300     MOVE '  GE' TO GODK-STATUSKODER                                      
595400     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2                 
595500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
595600     PERFORM IMS-STATUSKONTROLL                                           
595700     .                                                                    
595800     SKIP3                                                                
595900 IMS-GHU-INLB11 SECTION.                                                  
596000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
596100          DELIMITED BY SIZE INTO SSA1                                     
596200     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
596300          DELIMITED BY SIZE INTO SSA2                                     
596400     MOVE '  ' TO GODK-STATUSKODER                                        
596500     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2                
596600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
596700     PERFORM IMS-STATUSKONTROLL                                           
596800     .                                                                    
596900     EJECT                                                                
597000 IMS-GHNP-INLB23 SECTION.                                                 
597100     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
597200          DELIMITED BY SIZE INTO SSA1                                     
597300     MOVE '  GE' TO GODK-STATUSKODER                                      
597400     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
597500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
597600     PERFORM IMS-STATUSKONTROLL                                           
597700     .                                                                    
597800     SKIP3                                                                
597900 IMS-REPL-INLB SECTION.                                                   
598000     MOVE '  ' TO GODK-STATUSKODER                                        
598100     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA                         
598200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
598300     PERFORM IMS-STATUSKONTROLL                                           
598400     .                                                                    
598500     EJECT                                                                
598600 IMS-GHNP-INLB31-F-KV  SECTION.                                           
598700     STRING 'WLINLB23*F(WDD905KY =' W-WDD905KY-X                          
598800                      '&KDAVROP  =' W-KDAVROP-X ')'                       
598900            DELIMITED BY SIZE INTO SSA1                                   
599000     STRING 'WLINLB31(IDLOPNRM =' W-INLB31-IDLOPNRM-X ')'                 
599100            DELIMITED BY SIZE INTO SSA2                                   
599200     MOVE '  ' TO GODK-STATUSKODER                                        
599300     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA5 SSA1 SSA2              
599400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
599500     PERFORM IMS-STATUSKONTROLL                                           
599600     .                                                                    
599700     SKIP3                                                                
599800 IMS-DLET-INLB31 SECTION.                                                 
599900     MOVE '  ' TO GODK-STATUSKODER                                        
600000     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA5                        
600100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
600200     PERFORM IMS-STATUSKONTROLL                                           
600300     .                                                                    
600400     SKIP3                                                                
600500 IMS-ISRT-INLB31 SECTION.                                                 
600600     MOVE 'WLINLB31 ' TO SSA1                                             
600700     MOVE '  ' TO GODK-STATUSKODER                                        
600800     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA5 SSA1                   
600900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
601000     PERFORM IMS-STATUSKONTROLL                                           
601100     .                                                                    
601200     EJECT                                                                
601300 IMS-GHNP-INLB23-31-PATH SECTION.                                         
601400     MOVE 'WLINLB23*D' TO SSA1                                            
601500     STRING 'WLINLB31(IDLOPNRM =' W-INLB31-IDLOPNRM-X ')'                 
601600            DELIMITED BY SIZE INTO SSA2                                   
601700     MOVE '  GE' TO GODK-STATUSKODER                                      
601800     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA6 SSA1 SSA2              
601900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
602000     PERFORM IMS-STATUSKONTROLL                                           
602100     .                                                                    
602200     SKIP3                                                                
602300 IMS-REPL-INLB23-31 SECTION.                                              
602400     MOVE '  ' TO GODK-STATUSKODER                                        
602500     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA6                        
602600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
602700     PERFORM IMS-STATUSKONTROLL                                           
602800     .                                                                    
602900     SKIP3                                                                
603000 IMS-REPL-INLB23-NOT-31 SECTION.                                          
603100     MOVE 'WLINLB31*N' TO SSA1                                            
603200     MOVE '  ' TO GODK-STATUSKODER                                        
603300     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA6 SSA1                   
603400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
603500     PERFORM IMS-STATUSKONTROLL                                           
603600     .                                                                    
603700     EJECT                                                                
603800 IMS-GU-WLINLE21 SECTION.                                                 
603900     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
604000          DELIMITED BY SIZE INTO SSA1                                     
604100     MOVE 'WLINLE11 ' TO SSA2                                             
604200     STRING 'WLINLE21(IDLOPNRM =' W-IDLOPNRM-X ')'                        
604300          DELIMITED BY SIZE INTO SSA3                                     
604400     MOVE '  GE' TO GODK-STATUSKODER                                      
604500     CALL CBLTDLI USING GHU INLE-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
604600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
604700     PERFORM IMS-STATUSKONTROLL                                           
604800     .                                                                    
604900     SKIP3                                                                
605000 IMS-GHU-INLE-MOT SECTION.                                                
605100     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
605200          DELIMITED BY SIZE INTO SSA1                                     
605300     MOVE 'WLINLE11 ' TO SSA2                                             
605400     STRING 'WLINLE21(IDLOPNRM =' W-IDLOPNRM-X ')'                        
605500          DELIMITED BY SIZE INTO SSA3                                     
605600     MOVE '  GE' TO GODK-STATUSKODER                                      
605700     CALL CBLTDLI USING GHU INLE-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
605800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
605900     PERFORM IMS-STATUSKONTROLL                                           
606000     .                                                                    
606100     SKIP3                                                                
606200 IMS-GHNP-INLE-DEL SECTION.                                               
606300     MOVE 'WLINLE31 ' TO SSA1                                             
606400     MOVE '  GE' TO GODK-STATUSKODER                                      
606500     CALL CBLTDLI USING GHNP INLE-PCB DLI-IO-AREA SSA1                    
606600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
606700     PERFORM IMS-STATUSKONTROLL                                           
606800     .                                                                    
606900     SKIP3                                                                
607000 IMS-ISRT-INLE-DEL SECTION.                                               
607100     MOVE 'WLINLE31 ' TO SSA1                                             
607200     MOVE '  ' TO GODK-STATUSKODER                                        
607300     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1                    
607400     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
607500     PERFORM IMS-STATUSKONTROLL                                           
607600     .                                                                    
607700     EJECT                                                                
607800 IMS-DLET-INLE SECTION.                                                   
607900     MOVE '  ' TO GODK-STATUSKODER                                        
608000     CALL CBLTDLI USING DLET INLE-PCB DLI-IO-AREA                         
608100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
608200     PERFORM IMS-STATUSKONTROLL                                           
608300     .                                                                    
608400     EJECT                                                                
608500 IMS-GHNP-WDL621    SECTION.                                              
608600     STRING 'WDL611  (DAINLEV = ' W-DAINLEV-X ')'                         
608700          DELIMITED BY SIZE INTO SSA1                                     
608800     MOVE 'WDL621   ' TO SSA2                                             
608900     MOVE '  GE' TO GODK-STATUSKODER                                      
609000     CALL CBLTDLI USING GHNP WDL6-PCB DLI-IO-AREA-WDL621 SSA1 SSA2        
609100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
609200     PERFORM IMS-STATUSKONTROLL                                           
609300     .                                                                    
609400     SKIP3                                                                
609500 IMS-ISRT-WDL621    SECTION.                                              
609600     MOVE 'WDL621   ' TO SSA1                                             
609700     MOVE '  ' TO GODK-STATUSKODER                                        
609800     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-AREA-WDL621 SSA1             
609900     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
610000     PERFORM IMS-STATUSKONTROLL                                           
610100     .                                                                    
610200     EJECT                                                                
610300 IMS-DLET-WDL621    SECTION.                                              
610400     MOVE '  ' TO GODK-STATUSKODER                                        
610500     CALL CBLTDLI USING DLET WDL6-PCB DLI-IO-AREA-WDL621                  
610600     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
610700     PERFORM IMS-STATUSKONTROLL                                           
610800     .                                                                    
610900     EJECT                                                                
611000 IMS-REPL-INLE SECTION.                                                   
611100     MOVE '  ' TO GODK-STATUSKODER                                        
611200     CALL CBLTDLI USING REPL INLE-PCB DLI-IO-AREA                         
611300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
611400     PERFORM IMS-STATUSKONTROLL                                           
611500     .                                                                    
611600     SKIP3                                                                
611700 IMS-ISRT-ZZAC01 SECTION.                                                 
611800     MOVE 'WLZZAC01 ' TO SSA1                                             
611900     MOVE '  II' TO GODK-STATUSKODER                                      
612000     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
612100     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
612200     PERFORM IMS-STATUSKONTROLL                                           
612300     .                                                                    
612400     SKIP3                                                                
612500 IMS-ISRT-XXBW-2228 SECTION.                                              
612600     STRING 'WLXXBW01(WDGXKEY  =' W-2227-KEY-X ')'                        
612700          DELIMITED BY SIZE INTO SSA1                                     
612800     MOVE 'WLXXBW11 ' TO SSA2                                             
612900*MS ENLIGT W2234                                                          
613000     MOVE '  II' TO GODK-STATUSKODER                                      
613100     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-AREA SSA1 SSA2               
613200     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
613300     PERFORM IMS-STATUSKONTROLL                                           
613400     .                                                                    
613500     SKIP3                                                                
613600 IMS-ISRT-4506 SECTION.                                                   
613700     STRING 'WL450501(WDGXKEY  =' W-4505-KEY-X ')'                        
613800          DELIMITED BY SIZE INTO SSA1                                     
613900     MOVE 'WL450511 ' TO SSA2                                             
614000     MOVE '  ' TO GODK-STATUSKODER                                        
614100     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA-4505 SSA1 SSA2          
614200     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
614300     PERFORM IMS-STATUSKONTROLL                                           
614400     .                                                                    
614500     SKIP3                                                                
614600 IMS-ISRT-WDL901 SECTION.                                                 
614700     SKIP2                                                                
614800     MOVE 'WLLOGA01 ' TO SSA1                                             
614900     MOVE '  II' TO GODK-STATUSKODER                                      
615000     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
615100     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
615200     PERFORM IMS-STATUSKONTROLL                                           
615300     .                                                                    
615400     EJECT                                                                
615500 IMS-ISRT-EKOTRANS  SECTION.                                              
615600     MOVE 'WLFILB01 ' TO SSA1                                             
615700     MOVE '  II' TO GODK-STATUSKODER                                      
615800     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA-FILB01 SSA1             
615900     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
616000     PERFORM IMS-STATUSKONTROLL                                           
616100     .                                                                    
616200     EJECT                                                                
616300 IMS-ISRT-WLSAPA01 SECTION.                                               
616400     MOVE 'WLSAPA01 ' TO SSA1                                             
616500     MOVE '  II' TO GODK-STATUSKODER                                      
616600     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
616700     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
616800     PERFORM IMS-STATUSKONTROLL                                           
616900     .                                                                    
617000     SKIP2                                                                
617100 IMS-GU-WDK711 SECTION.                                                   
617200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
617300          DELIMITED BY SIZE INTO SSA1                                     
617400     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
617500          DELIMITED BY SIZE INTO SSA2                                     
617600     MOVE '  GE' TO GODK-STATUSKODER                                      
617700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711                         
617800          SSA1 SSA2                                                       
617900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
618000     PERFORM IMS-STATUSKONTROLL                                           
618100     .                                                                    
618200                                                                          
618300 IMS-GHU-WDK712 SECTION.                                                  
618400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
618500          DELIMITED BY SIZE INTO SSA1                                     
618600     STRING 'WDK712  (IDLAND   =' W-IDLAND-K7-X ')'                       
618700          DELIMITED BY SIZE INTO SSA2                                     
618800     MOVE '  GE' TO GODK-STATUSKODER                                      
618900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712                        
619000          SSA1 SSA2                                                       
619100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
619200     PERFORM IMS-STATUSKONTROLL                                           
619300     .                                                                    
619400                                                                          
619500 IMS-REPL-WDK712 SECTION.                                                 
619600     MOVE '  ' TO GODK-STATUSKODER                                        
619700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
619800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
619900     PERFORM IMS-STATUSKONTROLL                                           
620000     .                                                                    
620100                                                                          
620200 IMS-GNP-WDK723 SECTION.                                                  
620300     MOVE   'WDK723'    TO SSA1                                           
620400     MOVE '  GE' TO GODK-STATUSKODER                                      
620500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK723 SSA1                   
620600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
620700     PERFORM IMS-STATUSKONTROLL                                           
620800     .                                                                    
620900     EJECT                                                                
621000 IMS-GNP-WDK724 SECTION.                                                  
621100     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
621200                    '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                      
621300          DELIMITED BY SIZE INTO SSA1                                     
621400     MOVE '  GE' TO GODK-STATUSKODER                                      
621500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
621600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
621700     PERFORM IMS-STATUSKONTROLL                                           
621800     .                                                                    
621900     EJECT                                                                
622000 IMS-GHNP-WDK724 SECTION.                                                 
622100     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
622200                    '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                      
622300          DELIMITED BY SIZE INTO SSA1                                     
622400     MOVE '  GE' TO GODK-STATUSKODER                                      
622500     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK724 SSA1                  
622600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
622700     PERFORM IMS-STATUSKONTROLL                                           
622800     .                                                                    
622900     EJECT                                                                
623000 IMS-REPL-WDK724 SECTION.                                                 
623100     MOVE '  ' TO GODK-STATUSKODER                                        
623200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK724                       
623300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
623400     PERFORM IMS-STATUSKONTROLL                                           
623500     .                                                                    
623600     EJECT                                                                
623700 IMS-GU-WDB601    SECTION.                                                
623800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
623900          DELIMITED BY SIZE INTO SSA1                                     
624000     MOVE '  GE' TO GODK-STATUSKODER                                      
624100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
624200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
624300     PERFORM IMS-STATUSKONTROLL                                           
624400     IF SEGMENT-SAKNAS                                                    
624500         MOVE SPACE TO DCS-KDDC                                           
624600     END-IF                                                               
624700     .                                                                    
624800                                                                          
624900 IMS-GNP-WDB617    SECTION.                                               
625000     MOVE 'WDB617   ' TO SSA1                                             
625100     MOVE '  GE' TO GODK-STATUSKODER                                      
625200     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
625300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
625400     PERFORM IMS-STATUSKONTROLL                                           
625500     .                                                                    
625600                                                                          
625700 IMS-GN-WDB601    SECTION.                                                
625800     MOVE 'WDB601  ' TO SSA1                                              
625900     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
626000     CALL CBLTDLI USING GN WDB6-NEXT-PCB DLI-IO-B601-NEXT SSA1            
626100     MOVE WDB6-NEXT-STATUS-CODE    TO STATUS-WS                           
626200     PERFORM IMS-STATUSKONTROLL                                           
626300     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
626400         MOVE SPACE TO NEXT-DCS-KDDC                                      
626500     END-IF                                                               
626600     .                                                                    
626700                                                                          
626800 IMS-GU-WLLEVA01 SECTION.                                                 
626900     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
627000     DELIMITED BY SIZE INTO SSA1                                          
627100     MOVE '  GE' TO GODK-STATUSKODER                                      
627200     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
627300     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
627400     PERFORM IMS-STATUSKONTROLL                                           
627500     .                                                                    
627600     SKIP3                                                                
627700                                                                          
627800 IMS-GNP-WLLEVA11 SECTION.                                                
627900     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
628000     DELIMITED BY SIZE INTO SSA1                                          
628100     MOVE '  GE' TO GODK-STATUSKODER                                      
628200     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
628300     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
628400     PERFORM IMS-STATUSKONTROLL                                           
628500     .                                                                    
628600     EJECT                                                                
628700                                                                          
628800 IMS-GU-WDGX9306 SECTION.                                                 
628900     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
629000             DELIMITED BY SIZE INTO SSA1                                  
629100     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
629200             DELIMITED BY SIZE INTO SSA2                                  
629300     MOVE '  GE'   TO GODK-STATUSKODER                                    
629400     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
629500     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
629600     PERFORM IMS-STATUSKONTROLL                                           
629700     .                                                                    
629800     SKIP3                                                                
629900                                                                          
630000 IMS-GNP-WDGX9308 SECTION.                                                
630100     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
630200             DELIMITED BY SIZE INTO SSA1                                  
630300     MOVE '  GE'   TO GODK-STATUSKODER                                    
630400     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
630500     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
630600     PERFORM IMS-STATUSKONTROLL                                           
630700     .                                                                    
630800     SKIP3                                                                
630900                                                                          
631000 IMS-GNP-WDGX9308-FIRST SECTION.                                          
631100     MOVE 'WDGX9308*F' TO SSA1                                            
631200     MOVE '  GE'   TO GODK-STATUSKODER                                    
631300     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
631400     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
631500     PERFORM IMS-STATUSKONTROLL                                           
631600     .                                                                    
631700     SKIP3                                                                
631800                                                                          
631900 IMS-GU-WDGX2216 SECTION.                                                 
632000     MOVE 'IMS-GU-WDGX2216 '  TO CURRENT-IMS-SECTION                      
632100                                                                          
632200     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2215-X ')'                    
632300          DELIMITED BY SIZE INTO SSA1                                     
632400     STRING 'WDR242  (IDLEVNR  =' W-IDLEVNR-2216-X ')'                    
632500          DELIMITED BY SIZE INTO SSA2                                     
632600     MOVE '  GE' TO GODK-STATUSKODER                                      
632700     CALL CBLTDLI USING GU  2215-PCB DLI-IO-WDGX2216 SSA1 SSA2            
632800     MOVE 2215-STATUS-CODE TO STATUS-WS                                   
632900     PERFORM IMS-STATUSKONTROLL                                           
633000     .                                                                    
633100     SKIP3                                                                
633200                                                                          
633300 IMS-ISRT-WDGX2218 SECTION.                                               
633400     MOVE 'IMS-ISRT-WDGX2218 '  TO CURRENT-IMS-SECTION                    
633500                                                                          
633600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2217-X ')'                    
633700          DELIMITED BY SIZE INTO SSA1                                     
633800     MOVE   'WDR551  '        TO SSA2                                     
633900     MOVE '  II'              TO GODK-STATUSKODER                         
634000     CALL CBLTDLI USING ISRT 2217-PCB DLI-IO-WDGX2218 SSA1 SSA2           
634100     MOVE 2217-STATUS-CODE    TO STATUS-WS                                
634200     PERFORM IMS-STATUSKONTROLL                                           
634300     .                                                                    
634400     SKIP3                                                                
634500                                                                          
634600 IMS-ISRT-WDGX2218-PERIOD SECTION.                                        
634700     MOVE 'IMS-ISRT-WDGX2218-PERIOD '  TO CURRENT-IMS-SECTION             
634800                                                                          
634900     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2217-P-X ')'                  
635000          DELIMITED BY SIZE INTO SSA1                                     
635100     MOVE   'WDR551  '        TO SSA2                                     
635200     MOVE '  II'              TO GODK-STATUSKODER                         
635300     CALL CBLTDLI USING ISRT 2217-PCB DLI-IO-WDGX2218 SSA1 SSA2           
635400     MOVE 2217-STATUS-CODE    TO STATUS-WS                                
635500     PERFORM IMS-STATUSKONTROLL                                           
635600     .                                                                    
635700     SKIP3                                                                
635800 IMS-GU-WDGX2206 SECTION.                                                 
635900     MOVE 'IMS-GU-WDGX2206 '  TO CURRENT-IMS-SECTION                      
636000                                                                          
636100     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-2205-X ')'                    
636200          DELIMITED BY SIZE INTO SSA1                                     
636300     STRING 'WDGX2206(KY2206   =' W-KY2206-X ')'                          
636400          DELIMITED BY SIZE INTO SSA2                                     
636500     MOVE '  GE'              TO GODK-STATUSKODER                         
636600     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2206 SSA1 SSA2             
636700     MOVE WDR2-STATUS-CODE    TO STATUS-WS                                
636800     PERFORM IMS-STATUSKONTROLL                                           
636900     .                                                                    
637000     SKIP3                                                                
637100 IMS-ISRT-WDGX2248 SECTION.                                               
637200     MOVE 'IMS-ISRT-WDGX2248 '  TO CURRENT-IMS-SECTION                    
637300                                                                          
637400     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-X ')'                    
637500          DELIMITED BY SIZE INTO SSA1                                     
637600     MOVE   'WDGX2248'        TO SSA2                                     
637700     MOVE '  II'              TO GODK-STATUSKODER                         
637800     CALL CBLTDLI USING ISRT 2247-PCB DLI-IO-WDGX2248 SSA1 SSA2           
637900     MOVE 2247-STATUS-CODE    TO STATUS-WS                                
638000     PERFORM IMS-STATUSKONTROLL                                           
638100     .                                                                    
638200     SKIP3                                                                
638300                                                                          
638400 IMS-ISRT-WDGX2248-PERIOD SECTION.                                        
638500     MOVE 'IMS-ISRT-WDGX2248-PERIOD '  TO CURRENT-IMS-SECTION             
638600                                                                          
638700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2247-P-X ')'                  
638800          DELIMITED BY SIZE INTO SSA1                                     
638900     MOVE   'WDGX2248'        TO SSA2                                     
639000     MOVE '  II'              TO GODK-STATUSKODER                         
639100     CALL CBLTDLI USING ISRT 2247-PCB DLI-IO-WDGX2248 SSA1 SSA2           
639200     MOVE 2247-STATUS-CODE    TO STATUS-WS                                
639300     PERFORM IMS-STATUSKONTROLL                                           
639400     .                                                                    
639500     SKIP3                                                                
639600                                                                          
639700 IMS-STATUSKONTROLL SECTION.                                              
639800                                                                          
639900     SET STATUS-IX TO 1                                                   
640000     SEARCH GODK-STATUS                                                   
640100       AT END                                                             
640200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
640300         DELIMITED BY SIZE INTO FELTEXT                                   
640400         CALL FELLOG                                                      
640500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
640600         CONTINUE                                                         
640700     END-SEARCH                                                           
640800     .                                                                    
640900     EJECT                                                                
641000*    -COPY WY2000P1                                                       
