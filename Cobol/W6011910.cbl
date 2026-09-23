000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011910.                                                
000300 AUTHOR.         GUNNAR LARSSON IDK.                                      
000400 DATE-WRITTEN.   92/08/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MAKULERA PARTI/BACKA R32(PF23)                                   
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001100*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
001200*        PROGRAMMET UPPDATERAR WLINLC (WDL6)                              
001300*        PROGRAMMET UPPDATERAR WLINLB (WDD9)                              
001400*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001500*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001600*        PROGRAMMET UPPDATERAR WLARTS (WDK6)                              
001700*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
001800*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
001900*        PROGRAMMET UPPDATERAR W6LASA (W6G2)                              
002000*        PROGRAMMET UPPDATERAR WLFILB (WDR8)                              
002100*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
002200*        PROGRAMMET LÄSER      W6KVAE (W6H7)                              
002300*        PROGRAMMET LÄSER MÅNADSKURSER WDG2                               
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W6T119                                              
002700*        MID:         W6I11901                                            
002800*        WEB REQU.    W60119I1                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W6O11901                                            
003200*        WEB RESP.    W60119O1                                            
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900*    -COPY WY2000W9                                                       
004000     SKIP3                                                                
004100 77  IDPGM                       PIC X(08)   VALUE 'W6011910'.            
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500 77  FILLER                      PIC X(16) VALUE '*ANTAL UPP.'.           
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  WS-IDDC-WDB6                PIC X(2)    VALUE SPACE.                 
005100 77  WS-IDDISTR-RETUR            PIC S9(5) COMP-3 VALUE 0.                
005200                                                                          
005300*01  -COPY WWDCLAND                                                       
005400                                                                          
005500*01  -COPY WWDCKONS                                                       
005600*01  -COPY WWDC99                                                         
005700*01  -COPY WWPRODSL                                                       
005800     EJECT                                                                
005900                                                                          
006000 77  CD-IX                       PIC S9(4)  VALUE ZERO  COMP-3.           
006100 77  IX-IDFS                     PIC S9(9)  VALUE +0    COMP SYNC.        
006200 77  IX-IDAVINR                  PIC S9(9)  VALUE +0    COMP SYNC.        
006300 77  WS-SPAR-KVAVIS              PIC S9(7)  VALUE +0    COMP-3.           
006400 77  WS-KVBEART                  PIC S9(7)  VALUE +0    COMP-3.           
006500 77  WS-INLE-MOT-KVANTMOT        PIC S9(7)  VALUE +0    COMP-3.           
006600 77  WS-INLE-MOT-KDAVVANT        PIC S9(1)  VALUE +0    COMP-3.           
006700 77  WS-INLE-MOT-KVRETUR         PIC S9(7)   VALUE ZERO COMP-3.           
006800 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
006900 77  WS-SAP-IDLOPNRM             PIC 9(9)    VALUE ZERO.                  
007000 77  WS-SAP-X-IDLOPNRM           PIC X(9)    VALUE SPACE.                 
007100 77  W-PRARTBEL-PR               PIC S9(8)V9(5) VALUE ZERO.               
007200 77  W-PRKURS                    PIC S9(6)V9(5) VALUE ZERO.               
007300 77  W-PRKURS-USD                PIC S9(6)V9(5) VALUE ZERO.               
007400 77  W-PRKURS-CAD                PIC S9(6)V9(5) VALUE ZERO.               
007500 77  W-PRKURS-ML                 PIC S9(6)V9(5) VALUE ZERO.               
007600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +241  COMP SYNC.        
007700 77  WS-SUMMA-R32                PIC S9(7)  VALUE ZERO  COMP-3.           
007800 77  WS-SUMMA-R32-SVS            PIC S9(7)  VALUE ZERO  COMP-3.           
007900 77  WS-WDL221                   PIC X(102)  VALUE SPACE.                 
008000 77  WS-KVINLART-TRP             PIC S9(7)   VALUE ZERO COMP-3.           
008100 77  WS-KVDIFF-MOT-AVIS          PIC S9(7)   VALUE ZERO COMP-3.           
008200 77  WS-ARTC23-IDAVTAL           PIC 9(13)   VALUE ZERO.                  
008300 77  WS-PRAVCOST                 PIC S9(7)V9(2).                          
008400 77  WS-KVAE-IDKRFEL             PIC X(2)   VALUE SPACE.                  
008500 77  WS-KVAE-FLKRLIM             PIC X(1)   VALUE SPACE.                  
008600 77  WS-SAP-MM-POST              PIC X(1)   VALUE SPACE.                  
008700 77  W-DATE-AAMM                 PIC 9(4)   VALUE ZERO.                   
008800 77  WS-KDVALISO-HUV             PIC X(3)   VALUE 'SEK'.                  
008900 77  WS-KDVALISO-HUV-CN          PIC X(3)   VALUE 'CNY'.                  
009000 77  WS-KDVALISO-HUV-US          PIC X(3)   VALUE 'USD'.                  
009100 01  SPAR-PRKURS                 PIC S9(5)V9(5) VALUE +0   COMP-3.        
009200 01  W-REVALUTA                  PIC S9(5)      VALUE +0   COMP-3.        
009300                                                                          
009400 01    WS-SUM-CD.                                                         
009500   03  WS-SUMMA-R32-CD           OCCURS 4 TIMES                           
009600                                 PIC S9(7)   VALUE ZERO  COMP-3.          
009700                                                                          
009800* VARIABLER FÖR ATT KOLLA OM PARTIET AVSLUTADES FÖRE ELLER EFTER          
009900* ÄT-INSTALLATION RÖRANDE AUTOMATFAKTURERING AV KONTROLLRAPPORTER         
010000 77  INSTALLATIONSDATUM          PIC 9(8)    VALUE 20010128.              
010100 77  FL-FORE-INST                PIC X       VALUE 'N'.                   
010200 77  TRAEFF                      PIC X       VALUE 'N'.                   
010300 01  SPAR-RAD-DAUPPDAT.                                                   
010400     03  SPAR-RAD-SEKEL          PIC 9(2)    VALUE ZERO.                  
010500     03  SPAR-RAD-TIUPPDAT       PIC 9(6)    VALUE ZERO.                  
010600 01  KOLLA-RAD-DAUPPDAT.                                                  
010700     03  KOLLA-RAD-SEKEL         PIC 9(2)    VALUE ZERO.                  
010800     03  KOLLA-RAD-TIUPPDAT      PIC 9(6)    VALUE ZERO.                  
010900 01  SPAR-KR-DAKRANS.                                                     
011000     03  SPAR-KR-SEKEL           PIC 9(2)    VALUE ZERO.                  
011100     03  SPAR-KR-TIKRANS         PIC 9(6)    VALUE ZERO.                  
011200 01  KOLLA-KR-DAKRANS.                                                    
011300     03  KOLLA-KR-SEKEL          PIC 9(2)    VALUE ZERO.                  
011400     03  KOLLA-KR-TIKRANS        PIC 9(6)    VALUE ZERO.                  
011500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
011600 77  WS-IDLOPNRM                 PIC X(8)    VALUE SPACE.                 
011700 77  W-IDLOPNRM-NUM9             PIC 9(9)    VALUE ZERO.                  
011800                                                                          
011900 01  WS-DAINLINL.                                                         
012000     03 WS-DAINLINL-SEKEL        PIC 9(2).                                
012100     03 WS-DAINLINL-AAMMDD       PIC 9(6).                                
012200                                                                          
012300 01  WS-PRKURS                   PIC S9(5)V9(5) VALUE +0   COMP-3.        
012400 01  WS-REVALUTA                 PIC S9(5)      VALUE +0   COMP-3.        
012500 01  W-RETULF                    PIC S9(3)V9(4) VALUE +0.                 
012600 01  WS-PRARTKALKYL              PIC S9(7)V9(5) VALUE +0 COMP-3.          
012700                                                                          
012800 01  WS-SUDIRLON                 PIC S9(7)V9(2) VALUE +0 COMP-3.          
012900 01  WS-SUDIRMTRL                PIC S9(7)V9(2) VALUE +0 COMP-3.          
013000 01  WS-SUHEMT                   PIC S9(7)V9(2) VALUE +0 COMP-3.          
013100 01  WS-SUARTSTD                 PIC S9(9)V9(2) VALUE +0 COMP-3.          
013200                                                                          
013300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013400     88  INDATA-OK                           VALUE 'J'.                   
013500     88  INDATA-FEL                          VALUE 'N'.                   
013600                                                                          
013700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
013800     88  NYCKLAR-OK                          VALUE 'J'.                   
013900     88  NYCKLAR-FEL                         VALUE 'N'.                   
014000                                                                          
014100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014200     88  EGEN-MID                            VALUE '6119'.                
014300     88  GODK-MID                            VALUE '6111' '6112'          
014400                                                   '6113' '6114'          
014500                                                   '6115' '6116'          
014600                                                   '6118' '6119'.         
014700     88  HELP-MID                            VALUE '0551'.                
014800     EJECT                                                                
014900 01      WS.                                                              
015000*     -- SPARAT FRÅN SEGMENT                                              
015100                                                                          
015200  02     WS-ARTC.                                                         
015300                                                                          
015400   03    WS-ARTC.                                                         
015500    04   WS-ARTC-IDFTG         PIC 9(2)    VALUE ZERO.                    
015600    04   WS-ARTC-IDLEVNR       PIC  X(5)   VALUE SPACE.                   
015700    04   WS-ARTC-KDPRODSL      PIC S9(3)   VALUE ZERO COMP-3.             
015800    04   WS-ARTC-IDFKNGRP      PIC S9(5)   VALUE ZERO COMP-3.             
015900    04   WS-ARTC-KDPSLLOC      PIC S9(3)   VALUE ZERO COMP-3.             
016000    04   WS-ARTC-KDSORT        PIC  X(2)   VALUE SPACE.                   
016100                                                                          
016200    04   WS-ARTC-IDANSK        PIC S9(3)   VALUE ZERO COMP-3.             
016300    04   WS-ARTC-IDINK         PIC  9(5)   VALUE ZERO.                    
016400    04   WS-ARTC-IDINK-X       PIC  X(4)   VALUE SPACE.                   
016500    04   WS-ARTC-KDHF          PIC S9(1)   VALUE ZERO COMP-3.             
016600                                                                          
016700    04   WS-ARTC-KDTIPPR       PIC S9(1)   VALUE ZERO COMP-3.             
016800    04   WS-ARTC-KDVTH         PIC S9(1)   VALUE ZERO COMP-3.             
016900    04   WS-ARTC-PRARTBES      PIC S9(7)V9(2) VALUE ZERO COMP-3.          
017000    04   WS-ARTC-PRHEMTAG      PIC S9(7)V9(2) VALUE ZERO COMP-3.          
017100    04   WS-ARTC-KDVALISO      PIC X(3)    VALUE SPACE.                   
017200    04   WS-ARTC-PRDIRLON      PIC S9(4)V9(3) VALUE ZERO COMP-3.          
017300    04   WS-ARTC-PRDMTRL       PIC S9(6)V9(3) VALUE ZERO COMP-3.          
017400    04   WS-ARTC-PRINK         PIC S9(7)V9(2) VALUE ZERO COMP-3.          
017500    04   WS-ARTC-PRARTSTD      PIC S9(7)V9(2) VALUE ZERO COMP-3.          
017600    04   WS-ARTC-PROVRPAL      PIC S9(4)V9(3) VALUE ZERO COMP-3.          
017700    04   WS-ARTC-IDARTNR-EMBQ3 PIC S9(9)   VALUE ZERO COMP-3.             
017800                                                                          
017900    04   WS-ARTC-PRARTBEL-PR   PIC S9(8)V9(5) VALUE ZERO COMP-3.          
018000    04   WS-ARTC-PRARTBEL-SUM  PIC S9(8)V9(5) VALUE ZERO COMP-3.          
018100    04   WS-ARTC-PRARTBES-PR   PIC S9(7)V9(2) VALUE ZERO COMP-3.          
018200    04   WS-ARTC-CDLAGER  OCCURS 4.                                       
018300      05   WS-ARTC-ADLAGOMR-CD PIC  S9(3)     VALUE ZERO COMP-3.          
018400     SKIP2                                                                
018500*     -- DAGENS AAMMDD                                                    
018600  02     WS-TIAAMMDD             PIC 9(6)    VALUE ZERO.                  
018700  02     FILLER                  REDEFINES WS-TIAAMMDD.                   
018800   03    WS-TIAAMMDD-AA          PIC 9(2).                                
018900   03    WS-TIAAMMDD-MM          PIC 9(2).                                
019000   03    WS-TIAAMMDD-DD          PIC 9(2).                                
019100  02     DAGENS-DATUM              PIC 9(8).                              
019200  02     TRANS-TID                 PIC 9(9).                              
019300                                                                          
019400*     -- DAAVIDAT TILL SAP                                                
019500  02     WS-DAAVIDAT             PIC 9(8)    VALUE ZERO.                  
019600  02     FILLER REDEFINES WS-DAAVIDAT.                                    
019700   03    WS-DAAVIDAT-SEKEL       PIC 9(2).                                
019800   03    WS-DAAVIDAT-YYMMDD      PIC 9(6).                                
019900                                                                          
020000*     -- DAGENS AAVVD                                                     
020100  02     WS-TIAAVVD              PIC 9(5)    VALUE ZERO.                  
020200  02     FILLER                  REDEFINES WS-TIAAVVD.                    
020300   03    WS-TIAAVVD-AAVV         PIC 9(4).                                
020400   03    FILLER                  REDEFINES WS-TIAAVVD-AAVV.               
020500    04   WS-TIAAVVD-AA           PIC 9(2).                                
020600    04   WS-TIAAVVD-VV           PIC 9(2).                                
020700   03    WS-TIAAVVD-D            PIC 9(1).                                
020800                                                                          
020900*     -- TIAVIDAT MED SEKEL-SIFFRA                                        
021000  02     WS-INLA-TIAVIDAT        PIC 9(8)    VALUE ZERO.                  
021100  02     FILLER REDEFINES WS-INLA-TIAVIDAT.                               
021200   03    WS-TIAVIDAT-SEKEL       PIC 9(2).                                
021300   03    WS-INLA-INL-TIAVIDAT    PIC 9(6).                                
021400*                                                                         
021500*     -- DATE + TIME                                                      
021600  02     WS-TIAAAAMMDDTTMMSSTH     PIC 9(16)   VALUE ZERO.                
021700  02     FILLER                  REDEFINES WS-TIAAAAMMDDTTMMSSTH.         
021800   03    WS-TISEKEL               PIC 9(2).                               
021900   03    WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                               
022000   03    WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                               
022100*                                                                         
022200     03  WS-DATE-YYMMDD            PIC 9(06).                             
022300     03  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                          
022400         05  WS-DATE-YYMM          PIC 9(04).                             
022500         05  WS-DATE-DD            PIC 9(02).                             
022600                                                                          
022700*     -- IDLOPNRM I VALFRI FORM                                           
022800  02     WS-IDLOPNRM-AAVVDLLLLK  PIC 9(10)   VALUE ZERO.                  
022900  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
023000   03    WS-IDLOPNRM-AA          PIC 9(2).                                
023100   03    WS-IDLOPNRM-VVDLLLLK    PIC 9(8).                                
023200   03    FILLER                  REDEFINES WS-IDLOPNRM-VVDLLLLK.          
023300    04   WS-IDLOPNRM-VVDLLLL     PIC 9(7).                                
023400    04   FILLER                  REDEFINES WS-IDLOPNRM-VVDLLLL.           
023500     05  WS-IDLOPNRM-VV          PIC 9(2).                                
023600     05  FILLER                  PIC X(5).                                
023700    04   FILLER                  PIC X(1).                                
023800  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
023900   03    WS-IDLOPNRM-AAVVDLLLL   PIC 9(9).                                
024000   03    FILLER                  PIC X(1).                                
024100                                                                          
024200*     -- INLEVERANS-ID (9-KOMPLEMENT TILL DATE+TIME)                      
024300  02     WS-DAINLEV              PIC 9(16)   VALUE ZERO.                  
024400*     -- ARTC21-PRARTBES FÖRE UPPDAT (FÖR DEN SOM MAKULERAS)              
024500 02      WS-PRARTBES-MAK         PIC S9(7)V9(2)                           
024600                                             VALUE ZERO COMP-3.           
024700                                                                          
024800*     -- KVANT FÖR BOKNING LEVPLAN                                        
024900  02     WS-KV-LPLAN             PIC S9(7)   VALUE ZERO COMP-3.           
025000*     -- KVANT KVAR ATT BOKA I LEVPLAN                                    
025100  02     WS-KV-OBOK              PIC S9(7)   VALUE ZERO COMP-3.           
025200*     -- ÅTERBOKAD AVROPSKVANT I LEVPLAN                                  
025300  02     WS-KVAVROP-ATERBOK      PIC S9(7)   VALUE ZERO COMP-3.           
025400                                                                          
025500*     -- FÖR REDIG. AV IDAVINR FRÅN IDFS                                  
025600  02     WS-IDAVINR            PIC 9(7)      VALUE ZERO.                  
025700  02     FILLER                REDEFINES WS-IDAVINR.                      
025800   03    WS-IDAVINR-TKN        OCCURS 7 PIC 9(1).                         
025900*                                                                         
026000  02     WS-IDFS               PIC X(8)      VALUE SPACE.                 
026100  02     FILLER                REDEFINES WS-IDFS.                         
026200   03    WS-IDFS-TKN           OCCURS 8 PIC X(1).                         
026300*                                                                         
026400*     -- LÖPNUMMER I LOGGPOST FÖR ATT FÖRSÄKRA SIG OM UNIK NKL.           
026500  02     WS-IDLOGLOP             PIC S9(1)   VALUE ZERO COMP-3.           
026600                                                                          
026700*     -- LOGG-TRANSAR                                                     
026800  02     WS-ZZAC01.                                                       
026900   03    WS-ZZAC01-LOGGPOST      PIC X(90)   VALUE SPACE.                 
027000   03    FILLER                  REDEFINES WS-ZZAC01-LOGGPOST.            
027100    04   WS-ZZAC01-LOGGPOST-1-3  PIC X(3).                                
027200    04   WS-ZZAC01-LOGGPOST-4-90 PIC X(87).                               
027300   03    WS-ZZAC01-SORTPOST      PIC X(36)   VALUE SPACE.                 
027400                                                                          
027500 01  W-PRL-DADAT                 PIC 9(8)    VALUE ZERO.                  
027600     EJECT                                                                
027700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
027800 01  GENERELLA-SUBPROGRAM.                                                
027900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
028000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
028300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
028400     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
028500     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
028600     EJECT                                                                
028700*01 -COPY WMSGINIT                                                        
028800     SKIP3                                                                
028900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
029000*01 -COPY WMEDAREA                                                        
029100     SKIP3                                                                
029200 01  MESSAGE-CODES.                                                       
029300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
029400     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
029500     03  ERR-007-OTILLATEN-UPD   PIC X(3)    VALUE '007'.                 
029600     03  ERR-010-NOT-IN-REG      PIC X(3)    VALUE '025'.                 
029700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
029800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
029900     03  ERR-QTY-QUAL-IR-EXIST   PIC X(3)    VALUE '351'.                 
030000     03  ERR-TECH-QUAL-IR-EXIST  PIC X(3)    VALUE '352'.                 
030100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
030200     03  QUANT-TOO-BIG           PIC X(3)    VALUE '298'.                 
030300     03  SPL-ADV-QTY             PIC X(3)    VALUE '353'.                 
030400     EJECT                                                                
030500*    --- AREA FÖR WDATKONV                                                
030600 01  FILLER                    PIC X(16) VALUE 'WDATAREA********'.        
030700                                                                          
030800*01  -COPY WDATAREA                                                       
030900     EJECT                                                                
031000                                                                          
031100*    --- AREOR FÖR LOGGTRANSAR                                            
031200 01  FILLER                    PIC X(16) VALUE 'W211R31*********'.        
031300                                                                          
031400*01  -COPY W611R32  -PRE W611R32-                                         
031500     EJECT                                                                
031600 01  FILLER                    PIC X(16) VALUE 'R320-W211310****'.        
031700                                                                          
031800*01  -COPY W211310  -PRE R320-                                            
031900     EJECT                                                                
032000 01  FILLER                    PIC X(16) VALUE 'W211FEL*********'.        
032100                                                                          
032200*01  -COPY W211FEL  -PRE W211FEL-                                         
032300     EJECT                                                                
032400 01  FILLER                    PIC X(16) VALUE 'W211M103********'.        
032500                                                                          
032600*01  -COPY W211M103 -PRE M103-                                            
032700     EJECT                                                                
032800 01  FILLER                    PIC X(16) VALUE 'W211M109********'.        
032900                                                                          
033000*01  -COPY W211M109 -PRE M109-                                            
033100     EJECT                                                                
033200*                                                                         
033300*01  -COPY WMFSAREA                                                       
033400     EJECT                                                                
033500                                                                          
033600*    --- AREA FÖR W510AVG                                                 
033700 01  FILLER                    PIC X(16) VALUE 'W510AVGAREA*****'.        
033800*01  -COPY W510AVG                                                        
033900     EJECT                                                                
034000*    --- AREA FÖR W510CURR                                                
034100*01  -COPY W510CURR                                                       
034200     EJECT                                                                
034300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
034400*                                                                         
034500     SKIP2                                                                
034600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
034700     SKIP2                                                                
034800 01  NYCKLAR-TILL-DLI.                                                    
034900                                                                          
035000*    -- W6D1 INDEXBAS B. MIN O MAX.                                       
035100     03  W-IDARTNR-X.                                                     
035200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
035300                                                                          
035400     03  W-IDRADNR-INL-X.                                                 
035500         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
035600                                                                          
035700     03  W-IDINLEV-X.                                                     
035800         05  W-IDINLEV           PIC S9(15)  VALUE ZERO COMP-3.           
035900                                                                          
036000     03  W-KDAVROP-X.                                                     
036100         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
036200                                                                          
036300     03  W-WDD905KY-X.                                                    
036400         05  W-DAAVROP-X.                                                 
036500             07  W-DAAVROP       PIC  9(6)    VALUE ZERO.                 
036600         05  W-TILEVDAG-X.                                                
036700             07  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.          
036800                                                                          
036900     03  W-WDD901KY-X.                                                    
037000         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
037100         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
037200                                                                          
037300     03  W-INLB11-IDLEVNR-X.                                              
037400         05  W-INLB11-IDLEVNR    PIC  X(5)   VALUE SPACE.                 
037500                                                                          
037600     03  W-INLB31-IDLOPNRM-X.                                             
037700         05  W-INLB31-IDLOPNRM   PIC S9(9)   VALUE ZERO COMP-3.           
037800                                                                          
037900     03  W-IDLOPNRM-X.                                                    
038000         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
038100                                                                          
038200     03  W-IDLEVNR-X.                                                     
038300         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
038400                                                                          
038500     03  W-IDLEVNR-21-X.                                                  
038600         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
038700                                                                          
038800     03  W-IDLEVNR-PR-X.                                                  
038900         05  W-IDLEVNR-PR        PIC X(5)    VALUE SPACE.                 
039000                                                                          
039100     03  W-DAPRLIST-K7-N.                                                 
039200         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
039300                                                                          
039400     03  W-IDLAND-K7-X.                                                   
039500         05  W-IDLAND-K7         PIC X(2)    VALUE SPACE.                 
039600     03  W-IDLAND-X.                                                      
039700         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
039800                                                                          
039900     03  W-W6D101KY-X.                                                    
040000         05  W-W6D101KY-IDDC     PIC X(2)    VALUE SPACE.                 
040100         05  W-W6D101KY-IDLEVNR  PIC  X(5)   VALUE SPACE.                 
040200         05  W-W6D101KY-IDFS     PIC X(8)    VALUE SPACE.                 
040300         05  W-W6D101KY-TIAVIDAT PIC S9(7)   VALUE ZERO COMP-3.           
040400                                                                          
040500     03  W-IDDC-X.                                                        
040600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
040700                                                                          
040800     03  W-IDDC-K7-X.                                                     
040900         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
041000                                                                          
041100     03  W-6003-KEY-X.                                                    
041200         05  FILLER              PIC X(4)    VALUE '6003'.                
041300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
041400                                                                          
041500     03  W-W6H7CSEQ-X.                                                    
041600         05 W-IDLOPNRM-H7        PIC S9(9)   VALUE ZERO COMP-3.           
041700         05 W-DAAVSDAT-H7        PIC  9(8)   VALUE ZERO.                  
041800                                                                          
041900*--------FYSISK NKL TILL LASA                                             
042000     03  WL-W6GX01KY-X.                                                   
042100         05  WL-IDHTYP           PIC X(4)    VALUE '6107'.                
042200         05  WL-IDDC             PIC X(2)    VALUE SPACE.                 
042300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
042400                                                                          
042500     03  WL-W6GX11KY-X.                                                   
042600         05  WL-IDLBBET          PIC X(12)   VALUE SPACE.                 
042700         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
042800                                                                          
042900*--------SÖK NKL 1 TILL LASA-G121                                         
043000                                                                          
043100     03  WLS1-IDLEVNR-X.                                                  
043200         05  WLS1-IDLEVNR        PIC  X(5)   VALUE SPACE.                 
043300                                                                          
043400     03  WLS1-IDFS-X.                                                     
043500         05  WLS1-IDFS           PIC X(8).                                
043600                                                                          
043700     03  WLS1-IDARTNR-X.                                                  
043800         05  WLS1-IDARTNR        PIC S9(9)   COMP-3.                      
043900                                                                          
044000     03  WLS1-TIAVIDAT-X.                                                 
044100         05  WLS1-TIAVIDAT       PIC S9(7)   COMP-3.                      
044200                                                                          
044300     03  W-IDDC-B6-X.                                                     
044400         05 W-IDDC-B6                  PIC X(2).                          
044500     03  W-IDDC-B6-ART-X.                                                 
044600         05 W-IDDC-B6-ART              PIC X(2).                          
044700     03  W-IDDC-B6-LEV-X.                                                 
044800         05 W-IDDC-B6-LEV              PIC X(5).                          
044900     03  W-IDLANDX2-X.                                                    
045000         05    W-IDLANDX2              PIC X(2)    VALUE SPACE.           
045100     03  W-WDGX9305-X.                                                    
045200         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
045300         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
045400         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
045500         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
045600     03  W-KDVALISO-X.                                                    
045700         05  W-KDVALISO-ROW      PIC X(3)    VALUE SPACE.                 
045800     03  W-TISTADA9-X.                                                    
045900         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
046000     EJECT                                                                
046100                                                                          
046200     EJECT                                                                
046300*    --- STATUS-KOD FRÅN IMS                                              
046400 01  STATUS-WS                   PIC XX.                                  
046500     88  SEGMENT-FINNS                       VALUE '  '.                  
046600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
046700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
046800     88  BASEN-SLUT                          VALUE 'GE'.                  
046900     SKIP2                                                                
047000 01  GODK-STATUSKODER.                                                    
047100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
047200     SKIP3                                                                
047300 01  SSA1                        PIC X(128).                              
047400 01  SSA2                        PIC X(64).                               
047500 01  SSA3                        PIC X(64).                               
047600     EJECT                                                                
047700*    --- IMS FUNKTIONSKODER                                               
047800*01  -COPY W0003                                                          
047900     EJECT                                                                
048000*    ---  DLI INPUT-OUTPUT AREA                                           
048100                                                                          
048200*         DLI-IO-AREA                                                     
048300*                        WLARTC01,-11,-21                                 
048400*                        WLINLB11,                                        
048500*                        WLINLE21                                         
048600*                        WLZZAC01,                                        
048700*                        W6INLA21                                         
048800*                        W6INLC01                                         
048900                                                                          
049000*         DLI-IO-AREA2   W6INLA01                                         
049100                                                                          
049200*         DLI-IO-AREA3   W6INLA11                                         
049300                                                                          
049400*         DLI-IO-AREA4   WLINLB31                                         
049500                                                                          
049600*         DLI-IO-AREA6   WLINLB23+31 (PATH-CALL)                          
049700                                                                          
049800*         DLI-IO-AREA7   W6LASA                                           
049900                                                                          
050000*         DLI-IO-AREA8   W6KVAE                                           
050100     EJECT                                                                
050200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
050300     SKIP3                                                                
050400 01  DLI-IO-AREA.                                                         
050500     03  IO-AREA                 PIC X(800)  VALUE SPACE.                 
050600     SKIP3                                                                
050700     03  WLINLB11 REDEFINES IO-AREA.                                      
050800*        05  -COPY WDD902  -PRE INLB11-                                   
050900     EJECT                                                                
051000     03  WLZZAC01 REDEFINES IO-AREA.                                      
051100*        05  -COPY WDG601  -PRE ZZAC01-                                   
051200     EJECT                                                                
051300     03  W6INLA21 REDEFINES IO-AREA.                                      
051400*        05  -COPY W6D121  -PRE INLA-                                     
051500     EJECT                                                                
051600     03  W6INLC01 REDEFINES IO-AREA.                                      
051700*        05  -COPY W6D1B1  -PRE INLC-                                     
051800     EJECT                                                                
051900 01  FILLER                      PIC X(16)  VALUE 'WDK623'.               
052000                                                                          
052100*01  WLARTC23 -COPY WDK623                                                
052200     EJECT                                                                
052300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL2'.                        
052400 01  DLI-IO-WDL2.                                                         
052500     03  IO-WDL2                 PIC X(600)  VALUE SPACE.                 
052600     SKIP3                                                                
052700     03  WLINLE11 REDEFINES IO-WDL2.                                      
052800*        05  -COPY WDL201  -PRE INLE-                                     
052900     EJECT                                                                
053000     03  WLINLE11 REDEFINES IO-WDL2.                                      
053100*        05  -COPY WDL211  -PRE INLE-                                     
053200     EJECT                                                                
053300     03  WLINLE21 REDEFINES IO-WDL2.                                      
053400*        05  -COPY WDL221  -PRE INLE-                                     
053500     EJECT                                                                
053600*    ---  DLI INPUT-OUTPUT AREA 2                                         
053700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
053800     SKIP3                                                                
053900 01  DLI-IO-AREA2.                                                        
054000     03  IO-AREA2                PIC X(100)  VALUE SPACE.                 
054100     SKIP3                                                                
054200     03  W6INLA01 REDEFINES IO-AREA2.                                     
054300*        05  -COPY W6D101  -PRE INLA-                                     
054400     EJECT                                                                
054500*    ---  DLI INPUT-OUTPUT AREA 3                                         
054600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
054700     SKIP3                                                                
054800 01  DLI-IO-AREA3.                                                        
054900     03  IO-AREA3                PIC X(200)  VALUE SPACE.                 
055000     SKIP3                                                                
055100     03  W6INLA11 REDEFINES IO-AREA3.                                     
055200*        05  -COPY W6D111  -PRE INLA-                                     
055300     EJECT                                                                
055400*    ---  DLI INPUT-OUTPUT AREA 4                                         
055500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
055600     SKIP3                                                                
055700 01  DLI-IO-AREA4.                                                        
055800     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
055900     SKIP3                                                                
056000     03  WLINLB31 REDEFINES IO-AREA4.                                     
056100*        05  -COPY WDD906  -PRE INLB31-                                   
056200     EJECT                                                                
056300*    ---  DLI INPUT-OUTPUT AREA 6  KONKATINERADE SEGMENT (PATH)           
056400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
056500     SKIP3                                                                
056600 01  DLI-IO-AREA6.                                                        
056700     SKIP3                                                                
056800     03  WLINLB23-PATH.                                                   
056900*        05  -COPY WDD905  -PRE INLB23P-                                  
057000     EJECT                                                                
057100     03  WLINLB31-PATH.                                                   
057200*        05  -COPY WDD906  -PRE INLB31P-                                  
057300     EJECT                                                                
057400*    ---  DLI INPUT-OUTPUT AREA 7                                         
057500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA7'.        
057600     SKIP3                                                                
057700 01  DLI-IO-AREA7.                                                        
057800     03  IO-AREA7.                                                        
057900         05   -COPY W6GX6110                                              
058000     SKIP3                                                                
058100*    ---  DLI INPUT-OUTPUT AREA 8                                         
058200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA8'.        
058300     SKIP3                                                                
058400 01  DLI-IO-AREA8.                                                        
058500     03  IO-AREA8.                                                        
058600         05   -COPY W6H701  -PRE KVAE-                                    
058700     EJECT                                                                
058800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-INLC01'.         
058900     SKIP3                                                                
059000 01  DLI-IO-AREA-INLC01.                                                  
059100     03  -COPY WDL601  -PRE INLC-                                         
059200     EJECT                                                                
059300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-INLC11'.         
059400     SKIP3                                                                
059500 01  DLI-IO-AREA-INLC11.                                                  
059600     03  -COPY WDL611  -PRE INLC-                                         
059700     EJECT                                                                
059800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-INLC12'.         
059900     SKIP3                                                                
060000 01  DLI-IO-AREA-INLC12.                                                  
060100     03  -COPY WDL612  -PRE INLC-                                         
060200     EJECT                                                                
060300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ARTS01'.         
060400     SKIP3                                                                
060500 01  DLI-IO-AREA-ARTS01.                                                  
060600     03  -COPY WDK701  -PRE ARTS-                                         
060700     EJECT                                                                
060800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ARTS11'.         
060900     SKIP3                                                                
061000 01  DLI-IO-AREA-ARTS11.                                                  
061100     03  -COPY WDK711  -PRE ARTS-                                         
061200     EJECT                                                                
061300                                                                          
061400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
061500     SKIP3                                                                
061600 01  DLI-IO-WDK711.                                                       
061700     03  -COPY WDK711                                                     
061800                                                                          
061900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK712'.         
062000     SKIP3                                                                
062100 01  DLI-IO-WDK712.                                                       
062200     03  -COPY WDK712                                                     
062300                                                                          
062400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK723'.         
062500     SKIP3                                                                
062600 01  DLI-IO-WDK723.                                                       
062700     03  -COPY WDK723                                                     
062800                                                                          
062900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK724'.         
063000     SKIP3                                                                
063100 01  DLI-IO-WDK724.                                                       
063200     03  -COPY WDK724                                                     
063300     EJECT                                                                
063400                                                                          
063500 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
063600*01  WLLOGA01    -COPY WDL901                                             
063700     EJECT                                                                
063800 01  DLI-IO-AREA-FILB01.                                                  
063900*    05  -COPY WDR801  -PRE EKO-                                          
064000       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
064100         09  -COPY W51080 -PRE EKO-                                       
064200       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
064300         09  -COPY W510A11 -PRE LAB-                                      
064400       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
064500         09  -COPY W510EKHA -PRE EKO-                                     
064600     EJECT                                                                
064700 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
064800 01  DLI-IO-WLSAPA01.                                                     
064900*    03  WLSAPA01  -COPY WDR901                                           
065000*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
065100     EJECT                                                                
065200 01  DLI-IO-WDK6.                                                         
065300     03  IO-WDK6                 PIC X(900)  VALUE SPACE.                 
065400     SKIP3                                                                
065500     03  WLARTC01 REDEFINES IO-WDK6.                                      
065600*        05  -COPY WDK601  -PRE ARTC-                                     
065700     EJECT                                                                
065800     03  WLARTC11 REDEFINES IO-WDK6.                                      
065900*        05  -COPY WDK611  -PRE ARTC-                                     
066000     EJECT                                                                
066100     03  WLARTC21 REDEFINES IO-WDK6.                                      
066200*        05  -COPY WDK621  -PRE ARTC-                                     
066300                                                                          
066400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
066500 01   DLI-IO-AREA-B601.                                                   
066600*     03  -COPY WDB601                                                    
066700                                                                          
066800 01  FILLER               PIC X(16)   VALUE 'WDB601 ART'.                 
066900 01   DLI-IO-AREA-B601-ART.                                               
067000*     03  -COPY WDB601   -PRE ART-                                        
067100                                                                          
067200 01  FILLER               PIC X(16)   VALUE 'WDB601 LEV'.                 
067300 01   DLI-IO-AREA-B601-LEV.                                               
067400*     03  -COPY WDB601   -PRE LEV-                                        
067500                                                                          
067600 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
067700 01   DLI-IO-AREA-B617.                                                   
067800*     03  -COPY WDB617                                                    
067900     EJECT                                                                
068000 01  DLI-IO-AREA-F1          PIC X(100).                                  
068100     SKIP2                                                                
068200*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
068300     EJECT                                                                
068400                                                                          
068500 01  DLI-IO-AREA-F102        PIC X(100).                                  
068600     SKIP2                                                                
068700*01  WLLEVA11 -COPY WDF102 -PRE F102-     -RED DLI-IO-AREA-F102           
068800     EJECT                                                                
068900                                                                          
069000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
069100 01  DLI-IO-WDGX9306.                                                     
069200*    03  -COPY WDGX9306                                                   
069300     EJECT                                                                
069400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
069500 01  DLI-IO-WDGX9308.                                                     
069600*    03  -COPY WDGX9308                                                   
069700                                                                          
069800 LINKAGE SECTION.                                                         
069900 01  REQU-AREA.                                                           
070000*    03 -COPY WZ01REQU                                                    
070100*    03 -COPY W60119I1                                                    
070200     EJECT                                                                
070300 01  RESP-AREA.                                                           
070400*    03 -COPY WZ01RESP                                                    
070500*    03 -COPY W60119O1                                                    
070600     EJECT                                                                
070700                                                                          
070800*01  -COPY W0009   -PRE MSG-                                              
070900     EJECT                                                                
071000*01  -COPY W0008  -PRE INLA-                                              
071100     05  FILLER                  PIC X.                                   
071200                                                                          
071300*01  -COPY W0008  -PRE INLC-                                              
071400     05  FILLER                  PIC X.                                   
071500     EJECT                                                                
071600*01  -COPY W0008  -PRE ARTC-                                              
071700     05  FILLER                  PIC X.                                   
071800                                                                          
071900*01  -COPY W0008  -PRE INLB-                                              
072000     05  FILLER                  PIC X.                                   
072100     EJECT                                                                
072200*01  -COPY W0008  -PRE INLE-                                              
072300     05  FILLER                  PIC X.                                   
072400     EJECT                                                                
072500*01  -COPY W0008  -PRE ZZAC-                                              
072600     05  FILLER                  PIC X.                                   
072700                                                                          
072800*01  -COPY W0008  -PRE LASA-                                              
072900     05  FILLER                  PIC X.                                   
073000                                                                          
073100*01  -COPY W0008  -PRE KVAE-                                              
073200     05  FILLER                  PIC X.                                   
073300                                                                          
073400*01  -COPY W0008  -PRE ARTS-                                              
073500     05  FILLER                  PIC X.                                   
073600                                                                          
073700*01  -COPY W0008  -PRE INLC-INL-                                          
073800     05  FILLER                  PIC X.                                   
073900     EJECT                                                                
074000*01  -COPY W0008  -PRE LOGA-                                              
074100     05  FILLER                  PIC X.                                   
074200     EJECT                                                                
074300*01  -COPY W0008  -PRE FILB-                                              
074400     05  FILLER                  PIC X.                                   
074500     EJECT                                                                
074600*01  -COPY W0008  -PRE SAPA-                                              
074700     05  FILLER                  PIC X.                                   
074800     EJECT                                                                
074900*01  -COPY W0008  -PRE WDG2-                                              
075000     05  FILLER                  PIC X.                                   
075100     EJECT                                                                
075200*01  -COPY W0008  -PRE 9305-AVG-                                          
075300     05  FILLER                  PIC X.                                   
075400     EJECT                                                                
075500*01  -COPY W0008  -PRE AVG-WDB6-                                          
075600     05  FILLER                  PIC X.                                   
075700     EJECT                                                                
075800*01  -COPY W0008  -PRE WDB6-LEV-                                          
075900     05  FILLER                  PIC X.                                   
076000     EJECT                                                                
076100*01  -COPY W0008  -PRE WDB6-                                              
076200     05  FILLER                  PIC X.                                   
076300     EJECT                                                                
076400*01  -COPY W0008  -PRE WDK7-                                              
076500     05  FILLER                  PIC X.                                   
076600     EJECT                                                                
076700*01  -COPY W0008  -PRE WDF1-                                              
076800     05  FILLER                  PIC X.                                   
076900     EJECT                                                                
077000*01  -COPY W0008  -PRE 9305-                                              
077100     05  FILLER                  PIC X.                                   
077200                                                                          
077300 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA                            
077400                     MSG-PCB           INLC-PCB                           
077500                     INLA-PCB ARTC-PCB INLB-PCB                           
077600                     INLE-PCB ZZAC-PCB                                    
077700                     LASA-PCB KVAE-PCB ARTS-PCB                           
077800                     INLC-INL-PCB LOGA-PCB                                
077900                     FILB-PCB SAPA-PCB WDG2-PCB 9305-AVG-PCB              
078000                     AVG-WDB6-PCB WDB6-LEV-PCB WDB6-PCB                   
078100                     WDK7-PCB WDF1-PCB 9305-PCB.                          
078200                                                                          
078300     PERFORM A-INIT                                                       
078400     PERFORM B-KOLLA-NYCKLAR                                              
078500     IF NYCKLAR-OK                                                        
078600       IF REQU-UPDATE                                                     
078700         PERFORM G-KOLLA-INPUT                                            
078800         IF INDATA-OK                                                     
078900           PERFORM H-UPPDATERA                                            
079000         END-IF                                                           
079100       ELSE                                                               
079200         IF REQU-UPD-V                                                    
079300           PERFORM I-KOLLA-INPUT                                          
079400           IF INDATA-OK                                                   
079500             PERFORM J-UPPDATERA-R32                                      
079600           END-IF                                                         
079700         ELSE                                                             
079800           PERFORM F-LAES-VISA-INFO                                       
079900           IF INDATA-OK                                                   
080000             IF REQU-FIRST                                                
080100               MOVE REQU-IDLOPNRM-KEY  TO WS-IDLOPNRM                     
080200             ELSE                                                         
080300               IF REQU-QUERY                                              
080400                 PERFORM E-SAMMA-SIDA                                     
080500               END-IF                                                     
080600             END-IF                                                       
080700           END-IF                                                         
080800         END-IF                                                           
080900       END-IF                                                             
081000     END-IF                                                               
081100                                                                          
081200     MOVE ZERO TO RETURN-CODE                                             
081300     GOBACK                                                               
081400     .                                                                    
081500     EJECT                                                                
081600 A-INIT SECTION.                                                          
081700     MOVE FUNCTION CURRENT-DATE (3:4) TO WS-DATE-YYMM                     
081800     MOVE 01                          TO WS-DATE-DD                       
081900     ACCEPT DAGENS-DATUM       FROM DATE                                  
082000                                                                          
082100     MOVE ALL '+'     TO RESP-W60119O1                                    
082200*    -- SET VALID ATTRIBUTE VALUES (DESTROYED ABOVE)                      
082300     PERFORM RESP-FORM-ATTR                                               
082400     MOVE 001         TO RESP-IDMSGVER                                    
082500     MOVE SPACE       TO RESP-IDMSG-ERROR                                 
082600                         RESP-IDMSG-INFO                                  
082700                         RESP-IDELMT-ERROR                                
082800                                                                          
082900     MOVE 'IDAG'                 TO DAT-KDDATFORM                         
083000     CALL WDATKONV USING  DAT-KDDATFORM                                   
083100                          DAT-I-TIDATUM                                   
083200                          DAT-O-TIDATUM                                   
083300                          DAT-KDSVAR                                      
083400                                                                          
083500     IF  DAT-KDSVAR-OK                                                    
083600       MOVE DAT-TISEKEL          TO WS-DAINLINL-SEKEL                     
083700       MOVE DAT-TIAAMMDD         TO WS-TIAAMMDD                           
083800                                    WS-DAINLINL-AAMMDD                    
083900       MOVE DAT-TIAAVVD          TO WS-TIAAVVD                            
084000     ELSE                                                                 
084100       STRING 'FEL FRÅN WDATKONV:' DAT-KDSVAR                             
084200         DELIMITED BY SIZE INTO FELTEXT                                   
084300       CALL FELLOG                                                        
084400     END-IF                                                               
084500     .                                                                    
084600     EJECT                                                                
084700*----------------------------------------------------------------*        
084800 B-KOLLA-NYCKLAR SECTION.                                                 
084900                                                                          
085000     MOVE JA TO NYCKLAR-SW                                                
085100                                                                          
085200     PERFORM BA-KONTROLL-AV-IDLOPNRM                                      
085300     PERFORM BB-KONTROLL-AV-IDDC                                          
085400                                                                          
085500     IF NYCKLAR-OK                                                        
085600       MOVE WS-IDLOPNRM     TO W-IDLOPNRM                                 
085700                                                                          
085800       MOVE DCS-IDDC        TO W-IDDC                                     
085900                               W-IDDC-K7                                  
086000                               W-W6D101KY-IDDC                            
086100                                                                          
086200     ELSE                                                                 
086300       MOVE NEJ            TO NYCKLAR-SW                                  
086400       MOVE ERR-WRONG-KEY  TO RESP-IDMSG-ERROR                            
086500     END-IF                                                               
086600     .                                                                    
086700     EJECT                                                                
086800 BA-KONTROLL-AV-IDLOPNRM    SECTION.                                      
086900     MOVE REQU-IDLOPNRM-KEY TO WS-IDLOPNRM                                
087000     IF WS-IDLOPNRM NUMERIC AND WS-IDLOPNRM > ZERO                        
087100       CONTINUE                                                           
087200     ELSE                                                                 
087300       MOVE 'IDLOPNRM' TO RESP-IDELMT-ERROR                               
087400       MOVE NEJ TO NYCKLAR-SW                                             
087500     END-IF                                                               
087600     .                                                                    
087700     EJECT                                                                
087800 BB-KONTROLL-AV-IDDC        SECTION.                                      
087900     MOVE REQU-IDDC-KEY   TO W-IDDC-B6                                    
088000                             WS-IDDC                                      
088100     PERFORM IMS-GU-WDB601                                                
088200                                                                          
088300     IF DCS-CDC OR DCS-CDC-TR OR DCS-NDC-NA OR DCS-AUSTRALIA              
088400     OR DCS-JAPAN OR DCS-LAND-NON-VCC-OWNED                               
088500       IF REQU-UPD-V                                                      
088600         IF DCS-CDC OR DCS-CDC-TR OR DCS-AUSTRALIA OR DCS-JAPAN           
088700         OR DCS-USA OR DCS-LAND-NON-VCC-OWNED                             
088800           CONTINUE                                                       
088900         ELSE                                                             
089000           MOVE NEJ      TO NYCKLAR-SW                                    
089100           MOVE SPACE    TO DCS-IDDC                                      
089200           MOVE 'IDDC'   TO RESP-IDELMT-ERROR                             
089300         END-IF                                                           
089400       END-IF                                                             
089500     ELSE                                                                 
089600       MOVE NEJ      TO NYCKLAR-SW                                        
089700       MOVE SPACE    TO DCS-IDDC                                          
089800       MOVE 'IDDC'   TO RESP-IDELMT-ERROR                                 
089900     END-IF                                                               
090000     .                                                                    
090100     EJECT                                                                
090200 E-SAMMA-SIDA SECTION.                                                    
090300     IF EGEN-MID OR HELP-MID                                              
090400       IF REQU-INPUT = ALL '+'                                            
090500         CONTINUE                                                         
090600       ELSE                                                               
090700         PERFORM EA-REQU-INDATA-TILL-RESP                                 
090800         MOVE NEJ TO INDATA-SW                                            
090900         MOVE INF-PRESS-PF11       TO RESP-IDMSG-INFO                     
091000       END-IF                                                             
091100     END-IF                                                               
091200     .                                                                    
091300     EJECT                                                                
091400 EA-REQU-INDATA-TILL-RESP SECTION.                                        
091500     IF REQU-FLMAK = ALL '+'                                              
091600       MOVE SPACE                 TO RESP-FLMAK                           
091700     ELSE                                                                 
091800       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLMAK-ATTR                      
091900       MOVE REQU-FLMAK            TO RESP-FLMAK                           
092000     END-IF                                                               
092100                                                                          
092200     IF REQU-FLBACK = ALL '+'                                             
092300       MOVE MFS-RENSA-FAELT       TO RESP-FLBACK                          
092400     ELSE                                                                 
092500       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLBACK-ATTR                     
092600       MOVE REQU-FLBACK           TO RESP-FLBACK                          
092700     END-IF                                                               
092800     .                                                                    
092900     EJECT                                                                
093000 F-LAES-VISA-INFO SECTION.                                                
093100                                                                          
093200     PERFORM S10-KTRL-INLA                                                
093300                                                                          
093400     IF INLA-ART-FLSPLPART = JA                                           
093500       MOVE SPL-ADV-QTY TO RESP-IDMSG-INFO                                
093600     END-IF                                                               
093700     .                                                                    
093800     EJECT                                                                
093900 G-KOLLA-INPUT SECTION.                                                   
094000     MOVE JA  TO INDATA-SW                                                
094100     IF REQU-INPUT = ALL '+'                                              
094200       MOVE ERR-PF11-AND-NO-DATA  TO RESP-IDMSG-ERROR                     
094300       MOVE NEJ                   TO INDATA-SW                            
094400     ELSE                                                                 
094500       IF REQU-FLMAK NOT = ALL '+'                                        
094600         IF REQU-FLMAK  = JA OR YES                                       
094700           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLMAK-ATTR                   
094800         ELSE                                                             
094900           MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLMAK-ATTR                   
095000           MOVE REQU-FLMAK           TO RESP-FLMAK                        
095100           MOVE NEJ                  TO INDATA-SW                         
095200         END-IF                                                           
095300       ELSE                                                               
095400         MOVE MFS-ALFA-FAELT-FEL     TO RESP-FLMAK-ATTR                   
095500         MOVE REQU-FLMAK             TO RESP-FLMAK                        
095600         MOVE NEJ                    TO INDATA-SW                         
095700       END-IF                                                             
095800                                                                          
095900       IF INDATA-FEL                                                      
096000         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
096100       ELSE                                                               
096200         PERFORM S10-KTRL-INLA                                            
096300         IF INDATA-OK                                                     
096400           PERFORM S12-KTRL-KVAE                                          
096500         END-IF                                                           
096600       END-IF                                                             
096700     END-IF                                                               
096800     .                                                                    
096900     EJECT                                                                
097000 H-UPPDATERA SECTION.                                                     
097100                                                                          
097200     PERFORM S04-KONVERTERA-IDFS-IDAVINR                                  
097300     PERFORM HB-UPPD-INLE-INLC-HIST                                       
097400     IF DCS-CDC OR DCS-CDC-TR OR DCS-NDC-CN OR DCS-USA                    
097500       PERFORM S01-UPPD-INLB-LEVPL                                        
097600     END-IF                                                               
097700     PERFORM HE-UPPD-ARTC-ARTS                                            
097800     PERFORM HF-UPPD-ZZAC-LOGG                                            
097900     PERFORM HH-UPPD-INLA                                                 
098000     PERFORM HI-UPPD-LASA                                                 
098100                                                                          
098200     MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                              
098300     .                                                                    
098400     EJECT                                                                
098500 HB-UPPD-INLE-INLC-HIST SECTION.                                          
098600                                                                          
098700     IF DCS-CDC OR DCS-CDC-TR                                             
098800       PERFORM HBA-UPPD-INLE-HIST                                         
098900     ELSE                                                                 
099000       PERFORM HBB-UPPD-INLC-HIST                                         
099100     END-IF                                                               
099200     .                                                                    
099300     EJECT                                                                
099400 HBA-UPPD-INLE-HIST SECTION.                                              
099500     SKIP2                                                                
099600     MOVE INLA-ART-IDARTNR       TO W-IDARTNR                             
099700     MOVE INLA-ART-IDRADNR-INL   TO W-IDRADNR-INL                         
099800     MOVE INLA-ART-IDLOPNRM      TO W-IDLOPNRM                            
099900     PERFORM IMS-GHU-INLE-MOT                                             
100000                                                                          
100100                                                                          
100200     IF  SEGMENT-FINNS                                                    
100300       MOVE INLE-MOT-KVANTMOT    TO WS-INLE-MOT-KVANTMOT                  
100400       MOVE 'R32'                TO INLE-MOT-IDPTYP                       
100500       MOVE 2                    TO INLE-MOT-KDAVVANT                     
100600       MOVE WS-TIAAMMDD          TO INLE-MOT-TIUPPDAT                     
100700       PERFORM IMS-REPL-INLE                                              
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100 HBB-UPPD-INLC-HIST SECTION.                                              
101200     SKIP2                                                                
101300     MOVE INLA-ART-IDARTNR       TO W-IDARTNR                             
101400     MOVE INLA-ART-IDRADNR-INL   TO W-IDRADNR-INL                         
101500     MOVE INLA-ART-IDLOPNRM      TO W-IDLOPNRM                            
101600     PERFORM IMS-GU-INLC-ART                                              
101700                                                                          
101800     PERFORM HBBA-UPPD-HISTORIK                                           
101900     PERFORM HBBB-UPPD-ORDER                                              
102000     .                                                                    
102100     EJECT                                                                
102200 HBBA-UPPD-HISTORIK SECTION.                                              
102300     SKIP2                                                                
102400     PERFORM IMS-GHNP-INLC-INL                                            
102500                                                                          
102600     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
102700                   INLC-INL-IDLOPNRM = W-IDLOPNRM                         
102800       PERFORM IMS-GHNP-INLC-INL                                          
102900     END-PERFORM                                                          
103000                                                                          
103100     IF  SEGMENT-FINNS                                                    
103200     AND INLC-INL-IDLOPNRM = W-IDLOPNRM                                   
103300       MOVE 'R32'                TO INLC-INL-IDPTYP                       
103400       IF NDC-CN                                                          
103500         MOVE 2                    TO INLC-INL-KDAVVANT                   
103600       END-IF                                                             
103700       MOVE JA                   TO INLC-INL-FLMAKUL                      
103800       MOVE WS-TIAAMMDD          TO INLC-INL-TIINLINL                     
103900       PERFORM IMS-REPL-INLC-INL                                          
104000     END-IF                                                               
104100     .                                                                    
104200     EJECT                                                                
104300 HBBB-UPPD-ORDER SECTION.                                                 
104400     SKIP2                                                                
104500     MOVE +0   TO WS-KVBEART                                              
104600                                                                          
104700     PERFORM IMS-GU-INLC-ART                                              
104800     PERFORM IMS-GHNP-INLC-ORD                                            
104900                                                                          
105000     PERFORM UNTIL SEGMENT-SAKNAS                                         
105100                                                                          
105200       IF INLC-ORD-IDLOPNRM = W-IDLOPNRM                                  
105300           IF INLC-ORD-IDKUNDRF > +0                                      
105400               MOVE +0        TO INLC-ORD-IDLOPNRM                        
105500                                 INLC-ORD-KVAVIS                          
105600                                                                          
105700               ADD INLC-ORD-KVBEART TO WS-KVBEART                         
105800                                                                          
105900               PERFORM IMS-REPL-INLC-ORD                                  
106000           ELSE                                                           
106100               PERFORM IMS-DLET-INLC-ORD                                  
106200           END-IF                                                         
106300       END-IF                                                             
106400       PERFORM IMS-GHNP-INLC-ORD                                          
106500     END-PERFORM                                                          
106600     .                                                                    
106700     EJECT                                                                
106800 HE-UPPD-ARTC-ARTS SECTION.                                               
106900                                                                          
107000*    -- ARTC01                                                            
107100     PERFORM IMS-GU-ARTC01                                                
107200     MOVE ARTC-ART-IDFTG       TO WS-ARTC-IDFTG                           
107300     MOVE ARTC-ART-IDLEVNR     TO WS-ARTC-IDLEVNR                         
107400     MOVE ARTC-ART-KDPRODSL    TO WS-ARTC-KDPRODSL                        
107500     MOVE ARTC-ART-IDFKNGRP    TO WS-ARTC-IDFKNGRP                        
107600     MOVE ARTC-ART-KDSORT      TO WS-ARTC-KDSORT                          
107700                                                                          
107800*    -- ARTC11                                                            
107900     PERFORM IMS-GHNP-ARTC11                                              
108000                                                                          
108100     IF DCS-CDC OR DCS-CDC-TR                                             
108200       IF  INLA-ART-KDRT           = 09                                   
108300         SUBTRACT INLA-ART-KVAVIS  FROM ARTC-CLAG-KVOVERF                 
108400       END-IF                                                             
108500     END-IF                                                               
108600                                                                          
108700     MOVE ARTC-CLAG-IDANSK          TO WS-ARTC-IDANSK                     
108800******                                                                    
108900     MOVE ARTC-CLAG-IDINK           TO WS-ARTC-IDINK-X                    
109000       IF ARTC-CLAG-IDINK (1:3) NUMERIC                                   
109100          MOVE ARTC-CLAG-IDINK (1:3) TO WS-ARTC-IDINK                     
109200       ELSE                                                               
109300          IF ARTC-CLAG-IDINK (2:3) NUMERIC                                
109400             MOVE ARTC-CLAG-IDINK (2:3) TO WS-ARTC-IDINK                  
109500          ELSE                                                            
109600             MOVE ZERO TO WS-ARTC-IDINK                                   
109700          END-IF                                                          
109800       END-IF                                                             
109900******                                                                    
110000     MOVE ARTC-CLAG-KDHF            TO WS-ARTC-KDHF                       
110100     MOVE ARTC-CLAG-KDTIPPR         TO WS-ARTC-KDTIPPR                    
110200     MOVE ARTC-CLAG-KDVTH           TO WS-ARTC-KDVTH                      
110300     MOVE ARTC-CLAG-KDPSLLOC        TO WS-ARTC-KDPSLLOC                   
110400     MOVE ARTC-CLAG-PRDIRLON        TO WS-ARTC-PRDIRLON                   
110500     MOVE ARTC-CLAG-PRDMTRL         TO WS-ARTC-PRDMTRL                    
110600     MOVE ARTC-CLAG-PRINK           TO WS-ARTC-PRINK                      
110700     MOVE ARTC-CLAG-PRHEMTAG        TO WS-ARTC-PRHEMTAG                   
110800     MOVE ARTC-CLAG-PRARTSTD        TO WS-ARTC-PRARTSTD                   
110900     MOVE ARTC-CLAG-PROVRPAL        TO WS-ARTC-PROVRPAL                   
111000                                                                          
111100     MOVE ARTC-CLAG-IDARTNR-EMBQ3   TO WS-ARTC-IDARTNR-EMBQ3              
111200     IF DCS-CDC OR DCS-CDC-TR                                             
111300       IF DCS-CDC                                                         
111400         SUBTRACT INLA-ART-KVAVIS    FROM ARTC-CLAG-KVAKS-CDC             
111500         MOVE ARTC-CLAG-KVAKS-CDC    TO   LOGG-KVAKS                      
111600       ELSE                                                               
111700         SUBTRACT INLA-ART-KVAVIS    FROM ARTC-CLAG-KVAKS-T               
111800         MOVE ARTC-CLAG-KVAKS-T      TO   LOGG-KVAKS                      
111900       END-IF                                                             
112000     ELSE                                                                 
112100       PERFORM IMS-GU-ARTS01                                              
112200       PERFORM IMS-GHNP-ARTS11                                            
112300                                                                          
112400                                                                          
112500       SUBTRACT INLA-ART-KVAVIS      FROM ARTS-SLAG-KVAKS-SDC             
112600       ADD      WS-KVBEART           TO   ARTS-SLAG-KVBEART               
112700       PERFORM IMS-REPL-ARTS                                              
112800       PERFORM HEC-FLYTTA-LOGG-WDK7                                       
112900       PERFORM HED-UPPDATERA-LOGG                                         
113000     END-IF                                                               
113100                                                                          
113200     PERFORM IMS-REPL-ARTC                                                
113300     IF DCS-CDC OR DCS-CDC-TR                                             
113400        PERFORM HEB-FLYTTA-LOGG-WDK6                                      
113500        PERFORM HED-UPPDATERA-LOGG                                        
113600     END-IF                                                               
113700                                                                          
113800*    -- ARTC21                                                            
113900                                                                          
114000     MOVE ZERO                   TO WS-ARTC-PRARTBEL-PR                   
114100     MOVE ZERO                   TO WS-ARTC-PRARTBEL-SUM                  
114200     MOVE ZERO                   TO WS-ARTC-PRARTBES-PR                   
114300                                                                          
114400     IF  INLA-ART-KDRT <= 05                                              
114500       PERFORM S05-UPPD-PRISJUST                                          
114600     END-IF                                                               
114700                                                                          
114800     PERFORM IMS-GNP-ARTC23                                               
114900     IF SEGMENT-FINNS                                                     
115000       MOVE AVT-IDAVTAL     TO WS-ARTC23-IDAVTAL                          
115100     ELSE                                                                 
115200       MOVE ZERO            TO WS-ARTC23-IDAVTAL                          
115300     END-IF                                                               
115400     .                                                                    
115500     EJECT                                                                
115600 HEB-FLYTTA-LOGG-WDK6 SECTION.                                            
115700* LÄGGER UPP SALDOLOGG I WDL9                                             
115800     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
115900     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
116000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
116100     ACCEPT TRANS-TID FROM TIME                                           
116200     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
116300     MOVE 9                       TO LOGG-IDSEKVNR                        
116400     IF DCS-CDC-TR                                                        
116500       MOVE WC-CDC-SE             TO LOGG-IDDC                            
116600     ELSE                                                                 
116700       MOVE W-IDDC                TO LOGG-IDDC                            
116800     END-IF                                                               
116900     MOVE 'INBO'                  TO LOGG-IDHUVTYP                        
117000     MOVE 'R31'                   TO LOGG-IDSUBTYP                        
117100     MOVE IDPGM                   TO LOGG-IDPGM                           
117200     MOVE '6119'                  TO LOGG-IDTRANS                         
117300     MOVE MSGI-IDUSER             TO LOGG-IDUSER                          
117400     MOVE SPACE                   TO LOGG-REF                             
117500     MOVE W-IDLOPNRM              TO LOGG-IDLOPNRM                        
117600     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
117700     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
117800     MOVE '-'                     TO LOGG-IDTECKEN-KVAKS                  
117900     MOVE SPACE                   TO LOGG-IDTECKEN-KVLS                   
118000     MOVE INLA-ART-KVAVIS         TO LOGG-KVART-SALDO                     
118100     MOVE ARTC-CLAG-KVLS          TO LOGG-KVLS                            
118200     MOVE ARTC-CLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                       
118300     MOVE ARTC-CLAG-KVEFRS        TO LOGG-KVEFRS                          
118400     COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                           
118500                          ARTC-CLAG-KVAKS-T                               
118600     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
118700                                                                          
118800     .                                                                    
118900     EJECT                                                                
119000 HEC-FLYTTA-LOGG-WDK7 SECTION.                                            
119100* LÄGGER UPP SALDOLOGG I WDL9                                             
119200     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
119300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
119400     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
119500     ACCEPT TRANS-TID FROM TIME                                           
119600     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
119700     MOVE 9                     TO LOGG-IDSEKVNR                          
119800     IF DCS-CDC-TR                                                        
119900       MOVE WC-CDC-SE           TO LOGG-IDDC                              
120000     ELSE                                                                 
120100       MOVE W-IDDC              TO LOGG-IDDC                              
120200     END-IF                                                               
120300     MOVE 'INBO'                TO LOGG-IDHUVTYP                          
120400     MOVE 'R31'                 TO LOGG-IDSUBTYP                          
120500     MOVE IDPGM                 TO LOGG-IDPGM                             
120600     MOVE '6119'                TO LOGG-IDTRANS                           
120700     MOVE MSGI-IDUSER           TO LOGG-IDUSER                            
120800     MOVE SPACE                 TO LOGG-REF                               
120900     MOVE W-IDLOPNRM            TO LOGG-IDLOPNRM                          
121000     MOVE SPACE                 TO LOGG-IDTECKEN-KVAKS-PAV                
121100     MOVE SPACE                 TO LOGG-IDTECKEN-KVEFRS                   
121200     MOVE '-'                   TO LOGG-IDTECKEN-KVAKS                    
121300     MOVE SPACE                 TO LOGG-IDTECKEN-KVLS                     
121400     MOVE INLA-ART-KVAVIS       TO LOGG-KVART-SALDO                       
121500     MOVE ARTS-SLAG-KVAKS-SDC   TO LOGG-KVAKS                             
121600     MOVE ARTS-SLAG-KVLS        TO LOGG-KVLS                              
121700     MOVE ARTS-SLAG-KVAKS-PAV   TO LOGG-KVAKS-PAV                         
121800     MOVE ARTS-SLAG-KVEFRS      TO LOGG-KVEFRS                            
121900     MOVE ZERO                  TO LOGG-DAREGDAT-LADD                     
122000     .                                                                    
122100     EJECT                                                                
122200 HED-UPPDATERA-LOGG SECTION.                                              
122300     PERFORM IMS-ISRT-WDL901                                              
122400     IF SEGMENT-FINNS-REDAN                                               
122500        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
122600          ADD -1 TO LOGG-IDSEKVNR                                         
122700          PERFORM IMS-ISRT-WDL901                                         
122800        END-PERFORM                                                       
122900     END-IF                                                               
123000     .                                                                    
123100     EJECT                                                                
123200 HF-UPPD-ZZAC-LOGG SECTION.                                               
123300                                                                          
123400* ÖVRIGA TRANSAR SKICKAS BARA OM DET ÄR CDC (DC = 11, 12)                 
123500*                                                                         
123600     IF DCS-CDC OR DCS-CDC-TR                                             
123700       PERFORM HFA-LOGG-R32                                               
123800                                                                          
123900       PERFORM HFB-LOGG-320                                               
124000       PERFORM HFH-LOGG-092-M103                                          
124100     END-IF                                                               
124200                                                                          
124300     IF DCS-CANADA                                                        
124400          PERFORM HFDA-LOGG-LAB-WDR8                                      
124500     ELSE                                                                 
124600       IF  INLA-ART-KVAVIS NOT = WS-INLE-MOT-KVANTMOT                     
124700       AND INLA-ART-KDRT = 0 OR 9                                         
124800          PERFORM HFDB-LOGG-EKO-WDR8-WDR9                                 
124900       ELSE                                                               
125000          IF INLA-ART-KDRT = 6                                            
125100            PERFORM HFG-LOGG-EKO-R31-WDR9-RT6                             
125200          ELSE                                                            
125300            PERFORM HFE-LOGG-EKO-WDR9                                     
125400          END-IF                                                          
125500       END-IF                                                             
125600       IF WS-ARTC-PRINK = WS-ARTC-PRARTSTD                                
125700         CONTINUE                                                         
125800       ELSE                                                               
125900         IF  INLA-ART-KVAVIS NOT = 0                                      
126000         AND INLA-ART-KDRT   NOT = 6                                      
126100           PERFORM HFF-LOGG-EKO-WDR9-PALAGG                               
126200         END-IF                                                           
126300       END-IF                                                             
126400     END-IF                                                               
126500     .                                                                    
126600     EJECT                                                                
126700 HFA-LOGG-R32 SECTION.                                                    
126800                                                                          
126900     MOVE SPACE                  TO W611R32-W611R32                       
127000                                                                          
127100     MOVE 'R32'                  TO W611R32-IDPTYP                        
127200     MOVE +5                     TO W611R32-KDSORT2                       
127300     MOVE INLA-ART-IDDC (2:1)    TO W611R32-KDCLAGER                      
127400     MOVE INLA-ART-IDARTNR       TO W611R32-IDARTNR                       
127500     MOVE INLA-ART-IDLOPNRM      TO W611R32-IDLOPNRM                      
127600     MOVE 2                      TO W611R32-KDAVVANT                      
127700     MOVE ZERO                   TO W611R32-KVANTMOT                      
127800     MOVE ZERO                   TO W611R32-KVFORDEL                      
127900     MOVE ZERO                   TO W611R32-IDKOLLI                       
128000     MOVE ZERO                   TO W611R32-KDAVVKV                       
128100     MOVE ZERO                   TO W611R32-KVRETUR                       
128200                                                                          
128300     MOVE W611R32-W611R32        TO WS-ZZAC01-LOGGPOST                    
128400     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
128500     PERFORM S02-SKAPA-ZZAC01                                             
128600     .                                                                    
128700     EJECT                                                                
128800 HFB-LOGG-320 SECTION.                                                    
128900                                                                          
129000     MOVE ZERO                   TO R320-W211310                          
129100                                                                          
129200     MOVE '221'                  TO R320-IDTTYP                           
129300     MOVE INLA-ART-IDARTNR       TO R320-IDARTNR-S                        
129400     MOVE INLA-ART-IDDC (2:1)    TO R320-KDCLAGER-S                       
129500     MOVE ZERO                   TO R320-SORTFLT1                         
129600     MOVE WS-ARTC-IDANSK         TO R320-IDANSKNR                         
129700     MOVE INLA-ART-PRARTSTD      TO R320-PRARTSTD                         
129800     MOVE INLA-ART-IDLOPNRM      TO R320-IDLOPNR                          
129900     MOVE 2                      TO R320-KDAVVANT                         
130000     MOVE ZERO                   TO R320-POSTLGD                          
130100     MOVE '320'                  TO R320-IDPTYP                           
130200     MOVE ZERO                   TO R320-NOLLOR-20                        
130300     MOVE INLA-ART-IDARTNR       TO R320-IDARTNR                          
130400     MOVE INLA-ART-IDDC (2:1)    TO R320-KDCLAGER                         
130500     MOVE ZERO                   TO R320-KVMOTANT                         
130600     MOVE ZERO                   TO R320-NOLLOR-41                        
130700     MOVE INLA-INL-IDLEVNR       TO R320-IDLEVNR-INL                      
130800     MOVE INLA-INL-TIAVIDAT      TO R320-TIAVSDAT                         
130900     MOVE ZERO                   TO R320-NOLLOR-21                        
131000     MOVE INLA-ART-KDRT          TO R320-KDRT                             
131100     MOVE INLA-ART-KVAVIS        TO R320-KVAVIS                           
131200     MOVE WS-IDAVINR             TO R320-IDAVINR                          
131300     MOVE WS-ARTC-IDINK          TO R320-KDPKINR                          
131400                                                                          
131500     MOVE R320-W211310           TO WS-ZZAC01-LOGGPOST                    
131600     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
131700     PERFORM S02-SKAPA-ZZAC01                                             
131800     .                                                                    
131900     EJECT                                                                
132000 HFDA-LOGG-LAB-WDR8 SECTION.                                              
132100                                                                          
132200     MOVE SPACE                  TO LAB-W510A11                           
132300                                                                          
132400     MOVE 'A11'                  TO LAB-IDPTYP                            
132500     MOVE 'I10'                  TO LAB-KDEKOHT                           
132600     IF INLA-ART-IDDC NOT = ART-DCS-IDDC                                  
132700        MOVE INLA-ART-IDDC       TO W-IDDC-B6-ART                         
132800        PERFORM IMS-GU-WDB601-ART                                         
132900     END-IF                                                               
133000     IF ART-DCS-NDC-NA AND ART-DCS-CANADA                                 
133100       MOVE 54                   TO LAB-IDFTG                             
133200     ELSE                                                                 
133300       MOVE 53                   TO LAB-IDFTG                             
133400     END-IF                                                               
133500     MOVE INLA-ART-IDDC          TO LAB-IDDC-SEND                         
133600                                    LAB-IDDC-REC                          
133700     MOVE INLA-INL-IDLEVNR       TO LAB-IDLEVNR                           
133800     MOVE INLA-INL-IDFS          TO LAB-IDFS                              
133900     MOVE WS-DAINLINL            TO LAB-DAINLINL                          
134000     MOVE ZERO                   TO LAB-IDORDNR7                          
134100     MOVE INLA-ART-IDARTNR       TO LAB-IDARTNR                           
134200     MOVE WS-ARTC-KDPRODSL       TO LAB-KDPRODSL                          
134300     MOVE ARTC-ART-IDFKNGRP      TO WS-ARTC-IDFKNGRP                      
134400     MOVE WS-ARTC-KDPSLLOC       TO LAB-KDPSLLOC                          
134500     MOVE ZERO                   TO LAB-PRARTBEU                          
134600     MOVE INLA-ART-KVAVIS        TO LAB-KVAVIS                            
134700     MOVE ZERO                   TO LAB-KVANTMOT                          
134800                                    LAB-PRAVCOST                          
134900                                    LAB-PRAVCOST-OLD                      
135000                                    LAB-KVLS-OLD                          
135100                                    LAB-REMARKUP                          
135200     MOVE WS-TIAAMMDD-AA         TO W-DATE-AAMM(1:2)                      
135300     MOVE WS-TIAAMMDD-MM         TO W-DATE-AAMM(3:2)                      
135400     MOVE W-DATE-AAMM            TO CURR-TIAAMM                           
135500     MOVE WS-KDVALISO-HUV        TO CURR-KDVALISO-HUV                     
135600     MOVE 'M'                    TO CURR-KDVALTYP                         
135700                                                                          
135800     IF WS-ARTC-PRARTBEL-PR > ZERO                                        
135900       MOVE WS-ARTC-PRARTBEL-PR    TO W-PRARTBEL-PR                       
136000       MOVE WS-ARTC-KDVALISO       TO CURR-KDVALISO-ROW                   
136100     ELSE                                                                 
136200       MOVE 0.1                    TO W-PRARTBEL-PR                       
136300       MOVE 'SEK'                  TO CURR-KDVALISO-ROW                   
136400     END-IF                                                               
136500     IF (ART-DCS-NDC-NA AND ART-DCS-USA    AND                            
136600         CURR-KDVALISO-ROW = 'USD') OR                                    
136700        (ART-DCS-NDC-NA AND ART-DCS-CANADA AND                            
136800         CURR-KDVALISO-ROW = 'CAD')                                       
136900       MOVE W-PRARTBEL-PR         TO LAB-PRARTBEU                         
137000     ELSE                                                                 
137100       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
137200       IF CURR-KDSVAR = ' '                                               
137300         MOVE CURR-PRKURS-NEW     TO W-PRKURS                             
137400       ELSE                                                               
137500         MOVE 1                   TO W-PRKURS                             
137600       END-IF                                                             
137700       MOVE 'USD'                 TO CURR-KDVALISO-ROW                    
137800       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
137900       IF CURR-KDSVAR = ' '                                               
138000          MOVE CURR-PRKURS-NEW    TO W-PRKURS-USD                         
138100       ELSE                                                               
138200          MOVE 1                  TO W-PRKURS-USD                         
138300       END-IF                                                             
138400                                                                          
138500       MOVE 'CAD'                 TO CURR-KDVALISO-ROW                    
138600       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
138700       IF CURR-KDSVAR = ' '                                               
138800          MOVE CURR-PRKURS-NEW    TO W-PRKURS-CAD                         
138900       ELSE                                                               
139000          MOVE 1                  TO W-PRKURS-CAD                         
139100       END-IF                                                             
139200       IF ART-DCS-NDC-NA AND ART-DCS-USA                                  
139300         COMPUTE W-PRKURS-ML ROUNDED = W-PRKURS-USD / W-PRKURS            
139400       ELSE                                                               
139500         COMPUTE W-PRKURS-ML ROUNDED = W-PRKURS-CAD / W-PRKURS            
139600       END-IF                                                             
139700       COMPUTE LAB-PRARTBEU ROUNDED = W-PRARTBEL-PR / W-PRKURS-ML         
139800     END-IF                                                               
139900                                                                          
140000     MOVE 'W6011910'        TO EKO-FIL-IDPGM                              
140100     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
140200     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
140300     MOVE 1                 TO EKO-FIL-IDSEKVNR                           
140400     MOVE 'W510'            TO EKO-FIL-CT-IDSYSTEM                        
140500     MOVE 'A11'             TO EKO-FIL-CT-IDPTYP                          
140600     MOVE ' '               TO EKO-FIL-CT-IDVTYP                          
140700                                                                          
140800     PERFORM IMS-ISRT-EKOTRANS                                            
140900                                                                          
141000     PERFORM UNTIL SEGMENT-FINNS                                          
141100       ADD +1 TO EKO-FIL-IDSEKVNR                                         
141200       PERFORM IMS-ISRT-EKOTRANS                                          
141300     END-PERFORM                                                          
141400     .                                                                    
141500     EJECT                                                                
141600                                                                          
141700 HFDB-LOGG-EKO-WDR8-WDR9 SECTION.                                         
141800     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
141900       PERFORM HFDBA-LOGG-EKO-VCCN-DET                                    
142000       IF DCS-NDC-CN OR DCS-USA                                           
142100         PERFORM HFDBA-LOGG-EKO-VCCN-HEMT                                 
142200       END-IF                                                             
142300       IF  WS-ARTC-PRINK NOT = WS-ARTC-PRARTSTD                           
142400       AND (INLA-ART-KDRT NOT = 3 AND 7 AND 8 AND 77)                     
142500         PERFORM IMS-GU-WDB601                                            
142600         MOVE DCS-IDLANDX2            TO W-IDLANDX2                       
142700         IF SEGMENT-FINNS                                                 
142800           PERFORM IMS-GNP-WDB617                                         
142900           IF SEGMENT-FINNS                                               
143000             IF DCS-USA                                                   
143100               CONTINUE                                                   
143200             ELSE                                                         
143300               PERFORM HFDBA-LOGG-EKO-VCCN-KALK                           
143400             END-IF                                                       
143500           END-IF                                                         
143600         END-IF                                                           
143700       END-IF                                                             
143800       IF DCS-USA                                                         
143900         CONTINUE                                                         
144000       ELSE                                                               
144100         PERFORM HFDBA-LOGG-EKO-VCCN-SUM                                  
144200       END-IF                                                             
144300     ELSE                                                                 
144400       PERFORM HFDBB-LOGG-EKO-VCCS                                        
144500     END-IF                                                               
144600     .                                                                    
144700     EJECT                                                                
144800                                                                          
144900 HFDBA-LOGG-EKO-VCCN-DET SECTION.                                         
145000     IF DCS-NDC-CN                                                        
145100     OR DCS-USA                                                           
145200       MOVE SPACE                    TO EKO-W51080                        
145300       MOVE SPACE                    TO WS-SAP-MM-POST                    
145400                                                                          
145500       MOVE INLA-ART-IDARTNR         TO EKO-IDARTNR                       
145600       MOVE INLA-ART-IDDC            TO EKO-IDDC                          
145700       MOVE INLA-INL-IDFS            TO EKO-IDFS                          
145800       MOVE WS-ARTC-IDINK-X          TO EKO-IDINK                         
145900       MOVE INLA-INL-IDKONTO         TO EKO-IDKONTO                       
146000       MOVE INLA-INL-IDLEVNR         TO EKO-IDLEVNR                       
146100       MOVE INLA-ART-IDLOPNRM        TO EKO-IDLOPNRM                      
146200       MOVE WS-ARTC-KDPRODSL         TO EKO-KDPRODSL                      
146300       MOVE INLA-ART-KDRT            TO EKO-KDRT                          
146400       MOVE WS-ARTC-KDSORT           TO EKO-KDSORT                        
146500       MOVE WS-ARTC-KDTIPPR          TO EKO-KDTIPPR                       
146600       MOVE WS-ARTC-KDVALISO         TO EKO-KDVALISO                      
146700*                                                                         
146800       IF INLA-INL-IDLEVNR = '1441'                                       
146900         MOVE NEJ                    TO WS-SAP-MM-POST                    
147000       END-IF                                                             
147100*                                                                         
147200       IF DCS-NDC-CN                                                      
147300       OR DCS-USA                                                         
147400         IF DCS-NDC-CN                                                    
147500           MOVE 60                   TO EKO-IDFTG                         
147600           MOVE WS-KDVALISO-HUV-CN   TO CURR-KDVALISO-HUV                 
147700         ELSE                                                             
147800           MOVE 53                   TO EKO-IDFTG                         
147900           MOVE WS-KDVALISO-HUV-US   TO CURR-KDVALISO-HUV                 
148000         END-IF                                                           
148100         PERFORM S06-GET-PRARTBEL                                         
148200         MOVE INLA-INL-TIAVIDAT      TO WS-DAAVIDAT-YYMMDD                
148300         MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                 
148400         MOVE WS-DAAVIDAT-YYMMDD(3:2) TO W-DATE-AAMM(3:2)                 
148500         MOVE WS-ARTC-KDVALISO       TO CURR-KDVALISO-ROW                 
148600         MOVE W-DATE-AAMM            TO CURR-TIAAMM                       
148700         MOVE 'M'                    TO CURR-KDVALTYP                     
148800         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
148900         IF CURR-KDSVAR = ' '                                             
149000           MOVE CURR-PRKURS-NEW      TO WS-PRKURS                         
149100           MOVE CURR-REVALUTA-TO     TO WS-REVALUTA                       
149200         ELSE                                                             
149300           MOVE 1                    TO WS-PRKURS                         
149400           MOVE 1                    TO WS-REVALUTA                       
149500         END-IF                                                           
149600         MOVE WS-ARTC-KDVALISO       TO EKO-KDVALISO                      
149700****   AGREE PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED              
149800****   TO COUNTRY CURRENCY                                                
149900         COMPUTE WS-ARTC-PRARTBES-PR = WS-ARTC-PRARTBEL-PR                
150000                                       * WS-PRKURS / WS-REVALUTA          
150100       END-IF                                                             
150200*                                                                         
150300       IF WS-ARTC-PRARTBEL-PR  > ZERO                                     
150400         MOVE WS-ARTC-PRARTBEL-PR    TO EKO-PRARTBEL-PR                   
150500       ELSE                                                               
150600*  -- OBS! RADPRIS SAKNAS, KDVALISO=XXX SIGNALERAR DETTA FÖR LEVA1        
150700         MOVE 0.1                    TO EKO-PRARTBEL-PR                   
150800         MOVE 'XXX'                  TO EKO-KDVALISO                      
150900       END-IF                                                             
151000       IF WS-ARTC-PRARTBES-PR  > ZERO                                     
151100         MOVE WS-ARTC-PRARTBES-PR    TO EKO-PRARTBES                      
151200       ELSE                                                               
151300         MOVE 0.1                    TO EKO-PRARTBES                      
151400       END-IF                                                             
151500       COMPUTE EKO-KVAVIS = INLA-ART-KVAVIS * -1                          
151600       END-COMPUTE                                                        
151700       MOVE EKO-KVAVIS TO WS-SPAR-KVAVIS                                  
151800       MOVE ZERO                     TO EKO-PRINK                         
151900       MOVE ZERO                     TO EKO-PRHEMTAG                      
152000       MOVE ZERO                     TO EKO-RETULF                        
152100       MOVE INLA-INL-TIAVIDAT        TO EKO-TIAVIDAT                      
152200       MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                    
152300       MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                     
152400       MOVE JA                       TO EKO-FLLSBOK                       
152500       MOVE ZERO                     TO EKO-IDDISTR                       
152600       MOVE NEJ                      TO EKO-FLDIRLEV                      
152700                                                                          
152800       MOVE JA                       TO EKO-FLAVVINL                      
152900       MOVE WS-ARTC23-IDAVTAL        TO EKO-IDAVTAL                       
153000       MOVE 'V'                      TO EKO-KDINLAVV                      
153100       IF DCS-NDC-CN OR DCS-USA                                           
153200         IF DCS-NDC-CN                                                    
153300           MOVE 60                   TO EKO-IDFTG                         
153400         ELSE                                                             
153500           MOVE 53                   TO EKO-IDFTG                         
153600         END-IF                                                           
153700       END-IF                                                             
153800                                                                          
153900       MOVE 'W6011910'               TO EKO-FIL-IDPGM                     
154000       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
154100       ACCEPT EKO-FIL-TIKLOCK FROM TIME                                   
154200       MOVE 1                        TO EKO-FIL-IDSEKVNR                  
154300       MOVE 'W510'                   TO EKO-FIL-CT-IDSYSTEM               
154400       MOVE '80 '                    TO EKO-FIL-CT-IDPTYP                 
154500       MOVE ' '                      TO EKO-FIL-CT-IDVTYP                 
154600                                                                          
154700       IF WS-SAP-MM-POST = NEJ                                            
154800          CONTINUE                                                        
154900       ELSE                                                               
155000         PERFORM IMS-ISRT-EKOTRANS                                        
155100                                                                          
155200         PERFORM UNTIL SEGMENT-FINNS                                      
155300           ADD +1 TO EKO-FIL-IDSEKVNR                                     
155400           PERFORM IMS-ISRT-EKOTRANS                                      
155500         END-PERFORM                                                      
155600       END-IF                                                             
155700     END-IF                                                               
155800                                                                          
155900     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
156000     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
156100     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
156200     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
156300     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
156400     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
156500     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
156600     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
156700     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
156800     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
156900     MOVE +0                          TO EKO-EKH-IDDISTR                  
157000                                         EKO-EKH-IDKUNDNR                 
157100*******************************                                           
157200*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
157300     MOVE ZERO TO NOLL-RAKNARE                                            
157400     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
157500     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
157600     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
157700          FOR LEADING ZERO                                                
157800     ADD +1 TO NOLL-RAKNARE                                               
157900     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
158000          WITH POINTER NOLL-RAKNARE                                       
158100*******************************                                           
158200     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
158300     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
158400                                         EKO-EKH-PRARTNTO                 
158500                                         EKO-EKH-PRARTSJK                 
158600                                         EKO-EKH-PRLANDCO                 
158700                                         EKO-EKH-SUBEL                    
158800     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
158900     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
159000                                                                          
159100     MOVE 1.00                        TO EKO-EKH-PRKURS                   
159200                                                                          
159300     MOVE ZERO                        TO EKO-EKH-PRINK                    
159400                                                                          
159500     IF DCS-NDC-CN                                                        
159600     OR DCS-USA                                                           
159700       PERFORM S06-GET-PRARTBEL                                           
159800     END-IF                                                               
159900*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
160000*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
160100     IF XDC-NON-VCC-OWNED OR DCS-LAND-NON-VCC-OWNED                       
160200     AND NOT (NDC-CN OR NDC-US)                                           
160300       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
160400     END-IF                                                               
160500***                                                                       
160600     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
160700       MOVE WS-ARTC-PRARTBEL-PR       TO EKO-EKH-PRARTSTD                 
160800       MOVE WS-ARTC-KDVALISO          TO EKO-EKH-KDVALISO                 
160900     ELSE                                                                 
161000       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
161100       MOVE DCS-KDVALISO              TO EKO-EKH-KDVALISO                 
161200**** TAIWAN NEEDS A INTEGER VALUE AS TAIWAN DOSENT USE                    
161300**** DECIMALS. 0.1 MAKES THE BOOKING TO ZERO WHICH IS NOT                 
161400**** VALID IN SAP SYSTEM.                                                 
161500       IF DCS-TAIWAN                                                      
161600         MOVE 1.0                     TO EKO-EKH-PRARTSTD                 
161700       END-IF                                                             
161800     END-IF                                                               
161900     COMPUTE EKO-EKH-KVANTAL = INLA-ART-KVAVIS * -1                       
162000     MOVE EKO-EKH-PRARTSTD            TO WS-SUARTSTD                      
162100     COMPUTE WS-SUARTSTD = WS-ARTC-PRARTBEL-SUM *                         
162200                           EKO-EKH-KVANTAL                                
162300     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
162400     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
162500     MOVE SPACE                       TO EKO-EKH-BEVAT                    
162600                                         EKO-EKH-KDANMORS                 
162700                                         EKO-EKH-IDKST                    
162800     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
162900                                         EKO-EKH-KDFRAKT                  
163000                                         EKO-EKH-SUVAT                    
163100                                                                          
163200                                                                          
163300     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
163400     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
163500     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
163600     IF INLA-ART-KVAVIS > ZERO                                            
163700       MOVE 1                        TO EKO-EKH-KDAVVTYP                  
163800     ELSE                                                                 
163900       MOVE 0                        TO EKO-EKH-KDAVVTYP                  
164000     END-IF                                                               
164100     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
164200     MOVE WS-INLE-MOT-KVANTMOT       TO EKO-EKH-KVANTMOT                  
164300     MOVE INLA-ART-KVAVIS            TO EKO-EKH-KVAVIS                    
164400     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
164500                                                                          
164600     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
164700                                        EKO-EKH-PRDMTRL                   
164800                                        EKO-EKH-PROVRPAL                  
164900     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
165000     MOVE SPACE                      TO EKO-EKH-FLDCET                    
165100     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
165200     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
165300     EVALUATE TRUE                                                        
165400       WHEN NDC-CN                                                        
165500         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
165600       WHEN NDC-IN                                                        
165700         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
165800       WHEN NDC-US                                                        
165900         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
166000       WHEN OTHER                                                         
166100         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
166200         MOVE 'BAK'                  TO EKO-EKH-CMD                       
166300**** WHEN MAK OR BACK WE NEED A NEW EVENT FOR KOREA                       
166400         IF NDC-KR                                                        
166410         OR NDC-MX                                                        
166420         OR NDC-BR                                                        
166430         OR NDC-ZA                                                        
166500           MOVE '103'                TO EKO-EKH-KDEKHHT                   
166600           MOVE '106'                TO EKO-EKH-KDEKSHT                   
166700         END-IF                                                           
166800     END-EVALUATE                                                         
166900     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
167000                                                                          
167100     PERFORM IMS-ISRT-EKOTRANS                                            
167200                                                                          
167300     PERFORM UNTIL SEGMENT-FINNS                                          
167400       ADD +1                     TO EKO-FIL-IDSEKVNR                     
167500       PERFORM IMS-ISRT-EKOTRANS                                          
167600     END-PERFORM                                                          
167700     .                                                                    
167800     EJECT                                                                
167900                                                                          
168000 HFDBA-LOGG-EKO-VCCN-HEMT SECTION.                                        
168100     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
168200     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
168300     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
168400     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
168500     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
168600     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
168700     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
168800     MOVE 'HEMT '                     TO EKO-EKH-KDEKNIVA                 
168900     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
169000     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
169100     MOVE +0                          TO EKO-EKH-IDDISTR                  
169200                                         EKO-EKH-IDKUNDNR                 
169300*******************************                                           
169400*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
169500     MOVE ZERO TO NOLL-RAKNARE                                            
169600     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
169700     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
169800     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
169900          FOR LEADING ZERO                                                
170000     ADD +1 TO NOLL-RAKNARE                                               
170100     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
170200          WITH POINTER NOLL-RAKNARE                                       
170300*******************************                                           
170400     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
170500     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
170600                                         EKO-EKH-PRARTNTO                 
170700                                         EKO-EKH-PRARTSJK                 
170800                                         EKO-EKH-PRLANDCO                 
170900                                         EKO-EKH-SUBEL                    
171000     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
171100     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
171200                                                                          
171300     MOVE 1.00                        TO EKO-EKH-PRKURS                   
171400                                                                          
171500     MOVE ZERO                        TO EKO-EKH-PRINK                    
171600                                                                          
171700     IF DCS-NDC-CN                                                        
171800     OR DCS-USA                                                           
171900       PERFORM S06-GET-PRARTBEL                                           
172000     END-IF                                                               
172100*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
172200*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
172300     IF XDC-NON-VCC-OWNED OR DCS-LAND-NON-VCC-OWNED                       
172400     AND NOT (NDC-CN OR NDC-US)                                           
172500       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
172600     END-IF                                                               
172700***                                                                       
172800     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
172900       MOVE WS-ARTC-PRARTBEL-PR       TO EKO-EKH-PRARTSTD                 
173000       MOVE WS-ARTC-KDVALISO          TO EKO-EKH-KDVALISO                 
173100     ELSE                                                                 
173200       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
173300       MOVE DCS-KDVALISO              TO EKO-EKH-KDVALISO                 
173400**** TAIWAN NEEDS A INTEGER VALUE AS TAIWAN DOSENT USE                    
173500**** DECIMALS. 0.1 MAKES THE BOOKING TO ZERO WHICH IS NOT                 
173600**** VALID IN SAP SYSTEM.                                                 
173700       IF DCS-TAIWAN                                                      
173800         MOVE 1.0                     TO EKO-EKH-PRARTSTD                 
173900       END-IF                                                             
174000     END-IF                                                               
174100******                                                                    
174200     MOVE INLA-INL-IDLEVNR      TO W-IDLEVNR                              
174300     PERFORM IMS-GU-WDF101                                                
174400     IF SEGMENT-SAKNAS                                                    
174500       MOVE ZERO TO W-RETULF                                              
174600     ELSE                                                                 
174700       MOVE WS-IDDC          TO W-IDDC-B6                                 
174800       PERFORM IMS-GU-WDB601                                              
174900       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
175000       PERFORM IMS-GNP-WDF102                                             
175100       IF SEGMENT-FINNS                                                   
175200         IF F102-TULL-TITULF < DAGENS-DATUM                               
175300           MOVE F102-TULL-RETULF-1  TO W-RETULF                           
175400         ELSE                                                             
175500           MOVE F102-TULL-RETULF-2  TO W-RETULF                           
175600         END-IF                                                           
175700       END-IF                                                             
175800     END-IF                                                               
175900******                                                                    
176000     COMPUTE EKO-EKH-PRHEMTAG ROUNDED = (W-RETULF - 1) *                  
176100             EKO-EKH-PRARTSTD * INLA-ART-KVAVIS                           
176200     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
176300     IF EKO-EKH-PRHEMTAG > 0                                              
176400       COMPUTE WS-SUHEMT = EKO-EKH-PRHEMTAG * -1                          
176500     ELSE                                                                 
176600       MOVE EKO-EKH-PRHEMTAG          TO WS-SUHEMT                        
176700     END-IF                                                               
176800     MOVE WS-SUHEMT                   TO EKO-EKH-SUBEL                    
176900     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
177000     MOVE SPACE                       TO EKO-EKH-BEVAT                    
177100                                         EKO-EKH-KDANMORS                 
177200                                         EKO-EKH-IDKST                    
177300     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
177400                                         EKO-EKH-KDFRAKT                  
177500                                         EKO-EKH-SUVAT                    
177600                                         EKO-EKH-KVANTAL                  
177700                                                                          
177800     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
177900     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
178000     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
178100     MOVE 0                          TO EKO-EKH-KDAVVTYP                  
178200     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
178300     MOVE ZERO                       TO EKO-EKH-KVANTMOT                  
178400     MOVE ZERO                       TO EKO-EKH-KVAVIS                    
178500     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
178600     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
178700     EVALUATE TRUE                                                        
178800       WHEN NDC-CN                                                        
178900         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
179000       WHEN NDC-IN                                                        
179100         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
179200       WHEN NDC-US                                                        
179300         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
179400       WHEN OTHER                                                         
179500         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
179600     END-EVALUATE                                                         
179700     MOVE 'BAK'                      TO EKO-EKH-CMD                       
179800     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
179900                                                                          
180000     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
180100                                        EKO-EKH-PRDMTRL                   
180200                                        EKO-EKH-PROVRPAL                  
180300     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
180400     MOVE SPACE                      TO EKO-EKH-FLDCET                    
180500     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
180600                                                                          
180700     IF EKO-EKH-PRHEMTAG > ZERO                                           
180800     OR EKO-EKH-PRHEMTAG < ZERO                                           
180900       PERFORM IMS-ISRT-EKOTRANS                                          
181000                                                                          
181100       PERFORM UNTIL SEGMENT-FINNS                                        
181200         ADD +1                     TO EKO-FIL-IDSEKVNR                   
181300         PERFORM IMS-ISRT-EKOTRANS                                        
181400       END-PERFORM                                                        
181500     END-IF                                                               
181600     .                                                                    
181700     EJECT                                                                
181800                                                                          
181900 HFDBA-LOGG-EKO-VCCN-KALK SECTION.                                        
182000     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
182100     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
182200     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
182300     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
182400     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
182500     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
182600     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
182700     MOVE 'KALK '                     TO EKO-EKH-KDEKNIVA                 
182800     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
182900     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
183000     MOVE +0                          TO EKO-EKH-IDDISTR                  
183100                                         EKO-EKH-IDKUNDNR                 
183200*******************************                                           
183300*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
183400     MOVE ZERO TO NOLL-RAKNARE                                            
183500     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
183600     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
183700     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
183800          FOR LEADING ZERO                                                
183900     ADD +1 TO NOLL-RAKNARE                                               
184000     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
184100          WITH POINTER NOLL-RAKNARE                                       
184200*******************************                                           
184300     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
184400     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
184500                                         EKO-EKH-PRARTNTO                 
184600                                         EKO-EKH-PRARTSJK                 
184700                                         EKO-EKH-PRLANDCO                 
184800                                         EKO-EKH-SUBEL                    
184900     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
185000     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
185100                                                                          
185200     MOVE 1.00                        TO EKO-EKH-PRKURS                   
185300                                                                          
185400     MOVE ZERO                        TO EKO-EKH-PRINK                    
185500     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
185600                                                                          
185700     MOVE DAGENS-DATUM(3:2) TO W-DATE-AAMM(1:2)                           
185800     MOVE 01                TO W-DATE-AAMM(3:2)                           
185900     MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                          
186000     MOVE DCS-KDVALISO      TO CURR-KDVALISO-ROW                          
186100     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
186200     MOVE 'A'               TO CURR-KDVALTYP                              
186300     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
186400     IF CURR-KDSVAR = ' '                                                 
186500       MOVE CURR-PRKURS-NEW  TO WS-PRKURS                                 
186600       MOVE CURR-REVALUTA-TO TO WS-REVALUTA                               
186700     ELSE                                                                 
186800       MOVE 1                TO WS-PRKURS                                 
186900       MOVE 1                TO WS-REVALUTA                               
187000     END-IF                                                               
187100     COMPUTE EKO-EKH-PRDIRLON ROUNDED = WS-ARTC-PRDIRLON *                
187200      PROC-REDIRLON /  WS-PRKURS / WS-REVALUTA * INLA-ART-KVAVIS          
187300     COMPUTE EKO-EKH-PRDMTRL  ROUNDED = WS-ARTC-PRDMTRL  *                
187400      PROC-REDMTRL  /  WS-PRKURS / WS-REVALUTA * INLA-ART-KVAVIS          
187500     MOVE ZERO                        TO EKO-EKH-PROVRPAL                 
187600     COMPUTE WS-SUDIRMTRL = EKO-EKH-PRDMTRL  * -1                         
187700     COMPUTE WS-SUDIRMTRL = EKO-EKH-PRDIRLON * -1                         
187800     COMPUTE EKO-EKH-SUBEL = EKO-EKH-PRDIRLON +                           
187900                             EKO-EKH-PRDMTRL                              
188000                                                                          
188100     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
188200     MOVE SPACE                       TO EKO-EKH-BEVAT                    
188300                                         EKO-EKH-KDANMORS                 
188400                                         EKO-EKH-IDKST                    
188500     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
188600                                         EKO-EKH-KDFRAKT                  
188700                                         EKO-EKH-SUVAT                    
188800                                         EKO-EKH-KVANTAL                  
188900                                                                          
189000     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
189100     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
189200     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
189300     MOVE 0                          TO EKO-EKH-KDAVVTYP                  
189400     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
189500     MOVE ZERO                       TO EKO-EKH-KVANTMOT                  
189600     MOVE ZERO                       TO EKO-EKH-KVAVIS                    
189700     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
189800     MOVE WS-ARTC-KDVALISO           TO EKO-EKH-KDVALISO                  
189900     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
190000     EVALUATE TRUE                                                        
190100       WHEN NDC-CN                                                        
190200         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
190300       WHEN NDC-IN                                                        
190400         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
190500       WHEN NDC-US                                                        
190600         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
190700       WHEN OTHER                                                         
190800         IF NDC-KR                                                        
190810         OR NDC-MX                                                        
190820         OR NDC-BR                                                        
190830         OR NDC-ZA                                                        
190900           MOVE '103'                TO EKO-EKH-KDEKHHT                   
191000           MOVE '106'                TO EKO-EKH-KDEKSHT                   
191100         END-IF                                                           
191200         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
191300     END-EVALUATE                                                         
191400     MOVE 'REC'                      TO EKO-EKH-CMD                       
191500     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
191600                                                                          
191700     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
191800     MOVE SPACE                      TO EKO-EKH-FLDCET                    
191900     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
192000                                                                          
192100     IF EKO-EKH-SUBEL > ZERO                                              
192200     OR EKO-EKH-SUBEL < ZERO                                              
192300       PERFORM IMS-ISRT-EKOTRANS                                          
192400       PERFORM UNTIL SEGMENT-FINNS                                        
192500         ADD +1                     TO EKO-FIL-IDSEKVNR                   
192600         PERFORM IMS-ISRT-EKOTRANS                                        
192700       END-PERFORM                                                        
192800     END-IF                                                               
192900     .                                                                    
193000     EJECT                                                                
193100                                                                          
193200 HFDBA-LOGG-EKO-VCCN-SUM SECTION.                                         
193300     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
193400     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
193500     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
193600     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
193700     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
193800     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
193900     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
194000     MOVE 'SUM  '                     TO EKO-EKH-KDEKNIVA                 
194100     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
194200     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
194300     MOVE +0                          TO EKO-EKH-IDDISTR                  
194400                                         EKO-EKH-IDKUNDNR                 
194500*******************************                                           
194600*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
194700     MOVE ZERO TO NOLL-RAKNARE                                            
194800     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
194900     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
195000     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
195100          FOR LEADING ZERO                                                
195200     ADD +1 TO NOLL-RAKNARE                                               
195300     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
195400          WITH POINTER NOLL-RAKNARE                                       
195500*******************************                                           
195600     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
195700     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
195800                                         EKO-EKH-PRARTNTO                 
195900                                         EKO-EKH-PRARTSJK                 
196000                                         EKO-EKH-PRLANDCO                 
196100                                         EKO-EKH-SUBEL                    
196200     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
196300     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
196400                                                                          
196500     MOVE 1.00                        TO EKO-EKH-PRKURS                   
196600                                                                          
196700     MOVE ZERO                        TO EKO-EKH-PRINK                    
196800                                                                          
196900     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
197000     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
197100     MOVE SPACE                       TO EKO-EKH-BEVAT                    
197200                                         EKO-EKH-KDANMORS                 
197300                                         EKO-EKH-IDKST                    
197400     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
197500                                         EKO-EKH-KDFRAKT                  
197600                                         EKO-EKH-SUVAT                    
197700                                         EKO-EKH-PRHEMTAG                 
197800                                         EKO-EKH-KVANTAL                  
197900*    COMPUTE EKO-EKH-SUBEL = WS-SUDIRLON + WS-SUDIRMTRL +                 
198000*                            WS-SUHEMT + WS-SUARTSTD                      
198100     COMPUTE EKO-EKH-SUBEL = WS-SUARTSTD                                  
198200                                                                          
198300     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
198400     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
198500     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
198600     MOVE 0                          TO EKO-EKH-KDAVVTYP                  
198700     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
198800     MOVE ZERO                       TO EKO-EKH-KVANTMOT                  
198900     MOVE ZERO                       TO EKO-EKH-KVAVIS                    
199000     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
199100                                                                          
199200     MOVE WS-ARTC-KDVALISO           TO EKO-EKH-KDVALISO                  
199300     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
199400     EVALUATE TRUE                                                        
199500       WHEN NDC-CN                                                        
199600         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
199700       WHEN NDC-IN                                                        
199800         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
199900       WHEN NDC-US                                                        
200000         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
200100       WHEN OTHER                                                         
200200         IF NDC-KR                                                        
200210         OR NDC-MX                                                        
200220         OR NDC-BR                                                        
200230         OR NDC-ZA                                                        
200300           MOVE '103'                TO EKO-EKH-KDEKHHT                   
200400           MOVE '106'                TO EKO-EKH-KDEKSHT                   
200500         END-IF                                                           
200600         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
200700     END-EVALUATE                                                         
200800     MOVE 'BAK'                      TO EKO-EKH-CMD                       
200900     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
201000     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
201100                                        EKO-EKH-PRDMTRL                   
201200                                        EKO-EKH-PROVRPAL                  
201300     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
201400     MOVE SPACE                      TO EKO-EKH-FLDCET                    
201500     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
201600                                                                          
201700     PERFORM IMS-ISRT-EKOTRANS                                            
201800     PERFORM UNTIL SEGMENT-FINNS                                          
201900       ADD +1                     TO EKO-FIL-IDSEKVNR                     
202000       PERFORM IMS-ISRT-EKOTRANS                                          
202100     END-PERFORM                                                          
202200     .                                                                    
202300     EJECT                                                                
202400                                                                          
202500 HFDBB-LOGG-EKO-VCCS SECTION.                                             
202600     MOVE SPACE                      TO EKO-W51080                        
202700     MOVE SPACE                      TO WS-SAP-MM-POST                    
202800                                                                          
202900     MOVE INLA-ART-IDARTNR           TO EKO-IDARTNR                       
203000     MOVE INLA-ART-IDDC              TO EKO-IDDC                          
203100     MOVE INLA-INL-IDFS              TO EKO-IDFS                          
203200     MOVE WS-ARTC-IDINK-X            TO EKO-IDINK                         
203300     MOVE INLA-INL-IDKONTO           TO EKO-IDKONTO                       
203400     MOVE INLA-INL-IDLEVNR           TO EKO-IDLEVNR                       
203500     MOVE INLA-ART-IDLOPNRM          TO EKO-IDLOPNRM                      
203600     MOVE WS-ARTC-KDPRODSL           TO EKO-KDPRODSL                      
203700     MOVE INLA-ART-KDRT              TO EKO-KDRT                          
203800     MOVE WS-ARTC-KDSORT             TO EKO-KDSORT                        
203900     MOVE WS-ARTC-KDTIPPR            TO EKO-KDTIPPR                       
204000     MOVE WS-ARTC-KDVALISO           TO EKO-KDVALISO                      
204100*                                                                         
204200     IF INLA-INL-IDLEVNR = '1441'                                         
204300       MOVE NEJ                      TO WS-SAP-MM-POST                    
204400     END-IF                                                               
204500*                                                                         
204600     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
204700       MOVE WS-ARTC-PRARTBEL-PR      TO EKO-PRARTBEL-PR                   
204800     ELSE                                                                 
204900*  -- OBS! RADPRIS SAKNAS, KDVALISO=XXX SIGNALERAR DETTA FÖR LEVA1        
205000       MOVE 0.1                      TO EKO-PRARTBEL-PR                   
205100       MOVE 'XXX'                    TO EKO-KDVALISO                      
205200     END-IF                                                               
205300     IF  WS-ARTC-PRARTBES-PR   > ZERO                                     
205400       MOVE WS-ARTC-PRARTBES-PR      TO EKO-PRARTBES                      
205500     ELSE                                                                 
205600       MOVE 0.1                      TO EKO-PRARTBES                      
205700     END-IF                                                               
205800     COMPUTE EKO-KVAVIS = INLA-ART-KVAVIS * -1                            
205900     END-COMPUTE                                                          
206000     MOVE EKO-KVAVIS TO WS-SPAR-KVAVIS                                    
206100     MOVE WS-ARTC-PRINK              TO EKO-PRINK                         
206200     MOVE WS-ARTC-PRHEMTAG           TO EKO-PRHEMTAG                      
206300     MOVE ZERO                       TO EKO-RETULF                        
206400     MOVE INLA-INL-TIAVIDAT          TO EKO-TIAVIDAT                      
206500     MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                      
206600     MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                       
206700     MOVE JA                         TO EKO-FLLSBOK                       
206800     MOVE ZERO                       TO EKO-IDDISTR                       
206900     MOVE NEJ                        TO EKO-FLDIRLEV                      
207000                                                                          
207100     MOVE JA                         TO EKO-FLAVVINL                      
207200     MOVE WS-ARTC23-IDAVTAL          TO EKO-IDAVTAL                       
207300     MOVE 'V'                        TO EKO-KDINLAVV                      
207400     MOVE 57                         TO EKO-IDFTG                         
207500                                                                          
207600     MOVE 'W6011910'                 TO EKO-FIL-IDPGM                     
207700     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
207800     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
207900     MOVE 1                          TO EKO-FIL-IDSEKVNR                  
208000     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
208100     MOVE '80 '                      TO EKO-FIL-CT-IDPTYP                 
208200     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
208300                                                                          
208400     IF WS-SAP-MM-POST = NEJ                                              
208500        CONTINUE                                                          
208600     ELSE                                                                 
208700       PERFORM IMS-ISRT-EKOTRANS                                          
208800                                                                          
208900       PERFORM UNTIL SEGMENT-FINNS                                        
209000         ADD +1 TO EKO-FIL-IDSEKVNR                                       
209100         PERFORM IMS-ISRT-EKOTRANS                                        
209200       END-PERFORM                                                        
209300     END-IF                                                               
209400                                                                          
209500     MOVE 'W6011910'                  TO FIL-IDPGM                        
209600     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
209700                                         EKH-DAVERDAT                     
209800     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
209900     MOVE +1                          TO FIL-IDSEKVNR                     
210000     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
210100     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
210200     MOVE '103'                       TO EKH-KDEKHHT                      
210300     MOVE '102'                       TO EKH-KDEKSHT                      
210400     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
210500     MOVE INLA-ART-IDDC               TO EKH-IDDC-SEND                    
210600     MOVE SPACE                       TO EKH-IDDC-REC                     
210700     MOVE +0                          TO EKH-IDDISTR                      
210800                                         EKH-IDKUNDNR                     
210900*******************************                                           
211000*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
211100     MOVE ZERO TO NOLL-RAKNARE                                            
211200     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
211300     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
211400     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
211500          FOR LEADING ZERO                                                
211600     ADD +1 TO NOLL-RAKNARE                                               
211700     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
211800          WITH POINTER NOLL-RAKNARE                                       
211900*******************************                                           
212000     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
212100     MOVE ZERO                        TO EKH-KDPSLLOC                     
212200                                         EKH-PRARTNTO                     
212300                                         EKH-PRARTSJK                     
212400                                         EKH-PRLANDCO                     
212500                                         EKH-SUBEL                        
212600     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
212700     MOVE SPACE                       TO EKH-FLLSBOK                      
212800                                                                          
212900     IF EKO-KDVALISO = 'XXX'                                              
213000       MOVE 'SEK'                     TO EKH-KDVALISO                     
213100     ELSE                                                                 
213200       MOVE EKO-KDVALISO              TO EKH-KDVALISO                     
213300     END-IF                                                               
213400     MOVE 1.00                        TO EKH-PRKURS                       
213500                                                                          
213600     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
213700     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
213800     IF INLA-INL-IDLEVNR = '1002 '                                        
213900       MOVE ZERO                      TO EKH-PRHEMTAG                     
214000     ELSE                                                                 
214100       MOVE WS-ARTC-PRHEMTAG          TO EKH-PRHEMTAG                     
214200     END-IF                                                               
214300     COMPUTE EKH-KVANTAL = INLA-ART-KVAVIS * -1                           
214400     END-COMPUTE                                                          
214500     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
214600     MOVE SPACE                       TO EKH-BEVAT                        
214700                                         EKH-KDANMORS                     
214800                                         EKH-IDKST                        
214900     MOVE ZERO                        TO EKH-IDKONTO                      
215000                                         EKH-KDFRAKT                      
215100                                         EKH-SUVAT                        
215200                                                                          
215300                                                                          
215400     MOVE WS-INLA-TIAVIDAT           TO EKH-DAAVIDAT                      
215500     MOVE WS-IDAVINR                 TO EKH-IDAVINR                       
215600     MOVE INLA-INL-IDLEVNR           TO EKH-IDLEVNR                       
215700     IF INLA-ART-KVAVIS > ZERO                                            
215800       MOVE 1                        TO EKH-KDAVVTYP                      
215900     ELSE                                                                 
216000       MOVE 0                        TO EKH-KDAVVTYP                      
216100     END-IF                                                               
216200     MOVE INLA-ART-KDRT              TO EKH-KDRT                          
216300     MOVE WS-INLE-MOT-KVANTMOT       TO EKH-KVANTMOT                      
216400     MOVE INLA-ART-KVAVIS            TO EKH-KVAVIS                        
216500     MOVE WS-ARTC-KDSORT             TO EKH-KDSORT                        
216600     MOVE 'SEPV'                     TO EKH-KDTRADP                       
216700                                                                          
216800     MOVE ZERO                       TO EKH-PRDIRLON                      
216900                                        EKH-PRDMTRL                       
217000                                        EKH-PROVRPAL                      
217100     MOVE INLA-INL-IDANALYS          TO EKH-IDANALYS                      
217200     MOVE SPACE                      TO EKH-FLDCET                        
217300     MOVE SPACE                      TO EKH-IDKUNDRF                      
217400     MOVE SPACE                      TO EKH-IDFAKT-EXP                    
217500                                                                          
217600     PERFORM IMS-ISRT-WLSAPA01                                            
217700                                                                          
217800     PERFORM UNTIL SEGMENT-FINNS                                          
217900       ADD +1                     TO FIL-IDSEKVNR                         
218000       PERFORM IMS-ISRT-WLSAPA01                                          
218100     END-PERFORM                                                          
218200     .                                                                    
218300     EJECT                                                                
218400                                                                          
218500 HFE-LOGG-EKO-WDR9 SECTION.                                               
218600     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
218700       PERFORM HFEA-LOGG-EKO-VCCN                                         
218800     ELSE                                                                 
218900       PERFORM HFEB-LOGG-EKO-VCCS                                         
219000     END-IF                                                               
219100     .                                                                    
219200     EJECT                                                                
219300                                                                          
219400 HFEA-LOGG-EKO-VCCN SECTION.                                              
219500     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
219600     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
219700     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
219800     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
219900     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
220000     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
220100     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
220200     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
220300     MOVE +0                          TO EKO-EKH-IDDISTR                  
220400                                         EKO-EKH-IDKUNDNR                 
220500*******************************                                           
220600     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
220700       MOVE WS-ARTC-PRARTBEL-PR      TO EKO-PRARTBEL-PR                   
220800       MOVE WS-ARTC-KDVALISO         TO EKO-EKH-KDVALISO                  
220900     ELSE                                                                 
221000*  -- OBS! RADPRIS SAKNAS, KDVALISO=XXX SIGNALERAR DETTA FÖR LEVA1        
221100       MOVE 0.1                      TO EKO-PRARTBEL-PR                   
221200       MOVE 'XXX'                    TO EKO-KDVALISO                      
221300       MOVE DCS-KDVALISO             TO EKO-EKH-KDVALISO                  
221400     END-IF                                                               
221500*******************************                                           
221600*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
221700     MOVE ZERO TO NOLL-RAKNARE                                            
221800     MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                  
221900     MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM                
222000     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
222100          FOR LEADING ZERO                                                
222200     ADD +1 TO NOLL-RAKNARE                                               
222300     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
222400          WITH POINTER NOLL-RAKNARE                                       
222500*******************************                                           
222600     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
222700     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
222800                                         EKO-EKH-PRARTNTO                 
222900                                         EKO-EKH-PRARTSJK                 
223000                                         EKO-EKH-PRHEMTAG                 
223100                                         EKO-EKH-PRLANDCO                 
223200                                         EKO-EKH-PRDIRLON                 
223300                                         EKO-EKH-PRDMTRL                  
223400                                         EKO-EKH-PROVRPAL                 
223500                                         EKO-EKH-SUBEL                    
223600     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
223700     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
223800                                                                          
223900     MOVE 1.00                        TO EKO-EKH-PRKURS                   
224000                                                                          
224100     MOVE WS-ARTC-PRINK               TO EKO-EKH-PRINK                    
224200*    MOVE ARTS-SLAG-PRAVCOST          TO EKO-EKH-PRARTSTD                 
224300     COMPUTE EKO-EKH-KVANTAL = INLA-ART-KVAVIS * -1                       
224400     END-COMPUTE                                                          
224500     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
224600     MOVE SPACE                       TO EKO-EKH-BEVAT                    
224700                                         EKO-EKH-KDANMORS                 
224800     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
224900                                         EKO-EKH-SUVAT                    
225000                                                                          
225100     MOVE WS-INLA-TIAVIDAT            TO EKO-EKH-DAAVIDAT                 
225200     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
225300     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
225400     IF INLA-ART-KVAVIS > ZERO                                            
225500       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
225600     ELSE                                                                 
225700       MOVE 0                         TO EKO-EKH-KDAVVTYP                 
225800     END-IF                                                               
225900     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
226000     MOVE WS-INLE-MOT-KVANTMOT        TO EKO-EKH-KVANTMOT                 
226100     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
226200     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
226300     MOVE SPACE                       TO EKO-EKH-FLDCET                   
226400     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
226500     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
226600     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
226700     EVALUATE TRUE                                                        
226800       WHEN NDC-CN                                                        
226900         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
227000       WHEN NDC-IN                                                        
227100         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
227200       WHEN NDC-US                                                        
227300         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
227400       WHEN OTHER                                                         
227500         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
227600     END-EVALUATE                                                         
227700     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
227800                                                                          
227900     IF INLA-ART-KDRT NOT = 6                                             
228000       MOVE '102'                     TO EKO-EKH-KDEKHHT                  
228100       MOVE '104'                     TO EKO-EKH-KDEKSHT                  
228200       MOVE SPACE                     TO EKO-EKH-IDANALYS                 
228300                                         EKO-EKH-IDKST                    
228400       MOVE ZERO                      TO EKO-EKH-IDKONTO                  
228500       IF  WS-ARTC-PRARTBEL-PR   > ZERO                                   
228600         MOVE WS-ARTC-PRARTBEL-PR       TO EKO-EKH-PRARTSTD               
228700       ELSE                                                               
228800         MOVE 1                         TO EKO-EKH-PRARTSTD               
228900       END-IF                                                             
229000     ELSE                                                                 
229100       MOVE '102'                     TO EKO-EKH-KDEKHHT                  
229200       MOVE '102'                     TO EKO-EKH-KDEKSHT                  
229300                                                                          
229400       MOVE INLA-INL-IDANALYS         TO EKO-EKH-IDANALYS                 
229500       MOVE INLA-INL-IDKONTO          TO EKO-EKH-IDKONTO                  
229600       MOVE INLA-INL-IDKST            TO EKO-EKH-IDKST                    
229700       IF  WS-ARTC-PRARTBEL-PR   > ZERO                                   
229800         MOVE WS-ARTC-PRARTBEL-PR       TO EKO-EKH-PRARTSTD               
229900       ELSE                                                               
230000         MOVE 0.1                       TO EKO-EKH-PRARTSTD               
230100**** TAIWAN NEEDS A INTEGER VALUE AS TAIWAN DOSENT USE                    
230200**** DECIMALS. 0.1 MAKES THE BOOKING TO ZERO WHICH IS NOT                 
230300**** VALID IN SAP SYSTEM.                                                 
230400         IF DCS-TAIWAN                                                    
230500           MOVE 1.0                     TO EKO-EKH-PRARTSTD               
230600         END-IF                                                           
230700       END-IF                                                             
230800     END-IF                                                               
230900                                                                          
231000     IF INLA-ART-KDRT = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7                   
231100       PERFORM IMS-ISRT-EKOTRANS                                          
231200                                                                          
231300       PERFORM UNTIL SEGMENT-FINNS                                        
231400         ADD +1                       TO EKO-FIL-IDSEKVNR                 
231500         PERFORM IMS-ISRT-EKOTRANS                                        
231600       END-PERFORM                                                        
231700     END-IF                                                               
231800     .                                                                    
231900     EJECT                                                                
232000                                                                          
232100 HFEB-LOGG-EKO-VCCS SECTION.                                              
232200     MOVE 'W6011910'                  TO FIL-IDPGM                        
232300     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
232400                                         EKH-DAVERDAT                     
232500     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
232600     MOVE +1                          TO FIL-IDSEKVNR                     
232700     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
232800     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
232900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
233000     MOVE INLA-ART-IDDC               TO EKH-IDDC-SEND                    
233100     MOVE SPACE                       TO EKH-IDDC-REC                     
233200     MOVE +0                          TO EKH-IDDISTR                      
233300                                         EKH-IDKUNDNR                     
233400*******************************                                           
233500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
233600     MOVE ZERO TO NOLL-RAKNARE                                            
233700     MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                  
233800     MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM                
233900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
234000          FOR LEADING ZERO                                                
234100     ADD +1 TO NOLL-RAKNARE                                               
234200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
234300          WITH POINTER NOLL-RAKNARE                                       
234400*******************************                                           
234500     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
234600     MOVE ZERO                        TO EKH-KDPSLLOC                     
234700                                         EKH-PRARTNTO                     
234800                                         EKH-PRARTSJK                     
234900                                         EKH-PRHEMTAG                     
235000                                         EKH-PRLANDCO                     
235100                                         EKH-PRDIRLON                     
235200                                         EKH-PRDMTRL                      
235300                                         EKH-PROVRPAL                     
235400                                         EKH-SUBEL                        
235500     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
235600     MOVE SPACE                       TO EKH-FLLSBOK                      
235700                                                                          
235800     MOVE 'SEK'                       TO EKH-KDVALISO                     
235900     MOVE 1.00                        TO EKH-PRKURS                       
236000                                                                          
236100     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
236200     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
236300     COMPUTE EKH-KVANTAL = INLA-ART-KVAVIS * -1                           
236400     END-COMPUTE                                                          
236500     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
236600     MOVE SPACE                       TO EKH-BEVAT                        
236700                                         EKH-KDANMORS                     
236800     MOVE ZERO                        TO EKH-KDFRAKT                      
236900                                         EKH-SUVAT                        
237000                                                                          
237100                                                                          
237200     MOVE WS-INLA-TIAVIDAT            TO EKH-DAAVIDAT                     
237300     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
237400     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
237500     IF INLA-ART-KVAVIS > ZERO                                            
237600       MOVE 1                         TO EKH-KDAVVTYP                     
237700     ELSE                                                                 
237800       MOVE 0                         TO EKH-KDAVVTYP                     
237900     END-IF                                                               
238000     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
238100     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
238200     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
238300     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
238400     MOVE 'SEPV'                      TO EKH-KDTRADP                      
238500     MOVE SPACE                       TO EKH-FLDCET                       
238600     MOVE SPACE                       TO EKH-IDKUNDRF                     
238700     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
238800                                                                          
238900     IF INLA-ART-KDRT NOT = 6                                             
239000       MOVE '102'                     TO EKH-KDEKHHT                      
239100       MOVE '104'                     TO EKH-KDEKSHT                      
239200       MOVE SPACE                     TO EKH-IDANALYS                     
239300                                         EKH-IDKST                        
239400       MOVE ZERO                      TO EKH-IDKONTO                      
239500     ELSE                                                                 
239600       MOVE '102'                     TO EKH-KDEKHHT                      
239700       MOVE '102'                     TO EKH-KDEKSHT                      
239800                                                                          
239900       MOVE INLA-INL-IDANALYS         TO EKH-IDANALYS                     
240000       MOVE INLA-INL-IDKONTO          TO EKH-IDKONTO                      
240100       MOVE INLA-INL-IDKST            TO EKH-IDKST                        
240200     END-IF                                                               
240300                                                                          
240400     IF INLA-ART-KDRT = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7                   
240500       PERFORM IMS-ISRT-WLSAPA01                                          
240600                                                                          
240700       PERFORM UNTIL SEGMENT-FINNS                                        
240800         ADD +1                       TO FIL-IDSEKVNR                     
240900         PERFORM IMS-ISRT-WLSAPA01                                        
241000       END-PERFORM                                                        
241100     END-IF                                                               
241200     .                                                                    
241300     EJECT                                                                
241400                                                                          
241500 HFF-LOGG-EKO-WDR9-PALAGG SECTION.                                        
241600     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
241700       PERFORM HFFA-LOGG-EKO-VCCN                                         
241800     ELSE                                                                 
241900       PERFORM HFFB-LOGG-EKO-VCCS                                         
242000     END-IF                                                               
242100     .                                                                    
242200     EJECT                                                                
242300                                                                          
242400 HFFA-LOGG-EKO-VCCN SECTION.                                              
242500     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
242600     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
242700     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
242800     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
242900     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
243000     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
243100     MOVE '101'                       TO EKO-EKH-KDEKSHT                  
243200     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
243300     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
243400     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
243500     MOVE +0                          TO EKO-EKH-IDDISTR                  
243600                                         EKO-EKH-IDKUNDNR                 
243700*******************************                                           
243800*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
243900     MOVE ZERO TO NOLL-RAKNARE                                            
244000     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
244100     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
244200     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
244300          FOR LEADING ZERO                                                
244400     ADD +1 TO NOLL-RAKNARE                                               
244500     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
244600          WITH POINTER NOLL-RAKNARE                                       
244700*******************************                                           
244800     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
244900     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
245000                                         EKO-EKH-PRARTNTO                 
245100                                         EKO-EKH-PRARTSJK                 
245200                                         EKO-EKH-PRHEMTAG                 
245300                                         EKO-EKH-PRLANDCO                 
245400                                         EKO-EKH-SUBEL                    
245500     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
245600     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
245700                                                                          
245800     MOVE 1.00                        TO EKO-EKH-PRKURS                   
245900                                                                          
246000     MOVE ZERO                        TO EKO-EKH-PRINK                    
246100     MOVE ARTS-SLAG-PRAVCOST          TO EKO-EKH-PRARTSTD                 
246200     COMPUTE EKO-EKH-KVANTAL = INLA-ART-KVAVIS * -1                       
246300     END-COMPUTE                                                          
246400     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
246500     MOVE SPACE                       TO EKO-EKH-BEVAT                    
246600                                         EKO-EKH-KDANMORS                 
246700                                         EKO-EKH-IDKST                    
246800     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
246900                                         EKO-EKH-KDFRAKT                  
247000                                         EKO-EKH-SUVAT                    
247100                                                                          
247200                                                                          
247300     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
247400     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
247500     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
247600     IF INLA-ART-KVAVIS > ZERO                                            
247700       MOVE 1                        TO EKO-EKH-KDAVVTYP                  
247800     ELSE                                                                 
247900       MOVE 0                        TO EKO-EKH-KDAVVTYP                  
248000     END-IF                                                               
248100     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
248200     MOVE WS-INLE-MOT-KVANTMOT       TO EKO-EKH-KVANTMOT                  
248300     MOVE INLA-ART-KVAVIS            TO EKO-EKH-KVAVIS                    
248400     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
248500     MOVE SPACE                      TO EKO-EKH-FLDCET                    
248600     MOVE SPACE                      TO EKO-EKH-IDKUNDRF                  
248700     MOVE SPACE                      TO EKO-EKH-IDFAKT-EXP                
248800     MOVE DCS-KDVALISO               TO EKO-EKH-KDVALISO                  
248900     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
249000     EVALUATE TRUE                                                        
249100       WHEN NDC-CN                                                        
249200         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
249300       WHEN NDC-IN                                                        
249400         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
249500       WHEN NDC-US                                                        
249600         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
249700       WHEN OTHER                                                         
249800         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
249900     END-EVALUATE                                                         
250000     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
250100                                                                          
250200     MOVE DAGENS-DATUM(3:2) TO W-DATE-AAMM(1:2)                           
250300     MOVE 01                TO W-DATE-AAMM(3:2)                           
250400     MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                          
250500     MOVE DCS-KDVALISO      TO CURR-KDVALISO-ROW                          
250600     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
250700     MOVE 'A'               TO CURR-KDVALTYP                              
250800     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
250900     IF CURR-KDSVAR = ' '                                                 
251000       MOVE CURR-PRKURS-NEW  TO WS-PRKURS                                 
251100       MOVE CURR-REVALUTA-TO TO WS-REVALUTA                               
251200     ELSE                                                                 
251300       MOVE 1                TO WS-PRKURS                                 
251400       MOVE 1                TO WS-REVALUTA                               
251500     END-IF                                                               
251600     PERFORM IMS-GU-WDB601                                                
251700     MOVE DCS-IDLANDX2            TO W-IDLANDX2                           
251800     IF SEGMENT-FINNS                                                     
251900       PERFORM IMS-GNP-WDB617                                             
252000       IF SEGMENT-FINNS                                                   
252100         CONTINUE                                                         
252200       ELSE                                                               
252300         MOVE ZERO TO PROC-REDIRLON                                       
252400         MOVE ZERO TO PROC-REDMTRL                                        
252500       END-IF                                                             
252600     ELSE                                                                 
252700       MOVE ZERO TO PROC-REDIRLON                                         
252800       MOVE ZERO TO PROC-REDMTRL                                          
252900     END-IF                                                               
253000     COMPUTE EKO-EKH-PRDIRLON ROUNDED = WS-ARTC-PRDIRLON *                
253100                    PROC-REDIRLON /  WS-PRKURS / WS-REVALUTA              
253200     COMPUTE EKO-EKH-PRDMTRL  ROUNDED = WS-ARTC-PRDMTRL  *                
253300                    PROC-REDMTRL  /  WS-PRKURS / WS-REVALUTA              
253400     MOVE ZERO                       TO EKO-EKH-PROVRPAL                  
253500     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
253600                                                                          
253700     IF EKO-EKH-PRDIRLON > ZERO                                           
253800     OR EKO-EKH-PRDMTRL  > ZERO                                           
253900       PERFORM IMS-ISRT-EKOTRANS                                          
254000                                                                          
254100       PERFORM UNTIL SEGMENT-FINNS                                        
254200         ADD +1                     TO EKO-FIL-IDSEKVNR                   
254300         PERFORM IMS-ISRT-EKOTRANS                                        
254400       END-PERFORM                                                        
254500     END-IF                                                               
254600     .                                                                    
254700     EJECT                                                                
254800                                                                          
254900 HFFB-LOGG-EKO-VCCS SECTION.                                              
255000     MOVE 'W6011910'                  TO FIL-IDPGM                        
255100     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
255200                                         EKH-DAVERDAT                     
255300     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
255400     MOVE +1                          TO FIL-IDSEKVNR                     
255500     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
255600     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
255700     MOVE '103'                       TO EKH-KDEKHHT                      
255800     MOVE '101'                       TO EKH-KDEKSHT                      
255900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
256000     MOVE INLA-ART-IDDC               TO EKH-IDDC-SEND                    
256100     MOVE SPACE                       TO EKH-IDDC-REC                     
256200     MOVE +0                          TO EKH-IDDISTR                      
256300                                         EKH-IDKUNDNR                     
256400*******************************                                           
256500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
256600     MOVE ZERO TO NOLL-RAKNARE                                            
256700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
256800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
256900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
257000          FOR LEADING ZERO                                                
257100     ADD +1 TO NOLL-RAKNARE                                               
257200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
257300          WITH POINTER NOLL-RAKNARE                                       
257400*******************************                                           
257500     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
257600     MOVE ZERO                        TO EKH-KDPSLLOC                     
257700                                         EKH-PRARTNTO                     
257800                                         EKH-PRARTSJK                     
257900                                         EKH-PRHEMTAG                     
258000                                         EKH-PRLANDCO                     
258100                                         EKH-SUBEL                        
258200     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
258300     MOVE SPACE                       TO EKH-FLLSBOK                      
258400                                                                          
258500     MOVE 'SEK'                       TO EKH-KDVALISO                     
258600     MOVE 1.00                        TO EKH-PRKURS                       
258700                                                                          
258800     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
258900     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
259000     COMPUTE EKH-KVANTAL = INLA-ART-KVAVIS * -1                           
259100     END-COMPUTE                                                          
259200     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
259300     MOVE SPACE                       TO EKH-BEVAT                        
259400                                         EKH-KDANMORS                     
259500                                         EKH-IDKST                        
259600     MOVE ZERO                        TO EKH-IDKONTO                      
259700                                         EKH-KDFRAKT                      
259800                                         EKH-SUVAT                        
259900                                                                          
260000                                                                          
260100     MOVE WS-INLA-TIAVIDAT           TO EKH-DAAVIDAT                      
260200     MOVE WS-IDAVINR                 TO EKH-IDAVINR                       
260300     MOVE INLA-INL-IDLEVNR           TO EKH-IDLEVNR                       
260400     IF INLA-ART-KVAVIS > ZERO                                            
260500       MOVE 1                        TO EKH-KDAVVTYP                      
260600     ELSE                                                                 
260700       MOVE 0                        TO EKH-KDAVVTYP                      
260800     END-IF                                                               
260900     MOVE INLA-ART-KDRT              TO EKH-KDRT                          
261000     MOVE WS-INLE-MOT-KVANTMOT       TO EKH-KVANTMOT                      
261100     MOVE INLA-ART-KVAVIS            TO EKH-KVAVIS                        
261200     MOVE WS-ARTC-KDSORT             TO EKH-KDSORT                        
261300     MOVE 'SEPV'                     TO EKH-KDTRADP                       
261400     MOVE SPACE                      TO EKH-FLDCET                        
261500     MOVE SPACE                      TO EKH-IDKUNDRF                      
261600     MOVE SPACE                      TO EKH-IDFAKT-EXP                    
261700                                                                          
261800     MOVE WS-ARTC-PRDIRLON           TO EKH-PRDIRLON                      
261900     MOVE WS-ARTC-PRDMTRL            TO EKH-PRDMTRL                       
262000     MOVE WS-ARTC-PROVRPAL           TO EKH-PROVRPAL                      
262100     MOVE INLA-INL-IDANALYS          TO EKH-IDANALYS                      
262200                                                                          
262300     IF EKH-PRDIRLON > ZERO                                               
262400     OR EKH-PRDMTRL > ZERO                                                
262500       PERFORM IMS-ISRT-WLSAPA01                                          
262600                                                                          
262700       PERFORM UNTIL SEGMENT-FINNS                                        
262800         ADD +1                   TO FIL-IDSEKVNR                         
262900         PERFORM IMS-ISRT-WLSAPA01                                        
263000       END-PERFORM                                                        
263100     END-IF                                                               
263200     .                                                                    
263300     EJECT                                                                
263400                                                                          
263500 HFG-LOGG-EKO-R31-WDR9-RT6 SECTION.                                       
263600     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
263700       PERFORM HFFA-LOGG-EKO-VCCN                                         
263800     ELSE                                                                 
263900       PERFORM HFFB-LOGG-EKO-VCCS                                         
264000     END-IF                                                               
264100     .                                                                    
264200     EJECT                                                                
264300                                                                          
264400 HFGA-LOGG-EKO-VCCN SECTION.                                              
264500     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
264600     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
264700     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
264800     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
264900     MOVE 1                           TO EKO-FIL-IDSEKVNR                 
265000     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
265100     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
265200     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
265300     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
265400     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
265500     MOVE +0                          TO EKO-EKH-IDDISTR                  
265600                                         EKO-EKH-IDKUNDNR                 
265700*******************************                                           
265800*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
265900     MOVE ZERO TO NOLL-RAKNARE                                            
266000     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
266100     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
266200     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
266300          FOR LEADING ZERO                                                
266400     ADD +1 TO NOLL-RAKNARE                                               
266500     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
266600          WITH POINTER NOLL-RAKNARE                                       
266700*******************************                                           
266800     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
266900     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
267000                                         EKO-EKH-PRARTNTO                 
267100                                         EKO-EKH-PRARTSJK                 
267200                                         EKO-EKH-PRHEMTAG                 
267300                                         EKO-EKH-PRLANDCO                 
267400                                         EKO-EKH-PRDIRLON                 
267500                                         EKO-EKH-PRDMTRL                  
267600                                         EKO-EKH-PROVRPAL                 
267700                                         EKO-EKH-SUBEL                    
267800     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
267900     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
268000                                                                          
268100     MOVE 1.00                        TO EKO-EKH-PRKURS                   
268200                                                                          
268300     MOVE WS-ARTC-PRINK               TO EKO-EKH-PRINK                    
268400     MOVE ARTS-SLAG-PRAVCOST          TO EKO-EKH-PRARTSTD                 
268500     COMPUTE EKO-EKH-KVANTAL = INLA-ART-KVAVIS * -1                       
268600     END-COMPUTE                                                          
268700     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
268800     MOVE INLA-INL-IDANALYS           TO EKO-EKH-IDANALYS                 
268900     MOVE INLA-INL-IDKONTO            TO EKO-EKH-IDKONTO                  
269000     MOVE INLA-INL-IDKST              TO EKO-EKH-IDKST                    
269100     MOVE SPACE                       TO EKO-EKH-BEVAT                    
269200                                         EKO-EKH-KDANMORS                 
269300     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
269400                                         EKO-EKH-SUVAT                    
269500                                                                          
269600     MOVE WS-INLA-TIAVIDAT            TO EKO-EKH-DAAVIDAT                 
269700     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
269800     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
269900     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
270000     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
270100     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
270200     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
270300     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
270400     MOVE SPACE                       TO EKO-EKH-FLDCET                   
270500     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
270600     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
270700     MOVE DCS-KDVALISO               TO EKO-EKH-KDVALISO                  
270800     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
270900     EVALUATE TRUE                                                        
271000       WHEN NDC-CN                                                        
271100         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
271200       WHEN NDC-IN                                                        
271300         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
271400       WHEN NDC-US                                                        
271500         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
271600       WHEN OTHER                                                         
271700         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
271800     END-EVALUATE                                                         
271900     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
272000                                                                          
272100                                                                          
272200     PERFORM IMS-ISRT-EKOTRANS                                            
272300                                                                          
272400     PERFORM UNTIL SEGMENT-FINNS                                          
272500       ADD +1 TO EKO-FIL-IDSEKVNR                                         
272600       PERFORM IMS-ISRT-EKOTRANS                                          
272700     END-PERFORM                                                          
272800     .                                                                    
272900     EJECT                                                                
273000                                                                          
273100 HFGB-LOGG-EKO-VCCS SECTION.                                              
273200     MOVE 'W6011910'                  TO FIL-IDPGM                        
273300     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
273400                                         EKH-DAVERDAT                     
273500     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
273600     MOVE 1                           TO FIL-IDSEKVNR                     
273700     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
273800     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
273900     MOVE '102'                       TO EKH-KDEKHHT                      
274000     MOVE '102'                       TO EKH-KDEKSHT                      
274100     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
274200     MOVE INLA-ART-IDDC               TO EKH-IDDC-SEND                    
274300     MOVE SPACE                       TO EKH-IDDC-REC                     
274400     MOVE +0                          TO EKH-IDDISTR                      
274500                                         EKH-IDKUNDNR                     
274600*******************************                                           
274700*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
274800     MOVE ZERO TO NOLL-RAKNARE                                            
274900     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
275000     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
275100     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
275200          FOR LEADING ZERO                                                
275300     ADD +1 TO NOLL-RAKNARE                                               
275400     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
275500          WITH POINTER NOLL-RAKNARE                                       
275600*******************************                                           
275700     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
275800     MOVE ZERO                        TO EKH-KDPSLLOC                     
275900                                         EKH-PRARTNTO                     
276000                                         EKH-PRARTSJK                     
276100                                         EKH-PRHEMTAG                     
276200                                         EKH-PRLANDCO                     
276300                                         EKH-PRDIRLON                     
276400                                         EKH-PRDMTRL                      
276500                                         EKH-PROVRPAL                     
276600                                         EKH-SUBEL                        
276700     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
276800     MOVE SPACE                       TO EKH-FLLSBOK                      
276900                                                                          
277000     MOVE 'SEK'                       TO EKH-KDVALISO                     
277100     MOVE 1.00                        TO EKH-PRKURS                       
277200                                                                          
277300     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
277400     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
277500     COMPUTE EKH-KVANTAL = INLA-ART-KVAVIS * -1                           
277600     END-COMPUTE                                                          
277700     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
277800     MOVE INLA-INL-IDANALYS           TO EKH-IDANALYS                     
277900     MOVE INLA-INL-IDKONTO            TO EKH-IDKONTO                      
278000     MOVE INLA-INL-IDKST              TO EKH-IDKST                        
278100     MOVE SPACE                       TO EKH-BEVAT                        
278200                                         EKH-KDANMORS                     
278300     MOVE ZERO                        TO EKH-KDFRAKT                      
278400                                         EKH-SUVAT                        
278500                                                                          
278600     MOVE WS-INLA-TIAVIDAT            TO EKH-DAAVIDAT                     
278700     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
278800     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
278900     MOVE ZERO                        TO EKH-KDAVVTYP                     
279000     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
279100     MOVE ZERO                        TO EKH-KVANTMOT                     
279200     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
279300     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
279400     MOVE SPACE                       TO EKH-KDTRADP                      
279500     MOVE SPACE                       TO EKH-FLDCET                       
279600     MOVE SPACE                       TO EKH-IDKUNDRF                     
279700     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
279800                                                                          
279900                                                                          
280000     PERFORM IMS-ISRT-WLSAPA01                                            
280100                                                                          
280200     PERFORM UNTIL SEGMENT-FINNS                                          
280300       ADD +1 TO FIL-IDSEKVNR                                             
280400       PERFORM IMS-ISRT-WLSAPA01                                          
280500     END-PERFORM                                                          
280600     .                                                                    
280700     EJECT                                                                
280800 HFH-LOGG-092-M103 SECTION.                                               
280900                                                                          
281000     MOVE INLA-ART-IDLOPNRM      TO M103-IDLOPNRM                         
281100     MOVE WS-ARTC-IDANSK         TO M103-IDANSKNR                         
281200     MOVE INLA-INL-IDLEVNR       TO M103-IDLEVNR                          
281300     COMPUTE M103-KVDIFF         = INLA-ART-KVAVIS * -1                   
281400     MOVE ZERO                   TO M103-KDANTAV                          
281500     MOVE WS-ARTC-PRINK          TO M103-PRINK                            
281600     MOVE INLA-ART-PRARTSTD      TO M103-PRARTSTD                         
281700                                                                          
281800     PERFORM S03-RED-W211FEL-GNRL                                         
281900     MOVE '103'                  TO W211FEL-IDFELKODX                     
282000     MOVE M103-M103              TO W211FEL-FELMED                        
282100                                                                          
282200     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
282300     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
282400     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
282500     PERFORM S02-SKAPA-ZZAC01                                             
282600     .                                                                    
282700     EJECT                                                                
282800 HH-UPPD-INLA SECTION.                                                    
282900                                                                          
283000     PERFORM IMS-GHU-INLA-ART                                             
283100     MOVE JA                     TO INLA-ART-FLKLAR                       
283200                                    INLA-ART-FLANNULL                     
283300     MOVE WS-TIAAMMDD            TO INLA-ART-TIUPPDAT                     
283400     PERFORM IMS-REPL-INLA-ART                                            
283500                                                                          
283600     PERFORM IMS-GHNP-INLA-RAD                                            
283700     PERFORM UNTIL SEGMENT-SAKNAS                                         
283800       MOVE 'MAK'                TO INLA-RAD-KDINLSTA                     
283900       MOVE SPACE                TO INLA-RAD-ADINLOMR                     
284000                                    INLA-RAD-ADINLOMR-NXT                 
284100       MOVE ZERO                 TO INLA-RAD-IDILIST                      
284200                                    INLA-RAD-IDILIRAD                     
284300                                    INLA-RAD-IDINLVGN                     
284400       PERFORM IMS-REPL-INLA-RAD                                          
284500       PERFORM IMS-GHNP-INLA-RAD                                          
284600     END-PERFORM                                                          
284700     .                                                                    
284800     EJECT                                                                
284900 HI-UPPD-LASA SECTION.                                                    
285000                                                                          
285100     PERFORM IMS-GU-INLA-INL                                              
285200     IF INLA-INL-IDLBBET NOT = SPACE                                      
285300       MOVE INLA-INL-IDLBBET  TO WL-IDLBBET                               
285400       MOVE INLA-INL-IDLEVNR  TO WLS1-IDLEVNR                             
285500       MOVE INLA-INL-IDFS     TO WLS1-IDFS                                
285600       MOVE INLA-INL-TIAVIDAT TO WLS1-TIAVIDAT                            
285700       MOVE W-IDARTNR    TO WLS1-IDARTNR                                  
285800       PERFORM IMS-GU-LASA-W6G210                                         
285900       IF SEGMENT-FINNS                                                   
286000         PERFORM IMS-GHNP-LASA-W6G215-SOEK                                
286100         PERFORM UNTIL SEGMENT-SAKNAS                                     
286200           PERFORM IMS-DLET-LASA-W6G215                                   
286300           PERFORM IMS-GHNP-LASA-W6G215-SOEK                              
286400         END-PERFORM                                                      
286500       END-IF                                                             
286600     END-IF                                                               
286700     .                                                                    
286800     EJECT                                                                
286900 I-KOLLA-INPUT SECTION.                                                   
287000                                                                          
287100     MOVE JA  TO INDATA-SW                                                
287200     IF REQU-INPUT = ALL '+'                                              
287300       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
287400       MOVE NEJ TO INDATA-SW                                              
287500     ELSE                                                                 
287600       IF REQU-FLBACK NOT = ALL '+'                                       
287700         IF REQU-FLBACK  = JA OR  YES                                     
287800           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLBACK-ATTR                  
287900         ELSE                                                             
288000           MOVE MFS-ALFA-FAELT-FEL TO RESP-FLBACK-ATTR                    
288100           MOVE REQU-FLBACK        TO RESP-FLBACK                         
288200           MOVE NEJ TO INDATA-SW                                          
288300         END-IF                                                           
288400       ELSE                                                               
288500         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLBACK-ATTR                      
288600         MOVE REQU-FLBACK        TO RESP-FLBACK                           
288700         MOVE NEJ TO INDATA-SW                                            
288800       END-IF                                                             
288900                                                                          
289000       IF INDATA-FEL                                                      
289100         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
289200       ELSE                                                               
289300         PERFORM S10-KTRL-INLA                                            
289400         IF INDATA-OK                                                     
289500           PERFORM S12-KTRL-KVAE                                          
289600           IF INDATA-OK                                                   
289700             PERFORM IA-KOLLA-SALDO                                       
289800             PERFORM IB-KOLLA-DC                                          
289900           END-IF                                                         
290000         END-IF                                                           
290100       END-IF                                                             
290200     END-IF                                                               
290300     .                                                                    
290400     EJECT                                                                
290500 IA-KOLLA-SALDO  SECTION.                                                 
290600     PERFORM IMS-GU-ARTC11                                                
290700     IF ARTC-CLAG-KVLS < WS-SUMMA-R32                                     
290800       MOVE QUANT-TOO-BIG TO RESP-IDMSG-ERROR                             
290900     END-IF                                                               
291000     .                                                                    
291100     EJECT                                                                
291200 IB-KOLLA-DC     SECTION.                                                 
291300                                                                          
291400     MOVE INLA-INL-IDLEVNR TO W-IDDC-B6-LEV                               
291500     PERFORM IMS-GU-WDB601-LEV                                            
291600     IF SEGMENT-FINNS                                                     
291700        MOVE LEV-DCS-IDDC TO WS-IDDC-WDB6                                 
291800        MOVE LEV-DCS-IDDISTR-RETUR                                        
291900                          TO WS-IDDISTR-RETUR                             
292000     ELSE                                                                 
292100        MOVE SPACE        TO WS-IDDC-WDB6                                 
292200     END-IF                                                               
292300     .                                                                    
292400     EJECT                                                                
292500 J-UPPDATERA-R32 SECTION.                                                 
292600     PERFORM S04-KONVERTERA-IDFS-IDAVINR                                  
292700     PERFORM JB-UPPD-INLE-INLC-HIST                                       
292800     IF DCS-CDC OR DCS-CDC-TR OR DCS-NDC-CN OR DCS-USA                    
292900       PERFORM S01-UPPD-INLB-LEVPL                                        
293000     END-IF                                                               
293100     PERFORM JE-UPPD-ARTC-ARTS                                            
293200     PERFORM JF-UPPD-ZZAC-LOGG                                            
293300     PERFORM JK-UPPD-INLA                                                 
293400                                                                          
293500     MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                              
293600     .                                                                    
293700     EJECT                                                                
293800 JB-UPPD-INLE-INLC-HIST SECTION.                                          
293900     IF DCS-CDC OR DCS-CDC-TR                                             
294000       PERFORM JBA-UPPD-INLE-HIST                                         
294100     ELSE                                                                 
294200       PERFORM JBB-UPPD-INLC-HIST                                         
294300     END-IF                                                               
294400     .                                                                    
294500     EJECT                                                                
294600 JBA-UPPD-INLE-HIST SECTION.                                              
294700     SKIP2                                                                
294800     MOVE INLA-ART-IDARTNR       TO W-IDARTNR                             
294900     MOVE INLA-ART-IDRADNR-INL   TO W-IDRADNR-INL                         
295000     MOVE INLA-ART-IDLOPNRM      TO W-IDLOPNRM                            
295100     PERFORM IMS-GHU-INLE-MOT                                             
295200** SPARAR URSPRUNGLIGA SEGMENTET                                          
295300     MOVE INLE-MOT-WDL221        TO WS-WDL221                             
295400     MOVE INLE-MOT-KDAVVANT      TO WS-INLE-MOT-KDAVVANT                  
295500     MOVE INLE-MOT-KVANTMOT      TO WS-INLE-MOT-KVANTMOT                  
295600                                    WS-SUMMA-R32                          
295700     MOVE INLE-MOT-KVRETUR       TO WS-INLE-MOT-KVRETUR                   
295800                                                                          
295900*    -- SKAPA IDINLEV                                                     
296000     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
296100     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
296200     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
296300     COMPUTE WS-DAINLEV          = 9999999999999999                       
296400                                 - WS-TIAAAAMMDDTTMMSSTH                  
296500     END-COMPUTE                                                          
296600                                                                          
296700     PERFORM IMS-GU-INLE-ART                                              
296800                                                                          
296900     MOVE WS-DAINLEV             TO INLE-INL-DAINLEV                      
297000     PERFORM IMS-ISRT-INLE-INL                                            
297100     PERFORM UNTIL SEGMENT-FINNS                                          
297200       SUBTRACT +1 FROM WS-DAINLEV                                        
297300       MOVE WS-DAINLEV             TO INLE-INL-DAINLEV                    
297400       PERFORM IMS-ISRT-INLE-INL                                          
297500     END-PERFORM                                                          
297600                                                                          
297700     MOVE WS-WDL221           TO INLE-MOT-WDL221                          
297800     COMPUTE INLE-MOT-KVANTMOT = INLE-MOT-KVANTMOT * -1                   
297900     COMPUTE INLE-MOT-KVRETUR  = INLE-MOT-KVRETUR * -1                    
298000     MOVE WS-TIAAMMDD         TO INLE-MOT-TIUPPDAT                        
298100                                                                          
298200     PERFORM IMS-ISRT-INLE-MOT                                            
298300     .                                                                    
298400     EJECT                                                                
298500 JBB-UPPD-INLC-HIST SECTION.                                              
298600     SKIP2                                                                
298700     MOVE INLA-ART-IDARTNR       TO W-IDARTNR                             
298800     MOVE INLA-ART-IDRADNR-INL   TO W-IDRADNR-INL                         
298900     MOVE INLA-ART-IDLOPNRM      TO W-IDLOPNRM                            
299000     PERFORM IMS-GU-INLC-ART                                              
299100                                                                          
299200     PERFORM JBBA-UPPD-HISTORIK                                           
299300     .                                                                    
299400     EJECT                                                                
299500 JBBA-UPPD-HISTORIK SECTION.                                              
299600     SKIP2                                                                
299700     PERFORM IMS-GHNP-INLC-INL                                            
299800                                                                          
299900     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
300000                   INLC-INL-IDLOPNRM = W-IDLOPNRM                         
300100       PERFORM IMS-GHNP-INLC-INL                                          
300200     END-PERFORM                                                          
300300                                                                          
300400     IF  SEGMENT-FINNS AND                                                
300500         INLC-INL-IDLOPNRM = W-IDLOPNRM                                   
300600                                                                          
300700       MOVE INLC-INL-KVANTMOT      TO WS-INLE-MOT-KVANTMOT                
300800       MOVE INLC-INL-KDAVVANT      TO WS-INLE-MOT-KDAVVANT                
300900       MOVE INLC-INL-KVRETUR       TO WS-INLE-MOT-KVRETUR                 
301000                                                                          
301100*    -- SKAPA IDINLEV                                                     
301200       MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                     
301300       ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                          
301400       ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                          
301500       COMPUTE WS-DAINLEV          = 9999999999999999                     
301600                                   - WS-TIAAAAMMDDTTMMSSTH                
301700       END-COMPUTE                                                        
301800                                                                          
301900       MOVE WS-DAINLEV             TO INLC-INL-DAINLEV                    
302000       MOVE INLC-INL-KVANTMOT      TO WS-SUMMA-R32                        
302100       COMPUTE INLC-INL-KVANTMOT = INLC-INL-KVANTMOT * -1                 
302200       COMPUTE INLC-INL-KVRETUR  = INLC-INL-KVRETUR * -1                  
302300       MOVE WS-TIAAMMDD            TO INLC-INL-TIINLINL                   
302400       PERFORM IMS-ISRT-INLC11                                            
302500       PERFORM UNTIL SEGMENT-FINNS                                        
302600         SUBTRACT +1 FROM WS-DAINLEV                                      
302700         MOVE WS-DAINLEV           TO INLC-INL-DAINLEV                    
302800         PERFORM IMS-ISRT-INLC11                                          
302900       END-PERFORM                                                        
303000     END-IF                                                               
303100     .                                                                    
303200     EJECT                                                                
303300 JE-UPPD-ARTC-ARTS SECTION.                                               
303400                                                                          
303500*    -- ARTC01                                                            
303600     PERFORM IMS-GU-ARTC01                                                
303700     MOVE ARTC-ART-IDFTG       TO WS-ARTC-IDFTG                           
303800     MOVE ARTC-ART-IDLEVNR     TO WS-ARTC-IDLEVNR                         
303900     MOVE ARTC-ART-KDPRODSL    TO WS-ARTC-KDPRODSL                        
304000     MOVE ARTC-ART-KDSORT      TO WS-ARTC-KDSORT                          
304100                                                                          
304200*    -- ARTC11                                                            
304300     PERFORM IMS-GHNP-ARTC11                                              
304400                                                                          
304500     IF DCS-CDC OR DCS-CDC-TR                                             
304600       IF  INLA-ART-KDRT           = 09                                   
304700         SUBTRACT INLA-ART-KVAVIS  FROM ARTC-CLAG-KVOVERF                 
304800       END-IF                                                             
304900     END-IF                                                               
305000                                                                          
305100     MOVE ARTC-CLAG-IDANSK          TO WS-ARTC-IDANSK                     
305200******                                                                    
305300     MOVE ARTC-CLAG-IDINK           TO WS-ARTC-IDINK-X                    
305400       IF ARTC-CLAG-IDINK (1:3) NUMERIC                                   
305500          MOVE ARTC-CLAG-IDINK (1:3) TO WS-ARTC-IDINK                     
305600       ELSE                                                               
305700          IF ARTC-CLAG-IDINK (2:3) NUMERIC                                
305800             MOVE ARTC-CLAG-IDINK (2:3) TO WS-ARTC-IDINK                  
305900          ELSE                                                            
306000             MOVE ZERO TO WS-ARTC-IDINK                                   
306100          END-IF                                                          
306200       END-IF                                                             
306300******                                                                    
306400     MOVE ARTC-CLAG-KDHF            TO WS-ARTC-KDHF                       
306500     MOVE ARTC-CLAG-KDTIPPR         TO WS-ARTC-KDTIPPR                    
306600     MOVE ARTC-CLAG-KDVTH           TO WS-ARTC-KDVTH                      
306700     MOVE ARTC-CLAG-KDPSLLOC        TO WS-ARTC-KDPSLLOC                   
306800     MOVE ARTC-CLAG-PRDIRLON        TO WS-ARTC-PRDIRLON                   
306900     MOVE ARTC-CLAG-PRDMTRL         TO WS-ARTC-PRDMTRL                    
307000     MOVE ARTC-CLAG-PRINK           TO WS-ARTC-PRINK                      
307100     MOVE ARTC-CLAG-PRHEMTAG        TO WS-ARTC-PRHEMTAG                   
307200     MOVE ARTC-CLAG-PROVRPAL        TO WS-ARTC-PROVRPAL                   
307300     MOVE ARTC-CLAG-PRARTSTD        TO WS-ARTC-PRARTSTD                   
307400                                                                          
307500     MOVE ARTC-CLAG-IDARTNR-EMBQ3   TO WS-ARTC-IDARTNR-EMBQ3              
307600     IF DCS-CDC OR DCS-CDC-TR                                             
307700       IF DCS-CDC                                                         
307800         IF WS-SUMMA-R32-SVS > ARTC-CLAG-KVLS-SVS                         
307900           MOVE ZERO                     TO ARTC-CLAG-KVLS-SVS            
308000         ELSE                                                             
308100           SUBTRACT WS-SUMMA-R32-SVS   FROM ARTC-CLAG-KVLS-SVS            
308200         END-IF                                                           
308300         MOVE 1 TO CD-IX                                                  
308400         PERFORM UNTIL CD-IX > 4                                          
308500           IF WS-SUMMA-R32-CD(CD-IX) > ARTC-CLAG-KVLS-CD(CD-IX)           
308600             MOVE ZERO                TO ARTC-CLAG-KVLS-CD(CD-IX)         
308700           ELSE                                                           
308800             SUBTRACT WS-SUMMA-R32-CD(CD-IX) FROM                         
308900                                         ARTC-CLAG-KVLS-CD(CD-IX)         
309000           END-IF                                                         
309100           ADD 1 TO CD-IX                                                 
309200         END-PERFORM                                                      
309300         SUBTRACT WS-SUMMA-R32       FROM ARTC-CLAG-KVLS                  
309400         MOVE ARTC-CLAG-KVLS         TO   LOGG-KVLS                       
309500         PERFORM IMS-REPL-ARTC                                            
309600       END-IF                                                             
309700     ELSE                                                                 
309800       PERFORM IMS-GU-ARTS01                                              
309900       PERFORM IMS-GHNP-ARTS11                                            
310000                                                                          
310100       IF DCS-USA OR DCS-LAND-NON-VCC-OWNED                               
310200         IF DCS-USA                                                       
310300           MOVE 080                   TO AVG-KDCALL                       
310400         ELSE                                                             
310500           IF DCS-INDIA                                                   
310600             MOVE 082                   TO AVG-KDCALL                     
310700           ELSE                                                           
310800             MOVE 081                   TO AVG-KDCALL                     
310900           END-IF                                                         
311000         END-IF                                                           
311100****   KINAS AVERAGE COST SKALL TA HÄNSYN TILL EFR                        
311200         COMPUTE AVG-KVLS-OLD = ARTS-SLAG-KVLS +                          
311300                 ARTS-SLAG-KVEFRS                                         
311400         MOVE +0                    TO AVG-REMARKUP                       
311500         MOVE +0                    TO AVG-PRARTBEL                       
311600                                                                          
311700         MOVE INLA-INL-TIAVIDAT       TO WS-DAAVIDAT-YYMMDD               
311800         MOVE INLA-INL-IDLEVNR        TO W-IDLEVNR                        
311900         PERFORM IMS-GU-WDF101                                            
312000         IF SEGMENT-SAKNAS                                                
312100           MOVE 1    TO W-RETULF                                          
312200         ELSE                                                             
312300           PERFORM IMS-GU-WDB601                                          
312400           MOVE DCS-IDLANDX2 TO W-IDLAND                                  
312500           PERFORM IMS-GNP-WDF102                                         
312600           IF SEGMENT-FINNS                                               
312700             IF F102-TULL-TITULF < WS-DAAVIDAT-YYMMDD                     
312800               MOVE F102-TULL-RETULF-1 TO W-RETULF                        
312900             ELSE                                                         
313000               MOVE F102-TULL-RETULF-2 TO W-RETULF                        
313100             END-IF                                                       
313200           ELSE                                                           
313300             MOVE 1                    TO W-RETULF                        
313400           END-IF                                                         
313500         END-IF                                                           
313600         MOVE W-RETULF                TO AVG-REMARKUP                     
313700                                                                          
313800         MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                 
313900         MOVE 01                      TO W-DATE-AAMM(3:2)                 
314000         MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                      
314100         MOVE DCS-KDVALISO      TO CURR-KDVALISO-ROW                      
314200         MOVE W-DATE-AAMM       TO CURR-TIAAMM                            
314300         MOVE 'A'               TO CURR-KDVALTYP                          
314400         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
314500         IF CURR-KDSVAR = ' '                                             
314600           MOVE CURR-PRKURS-NEW  TO WS-PRKURS                             
314700           MOVE CURR-REVALUTA-TO TO WS-REVALUTA                           
314800         ELSE                                                             
314900           MOVE 1                TO WS-PRKURS                             
315000           MOVE 1                TO WS-REVALUTA                           
315100         END-IF                                                           
315200                                                                          
315300         PERFORM IMS-GU-WDB601                                            
315400         MOVE DCS-IDLANDX2            TO W-IDLANDX2                       
315500         IF SEGMENT-FINNS                                                 
315600           PERFORM IMS-GNP-WDB617                                         
315700           IF SEGMENT-FINNS                                               
315800             COMPUTE WS-PRARTKALKYL          ROUNDED =                    
315900                    (ARTC-CLAG-PRDIRLON *                                 
316000                     PROC-REDIRLON /  WS-PRKURS / WS-REVALUTA ) +         
316100                    (ARTC-CLAG-PRDMTRL *                                  
316200                     PROC-REDMTRL /   WS-PRKURS / WS-REVALUTA )           
316300           ELSE                                                           
316400             MOVE ZERO TO WS-PRARTKALKYL                                  
316500           END-IF                                                         
316600         END-IF                                                           
316700         MOVE WS-PRARTKALKYL          TO AVG-PRARTNTO                     
316800         IF DCS-NDC-CN                                                    
316900         OR DCS-USA                                                       
317000           PERFORM S07-GET-PRARTBEL-BACKNING                              
317100         ELSE                                                             
317200           PERFORM S07-GET-PRARTBEL-BACK-WDK621                           
317300         END-IF                                                           
317400         PERFORM DDAB-GET-CURRENCY-RATE                                   
317500                                                                          
317600         MOVE WS-SUMMA-R32          TO AVG-KVANTMOT                       
317700         MOVE ARTS-SLAG-PRAVCOST    TO AVG-PRAVCOST-OLD                   
317800         MOVE SPAR-PRKURS           TO AVG-PRKURS                         
317900         MOVE +0                    TO AVG-KVLEVART                       
318000         MOVE WS-ARTC-KDVALISO      TO AVG-KDVALISO                       
318100         MOVE WS-ARTC-PRARTBEL-PR   TO AVG-PRARTBEL                       
318200         MOVE WS-ARTC-KDPSLLOC      TO AVG-KDPSLLOC                       
318300         MOVE WS-ARTC-KDPRODSL      TO AVG-KDPRODSL                       
318400         MOVE WS-ARTC-IDFKNGRP      TO AVG-IDFKNGRP                       
318500         MOVE W-IDDC                TO AVG-IDDC                           
318600         MOVE +0                    TO AVG-PRAVCOST-NEW                   
318700         MOVE SPACE                 TO AVG-KDSVAR                         
318800         MOVE WS-TIAAMMDD-AA        TO AVG-TIAA                           
318900         MOVE WS-TIAAMMDD-MM        TO AVG-TIMM                           
319000                                                                          
319100         CALL W510AVG USING AVG-W510AVG 9305-AVG-PCB                      
319200                            AVG-WDB6-PCB                                  
319300         IF AVG-KDSVAR = SPACE                                            
319400           MOVE AVG-PRAVCOST-NEW TO WS-PRAVCOST                           
319500         ELSE                                                             
319600           IF AVG-KDSVAR = '4'                                            
319700             MOVE ARTS-SLAG-PRAVCOST  TO WS-PRAVCOST                      
319800           ELSE                                                           
319900             STRING 'FEL FRÅN W510AVG ' AVG-KDSVAR                        
320000              DELIMITED BY SIZE INTO FELTEXT                              
320100               CALL FELLOG                                                
320200           END-IF                                                         
320300         END-IF                                                           
320400         MOVE     WS-PRAVCOST          TO   ARTS-SLAG-PRAVCOST            
320500       END-IF                                                             
320600                                                                          
320700       SUBTRACT WS-SUMMA-R32         FROM ARTS-SLAG-KVLS                  
320800       PERFORM IMS-REPL-ARTS                                              
320900       PERFORM JEC-FLYTTA-LOGG-WDK7                                       
321000       PERFORM JED-UPPDATERA-LOGG                                         
321100     END-IF                                                               
321200                                                                          
321300*    PERFORM IMS-REPL-ARTC                                                
321400     IF DCS-CDC OR DCS-CDC-TR                                             
321500        PERFORM JEB-FLYTTA-LOGG-WDK6                                      
321600        PERFORM JED-UPPDATERA-LOGG                                        
321700     END-IF                                                               
321800                                                                          
321900     MOVE ZERO                   TO WS-ARTC-PRARTBEL-PR                   
322000     MOVE ZERO                   TO WS-ARTC-PRARTBEL-SUM                  
322100     MOVE ZERO                   TO WS-ARTC-PRARTBES-PR                   
322200                                                                          
322300*    -- ARTC21                                                            
322400                                                                          
322500     IF  INLA-ART-KDRT <= 05                                              
322600       PERFORM S05-UPPD-PRISJUST                                          
322700     END-IF                                                               
322800                                                                          
322900     PERFORM IMS-GNP-ARTC23                                               
323000     IF SEGMENT-FINNS                                                     
323100       MOVE AVT-IDAVTAL     TO WS-ARTC23-IDAVTAL                          
323200     ELSE                                                                 
323300       MOVE ZERO            TO WS-ARTC23-IDAVTAL                          
323400     END-IF                                                               
323500     .                                                                    
323600     EJECT                                                                
323700 JEB-FLYTTA-LOGG-WDK6 SECTION.                                            
323800* LÄGGER UPP SALDOLOGG I WDL9                                             
323900     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
324000     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
324100     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
324200     ACCEPT TRANS-TID FROM TIME                                           
324300     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
324400     MOVE 9                       TO LOGG-IDSEKVNR                        
324500     IF DCS-CDC-TR                                                        
324600       MOVE WC-CDC-SE             TO LOGG-IDDC                            
324700     ELSE                                                                 
324800       MOVE W-IDDC                TO LOGG-IDDC                            
324900     END-IF                                                               
325000     MOVE 'INBO'                  TO LOGG-IDHUVTYP                        
325100     MOVE 'R32'                   TO LOGG-IDSUBTYP                        
325200     MOVE IDPGM                   TO LOGG-IDPGM                           
325300     MOVE '6119'                  TO LOGG-IDTRANS                         
325400     MOVE MSGI-IDUSER             TO LOGG-IDUSER                          
325500     MOVE SPACE                   TO LOGG-REF                             
325600     MOVE W-IDLOPNRM              TO LOGG-IDLOPNRM                        
325700     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
325800     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
325900     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
326000     MOVE '-'                     TO LOGG-IDTECKEN-KVLS                   
326100     MOVE WS-SUMMA-R32            TO LOGG-KVART-SALDO                     
326200     MOVE ARTC-CLAG-KVLS          TO LOGG-KVLS                            
326300     MOVE ARTC-CLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                       
326400     MOVE ARTC-CLAG-KVEFRS        TO LOGG-KVEFRS                          
326500     COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC +                           
326600                          ARTC-CLAG-KVAKS-T                               
326700     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
326800                                                                          
326900     .                                                                    
327000     EJECT                                                                
327100 JEC-FLYTTA-LOGG-WDK7 SECTION.                                            
327200* LÄGGER UPP SALDOLOGG I WDL9                                             
327300     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
327400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
327500     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
327600     ACCEPT TRANS-TID FROM TIME                                           
327700     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
327800     MOVE 9                     TO LOGG-IDSEKVNR                          
327900     IF DCS-CDC-TR                                                        
328000       MOVE WC-CDC-SE           TO LOGG-IDDC                              
328100     ELSE                                                                 
328200       MOVE W-IDDC              TO LOGG-IDDC                              
328300     END-IF                                                               
328400     MOVE 'INBO'                TO LOGG-IDHUVTYP                          
328500     MOVE 'R32'                 TO LOGG-IDSUBTYP                          
328600     MOVE IDPGM                 TO LOGG-IDPGM                             
328700     MOVE '6119'                TO LOGG-IDTRANS                           
328800     MOVE MSGI-IDUSER           TO LOGG-IDUSER                            
328900     MOVE SPACE                 TO LOGG-REF                               
329000     MOVE W-IDLOPNRM            TO LOGG-IDLOPNRM                          
329100     MOVE SPACE                 TO LOGG-IDTECKEN-KVAKS-PAV                
329200     MOVE SPACE                 TO LOGG-IDTECKEN-KVEFRS                   
329300     MOVE SPACE                 TO LOGG-IDTECKEN-KVAKS                    
329400     MOVE '-'                   TO LOGG-IDTECKEN-KVLS                     
329500     MOVE WS-SUMMA-R32          TO LOGG-KVART-SALDO                       
329600     MOVE ARTS-SLAG-KVAKS-SDC   TO LOGG-KVAKS                             
329700     MOVE ARTS-SLAG-KVLS        TO LOGG-KVLS                              
329800     MOVE ARTS-SLAG-KVAKS-PAV   TO LOGG-KVAKS-PAV                         
329900     MOVE ARTS-SLAG-KVEFRS      TO LOGG-KVEFRS                            
330000     MOVE ZERO                  TO LOGG-DAREGDAT-LADD                     
330100     .                                                                    
330200     EJECT                                                                
330300 JED-UPPDATERA-LOGG SECTION.                                              
330400     PERFORM IMS-ISRT-WDL901                                              
330500     IF SEGMENT-FINNS-REDAN                                               
330600        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
330700          ADD -1 TO LOGG-IDSEKVNR                                         
330800          PERFORM IMS-ISRT-WDL901                                         
330900        END-PERFORM                                                       
331000     END-IF                                                               
331100     .                                                                    
331200     EJECT                                                                
331300 JF-UPPD-ZZAC-LOGG SECTION.                                               
331400     IF DCS-CDC OR DCS-CDC-TR                                             
331500       PERFORM JFA-LOGG-R32                                               
331600       PERFORM JFB-LOGG-320                                               
331700     END-IF                                                               
331800                                                                          
331900** BACKA R32-TRANSAR TILL EKONOMI (LOGIK FRÅN PGM W60193)                 
332000     IF WS-INLE-MOT-KVANTMOT NOT = INLA-ART-KVAVIS                        
332100**UMESH                                                                   
332200      IF WS-KVAE-FLKRLIM = NEJ AND                                        
332300         WS-KVAE-IDKRFEL(1:1) = 'P' AND                                   
332400         DCS-CDC                                                          
332500        PERFORM JQ-LOGG-EKO-WDR9                                          
332600      ELSE                                                                
332700**UMESH                                                                   
332800                                                                          
332900       IF INLA-ART-KDRT = 0 OR 9                                          
333000         IF WS-INLE-MOT-KDAVVANT = +3                                     
333100           PERFORM JG-LOGG-EKO-WDR9                                       
333200         ELSE                                                             
333300           IF (SPAR-RAD-DAUPPDAT < INSTALLATIONSDATUM AND                 
333400               SPAR-KR-DAKRANS < INSTALLATIONSDATUM)                      
333500               MOVE JA TO FL-FORE-INST                                    
333600           END-IF                                                         
333700           IF FL-FORE-INST = JA                                           
333800* AVSLUTAD INNAN INSTALLATION - RAPPORTERAS ENLIGT GAMLA                  
333900* REGELVERKET                                                             
334000* FÖRE INSTALLATIONS-LOGIK KAN RENSAS RUNT ÅRSSKIFTET 2001/2002           
334100             COMPUTE EKO-KVAVIS = WS-INLE-MOT-KVANTMOT -                  
334200                     INLA-ART-KVAVIS + WS-INLE-MOT-KVRETUR                
334300             END-COMPUTE                                                  
334400             MOVE EKO-KVAVIS TO WS-SPAR-KVAVIS                            
334500             IF EKO-KVAVIS NOT = ZERO                                     
334600               PERFORM JH-LOGG-EKO-WDR8                                   
334700               PERFORM S13-KOLLA-PALAGG                                   
334800             END-IF                                                       
334900           ELSE                                                           
335000             COMPUTE EKO-KVAVIS = WS-INLE-MOT-KVANTMOT -                  
335100                     INLA-ART-KVAVIS                                      
335200             END-COMPUTE                                                  
335300             MOVE EKO-KVAVIS TO WS-SPAR-KVAVIS                            
335400             IF (EKO-KVAVIS > ZERO) OR (EKO-KVAVIS < ZERO AND             
335500                                  NOT DCS-CDC)                            
335600                 IF EKO-KVAVIS < ZERO                                     
335700**** SHOULD NOT BE DONE FOR MARKETS WITH LOCAL SOURCING                   
335800**** IT IS DONE IN CLAIM STEP                                             
335900                   IF DCS-CHINA OR DCS-USA                                
336000                     CONTINUE                                             
336100                   ELSE                                                   
336200**** SHOULD BE DONE FOR MARKETS THAT IS NOT OWN BY VCC                    
336300                     IF DCS-LAND-NON-VCC-OWNED                            
336400                       PERFORM JJ-LOGG-R32-ULEV-WDR8                      
336500                     END-IF                                               
336600                   END-IF                                                 
336700                 ELSE                                                     
336800                   PERFORM JH-LOGG-EKO-WDR8                               
336900                   PERFORM S13-KOLLA-PALAGG                               
337000                 END-IF                                                   
337100             ELSE                                                         
337200               IF EKO-KVAVIS < ZERO                                       
337300                 PERFORM JJ-LOGG-R32-ULEV-WDR9                            
337400               END-IF                                                     
337500             END-IF                                                       
337600           END-IF                                                         
337700         END-IF                                                           
337800       ELSE                                                               
337900         COMPUTE WS-KVDIFF-MOT-AVIS  =                                    
338000             WS-INLE-MOT-KVANTMOT   -  INLA-ART-KVAVIS                    
338100                                    +  WS-KVINLART-TRP                    
338200         IF  WS-KVDIFF-MOT-AVIS      NOT = ZERO                           
338300           IF INLA-INL-IDDC NOT = DCS-IDDC                                
338400              MOVE INLA-INL-IDDC     TO W-IDDC-B6                         
338500              PERFORM IMS-GU-WDB601                                       
338600           END-IF                                                         
338700           IF DCS-CDC AND INLA-ART-KDRT = 8                               
338800             PERFORM S08-SKAPA-EK-TRANS                                   
338900           END-IF                                                         
339000         END-IF                                                           
339100         IF INLA-ART-KVAVIS NOT = ZERO                                    
339200           IF INLA-ART-KDRT = 6                                           
339300             IF WS-KVDIFF-MOT-AVIS NOT = ZERO                             
339400               PERFORM JP-LOGG-EKO-WDR9-RT6-AVV                           
339500             END-IF                                                       
339600           ELSE                                                           
339700             PERFORM JG-LOGG-EKO-WDR9                                     
339800           END-IF                                                         
339900         END-IF                                                           
340000         IF INLA-ART-KDRT NOT = 6                                         
340100           PERFORM S13-KOLLA-PALAGG                                       
340200         END-IF                                                           
340300       END-IF                                                             
340400      END-IF                                                              
340500     END-IF                                                               
340600                                                                          
340700** BACKA R31-TRANSAR TILL EKONOMI (LOGIK FRÅN PGM W60192)                 
340800     IF INLA-ART-KDRT = 0 OR 9 OR 10                                      
340900       PERFORM JL-LOGG-EKO-R31-WDR8                                       
341000     ELSE                                                                 
341100       IF INLA-ART-KDRT = 6                                               
341200         PERFORM JN-LOGG-EKO-R31-WDR9-RT6                                 
341300       ELSE                                                               
341400         PERFORM JM-LOGG-EKO-R31-WDR9                                     
341500       END-IF                                                             
341600     END-IF                                                               
341700     IF WS-ARTC-PRINK = WS-ARTC-PRARTSTD                                  
341800       CONTINUE                                                           
341900     ELSE                                                                 
342000       IF INLA-ART-KVAVIS > 0 AND INLA-ART-KDRT NOT = 6                   
342100         PERFORM JO-LOGG-EKO-R31-WDR9-PALAGG                              
342200       END-IF                                                             
342300     END-IF                                                               
342400     .                                                                    
342500     EJECT                                                                
342600 JFA-LOGG-R32 SECTION.                                                    
342700                                                                          
342800     MOVE SPACE                  TO W611R32-W611R32                       
342900                                                                          
343000     MOVE 'R32'                  TO W611R32-IDPTYP                        
343100     MOVE +5                     TO W611R32-KDSORT2                       
343200     MOVE INLA-ART-IDDC (2:1)    TO W611R32-KDCLAGER                      
343300     MOVE INLA-ART-IDARTNR       TO W611R32-IDARTNR                       
343400     MOVE INLA-ART-IDLOPNRM      TO W611R32-IDLOPNRM                      
343500     MOVE 2                      TO W611R32-KDAVVANT                      
343600     MOVE ZERO                   TO W611R32-KVANTMOT                      
343700     MOVE ZERO                   TO W611R32-KVFORDEL                      
343800     MOVE ZERO                   TO W611R32-IDKOLLI                       
343900     MOVE ZERO                   TO W611R32-KDAVVKV                       
344000     MOVE ZERO                   TO W611R32-KVRETUR                       
344100                                                                          
344200     MOVE W611R32-W611R32        TO WS-ZZAC01-LOGGPOST                    
344300     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
344400     PERFORM S02-SKAPA-ZZAC01                                             
344500     .                                                                    
344600     EJECT                                                                
344700 JFB-LOGG-320 SECTION.                                                    
344800                                                                          
344900     MOVE ZERO                   TO R320-W211310                          
345000                                                                          
345100     MOVE '221'                  TO R320-IDTTYP                           
345200     MOVE INLA-ART-IDARTNR       TO R320-IDARTNR-S                        
345300     MOVE INLA-ART-IDDC (2:1)    TO R320-KDCLAGER-S                       
345400     MOVE ZERO                   TO R320-SORTFLT1                         
345500     MOVE WS-ARTC-IDANSK         TO R320-IDANSKNR                         
345600     MOVE INLA-ART-PRARTSTD      TO R320-PRARTSTD                         
345700     MOVE INLA-ART-IDLOPNRM      TO R320-IDLOPNR                          
345800     MOVE 2                      TO R320-KDAVVANT                         
345900     MOVE ZERO                   TO R320-POSTLGD                          
346000     MOVE ZERO                   TO R320-IDPTYP                           
346100     MOVE ZERO                   TO R320-NOLLOR-20                        
346200     MOVE INLA-ART-IDARTNR       TO R320-IDARTNR                          
346300     MOVE INLA-ART-IDDC (2:1)    TO R320-KDCLAGER                         
346400     MOVE ZERO                   TO R320-KVMOTANT                         
346500     MOVE ZERO                   TO R320-NOLLOR-41                        
346600     MOVE INLA-INL-IDLEVNR       TO R320-IDLEVNR-INL                      
346700     MOVE INLA-INL-TIAVIDAT      TO R320-TIAVSDAT                         
346800     MOVE ZERO                   TO R320-NOLLOR-21                        
346900     MOVE INLA-ART-KDRT          TO R320-KDRT                             
347000     MOVE INLA-ART-KVAVIS        TO R320-KVAVIS                           
347100     MOVE WS-IDAVINR             TO R320-IDAVINR                          
347200     MOVE WS-ARTC-IDINK          TO R320-KDPKINR                          
347300                                                                          
347400     MOVE R320-W211310           TO WS-ZZAC01-LOGGPOST                    
347500     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
347600     PERFORM S02-SKAPA-ZZAC01                                             
347700     .                                                                    
347800     EJECT                                                                
347900 JG-LOGG-EKO-WDR9 SECTION.                                                
348000                                                                          
348100     IF INLA-ART-KDRT = 3                                                 
348200** UTTAG SATS                                                             
348300       MOVE 'W6011910'                  TO FIL-IDPGM                      
348400       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
348500                                           EKH-DAVERDAT                   
348600       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
348700       MOVE +1                          TO FIL-IDSEKVNR                   
348800       MOVE '102'                       TO EKH-KDEKHHT                    
348900       MOVE '103'                       TO EKH-KDEKSHT                    
349000       MOVE 'DET  '                     TO EKH-KDEKNIVA                   
349100       MOVE DCS-IDDC                    TO EKH-IDDC-SEND                  
349200       MOVE SPACE                       TO EKH-IDDC-REC                   
349300       MOVE +0                          TO EKH-IDDISTR                    
349400                                           EKH-IDKUNDNR                   
349500*******************************                                           
349600*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
349700       MOVE ZERO TO NOLL-RAKNARE                                          
349800       MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                
349900       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
350000       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
350100            FOR LEADING ZERO                                              
350200       ADD +1 TO NOLL-RAKNARE                                             
350300       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                     
350400          WITH POINTER NOLL-RAKNARE                                       
350500*******************************                                           
350600       MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                   
350700       MOVE ZERO                        TO EKH-KDPSLLOC                   
350800                                           EKH-PRARTNTO                   
350900                                           EKH-PRARTSJK                   
351000                                           EKH-PRHEMTAG                   
351100                                           EKH-PRLANDCO                   
351200                                           EKH-PRDIRLON                   
351300                                           EKH-PRDMTRL                    
351400                                           EKH-PROVRPAL                   
351500                                           EKH-SUBEL                      
351600       MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                    
351700       MOVE SPACE                       TO EKH-FLLSBOK                    
351800                                                                          
351900       MOVE 'SEK'                       TO EKH-KDVALISO                   
352000       MOVE 1.00                        TO EKH-PRKURS                     
352100                                                                          
352200       MOVE WS-ARTC-PRINK               TO EKH-PRINK                      
352300       MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                   
352400                                                                          
352500       COMPUTE EKH-KVANTAL = INLA-ART-KVAVIS * -1                         
352600       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
352700       MOVE W-IDTRANS                   TO EKH-IDTRANS                    
352800       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
352900       MOVE ZERO                        TO EKH-IDKONTO                    
353000                                           EKH-KDFRAKT                    
353100                                           EKH-SUVAT                      
353200       MOVE SPACE                       TO EKH-BEVAT                      
353300                                           EKH-KDANMORS                   
353400                                           EKH-IDKST                      
353500                                           EKH-IDANALYS                   
353600                                                                          
353700       MOVE INLA-INL-TIAVIDAT           TO WS-DAAVIDAT-YYMMDD             
353800       IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                    
353900         MOVE 19                        TO WS-DAAVIDAT-SEKEL              
354000       ELSE                                                               
354100         MOVE 20                        TO WS-DAAVIDAT-SEKEL              
354200       END-IF                                                             
354300       MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                   
354400       MOVE WS-IDAVINR                  TO EKH-IDAVINR                    
354500       MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                    
354600       IF INLA-ART-KVAVIS > ZERO                                          
354700         MOVE 1                         TO EKH-KDAVVTYP                   
354800       ELSE                                                               
354900         MOVE 0                         TO EKH-KDAVVTYP                   
355000       END-IF                                                             
355100       MOVE INLA-ART-KDRT               TO EKH-KDRT                       
355200       COMPUTE EKH-KVANTMOT = WS-INLE-MOT-KVANTMOT * -1                   
355300       MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                     
355400       MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                     
355500       MOVE SPACE                       TO EKH-KDTRADP                    
355600       MOVE SPACE                       TO EKH-FLDCET                     
355700       MOVE SPACE                       TO EKH-IDKUNDRF                   
355800       MOVE SPACE                       TO EKH-IDFAKT-EXP                 
355900                                                                          
356000       PERFORM IMS-ISRT-WLSAPA01                                          
356100       PERFORM UNTIL SEGMENT-FINNS                                        
356200         ADD +1 TO FIL-IDSEKVNR                                           
356300         PERFORM IMS-ISRT-WLSAPA01                                        
356400       END-PERFORM                                                        
356500     ELSE                                                                 
356600       IF WS-KVINLART-TRP > ZERO                                          
356700          PERFORM S09-SKAPA-TRANSPORT-TRANS                               
356800       END-IF                                                             
356900     END-IF                                                               
357000     .                                                                    
357100     EJECT                                                                
357200                                                                          
357300 JH-LOGG-EKO-WDR8 SECTION.                                                
357400     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
357500       PERFORM JHA-LOGG-EKO-VCCN-DET                                      
357600       IF DCS-NDC-CN OR DCS-USA                                           
357700         PERFORM JHA-LOGG-EKO-VCCN-HEMT                                   
357800       END-IF                                                             
357900       IF  WS-ARTC-PRINK NOT = WS-ARTC-PRARTSTD                           
358000       AND (INLA-ART-KDRT NOT = 3 AND 7 AND 8 AND 77)                     
358100         PERFORM IMS-GU-WDB601                                            
358200         MOVE DCS-IDLANDX2            TO W-IDLANDX2                       
358300         IF SEGMENT-FINNS                                                 
358400           PERFORM IMS-GNP-WDB617                                         
358500           IF SEGMENT-FINNS                                               
358600             IF DCS-USA                                                   
358700               CONTINUE                                                   
358800             ELSE                                                         
358900               PERFORM JHA-LOGG-EKO-VCCN-KALK                             
359000             END-IF                                                       
359100           END-IF                                                         
359200         END-IF                                                           
359300       END-IF                                                             
359400       IF DCS-USA                                                         
359500         CONTINUE                                                         
359600       ELSE                                                               
359700         PERFORM JHA-LOGG-EKO-VCCN-SUM                                    
359800       END-IF                                                             
359900     ELSE                                                                 
360000       PERFORM JHB-LOGG-EKO-VCCS                                          
360100     END-IF                                                               
360200     .                                                                    
360300     EJECT                                                                
360400                                                                          
360500 JHA-LOGG-EKO-VCCN-DET SECTION.                                           
360600     IF DCS-NDC-CN                                                        
360700     OR DCS-USA                                                           
360800       MOVE SPACE                    TO EKO-W51080                        
360900       MOVE SPACE                    TO WS-SAP-MM-POST                    
361000                                                                          
361100       MOVE INLA-ART-IDARTNR         TO EKO-IDARTNR                       
361200       MOVE INLA-INL-IDDC            TO EKO-IDDC                          
361300       MOVE INLA-INL-IDFS            TO EKO-IDFS                          
361400       MOVE WS-ARTC-IDINK-X          TO EKO-IDINK                         
361500       MOVE INLA-INL-IDKONTO         TO EKO-IDKONTO                       
361600       MOVE INLA-INL-IDLEVNR         TO EKO-IDLEVNR                       
361700       MOVE INLA-ART-IDLOPNRM        TO EKO-IDLOPNRM                      
361800       MOVE WS-ARTC-KDPRODSL         TO EKO-KDPRODSL                      
361900       MOVE INLA-ART-KDRT            TO EKO-KDRT                          
362000       MOVE WS-ARTC-KDSORT           TO EKO-KDSORT                        
362100       MOVE WS-ARTC-KDTIPPR          TO EKO-KDTIPPR                       
362200                                                                          
362300       IF DCS-NDC-CN                                                      
362400       OR DCS-USA                                                         
362500         PERFORM S07-GET-PRARTBEL-BACKNING                                
362600         IF DCS-NDC-CN                                                    
362700           MOVE 60                   TO EKO-IDFTG                         
362800           MOVE WS-KDVALISO-HUV-CN   TO CURR-KDVALISO-HUV                 
362900         ELSE                                                             
363000           MOVE 53                   TO EKO-IDFTG                         
363100           MOVE WS-KDVALISO-HUV-US   TO CURR-KDVALISO-HUV                 
363200         END-IF                                                           
363300         PERFORM S06-GET-PRARTBEL                                         
363400         MOVE INLA-INL-TIAVIDAT      TO WS-DAAVIDAT-YYMMDD                
363500         MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                 
363600         MOVE WS-DAAVIDAT-YYMMDD(3:2) TO W-DATE-AAMM(3:2)                 
363700         MOVE WS-ARTC-KDVALISO       TO CURR-KDVALISO-ROW                 
363800         MOVE W-DATE-AAMM            TO CURR-TIAAMM                       
363900         MOVE 'M'                    TO CURR-KDVALTYP                     
364000         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
364100         IF CURR-KDSVAR = ' '                                             
364200           MOVE CURR-PRKURS-NEW      TO WS-PRKURS                         
364300           MOVE CURR-REVALUTA-TO     TO WS-REVALUTA                       
364400         ELSE                                                             
364500           MOVE 1                    TO WS-PRKURS                         
364600           MOVE 1                    TO WS-REVALUTA                       
364700         END-IF                                                           
364800         MOVE WS-ARTC-KDVALISO       TO EKO-KDVALISO                      
364900****   AGREE PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED              
365000****   TO COUNTRY CURRENCY                                                
365100         COMPUTE WS-ARTC-PRARTBES-PR = WS-ARTC-PRARTBEL-PR                
365200                                       * WS-PRKURS / WS-REVALUTA          
365300       END-IF                                                             
365400                                                                          
365500       IF WS-ARTC-PRARTBEL-PR  > ZERO                                     
365600         MOVE WS-ARTC-PRARTBEL-PR    TO EKO-PRARTBEL-PR                   
365700         MOVE WS-ARTC-KDVALISO       TO EKO-KDVALISO                      
365800       ELSE                                                               
365900         MOVE 0.1                    TO EKO-PRARTBEL-PR                   
366000         MOVE 'XXX'                  TO EKO-KDVALISO                      
366100       END-IF                                                             
366200       IF INLA-INL-IDLEVNR = '1441'                                       
366300         MOVE NEJ                    TO WS-SAP-MM-POST                    
366400       END-IF                                                             
366500                                                                          
366600       IF FL-FORE-INST = JA                                               
366700         COMPUTE EKO-KVAVIS = (WS-INLE-MOT-KVANTMOT -                     
366800                       INLA-ART-KVAVIS + WS-INLE-MOT-KVRETUR) * -1        
366900         END-COMPUTE                                                      
367000       ELSE                                                               
367100         COMPUTE EKO-KVAVIS = (WS-INLE-MOT-KVANTMOT -                     
367200                           INLA-ART-KVAVIS) * -1                          
367300         END-COMPUTE                                                      
367400       END-IF                                                             
367500       MOVE EKO-KVAVIS TO WS-SPAR-KVAVIS                                  
367600                                                                          
367700       IF WS-ARTC-PRARTBES-PR  > ZERO                                     
367800         MOVE WS-ARTC-PRARTBES-PR    TO EKO-PRARTBES                      
367900       ELSE                                                               
368000         MOVE 0.1                    TO EKO-PRARTBES                      
368100       END-IF                                                             
368200                                                                          
368300       MOVE ZERO                     TO EKO-PRINK                         
368400       MOVE ZERO                     TO EKO-PRHEMTAG                      
368500       MOVE ZERO                     TO EKO-RETULF                        
368600       MOVE INLA-INL-TIAVIDAT        TO EKO-TIAVIDAT                      
368700       MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                    
368800       MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                     
368900       MOVE JA                       TO EKO-FLLSBOK                       
369000       MOVE ZERO                     TO EKO-IDDISTR                       
369100       MOVE NEJ                      TO EKO-FLDIRLEV                      
369200       MOVE WS-ARTC23-IDAVTAL        TO EKO-IDAVTAL                       
369300                                                                          
369400       MOVE JA                       TO EKO-FLAVVINL                      
369500       MOVE 'V'                      TO EKO-KDINLAVV                      
369600       IF DCS-USA OR DCS-NDC-CN                                           
369700         IF DCS-NDC-CN                                                    
369800           MOVE 60                   TO EKO-IDFTG                         
369900         ELSE                                                             
370000           MOVE 53                   TO EKO-IDFTG                         
370100         END-IF                                                           
370200       END-IF                                                             
370300                                                                          
370400       MOVE 'W6011910'               TO EKO-FIL-IDPGM                     
370500       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
370600       ACCEPT EKO-FIL-TIKLOCK FROM TIME                                   
370700       MOVE 1                        TO EKO-FIL-IDSEKVNR                  
370800       MOVE 'W510'                   TO EKO-FIL-CT-IDSYSTEM               
370900       MOVE '80 '                    TO EKO-FIL-CT-IDPTYP                 
371000       MOVE ' '                      TO EKO-FIL-CT-IDVTYP                 
371100                                                                          
371200       IF WS-SAP-MM-POST = NEJ                                            
371300         CONTINUE                                                         
371400       ELSE                                                               
371500         PERFORM IMS-ISRT-EKOTRANS                                        
371600                                                                          
371700         PERFORM UNTIL SEGMENT-FINNS                                      
371800           ADD +1 TO EKO-FIL-IDSEKVNR                                     
371900           PERFORM IMS-ISRT-EKOTRANS                                      
372000         END-PERFORM                                                      
372100       END-IF                                                             
372200     END-IF                                                               
372300                                                                          
372400     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
372500     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
372600     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
372700     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
372800     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
372900     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
373000     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
373100     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
373200     MOVE INLA-INL-IDDC               TO EKO-EKH-IDDC-SEND                
373300     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
373400     MOVE +0                          TO EKO-EKH-IDDISTR                  
373500                                         EKO-EKH-IDKUNDNR                 
373600*******************************                                           
373700*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
373800     MOVE ZERO TO NOLL-RAKNARE                                            
373900     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
374000     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
374100     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
374200          FOR LEADING ZERO                                                
374300     ADD +1 TO NOLL-RAKNARE                                               
374400     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
374500          WITH POINTER NOLL-RAKNARE                                       
374600*******************************                                           
374700     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
374800     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
374900                                         EKO-EKH-PRARTNTO                 
375000                                         EKO-EKH-PRARTSJK                 
375100                                         EKO-EKH-PRLANDCO                 
375200                                         EKO-EKH-SUBEL                    
375300     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
375400     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
375500                                                                          
375600     MOVE 1.00                        TO EKO-EKH-PRKURS                   
375700                                                                          
375800     MOVE ZERO                        TO EKO-EKH-PRINK                    
375900                                                                          
376000     IF DCS-NDC-CN                                                        
376100     OR DCS-USA                                                           
376200       PERFORM S07-GET-PRARTBEL-BACKNING                                  
376300     ELSE                                                                 
376400       PERFORM S07-GET-PRARTBEL-BACK-WDK621                               
376500     END-IF                                                               
376600     PERFORM DDAB-GET-CURRENCY-RATE                                       
376700*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
376800*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
376900     IF XDC-NON-VCC-OWNED OR DCS-LAND-NON-VCC-OWNED                       
377000     AND NOT (NDC-CN OR NDC-US)                                           
377100       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
377200     END-IF                                                               
377300***                                                                       
377400     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
377500       MOVE WS-ARTC-PRARTBEL-PR       TO EKO-EKH-PRARTSTD                 
377600       MOVE WS-ARTC-KDVALISO          TO EKO-EKH-KDVALISO                 
377700       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
377800     ELSE                                                                 
377900       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
378000       MOVE DCS-KDVALISO              TO EKO-EKH-KDVALISO                 
378100**** TAIWAN NEEDS A INTEGER VALUE AS TAIWAN DOSENT USE                    
378200**** DECIMALS. 0.1 MAKES THE BOOKING TO ZERO WHICH IS NOT                 
378300**** VALID IN SAP SYSTEM.                                                 
378400       IF DCS-TAIWAN                                                      
378500         MOVE 1.0                     TO EKO-EKH-PRARTSTD                 
378600       END-IF                                                             
378700     END-IF                                                               
378800     IF FL-FORE-INST = JA                                                 
378900       COMPUTE EKO-EKH-KVAVIS  = (WS-INLE-MOT-KVANTMOT -                  
379000                      INLA-ART-KVAVIS + WS-INLE-MOT-KVRETUR) * -1         
379100       END-COMPUTE                                                        
379200     ELSE                                                                 
379300       COMPUTE EKO-EKH-KVAVIS  = (WS-INLE-MOT-KVANTMOT -                  
379400                         INLA-ART-KVAVIS) * -1                            
379500       END-COMPUTE                                                        
379600     END-IF                                                               
379700     MOVE EKO-EKH-KVAVIS              TO WS-SPAR-KVAVIS                   
379800     MOVE WS-SPAR-KVAVIS              TO EKO-EKH-KVANTAL                  
379900*    COMPUTE EKO-EKH-KVANTAL = INLA-ART-KVAVIS * -1                       
380000     MOVE EKO-EKH-PRARTSTD            TO WS-SUARTSTD                      
380100     COMPUTE WS-SUARTSTD = WS-ARTC-PRARTBEL-SUM *                         
380200                           EKO-EKH-KVANTAL                                
380300                                                                          
380400     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
380500     MOVE SPACE                       TO EKO-EKH-BEVAT                    
380600                                         EKO-EKH-KDANMORS                 
380700                                         EKO-EKH-IDKST                    
380800     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
380900                                         EKO-EKH-KDFRAKT                  
381000                                         EKO-EKH-SUVAT                    
381100                                         EKO-EKH-PRHEMTAG                 
381200                                                                          
381300     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
381400     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
381500     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
381600     IF EKO-EKH-KVANTAL > ZERO                                            
381700       MOVE 1                        TO EKO-EKH-KDAVVTYP                  
381800     ELSE                                                                 
381900       MOVE 0                        TO EKO-EKH-KDAVVTYP                  
382000     END-IF                                                               
382100     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
382200     MOVE WS-INLE-MOT-KVANTMOT       TO EKO-EKH-KVANTMOT                  
382300     MOVE INLA-ART-KVAVIS            TO EKO-EKH-KVAVIS                    
382400     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
382500     MOVE SPACE                      TO EKO-EKH-FLDCET                    
382600     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
382700                                                                          
382800     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
382900                                        EKO-EKH-PRDMTRL                   
383000                                        EKO-EKH-PROVRPAL                  
383100     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
383200     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
383300     EVALUATE TRUE                                                        
383400       WHEN NDC-CN                                                        
383500         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
383600       WHEN NDC-IN                                                        
383700         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
383800       WHEN NDC-US                                                        
383900         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
384000       WHEN OTHER                                                         
384100         IF NDC-KR                                                        
384110         OR NDC-MX                                                        
384120         OR NDC-BR                                                        
384130         OR NDC-ZA                                                        
384200           MOVE '103'                TO EKO-EKH-KDEKHHT                   
384300           MOVE '106'                TO EKO-EKH-KDEKSHT                   
384400         END-IF                                                           
384500         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
384600     END-EVALUATE                                                         
384700     MOVE 'BAK'                      TO EKO-EKH-CMD                       
384800     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
384900                                                                          
385000     PERFORM IMS-ISRT-EKOTRANS                                            
385100                                                                          
385200     PERFORM UNTIL SEGMENT-FINNS                                          
385300       ADD +1                     TO EKO-FIL-IDSEKVNR                     
385400       PERFORM IMS-ISRT-EKOTRANS                                          
385500     END-PERFORM                                                          
385600     .                                                                    
385700     EJECT                                                                
385800                                                                          
385900 JHA-LOGG-EKO-VCCN-HEMT SECTION.                                          
386000     IF FL-FORE-INST = JA                                                 
386100       COMPUTE EKO-EKH-KVAVIS = (WS-INLE-MOT-KVANTMOT -                   
386200                      INLA-ART-KVAVIS + WS-INLE-MOT-KVRETUR) * -1         
386300       END-COMPUTE                                                        
386400     ELSE                                                                 
386500       COMPUTE EKO-EKH-KVAVIS = (WS-INLE-MOT-KVANTMOT -                   
386600                         INLA-ART-KVAVIS) * -1                            
386700       END-COMPUTE                                                        
386800     END-IF                                                               
386900     MOVE EKO-EKH-KVAVIS              TO WS-SPAR-KVAVIS                   
387000     MOVE WS-SPAR-KVAVIS              TO EKO-EKH-KVANTAL                  
387100     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
387200     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
387300     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
387400     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
387500     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
387600     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
387700     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
387800     MOVE 'HEMT '                     TO EKO-EKH-KDEKNIVA                 
387900     MOVE INLA-INL-IDDC               TO EKO-EKH-IDDC-SEND                
388000     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
388100     MOVE +0                          TO EKO-EKH-IDDISTR                  
388200                                         EKO-EKH-IDKUNDNR                 
388300*******************************                                           
388400*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
388500     MOVE ZERO TO NOLL-RAKNARE                                            
388600     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
388700     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
388800     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
388900          FOR LEADING ZERO                                                
389000     ADD +1 TO NOLL-RAKNARE                                               
389100     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
389200          WITH POINTER NOLL-RAKNARE                                       
389300*******************************                                           
389400     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
389500     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
389600                                         EKO-EKH-PRARTNTO                 
389700                                         EKO-EKH-PRARTSJK                 
389800                                         EKO-EKH-PRLANDCO                 
389900                                         EKO-EKH-SUBEL                    
390000     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
390100     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
390200                                                                          
390300     MOVE 1.00                        TO EKO-EKH-PRKURS                   
390400                                                                          
390500     MOVE ZERO                        TO EKO-EKH-PRINK                    
390600                                                                          
390700     IF DCS-NDC-CN                                                        
390800     OR DCS-USA                                                           
390900       PERFORM S07-GET-PRARTBEL-BACKNING                                  
391000     ELSE                                                                 
391100       PERFORM S07-GET-PRARTBEL-BACK-WDK621                               
391200     END-IF                                                               
391300     PERFORM DDAB-GET-CURRENCY-RATE                                       
391400*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
391500*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
391600     IF XDC-NON-VCC-OWNED OR DCS-LAND-NON-VCC-OWNED                       
391700     AND NOT (NDC-CN OR NDC-US)                                           
391800       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
391900     END-IF                                                               
392000***                                                                       
392100     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
392200       MOVE WS-ARTC-PRARTBEL-PR       TO EKO-EKH-PRARTSTD                 
392300       MOVE WS-ARTC-KDVALISO          TO EKO-EKH-KDVALISO                 
392400       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
392500     ELSE                                                                 
392600       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
392700       MOVE DCS-KDVALISO              TO EKO-EKH-KDVALISO                 
392800**** TAIWAN NEEDS A INTEGER VALUE AS TAIWAN DOSENT USE                    
392900**** DECIMALS. 0.1 MAKES THE BOOKING TO ZERO WHICH IS NOT                 
393000**** VALID IN SAP SYSTEM.                                                 
393100       IF DCS-TAIWAN                                                      
393200         MOVE 1.0                     TO EKO-EKH-PRARTSTD                 
393300       END-IF                                                             
393400     END-IF                                                               
393500                                                                          
393600     MOVE INLA-INL-IDLEVNR      TO W-IDLEVNR                              
393700     PERFORM IMS-GU-WDF101                                                
393800     IF SEGMENT-SAKNAS                                                    
393900       MOVE ZERO TO W-RETULF                                              
394000     ELSE                                                                 
394100       MOVE WS-IDDC          TO W-IDDC-B6                                 
394200       PERFORM IMS-GU-WDB601                                              
394300       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
394400       PERFORM IMS-GNP-WDF102                                             
394500       IF SEGMENT-FINNS                                                   
394600         IF F102-TULL-TITULF < DAGENS-DATUM                               
394700           MOVE F102-TULL-RETULF-1  TO W-RETULF                           
394800         ELSE                                                             
394900           MOVE F102-TULL-RETULF-2  TO W-RETULF                           
395000         END-IF                                                           
395100       END-IF                                                             
395200     END-IF                                                               
395300                                                                          
395400     COMPUTE EKO-EKH-PRHEMTAG ROUNDED = (W-RETULF - 1) *                  
395500             EKO-EKH-PRARTSTD * EKO-EKH-KVAVIS                            
395600     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
395700     IF EKO-EKH-PRHEMTAG > 0                                              
395800       COMPUTE WS-SUHEMT = EKO-EKH-PRHEMTAG * -1                          
395900     ELSE                                                                 
396000       MOVE EKO-EKH-PRHEMTAG          TO WS-SUHEMT                        
396100     END-IF                                                               
396200     MOVE WS-SUHEMT                   TO EKO-EKH-SUBEL                    
396300     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
396400     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
396500     MOVE SPACE                       TO EKO-EKH-BEVAT                    
396600                                         EKO-EKH-KDANMORS                 
396700                                         EKO-EKH-IDKST                    
396800     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
396900                                         EKO-EKH-KDFRAKT                  
397000                                         EKO-EKH-SUVAT                    
397100                                                                          
397200                                                                          
397300     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
397400     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
397500     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
397600     MOVE 0                          TO EKO-EKH-KDAVVTYP                  
397700     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
397800     MOVE ZERO                       TO EKO-EKH-KVANTMOT                  
397900     MOVE ZERO                       TO EKO-EKH-KVAVIS                    
398000     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
398100     MOVE SPACE                      TO EKO-EKH-FLDCET                    
398200     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
398300                                                                          
398400     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
398500                                        EKO-EKH-PRDMTRL                   
398600                                        EKO-EKH-PROVRPAL                  
398700     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
398800     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
398900     EVALUATE TRUE                                                        
399000       WHEN NDC-CN                                                        
399100         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
399200       WHEN NDC-IN                                                        
399300         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
399400       WHEN NDC-US                                                        
399500         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
399600       WHEN OTHER                                                         
399700         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
399800     END-EVALUATE                                                         
399900     MOVE 'BAK'                      TO EKO-EKH-CMD                       
400000     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
400100                                                                          
400200     IF EKO-EKH-PRHEMTAG > ZERO                                           
400300     OR EKO-EKH-PRHEMTAG < ZERO                                           
400400       PERFORM IMS-ISRT-EKOTRANS                                          
400500                                                                          
400600       PERFORM UNTIL SEGMENT-FINNS                                        
400700         ADD +1                     TO EKO-FIL-IDSEKVNR                   
400800         PERFORM IMS-ISRT-EKOTRANS                                        
400900       END-PERFORM                                                        
401000     END-IF                                                               
401100     .                                                                    
401200     EJECT                                                                
401300                                                                          
401400 JHA-LOGG-EKO-VCCN-KALK SECTION.                                          
401500     IF FL-FORE-INST = JA                                                 
401600       COMPUTE EKO-EKH-KVAVIS = (WS-INLE-MOT-KVANTMOT -                   
401700                      INLA-ART-KVAVIS + WS-INLE-MOT-KVRETUR) * -1         
401800       END-COMPUTE                                                        
401900     ELSE                                                                 
402000       COMPUTE EKO-EKH-KVAVIS = (WS-INLE-MOT-KVANTMOT -                   
402100                         INLA-ART-KVAVIS) * -1                            
402200       END-COMPUTE                                                        
402300     END-IF                                                               
402400     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
402500     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
402600     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
402700     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
402800     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
402900     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
403000     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
403100     MOVE 'KALK '                     TO EKO-EKH-KDEKNIVA                 
403200     MOVE INLA-INL-IDDC               TO EKO-EKH-IDDC-SEND                
403300     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
403400     MOVE +0                          TO EKO-EKH-IDDISTR                  
403500                                         EKO-EKH-IDKUNDNR                 
403600*******************************                                           
403700*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
403800     MOVE ZERO TO NOLL-RAKNARE                                            
403900     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
404000     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
404100     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
404200          FOR LEADING ZERO                                                
404300     ADD +1 TO NOLL-RAKNARE                                               
404400     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
404500          WITH POINTER NOLL-RAKNARE                                       
404600*******************************                                           
404700     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
404800     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
404900                                         EKO-EKH-PRARTNTO                 
405000                                         EKO-EKH-PRARTSJK                 
405100                                         EKO-EKH-PRLANDCO                 
405200                                         EKO-EKH-SUBEL                    
405300     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
405400     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
405500                                                                          
405600     MOVE 1.00                        TO EKO-EKH-PRKURS                   
405700                                                                          
405800     MOVE ZERO                        TO EKO-EKH-PRINK                    
405900                                                                          
406000     MOVE DAGENS-DATUM(3:2) TO W-DATE-AAMM(1:2)                           
406100     MOVE 01                TO W-DATE-AAMM(3:2)                           
406200     MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                          
406300     MOVE DCS-KDVALISO      TO CURR-KDVALISO-ROW                          
406400     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
406500     MOVE 'A'               TO CURR-KDVALTYP                              
406600     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
406700     IF CURR-KDSVAR = ' '                                                 
406800       MOVE CURR-PRKURS-NEW  TO WS-PRKURS                                 
406900       MOVE CURR-REVALUTA-TO TO WS-REVALUTA                               
407000     ELSE                                                                 
407100       MOVE 1                TO WS-PRKURS                                 
407200       MOVE 1                TO WS-REVALUTA                               
407300     END-IF                                                               
407400     COMPUTE EKO-EKH-PRDIRLON ROUNDED = WS-ARTC-PRDIRLON *                
407500      PROC-REDIRLON /  WS-PRKURS / WS-REVALUTA * EKO-EKH-KVAVIS           
407600     COMPUTE EKO-EKH-PRDMTRL  ROUNDED = WS-ARTC-PRDMTRL  *                
407700      PROC-REDMTRL  /  WS-PRKURS / WS-REVALUTA * EKO-EKH-KVAVIS           
407800     MOVE ZERO                        TO EKO-EKH-PROVRPAL                 
407900     MOVE EKO-EKH-PRDMTRL             TO WS-SUDIRMTRL                     
408000     MOVE EKO-EKH-PRDIRLON            TO WS-SUDIRLON                      
408100     COMPUTE EKO-EKH-SUBEL = EKO-EKH-PRDIRLON +                           
408200                             EKO-EKH-PRDMTRL                              
408300                                                                          
408400     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
408500     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
408600     MOVE SPACE                       TO EKO-EKH-BEVAT                    
408700                                         EKO-EKH-KDANMORS                 
408800                                         EKO-EKH-IDKST                    
408900     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
409000                                         EKO-EKH-KDFRAKT                  
409100                                         EKO-EKH-SUVAT                    
409200                                                                          
409300     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
409400     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
409500     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
409600     MOVE 0                          TO EKO-EKH-KDAVVTYP                  
409700     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
409800     MOVE ZERO                       TO EKO-EKH-KVANTMOT                  
409900     MOVE ZERO                       TO EKO-EKH-KVAVIS                    
410000     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
410100     MOVE SPACE                      TO EKO-EKH-FLDCET                    
410200     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
410300                                                                          
410400     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
410500                                        EKO-EKH-PRDMTRL                   
410600                                        EKO-EKH-PROVRPAL                  
410700     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
410800     MOVE WS-ARTC-KDVALISO           TO EKO-EKH-KDVALISO                  
410900     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
411000     EVALUATE TRUE                                                        
411100       WHEN NDC-CN                                                        
411200         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
411300       WHEN NDC-IN                                                        
411400         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
411500       WHEN NDC-US                                                        
411600         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
411700       WHEN OTHER                                                         
411800         IF NDC-KR                                                        
411810         OR NDC-MX                                                        
411820         OR NDC-BR                                                        
411830         OR NDC-ZA                                                        
411900           MOVE '103'                TO EKO-EKH-KDEKHHT                   
412000           MOVE '106'                TO EKO-EKH-KDEKSHT                   
412100         END-IF                                                           
412200         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
412300     END-EVALUATE                                                         
412400     MOVE 'BAK'                      TO EKO-EKH-CMD                       
412500     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
412600                                                                          
412700     IF EKO-EKH-SUBEL > ZERO                                              
412800     OR EKO-EKH-SUBEL < ZERO                                              
412900       PERFORM IMS-ISRT-EKOTRANS                                          
413000       PERFORM UNTIL SEGMENT-FINNS                                        
413100         ADD +1                     TO EKO-FIL-IDSEKVNR                   
413200         PERFORM IMS-ISRT-EKOTRANS                                        
413300       END-PERFORM                                                        
413400     END-IF                                                               
413500     .                                                                    
413600     EJECT                                                                
413700                                                                          
413800 JHA-LOGG-EKO-VCCN-SUM SECTION.                                           
413900     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
414000     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
414100     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
414200     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
414300     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
414400     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
414500     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
414600     MOVE 'SUM  '                     TO EKO-EKH-KDEKNIVA                 
414700     MOVE INLA-INL-IDDC               TO EKO-EKH-IDDC-SEND                
414800     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
414900     MOVE +0                          TO EKO-EKH-IDDISTR                  
415000                                         EKO-EKH-IDKUNDNR                 
415100*******************************                                           
415200*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
415300     MOVE ZERO TO NOLL-RAKNARE                                            
415400     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
415500     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
415600     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
415700          FOR LEADING ZERO                                                
415800     ADD +1 TO NOLL-RAKNARE                                               
415900     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
416000          WITH POINTER NOLL-RAKNARE                                       
416100*******************************                                           
416200     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
416300     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
416400                                         EKO-EKH-PRARTNTO                 
416500                                         EKO-EKH-PRARTSJK                 
416600                                         EKO-EKH-PRLANDCO                 
416700                                         EKO-EKH-SUBEL                    
416800     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
416900     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
417000                                                                          
417100     MOVE 1.00                        TO EKO-EKH-PRKURS                   
417200                                                                          
417300     MOVE ZERO                        TO EKO-EKH-PRINK                    
417400                                                                          
417500     COMPUTE EKO-EKH-SUBEL = WS-SUARTSTD                                  
417600                                                                          
417700     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
417800     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
417900     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
418000     MOVE SPACE                       TO EKO-EKH-BEVAT                    
418100                                         EKO-EKH-KDANMORS                 
418200                                         EKO-EKH-IDKST                    
418300     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
418400                                         EKO-EKH-KDFRAKT                  
418500                                         EKO-EKH-SUVAT                    
418600                                                                          
418700                                                                          
418800     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
418900     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
419000     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
419100     MOVE 0                          TO EKO-EKH-KDAVVTYP                  
419200     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
419300     MOVE ZERO                       TO EKO-EKH-KVANTMOT                  
419400     MOVE ZERO                       TO EKO-EKH-KVAVIS                    
419500     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
419600     MOVE SPACE                      TO EKO-EKH-FLDCET                    
419700     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
419800                                                                          
419900     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
420000                                        EKO-EKH-PRDMTRL                   
420100                                        EKO-EKH-PROVRPAL                  
420200     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
420300                                                                          
420400     MOVE WS-ARTC-KDVALISO           TO EKO-EKH-KDVALISO                  
420500     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
420600     EVALUATE TRUE                                                        
420700       WHEN NDC-CN                                                        
420800         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
420900       WHEN NDC-IN                                                        
421000         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
421100       WHEN NDC-US                                                        
421200         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
421300       WHEN OTHER                                                         
421400         IF NDC-KR                                                        
421410         OR NDC-MX                                                        
421420         OR NDC-BR                                                        
421430         OR NDC-ZA                                                        
421500           MOVE '103'                TO EKO-EKH-KDEKHHT                   
421600           MOVE '106'                TO EKO-EKH-KDEKSHT                   
421700         END-IF                                                           
421800         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
421900     END-EVALUATE                                                         
422000     MOVE 'BAK'                      TO EKO-EKH-CMD                       
422100     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
422200     IF EKO-EKH-SUBEL > ZERO                                              
422300     OR EKO-EKH-SUBEL < ZERO                                              
422400       PERFORM IMS-ISRT-EKOTRANS                                          
422500       PERFORM UNTIL SEGMENT-FINNS                                        
422600         ADD +1                     TO EKO-FIL-IDSEKVNR                   
422700         PERFORM IMS-ISRT-EKOTRANS                                        
422800       END-PERFORM                                                        
422900     END-IF                                                               
423000     .                                                                    
423100     EJECT                                                                
423200                                                                          
423300 JHB-LOGG-EKO-VCCS SECTION.                                               
423400     MOVE SPACE                      TO EKO-W51080                        
423500     MOVE SPACE                      TO WS-SAP-MM-POST                    
423600                                                                          
423700     MOVE INLA-ART-IDARTNR           TO EKO-IDARTNR                       
423800     MOVE INLA-INL-IDDC              TO EKO-IDDC                          
423900     MOVE INLA-INL-IDFS              TO EKO-IDFS                          
424000     MOVE WS-ARTC-IDINK-X            TO EKO-IDINK                         
424100     MOVE INLA-INL-IDKONTO           TO EKO-IDKONTO                       
424200     MOVE INLA-INL-IDLEVNR           TO EKO-IDLEVNR                       
424300     MOVE INLA-ART-IDLOPNRM          TO EKO-IDLOPNRM                      
424400     MOVE WS-ARTC-KDPRODSL           TO EKO-KDPRODSL                      
424500     MOVE INLA-ART-KDRT              TO EKO-KDRT                          
424600     MOVE WS-ARTC-KDSORT             TO EKO-KDSORT                        
424700     MOVE WS-ARTC-KDTIPPR            TO EKO-KDTIPPR                       
424800     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
424900       MOVE WS-ARTC-PRARTBEL-PR      TO EKO-PRARTBEL-PR                   
425000       MOVE WS-ARTC-KDVALISO         TO EKO-KDVALISO                      
425100     ELSE                                                                 
425200       MOVE 0.1                      TO EKO-PRARTBEL-PR                   
425300       MOVE 'XXX'                    TO EKO-KDVALISO                      
425400     END-IF                                                               
425500     IF INLA-INL-IDLEVNR = '1441'                                         
425600       MOVE NEJ                      TO WS-SAP-MM-POST                    
425700     END-IF                                                               
425800                                                                          
425900     IF FL-FORE-INST = JA                                                 
426000       COMPUTE EKO-KVAVIS = (WS-INLE-MOT-KVANTMOT -                       
426100                      INLA-ART-KVAVIS + WS-INLE-MOT-KVRETUR) * -1         
426200       END-COMPUTE                                                        
426300     ELSE                                                                 
426400       COMPUTE EKO-KVAVIS = (WS-INLE-MOT-KVANTMOT -                       
426500                         INLA-ART-KVAVIS) * -1                            
426600       END-COMPUTE                                                        
426700     END-IF                                                               
426800     MOVE EKO-KVAVIS TO WS-SPAR-KVAVIS                                    
426900                                                                          
427000     IF  WS-ARTC-PRARTBES-PR   > ZERO                                     
427100       MOVE WS-ARTC-PRARTBES-PR      TO EKO-PRARTBES                      
427200     ELSE                                                                 
427300       MOVE 0.1                      TO EKO-PRARTBES                      
427400     END-IF                                                               
427500                                                                          
427600     MOVE WS-ARTC-PRINK              TO EKO-PRINK                         
427700     MOVE WS-ARTC-PRHEMTAG           TO EKO-PRHEMTAG                      
427800     MOVE ZERO                       TO EKO-RETULF                        
427900     MOVE INLA-INL-TIAVIDAT          TO EKO-TIAVIDAT                      
428000     MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                      
428100     MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                       
428200     MOVE JA                         TO EKO-FLLSBOK                       
428300     MOVE ZERO                       TO EKO-IDDISTR                       
428400     MOVE NEJ                        TO EKO-FLDIRLEV                      
428500     MOVE WS-ARTC23-IDAVTAL          TO EKO-IDAVTAL                       
428600     MOVE 57                         TO EKO-IDFTG                         
428700                                                                          
428800     MOVE JA                         TO EKO-FLAVVINL                      
428900     MOVE 'V'                        TO EKO-KDINLAVV                      
429000                                                                          
429100     MOVE 'W6011910'                 TO EKO-FIL-IDPGM                     
429200     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
429300     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
429400     MOVE 1                          TO EKO-FIL-IDSEKVNR                  
429500     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
429600     MOVE '80 '                      TO EKO-FIL-CT-IDPTYP                 
429700     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
429800                                                                          
429900     IF WS-SAP-MM-POST = NEJ                                              
430000       CONTINUE                                                           
430100     ELSE                                                                 
430200       PERFORM IMS-ISRT-EKOTRANS                                          
430300                                                                          
430400       PERFORM UNTIL SEGMENT-FINNS                                        
430500         ADD +1 TO EKO-FIL-IDSEKVNR                                       
430600         PERFORM IMS-ISRT-EKOTRANS                                        
430700       END-PERFORM                                                        
430800     END-IF                                                               
430900                                                                          
431000     MOVE 'W6011910'                  TO FIL-IDPGM                        
431100     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
431200                                         EKH-DAVERDAT                     
431300     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
431400     MOVE +1                          TO FIL-IDSEKVNR                     
431500     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
431600     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
431700     MOVE '103'                       TO EKH-KDEKHHT                      
431800     MOVE '102'                       TO EKH-KDEKSHT                      
431900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
432000     MOVE INLA-INL-IDDC               TO EKH-IDDC-SEND                    
432100     MOVE SPACE                       TO EKH-IDDC-REC                     
432200     MOVE +0                          TO EKH-IDDISTR                      
432300                                         EKH-IDKUNDNR                     
432400*******************************                                           
432500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
432600     MOVE ZERO TO NOLL-RAKNARE                                            
432700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
432800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
432900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
433000          FOR LEADING ZERO                                                
433100     ADD +1 TO NOLL-RAKNARE                                               
433200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
433300          WITH POINTER NOLL-RAKNARE                                       
433400*******************************                                           
433500     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
433600     MOVE ZERO                        TO EKH-KDPSLLOC                     
433700                                         EKH-PRARTNTO                     
433800                                         EKH-PRARTSJK                     
433900                                         EKH-PRLANDCO                     
434000                                         EKH-SUBEL                        
434100     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
434200     MOVE SPACE                       TO EKH-FLLSBOK                      
434300                                                                          
434400     IF EKO-KDVALISO = 'XXX'                                              
434500       MOVE 'SEK'                     TO EKH-KDVALISO                     
434600     ELSE                                                                 
434700       MOVE EKO-KDVALISO              TO EKH-KDVALISO                     
434800     END-IF                                                               
434900     MOVE 1.00                        TO EKH-PRKURS                       
435000                                                                          
435100     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
435200     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
435300     IF INLA-INL-IDLEVNR = '1002 '                                        
435400       MOVE ZERO                      TO EKH-PRHEMTAG                     
435500     ELSE                                                                 
435600       MOVE WS-ARTC-PRHEMTAG          TO EKH-PRHEMTAG                     
435700     END-IF                                                               
435800     MOVE WS-SPAR-KVAVIS              TO EKH-KVANTAL                      
435900     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
436000     MOVE SPACE                       TO EKH-BEVAT                        
436100                                         EKH-KDANMORS                     
436200                                         EKH-IDKST                        
436300     MOVE ZERO                        TO EKH-IDKONTO                      
436400                                         EKH-KDFRAKT                      
436500                                         EKH-SUVAT                        
436600                                                                          
436700                                                                          
436800     MOVE WS-INLA-TIAVIDAT           TO EKH-DAAVIDAT                      
436900     MOVE WS-IDAVINR                 TO EKH-IDAVINR                       
437000     MOVE INLA-INL-IDLEVNR           TO EKH-IDLEVNR                       
437100     IF EKH-KVANTAL > ZERO                                                
437200       MOVE 1                        TO EKH-KDAVVTYP                      
437300     ELSE                                                                 
437400       MOVE 0                        TO EKH-KDAVVTYP                      
437500     END-IF                                                               
437600     MOVE INLA-ART-KDRT              TO EKH-KDRT                          
437700     MOVE WS-INLE-MOT-KVANTMOT       TO EKH-KVANTMOT                      
437800     MOVE INLA-ART-KVAVIS            TO EKH-KVAVIS                        
437900     MOVE WS-ARTC-KDSORT             TO EKH-KDSORT                        
438000     MOVE 'SEPV'                     TO EKH-KDTRADP                       
438100     MOVE SPACE                      TO EKH-FLDCET                        
438200     MOVE SPACE                      TO EKH-IDKUNDRF                      
438300     MOVE SPACE                      TO EKH-IDFAKT-EXP                    
438400                                                                          
438500     MOVE ZERO                       TO EKH-PRDIRLON                      
438600                                        EKH-PRDMTRL                       
438700                                        EKH-PROVRPAL                      
438800     MOVE INLA-INL-IDANALYS          TO EKH-IDANALYS                      
438900                                                                          
439000     PERFORM IMS-ISRT-WLSAPA01                                            
439100                                                                          
439200     PERFORM UNTIL SEGMENT-FINNS                                          
439300       ADD +1                     TO FIL-IDSEKVNR                         
439400       PERFORM IMS-ISRT-WLSAPA01                                          
439500     END-PERFORM                                                          
439600     .                                                                    
439700     EJECT                                                                
439800 JI-PALAGG-WDR9 SECTION.                                                  
439900     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
440000       CONTINUE                                                           
440100     ELSE                                                                 
440200       PERFORM JIB-PALAGG-VCCS                                            
440300     END-IF                                                               
440400     .                                                                    
440500     EJECT                                                                
440600                                                                          
440700                                                                          
440800 JIB-PALAGG-VCCS SECTION.                                                 
440900     MOVE 'W6011910'                  TO FIL-IDPGM                        
441000     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
441100                                         EKH-DAVERDAT                     
441200     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
441300     MOVE +1                          TO FIL-IDSEKVNR                     
441400     MOVE '103'                       TO EKH-KDEKHHT                      
441500     MOVE '101'                       TO EKH-KDEKSHT                      
441600     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
441700     MOVE INLA-INL-IDDC               TO EKH-IDDC-SEND                    
441800     MOVE SPACE                       TO EKH-IDDC-REC                     
441900     MOVE +0                          TO EKH-IDDISTR                      
442000                                         EKH-IDKUNDNR                     
442100*******************************                                           
442200*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
442300     MOVE ZERO TO NOLL-RAKNARE                                            
442400     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
442500     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
442600     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
442700          FOR LEADING ZERO                                                
442800     ADD +1 TO NOLL-RAKNARE                                               
442900     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
443000          WITH POINTER NOLL-RAKNARE                                       
443100*******************************                                           
443200     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
443300     MOVE ZERO                        TO EKH-KDPSLLOC                     
443400                                         EKH-PRARTNTO                     
443500                                         EKH-PRARTSJK                     
443600                                         EKH-PRHEMTAG                     
443700                                         EKH-PRLANDCO                     
443800                                         EKH-SUBEL                        
443900     MOVE WS-ARTC-PRDIRLON            TO EKH-PRDIRLON                     
444000     MOVE WS-ARTC-PRDMTRL             TO EKH-PRDMTRL                      
444100     MOVE WS-ARTC-PROVRPAL            TO EKH-PROVRPAL                     
444200     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
444300     MOVE SPACE                       TO EKH-FLLSBOK                      
444400                                                                          
444500     MOVE 'SEK'                       TO EKH-KDVALISO                     
444600     MOVE 1.00                        TO EKH-PRKURS                       
444700                                                                          
444800     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
444900     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
445000     MOVE INLA-INL-IDANALYS           TO EKH-IDANALYS                     
445100                                                                          
445200     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
445300     COMPUTE EKH-KVANTAL =                                                
445400         (WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS) * -1                    
445500     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
445600     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
445700     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
445800     MOVE ZERO                        TO EKH-IDKONTO                      
445900                                         EKH-KDFRAKT                      
446000                                         EKH-SUVAT                        
446100     MOVE SPACE                       TO EKH-BEVAT                        
446200                                         EKH-KDANMORS                     
446300                                         EKH-IDKST                        
446400                                                                          
446500     MOVE INLA-INL-TIAVIDAT           TO WS-DAAVIDAT-YYMMDD               
446600     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
446700       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
446800     ELSE                                                                 
446900       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
447000     END-IF                                                               
447100     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
447200     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
447300     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
447400     IF INLA-ART-KVAVIS > ZERO                                            
447500       MOVE 1                         TO EKH-KDAVVTYP                     
447600     ELSE                                                                 
447700       MOVE 0                         TO EKH-KDAVVTYP                     
447800     END-IF                                                               
447900     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
448000     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
448100     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
448200     MOVE 'SEPV'                      TO EKH-KDTRADP                      
448300     MOVE SPACE                       TO EKH-FLDCET                       
448400     MOVE SPACE                       TO EKH-IDKUNDRF                     
448500     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
448600                                                                          
448700     IF EKH-PRDIRLON > ZERO                                               
448800     OR EKH-PRDMTRL  > ZERO                                               
448900       IF EKH-KVANTAL NOT = 0                                             
449000         PERFORM IMS-ISRT-WLSAPA01                                        
449100                                                                          
449200         PERFORM UNTIL SEGMENT-FINNS                                      
449300           ADD +1 TO FIL-IDSEKVNR                                         
449400           PERFORM IMS-ISRT-WLSAPA01                                      
449500         END-PERFORM                                                      
449600       END-IF                                                             
449700     END-IF                                                               
449800     .                                                                    
449900     EJECT                                                                
450000 JJ-LOGG-R32-ULEV-WDR8 SECTION.                                           
450100     IF DCS-LAND-NON-VCC-OWNED                                            
450200       PERFORM JHA-LOGG-EKO-VCCN-DET                                      
450300       IF  WS-ARTC-PRINK NOT = WS-ARTC-PRARTSTD                           
450400       AND (INLA-ART-KDRT NOT = 3 AND 7 AND 8 AND 77)                     
450500         PERFORM IMS-GU-WDB601                                            
450600         MOVE DCS-IDLANDX2            TO W-IDLANDX2                       
450700         IF SEGMENT-FINNS                                                 
450800           PERFORM IMS-GNP-WDB617                                         
450900           IF SEGMENT-FINNS                                               
451000             IF DCS-USA                                                   
451100               CONTINUE                                                   
451200             ELSE                                                         
451300               PERFORM JHA-LOGG-EKO-VCCN-KALK                             
451400             END-IF                                                       
451500           END-IF                                                         
451600         END-IF                                                           
451700       END-IF                                                             
451800       IF DCS-USA                                                         
451900         CONTINUE                                                         
452000       ELSE                                                               
452100         PERFORM JHA-LOGG-EKO-VCCN-SUM                                    
452200       END-IF                                                             
452300     END-IF                                                               
452400     .                                                                    
452500     EJECT                                                                
452600 JJ-LOGG-R32-ULEV-WDR9 SECTION.                                           
452700     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
452800       CONTINUE                                                           
452900     ELSE                                                                 
453000       PERFORM JJB-LOGG-R32-ULEV-VCCS                                     
453100     END-IF                                                               
453200     .                                                                    
453300     EJECT                                                                
453400                                                                          
453500 JJB-LOGG-R32-ULEV-VCCS SECTION.                                          
453600     MOVE 'W6011910'                  TO FIL-IDPGM                        
453700     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
453800                                         EKH-DAVERDAT                     
453900     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
454000     MOVE +1                          TO FIL-IDSEKVNR                     
454100     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
454200     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
454300     MOVE '102'                       TO EKH-KDEKHHT                      
454400     MOVE '106'                       TO EKH-KDEKSHT                      
454500     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
454600     MOVE WC-CDC-SE                   TO EKH-IDDC-SEND                    
454700     MOVE SPACE                       TO EKH-IDDC-REC                     
454800     MOVE +0                          TO EKH-IDDISTR                      
454900                                         EKH-IDKUNDNR                     
455000*******************************                                           
455100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
455200     MOVE ZERO TO NOLL-RAKNARE                                            
455300     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
455400     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
455500     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
455600          FOR LEADING ZERO                                                
455700     ADD +1 TO NOLL-RAKNARE                                               
455800     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
455900          WITH POINTER NOLL-RAKNARE                                       
456000*******************************                                           
456100     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
456200     MOVE ZERO                        TO EKH-KDPSLLOC                     
456300                                         EKH-PRARTNTO                     
456400                                         EKH-PRARTSJK                     
456500                                         EKH-PRHEMTAG                     
456600                                         EKH-PRINK                        
456700                                         EKH-PRLANDCO                     
456800                                         EKH-PRDIRLON                     
456900                                         EKH-PRDMTRL                      
457000                                         EKH-PROVRPAL                     
457100                                         EKH-SUBEL                        
457200     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
457300     MOVE SPACE                       TO EKH-FLLSBOK                      
457400     MOVE 'SEK'                       TO EKH-KDVALISO                     
457500     MOVE 1.00                        TO EKH-PRKURS                       
457600                                                                          
457700     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
457800     MOVE WS-SPAR-KVAVIS              TO EKH-KVANTAL                      
457900     MOVE '6119'                      TO EKH-IDTRANS                      
458000     MOVE SPACE                       TO EKH-BEVAT                        
458100                                         EKH-IDANALYS                     
458200                                         EKH-KDANMORS                     
458300                                         EKH-IDKST                        
458400     MOVE ZERO                        TO EKH-IDKONTO                      
458500                                         EKH-KDFRAKT                      
458600                                         EKH-SUVAT                        
458700                                                                          
458800                                                                          
458900     MOVE FUNCTION CURRENT-DATE (1:8) TO EKH-DAAVIDAT                     
459000     MOVE ZERO                        TO EKH-IDAVINR                      
459100     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
459200     MOVE ZERO                        TO EKH-KDAVVTYP                     
459300                                         EKH-KDRT                         
459400                                         EKH-KVANTMOT                     
459500                                         EKH-KVAVIS                       
459600     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
459700     MOVE 'SEPV'                      TO EKH-KDTRADP                      
459800     MOVE SPACE                       TO EKH-FLDCET                       
459900     MOVE SPACE                       TO EKH-IDKUNDRF                     
460000     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
460100                                                                          
460200                                                                          
460300     PERFORM IMS-ISRT-WLSAPA01                                            
460400                                                                          
460500     PERFORM UNTIL SEGMENT-FINNS                                          
460600       ADD +1 TO FIL-IDSEKVNR                                             
460700       PERFORM IMS-ISRT-WLSAPA01                                          
460800     END-PERFORM                                                          
460900     .                                                                    
461000     EJECT                                                                
461100 JK-UPPD-INLA SECTION.                                                    
461200                                                                          
461300     PERFORM IMS-GHU-INLA-ART                                             
461400     MOVE JA                     TO INLA-ART-FLANNULL                     
461500     MOVE WS-TIAAMMDD            TO INLA-ART-TIUPPDAT                     
461600     PERFORM IMS-REPL-INLA-ART                                            
461700     .                                                                    
461800     EJECT                                                                
461900                                                                          
462000 JL-LOGG-EKO-R31-WDR8 SECTION.                                            
462100     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
462200       PERFORM JLA-LOGG-EKO-R31-VCCN-DET                                  
462300       IF DCS-NDC-CN OR DCS-USA                                           
462400         PERFORM JLA-LOGG-EKO-R31-VCCN-HEMT                               
462500       END-IF                                                             
462600       IF  WS-ARTC-PRINK NOT = WS-ARTC-PRARTSTD                           
462700       AND (INLA-ART-KDRT NOT = 3 AND 7 AND 8 AND 77)                     
462800         PERFORM IMS-GU-WDB601                                            
462900         MOVE DCS-IDLANDX2            TO W-IDLANDX2                       
463000         IF SEGMENT-FINNS                                                 
463100           PERFORM IMS-GNP-WDB617                                         
463200           IF SEGMENT-FINNS                                               
463300             IF DCS-USA                                                   
463400               CONTINUE                                                   
463500             ELSE                                                         
463600               PERFORM JLA-LOGG-EKO-R31-VCCN-KALK                         
463700             END-IF                                                       
463800           END-IF                                                         
463900         END-IF                                                           
464000       END-IF                                                             
464100       IF DCS-USA                                                         
464200         CONTINUE                                                         
464300       ELSE                                                               
464400         PERFORM JLA-LOGG-EKO-R31-VCCN-SUM                                
464500       END-IF                                                             
464600     ELSE                                                                 
464700       PERFORM JLB-LOGG-EKO-R31-VCCS                                      
464800     END-IF                                                               
464900     .                                                                    
465000     EJECT                                                                
465100                                                                          
465200 JLA-LOGG-EKO-R31-VCCN-DET SECTION.                                       
465300     IF DCS-NDC-CN                                                        
465400     OR DCS-USA                                                           
465500       MOVE SPACE                    TO EKO-W51080                        
465600       MOVE SPACE                    TO WS-SAP-MM-POST                    
465700                                                                          
465800       MOVE INLA-ART-IDARTNR         TO EKO-IDARTNR                       
465900       MOVE INLA-ART-IDDC            TO EKO-IDDC                          
466000       MOVE INLA-ART-KDRT            TO EKO-KDRT                          
466100       COMPUTE EKO-KVAVIS = INLA-ART-KVAVIS * -1                          
466200       MOVE EKO-KVAVIS TO WS-SPAR-KVAVIS                                  
466300       MOVE ZERO                     TO EKO-PRINK                         
466400       MOVE ZERO                     TO EKO-PRHEMTAG                      
466500                                                                          
466600       IF DCS-NDC-CN                                                      
466700       OR DCS-USA                                                         
466800         PERFORM S07-GET-PRARTBEL-BACKNING                                
466900         IF DCS-NDC-CN                                                    
467000           MOVE 60                   TO EKO-IDFTG                         
467100           MOVE WS-KDVALISO-HUV-CN   TO CURR-KDVALISO-HUV                 
467200         ELSE                                                             
467300           MOVE 53                   TO EKO-IDFTG                         
467400           MOVE WS-KDVALISO-HUV-US   TO CURR-KDVALISO-HUV                 
467500         END-IF                                                           
467600         PERFORM S06-GET-PRARTBEL                                         
467700         MOVE INLA-INL-TIAVIDAT      TO WS-DAAVIDAT-YYMMDD                
467800         MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)                 
467900         MOVE WS-DAAVIDAT-YYMMDD(3:2) TO W-DATE-AAMM(3:2)                 
468000         MOVE WS-ARTC-KDVALISO       TO CURR-KDVALISO-ROW                 
468100         MOVE W-DATE-AAMM            TO CURR-TIAAMM                       
468200         MOVE 'M'                    TO CURR-KDVALTYP                     
468300         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
468400         IF CURR-KDSVAR = ' '                                             
468500           MOVE CURR-PRKURS-NEW      TO WS-PRKURS                         
468600           MOVE CURR-REVALUTA-TO     TO WS-REVALUTA                       
468700         ELSE                                                             
468800           MOVE 1                    TO WS-PRKURS                         
468900           MOVE 1                    TO WS-REVALUTA                       
469000         END-IF                                                           
469100         MOVE WS-ARTC-KDVALISO       TO EKO-KDVALISO                      
469200****   AGREE PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED              
469300****   TO COUNTRY CURRENCY                                                
469400         COMPUTE WS-ARTC-PRARTBES-PR = WS-ARTC-PRARTBEL-PR                
469500                                     * WS-PRKURS / WS-REVALUTA            
469600       END-IF                                                             
469700                                                                          
469800       MOVE INLA-INL-IDKONTO         TO EKO-IDKONTO                       
469900       MOVE INLA-ART-IDLOPNRM        TO EKO-IDLOPNRM                      
470000       MOVE INLA-INL-IDFS            TO EKO-IDFS                          
470100       MOVE INLA-INL-IDLEVNR         TO EKO-IDLEVNR                       
470200       MOVE WS-ARTC-KDTIPPR          TO EKO-KDTIPPR                       
470300       MOVE INLA-INL-TIAVIDAT        TO EKO-TIAVIDAT                      
470400       IF WS-ARTC-PRARTBEL-PR  > ZERO                                     
470500         MOVE WS-ARTC-PRARTBEL-PR    TO EKO-PRARTBEL-PR                   
470600         MOVE WS-ARTC-KDVALISO       TO EKO-KDVALISO                      
470700       ELSE                                                               
470800         MOVE 0.1                    TO EKO-PRARTBEL-PR                   
470900         MOVE 'XXX'                  TO EKO-KDVALISO                      
471000       END-IF                                                             
471100       IF WS-ARTC-PRARTBES-PR  > ZERO                                     
471200         MOVE WS-ARTC-PRARTBES-PR    TO EKO-PRARTBES                      
471300       ELSE                                                               
471400         MOVE 0.1                    TO EKO-PRARTBES                      
471500       END-IF                                                             
471600       IF INLA-INL-IDLEVNR = '1441'                                       
471700         MOVE NEJ                    TO WS-SAP-MM-POST                    
471800       END-IF                                                             
471900       MOVE ZERO                     TO EKO-RETULF                        
472000                                                                          
472100       MOVE WS-ARTC-KDPRODSL         TO EKO-KDPRODSL                      
472200       MOVE WS-ARTC-IDINK-X          TO EKO-IDINK                         
472300       MOVE WS-ARTC-KDSORT           TO EKO-KDSORT                        
472400       MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                    
472500       MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                     
472600       MOVE JA                       TO EKO-FLLSBOK                       
472700       MOVE ZERO                     TO EKO-IDDISTR                       
472800       MOVE NEJ                      TO EKO-FLDIRLEV                      
472900       MOVE WS-ARTC23-IDAVTAL        TO EKO-IDAVTAL                       
473000       IF DCS-NDC-CN OR DCS-USA                                           
473100         IF DCS-NDC-CN                                                    
473200           MOVE 60                   TO EKO-IDFTG                         
473300         ELSE                                                             
473400           MOVE 53                   TO EKO-IDFTG                         
473500         END-IF                                                           
473600       END-IF                                                             
473700                                                                          
473800       MOVE JA                       TO EKO-FLAVVINL                      
473900       MOVE 'V'                      TO EKO-KDINLAVV                      
474000                                                                          
474100       MOVE 'W6011910'               TO EKO-FIL-IDPGM                     
474200       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
474300       ACCEPT EKO-FIL-TIKLOCK FROM TIME                                   
474400       MOVE 1                        TO EKO-FIL-IDSEKVNR                  
474500       MOVE 'W510'                   TO EKO-FIL-CT-IDSYSTEM               
474600       MOVE '80 '                    TO EKO-FIL-CT-IDPTYP                 
474700       MOVE ' '                      TO EKO-FIL-CT-IDVTYP                 
474800                                                                          
474900       IF WS-SAP-MM-POST = NEJ                                            
475000         CONTINUE                                                         
475100       ELSE                                                               
475200         PERFORM IMS-ISRT-EKOTRANS                                        
475300                                                                          
475400         PERFORM UNTIL SEGMENT-FINNS                                      
475500           ADD +1 TO EKO-FIL-IDSEKVNR                                     
475600           PERFORM IMS-ISRT-EKOTRANS                                      
475700         END-PERFORM                                                      
475800       END-IF                                                             
475900     END-IF                                                               
476000                                                                          
476100     COMPUTE EKO-EKH-KVAVIS = INLA-ART-KVAVIS * -1                        
476200     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
476300     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
476400     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
476500     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
476600     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
476700     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
476800     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
476900     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
477000     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
477100     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
477200     MOVE +0                          TO EKO-EKH-IDDISTR                  
477300                                         EKO-EKH-IDKUNDNR                 
477400*******************************                                           
477500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
477600     MOVE ZERO TO NOLL-RAKNARE                                            
477700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
477800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
477900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
478000          FOR LEADING ZERO                                                
478100     ADD +1 TO NOLL-RAKNARE                                               
478200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
478300          WITH POINTER NOLL-RAKNARE                                       
478400*******************************                                           
478500     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
478600     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
478700                                         EKO-EKH-PRARTNTO                 
478800                                         EKO-EKH-PRARTSJK                 
478900                                         EKO-EKH-PRLANDCO                 
479000                                         EKO-EKH-SUBEL                    
479100     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
479200     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
479300                                                                          
479400     MOVE 1.00                        TO EKO-EKH-PRKURS                   
479500                                                                          
479600     MOVE ZERO                        TO EKO-EKH-PRINK                    
479700                                                                          
479800     IF DCS-NDC-CN                                                        
479900     OR DCS-USA                                                           
480000       PERFORM S07-GET-PRARTBEL-BACKNING                                  
480100     ELSE                                                                 
480200       PERFORM S07-GET-PRARTBEL-BACK-WDK621                               
480300     END-IF                                                               
480400     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
480500       MOVE WS-ARTC-PRARTBEL-PR       TO EKO-EKH-PRARTSTD                 
480600       MOVE WS-ARTC-KDVALISO          TO EKO-EKH-KDVALISO                 
480700     ELSE                                                                 
480800       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
480900       MOVE DCS-KDVALISO              TO EKO-EKH-KDVALISO                 
481000**** TAIWAN NEEDS A INTEGER VALUE AS TAIWAN DOSENT USE                    
481100**** DECIMALS. 0.1 MAKES THE BOOKING TO ZERO WHICH IS NOT                 
481200**** VALID IN SAP SYSTEM.                                                 
481300       IF DCS-TAIWAN                                                      
481400         MOVE 1.0                     TO EKO-EKH-PRARTSTD                 
481500       END-IF                                                             
481600     END-IF                                                               
481700     PERFORM DDAB-GET-CURRENCY-RATE                                       
481800*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
481900*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
482000     IF XDC-NON-VCC-OWNED OR DCS-LAND-NON-VCC-OWNED                       
482100     AND NOT (NDC-CN OR NDC-US)                                           
482200       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
482300     END-IF                                                               
482400***                                                                       
482500     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
482600       MOVE WS-ARTC-PRARTBEL-PR       TO EKO-EKH-PRARTSTD                 
482700       MOVE WS-ARTC-KDVALISO          TO EKO-EKH-KDVALISO                 
482800     ELSE                                                                 
482900       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
483000       MOVE DCS-KDVALISO              TO EKO-EKH-KDVALISO                 
483100**** TAIWAN NEEDS A INTEGER VALUE AS TAIWAN DOSENT USE                    
483200**** DECIMALS. 0.1 MAKES THE BOOKING TO ZERO WHICH IS NOT                 
483300**** VALID IN SAP SYSTEM.                                                 
483400       IF DCS-TAIWAN                                                      
483500         MOVE 1.0                     TO EKO-EKH-PRARTSTD                 
483600       END-IF                                                             
483700     END-IF                                                               
483800     MOVE EKO-EKH-KVAVIS              TO WS-SPAR-KVAVIS                   
483900     MOVE WS-SPAR-KVAVIS              TO EKO-EKH-KVANTAL                  
484000     MOVE EKO-EKH-PRARTSTD            TO WS-SUARTSTD                      
484100     COMPUTE WS-SUARTSTD = WS-ARTC-PRARTBEL-SUM *                         
484200                           EKO-EKH-KVANTAL                                
484300                                                                          
484400     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
484500     MOVE SPACE                       TO EKO-EKH-BEVAT                    
484600                                         EKO-EKH-KDANMORS                 
484700                                         EKO-EKH-IDKST                    
484800     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
484900                                         EKO-EKH-KDFRAKT                  
485000                                         EKO-EKH-SUVAT                    
485100                                                                          
485200                                                                          
485300     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
485400     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
485500     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
485600     IF EKO-EKH-KVANTAL > ZERO                                            
485700       MOVE 1                        TO EKO-EKH-KDAVVTYP                  
485800     ELSE                                                                 
485900       MOVE 0                        TO EKO-EKH-KDAVVTYP                  
486000     END-IF                                                               
486100     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
486200     MOVE WS-INLE-MOT-KVANTMOT       TO EKO-EKH-KVANTMOT                  
486300     MOVE INLA-ART-KVAVIS            TO EKO-EKH-KVAVIS                    
486400     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
486500     MOVE SPACE                      TO EKO-EKH-FLDCET                    
486600     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
486700                                                                          
486800     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
486900                                        EKO-EKH-PRHEMTAG                  
487000                                        EKO-EKH-PRDMTRL                   
487100                                        EKO-EKH-PROVRPAL                  
487200     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
487300     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
487400     EVALUATE TRUE                                                        
487500       WHEN NDC-CN                                                        
487600         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
487700       WHEN NDC-IN                                                        
487800         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
487900       WHEN NDC-US                                                        
488000         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
488100       WHEN OTHER                                                         
488200         IF NDC-KR                                                        
488210         OR NDC-MX                                                        
488220         OR NDC-BR                                                        
488230         OR NDC-ZA                                                        
488300           MOVE '103'                TO EKO-EKH-KDEKHHT                   
488400           MOVE '106'                TO EKO-EKH-KDEKSHT                   
488500         END-IF                                                           
488600         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
488700     END-EVALUATE                                                         
488800         MOVE 'CAN'                  TO EKO-EKH-CMD                       
488900     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
489000                                                                          
489100     PERFORM IMS-ISRT-EKOTRANS                                            
489200     PERFORM UNTIL SEGMENT-FINNS                                          
489300       ADD +1                     TO EKO-FIL-IDSEKVNR                     
489400       PERFORM IMS-ISRT-EKOTRANS                                          
489500     END-PERFORM                                                          
489600     .                                                                    
489700     EJECT                                                                
489800                                                                          
489900 JLA-LOGG-EKO-R31-VCCN-HEMT SECTION.                                      
490000     COMPUTE EKO-EKH-KVAVIS = INLA-ART-KVAVIS * -1                        
490100     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
490200     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
490300     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
490400     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
490500     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
490600     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
490700     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
490800     MOVE 'HEMT '                     TO EKO-EKH-KDEKNIVA                 
490900     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
491000     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
491100     MOVE +0                          TO EKO-EKH-IDDISTR                  
491200                                         EKO-EKH-IDKUNDNR                 
491300*******************************                                           
491400*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
491500     MOVE ZERO TO NOLL-RAKNARE                                            
491600     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
491700     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
491800     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
491900          FOR LEADING ZERO                                                
492000     ADD +1 TO NOLL-RAKNARE                                               
492100     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
492200          WITH POINTER NOLL-RAKNARE                                       
492300*******************************                                           
492400     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
492500     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
492600                                         EKO-EKH-PRARTNTO                 
492700                                         EKO-EKH-PRARTSJK                 
492800                                         EKO-EKH-PRLANDCO                 
492900                                         EKO-EKH-SUBEL                    
493000     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
493100     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
493200                                                                          
493300     MOVE 1.00                        TO EKO-EKH-PRKURS                   
493400                                                                          
493500     MOVE ZERO                        TO EKO-EKH-PRINK                    
493600                                                                          
493700     IF DCS-NDC-CN                                                        
493800     OR DCS-USA                                                           
493900       PERFORM S07-GET-PRARTBEL-BACKNING                                  
494000     ELSE                                                                 
494100       PERFORM S07-GET-PRARTBEL-BACK-WDK621                               
494200     END-IF                                                               
494300     PERFORM DDAB-GET-CURRENCY-RATE                                       
494400*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
494500*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
494600     IF XDC-NON-VCC-OWNED OR DCS-LAND-NON-VCC-OWNED                       
494700     AND NOT (NDC-CN OR NDC-US)                                           
494800       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
494900     END-IF                                                               
495000***                                                                       
495100     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
495200       MOVE WS-ARTC-PRARTBEL-PR       TO EKO-EKH-PRARTSTD                 
495300       MOVE WS-ARTC-KDVALISO          TO EKO-EKH-KDVALISO                 
495400       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
495500     ELSE                                                                 
495600       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
495700       MOVE DCS-KDVALISO              TO EKO-EKH-KDVALISO                 
495800**** TAIWAN NEEDS A INTEGER VALUE AS TAIWAN DOSENT USE                    
495900**** DECIMALS. 0.1 MAKES THE BOOKING TO ZERO WHICH IS NOT                 
496000**** VALID IN SAP SYSTEM.                                                 
496100       IF DCS-TAIWAN                                                      
496200         MOVE 1.0                     TO EKO-EKH-PRARTSTD                 
496300       END-IF                                                             
496400     END-IF                                                               
496500******                                                                    
496600     MOVE INLA-INL-IDLEVNR      TO W-IDLEVNR                              
496700     PERFORM IMS-GU-WDF101                                                
496800     IF SEGMENT-SAKNAS                                                    
496900       MOVE ZERO TO W-RETULF                                              
497000     ELSE                                                                 
497100       MOVE WS-IDDC          TO W-IDDC-B6                                 
497200       PERFORM IMS-GU-WDB601                                              
497300       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
497400       PERFORM IMS-GNP-WDF102                                             
497500       IF SEGMENT-FINNS                                                   
497600         IF F102-TULL-TITULF < DAGENS-DATUM                               
497700           MOVE F102-TULL-RETULF-1  TO W-RETULF                           
497800         ELSE                                                             
497900           MOVE F102-TULL-RETULF-2  TO W-RETULF                           
498000         END-IF                                                           
498100       END-IF                                                             
498200     END-IF                                                               
498300******                                                                    
498400     COMPUTE EKO-EKH-PRHEMTAG ROUNDED = (W-RETULF - 1) *                  
498500                EKO-EKH-PRARTSTD * EKO-EKH-KVAVIS                         
498600     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
498700     IF EKO-EKH-PRHEMTAG > 0                                              
498800       COMPUTE WS-SUHEMT = EKO-EKH-PRHEMTAG * -1                          
498900     ELSE                                                                 
499000       MOVE EKO-EKH-PRHEMTAG          TO WS-SUHEMT                        
499100     END-IF                                                               
499200     MOVE WS-SUHEMT                   TO EKO-EKH-SUBEL                    
499300     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
499400     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
499500     MOVE SPACE                       TO EKO-EKH-BEVAT                    
499600                                         EKO-EKH-KDANMORS                 
499700                                         EKO-EKH-IDKST                    
499800     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
499900                                         EKO-EKH-KDFRAKT                  
500000                                         EKO-EKH-SUVAT                    
500100                                                                          
500200                                                                          
500300     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
500400     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
500500     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
500600     MOVE 0                          TO EKO-EKH-KDAVVTYP                  
500700     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
500800     MOVE ZERO                       TO EKO-EKH-KVANTMOT                  
500900     MOVE ZERO                       TO EKO-EKH-KVAVIS                    
501000     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
501100     MOVE SPACE                      TO EKO-EKH-FLDCET                    
501200     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
501300                                                                          
501400     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
501500                                        EKO-EKH-PRDMTRL                   
501600                                        EKO-EKH-PROVRPAL                  
501700     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
501800                                                                          
501900     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
502000     EVALUATE TRUE                                                        
502100       WHEN NDC-CN                                                        
502200         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
502300       WHEN NDC-IN                                                        
502400         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
502500       WHEN NDC-US                                                        
502600         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
502700       WHEN OTHER                                                         
502800         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
502900     END-EVALUATE                                                         
503000     MOVE 'CAN'                      TO EKO-EKH-CMD                       
503100     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
503200     IF EKO-EKH-PRHEMTAG > ZERO                                           
503300     OR EKO-EKH-PRHEMTAG < ZERO                                           
503400       PERFORM IMS-ISRT-EKOTRANS                                          
503500       PERFORM UNTIL SEGMENT-FINNS                                        
503600         ADD +1                     TO EKO-FIL-IDSEKVNR                   
503700         PERFORM IMS-ISRT-EKOTRANS                                        
503800       END-PERFORM                                                        
503900     END-IF                                                               
504000     .                                                                    
504100     EJECT                                                                
504200                                                                          
504300 JLA-LOGG-EKO-R31-VCCN-KALK SECTION.                                      
504400     COMPUTE EKO-EKH-KVAVIS = INLA-ART-KVAVIS * -1                        
504500     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
504600     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
504700     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
504800     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
504900     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
505000     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
505100     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
505200     MOVE 'KALK '                     TO EKO-EKH-KDEKNIVA                 
505300     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
505400     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
505500     MOVE +0                          TO EKO-EKH-IDDISTR                  
505600                                         EKO-EKH-IDKUNDNR                 
505700*******************************                                           
505800*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
505900     MOVE ZERO TO NOLL-RAKNARE                                            
506000     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
506100     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
506200     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
506300          FOR LEADING ZERO                                                
506400     ADD +1 TO NOLL-RAKNARE                                               
506500     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
506600          WITH POINTER NOLL-RAKNARE                                       
506700*******************************                                           
506800     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
506900     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
507000                                         EKO-EKH-PRARTNTO                 
507100                                         EKO-EKH-PRARTSJK                 
507200                                         EKO-EKH-PRLANDCO                 
507300                                         EKO-EKH-SUBEL                    
507400     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
507500     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
507600                                                                          
507700     MOVE 1.00                        TO EKO-EKH-PRKURS                   
507800                                                                          
507900     MOVE ZERO                        TO EKO-EKH-PRINK                    
508000                                                                          
508100     MOVE DAGENS-DATUM(3:2) TO W-DATE-AAMM(1:2)                           
508200     MOVE 01                TO W-DATE-AAMM(3:2)                           
508300     MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                          
508400     MOVE DCS-KDVALISO      TO CURR-KDVALISO-ROW                          
508500     MOVE W-DATE-AAMM       TO CURR-TIAAMM                                
508600     MOVE 'A'               TO CURR-KDVALTYP                              
508700     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
508800     IF CURR-KDSVAR = ' '                                                 
508900       MOVE CURR-PRKURS-NEW  TO WS-PRKURS                                 
509000       MOVE CURR-REVALUTA-TO TO WS-REVALUTA                               
509100     ELSE                                                                 
509200       MOVE 1                TO WS-PRKURS                                 
509300       MOVE 1                TO WS-REVALUTA                               
509400     END-IF                                                               
509500     COMPUTE EKO-EKH-PRDIRLON ROUNDED = WS-ARTC-PRDIRLON *                
509600      PROC-REDIRLON /  WS-PRKURS / WS-REVALUTA * EKO-EKH-KVAVIS           
509700     COMPUTE EKO-EKH-PRDMTRL  ROUNDED = WS-ARTC-PRDMTRL  *                
509800      PROC-REDMTRL  /  WS-PRKURS / WS-REVALUTA * EKO-EKH-KVAVIS           
509900     MOVE ZERO                        TO EKO-EKH-PROVRPAL                 
510000     MOVE EKO-EKH-PRDMTRL             TO WS-SUDIRMTRL                     
510100     MOVE EKO-EKH-PRDIRLON            TO WS-SUDIRLON                      
510200     COMPUTE EKO-EKH-SUBEL = EKO-EKH-PRDIRLON +                           
510300                             EKO-EKH-PRDMTRL                              
510400                                                                          
510500     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
510600     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
510700     MOVE SPACE                       TO EKO-EKH-BEVAT                    
510800                                         EKO-EKH-KDANMORS                 
510900                                         EKO-EKH-IDKST                    
511000     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
511100                                         EKO-EKH-KDFRAKT                  
511200                                         EKO-EKH-SUVAT                    
511300                                                                          
511400                                                                          
511500     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
511600     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
511700     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
511800     MOVE 0                          TO EKO-EKH-KDAVVTYP                  
511900     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
512000     MOVE ZERO                       TO EKO-EKH-KVANTMOT                  
512100     MOVE ZERO                       TO EKO-EKH-KVAVIS                    
512200     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
512300     MOVE SPACE                      TO EKO-EKH-FLDCET                    
512400     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
512500                                                                          
512600     MOVE ZERO                       TO EKO-EKH-PRHEMTAG                  
512700                                        EKO-EKH-PROVRPAL                  
512800     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
512900     MOVE WS-ARTC-KDVALISO           TO EKO-EKH-KDVALISO                  
513000     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
513100     EVALUATE TRUE                                                        
513200       WHEN NDC-CN                                                        
513300         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
513400       WHEN NDC-IN                                                        
513500         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
513600       WHEN NDC-US                                                        
513700         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
513800       WHEN OTHER                                                         
513900         IF NDC-KR                                                        
513910         OR NDC-MX                                                        
513920         OR NDC-BR                                                        
513930         OR NDC-ZA                                                        
514000           MOVE '103'                TO EKO-EKH-KDEKHHT                   
514100           MOVE '106'                TO EKO-EKH-KDEKSHT                   
514200         END-IF                                                           
514300         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
514400     END-EVALUATE                                                         
514500     MOVE 'CAN'                      TO EKO-EKH-CMD                       
514600     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
514700                                                                          
514800     IF EKO-EKH-SUBEL > ZERO                                              
514900     OR EKO-EKH-SUBEL < ZERO                                              
515000       PERFORM IMS-ISRT-EKOTRANS                                          
515100                                                                          
515200       PERFORM UNTIL SEGMENT-FINNS                                        
515300         ADD +1                     TO EKO-FIL-IDSEKVNR                   
515400         PERFORM IMS-ISRT-EKOTRANS                                        
515500       END-PERFORM                                                        
515600     END-IF                                                               
515700     .                                                                    
515800     EJECT                                                                
515900                                                                          
516000 JLA-LOGG-EKO-R31-VCCN-SUM SECTION.                                       
516100     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
516200     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
516300     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
516400     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
516500     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
516600     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
516700     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
516800     MOVE 'SUM  '                     TO EKO-EKH-KDEKNIVA                 
516900     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
517000     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
517100     MOVE +0                          TO EKO-EKH-IDDISTR                  
517200                                         EKO-EKH-IDKUNDNR                 
517300*******************************                                           
517400*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
517500     MOVE ZERO TO NOLL-RAKNARE                                            
517600     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
517700     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
517800     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
517900          FOR LEADING ZERO                                                
518000     ADD +1 TO NOLL-RAKNARE                                               
518100     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
518200          WITH POINTER NOLL-RAKNARE                                       
518300*******************************                                           
518400     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
518500     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
518600                                         EKO-EKH-PRARTNTO                 
518700                                         EKO-EKH-PRARTSJK                 
518800                                         EKO-EKH-PRLANDCO                 
518900                                         EKO-EKH-SUBEL                    
519000     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
519100     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
519200                                                                          
519300     MOVE 1.00                        TO EKO-EKH-PRKURS                   
519400                                                                          
519500     MOVE ZERO                        TO EKO-EKH-PRINK                    
519600                                                                          
519700     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
519800                                                                          
519900     COMPUTE EKO-EKH-SUBEL = WS-SUARTSTD                                  
520000                                                                          
520100     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
520200     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
520300     MOVE SPACE                       TO EKO-EKH-BEVAT                    
520400                                         EKO-EKH-KDANMORS                 
520500                                         EKO-EKH-IDKST                    
520600     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
520700                                         EKO-EKH-KDFRAKT                  
520800                                         EKO-EKH-SUVAT                    
520900                                                                          
521000                                                                          
521100     MOVE WS-INLA-TIAVIDAT           TO EKO-EKH-DAAVIDAT                  
521200     MOVE WS-IDAVINR                 TO EKO-EKH-IDAVINR                   
521300     MOVE INLA-INL-IDLEVNR           TO EKO-EKH-IDLEVNR                   
521400     MOVE 0                          TO EKO-EKH-KDAVVTYP                  
521500     MOVE INLA-ART-KDRT              TO EKO-EKH-KDRT                      
521600     MOVE ZERO                       TO EKO-EKH-KVANTMOT                  
521700     MOVE ZERO                       TO EKO-EKH-KVAVIS                    
521800     MOVE WS-ARTC-KDSORT             TO EKO-EKH-KDSORT                    
521900     MOVE SPACE                      TO EKO-EKH-FLDCET                    
522000     MOVE INLC-INL-IDKUNDRF          TO EKO-EKH-IDKUNDRF                  
522100                                                                          
522200     MOVE ZERO                       TO EKO-EKH-PRDIRLON                  
522300                                        EKO-EKH-PRDMTRL                   
522400                                        EKO-EKH-PROVRPAL                  
522500     MOVE INLA-INL-IDANALYS          TO EKO-EKH-IDANALYS                  
522600     MOVE WS-ARTC-KDVALISO           TO EKO-EKH-KDVALISO                  
522700     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
522800     EVALUATE TRUE                                                        
522900       WHEN NDC-CN                                                        
523000         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
523100       WHEN NDC-IN                                                        
523200         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
523300       WHEN NDC-US                                                        
523400         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
523500       WHEN OTHER                                                         
523600         IF NDC-KR                                                        
523610         OR NDC-MX                                                        
523620         OR NDC-BR                                                        
523630         OR NDC-ZA                                                        
523700           MOVE '103'                TO EKO-EKH-KDEKHHT                   
523800           MOVE '106'                TO EKO-EKH-KDEKSHT                   
523900         END-IF                                                           
524000         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
524100     END-EVALUATE                                                         
524200     MOVE 'CAN'                      TO EKO-EKH-CMD                       
524300     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
524400                                                                          
524500     IF EKO-EKH-SUBEL > ZERO                                              
524600     OR EKO-EKH-SUBEL < ZERO                                              
524700       PERFORM IMS-ISRT-EKOTRANS                                          
524800       PERFORM UNTIL SEGMENT-FINNS                                        
524900         ADD +1                     TO EKO-FIL-IDSEKVNR                   
525000         PERFORM IMS-ISRT-EKOTRANS                                        
525100       END-PERFORM                                                        
525200     END-IF                                                               
525300     .                                                                    
525400     EJECT                                                                
525500                                                                          
525600 JLB-LOGG-EKO-R31-VCCS SECTION.                                           
525700     MOVE SPACE                      TO EKO-W51080                        
525800     MOVE SPACE                      TO WS-SAP-MM-POST                    
525900                                                                          
526000     MOVE INLA-ART-IDARTNR           TO EKO-IDARTNR                       
526100     MOVE INLA-ART-IDDC              TO EKO-IDDC                          
526200     MOVE INLA-ART-KDRT              TO EKO-KDRT                          
526300     COMPUTE EKO-KVAVIS = INLA-ART-KVAVIS * -1                            
526400     MOVE EKO-KVAVIS TO WS-SPAR-KVAVIS                                    
526500     MOVE WS-ARTC-PRINK              TO EKO-PRINK                         
526600     MOVE WS-ARTC-PRHEMTAG           TO EKO-PRHEMTAG                      
526700     IF  WS-ARTC-PRARTBES-PR   > ZERO                                     
526800       MOVE WS-ARTC-PRARTBES-PR      TO EKO-PRARTBES                      
526900     ELSE                                                                 
527000       MOVE 0.1                      TO EKO-PRARTBES                      
527100     END-IF                                                               
527200     MOVE INLA-INL-IDKONTO           TO EKO-IDKONTO                       
527300     MOVE INLA-ART-IDLOPNRM          TO EKO-IDLOPNRM                      
527400     MOVE INLA-INL-IDFS              TO EKO-IDFS                          
527500     MOVE INLA-INL-IDLEVNR           TO EKO-IDLEVNR                       
527600     MOVE WS-ARTC-KDTIPPR            TO EKO-KDTIPPR                       
527700     MOVE INLA-INL-TIAVIDAT          TO EKO-TIAVIDAT                      
527800     IF  WS-ARTC-PRARTBEL-PR   > ZERO                                     
527900       MOVE WS-ARTC-PRARTBEL-PR      TO EKO-PRARTBEL-PR                   
528000       MOVE WS-ARTC-KDVALISO         TO EKO-KDVALISO                      
528100     ELSE                                                                 
528200       MOVE 0.1                      TO EKO-PRARTBEL-PR                   
528300       MOVE 'XXX'                    TO EKO-KDVALISO                      
528400     END-IF                                                               
528500     IF INLA-INL-IDLEVNR = '1441'                                         
528600       MOVE NEJ                      TO WS-SAP-MM-POST                    
528700     END-IF                                                               
528800     MOVE ZERO                       TO EKO-RETULF                        
528900                                                                          
529000     MOVE WS-ARTC-KDPRODSL           TO EKO-KDPRODSL                      
529100     MOVE WS-ARTC-IDINK-X            TO EKO-IDINK                         
529200     MOVE WS-ARTC-KDSORT             TO EKO-KDSORT                        
529300     MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                      
529400     MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                       
529500     MOVE JA                         TO EKO-FLLSBOK                       
529600     MOVE ZERO                       TO EKO-IDDISTR                       
529700     MOVE NEJ                        TO EKO-FLDIRLEV                      
529800     MOVE WS-ARTC23-IDAVTAL          TO EKO-IDAVTAL                       
529900     MOVE 57                         TO EKO-IDFTG                         
530000                                                                          
530100     MOVE JA                         TO EKO-FLAVVINL                      
530200     MOVE 'V'                        TO EKO-KDINLAVV                      
530300                                                                          
530400     MOVE 'W6011910'                 TO EKO-FIL-IDPGM                     
530500     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
530600     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
530700     MOVE 1                          TO EKO-FIL-IDSEKVNR                  
530800     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
530900     MOVE '80 '                      TO EKO-FIL-CT-IDPTYP                 
531000     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
531100                                                                          
531200     IF WS-SAP-MM-POST = NEJ                                              
531300       CONTINUE                                                           
531400     ELSE                                                                 
531500       PERFORM IMS-ISRT-EKOTRANS                                          
531600                                                                          
531700       PERFORM UNTIL SEGMENT-FINNS                                        
531800         ADD +1 TO EKO-FIL-IDSEKVNR                                       
531900         PERFORM IMS-ISRT-EKOTRANS                                        
532000       END-PERFORM                                                        
532100     END-IF                                                               
532200                                                                          
532300     MOVE 'W6011910'                  TO FIL-IDPGM                        
532400     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
532500                                         EKH-DAVERDAT                     
532600     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
532700     MOVE +1                          TO FIL-IDSEKVNR                     
532800     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
532900     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
533000     MOVE '103'                       TO EKH-KDEKHHT                      
533100     MOVE '102'                       TO EKH-KDEKSHT                      
533200     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
533300     MOVE INLA-ART-IDDC               TO EKH-IDDC-SEND                    
533400     MOVE SPACE                       TO EKH-IDDC-REC                     
533500     MOVE +0                          TO EKH-IDDISTR                      
533600                                         EKH-IDKUNDNR                     
533700*******************************                                           
533800*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
533900     MOVE ZERO TO NOLL-RAKNARE                                            
534000     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
534100     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
534200     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
534300          FOR LEADING ZERO                                                
534400     ADD +1 TO NOLL-RAKNARE                                               
534500     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
534600          WITH POINTER NOLL-RAKNARE                                       
534700*******************************                                           
534800     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
534900     MOVE ZERO                        TO EKH-KDPSLLOC                     
535000                                         EKH-PRARTNTO                     
535100                                         EKH-PRARTSJK                     
535200                                         EKH-PRLANDCO                     
535300                                         EKH-SUBEL                        
535400     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
535500     MOVE SPACE                       TO EKH-FLLSBOK                      
535600                                                                          
535700     IF EKO-KDVALISO = 'XXX'                                              
535800       MOVE 'SEK'                     TO EKH-KDVALISO                     
535900     ELSE                                                                 
536000       MOVE EKO-KDVALISO              TO EKH-KDVALISO                     
536100     END-IF                                                               
536200     MOVE 1.00                        TO EKH-PRKURS                       
536300                                                                          
536400     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
536500     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
536600     IF INLA-INL-IDLEVNR = '1002 '                                        
536700       MOVE ZERO                      TO EKH-PRHEMTAG                     
536800     ELSE                                                                 
536900       MOVE WS-ARTC-PRHEMTAG          TO EKH-PRHEMTAG                     
537000     END-IF                                                               
537100     MOVE WS-SPAR-KVAVIS              TO EKH-KVANTAL                      
537200     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
537300     MOVE SPACE                       TO EKH-BEVAT                        
537400                                         EKH-KDANMORS                     
537500                                         EKH-IDKST                        
537600     MOVE ZERO                        TO EKH-IDKONTO                      
537700                                         EKH-KDFRAKT                      
537800                                         EKH-SUVAT                        
537900                                                                          
538000                                                                          
538100     MOVE WS-INLA-TIAVIDAT           TO EKH-DAAVIDAT                      
538200     MOVE WS-IDAVINR                 TO EKH-IDAVINR                       
538300     MOVE INLA-INL-IDLEVNR           TO EKH-IDLEVNR                       
538400     IF EKH-KVANTAL > ZERO                                                
538500       MOVE 1                        TO EKH-KDAVVTYP                      
538600     ELSE                                                                 
538700       MOVE 0                        TO EKH-KDAVVTYP                      
538800     END-IF                                                               
538900     MOVE INLA-ART-KDRT              TO EKH-KDRT                          
539000     MOVE WS-INLE-MOT-KVANTMOT       TO EKH-KVANTMOT                      
539100     MOVE INLA-ART-KVAVIS            TO EKH-KVAVIS                        
539200     MOVE WS-ARTC-KDSORT             TO EKH-KDSORT                        
539300     MOVE 'SEPV'                     TO EKH-KDTRADP                       
539400     MOVE SPACE                      TO EKH-FLDCET                        
539500     MOVE SPACE                      TO EKH-IDKUNDRF                      
539600     MOVE SPACE                      TO EKH-IDFAKT-EXP                    
539700                                                                          
539800     MOVE ZERO                       TO EKH-PRDIRLON                      
539900                                        EKH-PRDMTRL                       
540000                                        EKH-PROVRPAL                      
540100     MOVE INLA-INL-IDANALYS          TO EKH-IDANALYS                      
540200                                                                          
540300     IF EKH-KVANTAL NOT = +0                                              
540400       PERFORM IMS-ISRT-WLSAPA01                                          
540500                                                                          
540600       PERFORM UNTIL SEGMENT-FINNS                                        
540700         ADD +1                     TO FIL-IDSEKVNR                       
540800         PERFORM IMS-ISRT-WLSAPA01                                        
540900       END-PERFORM                                                        
541000     END-IF                                                               
541100     .                                                                    
541200     EJECT                                                                
541300                                                                          
541400 JM-LOGG-EKO-R31-WDR9 SECTION.                                            
541500     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
541600       PERFORM JMA-LOGG-EKO-R31-VCCN                                      
541700     ELSE                                                                 
541800       PERFORM JMB-LOGG-EKO-R31-VCCS                                      
541900     END-IF                                                               
542000     .                                                                    
542100     EJECT                                                                
542200                                                                          
542300 JMA-LOGG-EKO-R31-VCCN SECTION.                                           
542400     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
542500     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
542600     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
542700     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
542800     MOVE 1                           TO EKO-FIL-IDSEKVNR                 
542900     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
543000     MOVE '105'                       TO EKO-EKH-KDEKSHT                  
543100     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
543200     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
543300     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
543400     MOVE +0                          TO EKO-EKH-IDDISTR                  
543500                                         EKO-EKH-IDKUNDNR                 
543600*******************************                                           
543700*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
543800     MOVE ZERO TO NOLL-RAKNARE                                            
543900     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
544000     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
544100     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
544200          FOR LEADING ZERO                                                
544300     ADD +1 TO NOLL-RAKNARE                                               
544400     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
544500          WITH POINTER NOLL-RAKNARE                                       
544600*******************************                                           
544700     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
544800     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
544900                                         EKO-EKH-PRARTNTO                 
545000                                         EKO-EKH-PRARTSJK                 
545100                                         EKO-EKH-PRHEMTAG                 
545200                                         EKO-EKH-PRLANDCO                 
545300                                         EKO-EKH-PRDIRLON                 
545400                                         EKO-EKH-PRDMTRL                  
545500                                         EKO-EKH-PROVRPAL                 
545600                                         EKO-EKH-SUBEL                    
545700     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
545800     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
545900                                                                          
546000     MOVE 1.00                        TO EKO-EKH-PRKURS                   
546100                                                                          
546200     MOVE WS-ARTC-PRINK               TO EKO-EKH-PRINK                    
546300     MOVE ARTS-SLAG-PRAVCOST          TO EKO-EKH-PRARTSTD                 
546400     COMPUTE EKO-EKH-KVANTAL = INLA-ART-KVAVIS * -1                       
546500     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
546600     MOVE SPACE                       TO EKO-EKH-BEVAT                    
546700                                         EKO-EKH-KDANMORS                 
546800                                         EKO-EKH-IDANALYS                 
546900                                         EKO-EKH-IDKST                    
547000     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
547100                                         EKO-EKH-KDFRAKT                  
547200                                         EKO-EKH-SUVAT                    
547300                                                                          
547400     MOVE FUNCTION CURRENT-DATE(1:8)  TO EKO-EKH-DAAVIDAT                 
547500     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
547600     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
547700     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
547800     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
547900     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
548000     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
548100     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
548200     MOVE SPACE                       TO EKO-EKH-FLDCET                   
548300     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
548400     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
548500                                                                          
548600     MOVE DCS-KDVALISO               TO EKO-EKH-KDVALISO                  
548700     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
548800     EVALUATE TRUE                                                        
548900       WHEN NDC-CN                                                        
549000         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
549100       WHEN NDC-IN                                                        
549200         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
549300       WHEN NDC-US                                                        
549400         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
549500       WHEN OTHER                                                         
549600         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
549700     END-EVALUATE                                                         
549800     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
549900                                                                          
550000     PERFORM IMS-ISRT-EKOTRANS                                            
550100                                                                          
550200     PERFORM UNTIL SEGMENT-FINNS                                          
550300       ADD +1 TO EKO-FIL-IDSEKVNR                                         
550400       PERFORM IMS-ISRT-EKOTRANS                                          
550500     END-PERFORM                                                          
550600     .                                                                    
550700     EJECT                                                                
550800                                                                          
550900 JMB-LOGG-EKO-R31-VCCS SECTION.                                           
551000     MOVE 'W6011910'                  TO FIL-IDPGM                        
551100     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
551200                                         EKH-DAVERDAT                     
551300     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
551400     MOVE 1                           TO FIL-IDSEKVNR                     
551500     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
551600     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
551700     MOVE '102'                       TO EKH-KDEKHHT                      
551800     MOVE '105'                       TO EKH-KDEKSHT                      
551900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
552000     MOVE INLA-ART-IDDC               TO EKH-IDDC-SEND                    
552100     MOVE SPACE                       TO EKH-IDDC-REC                     
552200     MOVE +0                          TO EKH-IDDISTR                      
552300                                         EKH-IDKUNDNR                     
552400*******************************                                           
552500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
552600     MOVE ZERO TO NOLL-RAKNARE                                            
552700     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
552800     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
552900     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
553000          FOR LEADING ZERO                                                
553100     ADD +1 TO NOLL-RAKNARE                                               
553200     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
553300          WITH POINTER NOLL-RAKNARE                                       
553400*******************************                                           
553500     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
553600     MOVE ZERO                        TO EKH-KDPSLLOC                     
553700                                         EKH-PRARTNTO                     
553800                                         EKH-PRARTSJK                     
553900                                         EKH-PRHEMTAG                     
554000                                         EKH-PRLANDCO                     
554100                                         EKH-PRDIRLON                     
554200                                         EKH-PRDMTRL                      
554300                                         EKH-PROVRPAL                     
554400                                         EKH-SUBEL                        
554500     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
554600     MOVE SPACE                       TO EKH-FLLSBOK                      
554700                                                                          
554800     MOVE 'SEK'                       TO EKH-KDVALISO                     
554900     MOVE 1.00                        TO EKH-PRKURS                       
555000                                                                          
555100     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
555200     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
555300     COMPUTE EKH-KVANTAL = INLA-ART-KVAVIS * -1                           
555400     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
555500     MOVE SPACE                       TO EKH-BEVAT                        
555600                                         EKH-KDANMORS                     
555700                                         EKH-IDANALYS                     
555800                                         EKH-IDKST                        
555900     MOVE ZERO                        TO EKH-IDKONTO                      
556000                                         EKH-KDFRAKT                      
556100                                         EKH-SUVAT                        
556200                                                                          
556300     MOVE FUNCTION CURRENT-DATE(1:8)  TO EKH-DAAVIDAT                     
556400     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
556500     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
556600     MOVE ZERO                        TO EKH-KDAVVTYP                     
556700     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
556800     MOVE ZERO                        TO EKH-KVANTMOT                     
556900     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
557000     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
557100     MOVE 'SEPV'                      TO EKH-KDTRADP                      
557200     MOVE SPACE                       TO EKH-FLDCET                       
557300     MOVE SPACE                       TO EKH-IDKUNDRF                     
557400     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
557500                                                                          
557600     PERFORM IMS-ISRT-WLSAPA01                                            
557700                                                                          
557800     PERFORM UNTIL SEGMENT-FINNS                                          
557900       ADD +1 TO FIL-IDSEKVNR                                             
558000       PERFORM IMS-ISRT-WLSAPA01                                          
558100     END-PERFORM                                                          
558200     .                                                                    
558300     EJECT                                                                
558400                                                                          
558500 JN-LOGG-EKO-R31-WDR9-RT6 SECTION.                                        
558600     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
558700       PERFORM JNA-LOGG-EKO-R31-VCCN                                      
558800     ELSE                                                                 
558900       PERFORM JNB-LOGG-EKO-R31-VCCS                                      
559000     END-IF                                                               
559100     .                                                                    
559200     EJECT                                                                
559300                                                                          
559400 JNA-LOGG-EKO-R31-VCCN SECTION.                                           
559500     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
559600     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
559700     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
559800     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
559900     MOVE 1                           TO EKO-FIL-IDSEKVNR                 
560000     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
560100     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
560200     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
560300     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
560400     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
560500     MOVE +0                          TO EKO-EKH-IDDISTR                  
560600                                         EKO-EKH-IDKUNDNR                 
560700*******************************                                           
560800*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
560900     MOVE ZERO TO NOLL-RAKNARE                                            
561000     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
561100     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
561200     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
561300          FOR LEADING ZERO                                                
561400     ADD +1 TO NOLL-RAKNARE                                               
561500     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
561600          WITH POINTER NOLL-RAKNARE                                       
561700*******************************                                           
561800     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
561900     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
562000                                         EKO-EKH-PRARTNTO                 
562100                                         EKO-EKH-PRARTSJK                 
562200                                         EKO-EKH-PRHEMTAG                 
562300                                         EKO-EKH-PRLANDCO                 
562400                                         EKO-EKH-PRDIRLON                 
562500                                         EKO-EKH-PRDMTRL                  
562600                                         EKO-EKH-PROVRPAL                 
562700                                         EKO-EKH-SUBEL                    
562800     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
562900     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
563000                                                                          
563100     MOVE 1.00                        TO EKO-EKH-PRKURS                   
563200                                                                          
563300     MOVE WS-ARTC-PRINK               TO EKO-EKH-PRINK                    
563400     MOVE ARTS-SLAG-PRAVCOST          TO EKO-EKH-PRARTSTD                 
563500     COMPUTE EKO-EKH-KVANTAL = INLA-ART-KVAVIS * -1                       
563600     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
563700     MOVE INLA-INL-IDANALYS           TO EKO-EKH-IDANALYS                 
563800     MOVE INLA-INL-IDKONTO            TO EKO-EKH-IDKONTO                  
563900     MOVE INLA-INL-IDKST              TO EKO-EKH-IDKST                    
564000     MOVE SPACE                       TO EKO-EKH-BEVAT                    
564100                                         EKO-EKH-KDANMORS                 
564200     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
564300                                         EKO-EKH-SUVAT                    
564400                                                                          
564500     MOVE FUNCTION CURRENT-DATE(1:8)  TO EKO-EKH-DAAVIDAT                 
564600     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
564700     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
564800     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
564900     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
565000     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
565100     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
565200     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
565300     MOVE SPACE                       TO EKO-EKH-FLDCET                   
565400     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
565500     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
565600     MOVE DCS-KDVALISO               TO EKO-EKH-KDVALISO                  
565700     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
565800     EVALUATE TRUE                                                        
565900       WHEN NDC-CN                                                        
566000         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
566100       WHEN NDC-IN                                                        
566200         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
566300       WHEN NDC-US                                                        
566400         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
566500       WHEN OTHER                                                         
566600         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
566700     END-EVALUATE                                                         
566800     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
566900                                                                          
567000                                                                          
567100     PERFORM IMS-ISRT-EKOTRANS                                            
567200                                                                          
567300     PERFORM UNTIL SEGMENT-FINNS                                          
567400       ADD +1 TO EKO-FIL-IDSEKVNR                                         
567500       PERFORM IMS-ISRT-EKOTRANS                                          
567600     END-PERFORM                                                          
567700     .                                                                    
567800     EJECT                                                                
567900                                                                          
568000 JNB-LOGG-EKO-R31-VCCS SECTION.                                           
568100     MOVE 'W6011910'                  TO FIL-IDPGM                        
568200     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
568300                                         EKH-DAVERDAT                     
568400     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
568500     MOVE 1                           TO FIL-IDSEKVNR                     
568600     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
568700     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
568800     MOVE '102'                       TO EKH-KDEKHHT                      
568900     MOVE '102'                       TO EKH-KDEKSHT                      
569000     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
569100     MOVE INLA-ART-IDDC               TO EKH-IDDC-SEND                    
569200     MOVE SPACE                       TO EKH-IDDC-REC                     
569300     MOVE +0                          TO EKH-IDDISTR                      
569400                                         EKH-IDKUNDNR                     
569500*******************************                                           
569600*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
569700     MOVE ZERO TO NOLL-RAKNARE                                            
569800     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
569900     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
570000     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
570100          FOR LEADING ZERO                                                
570200     ADD +1 TO NOLL-RAKNARE                                               
570300     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
570400          WITH POINTER NOLL-RAKNARE                                       
570500*******************************                                           
570600     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
570700     MOVE ZERO                        TO EKH-KDPSLLOC                     
570800                                         EKH-PRARTNTO                     
570900                                         EKH-PRARTSJK                     
571000                                         EKH-PRHEMTAG                     
571100                                         EKH-PRLANDCO                     
571200                                         EKH-PRDIRLON                     
571300                                         EKH-PRDMTRL                      
571400                                         EKH-PROVRPAL                     
571500                                         EKH-SUBEL                        
571600     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
571700     MOVE SPACE                       TO EKH-FLLSBOK                      
571800                                                                          
571900     MOVE 'SEK'                       TO EKH-KDVALISO                     
572000     MOVE 1.00                        TO EKH-PRKURS                       
572100                                                                          
572200     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
572300     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
572400     COMPUTE EKH-KVANTAL = INLA-ART-KVAVIS * -1                           
572500     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
572600     MOVE INLA-INL-IDANALYS           TO EKH-IDANALYS                     
572700     MOVE INLA-INL-IDKONTO            TO EKH-IDKONTO                      
572800     MOVE INLA-INL-IDKST              TO EKH-IDKST                        
572900     MOVE SPACE                       TO EKH-BEVAT                        
573000                                         EKH-KDANMORS                     
573100     MOVE ZERO                        TO EKH-KDFRAKT                      
573200                                         EKH-SUVAT                        
573300                                                                          
573400     MOVE FUNCTION CURRENT-DATE(1:8)  TO EKH-DAAVIDAT                     
573500     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
573600     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
573700     MOVE ZERO                        TO EKH-KDAVVTYP                     
573800     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
573900     MOVE ZERO                        TO EKH-KVANTMOT                     
574000     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
574100     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
574200     MOVE 'SEPV'                      TO EKH-KDTRADP                      
574300     MOVE SPACE                       TO EKH-FLDCET                       
574400     MOVE SPACE                       TO EKH-IDKUNDRF                     
574500     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
574600                                                                          
574700     PERFORM IMS-ISRT-WLSAPA01                                            
574800                                                                          
574900     PERFORM UNTIL SEGMENT-FINNS                                          
575000       ADD +1 TO FIL-IDSEKVNR                                             
575100       PERFORM IMS-ISRT-WLSAPA01                                          
575200     END-PERFORM                                                          
575300     .                                                                    
575400     EJECT                                                                
575500                                                                          
575600 JO-LOGG-EKO-R31-WDR9-PALAGG  SECTION.                                    
575700     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
575800       CONTINUE                                                           
575900     ELSE                                                                 
576000       PERFORM JOB-LOGG-EKO-R31-VCCS                                      
576100     END-IF                                                               
576200     .                                                                    
576300     EJECT                                                                
576400                                                                          
576500                                                                          
576600 JOB-LOGG-EKO-R31-VCCS SECTION.                                           
576700     MOVE 'W6011910'                  TO FIL-IDPGM                        
576800     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
576900                                         EKH-DAVERDAT                     
577000     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
577100     MOVE 1                           TO FIL-IDSEKVNR                     
577200     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
577300     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
577400     MOVE '103'                       TO EKH-KDEKHHT                      
577500     MOVE '101'                       TO EKH-KDEKSHT                      
577600     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
577700     MOVE INLA-ART-IDDC               TO EKH-IDDC-SEND                    
577800     MOVE SPACE                       TO EKH-IDDC-REC                     
577900     MOVE +0                          TO EKH-IDDISTR                      
578000                                         EKH-IDKUNDNR                     
578100*******************************                                           
578200*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
578300     MOVE ZERO TO NOLL-RAKNARE                                            
578400     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
578500     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
578600     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
578700          FOR LEADING ZERO                                                
578800     ADD +1 TO NOLL-RAKNARE                                               
578900     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
579000          WITH POINTER NOLL-RAKNARE                                       
579100*******************************                                           
579200     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
579300     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
579400     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
579500     MOVE WS-ARTC-PRDIRLON            TO EKH-PRDIRLON                     
579600     MOVE WS-ARTC-PRDMTRL             TO EKH-PRDMTRL                      
579700     MOVE WS-ARTC-PROVRPAL            TO EKH-PROVRPAL                     
579800     MOVE INLA-INL-IDANALYS           TO EKH-IDANALYS                     
579900     MOVE ZERO                        TO EKH-KDPSLLOC                     
580000                                         EKH-PRARTNTO                     
580100                                         EKH-PRARTSJK                     
580200                                         EKH-PRHEMTAG                     
580300                                         EKH-PRLANDCO                     
580400                                         EKH-SUBEL                        
580500     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
580600     MOVE SPACE                       TO EKH-FLLSBOK                      
580700                                                                          
580800     MOVE 'SEK'                       TO EKH-KDVALISO                     
580900     MOVE 1.00                        TO EKH-PRKURS                       
581000                                                                          
581100     COMPUTE EKH-KVANTAL = INLA-ART-KVAVIS * -1                           
581200     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
581300     MOVE SPACE                       TO EKH-BEVAT                        
581400                                         EKH-KDANMORS                     
581500                                         EKH-IDKST                        
581600     MOVE ZERO                        TO EKH-IDKONTO                      
581700                                         EKH-KDFRAKT                      
581800                                         EKH-SUVAT                        
581900                                                                          
582000     MOVE FUNCTION CURRENT-DATE (1:8) TO EKH-DAAVIDAT                     
582100     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
582200     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
582300     MOVE ZERO                        TO EKH-KDAVVTYP                     
582400     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
582500     MOVE ZERO                        TO EKH-KVANTMOT                     
582600     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
582700     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
582800     MOVE 'SEPV'                      TO EKH-KDTRADP                      
582900     MOVE SPACE                       TO EKH-FLDCET                       
583000     MOVE SPACE                       TO EKH-IDKUNDRF                     
583100     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
583200                                                                          
583300     IF EKH-PRDIRLON > ZERO                                               
583400     OR EKH-PRDMTRL  > ZERO                                               
583500       PERFORM IMS-ISRT-WLSAPA01                                          
583600                                                                          
583700       PERFORM UNTIL SEGMENT-FINNS                                        
583800         ADD +1 TO FIL-IDSEKVNR                                           
583900         PERFORM IMS-ISRT-WLSAPA01                                        
584000       END-PERFORM                                                        
584100     END-IF                                                               
584200     .                                                                    
584300     EJECT                                                                
584400                                                                          
584500 JP-LOGG-EKO-WDR9-RT6-AVV SECTION.                                        
584600     IF DCS-LAND-NON-VCC-OWNED OR DCS-USA                                 
584700       PERFORM JPA-LOGG-EKO-VCCN                                          
584800     ELSE                                                                 
584900       PERFORM JPB-LOGG-EKO-VCCS                                          
585000     END-IF                                                               
585100     .                                                                    
585200     EJECT                                                                
585300                                                                          
585400 JPA-LOGG-EKO-VCCN SECTION.                                               
585500     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
585600     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
585700     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
585800     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
585900     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
586000     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
586100     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
586200     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
586300     MOVE INLA-ART-IDDC               TO EKO-EKH-IDDC-SEND                
586400     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
586500     MOVE +0                          TO EKO-EKH-IDDISTR                  
586600                                         EKO-EKH-IDKUNDNR                 
586700                                                                          
586800*******************************                                           
586900*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
587000     MOVE ZERO TO NOLL-RAKNARE                                            
587100     MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                  
587200     MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM                
587300     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
587400          FOR LEADING ZERO                                                
587500     ADD +1 TO NOLL-RAKNARE                                               
587600     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                   
587700        WITH POINTER NOLL-RAKNARE                                         
587800*******************************                                           
587900     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
588000                                                                          
588100     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
588200                                         EKO-EKH-PRARTNTO                 
588300                                         EKO-EKH-PRARTSJK                 
588400                                         EKO-EKH-PRHEMTAG                 
588500                                         EKO-EKH-PRLANDCO                 
588600                                         EKO-EKH-PRDIRLON                 
588700                                         EKO-EKH-PRDMTRL                  
588800                                         EKO-EKH-PROVRPAL                 
588900                                         EKO-EKH-SUBEL                    
589000                                         EKO-EKH-IDORDNR5                 
589100     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
589200     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
589300                                                                          
589400     MOVE 1.00                        TO EKO-EKH-PRKURS                   
589500                                                                          
589600     MOVE WS-ARTC-PRINK               TO EKO-EKH-PRINK                    
589700     MOVE ARTS-SLAG-PRAVCOST          TO EKO-EKH-PRARTSTD                 
589800                                                                          
589900     COMPUTE EKO-EKH-KVANTAL =                                            
590000          (WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS) * -1                   
590100     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
590200     MOVE SPACE                       TO EKO-EKH-BEVAT                    
590300                                         EKO-EKH-KDANMORS                 
590400     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
590500                                         EKO-EKH-SUVAT                    
590600     MOVE INLA-INL-IDANALYS           TO EKO-EKH-IDANALYS                 
590700     MOVE INLA-INL-IDKONTO            TO EKO-EKH-IDKONTO                  
590800     MOVE SPACE                       TO EKO-EKH-IDKST                    
590900     MOVE INLA-INL-TIAVIDAT           TO WS-DAAVIDAT-YYMMDD               
591000     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
591100       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
591200     ELSE                                                                 
591300       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
591400     END-IF                                                               
591500     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
591600     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
591700     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
591800     IF INLA-ART-KVAVIS > ZERO                                            
591900       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
592000     ELSE                                                                 
592100       MOVE 0                         TO EKO-EKH-KDAVVTYP                 
592200     END-IF                                                               
592300     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
592400     MOVE WS-INLE-MOT-KVANTMOT        TO EKO-EKH-KVANTMOT                 
592500     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
592600     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
592700     MOVE SPACE                       TO EKO-EKH-FLDCET                   
592800     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
592900     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
593000     MOVE DCS-KDVALISO               TO EKO-EKH-KDVALISO                  
593100     MOVE DCS-KDTRADP                TO EKO-EKH-KDTRADP                   
593200     EVALUATE TRUE                                                        
593300       WHEN NDC-CN                                                        
593400         MOVE 'W570'                 TO EKO-FIL-IDCPYTXT(1:4)             
593500       WHEN NDC-IN                                                        
593600         MOVE 'W515'                 TO EKO-FIL-IDCPYTXT(1:4)             
593700       WHEN NDC-US                                                        
593800         MOVE 'W561'                 TO EKO-FIL-IDCPYTXT(1:4)             
593900       WHEN OTHER                                                         
594000         MOVE DCS-KDTRADP            TO EKO-FIL-IDCPYTXT(1:4)             
594100     END-EVALUATE                                                         
594200     MOVE 'EKHA'                     TO EKO-FIL-IDCPYTXT(5:4)             
594300                                                                          
594400     PERFORM IMS-ISRT-EKOTRANS                                            
594500                                                                          
594600     PERFORM UNTIL SEGMENT-FINNS                                          
594700       ADD +1 TO EKO-FIL-IDSEKVNR                                         
594800       PERFORM IMS-ISRT-EKOTRANS                                          
594900     END-PERFORM                                                          
595000     .                                                                    
595100     EJECT                                                                
595200                                                                          
595300 JPB-LOGG-EKO-VCCS SECTION.                                               
595400     MOVE 'W6011910'                  TO FIL-IDPGM                        
595500     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
595600                                         EKH-DAVERDAT                     
595700     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
595800     MOVE +1                          TO FIL-IDSEKVNR                     
595900     MOVE 'W510'                      TO FIL-CT-IDSYSTEM                  
596000     MOVE 'EKH'                       TO FIL-CT-IDPTYP                    
596100     MOVE 'A'                         TO FIL-CT-IDVTYP                    
596200     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
596300     MOVE '102'                       TO EKH-KDEKHHT                      
596400     MOVE '102'                       TO EKH-KDEKSHT                      
596500     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
596600     MOVE INLA-ART-IDDC               TO EKH-IDDC-SEND                    
596700     MOVE SPACE                       TO EKH-IDDC-REC                     
596800     MOVE +0                          TO EKH-IDDISTR                      
596900                                         EKH-IDKUNDNR                     
597000                                                                          
597100*******************************                                           
597200*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
597300     MOVE ZERO TO NOLL-RAKNARE                                            
597400     MOVE INLA-ART-IDLOPNRM           TO WS-SAP-IDLOPNRM                  
597500     MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM                
597600     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
597700          FOR LEADING ZERO                                                
597800     ADD +1 TO NOLL-RAKNARE                                               
597900     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
598000        WITH POINTER NOLL-RAKNARE                                         
598100*******************************                                           
598200     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
598300                                                                          
598400     MOVE ZERO                        TO EKH-KDPSLLOC                     
598500                                         EKH-PRARTNTO                     
598600                                         EKH-PRARTSJK                     
598700                                         EKH-PRHEMTAG                     
598800                                         EKH-PRLANDCO                     
598900                                         EKH-PRDIRLON                     
599000                                         EKH-PRDMTRL                      
599100                                         EKH-PROVRPAL                     
599200                                         EKH-SUBEL                        
599300                                         EKH-IDORDNR5                     
599400     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
599500     MOVE SPACE                       TO EKH-FLLSBOK                      
599600                                                                          
599700     MOVE 'SEK'                       TO EKH-KDVALISO                     
599800     MOVE 1.00                        TO EKH-PRKURS                       
599900                                                                          
600000     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
600100     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
600200                                                                          
600300     COMPUTE EKH-KVANTAL =                                                
600400          (WS-INLE-MOT-KVANTMOT - INLA-ART-KVAVIS) * -1                   
600500     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
600600     MOVE SPACE                       TO EKH-BEVAT                        
600700                                         EKH-KDANMORS                     
600800     MOVE ZERO                        TO EKH-KDFRAKT                      
600900                                         EKH-SUVAT                        
601000     MOVE INLA-INL-IDANALYS           TO EKH-IDANALYS                     
601100     MOVE INLA-INL-IDKONTO            TO EKH-IDKONTO                      
601200     MOVE SPACE                       TO EKH-IDKST                        
601300     MOVE INLA-INL-TIAVIDAT           TO WS-DAAVIDAT-YYMMDD               
601400     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
601500       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
601600     ELSE                                                                 
601700       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
601800     END-IF                                                               
601900     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
602000     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
602100     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
602200     IF INLA-ART-KVAVIS > ZERO                                            
602300       MOVE 1                         TO EKH-KDAVVTYP                     
602400     ELSE                                                                 
602500       MOVE 0                         TO EKH-KDAVVTYP                     
602600     END-IF                                                               
602700     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
602800     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
602900     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
603000     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
603100     MOVE 'SEPV'                      TO EKH-KDTRADP                      
603200     MOVE SPACE                       TO EKH-FLDCET                       
603300     MOVE SPACE                       TO EKH-IDKUNDRF                     
603400     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
603500                                                                          
603600     PERFORM IMS-ISRT-WLSAPA01                                            
603700                                                                          
603800     PERFORM UNTIL SEGMENT-FINNS                                          
603900       ADD +1 TO FIL-IDSEKVNR                                             
604000       PERFORM IMS-ISRT-WLSAPA01                                          
604100     END-PERFORM                                                          
604200     .                                                                    
604300     EJECT                                                                
604400 JQ-LOGG-EKO-WDR9 SECTION.                                                
604500                                                                          
604600     MOVE 'W6011910'                  TO FIL-IDPGM                        
604700     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
604800                                         EKH-DAVERDAT                     
604900     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
605000     MOVE +1                          TO FIL-IDSEKVNR                     
605100     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
605200     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
605300     MOVE '102'                       TO EKH-KDEKHHT                      
605400     MOVE '109'                       TO EKH-KDEKSHT                      
605500     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
605600     MOVE INLA-INL-IDDC               TO EKH-IDDC-SEND                    
605700     MOVE SPACE                       TO EKH-IDDC-REC                     
605800     MOVE +0                          TO EKH-IDDISTR                      
605900                                         EKH-IDKUNDNR                     
606000*******************************                                           
606100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
606200     MOVE ZERO TO NOLL-RAKNARE                                            
606300     MOVE INLA-ART-IDLOPNRM          TO WS-SAP-IDLOPNRM                   
606400     MOVE WS-SAP-IDLOPNRM            TO WS-SAP-X-IDLOPNRM                 
606500     INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                      
606600          FOR LEADING ZERO                                                
606700     ADD +1 TO NOLL-RAKNARE                                               
606800     UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                       
606900          WITH POINTER NOLL-RAKNARE                                       
607000*******************************                                           
607100     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
607200     MOVE ZERO                        TO EKH-KDPSLLOC                     
607300                                         EKH-PRARTNTO                     
607400                                         EKH-PRARTSJK                     
607500                                         EKH-PRLANDCO                     
607600                                         EKH-SUBEL                        
607700     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
607800     MOVE SPACE                       TO EKH-FLLSBOK                      
607900                                                                          
608000     IF FL-FORE-INST = JA                                                 
608100       COMPUTE EKH-KVANTAL = (WS-INLE-MOT-KVANTMOT -                      
608200                      INLA-ART-KVAVIS + WS-INLE-MOT-KVRETUR) * -1         
608300       END-COMPUTE                                                        
608400     ELSE                                                                 
608500       COMPUTE EKH-KVANTAL = (WS-INLE-MOT-KVANTMOT -                      
608600                         INLA-ART-KVAVIS) * -1                            
608700       END-COMPUTE                                                        
608800     END-IF                                                               
608900*    MOVE EKO-KVAVIS                  TO EKH-KVANTAL                      
609000                                                                          
609100     MOVE 'SEK'                       TO EKH-KDVALISO                     
609200     MOVE 1.00                        TO EKH-PRKURS                       
609300                                                                          
609400     MOVE WS-ARTC-PRINK               TO EKH-PRINK                        
609500     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
609600     IF INLA-INL-IDLEVNR = '1002 '                                        
609700       MOVE ZERO                      TO EKH-PRHEMTAG                     
609800     ELSE                                                                 
609900       MOVE WS-ARTC-PRHEMTAG          TO EKH-PRHEMTAG                     
610000     END-IF                                                               
610100*    MOVE EKO-KVAVIS                  TO EKH-KVANTAL                      
610200     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
610300     MOVE SPACE                       TO EKH-BEVAT                        
610400                                         EKH-KDANMORS                     
610500                                         EKH-IDKST                        
610600     MOVE ZERO                        TO EKH-IDKONTO                      
610700                                         EKH-KDFRAKT                      
610800                                         EKH-SUVAT                        
610900                                                                          
611000                                                                          
611100     MOVE WS-INLA-TIAVIDAT           TO EKH-DAAVIDAT                      
611200     MOVE WS-IDAVINR                 TO EKH-IDAVINR                       
611300     MOVE INLA-INL-IDLEVNR           TO EKH-IDLEVNR                       
611400     IF EKH-KVANTAL > ZERO                                                
611500       MOVE 1                        TO EKH-KDAVVTYP                      
611600     ELSE                                                                 
611700       MOVE 0                        TO EKH-KDAVVTYP                      
611800     END-IF                                                               
611900     MOVE INLA-ART-KDRT              TO EKH-KDRT                          
612000     MOVE WS-INLE-MOT-KVANTMOT       TO EKH-KVANTMOT                      
612100     MOVE INLA-ART-KVAVIS            TO EKH-KVAVIS                        
612200     MOVE WS-ARTC-KDSORT             TO EKH-KDSORT                        
612300     MOVE SPACE                      TO EKH-KDTRADP                       
612400                                                                          
612500     MOVE ZERO                       TO EKH-PRDIRLON                      
612600                                        EKH-PRDMTRL                       
612700                                        EKH-PROVRPAL                      
612800     MOVE INLA-INL-IDANALYS          TO EKH-IDANALYS                      
612900     MOVE SPACE                      TO EKH-FLDCET                        
613000     MOVE SPACE                      TO EKH-IDKUNDRF                      
613100     MOVE SPACE                      TO EKH-IDFAKT-EXP                    
613200                                                                          
613300     PERFORM IMS-ISRT-WLSAPA01                                            
613400                                                                          
613500     PERFORM UNTIL SEGMENT-FINNS                                          
613600       ADD +1                     TO FIL-IDSEKVNR                         
613700       PERFORM IMS-ISRT-WLSAPA01                                          
613800     END-PERFORM                                                          
613900     .                                                                    
614000     EJECT                                                                
614100 S01-UPPD-INLB-LEVPL SECTION.                                             
614200                                                                          
614300     MOVE INLA-ART-KVAVIS        TO WS-KV-LPLAN                           
614400     MOVE ZERO                   TO WS-KVAVROP-ATERBOK                    
614500                                                                          
614600     IF  WS-KV-LPLAN             > ZERO                                   
614700                                                                          
614800       IF  INLA-ART-KDRT         = 00                                     
614900       OR  (INLA-ART-KDRT        = 01                                     
615000        AND WS-ARTC-KDHF       > ZERO)                                    
615100       OR  (INLA-ART-KDRT        = 02                                     
615200        AND WS-ARTC-KDHF       > ZERO)                                    
615300       OR  INLA-ART-KDRT         = 05                                     
615400       OR  (INLA-ART-KDRT        = 06                                     
615500        AND INLA-INL-IDLEVNR  NOT = SPACE                                 
615600        AND INLA-INL-IDLEVNR  NOT = '9999 ')                              
615700       OR  INLA-ART-KDRT         = 10                                     
615800                                                                          
615900         MOVE INLA-ART-IDARTNR   TO W-IDARTNR-D9                          
616000         MOVE INLA-ART-IDDC      TO W-IDDC-D9                             
616100                                                                          
616200         IF  (INLA-ART-KDRT      = 00                                     
616300          OR  INLA-ART-KDRT      = 01                                     
616400          OR  INLA-ART-KDRT      = 02)                                    
616500         AND WS-ARTC-KDHF      > ZERO                                     
616600           MOVE WS-ARTC-IDLEVNR TO W-INLB11-IDLEVNR                       
616700         ELSE                                                             
616800           MOVE INLA-INL-IDLEVNR TO W-INLB11-IDLEVNR                      
616900         END-IF                                                           
617000                                                                          
617100         PERFORM IMS-GU-INLB11                                            
617200                                                                          
617300         IF  SEGMENT-FINNS                                                
617400                                                                          
617500           MOVE INLA-ART-IDLOPNRM TO WS-IDLOPNRM-VVDLLLLK                 
617600           IF  WS-IDLOPNRM-VV > WS-TIAAVVD-VV                             
617700             MOVE WS-IDLOPNRM-AA   TO TMP1-YY                             
617800             PERFORM WY2000P9                                             
617900             COMPUTE TMP1-YY = TMP1-YY - 1                                
618000             PERFORM WY2000P9                                             
618100             MOVE TMP1-YY          TO WS-IDLOPNRM-AA                      
618200           ELSE                                                           
618300             MOVE WS-TIAAVVD-AA  TO WS-IDLOPNRM-AA                        
618400           END-IF                                                         
618500                                                                          
618600           IF  INLA-ART-KDRT   NOT = 10                                   
618700             PERFORM S011-ATERBOKA-LEVPL-AVROP                            
618800*            -- B.REST ÅTERBOKAS MED ÅTERBOKAD AVROP-KVANT                
618900             MOVE WS-KVAVROP-ATERBOK TO WS-KV-OBOK                        
619000           ELSE                                                           
619100*            -- B.REST ÅTERBOKAS MED LEV.PLANENS OBOKADE KVANT            
619200             MOVE WS-KV-LPLAN  TO WS-KV-OBOK                              
619300           END-IF                                                         
619400                                                                          
619500           PERFORM S012-ATERBOKA-LEVPL-BREST                              
619600         END-IF                                                           
619700       END-IF                                                             
619800     END-IF                                                               
619900     .                                                                    
620000     EJECT                                                                
620100 S011-ATERBOKA-LEVPL-AVROP SECTION.                                       
620200                                                                          
620300     MOVE WS-IDLOPNRM-AAVVDLLLL  TO W-INLB31-IDLOPNRM                     
620400     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
620500                                                                          
620600     PERFORM IMS-GHNP-INLB23-31-PATH                                      
620700                                                                          
620800     PERFORM UNTIL (SEGMENT-SAKNAS                                        
620900                OR  WS-KV-OBOK   <= ZERO)                                 
621000                                                                          
621100*      -- AVROP ÅTERBOKAS HELT                                            
621200       SUBTRACT INLB31P-KVAVROP-AVB FROM WS-KV-OBOK                       
621300       ADD INLB31P-KVAVROP-AVB TO WS-KVAVROP-ATERBOK                      
621400       ADD INLB31P-KVAVROP-AVB TO INLB23P-KVAVROP                         
621500       MOVE +2                 TO INLB23P-KDAVROP                         
621600       PERFORM IMS-REPL-INLB23-NOT-31                                     
621700                                                                          
621800       MOVE INLB23P-DAAVROP-AVS TO W-DAAVROP                              
621900       MOVE INLB23P-TILEVDAG    TO W-TILEVDAG                             
622000       MOVE +2                 TO W-KDAVROP                               
622100       PERFORM IMS-GHNP-INLB31-F-KV                                       
622200       PERFORM IMS-DLET-INLB31                                            
622300                                                                          
622400       IF  WS-KV-OBOK            > ZERO                                   
622500         PERFORM IMS-GHNP-INLB23-31-PATH                                  
622600       END-IF                                                             
622700     END-PERFORM                                                          
622800     .                                                                    
622900     EJECT                                                                
623000 S012-ATERBOKA-LEVPL-BREST SECTION.                                       
623100                                                                          
623200     IF  WS-KV-OBOK > ZERO                                                
623300       PERFORM IMS-GHU-INLB11                                             
623400       ADD WS-KV-OBOK            TO INLB11-KVBR                           
623500       PERFORM IMS-REPL-INLB                                              
623600     END-IF                                                               
623700     .                                                                    
623800     EJECT                                                                
623900 S02-SKAPA-ZZAC01 SECTION.                                                
624000                                                                          
624100     ACCEPT ZZAC01-TIAAMMDD      FROM DATE                                
624200     ACCEPT ZZAC01-TIKLOCK       FROM TIME                                
624300                                                                          
624400     ADD +1                      TO WS-IDLOGLOP                           
624500     MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                       
624600                                                                          
624700     MOVE WS-ZZAC01-LOGGPOST     TO ZZAC01-LOGGPOST                       
624800     MOVE WS-ZZAC01-SORTPOST     TO ZZAC01-SORTPOST                       
624900                                                                          
625000     PERFORM IMS-ISRT-ZZAC01                                              
625100     PERFORM UNTIL SEGMENT-FINNS                                          
625200       ACCEPT ZZAC01-TIAAMMDD      FROM DATE                              
625300       ACCEPT ZZAC01-TIKLOCK       FROM TIME                              
625400                                                                          
625500       ADD +1                      TO WS-IDLOGLOP                         
625600       MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                     
625700       PERFORM IMS-ISRT-ZZAC01                                            
625800     END-PERFORM                                                          
625900     .                                                                    
626000     EJECT                                                                
626100 S03-RED-W211FEL-GNRL SECTION.                                            
626200                                                                          
626300     MOVE ZERO                   TO W211FEL-SORT-FLT                      
626400     MOVE SPACE                  TO W211FEL-FILLER2                       
626500                                                                          
626600     MOVE 'R32'                  TO W211FEL-IDPTYP-S                      
626700     MOVE DCS-IDDC(2:1)          TO W211FEL-KDCLAGER-S                    
626800     MOVE INLA-ART-IDARTNR       TO W211FEL-SORTBGP                       
626900     MOVE 1                      TO W211FEL-KDFELMRK                      
627000     .                                                                    
627100     EJECT                                                                
627200 S04-KONVERTERA-IDFS-IDAVINR SECTION.                                     
627300                                                                          
627400     MOVE INLA-INL-IDFS          TO WS-IDFS                               
627500     MOVE +8                     TO IX-IDFS                               
627600     MOVE +7                     TO IX-IDAVINR                            
627700                                                                          
627800     PERFORM UNTIL (IX-IDFS    = ZERO) OR                                 
627900                   (IX-IDAVINR = ZERO)                                    
628000       IF WS-IDFS-TKN (IX-IDFS) NUMERIC                                   
628100         MOVE WS-IDFS-TKN (IX-IDFS) TO WS-IDAVINR-TKN (IX-IDAVINR)        
628200         SUBTRACT 1               FROM IX-IDAVINR                         
628300       END-IF                                                             
628400       SUBTRACT 1                 FROM IX-IDFS                            
628500     END-PERFORM                                                          
628600     .                                                                    
628700     EJECT                                                                
628800 S05-UPPD-PRISJUST SECTION.                                               
628900                                                                          
629000     MOVE INLA-INL-TIAVIDAT TO WS-INLA-INL-TIAVIDAT                       
629100                               DAT-I-TIDATUM                              
629200     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
629300     CALL WDATKONV USING       DAT-KDDATFORM                              
629400                               DAT-I-TIDATUM                              
629500                               DAT-O-TIDATUM                              
629600                               DAT-KDSVAR                                 
629700     IF DAT-KDSVAR-FEL                                                    
629800       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
629900       CALL FELLOG                                                        
630000     END-IF                                                               
630100                                                                          
630200     MOVE DAT-TISEKEL TO WS-TIAVIDAT-SEKEL                                
630300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
630400     MOVE NEJ TO TRAEFF                                                   
630500                                                                          
630600     IF DCS-NDC-CN                                                        
630700       PERFORM S05A-UPPD-WDK7-PRISJUST                                    
630800     ELSE                                                                 
630900       PERFORM S05B-UPPD-ARTC-PRISJUST                                    
631000     END-IF                                                               
631100     .                                                                    
631200     EJECT                                                                
631300                                                                          
631400 S05A-UPPD-WDK7-PRISJUST SECTION.                                         
631500*    -- WDK711                                                            
631600     PERFORM IMS-GHU-WDK711                                               
631700     IF SEGMENT-FINNS                                                     
631800                                                                          
631900*    -- WDK723                                                            
632000       PERFORM IMS-GNP-WDK723                                             
632100       IF SEGMENT-FINNS                                                   
632200         MOVE SAVT-IDAVTAL    TO WS-ARTC23-IDAVTAL                        
632300       ELSE                                                               
632400         MOVE ZERO            TO WS-ARTC23-IDAVTAL                        
632500       END-IF                                                             
632600                                                                          
632700*    -- WDK724                                                            
632800       MOVE INLA-INL-IDLEVNR     TO W-IDLEVNR-PR                          
632900       COMPUTE W-DAPRLIST-K7 = 99999999 - DAGENS-DATUM                    
633000       PERFORM IMS-GHNP-WDK724                                            
633100                                                                          
633200       IF SEGMENT-FINNS                                                   
633300         MOVE JA TO TRAEFF                                                
633400         MOVE SPRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-PR                   
633500         MOVE SPRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-SUM                  
633600         MOVE SPRL-PRARTBES-PR   TO WS-ARTC-PRARTBES-PR                   
633700         MOVE SPRL-KDVALISO      TO WS-ARTC-KDVALISO                      
633800                                                                          
633900*        SUBTRACT +1               FROM SPRL-SUINLEV-PR                   
634000*        PERFORM IMS-REPL-WDK724                                          
634100                                                                          
634200         IF SPRL-SUINLEV-PR     = ZERO                                    
634300           MOVE NEJ TO TRAEFF                                             
634400                                                                          
634500*    -- WDK711                                                            
634600           PERFORM IMS-GU-WDK711                                          
634700                                                                          
634800*    -- WDK724                                                            
634900**** SENASTE INLEVERANS BEHÖVER INTE VARA MED BORTTAGEN LEVERANTÖR        
635000           MOVE INLA-INL-IDLEVNR     TO W-IDLEVNR-PR                      
635100           COMPUTE W-DAPRLIST-K7 = 99999999 - DAGENS-DATUM                
635200           PERFORM IMS-GNP-WDK724                                         
635300                                                                          
635400           IF  SEGMENT-FINNS                                              
635500             MOVE JA TO TRAEFF                                            
635600             MOVE SPRL-PRARTBEL-PR TO WS-ARTC-PRARTBEL-PR                 
635700             MOVE SPRL-PRARTBEL-PR TO WS-ARTC-PRARTBEL-SUM                
635800             MOVE SPRL-PRARTBES-PR TO WS-ARTC-PRARTBES-PR                 
635900             MOVE SPRL-KDVALISO    TO WS-ARTC-KDVALISO                    
636000                                                                          
636100             PERFORM IMS-GHNP-ARTC11-FIRST                                
636200*    -- WDK712                                                            
636300             SEARCH ALL DC-LAND                                           
636400               AT END                                                     
636500                 MOVE 'EJ TRÄFF I DCTAB DCLAND'                           
636600                                    TO FELTEXT                            
636700                 CALL FELLOG                                              
636800               WHEN DCLAND-IDDC (DCLAND-IX) = DCS-IDDC                    
636900                 MOVE DCLAND-IDLANDX2 (DCLAND-IX)                         
637000                                    TO W-IDLAND-K7                        
637100             END-SEARCH                                                   
637200             PERFORM IMS-GHU-WDK712                                       
637300**** HÄMTA VALUTAKURS FÖR CNY, AVGCO ÄR I CNY SKALL OMVANDLAS TILL        
637400****   SEK                                                                
637500             MOVE DAGENS-DATUM(3:2) TO W-DATE-AAMM(1:2)                   
637600             MOVE 01                TO W-DATE-AAMM(3:2)                   
637700             MOVE WS-KDVALISO-HUV   TO CURR-KDVALISO-HUV                  
637800             MOVE 'CNY'             TO CURR-KDVALISO-ROW                  
637900             MOVE W-DATE-AAMM       TO CURR-TIAAMM                        
638000             MOVE 'A'               TO CURR-KDVALTYP                      
638100             CALL W510CURR USING CURR-W510CURR 9305-PCB                   
638200             IF CURR-KDSVAR = ' '                                         
638300               MOVE CURR-PRKURS-NEW  TO WS-PRKURS                         
638400               MOVE CURR-REVALUTA-TO TO WS-REVALUTA                       
638500             ELSE                                                         
638600               MOVE 1                TO WS-PRKURS                         
638700               MOVE 1                TO WS-REVALUTA                       
638800             END-IF                                                       
638900****   PRARTSJK SKALL JUSTERAS TILLBAKA TILL DEN SENASTE                  
639000****   INLEVERANSEN                                                       
639100             MOVE SPRL-PRARTBES-PR TO LART-PRARTSJK                       
639200**** JUSTERA OM DET FINNS KALKYLPÅLÄGG PÅ ARTIKELN                        
639300             MOVE WS-IDDC       TO W-IDDC-B6                              
639400             PERFORM IMS-GU-WDB601                                        
639500             MOVE DCS-IDLANDX2 TO W-IDLANDX2                              
639600             IF SEGMENT-FINNS                                             
639700               PERFORM IMS-GNP-WDB617                                     
639800               IF SEGMENT-FINNS                                           
639900****   HÄMTA VALUTAKURS FÖR CNY, PRISERNA ÄR I SEK I CLAG                 
640000                COMPUTE LART-PRARTSJK ROUNDED =                           
640100                  (SPRL-PRARTBES-PR *  WS-PRKURS / WS-REVALUTA ) +        
640200                  (PROC-REDIRLON * ARTC-CLAG-PRDIRLON) +                  
640300                  (PROC-REDMTRL  * ARTC-CLAG-PRDMTRL)                     
640400                 END-COMPUTE                                              
640500               END-IF                                                     
640600             END-IF                                                       
640700             PERFORM IMS-REPL-WDK712                                      
640800           END-IF                                                         
640900         END-IF                                                           
641000       END-IF                                                             
641100     END-IF                                                               
641200     .                                                                    
641300     EJECT                                                                
641400                                                                          
641500 S05B-UPPD-ARTC-PRISJUST SECTION.                                         
641600*    -- ARTC21                                                            
641700     MOVE INLA-INL-IDLEVNR     TO W-IDLEVNR-21                            
641800     PERFORM IMS-GHNP-ARTC21                                              
641900                                                                          
642000     PERFORM UNTIL  SEGMENT-SAKNAS OR TRAEFF = JA OR                      
642100      INLA-INL-IDLEVNR = '3324 '                                          
642200      COMPUTE W-PRL-DADAT = 99999999 - ARTC-PRL-DAPRLIST-9KOMPL           
642300      IF W-PRL-DADAT > DAGENS-DATUM OR ARTC-PRL-KDSTATUS-PR = 0           
642400        PERFORM IMS-GHNP-ARTC21                                           
642500      ELSE                                                                
642600        MOVE JA TO TRAEFF                                                 
642700      END-IF                                                              
642800     END-PERFORM                                                          
642900                                                                          
643000     IF  SEGMENT-FINNS                                                    
643100       MOVE ARTC-PRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-PR                 
643200       MOVE ARTC-PRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-SUM                
643300       MOVE ARTC-PRL-PRARTBES-PR   TO WS-ARTC-PRARTBES-PR                 
643400       MOVE ARTC-PRL-KDVALISO      TO WS-ARTC-KDVALISO                    
643500                                                                          
643600      IF INLA-INL-IDLEVNR = '3324 '                                       
643700       CONTINUE                                                           
643800      ELSE                                                                
643900       SUBTRACT +1               FROM ARTC-PRL-SUINLEV-PR                 
644000       PERFORM IMS-REPL-ARTC                                              
644100                                                                          
644200       IF  ARTC-PRL-SUINLEV-PR     = ZERO                                 
644300       AND DCS-CDC OR DCS-CDC-TR                                          
644400                                                                          
644500         PERFORM IMS-GHNP-ARTC21                                          
644600                                                                          
644700         MOVE NEJ TO TRAEFF                                               
644800         PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF = JA                      
644900           COMPUTE W-PRL-DADAT = 99999999 -                               
645000                                       ARTC-PRL-DAPRLIST-9KOMPL           
645100           IF W-PRL-DADAT <= DAGENS-DATUM AND                             
645200             ARTC-PRL-KDSTATUS-PR = +1                                    
645300             MOVE JA TO TRAEFF                                            
645400           ELSE                                                           
645500             PERFORM IMS-GHNP-ARTC21                                      
645600           END-IF                                                         
645700         END-PERFORM                                                      
645800                                                                          
645900         IF  SEGMENT-FINNS                                                
646000           MOVE ARTC-PRL-PRARTBEL-PR TO WS-ARTC-PRARTBEL-PR               
646100           MOVE ARTC-PRL-PRARTBEL-PR TO WS-ARTC-PRARTBEL-SUM              
646200           MOVE ARTC-PRL-PRARTBES-PR TO WS-ARTC-PRARTBES-PR               
646300           MOVE ARTC-PRL-KDVALISO    TO WS-ARTC-KDVALISO                  
646400                                                                          
646500           PERFORM IMS-GHNP-ARTC11-FIRST                                  
646600           COMPUTE ARTC-CLAG-PRARTSJK = WS-ARTC-PRARTBES-PR +             
646700                                        ARTC-CLAG-PRDIRLON  +             
646800                                        ARTC-CLAG-PRDMTRL   +             
646900                                        ARTC-CLAG-PROVRPAL                
647000           END-COMPUTE                                                    
647100           PERFORM IMS-REPL-ARTC                                          
647200         END-IF                                                           
647300       END-IF                                                             
647400      END-IF                                                              
647500     END-IF                                                               
647600     .                                                                    
647700     EJECT                                                                
647800 S06-GET-PRARTBEL SECTION.                                                
647900     MOVE ZERO        TO WS-ARTC-PRARTBEL-PR                              
648000     MOVE ZERO        TO WS-ARTC-PRARTBEL-SUM                             
648100     MOVE ZERO        TO WS-ARTC-PRARTBES-PR                              
648200                                                                          
648300     MOVE INLA-INL-TIAVIDAT TO WS-INLA-INL-TIAVIDAT                       
648400                               DAT-I-TIDATUM                              
648500     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
648600     CALL WDATKONV USING       DAT-KDDATFORM                              
648700                               DAT-I-TIDATUM                              
648800                               DAT-O-TIDATUM                              
648900                               DAT-KDSVAR                                 
649000     IF DAT-KDSVAR-FEL                                                    
649100       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
649200       CALL FELLOG                                                        
649300     END-IF                                                               
649400                                                                          
649500     MOVE DAT-TISEKEL TO WS-TIAVIDAT-SEKEL                                
649600                                                                          
649700     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
649800                                                                          
649900*    -- WDK711                                                            
650000     PERFORM IMS-GHU-WDK711                                               
650100     IF SEGMENT-FINNS                                                     
650200                                                                          
650300*    -- WDK723                                                            
650400       MOVE SLAG-PRAVCOST        TO WS-PRAVCOST                           
650500       PERFORM IMS-GNP-WDK723                                             
650600       IF SEGMENT-FINNS                                                   
650700         MOVE SAVT-IDAVTAL    TO WS-ARTC23-IDAVTAL                        
650800       ELSE                                                               
650900         MOVE ZERO            TO WS-ARTC23-IDAVTAL                        
651000       END-IF                                                             
651100                                                                          
651200*    -- WDK724                                                            
651300       MOVE INLA-INL-IDLEVNR     TO W-IDLEVNR-PR                          
651400       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-INLA-TIAVIDAT                
651500       PERFORM IMS-GHNP-WDK724                                            
651600                                                                          
651700       MOVE NEJ TO TRAEFF                                                 
651800       PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF = JA                        
651900         IF SEGMENT-FINNS                                                 
652000           MOVE JA TO TRAEFF                                              
652100           MOVE SPRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-PR                 
652200           MOVE SPRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-SUM                
652300           MOVE SPRL-PRARTBES-PR   TO WS-ARTC-PRARTBES-PR                 
652400           MOVE SPRL-KDVALISO      TO WS-ARTC-KDVALISO                    
652500         END-IF                                                           
652600       END-PERFORM                                                        
652700     END-IF                                                               
652800     .                                                                    
652900     EJECT                                                                
653000                                                                          
653100 S07-GET-PRARTBEL-BACKNING SECTION.                                       
653200     MOVE ZERO        TO WS-ARTC-PRARTBEL-PR                              
653300     MOVE ZERO        TO WS-ARTC-PRARTBEL-SUM                             
653400     MOVE ZERO        TO WS-ARTC-PRARTBES-PR                              
653500                                                                          
653600     MOVE INLA-INL-TIAVIDAT TO WS-INLA-INL-TIAVIDAT                       
653700                               DAT-I-TIDATUM                              
653800     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
653900     CALL WDATKONV USING       DAT-KDDATFORM                              
654000                               DAT-I-TIDATUM                              
654100                               DAT-O-TIDATUM                              
654200                               DAT-KDSVAR                                 
654300     IF DAT-KDSVAR-FEL                                                    
654400       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
654500       CALL FELLOG                                                        
654600     END-IF                                                               
654700                                                                          
654800     MOVE DAT-TISEKEL TO WS-TIAVIDAT-SEKEL                                
654900                                                                          
655000*    -- WDK711                                                            
655100     PERFORM IMS-GHU-WDK711                                               
655200     IF SEGMENT-FINNS                                                     
655300                                                                          
655400*    -- WDK723                                                            
655500       PERFORM IMS-GNP-WDK723                                             
655600       IF SEGMENT-FINNS                                                   
655700         MOVE SAVT-IDAVTAL    TO WS-ARTC23-IDAVTAL                        
655800       ELSE                                                               
655900         MOVE ZERO            TO WS-ARTC23-IDAVTAL                        
656000       END-IF                                                             
656100                                                                          
656200*    -- WDK724                                                            
656300       MOVE INLA-INL-IDLEVNR     TO W-IDLEVNR-PR                          
656400       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-INLA-TIAVIDAT                
656500       PERFORM IMS-GHNP-WDK724                                            
656600       MOVE NEJ TO TRAEFF                                                 
656700                                                                          
656800       PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF = JA                        
656900         IF SEGMENT-FINNS                                                 
657000           IF SPRL-SUINLEV-PR > ZERO                                      
657100             MOVE JA TO TRAEFF                                            
657200             MOVE SPRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-PR               
657300             MOVE SPRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-SUM              
657400             MOVE SPRL-PRARTBES-PR   TO WS-ARTC-PRARTBES-PR               
657500             MOVE SPRL-KDVALISO      TO WS-ARTC-KDVALISO                  
657600           ELSE                                                           
657700             PERFORM IMS-GHNP-WDK724                                      
657800           END-IF                                                         
657900         END-IF                                                           
658000       END-PERFORM                                                        
658100     END-IF                                                               
658200     .                                                                    
658300     EJECT                                                                
658400                                                                          
658500 S07-GET-PRARTBEL-BACK-WDK621 SECTION.                                    
658600*    -- ARTC21                                                            
658700     MOVE INLA-INL-IDLEVNR     TO W-IDLEVNR-21                            
658800     PERFORM IMS-GU-ARTC11                                                
658900     IF SEGMENT-FINNS                                                     
659000       PERFORM IMS-GHNP-ARTC21                                            
659100     END-IF                                                               
659200     MOVE NEJ                  TO TRAEFF                                  
659300                                                                          
659400     PERFORM UNTIL  SEGMENT-SAKNAS OR TRAEFF = JA                         
659500      IF SEGMENT-FINNS                                                    
659600        MOVE ARTC-PRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-PR                
659700        MOVE ARTC-PRL-PRARTBEL-PR   TO WS-ARTC-PRARTBEL-SUM               
659800        MOVE ARTC-PRL-PRARTBES-PR   TO WS-ARTC-PRARTBES-PR                
659900        MOVE ARTC-PRL-KDVALISO      TO WS-ARTC-KDVALISO                   
660000        COMPUTE W-PRL-DADAT = 99999999                                    
660100                            - ARTC-PRL-DAPRLIST-9KOMPL                    
660200        IF W-PRL-DADAT > DAGENS-DATUM OR ARTC-PRL-KDSTATUS-PR = 0         
660300          PERFORM IMS-GHNP-ARTC21                                         
660400        ELSE                                                              
660500          MOVE JA TO TRAEFF                                               
660600        END-IF                                                            
660700      END-IF                                                              
660800     END-PERFORM                                                          
660900     .                                                                    
661000     EJECT                                                                
661100 DDAB-GET-CURRENCY-RATE SECTION.                                          
661200     MOVE WS-IDDC         TO W-IDDC                                       
661300     PERFORM IMS-GU-WDB601                                                
661400     MOVE DCS-KDVALISO      TO W-KDVALISO-HUV                             
661500     MOVE WS-ARTC-KDVALISO  TO W-KDVALISO-ROW                             
661600     IF W-KDVALISO-HUV = WS-ARTC-KDVALISO                                 
661700       MOVE 1 TO SPAR-PRKURS                                              
661800       MOVE 1 TO W-REVALUTA                                               
661900     ELSE                                                                 
662000       PERFORM IMS-GU-WDGX9306                                            
662100       IF SEGMENT-SAKNAS                                                  
662200         MOVE 1               TO SPAR-PRKURS                              
662300         MOVE 1               TO W-REVALUTA                               
662400       ELSE                                                               
662500         COMPUTE W-TISTADAT-9KOMPL =                                      
662600                 9999999 - WS-DATE-YYMMDD                                 
662700         PERFORM IMS-GNP-WDGX9308                                         
662800         IF SEGMENT-SAKNAS                                                
662900           PERFORM IMS-GNP-WDGX9308-FIRST                                 
663000           IF SEGMENT-SAKNAS                                              
663100             MOVE 1               TO SPAR-PRKURS                          
663200             MOVE 1               TO W-REVALUTA                           
663300           ELSE                                                           
663400             MOVE 9308-PRKURS   TO SPAR-PRKURS                            
663500             MOVE 9308-REVALUTA-TO TO W-REVALUTA                          
663600           END-IF                                                         
663700         ELSE                                                             
663800           MOVE 9308-PRKURS   TO SPAR-PRKURS                              
663900           MOVE 9308-REVALUTA-TO TO W-REVALUTA                            
664000         END-IF                                                           
664100       END-IF                                                             
664200     END-IF                                                               
664300     .                                                                    
664400     EJECT                                                                
664500                                                                          
664600 DDAC-GET-CURR-RATE-LOCAL SECTION.                                        
664700                                                                          
664800     IF W-KDVALISO-HUV = WS-ARTC-KDVALISO                                 
664900       CONTINUE                                                           
665000     ELSE                                                                 
665100       MOVE WS-ARTC-KDPRODSL           TO TEST-KDPRODSL                   
665200       IF KDPRODSL-LOCAL                                                  
665300         MOVE INLA-INL-TIAVIDAT        TO WS-DAAVIDAT-YYMMDD              
665400         MOVE WS-DAAVIDAT-YYMMDD(1:2)  TO W-DATE-AAMM(1:2)                
665500         MOVE WS-DAAVIDAT-YYMMDD(3:2)  TO W-DATE-AAMM(3:2)                
665600         MOVE W-KDVALISO-HUV           TO CURR-KDVALISO-HUV               
665700         MOVE WS-ARTC-KDVALISO         TO CURR-KDVALISO-ROW               
665800         MOVE W-DATE-AAMM              TO CURR-TIAAMM                     
665900         MOVE 'M'                      TO CURR-KDVALTYP                   
666000         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
666100         IF CURR-KDSVAR = ' '                                             
666200           MOVE CURR-PRKURS-NEW        TO WS-PRKURS                       
666300                                          SPAR-PRKURS                     
666400           MOVE CURR-REVALUTA-TO       TO WS-REVALUTA                     
666500         ELSE                                                             
666600           MOVE 1                      TO WS-PRKURS                       
666700                                          SPAR-PRKURS                     
666800           MOVE 1                      TO WS-REVALUTA                     
666900         END-IF                                                           
667000**** PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED                      
667100**** TO COUNTRY CURRENCY                                                  
667200         COMPUTE WS-ARTC-PRARTBEL-PR = WS-ARTC-PRARTBEL-PR                
667300                                   * WS-PRKURS / WS-REVALUTA              
667400       END-IF                                                             
667500     END-IF                                                               
667600     .                                                                    
667700     EJECT                                                                
667800                                                                          
667900 S08-SKAPA-EK-TRANS     SECTION.                                          
668000     IF INLA-INL-IDLEVNR = '3324 '                                        
668100       MOVE WC-CDC-TR      TO W-IDDC                                      
668200     ELSE                                                                 
668300       IF WS-IDDC-WDB6 NOT = SPACE                                        
668400         MOVE WS-IDDC-WDB6 TO W-IDDC                                      
668500       END-IF                                                             
668600     END-IF                                                               
668700                                                                          
668800** UPPDATERING AV WDR9 (SAPA)                                             
668900     MOVE 'W6011910'                  TO FIL-IDPGM                        
669000     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
669100                                         EKH-DAVERDAT                     
669200     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
669300     MOVE +1                          TO FIL-IDSEKVNR                     
669400                                                                          
669500     IF INLA-INL-IDLEVNR = '3324 '                                        
669600       MOVE WC-CDC-TR      TO EKH-IDDC-SEND                               
669700       IF WS-KVDIFF-MOT-AVIS > +0                                         
669800         COMPUTE EKH-KVANTAL = WS-KVDIFF-MOT-AVIS * -1                    
669900         MOVE '101'              TO EKH-KDEKHHT                           
670000         MOVE '106'              TO EKH-KDEKSHT                           
670100       ELSE                                                               
670200         MOVE WS-KVDIFF-MOT-AVIS TO EKH-KVANTAL                           
670300         MOVE '101'              TO EKH-KDEKHHT                           
670400         MOVE '107'              TO EKH-KDEKSHT                           
670500       END-IF                                                             
670600       MOVE +0                   TO EKH-IDDISTR                           
670700     ELSE                                                                 
670800       IF WS-KVDIFF-MOT-AVIS > +0                                         
670900         COMPUTE EKH-KVANTAL = WS-KVDIFF-MOT-AVIS * -1                    
671000         MOVE '502'              TO EKH-KDEKHHT                           
671100         MOVE '502'              TO EKH-KDEKSHT                           
671200       ELSE                                                               
671300         COMPUTE EKH-KVANTAL = WS-KVDIFF-MOT-AVIS * -1                    
671400         MOVE '502'              TO EKH-KDEKHHT                           
671500         MOVE '503'              TO EKH-KDEKSHT                           
671600       END-IF                                                             
671700                                                                          
671800       IF WS-IDDC-WDB6 NOT = SPACE                                        
671900         MOVE WS-IDDC-WDB6       TO EKH-IDDC-SEND                         
672000*** FOR RETURNS DEVIATION (OVERLEVERANS/UNDERLEVERANS) FOR VCCS,          
672100*** THE RETURNS DISTRICT FROM WDB6 WILL BE USED. THIS IS NEEDED           
672200*** IN W51068 PROGRAM WHEN UPDATING THE VAT CODES FOR RETURNS             
672300*** DEVIATION                                                             
672400       MOVE WS-IDDISTR-RETUR     TO EKH-IDDISTR                           
672500                                                                          
672600       END-IF                                                             
672700     END-IF                                                               
672800                                                                          
672900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
673000     MOVE WC-CDC-SE                   TO EKH-IDDC-REC                     
673100     MOVE +0                          TO EKH-IDKUNDNR                     
673200                                                                          
673300*******************************                                           
673400*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
673500       MOVE ZERO TO NOLL-RAKNARE                                          
673600       MOVE WS-IDAVINR                  TO WS-SAP-IDLOPNRM                
673700       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
673800       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
673900            FOR LEADING ZERO                                              
674000       ADD +1 TO NOLL-RAKNARE                                             
674100       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                     
674200          WITH POINTER NOLL-RAKNARE                                       
674300*******************************                                           
674400     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
674500     MOVE ZERO                        TO EKH-KDPSLLOC                     
674600                                         EKH-PRARTNTO                     
674700                                         EKH-PRARTSJK                     
674800                                         EKH-PRHEMTAG                     
674900                                         EKH-PRINK                        
675000                                         EKH-PRLANDCO                     
675100                                         EKH-PRDIRLON                     
675200                                         EKH-PRDMTRL                      
675300                                         EKH-PROVRPAL                     
675400                                         EKH-SUBEL                        
675500     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
675600     MOVE SPACE                       TO EKH-FLLSBOK                      
675700     MOVE 'SEK'                       TO EKH-KDVALISO                     
675800     MOVE 1.00                        TO EKH-PRKURS                       
675900     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
676000     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
676100     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
676200     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
676300     MOVE ZERO                        TO EKH-IDKONTO                      
676400                                         EKH-KDFRAKT                      
676500                                         EKH-SUVAT                        
676600     MOVE SPACE                       TO EKH-BEVAT                        
676700                                         EKH-KDANMORS                     
676800                                         EKH-IDKST                        
676900                                         EKH-IDANALYS                     
677000                                                                          
677100     MOVE INLA-INL-TIAVIDAT           TO WS-DAAVIDAT-YYMMDD               
677200     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
677300       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
677400     ELSE                                                                 
677500       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
677600     END-IF                                                               
677700     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
677800     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
677900     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
678000     IF INLA-ART-KVAVIS > ZERO                                            
678100       MOVE 1                         TO EKH-KDAVVTYP                     
678200     ELSE                                                                 
678300       MOVE 0                         TO EKH-KDAVVTYP                     
678400     END-IF                                                               
678500     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
678600     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
678700     COMPUTE EKH-KVANTMOT = WS-INLE-MOT-KVANTMOT * -1                     
678800     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
678900     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
679000     MOVE SPACE                       TO EKH-KDTRADP                      
679100     MOVE SPACE                       TO EKH-FLDCET                       
679200     MOVE SPACE                       TO EKH-IDKUNDRF                     
679300     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
679400                                                                          
679500     PERFORM IMS-ISRT-WLSAPA01                                            
679600                                                                          
679700     PERFORM UNTIL SEGMENT-FINNS                                          
679800       ADD +1 TO FIL-IDSEKVNR                                             
679900       PERFORM IMS-ISRT-WLSAPA01                                          
680000     END-PERFORM                                                          
680100     .                                                                    
680200     EJECT                                                                
680300                                                                          
680400 S09-SKAPA-TRANSPORT-TRANS SECTION.                                       
680500     IF WS-IDDC-WDB6 = SPACE                                              
680600       IF INLA-INL-IDDC NOT = DCS-IDDC                                    
680700          MOVE INLA-INL-IDDC          TO W-IDDC-B6                        
680800          PERFORM IMS-GU-WDB601                                           
680900       END-IF                                                             
681000       IF DCS-CDC                                                         
681100         MOVE WC-CDC-SE               TO EKH-IDDC-REC                     
681200                                         EKO-EKH-IDDC-REC                 
681300                                         EKH-IDDC-SEND                    
681400                                         EKO-EKH-IDDC-SEND                
681500       ELSE                                                               
681600         MOVE WC-CDC-TR               TO EKH-IDDC-REC                     
681700                                         EKO-EKH-IDDC-REC                 
681800                                         EKH-IDDC-SEND                    
681900                                         EKO-EKH-IDDC-SEND                
682000       END-IF                                                             
682100     ELSE                                                                 
682200       MOVE WS-IDDC-WDB6              TO EKH-IDDC-SEND                    
682300                                         EKO-EKH-IDDC-SEND                
682400       MOVE WC-CDC-SE                 TO EKH-IDDC-REC                     
682500                                         EKO-EKH-IDDC-REC                 
682600     END-IF                                                               
682700                                                                          
682800     IF DCS-NDC-CN                                                        
682900       PERFORM S09A-SKAPA-TRANSPORT-VCCN                                  
683000     ELSE                                                                 
683100       PERFORM S09B-SKAPA-TRANSPORT-VCCS                                  
683200     END-IF                                                               
683300     .                                                                    
683400     EJECT                                                                
683500                                                                          
683600 S09A-SKAPA-TRANSPORT-VCCN SECTION.                                       
683700** UPPDATERING AV WDR8                                                    
683800     MOVE 'W6011910'                  TO EKO-FIL-IDPGM                    
683900     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
684000     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
684100     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAVERDAT                 
684200     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
684300                                                                          
684400     MOVE '502'                       TO EKO-EKH-KDEKHHT                  
684500     MOVE '501'                       TO EKO-EKH-KDEKSHT                  
684600                                                                          
684700     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
684800     MOVE +0                          TO EKO-EKH-IDDISTR                  
684900                                         EKO-EKH-IDKUNDNR                 
685000*******************************                                           
685100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
685200       MOVE ZERO TO NOLL-RAKNARE                                          
685300       MOVE WS-IDAVINR                  TO WS-SAP-IDLOPNRM                
685400       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
685500       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
685600            FOR LEADING ZERO                                              
685700       ADD +1 TO NOLL-RAKNARE                                             
685800       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKO-EKH-IDVERGL                 
685900          WITH POINTER NOLL-RAKNARE                                       
686000*******************************                                           
686100     MOVE WS-ARTC-KDPRODSL            TO EKO-EKH-KDPRODSL                 
686200     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
686300                                         EKO-EKH-PRARTNTO                 
686400                                         EKO-EKH-PRARTSJK                 
686500                                         EKO-EKH-PRHEMTAG                 
686600                                         EKO-EKH-PRINK                    
686700                                         EKO-EKH-PRLANDCO                 
686800                                         EKO-EKH-PRDIRLON                 
686900                                         EKO-EKH-PRDMTRL                  
687000                                         EKO-EKH-PROVRPAL                 
687100                                         EKO-EKH-SUBEL                    
687200     MOVE INLA-ART-IDARTNR            TO EKO-EKH-IDARTNR                  
687300     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
687400     MOVE 'CNY'                       TO EKO-EKH-KDVALISO                 
687500     MOVE 1.00                        TO EKO-EKH-PRKURS                   
687600     MOVE ARTS-SLAG-PRAVCOST          TO EKO-EKH-PRARTSTD                 
687700                                                                          
687800     MOVE WS-KVINLART-TRP             TO EKO-EKH-KVANTAL                  
687900     MOVE 'W570EKHA'                  TO EKO-FIL-IDCPYTXT                 
688000     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
688100     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
688200                                         EKO-EKH-KDFRAKT                  
688300                                         EKH-SUVAT                        
688400     MOVE SPACE                       TO EKH-BEVAT                        
688500                                         EKO-EKH-IDKST                    
688600                                         EKH-KDANMORS                     
688700                                         EKH-IDANALYS                     
688800                                                                          
688900     MOVE INLA-INL-TIAVIDAT           TO WS-DAAVIDAT-YYMMDD               
689000     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
689100       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
689200     ELSE                                                                 
689300       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
689400     END-IF                                                               
689500     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
689600     MOVE WS-IDAVINR                  TO EKO-EKH-IDAVINR                  
689700     MOVE INLA-INL-IDLEVNR            TO EKO-EKH-IDLEVNR                  
689800     IF INLA-ART-KVAVIS > ZERO                                            
689900       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
690000     ELSE                                                                 
690100       MOVE 0                         TO EKO-EKH-KDAVVTYP                 
690200     END-IF                                                               
690300     MOVE INLA-ART-KDRT               TO EKO-EKH-KDRT                     
690400     MOVE WS-INLE-MOT-KVANTMOT        TO EKO-EKH-KVANTMOT                 
690500     MOVE INLA-ART-KVAVIS             TO EKO-EKH-KVAVIS                   
690600     MOVE WS-ARTC-KDSORT              TO EKO-EKH-KDSORT                   
690700     MOVE 'CN05'                      TO EKO-EKH-KDTRADP                  
690800     MOVE SPACE                       TO EKO-EKH-FLDCET                   
690900     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
691000     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
691100                                                                          
691200     PERFORM IMS-ISRT-EKOTRANS                                            
691300                                                                          
691400     PERFORM UNTIL SEGMENT-FINNS                                          
691500       ADD +1 TO EKO-FIL-IDSEKVNR                                         
691600       PERFORM IMS-ISRT-EKOTRANS                                          
691700     END-PERFORM                                                          
691800     .                                                                    
691900     EJECT                                                                
692000                                                                          
692100 S09B-SKAPA-TRANSPORT-VCCS SECTION.                                       
692200** UPPDATERING AV WDR9                                                    
692300     MOVE 'W6011910'                  TO FIL-IDPGM                        
692400     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
692500                                         EKH-DAVERDAT                     
692600     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
692700     MOVE +1                          TO FIL-IDSEKVNR                     
692800                                                                          
692900     MOVE '502'                       TO EKH-KDEKHHT                      
693000     MOVE '501'                       TO EKH-KDEKSHT                      
693100                                                                          
693200     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
693300     MOVE +0                          TO EKH-IDDISTR                      
693400                                         EKH-IDKUNDNR                     
693500*******************************                                           
693600*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
693700       MOVE ZERO TO NOLL-RAKNARE                                          
693800       MOVE WS-IDAVINR                  TO WS-SAP-IDLOPNRM                
693900       MOVE WS-SAP-IDLOPNRM             TO WS-SAP-X-IDLOPNRM              
694000       INSPECT WS-SAP-X-IDLOPNRM TALLYING NOLL-RAKNARE                    
694100            FOR LEADING ZERO                                              
694200       ADD +1 TO NOLL-RAKNARE                                             
694300       UNSTRING WS-SAP-X-IDLOPNRM    INTO EKH-IDVERGL                     
694400          WITH POINTER NOLL-RAKNARE                                       
694500*******************************                                           
694600     MOVE WS-ARTC-KDPRODSL            TO EKH-KDPRODSL                     
694700     MOVE ZERO                        TO EKH-KDPSLLOC                     
694800                                         EKH-PRARTNTO                     
694900                                         EKH-PRARTSJK                     
695000                                         EKH-PRHEMTAG                     
695100                                         EKH-PRINK                        
695200                                         EKH-PRLANDCO                     
695300                                         EKH-PRDIRLON                     
695400                                         EKH-PRDMTRL                      
695500                                         EKH-PROVRPAL                     
695600                                         EKH-SUBEL                        
695700     MOVE INLA-ART-IDARTNR            TO EKH-IDARTNR                      
695800     MOVE SPACE                       TO EKH-FLLSBOK                      
695900     MOVE 'SEK'                       TO EKH-KDVALISO                     
696000     MOVE 1.00                        TO EKH-PRKURS                       
696100     MOVE WS-ARTC-PRARTSTD            TO EKH-PRARTSTD                     
696200                                                                          
696300     MOVE WS-KVINLART-TRP             TO EKH-KVANTAL                      
696400     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
696500     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
696600     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
696700     MOVE ZERO                        TO EKH-IDKONTO                      
696800                                         EKH-KDFRAKT                      
696900                                         EKH-SUVAT                        
697000     MOVE SPACE                       TO EKH-BEVAT                        
697100                                         EKH-KDANMORS                     
697200                                         EKH-IDKST                        
697300                                         EKH-IDANALYS                     
697400                                                                          
697500     MOVE INLA-INL-TIAVIDAT           TO WS-DAAVIDAT-YYMMDD               
697600     IF WS-DAAVIDAT-YYMMDD(1:2) > 50                                      
697700       MOVE 19                        TO WS-DAAVIDAT-SEKEL                
697800     ELSE                                                                 
697900       MOVE 20                        TO WS-DAAVIDAT-SEKEL                
698000     END-IF                                                               
698100     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
698200     MOVE WS-IDAVINR                  TO EKH-IDAVINR                      
698300     MOVE INLA-INL-IDLEVNR            TO EKH-IDLEVNR                      
698400     IF INLA-ART-KVAVIS > ZERO                                            
698500       MOVE 1                         TO EKH-KDAVVTYP                     
698600     ELSE                                                                 
698700       MOVE 0                         TO EKH-KDAVVTYP                     
698800     END-IF                                                               
698900     MOVE INLA-ART-KDRT               TO EKH-KDRT                         
699000     MOVE WS-INLE-MOT-KVANTMOT        TO EKH-KVANTMOT                     
699100     MOVE INLA-ART-KVAVIS             TO EKH-KVAVIS                       
699200     MOVE WS-ARTC-KDSORT              TO EKH-KDSORT                       
699300     MOVE 'SEPV'                      TO EKH-KDTRADP                      
699400     MOVE SPACE                       TO EKH-FLDCET                       
699500     MOVE SPACE                       TO EKH-IDKUNDRF                     
699600     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
699700                                                                          
699800     PERFORM IMS-ISRT-WLSAPA01                                            
699900                                                                          
700000     PERFORM UNTIL SEGMENT-FINNS                                          
700100       ADD +1 TO FIL-IDSEKVNR                                             
700200       PERFORM IMS-ISRT-WLSAPA01                                          
700300     END-PERFORM                                                          
700400     .                                                                    
700500     EJECT                                                                
700600                                                                          
700700 S10-KTRL-INLA SECTION.                                                   
700800                                                                          
700900     MOVE WS-IDLOPNRM            TO W-IDLOPNRM                            
701000                                                                          
701100     PERFORM IMS-GU-INLC-SEQB                                             
701200     IF  SEGMENT-FINNS                                                    
701300       MOVE INLC-SEQB-IDLEVNR  TO W-W6D101KY-IDLEVNR                      
701400                                  W-IDLEVNR                               
701500       MOVE INLC-SEQB-IDFS     TO W-W6D101KY-IDFS                         
701600       MOVE INLC-SEQB-TIAVIDAT TO W-W6D101KY-TIAVIDAT                     
701700                                  W-DAAVSDAT-H7                           
701800       IF INLC-SEQB-TIAVIDAT NOT = ZERO                                   
701900         IF INLC-SEQB-TIAVIDAT < 500000                                   
702000           MOVE 20             TO W-DAAVSDAT-H7 (1:2)                     
702100         ELSE                                                             
702200           IF INLC-SEQB-TIAVIDAT < 999999                                 
702300             MOVE 19           TO W-DAAVSDAT-H7 (1:2)                     
702400           ELSE                                                           
702500             MOVE 99999999     TO W-DAAVSDAT-H7                           
702600           END-IF                                                         
702700         END-IF                                                           
702800       END-IF                                                             
702900       MOVE INLC-SEQB-IDRADNR-INL  TO W-IDRADNR-INL                       
703000       MOVE INLC-SEQB-IDDC         TO W-W6D101KY-IDDC                     
703100       PERFORM IMS-GU-INLA-INL                                            
703200       PERFORM IMS-GNP-INLA-ART                                           
703300       MOVE INLA-ART-IDARTNR TO W-IDARTNR                                 
703400       IF REQU-UPD-V                                                      
703500         PERFORM IMS-GU-ARTC11                                            
703600         MOVE ARTC-CLAG-ADLAGOMR-CD(1)  TO WS-ARTC-ADLAGOMR-CD(1)         
703700         MOVE ARTC-CLAG-ADLAGOMR-CD(2)  TO WS-ARTC-ADLAGOMR-CD(2)         
703800         MOVE ARTC-CLAG-ADLAGOMR-CD(3)  TO WS-ARTC-ADLAGOMR-CD(3)         
703900         MOVE ARTC-CLAG-ADLAGOMR-CD(4)  TO WS-ARTC-ADLAGOMR-CD(4)         
704000         PERFORM IMS-GU-INLA-INL                                          
704100         PERFORM IMS-GNP-INLA-ART                                         
704200       END-IF                                                             
704300*      -- REDIGERA IDLOPNRM                                               
704400       MOVE WS-TIAAVVD-AA        TO WS-IDLOPNRM-AA                        
704500       MOVE INLA-ART-IDLOPNRM    TO WS-IDLOPNRM-VVDLLLLK                  
704600                                                                          
704700* WEB FIX TO GET OUTPUT DATA ALSO AFTER UPDATE                            
704800*      IF  REQU-UPDATE OR REQU-UPD-V                                      
704900*        CONTINUE                                                         
705000*      ELSE                                                               
705100         PERFORM S11-RED-MOD                                              
705200*      END-IF                                                             
705300                                                                          
705400       IF REQU-UPD-V                                                      
705500** PF23 - R32                                                             
705600         IF (INLA-ART-KDRT = 7 OR 77 OR 8)                                
705700         OR (INLA-ART-FLKLAR = NEJ) OR (INLA-ART-FLANNULL = JA)           
705800           MOVE ERR-007-OTILLATEN-UPD TO RESP-IDMSG-ERROR                 
705900           MOVE MFS-ALFA-FAELT-FEL    TO RESP-FLBACK-ATTR                 
706000           MOVE NEJ            TO INDATA-SW                               
706100         ELSE                                                             
706200** SUMMERAR IHOP INRAPPORTERAT ANTAL                                      
706300           PERFORM IMS-GNP-INLA-RAD                                       
706400                                                                          
706500           MOVE 0 TO SPAR-RAD-SEKEL                                       
706600                     SPAR-RAD-TIUPPDAT                                    
706700                     WS-SUMMA-R32-SVS                                     
706800                     WS-SUMMA-R32-CD(1)                                   
706900                     WS-SUMMA-R32-CD(2)                                   
707000                     WS-SUMMA-R32-CD(3)                                   
707100                     WS-SUMMA-R32-CD(4)                                   
707200           PERFORM UNTIL SEGMENT-SAKNAS                                   
707300                                                                          
707400** KOLLAR SISTA UPPDATERINGSDATUM                                         
707500             MOVE INLA-RAD-TIUPPDAT TO KOLLA-RAD-TIUPPDAT                 
707600             IF KOLLA-RAD-TIUPPDAT > 500000                               
707700               MOVE 19 TO KOLLA-RAD-SEKEL                                 
707800             ELSE                                                         
707900               MOVE 20 TO KOLLA-RAD-SEKEL                                 
708000             END-IF                                                       
708100             IF KOLLA-RAD-DAUPPDAT > SPAR-RAD-DAUPPDAT                    
708200               MOVE KOLLA-RAD-DAUPPDAT TO SPAR-RAD-DAUPPDAT               
708300             END-IF                                                       
708400                                                                          
708500             IF INLA-RAD-KDINLSTA = 'TRP'                                 
708600               ADD INLA-RAD-KVINLART TO WS-KVINLART-TRP                   
708700             END-IF                                                       
708800             IF INLA-RAD-KDINLSTA = 'INL' AND                             
708900                INLA-RAD-FLSVSLS = JA                                     
709000               ADD INLA-RAD-KVINLART TO WS-SUMMA-R32-SVS                  
709100             END-IF                                                       
709200             IF INLA-RAD-KDINLSTA = 'INL' AND                             
709300                INLA-ART-ADTRDEST(1:2) = 'CD'                             
709400                MOVE 1 TO CD-IX                                           
709500                PERFORM UNTIL (CD-IX > 4) OR                              
709600                  (WS-ARTC-ADLAGOMR-CD(CD-IX) =                           
709700                                                INLA-ART-ADLAGOMR)        
709800                  ADD 1 TO CD-IX                                          
709900                END-PERFORM                                               
710000                IF CD-IX < 5                                              
710100                 IF WS-ARTC-ADLAGOMR-CD(CD-IX) =                          
710200                                                 INLA-ART-ADLAGOMR        
710300                   ADD INLA-RAD-KVINLART TO WS-SUMMA-R32-CD(CD-IX)        
710400                 END-IF                                                   
710500                END-IF                                                    
710600             END-IF                                                       
710700             PERFORM IMS-GNP-INLA-RAD                                     
710800           END-PERFORM                                                    
710900         END-IF                                                           
711000       ELSE                                                               
711100         IF REQU-UPDATE                                                   
711200           IF INLA-ART-KDRT = 3 OR 8                                      
711300           OR INLA-ART-FLKLAR = JA                                        
711400             MOVE ERR-007-OTILLATEN-UPD TO RESP-IDMSG-ERROR               
711500             MOVE MFS-ALFA-FAELT-FEL    TO RESP-FLMAK-ATTR                
711600             MOVE NEJ                   TO INDATA-SW                      
711700           ELSE                                                           
711800             PERFORM IMS-GNP-INLA-RAD                                     
711900                                                                          
712000             PERFORM UNTIL SEGMENT-SAKNAS                                 
712100                        OR  INDATA-FEL                                    
712200                                                                          
712300               IF INLA-RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK' OR          
712400                                      'AVV' OR 'ANT'                      
712500                 PERFORM IMS-GNP-INLA-RAD                                 
712600               ELSE                                                       
712700                 MOVE ERR-007-OTILLATEN-UPD TO RESP-IDMSG-ERROR           
712800                 MOVE MFS-ALFA-FAELT-FEL    TO RESP-FLMAK-ATTR            
712900                 MOVE NEJ                   TO INDATA-SW                  
713000               END-IF                                                     
713100             END-PERFORM                                                  
713200           END-IF                                                         
713300         END-IF                                                           
713400       END-IF                                                             
713500     ELSE                                                                 
713600       MOVE ERR-010-NOT-IN-REG   TO RESP-IDMSG-ERROR                      
713700       MOVE 'IDLOPNRM'           TO RESP-IDELMT-ERROR                     
713800       MOVE NEJ              TO INDATA-SW                                 
713900     END-IF                                                               
714000     .                                                                    
714100     EJECT                                                                
714200 S11-RED-MOD SECTION.                                                     
714300                                                                          
714400     MOVE INLA-ART-IDARTNR       TO RESP-IDARTNR                          
714500     MOVE INLA-ART-KVAVIS        TO RESP-KVAVIS                           
714600     MOVE INLA-ART-BEART         TO RESP-BEART                            
714700     MOVE INLA-INL-IDLEVNR       TO RESP-IDLEVNR                          
714800     MOVE INLA-INL-IDFS          TO RESP-IDFS                             
714900     MOVE INLA-INL-TIAVIDAT      TO RESP-TIAVIDAT                         
715000     .                                                                    
715100     EJECT                                                                
715200 S12-KTRL-KVAE SECTION.                                                   
715300                                                                          
715400     MOVE W-IDLOPNRM TO W-IDLOPNRM-H7                                     
715500                                                                          
715600     PERFORM IMS-GU-KVAE-W6H701                                           
715700                                                                          
715800     IF SEGMENT-FINNS                                                     
715900       MOVE KVAE-KR-TIKRANS TO SPAR-KR-TIKRANS                            
716000       MOVE KVAE-KR-FLKRLIM TO WS-KVAE-FLKRLIM                            
716100       MOVE KVAE-KR-IDKRFEL TO WS-KVAE-IDKRFEL                            
716200       IF SPAR-KR-TIKRANS > 500000                                        
716300         MOVE 19 TO SPAR-KR-SEKEL                                         
716400       ELSE                                                               
716500         MOVE 20 TO SPAR-KR-SEKEL                                         
716600       END-IF                                                             
716700       IF KVAE-KR-FLANNULL = NEJ                                          
716800          IF KVAE-KR-IDKRFEL    = 'PA' OR 'PB' OR 'K '                    
716900            MOVE ERR-QTY-QUAL-IR-EXIST TO RESP-IDMSG-ERROR                
717000          ELSE                                                            
717100            MOVE ERR-TECH-QUAL-IR-EXIST  TO RESP-IDMSG-ERROR              
717200          END-IF                                                          
717300          MOVE NEJ TO INDATA-SW                                           
717400       ELSE                                                               
717500          PERFORM IMS-GN-KVAE-W6H701                                      
717600          IF SEGMENT-FINNS                                                
717700            MOVE KVAE-KR-TIKRANS TO KOLLA-KR-TIKRANS                      
717800            IF KOLLA-KR-TIKRANS > 500000                                  
717900              MOVE 19 TO KOLLA-KR-SEKEL                                   
718000            ELSE                                                          
718100              MOVE 20 TO KOLLA-KR-SEKEL                                   
718200            END-IF                                                        
718300            IF KOLLA-KR-DAKRANS > SPAR-KR-DAKRANS                         
718400              MOVE KOLLA-KR-DAKRANS TO SPAR-KR-DAKRANS                    
718500            END-IF                                                        
718600                                                                          
718700            IF KVAE-KR-FLANNULL = NEJ                                     
718800              IF KVAE-KR-IDKRFEL    = 'PA' OR 'PB' OR 'K '                
718900                MOVE ERR-QTY-QUAL-IR-EXIST TO RESP-IDMSG-ERROR            
719000              ELSE                                                        
719100                MOVE ERR-TECH-QUAL-IR-EXIST TO RESP-IDMSG-ERROR           
719200              END-IF                                                      
719300              MOVE NEJ TO INDATA-SW                                       
719400            END-IF                                                        
719500          END-IF                                                          
719600       END-IF                                                             
719700     END-IF                                                               
719800     .                                                                    
719900     EJECT                                                                
720000 S13-KOLLA-PALAGG SECTION.                                                
720100** OMKOSTNADSPÅLÄGG LOGGAS PÅ WDR9                                        
720200     IF WS-ARTC-PRINK NOT = WS-ARTC-PRARTSTD AND                          
720300       (INLA-ART-KDRT NOT = 3 AND 7 AND 8 AND 77)                         
720400       PERFORM JI-PALAGG-WDR9                                             
720500     END-IF                                                               
720600     .                                                                    
720700     EJECT                                                                
720800* --- IMS SEKTIONER ---                                                   
720900     SKIP3                                                                
721000 RESP-FORM-ATTR SECTION.                                                  
721100*    --- ALLA INDATA-FÄLT                                                 
721200       MOVE MFS-FORMATETS-ATTR TO RESP-FLMAK-ATTR                         
721300                                  RESP-FLBACK-ATTR                        
721400     .                                                                    
721500                                                                          
721600 IMS-GU-INLC-SEQB SECTION.                                                
721700     STRING 'W6INLC01(W6D1B1KY =' W-IDLOPNRM-X                            
721800                    '&IDDC     =' W-IDDC-X ')'                            
721900          DELIMITED BY SIZE INTO SSA1                                     
722000     MOVE '  GE' TO GODK-STATUSKODER                                      
722100     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA SSA1                      
722200     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
722300     PERFORM IMS-STATUSKONTROLL                                           
722400     .                                                                    
722500     SKIP3                                                                
722600 IMS-GU-INLA-INL SECTION.                                                 
722700     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
722800          DELIMITED BY SIZE INTO SSA1                                     
722900     MOVE '  ' TO GODK-STATUSKODER                                        
723000     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA2 SSA1                     
723100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
723200     PERFORM IMS-STATUSKONTROLL                                           
723300     .                                                                    
723400     SKIP3                                                                
723500 IMS-GNP-INLA-ART SECTION.                                                
723600     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
723700          DELIMITED BY SIZE INTO SSA1                                     
723800     MOVE '  ' TO GODK-STATUSKODER                                        
723900     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA3 SSA1                    
724000     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
724100     PERFORM IMS-STATUSKONTROLL                                           
724200     .                                                                    
724300     SKIP3                                                                
724400 IMS-GNP-INLA-RAD SECTION.                                                
724500     MOVE 'W6INLA21 ' TO SSA2                                             
724600     MOVE '  GE' TO GODK-STATUSKODER                                      
724700     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA SSA1 SSA2                
724800     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
724900     PERFORM IMS-STATUSKONTROLL                                           
725000     .                                                                    
725100     EJECT                                                                
725200 IMS-GHU-INLA-ART SECTION.                                                
725300     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
725400          DELIMITED BY SIZE INTO SSA1                                     
725500     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
725600          DELIMITED BY SIZE INTO SSA2                                     
725700     MOVE '  ' TO GODK-STATUSKODER                                        
725800     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA3 SSA1 SSA2               
725900     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
726000     PERFORM IMS-STATUSKONTROLL                                           
726100     .                                                                    
726200     SKIP3                                                                
726300 IMS-REPL-INLA-ART SECTION.                                               
726400     MOVE '  ' TO GODK-STATUSKODER                                        
726500     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA3                        
726600     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
726700     PERFORM IMS-STATUSKONTROLL                                           
726800     .                                                                    
726900     SKIP3                                                                
727000 IMS-GHNP-INLA-RAD SECTION.                                               
727100     MOVE 'W6INLA21 ' TO SSA1                                             
727200     MOVE '  GE' TO GODK-STATUSKODER                                      
727300     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-AREA SSA1                    
727400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
727500     PERFORM IMS-STATUSKONTROLL                                           
727600     .                                                                    
727700     SKIP3                                                                
727800 IMS-REPL-INLA-RAD SECTION.                                               
727900     MOVE '  ' TO GODK-STATUSKODER                                        
728000     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA                         
728100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
728200     PERFORM IMS-STATUSKONTROLL                                           
728300     .                                                                    
728400     EJECT                                                                
728500 IMS-GU-KVAE-W6H701 SECTION.                                              
728600     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
728700          DELIMITED BY SIZE INTO SSA1                                     
728800     MOVE '  GE' TO GODK-STATUSKODER                                      
728900     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA8 SSA1                     
729000     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
729100     PERFORM IMS-STATUSKONTROLL                                           
729200     .                                                                    
729300     SKIP3                                                                
729400 IMS-GN-KVAE-W6H701 SECTION.                                              
729500     STRING 'W6KVAE01(W6H7CSEQ =' W-W6H7CSEQ-X ')'                        
729600          DELIMITED BY SIZE INTO SSA1                                     
729700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
729800     CALL CBLTDLI USING GN KVAE-PCB DLI-IO-AREA8 SSA1                     
729900     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
730000     PERFORM IMS-STATUSKONTROLL                                           
730100     .                                                                    
730200     SKIP3                                                                
730300 IMS-GU-ARTC01 SECTION.                                                   
730400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
730500          DELIMITED BY SIZE INTO SSA1                                     
730600     MOVE '  ' TO GODK-STATUSKODER                                        
730700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WDK6 SSA1                      
730800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
730900     PERFORM IMS-STATUSKONTROLL                                           
731000     .                                                                    
731100     SKIP3                                                                
731200 IMS-GU-ARTC11 SECTION.                                                   
731300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
731400          DELIMITED BY SIZE INTO SSA1                                     
731500     MOVE 'WLARTC11'       TO SSA2                                        
731600     MOVE '  ' TO GODK-STATUSKODER                                        
731700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WDK6 SSA1 SSA2                 
731800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
731900     PERFORM IMS-STATUSKONTROLL                                           
732000     .                                                                    
732100     SKIP3                                                                
732200 IMS-GHNP-ARTC11-FIRST SECTION.                                           
732300     MOVE 'WLARTC11*F' TO SSA1                                            
732400     MOVE '  ' TO GODK-STATUSKODER                                        
732500     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WDK6 SSA1                    
732600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
732700     PERFORM IMS-STATUSKONTROLL                                           
732800     .                                                                    
732900     SKIP3                                                                
733000 IMS-GHNP-ARTC11 SECTION.                                                 
733100     MOVE 'WLARTC11 ' TO SSA1                                             
733200     MOVE '  ' TO GODK-STATUSKODER                                        
733300     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WDK6 SSA1                    
733400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
733500     PERFORM IMS-STATUSKONTROLL                                           
733600     .                                                                    
733700     SKIP3                                                                
733800 IMS-GNP-ARTC23 SECTION.                                                  
733900                                                                          
734000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
734100     MOVE 'WLARTC23'              TO SSA2                                 
734200     MOVE '  GE' TO GODK-STATUSKODER                                      
734300     CALL CBLTDLI USING GNP ARTC-PCB WLARTC23 SSA1 SSA2                   
734400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
734500     PERFORM IMS-STATUSKONTROLL                                           
734600     .                                                                    
734700     SKIP3                                                                
734800 IMS-GHNP-ARTC21 SECTION.                                                 
734900                                                                          
735000     STRING 'WLARTC21(IDLEVNR  =' W-IDLEVNR-21-X ')'                      
735100          DELIMITED BY SIZE INTO SSA1                                     
735200     MOVE '  GE' TO GODK-STATUSKODER                                      
735300     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-WDK6 SSA1                    
735400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
735500     PERFORM IMS-STATUSKONTROLL                                           
735600     .                                                                    
735700     SKIP3                                                                
735800 IMS-REPL-ARTC SECTION.                                                   
735900                                                                          
736000     MOVE '  ' TO GODK-STATUSKODER                                        
736100     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WDK6                         
736200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
736300     PERFORM IMS-STATUSKONTROLL                                           
736400     .                                                                    
736500     EJECT                                                                
736600 IMS-ISRT-WDL901 SECTION.                                                 
736700     SKIP2                                                                
736800     MOVE 'WLLOGA01 ' TO SSA1                                             
736900     MOVE '  II' TO GODK-STATUSKODER                                      
737000     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
737100     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
737200     PERFORM IMS-STATUSKONTROLL                                           
737300     .                                                                    
737400     EJECT                                                                
737500 IMS-GU-ARTS01 SECTION.                                                   
737600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
737700          DELIMITED BY SIZE INTO SSA1                                     
737800     MOVE '  ' TO GODK-STATUSKODER                                        
737900     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-ARTS01 SSA1               
738000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
738100     PERFORM IMS-STATUSKONTROLL                                           
738200     .                                                                    
738300     SKIP3                                                                
738400 IMS-GHNP-ARTS11 SECTION.                                                 
738500     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
738600          DELIMITED BY SIZE INTO SSA1                                     
738700     MOVE '    ' TO GODK-STATUSKODER                                      
738800     CALL CBLTDLI USING GHNP ARTS-PCB DLI-IO-AREA-ARTS11 SSA1             
738900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
739000     PERFORM IMS-STATUSKONTROLL                                           
739100     .                                                                    
739200     SKIP3                                                                
739300 IMS-REPL-ARTS SECTION.                                                   
739400                                                                          
739500     MOVE '  ' TO GODK-STATUSKODER                                        
739600     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA-ARTS11                  
739700     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
739800     PERFORM IMS-STATUSKONTROLL                                           
739900     .                                                                    
740000     EJECT                                                                
740100                                                                          
740200 IMS-GU-WDK711 SECTION.                                                   
740300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
740400          DELIMITED BY SIZE INTO SSA1                                     
740500     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
740600          DELIMITED BY SIZE INTO SSA2                                     
740700     MOVE '  GE' TO GODK-STATUSKODER                                      
740800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711                         
740900          SSA1 SSA2                                                       
741000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
741100     PERFORM IMS-STATUSKONTROLL                                           
741200     .                                                                    
741300                                                                          
741400 IMS-GHU-WDK711 SECTION.                                                  
741500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
741600          DELIMITED BY SIZE INTO SSA1                                     
741700     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
741800          DELIMITED BY SIZE INTO SSA2                                     
741900     MOVE '  GE' TO GODK-STATUSKODER                                      
742000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711                        
742100          SSA1 SSA2                                                       
742200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
742300     PERFORM IMS-STATUSKONTROLL                                           
742400     .                                                                    
742500                                                                          
742600 IMS-GHU-WDK712 SECTION.                                                  
742700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
742800          DELIMITED BY SIZE INTO SSA1                                     
742900     STRING 'WDK712  (IDLAND   =' W-IDLAND-K7-X ')'                       
743000          DELIMITED BY SIZE INTO SSA2                                     
743100     MOVE '    ' TO GODK-STATUSKODER                                      
743200     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
743300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
743400     PERFORM IMS-STATUSKONTROLL                                           
743500     .                                                                    
743600                                                                          
743700 IMS-REPL-WDK712 SECTION.                                                 
743800     MOVE '  ' TO GODK-STATUSKODER                                        
743900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
744000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
744100     PERFORM IMS-STATUSKONTROLL                                           
744200     .                                                                    
744300                                                                          
744400 IMS-GNP-WDK723 SECTION.                                                  
744500     MOVE 'WDK723'              TO SSA1                                   
744600     MOVE '  GE' TO GODK-STATUSKODER                                      
744700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK723 SSA1                   
744800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
744900     PERFORM IMS-STATUSKONTROLL                                           
745000     .                                                                    
745100                                                                          
745200 IMS-GNP-WDK724 SECTION.                                                  
745300     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
745400                     '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                     
745500          DELIMITED BY SIZE INTO SSA1                                     
745600     MOVE '  GE' TO GODK-STATUSKODER                                      
745700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
745800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
745900     PERFORM IMS-STATUSKONTROLL                                           
746000     .                                                                    
746100                                                                          
746200 IMS-GHNP-WDK724 SECTION.                                                 
746300     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
746400                     '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                     
746500          DELIMITED BY SIZE INTO SSA1                                     
746600     MOVE '  GE' TO GODK-STATUSKODER                                      
746700     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK724 SSA1                  
746800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
746900     PERFORM IMS-STATUSKONTROLL                                           
747000     .                                                                    
747100                                                                          
747200 IMS-REPL-WDK724 SECTION.                                                 
747300     MOVE '  ' TO GODK-STATUSKODER                                        
747400     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK724                       
747500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
747600     PERFORM IMS-STATUSKONTROLL                                           
747700     .                                                                    
747800     EJECT                                                                
747900                                                                          
748000 IMS-GU-INLB11 SECTION.                                                   
748100     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
748200          DELIMITED BY SIZE INTO SSA1                                     
748300     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
748400          DELIMITED BY SIZE INTO SSA2                                     
748500     MOVE '  GE' TO GODK-STATUSKODER                                      
748600     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2                 
748700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
748800     PERFORM IMS-STATUSKONTROLL                                           
748900     .                                                                    
749000     SKIP3                                                                
749100 IMS-GHU-INLB11 SECTION.                                                  
749200     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
749300          DELIMITED BY SIZE INTO SSA1                                     
749400     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
749500          DELIMITED BY SIZE INTO SSA2                                     
749600     MOVE '  ' TO GODK-STATUSKODER                                        
749700     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2                
749800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
749900     PERFORM IMS-STATUSKONTROLL                                           
750000     .                                                                    
750100     SKIP3                                                                
750200 IMS-REPL-INLB SECTION.                                                   
750300     MOVE '  ' TO GODK-STATUSKODER                                        
750400     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA                         
750500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
750600     PERFORM IMS-STATUSKONTROLL                                           
750700     .                                                                    
750800     EJECT                                                                
750900 IMS-GHNP-INLB31-F-KV  SECTION.                                           
751000     STRING 'WLINLB23*F(WDD905KY =' W-WDD905KY-X                          
751100                      '&KDAVROP  =' W-KDAVROP-X ')'                       
751200            DELIMITED BY SIZE INTO SSA1                                   
751300     STRING 'WLINLB31(IDLOPNRM =' W-INLB31-IDLOPNRM-X ')'                 
751400            DELIMITED BY SIZE INTO SSA2                                   
751500     MOVE '  ' TO GODK-STATUSKODER                                        
751600     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA4 SSA1 SSA2              
751700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
751800     PERFORM IMS-STATUSKONTROLL                                           
751900     .                                                                    
752000     SKIP3                                                                
752100 IMS-DLET-INLB31 SECTION.                                                 
752200     MOVE '  ' TO GODK-STATUSKODER                                        
752300     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA4                        
752400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
752500     PERFORM IMS-STATUSKONTROLL                                           
752600     .                                                                    
752700     EJECT                                                                
752800 IMS-GHNP-INLB23-31-PATH SECTION.                                         
752900     MOVE 'WLINLB23*D' TO SSA1                                            
753000     STRING 'WLINLB31(IDLOPNRM =' W-INLB31-IDLOPNRM-X ')'                 
753100            DELIMITED BY SIZE INTO SSA2                                   
753200     MOVE '  GE' TO GODK-STATUSKODER                                      
753300     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA6 SSA1 SSA2              
753400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
753500     PERFORM IMS-STATUSKONTROLL                                           
753600     .                                                                    
753700     SKIP3                                                                
753800 IMS-REPL-INLB23-NOT-31 SECTION.                                          
753900     MOVE 'WLINLB31*N' TO SSA1                                            
754000     MOVE '  ' TO GODK-STATUSKODER                                        
754100     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA6 SSA1                   
754200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
754300     PERFORM IMS-STATUSKONTROLL                                           
754400     .                                                                    
754500     EJECT                                                                
754600 IMS-GU-INLE-ART SECTION.                                                 
754700     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
754800          DELIMITED BY SIZE INTO SSA1                                     
754900     MOVE '  GE' TO GODK-STATUSKODER                                      
755000     CALL CBLTDLI USING GU INLE-PCB DLI-IO-WDL2 SSA1                      
755100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
755200     PERFORM IMS-STATUSKONTROLL                                           
755300     .                                                                    
755400     SKIP3                                                                
755500 IMS-GHU-INLE-MOT SECTION.                                                
755600     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
755700          DELIMITED BY SIZE INTO SSA1                                     
755800     MOVE 'WLINLE11 ' TO SSA2                                             
755900     STRING 'WLINLE21(IDLOPNRM =' W-IDLOPNRM-X ')'                        
756000          DELIMITED BY SIZE INTO SSA3                                     
756100     MOVE '  ' TO GODK-STATUSKODER                                        
756200     CALL CBLTDLI USING GHU INLE-PCB DLI-IO-WDL2 SSA1 SSA2 SSA3           
756300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
756400     PERFORM IMS-STATUSKONTROLL                                           
756500     .                                                                    
756600     SKIP3                                                                
756700 IMS-REPL-INLE SECTION.                                                   
756800     MOVE '  ' TO GODK-STATUSKODER                                        
756900     CALL CBLTDLI USING REPL INLE-PCB DLI-IO-WDL2                         
757000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
757100     PERFORM IMS-STATUSKONTROLL                                           
757200     .                                                                    
757300     EJECT                                                                
757400 IMS-ISRT-INLE-INL SECTION.                                               
757500     MOVE 'WLINLE11 ' TO SSA1                                             
757600     MOVE '  II' TO GODK-STATUSKODER                                      
757700     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-WDL2 SSA1                    
757800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
757900     PERFORM IMS-STATUSKONTROLL                                           
758000     .                                                                    
758100     SKIP3                                                                
758200 IMS-ISRT-INLE-MOT SECTION.                                               
758300     MOVE 'WLINLE21 ' TO SSA1                                             
758400     MOVE '  ' TO GODK-STATUSKODER                                        
758500     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-WDL2 SSA1                    
758600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
758700     PERFORM IMS-STATUSKONTROLL                                           
758800     .                                                                    
758900     EJECT                                                                
759000 IMS-ISRT-ZZAC01 SECTION.                                                 
759100     MOVE 'WLZZAC01 ' TO SSA1                                             
759200     MOVE '  II' TO GODK-STATUSKODER                                      
759300     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
759400     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
759500     PERFORM IMS-STATUSKONTROLL                                           
759600     .                                                                    
759700     SKIP3                                                                
759800 IMS-GU-LASA-W6G210      SECTION.                                         
759900     STRING 'W6LASA01(W6GXKEY  =' WL-W6GX01KY-X ')'                       
760000          DELIMITED BY SIZE INTO SSA1                                     
760100     STRING 'W6LASA11(W6GXKEY  =' WL-W6GX11KY-X ')'                       
760200          DELIMITED BY SIZE INTO SSA2                                     
760300     MOVE '  GE'            TO GODK-STATUSKODER                           
760400     CALL CBLTDLI USING GU  LASA-PCB                                      
760500                            DLI-IO-AREA7                                  
760600                            SSA1                                          
760700                            SSA2                                          
760800     MOVE LASA-STATUS-CODE  TO STATUS-WS                                  
760900     PERFORM IMS-STATUSKONTROLL                                           
761000     .                                                                    
761100     SKIP3                                                                
761200 IMS-GHNP-LASA-W6G215-SOEK     SECTION.                                   
761300     STRING 'W6LASA21(IDLEVNR  =' WLS1-IDLEVNR-X                          
761400                    '&IDFS     =' WLS1-IDFS-X                             
761500                    '&TIAVIDAT =' WLS1-TIAVIDAT-X                         
761600                    '&IDARTNR  =' WLS1-IDARTNR-X ')'                      
761700             DELIMITED BY SIZE INTO SSA1                                  
761800     MOVE '  GE'            TO GODK-STATUSKODER                           
761900     CALL CBLTDLI USING GHNP LASA-PCB                                     
762000                            DLI-IO-AREA7                                  
762100                            SSA1                                          
762200     MOVE LASA-STATUS-CODE  TO STATUS-WS                                  
762300     PERFORM IMS-STATUSKONTROLL                                           
762400     .                                                                    
762500     SKIP3                                                                
762600 IMS-DLET-LASA-W6G215 SECTION.                                            
762700     MOVE '  ' TO GODK-STATUSKODER                                        
762800     CALL CBLTDLI USING DLET LASA-PCB DLI-IO-AREA7                        
762900     MOVE LASA-STATUS-CODE TO STATUS-WS                                   
763000     PERFORM IMS-STATUSKONTROLL                                           
763100     .                                                                    
763200     EJECT                                                                
763300     SKIP3                                                                
763400 IMS-GU-INLC-ART SECTION.                                                 
763500     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
763600          DELIMITED BY SIZE INTO SSA1                                     
763700     MOVE '  ' TO GODK-STATUSKODER                                        
763800     CALL CBLTDLI USING GU INLC-INL-PCB DLI-IO-AREA-INLC01 SSA1           
763900     MOVE INLC-INL-STATUS-CODE TO STATUS-WS                               
764000     PERFORM IMS-STATUSKONTROLL                                           
764100     .                                                                    
764200     SKIP3                                                                
764300 IMS-GHNP-INLC-INL SECTION.                                               
764400     STRING 'WLINLC11 '                                                   
764500          DELIMITED BY SIZE INTO SSA1                                     
764600     MOVE '  GE' TO GODK-STATUSKODER                                      
764700     CALL CBLTDLI USING GHNP INLC-INL-PCB DLI-IO-AREA-INLC11 SSA1         
764800     MOVE INLC-INL-STATUS-CODE TO STATUS-WS                               
764900     PERFORM IMS-STATUSKONTROLL                                           
765000     .                                                                    
765100     SKIP3                                                                
765200 IMS-GHNP-INLC-ORD SECTION.                                               
765300     STRING 'WLINLC12 '                                                   
765400          DELIMITED BY SIZE INTO SSA1                                     
765500     MOVE '  GE' TO GODK-STATUSKODER                                      
765600     CALL CBLTDLI USING GHNP INLC-INL-PCB DLI-IO-AREA-INLC12 SSA1         
765700     MOVE INLC-INL-STATUS-CODE TO STATUS-WS                               
765800     PERFORM IMS-STATUSKONTROLL                                           
765900     .                                                                    
766000     SKIP3                                                                
766100 IMS-REPL-INLC-INL SECTION.                                               
766200                                                                          
766300     MOVE '  ' TO GODK-STATUSKODER                                        
766400     CALL CBLTDLI USING REPL INLC-INL-PCB DLI-IO-AREA-INLC11 SSA1         
766500     MOVE INLC-INL-STATUS-CODE TO STATUS-WS                               
766600     PERFORM IMS-STATUSKONTROLL                                           
766700     .                                                                    
766800     SKIP3                                                                
766900 IMS-ISRT-INLC11 SECTION.                                                 
767000     STRING 'WLINLC11 '                                                   
767100          DELIMITED BY SIZE INTO SSA1                                     
767200     MOVE '  II' TO GODK-STATUSKODER                                      
767300     CALL CBLTDLI USING ISRT INLC-INL-PCB DLI-IO-AREA-INLC11 SSA1         
767400     MOVE INLC-INL-STATUS-CODE TO STATUS-WS                               
767500     PERFORM IMS-STATUSKONTROLL                                           
767600     .                                                                    
767700     SKIP3                                                                
767800 IMS-REPL-INLC-ORD SECTION.                                               
767900                                                                          
768000     MOVE '  ' TO GODK-STATUSKODER                                        
768100     CALL CBLTDLI USING REPL INLC-INL-PCB DLI-IO-AREA-INLC12 SSA1         
768200     MOVE INLC-INL-STATUS-CODE TO STATUS-WS                               
768300     PERFORM IMS-STATUSKONTROLL                                           
768400     .                                                                    
768500     SKIP3                                                                
768600 IMS-DLET-INLC-ORD SECTION.                                               
768700                                                                          
768800     MOVE '  ' TO GODK-STATUSKODER                                        
768900     CALL CBLTDLI USING DLET INLC-INL-PCB DLI-IO-AREA-INLC12 SSA1         
769000     MOVE INLC-INL-STATUS-CODE TO STATUS-WS                               
769100     PERFORM IMS-STATUSKONTROLL                                           
769200     .                                                                    
769300     SKIP3                                                                
769400 IMS-ISRT-EKOTRANS  SECTION.                                              
769500     MOVE 'WLFILB01 ' TO SSA1                                             
769600     MOVE '  II' TO GODK-STATUSKODER                                      
769700     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA-FILB01 SSA1             
769800     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
769900     PERFORM IMS-STATUSKONTROLL                                           
770000     .                                                                    
770100     EJECT                                                                
770200 IMS-ISRT-WLSAPA01 SECTION.                                               
770300     MOVE 'WLSAPA01 ' TO SSA1                                             
770400     MOVE '  II' TO GODK-STATUSKODER                                      
770500     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
770600     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
770700     PERFORM IMS-STATUSKONTROLL                                           
770800     .                                                                    
770900     SKIP2                                                                
771000 IMS-GU-WDB601-LEV SECTION.                                               
771100     STRING 'WDB601  (IDLEVNDC =' W-IDDC-B6-LEV-X ')'                     
771200          DELIMITED BY SIZE INTO SSA1                                     
771300     MOVE '  GE' TO GODK-STATUSKODER                                      
771400     CALL CBLTDLI USING GU WDB6-LEV-PCB DLI-IO-AREA-B601-LEV SSA1         
771500     MOVE WDB6-LEV-STATUS-CODE    TO STATUS-WS                            
771600     PERFORM IMS-STATUSKONTROLL                                           
771700     .                                                                    
771800     EJECT                                                                
771900 IMS-GU-WDB601    SECTION.                                                
772000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
772100          DELIMITED BY SIZE INTO SSA1                                     
772200     MOVE '  GE' TO GODK-STATUSKODER                                      
772300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
772400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
772500     PERFORM IMS-STATUSKONTROLL                                           
772600     IF SEGMENT-SAKNAS                                                    
772700         MOVE SPACE TO DCS-KDDC                                           
772800     END-IF                                                               
772900     .                                                                    
773000                                                                          
773100 IMS-GNP-WDB617    SECTION.                                               
773200     MOVE 'WDB617   ' TO SSA1                                             
773300     MOVE '  GE' TO GODK-STATUSKODER                                      
773400     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
773500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
773600     PERFORM IMS-STATUSKONTROLL                                           
773700     .                                                                    
773800                                                                          
773900 IMS-GU-WDB601-ART SECTION.                                               
774000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
774100          DELIMITED BY SIZE INTO SSA1                                     
774200     MOVE '  GE' TO GODK-STATUSKODER                                      
774300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-ART SSA1             
774400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
774500     PERFORM IMS-STATUSKONTROLL                                           
774600     IF SEGMENT-SAKNAS                                                    
774700         MOVE SPACE TO ART-DCS-KDDC                                       
774800     END-IF                                                               
774900     .                                                                    
775000                                                                          
775100 IMS-GU-WDF101   SECTION.                                                 
775200     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
775300     DELIMITED BY SIZE INTO SSA1                                          
775400     MOVE '  GE' TO GODK-STATUSKODER                                      
775500     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
775600     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
775700     PERFORM IMS-STATUSKONTROLL                                           
775800     .                                                                    
775900     SKIP3                                                                
776000                                                                          
776100 IMS-GNP-WDF102   SECTION.                                                
776200     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
776300     DELIMITED BY SIZE INTO SSA1                                          
776400     MOVE '  GE' TO GODK-STATUSKODER                                      
776500     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-AREA-F102 SSA1                
776600     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
776700     PERFORM IMS-STATUSKONTROLL                                           
776800     .                                                                    
776900     EJECT                                                                
777000                                                                          
777100 IMS-GU-WDGX9306 SECTION.                                                 
777200     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
777300             DELIMITED BY SIZE INTO SSA1                                  
777400     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
777500             DELIMITED BY SIZE INTO SSA2                                  
777600     MOVE '  GE'   TO GODK-STATUSKODER                                    
777700     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
777800     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
777900     PERFORM IMS-STATUSKONTROLL                                           
778000     .                                                                    
778100     SKIP3                                                                
778200                                                                          
778300 IMS-GNP-WDGX9308 SECTION.                                                
778400     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
778500             DELIMITED BY SIZE INTO SSA1                                  
778600     MOVE '  GE'   TO GODK-STATUSKODER                                    
778700     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
778800     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
778900     PERFORM IMS-STATUSKONTROLL                                           
779000     .                                                                    
779100     SKIP3                                                                
779200                                                                          
779300 IMS-GNP-WDGX9308-FIRST SECTION.                                          
779400     MOVE 'WDGX9308*F' TO SSA1                                            
779500     MOVE '  GE'   TO GODK-STATUSKODER                                    
779600     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
779700     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
779800     PERFORM IMS-STATUSKONTROLL                                           
779900     .                                                                    
780000     SKIP3                                                                
780100                                                                          
780200 IMS-STATUSKONTROLL SECTION.                                              
780300                                                                          
780400     SET STATUS-IX TO 1                                                   
780500     SEARCH GODK-STATUS                                                   
780600       AT END                                                             
780700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
780800         DELIMITED BY SIZE INTO FELTEXT                                   
780900         CALL FELLOG                                                      
781000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
781100         CONTINUE                                                         
781200     END-SEARCH                                                           
781300     .                                                                    
781400     EJECT                                                                
781500*    -COPY WY2000P9                                                       
