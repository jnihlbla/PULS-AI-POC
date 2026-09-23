000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4079200.                                                
000400 AUTHOR.         MÅNS SAMUELSSON.                                         
000500 DATE-WRITTEN.   95/08/03.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION: (TILLÄGG )                                                 
001000*        KOMPLETTERAT AV SUSANNE OLSSON, DEC 1997.                        
001100*        GENERELL PROGRAMKOD FÖR UPPLÄGG AV SALDOLOGG                     
001200*        I DATABAS WDL9/WLLOGA.                                           
001300*                                                                         
001400*    FUNKTION:                                                            
001500*        KOPPLINGS PROGRAM FÖR RETURENS R31 OR                            
001600*        UPPDATERAR ALLA ANDRA REGISTER MED INFO FRÅN WDA3                
001700*                                                                         
001800*        PROGRAMMET LÄSER      WLRETA (WDA3)                              
001900*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
002000*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002100*        PROGRAMMET UPPDATERAR WDK7                                       
002200*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
002300*        PROGRAMMET UPPDATERAR WLINLC (WDL6)                              
002400*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
002500*        PROGRAMMET UPPDATERAR WLZZAC (WDR8)                              
002600*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
002700*        PROGRAMMET UPPDATERAR WDR6                                       
002800*                                                                         
002900*    E-TRACKER: 1658417  DATE 2006-03-20                                  
003000*    E-TRACKER: 3107778  DATE 2006-04                                     
003100*    E-TRACKER: 3744997  DATE 2006-07                                     
003200*    E-TRACKER: 3846737  DATE 2006-08-29                                  
003300*    E-TRACKER: 5068896  DATE 2007-05-22                                  
003400*    E-TRACKER: 4823800  DATE 2007-10-23  LDC-ROLL-OUT                    
003500*                                                                         
003600*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
003700*                                                                         
003800*                                                                         
003900*                                                                         
004000*    INDATA.                                                              
004100*        TRANSAKTION: W4T792                                              
004200*        MID:         W4I79201                                            
004300*                                                                         
004400*    UTDATA.                                                              
004500*        MOD:         W4O79201                                            
004600                                                                          
004700     SKIP3                                                                
004800 ENVIRONMENT DIVISION.                                                    
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(08)   VALUE 'W4079200'.            
005500                                                                          
005600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005800                                                                          
005900 01  W-IDLOPNRM-START            PIC 9(9)    VALUE ZERO.                  
006000                                                                          
006100 01  W-IDLOPNRM                  PIC 9(9)    VALUE ZERO.                  
006200                                                                          
006300 01  W-0VVDLLLLK  REDEFINES W-IDLOPNRM.                                   
006400     03 W-0                      PIC 9(1).                                
006500     03 W-VVD                    PIC 9(3).                                
006600     03 W-LLLL                   PIC 9(4).                                
006700     03 W-K                      PIC 9(1).                                
006800                                                                          
006900 01  WS-IDDC-LOCAL.                                                       
007000     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
007100     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
007200     03  FILLER                  PIC X(1)   VALUE SPACE.                  
007300                                                                          
007400 01  WS-IDAPIDISCREF.                                                     
007500     03 WS-IDDISTR-EVENT         PIC 9(4).                                
007600     03 WS-IDKUNDNR-EVENT        PIC 9(6).                                
007700     03 WS-IDRAPPNR-EVENT        PIC 9(7).                                
007701                                                                          
007702                                                                          
007703 77  JA                          PIC X       VALUE 'J'.                   
007704 77  NEJ                         PIC X       VALUE 'N'.                   
007705 77  IX1                         PIC 9(2)    VALUE ZERO.                  
007706 77  IX2                         PIC 9(2)    VALUE ZERO.                  
007800 77  INDX                        PIC 9(3)    VALUE ZERO.                  
007900 77  WS2-IDDC                    PIC X(2)    VALUE SPACE.                 
008000 77  WS2-IDDC-RET                PIC X(2)    VALUE SPACE.                 
008100 77  WS-CDC-SE                   PIC X(2)    VALUE '11'.                  
008200 77  WS-FLLSBOK                  PIC X       VALUE 'N'.                   
008300 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008310 77  WS-EVENT-KUND               PIC X(1)    VALUE SPACE.                 
008400                                                                          
008500 77  STATUS-5-SW                 PIC X       VALUE 'N'.                   
008600     88 REDAN-STATUS-5                       VALUE 'J'.                   
008700                                                                          
008800 77  EVENT-SW                     PIC X(4)   VALUE SPACE.                 
008900     88 EVENT-YES                            VALUE 'LYNK'                 
009000                                                   'POLE'                 
009001                                                   'ACC '                 
009002                                                   'APA '                 
009003                                                   'APB '                 
009004                                                   'APC '                 
009005                                                   'APD '                 
009006                                                   'APE '                 
009007                                                   'APF '                 
009008                                                   'APG '                 
009009                                                   'APH '                 
009010                                                   'API '                 
009011                                                   'APJ '                 
009020                                                   'ECOM'.                
009100     88 EVENT-NO                             VALUE '    '.                
009101                                                                          
009102*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
009103 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
009104 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
009105                                                                          
009200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009300                                                                          
009400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009500     88  INDATA-OK                           VALUE 'J'.                   
009600     88  INDATA-FEL                          VALUE 'N'.                   
009700                                                                          
009800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009900     88  NYCKLAR-OK                          VALUE 'J'.                   
010000     88  NYCKLAR-FEL                         VALUE 'N'.                   
010100                                                                          
010110 77  SW-LYNK-NON-API             PIC X       VALUE 'N'.                   
010120     88  LYNK-NON-API                        VALUE 'J'.                   
010130                                                                          
010200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010300     88  EGEN-MID                            VALUE '4792'.                
010400     88  GODK-MID                            VALUE '4791' '4792'          
010500                                                   '4793' '4794'          
010600                                                   '4795' '4796'          
010700                                                   '4797' '4798'          
010800                                                   '4799'.                
010900     88  HELP-MID                            VALUE '0551'.                
011000                                                                          
011100 01  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
011200                                                                          
011300 01  FILLER.                                                              
011400     03  W-IDLEVNR-PIC9          PIC 9(5).                                
011500                                                                          
011600 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +33 COMP SYNC.         
011700                                                                          
011800 01  CHKP-ANT                    PIC S9(3)   VALUE +0 COMP-3.             
011900                                                                          
012000 01  WS-IDLOGLOP                 PIC S9(1)   VALUE +0 COMP-3.             
012100                                                                          
012200 01  WS-ARTC.                                                             
012300     03   WS-ARTC-CLAG-KDERS     PIC S9(3)   VALUE +0 COMP-3.             
012400     03   WS-ARTC-CLAG-KDLTK     PIC S9      VALUE +0 COMP-3.             
012500     03   WS-ARTC-CLAG-IDANSK    PIC S9(3)   VALUE +0 COMP-3.             
012600     03   WS-ARTC-CLAG-IDINK     PIC X(4)    VALUE SPACE.                 
012700     03   WS-ARTC-CLAG-ADLAGOMR  PIC S9(3)   VALUE +0 COMP-3.             
012800     03   WS-ARTC-CLAG-ADGANG    PIC S9(3)   VALUE +0 COMP-3.             
012900     03   WS-ARTC-CLAG-ADPLATS   PIC S9(5)   VALUE +0 COMP-3.             
013000     03   WS-ARTC-CLAG-PRARTSTD  PIC S9(7)V9(2) VALUE +0 COMP-3.          
013100     03   WS-ARTC-CLAG-PRHEMTAG  PIC S9(7)V9(2) VALUE +0 COMP-3.          
013200     03   WS-ARTC-ART-KDPRODSL   PIC S9(3)   VALUE +0 COMP-3.             
013300     03   WS-ARTC-CLAG-KDPSLLOC  PIC S9(3)   VALUE +0 COMP-3.             
013400     03   WS-ARTC-ART-KDSORT     PIC X(2)    VALUE SPACE.                 
013500     03   WS-ARTC-CLAG-KDLEVSP   PIC S9(3)   VALUE +0 COMP-3.             
013600     03   WS-ARTC-CLAG-IDUSER-SPKVAL PIC X(8) VALUE SPACE.                
013700                                                                          
013800 01  WS-SLAG-PRAVCOST  PIC S9(7)V9(2) VALUE +0 COMP-3.                    
013900                                                                          
014000 01  WS-KREE.                                                             
014100     03   WS-KREE-IDFTG          PIC  9(2)   VALUE ZERO.                  
014200                                                                          
014300 01  WS-IDANSTNR                 PIC S9(5)   VALUE +0 COMP-3.             
014400                                                                          
014500 01  WS-IDDC-SPAR                PIC  X(2)   VALUE SPACE.                 
014600                                                                          
014700 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
014800     03  FLT-LGD                 PIC S9  COMP SYNC   VALUE +7.            
014900     03  VAEGNINGSTAL            PIC 9(7)        VALUE 2121212.           
015000     03  VAEGNTAL-LGD            PIC S9  COMP SYNC   VALUE +7.            
015100     03  MODUL-10-11             PIC 9(2)            VALUE 10.            
015200     03  ALT-A-B                 PIC X(1)            VALUE 'B'.           
015300                                                                          
015400*     -- DATE + TIME                                                      
015500 01      WS-TIAAAAMMDDTTMMSSTH     PIC 9(16)   VALUE ZERO.                
015600 01       FILLER                REDEFINES WS-TIAAAAMMDDTTMMSSTH.          
015700     03    WS-TISEKEL               PIC 9(2).                             
015800     03    WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                             
015900     03    WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                             
016000                                                                          
016100*     -- LOGG-TRANSAR                                                     
016200 01      WS-ZZAC01.                                                       
016300     03  WS-ZZAC01-LOGGPOST      PIC X(90)   VALUE SPACE.                 
016400     03  FILLER                  REDEFINES WS-ZZAC01-LOGGPOST.            
016500        04   WS-ZZAC01-LOGGPOST-1-3  PIC X(3).                            
016600        04   WS-ZZAC01-LOGGPOST-4-90 PIC X(87).                           
016700     03  WS-ZZAC01-SORTPOST      PIC X(36)   VALUE SPACE.                 
016800     EJECT                                                                
016900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017000 01  GENERELLA-SUBPROGRAM.                                                
017100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017300     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
017400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017600     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
017700     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
017800     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
017900     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
018000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
018010     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
018100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
018200     EJECT                                                                
018300*    ---  LÄNKAREA TILL W418OKOD                                          
018400 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
018500                                                                          
018600*01 -COPY W418OKOD           -PRE OKOD-.                                  
018700     EJECT                                                                
018800*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
018900 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
019000*   -COPY W005WDK7                                                        
019100     EJECT                                                                
019200*    --- PARAMETRAR TILL SUBPROGRAM W005WDL7                              
019300 01 FILLER                       PIC X(8)    VALUE 'W005WDL7'.            
019400*   -COPY W005WDL7                                                        
019500     EJECT                                                                
019600*    --- PARAMETRAR TILL W009CIA                                          
019700*01  -COPY W009CIA                                                        
019800     EJECT                                                                
019900*01  -COPY WMSGINIT                                                       
020000     EJECT                                                                
020100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
020200*01 -COPY WMEDAREA                                                        
020300     SKIP3                                                                
020400 01  MESSAGE-CODES.                                                       
020500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
020600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
020700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
020800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
020900     03  INF-DISP-OMSTART        PIC X(3)    VALUE '249'.                 
021000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
021100     EJECT                                                                
021200                                                                          
021300 01  TEST-IDDISTR        PIC 9(5)        COMP-3.                          
021400                                                                          
021500     EJECT                                                                
021600*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
021700*                                                                         
021800 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
021900     SKIP3                                                                
022000*01 -COPY WDATAREA                                                        
022100     EJECT                                                                
022200 01  FILLER                      PIC X(16)   VALUE 'W211FEL '.            
022300     SKIP3                                                                
022400*01 -COPY W211FEL  -PRE W211FEL-                                          
022500*01 -COPY W211M109 -PRE M109-                                             
022600*01 -COPY W211M113 -PRE M113-                                             
022700     SKIP3                                                                
022800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022900*                                                                         
023000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023100     SKIP3                                                                
023200*01  MID -COPY W4I79201                                                   
023300     EJECT                                                                
023400 01  FILLER                      PIC X(16)  VALUE 'MSGKOM -AREA'.         
023500     SKIP3                                                                
023600*01  -COPY WMSGKOM                                                        
023700     EJECT                                                                
023701 01  FILLER                      PIC X(16)  VALUE 'MSGKOM1 -AREA'.        
023702     SKIP3                                                                
023703*01  -COPY WMSGKOM  -PRE MSG1-                                            
023704     EJECT                                                                
023800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023900     SKIP3                                                                
024000*01  -COPY WMSGAREA                                                       
024100     EJECT                                                                
024200 01  FILLER                     PIC X(16)  VALUE 'Z430-REQU-AREA'.        
024300*01  -COPY WZ0430I1  -PRE Z430-                                           
024400*    03  -COPY WAPIDISC -RED Z430-REQU-EVENT-DATA -PRE Z430-              
024401                                                                          
024402 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024403     SKIP3                                                                
024404*01  -COPY WMFSAREA                                                       
024500     EJECT                                                                
024600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024700*                                                                         
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025000     SKIP3                                                                
025100 01  NYCKLAR-TILL-DLI.                                                    
025200     03  W-WDA301KY-X.                                                    
025300         05  W-IDDC              PIC  X(2).                               
025400         05  W-DAREGDAT          PIC  9(8)    VALUE ZERO.                 
025500         05  W-TIKLOCK           PIC S9(9)    VALUE +0 COMP-3.            
025600     03  W-IDARTNR-X.                                                     
025700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
025800     03  W-DAINLEV-X.                                                     
025900         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
026000     03  W-IDLEVANM-X.                                                    
026100         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
026200         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
026300         05  W-IDRAPPNR          PIC  9(7)    VALUE ZERO.                 
026400     SKIP2                                                                
026500     03  W-W6GXKEY-6017-X.                                                
026600         05  W-6017-IDHTYP       PIC X(4)    VALUE '6017'.                
026700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
026800*                                                                         
026900     03  W-W6GXKEY-6018-X.                                                
027000         05  W-6018-KDSEGKEY     PIC X(1)    VALUE '1'.                   
027100     SKIP2                                                                
027200*                                                                         
027300     03  W-IDDC-B6-X.                                                     
027400         05 W-IDDC-B6                  PIC X(2)    VALUE SPACE.           
027500     03  W-IDDC-B6-RET-X.                                                 
027600         05 W-IDDC-B6-RET              PIC X(2)    VALUE SPACE.           
027610     03  W-PRAVCOST-X.                                                    
027620         05 W-PRAVCOST          PIC S9(7)V9(2) VALUE ZERO COMP-3.         
027630                                                                          
027640     03  W-IDDC1-CN                    PIC  X(1)   VALUE '7'.             
027700                                                                          
027710     03  W-IDGMT-X.                                                       
027720         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
027730         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
027740                                                                          
027800*    --- STATUS-KOD FRÅN IMS                                              
027900 01  STATUS-WS                   PIC XX.                                  
028000     88  SEGMENT-FINNS                       VALUE '  '.                  
028100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
028200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028300     SKIP2                                                                
028400 01  GODK-STATUSKODER.                                                    
028500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028600     SKIP3                                                                
028700 01  SSA1                        PIC X(64).                               
028800 01  SSA2                        PIC X(64).                               
028900 01  SSA3                        PIC X(64).                               
029000     EJECT                                                                
029100*01  -COPY WWDCKONS                                                       
029200     EJECT                                                                
029300*01  -COPY WWDC99                                                         
029400     EJECT                                                                
029500*    --- IMS FUNKTIONSKODER                                               
029600*01  -COPY W0003                                                          
029700     EJECT                                                                
029800*    ---  DLI INPUT-OUTPUT AREA                                           
029900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
030000     SKIP3                                                                
030100 01  DLI-IO-AREA.                                                         
030200     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
030300     SKIP3                                                                
030400     03  WLRETA01 REDEFINES IO-AREA.                                      
030500*        05  -COPY WDA301  -PRE RETA-                                     
030600     SKIP3                                                                
030700     03  WLARTC01 REDEFINES IO-AREA.                                      
030800*        05  -COPY WDK601  -PRE ARTC-                                     
030900     SKIP3                                                                
031000     03  WLARTC11 REDEFINES IO-AREA.                                      
031100*        05  -COPY WDK611  -PRE ARTC-                                     
031200     SKIP3                                                                
031300     03  WDK711 REDEFINES IO-AREA.                                        
031400*        05  -COPY WDK711  -PRE WDK7-                                     
031500     SKIP3                                                                
031600     03  WLINLE01 REDEFINES IO-AREA.                                      
031700*        05  -COPY WDL201  -PRE INLE-                                     
031800     SKIP3                                                                
031900     03  WLINLE11 REDEFINES IO-AREA.                                      
032000*        05  -COPY WDL211  -PRE INLE-                                     
032100     SKIP3                                                                
032200     03  WLINLE21 REDEFINES IO-AREA.                                      
032300*        05  -COPY WDL221  -PRE INLE-                                     
032400     SKIP3                                                                
032500     03  WLINLC01 REDEFINES IO-AREA.                                      
032600*        05  -COPY WDL601  -PRE INLC-                                     
032700     SKIP3                                                                
032800     03  WLINLC11 REDEFINES IO-AREA.                                      
032900*        05  -COPY WDL611  -PRE INLC-                                     
033000     SKIP3                                                                
033100     03  WLZZAC01 REDEFINES IO-AREA.                                      
033200*        05  -COPY WDG601  -PRE ZZAC01-                                   
033300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA2'.         
033400 01  DLI-IO-AREA2.                                                        
033500     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
033600     SKIP3                                                                
033700     03  WLKREE01 REDEFINES IO-AREA2.                                     
033800*        05  -COPY WDA201  -PRE KREE-                                     
033900     SKIP3                                                                
034000     03  WLKREE11 REDEFINES IO-AREA2.                                     
034100*        05  -COPY WDA211  -PRE KREE-                                     
034200     SKIP3                                                                
034300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA3'.         
034400 01  DLI-IO-AREA3.                                                        
034500     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
034600     03  W6LOPA11 REDEFINES IO-AREA3.                                     
034700*        05  -COPY W6GX6018 -PRE LOPA-                                    
034800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
034900     SKIP3                                                                
035000 01  DLI-IO-AREA4.                                                        
035100     03  IO-AREA4                PIC X(250)  VALUE SPACE.                 
035200     SKIP3                                                                
035300     03  WLFILB01 REDEFINES IO-AREA4.                                     
035400*        05  -COPY WDR801   -PRE EKO-                                     
035500         07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                        
035600*        09  -COPY W510A06  -PRE L06-                                     
035700         07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                        
035800*        09  -COPY W510EKHA -PRE EKO-                                     
035900     SKIP3                                                                
036000     EJECT                                                                
036100 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLLOGA01'.               
036200 01  DLI-IO-WLLOGA01.                                                     
036300*    03  WLLOGA01  -COPY WDL901                                           
036400     EJECT                                                                
036500 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
036600 01  DLI-IO-WLSAPA01.                                                     
036700*    03  WLSAPA01  -COPY WDR901                                           
036800*    07  -COPY W510EKHA -RED FIL-WDR901-DATA                              
036900     EJECT                                                                
037000 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDK711'.                 
037100 01  DLI-IO-WDK711.                                                       
037200*    03  -COPY WDK711                                                     
037300     EJECT                                                                
037400 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDR601'.                 
037500 01  DLI-IO-WDR601.                                                       
037600*    03  -COPY WDR601  -PRE LOG-                                          
037700*      05  -COPY W407R31A  -RED LOG-FIL-WDR601-DATA                       
037800     EJECT                                                                
037900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
038000 01   DLI-IO-AREA-B601.                                                   
038100*     03  -COPY WDB601                                                    
038200                                                                          
038300 01  FILLER               PIC X(16)   VALUE 'WDB601 RET-AREA'.            
038400 01   DLI-IO-AREA-B601-RET.                                               
038500*     03  -COPY WDB601   -PRE RET-                                        
038600                                                                          
038610 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
038620 01   DLI-IO-AREA-B201.                                                   
038630*     03  -COPY WDB201                                                    
038640                                                                          
038700 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA11'.                      
038800 01  DLI-IO-OIGA11.                                                       
038900*    03  -COPY WDL711                                                     
039000     EJECT                                                                
039100 LINKAGE SECTION.                                                         
039200                                                                          
039300*01  -COPY W0009   -PRE MSG-                                              
039400*01  -COPY W0009   -PRE 0693-                                             
039500     EJECT                                                                
039600*01  -COPY W0008  -PRE USEA-                                              
039700     05  FILLER                  PIC X.                                   
039800     EJECT                                                                
039900*01  -COPY W0008  -PRE RETA-                                              
040000     05  FILLER                  PIC X.                                   
040100     EJECT                                                                
040200*01  -COPY W0008  -PRE KREE-                                              
040300     05  FILLER                  PIC X.                                   
040400     EJECT                                                                
040500*01  -COPY W0008  -PRE ARTC-                                              
040600     05  FILLER                  PIC X.                                   
040700     EJECT                                                                
040800*01  -COPY W0008  -PRE WDK7-                                              
040900     05  FILLER                  PIC X.                                   
041000     EJECT                                                                
041100*01  -COPY W0008  -PRE INLE-                                              
041200     05  FILLER                  PIC X.                                   
041300     EJECT                                                                
041400*01  -COPY W0008  -PRE ZZAC-                                              
041500     05  FILLER                  PIC X.                                   
041600     EJECT                                                                
041700*01  -COPY W0008  -PRE LOPA-                                              
041800     05  FILLER                  PIC X.                                   
041900     EJECT                                                                
042000*01  -COPY W0008  -PRE FILB-                                              
042100     05  FILLER                  PIC X.                                   
042200     EJECT                                                                
042300*01  -COPY W0008  -PRE INLC-                                              
042400     05  FILLER                  PIC X.                                   
042500     EJECT                                                                
042600*01  -COPY W0008  -PRE WLLOGA-                                            
042700     05  FILLER                  PIC X.                                   
042800     EJECT                                                                
042900*01  -COPY W0008  -PRE WLSAPA-                                            
043000     05  FILLER                  PIC X.                                   
043100     EJECT                                                                
043200*01  -COPY W0008  -PRE WDR6-                                              
043300     05  FILLER                  PIC X.                                   
043400     EJECT                                                                
043500*01  -COPY W0008  -PRE WDB6-                                              
043600     05  FILLER                  PIC X.                                   
043700     EJECT                                                                
043710*01  -COPY W0008  -PRE WDB2-                                              
043720     05  FILLER                  PIC X.                                   
043730     EJECT                                                                
043800*01  -COPY W0008  -PRE OIGA-                                              
043900     05  FILLER                  PIC X.                                   
044000     EJECT                                                                
044100 01  WDP8-PCB                PIC X.                                       
044200     EJECT                                                                
044201                                                                          
044202 PROCEDURE DIVISION  USING MSG-PCB 0693-PCB                               
044203     USEA-PCB RETA-PCB KREE-PCB ARTC-PCB WDK7-PCB                         
044300     INLE-PCB ZZAC-PCB LOPA-PCB FILB-PCB INLC-PCB WLLOGA-PCB              
044400     WLSAPA-PCB WDR6-PCB WDB6-PCB WDB2-PCB OIGA-PCB WDP8-PCB.             
044500 MAIN SECTION.                                                            
044600     ENTRY 'DLITCBL' USING MSG-PCB 0693-PCB                               
044700     USEA-PCB RETA-PCB KREE-PCB ARTC-PCB WDK7-PCB                         
044800     INLE-PCB ZZAC-PCB LOPA-PCB FILB-PCB INLC-PCB WLLOGA-PCB              
044900     WLSAPA-PCB WDR6-PCB WDB6-PCB WDB2-PCB OIGA-PCB WDP8-PCB.             
045000                                                                          
045100     PERFORM IMS-GET-MSG                                                  
045200     IF SEGMENT-FINNS                                                     
045300       PERFORM IMS-GN-MSG                                                 
045400       PERFORM A-INIT                                                     
045500       MOVE +1 TO IX                                                      
045600       PERFORM UNTIL IX > MID-KVPOST  OR CHKP-ANT > 50                    
045700         PERFORM H-UPPDATERA                                              
045800         ADD +1 TO IX                                                     
045900       END-PERFORM                                                        
046000       PERFORM Z-FINIT                                                    
046100     END-IF                                                               
046200                                                                          
046300     MOVE ZERO TO RETURN-CODE                                             
046400     GOBACK                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 A-INIT SECTION.                                                          
046800                                                                          
046900     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I79201                    
047000     MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                   
047100     MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                  
047200                                                                          
047300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
047400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
047500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
047600                                                                          
047700     MOVE LOW-VALUE TO MSG-AREA                                           
047800                                                                          
047900*    -- INITIALIZE W006KOM                                                
048000     MOVE SPACE                      TO MSG1-MSG-KOM-WMSGKOM              
048100     MOVE SPACE                      TO MSG1-MSG-KOM-KDTRANS              
048200     MOVE SPACE                      TO MSG1-MSG-KOM-IDMFSMED             
048300     MOVE LENGTH OF MSG1-MSG-KOM-WMSGKOM TO MSG1-MSG-KOM-KVLL             
048400     MOVE LOW-VALUE                  TO MSG1-MSG-KOM-KDZ1                 
048500     MOVE LOW-VALUE                  TO MSG1-MSG-KOM-KDZ2                 
048600     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG1-MSG-KOM-TIREGDAT             
048700     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG1-MSG-KOM-TIKLOCK              
048800     MOVE LOW-VALUE                  TO MSG-KDZ1                          
048900     MOVE LOW-VALUE                  TO MSG-KDZ2                          
048901*                                                                         
048902*FÖR NDC:ERNAS SKULL HÄMTAR MAN LOKAL TID MHA ETT ANROP TILL              
048903*W005INIT. DETTA SKA KUNNA GÄLLA FÖR SAMTLIGA DC:N ÄVEN CDC               
048904*                                                                         
048905     MOVE ALL '+'         TO MSGI-WMSGINIT                                
048906     MOVE '013'           TO MSGI-KDCALL                                  
048907     MOVE MID-IDDC        TO WS-IDDC-LOCAL-DATE                           
048908                                                                          
048909     MOVE WS-IDDC-LOCAL   TO MSGI-IDUSER                                  
048910                                                                          
048911     CALL W005INIT  USING  MSGI-WMSGINIT USEA-PCB                         
048912     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
049000     MOVE MSGI-TILOKDAT       TO DAT-I-TIDATUM                            
049100     CALL WDATKONV USING DAT-KDDATFORM                                    
049200                         DAT-I-TIDATUM                                    
049300                         DAT-O-TIDATUM                                    
049400                         DAT-KDSVAR                                       
049500     IF DAT-KDSVAR = 'F'                                                  
049600       MOVE 'FEL FRÅN DATKONV ' TO FELTEXT                                
049700       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
049800     END-IF                                                               
049900                                                                          
049910     MOVE NEJ                   TO SW-LYNK-NON-API                        
049920                                                                          
050000     PERFORM IMS-GHU-LOPA-LOPA11                                          
050100     MOVE MID-IDDC TO WS2-IDDC                                            
050200     IF WS2-IDDC NOT = DCS-IDDC                                           
050300        MOVE WS2-IDDC TO W-IDDC-B6                                        
050400        PERFORM IMS-GU-WDB601                                             
050500     END-IF                                                               
050600                                                                          
050700     EVALUATE TRUE                                                        
050800       WHEN DCS-LAND-NON-VCC-OWNED OR                                     
050900            DCS-AUSTRALIA OR DCS-JAPAN OR                                 
051000           (DCS-SDC AND DCS-CHINA)                                        
051100         MOVE LOPA-6018-IDLOPNRM-JP-AU-RET  TO W-IDLOPNRM                 
051200                                               W-IDLOPNRM-START           
051300       WHEN DCS-NDC-NA                                                    
051400         MOVE LOPA-6018-IDLOPNRM-NDC-RET    TO W-IDLOPNRM                 
051500                                               W-IDLOPNRM-START           
051600       WHEN DCS-SDC                                                       
051700         MOVE LOPA-6018-IDLOPNRM-SDC-RET    TO W-IDLOPNRM                 
051800                                               W-IDLOPNRM-START           
051900       WHEN OTHER                                                         
052000         MOVE LOPA-6018-IDLOPNRM-RET        TO W-IDLOPNRM                 
052100                                               W-IDLOPNRM-START           
052200     END-EVALUATE                                                         
052300                                                                          
052400     MOVE +0                   TO EKO-FIL-IDSEKVNR                        
052500                                                                          
052600     .                                                                    
052700     EJECT                                                                
052800 H-UPPDATERA SECTION.                                                     
052900                                                                          
053000     MOVE MID-IDDC          TO W-IDDC                                     
053100     MOVE MID-TIREGDAT (IX) TO W-DAREGDAT                                 
053200     IF MID-TIREGDAT (IX) NOT = ZERO                                      
053300       IF MID-TIREGDAT (IX) < 500000                                      
053400         MOVE 20            TO W-DAREGDAT (1:2)                           
053500       ELSE                                                               
053600         IF MID-TIREGDAT (IX) < 999999                                    
053700           MOVE 19          TO W-DAREGDAT (1:2)                           
053800         ELSE                                                             
053900           MOVE 99999999    TO W-DAREGDAT                                 
054000         END-IF                                                           
054100       END-IF                                                             
054200     END-IF                                                               
054300     MOVE MID-TIKLOCK  (IX) TO W-TIKLOCK                                  
054400                                                                          
054500     PERFORM IMS-GET-RETA-RETA01                                          
054600                                                                          
054700     MOVE RETA-RET-IDDISTR   TO W-IDDISTR                                 
054800     MOVE RETA-RET-IDKUNDNR  TO W-IDKUNDNR                                
054900     MOVE RETA-RET-IDRAPPNR  TO W-IDRAPPNR                                
055000     IF RETA-RET-IDANSTNR-MOT > +0                                        
055100        MOVE RETA-RET-IDANSTNR-MOT  TO WS-IDANSTNR                        
055200     ELSE                                                                 
055300        MOVE RETA-RET-IDANSTNR-LOSS TO WS-IDANSTNR                        
055400     END-IF                                                               
055410                                                                          
055420     PERFORM S07-GET-KDKUNDKAT                                            
055500                                                                          
055600     PERFORM HA-UPPD-KREE01                                               
055700     .                                                                    
055800     EJECT                                                                
055900 HA-UPPD-KREE01  SECTION.                                                 
056000                                                                          
056100     PERFORM IMS-GHU-KREE-KREE01                                          
056200     IF KREE-ANM-KDLEVANM = 5                                             
056300       MOVE JA  TO STATUS-5-SW                                            
056400     ELSE                                                                 
056500       MOVE NEJ TO STATUS-5-SW                                            
056600     END-IF                                                               
056700                                                                          
056800     MOVE SPACE  TO EVENT-SW                                              
056801                                                                          
056802     MOVE 5 TO KREE-ANM-KDLEVANM                                          
056803     MOVE KREE-ANM-IDSYSTEM  TO EVENT-SW                                  
056900     MOVE DAT-TIAAMMDD TO KREE-ANM-DARETANK                               
057000     MOVE DAT-TISEKEL  TO KREE-ANM-DARETANK (1:2)                         
057100     IF KREE-ANM-KDARBTYP = RETA-RET-KDARBTYP                             
057200       CONTINUE                                                           
057300     ELSE                                                                 
057400       MOVE RETA-RET-KDARBTYP TO KREE-ANM-KDARBTYP                        
057500     END-IF                                                               
057600     IF KREE-ANM-IDPERSON = RETA-RET-IDPERSON                             
057700       CONTINUE                                                           
057800     ELSE                                                                 
057900       MOVE RETA-RET-IDPERSON TO KREE-ANM-IDPERSON                        
058000     END-IF                                                               
058100     MOVE KREE-ANM-IDFTG      TO WS-KREE-IDFTG                            
058200     PERFORM IMS-REPL-KREE-KREE                                           
058300     IF STATUS-5-SW = 'N'                                                 
058400        MOVE RETA-RET-IDDISTR   TO WS-IDDISTR-EVENT                       
058500        MOVE RETA-RET-IDKUNDNR  TO WS-IDKUNDNR-EVENT                      
058600        MOVE RETA-RET-IDRAPPNR  TO WS-IDRAPPNR-EVENT                      
058700                                                                          
058800                                                                          
058900        IF EVENT-YES OR LYNK-NON-API                                      
059000                                                                          
059010           IF LYNK-NON-API                                                
059020              MOVE 'LYNK'             TO Z430-REQU-IDEVENTREC             
059030           ELSE                                                           
059040              MOVE KREE-ANM-IDSYSTEM  TO Z430-REQU-IDEVENTREC             
059050           END-IF                                                         
059060                                                                          
059100           IF (KREE-ANM-IDSYSTEM = 'LYNK') OR LYNK-NON-API                
059200             MOVE 'L'              TO WS-EVENT-KUND                       
059300           END-IF                                                         
059400           IF KREE-ANM-IDSYSTEM = 'POLE'                                  
059500             MOVE 'P'              TO WS-EVENT-KUND                       
059600           END-IF                                                         
059700           IF KREE-ANM-IDSYSTEM = 'ECOM'                                  
059800             MOVE 'E'              TO WS-EVENT-KUND                       
059900           END-IF                                                         
059901           IF KREE-ANM-IDSYSTEM = 'ACC '                                  
059902             MOVE 'A'              TO WS-EVENT-KUND                       
059903           END-IF                                                         
059904           IF KREE-ANM-IDSYSTEM = 'APA '                                  
059905             MOVE 'K'              TO WS-EVENT-KUND                       
059906           END-IF                                                         
059907           IF KREE-ANM-IDSYSTEM = 'APB '                                  
059908             MOVE 'B'              TO WS-EVENT-KUND                       
059909           END-IF                                                         
059910           IF KREE-ANM-IDSYSTEM = 'APC '                                  
059911             MOVE 'C'              TO WS-EVENT-KUND                       
059912           END-IF                                                         
059913           IF KREE-ANM-IDSYSTEM = 'APD '                                  
059914             MOVE 'D'              TO WS-EVENT-KUND                       
059915           END-IF                                                         
059916           IF KREE-ANM-IDSYSTEM = 'APE '                                  
059917             MOVE 'M'              TO WS-EVENT-KUND                       
059918           END-IF                                                         
059919           IF KREE-ANM-IDSYSTEM = 'APF '                                  
059920             MOVE 'F'              TO WS-EVENT-KUND                       
059921           END-IF                                                         
059922           IF KREE-ANM-IDSYSTEM = 'APG '                                  
059923             MOVE 'G'              TO WS-EVENT-KUND                       
059924           END-IF                                                         
059925           IF KREE-ANM-IDSYSTEM = 'APH '                                  
059926             MOVE 'H'              TO WS-EVENT-KUND                       
059927           END-IF                                                         
059928           IF KREE-ANM-IDSYSTEM = 'API '                                  
059929             MOVE 'I'              TO WS-EVENT-KUND                       
059930           END-IF                                                         
059931           IF KREE-ANM-IDSYSTEM = 'APJ '                                  
059932             MOVE 'J'              TO WS-EVENT-KUND                       
059933           END-IF                                                         
059934           PERFORM HAH-CREATE-EVENT                                       
059935        END-IF                                                            
059936                                                                          
059937     END-IF                                                               
059938                                                                          
059939     PERFORM S04-GHNP-KREE-KREE11                                         
059940     PERFORM UNTIL SEGMENT-SAKNAS OR CHKP-ANT > 50                        
059941       IF KREE-LEV-IDLOPNRM = ZERO AND                                    
059942          KREE-LEV-FLANNULL = NEJ                                         
059943         MOVE WS-IDANSTNR    TO KREE-LEV-IDANSTNR-RET                     
059944         PERFORM S01-TA-UT-IDLOPNRM                                       
059945         MOVE W-IDLOPNRM     TO KREE-LEV-IDLOPNRM                         
059946         PERFORM IMS-REPL-KREE-KREE                                       
059947         MOVE KREE-LEV-IDARTNR TO W-IDARTNR                               
059948         IF OKOD-FL-SALDOBOK-RETUR = JA                                   
059949           MOVE JA                TO WS-FLLSBOK                           
059950           MOVE KREE-LEV-IDDC    TO WS2-IDDC                              
059951           IF WS2-IDDC NOT = DCS-IDDC                                     
059952              MOVE WS2-IDDC TO W-IDDC-B6                                  
059953              PERFORM IMS-GU-WDB601                                       
059954           END-IF                                                         
059960           IF (DCS-CDC AND KREE-LEV-DALEVANM  < 19970520) AND             
060000              (KREE-LEV-KDANMORS = '12' OR '22')                          
060100             PERFORM HAD-LAES-ARTC11                                      
060200             MOVE NEJ             TO WS-FLLSBOK                           
060300           ELSE                                                           
060400             MOVE KREE-LEV-IDDC-RET       TO WS2-IDDC                     
060500             IF WS2-IDDC NOT = DCS-IDDC                                   
060600                MOVE WS2-IDDC TO W-IDDC-B6                                
060700                PERFORM IMS-GU-WDB601                                     
060800             END-IF                                                       
060900             IF DCS-NDC OR DCS-SDC                                        
061000               PERFORM HAD-LAES-ARTC11                                    
061100               PERFORM HAE-UPPD-WDK711                                    
061200             ELSE                                                         
061300               PERFORM HAA-UPPD-ARTC11                                    
061400             END-IF                                                       
061500             PERFORM HAC-UPPD-LOGG                                        
061600           END-IF                                                         
061700         ELSE                                                             
061800           PERFORM HAD-LAES-ARTC11                                        
061900         END-IF                                                           
062000         MOVE KREE-LEV-IDDC-RET       TO WS2-IDDC                         
062100         IF WS2-IDDC NOT = DCS-IDDC                                       
062200            MOVE WS2-IDDC TO W-IDDC-B6                                    
062300            PERFORM IMS-GU-WDB601                                         
062400         END-IF                                                           
062500         IF DCS-NDC OR DCS-SDC                                            
062600           PERFORM HAF-UPPD-INLC                                          
062700         ELSE                                                             
062800           PERFORM HAB-UPPD-INLE                                          
062900         END-IF                                                           
063000         ADD +1 TO CHKP-ANT                                               
063100       ELSE                                                               
063200         IF KREE-LEV-FLANNULL = NEJ     AND                               
063300           (KREE-LEV-KDANMORS = '54' OR '94')                             
063400           IF REDAN-STATUS-5                                              
063500             CONTINUE                                                     
063600           ELSE                                                           
063700             MOVE KREE-LEV-IDARTNR TO W-IDARTNR                           
063800             PERFORM HAG-UPPD-ARTC11-KVAKS-PAV                            
063900           END-IF                                                         
064000         END-IF                                                           
064100       END-IF                                                             
064200       PERFORM S04-GHNP-KREE-KREE11                                       
064300     END-PERFORM                                                          
064400     .                                                                    
064500     EJECT                                                                
064600 HAA-UPPD-ARTC11  SECTION.                                                
064700                                                                          
064800     PERFORM IMS-GU-ARTC-ARTC01                                           
064900     MOVE ARTC-ART-KDPRODSL    TO WS-ARTC-ART-KDPRODSL                    
065000     MOVE ARTC-ART-KDSORT      TO WS-ARTC-ART-KDSORT                      
065100     PERFORM IMS-GHNP-ARTC-ARTC11                                         
065200                                                                          
065300     MOVE ARTC-CLAG-KDERS      TO WS-ARTC-CLAG-KDERS                      
065400     MOVE ARTC-CLAG-KDLTK      TO WS-ARTC-CLAG-KDLTK                      
065500     MOVE ARTC-CLAG-IDANSK     TO WS-ARTC-CLAG-IDANSK                     
065600     MOVE ARTC-CLAG-IDINK      TO WS-ARTC-CLAG-IDINK                      
065700     MOVE ARTC-CLAG-ADLAGOMR   TO WS-ARTC-CLAG-ADLAGOMR                   
065800     MOVE ARTC-CLAG-ADGANG     TO WS-ARTC-CLAG-ADGANG                     
065900     MOVE ARTC-CLAG-ADPLATS    TO WS-ARTC-CLAG-ADPLATS                    
066000     MOVE ARTC-CLAG-PRARTSTD   TO WS-ARTC-CLAG-PRARTSTD                   
066100     MOVE ARTC-CLAG-PRHEMTAG   TO WS-ARTC-CLAG-PRHEMTAG                   
066200     MOVE ARTC-CLAG-KDPSLLOC   TO WS-ARTC-CLAG-KDPSLLOC                   
066300                                                                          
066400     ADD KREE-LEV-KVLEVANM-BEKR  TO ARTC-CLAG-KVAKS-CDC                   
066500                                                                          
066600     PERFORM IMS-REPL-ARTC-ARTC                                           
066700                                                                          
066800**** SKALL LOGGA SALDOFÖRÄNDRING I DATABAS WDL9. ********                 
066900     MOVE ' '                  TO LOGG-IDTECKEN-KVAKS-PAV                 
067000     PERFORM S05-SKAPA-SALDOLOGG                                          
067100     .                                                                    
067200     EJECT                                                                
067300 HAB-UPPD-INLE    SECTION.                                                
067400                                                                          
067500     PERFORM IMS-GET-INLE-INLE01                                          
067600     IF SEGMENT-SAKNAS                                                    
067700       MOVE KREE-LEV-IDARTNR TO INLE-ART-IDARTNR                          
067800       PERFORM IMS-ISRT-INLE-INLE01                                       
067900     END-IF                                                               
068000*    -- SKAPA IDINLEV                                                     
068100     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
068200     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
068300     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
068400     COMPUTE W-DAINLEV          = 9999999999999999                        
068500                                - WS-TIAAAAMMDDTTMMSSTH                   
068600     END-COMPUTE                                                          
068700                                                                          
068800     MOVE W-DAINLEV TO INLE-INL-DAINLEV                                   
068900     PERFORM IMS-ISRT-INLE-INLE11                                         
069000     PERFORM UNTIL SEGMENT-FINNS                                          
069100        SUBTRACT 1 FROM W-DAINLEV                                         
069200        MOVE W-DAINLEV TO INLE-INL-DAINLEV                                
069300        PERFORM IMS-ISRT-INLE-INLE11                                      
069400     END-PERFORM                                                          
069500                                                                          
069600     MOVE '310'           TO INLE-MOT-IDPTYP                              
069700     MOVE W-IDLOPNRM      TO INLE-MOT-IDLOPNRM                            
069800     MOVE W-IDRAPPNR      TO INLE-MOT-IDAVINR                             
069900     MOVE W-IDKUNDNR      TO INLE-MOT-IDKONTO                             
070000***  MOVE W-IDDISTR       TO INLE-MOT-IDLEVNR                             
070100     MOVE W-IDDISTR        TO W-IDLEVNR-PIC9                              
070200     MOVE ZERO TO TALLY                                                   
070300     INSPECT W-IDLEVNR-PIC9 TALLYING TALLY FOR LEADING ZEROES             
070400     IF TALLY = 5                                                         
070500         MOVE SPACE TO INLE-MOT-IDLEVNR                                   
070600     ELSE                                                                 
070700        MOVE W-IDLEVNR-PIC9(TALLY + 1:) TO INLE-MOT-IDLEVNR               
070800     END-IF                                                               
070900     MOVE WS-ARTC-CLAG-ADLAGOMR TO INLE-MOT-ADLAGOMR                      
071000     MOVE WS-ARTC-CLAG-ADGANG   TO INLE-MOT-ADGANG                        
071100     MOVE WS-ARTC-CLAG-ADPLATS  TO INLE-MOT-ADPLATS                       
071200     MOVE KREE-LEV-IDDC-RET     TO INLE-MOT-IDDC                          
071300     MOVE KREE-LEV-IDDC         TO WS2-IDDC                               
071400     IF WS2-IDDC NOT = DCS-IDDC                                           
071500        MOVE WS2-IDDC TO W-IDDC-B6                                        
071600        PERFORM IMS-GU-WDB601                                             
071700     END-IF                                                               
071800     MOVE KREE-LEV-IDDC-RET     TO WS2-IDDC-RET                           
071900     IF WS2-IDDC-RET NOT = RET-DCS-IDDC                                   
072000        MOVE WS2-IDDC-RET TO W-IDDC-B6-RET                                
072100        PERFORM IMS-GU-WDB601-RET                                         
072200     END-IF                                                               
072300     IF OKOD-FL-SALDOBOK-RETUR = JA  OR                                   
072400       (DCS-SDC OR RET-DCS-NDC-NA OR RET-DCS-AUSTRALIA OR                 
072500        RET-DCS-JAPAN OR RET-DCS-LAND-NON-VCC-OWNED)                      
072600       MOVE 7             TO INLE-MOT-KDRT                                
072700     ELSE                                                                 
072800       MOVE 77            TO INLE-MOT-KDRT                                
072900     END-IF                                                               
073000     MOVE SPACE           TO INLE-MOT-IDFS                                
073100     MOVE ZERO            TO INLE-MOT-KDAVVANT                            
073200     MOVE ZERO            TO INLE-MOT-KDAVVKV                             
073300     MOVE ZERO            TO INLE-MOT-KVANTMOT                            
073400     MOVE KREE-LEV-KVLEVANM-BEKR TO INLE-MOT-KVAVIS                       
073500     MOVE ZERO            TO INLE-MOT-KVFORDEL                            
073600     MOVE ZERO            TO INLE-MOT-KVRETUR                             
073700     MOVE ZERO            TO INLE-MOT-KVFORV                              
073800     MOVE MID-TIREGDAT (IX) TO INLE-MOT-TIAVIDAT                          
073900     MOVE ZERO            TO INLE-MOT-TIUPPDAT                            
074000     MOVE ZERO            TO INLE-MOT-IDSHIPM                             
074100     PERFORM IMS-ISRT-INLE-INLE21                                         
074200     .                                                                    
074300     EJECT                                                                
074400 HAC-UPPD-LOGG    SECTION.                                                
074500                                                                          
074600     MOVE W-IDDC     TO WS2-IDDC                                          
074700     IF WS2-IDDC NOT = DCS-IDDC                                           
074800        MOVE WS2-IDDC TO W-IDDC-B6                                        
074900        PERFORM IMS-GU-WDB601                                             
075000     END-IF                                                               
075100     IF DCS-CDC                                                           
075200       IF  WS-ARTC-CLAG-KDERS    > 9                                      
075300         PERFORM HACD-LOGG-092-M113                                       
075400       END-IF                                                             
075500     END-IF                                                               
075600     IF WS-KREE-IDFTG = 53 OR 54                                          
075700       PERFORM HACF-EKONOMITRANS                                          
075800     ELSE                                                                 
075900       PERFORM HACE-EKOTRANS-WDR801-WDR901                                
076000     END-IF                                                               
076100                                                                          
076200     .                                                                    
076300     EJECT                                                                
076400 HACF-EKONOMITRANS SECTION.                                               
076500                                                                          
076600     MOVE 'L06'                  TO L06-IDPTYP                            
076700     MOVE SPACE                  TO L06-KDEKOHT                           
076800     MOVE WS-KREE-IDFTG          TO L06-IDFTG                             
076900                                                                          
077000     IF DCS-DDC                                                           
077100       MOVE '11'                 TO L06-IDDC-SEND                         
077200     ELSE                                                                 
077300       MOVE KREE-LEV-IDDC        TO L06-IDDC-SEND                         
077400     END-IF                                                               
077500                                                                          
077600     MOVE KREE-LEV-IDDC-RET      TO L06-IDDC-REC                          
077700     MOVE W-IDDISTR              TO L06-IDDISTR                           
077800     MOVE W-IDKUNDNR             TO L06-IDKUNDNR                          
077900     MOVE W-IDRAPPNR             TO L06-IDRAPPNR                          
078000     MOVE ZERO                   TO L06-DARETILL                          
078100     MOVE W-IDARTNR              TO L06-IDARTNR                           
078200     MOVE WS-ARTC-ART-KDPRODSL   TO L06-KDPRODSL                          
078300     MOVE WS-ARTC-CLAG-KDPSLLOC  TO L06-KDPSLLOC                          
078400     MOVE KREE-LEV-KVLEVANM-BEKR TO L06-KVLEVANM                          
078500     MOVE KREE-LEV-KDANMORS      TO L06-KDANMORS                          
078600     MOVE ZERO                   TO L06-KVRETINL                          
078700                                    L06-KVRETINL-SKR                      
078800     MOVE ZERO                   TO L06-PRAVCOST                          
078900                                                                          
079000     MOVE IDPGM                  TO EKO-FIL-IDPGM                         
079100     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
079200     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
079300     ADD  +1                     TO EKO-FIL-IDSEKVNR                      
079400     MOVE 'W510'                 TO EKO-FIL-CT-IDSYSTEM                   
079500     MOVE 'A06'                  TO EKO-FIL-CT-IDPTYP                     
079600     MOVE ' '                    TO EKO-FIL-CT-IDVTYP                     
079700     PERFORM IMS-ISRT-EKOTRANS                                            
079800     PERFORM UNTIL SEGMENT-FINNS                                          
079900       ADD +1  TO EKO-FIL-IDSEKVNR                                        
080000       PERFORM IMS-ISRT-EKOTRANS                                          
080100     END-PERFORM                                                          
080200     .                                                                    
080300     EJECT                                                                
080400 HACD-LOGG-092-M113 SECTION.                                              
080500                                                                          
080600     MOVE W-IDLOPNRM             TO M113-IDLOPNRM                         
080700     MOVE W-IDRAPPNR             TO M113-IDAVINR                          
080800***  MOVE W-IDDISTR              TO M113-IDLEVNR                          
080900     MOVE W-IDDISTR        TO W-IDLEVNR-PIC9                              
081000     MOVE ZERO TO TALLY                                                   
081100     INSPECT W-IDLEVNR-PIC9 TALLYING TALLY FOR LEADING ZEROES             
081200     IF TALLY = 5                                                         
081300         MOVE SPACE TO M113-IDLEVNR                                       
081400     ELSE                                                                 
081500        MOVE W-IDLEVNR-PIC9(TALLY + 1:) TO M113-IDLEVNR                   
081600     END-IF                                                               
081700     MOVE 7                      TO M113-KDRT                             
081800     MOVE KREE-LEV-KVLEVANM-BEKR TO M113-KVANTAL                          
081900     MOVE WS-ARTC-CLAG-KDERS   TO M113-KDERS                              
082000     MOVE WS-ARTC-CLAG-KDLTK   TO M113-KDLTK                              
082100     MOVE WS-ARTC-CLAG-IDANSK  TO M113-IDANSKNR                           
082200                                                                          
082300     PERFORM S03-RED-W211FEL-GNRL                                         
082400     MOVE WS-ARTC-CLAG-IDANSK  TO W211FEL-IDKUNDNR-S                      
082500     MOVE '113'                  TO W211FEL-IDFELKODX                     
082600     MOVE M113-M113              TO W211FEL-FELMED                        
082700                                                                          
082800     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
082900     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
083000     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
083100     PERFORM S02-SKAPA-ZZAC01                                             
083200     .                                                                    
083300     EJECT                                                                
083400 HACE-EKOTRANS-WDR801-WDR901 SECTION.                                     
083500                                                                          
083600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
083700     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
083800                                                                          
083900     MOVE MID-IDDC                 TO WS-IDDC                             
085200     IF XDC-NON-VCC-OWNED OR LDC-CN                                       
085210       MOVE 'W4079200'             TO EKO-FIL-IDPGM                       
085220       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
085230       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
085240       MOVE 1                      TO EKO-FIL-IDSEKVNR                    
085250       IF NDC-CN OR LDC-CN                                                
085260         MOVE 'W570'               TO EKO-FIL-IDCPYTXT(1:4)               
085270       ELSE                                                               
085280         IF NDC-IN                                                        
085290           MOVE 'W515'             TO EKO-FIL-IDCPYTXT(1:4)               
085291         ELSE                                                             
085292           MOVE DCS-KDTRADP        TO EKO-FIL-IDCPYTXT(1:4)               
085293         END-IF                                                           
085294       END-IF                                                             
085295       MOVE 'EKHA'                 TO EKO-FIL-IDCPYTXT(5:4)               
085296                                                                          
085297       PERFORM HACEA-EKOTRANS-WDR801                                      
086001     ELSE                                                                 
086002       MOVE 'W4079200'             TO FIL-IDPGM                           
086003       MOVE WS-AAAAMMDD            TO FIL-DAREGDAT                        
086004       MOVE WS-TTMMSSTH            TO FIL-TIKLOCK                         
086005       MOVE 1                      TO FIL-IDSEKVNR                        
086006       MOVE 'W510EKHA'             TO FIL-IDCPYTXT                        
086007       MOVE MSG-SIGNON-USERID      TO FIL-IDUSER                          
086008                                                                          
086009       PERFORM HACEB-EKOTRANS-WDR901                                      
086100     END-IF                                                               
086200     .                                                                    
086300     EJECT                                                                
086400 HACEA-EKOTRANS-WDR801 SECTION.                                           
086500                                                                          
086600     MOVE '302'                  TO EKO-EKH-KDEKHHT                       
086700     MOVE '302'                  TO EKO-EKH-KDEKSHT                       
086800                                                                          
086900     MOVE 'DET'                  TO EKO-EKH-KDEKNIVA                      
087000                                                                          
087100     MOVE KREE-LEV-IDDC          TO EKO-EKH-IDDC-SEND                     
087200     MOVE KREE-LEV-IDDC-RET      TO EKO-EKH-IDDC-REC                      
087300     MOVE W-IDDISTR              TO EKO-EKH-IDDISTR                       
087400     MOVE W-IDKUNDNR             TO EKO-EKH-IDKUNDNR                      
087500                                                                          
087600*    MOVE W-IDRAPPNR             TO EKO-EKH-IDVERGL                       
087700     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
087800     MOVE W-IDRAPPNR             TO CIA-IDARTBET-IN                       
087900     CALL W009CIA USING             CIA-W009CIA                           
088000     MOVE CIA-IDARTBET-UT        TO EKO-EKH-IDVERGL                       
088100                                                                          
088200     MOVE WS-AAAAMMDD            TO EKO-EKH-DAVERDAT                      
088300     MOVE WS-ARTC-ART-KDPRODSL   TO EKO-EKH-KDPRODSL                      
088400     MOVE 0                      TO EKO-EKH-KDPSLLOC                      
088500     MOVE W-IDARTNR              TO EKO-EKH-IDARTNR                       
088600     MOVE ' '                    TO EKO-EKH-FLLSBOK                       
089100     MOVE 1.00                   TO EKO-EKH-PRKURS                        
089200     MOVE 0                      TO EKO-EKH-PRARTNTO                      
089300     MOVE 0                      TO EKO-EKH-PRARTSJK                      
089400     MOVE 0                      TO EKO-EKH-PRHEMTAG                      
089500     MOVE WDK7-SLAG-PRAVCOST     TO EKO-EKH-PRARTSTD                      
089600     MOVE 0                      TO EKO-EKH-PRLANDCO                      
089700     MOVE 0                      TO EKO-EKH-PRINK                         
089800     MOVE 0                      TO EKO-EKH-PRDIRLON                      
089900     MOVE 0                      TO EKO-EKH-PRDMTRL                       
090000     MOVE 0                      TO EKO-EKH-PROVRPAL                      
090100     MOVE KREE-LEV-KVLEVANM-BEKR TO EKO-EKH-KVANTAL                       
090200     MOVE 0                      TO EKO-EKH-SUBEL                         
090300                                                                          
090400     MOVE KREE-LEV-KDANMORS      TO EKO-EKH-KDANMORS                      
090500     MOVE KREE-LEV-IDANALYS      TO EKO-EKH-IDANALYS                      
090600     MOVE KREE-LEV-IDKONTO       TO EKO-EKH-IDKONTO                       
090700     MOVE KREE-LEV-IDKST         TO EKO-EKH-IDKST                         
090800     MOVE ZERO                   TO EKO-EKH-BEVAT                         
090900                                    EKO-EKH-KDFRAKT                       
091000                                    EKO-EKH-SUVAT                         
091100     MOVE ZERO                   TO EKO-EKH-DAAVIDAT                      
091200                                    EKO-EKH-IDAVINR                       
091300                                    EKO-EKH-KDAVVTYP                      
091400                                    EKO-EKH-KDRT                          
091500                                    EKO-EKH-KVANTMOT                      
091600                                    EKO-EKH-KVAVIS                        
091700                                                                          
091800     MOVE W-IDTRANS              TO EKO-EKH-IDTRANS                       
091900     MOVE WS-ARTC-ART-KDSORT     TO EKO-EKH-KDSORT                        
092400     MOVE SPACE                  TO EKO-EKH-IDLEVNR                       
092500     MOVE SPACE                  TO EKO-EKH-FLDCET                        
092600     MOVE SPACE                  TO EKO-EKH-IDKUNDRF                      
092610     MOVE SPACE                  TO EKO-EKH-IDFAKT-EXP                    
092620     MOVE DCS-KDVALISO           TO EKO-EKH-KDVALISO                      
092630     MOVE DCS-KDTRADP            TO EKO-EKH-KDTRADP                       
092700                                                                          
092800     PERFORM IMS-ISRT-EKOTRANS                                            
092900     PERFORM UNTIL SEGMENT-FINNS                                          
093000       ADD +1  TO EKO-FIL-IDSEKVNR                                        
093100       PERFORM IMS-ISRT-EKOTRANS                                          
093200     END-PERFORM                                                          
093300     .                                                                    
093400     EJECT                                                                
093500 HACEB-EKOTRANS-WDR901 SECTION.                                           
093600                                                                          
093700     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
093800     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
093900                                                                          
094000     MOVE '302'                  TO EKH-KDEKHHT                           
094100     MOVE '302'                  TO EKH-KDEKSHT                           
094200                                                                          
094300     MOVE 'DET'                  TO EKH-KDEKNIVA                          
094400     MOVE KREE-LEV-IDDC          TO EKH-IDDC-SEND                         
094500     MOVE KREE-LEV-IDDC-RET      TO EKH-IDDC-REC                          
094600     MOVE W-IDDISTR              TO EKH-IDDISTR                           
094700     MOVE W-IDKUNDNR             TO EKH-IDKUNDNR                          
094800                                                                          
094900*    MOVE W-IDRAPPNR             TO EKH-IDVERGL                           
095000     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
095100     MOVE W-IDRAPPNR             TO CIA-IDARTBET-IN                       
095200     CALL W009CIA USING             CIA-W009CIA                           
095300     MOVE CIA-IDARTBET-UT        TO EKH-IDVERGL                           
095400                                                                          
095500     MOVE WS-AAAAMMDD            TO EKH-DAVERDAT                          
095600     MOVE WS-ARTC-ART-KDPRODSL   TO EKH-KDPRODSL                          
095700     MOVE 0                      TO EKH-KDPSLLOC                          
095800     MOVE W-IDARTNR              TO EKH-IDARTNR                           
095900     MOVE ' '                    TO EKH-FLLSBOK                           
096000     MOVE 'SEK'                  TO EKH-KDVALISO                          
096100     MOVE 1.00                   TO EKH-PRKURS                            
096200     MOVE 0                      TO EKH-PRARTNTO                          
096300     MOVE 0                      TO EKH-PRARTSJK                          
096400     MOVE WS-ARTC-CLAG-PRHEMTAG  TO EKH-PRHEMTAG                          
096500     MOVE WS-ARTC-CLAG-PRARTSTD  TO EKH-PRARTSTD                          
096600     MOVE 0                      TO EKH-PRLANDCO                          
096700     MOVE 0                      TO EKH-PRINK                             
096800     MOVE 0                      TO EKH-PRDIRLON                          
096900     MOVE 0                      TO EKH-PRDMTRL                           
097000     MOVE 0                      TO EKH-PROVRPAL                          
097100     MOVE KREE-LEV-KVLEVANM-BEKR TO EKH-KVANTAL                           
097200     MOVE 0                      TO EKH-SUBEL                             
097300                                                                          
097400     MOVE KREE-LEV-KDANMORS      TO EKH-KDANMORS                          
097500     MOVE KREE-LEV-IDANALYS      TO EKH-IDANALYS                          
097600     MOVE KREE-LEV-IDKONTO       TO EKH-IDKONTO                           
097700     MOVE KREE-LEV-IDKST         TO EKH-IDKST                             
097800     MOVE ZERO                   TO EKH-BEVAT                             
097900                                    EKH-KDFRAKT                           
098000                                    EKH-SUVAT                             
098100     MOVE ZERO                   TO EKH-DAAVIDAT                          
098200                                    EKH-IDAVINR                           
098300                                    EKH-KDAVVTYP                          
098400                                    EKH-KDRT                              
098500                                    EKH-KVANTMOT                          
098600                                    EKH-KVAVIS                            
098700                                                                          
098800     MOVE W-IDTRANS              TO EKH-IDTRANS                           
098900     MOVE WS-ARTC-ART-KDSORT     TO EKH-KDSORT                            
099000     MOVE 'SEPV'                 TO EKH-KDTRADP                           
099100     MOVE SPACE                  TO EKH-IDLEVNR                           
099200     MOVE SPACE                  TO EKH-FLDCET                            
099300     MOVE SPACE                  TO EKH-IDKUNDRF                          
099310     MOVE SPACE                  TO EKH-IDFAKT-EXP                        
099400                                                                          
099500     PERFORM IMS-ISRT-WDR901                                              
099600     PERFORM UNTIL SEGMENT-FINNS                                          
099700       ADD +1  TO FIL-IDSEKVNR                                            
099800       PERFORM IMS-ISRT-WDR901                                            
099900     END-PERFORM                                                          
100000     .                                                                    
100100     EJECT                                                                
100200 HAD-LAES-ARTC11  SECTION.                                                
100300                                                                          
100400     PERFORM IMS-GU-ARTC-ARTC01                                           
100500     MOVE ARTC-ART-KDPRODSL    TO WS-ARTC-ART-KDPRODSL                    
100600     PERFORM IMS-GHU-ARTC-ARTC11                                          
100700     MOVE ARTC-CLAG-KDERS      TO WS-ARTC-CLAG-KDERS                      
100800     MOVE ARTC-CLAG-KDLTK      TO WS-ARTC-CLAG-KDLTK                      
100900     MOVE ARTC-CLAG-IDANSK     TO WS-ARTC-CLAG-IDANSK                     
101000     MOVE ARTC-CLAG-IDINK      TO WS-ARTC-CLAG-IDINK                      
101100     MOVE ARTC-CLAG-ADLAGOMR   TO WS-ARTC-CLAG-ADLAGOMR                   
101200     MOVE ARTC-CLAG-ADGANG     TO WS-ARTC-CLAG-ADGANG                     
101300     MOVE ARTC-CLAG-ADPLATS    TO WS-ARTC-CLAG-ADPLATS                    
101400     MOVE ARTC-CLAG-PRARTSTD   TO WS-ARTC-CLAG-PRARTSTD                   
101500     MOVE ARTC-CLAG-PRHEMTAG   TO WS-ARTC-CLAG-PRHEMTAG                   
101600*    MOVE ARTC-CLAG-KDLEVSP    TO WDK7-KDLEVSP                            
101700     MOVE ARTC-CLAG-KDLEVSP    TO WS-ARTC-CLAG-KDLEVSP                    
101800     MOVE ARTC-CLAG-IDUSER-SPKVAL TO WS-ARTC-CLAG-IDUSER-SPKVAL           
101900     .                                                                    
102000     EJECT                                                                
102100 HAE-UPPD-WDK711  SECTION.                                                
102200     PERFORM IMS-GHU-WDK711                                               
102300     IF SEGMENT-SAKNAS                                                    
102400       PERFORM S20-SKAPA-WDK7                                             
102500       PERFORM IMS-GHU-WDK711                                             
102600       IF KREE-LEV-KDANMORS = '72' AND                                    
102700       (DCS-SDC AND NOT DCS-CHINA)                                        
102800         MOVE WS2-IDDC TO R31-IDDC                                        
102900         PERFORM HAEB-SKAPA-LDC-LOGG                                      
103000       END-IF                                                             
103100     END-IF                                                               
103200                                                                          
103300**** DET KAN VARA SÅ ATT MAN HAR BYTT LAGER FÖR KUNDEN OCH DÅ             
103400**** ANVÄNDER MAN ETT LAGER DÄR DET INTE FINNS ETT AVERAGE COST           
103500     IF DCS-CHINA                                                         
103600       IF WDK7-SLAG-PRAVCOST = ZERO                                       
103900         PERFORM IMS-GU-WDK711-CN                                         
104000         MOVE SLAG-PRAVCOST       TO WS-SLAG-PRAVCOST                     
104200         PERFORM IMS-GHU-WDK711                                           
104300         MOVE WS-SLAG-PRAVCOST    TO WDK7-SLAG-PRAVCOST                   
104400       END-IF                                                             
104500     END-IF                                                               
104600****                                                                      
104700                                                                          
104800     ADD KREE-LEV-KVLEVANM-BEKR  TO WDK7-SLAG-KVAKS-SDC                   
104900                                                                          
105000     PERFORM IMS-REPL-WDK7                                                
105100     PERFORM HAEA-SKAPA-SALDOLOGG                                         
105200     .                                                                    
105300     EJECT                                                                
105400 HAEA-SKAPA-SALDOLOGG SECTION.                                            
105500                                                                          
105600     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
105700                                                                          
105800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
105900     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
106000                                   - WS-AAAAMMDD                          
106100     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
106200     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
106300                                   - WS-TTMMSSTH                          
106400     MOVE 9                        TO LOGG-IDSEKVNR                       
106500     MOVE W-IDDC                   TO LOGG-IDDC                           
106600     MOVE 'DISC'                   TO LOGG-IDHUVTYP                       
106700     MOVE 'RET'                    TO LOGG-IDSUBTYP                       
106800     MOVE 'W4079200'               TO LOGG-IDPGM                          
106900     MOVE W-IDTRANS                TO LOGG-IDTRANS                        
107000     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
107100     MOVE SPACE                    TO LOGG-REF                            
107200     MOVE W-IDDISTR                TO LOGG-IDDISTR                        
107300     MOVE W-IDKUNDNR               TO LOGG-IDKUNDNR                       
107400     MOVE W-IDRAPPNR               TO LOGG-IDRAPPNR                       
107500     MOVE '+'                      TO LOGG-IDTECKEN-KVAKS                 
107600     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
107700     MOVE ' '                      TO LOGG-IDTECKEN-KVEFRS                
107800     MOVE ' '                      TO LOGG-IDTECKEN-KVLS                  
107900     MOVE KREE-LEV-KVLEVANM-BEKR   TO LOGG-KVART-SALDO                    
108000     MOVE WDK7-SLAG-KVAKS-SDC      TO LOGG-KVAKS                          
108100     MOVE WDK7-SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                      
108200     MOVE WDK7-SLAG-KVEFRS         TO LOGG-KVEFRS                         
108300     MOVE WDK7-SLAG-KVLS           TO LOGG-KVLS                           
108400     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
108500                                                                          
108600     PERFORM S06-IMS-ISRT-WDL901                                          
108700     .                                                                    
108800     EJECT                                                                
108900 HAEB-SKAPA-LDC-LOGG SECTION.                                             
109000                                                                          
109100*--- SKRIVER LOGGPOST PÅ WDR601                                           
109200                                                                          
109300     MOVE IDPGM                  TO LOG-FIL-IDPGM                         
109400     ACCEPT LOG-FIL-TIREGDAT FROM DATE                                    
109500     ACCEPT LOG-FIL-TIKLOCK FROM TIME                                     
109600     MOVE ZERO                   TO LOG-FIL-IDSEKVNR                      
109700     MOVE 'W407'                 TO LOG-FIL-CT-IDSYSTEM                   
109800     MOVE 'A'                    TO LOG-FIL-CT-IDVTYP                     
109900     MOVE 'R31'                  TO LOG-FIL-CT-IDPTYP                     
110000     ADD +1                      TO LOG-FIL-IDSEKVNR                      
110100                                                                          
110200     MOVE W-IDARTNR              TO R31-IDARTNR                           
110300     MOVE SPACE                  TO R31-BEART-SVE                         
110400     MOVE WS-ARTC-CLAG-PRARTSTD  TO R31-PRARTSTD                          
110500     MOVE WDK7-SLAG-KVLS         TO R31-KVLS-LDC                          
110600     MOVE KREE-LEV-KVLEVANM-BEKR TO R31-KVAKS-LDC                         
110700     MOVE ZERO                   TO R31-SULAGVDE-LDC                      
110800                                                                          
110900     MOVE WS-ARTC-CLAG-KDERS     TO R31-KDERS                             
111000                                                                          
111100     MOVE ZERO                   TO R31-KVLS-CDC                          
111200                                                                          
111300     MOVE ZERO                   TO R31-KVPB-TOT                          
111400                                                                          
111500     MOVE W-IDDISTR              TO R31-IDDISTR                           
111600     MOVE W-IDKUNDNR             TO R31-IDKUNDNR                          
111700     MOVE W-IDRAPPNR             TO R31-IDRAPPNR                          
111800                                                                          
111900     PERFORM IMS-ISRT-WDR601                                              
112000     IF SEGMENT-FINNS-REDAN                                               
112100        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
112200           ADD +1 TO LOG-FIL-IDSEKVNR                                     
112300           PERFORM IMS-ISRT-WDR601                                        
112400        END-PERFORM                                                       
112500     END-IF                                                               
112600     .                                                                    
112700     EJECT                                                                
112800 HAF-UPPD-INLC SECTION.                                                   
112900*    -- SKAPA IDINLEV                                                     
113000     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
113100     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
113200     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
113300     COMPUTE W-DAINLEV          = 9999999999999999                        
113400                                - WS-TIAAAAMMDDTTMMSSTH                   
113500     END-COMPUTE                                                          
113600     PERFORM IMS-GU-INLC01                                                
113700     IF SEGMENT-SAKNAS                                                    
113800       MOVE W-IDARTNR    TO INLC-ART-IDARTNR                              
113900       PERFORM IMS-ISRT-INLC01                                            
114000     END-IF                                                               
114100     MOVE W-DAINLEV      TO INLC-INL-DAINLEV                              
114200                                                                          
114300     PERFORM IMS-GU-WDK711                                                
114400     IF SEGMENT-FINNS                                                     
114500       MOVE SLAG-ADLAGOMR TO INLC-INL-ADLAGOMR                            
114600       MOVE SLAG-ADGANG   TO INLC-INL-ADGANG                              
114700       MOVE SLAG-ADPLATS  TO INLC-INL-ADPLATS                             
114800     ELSE                                                                 
114900       MOVE ZERO               TO INLC-INL-ADLAGOMR                       
115000       MOVE ZERO               TO INLC-INL-ADGANG                         
115100       MOVE ZERO               TO INLC-INL-ADPLATS                        
115200     END-IF                                                               
115300                                                                          
115400     MOVE NEJ            TO INLC-INL-FLMAKUL                              
115500                            INLC-INL-FLSKAKOL                             
115600                            INLC-INL-FLPRIO                               
115700     MOVE W-IDDC         TO INLC-INL-IDDC                                 
115800***  MOVE W-IDKUNDNR     TO INLC-INL-IDLEVNR                              
115900     MOVE W-IDKUNDNR     TO W-IDLEVNR-PIC9                                
116000     MOVE ZERO TO TALLY                                                   
116100     INSPECT W-IDLEVNR-PIC9 TALLYING TALLY FOR LEADING ZEROES             
116200     IF TALLY = 5                                                         
116300         MOVE SPACE TO INLC-INL-IDLEVNR                                   
116400     ELSE                                                                 
116500        MOVE W-IDLEVNR-PIC9(TALLY + 1:) TO INLC-INL-IDLEVNR               
116600     END-IF                                                               
116700     MOVE W-IDLOPNRM     TO INLC-INL-IDLOPNRM                             
116800     MOVE ZERO           TO INLC-INL-IDFAKT                               
116900     MOVE W-IDDISTR      TO INLC-INL-IDDISTR                              
117000     MOVE W-IDKUNDNR     TO INLC-INL-IDKUNDNR                             
117100     MOVE W-IDRAPPNR     TO INLC-INL-IDKUNDRF                             
117200     MOVE ZERO           TO INLC-INL-IDKOLLI                              
117300     MOVE '310'          TO INLC-INL-IDPTYP                               
117400     MOVE ZERO           TO INLC-INL-KDFRAKT                              
117500     MOVE SPACE          TO INLC-INL-KDKOLLI                              
117600                            INLC-INL-IDUSER-003                           
117700     IF OKOD-FL-SALDOBOK-RETUR = JA                                       
117800       MOVE 7             TO INLC-INL-KDRT                                
117900     ELSE                                                                 
118000       MOVE 77            TO INLC-INL-KDRT                                
118100     END-IF                                                               
118200     MOVE SPACE          TO INLC-INL-KDVALISO                             
118300     MOVE ZERO           TO INLC-INL-KVANTMOT                             
118400                            INLC-INL-KVART-SKROT                          
118500     MOVE KREE-LEV-KVLEVANM-BEKR TO INLC-INL-KVAVIS                       
118600     MOVE ZERO           TO INLC-INL-PRARTNTO                             
118700                            INLC-INL-PRKURS                               
118800                            INLC-INL-TIBERANK                             
118900     ACCEPT INLC-INL-TIINLMOT FROM DATE                                   
119000     MOVE ZERO           TO INLC-INL-TIINLINL                             
119100                            INLC-INL-TIINLMTI                             
119200                            INLC-INL-TIINLITI                             
119300     MOVE SPACE          TO INLC-INL-ADINLOMR                             
119400                            INLC-INL-IDANALYS                             
119500                            INLC-INL-IDKST                                
119510                            INLC-INL-IDDC-LEV                             
119600     MOVE NEJ            TO INLC-INL-FLTULLST                             
119700     MOVE ZERO           TO INLC-INL-IDKONTO                              
119900                            INLC-INL-KVTULRET                             
119910                            INLC-INL-KVRETUR                              
120000                            INLC-INL-KDAVVANT                             
120100                            INLC-INL-TIAVIDAT                             
120200                                                                          
120300     PERFORM IMS-ISRT-INLC11                                              
120400                                                                          
120500     PERFORM UNTIL SEGMENT-FINNS                                          
120600       SUBTRACT 1 FROM W-DAINLEV                                          
120700       MOVE W-DAINLEV TO INLC-INL-DAINLEV                                 
120800       PERFORM IMS-ISRT-INLC11                                            
120900     END-PERFORM                                                          
121000     .                                                                    
121100     EJECT                                                                
121200 HAG-UPPD-ARTC11-KVAKS-PAV  SECTION.                                      
121300                                                                          
121400     PERFORM IMS-GU-ARTC-ARTC01                                           
121500     PERFORM IMS-GHNP-ARTC-ARTC11                                         
121600                                                                          
121700     SUBTRACT KREE-LEV-KVLEVANM-BEKR  FROM ARTC-CLAG-KVAKS-PAV            
121800     ADD KREE-LEV-KVLEVANM-BEKR  TO ARTC-CLAG-KVAKS-CDC                   
121900                                                                          
122000     PERFORM IMS-REPL-ARTC-ARTC                                           
122100                                                                          
122200**** SKALL LOGGA SALDOFÖRÄNDRING I DATABAS WDL9. ********                 
122300     MOVE '-'                      TO LOGG-IDTECKEN-KVAKS-PAV             
122400     PERFORM S05-SKAPA-SALDOLOGG                                          
122500     .                                                                    
122600     EJECT                                                                
122700 HAH-CREATE-EVENT SECTION.                                                
122800                                                                          
122900     MOVE '001'                      TO Z430-REQU-IDMSGVER                
123000     MOVE 'Discrepancy'              TO Z430-REQU-IDEVENT                 
123100     MOVE 'UPDATE'                   TO Z430-REQU-IDEVENTTYP              
123200     MOVE FUNCTION CURRENT-DATE      TO Z430-REQU-TIMESTAMP               
123300     MOVE 'WAPIDISC'                 TO Z430-REQU-IDCPYTXT                
123400     MOVE WS-IDAPIDISCREF            TO Z430-IDAPIDISCREF                 
123500     MOVE '157'                      TO Z430-IDMSG                        
123600     MOVE 'Return goods is received' TO Z430-TEMFSINF                     
123700                                                                          
123800     MOVE 'WZ0430X '                 TO MSG-KDTRANS-1                     
123900     MOVE 'Z430'                     TO MSG-IDTRANS-1                     
124000     MOVE '1'                        TO MSG-KDMFSFOR-1                    
124100     MOVE 'W4079200'                 TO MSG1-MSG-KOM-IDSNDJOB             
124101     MOVE 'WZ0430I1'                 TO MSG1-MSG-KOM-IDCPYTXT             
124102                                                                          
124103*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
124104*    -- IDSNDNOD REFER AS EVE-XXXX (XXX -CUSTOMER DETAILS)                
124105*    -- IDCPYTXT REFER AS RETURN COPYBOOK                                 
124106**   MOVE 'WAPIDISC'          TO MSG1-MSG-KOM-IDCPYTXT                    
124107     STRING 'EVE' WS-EVENT-KUND WS-IDDISTR-EVENT                          
124108          DELIMITED BY SIZE INTO MSG1-MSG-KOM-IDSNDNOD                    
124109     ADD +1 TO MSG1-MSG-KOM-TIKLOCK                                       
124110     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
124120     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
124121                                                                          
124122     CALL W006KOM USING MSG-PCB                                           
124123                        0693-PCB                                          
124200                        WDP8-PCB                                          
124300                        MSG1-MSG-KOM-WMSGKOM                              
124400                        MSG-IO-AREA                                       
124500     IF MSG1-MSG-KOM-IDMFSMED NOT = SPACE                                 
124600        MOVE                                                              
124700        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
124800                                     TO FELTEXT                           
124900        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
124901     END-IF                                                               
124902     .                                                                    
124903     EJECT                                                                
124904 Z-FINIT          SECTION.                                                
124905                                                                          
124906     MOVE W-IDDC             TO WS2-IDDC                                  
124907     IF WS2-IDDC NOT = DCS-IDDC                                           
124908        MOVE WS2-IDDC TO W-IDDC-B6                                        
124909        PERFORM IMS-GU-WDB601                                             
124910     END-IF                                                               
124911                                                                          
124912     EVALUATE TRUE                                                        
124913       WHEN DCS-LAND-NON-VCC-OWNED OR                                     
124914            DCS-AUSTRALIA OR DCS-JAPAN OR                                 
124915           (DCS-SDC AND DCS-CHINA)                                        
124916         MOVE W-IDLOPNRM     TO LOPA-6018-IDLOPNRM-JP-AU-RET              
124917       WHEN DCS-NDC-NA                                                    
124918         MOVE W-IDLOPNRM     TO LOPA-6018-IDLOPNRM-NDC-RET                
124919       WHEN DCS-SDC                                                       
124920         MOVE W-IDLOPNRM     TO LOPA-6018-IDLOPNRM-SDC-RET                
124921       WHEN OTHER                                                         
124922         MOVE W-IDLOPNRM     TO LOPA-6018-IDLOPNRM-RET                    
124923     END-EVALUATE                                                         
124924                                                                          
124925     PERFORM IMS-REPL-LOPA-LOPA11                                         
124926                                                                          
125000     IF CHKP-ANT > 50                                                     
125100       MOVE INF-DISP-OMSTART       TO MSG-KOM-IDMFSMED                    
125200     ELSE                                                                 
125300       MOVE INF-UPDATE-DONE        TO MSG-KOM-IDMFSMED                    
125400     END-IF                                                               
125500                                                                          
125600     PERFORM IMS-ISRT-DISP-MSG                                            
125700     .                                                                    
125800     EJECT                                                                
125900 S01-TA-UT-IDLOPNRM SECTION.                                              
126000                                                                          
126100****************************************************************          
126200*** MID-IDDC = MSGI-IDDC = LEV-IDDC-RET SE PGM W4073300        *          
126300****************************************************************          
126400                                                                          
126500     IF DAT-TIAAVVD-GRP (3:3) = W-VVD                                     
126600         ADD +1             TO W-LLLL                                     
126700     ELSE                                                                 
126800       MOVE DAT-TIAAVVD-GRP (3:3) TO W-VVD                                
126900                                                                          
127000       MOVE MID-IDDC    TO WS2-IDDC                                       
127100       IF WS2-IDDC NOT = DCS-IDDC                                         
127200          MOVE WS2-IDDC  TO W-IDDC-B6                                     
127300          PERFORM IMS-GU-WDB601                                           
127400       END-IF                                                             
127500       EVALUATE TRUE                                                      
127600         WHEN DCS-LAND-NON-VCC-OWNED OR                                   
127700              DCS-AUSTRALIA OR DCS-JAPAN OR                               
127800             (DCS-SDC AND DCS-CHINA)                                      
127900           MOVE +6                 TO W-0                                 
128000           MOVE +1                 TO W-LLLL                              
128100         WHEN DCS-NDC-NA                                                  
128200           MOVE +4                 TO W-0                                 
128300           MOVE +1                 TO W-LLLL                              
128400         WHEN DCS-SDC                                                     
128500           MOVE +2                 TO W-0                                 
128600           MOVE +1                 TO W-LLLL                              
128700         WHEN OTHER                                                       
128800           MOVE +1                 TO W-0                                 
128900           MOVE +1                 TO W-LLLL                              
129000       END-EVALUATE                                                       
129100     END-IF                                                               
129200     CALL CHECK USING W-IDLOPNRM (2:7) FLT-LGD                            
129300          VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B               
129400     .                                                                    
129500     EJECT                                                                
129600 S02-SKAPA-ZZAC01 SECTION.                                                
129700                                                                          
129800     ACCEPT ZZAC01-TIAAMMDD      FROM DATE                                
129900     ACCEPT ZZAC01-TIKLOCK       FROM TIME                                
130000                                                                          
130100     ADD +1                      TO WS-IDLOGLOP                           
130200     MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                       
130300                                                                          
130400     MOVE WS-ZZAC01-LOGGPOST     TO ZZAC01-LOGGPOST                       
130500     MOVE WS-ZZAC01-SORTPOST     TO ZZAC01-SORTPOST                       
130600                                                                          
130700     PERFORM IMS-ISRT-ZZAC-LOGG                                           
130800     PERFORM UNTIL SEGMENT-FINNS                                          
130900       ACCEPT ZZAC01-TIAAMMDD      FROM DATE                              
131000       ACCEPT ZZAC01-TIKLOCK       FROM TIME                              
131100       ADD +1                      TO WS-IDLOGLOP                         
131200       MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                     
131300       PERFORM IMS-ISRT-ZZAC-LOGG                                         
131400     END-PERFORM                                                          
131500     .                                                                    
131600     EJECT                                                                
131700 S03-RED-W211FEL-GNRL SECTION.                                            
131800                                                                          
131900     MOVE ZERO                   TO W211FEL-SORT-FLT                      
132000     MOVE SPACE                  TO W211FEL-FILLER2                       
132100                                                                          
132200     MOVE 'R31'                  TO W211FEL-IDPTYP-S                      
132300     MOVE W-IDARTNR              TO W211FEL-SORTBGP                       
132400     MOVE 1                      TO W211FEL-KDFELMRK                      
132500     .                                                                    
132600     EJECT                                                                
132700 S04-GHNP-KREE-KREE11     SECTION.                                        
132800                                                                          
132900     MOVE NEJ                    TO OKOD-FL-RETILL                        
133000                                    OKOD-FL-INTERNUPPACKNING              
133100     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
133200                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
133300        PERFORM IMS-GHNP-KREE-KREE11                                      
133400        IF KREE-LEV-KDKREBEH(1:1) = 'Y' OR                                
133500           KREE-LEV-KDKREBEH(1:1) = 'J' OR                                
133600           KREE-LEV-KDKREBEH(1:1) = 'C'                                   
133700          IF KREE-LEV-IDARTNR NOT = 100                                   
133800            MOVE KREE-LEV-KDANMORS TO OKOD-KDANMORS                       
133900*--ANROPA KONTROLL AV ORSAKSKODER                                         
134000            CALL W418OKOD USING OKOD-W418OKOD                             
134100          END-IF                                                          
134200        END-IF                                                            
134300     END-PERFORM                                                          
134400                                                                          
134500     .                                                                    
134600     EJECT                                                                
134700 S05-SKAPA-SALDOLOGG SECTION.                                             
134800                                                                          
134900     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
135000                                                                          
135100     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
135200     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
135300                                   - WS-AAAAMMDD                          
135400     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
135500     COMPUTE LOGG-TIKLOCK-9KOMPL =  999999999                             
135600                                 - WS-TTMMSSTH                            
135700     MOVE 9                        TO LOGG-IDSEKVNR                       
135800     MOVE WS-CDC-SE                TO LOGG-IDDC                           
135900     MOVE 'DISC'                   TO LOGG-IDHUVTYP                       
136000     MOVE 'RET'                    TO LOGG-IDSUBTYP                       
136100     MOVE 'W4079200'               TO LOGG-IDPGM                          
136200     MOVE W-IDTRANS                TO LOGG-IDTRANS                        
136300     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
136400     MOVE SPACE                    TO LOGG-REF                            
136500     MOVE W-IDDISTR                TO LOGG-IDDISTR                        
136600     MOVE W-IDKUNDNR               TO LOGG-IDKUNDNR                       
136700     MOVE W-IDRAPPNR               TO LOGG-IDRAPPNR                       
136800     MOVE '+'                      TO LOGG-IDTECKEN-KVAKS                 
136900     MOVE ' '                      TO LOGG-IDTECKEN-KVEFRS                
137000     MOVE ' '                      TO LOGG-IDTECKEN-KVLS                  
137100     MOVE KREE-LEV-KVLEVANM-BEKR   TO LOGG-KVART-SALDO                    
137200                                                                          
137300     COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC                             
137400                        + ARTC-CLAG-KVAKS-T                               
137500                                                                          
137600     MOVE ARTC-CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                      
137700     MOVE ARTC-CLAG-KVEFRS         TO LOGG-KVEFRS                         
137800     MOVE ARTC-CLAG-KVLS           TO LOGG-KVLS                           
137900     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
138000                                                                          
138100     PERFORM S06-IMS-ISRT-WDL901                                          
138200     .                                                                    
138300     EJECT                                                                
138400 S06-IMS-ISRT-WDL901 SECTION.                                             
138500     PERFORM IMS-ISRT-WDL901                                              
138600     IF SEGMENT-FINNS-REDAN                                               
138700       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
138800         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
138900         PERFORM IMS-ISRT-WDL901                                          
139000       END-PERFORM                                                        
139100     END-IF                                                               
139200     .                                                                    
139300     EJECT                                                                
139310 S07-GET-KDKUNDKAT SECTION.                                               
139320                                                                          
139330     MOVE W-IDDISTR         TO W-IDDISTR-WDB2                             
139340     MOVE W-IDKUNDNR        TO W-IDKUNDNR-WDB2                            
139341     MOVE NEJ               TO SW-LYNK-NON-API                            
139350                                                                          
139360     PERFORM IMS-GU-WDB201                                                
139370     IF SEGMENT-FINNS                                                     
139380        IF GMT-KDKUNDKAT  = 03                                            
139390            MOVE JA         TO SW-LYNK-NON-API                            
139391        END-IF                                                            
139392     END-IF                                                               
139393     .                                                                    
139394     EJECT                                                                
139400 S20-SKAPA-WDK7        SECTION.                                           
139500                                                                          
139600     MOVE ALL '+'           TO WDK7-W005WDK7                              
139700     MOVE 'WDK711'          TO WDK7-IDSEGM                                
139800     MOVE W-IDARTNR         TO WDK7-IDARTNR-KFB                           
139900     MOVE KREE-LEV-IDDC-RET TO WDK7-IDDC-KFB                              
140000                               WDK7-IDDC                                  
140100     MOVE FUNCTION CURRENT-DATE(1:8) TO WDK7-DASPSEA                      
140200                                                                          
140300     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB WDK7-PCB         
140400                                                                          
140500     .                                                                    
140600     EJECT                                                                
140700                                                                          
140800* --- IMS SEKTIONER ---                                                   
140900     SKIP3                                                                
141000 IMS-GET-MSG SECTION.                                                     
141100                                                                          
141200     MOVE '  QC' TO GODK-STATUSKODER                                      
141300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
141400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
141500     PERFORM IMS-STATUSKONTROLL                                           
141600     .                                                                    
141700     SKIP3                                                                
141800 IMS-GN-MSG SECTION.                                                      
141900                                                                          
142000     MOVE '  ' TO GODK-STATUSKODER                                        
142100     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
142200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500     SKIP3                                                                
142600 IMS-ISRT-DISP-MSG SECTION.                                               
142700                                                                          
142800     MOVE    '  '             TO GODK-STATUSKODER                         
142900     CALL    CBLTDLI          USING ISRT 0693-PCB MSG-KOM-WMSGKOM         
143000     MOVE    0693-STATUS-CODE TO STATUS-WS                                
143100     PERFORM IMS-STATUSKONTROLL                                           
143200     .                                                                    
143300     EJECT                                                                
143400                                                                          
143500 IMS-GET-RETA-RETA01 SECTION.                                             
143600                                                                          
143700     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
143800          DELIMITED BY SIZE INTO SSA1                                     
143900     MOVE '  ' TO GODK-STATUSKODER                                        
144000     CALL CBLTDLI USING GU RETA-PCB DLI-IO-AREA SSA1                      
144100     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
144200     PERFORM IMS-STATUSKONTROLL                                           
144300     .                                                                    
144400     EJECT                                                                
144500 IMS-GHU-KREE-KREE01 SECTION.                                             
144600                                                                          
144700     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
144800          DELIMITED BY SIZE INTO SSA1                                     
144900     MOVE '  ' TO GODK-STATUSKODER                                        
145000     CALL CBLTDLI USING GHU KREE-PCB DLI-IO-AREA2 SSA1                    
145100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
145200     PERFORM IMS-STATUSKONTROLL                                           
145300     .                                                                    
145400     SKIP3                                                                
145500 IMS-GHNP-KREE-KREE11 SECTION.                                            
145600                                                                          
145700     MOVE 'WLKREE11 ' TO SSA1                                             
145800     MOVE '  GE' TO GODK-STATUSKODER                                      
145900     CALL CBLTDLI USING GHNP KREE-PCB DLI-IO-AREA2 SSA1                   
146000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
146100     PERFORM IMS-STATUSKONTROLL                                           
146200     .                                                                    
146300     SKIP3                                                                
146400 IMS-REPL-KREE-KREE SECTION.                                              
146500                                                                          
146600     MOVE '  ' TO GODK-STATUSKODER                                        
146700     CALL CBLTDLI USING REPL KREE-PCB DLI-IO-AREA2                        
146800     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
146900     PERFORM IMS-STATUSKONTROLL                                           
147000     .                                                                    
147100     EJECT                                                                
147200 IMS-GU-ARTC-ARTC01 SECTION.                                              
147300                                                                          
147400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
147500          DELIMITED BY SIZE INTO SSA1                                     
147600     MOVE '  ' TO GODK-STATUSKODER                                        
147700     CALL CBLTDLI USING GU   ARTC-PCB DLI-IO-AREA SSA1                    
147800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
147900     PERFORM IMS-STATUSKONTROLL                                           
148000     .                                                                    
148100     SKIP3                                                                
148200 IMS-GHNP-ARTC-ARTC11 SECTION.                                            
148300                                                                          
148400     MOVE 'WLARTC11 ' TO SSA1                                             
148500     MOVE '  ' TO GODK-STATUSKODER                                        
148600     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
148700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
148800     PERFORM IMS-STATUSKONTROLL                                           
148900     .                                                                    
149000     SKIP3                                                                
149100 IMS-GHU-ARTC-ARTC11 SECTION.                                             
149200                                                                          
149300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
149400          DELIMITED BY SIZE INTO SSA1                                     
149500     MOVE 'WLARTC11 ' TO SSA2                                             
149600     MOVE '  ' TO GODK-STATUSKODER                                        
149700     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
149800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
149900     PERFORM IMS-STATUSKONTROLL                                           
150000     .                                                                    
150100     SKIP3                                                                
150200 IMS-REPL-ARTC-ARTC SECTION.                                              
150300                                                                          
150400     MOVE '  ' TO GODK-STATUSKODER                                        
150500     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
150600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
150700     PERFORM IMS-STATUSKONTROLL                                           
150800     .                                                                    
150900     EJECT                                                                
151000 IMS-GHU-WDK711 SECTION.                                                  
151100                                                                          
151200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
151300          DELIMITED BY SIZE INTO SSA1                                     
151400     STRING 'WDK711  (IDDC     =' W-IDDC     ')'                          
151500          DELIMITED BY SIZE INTO SSA2                                     
151600     MOVE '  GE' TO GODK-STATUSKODER                                      
151700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA SSA1 SSA2                
151800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
151900     PERFORM IMS-STATUSKONTROLL                                           
152000     .                                                                    
152100     SKIP3                                                                
152200 IMS-GU-WDK711 SECTION.                                                   
152300                                                                          
152400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
152500          DELIMITED BY SIZE INTO SSA1                                     
152600     STRING 'WDK711  (IDDC     =' W-IDDC     ')'                          
152700          DELIMITED BY SIZE INTO SSA2                                     
152800     MOVE '  GE' TO GODK-STATUSKODER                                      
152900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
153000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
153100     PERFORM IMS-STATUSKONTROLL                                           
153200     .                                                                    
153300     SKIP3                                                                
153310 IMS-GU-WDK711-CN SECTION.                                                
153320                                                                          
153330     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
153340          DELIMITED BY SIZE INTO SSA1                                     
153350     STRING 'WDK711  (IDDC1    =' W-IDDC1-CN                              
153351                    '&PRAVCOST >' W-PRAVCOST-X ')'                        
153360          DELIMITED BY SIZE INTO SSA2                                     
153370     MOVE '  ' TO GODK-STATUSKODER                                        
153380     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
153390     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
153391     PERFORM IMS-STATUSKONTROLL                                           
153392     .                                                                    
153393     SKIP3                                                                
153400 IMS-REPL-WDK7 SECTION.                                                   
153500                                                                          
153600     MOVE '  ' TO GODK-STATUSKODER                                        
153700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA                         
153800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
153900     PERFORM IMS-STATUSKONTROLL                                           
154000     .                                                                    
154100     EJECT                                                                
154200 IMS-GET-INLE-INLE01 SECTION.                                             
154300                                                                          
154400     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
154500          DELIMITED BY SIZE INTO SSA1                                     
154600     MOVE '  GE' TO GODK-STATUSKODER                                      
154700     CALL CBLTDLI USING GU   INLE-PCB DLI-IO-AREA SSA1                    
154800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
154900     PERFORM IMS-STATUSKONTROLL                                           
155000     .                                                                    
155100     SKIP3                                                                
155200 IMS-ISRT-INLE-INLE01 SECTION.                                            
155300                                                                          
155400     MOVE 'WLINLE01 ' TO SSA1                                             
155500     MOVE '  ' TO GODK-STATUSKODER                                        
155600     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1                    
155700     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
155800     PERFORM IMS-STATUSKONTROLL                                           
155900     .                                                                    
156000     SKIP3                                                                
156100 IMS-ISRT-INLE-INLE11 SECTION.                                            
156200                                                                          
156300     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
156400          DELIMITED BY SIZE INTO SSA1                                     
156500     MOVE 'WLINLE11 ' TO SSA2                                             
156600     MOVE '  II' TO GODK-STATUSKODER                                      
156700     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2               
156800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
156900     PERFORM IMS-STATUSKONTROLL                                           
157000     .                                                                    
157100     EJECT                                                                
157200 IMS-ISRT-INLE-INLE21 SECTION.                                            
157300                                                                          
157400     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
157500          DELIMITED BY SIZE INTO SSA1                                     
157600     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
157700          DELIMITED BY SIZE INTO SSA2                                     
157800     MOVE 'WLINLE21 ' TO SSA3                                             
157900     MOVE '  ' TO GODK-STATUSKODER                                        
158000     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
158100     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
158200     PERFORM IMS-STATUSKONTROLL                                           
158300     .                                                                    
158400     EJECT                                                                
158500 IMS-GU-INLC01      SECTION.                                              
158600                                                                          
158700     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
158800          DELIMITED BY SIZE INTO SSA1                                     
158900     MOVE '  GE' TO GODK-STATUSKODER                                      
159000     CALL CBLTDLI USING GU   INLC-PCB DLI-IO-AREA SSA1                    
159100     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
159200     PERFORM IMS-STATUSKONTROLL                                           
159300     .                                                                    
159400     SKIP3                                                                
159500 IMS-ISRT-INLC01      SECTION.                                            
159600                                                                          
159700     MOVE 'WLINLC01 ' TO SSA1                                             
159800     MOVE '  ' TO GODK-STATUSKODER                                        
159900     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA SSA1                    
160000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
160100     PERFORM IMS-STATUSKONTROLL                                           
160200     .                                                                    
160300     SKIP3                                                                
160400 IMS-ISRT-INLC11      SECTION.                                            
160500                                                                          
160600     MOVE 'WLINLC11 ' TO SSA1                                             
160700     MOVE '  II' TO GODK-STATUSKODER                                      
160800     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA SSA1                    
160900     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
161000     PERFORM IMS-STATUSKONTROLL                                           
161100     .                                                                    
161200     SKIP3                                                                
161300 IMS-ISRT-ZZAC-LOGG SECTION.                                              
161400                                                                          
161500     MOVE 'WLZZAC01 ' TO SSA1                                             
161600     MOVE '  II' TO GODK-STATUSKODER                                      
161700     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
161800     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
161900     PERFORM IMS-STATUSKONTROLL                                           
162000     .                                                                    
162100     EJECT                                                                
162200 IMS-GHU-LOPA-LOPA11 SECTION.                                             
162300     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
162400          DELIMITED BY SIZE INTO SSA1                                     
162500     STRING 'W6LOPA11(KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
162600          DELIMITED BY SIZE INTO SSA2                                     
162700     MOVE '    ' TO GODK-STATUSKODER                                      
162800     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA3 SSA1 SSA2               
162900     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
163000     PERFORM IMS-STATUSKONTROLL                                           
163100     .                                                                    
163200     SKIP3                                                                
163300 IMS-REPL-LOPA-LOPA11 SECTION.                                            
163400     MOVE '    ' TO GODK-STATUSKODER                                      
163500     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA3                        
163600     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
163700     PERFORM IMS-STATUSKONTROLL                                           
163800     .                                                                    
163900     SKIP3                                                                
164000 IMS-ISRT-EKOTRANS  SECTION.                                              
164100     MOVE 'WLFILB01 ' TO SSA1                                             
164200     MOVE '  II' TO GODK-STATUSKODER                                      
164300     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA4 SSA1                   
164400     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
164500     PERFORM IMS-STATUSKONTROLL                                           
164600     .                                                                    
164700     EJECT                                                                
164800 IMS-ISRT-WDL901 SECTION.                                                 
164900                                                                          
165000     MOVE 'WLLOGA01 ' TO SSA1                                             
165100     MOVE '  II' TO GODK-STATUSKODER                                      
165200     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
165300     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
165400     PERFORM IMS-STATUSKONTROLL                                           
165500     .                                                                    
165600     EJECT                                                                
165700 IMS-ISRT-WDR901 SECTION.                                                 
165800                                                                          
165900     MOVE 'WLSAPA01 ' TO SSA1                                             
166000     MOVE '  II' TO GODK-STATUSKODER                                      
166100     CALL CBLTDLI USING ISRT WLSAPA-PCB WLSAPA01 SSA1                     
166200     MOVE WLSAPA-STATUS-CODE TO STATUS-WS                                 
166300     PERFORM IMS-STATUSKONTROLL                                           
166400     .                                                                    
166500     EJECT                                                                
166600 IMS-ISRT-WDR601 SECTION.                                                 
166700                                                                          
166800     MOVE 'WDR601  ' TO SSA1                                              
166900     MOVE '  II' TO GODK-STATUSKODER                                      
167000     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-WDR601 SSA1                  
167100     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
167200     PERFORM IMS-STATUSKONTROLL                                           
167300     .                                                                    
167400     EJECT                                                                
167500 IMS-GU-WDB601    SECTION.                                                
167600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
167700          DELIMITED BY SIZE INTO SSA1                                     
167800     MOVE '  ' TO GODK-STATUSKODER                                        
167900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
168000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
168100     PERFORM IMS-STATUSKONTROLL                                           
168200     .                                                                    
168300                                                                          
168400 IMS-GU-WDB601-RET SECTION.                                               
168500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-RET-X ')'                     
168600          DELIMITED BY SIZE INTO SSA1                                     
168700     MOVE '  ' TO GODK-STATUSKODER                                        
168800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-RET SSA1             
168900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
169000     PERFORM IMS-STATUSKONTROLL                                           
169100     .                                                                    
169110 IMS-GU-WDB201    SECTION.                                                
169120     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
169130          DELIMITED BY SIZE INTO SSA1                                     
169140     MOVE '  ' TO GODK-STATUSKODER                                        
169150     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-B201 SSA1                 
169160     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
169170     PERFORM IMS-STATUSKONTROLL                                           
169180     .                                                                    
169190     EJECT                                                                
169200 IMS-GU-DC71-WDL711 SECTION.                                              
169300                                                                          
169400     STRING 'WLOIGA01(IDARTNR  =' W-IDARTNR-X ')'                         
169500          DELIMITED BY SIZE INTO SSA1                                     
169600     STRING 'WLOIGA11(IDDC     =' W-IDDC   ')'                            
169700          DELIMITED BY SIZE INTO SSA2                                     
169800     MOVE '  GE' TO GODK-STATUSKODER                                      
169900     CALL CBLTDLI USING GU OIGA-PCB DLI-IO-OIGA11 SSA1 SSA2               
170000     MOVE OIGA-STATUS-CODE TO STATUS-WS                                   
170100     PERFORM IMS-STATUSKONTROLL                                           
170200     .                                                                    
170300     SKIP3                                                                
170400 IMS-STATUSKONTROLL SECTION.                                              
170500                                                                          
170600     SET STATUS-IX TO 1                                                   
170700     SEARCH GODK-STATUS                                                   
170800       AT END                                                             
170900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
171000         DELIMITED BY SIZE INTO FELTEXT                                   
171100         CALL FELLOG                                                      
171200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
171300         CONTINUE                                                         
171400     END-SEARCH                                                           
171500     .                                                                    
