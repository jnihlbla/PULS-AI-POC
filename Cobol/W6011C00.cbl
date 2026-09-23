000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6011C00.                                                
000400 AUTHOR.         MÅNS SAMUELSSON.                                         
000500 DATE-WRITTEN.   94/11/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄGGER UPP R34 OR PÅ REGISTER                                    
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
001200*                              WLINLC (WDL6)                              
001300*                              WLARTC (WDK6)                              
001400*                              WDK7                                       
001500*                              WL4505/4506                                
001600*        PROGRAMMET UPPDATERAR WLFILB (WDR8)                              
001700*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
001800*        PROGRAMMET LOGGAR SALDO FÖRÄNDRINGAR PÅ WLLOGA (WDL9)            
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T11C                                              
002100*        MID:         W6I11C01                                            
002200*                                                                         
002300*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100*    -COPY WY2000W1                                                       
003200     SKIP3                                                                
003300 77  IDPGM                       PIC X(08)     VALUE 'W6011C00'.          
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80)     VALUE SPACE.               
003700 77  KDRC-DISPLAY                PIC Z(5).                                
003800                                                                          
003900 01  WS-IDLOPNRM                 PIC 9(9)      VALUE ZERO.                
004000 01  W-0VVDLLLLK  REDEFINES WS-IDLOPNRM.                                  
004100     03 FILLER                   PIC 9(1).                                
004200     03 W-VVD                    PIC 9(3).                                
004300     03 W-LLLL                   PIC 9(4).                                
004400     03 W-K                      PIC 9(1).                                
004500                                                                          
004600 01  SPAR-TIAAVVD-GRP            PIC 9(5)      VALUE ZERO.                
004700 01  W-RETULF                    PIC S9(3)V9(4) VALUE +0.                 
004800                                                                          
004900 77  JA                          PIC X         VALUE 'J'.                 
005000 77  NEJ                         PIC X         VALUE 'N'.                 
005100 77  IX1                         PIC 9(2)    VALUE ZERO.                  
005200 77  IX2                         PIC 9(2)    VALUE ZERO.                  
005300 77  INDX                        PIC 9(3)    VALUE ZERO.                  
005400                                                                          
005500 77  NOLL-RAKNARE                PIC S9(5)     VALUE ZERO COMP-3.         
005600 77  WS-SAP-X-IDAVINR            PIC X(7)      VALUE SPACE.               
005700 77  WS-SAP-MM-POST              PIC X(1)      VALUE SPACE.               
005800 77  W-DATE-AAMM                 PIC 9(4)      VALUE ZERO.                
005900                                                                          
006000 77  MID-IX                      PIC S9(9)     VALUE +1 COMP SYNC.        
006100                                                                          
006200 77  IX                          PIC S9(9)     VALUE +0 COMP SYNC.        
006300                                                                          
006400 77  WS-FLPALAGG                 PIC X          VALUE 'J'.                
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006800                                                                          
006900 01  WS-IDDC-LOCAL.                                                       
007000     03  FILLER                  PIC X(4)       VALUE 'IDDC'.             
007100     03  IDDC-WS                 PIC X(2).                                
007200                                                                          
007300 01  W-PRKURS                    PIC S9(5)V9(5) VALUE +0   COMP-3.        
007400 01  W-REVALUTA                  PIC S9(5)      VALUE +0   COMP-3.        
007500                                                                          
007600 01  WS-SUDIRLON                 PIC S9(7)V9(2) VALUE +0 COMP-3.          
007700 01  WS-SUDIRMTRL                PIC S9(7)V9(2) VALUE +0 COMP-3.          
007800 01  WS-SUHEMT                   PIC S9(7)V9(2) VALUE +0 COMP-3.          
007900 01  WS-SUARTSTD                 PIC S9(9)V9(2) VALUE +0 COMP-3.          
008000                                                                          
008100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008200                                                                          
008300 77  W-IDTRANS                   PIC X(4)       VALUE SPACE.              
008400     88  EGEN-MID                               VALUE '611C'.             
008500     88  GODK-MID                               VALUE '6100'.             
008600                                                                          
008700 77  TRAEFF-SW                   PIC X          VALUE 'N'.                
008800     88  TRAEFF                                 VALUE 'J'.                
008900                                                                          
009000 77  AVROP-SW                    PIC X          VALUE 'N'.                
009100     88  AVROP-SAKNAS                           VALUE 'N'.                
009200                                                                          
009300 77  PRIS-FINNS-SW               PIC X          VALUE 'N'.                
009400   88 PRIS-FINNS                                VALUE 'J'.                
009500   88 PRIS-FINNS-INTE                           VALUE 'N'.                
009600 01  AKTUELL-TID.                                                         
009700     03  AKTUELL-TTMM-LOC    PIC 9(4).                                    
009800     03  FILLER              PIC 9(4).                                    
009900                                                                          
010000 77  DISP-SVAR                   PIC X          VALUE 'N'.                
010100 77  WS-FLTRACK                  PIC X(1)    VALUE 'N'.                   
010200 77  WS-TID                      PIC 9(9)          VALUE ZERO.            
010300 77  WS-VALD-KVAVIS              PIC 9(6)    VALUE ZERO.                  
010400 77  WS-TEMP-KVAVIS              PIC 9(6)    VALUE ZERO.                  
010500 77  WS-INL-KVAVIS               PIC 9(6)    VALUE ZERO.                  
010600 77  WS-TRCK-KVANTMOT            PIC S9(7)  COMP-3.                       
010700 77  WS-SLAG-KVLS                PIC S9(7)  COMP-3.                       
010800 77  IDTRACK-QTY-SW              PIC X       VALUE 'N'.                   
010900     88  IDTRACK-QTY-DONE                    VALUE 'J'.                   
011000     88  IDTRACK-QTY-NOT-DONE                VALUE 'N'.                   
011100                                                                          
011200 77  WS-IDDC-NUM                 PIC 9(2).                                
011300     EJECT                                                                
011400*      --- VALID IDDC CODES                                               
011500*                                                                         
011600*01    -COPY WWDCLAND                                                     
011700       EJECT                                                              
011800*01    -COPY WWDC99                                                       
011900       EJECT                                                              
012000*01    -COPY WWDCKONS                                                     
012100       EJECT                                                              
012200 01  WS-DAAVIDAT                 PIC 9(8).                                
012300 01  SPAR-ARTC01-IDLEVNR         PIC  X(5)      VALUE SPACE.              
012400 01  SPAR-ARTC01-KDPRODSL        PIC S9(3)      VALUE +0 COMP-3.          
012500 01  SPAR-ARTC01-KDSORT          PIC  X(2)      VALUE SPACE.              
012600 01  SPAR-ARTC11-KDPSLLOC        PIC 9(2)       VALUE ZERO.               
012700 01  SPAR-ARTC21-PRARTBES-PR     PIC S9(7)V9(2) VALUE +0 COMP-3.          
012800 01  SPAR-ARTC21-PRARTBEL-PR     PIC S9(7)V9(5) VALUE +0 COMP-3.          
012900 01  SPAR-ARTC21-PRARTBEL-SUM    PIC S9(7)V9(5) VALUE +0 COMP-3.          
013000 01  SPAR-ARTC21-KDVALISO        PIC X(3)       VALUE SPACE.              
013100 01  SPARA-KVLS-OLD              PIC S9(7) COMP-3 VALUE ZERO.             
013200 01  SPARA-KVLS                  PIC S9(7) COMP-3 VALUE ZERO.             
013300 01  SPAR-PRKURS                 PIC S9(5)V9(5) VALUE +0   COMP-3.        
013400*                                                                         
013500 01  W-PRL-DADAT                 PIC 9(8)       VALUE ZERO.               
013600 01  WS-ARTC23-IDAVTAL           PIC 9(13)      VALUE ZERO.               
013700 01  WS-DAINLEV                  PIC 9(16).                               
013800*                                                                         
013900 01  WS-IDLOGLOP                 PIC S9(1)      VALUE ZERO.               
014000*                                                                         
014100 01  WS.                                                                  
014200  02 WS-DAINLEV-WDL6             PIC 9(16)      VALUE ZERO.               
014300  02 FILLER REDEFINES WS-DAINLEV-WDL6.                                    
014400   03  WS-DAINLEV-SEKEL          PIC 9(2).                                
014500   03  WS-DAINLEV-DATUM          PIC 9(6).                                
014600   03  WS-DAINLEV-TID            PIC 9(8).                                
014700  02 WS-TIAAVV                   PIC 9(04)      VALUE ZERO.               
014800*                                                                         
014900 01  DAGENS-DATUM                PIC 9(6)       VALUE ZERO.               
015000*     -- DAGENS-DATUM MED SEKEL-SIFFRA                                    
015100 01      WS-DAGENS-DATUM         PIC 9(8)       VALUE ZERO.               
015200 01      FILLER REDEFINES WS-DAGENS-DATUM.                                
015300   03    WS-DAGENS-SEKEL         PIC 9(2).                                
015400   03    WS-IDAG                 PIC 9(6).                                
015500 01  WS-DAGENS-TID.                                                       
015600     03 DAGENS-TID               PIC 9(8)    VALUE ZERO.                  
015700     03 FILLER REDEFINES DAGENS-TID.                                      
015800         05 DAGENS-HHMMSS        PIC 9(6).                                
015900         05 FILLER               PIC 9(2).                                
016000                                                                          
016100     03  WS-DATE-YYMMDD            PIC 9(06).                             
016200     03  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                          
016300         05  WS-DATE-YYMM          PIC 9(04).                             
016400         05  WS-DATE-DD            PIC 9(02).                             
016500 01  WS-DAGENS-DATUM-GRP.                                                 
016600     03 DAGENS-AA               PIC 9(2).                                 
016700     03 DAGENS-AAMMDD           PIC 9(6).                                 
016800                                                                          
016900 01  WLOGG-TID                   PIC S9(9)      VALUE ZERO.               
017000 01  LOGG-DATUM                  PIC S9(8)      VALUE ZERO.               
017100*                                                                         
017200 01  WS-TIAAAAMMDDTTMMSSTH       PIC 9(16)      VALUE ZERO.               
017300 01  FILLER                      REDEFINES WS-TIAAAAMMDDTTMMSSTH.         
017400     03 WS-TISEKEL               PIC 9(2).                                
017500     03 WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                                
017600     03 WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                                
017700*                                                                         
017800*     -- IDLOPNRM I VALFRI FORM                                           
017900 01      FILLER.                                                          
018000  02     WS-IDLOPNRM-AAVVDLLLLK  PIC 9(10)      VALUE ZERO.               
018100  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
018200   03    WS-IDLOPNRM-AA          PIC 9(2).                                
018300   03    WS-IDLOPNRM-VVDLLLLK    PIC 9(8).                                
018400   03    FILLER                  REDEFINES WS-IDLOPNRM-VVDLLLLK.          
018500    04   WS-IDLOPNRM-VVDLLLL     PIC 9(7).                                
018600    04   FILLER                  PIC X(1).                                
018700  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
018800   03    WS-IDLOPNRM-AAVVDLLLL   PIC 9(9).                                
018900   03    FILLER                  PIC X(1).                                
019000*                                                                         
019100 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
019200     03 FLT-LGD                  PIC S9(1) COMP SYNC VALUE +7.            
019300     03 VAEGNINGSTAL             PIC 9(7)       VALUE 2121212.            
019400     03 VAEGNTAL-LGD             PIC S9 COMP SYNC VALUE +7.               
019500     03 MODUL-10-11              PIC 9(2)       VALUE 10.                 
019600     03 ALT-A-B                  PIC X(1)       VALUE 'B'.                
019700*                                                                         
019800*    --- LOGG-TRANSAR                                                     
019900 01  WS-ZZAC01.                                                           
020000     03 WS-ZZAC01-LOGGPOST       PIC X(90)      VALUE SPACE.              
020100     03 FILLER                   REDEFINES WS-ZZAC01-LOGGPOST.            
020200      04 WS-ZZAC01-LOGGPOST-1-3  PIC X(3).                                
020300      04 WS-ZZAC01-LOGGPOST-4-90 PIC X(87).                               
020400     03 WS-ZZAC01-SORTPOST       PIC X(36)      VALUE SPACE.              
020500*    --- WDD9-FAELT                                                       
020600 01  WS-INLB.                                                             
020700     03 WS-INLB23.                                                        
020800      04 WS-INLB23-TIAVROP-INL   PIC S9(5)      VALUE ZERO COMP-3.        
020900*                                                                         
021000     03 WS-KV-LPLAN              PIC S9(7)      VALUE ZERO COMP-3.        
021100     03 WS-KV-OBOK               PIC S9(7)      VALUE ZERO COMP-3.        
021200     03 WS-IDAVINR               PIC 9(7)       VALUE ZERO.               
021300     03 WS-BSKKVAR-GGR-10        PIC S9(7)      VALUE ZERO COMP-3.        
021400     03 WS-BSKURS                PIC S9(7)      VALUE ZERO COMP-3.        
021500 77  FIRST-REC-TRANS-SW          PIC X       VALUE 'N'.                   
021600     88  NOT-FIRST-REC-TRANS                 VALUE 'N'.                   
021700     88  FIRST-REC-TRANS                     VALUE 'J'.                   
021800 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
021900 77  W-IDARTNR-EDIT-X            PIC Z(9).                                
022000 77  WS-ADDRESS-WHSTOCKA         PIC X(50)                                
022100       VALUE 'CARPARTS.PULS.WHSTOCKADJ'.                                  
022200 77  WS-ADDRESS-MQASYNC          PIC X(50)                                
022300       VALUE 'CARPARTS.PULS.MQASYNC'.                                     
022400                                                                          
022500                                                                          
022600 01  GENERELLA-SUBPROGRAM.                                                
022700     03  WDATKONV                PIC X(8)       VALUE 'WDATKONV'.         
022800     03  W005INIT                PIC X(8)       VALUE 'W005INIT'.         
022900     03  W005WDK7                PIC X(8)       VALUE 'W005WDK7'.         
023000     03  W005WDL7                PIC X(8)       VALUE 'W005WDL7'.         
023100     03  WZ01SEND                PIC X(8)       VALUE 'WZ01SEND'.         
023200     03  CBLTDLI                 PIC X(8)       VALUE 'CBLTDLI '.         
023300     03  FELLOG                  PIC X(8)       VALUE 'FELLOG  '.         
023400     03  CHECK                   PIC X(8)       VALUE 'CHECK   '.         
023500     03  W510CURR                PIC X(8)       VALUE 'W510CURR'.         
023600     03  WL01TIDZ                PIC X(8)       VALUE 'WL01TIDZ'.         
023700     03  ABEND                   PIC X(8)       VALUE 'ABEND   '.         
023800     EJECT                                                                
023900*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
024000 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
024100*01  -COPY WZ01SEND                                                       
024200     EJECT                                                                
024300 01 FILLER                       PIC X(8)       VALUE 'W005WDK7'.         
024400*   -COPY W005WDK7                                                        
024500     EJECT                                                                
024600 01 FILLER                       PIC X(8)       VALUE 'W005WDL7'.         
024700*   -COPY W005WDL7                                                        
024800     EJECT                                                                
024900*01  -COPY WMSGINIT                                                       
025000     EJECT                                                                
025100*01  -COPY W510CURR                                                       
025200     EJECT                                                                
025300 01  MESSAGE-CODES.                                                       
025400     03  INF-UPPDATE-DONE        PIC X(3)       VALUE '101'.              
025500*    NOTAFISCAL                                                           
025600 01  NOTF-AREA.                                                           
025700*    03  -COPY W611NOTF                                                   
025800     EJECT                                                                
025900 01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
026000*01  -COPY WZ04PROP                                                       
026100     EJECT                                                                
026200*01  -COPY W211310  -PRE W330-                                            
026300     EJECT                                                                
026400*01  -COPY W211FEL  -PRE W211FEL-                                         
026500     EJECT                                                                
026600*01  -COPY W211M107 -PRE M107-                                            
026700     EJECT                                                                
026800*01  -COPY W211M108 -PRE M108-                                            
026900     EJECT                                                                
027000*01  -COPY W211M109 -PRE M109-                                            
027100     EJECT                                                                
027200*01  -COPY W211M113 -PRE M113-                                            
027300     EJECT                                                                
027400*01  -COPY W211M117 -PRE M117-                                            
027500     EJECT                                                                
027600*01  -COPY W601R34A -PRE LOGG34-                                          
027700     EJECT                                                                
027800*01  -COPY WDATAREA                                                       
027900     EJECT                                                                
028000 01  FILLER              PIC X(16) VALUE 'WL01TIDZ-AREA'.                 
028100*01  -COPY WL01TIDZ -PRE TIDZ-                                            
028200     EJECT                                                                
028300*01  -COPY WWPRODSL                                                       
028400     EJECT                                                                
028500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
028600*                                                                         
028700 01  FILLER                      PIC X(16)      VALUE 'MID-AREA'.         
028800     SKIP3                                                                
028900*01  MID -COPY W6I11C01                                                   
029000     EJECT                                                                
029100 01  FILLER                      PIC X(16)      VALUE 'MFS-AREA'.         
029200     SKIP3                                                                
029300*01  -COPY WMSGKOM                                                        
029400     EJECT                                                                
029500*01  -COPY WMFSAREA                                                       
029600     EJECT                                                                
029700*01  -COPY WMSGAREA                                                       
029800     EJECT                                                                
029900 01  FILLER         PIC X(24) VALUE 'DLI-IO-ARTC11'.                      
030000 01  DLI-IO-ARTC11.                                                       
030100*    03  -COPY WDK611                                                     
030200     EJECT                                                                
030300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
030400*                                                                         
030500     EJECT                                                                
030600 01  FILLER                      PIC X(16)      VALUE 'IMS-WS'.           
030700     SKIP3                                                                
030800 01  NYCKLAR-TILL-DLI.                                                    
030900     03  W-IDARTNR-X.                                                     
031000         05  W-IDARTNR           PIC S9(9)      VALUE ZERO COMP-3.        
031100     03  W-DAINLEV-X.                                                     
031200         05  W-DAINLEV           PIC 9(16)      VALUE ZERO.               
031300     03  W-WDD901KY-X.                                                    
031400         05  W-IDARTNR-D9        PIC S9(9)      VALUE ZERO COMP-3.        
031500         05  W-IDDC-D9           PIC X(2)       VALUE SPACE.              
031600                                                                          
031700     03  W-IDDC-K7-X.                                                     
031800         05  W-IDDC-K7           PIC X(2)       VALUE SPACE.              
031900     03  W-IDLEVNR-X.                                                     
032000         05  W-IDLEVNR           PIC  X(5)      VALUE SPACE.              
032100     03  W-IDLEVNR-21-X.                                                  
032200         05  W-IDLEVNR-21        PIC X(5)       VALUE LOW-VALUE.          
032300     03  W-IDLEVNR-PR-X.                                                  
032400         05  W-IDLEVNR-PR       PIC X(5) VALUE LOW-VALUE.                 
032500     03  W-DAPRLIST-K7-N.                                                 
032600         05  W-DAPRLIST-K7      PIC 9(8)    VALUE ZERO.                   
032700     03  W-IDLAND-K7-X.                                                   
032800         05  W-IDLAND-K7        PIC X(2)    VALUE SPACE.                  
032900                                                                          
033000     03  W-IDLAND-X.                                                      
033100         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
033200                                                                          
033300     03  W-IDDC-X.                                                        
033400         05  W-IDDC              PIC X(2)       VALUE SPACE.              
033500     03  W-KDAVROP-X.                                                     
033600         05  W-KDAVROP           PIC S9(1)      VALUE ZERO COMP-3.        
033700                                                                          
033800     03  W-WDD905KY-X.                                                    
033900         05  W-DAAVROP-X.                                                 
034000             07  W-DAAVROP       PIC  9(6)      VALUE ZERO.               
034100         05  W-TILEVDAG-X.                                                
034200             07  W-TILEVDAG      PIC  S9        VALUE ZERO COMP-3.        
034300     03  W-IDORDNSB-X.                                                    
034400         05  W-IDORDNSB          PIC S9(5)      VALUE ZERO COMP-3.        
034500     03  W-INLB11-IDLEVNR-X.                                              
034600         05  W-INLB11-IDLEVNR    PIC  X(5)      VALUE SPACE.              
034700     03  W-6017KEY-X.                                                     
034800         05  FILLER              PIC X(4)       VALUE '6017'.             
034900         05  FILLER              PIC X(26)      VALUE LOW-VALUE.          
035000     03  W-IDSEKVNR              PIC S9(3)      COMP-3 VALUE ZERO.        
035100     03  W-4505-KEY-X.                                                    
035200         05  FILLER              PIC X(4)       VALUE '4505'.             
035300         05  4505-IDDC           PIC X(2)       VALUE SPACE.              
035400         05  FILLER              PIC X(24)      VALUE LOW-VALUE.          
035500     03  W-IDLANDX2-X.                                                    
035600         05    W-IDLANDX2              PIC X(2)    VALUE SPACE.           
035700     03  W-WDGX9305-X.                                                    
035800         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
035900         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
036000         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
036100         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
036200     03  W-KDVALISO-X.                                                    
036300         05  W-KDVALISO-ROW      PIC X(3)    VALUE SPACE.                 
036400     03  W-TISTADA9-X.                                                    
036500         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
036600     SKIP2                                                                
036700*    --- STATUS-KOD FRÅN IMS                                              
036800 01  STATUS-WS                   PIC XX.                                  
036900     88  SEGMENT-FINNS                          VALUE '  '.               
037000     88  SEGMENT-FINNS-REDAN                    VALUE 'II'.               
037100     88  SEGMENT-SAKNAS                         VALUE 'GE'.               
037200     SKIP2                                                                
037300 01  GODK-STATUSKODER.                                                    
037400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
037500     SKIP3                                                                
037600 01  SSA1                        PIC X(64).                               
037700 01  SSA2                        PIC X(64).                               
037800 01  SSA3                        PIC X(64).                               
037900     EJECT                                                                
038000*    --- IMS FUNKTIONSKODER                                               
038100*01  -COPY W0003                                                          
038200     EJECT                                                                
038300*    ---  DLI INPUT-OUTPUT AREA                                           
038400 01  FILLER                      PIC X(16)    VALUE 'DLI-IO-AREA'.        
038500     SKIP3                                                                
038600 01  DLI-IO-AREA.                                                         
038700     03  IO-AREA                 PIC X(150)   VALUE SPACE.                
038800     SKIP3                                                                
038900     03  WLINLE01 REDEFINES IO-AREA.                                      
039000*        05  -COPY WDL201  -PRE INLE-                                     
039100     SKIP3                                                                
039200     03  WLINLE11 REDEFINES IO-AREA.                                      
039300*        05  -COPY WDL211  -PRE INLE-                                     
039400     SKIP3                                                                
039500     03  WLINLE22 REDEFINES IO-AREA.                                      
039600*        05  -COPY WDL222  -PRE INLE-                                     
039700     EJECT                                                                
039800     03  WLARTC01 REDEFINES IO-AREA.                                      
039900*        05  -COPY WDK601  -PRE ARTC01-                                   
040000     SKIP3                                                                
040100     03  WLARTC21 REDEFINES IO-AREA.                                      
040200*        05  -COPY WDK621  -PRE ARTC21-                                   
040300     SKIP3                                                                
040400     03  WLZZAC01 REDEFINES IO-AREA.                                      
040500*        05  -COPY WDG601  -PRE ZZAC01-                                   
040600     EJECT                                                                
040700     03  WLINLB11 REDEFINES IO-AREA.                                      
040800*        05  -COPY WDD902  -PRE INLB11-                                   
040900     SKIP3                                                                
041000     03  WLINLB23 REDEFINES IO-AREA.                                      
041100*        05  -COPY WDD905  -PRE INLB23-                                   
041200     SKIP3                                                                
041300     03  WLINLB24 REDEFINES IO-AREA.                                      
041400*        05  -COPY WDD924   -PRE INLB24-                                  
041500     SKIP3                                                                
041600     03  WLINLB32 REDEFINES IO-AREA.                                      
041700*        05  -COPY WDD907  -PRE INLB32-                                   
041800 01  FILLER                      PIC X(16)   VALUE 'WLLOGA01'.            
041900*01  WLLOGA01  -COPY WDL901                                               
042000                                                                          
042100     EJECT                                                                
042200 01  DLI-IO-AREA2.                                                        
042300     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
042400     SKIP3                                                                
042500     03  WLINLB31 REDEFINES IO-AREA2.                                     
042600*        05  -COPY WDD906  -PRE INLB31-                                   
042700     SKIP3                                                                
042800 01  DLI-IO-AREA3.                                                        
042900     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
043000     SKIP3                                                                
043100     03  W6LOPA11 REDEFINES IO-AREA3.                                     
043200*        05  -COPY W6GX6018                                               
043300     EJECT                                                                
043400 01  DLI-IO-AREA4.                                                        
043500     03  IO-AREA4                PIC X(900)  VALUE SPACE.                 
043600     SKIP3                                                                
043700     03  WLARTC11 REDEFINES IO-AREA4.                                     
043800*        05  -COPY WDK611   -PRE ARTC11-                                  
043900     EJECT                                                                
044000 01  FILLER                      PIC X(16)  VALUE 'WDK623'.               
044100                                                                          
044200*01  WLARTC23 -COPY WDK623                                                
044300     EJECT                                                                
044400 01  DLI-IO-AREA5.                                                        
044500     03  IO-AREA5                PIC X(600)  VALUE SPACE.                 
044600     03  WLINLC01 REDEFINES IO-AREA5.                                     
044700*        05  -COPY WDL601                                                 
044800     SKIP3                                                                
044900     03  WLINLC11 REDEFINES IO-AREA5.                                     
045000*        05  -COPY WDL611                                                 
045100     EJECT                                                                
045200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711-I'.                    
045300 01  DLI-IO-WDK711-I.                                                     
045400*    03  -COPY WDK711                                                     
045500     EJECT                                                                
045600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
045700 01  DLI-IO-WDK711.                                                       
045800*    03  -COPY WDK711 -PRE K7-                                            
045900     SKIP3                                                                
046000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
046100 01  DLI-IO-WDK712.                                                       
046200*        05  -COPY WDK712                                                 
046300     SKIP3                                                                
046400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK724'.                      
046500 01  DLI-IO-WDK724.                                                       
046600*        05  -COPY WDK724                                                 
046700     EJECT                                                                
046800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK728'.                      
046900 01  DLI-IO-WDK728.                                                       
047000*        05  -COPY WDK728                                                 
047100     EJECT                                                                
047200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL301'.                      
047300 01  DLI-IO-WDL301.                                                       
047400*    03  -COPY WDL301                                                     
047500     EJECT                                                                
047600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL623'.                      
047700 01  DLI-IO-WDL623.                                                       
047800*    03  -COPY WDL623                                                     
047900     EJECT                                                                
048000                                                                          
048100                                                                          
048200 01  DLI-IO-AREA7.                                                        
048300     03  IO-AREA7                PIC X(600)  VALUE SPACE.                 
048400     03  WLFILA01 REDEFINES IO-AREA7.                                     
048500*        05  -COPY WDR601                                                 
048600     EJECT                                                                
048700 01  DLI-IO-AREA-4505.                                                    
048800     03  IO-AREA-4505            PIC X(300) VALUE SPACE.                  
048900     SKIP3                                                                
049000     03  WL450611 REDEFINES IO-AREA-4505.                                 
049100*        05  -COPY WDGX4506                                               
049200     EJECT                                                                
049300 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-FILB01'.        
049400 01  DLI-IO-AREA-FILB01.                                                  
049500*    05  -COPY WDR801       -PRE EKO-                                     
049600       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
049700         09  -COPY W51080   -PRE EKO-                                     
049800       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
049900         09  -COPY W510A18  -PRE LAB-                                     
050000       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
050100         09  -COPY W510EKHA -PRE EKO-                                     
050200     EJECT                                                                
050300 01  FILLER             PIC X(16)         VALUE 'DLI-IO-WLSAPA01'.        
050400 01  DLI-IO-WLSAPA01.                                                     
050500*    03  WLSAPA01  -COPY WDR901                                           
050600*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
050700     EJECT                                                                
050800 01  FILLER         PIC X(24) VALUE 'DLI-IO-OIGA11'.                      
050900 01  DLI-IO-OIGA11.                                                       
051000*    03  -COPY WDL711                                                     
051100     EJECT                                                                
051200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
051300 01   DLI-IO-AREA-B601.                                                   
051400*     03  -COPY WDB601                                                    
051500     EJECT                                                                
051600                                                                          
051700 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
051800 01   DLI-IO-AREA-B617.                                                   
051900*     03  -COPY WDB617                                                    
052000     EJECT                                                                
052100 01  DLI-IO-AREA-F1          PIC X(100).                                  
052200     SKIP2                                                                
052300*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
052400     EJECT                                                                
052500                                                                          
052600 01  DLI-IO-AREA-F102        PIC X(100).                                  
052700     SKIP2                                                                
052800*01  WLLEVA11 -COPY WDF102 -PRE F102-     -RED DLI-IO-AREA-F102           
052900     EJECT                                                                
053000                                                                          
053100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
053200 01  DLI-IO-WDGX9306.                                                     
053300*    03  -COPY WDGX9306                                                   
053400     EJECT                                                                
053500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
053600 01  DLI-IO-WDGX9308.                                                     
053700*    03  -COPY WDGX9308                                                   
053800                                                                          
053900 LINKAGE SECTION.                                                         
054000                                                                          
054100*01  -COPY W0009   -PRE MSG-                                              
054200     EJECT                                                                
054300*01  -COPY W0009   -PRE DISP-                                             
054400     EJECT                                                                
054500 01  MQASYNC-PCB                 PIC X.                                   
054600*01  -COPY W0008  -PRE INLE-                                              
054700     05  FILLER                  PIC X.                                   
054800     EJECT                                                                
054900*01  -COPY W0008  -PRE ARTC-                                              
055000     05  FILLER                  PIC X.                                   
055100     EJECT                                                                
055200*01  -COPY W0008  -PRE ZZAC-                                              
055300     05  FILLER                  PIC X.                                   
055400     EJECT                                                                
055500*01  -COPY W0008  -PRE INLB-                                              
055600     05  FILLER                  PIC X(10).                               
055700     05  INLB-KFB-DAAVROP-AVS    PIC 9(6).                                
055800     05  INLB-KFB-TILEVDAG       PIC S9    COMP-3.                        
055900     EJECT                                                                
056000*01  -COPY W0008  -PRE LOPA-                                              
056100     05  FILLER                  PIC X.                                   
056200     EJECT                                                                
056300*01  -COPY W0008  -PRE WDK7I-                                             
056400     05  FILLER                  PIC X.                                   
056500     EJECT                                                                
056600*01  -COPY W0008  -PRE WDK7-                                              
056700     05  FILLER                  PIC X.                                   
056800     EJECT                                                                
056900*01  -COPY W0008  -PRE INLC-                                              
057000     05  FILLER                  PIC X.                                   
057100*01  -COPY W0008  -PRE FILA-                                              
057200     05  FILLER                  PIC X.                                   
057300     EJECT                                                                
057400*01  -COPY W0008  -PRE USEA-                                              
057500     05  FILLER                  PIC X.                                   
057600     EJECT                                                                
057700*01  -COPY W0008  -PRE 4505-                                              
057800     05  FILLER                  PIC X.                                   
057900*01  -COPY W0008  -PRE LOGA-                                              
058000     05  FILLER                  PIC X.                                   
058100     EJECT                                                                
058200*01  -COPY W0008  -PRE FILB-                                              
058300     05  FILLER                  PIC X.                                   
058400     EJECT                                                                
058500*01  -COPY W0008  -PRE SAPA-                                              
058600     05  FILLER                  PIC X.                                   
058700     EJECT                                                                
058800*01  -COPY W0008  -PRE OIGA-                                              
058900     05  FILLER                  PIC X.                                   
059000     EJECT                                                                
059100*01  -COPY W0008  -PRE WDB6-                                              
059200     05  FILLER                  PIC X.                                   
059300     EJECT                                                                
059400*01  -COPY W0008  -PRE WDF1-                                              
059500     05  FILLER                  PIC X.                                   
059600     EJECT                                                                
059700*01  -COPY W0008  -PRE 9305-                                              
059800     05  FILLER                  PIC X.                                   
059900*01  -COPY W0008  -PRE WDL3-                                              
060000     05  FILLER                  PIC X.                                   
060100*01  -COPY W0008  -PRE WDL6-                                              
060200     05  FILLER                  PIC X.                                   
060300     EJECT                                                                
060400                                                                          
060500 PROCEDURE DIVISION  USING MSG-PCB  DISP-PCB MQASYNC-PCB                  
060600                           INLE-PCB ARTC-PCB                              
060700                           ZZAC-PCB INLB-PCB LOPA-PCB WDK7I-PCB           
060800                           WDK7-PCB INLC-PCB FILA-PCB USEA-PCB            
060900                           4505-PCB LOGA-PCB FILB-PCB SAPA-PCB            
061000                           OIGA-PCB WDB6-PCB WDF1-PCB 9305-PCB            
061100                           WDL3-PCB WDL6-PCB.                             
061200                                                                          
061300     ENTRY 'DLITCBL' USING MSG-PCB  DISP-PCB MQASYNC-PCB                  
061400                           INLE-PCB ARTC-PCB                              
061500                           ZZAC-PCB INLB-PCB LOPA-PCB WDK7I-PCB           
061600                           WDK7-PCB INLC-PCB FILA-PCB USEA-PCB            
061700                           4505-PCB LOGA-PCB FILB-PCB SAPA-PCB            
061800                           OIGA-PCB WDB6-PCB WDF1-PCB 9305-PCB            
061900                           WDL3-PCB WDL6-PCB.                             
062000                                                                          
062100     PERFORM IMS-GET-MSG                                                  
062200     IF SEGMENT-FINNS                                                     
062300       PERFORM IMS-GN-MSG                                                 
062400       IF SEGMENT-FINNS                                                   
062500         MOVE JA    TO DISP-SVAR                                          
062600       END-IF                                                             
062700       PERFORM A-INIT                                                     
062800       PERFORM UNTIL MID-IX > MID-KVPOST                                  
062900         PERFORM B-UPPD-WLARTC-WDK7                                       
063000         PERFORM D-MEDDELANDE                                             
063100         PERFORM E-UPPD-HISTORIK                                          
063200          PERFORM S07-SKAPA-R34-LOGG                                      
063300         IF CDC-SE                                                        
063400             PERFORM F-UPPD-LEVPLAN                                       
063500         END-IF                                                           
063600         IF CDC OR NDC                                                    
063700             PERFORM G-RO-TAECKNING                                       
063800         END-IF                                                           
063900         ADD +1     TO MID-IX                                             
064000       END-PERFORM                                                        
064100       IF DCS-KDTRADP = 'BR12' AND FIRST-REC-TRANS                        
064200        PERFORM S23-SEND-CLOSE                                            
064300       END-IF                                                             
064400       PERFORM Z-FINIT                                                    
064500     END-IF                                                               
064600                                                                          
064700     MOVE ZERO TO RETURN-CODE                                             
064800     GOBACK                                                               
064900     .                                                                    
065000     EJECT                                                                
065100 A-INIT SECTION.                                                          
065200                                                                          
065300     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11C01                    
065400     MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                   
065500     MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                  
065600                                                                          
065700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
065800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
065900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
066000                                                                          
066100     MOVE 'IDAG'             TO DAT-KDDATFORM                             
066200     CALL WDATKONV USING        DAT-KDDATFORM                             
066300                                DAT-I-TIDATUM                             
066400                                DAT-O-TIDATUM                             
066500                                DAT-KDSVAR                                
066600     MOVE DAT-TIAA           TO WS-IDLOPNRM-AA                            
066700     MOVE DAT-TIAAVVD-GRP    TO SPAR-TIAAVVD-GRP                          
066800     PERFORM IMS-GHU-W6LOPA11                                             
066900     MOVE 6018-IDLOPNRM     TO WS-IDLOPNRM                                
067000     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME  FROM TIME                           
067100     MOVE MID-IDDC          TO WS-IDDC-NUM                                
067200                               WS-IDDC                                    
067300                               W-IDDC-K7                                  
067400     MOVE FUNCTION CURRENT-DATE TO WS-DAGENS-DATUM                        
067500     MOVE FUNCTION CURRENT-DATE (3:4) TO WS-DATE-YYMM                     
067600     MOVE DAT-TIAAMMDD        TO DAGENS-AAMMDD                            
067700     MOVE DAT-TISEKEL         TO DAGENS-AA                                
067800     MOVE 01                          TO WS-DATE-DD                       
067900     ACCEPT DAGENS-DATUM   FROM DATE                                      
068000     ACCEPT DAGENS-TID     FROM TIME                                      
068100     MOVE   WS-DAGENS-TID TO  AKTUELL-TID                                 
068200     .                                                                    
068300     EJECT                                                                
068400 B-UPPD-WLARTC-WDK7 SECTION.                                              
068500                                                                          
068600     IF CDC-SE                                                            
068700         PERFORM BA-UPPD-WLARTC                                           
068800     ELSE                                                                 
068900         PERFORM S06-LAES-SPARA-WLARTC                                    
069000         PERFORM BB-UPPD-WDK7                                             
069100         PERFORM BBA-VALIDATE-IDTRACK                                     
069200     END-IF                                                               
069300     .                                                                    
069400     EJECT                                                                
069500 BA-UPPD-WLARTC SECTION.                                                  
069600     SKIP2                                                                
069700     MOVE MID-IDARTNR (MID-IX) TO W-IDARTNR                               
069800     PERFORM IMS-GU-WLARTC01                                              
069900     MOVE ARTC01-ART-IDLEVNR          TO SPAR-ARTC01-IDLEVNR              
070000     MOVE ARTC01-ART-KDPRODSL         TO SPAR-ARTC01-KDPRODSL             
070100     MOVE ARTC01-ART-KDSORT           TO SPAR-ARTC01-KDSORT               
070200     PERFORM IMS-GHNP-WLARTC11                                            
070300     IF MID-KDRT (MID-IX) = 1 OR 2                                        
070400       IF MID-KVAVIS (MID-IX) > ARTC11-CLAG-KVLAAN                        
070500         COMPUTE WS-KV-LPLAN = MID-KVAVIS (MID-IX)                        
070600                             - ARTC11-CLAG-KVLAAN                         
070700         MOVE +0                 TO ARTC11-CLAG-KVLAAN                    
070800       ELSE                                                               
070900         SUBTRACT MID-KVAVIS (MID-IX) FROM ARTC11-CLAG-KVLAAN             
071000         MOVE +0                 TO WS-KV-LPLAN                           
071100       END-IF                                                             
071200     ELSE                                                                 
071300       MOVE MID-KVAVIS (MID-IX) TO WS-KV-LPLAN                            
071400     END-IF                                                               
071500                                                                          
071600     IF MID-KDRT (MID-IX) = 0 OR 1 OR 2 OR 4 OR 5                         
071700       MOVE ARTC11-CLAG-TIAVIDAT-SEN   TO TMP1-YYMMDD                     
071800       MOVE MID-TIAVIDAT (MID-IX)      TO TMP2-YYMMDD                     
071900       PERFORM WY2000P1                                                   
072000       IF TMP1-YYMMDD < TMP2-YYMMDD          AND                          
072100         (ARTC11-CLAG-KDHF > +0 OR MID-IDLEVNR (MID-IX)                   
072200          = SPAR-ARTC01-IDLEVNR)                                          
072300         MOVE MID-IDLEVNR (MID-IX)   TO ARTC11-CLAG-IDLEVNR-SEN           
072400         MOVE MID-IDAVINR (MID-IX)   TO ARTC11-CLAG-IDFS-SEN              
072500         MOVE MID-TIAVIDAT (MID-IX)  TO ARTC11-CLAG-TIAVIDAT-SEN          
072600         MOVE MID-KVAVIS  (MID-IX)   TO ARTC11-CLAG-KVAVIS-SEN            
072700       END-IF                                                             
072800     END-IF                                                               
072900     ADD MID-KVAVIS (MID-IX)    TO ARTC11-CLAG-KVLS                       
073000     IF MID-FLSVS = 'J'                                                   
073100       ADD MID-KVAVIS (MID-IX)  TO ARTC11-CLAG-KVLS-SVS                   
073200     END-IF                                                               
073300     PERFORM IMS-REPL-WLARTC11                                            
073400     PERFORM BAB-SKAPA-SALDOLOGG-WDK6                                     
073500     PERFORM BAA-PRIS-JUST-KONTROLL                                       
073600     .                                                                    
073700     EJECT                                                                
073800                                                                          
073900 BAA-PRIS-JUST-KONTROLL SECTION.                                          
074000     MOVE MID-TIAVIDAT (MID-IX) TO WS-IDAG                                
074100     MOVE WS-IDAG           TO DAT-I-TIDATUM                              
074200     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
074300     CALL WDATKONV USING       DAT-KDDATFORM                              
074400                               DAT-I-TIDATUM                              
074500                               DAT-O-TIDATUM                              
074600                               DAT-KDSVAR                                 
074700     IF DAT-KDSVAR-FEL                                                    
074800       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
074900       CALL FELLOG                                                        
075000     END-IF                                                               
075100                                                                          
075200     MOVE DAT-TISEKEL TO WS-DAGENS-SEKEL                                  
075300                                                                          
075400     MOVE NEJ TO PRIS-FINNS-SW                                            
075500     MOVE MID-IDLEVNR (MID-IX) TO W-IDLEVNR                               
075600                                  W-IDLEVNR-21                            
075700     MOVE +0                    TO SPAR-ARTC21-PRARTBEL-PR                
075800                                   SPAR-ARTC21-PRARTBEL-SUM               
075900                                   SPAR-ARTC21-PRARTBES-PR                
076000     PERFORM IMS-GHNP-WLARTC21                                            
076100     PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF                               
076200      COMPUTE W-PRL-DADAT = 99999999 - ARTC21-PRL-DAPRLIST-9KOMPL         
076300       IF ARTC21-PRL-KDSTATUS-PR = +1 AND                                 
076400         W-PRL-DADAT <= WS-DAGENS-DATUM                                   
076500         MOVE JA TO TRAEFF-SW                                             
076600       ELSE                                                               
076700         PERFORM IMS-GHNP-WLARTC21                                        
076800       END-IF                                                             
076900     END-PERFORM                                                          
077000                                                                          
077100     IF TRAEFF                                                            
077200       MOVE ARTC21-PRL-KDVALISO       TO SPAR-ARTC21-KDVALISO             
077300       MOVE ARTC21-PRL-PRARTBEL-PR    TO SPAR-ARTC21-PRARTBEL-PR          
077400       MOVE ARTC21-PRL-PRARTBEL-PR    TO SPAR-ARTC21-PRARTBEL-SUM         
077500       MOVE ARTC21-PRL-PRARTBES-PR    TO SPAR-ARTC21-PRARTBES-PR          
077600       IF ARTC21-PRL-SUINLEV-PR < +1                                      
077700         MOVE +1         TO ARTC21-PRL-SUINLEV-PR                         
077800         PERFORM IMS-REPL-WLARTC                                          
077900                                                                          
078000         PERFORM IMS-GHNP-WLARTC11-FIRST                                  
078100                                                                          
078200         COMPUTE ARTC11-CLAG-PRARTSJK = SPAR-ARTC21-PRARTBES-PR +         
078300                                        ARTC11-CLAG-PRDIRLON    +         
078400                                        ARTC11-CLAG-PRDMTRL     +         
078500                                        ARTC11-CLAG-PROVRPAL              
078600         END-COMPUTE                                                      
078700                                                                          
078800         MOVE +1                      TO ARTC11-CLAG-KDTIPPR              
078900                                                                          
079000         PERFORM IMS-REPL-WLARTC11                                        
079100       ELSE                                                               
079200         ADD +1        TO ARTC21-PRL-SUINLEV-PR                           
079300         PERFORM IMS-REPL-WLARTC                                          
079400       END-IF                                                             
079500     END-IF                                                               
079600     .                                                                    
079700     EJECT                                                                
079800                                                                          
079900 BAB-SKAPA-SALDOLOGG-WDK6 SECTION.                                        
080000     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
080100     MOVE 9                         TO LOGG-IDSEKVNR                      
080200     MOVE MID-IDDC                  TO LOGG-IDDC                          
080300     MOVE 'MISC'                    TO LOGG-IDHUVTYP                      
080400     MOVE 'R34'                     TO LOGG-IDSUBTYP                      
080500     MOVE 'W6011C00'                TO LOGG-IDPGM                         
080600     IF W-IDTRANS = '6100'                                                
080700        MOVE W-IDTRANS              TO LOGG-IDTRANS                       
080800     ELSE                                                                 
080900        MOVE '6117'                 TO LOGG-IDTRANS                       
081000     END-IF                                                               
081100     IF MSG-SIGNON-USERID NOT = SPACE                                     
081200        MOVE  MSG-SIGNON-USERID     TO LOGG-IDUSER                        
081300     ELSE                                                                 
081400        MOVE 'UNKNOWN'              TO LOGG-IDUSER                        
081500     END-IF                                                               
081600                                                                          
081700     MOVE SPACE                     TO LOGG-REF                           
081800     MOVE MID-IDLEVNR (MID-IX)      TO LOGG-IDLEVNR                       
081900     MOVE MID-IDAVINR (MID-IX)      TO LOGG-IDAVINR                       
082000     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
082100     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
082200     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
082300     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
082400     MOVE MID-KVAVIS (MID-IX)       TO LOGG-KVART-SALDO                   
082500     COMPUTE LOGG-KVAKS = ARTC11-CLAG-KVAKS-CDC +                         
082600                          ARTC11-CLAG-KVAKS-T                             
082700                                                                          
082800     MOVE ARTC11-CLAG-KVAKS-PAV     TO LOGG-KVAKS-PAV                     
082900     MOVE ARTC11-CLAG-KVEFRS        TO LOGG-KVEFRS                        
083000     MOVE ARTC11-CLAG-KVLS          TO LOGG-KVLS                          
083100     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
083200     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
083300     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
083400     ACCEPT WLOGG-TID FROM TIME                                           
083500     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
083600                                                                          
083700     PERFORM IMS-ISRT-WDL9                                                
083800     IF SEGMENT-FINNS-REDAN                                               
083900       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
084000          ADD -1 TO LOGG-IDSEKVNR                                         
084100          PERFORM IMS-ISRT-WDL9                                           
084200       END-PERFORM                                                        
084300     END-IF                                                               
084400     .                                                                    
084500     EJECT                                                                
084600                                                                          
084700 BB-UPPD-WDK7 SECTION.                                                    
084800     SKIP2                                                                
084900     MOVE MID-IDARTNR (MID-IX) TO W-IDARTNR                               
085000     MOVE MID-IDDC             TO W-IDDC                                  
085100     PERFORM IMS-GHU-WDK7-WDK711                                          
085200     IF SEGMENT-SAKNAS                                                    
085300       MOVE ALL '+'          TO WDK7-W005WDK7                             
085400       MOVE 'WDK711'         TO WDK7-IDSEGM                               
085500       MOVE W-IDARTNR        TO WDK7-IDARTNR-KFB                          
085600       MOVE MID-IDDC         TO WDK7-IDDC-KFB                             
085700                                WDK7-IDDC                                 
085800       MOVE MID-KVAVIS (MID-IX) TO WDK7-KVLS                              
085900                                   SPARA-KVLS                             
086000                                                                          
086100       CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB ARTC-PCB                
086200                                         WDK7I-PCB                        
086300       MOVE WDK7-WDK711         TO SLAG-WDK711                            
086400                                                                          
086500       MOVE ZERO                TO SPARA-KVLS-OLD                         
086600       PERFORM BC-SKAPA-SALDOLOGG-WDK7                                    
086700     ELSE                                                                 
086800       MOVE SLAG-KVLS         TO SPARA-KVLS-OLD                           
086900                                 WS-SLAG-KVLS                             
087000       ADD MID-KVAVIS (MID-IX) TO SLAG-KVLS                               
087100       MOVE SLAG-KVLS         TO SPARA-KVLS                               
087200       PERFORM IMS-REPL-WDK711                                            
087300       PERFORM BC-SKAPA-SALDOLOGG-WDK7                                    
087400     END-IF                                                               
087500                                                                          
087600     .                                                                    
087700     EJECT                                                                
087800 BBA-VALIDATE-IDTRACK SECTION.                                            
087900     SKIP2                                                                
088000     PERFORM IMS-GU-WDB601                                                
088100     IF DCS-FLTRACK = 'J'                                                 
088200        MOVE 'J'  TO WS-FLTRACK                                           
088300     ELSE                                                                 
088400        MOVE 'N'  TO WS-FLTRACK                                           
088500     END-IF                                                               
088600                                                                          
088700     IF WS-FLTRACK = 'J' AND SPAR-ARTC01-KDPRODSL < 90                    
088800        MOVE MID-IDARTNR (MID-IX) TO W-IDARTNR                            
088900        MOVE MID-IDDC             TO W-IDDC                               
089000                                     W-IDDC-K7                            
089100        MOVE MID-KVAVIS (MID-IX)  TO WS-VALD-KVAVIS                       
089200                                                                          
089300        MOVE 'N'                 TO IDTRACK-QTY-SW                        
089400        PERFORM IMS-GHU-WDK711-K7                                         
089500        MOVE 9999999999999999    TO W-DAINLEV                             
089600        PERFORM IMS-GHNP-WDK728-LAST                                      
089700        PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE                  
089800          IF TRCK-KVTRACK-KVAR < TRCK-KVANTMOT                            
089900             COMPUTE WS-TEMP-KVAVIS =                                     
090000              TRCK-KVANTMOT - TRCK-KVTRACK-KVAR                           
090100**** HAVE MORE THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
090200              IF WS-TEMP-KVAVIS >= WS-VALD-KVAVIS                         
090300                 MOVE 'J' TO IDTRACK-QTY-SW                               
090400                 ADD WS-VALD-KVAVIS TO TRCK-KVTRACK-KVAR                  
090500                 PERFORM IMS-REPL-WDK728                                  
090600                 MOVE WS-VALD-KVAVIS TO WS-TRCK-KVANTMOT                  
090700                 PERFORM S11-SALDOLOGT-DATA                               
090800                 PERFORM S12-ISRT-SALDOLOGT                               
090900                 MOVE WS-VALD-KVAVIS TO WS-INL-KVAVIS                     
091000                 PERFORM EA-UPPDATERA-SDC-HISTORIK                        
091100                 MOVE TRCK-IDTRACK TO TINL-IDTRACK                        
091200                 PERFORM IMS-ISRT-WDL623                                  
091300              ELSE                                                        
091400**** HAVE LESS THAN REQUIRED KVAVIS IN WDK728 SEGMENT                     
091500                 ADD WS-TEMP-KVAVIS TO TRCK-KVTRACK-KVAR                  
091600                 COMPUTE WS-VALD-KVAVIS =                                 
091700                     WS-VALD-KVAVIS - WS-TEMP-KVAVIS                      
091800                 PERFORM IMS-REPL-WDK728                                  
091900                 MOVE WS-TEMP-KVAVIS TO WS-TRCK-KVANTMOT                  
092000                 PERFORM S11-SALDOLOGT-DATA                               
092100                 PERFORM S12-ISRT-SALDOLOGT                               
092200                 MOVE WS-TEMP-KVAVIS TO WS-INL-KVAVIS                     
092300                 PERFORM EA-UPPDATERA-SDC-HISTORIK                        
092400                 MOVE TRCK-IDTRACK TO TINL-IDTRACK                        
092500                 PERFORM IMS-ISRT-WDL623                                  
092600                 MOVE 'N' TO IDTRACK-QTY-SW                               
092700              END-IF                                                      
092800                                                                          
092900          END-IF                                                          
093000          IF IDTRACK-QTY-NOT-DONE                                         
093100             PERFORM IMS-GHU-WDK711-K7                                    
093200             MOVE  TRCK-DAINLEV TO W-DAINLEV                              
093300             PERFORM IMS-GHNP-WDK728-LAST                                 
093400          END-IF                                                          
093500        END-PERFORM                                                       
093600     END-IF                                                               
093700                                                                          
093800     .                                                                    
093900     EJECT                                                                
094000 BC-SKAPA-SALDOLOGG-WDK7 SECTION.                                         
094100     MOVE W-IDARTNR                 TO LOGG-IDARTNR                       
094200     MOVE 9                         TO LOGG-IDSEKVNR                      
094300     MOVE MID-IDDC                  TO LOGG-IDDC                          
094400     MOVE 'MISC'                    TO LOGG-IDHUVTYP                      
094500     MOVE 'R34'                     TO LOGG-IDSUBTYP                      
094600     MOVE 'W6011C00'                TO LOGG-IDPGM                         
094700     IF W-IDTRANS = '6100'                                                
094800        MOVE W-IDTRANS              TO LOGG-IDTRANS                       
094900     ELSE                                                                 
095000        MOVE '6117'                 TO LOGG-IDTRANS                       
095100     END-IF                                                               
095200     IF MSG-SIGNON-USERID NOT = SPACE                                     
095300        MOVE  MSG-SIGNON-USERID     TO LOGG-IDUSER                        
095400     ELSE                                                                 
095500        MOVE 'UNKNOWN'              TO LOGG-IDUSER                        
095600     END-IF                                                               
095700                                                                          
095800     MOVE SPACE                     TO LOGG-REF                           
095900     MOVE MID-IDLEVNR (MID-IX)      TO LOGG-IDLEVNR                       
096000     MOVE MID-IDAVINR (MID-IX)      TO LOGG-IDAVINR                       
096100     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
096200     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
096300     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
096400     MOVE '+'                       TO LOGG-IDTECKEN-KVLS                 
096500     MOVE MID-KVAVIS (MID-IX)       TO LOGG-KVART-SALDO                   
096600     MOVE SLAG-KVAKS-SDC            TO LOGG-KVAKS                         
096700     MOVE SLAG-KVAKS-PAV            TO LOGG-KVAKS-PAV                     
096800     MOVE SLAG-KVEFRS               TO LOGG-KVEFRS                        
096900     MOVE SLAG-KVLS                 TO LOGG-KVLS                          
097000     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
097100     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
097200     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
097300     ACCEPT WLOGG-TID FROM TIME                                           
097400     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
097500                                                                          
097600     PERFORM IMS-ISRT-WDL9                                                
097700     IF SEGMENT-FINNS-REDAN                                               
097800       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
097900          ADD -1 TO LOGG-IDSEKVNR                                         
098000          PERFORM IMS-ISRT-WDL9                                           
098100       END-PERFORM                                                        
098200     END-IF                                                               
098300     .                                                                    
098400     EJECT                                                                
098500 D-MEDDELANDE              SECTION.                                       
098600                                                                          
098700     IF CDC-SE                                                            
098800         IF ARTC11-CLAG-ADLAGOMR = 0 AND                                  
098900            ARTC11-CLAG-ADGANG   = 0 AND                                  
099000            ARTC11-CLAG-ADPLATS  = 0                                      
099100                PERFORM DAA-MEDDELANDE-M107                               
099200         END-IF                                                           
099300     ELSE                                                                 
099400         IF SLAG-ADLAGOMR = 0 AND                                         
099500            SLAG-ADGANG   = 0 AND                                         
099600            SLAG-ADPLATS  = 0                                             
099700                PERFORM DAA-MEDDELANDE-M107                               
099800         END-IF                                                           
099900     END-IF                                                               
100000                                                                          
100100     IF NOT CDC-SE                                                        
100200         PERFORM IMS-GU-WLARTC01                                          
100300         MOVE ARTC01-ART-KDPRODSL TO SPAR-ARTC01-KDPRODSL                 
100400         PERFORM IMS-GNP-WLARTC11                                         
100500     END-IF                                                               
100600                                                                          
100700     IF ARTC11-CLAG-VKART    = 0                                          
100800        PERFORM DAB-MEDDELANDE-M108                                       
100900     END-IF                                                               
101000                                                                          
101100     IF ARTC11-CLAG-KDERS > +9                                            
101200        PERFORM DAD-MEDDELANDE-M113                                       
101300     END-IF                                                               
101400                                                                          
101500     IF MID-KDRT (MID-IX) NOT = 7                                         
101600        IF ((ARTC11-CLAG-PRARTSTD NOT = ARTC11-CLAG-PRINK) AND            
101700           ARTC11-CLAG-BEFT < 10 OR  ARTC11-CLAG-BEFT > 69) OR            
101800           ((ARTC11-CLAG-PRARTSTD = ARTC11-CLAG-PRINK) AND                
101900           ARTC11-CLAG-BEFT > 9 OR ARTC11-CLAG-BEFT < 70)                 
102000           PERFORM DAE-MEDDELANDE-M117                                    
102100        END-IF                                                            
102200     END-IF                                                               
102300                                                                          
102400     IF NDC-NA                                                            
102500          PERFORM DBA-MEDDELANDE-LAB-WDR8                                 
102600     ELSE                                                                 
102700       IF (MID-KDRT (MID-IX)  = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7)          
102800          OR  ((MID-KDRT (MID-IX) = 9 OR 10) AND                          
102900              (MID-IDLEVNR (MID-IX) = '1001 ' OR '1003 '                  
103000                                   OR 'BL3YA' OR 'BP2TH'))                
103100          PERFORM DC-MEDDELANDE-EKO-WDR8-WDR9                             
103200          MOVE NEJ TO WS-FLPALAGG                                         
103300       ELSE                                                               
103400          PERFORM DBB-MEDDELANDE-EKO-WDR8-WDR9                            
103500       END-IF                                                             
103600       IF ARTC11-CLAG-PRINK = ARTC11-CLAG-PRARTSTD                        
103700         CONTINUE                                                         
103800       ELSE                                                               
103900          IF XDC-NON-VCC-OWNED OR LDC-CN                                  
104000            IF WS-FLPALAGG = JA                                           
104100              CONTINUE                                                    
104200            END-IF                                                        
104300          ELSE                                                            
104400            IF WS-FLPALAGG = JA                                           
104500              PERFORM S10-PALAGG-WDR9                                     
104600            END-IF                                                        
104700          END-IF                                                          
104800       END-IF                                                             
104900     END-IF                                                               
105000                                                                          
105100     IF MID-KDRT (MID-IX) NOT = +8 AND CDC-SE                             
105200        PERFORM DD-MEDDELANDE-330-221                                     
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 DAA-MEDDELANDE-M107 SECTION.                                             
105700                                                                          
105800     MOVE +0                     TO M107-IDLOPNRM                         
105900                                                                          
106000     PERFORM S02-RED-W211FEL-GNRL                                         
106100     MOVE '107'                  TO W211FEL-IDFELKODX                     
106200     MOVE M107-M107              TO W211FEL-FELMED                        
106300                                                                          
106400     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
106500     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
106600     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
106700     PERFORM S01-SKAPA-ZZAC01                                             
106800     .                                                                    
106900     EJECT                                                                
107000 DAB-MEDDELANDE-M108 SECTION.                                             
107100                                                                          
107200     MOVE +0                     TO M108-IDLOPNRM                         
107300     MOVE ARTC11-CLAG-ADLAGOMR   TO M108-ADLAGOMR                         
107400     MOVE ARTC11-CLAG-ADGANG     TO M108-ADGANG                           
107500     MOVE ARTC11-CLAG-ADPLATS    TO M108-ADPLATS                          
107600                                                                          
107700     PERFORM S02-RED-W211FEL-GNRL                                         
107800     MOVE '108'                  TO W211FEL-IDFELKODX                     
107900     MOVE M108-M108              TO W211FEL-FELMED                        
108000                                                                          
108100     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
108200     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
108300     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
108400     PERFORM S01-SKAPA-ZZAC01                                             
108500     .                                                                    
108600     EJECT                                                                
108700 DAD-MEDDELANDE-M113 SECTION.                                             
108800                                                                          
108900     MOVE +0                     TO M113-IDLOPNRM                         
109000     MOVE MID-IDAVINR (MID-IX)   TO M113-IDAVINR                          
109100     MOVE MID-IDLEVNR (MID-IX)   TO M113-IDLEVNR                          
109200     MOVE MID-KDRT (MID-IX)      TO M113-KDRT                             
109300     MOVE MID-KVAVIS (MID-IX)    TO M113-KVANTAL                          
109400     MOVE ARTC11-CLAG-KDERS      TO M113-KDERS                            
109500     MOVE ARTC11-CLAG-KDLTK      TO M113-KDLTK                            
109600     MOVE ARTC11-CLAG-IDANSK     TO M113-IDANSKNR                         
109700                                                                          
109800     PERFORM S02-RED-W211FEL-GNRL                                         
109900     MOVE ARTC11-CLAG-IDANSK     TO W211FEL-IDKUNDNR-S                    
110000     MOVE '113'                  TO W211FEL-IDFELKODX                     
110100     MOVE M113-M113              TO W211FEL-FELMED                        
110200                                                                          
110300     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
110400     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
110500     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
110600     PERFORM S01-SKAPA-ZZAC01                                             
110700     .                                                                    
110800     EJECT                                                                
110900 DAE-MEDDELANDE-M117 SECTION.                                             
111000                                                                          
111100     MOVE ARTC11-CLAG-BEFT       TO M117-BEFT                             
111200     MOVE ARTC11-CLAG-PRDIRLON   TO M117-PRDIRLON                         
111300     MOVE ARTC11-CLAG-PRDMTRL    TO M117-PRDMTRL                          
111400     MOVE ARTC11-CLAG-PROVRPAL   TO M117-PROVRPAL                         
111500     MOVE ARTC11-CLAG-KDVTH      TO M117-KDVTH                            
111600                                                                          
111700     PERFORM S02-RED-W211FEL-GNRL                                         
111800     MOVE ARTC11-CLAG-IDANSK     TO W211FEL-IDKUNDNR-S                    
111900     MOVE '117'                  TO W211FEL-IDFELKODX                     
112000     MOVE M117-M117              TO W211FEL-FELMED                        
112100                                                                          
112200     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
112300     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
112400     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
112500     PERFORM S01-SKAPA-ZZAC01                                             
112600     .                                                                    
112700     EJECT                                                                
112800 DBA-MEDDELANDE-LAB-WDR8 SECTION.                                         
112900                                                                          
113000     MOVE SPACE                  TO LAB-W510A18                           
113100                                                                          
113200     MOVE 'A18'                  TO LAB-IDPTYP                            
113300     EVALUATE MID-KDRT(MID-IX)                                            
113400       WHEN 40                                                            
113500         MOVE 'M40'              TO LAB-KDEKOHT                           
113600       WHEN 41                                                            
113700         MOVE 'M41'              TO LAB-KDEKOHT                           
113800       WHEN 42                                                            
113900         MOVE 'M42'              TO LAB-KDEKOHT                           
114000       WHEN 43                                                            
114100         MOVE 'M43'              TO LAB-KDEKOHT                           
114200       WHEN 44                                                            
114300         MOVE 'M44'              TO LAB-KDEKOHT                           
114400       WHEN 45                                                            
114500         MOVE 'I83'              TO LAB-KDEKOHT                           
114600       WHEN 80                                                            
114700         MOVE 'I80'              TO LAB-KDEKOHT                           
114800       WHEN 81                                                            
114900         MOVE 'I81'              TO LAB-KDEKOHT                           
115000       WHEN 82                                                            
115100         MOVE 'I82'              TO LAB-KDEKOHT                           
115200     END-EVALUATE                                                         
115300     IF NDC-CA                                                            
115400       MOVE 54                   TO LAB-IDFTG                             
115500     ELSE                                                                 
115600       MOVE 53                   TO LAB-IDFTG                             
115700     END-IF                                                               
115800     MOVE MID-IDDC               TO LAB-IDDC-SEND                         
115900                                    LAB-IDDC-REC                          
116000     MOVE MID-IDLEVNR (MID-IX)   TO LAB-IDLEVNR                           
116100     MOVE MID-IDAVINR (MID-IX)   TO LAB-IDFS                              
116200     MOVE MID-IDARTNR (MID-IX)   TO LAB-IDARTNR                           
116300     MOVE SPAR-ARTC01-KDPRODSL   TO LAB-KDPRODSL                          
116400     MOVE SPAR-ARTC11-KDPSLLOC   TO LAB-KDPSLLOC                          
116500     MOVE WS-DAGENS-DATUM        TO LAB-DAINLINL                          
116600     MOVE MID-KVAVIS (MID-IX)    TO LAB-KVANTMOT                          
116700     MOVE SLAG-PRAVCOST          TO LAB-PRAVCOST                          
116800     MOVE MID-IDKONTO (MID-IX)   TO LAB-IDKONTO                           
116900                                                                          
117000     MOVE 'W6011C00'        TO EKO-FIL-IDPGM                              
117100     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
117200     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
117300     MOVE +1                TO EKO-FIL-IDSEKVNR                           
117400     MOVE 'W510'            TO EKO-FIL-CT-IDSYSTEM                        
117500     MOVE 'A18'             TO EKO-FIL-CT-IDPTYP                          
117600     MOVE ' '               TO EKO-FIL-CT-IDVTYP                          
117700                                                                          
117800     PERFORM IMS-ISRT-EKOTRANS                                            
117900                                                                          
118000     PERFORM UNTIL SEGMENT-FINNS                                          
118100       ADD +1 TO EKO-FIL-IDSEKVNR                                         
118200       PERFORM IMS-ISRT-EKOTRANS                                          
118300     END-PERFORM                                                          
118400     .                                                                    
118500     EJECT                                                                
118600 DBB-MEDDELANDE-EKO-WDR8-WDR9 SECTION.                                    
118700                                                                          
118800     MOVE SPACE                      TO EKO-W51080                        
118900     MOVE SPACE                      TO WS-SAP-MM-POST                    
119000                                                                          
119100     MOVE MID-IDARTNR (MID-IX)       TO EKO-IDARTNR                       
119200     MOVE MID-IDDC                   TO EKO-IDDC                          
119300     MOVE MID-IDAVINR (MID-IX)       TO EKO-IDFS                          
119400     MOVE ARTC11-CLAG-IDINK          TO EKO-IDINK                         
119500     MOVE MID-IDKONTO (MID-IX)       TO EKO-IDKONTO                       
119600     MOVE MID-IDLEVNR (MID-IX)       TO EKO-IDLEVNR                       
119700     MOVE ZERO                       TO EKO-IDLOPNRM                      
119800     MOVE SPAR-ARTC01-KDPRODSL       TO EKO-KDPRODSL                      
119900     MOVE MID-KDRT (MID-IX)          TO EKO-KDRT                          
120000     MOVE SPAR-ARTC01-KDSORT         TO EKO-KDSORT                        
120100     MOVE ARTC11-CLAG-KDTIPPR        TO EKO-KDTIPPR                       
120200*                                                                         
120300     IF MID-IDLEVNR (MID-IX) = '1441'                                     
120400       MOVE NEJ                      TO WS-SAP-MM-POST                    
120500     END-IF                                                               
120600*                                                                         
120700     IF SPAR-ARTC21-PRARTBEL-PR > ZERO                                    
120800        MOVE SPAR-ARTC21-PRARTBEL-PR TO EKO-PRARTBEL-PR                   
120900        MOVE SPAR-ARTC21-KDVALISO    TO EKO-KDVALISO                      
121000     ELSE                                                                 
121100*  -- OBS! RADPRIS SAKNAS, KDVALISO=XXX SIGNALERAR DETTA FÖR LEVA1        
121200        MOVE 0.1                     TO EKO-PRARTBEL-PR                   
121300        MOVE 'XXX'                   TO EKO-KDVALISO                      
121400     END-IF                                                               
121500     MOVE MID-KVAVIS (MID-IX)        TO EKO-KVAVIS                        
121600     IF SPAR-ARTC21-PRARTBES-PR > ZERO                                    
121700       MOVE SPAR-ARTC21-PRARTBES-PR  TO EKO-PRARTBES                      
121800     ELSE                                                                 
121900       MOVE 0.1                      TO EKO-PRARTBES                      
122000     END-IF                                                               
122100     MOVE ARTC11-CLAG-PRINK          TO EKO-PRINK                         
122200     MOVE ARTC11-CLAG-PRHEMTAG       TO EKO-PRHEMTAG                      
122300     MOVE ZERO                       TO EKO-RETULF                        
122400     MOVE MID-TIAVIDAT (MID-IX)      TO EKO-TIAVIDAT                      
122500     MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                      
122600     MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                       
122700     MOVE JA                         TO EKO-FLLSBOK                       
122800     MOVE ZERO                       TO EKO-IDDISTR                       
122900     MOVE NEJ                        TO EKO-FLDIRLEV                      
123000     MOVE NEJ                        TO EKO-FLAVVINL                      
123100     MOVE ' '                        TO EKO-KDINLAVV                      
123200     MOVE WS-ARTC23-IDAVTAL          TO EKO-IDAVTAL                       
123300     MOVE 57                         TO EKO-IDFTG                         
123400                                                                          
123500     MOVE 'W6011C00'                 TO EKO-FIL-IDPGM                     
123600     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
123700     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
123800     MOVE 1                          TO EKO-FIL-IDSEKVNR                  
123900     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
124000     MOVE '80 '                      TO EKO-FIL-CT-IDPTYP                 
124100     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
124200                                                                          
124300     IF NOT XDC-NON-VCC-OWNED AND NOT LDC-CN                              
124400       IF WS-SAP-MM-POST NOT = NEJ                                        
124500         PERFORM IMS-ISRT-EKOTRANS                                        
124600                                                                          
124700         PERFORM UNTIL SEGMENT-FINNS                                      
124800           ADD +1 TO EKO-FIL-IDSEKVNR                                     
124900           PERFORM IMS-ISRT-EKOTRANS                                      
125000         END-PERFORM                                                      
125100       END-IF                                                             
125200     END-IF                                                               
125300                                                                          
125400     IF XDC-NON-VCC-OWNED OR LDC-CN                                       
125500       MOVE 'W6011C00'             TO EKO-FIL-IDPGM                       
125600       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
125700       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
125800       MOVE FUNCTION CURRENT-DATE (1:8)                                   
125900                                   TO EKO-EKH-DAVERDAT                    
126000       MOVE +1                     TO EKO-FIL-IDSEKVNR                    
126100       MOVE WS-IDDC                TO W-IDDC                              
126200       PERFORM IMS-GU-WDB601                                              
126300       IF NDC-CN OR LDC-CN                                                
126400         MOVE 'W570'               TO EKO-FIL-IDCPYTXT(1:4)               
126500       ELSE                                                               
126600         IF NDC-IN                                                        
126700           MOVE 'W515'             TO EKO-FIL-IDCPYTXT(1:4)               
126800         ELSE                                                             
126900           MOVE DCS-KDTRADP        TO EKO-FIL-IDCPYTXT(1:4)               
127000         END-IF                                                           
127100       END-IF                                                             
127200       MOVE 'EKHA'                 TO EKO-FIL-IDCPYTXT(5:4)               
127300       PERFORM DBBA-MEDDELANDE-EKO-WDR8-DET                               
127400**** HEMT SHOULD ONLY BE USED WHEN LOCAL SOURCING                         
127500       IF NDC-CN OR LDC-CN                                                
127600         PERFORM DBBA-MEDDELANDE-EKO-WDR8-HEMT                            
127700       END-IF                                                             
127800       IF ARTC11-CLAG-PRINK = ARTC11-CLAG-PRARTSTD                        
127900         CONTINUE                                                         
128000       ELSE                                                               
128100         MOVE WS-IDDC         TO W-IDDC                                   
128200         PERFORM IMS-GU-WDB601                                            
128300         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
128400         IF SEGMENT-FINNS                                                 
128500           PERFORM IMS-GNP-WDB617                                         
128600           IF SEGMENT-FINNS                                               
128700             PERFORM DBBA-MEDDELANDE-EKO-WDR8-KALK                        
128800           END-IF                                                         
128900         END-IF                                                           
129000       END-IF                                                             
129100       PERFORM DBBA-MEDDELANDE-EKO-WDR8-SUM                               
129200     ELSE                                                                 
129300       MOVE 'W6011C00'             TO FIL-IDPGM IN FIL-WDR901             
129400       MOVE FUNCTION CURRENT-DATE (1:8)                                   
129500                                   TO FIL-DAREGDAT                        
129600                                      EKH-DAVERDAT                        
129700       MOVE FUNCTION CURRENT-DATE (9:8)                                   
129800                                   TO FIL-TIKLOCK  IN FIL-WDR901          
129900       MOVE +1                     TO FIL-IDSEKVNR IN FIL-WDR901          
130000       MOVE 'W510EKHA'             TO FIL-IDCPYTXT IN FIL-WDR901          
130100       MOVE MSG-SIGNON-USERID      TO FIL-IDUSER                          
130200       PERFORM DBBB-MEDDELANDE-EKO-WDR9                                   
130300     END-IF                                                               
130400     .                                                                    
130500     EJECT                                                                
130600                                                                          
130700 DBBA-MEDDELANDE-EKO-WDR8-DET SECTION.                                    
130800     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
130900     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
131000     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
131100     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
131200     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
131300     MOVE +0                          TO EKO-EKH-IDDISTR                  
131400     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
131500*******************************                                           
131600*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
131700     MOVE ZERO TO NOLL-RAKNARE                                            
131800     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
131900     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
132000          FOR LEADING ZERO                                                
132100     ADD +1 TO NOLL-RAKNARE                                               
132200     UNSTRING WS-SAP-X-IDAVINR     INTO EKO-EKH-IDVERGL                   
132300          WITH POINTER NOLL-RAKNARE                                       
132400*******************************                                           
132500     MOVE SPAR-ARTC01-KDPRODSL        TO EKO-EKH-KDPRODSL                 
132600     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
132700                                         EKO-EKH-PRARTNTO                 
132800                                         EKO-EKH-PRARTSJK                 
132900                                         EKO-EKH-PRLANDCO                 
133000                                         EKO-EKH-SUBEL                    
133100     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
133200     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
133300                                                                          
133400     MOVE ZERO                        TO EKO-EKH-PRINK                    
133500     MOVE MID-KVAVIS(MID-IX)          TO EKO-EKH-KVANTAL                  
133600     MOVE '6117'                      TO EKO-EKH-IDTRANS                  
133700     MOVE SPACE                       TO EKO-EKH-BEVAT                    
133800                                         EKO-EKH-KDANMORS                 
133900     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
134000                                         EKO-EKH-SUVAT                    
134100     MOVE MID-IDANALYS (MID-IX)       TO EKO-EKH-IDANALYS                 
134200     MOVE MID-IDKONTO (MID-IX)        TO EKO-EKH-IDKONTO                  
134300     MOVE MID-IDKST (MID-IX)          TO EKO-EKH-IDKST                    
134400     IF NDC-CN                                                            
134500       PERFORM DBBAA-GET-PRARTBEL                                         
134600     END-IF                                                               
134700     PERFORM DDAB-GET-CURRENCY-RATE                                       
134800                                                                          
134900*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
135000*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
135100     IF XDC-NON-VCC-OWNED                                                 
135200     AND NOT (NDC-CN OR NDC-US)                                           
135300       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
135400     END-IF                                                               
135500***                                                                       
135600     IF SPAR-ARTC21-PRARTBEL-PR > ZERO                                    
135700       MOVE SPAR-ARTC21-PRARTBEL-PR   TO EKO-EKH-PRARTSTD                 
135800       MOVE SPAR-ARTC21-KDVALISO      TO EKO-EKH-KDVALISO                 
135900       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
136000     ELSE                                                                 
136100       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
136200       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
136300       MOVE 1.00                      TO EKO-EKH-PRKURS                   
136400     END-IF                                                               
136500     COMPUTE WS-SUARTSTD = SPAR-ARTC21-PRARTBEL-SUM *                     
136600                           MID-KVAVIS(MID-IX)                             
136700     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
136800     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
136900                                         EKO-EKH-PRDMTRL                  
137000                                         EKO-EKH-PROVRPAL                 
137100     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAAVIDAT                 
137200     MOVE MID-IDAVINR (MID-IX)        TO EKO-EKH-IDAVINR                  
137300     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
137400     IF (MID-KDRT(MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7) OR           
137500       ((MID-KDRT(MID-IX) = 9 OR 10) AND                                  
137600       (MID-IDLEVNR(MID-IX) = '1001 ' OR '1003 '                          
137700                           OR 'BL3YA' OR 'BP2TH'))                        
137800       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
137900     ELSE                                                                 
138000       MOVE ZERO                      TO EKO-EKH-KDAVVTYP                 
138100     END-IF                                                               
138200     MOVE MID-KDRT(MID-IX)            TO EKO-EKH-KDRT                     
138300     MOVE MID-KVAVIS(MID-IX)          TO EKO-EKH-KVANTMOT                 
138400                                         EKO-EKH-KVAVIS                   
138500     MOVE SPAR-ARTC01-KDSORT          TO EKO-EKH-KDSORT                   
138600     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
138700     MOVE SPACE                       TO EKO-EKH-FLDCET                   
138800     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
138900     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
139000                                                                          
139100     PERFORM IMS-ISRT-EKOTRANS                                            
139200                                                                          
139300     PERFORM UNTIL SEGMENT-FINNS                                          
139400       ADD +1 TO EKO-FIL-IDSEKVNR                                         
139500       PERFORM IMS-ISRT-EKOTRANS                                          
139600     END-PERFORM                                                          
139700     .                                                                    
139800     EJECT                                                                
139900                                                                          
140000 DBBA-MEDDELANDE-EKO-WDR8-HEMT SECTION.                                   
140100     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
140200     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
140300     MOVE 'HEMT '                     TO EKO-EKH-KDEKNIVA                 
140400     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
140500     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
140600     MOVE +0                          TO EKO-EKH-IDDISTR                  
140700     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
140800*******************************                                           
140900*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
141000     MOVE ZERO TO NOLL-RAKNARE                                            
141100     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
141200     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
141300          FOR LEADING ZERO                                                
141400     ADD +1 TO NOLL-RAKNARE                                               
141500     UNSTRING WS-SAP-X-IDAVINR     INTO EKO-EKH-IDVERGL                   
141600          WITH POINTER NOLL-RAKNARE                                       
141700*******************************                                           
141800     MOVE SPAR-ARTC01-KDPRODSL        TO EKO-EKH-KDPRODSL                 
141900     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
142000                                         EKO-EKH-PRARTNTO                 
142100                                         EKO-EKH-PRARTSJK                 
142200                                         EKO-EKH-PRLANDCO                 
142300     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
142400     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
142500                                                                          
142600     MOVE ZERO                        TO EKO-EKH-PRINK                    
142700     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
142800     MOVE '6117'                      TO EKO-EKH-IDTRANS                  
142900     MOVE SPACE                       TO EKO-EKH-BEVAT                    
143000                                         EKO-EKH-KDANMORS                 
143100     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
143200                                         EKO-EKH-SUVAT                    
143300     MOVE MID-IDANALYS (MID-IX)       TO EKO-EKH-IDANALYS                 
143400     MOVE MID-IDKONTO (MID-IX)        TO EKO-EKH-IDKONTO                  
143500     MOVE MID-IDKST (MID-IX)          TO EKO-EKH-IDKST                    
143600     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
143700******                                                                    
143800     MOVE MID-IDLEVNR (MID-IX)  TO W-IDLEVNR                              
143900     PERFORM IMS-GU-WDF101                                                
144000     IF SEGMENT-SAKNAS                                                    
144100       MOVE ZERO TO W-RETULF                                              
144200     ELSE                                                                 
144300       MOVE WS-IDDC          TO W-IDDC                                    
144400       PERFORM IMS-GU-WDB601                                              
144500       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
144600       PERFORM IMS-GNP-WDF102                                             
144700       IF SEGMENT-FINNS                                                   
144800         IF F102-TULL-TITULF < WS-DAGENS-DATUM                            
144900           MOVE F102-TULL-RETULF-1  TO W-RETULF                           
145000         ELSE                                                             
145100           MOVE F102-TULL-RETULF-2  TO W-RETULF                           
145200         END-IF                                                           
145300       END-IF                                                             
145400     END-IF                                                               
145500****** EFTERSOM HEMTAG ÄR EN DEL AV STANDARDPRISET BEHÖVER RÄKNA          
145600     IF NDC-CN                                                            
145700       PERFORM DBBAA-GET-PRARTBEL                                         
145800     END-IF                                                               
145900     PERFORM DDAB-GET-CURRENCY-RATE                                       
146000                                                                          
146100*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
146200*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
146300     IF XDC-NON-VCC-OWNED                                                 
146400     AND NOT (NDC-CN OR NDC-US)                                           
146500       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
146600     END-IF                                                               
146700***                                                                       
146800     IF SPAR-ARTC21-PRARTBEL-PR > ZERO                                    
146900       MOVE SPAR-ARTC21-PRARTBEL-PR   TO EKO-EKH-PRARTSTD                 
147000       MOVE SPAR-ARTC21-KDVALISO      TO EKO-EKH-KDVALISO                 
147100       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
147200     ELSE                                                                 
147300       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
147400       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
147500       MOVE 1.00                      TO EKO-EKH-PRKURS                   
147600     END-IF                                                               
147700     COMPUTE EKO-EKH-PRHEMTAG ROUNDED = (W-RETULF - 1) *                  
147800                         EKO-EKH-PRARTSTD * MID-KVAVIS(MID-IX)            
147900     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
148000     MOVE EKO-EKH-PRHEMTAG            TO EKO-EKH-SUBEL                    
148100     MOVE EKO-EKH-PRHEMTAG            TO WS-SUHEMT                        
148200     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
148300                                         EKO-EKH-PRDMTRL                  
148400                                         EKO-EKH-PROVRPAL                 
148500     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAAVIDAT                 
148600     MOVE MID-IDAVINR (MID-IX)        TO EKO-EKH-IDAVINR                  
148700     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
148800     IF (MID-KDRT(MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7) OR           
148900       ((MID-KDRT(MID-IX) = 9 OR 10) AND                                  
149000       (MID-IDLEVNR(MID-IX) = '1001 ' OR '1003 '                          
149100                           OR 'BL3YA' OR 'BP2TH'))                        
149200       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
149300     ELSE                                                                 
149400       MOVE ZERO                      TO EKO-EKH-KDAVVTYP                 
149500     END-IF                                                               
149600     MOVE MID-KDRT(MID-IX)            TO EKO-EKH-KDRT                     
149700     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
149800                                         EKO-EKH-KVAVIS                   
149900     MOVE SPAR-ARTC01-KDSORT          TO EKO-EKH-KDSORT                   
150000     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
150100     MOVE SPACE                       TO EKO-EKH-FLDCET                   
150200     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
150300     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
150400                                                                          
150500     IF EKO-EKH-PRHEMTAG > ZERO                                           
150600       PERFORM IMS-ISRT-EKOTRANS                                          
150700                                                                          
150800       PERFORM UNTIL SEGMENT-FINNS                                        
150900         ADD +1 TO EKO-FIL-IDSEKVNR                                       
151000         PERFORM IMS-ISRT-EKOTRANS                                        
151100       END-PERFORM                                                        
151200     END-IF                                                               
151300     .                                                                    
151400     EJECT                                                                
151500                                                                          
151600 DBBA-MEDDELANDE-EKO-WDR8-KALK SECTION.                                   
151700     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
151800     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
151900     MOVE 'KALK '                     TO EKO-EKH-KDEKNIVA                 
152000     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
152100     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
152200     MOVE +0                          TO EKO-EKH-IDDISTR                  
152300     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
152400*******************************                                           
152500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
152600     MOVE ZERO TO NOLL-RAKNARE                                            
152700     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
152800     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
152900          FOR LEADING ZERO                                                
153000     ADD +1 TO NOLL-RAKNARE                                               
153100     UNSTRING WS-SAP-X-IDAVINR     INTO EKO-EKH-IDVERGL                   
153200          WITH POINTER NOLL-RAKNARE                                       
153300*******************************                                           
153400     MOVE SPAR-ARTC01-KDPRODSL        TO EKO-EKH-KDPRODSL                 
153500     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
153600                                         EKO-EKH-PRARTNTO                 
153700                                         EKO-EKH-PRARTSJK                 
153800                                         EKO-EKH-PRLANDCO                 
153900     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
154000     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
154100                                                                          
154200     MOVE 1.00                        TO EKO-EKH-PRKURS                   
154300                                                                          
154400     MOVE ZERO                        TO EKO-EKH-PRINK                    
154500     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
154600     MOVE '6117'                      TO EKO-EKH-IDTRANS                  
154700     MOVE SPACE                       TO EKO-EKH-BEVAT                    
154800                                         EKO-EKH-KDANMORS                 
154900     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
155000                                         EKO-EKH-SUVAT                    
155100     MOVE MID-IDANALYS (MID-IX)       TO EKO-EKH-IDANALYS                 
155200     MOVE MID-IDKONTO (MID-IX)        TO EKO-EKH-IDKONTO                  
155300     MOVE MID-IDKST (MID-IX)          TO EKO-EKH-IDKST                    
155400                                                                          
155500     MOVE WS-DAGENS-DATUM(3:2) TO W-DATE-AAMM(1:2)                        
155600     MOVE 01                   TO W-DATE-AAMM(3:2)                        
155700     MOVE 'SEK'                TO CURR-KDVALISO-HUV                       
155800     MOVE DCS-KDVALISO         TO CURR-KDVALISO-ROW                       
155900     MOVE W-DATE-AAMM          TO CURR-TIAAMM                             
156000     MOVE 'A'                  TO CURR-KDVALTYP                           
156100     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
156200     IF CURR-KDSVAR = ' '                                                 
156300       MOVE CURR-PRKURS-NEW    TO W-PRKURS                                
156400       MOVE CURR-REVALUTA-TO   TO W-REVALUTA                              
156500     ELSE                                                                 
156600       MOVE 1                  TO W-PRKURS                                
156700       MOVE 1                  TO W-REVALUTA                              
156800     END-IF                                                               
156900     COMPUTE EKO-EKH-PRDIRLON ROUNDED = ARTC11-CLAG-PRDIRLON *            
157000       PROC-REDIRLON /  W-PRKURS / W-REVALUTA * MID-KVAVIS(MID-IX)        
157100     COMPUTE EKO-EKH-PRDMTRL  ROUNDED = ARTC11-CLAG-PRDMTRL  *            
157200       PROC-REDMTRL  /  W-PRKURS / W-REVALUTA * MID-KVAVIS(MID-IX)        
157300     MOVE EKO-EKH-PRDMTRL             TO WS-SUDIRMTRL                     
157400     MOVE EKO-EKH-PRDIRLON            TO WS-SUDIRLON                      
157500     COMPUTE EKO-EKH-SUBEL = EKO-EKH-PRDIRLON +                           
157600                             EKO-EKH-PRDMTRL                              
157700     MOVE ZERO                        TO EKO-EKH-PROVRPAL                 
157800     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
157900     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
158000     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAAVIDAT                 
158100     MOVE MID-IDAVINR (MID-IX)        TO EKO-EKH-IDAVINR                  
158200     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
158300     IF (MID-KDRT(MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7) OR           
158400       ((MID-KDRT(MID-IX) = 9 OR 10) AND                                  
158500       (MID-IDLEVNR(MID-IX) = '1001 ' OR '1003 '                          
158600                           OR 'BL3YA' OR 'BP2TH'))                        
158700       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
158800     ELSE                                                                 
158900       MOVE ZERO                      TO EKO-EKH-KDAVVTYP                 
159000     END-IF                                                               
159100     MOVE MID-KDRT(MID-IX)            TO EKO-EKH-KDRT                     
159200     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
159300                                         EKO-EKH-KVAVIS                   
159400     MOVE SPAR-ARTC01-KDSORT          TO EKO-EKH-KDSORT                   
159500     MOVE SPACE                       TO EKO-EKH-FLDCET                   
159600     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
159700     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
159800     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
159900     IF NDC-CN                                                            
160000       MOVE 'W570'                    TO EKO-FIL-IDCPYTXT(1:4)            
160100     ELSE                                                                 
160200       IF NDC-IN                                                          
160300         MOVE 'W515'                  TO EKO-FIL-IDCPYTXT(1:4)            
160400       ELSE                                                               
160500         MOVE DCS-KDTRADP             TO EKO-FIL-IDCPYTXT(1:4)            
160600       END-IF                                                             
160700     END-IF                                                               
160800     MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)            
160900                                                                          
161000     IF EKO-EKH-SUBEL > ZERO                                              
161100       PERFORM IMS-ISRT-EKOTRANS                                          
161200                                                                          
161300       PERFORM UNTIL SEGMENT-FINNS                                        
161400         ADD +1 TO EKO-FIL-IDSEKVNR                                       
161500         PERFORM IMS-ISRT-EKOTRANS                                        
161600       END-PERFORM                                                        
161700     END-IF                                                               
161800     .                                                                    
161900     EJECT                                                                
162000                                                                          
162100 DBBA-MEDDELANDE-EKO-WDR8-SUM SECTION.                                    
162200     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
162300     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
162400     MOVE 'SUM  '                     TO EKO-EKH-KDEKNIVA                 
162500     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
162600     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
162700     MOVE +0                          TO EKO-EKH-IDDISTR                  
162800     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
162900*******************************                                           
163000*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
163100     MOVE ZERO TO NOLL-RAKNARE                                            
163200     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
163300     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
163400          FOR LEADING ZERO                                                
163500     ADD +1 TO NOLL-RAKNARE                                               
163600     UNSTRING WS-SAP-X-IDAVINR     INTO EKO-EKH-IDVERGL                   
163700          WITH POINTER NOLL-RAKNARE                                       
163800*******************************                                           
163900     MOVE SPAR-ARTC01-KDPRODSL        TO EKO-EKH-KDPRODSL                 
164000     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
164100                                         EKO-EKH-PRARTNTO                 
164200                                         EKO-EKH-PRARTSJK                 
164300                                         EKO-EKH-PRLANDCO                 
164400     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
164500     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
164600                                                                          
164700                                                                          
164800     MOVE ZERO                        TO EKO-EKH-PRINK                    
164900     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
165000     MOVE '6117'                      TO EKO-EKH-IDTRANS                  
165100     MOVE SPACE                       TO EKO-EKH-BEVAT                    
165200                                         EKO-EKH-KDANMORS                 
165300     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
165400                                         EKO-EKH-SUVAT                    
165500     MOVE MID-IDANALYS (MID-IX)       TO EKO-EKH-IDANALYS                 
165600     MOVE MID-IDKONTO (MID-IX)        TO EKO-EKH-IDKONTO                  
165700     MOVE MID-IDKST (MID-IX)          TO EKO-EKH-IDKST                    
165800     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
165900     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
166000                                         EKO-EKH-PRDMTRL                  
166100                                         EKO-EKH-PROVRPAL                 
166200                                         EKO-EKH-PRARTSTD                 
166300*    COMPUTE EKO-EKH-SUBEL = WS-SUDIRLON + WS-SUDIRMTRL +                 
166400*                            WS-SUHEMT + WS-SUARTSTD                      
166500     COMPUTE EKO-EKH-SUBEL = WS-SUARTSTD                                  
166600     MOVE SPAR-ARTC21-KDVALISO        TO EKO-EKH-KDVALISO                 
166700     MOVE SPAR-PRKURS                 TO EKO-EKH-PRKURS                   
166800     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAAVIDAT                 
166900     MOVE MID-IDAVINR (MID-IX)        TO EKO-EKH-IDAVINR                  
167000     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
167100     IF (MID-KDRT(MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7) OR           
167200       ((MID-KDRT(MID-IX) = 9 OR 10) AND                                  
167300       (MID-IDLEVNR(MID-IX) = '1001 ' OR '1003 '                          
167400                           OR 'BL3YA' OR 'BP2TH'))                        
167500       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
167600     ELSE                                                                 
167700       MOVE ZERO                      TO EKO-EKH-KDAVVTYP                 
167800     END-IF                                                               
167900     MOVE MID-KDRT(MID-IX)            TO EKO-EKH-KDRT                     
168000     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
168100                                         EKO-EKH-KVAVIS                   
168200     MOVE SPAR-ARTC01-KDSORT          TO EKO-EKH-KDSORT                   
168300     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
168400     MOVE SPACE                       TO EKO-EKH-FLDCET                   
168500     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
168600     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
168700                                                                          
168800     PERFORM IMS-ISRT-EKOTRANS                                            
168900                                                                          
169000     PERFORM UNTIL SEGMENT-FINNS                                          
169100       ADD +1 TO EKO-FIL-IDSEKVNR                                         
169200       PERFORM IMS-ISRT-EKOTRANS                                          
169300     END-PERFORM                                                          
169400     .                                                                    
169500     EJECT                                                                
169600                                                                          
169700 DBBAA-GET-PRARTBEL SECTION.                                              
169800     MOVE +0                TO SPAR-ARTC21-PRARTBEL-PR                    
169900                               SPAR-ARTC21-PRARTBEL-SUM                   
170000                               SPAR-ARTC21-PRARTBES-PR                    
170100                                                                          
170200*    -- WDK711                                                            
170300     PERFORM IMS-GHU-WDK711-K7                                            
170400     IF SEGMENT-FINNS                                                     
170500                                                                          
170600*    -- WDK724                                                            
170700       MOVE MID-IDLEVNR(MID-IX) TO W-IDLEVNR-PR                           
170800       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAGENS-DATUM                 
170900       PERFORM IMS-GHNP-WDK724-K7                                         
171000                                                                          
171100       IF SEGMENT-FINNS                                                   
171200         MOVE SPRL-PRARTBEL-PR  TO SPAR-ARTC21-PRARTBEL-PR                
171300         MOVE SPRL-PRARTBEL-PR  TO SPAR-ARTC21-PRARTBEL-SUM               
171400         MOVE SPRL-PRARTBES-PR  TO SPAR-ARTC21-PRARTBES-PR                
171500         MOVE SPRL-KDVALISO     TO SPAR-ARTC21-KDVALISO                   
171600       END-IF                                                             
171700     END-IF                                                               
171800     .                                                                    
171900     EJECT                                                                
172000                                                                          
172100 DDAB-GET-CURRENCY-RATE SECTION.                                          
172200     MOVE WS-IDDC         TO W-IDDC                                       
172300     PERFORM IMS-GU-WDB601                                                
172400     MOVE DCS-KDVALISO      TO W-KDVALISO-HUV                             
172500     IF W-KDVALISO-HUV = SPAR-ARTC21-KDVALISO                             
172600       MOVE 1 TO SPAR-PRKURS                                              
172700       MOVE 1 TO W-REVALUTA                                               
172800     ELSE                                                                 
172900       MOVE SPAR-ARTC21-KDVALISO TO W-KDVALISO-ROW                        
173000       PERFORM IMS-GU-WDGX9306                                            
173100       IF SEGMENT-SAKNAS                                                  
173200         MOVE 1               TO SPAR-PRKURS                              
173300         MOVE 1               TO W-REVALUTA                               
173400       ELSE                                                               
173500         COMPUTE W-TISTADAT-9KOMPL =                                      
173600                 9999999 - WS-DATE-YYMMDD                                 
173700         PERFORM IMS-GNP-WDGX9308                                         
173800         IF SEGMENT-SAKNAS                                                
173900           PERFORM IMS-GNP-WDGX9308-FIRST                                 
174000           IF SEGMENT-SAKNAS                                              
174100             MOVE 1               TO SPAR-PRKURS                          
174200             MOVE 1               TO W-REVALUTA                           
174300           ELSE                                                           
174400             MOVE 9308-PRKURS   TO SPAR-PRKURS                            
174500             MOVE 9308-REVALUTA-TO TO W-REVALUTA                          
174600           END-IF                                                         
174700         ELSE                                                             
174800           MOVE 9308-PRKURS   TO SPAR-PRKURS                              
174900           MOVE 9308-REVALUTA-TO TO W-REVALUTA                            
175000         END-IF                                                           
175100       END-IF                                                             
175200     END-IF                                                               
175300     .                                                                    
175400     EJECT                                                                
175500                                                                          
175600 DDAC-GET-CURR-RATE-LOCAL SECTION.                                        
175700                                                                          
175800     IF W-KDVALISO-HUV = SPAR-ARTC21-KDVALISO                             
175900       CONTINUE                                                           
176000     ELSE                                                                 
176100       MOVE SPAR-ARTC01-KDPRODSL       TO TEST-KDPRODSL                   
176200       IF KDPRODSL-LOCAL                                                  
176300         MOVE MID-TIAVIDAT(MID-IX)     TO WS-DAAVIDAT                     
176400         MOVE WS-DAAVIDAT(1:2)         TO W-DATE-AAMM(1:2)                
176500         MOVE WS-DAAVIDAT(3:2)         TO W-DATE-AAMM(3:2)                
176600         MOVE W-KDVALISO-HUV           TO CURR-KDVALISO-HUV               
176700         MOVE SPAR-ARTC21-KDVALISO     TO CURR-KDVALISO-ROW               
176800         MOVE W-DATE-AAMM              TO CURR-TIAAMM                     
176900         MOVE 'M'                      TO CURR-KDVALTYP                   
177000         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
177100         IF CURR-KDSVAR = ' '                                             
177200           MOVE CURR-PRKURS-NEW        TO SPAR-PRKURS                     
177300           MOVE CURR-REVALUTA-TO       TO W-REVALUTA                      
177400         ELSE                                                             
177500           MOVE 1                      TO SPAR-PRKURS                     
177600           MOVE 1                      TO W-REVALUTA                      
177700         END-IF                                                           
177800**** PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED                      
177900**** TO COUNTRY CURRENCY                                                  
178000         COMPUTE SPAR-ARTC21-PRARTBEL-PR =                                
178100         SPAR-ARTC21-PRARTBEL-PR * SPAR-PRKURS / W-REVALUTA               
178200       END-IF                                                             
178300     END-IF                                                               
178400     .                                                                    
178500     EJECT                                                                
178600 DBBB-MEDDELANDE-EKO-WDR9 SECTION.                                        
178700     MOVE '103'                       TO EKH-KDEKHHT                      
178800     MOVE '102'                       TO EKH-KDEKSHT                      
178900     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
179000     MOVE MID-IDDC                    TO EKH-IDDC-SEND                    
179100     MOVE SPACE                       TO EKH-IDDC-REC                     
179200     MOVE +0                          TO EKH-IDDISTR                      
179300     MOVE +0                          TO EKH-IDKUNDNR                     
179400*******************************                                           
179500*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
179600     MOVE ZERO TO NOLL-RAKNARE                                            
179700     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
179800     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
179900          FOR LEADING ZERO                                                
180000     ADD +1 TO NOLL-RAKNARE                                               
180100     UNSTRING WS-SAP-X-IDAVINR     INTO EKH-IDVERGL                       
180200          WITH POINTER NOLL-RAKNARE                                       
180300*******************************                                           
180400     MOVE SPAR-ARTC01-KDPRODSL        TO EKH-KDPRODSL                     
180500     MOVE ZERO                        TO EKH-KDPSLLOC                     
180600                                         EKH-PRARTNTO                     
180700                                         EKH-PRARTSJK                     
180800                                         EKH-PRLANDCO                     
180900                                         EKH-SUBEL                        
181000     MOVE MID-IDARTNR(MID-IX)         TO EKH-IDARTNR                      
181100     MOVE SPACE                       TO EKH-FLLSBOK                      
181200                                                                          
181300     IF EKO-KDVALISO = 'XXX'                                              
181400       MOVE 'SEK'                     TO EKH-KDVALISO                     
181500     ELSE                                                                 
181600       MOVE EKO-KDVALISO              TO EKH-KDVALISO                     
181700     END-IF                                                               
181800     MOVE 1.00                        TO EKH-PRKURS                       
181900                                                                          
182000     MOVE ARTC11-CLAG-PRINK           TO EKH-PRINK                        
182100     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVANTAL                      
182200     MOVE '6117'                      TO EKH-IDTRANS                      
182300     MOVE SPACE                       TO EKH-BEVAT                        
182400                                         EKH-KDANMORS                     
182500     MOVE ZERO                        TO EKH-KDFRAKT                      
182600                                         EKH-SUVAT                        
182700     MOVE MID-IDANALYS (MID-IX)       TO EKH-IDANALYS                     
182800     MOVE MID-IDKONTO (MID-IX)        TO EKH-IDKONTO                      
182900     MOVE MID-IDKST (MID-IX)          TO EKH-IDKST                        
183000     MOVE ARTC11-CLAG-PRARTSTD        TO EKH-PRARTSTD                     
183100     IF MID-IDLEVNR(MID-IX) = '1002 '                                     
183200       MOVE ZERO                      TO EKH-PRHEMTAG                     
183300     ELSE                                                                 
183400       MOVE ARTC11-CLAG-PRHEMTAG      TO EKH-PRHEMTAG                     
183500     END-IF                                                               
183600     MOVE ZERO                        TO EKH-PRDIRLON                     
183700                                         EKH-PRDMTRL                      
183800                                         EKH-PROVRPAL                     
183900     MOVE FUNCTION CURRENT-DATE (1:8) TO EKH-DAAVIDAT                     
184000     MOVE MID-IDAVINR (MID-IX)        TO EKH-IDAVINR                      
184100     MOVE MID-IDLEVNR (MID-IX)        TO EKH-IDLEVNR                      
184200     IF (MID-KDRT(MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7) OR           
184300       ((MID-KDRT(MID-IX) = 9 OR 10) AND                                  
184400       (MID-IDLEVNR(MID-IX) = '1001 ' OR '1003 '                          
184500                           OR 'BL3YA' OR 'BP2TH'))                        
184600       MOVE 1                         TO EKH-KDAVVTYP                     
184700     ELSE                                                                 
184800       MOVE ZERO                      TO EKH-KDAVVTYP                     
184900     END-IF                                                               
185000     MOVE MID-KDRT(MID-IX)            TO EKH-KDRT                         
185100     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVANTMOT                     
185200                                         EKH-KVAVIS                       
185300     MOVE SPAR-ARTC01-KDSORT          TO EKH-KDSORT                       
185400     MOVE 'SEPV'                      TO EKH-KDTRADP                      
185500     MOVE SPACE                       TO EKH-FLDCET                       
185600     MOVE SPACE                       TO EKH-IDKUNDRF                     
185700     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
185800                                                                          
185900     PERFORM IMS-ISRT-WLSAPA01                                            
186000                                                                          
186100     PERFORM UNTIL SEGMENT-FINNS                                          
186200       ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                               
186300       PERFORM IMS-ISRT-WLSAPA01                                          
186400     END-PERFORM                                                          
186500     .                                                                    
186600     EJECT                                                                
186700                                                                          
186800 DC-MEDDELANDE-EKO-WDR8-WDR9 SECTION.                                     
186900     IF XDC-NON-VCC-OWNED OR LDC-CN                                       
187000       MOVE 'W6011C00'              TO EKO-FIL-IDPGM                      
187100       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
187200       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
187300       MOVE FUNCTION CURRENT-DATE (1:8)                                   
187400                                    TO EKO-EKH-DAVERDAT                   
187500       MOVE +1                      TO EKO-FIL-IDSEKVNR                   
187600       PERFORM IMS-GU-WDB601                                              
187700       IF NDC-CN OR LDC-CN                                                
187800         MOVE 'W570'                TO EKO-FIL-IDCPYTXT(1:4)              
187900       ELSE                                                               
188000         IF NDC-IN                                                        
188100           MOVE 'W515'              TO EKO-FIL-IDCPYTXT(1:4)              
188200         ELSE                                                             
188300           MOVE DCS-KDTRADP         TO EKO-FIL-IDCPYTXT(1:4)              
188400         END-IF                                                           
188500       END-IF                                                             
188600       MOVE 'EKHA'                  TO EKO-FIL-IDCPYTXT(5:4)              
188700       PERFORM DCA-MEDDELANDE-EKO-WDR8                                    
188800     ELSE                                                                 
188900       MOVE 'W6011C00'              TO FIL-IDPGM IN FIL-WDR901            
189000       MOVE FUNCTION CURRENT-DATE (1:8)                                   
189100                                    TO FIL-DAREGDAT                       
189200                                       EKH-DAVERDAT                       
189300       MOVE FUNCTION CURRENT-DATE (9:8)                                   
189400                                    TO FIL-TIKLOCK IN FIL-WDR901          
189500       MOVE +1                      TO FIL-IDSEKVNR IN FIL-WDR901         
189600       MOVE 'W510EKHA'              TO FIL-IDCPYTXT IN FIL-WDR901         
189700       MOVE MSG-SIGNON-USERID       TO FIL-IDUSER IN FIL-WDR901           
189800       PERFORM DCB-MEDDELANDE-EKO-WDR9                                    
189900     END-IF                                                               
190000     .                                                                    
190100     EJECT                                                                
190200                                                                          
190300 DCA-MEDDELANDE-EKO-WDR8 SECTION.                                         
190400                                                                          
190500     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
190600     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
190700     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
190800     IF (NDC-CN OR LDC-CN) AND                                            
190900        MID-IDKONTO (MID-IX) = 483105                                     
191000        MOVE '103'                    TO EKO-EKH-KDEKSHT                  
191100     ELSE                                                                 
191200        MOVE '102'                    TO EKO-EKH-KDEKSHT                  
191300        IF (NDC-CN OR LDC-CN) AND                                         
191400           ARTC11-CLAG-IDPROJ = 'OBJ'                                     
191500           MOVE ARTC11-CLAG-IDPROJ    TO EKO-EKH-IDKUNDRF                 
191600        END-IF                                                            
191700     END-IF                                                               
191800     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
191900     MOVE MID-IDDC                    TO EKO-EKH-IDDC-SEND                
192000     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
192100     MOVE +0                          TO EKO-EKH-IDDISTR                  
192200     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
192300*******************************                                           
192400*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
192500     MOVE ZERO TO NOLL-RAKNARE                                            
192600     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
192700     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
192800          FOR LEADING ZERO                                                
192900     ADD +1 TO NOLL-RAKNARE                                               
193000     UNSTRING WS-SAP-X-IDAVINR     INTO EKO-EKH-IDVERGL                   
193100          WITH POINTER NOLL-RAKNARE                                       
193200*******************************                                           
193300     MOVE SPAR-ARTC01-KDPRODSL        TO EKO-EKH-KDPRODSL                 
193400     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
193500                                         EKO-EKH-PRARTNTO                 
193600                                         EKO-EKH-PRARTSJK                 
193700                                         EKO-EKH-PRHEMTAG                 
193800                                         EKO-EKH-PRLANDCO                 
193900                                         EKO-EKH-PRDIRLON                 
194000                                         EKO-EKH-PRDMTRL                  
194100                                         EKO-EKH-PROVRPAL                 
194200                                         EKO-EKH-SUBEL                    
194300     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
194400     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
194500                                                                          
194600     MOVE 1.00                        TO EKO-EKH-PRKURS                   
194700                                                                          
194800     MOVE ZERO                        TO EKO-EKH-PRINK                    
194900     MOVE SLAG-PRAVCOST               TO EKO-EKH-PRARTSTD                 
195000     MOVE MID-KVAVIS(MID-IX)          TO EKO-EKH-KVANTAL                  
195100     MOVE '6117'                      TO EKO-EKH-IDTRANS                  
195200     MOVE SPACE                       TO EKO-EKH-BEVAT                    
195300                                         EKO-EKH-KDANMORS                 
195400     MOVE ZERO                        TO EKO-EKH-KDFRAKT                  
195500                                         EKO-EKH-SUVAT                    
195600     MOVE MID-IDANALYS (MID-IX)       TO EKO-EKH-IDANALYS                 
195700     MOVE MID-IDKONTO (MID-IX)        TO EKO-EKH-IDKONTO                  
195800     MOVE MID-IDKST (MID-IX)          TO EKO-EKH-IDKST                    
195900     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAAVIDAT                 
196000     MOVE MID-IDAVINR (MID-IX)        TO EKO-EKH-IDAVINR                  
196100     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
196200     IF (MID-KDRT(MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7) OR           
196300       ((MID-KDRT(MID-IX) = 9 OR 10) AND                                  
196400       (MID-IDLEVNR(MID-IX) = '1001 ' OR '1003 '                          
196500                           OR 'BL3YA' OR 'BP2TH'))                        
196600       MOVE 1                         TO EKO-EKH-KDAVVTYP                 
196700     ELSE                                                                 
196800       MOVE ZERO                      TO EKO-EKH-KDAVVTYP                 
196900     END-IF                                                               
197000     MOVE MID-KDRT(MID-IX)            TO EKO-EKH-KDRT                     
197100     MOVE MID-KVAVIS(MID-IX)          TO EKO-EKH-KVANTMOT                 
197200                                         EKO-EKH-KVAVIS                   
197300     MOVE SPAR-ARTC01-KDSORT          TO EKO-EKH-KDSORT                   
197400     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
197500     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
197600     MOVE SPACE                       TO EKO-EKH-FLDCET                   
197700                                                                          
197800     IF MID-KDRT (MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR            
197900                            9 OR 10                                       
198000       PERFORM IMS-ISRT-EKOTRANS                                          
198100                                                                          
198200       PERFORM UNTIL SEGMENT-FINNS                                        
198300         ADD +1 TO EKO-FIL-IDSEKVNR                                       
198400         PERFORM IMS-ISRT-EKOTRANS                                        
198500       END-PERFORM                                                        
198600       IF DCS-KDTRADP = 'BR12'                                            
198700        IF NOT-FIRST-REC-TRANS                                            
198800         PERFORM S08-FIX-LOCAL-TIME                                       
198900         PERFORM S20-SEND-OPEN                                            
199000         MOVE SEND-IDCOM         TO WZ04-SEND-IDCOM                       
199100         PERFORM S21-SEND-PUT-PROP                                        
199200         MOVE ZERO TO NOTF-IDSEKVNR                                       
199300         MOVE JA  TO FIRST-REC-TRANS-SW                                   
199400        END-IF                                                            
199500        MOVE EKO-EKH-KDEKHHT  TO NOTF-KDEKHHT                             
199600        MOVE EKO-EKH-KDEKSHT  TO NOTF-KDEKSHT                             
199700        MOVE EKO-EKH-DAVERDAT TO NOTF-DAVERDAT                            
199800        MOVE AKTUELL-TID(1:6) TO NOTF-TIREGTID                            
199900        MOVE EKO-EKH-IDVERGL  TO NOTF-IDVERGL                             
200000        MOVE EKO-EKH-IDDC-REC TO NOTF-IDDC                                
200100        MOVE MID-IDAVINR (MID-IX) TO NOTF-IDFAKT                          
200200                                     NOTF-IDORDER                         
200300        MOVE ZERO                 TO NOTF-IDKUNDNR                        
200400                                     NOTF-IDKOLLI                         
200500        MOVE EKO-EKH-IDARTNR TO W-IDARTNR-EDIT-X                          
200600        MOVE FUNCTION TRIM(W-IDARTNR-EDIT-X LEADING)                      
200700                              TO NOTF-IDARTNR20                           
200800        MOVE EKO-EKH-KVANTAL  TO NOTF-KVANTAL                             
200900        ADD +1                TO NOTF-IDSEKVNR                            
201000        PERFORM S22-SEND-PUT                                              
201100       END-IF                                                             
860000     END-IF                                                               
130000     .                                                                    
140000     EJECT                                                                
150000                                                                          
160000 DCB-MEDDELANDE-EKO-WDR9 SECTION.                                         
170000     MOVE '102'                       TO EKH-KDEKHHT                      
180000     MOVE '102'                       TO EKH-KDEKSHT                      
190000     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
200000     MOVE MID-IDDC                    TO EKH-IDDC-SEND                    
210000     MOVE SPACE                       TO EKH-IDDC-REC                     
220000     MOVE +0                          TO EKH-IDDISTR                      
230000     MOVE +0                          TO EKH-IDKUNDNR                     
240000*******************************                                           
250000*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
260000     MOVE ZERO TO NOLL-RAKNARE                                            
270000     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
280000     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
290000          FOR LEADING ZERO                                                
300000     ADD +1 TO NOLL-RAKNARE                                               
310000     UNSTRING WS-SAP-X-IDAVINR     INTO EKH-IDVERGL                       
320000          WITH POINTER NOLL-RAKNARE                                       
330000*******************************                                           
340000     MOVE SPAR-ARTC01-KDPRODSL        TO EKH-KDPRODSL                     
350000     MOVE ZERO                        TO EKH-KDPSLLOC                     
360000                                         EKH-PRARTNTO                     
370000                                         EKH-PRARTSJK                     
380000                                         EKH-PRHEMTAG                     
390000                                         EKH-PRLANDCO                     
400000                                         EKH-PRDIRLON                     
410000                                         EKH-PRDMTRL                      
420000                                         EKH-PROVRPAL                     
430000                                         EKH-SUBEL                        
440000     MOVE MID-IDARTNR(MID-IX)         TO EKH-IDARTNR                      
450000     MOVE SPACE                       TO EKH-FLLSBOK                      
460000                                                                          
470000     MOVE 'SEK'                       TO EKH-KDVALISO                     
480000     MOVE 1.00                        TO EKH-PRKURS                       
490000                                                                          
500000     MOVE ARTC11-CLAG-PRINK           TO EKH-PRINK                        
510000     MOVE ARTC11-CLAG-PRARTSTD        TO EKH-PRARTSTD                     
520000     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVANTAL                      
530000     MOVE '6117'                      TO EKH-IDTRANS                      
540000     MOVE SPACE                       TO EKH-BEVAT                        
550000                                         EKH-KDANMORS                     
560000     MOVE ZERO                        TO EKH-KDFRAKT                      
570000                                         EKH-SUVAT                        
580000     MOVE MID-IDANALYS (MID-IX)       TO EKH-IDANALYS                     
590000     MOVE MID-IDKONTO (MID-IX)        TO EKH-IDKONTO                      
600000     MOVE MID-IDKST (MID-IX)          TO EKH-IDKST                        
610000     MOVE FUNCTION CURRENT-DATE (1:8) TO EKH-DAAVIDAT                     
620000     MOVE MID-IDAVINR (MID-IX)        TO EKH-IDAVINR                      
630000     MOVE MID-IDLEVNR (MID-IX)        TO EKH-IDLEVNR                      
640000     IF (MID-KDRT(MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7) OR           
650000       ((MID-KDRT(MID-IX) = 9 OR 10) AND                                  
660000       (MID-IDLEVNR(MID-IX) = '1001 ' OR '1003 '                          
670000                           OR 'BL3YA' OR 'BP2TH'))                        
680000       MOVE 1                         TO EKH-KDAVVTYP                     
690000     ELSE                                                                 
700000       MOVE ZERO                      TO EKH-KDAVVTYP                     
710000     END-IF                                                               
720000     MOVE MID-KDRT(MID-IX)            TO EKH-KDRT                         
730000     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVANTMOT                     
740000                                         EKH-KVAVIS                       
750000     MOVE SPAR-ARTC01-KDSORT          TO EKH-KDSORT                       
760000     MOVE 'SEPV'                      TO EKH-KDTRADP                      
770000     MOVE SPACE                       TO EKH-FLDCET                       
780000     MOVE SPACE                       TO EKH-IDKUNDRF                     
790000     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
800000                                                                          
810000     IF MID-KDRT (MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR            
820000                            9 OR 10                                       
830000       PERFORM IMS-ISRT-WLSAPA01                                          
840000                                                                          
850000       PERFORM UNTIL SEGMENT-FINNS                                        
860000         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                             
870000         PERFORM IMS-ISRT-WLSAPA01                                        
880000       END-PERFORM                                                        
890000     END-IF                                                               
900000     .                                                                    
910000     EJECT                                                                
920000 DD-MEDDELANDE-330-221 SECTION.                                           
930000                                                                          
940000     MOVE ZERO                   TO W330-W211310                          
950000     MOVE '221'                  TO W330-IDTTYP                           
960000     MOVE '330'                  TO W330-IDPTYP                           
970000******                                                                    
980000       IF ARTC11-CLAG-IDINK (1:3) NUMERIC                                 
990000          MOVE ARTC11-CLAG-IDINK (1:3) TO W330-KDPKINR                    
000000       ELSE                                                               
010000          IF ARTC11-CLAG-IDINK (2:3) NUMERIC                              
020000             MOVE ARTC11-CLAG-IDINK (2:3) TO W330-KDPKINR                 
030000          ELSE                                                            
040000             MOVE ZERO TO W330-KDPKINR                                    
050000          END-IF                                                          
060000       END-IF                                                             
070000******                                                                    
080000     MOVE ARTC11-CLAG-IDANSK     TO W330-IDANSKNR                         
090000     MOVE ARTC11-CLAG-PRARTSTD   TO W330-PRARTSTD                         
100000     MOVE MID-IDARTNR (MID-IX)   TO W330-IDARTNR                          
110000                                    W330-IDARTNR-S                        
120000     IF CDC-SE                                                            
130000         MOVE +1                 TO W330-KDCLAGER                         
140000                                    W330-KDCLAGER-S                       
150000     ELSE                                                                 
160000         MOVE +2                 TO W330-KDCLAGER                         
170000                                    W330-KDCLAGER-S                       
180000     END-IF                                                               
190000     MOVE 009                    TO W330-POSTLGD                          
200000     MOVE MID-KVAVIS (MID-IX)    TO W330-KVAVIS                           
210000                                    W330-KVMOTANT                         
220000     MOVE MID-IDLEVNR (MID-IX)   TO W330-IDLEVNR-INL                      
230000     MOVE MID-TIAVIDAT (MID-IX)  TO W330-TIAVSDAT                         
240000     MOVE MID-IDAVINR (MID-IX)   TO W330-IDAVINR                          
250000     MOVE MID-KDRT (MID-IX)      TO W330-KDRT                             
260000                                                                          
270000     MOVE W330-W211310           TO WS-ZZAC01-LOGGPOST                    
280000     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
290000     PERFORM S01-SKAPA-ZZAC01                                             
300000     .                                                                    
310000     EJECT                                                                
320000 E-UPPD-HISTORIK           SECTION.                                       
330000                                                                          
340000     IF CDC-SE                                                            
350000         PERFORM EB-UPPDATERA-CDC-HISTORIK                                
360000     ELSE                                                                 
370000         IF WS-FLTRACK = 'N'                                              
380000           PERFORM EA-UPPDATERA-SDC-HISTORIK                              
390000         END-IF                                                           
400000     END-IF                                                               
410000     .                                                                    
420000     EJECT                                                                
430000 EA-UPPDATERA-SDC-HISTORIK SECTION.                                       
440000     SKIP2                                                                
450000     PERFORM IMS-GU-WDL601                                                
460000     IF SEGMENT-SAKNAS                                                    
470000         MOVE W-IDARTNR  TO ART-IDARTNR                                   
480000         PERFORM IMS-ISRT-WDL601                                          
490000     END-IF                                                               
500000                                                                          
510000     PERFORM EAA-SKAPA-IDINLEV-NYCKEL                                     
520000                                                                          
530000     MOVE SLAG-ADLAGOMR            TO INL-ADLAGOMR                        
540000     MOVE SLAG-ADGANG              TO INL-ADGANG                          
550000     MOVE SLAG-ADPLATS             TO INL-ADPLATS                         
560000     MOVE 'N'                      TO INL-FLMAKUL                         
570000                                      INL-FLSKAKOL                        
580000     MOVE MID-IDDC                 TO INL-IDDC                            
590000     MOVE MID-IDARTNR-FROM (MID-IX) TO INL-IDLOPNRM                       
600000     MOVE MID-IDLEVNR (MID-IX)     TO INL-IDLEVNR                         
610000                                                                          
620000     MOVE MID-IDAVINR (MID-IX)     TO INL-IDFAKT                          
630000     MOVE +0                       TO INL-IDDISTR                         
640000                                      INL-IDKUNDNR                        
650000     MOVE MID-IDAVINR (MID-IX)     TO INL-IDKUNDRF                        
660000     MOVE +0                       TO INL-IDKOLLI                         
670000     MOVE 'R34'                    TO INL-IDPTYP                          
680000     MOVE +0                       TO INL-KDFRAKT                         
690000     MOVE SPACE                    TO INL-KDKOLLI                         
700000     MOVE MID-KDRT (MID-IX)        TO INL-KDRT                            
710000     MOVE SPAR-ARTC21-KDVALISO     TO INL-KDVALISO                        
720000     IF WS-FLTRACK = 'J'                                                  
730000        MOVE WS-INL-KVAVIS         TO INL-KVANTMOT                        
740000                                      INL-KVAVIS                          
750000     ELSE                                                                 
760000        MOVE MID-KVAVIS (MID-IX)   TO INL-KVANTMOT                        
770000                                      INL-KVAVIS                          
780000     END-IF                                                               
790000     MOVE +0                       TO INL-KVART-SKROT                     
800000     MOVE +0                       TO INL-PRARTNTO                        
810000     MOVE +0                       TO INL-PRKURS                          
820000                                      INL-TIBERANK                        
830000     MOVE MID-TIAVIDAT (MID-IX)    TO INL-TIINLMOT                        
840000     MOVE MID-TIAVIDAT (MID-IX)    TO INL-TIINLINL                        
850000     MOVE 'N'                      TO INL-FLPRIO                          
860000                                      INL-FLTULLST                        
870000     MOVE +0                       TO INL-TIINLMTI                        
880000     MOVE +0                       TO INL-TIINLITI                        
890000                                      INL-KVTULRET                        
900000                                      INL-KVRETUR                         
910000                                      INL-KDAVVANT                        
920000                                      INL-TIAVIDAT                        
930000                                                                          
940000     MOVE SPACE                    TO INL-ADINLOMR                        
950000                                      INL-IDUSER-003                      
960000                                      INL-IDDC-LEV                        
970000     MOVE MID-IDANALYS(MID-IX)     TO INL-IDANALYS                        
980000     MOVE MID-IDKONTO (MID-IX)     TO INL-IDKONTO                         
990000     MOVE MID-IDKST   (MID-IX)     TO INL-IDKST                           
000000                                                                          
010000     PERFORM IMS-ISRT-WDL611                                              
020000     PERFORM UNTIL SEGMENT-FINNS                                          
030000       SUBTRACT +1 FROM INL-DAINLEV                                       
040000       PERFORM IMS-ISRT-WDL611                                            
050000     END-PERFORM                                                          
060000     MOVE INL-DAINLEV TO W-DAINLEV                                        
070000     .                                                                    
080000     EJECT                                                                
090000                                                                          
100000 EAA-SKAPA-IDINLEV-NYCKEL SECTION.                                        
110000                                                                          
120000     ACCEPT WS-DAINLEV-DATUM  FROM DATE                                   
130000     ACCEPT WS-DAINLEV-TID    FROM TIME                                   
140000     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-DAINLEV-SEKEL                 
150000     COMPUTE INL-DAINLEV = 9999999999999999 - WS-DAINLEV-WDL6             
160000     .                                                                    
170000     EJECT                                                                
180000 EB-UPPDATERA-CDC-HISTORIK SECTION.                                       
190000     SKIP2                                                                
200000                                                                          
210000      PERFORM S04-SKAPA-IDINLEV-IDLOPNRM                                  
220000                                                                          
230000     PERFORM IMS-GU-WLINLE01                                              
240000     IF SEGMENT-SAKNAS                                                    
250000       MOVE W-IDARTNR TO INLE-ART-IDARTNR                                 
260000       PERFORM IMS-ISRT-WLINLE01                                          
270000     END-IF                                                               
280000                                                                          
290000     MOVE WS-DAINLEV      TO INLE-INL-DAINLEV                             
300000                             W-DAINLEV                                    
310000     PERFORM IMS-ISRT-WLINLE11                                            
320000                                                                          
330000     PERFORM UNTIL SEGMENT-FINNS                                          
340000        SUBTRACT 1      FROM INLE-INL-DAINLEV                             
350000                             W-DAINLEV                                    
360000        PERFORM IMS-ISRT-WLINLE11                                         
370000     END-PERFORM                                                          
380000                                                                          
390000                                                                          
400000     MOVE 'R34'                 TO INLE-DIR-IDPTYP                        
410000     MOVE MID-IDARTNR-FROM(MID-IX) TO INLE-DIR-IDLOPNRM                   
420000     MOVE MID-IDAVINR (MID-IX)  TO INLE-DIR-IDAVINR                       
430000     MOVE MID-IDANALYS(MID-IX)  TO INLE-DIR-IDANALYS                      
440000     MOVE MID-IDKONTO (MID-IX)  TO INLE-DIR-IDKONTO                       
450000     MOVE MID-IDKST   (MID-IX)  TO INLE-DIR-IDKST                         
460000     MOVE MID-IDLEVNR (MID-IX)  TO INLE-DIR-IDLEVNR                       
470000     MOVE WS-IDDC               TO INLE-DIR-IDDC                          
480000     MOVE MID-KDRT (MID-IX)     TO INLE-DIR-KDRT                          
490000     MOVE MID-KVAVIS (MID-IX)   TO INLE-DIR-KVAVIS                        
500000     MOVE MID-TIAVIDAT (MID-IX) TO INLE-DIR-TIAVSDAT                      
510000     MOVE ZERO                  TO INLE-DIR-IDDISTR                       
520000                                   INLE-DIR-IDKUNDNR                      
530000                                   INLE-DIR-IDKUNDRF                      
540000                                   INLE-DIR-IDPRODNR                      
550000                                   INLE-DIR-IDFAKT                        
560000                                                                          
570000     PERFORM IMS-ISRT-WLINLE22                                            
580000     .                                                                    
590000     EJECT                                                                
600000 F-UPPD-LEVPLAN            SECTION.                                       
610000                                                                          
620000     MOVE JA             TO AVROP-SW                                      
630000                                                                          
640000     IF  WS-KV-LPLAN             > ZERO                                   
650000                                                                          
660000       IF  MID-KDRT (MID-IX)     = 00                                     
670000       OR  (MID-KDRT (MID-IX)    = 01                                     
680000        AND ARTC11-CLAG-KDHF     > ZERO)                                  
690000       OR  (MID-KDRT (MID-IX)    = 02                                     
700000        AND ARTC11-CLAG-KDHF     > ZERO)                                  
710000       OR  MID-KDRT (MID-IX)     = 03                                     
720000       OR  MID-KDRT (MID-IX)     = 05                                     
730000       OR  (MID-KDRT (MID-IX)    = 06                                     
740000        AND MID-IDLEVNR (MID-IX)  NOT = SPACE                             
750000        AND MID-IDLEVNR (MID-IX)  NOT = '9999 ')                          
760000       OR  MID-KDRT (MID-IX)     = 09                                     
770000       OR  MID-KDRT (MID-IX)     = 10                                     
780000                                                                          
790000         IF  (MID-KDRT (MID-IX)  = 00                                     
800000          OR  MID-KDRT (MID-IX)  = 01                                     
810000          OR  MID-KDRT (MID-IX)  = 02                                     
820000          OR  MID-KDRT (MID-IX)  = 09)                                    
830000         AND ARTC11-CLAG-KDHF    > ZERO                                   
840000           MOVE SPAR-ARTC01-IDLEVNR TO W-INLB11-IDLEVNR                   
850000         ELSE                                                             
860000           MOVE MID-IDLEVNR (MID-IX) TO W-INLB11-IDLEVNR                  
870000         END-IF                                                           
880000                                                                          
890000         MOVE MID-IDARTNR(MID-IX) TO W-IDARTNR-D9                         
900000         MOVE MID-IDDC            TO W-IDDC-D9                            
910000         PERFORM IMS-GU-INLB11                                            
920000                                                                          
930000         IF  SEGMENT-FINNS                                                
940000                                                                          
950000           IF  MID-KDRT (MID-IX)  NOT = 10                                
960000             PERFORM FA-BOKA-LEVPL-AVROP                                  
970000           END-IF                                                         
980000                                                                          
990000           PERFORM FB-BOKA-LEVPL-LBESK                                    
000000                                                                          
010000           IF  MID-KDRT (MID-IX)  NOT = 03                                
020000             PERFORM FC-BOKA-LEVPL-BREST                                  
030000           END-IF                                                         
040000         ELSE                                                             
050000           MOVE NEJ              TO AVROP-SW                              
060000         END-IF                                                           
070000       END-IF                                                             
080000     END-IF                                                               
090000     .                                                                    
100000     EJECT                                                                
110000 FA-BOKA-LEVPL-AVROP SECTION.                                             
120000                                                                          
130000     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
140000     MOVE ZERO                   TO WS-INLB23-TIAVROP-INL                 
150000                                                                          
160000     IF  MID-KDRT (MID-IX)       NOT = 03                                 
170000                                                                          
180000       MOVE +2                   TO W-KDAVROP                             
190000       PERFORM IMS-GHNP-INLB23-KD                                         
200000       IF SEGMENT-SAKNAS                                                  
210000         MOVE NEJ       TO AVROP-SW                                       
220000       END-IF                                                             
230000       PERFORM UNTIL (SEGMENT-SAKNAS                                      
240000                  OR WS-KV-OBOK  = ZERO)                                  
250000                                                                          
260000         PERFORM FAA-BOKA-ETT-AVROP                                       
270000                                                                          
280000         IF  WS-KV-OBOK          > ZERO                                   
290000           PERFORM IMS-GHNP-INLB23-KD                                     
300000         END-IF                                                           
310000       END-PERFORM                                                        
320000                                                                          
330000     ELSE                                                                 
340000                                                                          
350000       MOVE +2                   TO W-KDAVROP                             
360000       MOVE MID-IDAVINR (MID-IX) TO WS-IDAVINR                            
370000       MOVE WS-IDAVINR (3:4)     TO W-IDORDNSB                            
380000       PERFORM IMS-GNP-INLB32                                             
390000                                                                          
400000       MOVE INLB-KFB-DAAVROP-AVS TO W-DAAVROP                             
410000       MOVE INLB-KFB-TILEVDAG    TO W-TILEVDAG                            
420000       PERFORM IMS-GHNP-INLB23-TI-F                                       
430000                                                                          
440000       PERFORM FAA-BOKA-ETT-AVROP                                         
450000     END-IF                                                               
460000     .                                                                    
470000     EJECT                                                                
480000 FAA-BOKA-ETT-AVROP SECTION.                                              
490000                                                                          
500000     IF  WS-INLB23-TIAVROP-INL   = ZERO                                   
510000       MOVE INLB23-TIAVRDAT-INL  TO DAT-I-TIDATUM                         
520000       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
530000       CALL WDATKONV USING          DAT-KDDATFORM                         
540000                                    DAT-I-TIDATUM                         
550000                                    DAT-O-TIDATUM                         
560000                                    DAT-KDSVAR                            
570000       IF DAT-KDSVAR-FEL                                                  
580000         MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT                   
590000         CALL FELLOG                                                      
600000       ELSE                                                               
610000         MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                             
620000         MOVE WS-TIAAVV          TO WS-INLB23-TIAVROP-INL                 
630000       END-IF                                                             
640000     END-IF                                                               
650000                                                                          
660000     IF  INLB23-KVAVROP          <= WS-KV-OBOK                            
670000       SUBTRACT INLB23-KVAVROP   FROM WS-KV-OBOK                          
680000       MOVE INLB23-KVAVROP       TO INLB31-KVAVROP-AVB                    
690000       MOVE ZERO                 TO INLB23-KVAVROP                        
700000       MOVE +9                   TO INLB23-KDAVROP                        
710000     ELSE                                                                 
720000       SUBTRACT WS-KV-OBOK       FROM INLB23-KVAVROP                      
730000       MOVE WS-KV-OBOK           TO INLB31-KVAVROP-AVB                    
740000       MOVE ZERO                 TO WS-KV-OBOK                            
750000     END-IF                                                               
760000                                                                          
770000     MOVE WS-IDLOPNRM-AAVVDLLLL  TO INLB31-IDLOPNRM-PL                    
780000                                                                          
790000     PERFORM IMS-REPL-INLB                                                
800000                                                                          
810000     PERFORM IMS-ISRT-INLB31                                              
820000     .                                                                    
830000     EJECT                                                                
840000 FB-BOKA-LEVPL-LBESK SECTION.                                             
850000                                                                          
860000     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
870000                                                                          
880000     PERFORM IMS-GHNP-INLB24                                              
890000                                                                          
900000     PERFORM UNTIL (SEGMENT-SAKNAS                                        
910000                OR  WS-KV-OBOK   = ZERO)                                  
920000                                                                          
930000       MOVE DAGENS-DATUM     TO INLB24-LEV-TIREGDAT                       
940000       MOVE DAGENS-HHMMSS    TO INLB24-LEV-TIREGTID                       
950000                                                                          
960000       IF  INLB24-LEV-KVAVIS-BSKKVAR < WS-KV-OBOK                         
970000*      -- CL BOKAS NED HELT                                               
980000         SUBTRACT INLB24-LEV-KVAVIS-BSKKVAR                               
990000                                   FROM WS-KV-OBOK                        
000000         MOVE ZERO TO INLB24-LEV-KVAVIS-BSKKVAR                           
010000       ELSE                                                               
020000*      -- CL BOKAS NED DELVIS                                             
030000         SUBTRACT WS-KV-OBOK                                              
040000             FROM INLB24-LEV-KVAVIS-BSKKVAR                               
050000         MOVE ZERO                 TO WS-KV-OBOK                          
060000       END-IF                                                             
070000                                                                          
080000       COMPUTE WS-BSKKVAR-GGR-10                                          
090000         = INLB24-LEV-KVAVIS-BSKKVAR                                      
100000         * 10                                                             
110000       END-COMPUTE                                                        
120000                                                                          
130000       IF WS-BSKKVAR-GGR-10 < INLB24-LEV-KVAVIS-BSKURS                    
140000         PERFORM IMS-DLET-INLB                                            
150000       ELSE                                                               
160000         PERFORM IMS-REPL-INLB                                            
170000       END-IF                                                             
180000                                                                          
190000       IF  WS-KV-OBOK            > ZERO                                   
200000         PERFORM IMS-GHNP-INLB24                                          
210000       END-IF                                                             
220000     END-PERFORM                                                          
230000     .                                                                    
240000     EJECT                                                                
250000 FC-BOKA-LEVPL-BREST SECTION.                                             
260000                                                                          
270000     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
280000                                                                          
290000     PERFORM IMS-GHU-INLB11                                               
300000                                                                          
310000     IF  INLB11-KVBR             <= WS-KV-OBOK                            
320000       MOVE ZERO                 TO INLB11-KVBR                           
330000     ELSE                                                                 
340000       SUBTRACT WS-KV-OBOK       FROM INLB11-KVBR                         
350000     END-IF                                                               
360000                                                                          
370000     PERFORM IMS-REPL-INLB                                                
380000     .                                                                    
390000     EJECT                                                                
400000 G-RO-TAECKNING SECTION.                                                  
410000                                                                          
420000     MOVE SPACE                  TO 4506-WDGX4506                         
430000                                                                          
440000     MOVE MID-IDDC               TO 4505-IDDC                             
450000     MOVE MID-IDARTNR(MID-IX)    TO 4506-IDARTNR                          
460000     MOVE +1                     TO 4506-KDTAKORS                         
470000     MOVE MID-KVAVIS(MID-IX)     TO 4506-KVANTMOT                         
480000     PERFORM IMS-ISRT-4506                                                
490000     .                                                                    
500000     EJECT                                                                
510000 S01-SKAPA-ZZAC01 SECTION.                                                
520000                                                                          
530000     ACCEPT ZZAC01-TIAAMMDD      FROM DATE                                
540000     ACCEPT ZZAC01-TIKLOCK       FROM TIME                                
550000                                                                          
560000     ADD +1                      TO WS-IDLOGLOP                           
570000     MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                       
580000                                                                          
590000     MOVE WS-ZZAC01-LOGGPOST     TO ZZAC01-LOGGPOST                       
600000     MOVE WS-ZZAC01-SORTPOST     TO ZZAC01-SORTPOST                       
610000                                                                          
620000     PERFORM IMS-ISRT-ZZAC01                                              
630000     PERFORM UNTIL SEGMENT-FINNS                                          
640000       IF WS-IDLOGLOP < +8                                                
650000         ADD +1 TO WS-IDLOGLOP                                            
660000         MOVE WS-IDLOGLOP   TO ZZAC01-IDLOGLOP                            
670000         PERFORM IMS-ISRT-ZZAC01                                          
680000       ELSE                                                               
690000         ACCEPT ZZAC01-TIAAMMDD      FROM DATE                            
700000         ACCEPT ZZAC01-TIKLOCK       FROM TIME                            
710000         MOVE +1 TO WS-IDLOGLOP                                           
720000         MOVE WS-IDLOGLOP   TO ZZAC01-IDLOGLOP                            
730000         PERFORM IMS-ISRT-ZZAC01                                          
740000       END-IF                                                             
750000     END-PERFORM                                                          
760000     .                                                                    
770000     EJECT                                                                
780000 S02-RED-W211FEL-GNRL SECTION.                                            
790000                                                                          
800000     MOVE ZERO                   TO W211FEL-SORT-FLT                      
810000     MOVE SPACE                  TO W211FEL-FILLER2                       
820000                                                                          
830000     MOVE 'R34'                  TO W211FEL-IDPTYP-S                      
840000     IF CDC-SE                                                            
850000         MOVE +1                     TO W211FEL-KDCLAGER-S                
860000     ELSE                                                                 
870000         MOVE +2                     TO W211FEL-KDCLAGER-S                
880000     END-IF                                                               
890000     MOVE MID-IDARTNR (MID-IX)   TO W211FEL-SORTBGP                       
900000     MOVE 1                      TO W211FEL-KDFELMRK                      
910000     .                                                                    
920000     EJECT                                                                
930000 S04-SKAPA-IDINLEV-IDLOPNRM SECTION.                                      
940000                                                                          
950000     IF SPAR-TIAAVVD-GRP (3:3) = W-VVD                                    
960000       ADD +1                      TO W-LLLL                              
970000     ELSE                                                                 
980000       MOVE SPAR-TIAAVVD-GRP (3:3) TO W-VVD                               
990000       MOVE +1                     TO W-LLLL                              
000000     END-IF                                                               
010000     CALL CHECK USING WS-IDLOPNRM (2:7) FLT-LGD                           
020000          VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B               
030000                                                                          
040000     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
050000     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
060000     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
070000     COMPUTE WS-DAINLEV          = 9999999999999999                       
080000                                 - WS-TIAAAAMMDDTTMMSSTH                  
090000     END-COMPUTE                                                          
100000     MOVE WS-IDLOPNRM            TO WS-IDLOPNRM-VVDLLLLK                  
110000     .                                                                    
120000     EJECT                                                                
130000 S06-LAES-SPARA-WLARTC SECTION.                                           
140000     SKIP2                                                                
150000     MOVE MID-IDARTNR (MID-IX) TO W-IDARTNR                               
160000     PERFORM IMS-GU-WLARTC01                                              
170000     MOVE ARTC01-ART-IDLEVNR  TO SPAR-ARTC01-IDLEVNR                      
180000     MOVE ARTC01-ART-KDPRODSL TO SPAR-ARTC01-KDPRODSL                     
190000     PERFORM IMS-GNP-WLARTC11                                             
200000     MOVE ARTC11-CLAG-KDPSLLOC  TO SPAR-ARTC11-KDPSLLOC                   
210000                                                                          
220000     MOVE +0                    TO SPAR-ARTC21-PRARTBEL-PR                
230000                                   SPAR-ARTC21-PRARTBEL-SUM               
240000                                   SPAR-ARTC21-PRARTBES-PR                
250000*    SPARA CLAG-KDLEVSP FÖR UPPLÄGG AV NYTT WDK711                        
260000     MOVE ARTC11-CLAG-KDLEVSP   TO WDK7-KDLEVSP                           
270000                                                                          
280000     IF XDC-NON-VCC-OWNED                                                 
290000       PERFORM S06A-LAES-SPARA-WLARTC                                     
300000     ELSE                                                                 
310000       PERFORM S06B-LAES-SPARA-WLARTC                                     
320000     END-IF                                                               
330000                                                                          
340000     PERFORM IMS-GNP-ARTC23                                               
350000     IF SEGMENT-FINNS                                                     
360000       MOVE AVT-IDAVTAL     TO WS-ARTC23-IDAVTAL                          
370000     ELSE                                                                 
380000       MOVE ZERO            TO WS-ARTC23-IDAVTAL                          
390000     END-IF                                                               
400000     .                                                                    
410000     EJECT                                                                
420000                                                                          
430000 S06A-LAES-SPARA-WLARTC SECTION.                                          
440000     MOVE MID-TIAVIDAT (MID-IX) TO WS-IDAG                                
450000     MOVE WS-IDAG           TO DAT-I-TIDATUM                              
460000     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
470000     CALL WDATKONV USING       DAT-KDDATFORM                              
480000                               DAT-I-TIDATUM                              
490000                               DAT-O-TIDATUM                              
500000                               DAT-KDSVAR                                 
510000     IF DAT-KDSVAR-FEL                                                    
520000       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
530000       CALL FELLOG                                                        
540000     END-IF                                                               
550000                                                                          
560000     MOVE DAT-TISEKEL TO WS-DAGENS-SEKEL                                  
570000                                                                          
580000*    -- WDK711                                                            
590000     PERFORM IMS-GHU-WDK711-K7                                            
600000     IF SEGMENT-FINNS                                                     
610000                                                                          
620000*    -- WDK724                                                            
630000       MOVE MID-IDLEVNR(MID-IX)   TO W-IDLEVNR-PR                         
640000       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAGENS-DATUM                 
650000       PERFORM IMS-GHNP-WDK724-K7                                         
660000                                                                          
670000       IF SEGMENT-FINNS                                                   
680000         MOVE SPRL-KDVALISO       TO SPAR-ARTC21-KDVALISO                 
690000         MOVE SPRL-PRARTBEL-PR    TO SPAR-ARTC21-PRARTBEL-PR              
700000         MOVE SPRL-PRARTBEL-PR    TO SPAR-ARTC21-PRARTBEL-SUM             
710000         MOVE SPRL-PRARTBES-PR    TO SPAR-ARTC21-PRARTBES-PR              
720000         IF SPRL-SUINLEV-PR < +1                                          
730000           MOVE +1         TO SPRL-SUINLEV-PR                             
740000           PERFORM IMS-REPL-WDK724-K7                                     
750000         ELSE                                                             
760000           ADD +1                  TO SPRL-SUINLEV-PR                     
770000           PERFORM IMS-REPL-WDK724-K7                                     
780000         END-IF                                                           
790000                                                                          
800000         PERFORM IMS-GHNP-WLARTC11-FIRST                                  
810000                                                                          
820000*  -- WDK712                                                              
830000         SEARCH ALL DC-LAND                                               
840000           AT END                                                         
850000             MOVE 'EJ TRÄFF I TAB DCLAND'                                 
860000                                TO FELTEXT                                
870000             CALL FELLOG                                                  
880000           WHEN DCLAND-IDDC (DCLAND-IX) = WS-IDDC                         
890000             MOVE DCLAND-IDLANDX2 (DCLAND-IX)                             
900000                                TO W-IDLAND-K7                            
910000         END-SEARCH                                                       
920000         PERFORM IMS-GHU-WDK712-K7                                        
930000**** HÄMTA VALUTAKURS FÖR CNY, AVGCO ÄR I CNY SKALL OMVANDLAS TILL        
940000**** SEK                                                                  
950000         MOVE WS-DAGENS-DATUM(3:2) TO W-DATE-AAMM(1:2)                    
960000         MOVE 01                   TO W-DATE-AAMM(3:2)                    
970000         MOVE 'SEK'                TO CURR-KDVALISO-HUV                   
980000         MOVE 'CNY'                TO CURR-KDVALISO-ROW                   
990000         MOVE W-DATE-AAMM          TO CURR-TIAAMM                         
000000         MOVE 'A'                  TO CURR-KDVALTYP                       
010000         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
020000         IF CURR-KDSVAR = ' '                                             
030000           MOVE CURR-PRKURS-NEW    TO W-PRKURS                            
040000           MOVE CURR-REVALUTA-TO   TO W-REVALUTA                          
050000         ELSE                                                             
060000           MOVE 1                  TO W-PRKURS                            
070000           MOVE 1                  TO W-REVALUTA                          
080000         END-IF                                                           
090000**** PRARTSJK SKALL JUSTERAS VID EN INLEVERANS                            
100000         MOVE SPRL-PRARTBES-PR TO LART-PRARTSJK                           
110000**** JUSTERA OM DET FINNS KALKYLPÅLÄGG PÅ ARTIKELN                        
120000         MOVE WS-IDDC         TO W-IDDC                                   
130000         PERFORM IMS-GU-WDB601                                            
140000         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
150000         IF SEGMENT-FINNS                                                 
160000           PERFORM IMS-GNP-WDB617                                         
170000           IF SEGMENT-FINNS                                               
180000             COMPUTE LART-PRARTSJK ROUNDED =                              
190000                (SPRL-PRARTBES-PR *  W-PRKURS / W-REVALUTA ) +            
200000                (PROC-REDIRLON * ARTC11-CLAG-PRDIRLON) +                  
210000                (PROC-REDMTRL  * ARTC11-CLAG-PRDMTRL)                     
220000             END-COMPUTE                                                  
230000           END-IF                                                         
240000         END-IF                                                           
250000         PERFORM IMS-REPL-WDK712-K7                                       
260000       ELSE                                                               
270000**** FINNS INGET PRIS FÖR BEGÄRD LEVERANTÖR, ANVÄND DEFAULT               
280000         MOVE ZERO TO PROC-REDIRLON                                       
290000         MOVE ZERO TO PROC-REDMTRL                                        
300000         MOVE 1 TO W-PRKURS                                               
310000         MOVE 1 TO W-REVALUTA                                             
320000       END-IF                                                             
330000     END-IF                                                               
340000     .                                                                    
350000     EJECT                                                                
360000                                                                          
370000 S06B-LAES-SPARA-WLARTC SECTION.                                          
380000     SKIP2                                                                
390000     MOVE MID-IDARTNR (MID-IX) TO W-IDARTNR                               
400000     PERFORM IMS-GU-WLARTC01                                              
410000     MOVE ARTC01-ART-IDLEVNR  TO SPAR-ARTC01-IDLEVNR                      
420000     MOVE ARTC01-ART-KDPRODSL TO SPAR-ARTC01-KDPRODSL                     
430000     PERFORM IMS-GNP-WLARTC11                                             
440000     MOVE ARTC11-CLAG-KDPSLLOC  TO SPAR-ARTC11-KDPSLLOC                   
450000                                                                          
460000     MOVE +0                    TO SPAR-ARTC21-PRARTBEL-PR                
470000                                   SPAR-ARTC21-PRARTBEL-SUM               
480000                                   SPAR-ARTC21-PRARTBES-PR                
490000*    SPARA CLAG-KDLEVSP FÖR UPPLÄGG AV NYTT WDK711                        
500000     MOVE ARTC11-CLAG-KDLEVSP   TO WDK7-KDLEVSP                           
510000**********                                                                
520000* NYTT SÄTT ATT HITTA PRIS                                                
530000     MOVE MID-TIAVIDAT (MID-IX) TO WS-IDAG                                
540000     MOVE WS-IDAG           TO DAT-I-TIDATUM                              
550000     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
560000     CALL WDATKONV USING       DAT-KDDATFORM                              
570000                               DAT-I-TIDATUM                              
580000                               DAT-O-TIDATUM                              
590000                               DAT-KDSVAR                                 
600000     IF DAT-KDSVAR-FEL                                                    
610000       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
620000       CALL FELLOG                                                        
630000     END-IF                                                               
640000                                                                          
650000     MOVE DAT-TISEKEL TO WS-DAGENS-SEKEL                                  
660000                                                                          
670000     MOVE NEJ TO PRIS-FINNS-SW                                            
680000     MOVE MID-IDLEVNR (MID-IX) TO W-IDLEVNR                               
690000     MOVE +0                    TO SPAR-ARTC21-PRARTBEL-PR                
700000                                   SPAR-ARTC21-PRARTBEL-SUM               
710000                                   SPAR-ARTC21-PRARTBES-PR                
720000     MOVE NEJ      TO TRAEFF-SW                                           
730000     MOVE MID-IDLEVNR(MID-IX)   TO W-IDLEVNR-21                           
740000     PERFORM IMS-GHNP-WLARTC21                                            
750000     PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF                               
760000      COMPUTE W-PRL-DADAT = 99999999 - ARTC21-PRL-DAPRLIST-9KOMPL         
770000       IF ARTC21-PRL-KDSTATUS-PR = +1 AND                                 
780000         W-PRL-DADAT <= WS-DAGENS-DATUM                                   
790000         MOVE JA TO TRAEFF-SW                                             
800000       ELSE                                                               
810000         PERFORM IMS-GHNP-WLARTC21                                        
820000       END-IF                                                             
830000     END-PERFORM                                                          
840000                                                                          
850000     IF TRAEFF                                                            
860000       MOVE ARTC21-PRL-PRARTBEL-PR                                        
870000                                   TO SPAR-ARTC21-PRARTBEL-PR             
880000                                      SPAR-ARTC21-PRARTBEL-SUM            
890000       MOVE ARTC21-PRL-PRARTBES-PR                                        
900000                                   TO SPAR-ARTC21-PRARTBES-PR             
910000       MOVE ARTC21-PRL-KDVALISO    TO SPAR-ARTC21-KDVALISO                
920000     END-IF                                                               
930000     .                                                                    
940000     EJECT                                                                
950000 S07-SKAPA-R34-LOGG SECTION.                                              
960000     SKIP2                                                                
970000     MOVE  MID-IDDC                TO LOGG34-IDDC                         
980000     MOVE  'R34'                   TO LOGG34-IDPTYP                       
990000     MOVE  MID-IDLEVNR (MID-IX)    TO LOGG34-IDLEVNR                      
000000     MOVE MID-KDRT (MID-IX)        TO LOGG34-KDRT                         
010000     MOVE MID-IDAVINR (MID-IX)     TO LOGG34-IDAVINR                      
020000     MOVE MID-TIAVIDAT (MID-IX)    TO LOGG34-TIAVIDAT                     
030000     MOVE MID-IDARTNR (MID-IX)     TO LOGG34-IDARTNR                      
040000     MOVE MID-KVAVIS (MID-IX)      TO LOGG34-KVAVIS                       
050000     MOVE MID-IDKONTO (MID-IX)     TO LOGG34-IDKONTO                      
060000     MOVE MID-IDARTNR-FROM (MID-IX) TO LOGG34-IDARTNR-FROM                
070000     MOVE MSG-SIGNON-USERID        TO LOGG34-SIGNON-USERID                
080000     MOVE  IDPGM                   TO FIL-IDPGM IN FIL-WDR601             
090000     MOVE  DAGENS-DATUM            TO FIL-TIREGDAT                        
100000     MOVE WS-TIAAMMDDTTMMSSTH-TIME TO FIL-TIKLOCK IN FIL-WDR601           
110000     ADD +1                        TO W-IDSEKVNR                          
120000     MOVE W-IDSEKVNR               TO FIL-IDSEKVNR IN FIL-WDR601          
130000     MOVE 'W601R34A'               TO FIL-IDCPYTXT IN FIL-WDR601          
140000     MOVE LOGG34-W601R34A-CTX      TO FIL-WDR601-DATA                     
150000                                                                          
160000     PERFORM IMS-ISRT-WLFILA01                                            
170000                                                                          
180000     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
190000         ADD +1 TO FIL-IDSEKVNR IN FIL-WDR601                             
200000         PERFORM IMS-ISRT-WLFILA01                                        
210000     END-PERFORM                                                          
220000     .                                                                    
230000     EJECT                                                                
240003 S08-FIX-LOCAL-TIME SECTION.                                              
250000                                                                          
260000******* ADAPT DATE AND TIME FOR TIMEZONES                                 
270000     PERFORM IMS-GU-WDB601                                                
280000                                                                          
290006     MOVE '011'                TO TIDZ-MSGI-KDCALL                        
300006     MOVE DCS-IDTIDZON         TO TIDZ-MSGI-IDTIDZON                      
310006     MOVE DCS-IDDC             TO TIDZ-MSGI-IDDC                          
320006     MOVE DAGENS-AAMMDD        TO TIDZ-MSGI-TILOKDAT                      
330006     MOVE WS-DAGENS-TID        TO TIDZ-MSGI-TILOKTID                      
340006     CALL WL01TIDZ USING          TIDZ-MSGI-WL01TIDZ                      
350006     MOVE TIDZ-MSGI-TILOKTID(1:4) TO AKTUELL-TID(1:4)                     
360000     .                                                                    
370000     EJECT                                                                
380000 S10-PALAGG-WDR9 SECTION.                                                 
390000                                                                          
400000     MOVE 'W6011C00'                  TO FIL-IDPGM IN FIL-WDR901          
410000     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
420000                                         EKH-DAVERDAT                     
430000     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK IN FIL-WDR901        
440000     MOVE +1                         TO FIL-IDSEKVNR IN FIL-WDR901        
450000     MOVE 'W510EKHA'                 TO FIL-IDCPYTXT IN FIL-WDR901        
460000     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER IN FIL-WDR901         
470000     MOVE '103'                       TO EKH-KDEKHHT                      
480000     MOVE '101'                       TO EKH-KDEKSHT                      
490000     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
500000     MOVE MID-IDDC                    TO EKH-IDDC-SEND                    
510000     MOVE SPACE                       TO EKH-IDDC-REC                     
520000     MOVE +0                          TO EKH-IDDISTR                      
530000     MOVE +0                          TO EKH-IDKUNDNR                     
540000*******************************                                           
550000*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
560000     MOVE ZERO TO NOLL-RAKNARE                                            
570000     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
580000     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
590000          FOR LEADING ZERO                                                
600000     ADD +1 TO NOLL-RAKNARE                                               
610000     UNSTRING WS-SAP-X-IDAVINR     INTO EKH-IDVERGL                       
620000          WITH POINTER NOLL-RAKNARE                                       
630000*******************************                                           
640000     MOVE SPAR-ARTC01-KDPRODSL        TO EKH-KDPRODSL                     
650000     MOVE ZERO                        TO EKH-KDPSLLOC                     
660000                                         EKH-PRARTNTO                     
670000                                         EKH-PRARTSJK                     
680000                                         EKH-PRHEMTAG                     
690000                                         EKH-PRLANDCO                     
700000                                         EKH-SUBEL                        
710000     MOVE MID-IDARTNR(MID-IX)         TO EKH-IDARTNR                      
720000     MOVE SPACE                       TO EKH-FLLSBOK                      
730000                                                                          
740000     MOVE 'SEK'                       TO EKH-KDVALISO                     
750000     MOVE 1.00                        TO EKH-PRKURS                       
760000                                                                          
770000     MOVE ARTC11-CLAG-PRINK           TO EKH-PRINK                        
780000     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVANTAL                      
790000     MOVE '6117'                      TO EKH-IDTRANS                      
800000     MOVE SPACE                       TO EKH-BEVAT                        
810000                                         EKH-KDANMORS                     
820000     MOVE ZERO                        TO EKH-KDFRAKT                      
830000                                         EKH-SUVAT                        
840000     MOVE MID-IDANALYS (MID-IX)       TO EKH-IDANALYS                     
850000     MOVE MID-IDKONTO (MID-IX)        TO EKH-IDKONTO                      
860000     MOVE MID-IDKST (MID-IX)          TO EKH-IDKST                        
870000     MOVE ARTC11-CLAG-PRARTSTD        TO EKH-PRARTSTD                     
880000     MOVE ARTC11-CLAG-PRDIRLON        TO EKH-PRDIRLON                     
890000     MOVE ARTC11-CLAG-PRDMTRL         TO EKH-PRDMTRL                      
900000     MOVE ARTC11-CLAG-PROVRPAL        TO EKH-PROVRPAL                     
910000     MOVE FUNCTION CURRENT-DATE (1:8) TO EKH-DAAVIDAT                     
920000     MOVE MID-IDAVINR (MID-IX)        TO EKH-IDAVINR                      
930000     MOVE MID-IDLEVNR (MID-IX)        TO EKH-IDLEVNR                      
940000     IF (MID-KDRT(MID-IX) = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7) OR           
950000       ((MID-KDRT(MID-IX) = 9 OR 10) AND                                  
960000       (MID-IDLEVNR(MID-IX) = '1001 ' OR '1003 '                          
970000                           OR 'BL3YA' OR 'BP2TH'))                        
980000       MOVE 1                         TO EKH-KDAVVTYP                     
990000     ELSE                                                                 
000000       MOVE ZERO                      TO EKH-KDAVVTYP                     
010000     END-IF                                                               
020000     MOVE MID-KDRT(MID-IX)            TO EKH-KDRT                         
030000     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVANTMOT                     
040000                                         EKH-KVAVIS                       
050000     MOVE SPAR-ARTC01-KDSORT          TO EKH-KDSORT                       
060000     MOVE SPACE                       TO EKH-KDTRADP                      
070000     MOVE SPACE                       TO EKH-FLDCET                       
080000     MOVE SPACE                       TO EKH-IDKUNDRF                     
090000     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
100000                                                                          
110000     PERFORM IMS-ISRT-WLSAPA01                                            
120000                                                                          
130000     PERFORM UNTIL SEGMENT-FINNS                                          
140000       ADD +1 TO FIL-IDSEKVNR IN FIL-WDR901                               
150000       PERFORM IMS-ISRT-WLSAPA01                                          
160000     END-PERFORM                                                          
170000     .                                                                    
180000     EJECT                                                                
190000                                                                          
200000 S11-SALDOLOGT-DATA SECTION.                                              
210000     INITIALIZE LOGT-WDL301                                               
220000     ACCEPT WS-TID                   FROM TIME                            
230000     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
240000     COMPUTE LOGT-DAREGDAT-9KOMPL = 99999999 - LOGG-DATUM                 
250000     COMPUTE LOGT-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
260000     MOVE W-IDARTNR          TO LOGT-IDARTNR                              
270000     MOVE 9                  TO LOGT-IDSEKVNR                             
280000     MOVE 'INBO'             TO LOGT-IDHUVTYP                             
290000     MOVE 'R34'              TO LOGT-IDSUBTYP                             
300000     MOVE 'W6011C00'         TO LOGT-IDPGM                                
310000     MOVE 'L108'             TO LOGT-IDTRANS                              
320000     MOVE MSG-SIGNON-USERID  TO LOGT-IDUSER                               
330000**   MOVE SPACE              TO LOGT-REF                                  
340000     MOVE MID-IDAVINR (MID-IX) TO LOGT-IDFAKT                             
350000                                  LOGT-IDKUNDRF                           
360000     MOVE +0                 TO LOGT-IDKUNDNR                             
370000     MOVE '00000000'         TO LOGT-DAREGDAT-LADD                        
380000     MOVE MID-IDDC           TO LOGT-IDDC                                 
390000     MOVE WS-TRCK-KVANTMOT   TO LOGT-KVART-SALDO                          
400000     COMPUTE WS-SLAG-KVLS = WS-SLAG-KVLS +                                
410000                            WS-TRCK-KVANTMOT                              
420000     MOVE WS-SLAG-KVLS       TO LOGT-KVLS                                 
430000     MOVE '+'                TO LOGT-IDTECKEN-KVLS                        
440000     MOVE '+'                TO LOGT-IDTECKEN-KVTRACK-KVAR                
450000     MOVE TRCK-KVTRACK-KVAR  TO LOGT-KVTRACK-KVAR                         
460000     MOVE TRCK-IDTRACK       TO LOGT-IDTRACK                              
470000     .                                                                    
480000     EJECT                                                                
490000                                                                          
500000 S12-ISRT-SALDOLOGT SECTION.                                              
510000     PERFORM IMS-ISRT-WDL301                                              
520000     IF SEGMENT-FINNS-REDAN                                               
530000       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
540000         SUBTRACT 1 FROM LOGT-IDSEKVNR                                    
550000         PERFORM IMS-ISRT-WDL301                                          
560000       END-PERFORM                                                        
570000     END-IF                                                               
580000     .                                                                    
590000     EJECT                                                                
600000 S20-SEND-OPEN SECTION.                                                   
610000     MOVE 'OPEN'                        TO SEND-KDFUNC                    
620004     MOVE WS-ADDRESS-MQASYNC            TO SEND-ADDISPABS                 
630000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
640000                         SEND-OPEN-AREA                                   
650000     IF SEND-KDRC > 0                                                     
660000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
670000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
680000       DELIMITED BY SIZE INTO FELTEXT                                     
690000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
700000     END-IF                                                               
710000     .                                                                    
720000     EJECT                                                                
730002 S21-SEND-PUT-PROP SECTION.                                               
740002                                                                          
750002     SET PROP-IX                 TO +1                                    
760002*    MANDATORY PROPERTY THAT SPECIFIES THE ACTUAL DESTINATION             
770002     MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
780002     MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
790002     MOVE WS-ADDRESS-WHSTOCKA    TO PROP-BEPROPVALUE (PROP-IX)            
800002                                                                          
810002     SET PROP-IX              UP BY +1                                    
820002*    OPTIONAL MQ MESSAGE PROPERTIES. CAN BE CASE-SENSITIVE                
830002     MOVE 'MQMPROP'              TO PROP-IDPROPTYPE  (PROP-IX)            
840002     MOVE 'CountryCode'          TO PROP-IDPROPNAME  (PROP-IX)            
850002     MOVE 'BR'                   TO PROP-BEPROPVALUE (PROP-IX)            
860002                                                                          
870002*    SET THE NUMBER OF PROPERTIES (KVANTAL) SO CORRECT LENGTH             
880002*    IS CALCULATED.                                                       
890002     SET PROP-KVANTAL            TO PROP-IX                               
900002                                                                          
910002     MOVE 'PUT'                            TO SEND-KDFUNC                 
920002     MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
930002     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
940002     CALL WZ01SEND USING SEND-CONTROL-AREA                                
950002                         SEND-KVDLEN                                      
960002                         PROP-WZ04PROP                                    
970002     IF SEND-KDRC > 1                                                     
980002       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
990002       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
000002       DELIMITED BY SIZE INTO FELTEXT                                     
010002       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020002     END-IF                                                               
030002     .                                                                    
040002     EJECT                                                                
050002 S22-SEND-PUT SECTION.                                                    
060000                                                                          
070000     MOVE 'PUT'                            TO SEND-KDFUNC                 
080000     MOVE LENGTH OF NOTF-AREA              TO SEND-KVDLEN                 
090000     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
100000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
110000                         SEND-KVDLEN                                      
120000                         NOTF-AREA                                        
130000     IF SEND-KDRC > 1                                                     
140000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
150000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
160000       DELIMITED BY SIZE INTO FELTEXT                                     
170000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
180000     END-IF                                                               
190000     .                                                                    
200000     EJECT                                                                
210002 S23-SEND-CLOSE SECTION.                                                  
220000     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
220100     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
230000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
240000                                                                          
250000     IF SEND-KDRC > 0                                                     
260000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
270000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
280000       DELIMITED BY SIZE INTO FELTEXT                                     
290000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
300000     END-IF                                                               
310000     .                                                                    
320000     EJECT                                                                
330000                                                                          
340000 Z-FINIT     SECTION.                                                     
350000     MOVE WS-IDLOPNRM     TO 6018-IDLOPNRM                                
360000     PERFORM IMS-REPL-W6LOPA                                              
370000                                                                          
380000     IF DISP-SVAR = JA                                                    
390000       MOVE INF-UPPDATE-DONE   TO MSG-KOM-IDMFSMED                        
400000       PERFORM IMS-ISRT-DISP-MSG                                          
410000     END-IF                                                               
420000     .                                                                    
430000     EJECT                                                                
440000                                                                          
450000* --- IMS SEKTIONER ---                                                   
460000     SKIP3                                                                
470000 IMS-GET-MSG SECTION.                                                     
480000                                                                          
490000     MOVE '  QC' TO GODK-STATUSKODER                                      
500000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
510000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
520000     PERFORM IMS-STATUSKONTROLL                                           
530000     .                                                                    
540000     SKIP3                                                                
550000 IMS-GN-MSG SECTION.                                                      
560000                                                                          
570000     MOVE '  QD' TO GODK-STATUSKODER                                      
580000     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
590000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
600000     PERFORM IMS-STATUSKONTROLL                                           
610000     .                                                                    
620000     SKIP3                                                                
630000 IMS-ISRT-DISP-MSG   SECTION.                                             
640000                                                                          
650000     MOVE '  ' TO GODK-STATUSKODER                                        
660000     CALL CBLTDLI USING ISRT DISP-PCB MSG-KOM-WMSGKOM                     
670000     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
680000     PERFORM IMS-STATUSKONTROLL                                           
690000     .                                                                    
700000     EJECT                                                                
710000 IMS-GU-WLARTC01     SECTION.                                             
720000                                                                          
730000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
740000          DELIMITED BY SIZE INTO SSA1                                     
750000     MOVE '  ' TO GODK-STATUSKODER                                        
760000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
770000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
780000     PERFORM IMS-STATUSKONTROLL                                           
790000     .                                                                    
800000     SKIP3                                                                
810000 IMS-GNP-WLARTC11   SECTION.                                              
820000                                                                          
830000     MOVE 'WLARTC11 ' TO SSA1                                             
840000     MOVE '  ' TO GODK-STATUSKODER                                        
850000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA4 SSA1                    
860000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
870000     PERFORM IMS-STATUSKONTROLL                                           
880000     .                                                                    
890000     EJECT                                                                
900000 IMS-GHNP-WLARTC11   SECTION.                                             
910000                                                                          
920000     MOVE 'WLARTC11 ' TO SSA1                                             
930000     MOVE '  ' TO GODK-STATUSKODER                                        
940000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA4 SSA1                   
950000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
960000     PERFORM IMS-STATUSKONTROLL                                           
970000     .                                                                    
980000     SKIP3                                                                
990000 IMS-GHNP-WLARTC11-FIRST   SECTION.                                       
000000                                                                          
010000     MOVE 'WLARTC11*F ' TO SSA1                                           
020000     MOVE '  ' TO GODK-STATUSKODER                                        
030000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA4 SSA1                   
040000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
050000     PERFORM IMS-STATUSKONTROLL                                           
060000     .                                                                    
070000     EJECT                                                                
080000 IMS-GHNP-WLARTC21 SECTION.                                               
090000                                                                          
100000     STRING 'WLARTC21(IDLEVNR  =' W-IDLEVNR-21-X ')'                      
110000          DELIMITED BY SIZE INTO SSA1                                     
120000     MOVE '  GE' TO GODK-STATUSKODER                                      
130000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
140000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
150000     PERFORM IMS-STATUSKONTROLL                                           
160000     SKIP3                                                                
170000     .                                                                    
180000 IMS-REPL-WLARTC     SECTION.                                             
190000                                                                          
200000     MOVE '  ' TO GODK-STATUSKODER                                        
210000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
220000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
230000     PERFORM IMS-STATUSKONTROLL                                           
240000     .                                                                    
250000     EJECT                                                                
260000 IMS-REPL-WLARTC11   SECTION.                                             
270000                                                                          
280000     MOVE '  ' TO GODK-STATUSKODER                                        
290000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA4                        
300000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
310000     PERFORM IMS-STATUSKONTROLL                                           
320000     .                                                                    
330000     SKIP3                                                                
340000 IMS-GNP-ARTC23 SECTION.                                                  
350000                                                                          
360000     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
370000     MOVE 'WLARTC23'              TO SSA2                                 
380000     MOVE '  GE' TO GODK-STATUSKODER                                      
390000     CALL CBLTDLI USING GNP ARTC-PCB WLARTC23 SSA1 SSA2                   
400000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
410000     PERFORM IMS-STATUSKONTROLL                                           
420000     .                                                                    
430000     EJECT                                                                
440000 IMS-GU-WLINLE01   SECTION.                                               
450000                                                                          
460000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
470000          DELIMITED BY SIZE INTO SSA1                                     
480000     MOVE '  GE' TO GODK-STATUSKODER                                      
490000     CALL CBLTDLI USING GU   INLE-PCB DLI-IO-AREA SSA1                    
500000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
510000     PERFORM IMS-STATUSKONTROLL                                           
520000     .                                                                    
530000     SKIP3                                                                
540000 IMS-ISRT-WLINLE01 SECTION.                                               
550000                                                                          
560000     MOVE 'WLINLE01 ' TO SSA1                                             
570000     MOVE '  ' TO GODK-STATUSKODER                                        
580000     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1                    
590000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
600000     PERFORM IMS-STATUSKONTROLL                                           
610000     .                                                                    
620000     SKIP3                                                                
630000 IMS-ISRT-WLINLE11 SECTION.                                               
640000                                                                          
650000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
660000          DELIMITED BY SIZE INTO SSA1                                     
670000     MOVE 'WLINLE11 ' TO SSA2                                             
680000     MOVE '  II' TO GODK-STATUSKODER                                      
690000     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2               
700000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
710000     PERFORM IMS-STATUSKONTROLL                                           
720000     .                                                                    
730000     SKIP3                                                                
740000 IMS-ISRT-WLINLE22 SECTION.                                               
750000                                                                          
760000     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
770000          DELIMITED BY SIZE INTO SSA1                                     
780000     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
790000          DELIMITED BY SIZE INTO SSA2                                     
800000     MOVE 'WLINLE22 ' TO SSA3                                             
810000     MOVE '  ' TO GODK-STATUSKODER                                        
820000     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
830000     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
840000     PERFORM IMS-STATUSKONTROLL                                           
850000     .                                                                    
860000     EJECT                                                                
870000 IMS-ISRT-ZZAC01 SECTION.                                                 
880000                                                                          
890000     MOVE 'WLZZAC01 ' TO SSA1                                             
900000     MOVE '  II' TO GODK-STATUSKODER                                      
910000     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
920000     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
930000     PERFORM IMS-STATUSKONTROLL                                           
940000     .                                                                    
950000     EJECT                                                                
960000 IMS-GU-INLB11 SECTION.                                                   
970000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
980000          DELIMITED BY SIZE INTO SSA1                                     
990000     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
000000          DELIMITED BY SIZE INTO SSA2                                     
010000     MOVE '  GE' TO GODK-STATUSKODER                                      
020000     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2                 
030000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
040000     PERFORM IMS-STATUSKONTROLL                                           
050000     .                                                                    
060000     SKIP3                                                                
070000 IMS-GHU-INLB11 SECTION.                                                  
080000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
090000          DELIMITED BY SIZE INTO SSA1                                     
100000     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
110000          DELIMITED BY SIZE INTO SSA2                                     
120000     MOVE '  ' TO GODK-STATUSKODER                                        
130000     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2                
140000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
150000     PERFORM IMS-STATUSKONTROLL                                           
160000     .                                                                    
170000     SKIP3                                                                
180000 IMS-GHNP-INLB23-KD SECTION.                                              
190000     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
200000          DELIMITED BY SIZE INTO SSA1                                     
210000     MOVE '  GE' TO GODK-STATUSKODER                                      
220000     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
230000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
240000     PERFORM IMS-STATUSKONTROLL                                           
250000     .                                                                    
260000     EJECT                                                                
270000 IMS-GHNP-INLB23-TI-F SECTION.                                            
280000* ?  STRING 'WLINLB23*F(DAAVROP  =' W-DAAVROP-X                           
290000     STRING 'WLINLB23*F(WDD905KY =' W-WDD905KY-X                          
300000                      '&KDAVROP  =' W-KDAVROP-X ')'                       
310000          DELIMITED BY SIZE INTO SSA1                                     
320000     MOVE '  ' TO GODK-STATUSKODER                                        
330000     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
340000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
350000     PERFORM IMS-STATUSKONTROLL                                           
360000     .                                                                    
370000     SKIP3                                                                
380000 IMS-GNP-INLB32 SECTION.                                                  
390000     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
400000          DELIMITED BY SIZE INTO SSA1                                     
410000     STRING 'WLINLB32(IDORDNSB =' W-IDORDNSB-X ')'                        
420000          DELIMITED BY SIZE INTO SSA2                                     
430000     MOVE '  ' TO GODK-STATUSKODER                                        
440000     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
450000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
460000     PERFORM IMS-STATUSKONTROLL                                           
470000     .                                                                    
480000     EJECT                                                                
490000 IMS-GHNP-INLB24 SECTION.                                                 
500000     MOVE 'WLINLB24 ' TO SSA1                                             
510000     MOVE '  GE' TO GODK-STATUSKODER                                      
520000     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
530000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
540000     PERFORM IMS-STATUSKONTROLL                                           
550000     .                                                                    
560000     SKIP3                                                                
570000 IMS-ISRT-INLB31 SECTION.                                                 
580000     MOVE 'WLINLB31 ' TO SSA1                                             
590000     MOVE '  ' TO GODK-STATUSKODER                                        
600000     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA2 SSA1                   
610000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
620000     PERFORM IMS-STATUSKONTROLL                                           
630000     .                                                                    
640000     SKIP3                                                                
650000 IMS-REPL-INLB SECTION.                                                   
660000     MOVE '  ' TO GODK-STATUSKODER                                        
670000     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA                         
680000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
690000     PERFORM IMS-STATUSKONTROLL                                           
700000     .                                                                    
710000     SKIP3                                                                
720000 IMS-DLET-INLB SECTION.                                                   
730000     MOVE '  ' TO GODK-STATUSKODER                                        
740000     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA                         
750000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
760000     PERFORM IMS-STATUSKONTROLL                                           
770000     .                                                                    
780000     EJECT                                                                
790000 IMS-GU-WDL601 SECTION.                                                   
800000     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
810000          DELIMITED BY SIZE INTO SSA1                                     
820000     MOVE '  GE' TO GODK-STATUSKODER                                      
830000     CALL CBLTDLI USING GU INLC-PCB DLI-IO-AREA5 SSA1                     
840000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
850000     PERFORM IMS-STATUSKONTROLL                                           
860000     .                                                                    
870000     SKIP3                                                                
880000 IMS-ISRT-WDL601 SECTION.                                                 
890000                                                                          
900000     MOVE 'WLINLC01 ' TO SSA1                                             
910000     MOVE '  ' TO GODK-STATUSKODER                                        
920000     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA5 SSA1                   
930000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
940000     PERFORM IMS-STATUSKONTROLL                                           
950000     .                                                                    
960000     EJECT                                                                
970000 IMS-ISRT-WDL611 SECTION.                                                 
980000                                                                          
990000     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
000000          DELIMITED BY SIZE INTO SSA1                                     
010000     MOVE 'WLINLC11 ' TO SSA2                                             
020000     MOVE '  II' TO GODK-STATUSKODER                                      
030000     CALL CBLTDLI USING ISRT INLC-PCB DLI-IO-AREA5 SSA1 SSA2              
040000     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
050000     PERFORM IMS-STATUSKONTROLL                                           
060000     .                                                                    
070000     EJECT                                                                
080000 IMS-ISRT-WDL623    SECTION.                                              
090000     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
100000          DELIMITED BY SIZE INTO SSA1                                     
110000     STRING 'WDL611  (DAINLEV  =' W-DAINLEV-X ')'                         
120000          DELIMITED BY SIZE INTO SSA2                                     
130000     MOVE 'WDL623 ' TO SSA3                                               
140000     MOVE '  II' TO GODK-STATUSKODER                                      
150000     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL623                       
160000                             SSA1 SSA2 SSA3                               
170000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
180000     PERFORM IMS-STATUSKONTROLL                                           
190000     .                                                                    
200000     EJECT                                                                
210000 IMS-GHU-W6LOPA11 SECTION.                                                
220000     STRING 'W6LOPA01(W6GXKEY  =' W-6017KEY-X ')'                         
230000          DELIMITED BY SIZE INTO SSA1                                     
240000     MOVE 'W6LOPA11 ' TO SSA2                                             
250000     MOVE '  ' TO GODK-STATUSKODER                                        
260000     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA3 SSA1 SSA2               
270000     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
280000     PERFORM IMS-STATUSKONTROLL                                           
290000     .                                                                    
300000     EJECT                                                                
310000 IMS-REPL-W6LOPA SECTION.                                                 
320000     MOVE '  ' TO GODK-STATUSKODER                                        
330000     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA3                        
340000     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
350000     PERFORM IMS-STATUSKONTROLL                                           
360000     .                                                                    
370000     EJECT                                                                
380000 IMS-ISRT-WLFILA01 SECTION.                                               
390000                                                                          
400000     MOVE 'WLFILA01 ' TO SSA1                                             
410000     MOVE '  II' TO GODK-STATUSKODER                                      
420000     CALL CBLTDLI USING ISRT FILA-PCB DLI-IO-AREA7 SSA1                   
430000     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
440000     PERFORM IMS-STATUSKONTROLL                                           
450000     .                                                                    
460000     EJECT                                                                
470000 IMS-ISRT-WDL301 SECTION.                                                 
480000     MOVE 'WDL301 ' TO SSA1                                               
490000     MOVE '  II' TO GODK-STATUSKODER                                      
500000     CALL CBLTDLI USING ISRT WDL3-PCB DLI-IO-WDL301 SSA1                  
510000     MOVE WDL3-STATUS-CODE TO STATUS-WS                                   
520000     PERFORM IMS-STATUSKONTROLL                                           
530000     .                                                                    
540000     EJECT                                                                
550000 IMS-ISRT-4506 SECTION.                                                   
560000     STRING 'WL450501(WDGXKEY  =' W-4505-KEY-X ')'                        
570000          DELIMITED BY SIZE INTO SSA1                                     
580000     MOVE 'WL450511 ' TO SSA2                                             
590000     MOVE '  ' TO GODK-STATUSKODER                                        
600000     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA-4505 SSA1 SSA2          
610000     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
620000     PERFORM IMS-STATUSKONTROLL                                           
630000     .                                                                    
640000     SKIP3                                                                
650000 IMS-ISRT-WDL9 SECTION.                                                   
660000     MOVE 'WLLOGA01 ' TO SSA1                                             
670000     MOVE '  II' TO GODK-STATUSKODER                                      
680000     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
690000     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
700000     PERFORM IMS-STATUSKONTROLL                                           
710000     .                                                                    
720000     SKIP3                                                                
730000 IMS-GHU-WDK711-K7 SECTION.                                               
740000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
750000          DELIMITED BY SIZE INTO SSA1                                     
760000     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
770000          DELIMITED BY SIZE INTO SSA2                                     
780000     MOVE '  GE' TO GODK-STATUSKODER                                      
790000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711  SSA1 SSA2             
800000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
810000     PERFORM IMS-STATUSKONTROLL                                           
820000     .                                                                    
830000                                                                          
840000 IMS-GHU-WDK712-K7 SECTION.                                               
850000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
860000     DELIMITED BY SIZE INTO SSA1                                          
870000     STRING 'WDK712  (IDLAND   =' W-IDLAND-K7-X ')'                       
880000     DELIMITED BY SIZE INTO SSA2                                          
890000     MOVE '    ' TO GODK-STATUSKODER                                      
900000     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1                   
910000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
920000     PERFORM IMS-STATUSKONTROLL                                           
930000     .                                                                    
940000 IMS-REPL-WDK712-K7 SECTION.                                              
950000     MOVE '  ' TO GODK-STATUSKODER                                        
960000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
970000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
980000     PERFORM IMS-STATUSKONTROLL                                           
990000     .                                                                    
000000                                                                          
010000 IMS-GHNP-WDK724-K7 SECTION.                                              
020000     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
030000                     '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                     
040000     DELIMITED BY SIZE INTO SSA1                                          
050000     MOVE '  GE' TO GODK-STATUSKODER                                      
060000     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK724 SSA1                  
070000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
080000     PERFORM IMS-STATUSKONTROLL                                           
090000     .                                                                    
100000 IMS-REPL-WDK724-K7 SECTION.                                              
110000     MOVE '  ' TO GODK-STATUSKODER                                        
120000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK724                       
130000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
140000     PERFORM IMS-STATUSKONTROLL                                           
150000     .                                                                    
160000     EJECT                                                                
170000                                                                          
180000 IMS-ISRT-EKOTRANS  SECTION.                                              
190000     MOVE 'WLFILB01 ' TO SSA1                                             
200000     MOVE '  II' TO GODK-STATUSKODER                                      
210000     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA-FILB01 SSA1             
220000     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
230000     PERFORM IMS-STATUSKONTROLL                                           
240000     .                                                                    
250000     EJECT                                                                
260000 IMS-ISRT-WLSAPA01 SECTION.                                               
270000     MOVE 'WLSAPA01 ' TO SSA1                                             
280000     MOVE '  II' TO GODK-STATUSKODER                                      
290000     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
300000     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
310000     PERFORM IMS-STATUSKONTROLL                                           
320000     .                                                                    
330000     SKIP2                                                                
340000 IMS-GHU-WDK7-WDK711 SECTION.                                             
350000                                                                          
360000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
370000          DELIMITED BY SIZE INTO SSA1                                     
380000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
390000          DELIMITED BY SIZE INTO SSA2                                     
400000     MOVE '  GE' TO GODK-STATUSKODER                                      
410000     CALL CBLTDLI USING GHU WDK7I-PCB DLI-IO-WDK711-I SSA1 SSA2           
420000     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
430000     PERFORM IMS-STATUSKONTROLL                                           
440000     .                                                                    
450000     SKIP3                                                                
460000 IMS-REPL-WDK711 SECTION.                                                 
470000     SKIP2                                                                
480000     MOVE '  ' TO GODK-STATUSKODER                                        
490000     CALL CBLTDLI USING REPL WDK7I-PCB DLI-IO-WDK711-I                    
500000     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
510000     PERFORM IMS-STATUSKONTROLL                                           
520000     .                                                                    
530000     EJECT                                                                
540000 IMS-GHNP-WDK728-LAST SECTION.                                            
550000                                                                          
560000     STRING 'WDK728  *L(DAINLEV < ' W-DAINLEV ')'                         
570000          DELIMITED BY SIZE INTO SSA1                                     
580000     MOVE '  GE' TO GODK-STATUSKODER                                      
590000     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK728 SSA1                  
600000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
610000     PERFORM IMS-STATUSKONTROLL                                           
620000     .                                                                    
630000     EJECT                                                                
640000 IMS-REPL-WDK728 SECTION.                                                 
650000                                                                          
660000     MOVE '  ' TO GODK-STATUSKODER                                        
670000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK728                       
680000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
690000     PERFORM IMS-STATUSKONTROLL                                           
700000     .                                                                    
710000     EJECT                                                                
720000 IMS-GU-WDB601 SECTION.                                                   
730000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
740000          DELIMITED BY SIZE INTO SSA1                                     
750000     MOVE '  ' TO GODK-STATUSKODER                                        
760000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
770000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
780000     PERFORM IMS-STATUSKONTROLL                                           
790000     .                                                                    
800000     EJECT                                                                
810000                                                                          
820000 IMS-GNP-WDB617    SECTION.                                               
830000     MOVE 'WDB617   ' TO SSA1                                             
840000     MOVE '  GE' TO GODK-STATUSKODER                                      
850000     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
860000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
870000     PERFORM IMS-STATUSKONTROLL                                           
880000     .                                                                    
890000                                                                          
900000 IMS-GU-WDF101   SECTION.                                                 
910000     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
920000     DELIMITED BY SIZE INTO SSA1                                          
930000     MOVE '  GE' TO GODK-STATUSKODER                                      
940000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
950000     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
960000     PERFORM IMS-STATUSKONTROLL                                           
970000     .                                                                    
980000     SKIP3                                                                
990000                                                                          
000000 IMS-GNP-WDF102   SECTION.                                                
010000     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
020000     DELIMITED BY SIZE INTO SSA1                                          
030000     MOVE '  GE' TO GODK-STATUSKODER                                      
040000     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-AREA-F102 SSA1                
050000     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
060000     PERFORM IMS-STATUSKONTROLL                                           
070000     .                                                                    
080000     EJECT                                                                
090000                                                                          
100000 IMS-GU-WDGX9306 SECTION.                                                 
110000     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
120000             DELIMITED BY SIZE INTO SSA1                                  
130000     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
140000             DELIMITED BY SIZE INTO SSA2                                  
150000     MOVE '  GE'   TO GODK-STATUSKODER                                    
160000     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
170000     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
180000     PERFORM IMS-STATUSKONTROLL                                           
190000     .                                                                    
200000     SKIP3                                                                
210000                                                                          
220000 IMS-GNP-WDGX9308 SECTION.                                                
230000     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
240000             DELIMITED BY SIZE INTO SSA1                                  
250000     MOVE '  GE'   TO GODK-STATUSKODER                                    
260000     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
270000     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
280000     PERFORM IMS-STATUSKONTROLL                                           
290000     .                                                                    
300000     SKIP3                                                                
310000                                                                          
320000 IMS-GNP-WDGX9308-FIRST SECTION.                                          
330000     MOVE 'WDGX9308*F' TO SSA1                                            
340000     MOVE '  GE'   TO GODK-STATUSKODER                                    
350000     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
360000     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
370000     PERFORM IMS-STATUSKONTROLL                                           
380000     .                                                                    
390000     SKIP3                                                                
400000                                                                          
410000 IMS-STATUSKONTROLL SECTION.                                              
420000                                                                          
430000     SET STATUS-IX TO 1                                                   
440000     SEARCH GODK-STATUS                                                   
450000       AT END                                                             
460000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
470000         DELIMITED BY SIZE INTO FELTEXT                                   
480000         CALL FELLOG                                                      
490000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
500000         CONTINUE                                                         
510000     END-SEARCH                                                           
520000     .                                                                    
530000     EJECT                                                                
540000*    -COPY WY2000P1                                                       
