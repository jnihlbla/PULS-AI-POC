000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6011B00.                                                
000400 AUTHOR.         MÅNS SAMUELSSON.                                         
000500 DATE-WRITTEN.   94/11/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄGGER UPP R33 OR PÅ REGISTER                                    
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
001200*        PROGRAMMET UPPDATERAR WLFILB (WDR8)                              
001300*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W6T11B                                              
001700*        MID:         W6I11B01                                            
001800*                                                                         
001900*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700*    -COPY WY2000W1                                                       
002800     SKIP3                                                                
002900 77  IDPGM                       PIC X(08)     VALUE 'W6011B00'.          
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80)     VALUE SPACE.               
003300                                                                          
003400 01  WS-INLEV.                                                            
003500     03  WS-INLEV-KONTO           PIC X(10).                              
003600     03  WS-INLEV-KST             PIC X(10).                              
003700     03  WS-INLEV-ANALYS          PIC X(12).                              
003800 01  WS-TIAAVV                    PIC 9(04)    VALUE ZERO.                
003900 01  WS-VVD                       PIC 9(3).                               
004000                                                                          
004100 01  WS-IDLOPNRM                 PIC 9(9)      VALUE ZERO.                
004200 01  W-0VVDLLLLK  REDEFINES WS-IDLOPNRM.                                  
004300     03 FILLER                   PIC 9(1).                                
004400     03 W-VVD                    PIC 9(3).                                
004500     03 W-LLLL                   PIC 9(4).                                
004600     03 W-K                      PIC 9(1).                                
004700 01  W-RETULF                    PIC S9(3)V9(4) VALUE +0.                 
004800                                                                          
004900 77  JA                          PIC X         VALUE 'J'.                 
005000 77  NEJ                         PIC X         VALUE 'N'.                 
005100                                                                          
005200 77  NOLL-RAKNARE                PIC S9(5)     VALUE ZERO COMP-3.         
005300 77  WS-SAP-X-IDAVINR            PIC X(7)      VALUE SPACE.               
005400 77  WS-SAP-MM-POST              PIC X(1)      VALUE SPACE.               
005500 77  W-DATE-AAMM                 PIC 9(4)      VALUE ZERO.                
005600                                                                          
005700 77  MID-IX                      PIC S9(9)     VALUE +1 COMP SYNC.        
005800                                                                          
005900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006000                                                                          
006100 77  W-IDTRANS                   PIC X(4)       VALUE SPACE.              
006200     88  EGEN-MID                               VALUE '611B'.             
006300     88  GODK-MID                               VALUE '6100'.             
006400                                                                          
006500 77  TRAEFF-SW                   PIC X          VALUE 'N'.                
006600     88  TRAEFF                                 VALUE 'J'.                
006700                                                                          
006800 77  AVROP-SW                    PIC X          VALUE 'N'.                
006900     88  AVROP-SAKNAS                           VALUE 'N'.                
007000     EJECT                                                                
007100 01  SPAR-ARTC01-IDLEVNR         PIC  X(5)      VALUE SPACE.              
007200 01  SPAR-ARTC01-KDPROSL         PIC S9(3)      VALUE +0 COMP-3.          
007300 01  SPAR-ARTC01-KDSORT          PIC  X(2)      VALUE SPACE.              
007400 01  SPAR-ARTC21-PRARTBES-PR     PIC S9(7)V9(2) VALUE +0 COMP-3.          
007500 01  SPAR-ARTC21-PRARTBEL-PR     PIC S9(7)V9(5) VALUE +0 COMP-3.          
007600 01  SPAR-ARTC21-PRARTBEL-SUM    PIC S9(7)V9(5) VALUE +0 COMP-3.          
007700 01  SPAR-ARTC21-KDVALISO        PIC X(3)       VALUE SPACE.              
007800 01  SPAR-PRKURS                 PIC S9(5)V9(5) VALUE +0   COMP-3.        
007900*                                                                         
008000 01  W-PRL-DADAT                 PIC 9(8)       VALUE ZERO.               
008100 01  WS-DAGENS-DATUM             PIC 9(8)       VALUE ZERO.               
008200 01  DAGENS-DATUM                PIC 9(6)       VALUE ZERO.               
008300 01  WS-DAGENS-TID.                                                       
008400     03 DAGENS-TID               PIC 9(8)    VALUE ZERO.                  
008500     03 FILLER REDEFINES DAGENS-TID.                                      
008600         05 DAGENS-HHMMSS        PIC 9(6).                                
008700         05 FILLER               PIC 9(2).                                
008800                                                                          
008900     03  WS-DATE-YYMMDD            PIC 9(06).                             
009000     03  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                          
009100         05  WS-DATE-YYMM          PIC 9(04).                             
009200         05  WS-DATE-DD            PIC 9(02).                             
009300                                                                          
009400 01  W-PRKURS                    PIC S9(5)V9(5) VALUE +0   COMP-3.        
009500 01  W-REVALUTA                  PIC S9(5)      VALUE +0   COMP-3.        
009600                                                                          
009700 01  WS-SUDIRLON                 PIC S9(7)V9(2) VALUE +0 COMP-3.          
009800 01  WS-SUDIRMTRL                PIC S9(7)V9(2) VALUE +0 COMP-3.          
009900 01  WS-SUHEMT                   PIC S9(7)V9(2) VALUE +0 COMP-3.          
010000 01  WS-SUARTSTD                 PIC S9(9)V9(2) VALUE +0 COMP-3.          
010100*                                                                         
010200 01  WS-ARTC23-IDAVTAL           PIC 9(13)      VALUE ZERO.               
010300 01  WS-DAINLEV                  PIC 9(16)      VALUE ZERO.               
010400*                                                                         
010500 01  WS-IDLOGLOP                 PIC S9(1)      VALUE ZERO.               
010600*                                                                         
010700 01  WS-TIAAAAMMDDTTMMSSTH       PIC 9(16)      VALUE ZERO.               
010800 01  FILLER                      REDEFINES WS-TIAAAAMMDDTTMMSSTH.         
010900     03 WS-TISEKEL               PIC 9(2).                                
011000     03 WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                                
011100     03 WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                                
011200*                                                                         
011300*     -- IDLOPNRM I VALFRI FORM                                           
011400 01      FILLER.                                                          
011500  02     WS-IDLOPNRM-AAVVDLLLLK  PIC 9(10)      VALUE ZERO.               
011600  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
011700   03    WS-IDLOPNRM-AA          PIC 9(2).                                
011800   03    WS-IDLOPNRM-VVDLLLLK    PIC 9(8).                                
011900   03    FILLER                  REDEFINES WS-IDLOPNRM-VVDLLLLK.          
012000    04   WS-IDLOPNRM-VVDLLLL     PIC 9(7).                                
012100    04   FILLER                  PIC X(1).                                
012200  02     FILLER                  REDEFINES WS-IDLOPNRM-AAVVDLLLLK.        
012300   03    WS-IDLOPNRM-AAVVDLLLL   PIC 9(9).                                
012400   03    FILLER                  PIC X(1).                                
012500*      --- VALID IDDC CODES                                               
012600*                                                                         
012700*01    -COPY WWDCLAND                                                     
012800       EJECT                                                              
012900*                                                                         
013000*01    -COPY WWDC99                                                       
013100       EJECT                                                              
013200*                                                                         
013300*01    -COPY WWLEV04                                                      
013400       EJECT                                                              
013500*                                                                         
013600 01  WS-DAAVIDAT                 PIC 9(8).                                
013700*                                                                         
013800 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
013900     03 FLT-LGD                  PIC S9(1) COMP SYNC VALUE +7.            
014000     03 VAEGNINGSTAL             PIC 9(7) VALUE 2121212.                  
014100     03 VAEGNTAL-LGD             PIC S9 COMP SYNC VALUE +7.               
014200     03 MODUL-10-11              PIC 9(2) VALUE 10.                       
014300     03 ALT-A-B                  PIC X(1) VALUE 'B'.                      
014400*                                                                         
014500*    --- LOGG-TRANSAR                                                     
014600 01  WS-ZZAC01.                                                           
014700     03 WS-ZZAC01-LOGGPOST       PIC X(90)   VALUE SPACE.                 
014800     03 FILLER                   REDEFINES WS-ZZAC01-LOGGPOST.            
014900      04 WS-ZZAC01-LOGGPOST-1-3  PIC X(3).                                
015000      04 WS-ZZAC01-LOGGPOST-4-90 PIC X(87).                               
015100     03 WS-ZZAC01-SORTPOST       PIC X(36)   VALUE SPACE.                 
015200*    --- WDD9-FAELT                                                       
015300 01  WS-INLB.                                                             
015400     03 WS-INLB23.                                                        
015500      04 WS-INLB23-TIAVROP-INL   PIC S9(5)  VALUE ZERO COMP-3.            
015600*                                                                         
015700     03 WS-KV-LPLAN              PIC S9(7)  VALUE ZERO COMP-3.            
015800     03 WS-KV-OBOK               PIC S9(7)  VALUE ZERO COMP-3.            
015900     03 WS-IDAVINR               PIC 9(7)   VALUE ZERO.                   
016000     03 WS-BSKKVAR-GGR-10        PIC S9(7)  VALUE ZERO COMP-3.            
016100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016200 01  GENERELLA-SUBPROGRAM.                                                
016300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
016400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016600     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
016700     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
016800     EJECT                                                                
016900 01  MESSAGE-CODES.                                                       
017000     03  INF-UPPDATE-DONE        PIC X(3)    VALUE '101'.                 
017100*01  -COPY W211310  -PRE W330-                                            
017200     EJECT                                                                
017300*01  -COPY W211FEL  -PRE W211FEL-                                         
017400     EJECT                                                                
017500*01  -COPY W211M109 -PRE M109-                                            
017600     EJECT                                                                
017700*01  -COPY W211M113 -PRE M113-                                            
017800     EJECT                                                                
017900*01  -COPY WDATAREA                                                       
018000     EJECT                                                                
018100*01  -COPY W510CURR                                                       
018200     EJECT                                                                
018300*01  -COPY WWPRODSL                                                       
018400     EJECT                                                                
018500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018600*                                                                         
018700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018800     SKIP3                                                                
018900*01  MID -COPY W6I11B01                                                   
019000     EJECT                                                                
019100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019200     SKIP3                                                                
019300*01  -COPY WMSGKOM                                                        
019400     EJECT                                                                
019500*01  -COPY WMFSAREA                                                       
019600     EJECT                                                                
019700*01  -COPY WMSGAREA                                                       
019800     EJECT                                                                
019900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020000*                                                                         
020100     EJECT                                                                
020200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020300     SKIP3                                                                
020400 01  NYCKLAR-TILL-DLI.                                                    
020500     03  W-IDARTNR-X.                                                     
020600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020700     03  W-DAINLEV-X.                                                     
020800         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
020900     03  W-KDAVROP-X.                                                     
021000         05  W-KDAVROP           PIC S9(1)    VALUE ZERO COMP-3.          
021100     03  W-WDD901KY-X.                                                    
021200         07  W-IDARTNR-D9        PIC  9(9)    VALUE ZERO COMP-3.          
021300         07  W-IDDC-D9           PIC  X(2)    VALUE SPACE.                
021400     03  W-WDD905KY-X.                                                    
021500         05  W-DAAVROP-X.                                                 
021600             07  W-DAAVROP       PIC  9(6)    VALUE ZERO.                 
021700         05  W-TILEVDAG-X.                                                
021800             07  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.          
021900     03  W-IDORDNSB-X.                                                    
022000         05  W-IDORDNSB          PIC S9(5)    VALUE ZERO COMP-3.          
022100     03  W-INLB11-IDLEVNR-X.                                              
022200         05  W-INLB11-IDLEVNR    PIC  X(5)    VALUE SPACE.                
022300     03  W-6017KEY-X.                                                     
022400         05  W-6017-IDHTYP      PIC X(4)     VALUE '6017'.                
022500         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
022600     03  W-IDLEVNR-X.                                                     
022700         05  W-IDLEVNR          PIC  X(5)      VALUE SPACE.               
022800     03  W-IDLEVNR-21-X.                                                  
022900         05  W-IDLEVNR-21       PIC X(5) VALUE LOW-VALUE.                 
023000     03  W-IDLEVNR-PR-X.                                                  
023100         05  W-IDLEVNR-PR       PIC X(5) VALUE LOW-VALUE.                 
023200     03  W-DAPRLIST-K7-N.                                                 
023300         05  W-DAPRLIST-K7      PIC 9(8)    VALUE ZERO.                   
023400     03  W-IDLAND-K7-X.                                                   
023500         05  W-IDLAND-K7        PIC X(2)    VALUE SPACE.                  
023600     03  W-IDLAND-X.                                                      
023700         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
023800                                                                          
023900     03  W-WDK701-X.                                                      
024000         05  W-WDK701-IDARTNR  PIC S9(9) VALUE ZERO       COMP-3.         
024100     03  W-WDK711-X.                                                      
024200         05  W-WDK711-IDDC     PIC X(2)  VALUE SPACE.                     
024300     03  W-WDK711-K7-X.                                                   
024400         05  W-WDK711-K7-IDDC  PIC X(2)  VALUE SPACE.                     
024500     03  W-IDDC-B6-X.                                                     
024600         05 W-IDDC-B6                  PIC X(2).                          
024700     03  W-IDLANDX2-X.                                                    
024800         05    W-IDLANDX2              PIC X(2)    VALUE SPACE.           
024900     03  W-WDGX9305-X.                                                    
025000         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
025100         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
025200         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
025300         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
025400     03  W-KDVALISO-X.                                                    
025500         05  W-KDVALISO-ROW      PIC X(3)    VALUE SPACE.                 
025600     03  W-TISTADA9-X.                                                    
025700         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
025800     SKIP2                                                                
025900                                                                          
026000 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
026100*01  FILLER  -COPY WWDIST35   -RED TEST-IDDISTR.                          
026200     EJECT                                                                
026300                                                                          
026400*    --- STATUS-KOD FRÅN IMS                                              
026500 01  STATUS-WS                   PIC XX.                                  
026600     88  SEGMENT-FINNS                       VALUE '  '.                  
026700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026900     SKIP2                                                                
027000 01  GODK-STATUSKODER.                                                    
027100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027200     SKIP3                                                                
027300 01  SSA1                        PIC X(128).                              
027400 01  SSA2                        PIC X(64).                               
027500 01  SSA3                        PIC X(64).                               
027600     EJECT                                                                
027700*    --- IMS FUNKTIONSKODER                                               
027800*01  -COPY W0003                                                          
027900     EJECT                                                                
028000*    ---  DLI INPUT-OUTPUT AREA                                           
028100 01  DLI-IO-WDK701.                                                       
028200*    03  -COPY WDK701                                                     
028300     EJECT                                                                
028400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
028500 01  DLI-IO-WDK711.                                                       
028600*    03  -COPY WDK711                                                     
028700     SKIP3                                                                
028800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
028900 01  DLI-IO-WDK712.                                                       
029000*        05  -COPY WDK712                                                 
029100     SKIP3                                                                
029200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK724'.                      
029300 01  DLI-IO-WDK724.                                                       
029400*        05  -COPY WDK724                                                 
029500                                                                          
029600     EJECT                                                                
029700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
029800     SKIP3                                                                
029900 01  DLI-IO-AREA.                                                         
030000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
030100     SKIP3                                                                
030200     03  WLINLE01 REDEFINES IO-AREA.                                      
030300*        05  -COPY WDL201  -PRE INLE-                                     
030400     SKIP3                                                                
030500     03  WLINLE11 REDEFINES IO-AREA.                                      
030600*        05  -COPY WDL211  -PRE INLE-                                     
030700     SKIP3                                                                
030800     03  WLINLE22 REDEFINES IO-AREA.                                      
030900*        05  -COPY WDL222  -PRE INLE-                                     
031000     EJECT                                                                
031100     03  WLARTC01 REDEFINES IO-AREA.                                      
031200*        05  -COPY WDK601  -PRE ARTC01-                                   
031300     SKIP3                                                                
031400     03  WLARTC21 REDEFINES IO-AREA.                                      
031500*        05  -COPY WDK621  -PRE ARTC21-                                   
031600     SKIP3                                                                
031700     03  WLZZAC01 REDEFINES IO-AREA.                                      
031800*        05  -COPY WDG601  -PRE ZZAC01-                                   
031900     EJECT                                                                
032000     03  WLINLB11 REDEFINES IO-AREA.                                      
032100*        05  -COPY WDD902  -PRE INLB11-                                   
032200     SKIP3                                                                
032300     03  WLINLB23 REDEFINES IO-AREA.                                      
032400*        05  -COPY WDD905  -PRE INLB23-                                   
032500     SKIP3                                                                
032600     03  WLINLB24 REDEFINES IO-AREA.                                      
032700*        05  -COPY WDD924   -PRE INLB24-                                  
032800     SKIP3                                                                
032900     03  WLINLB32 REDEFINES IO-AREA.                                      
033000*        05  -COPY WDD907  -PRE INLB32-                                   
033100     EJECT                                                                
033200 01  DLI-IO-AREA2.                                                        
033300     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
033400     SKIP3                                                                
033500     03  WLINLB31 REDEFINES IO-AREA2.                                     
033600*        05  -COPY WDD906  -PRE INLB31-                                   
033700     EJECT                                                                
033800 01  DLI-IO-AREA3.                                                        
033900     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
034000     SKIP3                                                                
034100     03  W6LOPA11 REDEFINES IO-AREA3.                                     
034200*        05  -COPY W6GX6018                                               
034300     EJECT                                                                
034400 01  DLI-IO-AREA4.                                                        
034500     03  IO-AREA4                PIC X(900)  VALUE SPACE.                 
034600     SKIP3                                                                
034700     03  WLARTC11 REDEFINES IO-AREA4.                                     
034800*        05  -COPY WDK611   -PRE ARTC11-                                  
034900     EJECT                                                                
035000 01  FILLER                      PIC X(16)  VALUE 'WDK623'.               
035100                                                                          
035200*01  WLARTC23 -COPY WDK623                                                
035300     EJECT                                                                
035400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-FILB01'.         
035500 01  DLI-IO-AREA-FILB01.                                                  
035600*    05  -COPY WDR801  -PRE EKO-                                          
035700       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
035800         09  -COPY W51080 -PRE EKO-                                       
035900       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
036000         09  -COPY W510EKHA -PRE EKO-                                     
036100     EJECT                                                                
036200 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
036300 01  DLI-IO-WLSAPA01.                                                     
036400*    03  WLSAPA01  -COPY WDR901                                           
036500*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
036600     EJECT                                                                
036700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
036800 01   DLI-IO-AREA-B601.                                                   
036900*     03  -COPY WDB601                                                    
037000                                                                          
037100 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
037200 01   DLI-IO-AREA-B617.                                                   
037300*     03  -COPY WDB617                                                    
037400     EJECT                                                                
037500 01  DLI-IO-AREA-F1          PIC X(100).                                  
037600     SKIP2                                                                
037700*01  WLLEVA01 -COPY WDF101     -PRE F1-       -RED DLI-IO-AREA-F1         
037800     EJECT                                                                
037900                                                                          
038000 01  DLI-IO-AREA-F102        PIC X(100).                                  
038100     SKIP2                                                                
038200*01  WLLEVA11 -COPY WDF102 -PRE F102-     -RED DLI-IO-AREA-F102           
038300     EJECT                                                                
038400                                                                          
038500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
038600 01  DLI-IO-WDGX9306.                                                     
038700*    03  -COPY WDGX9306                                                   
038800     EJECT                                                                
038900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
039000 01  DLI-IO-WDGX9308.                                                     
039100*    03  -COPY WDGX9308                                                   
039200                                                                          
039300 LINKAGE SECTION.                                                         
039400                                                                          
039500*01  -COPY W0009   -PRE MSG-                                              
039600     EJECT                                                                
039700*01  -COPY W0009   -PRE DISP-                                             
039800     EJECT                                                                
039900*01  -COPY W0008  -PRE INLE-                                              
040000     05  FILLER                  PIC X.                                   
040100     EJECT                                                                
040200*01  -COPY W0008  -PRE ARTC-                                              
040300     05  FILLER                  PIC X.                                   
040400     EJECT                                                                
040500*01  -COPY W0008  -PRE ZZAC-                                              
040600     05  FILLER                  PIC X.                                   
040700     EJECT                                                                
040800*01  -COPY W0008  -PRE INLB-                                              
040900     05  FILLER                  PIC X(10).                               
041000     05  INLB-KFB-DAAVROP-AVS    PIC 9(6).                                
041100     05  INLB-KFB-TILEVDAG       PIC S9   COMP-3.                         
041200     EJECT                                                                
041300*01  -COPY W0008  -PRE LOPA-                                              
041400     05  FILLER                  PIC X.                                   
041500     EJECT                                                                
041600*01  -COPY W0008  -PRE FILB-                                              
041700     05  FILLER                  PIC X.                                   
041800     EJECT                                                                
041900*01  -COPY W0008  -PRE SAPA-                                              
042000     05  FILLER                  PIC X.                                   
042100     EJECT                                                                
042200*01  -COPY W0008  -PRE WDK7-                                              
042300     05  FILLER                  PIC X.                                   
042400     EJECT                                                                
042500*01  -COPY W0008  -PRE WDB6-                                              
042600     05  FILLER                  PIC X.                                   
042700     EJECT                                                                
042800*01  -COPY W0008  -PRE WDF1-                                              
042900     05  FILLER                  PIC X.                                   
043000     EJECT                                                                
043100*01  -COPY W0008  -PRE 9305-                                              
043200     05  FILLER                  PIC X.                                   
043300                                                                          
043400 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB INLE-PCB ARTC-PCB             
043500                           ZZAC-PCB INLB-PCB LOPA-PCB FILB-PCB            
043600                           SAPA-PCB WDK7-PCB WDB6-PCB                     
043700                           WDF1-PCB 9305-PCB.                             
043800     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB INLE-PCB ARTC-PCB             
043900                           ZZAC-PCB INLB-PCB LOPA-PCB FILB-PCB            
044000                           SAPA-PCB WDK7-PCB WDB6-PCB                     
044100                           WDF1-PCB 9305-PCB.                             
044200     PERFORM IMS-GET-MSG                                                  
044300     IF SEGMENT-FINNS                                                     
044400       PERFORM IMS-GN-MSG                                                 
044500       PERFORM A-INIT                                                     
044600       PERFORM UNTIL MID-IX > MID-KVPOST                                  
044700**** IF R33 FROM BOUNCE FLOW NO UPDATES EXEPT HISTORY                     
044800         MOVE MID-IDARTNR (MID-IX) TO W-IDARTNR                           
044900         MOVE MID-IDDISTR(MID-IX)  TO TEST-IDDISTR                        
045000         IF DIST35-NONVCC-REFILL                                          
045010         OR DIST35-NONVCC-VCC-TRANSFER                                    
045020         OR DIST35-NONVCC-NONVCC-TRANSFER                                 
045100           PERFORM E-UPPD-HISTORIK                                        
045200         ELSE                                                             
045300           PERFORM B-EV-UPPD-WLARTC11                                     
045400           IF MID-KDRT (MID-IX) = +0                                      
045500             PERFORM C-PRIS-JUST-KONTROLL                                 
045600           END-IF                                                         
045700           PERFORM G-HAMTA-AVTAL                                          
045800           PERFORM E-UPPD-HISTORIK                                        
045900           PERFORM D-MEDDELANDE                                           
046000           PERFORM F-UPPD-LEVPLAN                                         
046100         END-IF                                                           
046200         ADD +1     TO MID-IX                                             
046300       END-PERFORM                                                        
046400       PERFORM Z-FINIT                                                    
046500     END-IF                                                               
046600                                                                          
046700     MOVE ZERO TO RETURN-CODE                                             
046800     GOBACK                                                               
046900     .                                                                    
047000     EJECT                                                                
047100 A-INIT SECTION.                                                          
047200                                                                          
047300     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11B01                    
047400     MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                   
047500     MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                  
047600                                                                          
047700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
047800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
047900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
048000                                                                          
048100     MOVE 'IDAG'             TO DAT-KDDATFORM                             
048200     CALL WDATKONV USING        DAT-KDDATFORM                             
048300                                DAT-I-TIDATUM                             
048400                                DAT-O-TIDATUM                             
048500                                DAT-KDSVAR                                
048600     MOVE DAT-TIAAVVD-GRP (3:3) TO WS-VVD                                 
048700                                                                          
048800     PERFORM IMS-GHU-W6LOPA11                                             
048900     MOVE 6018-IDLOPNRM      TO WS-IDLOPNRM                               
049000     MOVE FUNCTION CURRENT-DATE TO WS-DAGENS-DATUM                        
049100     MOVE FUNCTION CURRENT-DATE (3:4) TO WS-DATE-YYMM                     
049200     MOVE 01                          TO WS-DATE-DD                       
049300     ACCEPT DAGENS-DATUM   FROM DATE                                      
049400     ACCEPT DAGENS-TID     FROM TIME                                      
049500                                                                          
049600     .                                                                    
049700     EJECT                                                                
049800 B-EV-UPPD-WLARTC11        SECTION.                                       
049900                                                                          
050000     PERFORM IMS-GU-WLARTC01                                              
050100     MOVE ARTC01-ART-IDLEVNR          TO SPAR-ARTC01-IDLEVNR              
050200     MOVE ARTC01-ART-KDPRODSL         TO SPAR-ARTC01-KDPROSL              
050300     MOVE ARTC01-ART-KDSORT           TO SPAR-ARTC01-KDSORT               
050400     PERFORM IMS-GHNP-WLARTC11                                            
050500     MOVE ARTC11-CLAG-TIAVIDAT-SEN   TO TMP1-YYMMDD                       
050600     MOVE MID-TIAVIDAT (MID-IX)      TO TMP2-YYMMDD                       
050700     PERFORM WY2000P1                                                     
050800     IF TMP1-YYMMDD < TMP2-YYMMDD   AND                                   
050900       (ARTC11-CLAG-KDHF > +0 OR MID-IDLEVNR (MID-IX)                     
051000        = SPAR-ARTC01-IDLEVNR)                                            
051100       MOVE MID-IDLEVNR (MID-IX)   TO ARTC11-CLAG-IDLEVNR-SEN             
051200       MOVE MID-IDAVINR (MID-IX)   TO ARTC11-CLAG-IDFS-SEN                
051300       MOVE MID-KVAVIS  (MID-IX)   TO ARTC11-CLAG-KVAVIS-SEN              
051400       PERFORM IMS-REPL-WLARTC11                                          
051500     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051800 C-PRIS-JUST-KONTROLL      SECTION.                                       
051900                                                                          
052000     MOVE +0                    TO SPAR-ARTC21-PRARTBEL-PR                
052100                                   SPAR-ARTC21-PRARTBEL-SUM               
052200                                   SPAR-ARTC21-PRARTBES-PR                
052300     MOVE NEJ                   TO TRAEFF-SW                              
052400     IF MID-TIAVIDAT(MID-IX) > 500000                                     
052500       MOVE 19                  TO WS-DAAVIDAT(1:2)                       
052600     ELSE                                                                 
052700       MOVE 20                  TO WS-DAAVIDAT(1:2)                       
052800     END-IF                                                               
052900     MOVE MID-TIAVIDAT(MID-IX)  TO WS-DAAVIDAT(3:6)                       
053000                                                                          
053100     MOVE MID-IDDC(MID-IX)              TO WS-IDDC                        
053200                                           W-WDK711-K7-IDDC               
053300     IF NDC-CN                                                            
053400       PERFORM CA-PRIS-JUST-KONTROLL                                      
053500     ELSE                                                                 
053600       PERFORM CB-PRIS-JUST-KONTROLL                                      
053700     END-IF                                                               
053800     .                                                                    
053900     EJECT                                                                
054000                                                                          
054100 CA-PRIS-JUST-KONTROLL SECTION.                                           
054200*    -- WDK711                                                            
054300     PERFORM IMS-GHU-WDK711-K7                                            
054400     IF SEGMENT-FINNS                                                     
054500                                                                          
054600*    -- WDK724                                                            
054700       MOVE MID-IDLEVNR(MID-IX)   TO W-IDLEVNR-PR                         
054800       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAAVIDAT                     
054900       PERFORM IMS-GHNP-WDK724-K7                                         
055000                                                                          
055100       IF SEGMENT-FINNS                                                   
055200         MOVE SPRL-PRARTBEL-PR    TO SPAR-ARTC21-PRARTBEL-PR              
055300         MOVE SPRL-PRARTBEL-PR    TO SPAR-ARTC21-PRARTBEL-SUM             
055400         MOVE SPRL-PRARTBES-PR    TO SPAR-ARTC21-PRARTBES-PR              
055500         MOVE SPRL-KDVALISO       TO SPAR-ARTC21-KDVALISO                 
055600         IF SPRL-SUINLEV-PR < +1                                          
055700           MOVE +1                TO SPRL-SUINLEV-PR                      
055800           PERFORM IMS-REPL-WDK724-K7                                     
055900         ELSE                                                             
056000           ADD +1                  TO SPRL-SUINLEV-PR                     
056100           PERFORM IMS-REPL-WDK724-K7                                     
056200         END-IF                                                           
056300                                                                          
056400         PERFORM IMS-GHNP-WLARTC11-FIRST                                  
056500       END-IF                                                             
056600     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900                                                                          
057000 CB-PRIS-JUST-KONTROLL SECTION.                                           
057100     MOVE MID-IDLEVNR(MID-IX)   TO W-IDLEVNR-21                           
057200     PERFORM IMS-GHNP-WLARTC21                                            
057300     PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF                               
057400      COMPUTE W-PRL-DADAT = 99999999 - ARTC21-PRL-DAPRLIST-9KOMPL         
057500       IF ARTC21-PRL-KDSTATUS-PR = +1 AND                                 
057600         W-PRL-DADAT <= WS-DAAVIDAT                                       
057700         MOVE JA TO TRAEFF-SW                                             
057800       ELSE                                                               
057900         PERFORM IMS-GHNP-WLARTC21                                        
058000       END-IF                                                             
058100     END-PERFORM                                                          
058200                                                                          
058300     IF TRAEFF                                                            
058400       MOVE ARTC21-PRL-PRARTBEL-PR    TO SPAR-ARTC21-PRARTBEL-PR          
058500       MOVE ARTC21-PRL-PRARTBEL-PR    TO SPAR-ARTC21-PRARTBEL-SUM         
058600       MOVE ARTC21-PRL-PRARTBES-PR    TO SPAR-ARTC21-PRARTBES-PR          
058700       MOVE ARTC21-PRL-KDVALISO       TO SPAR-ARTC21-KDVALISO             
058800       IF ARTC21-PRL-SUINLEV-PR < +1                                      
058900         MOVE +1         TO ARTC21-PRL-SUINLEV-PR                         
059000         PERFORM IMS-REPL-WLARTC                                          
059100                                                                          
059200         PERFORM IMS-GHNP-WLARTC11-FIRST                                  
059300                                                                          
059400         COMPUTE ARTC11-CLAG-PRARTSJK = SPAR-ARTC21-PRARTBES-PR +         
059500                                        ARTC11-CLAG-PRDIRLON    +         
059600                                        ARTC11-CLAG-PRDMTRL     +         
059700                                        ARTC11-CLAG-PROVRPAL              
059800         END-COMPUTE                                                      
059900                                                                          
060000         MOVE +1                      TO ARTC11-CLAG-KDTIPPR              
060100                                                                          
060200         PERFORM IMS-REPL-WLARTC11                                        
060300       ELSE                                                               
060400         ADD +1        TO ARTC21-PRL-SUINLEV-PR                           
060500         PERFORM IMS-REPL-WLARTC                                          
060600       END-IF                                                             
060700     END-IF                                                               
060800                                                                          
060900     .                                                                    
061000     EJECT                                                                
061100 D-MEDDELANDE              SECTION.                                       
061200                                                                          
061300     IF ARTC11-CLAG-KDERS > +9                                            
061400        PERFORM DB-MEDDELANDE-M113                                        
061500     END-IF                                                               
061600                                                                          
061700     MOVE MID-IDDC(MID-IX)            TO WS-IDDC                          
061800     IF SPAR-ARTC01-KDSORT = 'SW'                                         
061900**** SKALL SKAPAS HÄNDELSE 102-110 OM DET ÄR SOFTWARE                     
062000       PERFORM DF-MEDDELANDE-WDR9                                         
062100     ELSE                                                                 
062200       PERFORM DD-MEDDELANDE-EKO-WDR8-WDR9                                
062300     END-IF                                                               
062400                                                                          
062500     IF ARTC11-CLAG-PRINK NOT = ARTC11-CLAG-PRARTSTD                      
062600     AND (NOT XDC-NON-VCC-OWNED)                                          
062700       IF SPAR-ARTC01-KDSORT = 'SW'                                       
062800**** SKALL INTE SKAPAS NÅGON 103-101 NÄR DET ÄR SOFTWARE                  
062900         CONTINUE                                                         
063000       ELSE                                                               
063100         PERFORM DC-MEDDELANDE-EKO-WDR9-PALAGG                            
063200       END-IF                                                             
063300     END-IF                                                               
063400                                                                          
063500     IF MID-KDRT (MID-IX) NOT = +8                                        
063600        PERFORM DE-MEDDELANDE-330-221                                     
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000 DB-MEDDELANDE-M113 SECTION.                                              
064100                                                                          
064200     MOVE +0                     TO M113-IDLOPNRM                         
064300     MOVE MID-IDAVINR (MID-IX)   TO M113-IDAVINR                          
064400     MOVE MID-IDLEVNR (MID-IX)   TO M113-IDLEVNR                          
064500     MOVE MID-KDRT (MID-IX)      TO M113-KDRT                             
064600     MOVE MID-KVAVIS (MID-IX)    TO M113-KVANTAL                          
064700     MOVE ARTC11-CLAG-KDERS           TO M113-KDERS                       
064800     MOVE ARTC11-CLAG-KDLTK      TO M113-KDLTK                            
064900     MOVE ARTC11-CLAG-IDANSK     TO M113-IDANSKNR                         
065000                                                                          
065100     PERFORM S02-RED-W211FEL-GNRL                                         
065200     MOVE ARTC11-CLAG-IDANSK     TO W211FEL-IDKUNDNR-S                    
065300     MOVE '113'                  TO W211FEL-IDFELKODX                     
065400     MOVE M113-M113              TO W211FEL-FELMED                        
065500                                                                          
065600     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
065700     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
065800     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
065900     PERFORM S01-SKAPA-ZZAC01                                             
066000     .                                                                    
066100     EJECT                                                                
066200                                                                          
066300 DC-MEDDELANDE-EKO-WDR9-PALAGG SECTION.                                   
066400     MOVE 'W6011B00'                  TO FIL-IDPGM                        
066500     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
066600     MOVE FUNCTION CURRENT-DATE(1:2)  TO EKH-DAVERDAT(1:2)                
066700     MOVE MID-TIAVIDAT (MID-IX)       TO EKH-DAVERDAT(3:6)                
066800     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
066900     MOVE +1                          TO FIL-IDSEKVNR                     
067000     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
067100     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
067200     MOVE '103'                       TO EKH-KDEKHHT                      
067300     MOVE '101'                       TO EKH-KDEKSHT                      
067400     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
067500     MOVE MID-IDDC(MID-IX)            TO EKH-IDDC-SEND                    
067600     MOVE SPACE                       TO EKH-IDDC-REC                     
067700     MOVE +0                          TO EKH-IDDISTR                      
067800     MOVE +0                          TO EKH-IDKUNDNR                     
067900     MOVE SPACE                       TO EKH-IDVERGL                      
068000*******************************                                           
068100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
068200     MOVE ZERO TO NOLL-RAKNARE                                            
068300     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
068400     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
068500          FOR LEADING ZERO                                                
068600     ADD +1 TO NOLL-RAKNARE                                               
068700     UNSTRING WS-SAP-X-IDAVINR     INTO EKH-IDVERGL                       
068800          WITH POINTER NOLL-RAKNARE                                       
068900*******************************                                           
069000     MOVE SPAR-ARTC01-KDPROSL         TO EKH-KDPRODSL                     
069100     MOVE ZERO                        TO EKH-KDPSLLOC                     
069200                                         EKH-PRARTNTO                     
069300                                         EKH-PRARTSJK                     
069400                                         EKH-PRHEMTAG                     
069500                                         EKH-PRLANDCO                     
069600                                         EKH-SUBEL                        
069700                                         EKH-KDFRAKT                      
069800     MOVE ARTC11-CLAG-PRDIRLON        TO EKH-PRDIRLON                     
069900     MOVE ARTC11-CLAG-PRDMTRL         TO EKH-PRDMTRL                      
070000     MOVE ARTC11-CLAG-PROVRPAL        TO EKH-PROVRPAL                     
070100     MOVE MID-IDARTNR(MID-IX)         TO EKH-IDARTNR                      
070200     MOVE 'N'                         TO EKH-FLLSBOK                      
070300     MOVE 'SEK'                       TO EKH-KDVALISO                     
070400     MOVE 1.00                        TO EKH-PRKURS                       
070500     MOVE ARTC11-CLAG-PRINK           TO EKH-PRINK                        
070600     MOVE ARTC11-CLAG-PRARTSTD        TO EKH-PRARTSTD                     
070700     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVANTAL                      
070800     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
070900     MOVE SPACE                       TO EKH-BEVAT                        
071000                                         EKH-IDANALYS                     
071100                                         EKH-KDANMORS                     
071200     MOVE ZERO                        TO EKH-IDKONTO                      
071300                                         EKH-SUVAT                        
071400     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
071500     MOVE MID-IDAVINR (MID-IX)        TO EKH-IDAVINR                      
071600     MOVE MID-IDLEVNR (MID-IX)        TO EKH-IDLEVNR                      
071700     MOVE ZERO                        TO EKH-KDAVVTYP                     
071800     MOVE MID-KDRT (MID-IX)           TO EKH-KDRT                         
071900     MOVE ZERO                        TO EKH-KVANTMOT                     
072000     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVAVIS                       
072100     MOVE SPAR-ARTC01-KDSORT          TO EKH-KDSORT                       
072200     MOVE 'SEPV'                      TO EKH-KDTRADP                      
072300     MOVE SPACE                       TO EKH-FLDCET                       
072400     MOVE SPACE                       TO EKH-IDKUNDRF                     
072500                                         EKH-IDKST                        
072600     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
072700                                                                          
072800     PERFORM IMS-ISRT-WLSAPA01                                            
072900                                                                          
073000     PERFORM UNTIL SEGMENT-FINNS                                          
073100       ADD +1 TO FIL-IDSEKVNR                                             
073200       PERFORM IMS-ISRT-WLSAPA01                                          
073300     END-PERFORM                                                          
073400     .                                                                    
073500     EJECT                                                                
073600                                                                          
073700 DD-MEDDELANDE-EKO-WDR8-WDR9 SECTION.                                     
073800     MOVE SPACE                      TO EKO-W51080                        
073900     MOVE SPACE                      TO WS-SAP-MM-POST                    
074000                                                                          
074100     MOVE MID-IDARTNR (MID-IX)       TO EKO-IDARTNR                       
074200     MOVE MID-IDDC(MID-IX)           TO EKO-IDDC                          
074300     MOVE MID-IDAVINR (MID-IX)       TO EKO-IDFS                          
074400*                                                                         
074500     MOVE MID-IDLEVNR (MID-IX)       TO LEV04-IDLEVNR                     
074600     IF LEV04-REFNR                                                       
074700       MOVE MID-IDSUPREF (MID-IX) (3:8)                                   
074800                                     TO EKO-IDFS                          
074900     END-IF                                                               
075000*                                                                         
075100     IF MID-IDLEVNR (MID-IX) = '1441'                                     
075200       MOVE NEJ                      TO WS-SAP-MM-POST                    
075300     END-IF                                                               
075400*                                                                         
075500     MOVE ARTC11-CLAG-IDINK          TO EKO-IDINK                         
075600     MOVE MID-IDKONTO (MID-IX)       TO EKO-IDKONTO                       
075700     MOVE MID-IDLEVNR (MID-IX)       TO EKO-IDLEVNR                       
075800     MOVE WS-IDLOPNRM                TO EKO-IDLOPNRM                      
075900     MOVE SPAR-ARTC01-KDPROSL        TO EKO-KDPRODSL                      
076000     MOVE MID-KDRT (MID-IX)          TO EKO-KDRT                          
076100     MOVE SPAR-ARTC01-KDSORT         TO EKO-KDSORT                        
076200     MOVE ARTC11-CLAG-KDTIPPR        TO EKO-KDTIPPR                       
076300     IF SPAR-ARTC21-PRARTBEL-PR > ZERO                                    
076400        MOVE SPAR-ARTC21-PRARTBEL-PR TO EKO-PRARTBEL-PR                   
076500        MOVE SPAR-ARTC21-KDVALISO    TO EKO-KDVALISO                      
076600     ELSE                                                                 
076700*  -- OBS! RADPRIS SAKNAS, KDVALISO=XXX SIGNALERAR DETTA FÖR LEVA1        
076800        MOVE 0.1                     TO EKO-PRARTBEL-PR                   
076900        MOVE 'XXX'                   TO EKO-KDVALISO                      
077000     END-IF                                                               
077100     MOVE MID-KVAVIS (MID-IX)        TO EKO-KVAVIS                        
077200     IF SPAR-ARTC21-PRARTBES-PR > ZERO                                    
077300       MOVE SPAR-ARTC21-PRARTBES-PR  TO EKO-PRARTBES                      
077400     ELSE                                                                 
077500       MOVE 0.1                      TO EKO-PRARTBES                      
077600     END-IF                                                               
077700     MOVE ARTC11-CLAG-PRINK          TO EKO-PRINK                         
077800     MOVE ZERO                       TO EKO-RETULF                        
077900     MOVE MID-TIAVIDAT (MID-IX)      TO EKO-TIAVIDAT                      
078000     MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                      
078100     MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                       
078200     MOVE NEJ                        TO EKO-FLLSBOK                       
078300     MOVE MID-IDDISTR(MID-IX)        TO EKO-IDDISTR                       
078400     MOVE JA                         TO EKO-FLDIRLEV                      
078500     MOVE NEJ                        TO EKO-FLAVVINL                      
078600     MOVE ' '                        TO EKO-KDINLAVV                      
078700     MOVE ARTC11-CLAG-PRHEMTAG       TO EKO-PRHEMTAG                      
078800     MOVE WS-ARTC23-IDAVTAL          TO EKO-IDAVTAL                       
078900     MOVE 57                         TO EKO-IDFTG                         
079000                                                                          
079100                                                                          
079200     MOVE 'W6011B00'                 TO EKO-FIL-IDPGM                     
079300     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
079400     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
079500     MOVE 1                          TO EKO-FIL-IDSEKVNR                  
079600     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
079700     MOVE '80 '                      TO EKO-FIL-CT-IDPTYP                 
079800     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
079900                                                                          
080000     IF XDC-NON-VCC-OWNED OR LDC-CN                                       
080100       CONTINUE                                                           
080200     ELSE                                                                 
080300       IF WS-SAP-MM-POST NOT = NEJ                                        
080400         PERFORM IMS-ISRT-WLFILB01                                        
080500                                                                          
080600         PERFORM UNTIL SEGMENT-FINNS                                      
080700           ADD +1 TO EKO-FIL-IDSEKVNR                                     
080800           PERFORM IMS-ISRT-WLFILB01                                      
080900         END-PERFORM                                                      
081000       END-IF                                                             
081100     END-IF                                                               
081200                                                                          
081300     MOVE MID-IDDC(MID-IX)              TO WS-IDDC                        
081400     IF XDC-NON-VCC-OWNED OR LDC-CN                                       
081500       MOVE 'W6011B00'                  TO EKO-FIL-IDPGM                  
081600       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
081700       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
081800       MOVE FUNCTION CURRENT-DATE(1:2)  TO EKO-EKH-DAVERDAT(1:2)          
081900       MOVE MID-TIAVIDAT (MID-IX)       TO EKO-EKH-DAVERDAT(3:6)          
082000       MOVE +1                          TO EKO-FIL-IDSEKVNR               
082100       MOVE WS-IDDC                     TO W-IDDC-B6                      
082200       PERFORM IMS-GU-WDB601                                              
082300       IF NDC-CN                                                          
082400         MOVE 'W570'                    TO EKO-FIL-IDCPYTXT(1:4)          
082500       ELSE                                                               
082600         IF NDC-IN                                                        
082700           MOVE 'W515'                  TO EKO-FIL-IDCPYTXT(1:4)          
082800         ELSE                                                             
082900           MOVE DCS-KDTRADP             TO EKO-FIL-IDCPYTXT(1:4)          
083000         END-IF                                                           
083100       END-IF                                                             
083200       MOVE 'EKHA'                      TO EKO-FIL-IDCPYTXT(5:4)          
083300       PERFORM DDA-MEDDELANDE-EKO-WDR8-DET                                
083400**** HEMT SHOULD ONLY BE USED WHEN LOCAL SOURCING                         
083500       IF NDC-CN OR LDC-CN                                                
083600         PERFORM DDA-MEDDELANDE-EKO-WDR8-HEMT                             
083700       END-IF                                                             
083800       IF ARTC11-CLAG-PRINK = ARTC11-CLAG-PRARTSTD                        
083900         CONTINUE                                                         
084000       ELSE                                                               
084100         MOVE WS-IDDC         TO W-IDDC-B6                                
084200         PERFORM IMS-GU-WDB601                                            
084300         MOVE DCS-IDLANDX2 TO W-IDLANDX2                                  
084400         IF SEGMENT-FINNS                                                 
084500           PERFORM IMS-GNP-WDB617                                         
084600           IF SEGMENT-FINNS                                               
084700             PERFORM DDA-MEDDELANDE-EKO-WDR8-KALK                         
084800           END-IF                                                         
084900         END-IF                                                           
085000       END-IF                                                             
085100       PERFORM DDA-MEDDELANDE-EKO-WDR8-SUM                                
085200     ELSE                                                                 
085300       MOVE 'W6011B00'                  TO FIL-IDPGM                      
085400       MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                   
085500       MOVE FUNCTION CURRENT-DATE(1:2)  TO EKH-DAVERDAT(1:2)              
085600       MOVE MID-TIAVIDAT (MID-IX)       TO EKH-DAVERDAT(3:6)              
085700       MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                    
085800       MOVE +1                          TO FIL-IDSEKVNR                   
085900       MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                   
086000       MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                     
086100       PERFORM DDB-MEDDELANDE-EKO-WDR9                                    
086200     END-IF                                                               
086300     .                                                                    
086400     EJECT                                                                
086500                                                                          
086600 DDA-MEDDELANDE-EKO-WDR8-DET SECTION.                                     
086700     MOVE MID-IDARTNR(MID-IX)         TO W-WDK701-IDARTNR                 
086800     MOVE MID-IDDC(MID-IX)            TO W-WDK711-IDDC                    
086900     PERFORM IMS-GU-WDK711                                                
087000                                                                          
087100     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
087200     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
087300     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
087400     MOVE MID-IDDC(MID-IX)            TO EKO-EKH-IDDC-SEND                
087500     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
087600     MOVE +0                          TO EKO-EKH-IDDISTR                  
087700     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
087800     MOVE SPACE                       TO EKO-EKH-IDVERGL                  
087900*******************************                                           
088000*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
088100     MOVE ZERO TO NOLL-RAKNARE                                            
088200     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
088300     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
088400          FOR LEADING ZERO                                                
088500     ADD +1 TO NOLL-RAKNARE                                               
088600     UNSTRING WS-SAP-X-IDAVINR     INTO EKO-EKH-IDVERGL                   
088700          WITH POINTER NOLL-RAKNARE                                       
088800*******************************                                           
088900     MOVE SPAR-ARTC01-KDPROSL         TO EKO-EKH-KDPRODSL                 
089000     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
089100                                         EKO-EKH-PRARTNTO                 
089200                                         EKO-EKH-PRARTSJK                 
089300                                         EKO-EKH-PRLANDCO                 
089400                                         EKO-EKH-SUBEL                    
089500                                         EKO-EKH-KDFRAKT                  
089600     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
089700                                         EKO-EKH-PRDMTRL                  
089800                                         EKO-EKH-PROVRPAL                 
089900     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
090000     MOVE 'N'                         TO EKO-EKH-FLLSBOK                  
090100     MOVE ZERO                        TO EKO-EKH-PRINK                    
090200     IF NDC-CN                                                            
090300       PERFORM DDAA-GET-PRARTBEL                                          
090400     END-IF                                                               
090500     PERFORM DDAB-GET-CURRENCY-RATE                                       
090600                                                                          
090700*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
090800*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
090900     IF XDC-NON-VCC-OWNED                                                 
091000     AND NOT (NDC-CN OR NDC-US)                                           
091100       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
091200     END-IF                                                               
091300***                                                                       
091400     IF SPAR-ARTC21-PRARTBEL-PR > ZERO                                    
091500       MOVE SPAR-ARTC21-PRARTBEL-PR   TO EKO-EKH-PRARTSTD                 
091600       MOVE SPAR-ARTC21-KDVALISO      TO EKO-EKH-KDVALISO                 
091700       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
091800     ELSE                                                                 
091900       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
092000       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
092100       MOVE 1.00                      TO EKO-EKH-PRKURS                   
092200     END-IF                                                               
092300     COMPUTE WS-SUARTSTD = SPAR-ARTC21-PRARTBEL-SUM *                     
092400                           MID-KVAVIS(MID-IX)                             
092500     MOVE MID-KVAVIS(MID-IX)          TO EKO-EKH-KVANTAL                  
092600     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
092700     MOVE SPACE                       TO EKO-EKH-BEVAT                    
092800                                         EKO-EKH-IDANALYS                 
092900                                         EKO-EKH-KDANMORS                 
093000                                         EKO-EKH-IDKST                    
093100     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
093200                                         EKO-EKH-SUVAT                    
093300                                         EKO-EKH-PRHEMTAG                 
093400     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
093500     MOVE MID-IDAVINR (MID-IX)        TO EKO-EKH-IDAVINR                  
093600     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
093700     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
093800     MOVE MID-KDRT (MID-IX)           TO EKO-EKH-KDRT                     
093900     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
094000     MOVE MID-KVAVIS(MID-IX)          TO EKO-EKH-KVAVIS                   
094100     MOVE SPAR-ARTC01-KDSORT          TO EKO-EKH-KDSORT                   
094200     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
094300     MOVE SPACE                       TO EKO-EKH-FLDCET                   
094400     MOVE MID-IDKUNDRF(MID-IX)        TO EKO-EKH-IDKUNDRF                 
094500                                                                          
094600     PERFORM IMS-ISRT-WLFILB01                                            
094700                                                                          
094800     PERFORM UNTIL SEGMENT-FINNS                                          
094900       ADD +1 TO EKO-FIL-IDSEKVNR                                         
095000       PERFORM IMS-ISRT-WLFILB01                                          
095100     END-PERFORM                                                          
095200     .                                                                    
095300     EJECT                                                                
095400                                                                          
095500 DDA-MEDDELANDE-EKO-WDR8-HEMT SECTION.                                    
095600     MOVE MID-IDARTNR(MID-IX)         TO W-WDK701-IDARTNR                 
095700     MOVE MID-IDDC(MID-IX)            TO W-WDK711-IDDC                    
095800     PERFORM IMS-GU-WDK711                                                
095900                                                                          
096000     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
096100     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
096200     MOVE 'HEMT '                     TO EKO-EKH-KDEKNIVA                 
096300     MOVE MID-IDDC(MID-IX)            TO EKO-EKH-IDDC-SEND                
096400     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
096500     MOVE +0                          TO EKO-EKH-IDDISTR                  
096600     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
096700     MOVE SPACE                       TO EKO-EKH-IDVERGL                  
096800*******************************                                           
096900*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
097000     MOVE ZERO TO NOLL-RAKNARE                                            
097100     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
097200     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
097300          FOR LEADING ZERO                                                
097400     ADD +1 TO NOLL-RAKNARE                                               
097500     UNSTRING WS-SAP-X-IDAVINR     INTO EKO-EKH-IDVERGL                   
097600          WITH POINTER NOLL-RAKNARE                                       
097700*******************************                                           
097800     MOVE SPAR-ARTC01-KDPROSL         TO EKO-EKH-KDPRODSL                 
097900     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
098000                                         EKO-EKH-PRARTNTO                 
098100                                         EKO-EKH-PRARTSJK                 
098200                                         EKO-EKH-PRLANDCO                 
098300                                         EKO-EKH-SUBEL                    
098400                                         EKO-EKH-KDFRAKT                  
098500     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
098600                                         EKO-EKH-PRDMTRL                  
098700                                         EKO-EKH-PROVRPAL                 
098800     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
098900     MOVE 'N'                         TO EKO-EKH-FLLSBOK                  
099000     MOVE ZERO                        TO EKO-EKH-PRINK                    
099100     IF NDC-CN                                                            
099200       PERFORM DDAA-GET-PRARTBEL                                          
099300     END-IF                                                               
099400     PERFORM DDAB-GET-CURRENCY-RATE                                       
099500                                                                          
099600*** SUPPLIER PRICE ROW SHOULD BE CALCULATED TO LOCAL CURRENCY             
099700*** FOR LOCAL PARTS FOR NON-VCC EXCEPT CHINA AND US                       
099800     IF XDC-NON-VCC-OWNED                                                 
099900     AND NOT (NDC-CN OR NDC-US)                                           
100000       PERFORM DDAC-GET-CURR-RATE-LOCAL                                   
100100     END-IF                                                               
100200***                                                                       
100300     IF SPAR-ARTC21-PRARTBEL-PR > ZERO                                    
100400       MOVE SPAR-ARTC21-PRARTBEL-PR   TO EKO-EKH-PRARTSTD                 
100500       MOVE SPAR-ARTC21-KDVALISO      TO EKO-EKH-KDVALISO                 
100600       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
100700     ELSE                                                                 
100800       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
100900       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
101000       MOVE 1.00                      TO EKO-EKH-PRKURS                   
101100     END-IF                                                               
101200******                                                                    
101300     MOVE MID-IDLEVNR (MID-IX)  TO W-IDLEVNR                              
101400     PERFORM IMS-GU-WDF101                                                
101500     IF SEGMENT-SAKNAS                                                    
101600       MOVE ZERO TO W-RETULF                                              
101700     ELSE                                                                 
101800       MOVE WS-IDDC          TO W-IDDC-B6                                 
101900       PERFORM IMS-GU-WDB601                                              
102000       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
102100       PERFORM IMS-GNP-WDF102                                             
102200       IF SEGMENT-FINNS                                                   
102300         IF F102-TULL-TITULF < WS-DAGENS-DATUM                            
102400           MOVE F102-TULL-RETULF-1  TO W-RETULF                           
102500         ELSE                                                             
102600           MOVE F102-TULL-RETULF-2  TO W-RETULF                           
102700         END-IF                                                           
102800       END-IF                                                             
102900     END-IF                                                               
103000******                                                                    
103100     COMPUTE EKO-EKH-PRHEMTAG ROUNDED = (W-RETULF - 1) *                  
103200                EKO-EKH-PRARTSTD * MID-KVAVIS(MID-IX)                     
103300     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
103400     MOVE EKO-EKH-PRHEMTAG            TO EKO-EKH-SUBEL                    
103500     MOVE EKO-EKH-PRHEMTAG            TO WS-SUHEMT                        
103600     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
103700     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
103800     MOVE SPACE                       TO EKO-EKH-BEVAT                    
103900                                         EKO-EKH-IDANALYS                 
104000                                         EKO-EKH-KDANMORS                 
104100                                         EKO-EKH-IDKST                    
104200     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
104300                                         EKO-EKH-SUVAT                    
104400     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
104500     MOVE MID-IDAVINR (MID-IX)        TO EKO-EKH-IDAVINR                  
104600     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
104700     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
104800     MOVE MID-KDRT (MID-IX)           TO EKO-EKH-KDRT                     
104900     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
105000     MOVE ZERO                        TO EKO-EKH-KVAVIS                   
105100     MOVE SPAR-ARTC01-KDSORT          TO EKO-EKH-KDSORT                   
105200     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
105300     MOVE SPACE                       TO EKO-EKH-FLDCET                   
105400     MOVE MID-IDKUNDRF(MID-IX)        TO EKO-EKH-IDKUNDRF                 
105500                                                                          
105600     IF EKO-EKH-PRHEMTAG > ZERO                                           
105700       PERFORM IMS-ISRT-WLFILB01                                          
105800                                                                          
105900       PERFORM UNTIL SEGMENT-FINNS                                        
106000         ADD +1 TO EKO-FIL-IDSEKVNR                                       
106100         PERFORM IMS-ISRT-WLFILB01                                        
106200       END-PERFORM                                                        
106300     END-IF                                                               
106400     .                                                                    
106500     EJECT                                                                
106600                                                                          
106700 DDA-MEDDELANDE-EKO-WDR8-KALK SECTION.                                    
106800     MOVE MID-IDARTNR(MID-IX)         TO W-WDK701-IDARTNR                 
106900     MOVE MID-IDDC(MID-IX)            TO W-WDK711-IDDC                    
107000     PERFORM IMS-GU-WDK711                                                
107100                                                                          
107200     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
107300     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
107400     MOVE 'KALK '                     TO EKO-EKH-KDEKNIVA                 
107500     MOVE MID-IDDC(MID-IX)            TO EKO-EKH-IDDC-SEND                
107600     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
107700     MOVE +0                          TO EKO-EKH-IDDISTR                  
107800     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
107900     MOVE SPACE                       TO EKO-EKH-IDVERGL                  
108000*******************************                                           
108100*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
108200     MOVE ZERO TO NOLL-RAKNARE                                            
108300     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
108400     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
108500          FOR LEADING ZERO                                                
108600     ADD +1 TO NOLL-RAKNARE                                               
108700     UNSTRING WS-SAP-X-IDAVINR     INTO EKO-EKH-IDVERGL                   
108800          WITH POINTER NOLL-RAKNARE                                       
108900*******************************                                           
109000     MOVE SPAR-ARTC01-KDPROSL         TO EKO-EKH-KDPRODSL                 
109100     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
109200                                         EKO-EKH-PRARTNTO                 
109300                                         EKO-EKH-PRARTSJK                 
109400                                         EKO-EKH-PRLANDCO                 
109500                                         EKO-EKH-SUBEL                    
109600                                         EKO-EKH-KDFRAKT                  
109700     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
109800     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
109900     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
110000                                         EKO-EKH-PRDMTRL                  
110100                                         EKO-EKH-PROVRPAL                 
110200*    PERFORM DDAB-GET-CURRENCY-RATE                                       
110300**** KALKYLPÅLÄGG PROCENT FRÅN SVENSKA KALKYLPÅLÄGGET                     
110400**** OMRÄKNAT TILL CNY                                                    
110500     MOVE WS-DAGENS-DATUM(3:2) TO W-DATE-AAMM(1:2)                        
110600     MOVE 01                   TO W-DATE-AAMM(3:2)                        
110700     MOVE 'SEK'                TO CURR-KDVALISO-HUV                       
110800     MOVE DCS-KDVALISO         TO CURR-KDVALISO-ROW                       
110900     MOVE W-DATE-AAMM          TO CURR-TIAAMM                             
111000     MOVE 'A'                  TO CURR-KDVALTYP                           
111100     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
111200     IF CURR-KDSVAR = ' '                                                 
111300       MOVE CURR-PRKURS-NEW    TO W-PRKURS                                
111400       MOVE CURR-REVALUTA-TO   TO W-REVALUTA                              
111500     ELSE                                                                 
111600       MOVE 1                  TO W-PRKURS                                
111700       MOVE 1                  TO W-REVALUTA                              
111800     END-IF                                                               
111900     COMPUTE EKO-EKH-PRDIRLON ROUNDED = ARTC11-CLAG-PRDIRLON *            
112000      PROC-REDIRLON /  W-PRKURS / W-REVALUTA * MID-KVAVIS(MID-IX)         
112100     COMPUTE EKO-EKH-PRDMTRL  ROUNDED = ARTC11-CLAG-PRDMTRL  *            
112200      PROC-REDMTRL  /  W-PRKURS / W-REVALUTA * MID-KVAVIS(MID-IX)         
112300     MOVE EKO-EKH-PRDMTRL             TO WS-SUDIRMTRL                     
112400     MOVE EKO-EKH-PRDIRLON            TO WS-SUDIRLON                      
112500     COMPUTE EKO-EKH-SUBEL = EKO-EKH-PRDIRLON +                           
112600                             EKO-EKH-PRDMTRL                              
112700     MOVE ZERO                        TO EKO-EKH-PROVRPAL                 
112800     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
112900     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
113000     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
113100     MOVE 'N'                         TO EKO-EKH-FLLSBOK                  
113200     MOVE 1.00                        TO EKO-EKH-PRKURS                   
113300     MOVE ZERO                        TO EKO-EKH-PRINK                    
113400     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
113500     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
113600     MOVE SPACE                       TO EKO-EKH-BEVAT                    
113700                                         EKO-EKH-IDANALYS                 
113800                                         EKO-EKH-KDANMORS                 
113900                                         EKO-EKH-IDKST                    
114000     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
114100                                         EKO-EKH-SUVAT                    
114200     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
114300     MOVE MID-IDAVINR (MID-IX)        TO EKO-EKH-IDAVINR                  
114400     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
114500     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
114600     MOVE MID-KDRT (MID-IX)           TO EKO-EKH-KDRT                     
114700     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
114800     MOVE ZERO                        TO EKO-EKH-KVAVIS                   
114900     MOVE SPAR-ARTC01-KDSORT          TO EKO-EKH-KDSORT                   
115000     MOVE SPACE                       TO EKO-EKH-FLDCET                   
115100     MOVE MID-IDKUNDRF(MID-IX)        TO EKO-EKH-IDKUNDRF                 
115200     MOVE DCS-KDVALISO                TO EKO-EKH-KDVALISO                 
115300     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
115400                                                                          
115500     IF EKO-EKH-SUBEL > ZERO                                              
115600       PERFORM IMS-ISRT-WLFILB01                                          
115700                                                                          
115800       PERFORM UNTIL SEGMENT-FINNS                                        
115900         ADD +1 TO EKO-FIL-IDSEKVNR                                       
116000         PERFORM IMS-ISRT-WLFILB01                                        
116100       END-PERFORM                                                        
116200     END-IF                                                               
116300     .                                                                    
116400     EJECT                                                                
116500                                                                          
116600 DDA-MEDDELANDE-EKO-WDR8-SUM SECTION.                                     
116700     MOVE MID-IDARTNR(MID-IX)         TO W-WDK701-IDARTNR                 
116800     MOVE MID-IDDC(MID-IX)            TO W-WDK711-IDDC                    
116900     PERFORM IMS-GU-WDK711                                                
117000                                                                          
117100     MOVE '103'                       TO EKO-EKH-KDEKHHT                  
117200     MOVE '102'                       TO EKO-EKH-KDEKSHT                  
117300     MOVE 'SUM  '                     TO EKO-EKH-KDEKNIVA                 
117400     MOVE MID-IDDC(MID-IX)            TO EKO-EKH-IDDC-SEND                
117500     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
117600     MOVE +0                          TO EKO-EKH-IDDISTR                  
117700     MOVE +0                          TO EKO-EKH-IDKUNDNR                 
117800     MOVE SPACE                       TO EKO-EKH-IDVERGL                  
117900*******************************                                           
118000*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
118100     MOVE ZERO TO NOLL-RAKNARE                                            
118200     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
118300     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
118400          FOR LEADING ZERO                                                
118500     ADD +1 TO NOLL-RAKNARE                                               
118600     UNSTRING WS-SAP-X-IDAVINR     INTO EKO-EKH-IDVERGL                   
118700          WITH POINTER NOLL-RAKNARE                                       
118800*******************************                                           
118900     MOVE SPAR-ARTC01-KDPROSL         TO EKO-EKH-KDPRODSL                 
119000     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
119100                                         EKO-EKH-PRARTNTO                 
119200                                         EKO-EKH-PRARTSJK                 
119300                                         EKO-EKH-PRLANDCO                 
119400                                         EKO-EKH-SUBEL                    
119500                                         EKO-EKH-KDFRAKT                  
119600     MOVE ZERO                        TO EKO-EKH-PRHEMTAG                 
119700     MOVE ZERO                        TO EKO-EKH-PRDIRLON                 
119800                                         EKO-EKH-PRDMTRL                  
119900                                         EKO-EKH-PROVRPAL                 
120000     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
120100     MOVE 'N'                         TO EKO-EKH-FLLSBOK                  
120200     MOVE ZERO                        TO EKO-EKH-PRINK                    
120300     MOVE ZERO                        TO EKO-EKH-PRARTSTD                 
120400     MOVE ZERO                        TO EKO-EKH-KVANTAL                  
120500     MOVE W-IDTRANS                   TO EKO-EKH-IDTRANS                  
120600     MOVE SPACE                       TO EKO-EKH-BEVAT                    
120700                                         EKO-EKH-IDANALYS                 
120800                                         EKO-EKH-KDANMORS                 
120900                                         EKO-EKH-IDKST                    
121000     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
121100                                         EKO-EKH-SUVAT                    
121200*    COMPUTE EKO-EKH-SUBEL = WS-SUDIRLON + WS-SUDIRMTRL +                 
121300*                            WS-SUHEMT + WS-SUARTSTD                      
121400     COMPUTE EKO-EKH-SUBEL = WS-SUARTSTD                                  
121500*    MOVE 'CNY'                       TO EKO-EKH-KDVALISO                 
121600*    MOVE 1.00                        TO EKO-EKH-PRKURS                   
121700     MOVE SPAR-ARTC21-KDVALISO        TO EKO-EKH-KDVALISO                 
121800     MOVE SPAR-PRKURS                 TO EKO-EKH-PRKURS                   
121900     MOVE WS-DAAVIDAT                 TO EKO-EKH-DAAVIDAT                 
122000     MOVE MID-IDAVINR (MID-IX)        TO EKO-EKH-IDAVINR                  
122100     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
122200     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
122300     MOVE MID-KDRT (MID-IX)           TO EKO-EKH-KDRT                     
122400     MOVE ZERO                        TO EKO-EKH-KVANTMOT                 
122500     MOVE ZERO                        TO EKO-EKH-KVAVIS                   
122600     MOVE SPAR-ARTC01-KDSORT          TO EKO-EKH-KDSORT                   
122700     MOVE DCS-KDTRADP                 TO EKO-EKH-KDTRADP                  
122800                                                                          
122900     MOVE SPACE                       TO EKO-EKH-FLDCET                   
123000     MOVE MID-IDKUNDRF(MID-IX)        TO EKO-EKH-IDKUNDRF                 
123100                                                                          
123200     PERFORM IMS-ISRT-WLFILB01                                            
123300                                                                          
123400     PERFORM UNTIL SEGMENT-FINNS                                          
123500       ADD +1 TO EKO-FIL-IDSEKVNR                                         
123600       PERFORM IMS-ISRT-WLFILB01                                          
123700     END-PERFORM                                                          
123800     .                                                                    
123900     EJECT                                                                
124000                                                                          
124100 DDAA-GET-PRARTBEL SECTION.                                               
124200     MOVE +0                    TO SPAR-ARTC21-PRARTBEL-PR                
124300                                   SPAR-ARTC21-PRARTBEL-SUM               
124400                                   SPAR-ARTC21-PRARTBES-PR                
124500                                                                          
124600*    -- WDK711                                                            
124700     PERFORM IMS-GHU-WDK711-K7                                            
124800     IF SEGMENT-FINNS                                                     
124900                                                                          
125000*    -- WDK724                                                            
125100       MOVE MID-IDLEVNR(MID-IX) TO W-IDLEVNR-PR                           
125200       COMPUTE W-DAPRLIST-K7 = 99999999 - WS-DAAVIDAT                     
125300       PERFORM IMS-GHNP-WDK724-K7                                         
125400                                                                          
125500       IF SEGMENT-FINNS                                                   
125600         MOVE SPRL-PRARTBEL-PR  TO SPAR-ARTC21-PRARTBEL-PR                
125700         MOVE SPRL-PRARTBEL-PR  TO SPAR-ARTC21-PRARTBEL-SUM               
125800         MOVE SPRL-PRARTBES-PR  TO SPAR-ARTC21-PRARTBES-PR                
125900         MOVE SPRL-KDVALISO     TO SPAR-ARTC21-KDVALISO                   
126000       END-IF                                                             
126100     END-IF                                                               
126200     .                                                                    
126300     EJECT                                                                
126400                                                                          
126500 DDAB-GET-CURRENCY-RATE SECTION.                                          
126600     MOVE WS-IDDC         TO W-IDDC-B6                                    
126700     PERFORM IMS-GU-WDB601                                                
126800     MOVE DCS-KDVALISO      TO W-KDVALISO-HUV                             
126900     IF W-KDVALISO-HUV = SPAR-ARTC21-KDVALISO                             
127000       MOVE 1 TO SPAR-PRKURS                                              
127100       MOVE 1 TO W-REVALUTA                                               
127200     ELSE                                                                 
127300       MOVE SPAR-ARTC21-KDVALISO TO W-KDVALISO-ROW                        
127400       PERFORM IMS-GU-WDGX9306                                            
127500       IF SEGMENT-SAKNAS                                                  
127600         MOVE 1               TO SPAR-PRKURS                              
127700         MOVE 1               TO W-REVALUTA                               
127800       ELSE                                                               
127900         COMPUTE W-TISTADAT-9KOMPL =                                      
128000                 9999999 - WS-DATE-YYMMDD                                 
128100         PERFORM IMS-GNP-WDGX9308                                         
128200         IF SEGMENT-SAKNAS                                                
128300           PERFORM IMS-GNP-WDGX9308-FIRST                                 
128400           IF SEGMENT-SAKNAS                                              
128500             MOVE 1               TO SPAR-PRKURS                          
128600             MOVE 1               TO W-REVALUTA                           
128700           ELSE                                                           
128800             MOVE 9308-PRKURS   TO SPAR-PRKURS                            
128900             MOVE 9308-REVALUTA-TO TO W-REVALUTA                          
129000           END-IF                                                         
129100         ELSE                                                             
129200           MOVE 9308-PRKURS   TO SPAR-PRKURS                              
129300           MOVE 9308-REVALUTA-TO TO W-REVALUTA                            
129400         END-IF                                                           
129500       END-IF                                                             
129600     END-IF                                                               
129700     .                                                                    
129800     EJECT                                                                
129900                                                                          
130000 DDAC-GET-CURR-RATE-LOCAL SECTION.                                        
130100                                                                          
130200     IF W-KDVALISO-HUV = SPAR-ARTC21-KDVALISO                             
130300       CONTINUE                                                           
130400     ELSE                                                                 
130500       MOVE SPAR-ARTC01-KDPROSL        TO TEST-KDPRODSL                   
130600       IF KDPRODSL-LOCAL                                                  
130700         MOVE MID-TIAVIDAT(MID-IX)     TO WS-DAAVIDAT                     
130800         MOVE WS-DAAVIDAT(1:2)         TO W-DATE-AAMM(1:2)                
130900         MOVE WS-DAAVIDAT(3:2)         TO W-DATE-AAMM(3:2)                
131000         MOVE W-KDVALISO-HUV           TO CURR-KDVALISO-HUV               
131100         MOVE SPAR-ARTC21-KDVALISO     TO CURR-KDVALISO-ROW               
131200         MOVE W-DATE-AAMM              TO CURR-TIAAMM                     
131300         MOVE 'M'                      TO CURR-KDVALTYP                   
131400         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
131500         IF CURR-KDSVAR = ' '                                             
131600           MOVE CURR-PRKURS-NEW        TO SPAR-PRKURS                     
131700           MOVE CURR-REVALUTA-TO       TO W-REVALUTA                      
131800         ELSE                                                             
131900           MOVE 1                      TO SPAR-PRKURS                     
132000           MOVE 1                      TO W-REVALUTA                      
132100         END-IF                                                           
132200**** PRICE IN SUPPLIER CURRENCY SHOULD BE CALCULATED                      
132300**** TO COUNTRY CURRENCY                                                  
132400         COMPUTE SPAR-ARTC21-PRARTBEL-PR =                                
132500         SPAR-ARTC21-PRARTBEL-PR * SPAR-PRKURS / W-REVALUTA               
132600       END-IF                                                             
132700     END-IF                                                               
132800     .                                                                    
132900     EJECT                                                                
133000 DDB-MEDDELANDE-EKO-WDR9 SECTION.                                         
133100     MOVE '103'                       TO EKH-KDEKHHT                      
133200     MOVE '102'                       TO EKH-KDEKSHT                      
133300     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
133400     MOVE MID-IDDC(MID-IX)            TO EKH-IDDC-SEND                    
133500     MOVE SPACE                       TO EKH-IDDC-REC                     
133600     MOVE +0                          TO EKH-IDDISTR                      
133700     MOVE +0                          TO EKH-IDKUNDNR                     
133800     MOVE SPACE                       TO EKH-IDVERGL                      
133900*******************************                                           
134000*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
134100     MOVE ZERO TO NOLL-RAKNARE                                            
134200     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
134300     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
134400          FOR LEADING ZERO                                                
134500     ADD +1 TO NOLL-RAKNARE                                               
134600     UNSTRING WS-SAP-X-IDAVINR     INTO EKH-IDVERGL                       
134700          WITH POINTER NOLL-RAKNARE                                       
134800*******************************                                           
134900     MOVE SPAR-ARTC01-KDPROSL         TO EKH-KDPRODSL                     
135000     MOVE ZERO                        TO EKH-KDPSLLOC                     
135100                                         EKH-PRARTNTO                     
135200                                         EKH-PRARTSJK                     
135300                                         EKH-PRLANDCO                     
135400                                         EKH-SUBEL                        
135500                                         EKH-KDFRAKT                      
135600     IF MID-IDLEVNR(MID-IX) = '1002 '                                     
135700       MOVE ZERO                      TO EKH-PRHEMTAG                     
135800     ELSE                                                                 
135900       MOVE ARTC11-CLAG-PRHEMTAG      TO EKH-PRHEMTAG                     
136000     END-IF                                                               
136100     MOVE ZERO                        TO EKH-PRDIRLON                     
136200                                         EKH-PRDMTRL                      
136300                                         EKH-PROVRPAL                     
136400     MOVE MID-IDARTNR(MID-IX)         TO EKH-IDARTNR                      
136500     MOVE 'N'                         TO EKH-FLLSBOK                      
136600     IF EKO-KDVALISO = 'XXX'                                              
136700       MOVE 'SEK'                     TO EKH-KDVALISO                     
136800     ELSE                                                                 
136900       MOVE EKO-KDVALISO              TO EKH-KDVALISO                     
137000     END-IF                                                               
137100     MOVE 1.00                        TO EKH-PRKURS                       
137200     MOVE ARTC11-CLAG-PRINK           TO EKH-PRINK                        
137300     MOVE ARTC11-CLAG-PRARTSTD        TO EKH-PRARTSTD                     
137400     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVANTAL                      
137500     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
137600     MOVE SPACE                       TO EKH-BEVAT                        
137700                                         EKH-IDANALYS                     
137800                                         EKH-KDANMORS                     
137900                                         EKH-IDKST                        
138000     MOVE ZERO                        TO EKH-IDKONTO                      
138100                                         EKH-SUVAT                        
138200     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
138300     MOVE MID-IDAVINR (MID-IX)        TO EKH-IDAVINR                      
138400     MOVE MID-IDLEVNR (MID-IX)        TO EKH-IDLEVNR                      
138500     MOVE ZERO                        TO EKH-KDAVVTYP                     
138600     MOVE MID-KDRT (MID-IX)           TO EKH-KDRT                         
138700     MOVE ZERO                        TO EKH-KVANTMOT                     
138800     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVAVIS                       
138900     MOVE SPAR-ARTC01-KDSORT          TO EKH-KDSORT                       
139000     MOVE 'SEPV'                      TO EKH-KDTRADP                      
139100     MOVE SPACE                       TO EKH-FLDCET                       
139200     MOVE SPACE                       TO EKH-IDKUNDRF                     
139300     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
139400                                                                          
139500     PERFORM IMS-ISRT-WLSAPA01                                            
139600                                                                          
139700     PERFORM UNTIL SEGMENT-FINNS                                          
139800       ADD +1 TO FIL-IDSEKVNR                                             
139900       PERFORM IMS-ISRT-WLSAPA01                                          
140000     END-PERFORM                                                          
140100     .                                                                    
140200     EJECT                                                                
140300 DE-MEDDELANDE-330-221 SECTION.                                           
140400                                                                          
140500     MOVE ZERO                   TO W330-W211310                          
140600     MOVE '221'                  TO W330-IDTTYP                           
140700     MOVE '330'                  TO W330-IDPTYP                           
140800******                                                                    
140900       IF ARTC11-CLAG-IDINK (1:3) NUMERIC                                 
141000          MOVE ARTC11-CLAG-IDINK (1:3) TO W330-KDPKINR                    
141100       ELSE                                                               
141200          IF ARTC11-CLAG-IDINK (2:3) NUMERIC                              
141300             MOVE ARTC11-CLAG-IDINK (2:3) TO W330-KDPKINR                 
141400          ELSE                                                            
141500             MOVE ZERO TO W330-KDPKINR                                    
141600          END-IF                                                          
141700       END-IF                                                             
141800******                                                                    
141900     MOVE ARTC11-CLAG-IDANSK     TO W330-IDANSKNR                         
142000     MOVE ARTC11-CLAG-PRARTSTD   TO W330-PRARTSTD                         
142100     MOVE MID-IDARTNR (MID-IX)   TO W330-IDARTNR                          
142200                                    W330-IDARTNR-S                        
142300     MOVE +1                     TO W330-KDCLAGER                         
142400                                    W330-KDCLAGER-S                       
142500     MOVE 009                    TO W330-POSTLGD                          
142600     MOVE MID-KVAVIS (MID-IX)    TO W330-KVAVIS                           
142700                                    W330-KVMOTANT                         
142800     MOVE MID-IDLEVNR (MID-IX)   TO W330-IDLEVNR-INL                      
142900     MOVE MID-TIAVIDAT (MID-IX)  TO W330-TIAVSDAT                         
143000     MOVE MID-IDAVINR (MID-IX)   TO W330-IDAVINR                          
143100     MOVE MID-KDRT (MID-IX)      TO W330-KDRT                             
143200                                                                          
143300     MOVE W330-W211310           TO WS-ZZAC01-LOGGPOST                    
143400     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
143500     PERFORM S01-SKAPA-ZZAC01                                             
143600     .                                                                    
143700     EJECT                                                                
143800                                                                          
143900 DF-MEDDELANDE-WDR9 SECTION.                                              
144000     MOVE SPACE                      TO EKO-W51080                        
144100     MOVE SPACE                      TO WS-SAP-MM-POST                    
144200                                                                          
144300     MOVE MID-IDARTNR (MID-IX)       TO EKO-IDARTNR                       
144400     MOVE MID-IDDC(MID-IX)           TO EKO-IDDC                          
144500     MOVE MID-IDAVINR (MID-IX)       TO EKO-IDFS                          
144600*                                                                         
144700     MOVE MID-IDLEVNR (MID-IX)       TO LEV04-IDLEVNR                     
144800     IF LEV04-REFNR                                                       
144900       MOVE MID-IDSUPREF (MID-IX) (3:8)                                   
145000                                     TO EKO-IDFS                          
145100     END-IF                                                               
145200*                                                                         
145300     IF MID-IDLEVNR (MID-IX) = '1441'                                     
145400       MOVE NEJ                      TO WS-SAP-MM-POST                    
145500     END-IF                                                               
145600*                                                                         
145700     MOVE ARTC11-CLAG-IDINK          TO EKO-IDINK                         
145800     MOVE MID-IDKONTO (MID-IX)       TO EKO-IDKONTO                       
145900     MOVE MID-IDLEVNR (MID-IX)       TO EKO-IDLEVNR                       
146000     MOVE WS-IDLOPNRM                TO EKO-IDLOPNRM                      
146100     MOVE SPAR-ARTC01-KDPROSL        TO EKO-KDPRODSL                      
146200     MOVE MID-KDRT (MID-IX)          TO EKO-KDRT                          
146300     MOVE SPAR-ARTC01-KDSORT         TO EKO-KDSORT                        
146400     MOVE ARTC11-CLAG-KDTIPPR        TO EKO-KDTIPPR                       
146500     IF SPAR-ARTC21-PRARTBEL-PR > ZERO                                    
146600        MOVE SPAR-ARTC21-PRARTBEL-PR TO EKO-PRARTBEL-PR                   
146700        MOVE SPAR-ARTC21-KDVALISO    TO EKO-KDVALISO                      
146800     ELSE                                                                 
146900*  -- OBS! RADPRIS SAKNAS, KDVALISO=XXX SIGNALERAR DETTA FÖR LEVA1        
147000        MOVE 0.1                     TO EKO-PRARTBEL-PR                   
147100        MOVE 'XXX'                   TO EKO-KDVALISO                      
147200     END-IF                                                               
147300     MOVE MID-KVAVIS (MID-IX)        TO EKO-KVAVIS                        
147400     IF SPAR-ARTC21-PRARTBES-PR > ZERO                                    
147500       MOVE SPAR-ARTC21-PRARTBES-PR  TO EKO-PRARTBES                      
147600     ELSE                                                                 
147700       MOVE 0.1                      TO EKO-PRARTBES                      
147800     END-IF                                                               
147900     MOVE ARTC11-CLAG-PRINK          TO EKO-PRINK                         
148000     MOVE ZERO                       TO EKO-RETULF                        
148100     MOVE MID-TIAVIDAT (MID-IX)      TO EKO-TIAVIDAT                      
148200     MOVE FUNCTION CURRENT-DATE(1:8) TO EKO-DAREGDAT                      
148300     MOVE FUNCTION CURRENT-DATE(9:8) TO EKO-TIKLOCK                       
148400     MOVE NEJ                        TO EKO-FLLSBOK                       
148500     MOVE MID-IDDISTR(MID-IX)        TO EKO-IDDISTR                       
148600     MOVE JA                         TO EKO-FLDIRLEV                      
148700     MOVE NEJ                        TO EKO-FLAVVINL                      
148800     MOVE ' '                        TO EKO-KDINLAVV                      
148900     MOVE ARTC11-CLAG-PRHEMTAG       TO EKO-PRHEMTAG                      
149000     MOVE WS-ARTC23-IDAVTAL          TO EKO-IDAVTAL                       
149100     MOVE 57                         TO EKO-IDFTG                         
149200                                                                          
149300                                                                          
149400     MOVE 'W6011B00'                 TO EKO-FIL-IDPGM                     
149500     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
149600     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
149700     MOVE 1                          TO EKO-FIL-IDSEKVNR                  
149800     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
149900     MOVE '80 '                      TO EKO-FIL-CT-IDPTYP                 
150000     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
150100                                                                          
150200     IF XDC-NON-VCC-OWNED OR LDC-CN                                       
150300       CONTINUE                                                           
150400     ELSE                                                                 
150500       IF WS-SAP-MM-POST NOT = NEJ                                        
150600         PERFORM IMS-ISRT-WLFILB01                                        
150700                                                                          
150800         PERFORM UNTIL SEGMENT-FINNS                                      
150900           ADD +1 TO EKO-FIL-IDSEKVNR                                     
151000           PERFORM IMS-ISRT-WLFILB01                                      
151100         END-PERFORM                                                      
151200       END-IF                                                             
151300     END-IF                                                               
151400                                                                          
151500     MOVE MID-IDDC(MID-IX)              TO WS-IDDC                        
151600     MOVE 'W6011B00'                  TO FIL-IDPGM                        
151700     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
151800     MOVE FUNCTION CURRENT-DATE(1:2)  TO EKH-DAVERDAT(1:2)                
151900     MOVE MID-TIAVIDAT (MID-IX)       TO EKH-DAVERDAT(3:6)                
152000     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
152100     MOVE +1                          TO FIL-IDSEKVNR                     
152200     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
152300     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
152400     MOVE '102'                       TO EKH-KDEKHHT                      
152500     MOVE '110'                       TO EKH-KDEKSHT                      
152600     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
152700     MOVE MID-IDDC(MID-IX)            TO EKH-IDDC-SEND                    
152800     MOVE SPACE                       TO EKH-IDDC-REC                     
152900     MOVE +0                          TO EKH-IDDISTR                      
153000     MOVE +0                          TO EKH-IDKUNDNR                     
153100     MOVE SPACE                       TO EKH-IDVERGL                      
153200*******************************                                           
153300*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
153400     MOVE ZERO TO NOLL-RAKNARE                                            
153500     MOVE MID-IDAVINR(MID-IX)        TO WS-SAP-X-IDAVINR                  
153600     INSPECT WS-SAP-X-IDAVINR  TALLYING NOLL-RAKNARE                      
153700          FOR LEADING ZERO                                                
153800     ADD +1 TO NOLL-RAKNARE                                               
153900     UNSTRING WS-SAP-X-IDAVINR     INTO EKH-IDVERGL                       
154000          WITH POINTER NOLL-RAKNARE                                       
154100*******************************                                           
154200     MOVE SPAR-ARTC01-KDPROSL         TO EKH-KDPRODSL                     
154300     MOVE ZERO                        TO EKH-KDPSLLOC                     
154400                                         EKH-PRARTNTO                     
154500                                         EKH-PRARTSJK                     
154600                                         EKH-PRLANDCO                     
154700                                         EKH-SUBEL                        
154800                                         EKH-KDFRAKT                      
154900     IF MID-IDLEVNR(MID-IX) = '1002 '                                     
155000       MOVE ZERO                      TO EKH-PRHEMTAG                     
155100     ELSE                                                                 
155200       MOVE ARTC11-CLAG-PRHEMTAG      TO EKH-PRHEMTAG                     
155300     END-IF                                                               
155400     MOVE ZERO                        TO EKH-PRDIRLON                     
155500                                         EKH-PRDMTRL                      
155600                                         EKH-PROVRPAL                     
155700     MOVE MID-IDARTNR(MID-IX)         TO EKH-IDARTNR                      
155800     MOVE 'N'                         TO EKH-FLLSBOK                      
155900     IF EKO-KDVALISO = 'XXX'                                              
156000       MOVE 'SEK'                     TO EKH-KDVALISO                     
156100     ELSE                                                                 
156200       MOVE EKO-KDVALISO              TO EKH-KDVALISO                     
156300     END-IF                                                               
156400     MOVE 1.00                        TO EKH-PRKURS                       
156500     MOVE ARTC11-CLAG-PRINK           TO EKH-PRINK                        
156600     MOVE ARTC11-CLAG-PRARTSTD        TO EKH-PRARTSTD                     
156700     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVANTAL                      
156800     MOVE W-IDTRANS                   TO EKH-IDTRANS                      
156900     MOVE SPACE                       TO EKH-BEVAT                        
157000                                         EKH-IDANALYS                     
157100                                         EKH-KDANMORS                     
157200                                         EKH-IDKST                        
157300     MOVE ZERO                        TO EKH-IDKONTO                      
157400                                         EKH-SUVAT                        
157500     MOVE WS-DAAVIDAT                 TO EKH-DAAVIDAT                     
157600     MOVE MID-IDAVINR (MID-IX)        TO EKH-IDAVINR                      
157700     MOVE MID-IDLEVNR (MID-IX)        TO EKH-IDLEVNR                      
157800     MOVE ZERO                        TO EKH-KDAVVTYP                     
157900     MOVE MID-KDRT (MID-IX)           TO EKH-KDRT                         
158000     MOVE ZERO                        TO EKH-KVANTMOT                     
158100     MOVE MID-KVAVIS(MID-IX)          TO EKH-KVAVIS                       
158200     MOVE SPAR-ARTC01-KDSORT          TO EKH-KDSORT                       
158300     MOVE 'SEPV'                      TO EKH-KDTRADP                      
158400     MOVE SPACE                       TO EKH-FLDCET                       
158500     MOVE SPACE                       TO EKH-IDKUNDRF                     
158600     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
158700                                                                          
158800     IF XDC-NON-VCC-OWNED OR LDC-CN                                       
158900       CONTINUE                                                           
159000     ELSE                                                                 
159100       PERFORM IMS-ISRT-WLSAPA01                                          
159200                                                                          
159300       PERFORM UNTIL SEGMENT-FINNS                                        
159400         ADD +1 TO FIL-IDSEKVNR                                           
159500         PERFORM IMS-ISRT-WLSAPA01                                        
159600       END-PERFORM                                                        
159700     END-IF                                                               
159800     .                                                                    
159900     EJECT                                                                
160000                                                                          
160100 E-UPPD-HISTORIK           SECTION.                                       
160200                                                                          
160300     PERFORM S04-SKAPA-IDINLEV-IDLOPNRM                                   
160400                                                                          
160500     PERFORM IMS-GU-WLINLE01                                              
160600     IF SEGMENT-SAKNAS                                                    
160700        MOVE W-IDARTNR TO INLE-ART-IDARTNR                                
160800        PERFORM IMS-ISRT-WLINLE01                                         
160900     END-IF                                                               
161000                                                                          
161100     MOVE WS-DAINLEV      TO INLE-INL-DAINLEV                             
161200                             W-DAINLEV                                    
161300     PERFORM IMS-ISRT-WLINLE11                                            
161400     PERFORM UNTIL SEGMENT-FINNS                                          
161500       SUBTRACT 1 FROM W-DAINLEV                                          
161600       MOVE W-DAINLEV     TO INLE-INL-DAINLEV                             
161700       PERFORM IMS-ISRT-WLINLE11                                          
161800     END-PERFORM                                                          
161900                                                                          
162000     MOVE 'R33'                    TO INLE-DIR-IDPTYP                     
162100     MOVE WS-IDLOPNRM              TO INLE-DIR-IDLOPNRM                   
162200                                                                          
162300     MOVE MID-IDLEVNR  (MID-IX)    TO INLE-DIR-IDLEVNR                    
162400     MOVE MID-IDAVINR  (MID-IX)    TO INLE-DIR-IDAVINR                    
162500     MOVE MID-TIAVIDAT (MID-IX)    TO INLE-DIR-TIAVSDAT                   
162600     MOVE MID-IDDC     (MID-IX)    TO INLE-DIR-IDDC                       
162700     MOVE MID-KDRT     (MID-IX)    TO INLE-DIR-KDRT                       
162800     MOVE MID-KVAVIS   (MID-IX)    TO INLE-DIR-KVAVIS                     
162900     MOVE MID-IDDISTR  (MID-IX)    TO INLE-DIR-IDDISTR                    
163000     MOVE MID-IDKUNDNR (MID-IX)    TO INLE-DIR-IDKUNDNR                   
163100     MOVE MID-IDKUNDRF (MID-IX)    TO INLE-DIR-IDKUNDRF                   
163200     MOVE MID-IDPRODNR (MID-IX)    TO INLE-DIR-IDPRODNR                   
163300     MOVE MID-IDFAKT   (MID-IX)    TO INLE-DIR-IDFAKT                     
163400     MOVE MID-IDSUPREF (MID-IX)    TO INLE-DIR-IDSUPREF                   
163500     IF (MID-IDLEVNR (MID-IX) = '2394 ' OR 'CDJDA')                       
163600        AND MID-KDRT(MID-IX) = 6                                          
163700        MOVE 0                     TO INLE-DIR-IDKONTO                    
163800        MOVE 'SE REGELVERK'        TO INLE-DIR-IDANALYS                   
163900        MOVE SPACE                 TO INLE-DIR-IDKST                      
164000     ELSE                                                                 
164100        MOVE MID-IDKONTO (MID-IX)  TO INLE-DIR-IDKONTO                    
164200        MOVE MID-IDANALYS(MID-IX)  TO INLE-DIR-IDANALYS                   
164300        MOVE MID-IDKST   (MID-IX)  TO INLE-DIR-IDKST                      
164400     END-IF                                                               
164500                                                                          
164600     PERFORM IMS-ISRT-WLINLE22                                            
164700     .                                                                    
164800     EJECT                                                                
164900 F-UPPD-LEVPLAN            SECTION.                                       
165000                                                                          
165100     MOVE JA             TO AVROP-SW                                      
165200     MOVE MID-KVAVIS (MID-IX)  TO WS-KV-LPLAN                             
165300                                                                          
165400     IF  WS-KV-LPLAN             > ZERO                                   
165500                                                                          
165600       IF  MID-KDRT (MID-IX)     = 00                                     
165700       OR  (MID-KDRT (MID-IX)    = 01                                     
165800        AND ARTC11-CLAG-KDHF     > ZERO)                                  
165900       OR  (MID-KDRT (MID-IX)    = 02                                     
166000        AND ARTC11-CLAG-KDHF     > ZERO)                                  
166100       OR  MID-KDRT (MID-IX)     = 03                                     
166200       OR  MID-KDRT (MID-IX)     = 05                                     
166300       OR  (MID-KDRT (MID-IX)    = 06                                     
166400        AND MID-IDLEVNR (MID-IX)  NOT = SPACE                             
166500        AND MID-IDLEVNR (MID-IX)  NOT = '9999 ')                          
166600       OR  MID-KDRT (MID-IX)     = 09                                     
166700       OR  MID-KDRT (MID-IX)     = 10                                     
166800                                                                          
166900         IF  (MID-KDRT (MID-IX)  = 00                                     
167000          OR  MID-KDRT (MID-IX)  = 01                                     
167100          OR  MID-KDRT (MID-IX)  = 02                                     
167200          OR  MID-KDRT (MID-IX)  = 09)                                    
167300         AND ARTC11-CLAG-KDHF    > ZERO                                   
167400         AND ARTC11-CLAG-REDIRLEV = ZERO                                  
167500           MOVE SPAR-ARTC01-IDLEVNR TO W-INLB11-IDLEVNR                   
167600         ELSE                                                             
167700           MOVE MID-IDLEVNR (MID-IX) TO W-INLB11-IDLEVNR                  
167800         END-IF                                                           
167900                                                                          
168000         MOVE MID-IDARTNR(MID-IX) TO W-IDARTNR-D9                         
168100         MOVE MID-IDDC   (MID-IX) TO W-IDDC-D9                            
168200         PERFORM IMS-GU-INLB11                                            
168300                                                                          
168400         IF  SEGMENT-FINNS                                                
168500           IF  MID-KDRT (MID-IX)  NOT = 10                                
168600            IF ARTC11-CLAG-REDIRLEV = +0                                  
168700             PERFORM FA-BOKA-LEVPL-AVROP                                  
168800            END-IF                                                        
168900           END-IF                                                         
169000                                                                          
169100           IF ARTC11-CLAG-REDIRLEV = +0                                   
169200             PERFORM FB-BOKA-LEVPL-LBESK                                  
169300           END-IF                                                         
169400                                                                          
169500           IF  MID-KDRT (MID-IX)  NOT = 03                                
169600             PERFORM FC-BOKA-LEVPL-BREST                                  
169700           END-IF                                                         
169800         ELSE                                                             
169900           MOVE NEJ              TO AVROP-SW                              
170000         END-IF                                                           
170100       END-IF                                                             
170200     END-IF                                                               
170300     .                                                                    
170400     EJECT                                                                
170500 FA-BOKA-LEVPL-AVROP SECTION.                                             
170600                                                                          
170700     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
170800     MOVE ZERO                   TO WS-INLB23-TIAVROP-INL                 
170900                                                                          
171000     IF  MID-KDRT (MID-IX)       NOT = 03                                 
171100                                                                          
171200       MOVE +2                   TO W-KDAVROP                             
171300       PERFORM IMS-GHNP-INLB23-KD                                         
171400       IF SEGMENT-SAKNAS                                                  
171500         MOVE NEJ       TO AVROP-SW                                       
171600       END-IF                                                             
171700       PERFORM UNTIL (SEGMENT-SAKNAS                                      
171800                  OR WS-KV-OBOK  = ZERO)                                  
171900                                                                          
172000         PERFORM FAA-BOKA-ETT-AVROP                                       
172100                                                                          
172200         IF  WS-KV-OBOK          > ZERO                                   
172300           PERFORM IMS-GHNP-INLB23-KD                                     
172400         END-IF                                                           
172500       END-PERFORM                                                        
172600                                                                          
172700     ELSE                                                                 
172800                                                                          
172900       MOVE +2                   TO W-KDAVROP                             
173000       MOVE MID-IDAVINR (MID-IX) TO WS-IDAVINR                            
173100       MOVE WS-IDAVINR (3:4)     TO W-IDORDNSB                            
173200       PERFORM IMS-GNP-INLB32                                             
173300                                                                          
173400       MOVE INLB-KFB-DAAVROP-AVS TO W-DAAVROP                             
173500       MOVE INLB-KFB-TILEVDAG    TO W-TILEVDAG                            
173600       PERFORM IMS-GHNP-INLB23-TI-F                                       
173700                                                                          
173800       PERFORM FAA-BOKA-ETT-AVROP                                         
173900     END-IF                                                               
174000     .                                                                    
174100     EJECT                                                                
174200 FAA-BOKA-ETT-AVROP SECTION.                                              
174300                                                                          
174400     IF  WS-INLB23-TIAVROP-INL   = ZERO                                   
174500       MOVE INLB23-TIAVRDAT-INL  TO DAT-I-TIDATUM                         
174600       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
174700       CALL WDATKONV USING          DAT-KDDATFORM                         
174800                                    DAT-I-TIDATUM                         
174900                                    DAT-O-TIDATUM                         
175000                                    DAT-KDSVAR                            
175100       IF DAT-KDSVAR-FEL                                                  
175200         MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT                   
175300         CALL FELLOG                                                      
175400       ELSE                                                               
175500         MOVE DAT-TIAAVV-GRP     TO WS-TIAAVV                             
175600         MOVE WS-TIAAVV          TO WS-INLB23-TIAVROP-INL                 
175700       END-IF                                                             
175800     END-IF                                                               
175900                                                                          
176000     IF  INLB23-KVAVROP          <= WS-KV-OBOK                            
176100       SUBTRACT INLB23-KVAVROP   FROM WS-KV-OBOK                          
176200       MOVE INLB23-KVAVROP       TO INLB31-KVAVROP-AVB                    
176300       MOVE ZERO                 TO INLB23-KVAVROP                        
176400       MOVE +9                   TO INLB23-KDAVROP                        
176500     ELSE                                                                 
176600       SUBTRACT WS-KV-OBOK       FROM INLB23-KVAVROP                      
176700       MOVE WS-KV-OBOK           TO INLB31-KVAVROP-AVB                    
176800       MOVE ZERO                 TO WS-KV-OBOK                            
176900     END-IF                                                               
177000                                                                          
177100     MOVE WS-IDLOPNRM-AAVVDLLLL  TO INLB31-IDLOPNRM-PL                    
177200                                                                          
177300     PERFORM IMS-REPL-INLB                                                
177400                                                                          
177500     PERFORM IMS-ISRT-INLB31                                              
177600     .                                                                    
177700     EJECT                                                                
177800 FB-BOKA-LEVPL-LBESK SECTION.                                             
177900                                                                          
178000     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
178100                                                                          
178200     PERFORM IMS-GHNP-INLB24                                              
178300                                                                          
178400     PERFORM UNTIL (SEGMENT-SAKNAS                                        
178500                OR  WS-KV-OBOK   = ZERO)                                  
178600                                                                          
178700                                                                          
178800       MOVE DAGENS-DATUM     TO INLB24-LEV-TIREGDAT                       
178900       MOVE DAGENS-HHMMSS    TO INLB24-LEV-TIREGTID                       
179000                                                                          
179100       IF  INLB24-LEV-KVAVIS-BSKKVAR < WS-KV-OBOK                         
179200*      -- CL BOKAS NED HELT                                               
179300         SUBTRACT INLB24-LEV-KVAVIS-BSKKVAR                               
179400                                   FROM WS-KV-OBOK                        
179500         MOVE ZERO TO INLB24-LEV-KVAVIS-BSKKVAR                           
179600       ELSE                                                               
179700*      -- CL BOKAS NED DELVIS                                             
179800         SUBTRACT WS-KV-OBOK                                              
179900             FROM INLB24-LEV-KVAVIS-BSKKVAR                               
180000         MOVE ZERO                 TO WS-KV-OBOK                          
180100       END-IF                                                             
180200                                                                          
180300       COMPUTE WS-BSKKVAR-GGR-10                                          
180400         = INLB24-LEV-KVAVIS-BSKKVAR                                      
180500         * 10                                                             
180600       END-COMPUTE                                                        
180700                                                                          
180800       IF WS-BSKKVAR-GGR-10 < INLB24-LEV-KVAVIS-BSKURS                    
180900         PERFORM IMS-DLET-INLB                                            
181000       ELSE                                                               
181100         PERFORM IMS-REPL-INLB                                            
181200       END-IF                                                             
181300                                                                          
181400       IF  WS-KV-OBOK            > ZERO                                   
181500         PERFORM IMS-GHNP-INLB24                                          
181600       END-IF                                                             
181700     END-PERFORM                                                          
181800     .                                                                    
181900     EJECT                                                                
182000 FC-BOKA-LEVPL-BREST SECTION.                                             
182100                                                                          
182200     MOVE WS-KV-LPLAN            TO WS-KV-OBOK                            
182300                                                                          
182400     PERFORM IMS-GHU-INLB11                                               
182500                                                                          
182600     IF  INLB11-KVBR             <= WS-KV-OBOK                            
182700       MOVE ZERO                 TO INLB11-KVBR                           
182800     ELSE                                                                 
182900       SUBTRACT WS-KV-OBOK       FROM INLB11-KVBR                         
183000     END-IF                                                               
183100                                                                          
183200     PERFORM IMS-REPL-INLB                                                
183300     .                                                                    
183400     EJECT                                                                
183500 G-HAMTA-AVTAL SECTION.                                                   
183600                                                                          
183700     PERFORM IMS-GNP-ARTC23                                               
183800     IF SEGMENT-FINNS                                                     
183900       MOVE AVT-IDAVTAL            TO WS-ARTC23-IDAVTAL                   
184000     ELSE                                                                 
184100       MOVE ZERO                   TO WS-ARTC23-IDAVTAL                   
184200     END-IF                                                               
184300     .                                                                    
184400     EJECT                                                                
184500 S01-SKAPA-ZZAC01 SECTION.                                                
184600                                                                          
184700     ACCEPT ZZAC01-TIAAMMDD      FROM DATE                                
184800     ACCEPT ZZAC01-TIKLOCK       FROM TIME                                
184900                                                                          
185000     ADD +1                      TO WS-IDLOGLOP                           
185100     MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                       
185200                                                                          
185300     MOVE WS-ZZAC01-LOGGPOST     TO ZZAC01-LOGGPOST                       
185400     MOVE WS-ZZAC01-SORTPOST     TO ZZAC01-SORTPOST                       
185500                                                                          
185600     PERFORM IMS-ISRT-ZZAC01                                              
185700     PERFORM UNTIL SEGMENT-FINNS                                          
185800       IF WS-IDLOGLOP < +8                                                
185900         ADD +1 TO WS-IDLOGLOP                                            
186000         MOVE WS-IDLOGLOP   TO ZZAC01-IDLOGLOP                            
186100         PERFORM IMS-ISRT-ZZAC01                                          
186200       ELSE                                                               
186300         ACCEPT ZZAC01-TIAAMMDD      FROM DATE                            
186400         ACCEPT ZZAC01-TIKLOCK       FROM TIME                            
186500         MOVE +1 TO WS-IDLOGLOP                                           
186600         MOVE WS-IDLOGLOP   TO ZZAC01-IDLOGLOP                            
186700         PERFORM IMS-ISRT-ZZAC01                                          
186800       END-IF                                                             
186900     END-PERFORM                                                          
187000     .                                                                    
187100     EJECT                                                                
187200 S02-RED-W211FEL-GNRL SECTION.                                            
187300                                                                          
187400     MOVE ZERO                   TO W211FEL-SORT-FLT                      
187500     MOVE SPACE                  TO W211FEL-FILLER2                       
187600                                                                          
187700     MOVE 'R33'                  TO W211FEL-IDPTYP-S                      
187800     MOVE +1                     TO W211FEL-KDCLAGER-S                    
187900     MOVE MID-IDARTNR (MID-IX)   TO W211FEL-SORTBGP                       
188000     MOVE 1                      TO W211FEL-KDFELMRK                      
188100     .                                                                    
188200     EJECT                                                                
188300 S04-SKAPA-IDINLEV-IDLOPNRM SECTION.                                      
188400                                                                          
188500     IF WS-VVD = W-VVD                                                    
188600       ADD +1                     TO W-LLLL                               
188700     ELSE                                                                 
188800       MOVE WS-VVD                TO W-VVD                                
188900       MOVE +1                    TO W-LLLL                               
189000     END-IF                                                               
189100     CALL CHECK USING WS-IDLOPNRM (2:7) FLT-LGD                           
189200          VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B               
189300                                                                          
189400     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
189500     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
189600     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
189700     COMPUTE WS-DAINLEV          = 9999999999999999                       
189800                                 - WS-TIAAAAMMDDTTMMSSTH                  
189900     END-COMPUTE                                                          
190000     .                                                                    
190100     EJECT                                                                
190200 Z-FINIT     SECTION.                                                     
190300     MOVE WS-IDLOPNRM     TO 6018-IDLOPNRM                                
190400     PERFORM IMS-REPL-W6LOPA                                              
190500     MOVE INF-UPPDATE-DONE   TO MSG-KOM-IDMFSMED                          
190600                                                                          
190700     PERFORM IMS-ISRT-DISP-MSG                                            
190800     .                                                                    
190900     EJECT                                                                
191000* --- IMS SEKTIONER ---                                                   
191100     SKIP3                                                                
191200 IMS-GET-MSG SECTION.                                                     
191300                                                                          
191400     MOVE '  QC' TO GODK-STATUSKODER                                      
191500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
191600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
191700     PERFORM IMS-STATUSKONTROLL                                           
191800     .                                                                    
191900     SKIP3                                                                
192000 IMS-GN-MSG SECTION.                                                      
192100                                                                          
192200     MOVE '  ' TO GODK-STATUSKODER                                        
192300     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
192400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
192500     PERFORM IMS-STATUSKONTROLL                                           
192600     .                                                                    
192700     SKIP3                                                                
192800 IMS-ISRT-DISP-MSG   SECTION.                                             
192900                                                                          
193000     MOVE '  ' TO GODK-STATUSKODER                                        
193100     CALL CBLTDLI USING ISRT DISP-PCB MSG-KOM-WMSGKOM                     
193200     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
193300     PERFORM IMS-STATUSKONTROLL                                           
193400     .                                                                    
193500     EJECT                                                                
193600 IMS-GU-WLARTC01     SECTION.                                             
193700                                                                          
193800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
193900          DELIMITED BY SIZE INTO SSA1                                     
194000     MOVE '  ' TO GODK-STATUSKODER                                        
194100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
194200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
194300     PERFORM IMS-STATUSKONTROLL                                           
194400     .                                                                    
194500     SKIP3                                                                
194600 IMS-GHNP-WLARTC11   SECTION.                                             
194700                                                                          
194800     MOVE 'WLARTC11 ' TO SSA1                                             
194900     MOVE '  ' TO GODK-STATUSKODER                                        
195000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA4 SSA1                   
195100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
195200     PERFORM IMS-STATUSKONTROLL                                           
195300     .                                                                    
195400     SKIP3                                                                
195500 IMS-GHNP-WLARTC11-FIRST   SECTION.                                       
195600                                                                          
195700     MOVE 'WLARTC11*F ' TO SSA1                                           
195800     MOVE '  ' TO GODK-STATUSKODER                                        
195900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA4 SSA1                   
196000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
196100     PERFORM IMS-STATUSKONTROLL                                           
196200     .                                                                    
196300     SKIP3                                                                
196400 IMS-GHNP-WLARTC21   SECTION.                                             
196500                                                                          
196600     STRING 'WLARTC21(IDLEVNR  =' W-IDLEVNR-21-X ')'                      
196700          DELIMITED BY SIZE INTO SSA1                                     
196800     MOVE '  GE' TO GODK-STATUSKODER                                      
196900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
197000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
197100     PERFORM IMS-STATUSKONTROLL                                           
197200     .                                                                    
197300     SKIP3                                                                
197400 IMS-REPL-WLARTC     SECTION.                                             
197500                                                                          
197600     MOVE '  ' TO GODK-STATUSKODER                                        
197700     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
197800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
197900     PERFORM IMS-STATUSKONTROLL                                           
198000     .                                                                    
198100     EJECT                                                                
198200 IMS-REPL-WLARTC11   SECTION.                                             
198300                                                                          
198400     MOVE '  ' TO GODK-STATUSKODER                                        
198500     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA4 SSA1                   
198600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
198700     PERFORM IMS-STATUSKONTROLL                                           
198800     .                                                                    
198900     SKIP3                                                                
199000 IMS-GNP-ARTC23 SECTION.                                                  
199100                                                                          
199200     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
199300     MOVE 'WLARTC23'              TO SSA2                                 
199400     MOVE '  GE' TO GODK-STATUSKODER                                      
199500     CALL CBLTDLI USING GNP ARTC-PCB WLARTC23 SSA1 SSA2                   
199600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
199700     PERFORM IMS-STATUSKONTROLL                                           
199800     .                                                                    
199900     EJECT                                                                
200000 IMS-GU-WLINLE01 SECTION.                                                 
200100                                                                          
200200     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
200300          DELIMITED BY SIZE INTO SSA1                                     
200400     MOVE '  GE' TO GODK-STATUSKODER                                      
200500     CALL CBLTDLI USING GU   INLE-PCB DLI-IO-AREA SSA1                    
200600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
200700     PERFORM IMS-STATUSKONTROLL                                           
200800     .                                                                    
200900     SKIP3                                                                
201000 IMS-ISRT-WLINLE01 SECTION.                                               
201100                                                                          
201200     MOVE 'WLINLE01 ' TO SSA1                                             
201300     MOVE '  ' TO GODK-STATUSKODER                                        
201400     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1                    
201500     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
201600     PERFORM IMS-STATUSKONTROLL                                           
201700     .                                                                    
201800     SKIP3                                                                
201900 IMS-ISRT-WLINLE11 SECTION.                                               
202000                                                                          
202100     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
202200          DELIMITED BY SIZE INTO SSA1                                     
202300     MOVE 'WLINLE11 ' TO SSA2                                             
202400     MOVE '  II' TO GODK-STATUSKODER                                      
202500     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2               
202600     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
202700     PERFORM IMS-STATUSKONTROLL                                           
202800     .                                                                    
202900     SKIP3                                                                
203000 IMS-ISRT-WLINLE22 SECTION.                                               
203100                                                                          
203200     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
203300          DELIMITED BY SIZE INTO SSA1                                     
203400     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
203500          DELIMITED BY SIZE INTO SSA2                                     
203600     MOVE 'WLINLE22 ' TO SSA3                                             
203700     MOVE '  ' TO GODK-STATUSKODER                                        
203800     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
203900     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
204000     PERFORM IMS-STATUSKONTROLL                                           
204100     .                                                                    
204200     EJECT                                                                
204300 IMS-ISRT-ZZAC01 SECTION.                                                 
204400                                                                          
204500     MOVE 'WLZZAC01 ' TO SSA1                                             
204600     MOVE '  II' TO GODK-STATUSKODER                                      
204700     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
204800     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
204900     PERFORM IMS-STATUSKONTROLL                                           
205000     .                                                                    
205100     EJECT                                                                
205200 IMS-GU-INLB11 SECTION.                                                   
205300     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
205400          DELIMITED BY SIZE INTO SSA1                                     
205500     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
205600          DELIMITED BY SIZE INTO SSA2                                     
205700     MOVE '  GE' TO GODK-STATUSKODER                                      
205800     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1 SSA2                 
205900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
206000     PERFORM IMS-STATUSKONTROLL                                           
206100     .                                                                    
206200     SKIP3                                                                
206300 IMS-GHU-INLB11 SECTION.                                                  
206400     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
206500          DELIMITED BY SIZE INTO SSA1                                     
206600     STRING 'WLINLB11(IDLEVNR  =' W-INLB11-IDLEVNR-X ')'                  
206700          DELIMITED BY SIZE INTO SSA2                                     
206800     MOVE '  ' TO GODK-STATUSKODER                                        
206900     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2                
207000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
207100     PERFORM IMS-STATUSKONTROLL                                           
207200     .                                                                    
207300     SKIP3                                                                
207400 IMS-GHNP-INLB23-KD SECTION.                                              
207500     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
207600          DELIMITED BY SIZE INTO SSA1                                     
207700     MOVE '  GE' TO GODK-STATUSKODER                                      
207800     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
207900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
208000     PERFORM IMS-STATUSKONTROLL                                           
208100     .                                                                    
208200     EJECT                                                                
208300 IMS-GHNP-INLB23-TI-F SECTION.                                            
208400* ?  STRING 'WLINLB23*F(DAAVROP  =' W-DAAVROP-X                           
208500     STRING 'WLINLB23*F(WDD905KY =' W-WDD905KY-X                          
208600                      '&KDAVROP  =' W-KDAVROP-X ')'                       
208700          DELIMITED BY SIZE INTO SSA1                                     
208800     MOVE '  ' TO GODK-STATUSKODER                                        
208900     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
209000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
209100     PERFORM IMS-STATUSKONTROLL                                           
209200     .                                                                    
209300     SKIP3                                                                
209400 IMS-GNP-INLB32 SECTION.                                                  
209500     STRING 'WLINLB23(KDAVROP  =' W-KDAVROP-X ')'                         
209600          DELIMITED BY SIZE INTO SSA1                                     
209700     STRING 'WLINLB32(IDORDNSB =' W-IDORDNSB-X ')'                        
209800          DELIMITED BY SIZE INTO SSA2                                     
209900     MOVE '  ' TO GODK-STATUSKODER                                        
210000     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
210100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
210200     PERFORM IMS-STATUSKONTROLL                                           
210300     .                                                                    
210400     EJECT                                                                
210500 IMS-GHNP-INLB24 SECTION.                                                 
210600     MOVE 'WLINLB24 ' TO SSA1                                             
210700     MOVE '  GE' TO GODK-STATUSKODER                                      
210800     CALL CBLTDLI USING GHNP INLB-PCB DLI-IO-AREA SSA1                    
210900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
211000     PERFORM IMS-STATUSKONTROLL                                           
211100     .                                                                    
211200     SKIP3                                                                
211300 IMS-ISRT-INLB31 SECTION.                                                 
211400     MOVE 'WLINLB31 ' TO SSA1                                             
211500     MOVE '  ' TO GODK-STATUSKODER                                        
211600     CALL CBLTDLI USING ISRT INLB-PCB DLI-IO-AREA2 SSA1                   
211700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
211800     PERFORM IMS-STATUSKONTROLL                                           
211900     .                                                                    
212000     SKIP3                                                                
212100 IMS-REPL-INLB SECTION.                                                   
212200     MOVE '  ' TO GODK-STATUSKODER                                        
212300     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA                         
212400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
212500     PERFORM IMS-STATUSKONTROLL                                           
212600     .                                                                    
212700     SKIP3                                                                
212800 IMS-DLET-INLB SECTION.                                                   
212900     MOVE '  ' TO GODK-STATUSKODER                                        
213000     CALL CBLTDLI USING DLET INLB-PCB DLI-IO-AREA                         
213100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
213200     PERFORM IMS-STATUSKONTROLL                                           
213300     .                                                                    
213400     EJECT                                                                
213500 IMS-GHU-W6LOPA11 SECTION.                                                
213600     STRING 'W6LOPA01(W6GXKEY  =' W-6017KEY-X ')'                         
213700          DELIMITED BY SIZE INTO SSA1                                     
213800     MOVE 'W6LOPA11 ' TO SSA2                                             
213900     MOVE '  ' TO GODK-STATUSKODER                                        
214000     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA3 SSA1 SSA2               
214100     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
214200     PERFORM IMS-STATUSKONTROLL                                           
214300     .                                                                    
214400     EJECT                                                                
214500 IMS-REPL-W6LOPA SECTION.                                                 
214600     MOVE '  ' TO GODK-STATUSKODER                                        
214700     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA3                        
214800     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
214900     PERFORM IMS-STATUSKONTROLL                                           
215000     .                                                                    
215100     EJECT                                                                
215200 IMS-ISRT-WLFILB01 SECTION.                                               
215300     MOVE 'WLFILB01 ' TO SSA1                                             
215400     MOVE '  II' TO GODK-STATUSKODER                                      
215500     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA-FILB01 SSA1             
215600     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
215700     PERFORM IMS-STATUSKONTROLL                                           
215800     .                                                                    
215900     EJECT                                                                
216000 IMS-ISRT-WLSAPA01 SECTION.                                               
216100     MOVE 'WLSAPA01 ' TO SSA1                                             
216200     MOVE '  II' TO GODK-STATUSKODER                                      
216300     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
216400     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
216500     PERFORM IMS-STATUSKONTROLL                                           
216600     .                                                                    
216700     EJECT                                                                
216800 IMS-GU-WDK711 SECTION.                                                   
216900                                                                          
217000     STRING 'WDK701  (IDARTNR  =' W-WDK701-X ')'                          
217100          DELIMITED BY SIZE INTO SSA1                                     
217200     STRING 'WDK711  (IDDC     =' W-WDK711-X ')'                          
217300          DELIMITED BY SIZE INTO SSA2                                     
217400     MOVE '  GE' TO GODK-STATUSKODER                                      
217500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711   SSA1 SSA2             
217600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
217700     PERFORM IMS-STATUSKONTROLL                                           
217800     .                                                                    
217900     EJECT                                                                
218000 IMS-GHU-WDK711-K7 SECTION.                                               
218100                                                                          
218200     STRING 'WDK701  (IDARTNR  =' W-WDK701-X ')'                          
218300          DELIMITED BY SIZE INTO SSA1                                     
218400     STRING 'WDK711  (IDDC     =' W-WDK711-K7-X ')'                       
218500          DELIMITED BY SIZE INTO SSA2                                     
218600     MOVE '  GE' TO GODK-STATUSKODER                                      
218700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711   SSA1 SSA2            
218800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
218900     PERFORM IMS-STATUSKONTROLL                                           
219000     .                                                                    
219100     EJECT                                                                
219200                                                                          
219300 IMS-GHU-WDK712-K7 SECTION.                                               
219400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
219500             DELIMITED BY SIZE INTO SSA1                                  
219600     STRING 'WDK712  (IDLAND   =' W-IDLAND-K7-X ')'                       
219700             DELIMITED BY SIZE INTO SSA2                                  
219800     MOVE '    ' TO GODK-STATUSKODER                                      
219900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1                   
220000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
220100     PERFORM IMS-STATUSKONTROLL                                           
220200     .                                                                    
220300 IMS-REPL-WDK712-K7 SECTION.                                              
220400     MOVE '  ' TO GODK-STATUSKODER                                        
220500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
220600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
220700     PERFORM IMS-STATUSKONTROLL                                           
220800     .                                                                    
220900                                                                          
221000 IMS-GHNP-WDK724-K7 SECTION.                                              
221100     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST-K7-N                         
221200                     '&IDLEVNRP =' W-IDLEVNR-PR-X ')'                     
221300     DELIMITED BY SIZE INTO SSA1                                          
221400     MOVE '  GE' TO GODK-STATUSKODER                                      
221500     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK724 SSA1                  
221600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
221700     PERFORM IMS-STATUSKONTROLL                                           
221800     .                                                                    
221900 IMS-REPL-WDK724-K7 SECTION.                                              
222000     MOVE '  ' TO GODK-STATUSKODER                                        
222100     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK724                       
222200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
222300     PERFORM IMS-STATUSKONTROLL                                           
222400     .                                                                    
222500     EJECT                                                                
222600                                                                          
222700 IMS-GU-WDB601    SECTION.                                                
222800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
222900          DELIMITED BY SIZE INTO SSA1                                     
223000     MOVE '  GE' TO GODK-STATUSKODER                                      
223100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
223200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
223300     PERFORM IMS-STATUSKONTROLL                                           
223400     IF SEGMENT-SAKNAS                                                    
223500         MOVE SPACE TO DCS-KDDC                                           
223600     END-IF                                                               
223700     .                                                                    
223800                                                                          
223900 IMS-GNP-WDB617    SECTION.                                               
224000     MOVE 'WDB617   ' TO SSA1                                             
224100     MOVE '  GE' TO GODK-STATUSKODER                                      
224200     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
224300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
224400     PERFORM IMS-STATUSKONTROLL                                           
224500     .                                                                    
224600                                                                          
224700 IMS-GU-WDF101   SECTION.                                                 
224800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
224900     DELIMITED BY SIZE INTO SSA1                                          
225000     MOVE '  GE' TO GODK-STATUSKODER                                      
225100     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA-F1 SSA1                   
225200     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
225300     PERFORM IMS-STATUSKONTROLL                                           
225400     .                                                                    
225500     SKIP3                                                                
225600                                                                          
225700 IMS-GNP-WDF102   SECTION.                                                
225800     STRING 'WDF102  (IDLAND   =' W-IDLAND-X ')'                          
225900     DELIMITED BY SIZE INTO SSA1                                          
226000     MOVE '  GE' TO GODK-STATUSKODER                                      
226100     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-AREA-F102 SSA1                
226200     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
226300     PERFORM IMS-STATUSKONTROLL                                           
226400     .                                                                    
226500     EJECT                                                                
226600                                                                          
226700 IMS-GU-WDGX9306 SECTION.                                                 
226800     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
226900             DELIMITED BY SIZE INTO SSA1                                  
227000     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
227100             DELIMITED BY SIZE INTO SSA2                                  
227200     MOVE '  GE'   TO GODK-STATUSKODER                                    
227300     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
227400     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
227500     PERFORM IMS-STATUSKONTROLL                                           
227600     .                                                                    
227700     SKIP3                                                                
227800                                                                          
227900 IMS-GNP-WDGX9308 SECTION.                                                
228000     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
228100             DELIMITED BY SIZE INTO SSA1                                  
228200     MOVE '  GE'   TO GODK-STATUSKODER                                    
228300     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
228400     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
228500     PERFORM IMS-STATUSKONTROLL                                           
228600     .                                                                    
228700     SKIP3                                                                
228800                                                                          
228900 IMS-GNP-WDGX9308-FIRST SECTION.                                          
229000     MOVE 'WDGX9308*F' TO SSA1                                            
229100     MOVE '  GE'   TO GODK-STATUSKODER                                    
229200     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
229300     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
229400     PERFORM IMS-STATUSKONTROLL                                           
229500     .                                                                    
229600     SKIP3                                                                
229700                                                                          
229800 IMS-STATUSKONTROLL SECTION.                                              
229900                                                                          
230000     SET STATUS-IX TO 1                                                   
230100     SEARCH GODK-STATUS                                                   
230200       AT END                                                             
230300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
230400         DELIMITED BY SIZE INTO FELTEXT                                   
230500         CALL FELLOG                                                      
230600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
230700         CONTINUE                                                         
230800     END-SEARCH                                                           
230900     .                                                                    
231000     EJECT                                                                
231100*    -COPY WY2000P1                                                       
