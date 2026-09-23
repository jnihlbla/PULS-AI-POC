000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL017300.                                                
000300 AUTHOR.         KJELLSON GÖRAN  GUIDE                                    
000400 DATE-WRITTEN.   NOVEMBER 2004                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.LDC.PRINVENTORYDOCBG                            
000800*    WEB-LDC: WL017300 PROGRAM IS A REPLICA OF W5030100 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        BEGÄRAN OM UTSKRIFT AV INVENTERINGSANMODAN                       
001300*        UTSKRIFT KAN VÄLJAS PÅ FÖLJANDE SÄTT:                            
001400*        - ETT VISST ARTIKELNUMMER                                        
001500*        - ETT VISST ANTAL                                                
001600*        - ETT VISST ANTAL INOM ETT LAGEROMRÅDE                           
001700*        OM ANTAL KOMBINERAS MED OMRÅDE LÄSES BASEN FRÅN BÖRJAN.          
001800*        PRIORITERADE ARTIKLAR (PRIORITET = 1) SKRIVS UT I FÖRSTA         
001900*        HAND OBEROENDE AV OMRÅDE.                                        
002000*                                                                         
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSACTION: WL0173T                                             
002400*        REQUEST:     WL0173I1                                            
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        RESPONSE:    WL01731                                             
002800                                                                          
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400 DATA DIVISION.                                                           
003500 FILE SECTION.                                                            
003600                                                                          
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'WL017300'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300 77  CURR-SECTION                PIC X(16) VALUE 'MAIN'.                  
004400 77  CURR-IMS-SECTION            PIC X(16) VALUE SPACE.                   
004500                                                                          
004600 77  YES                         PIC X       VALUE 'J'.                   
004700 77  NOO                         PIC X       VALUE 'N'.                   
004800 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77  WSDC-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77  IX-MAX                      PIC S9(9)   VALUE +10  COMP SYNC.        
005200 77  MAX-IX-2                    PIC S9(9)   VALUE +2   COMP SYNC.        
005300 77  MAX-IX-4                    PIC S9(9)   VALUE +4   COMP SYNC.        
005400 77  MAX-IX-7                    PIC S9(9)   VALUE +7   COMP SYNC.        
005500 77  SALDO-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005600 77  EFR-IX-MAX                  PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77  DOC-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005800 77  MSG-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005900                                                                          
006000 77  WS-IDSKYLT-CHINESE          PIC X(3)  VALUE 'RCN'.                   
006100 77  WS-IDSKYLT-ENGLISH          PIC X(3)  VALUE 'GB '.                   
006200                                                                          
006300 01  WS-KVEFRS-OLD               PIC S9(7) COMP-3.                        
006400 01  WS-TIREGDAT                 PIC 9(9)    VALUE ZERO.                  
006500 01  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
006600 01  WS-ANTAL-RADER-EFR          PIC 9(3).                                
006700 77  WS-ANTAL-RADER-BUF          PIC 9(3).                                
006800 01  WS-SDC-ANTAL                PIC S9(9)   VALUE  ZERO COMP-3.          
006900 01  WS-IDKUNDRF.                                                         
007000     03  WS-IDORDNR              PIC 9(5).                                
007100     03  FILLER                  PIC X(5).                                
007200 01  WS-DAGENS-DATUM             PIC 9(8)    VALUE ZERO.                  
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES DAGENS-DATUM.                                       
007500     03  DAGENS-AA               PIC 9(2).                                
007600     03  DAGENS-MM               PIC 9(2).                                
007700     03  DAGENS-DD               PIC 9(2).                                
007800                                                                          
007900 01  WS-IDDC-LOCAL.                                                       
008000     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
008100     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
008200     03  FILLER                  PIC X(1)   VALUE SPACE.                  
008300                                                                          
008400 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008500 77  KEYS-SW                     PIC X       VALUE 'J'.                   
008600     88  KEYS-OK                             VALUE 'J'.                   
008700     88  KEYS-WRONG                          VALUE 'N'.                   
008800                                                                          
008900 77  HEADER-SW                   PIC X       VALUE 'N'.                   
009000     88  HEADER-PRINTED                      VALUE 'Y'.                   
009100     88  HEADER-NOT-PRINTED                  VALUE 'N'.                   
009200                                                                          
009300 77    BUFFERT-SW                PIC X.                                   
009400   88  BUFFERT-PRINT                         VALUE 'J'.                   
009500                                                                          
009600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009700 01  GENERAL-SUBPROGRAMS.                                                 
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010400     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
010500     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
010600     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
010700     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
010800                                                                          
010900 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
011000*01  -COPY  WDATAREA                                                      
011100                                                                          
011200*    --- PARAMETERS TO ABEND                                              
011300                                                                          
011400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011700                                                                          
011800 01  WS-IDPRTINV.                                                         
011900     03 WS-IDPRTOMG              PIC S9     COMP-3.                       
012000     03 WS-IDLOPNR               PIC S9(5)  COMP-3.                       
012100 01  WS-IDLOPNR-5                PIC 9(5).                                
012200 01  WS-IDPRTINV-NUM             PIC 9(6).                                
012300                                                                          
012400 01  W-IDPRTOMG-ALFA             PIC X.                                   
012500                                                                          
012600 01  SDC-TAB-EFR.                                                         
012700   03  WSDC-TAB OCCURS 7  TIMES.                                          
012800     05  WSDC-IDKUNDRF           PIC 9(5)    VALUE ZERO.                  
012900     05  WSDC-IDKUNDNR           PIC 9(5)    VALUE ZERO.                  
013000     05  WSDC-IDDISTR            PIC 9(5)    VALUE ZERO.                  
013100     05  WSDC-KVLEVART           PIC 9(7)    VALUE ZERO.                  
013200                                                                          
013300 01  MESSAGE-CODES.                                                       
013400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
013500     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
013600                                                                          
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
013900*01  -COPY WZ01SUB                                                        
014000                                                                          
014100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
014200*01  -COPY WZ01SEND                                                       
014300                                                                          
014400 01  FILLER                      PIC X(16)  VALUE 'WTRAUTF8-AREA'.        
014500*01  -COPY WTRAUTF8                                                       
014600                                                                          
014700 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
014800*01  -COPY WMSGCONV                                                       
014900                                                                          
015000 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
015100*01  -COPY WZ01AUTH                                                       
015200                                                                          
015300 01  WS-API-TIJUSTDA             PIC 9(6)    VALUE ZERO.                  
015400                                                                          
015500 01  WS-API-DATE.                                                         
015600     03  FILLER                  PIC X(2)    VALUE '20'.                  
015700     03  WS-API-YEAR             PIC 9(2).                                
015800     03  FILLER                  PIC X(1)    VALUE '-'.                   
015900     03  WS-API-MONTH            PIC 9(2).                                
016000     03  FILLER                  PIC X(1)    VALUE '-'.                   
016100     03  WS-API-DAY              PIC 9(2).                                
016200                                                                          
016300 01  WS-API-TIME.                                                         
016400     03  WS-API-HH               PIC 9(2).                                
016500     03  FILLER                  PIC X(1)    VALUE ':'.                   
016600     03  WS-API-MM               PIC 9(2).                                
016700     03  FILLER                  PIC X(1)    VALUE ':'.                   
016800     03  WS-API-SS               PIC 9(2).                                
016900                                                                          
017000 01  CURRENT-OUTPUT.                                                      
017100     03 CURRO-KDINVPRIO       PIC 9(01).                                  
017200     03 CURRO-IDMSG-INFO      PIC X(03).                                  
017300     03 CURRO-IDMSG-ERROR     PIC X(03).                                  
017400     03 CURRO-IDELMT-ERROR    PIC X(16).                                  
017500     03 CURRO-BUFFAREA-GRP.                                               
017600        05 CURRO-ART-BUFF    OCCURS 4 TIMES.                              
017700           07 CURRO-ADBUFFOMR  PIC 9(2).                                  
017800           07 CURRO-ADBUFFGANG PIC 9(2).                                  
017900           07 CURRO-ADBUFFPL   PIC 9(5).                                  
018000           07 CURRO-KVBUFF-F   PIC -(7)9.                                 
018100           07 CURRO-KVKOLLI-F  PIC 9(4).                                  
018200                                                                          
018300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
018400 01  REQU-AREA.                                                           
018500*    03  -COPY WZ01REQ2                                                   
018600*    03  -COPY WL0173I1                                                   
018700                                                                          
018800* ONLY USED FOR API                                                       
018900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
019000 01  RESP-AREA.                                                           
019100*    03  -COPY WZ01RES2                                                   
019200*    03  -COPY WL0173O1                                                   
019300                                                                          
019400 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
019500 01  HDR-AREA.                                                            
019600*    03  -COPY WZ01REQU  -PRE HDR-                                        
019700*    03  -COPY WZ04HDR                                                    
019800*                                                                         
019900 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
020000 01  DOC-AREA.                                                            
020100*    03  -COPY WL01731                                                    
020200*                                                                         
020300*01  -COPY WL01TIDZ                                                       
020400     EJECT                                                                
020500************************************************************              
020600*  WDH111 SPAR AREA                                                       
020700 01  FILLER                    PIC X(16)   VALUE 'SPAR-WDH111'.           
020800 01  SPAR-WDH111-AREA.                                                    
020900     03  WDH111.                                                          
021000*        05  -COPY WDH111  -PRE SPAR-                                     
021100***************************************************************           
021200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021300*                                                                         
021400 01    IMS-WS.                                                            
021500   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
021600     SKIP3                                                                
021700*                        **** STATUS-KOD FRÅN IMS                         
021800   03    STATUS-WS       PIC XX.                                          
021900         88  SEGMENT-FOUND       VALUE '  '.                              
022000         88  SEGMENT-MISSING     VALUE 'GE'.                              
022100         88  SEGMENT-EXISTS      VALUE 'II'.                              
022200         88  INDEX-EXISTS        VALUE 'NI'.                              
022300         88  END-OF-DB           VALUE 'GB'.                              
022400                                                                          
022500   03    GOOD-STATUSCODES.                                                
022600     05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022700                                                                          
022800 01    ALL-SSA.                                                           
022900       03 SSA1                   PIC X(132).                              
023000       03 SSA2                   PIC X(132).                              
023100                                                                          
023200*                            IMS FUNKTIONSKODER                           
023300*01    -COPY W0003                                                        
023400                                                                          
023500 01  FILLER                    PIC X(16) VALUE 'NYC-TILL-DLI'.            
023600 01  NYCKLAR-TILL-DLI.                                                    
023700                                                                          
023800     03 W-IDDC                 PIC X(2)  VALUE SPACE.                     
023900                                                                          
024000     03 W-IDARTNR-X.                                                      
024100        05  W-IDARTNR          PIC S9(9) COMP-3.                          
024200                                                                          
024300     03 W-IDPRODNR-X.                                                     
024400        05  W-IDPRODNR         PIC S9(7) COMP-3.                          
024500                                                                          
024600     03 W-IDSKYLT-X.                                                      
024700        05  W-IDSKYLT          PIC X(3)    VALUE SPACE.                   
024800                                                                          
024900     03 W-TISEGKEY-X.                                                     
025000        05  W-TISEGKEY     PIC S9(9) VALUE +999999999 COMP-3.             
025100                                                                          
025200     03 W-ORDSTA-X.                                                       
025300        05  W-ORDSTA           PIC S9      COMP-3 VALUE +4.               
025400                                                                          
025500     03 W-WDG3KEY01-X.                                                    
025600        05  W-IDHTYP           PIC X(4)    VALUE '5101'.                  
025700        05  W-FILLER           PIC X(26)   VALUE LOW-VALUE.               
025800                                                                          
025900     03 W-ROT-WDG3KEY-X.                                                  
026000        05  FILLER             PIC X(4)  VALUE '5113'.                    
026100        05  W-IDDC-G3          PIC X(2)  VALUE SPACE.                     
026200        05  FILLER             PIC X(24) VALUE LOW-VALUE.                 
026300                                                                          
026400     03 W-WDH111KY-MIN-X.                                                 
026500        05 W-IDDC-WDH1-MIN     PIC X(2)  VALUE SPACE.                     
026600        05 W-KDINVKAT-WDH1-MIN PIC S9(3) COMP-3.                          
026700        05 W-TISEGKEY-WDH1-MIN PIC S9(9) VALUE ZERO  COMP-3.              
026800        05 W-DAREGDAT-SORT-MIN    PIC  9(8) VALUE ZERO.                   
026900     03 W-WDH111KY-MAX-X.                                                 
027000        05 W-IDDC-WDH1-MAX     PIC X(2)  VALUE SPACE.                     
027100        05 W-KDINVKAT-WDH1-MAX PIC S9(3) COMP-3.                          
027200        05 W-TISEGKEY-WDH1-MAX PIC S9(9) VALUE +999999999 COMP-3.         
027300        05 W-DAREGDAT-SORT-MIN PIC 9(8) VALUE 99999999.                   
027400                                                                          
027500     03  W-WDE4C1KY-LOW.                                                  
027600        05 W-IDARTNR-LOW       PIC S9(9)   COMP-3.                        
027700        05 FILLER              PIC X(7)    VALUE  LOW-VALUE.              
027800                                                                          
027900     03  W-WDE4C1KY-HIGH.                                                 
028000        05 W-IDARTNR-HIGH      PIC S9(9)   COMP-3.                        
028100        05 FILLER              PIC X(7)    VALUE  HIGH-VALUE.             
028200                                                                          
028300     03  W-WDE401-X.                                                      
028400         05  W-401-IDDISTR      PIC S9(5)      VALUE ZERO COMP-3.         
028500         05  W-401-IDKUNDNR     PIC S9(7)      VALUE ZERO COMP-3.         
028600         05  W-401-IDKUNDRF.                                              
028700           07  W-401-IDORDNR    PIC 9(5)       VALUE ZERO.                
028800           07  FILLER           PIC X(5)       VALUE SPACE.               
028900         05  W-401-IDPRODNR     PIC S9(7)      VALUE ZERO COMP-3.         
029000         05  W-401-IDPLKLST     PIC S9(3)      VALUE ZERO COMP-3.         
029100*                                                                         
029200     03  W-WDE411-X.                                                      
029300         05  W-411-IDPURAD      PIC S9(5)      VALUE ZERO COMP-3.         
029400                                                                          
029500     03  W-IDDC-B6-X.                                                     
029600         05 W-IDDC-B6            PIC X(2).                                
029700                                                                          
029800     03  W-WDD8B1KY-MIN-X.                                                
029900       05 W-IDDC-D8-MIN        PIC X(2)           VALUE SPACE.            
030000       05 W-IDARTNR-D8-MIN     PIC S9(9)   COMP-3 VALUE ZERO.             
030100       05 FILLER               PIC X(15)   VALUE  LOW-VALUE.              
030200                                                                          
030300     03 W-WDD8B1KY-MAX-X.                                                 
030400       05 W-IDDC-D8-MAX        PIC X(2)           VALUE SPACE.            
030500       05 W-IDARTNR-D8-MAX     PIC S9(9)   COMP-3 VALUE ZERO.             
030600       05 FILLER               PIC X(15)   VALUE  HIGH-VALUE.             
030700                                                                          
030800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
030900 01    DLI-IO-AREA.                                                       
031000   03  IO-AREA                 PIC X(900)  VALUE SPACE.                   
031100*  03  WLBENA11 -COPY WDD311 -PRE BEN-      -RED IO-AREA.                 
031200                                                                          
031300*-------- WDK6-ARTIKELREG                                                 
031400                                                                          
031500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK601'.           
031600 01  DLI-IO-WDK601.                                                       
031700*  03  -COPY WDK601.                                                      
031800                                                                          
031900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK611'.           
032000 01  DLI-IO-WDK611.                                                       
032100*  03  -COPY WDK611.                                                      
032200                                                                          
032300*-------- WDK7-ARTIKELREG                                                 
032400                                                                          
032500 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK701'.           
032600 01  DLI-IO-WDK701.                                                       
032700*  03  -COPY WDK701.                                                      
032800                                                                          
032900 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-WDK711'.           
033000 01  DLI-IO-WDK711.                                                       
033100*  03  -COPY WDK711.                                                      
033200                                                                          
033300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA5'.          
033400 01    DLI-IO-AREA5.                                                      
033500   03  IO-AREA5                  PIC X(300)  VALUE SPACE.                 
033600*  03  WLINVC01 -COPY WDH701               -RED IO-AREA5.                 
033700*  03  WLINVC11 -COPY WDH711               -RED IO-AREA5.                 
033800                                                                          
033900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E4C1'.           
034000 01  DLI-IO-E4C1.                                                         
034100*  03  -COPY WDE4C1                                                       
034200                                                                          
034300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-E401-11'.          
034400 01  DLI-IO-E401-11.                                                      
034500*  03  -COPY WDE401                                                       
034600*  03  -COPY WDE411                                                       
034700                                                                          
034800 01  FILLER                    PIC X(16)   VALUE 'WDH101-AREA'.           
034900 01  DLI-IO-AREA-INVA01.                                                  
035000     03  WDH101.                                                          
035100*        05  -COPY WDH101                                                 
035200                                                                          
035300 01  FILLER                    PIC X(16)   VALUE 'WDH111-AREA'.           
035400 01  DLI-IO-AREA-INVA11.                                                  
035500     03  WDH111.                                                          
035600*        05  -COPY WDH111                                                 
035700 01  FILLER                    PIC X(16)   VALUE 'WDH121-AREA'.           
035800 01  DLI-IO-AREA-INVA21.                                                  
035900     03  WDH121.                                                          
036000*        05  -COPY WDH121                                                 
036100                                                                          
036200 01  FILLER                      PIC X(16)   VALUE 'WDGX5102AREA'.        
036300 01  DLI-IO-AREA-WDGX5102.                                                
036400     03  WDGX5102.                                                        
036500*        05  -COPY WDGX5102                                               
036600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
036700 01   DLI-IO-AREA-B601.                                                   
036800*     03  -COPY WDB601                                                    
036900     EJECT                                                                
037000                                                                          
037100 01  FILLER               PIC X(16)   VALUE 'WDE601 AREA'.                
037200 01   DLI-IO-AREA-E601.                                                   
037300*     03  -COPY WDE601                                                    
037400     EJECT                                                                
037500 01  FILLER               PIC X(16)   VALUE 'WDD8B1 AREA'.                
037600 01   DLI-IO-AREA-D8B1.                                                   
037700*     03  -COPY WDD8B1                                                    
037800     EJECT                                                                
037900                                                                          
038000 LINKAGE SECTION.                                                         
038100*01  -COPY W0009           -PRE MSG-                                      
038200                                                                          
038300 01  DISTRDOC-PCB                PIC X.                                   
038400                                                                          
038500 01  DISTRSAV-PCB                PIC X.                                   
038600                                                                          
038700 01  ATAB-PCB                    PIC X.                                   
038800                                                                          
038900*01  -COPY W0008           -PRE WDK6-                                     
039000     05  FILLER      PIC X.                                               
039100*01  -COPY W0008           -PRE WDD8B-                                    
039200     05  FILLER      PIC X.                                               
039300*01  -COPY W0008           -PRE WDE4C-                                    
039400     05  FILLER      PIC X.                                               
039500*01  -COPY W0008           -PRE WDE4-                                     
039600     05  FILLER      PIC X.                                               
039700*01  -COPY W0008           -PRE INV-                                      
039800     05  FILLER      PIC X.                                               
039900*01  -COPY W0008           -PRE BEN-                                      
040000     05  FILLER      PIC X.                                               
040100*01  -COPY W0008           -PRE SATB-.                                    
040200     05    FILLER    PIC X.                                               
040300*01  -COPY W0008           -PRE WDK7-.                                    
040400     05    FILLER    PIC X.                                               
040500*01  -COPY W0008           -PRE INVC-.                                    
040600     05    FILLER    PIC X.                                               
040700*01  -COPY W0008           -PRE WDG3-.                                    
040800     05    FILLER    PIC X.                                               
040900*01  -COPY W0008           -PRE WDB6-.                                    
041000     05    FILLER    PIC X.                                               
041100*01  -COPY W0008           -PRE WDE6-.                                    
041200     05    FILLER    PIC X.                                               
041300     EJECT                                                                
041400 PROCEDURE DIVISION USING MSG-PCB DISTRDOC-PCB DISTRSAV-PCB               
041500         ATAB-PCB WDK6-PCB WDD8B-PCB WDE4C-PCB WDE4-PCB                   
041600         INV-PCB BEN-PCB SATB-PCB WDK7-PCB                                
041700         INVC-PCB WDG3-PCB WDB6-PCB WDE6-PCB.                             
041800 MAIN SECTION.                                                            
041900     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB DISTRSAV-PCB              
042000         ATAB-PCB WDK6-PCB WDD8B-PCB WDE4C-PCB WDE4-PCB                   
042100         INV-PCB BEN-PCB SATB-PCB WDK7-PCB                                
042200         INVC-PCB WDG3-PCB WDB6-PCB WDE6-PCB.                             
042300                                                                          
042400     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
042500                                                                          
042600     IF SUB-KDRC = 0                                                      
042700                                                                          
042800        PERFORM A-INIT                                                    
042900                                                                          
043000        IF KEYS-OK                                                        
043100          IF SUB-KDTRANS(1:7) = 'WL0173U' OR                              
043200             (SUB-KDTRANS(1:6) = 'WLA173' AND REQU-UPDATE)                
043300            PERFORM S90-OPEN-DAP-SEND                                     
043400          END-IF                                                          
043500                                                                          
043600          PERFORM B-BEARBETNING                                           
043700                                                                          
043800          IF SUB-KDTRANS(1:7) = 'WL0173U' OR                              
043900             (SUB-KDTRANS(1:6) = 'WLA173' AND REQU-UPDATE)                
044000            PERFORM S90-CLOSE-DAP-SEND                                    
044100          END-IF                                                          
044200        END-IF                                                            
044300                                                                          
044400        IF SUB-KDTRANS(1:6) = 'WLA173'                                    
044500          PERFORM C-MOVE-TO-RESP                                          
044600          PERFORM S11-MSG-CONV                                            
044700          PERFORM S02-RETURN-RESPONSE                                     
044800        END-IF                                                            
044900                                                                          
045000     END-IF                                                               
045100                                                                          
045200     MOVE ZERO TO RETURN-CODE                                             
045300     GOBACK                                                               
045400     .                                                                    
045500     EJECT                                                                
045600 A-INIT SECTION.                                                          
045700     MOVE 'A-INIT' TO CURR-SECTION                                        
045800                                                                          
045900     MOVE YES                    TO KEYS-SW                               
046000                                                                          
046100     IF SUB-KDTRANS(1:6) = 'WLA173'                                       
046200       PERFORM AA-INIT-API                                                
046300     END-IF                                                               
046400                                                                          
046500     MOVE SPACE                       TO DOC-WL01731                      
046600     MOVE 'LINE'                      TO DOC-IDAFPRCD                     
046700     MOVE REQU-IDDC-L173              TO DOC-IDDC                         
046800                                                                          
046900     ACCEPT DAGENS-DATUM FROM DATE                                        
047000     ACCEPT DAGENS-TID   FROM TIME                                        
047100******** ADAPT DATE AND TIME FOR TIMEZONES                                
047200         MOVE REQU-IDDC-L173     TO W-IDDC-B6                             
047300         PERFORM IMS-GU-WDB601                                            
047400                                                                          
047500         MOVE '011'                TO MSGI-KDCALL                         
047600         MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                       
047600         MOVE DCS-IDDC             TO MSGI-IDDC                           
047700         MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                       
047800         MOVE DAGENS-TID           TO MSGI-TILOKTID                       
047900         CALL WL01TIDZ USING          MSGI-WL01TIDZ                       
048000           MOVE MSGI-TILOKDAT(1:6) TO DAGENS-DATUM                        
048100           MOVE MSGI-TILOKTID      TO DAGENS-TID                          
048200********                                                                  
048300     MOVE DAGENS-DATUM                TO DOC-TIUTSKR                      
048400     MOVE DAGENS-TID(5:4)             TO DOC-TIUTSTID(1:4)                
048500                                                                          
048600     MOVE 001             TO HDR-REQU-IDMSGVER                            
048700     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
048800     MOVE REQU-IDUSER     TO HDR-REQU-IDUSER                              
048900                                                                          
049000     MOVE 'INVENTORY-SINGL'   TO HDR-IDOUTTYPE                            
049100     MOVE SPACE               TO HDR-IDOUTREC                             
049200     MOVE REQU-IDDC-L173      TO HDR-IDOUTREC(1:2)                        
049300     MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                        
049400     MOVE FUNCTION CURRENT-DATE(1:8)  TO WS-DAGENS-DATUM                  
049500     .                                                                    
049600                                                                          
049700 AA-INIT-API SECTION.                                                     
049800                                                                          
049900     MOVE ALL '+'   TO RESP-AREA                                          
050000     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
050100                       RESP-IDMSG-INFO                                    
050200                       RESP-IDELMT-ERROR                                  
050300     MOVE 001       TO RESP-IDRESVER                                      
050400                                                                          
050500     MOVE ALL '+'   TO CURRENT-OUTPUT                                     
050600     MOVE SPACE     TO CURRO-IDMSG-ERROR                                  
050700                       CURRO-IDMSG-INFO                                   
050800                       CURRO-IDELMT-ERROR                                 
050900                                                                          
051000     MOVE ZERO      TO WS-API-TIJUSTDA                                    
051100                                                                          
051200     MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT)     TO                      
051300                               REQU-KDPGMACT                              
051400     MOVE FUNCTION UPPER-CASE (REQU-IDDC-L173)    TO                      
051500                               REQU-IDDC-L173                             
051600                                                                          
051700     MOVE 001                    TO AUTH-KDCALL                           
051800     CALL WZ01AUTH            USING AUTH-WZ01AUTH                         
051900                                    REQU-WZ01REQ2                         
052000     IF AUTH-KDRC > 0                                                     
052100       MOVE ERR-UNAUTHORIZED     TO CURRO-IDMSG-ERROR                     
052200       MOVE NOO                  TO KEYS-SW                               
052300     END-IF                                                               
052400                                                                          
052500     .                                                                    
052600                                                                          
052700 B-BEARBETNING SECTION.                                                   
052800     MOVE 'B-BEARBETNING' TO CURR-SECTION                                 
052900                                                                          
053000     MOVE REQU-IDDC-L173             TO W-IDDC                            
053100                                        W-IDDC-G3                         
053200                                        W-IDDC-WDH1-MIN                   
053300                                        W-IDDC-WDH1-MAX                   
053400                                                                          
053500     MOVE +1 TO IX                                                        
053600     PERFORM UNTIL IX > IX-MAX OR                                         
053700                   REQU-ART-PRINT(IX) = ALL '+'                           
053800        IF REQU-IDARTNR-L173(IX) NOT NUMERIC                              
053900          MOVE ZERO TO REQU-IDARTNR-L173(IX)                              
054000        END-IF                                                            
054100        MOVE REQU-IDARTNR-L173(IX)   TO W-IDARTNR                         
054200                                        W-IDARTNR-LOW                     
054300                                        W-IDARTNR-HIGH                    
054400                                        W-IDARTNR-D8-MIN                  
054500                                        W-IDARTNR-D8-MAX                  
054600        MOVE REQU-KDINVKAT-L173(IX)  TO W-KDINVKAT-WDH1-MIN               
054700                                        W-KDINVKAT-WDH1-MAX               
054800                                                                          
054900        MOVE REQU-IDARTNR-L173(IX)   TO DOC-IDARTNR                       
055000                                                                          
055100        PERFORM BA-HAEMTA-ANM-WDH1                                        
055200        IF SEGMENT-MISSING                                                
055300           IF IX = 1 AND SUB-KDTRANS(1:6) = 'WLA173'                      
055400             IF REQU-QUERY                                                
055500               MOVE '221'     TO CURRO-IDMSG-ERROR                        
055600***            NO INVENTORY INFORMATION FOUND                             
055700             ELSE                                                         
055800               MOVE '250'     TO CURRO-IDMSG-ERROR                        
055900***            NOTHING PRINTED                                            
056000             END-IF                                                       
056100           END-IF                                                         
056200           MOVE +999 TO IX                                                
056300        ELSE                                                              
056400           MOVE WS-IDPRTINV-NUM   TO HDR-IDLIST                           
056500****       MOVE 5102-IDLOPNR  TO HDR-IDLIST                               
056600           IF HEADER-NOT-PRINTED                                          
056700             IF SUB-KDTRANS(1:7) = 'WL0173U' OR                           
056800                (SUB-KDTRANS(1:6) = 'WLA173' AND REQU-UPDATE)             
056900               PERFORM S90-PUT-DAP-HEADER                                 
057000             END-IF                                                       
057100             MOVE YES TO HEADER-SW                                        
057200           END-IF                                                         
057300           PERFORM BB-NOLLA-TABELL                                        
057400           PERFORM BC-HAEMTA-UPPG-WDK6-WDK7                               
057500           PERFORM BD-HAEMTA-UPPG-WDD8                                    
057600           PERFORM BE-HAEMTA-UPPG-WDD3                                    
057700           PERFORM BF-HAEMTA-WDE4-OVR-DISTR                               
057800           IF BUFFERT-PRINT                                               
057900              PERFORM BG-SKRIV-EXTRA-BUFFRADER                            
058000           END-IF                                                         
058100           IF SUB-KDTRANS(1:7) = 'WL0173U' OR                             
058200              (SUB-KDTRANS(1:6) = 'WLA173' AND REQU-UPDATE)               
058300             PERFORM S90-PUT-DOC                                          
058400           END-IF                                                         
058500                                                                          
058600           ADD +1 TO IX                                                   
058700        END-IF                                                            
058800     END-PERFORM                                                          
058900     .                                                                    
059000                                                                          
059100 BA-HAEMTA-ANM-WDH1 SECTION.                                              
059200     MOVE 'BA-HAEMTA-ANM' TO CURR-SECTION                                 
059300                                                                          
059400     MOVE SPACE  TO DOC-TEINVANM                                          
059500     PERFORM IMS-01-GHU-WDH101                                            
059600     IF SEGMENT-FOUND                                                     
059700        PERFORM IMS-02-GHNP-WDH111                                        
059800        IF SEGMENT-FOUND                                                  
059900****       START, FIXA TILL FÖRSTA ARTIKELNS PRINT-ID,                    
060000***        BLIR SAMMA PRINT-ID FÖR ALLA RADERNA                           
060100           IF IX = 1                                                      
060200*--- EN DUBBLETT                                                          
060300             IF INV-IDLOPNR > 0                                           
060400                MOVE INV-IDPRTOMG  TO WS-IDPRTINV-NUM(1:1)                
060500                                      WS-IDPRTOMG                         
060600                MOVE INV-IDLOPNR   TO WS-IDLOPNR-5                        
060700                                      WS-IDLOPNR                          
060800                                      WS-IDPRTINV-NUM(2:5)                
060900             ELSE                                                         
061000              IF SUB-KDTRANS(1:7) = 'WL0173U' OR                          
061100                 (SUB-KDTRANS(1:6) = 'WLA173' AND REQU-UPDATE)            
061200                IF INV-IDPRTOMG = +0 OR +1 OR +2                          
061300                   EVALUATE INV-IDPRTOMG                                  
061400                   WHEN +0 MOVE 1   TO WS-IDPRTINV-NUM(1:1)               
061500                                       WS-IDPRTOMG                        
061600                   WHEN +1 MOVE 2   TO WS-IDPRTINV-NUM(1:1)               
061700                                       WS-IDPRTOMG                        
061800                   WHEN +2 MOVE 3   TO WS-IDPRTINV-NUM(1:1)               
061900                                       WS-IDPRTOMG                        
062000                   WHEN +3 MOVE 4   TO WS-IDPRTINV-NUM(1:1)               
062100                                       WS-IDPRTOMG                        
062200                   END-EVALUATE                                           
062300                   MOVE WS-IDPRTOMG     TO W-IDPRTOMG-ALFA                
062400****  HÄMTA NÄSTA INV-IDLOPNR FRÅN HÄNDELSEBASEN.                         
062500                   PERFORM IMS-03-GHU-WDGX5102                            
062600                   IF SEGMENT-MISSING                                     
062700                      PERFORM IMS-04-GU-WDG301                            
062800                      MOVE +1           TO 5102-KDSEGKEY                  
062900                      MOVE +1           TO 5102-IDLOPNR                   
063000                      MOVE 5102-IDLOPNR TO WS-IDPRTINV-NUM(2:5)           
063100                      PERFORM IMS-05-ISRT-WDGX5102                        
063200                   ELSE                                                   
063300                      ADD +1 TO 5102-IDLOPNR                              
063400                      MOVE 5102-IDLOPNR   TO WS-IDLOPNR-5                 
063500                                             WS-IDLOPNR                   
063600                      MOVE WS-IDLOPNR-5   TO WS-IDPRTINV-NUM(2:5)         
063700                      PERFORM IMS-06-REPL-WDGX5102                        
063800                   END-IF                                                 
063900                ELSE                                                      
064000                   MOVE ZERO           TO WS-IDPRTINV                     
064100                   MOVE ZERO           TO WS-IDLOPNR                      
064200                END-IF                                                    
064300              ELSE                                                        
064400                MOVE INV-IDPRTOMG  TO WS-IDPRTINV-NUM(1:1)                
064500                                      WS-IDPRTOMG                         
064600                MOVE INV-IDLOPNR   TO WS-IDLOPNR-5                        
064700                                      WS-IDLOPNR                          
064800                                      WS-IDPRTINV-NUM(2:5)                
064900              END-IF                                                      
065000             END-IF                                                       
065100           END-IF                                                         
065200           MOVE WS-IDPRTINV-NUM    TO DOC-IDPRINTINV                      
065300****   PRINTID SLUT                                                       
065400                                                                          
065500           MOVE INV-KDINVPRIO       TO CURRO-KDINVPRIO                    
065600           MOVE INV-KDINVKAT        TO DOC-KDINVKAT                       
065700           MOVE INV-TISEGKEY        TO WS-TIREGDAT                        
065800           MOVE WS-TIREGDAT(3:6)    TO DOC-TIREGDAT                       
065900           IF DOC-TIREGDAT = '000000'                                     
066000             MOVE SPACE             TO DOC-TIREGDAT                       
066100           END-IF                                                         
066200           MOVE INV-TEINVANM        TO DOC-TEINVANM                       
066300           MOVE ZERO                TO INV-IDLOPNR                        
066400                                       INV-KVAKS-OLD                      
066500                                       INV-KVEFRS-OLD                     
066600                                       INV-KVLS-OLD                       
066700           IF INV-FLINVSKR = 'N'                                          
066800              MOVE 'J'              TO INV-FLINVSKR                       
066900              MOVE SPACE            TO DOC-BETEXT                         
067000           ELSE                                                           
067100              MOVE 'DUPLICATE'      TO DOC-BETEXT                         
067200              IF INV-FLINVSKR = 'J' AND                                   
067300                 SUB-KDTRANS(1:6) = 'WLA173' AND REQU-UPDATE              
067400                 MOVE '271'     TO CURRO-IDMSG-INFO                       
067500***              INVENTORY ALREADY EXIST, PRINTED                         
067600              END-IF                                                      
067700           END-IF                                                         
067800        END-IF                                                            
067900     END-IF                                                               
068000     .                                                                    
068100 BB-NOLLA-TABELL       SECTION.                                           
068200     MOVE 'BB-NOLLA-TABELL' TO CURR-SECTION                               
068300                                                                          
068400     MOVE SPACE  TO SDC-TAB-EFR                                           
068500                                                                          
068600*    INITIALIZE THE DOC-AREA                                              
068700*                                                                         
068800     MOVE +1          TO  DOC-IX                                          
068900     PERFORM UNTIL DOC-IX > MAX-IX-7                                      
069000       MOVE ZEROES    TO  DOC-IDDISTR  (DOC-IX)                           
069100                          DOC-IDKUNDNR (DOC-IX)                           
069200                          DOC-KVLEVART (DOC-IX)                           
069300       MOVE SPACES    TO  DOC-IDKUNDRF (DOC-IX)                           
069400       ADD  +1        TO  DOC-IX                                          
069500     END-PERFORM                                                          
069600     .                                                                    
069700 BC-HAEMTA-UPPG-WDK6-WDK7     SECTION.                                    
069800     MOVE 'BC-HAEMTA-UPPG-WDK6-WDK7' TO CURR-SECTION                      
069900                                                                          
070000     MOVE ZERO                        TO DOC-KVLS                         
070100                                         DOC-KVAKS                        
070200                                         DOC-KVUTRS                       
070300                                         DOC-TIJUSTDA                     
070400                                         DOC-KVJUSTKV                     
070500                                         DOC-KDJUSTYP                     
070600     MOVE SPACE                       TO DOC-KDJUSTYP-A                   
070700     PERFORM IMS-07-GET-ART-WDK6                                          
070800     MOVE ART-KDPRODSL                TO DOC-KDPRODSL                     
070900     MOVE ART-KDSORT                  TO DOC-KDSORT                       
071000                                                                          
071100     PERFORM IMS-08-GNP-CLAGERINFO                                        
071200     MOVE CLAG-KVROS                  TO DOC-KVROS                        
071300     MOVE CLAG-VKART                  TO DOC-VKART                        
071400     IF DCS-CDC                                                           
071500       MOVE CLAG-KVLS                 TO DOC-KVLS                         
071600       MOVE CLAG-KVAKS-CDC            TO DOC-KVAKS                        
071700       MOVE CLAG-KVUTRS               TO DOC-KVUTRS                       
071800       MOVE CLAG-ADLAGOMR             TO DOC-ADLAGOMR                     
071900       MOVE CLAG-ADGANG               TO DOC-ADGANG                       
072000       MOVE CLAG-ADPLATS              TO DOC-ADPLATS                      
072100       MOVE CLAG-PRARTSTD             TO DOC-PRARTSTD                     
072200                                                                          
072300       MOVE CLAG-KVLS                         TO INV-KVLS-OLD             
072400       MOVE CLAG-KVAKS-CDC                    TO INV-KVAKS-OLD            
072500       MOVE WS-IDLOPNR                        TO INV-IDLOPNR              
072600     ELSE                                                                 
072700       PERFORM IMS-09-GET-ART-WDK7                                        
072800       IF SEGMENT-FOUND                                                   
072900          MOVE SLAG-KVLS                TO DOC-KVLS                       
073000          MOVE SLAG-KVAKS-SDC           TO DOC-KVAKS                      
073100          MOVE SLAG-KVUTRS              TO DOC-KVUTRS                     
073200          MOVE SLAG-ADLAGOMR            TO DOC-ADLAGOMR                   
073300          MOVE SLAG-ADGANG              TO DOC-ADGANG                     
073400          MOVE SLAG-ADPLATS             TO DOC-ADPLATS                    
073500                                                                          
073600          MOVE SLAG-KVLS                        TO INV-KVLS-OLD           
073700          MOVE SLAG-KVAKS-SDC                   TO INV-KVAKS-OLD          
073800          MOVE WS-IDLOPNR                       TO INV-IDLOPNR            
073900       END-IF                                                             
074000     END-IF                                                               
074100     IF SEGMENT-FOUND                                                     
074200       IF WS-IDPRTOMG NOT = INV-IDPRTOMG                                  
074300         EVALUATE WS-IDPRTOMG                                             
074400           WHEN 1                                                         
074500             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR1          
074600           WHEN 2                                                         
074700             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR2          
074800           WHEN 3                                                         
074900             MOVE FUNCTION CURRENT-DATE(1:8) TO INV-DAREGDAT-PR3          
075000         END-EVALUATE                                                     
075100       END-IF                                                             
075200       MOVE WS-IDPRTOMG                      TO INV-IDPRTOMG              
075300                                                                          
075400       PERFORM IMS-10-GET-INVHIST-ROT                                     
075500       IF SEGMENT-FOUND                                                   
075600          PERFORM IMS-11-GET-INVHIST-SDC                                  
075700          IF SEGMENT-FOUND                                                
075800             PERFORM S10-KONV-DATUM                                       
075900             MOVE DAT-TIAAVVD        TO DOC-TIJUSTDA                      
076000             IF SUB-KDTRANS = 'WLA173'                                    
076100               MOVE DAT-TIAAMMDD     TO WS-API-TIJUSTDA                   
076200             END-IF                                                       
076300             MOVE INVH-KVJUSTKV      TO DOC-KVJUSTKV                      
076400             MOVE INVH-KDJUSTYP      TO DOC-KDJUSTYP                      
076500             IF INVH-FLAUTLSJ = 'J'                                       
076600                MOVE 'A'             TO DOC-KDJUSTYP-A                    
076700             ELSE                                                         
076800                MOVE ' '             TO DOC-KDJUSTYP-A                    
076900             END-IF                                                       
077000          END-IF                                                          
077100       END-IF                                                             
077200     END-IF                                                               
077300     .                                                                    
077400 BD-HAEMTA-UPPG-WDD8 SECTION.                                             
077500     MOVE 'BD-HAEMTA-UPPG-WDD8' TO CURR-SECTION                           
077600                                                                          
077700     MOVE NOO                   TO BUFFERT-SW                             
077800     MOVE REQU-IDDC-L173        TO W-IDDC-D8-MIN                          
077900                                   W-IDDC-D8-MAX                          
078000     MOVE +1 TO SALDO-IX                                                  
078100     PERFORM IMS-GU-WDD8B1                                                
078200     PERFORM UNTIL SALDO-IX > +4                                          
078300                                                                          
078400       IF SEGMENT-FOUND                                                   
078500                                                                          
078600        MOVE SEQB-ADBUFFOMR  TO DOC-ADBUFFOMR(SALDO-IX)                   
078700                                CURRO-ADBUFFOMR (SALDO-IX)                
078800        MOVE SEQB-ADBUFFGANG TO DOC-ADBUFFGANG(SALDO-IX)                  
078900                                CURRO-ADBUFFGANG(SALDO-IX)                
079000        MOVE SEQB-ADBUFFPL   TO DOC-ADBUFFPL(SALDO-IX)                    
079100                                CURRO-ADBUFFPL  (SALDO-IX)                
079200        MOVE SEQB-KVBUFF-F   TO CURRO-KVBUFF-F(SALDO-IX)                  
079300        MOVE SEQB-KVKOLLI-F  TO CURRO-KVKOLLI-F(SALDO-IX)                 
079400        ADD +1               TO WS-ANTAL-RADER-BUF                        
079500                                                                          
079600        PERFORM IMS-GN-WDD8B1                                             
079700       ELSE                                                               
079800        MOVE +0   TO DOC-ADBUFFOMR(SALDO-IX)                              
079900        MOVE +0   TO DOC-ADBUFFGANG(SALDO-IX)                             
080000        MOVE +0   TO DOC-ADBUFFPL(SALDO-IX)                               
080100       END-IF                                                             
080200       ADD +1 TO SALDO-IX                                                 
080300     END-PERFORM                                                          
080400     IF SEGMENT-FOUND                                                     
080500        MOVE YES TO BUFFERT-SW                                            
080600     END-IF                                                               
080700     .                                                                    
080800 BE-HAEMTA-UPPG-WDD3     SECTION.                                         
080900     MOVE 'BD-HAEMTA-UPPG-WDD3' TO CURR-SECTION                           
081000                                                                          
081100     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
081200     IF DCS-UNICODE-IDSKYLT                                               
081300        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
081400     ELSE                                                                 
081500        MOVE '278 '             TO TRAUTF8-KDCP                           
081600     END-IF                                                               
081700     PERFORM IMS-14-GU-BEN-SEQ                                            
081800     IF SEGMENT-FOUND                                                     
081900        MOVE BEN-TEXT-BEART     TO TRAUTF8-TECONV-FROM                    
082000     ELSE                                                                 
082100        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
082200        MOVE '278 '             TO TRAUTF8-KDCP                           
082300     END-IF                                                               
082400     IF TRAUTF8-TECONV-FROM = SPACES                                      
082500      MOVE 'GB'  TO W-IDSKYLT                                             
082600      MOVE '278' TO TRAUTF8-KDCP                                          
082700      PERFORM IMS-14-GU-BEN-SEQ                                           
082800      MOVE BEN-TEXT-BEART    TO TRAUTF8-TECONV-FROM                       
082900     END-IF                                                               
083000     MOVE 25                    TO TRAUTF8-KVMAXTL                        
083100     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
083200     MOVE TRAUTF8-TECONV-TO     TO DOC-BEART                              
083300     .                                                                    
083400 BF-HAEMTA-WDE4-OVR-DISTR SECTION.                                        
083500     MOVE 'BF-HAEMTA-WDE4-O' TO CURR-SECTION                              
083600                                                                          
083700     MOVE +1 TO INDX                                                      
083800     MOVE +0 TO WS-KVEFRS-OLD                                             
083900     MOVE +0 TO WS-ANTAL-RADER-EFR                                        
084000     MOVE +0 TO WS-SDC-ANTAL                                              
084100     PERFORM IMS-15-GU-WDE4C1                                             
084200                                                                          
084300     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DB                           
084400                   OR INDX > MAX-IX-7                                     
084500                                                                          
084600        MOVE SEQC-IDDISTR   TO W-401-IDDISTR                              
084700        MOVE SEQC-IDKUNDNR  TO W-401-IDKUNDNR                             
084800        MOVE SEQC-IDORDNR5  TO W-401-IDORDNR                              
084900        MOVE SEQC-IDPRODNR  TO W-401-IDPRODNR                             
085000        MOVE SEQC-IDPLKLST  TO W-401-IDPLKLST                             
085100        MOVE SEQC-IDPURAD   TO W-411-IDPURAD                              
085200        PERFORM IMS-GU-WDE401-11                                          
085300                                                                          
085400        IF (ORAD-FLDIRLEV NOT = 'J')                                      
085500           IF KORD-IDDC = REQU-IDDC-L173                                  
085600             IF KORD-IDDISTR NOT = +98                                    
085700               MOVE KORD-IDKUNDRF TO WS-IDKUNDRF                          
085800                                                                          
085900               MOVE KORD-IDPRODNR TO W-IDPRODNR                           
086000               PERFORM IMS-25-GU-WDE601                                   
086100                                                                          
086200               MOVE WS-IDORDNR          TO WSDC-IDKUNDRF (INDX)           
086300               IF VORD-KDMETOD = +3                                       
086400                 IF ORAD-KVLEVART NOT = ORAD-KVAVBART                     
086500                   COMPUTE WS-SDC-ANTAL =                                 
086600                   (ORAD-KVAVBART - ORAD-KVLEVART)                        
086700                 ELSE                                                     
086800                   MOVE ORAD-KVAVBART TO WS-SDC-ANTAL                     
086900                 END-IF                                                   
087000               ELSE                                                       
087100                 COMPUTE WS-SDC-ANTAL =                                   
087200                 (ORAD-KVAVBART - ORAD-KVLEVART)                          
087300               END-IF                                                     
087400                                                                          
087500               MOVE WS-SDC-ANTAL        TO WSDC-KVLEVART (INDX)           
087600               COMPUTE WS-KVEFRS-OLD =                                    
087700                       WS-KVEFRS-OLD + WS-SDC-ANTAL                       
087800                                                                          
087900               MOVE KORD-IDDISTR   TO WSDC-IDDISTR  (INDX)                
088000               MOVE KORD-IDKUNDNR  TO WSDC-IDKUNDNR (INDX)                
088100               ADD +1 TO INDX                                             
088200               ADD +1 TO WS-ANTAL-RADER-EFR                               
088300             END-IF                                                       
088400           END-IF                                                         
088500        END-IF                                                            
088600        PERFORM IMS-16-GN-WDE4C1                                          
088700     END-PERFORM                                                          
088800* NU HAR VI ALLA SALDO, GÖR REPLACE PÅ WDH1 MED AKTUELLA VÄRDEN           
088900* OM EFR SKALL DELAS MED 2 LÄGG TILL EN RÄKNARE I SLINGAN OVAN            
089000* EFR SKALL INTE RÄKNAS OM ENL SUSSI 060703                               
089100     MOVE +0                               TO INV-KVEFRS-OLD              
089200*    IF WS-ANTAL-RADER-EFR < 2                                            
089300       MOVE WS-KVEFRS-OLD                  TO INV-KVEFRS-OLD              
089400*    ELSE                                                                 
089500*      COMPUTE INV-KVEFRS-OLD = WS-KVEFRS-OLD / 2                         
089600*    END-IF                                                               
089700                                                                          
089800**** TA BORT POSTEN PÅ WDH1 OCH SKAPA SEDAN ETT NYTT MED                  
089900**** DAGENS DATUM I DAREGDAT-SORT OM DET ÄR FÖRSTA PRINTNINGEN            
090000     IF SUB-KDTRANS(1:7) = 'WL0173U' OR                                   
090100        (SUB-KDTRANS(1:6) = 'WLA173' AND REQU-UPDATE)                     
090200       IF WS-IDPRTOMG = 1                                                 
090300         PERFORM IMS-GNP-WDH121                                           
090400         IF SEGMENT-FOUND                                                 
090500           IF INVL-KDSEGKEY = 0                                           
090600             MOVE INVL-IDUSER     TO WS-IDUSER                            
090700           END-IF                                                         
090800         END-IF                                                           
090900         PERFORM IMS-01-GHU-WDH101                                        
091000         MOVE DLI-IO-AREA-INVA11 TO SPAR-WDH111-AREA                      
091100         PERFORM IMS-02-GHNP-WDH111                                       
091200                                                                          
091300         PERFORM IMS-DELETE-WDH111                                        
091400         MOVE WS-DAGENS-DATUM TO SPAR-INV-DAREGDAT-SORT                   
091500                                                                          
091600         PERFORM IMS-01-GHU-WDH101                                        
091700         MOVE SPAR-WDH111-AREA TO DLI-IO-AREA-INVA11                      
091800         PERFORM IMS-INSERT-WDH111                                        
091900                                                                          
092000         IF SEGMENT-EXISTS                                                
092100           PERFORM UNTIL SEGMENT-FOUND                                    
092200             IF SEGMENT-EXISTS OR INDEX-EXISTS                            
092300              ADD 1                TO INV-TISEGKEY                        
092400              PERFORM IMS-INSERT-WDH111                                   
092500             END-IF                                                       
092600           END-PERFORM                                                    
092700         END-IF                                                           
092800                                                                          
092900         MOVE '0'            TO INVL-KDSEGKEY                             
093000         MOVE WS-IDUSER      TO INVL-IDUSER                               
093100         PERFORM IMS-INSERT-WDH121                                        
093200         MOVE '1'            TO INVL-KDSEGKEY                             
093300         MOVE REQU-IDUSER    TO INVL-IDUSER                               
093400         PERFORM IMS-INSERT-WDH121                                        
093500         MOVE '2'            TO INVL-KDSEGKEY                             
093600         MOVE SPACE          TO INVL-IDUSER                               
093700         PERFORM IMS-INSERT-WDH121                                        
093800         MOVE '3'            TO INVL-KDSEGKEY                             
093900         MOVE SPACE          TO INVL-IDUSER                               
094000         PERFORM IMS-INSERT-WDH121                                        
094100       ELSE                                                               
094200         PERFORM IMS-17-REPLACE                                           
094300         PERFORM IMS-GNP-WDH121                                           
094400                                                                          
094500         IF SEGMENT-FOUND                                                 
094600           PERFORM UNTIL SEGMENT-MISSING                                  
094700             IF W-IDPRTOMG-ALFA =          INVL-KDSEGKEY                  
094800               MOVE REQU-IDUSER         TO INVL-IDUSER                    
094900               PERFORM IMS-REPLACE-WDH121                                 
095000             END-IF                                                       
095100             PERFORM IMS-GNP-WDH121                                       
095200           END-PERFORM                                                    
095300         END-IF                                                           
095400       END-IF                                                             
095500     END-IF                                                               
095600**                                                                        
095700     MOVE +1 TO WSDC-IX                                                   
095800     PERFORM UNTIL WSDC-IX > MAX-IX-7 OR                                  
095900                   WSDC-TAB (WSDC-IX) = SPACE                             
096000                                                                          
096100        MOVE WSDC-IDKUNDRF(WSDC-IX) TO DOC-IDKUNDRF(WSDC-IX)              
096200        MOVE WSDC-IDKUNDNR(WSDC-IX) TO DOC-IDKUNDNR(WSDC-IX)              
096300        MOVE WSDC-IDDISTR (WSDC-IX) TO DOC-IDDISTR (WSDC-IX)              
096400        MOVE WSDC-KVLEVART(WSDC-IX) TO DOC-KVLEVART(WSDC-IX)              
096500        ADD +1 TO WSDC-IX                                                 
096600     END-PERFORM                                                          
096700     .                                                                    
096800 BG-SKRIV-EXTRA-BUFFRADER SECTION.                                        
096900     MOVE 'BG-SKRIV-EXTRA-B' TO CURR-SECTION                              
097000                                                                          
097100**** SEGMENT MISSING KAN FINNAS I DE ANDRA SEKTIONERNA                    
097200**** SÅ MAN FÅR LÄSA OM HÄR                                               
097300     MOVE +1 TO SALDO-IX                                                  
097400     PERFORM IMS-GU-WDD8B1                                                
097500     PERFORM UNTIL SALDO-IX > 4                                           
097600       PERFORM IMS-GN-WDD8B1                                              
097700       ADD +1 TO SALDO-IX                                                 
097800     END-PERFORM                                                          
097900                                                                          
098000     MOVE +1 TO SALDO-IX                                                  
098100     PERFORM UNTIL SALDO-IX > MAX-IX-2                                    
098200        IF SEGMENT-FOUND                                                  
098300                                                                          
098400           MOVE SEQB-ADBUFFOMR    TO DOC-BU-ADBUFFOMR (SALDO-IX)          
098500           MOVE SEQB-ADBUFFGANG   TO DOC-BU-ADBUFFGANG(SALDO-IX)          
098600           MOVE SEQB-ADBUFFPL     TO DOC-BU-ADBUFFPL  (SALDO-IX)          
098700           PERFORM IMS-GN-WDD8B1                                          
098800        ELSE                                                              
098900           MOVE ZERO              TO DOC-BU-ADBUFFOMR (SALDO-IX)          
099000           MOVE ZERO              TO DOC-BU-ADBUFFGANG(SALDO-IX)          
099100           MOVE ZERO              TO DOC-BU-ADBUFFPL  (SALDO-IX)          
099200        END-IF                                                            
099300        ADD +1 TO SALDO-IX                                                
099400     END-PERFORM                                                          
099500     .                                                                    
099600     SKIP3                                                                
099700 C-MOVE-TO-RESP SECTION.                                                  
099800     MOVE CURRO-IDMSG-INFO   TO RESP-IDMSG-INFO                           
099900     MOVE CURRO-IDMSG-ERROR  TO RESP-IDMSG-ERROR                          
100000     MOVE CURRO-IDELMT-ERROR TO RESP-IDELMT-ERROR                         
100100     MOVE DOC-IDDC           TO RESP-IDDC                                 
100200     MOVE REQU-IDARTNR-L173(1) TO RESP-IDARTNR                            
100300                                                                          
100400     IF CURRO-IDMSG-ERROR = SPACES                                        
100500       MOVE CURRO-KDINVPRIO  TO RESP-KDINVPRIO                            
100600     ELSE                                                                 
100700       MOVE ZERO             TO RESP-KDINVPRIO                            
100800     END-IF                                                               
100900     MOVE DOC-KDINVKAT       TO RESP-KDINVKAT                             
101000                                                                          
101100     IF DOC-IDDC = 11                                                     
101200      MOVE DOC-PRARTSTD      TO RESP-PRARTSTD                             
101300     END-IF                                                               
101400     IF CURRO-IDMSG-ERROR = SPACES                                        
101500       MOVE DOC-TIREGDAT(1:2) TO WS-API-YEAR                              
101600       MOVE DOC-TIREGDAT(3:2) TO WS-API-MONTH                             
101700       MOVE DOC-TIREGDAT(5:2) TO WS-API-DAY                               
101800       MOVE WS-API-DATE       TO RESP-DAREGDAT                            
101900     ELSE                                                                 
102000       MOVE SPACE             TO RESP-DAREGDAT                            
102100     END-IF                                                               
102200                                                                          
102300     IF CURRO-IDMSG-ERROR = SPACES                                        
102400       MOVE DOC-TIUTSKR(1:2)  TO WS-API-YEAR                              
102500       MOVE DOC-TIUTSKR(3:2)  TO WS-API-MONTH                             
102600       MOVE DOC-TIUTSKR(5:2)  TO WS-API-DAY                               
102700       MOVE WS-API-DATE       TO RESP-TIUTSKR                             
102800     ELSE                                                                 
102900       MOVE SPACE             TO RESP-TIUTSKR                             
103000     END-IF                                                               
103100                                                                          
103200     IF CURRO-IDMSG-ERROR = SPACES                                        
103300       MOVE DAGENS-TID(5:2)   TO WS-API-HH                                
103400       MOVE DAGENS-TID(7:2)   TO WS-API-MM                                
103500       MOVE ZERO              TO WS-API-SS                                
103600       MOVE WS-API-TIME       TO RESP-TIUTSTID                            
103700     ELSE                                                                 
103800       MOVE SPACE             TO RESP-TIUTSTID                            
103900     END-IF                                                               
104000                                                                          
104100     MOVE DOC-IDPRINTINV     TO RESP-IDPRINTINV                           
104200                                                                          
104300     MOVE DOC-KDPRODSL       TO RESP-KDPRODSL                             
104400     MOVE DOC-KVUTRS         TO RESP-KVUTRS                               
104500     MOVE DOC-BEART          TO RESP-BEART                                
104600     MOVE DOC-KVAKS          TO RESP-KVAKS                                
104700     MOVE DOC-ADLAGOMR       TO RESP-ADLAGOMR                             
104800     MOVE DOC-ADGANG         TO RESP-ADGANG                               
104900     MOVE DOC-ADPLATS        TO RESP-ADPLATS                              
105000     MOVE DOC-KVLS           TO RESP-KVLS                                 
105100     MOVE DOC-KDSORT         TO RESP-KDSORT                               
105200     MOVE DOC-VKART          TO RESP-VKART                                
105300                                                                          
105400     IF CURRO-IDMSG-ERROR = SPACES AND WS-API-TIJUSTDA > ZERO             
105500       MOVE WS-API-TIJUSTDA(1:2) TO WS-API-YEAR                           
105600       MOVE WS-API-TIJUSTDA(3:2) TO WS-API-MONTH                          
105700       MOVE WS-API-TIJUSTDA(5:2) TO WS-API-DAY                            
105800       MOVE WS-API-DATE          TO RESP-TIJUSTDA                         
105900     ELSE                                                                 
106000       MOVE SPACE                TO RESP-TIJUSTDA                         
106100     END-IF                                                               
106200                                                                          
106300     MOVE DOC-KVJUSTKV       TO RESP-KVJUSTKV                             
106400     MOVE DOC-KDJUSTYP       TO RESP-KDJUSTYP                             
106500     MOVE DOC-KDJUSTYP-A     TO RESP-KDJUSTYP-A                           
106600     MOVE DOC-BETEXT         TO RESP-BETEXT                               
106700     MOVE DOC-KVROS          TO RESP-KVROS                                
106800     MOVE DOC-TEINVANM       TO RESP-TEINVANM                             
106900                                                                          
107000     MOVE WS-ANTAL-RADER-BUF TO RESP-KVRADER-BUF                          
107100     MOVE +1 TO SALDO-IX                                                  
107200     PERFORM UNTIL SALDO-IX > WS-ANTAL-RADER-BUF                          
107300       MOVE CURRO-ADBUFFOMR(SALDO-IX) TO RESP-ADBUFFOMR(SALDO-IX)         
107400       MOVE CURRO-ADBUFFGANG(SALDO-IX)                                    
107500                                      TO RESP-ADBUFFGANG(SALDO-IX)        
107600       MOVE CURRO-ADBUFFPL(SALDO-IX)  TO RESP-ADBUFFPL(SALDO-IX)          
107700       IF CURRO-KVBUFF-F(SALDO-IX) = ALL '+'                              
107800          MOVE ZERO                     TO RESP-KVBUFF-F(SALDO-IX)        
107900       ELSE                                                               
108000          MOVE CURRO-KVBUFF-F(SALDO-IX) TO RESP-KVBUFF-F(SALDO-IX)        
108100       END-IF                                                             
108200       IF CURRO-KVKOLLI-F(SALDO-IX) = ALL '+'                             
108300        MOVE ZERO                      TO RESP-KVKOLLI-F(SALDO-IX)        
108400       ELSE                                                               
108500        MOVE CURRO-KVKOLLI-F(SALDO-IX) TO RESP-KVKOLLI-F(SALDO-IX)        
108600       END-IF                                                             
108700       ADD +1 TO SALDO-IX                                                 
108800     END-PERFORM                                                          
108900                                                                          
109000     MOVE WS-ANTAL-RADER-EFR          TO RESP-KVRADER-EFR                 
109100     MOVE +1 TO INDX                                                      
109200     PERFORM UNTIL INDX > WS-ANTAL-RADER-EFR                              
109300       MOVE DOC-IDDISTR(INDX)         TO RESP-IDDISTR(INDX)               
109400       MOVE DOC-IDKUNDNR(INDX)        TO RESP-IDKUNDNR(INDX)              
109500       MOVE DOC-IDKUNDRF(INDX)        TO RESP-IDKUNDRF(INDX)              
109600       MOVE DOC-KVLEVART(INDX)        TO RESP-KVLEVART(INDX)              
109700       ADD +1 TO INDX                                                     
109800     END-PERFORM                                                          
109900     .                                                                    
110000                                                                          
110100*    --- DISPATCHER SECTIONS                                              
110200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
110300                                                                          
110400     MOVE 'GETARG'               TO SUB-KDFUNC                            
110500     MOVE 'CARPARTS.LDC.PRINVENTORYDOCBG' TO SUB-ADDISPABS                
110600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
110700                                                                          
110800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
110900                                                                          
111000     IF SUB-KDRC > 0                                                      
111100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
111200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
111300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
111400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
111500     END-IF                                                               
111600     .                                                                    
111700     SKIP3                                                                
111800 S02-RETURN-RESPONSE SECTION.                                             
111900                                                                          
112000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
112100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
112200                                                                          
112300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
112400                                                                          
112500     IF SUB-KDRC > 0                                                      
112600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
112700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
112800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
112900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
113000     END-IF                                                               
113100     .                                                                    
113200     SKIP3                                                                
113300 S10-KONV-DATUM SECTION.                                                  
113400     MOVE 'S10-KONV-DATUM' TO CURR-SECTION                                
113500                                                                          
113600     MOVE INVH-DAREGDAT-CLO(3:6)  TO DAT-I-TIDATUM                        
113700     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
113800                                                                          
113900     CALL WDATKONV USING             DAT-KDDATFORM                        
114000                                     DAT-I-TIDATUM                        
114100                                     DAT-O-TIDATUM                        
114200                                     DAT-KDSVAR                           
114300     IF NOT DAT-KDSVAR-OK                                                 
114400       MOVE ZERO                  TO DAT-TIAAVVD                          
114500     END-IF                                                               
114600     .                                                                    
114700     SKIP3                                                                
114800 S11-MSG-CONV SECTION.                                                    
114900     MOVE SPACES                  TO RESP-MESSAGES (1)                    
115000                                     RESP-MESSAGES (2)                    
115100     MOVE 1                       TO MSG-IX                               
115200*    REQUEST OK                                                           
115300     MOVE 200                     TO RESP-KDSTATUS-API                    
115400     IF RESP-IDMSG-INFO > SPACE                                           
115500       MOVE SPACES                TO MSG-CONV-AREA                        
115600       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
115700       CALL WMSGCONV           USING MSG-CONV-AREA                        
115800       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
115900       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
116000       ADD 1                      TO MSG-IX                               
116100     END-IF                                                               
116200     IF RESP-IDMSG-ERROR > SPACE                                          
116300*      BAD REQUEST                                                        
116400       MOVE 400                   TO RESP-KDSTATUS-API                    
116500       MOVE SPACES                TO MSG-CONV-AREA                        
116600       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
116700       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
116800       CALL WMSGCONV           USING MSG-CONV-AREA                        
116900       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
117000       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
117100     END-IF                                                               
117200     .                                                                    
117300 S90-OPEN-DAP-SEND SECTION.                                               
117400     MOVE 'S90-OPEN-DAP-S' TO CURR-SECTION                                
117500                                                                          
117600     IF SUB-KDTRANS(1:7) = 'WL0173U'                                      
117700       MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                  
117800     ELSE                                                                 
117900       MOVE 'CARPARTS.DAP.DISTRSAV'    TO SEND-ADDISPABS                  
118000     END-IF                                                               
118100     MOVE 'OPEN'                     TO SEND-KDFUNC                       
118200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
118300                         SEND-OPEN-AREA                                   
118400     IF SEND-KDRC > ZERO                                                  
118500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
118600       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
118700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
118800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
118900     END-IF                                                               
119000     .                                                                    
119100                                                                          
119200 S90-CLOSE-DAP-SEND SECTION.                                              
119300     MOVE 'S90-CLOSE-DAP-' TO CURR-SECTION                                
119400                                                                          
119500     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
119600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
119700                                                                          
119800     IF SEND-KDRC > 0                                                     
119900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
120000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
120100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
120200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
120300     END-IF                                                               
120400     .                                                                    
120500                                                                          
120600 S90-PUT-DAP-HEADER SECTION.                                              
120700     MOVE 'S90-PUT-DAP-HE' TO CURR-SECTION                                
120800                                                                          
120900     MOVE 'PUT'                           TO SEND-KDFUNC                  
121000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
121100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
121200                         SEND-KVDLEN                                      
121300                         HDR-AREA                                         
121400     IF SEND-KDRC > ZERO                                                  
121500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
121600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
121700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
121800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
121900     END-IF                                                               
122000     .                                                                    
122100 S90-PUT-DOC      SECTION.                                                
122200     MOVE 'S90-PUT-DOC ' TO CURR-SECTION                                  
122300                                                                          
122400     MOVE 'PUT'                           TO SEND-KDFUNC                  
122500     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
122600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
122700                         SEND-KVDLEN                                      
122800                         DOC-AREA                                         
122900     IF SEND-KDRC > ZERO                                                  
123000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
123100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
123200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
123300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
123400     END-IF                                                               
123500     .                                                                    
123600 IMS-01-GHU-WDH101 SECTION.                                               
123700     MOVE 'IMS-01'  TO CURR-IMS-SECTION                                   
123800                                                                          
123900     MOVE SPACE                  TO ALL-SSA                               
124000     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
124100             DELIMITED BY SIZE INTO SSA1                                  
124200     MOVE '  GE'                 TO GOOD-STATUSCODES                      
124300     CALL CBLTDLI USING GU INV-PCB DLI-IO-AREA-INVA01 SSA1                
124400     MOVE INV-STATUS-CODE        TO STATUS-WS                             
124500     PERFORM IMS-STATUS-CHECK                                             
124600     .                                                                    
124700 IMS-02-GHNP-WDH111 SECTION.                                              
124800     MOVE 'IMS-02'  TO CURR-IMS-SECTION                                   
124900                                                                          
125000     MOVE SPACE                  TO ALL-SSA                               
125100     STRING 'WDH111  (WDH111KY>=' W-WDH111KY-MIN-X                        
125200                    '&WDH111KY<=' W-WDH111KY-MAX-X                        
125300                    '&FLINVBEH =' NOO ')'                                 
125400             DELIMITED BY SIZE INTO SSA1                                  
125500     MOVE '  GE'                 TO GOOD-STATUSCODES                      
125600     CALL CBLTDLI USING GHNP INV-PCB DLI-IO-AREA-INVA11 SSA1              
125700     MOVE INV-STATUS-CODE        TO STATUS-WS                             
125800     PERFORM IMS-STATUS-CHECK                                             
125900     .                                                                    
126000 IMS-03-GHU-WDGX5102 SECTION.                                             
126100     MOVE 'IMS-03'  TO CURR-IMS-SECTION                                   
126200                                                                          
126300     MOVE SPACE                 TO ALL-SSA                                
126400     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY01-X ')'                       
126500            DELIMITED BY SIZE INTO SSA1                                   
126600     STRING 'WDGX5102 '                                                   
126700            DELIMITED BY SIZE INTO SSA2                                   
126800     MOVE '  GE'                TO GOOD-STATUSCODES                       
126900     CALL CBLTDLI USING GHU WDG3-PCB DLI-IO-AREA-WDGX5102                 
127000                                               SSA1 SSA2                  
127100     MOVE WDG3-STATUS-CODE      TO STATUS-WS                              
127200     PERFORM IMS-STATUS-CHECK                                             
127300     .                                                                    
127400 IMS-04-GU-WDG301 SECTION.                                                
127500     MOVE 'IMS-04'  TO CURR-IMS-SECTION                                   
127600                                                                          
127700     MOVE SPACE                 TO ALL-SSA                                
127800     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY01-X ')'                       
127900            DELIMITED BY SIZE INTO SSA1                                   
128000     MOVE '  '                  TO GOOD-STATUSCODES                       
128100     CALL CBLTDLI USING GU WDG3-PCB DLI-IO-AREA-WDGX5102 SSA1             
128200     MOVE WDG3-STATUS-CODE      TO STATUS-WS                              
128300     PERFORM IMS-STATUS-CHECK                                             
128400     .                                                                    
128500 IMS-05-ISRT-WDGX5102 SECTION.                                            
128600     MOVE 'IMS-05'  TO CURR-IMS-SECTION                                   
128700                                                                          
128800     MOVE SPACE            TO ALL-SSA                                     
128900     MOVE 'WDGX5102 '      TO SSA1                                        
129000     MOVE '  '             TO GOOD-STATUSCODES                            
129100     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-AREA-WDGX5102                
129200                                  SSA1                                    
129300     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
129400     PERFORM IMS-STATUS-CHECK                                             
129500     .                                                                    
129600 IMS-06-REPL-WDGX5102 SECTION.                                            
129700     MOVE 'IMS-06'  TO CURR-IMS-SECTION                                   
129800                                                                          
129900     MOVE SPACE            TO ALL-SSA                                     
130000     MOVE '  '             TO GOOD-STATUSCODES                            
130100     CALL CBLTDLI USING REPL WDG3-PCB DLI-IO-AREA-WDGX5102                
130200     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
130300     PERFORM IMS-STATUS-CHECK                                             
130400     .                                                                    
130500 IMS-07-GET-ART-WDK6 SECTION.                                             
130600     MOVE 'IMS-07'  TO CURR-IMS-SECTION                                   
130700                                                                          
130800     MOVE SPACE                 TO ALL-SSA                                
130900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
131000            DELIMITED BY SIZE INTO SSA1                                   
131100     MOVE '  '                  TO GOOD-STATUSCODES                       
131200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
131300     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
131400     PERFORM IMS-STATUS-CHECK                                             
131500     .                                                                    
131600 IMS-08-GNP-CLAGERINFO SECTION.                                           
131700     MOVE 'IMS-08'  TO CURR-IMS-SECTION                                   
131800                                                                          
131900     MOVE SPACE            TO ALL-SSA                                     
132000     MOVE 'WDK611   ' TO SSA1                                             
132100     MOVE '  '             TO GOOD-STATUSCODES                            
132200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
132300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
132400     PERFORM IMS-STATUS-CHECK                                             
132500     .                                                                    
132600 IMS-09-GET-ART-WDK7 SECTION.                                             
132700     MOVE 'IMS-09'  TO CURR-IMS-SECTION                                   
132800                                                                          
132900     MOVE SPACE                 TO ALL-SSA                                
133000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
133100            DELIMITED BY SIZE INTO SSA1                                   
133200     STRING 'WDK711  (IDDC     =' W-IDDC ')'                              
133300            DELIMITED BY SIZE INTO SSA2                                   
133400     MOVE '  GE'                TO GOOD-STATUSCODES                       
133500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
133600     MOVE WDK7-STATUS-CODE      TO STATUS-WS                              
133700     PERFORM IMS-STATUS-CHECK                                             
133800     .                                                                    
133900 IMS-10-GET-INVHIST-ROT SECTION.                                          
134000     MOVE 'IMS-10'  TO CURR-IMS-SECTION                                   
134100                                                                          
134200     MOVE SPACE                 TO ALL-SSA                                
134300     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
134400            DELIMITED BY SIZE INTO SSA1                                   
134500     MOVE '  GE'                TO GOOD-STATUSCODES                       
134600     CALL CBLTDLI USING GU INVC-PCB DLI-IO-AREA5 SSA1                     
134700     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
134800     PERFORM IMS-STATUS-CHECK                                             
134900     .                                                                    
135000 IMS-11-GET-INVHIST-SDC SECTION.                                          
135100     MOVE 'IMS-11'  TO CURR-IMS-SECTION                                   
135200                                                                          
135300     MOVE SPACE                 TO ALL-SSA                                
135400     STRING 'WLINVC11*F(TISEGKEY<=' W-TISEGKEY-X                          
135500                    '&IDDC     =' W-IDDC ')'                              
135600            DELIMITED BY SIZE INTO SSA1                                   
135700     MOVE '  GE'                TO GOOD-STATUSCODES                       
135800     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA5 SSA1                    
135900     MOVE INVC-STATUS-CODE      TO STATUS-WS                              
136000     PERFORM IMS-STATUS-CHECK                                             
136100     .                                                                    
136200 IMS-GU-WDD8B1 SECTION.                                                   
136300                                                                          
136400     STRING 'WDD8B1  (WDD8B1KY=>' W-WDD8B1KY-MIN-X                        
136500                    '&WDD8B1KY=<' W-WDD8B1KY-MAX-X ')'                    
136600            DELIMITED BY SIZE INTO SSA1                                   
136700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
136800     CALL CBLTDLI USING GU WDD8B-PCB DLI-IO-AREA-D8B1 SSA1                
136900     MOVE WDD8B-STATUS-CODE TO STATUS-WS                                  
137000     PERFORM IMS-STATUS-CHECK                                             
137100     .                                                                    
137200     EJECT                                                                
137300 IMS-GN-WDD8B1 SECTION.                                                   
137400                                                                          
137500     STRING 'WDD8B1  (WDD8B1KY=>' W-WDD8B1KY-MIN-X                        
137600                    '&WDD8B1KY=<' W-WDD8B1KY-MAX-X ')'                    
137700            DELIMITED BY SIZE INTO SSA1                                   
137800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
137900     CALL CBLTDLI USING GN WDD8B-PCB DLI-IO-AREA-D8B1 SSA1                
138000     MOVE WDD8B-STATUS-CODE TO STATUS-WS                                  
138100     PERFORM IMS-STATUS-CHECK                                             
138200     .                                                                    
138300     EJECT                                                                
138400 IMS-14-GU-BEN-SEQ SECTION.                                               
138500     MOVE 'IMS-14'  TO CURR-IMS-SECTION                                   
138600                                                                          
138700     MOVE SPACE                  TO ALL-SSA                               
138800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
138900             DELIMITED BY SIZE INTO SSA1                                  
139000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
139100             DELIMITED BY SIZE INTO SSA2                                  
139200     MOVE '  GE'                 TO GOOD-STATUSCODES                      
139300     CALL CBLTDLI USING GU BEN-PCB DLI-IO-AREA SSA1 SSA2                  
139400     MOVE BEN-STATUS-CODE        TO STATUS-WS                             
139500     PERFORM IMS-STATUS-CHECK                                             
139600     .                                                                    
139700 IMS-15-GU-WDE4C1   SECTION.                                              
139800     MOVE 'IMS-15'  TO CURR-IMS-SECTION                                   
139900                                                                          
140000     MOVE SPACE                  TO ALL-SSA                               
140100     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-LOW                          
140200                    '&WDE4C1KY<=' W-WDE4C1KY-HIGH                         
140300                    '&KDRADSTA <' W-ORDSTA-X ')'                          
140400             DELIMITED BY SIZE INTO SSA1                                  
140500     MOVE '  GE'                 TO GOOD-STATUSCODES                      
140600     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-E4C1 SSA1                     
140700     MOVE WDE4C-STATUS-CODE      TO STATUS-WS                             
140800     PERFORM IMS-STATUS-CHECK                                             
140900     .                                                                    
141000 IMS-16-GN-WDE4C1   SECTION.                                              
141100     MOVE 'IMS-16'  TO CURR-IMS-SECTION                                   
141200                                                                          
141300     MOVE SPACE                  TO ALL-SSA                               
141400     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-LOW                          
141500                    '&WDE4C1KY<=' W-WDE4C1KY-HIGH                         
141600                    '&KDRADSTA <' W-ORDSTA-X ')'                          
141700             DELIMITED BY SIZE INTO SSA1                                  
141800     MOVE '  GEGB'               TO GOOD-STATUSCODES                      
141900     CALL CBLTDLI USING GN WDE4C-PCB DLI-IO-E4C1 SSA1                     
142000     MOVE WDE4C-STATUS-CODE      TO STATUS-WS                             
142100     PERFORM IMS-STATUS-CHECK                                             
142200     .                                                                    
142300 IMS-GU-WDE401-11   SECTION.                                              
142400     MOVE 'IMS-GU-E401-11'  TO CURR-IMS-SECTION                           
142500                                                                          
142600     MOVE SPACE                  TO ALL-SSA                               
142700     STRING 'WDE401  *D(WDE401KY =' W-WDE401-X ')'                        
142800             DELIMITED BY SIZE INTO SSA1                                  
142900     STRING 'WDE411  (IDPURAD  =' W-WDE411-X ')'                          
143000            DELIMITED BY SIZE INTO SSA2                                   
143100     MOVE '    '                 TO GOOD-STATUSCODES                      
143200     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401-11 SSA1 SSA2              
143300     MOVE WDE4-STATUS-CODE      TO STATUS-WS                              
143400     PERFORM IMS-STATUS-CHECK                                             
143500     .                                                                    
143600 IMS-17-REPLACE SECTION.                                                  
143700     MOVE 'IMS-17'  TO CURR-IMS-SECTION                                   
143800                                                                          
143900     MOVE SPACE           TO ALL-SSA                                      
144000     MOVE '  '            TO GOOD-STATUSCODES                             
144100     CALL CBLTDLI USING REPL INV-PCB DLI-IO-AREA-INVA11                   
144200     MOVE INV-STATUS-CODE TO STATUS-WS                                    
144300     PERFORM IMS-STATUS-CHECK                                             
144400     .                                                                    
144500 IMS-GNP-WDH121 SECTION.                                                  
144600     MOVE 'IMS-GNP-WDH121        ' TO CURR-IMS-SECTION                    
144700                                                                          
144800     MOVE SPACE           TO ALL-SSA                                      
144900     STRING 'WDH111  (WDH111KY>=' W-WDH111KY-MIN-X                        
145000                    '&WDH111KY<=' W-WDH111KY-MAX-X                        
145100                    '&FLINVBEH =' NOO ')'                                 
145200             DELIMITED BY SIZE INTO SSA1                                  
145300     MOVE 'WDH121 ' TO SSA2                                               
145400     MOVE '  GE' TO GOOD-STATUSCODES                                      
145500     CALL CBLTDLI USING GHNP INV-PCB DLI-IO-AREA-INVA21 SSA1 SSA2         
145600     MOVE INV-STATUS-CODE TO STATUS-WS                                    
145700     PERFORM IMS-STATUS-CHECK                                             
145800     .                                                                    
145900     SKIP2                                                                
146000 IMS-DELETE-WDH111 SECTION.                                               
146100     MOVE 'IMS-DELETE-WDH111     ' TO CURR-IMS-SECTION                    
146200                                                                          
146300     MOVE SPACE           TO ALL-SSA                                      
146400     MOVE 'WDH111 ' TO SSA1                                               
146500     MOVE '  ' TO GOOD-STATUSCODES                                        
146600     CALL CBLTDLI USING DLET INV-PCB DLI-IO-AREA-INVA11                   
146700     MOVE INV-STATUS-CODE TO STATUS-WS                                    
146800     PERFORM IMS-STATUS-CHECK                                             
146900     .                                                                    
147000     SKIP2                                                                
147100 IMS-INSERT-WDH111 SECTION.                                               
147200                                                                          
147300*    STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
147400*         DELIMITED BY SIZE INTO SSA1                                     
147500                                                                          
147600     MOVE SPACE           TO ALL-SSA                                      
147700     MOVE 'WDH111 ' TO SSA1                                               
147800     MOVE '  IINI' TO GOOD-STATUSCODES                                    
147900     CALL CBLTDLI USING ISRT INV-PCB DLI-IO-AREA-INVA11 SSA1              
148000     MOVE INV-STATUS-CODE TO STATUS-WS                                    
148100     PERFORM IMS-STATUS-CHECK                                             
148200     .                                                                    
148300     SKIP3                                                                
148400 IMS-REPLACE-WDH121 SECTION.                                              
148500     MOVE 'IMS-REPLACE           ' TO CURR-IMS-SECTION                    
148600                                                                          
148700     MOVE SPACE           TO ALL-SSA                                      
148800     MOVE '  ' TO GOOD-STATUSCODES                                        
148900     CALL CBLTDLI USING REPL INV-PCB DLI-IO-AREA-INVA21                   
149000     MOVE INV-STATUS-CODE TO STATUS-WS                                    
149100     PERFORM IMS-STATUS-CHECK                                             
149200     .                                                                    
149300     EJECT                                                                
149400 IMS-INSERT-WDH121 SECTION.                                               
149500     MOVE 'IMS-INSERT-WDH121     ' TO CURR-IMS-SECTION                    
149600                                                                          
149700     MOVE SPACE           TO ALL-SSA                                      
149800     MOVE 'WDH121 ' TO SSA1                                               
149900     MOVE '  ' TO GOOD-STATUSCODES                                        
150000     CALL CBLTDLI USING ISRT INV-PCB DLI-IO-AREA-INVA21 SSA1              
150100     MOVE INV-STATUS-CODE TO STATUS-WS                                    
150200     PERFORM IMS-STATUS-CHECK                                             
150300     .                                                                    
150400     EJECT                                                                
150500 IMS-GU-WDB601    SECTION.                                                
150600     MOVE 'IMS-GU-WDB601         ' TO CURR-IMS-SECTION                    
150700                                                                          
150800     MOVE SPACE           TO ALL-SSA                                      
150900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
151000          DELIMITED BY SIZE INTO SSA1                                     
151100     MOVE '  ' TO GOOD-STATUSCODES                                        
151200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
151300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
151400     PERFORM IMS-STATUS-CHECK                                             
151500     .                                                                    
151600     SKIP3                                                                
151700 IMS-25-GU-WDE601 SECTION.                                                
151800     MOVE 'IMS-25-GU-WDE601      ' TO CURR-IMS-SECTION                    
151900                                                                          
152000     MOVE SPACE           TO ALL-SSA                                      
152100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
152200          DELIMITED BY SIZE INTO SSA1                                     
152300     MOVE '  ' TO GOOD-STATUSCODES                                        
152400     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-E601 SSA1                 
152500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
152600     PERFORM IMS-STATUS-CHECK                                             
152700     .                                                                    
152800     EJECT                                                                
152900                                                                          
153000 IMS-STATUS-CHECK   SECTION.                                              
153100                                                                          
153200     SET STATUS-IX TO 1                                                   
153300     SEARCH GOOD-STATUS                                                   
153400       AT END                                                             
153500         CALL FELLOG                                                      
153600     WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                             
153700       CONTINUE                                                           
153800     END-SEARCH                                                           
153900     .                                                                    
