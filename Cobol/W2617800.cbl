000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2617800.                                                
000300 AUTHOR.         P-A HELGEGREN, KOPIA W26169.                             
000400 DATE-WRITTEN.   13/09/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING AV DDGS-PARAMETRAR                                   
000900*        UPPDATERA VISS DIREKTLEVERANSINFO FÖR CLASSIC                    
000901*        LARM I VISSA FALL TILL 2171                                      
000902*                                                                         
001000*        PROGRAMMET UPPDATERAR WLARTC (WDK6)  (11,12)                     
001540*                                ZZAC (WDG6)  (B65)                       
001560*                                ZZAD (WDG9)  (K623)                      
001570*                                WDF2         (DIRLEV)                    
001580*                                2223 (WDR5)  (LARM)                      
001800*                                                                         
001900*    INDATA.                                                              
002100*        INFIL        W01160                                              
002200*                                                                         
002300*    UTDATA.                                                              
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*          --- ARTIKELREGISTER                                            
003500     SELECT W01160                     ASSIGN TO W26178D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W01160                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY W01160   -L.                                                   
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(08)   VALUE 'W2617800'.            
005100 77  LAES-SW                     PIC X     VALUE SPACE.                   
005200 77  WS-SKROTDATUM-SLUT          PIC X     VALUE SPACE.                   
005300 77  WS-DATUM-HITTAD             PIC X     VALUE SPACE.                   
005400 77  WS-ART-SLUT                 PIC X     VALUE SPACE.                   
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  PROGSW-IX                   PIC S9(3)  VALUE ZERO  COMP-3.           
005800 77  RAD-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
005900 77  RAD-IX-MAX                  PIC S9(3)  VALUE +12   COMP-3.           
006000 77  TAB-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
006100 77  TAB-IX-MAX                  PIC S9(3)  VALUE +30   COMP-3.           
006400 77  IX-BEEMB                    PIC S9(4)   VALUE +0  COMP SYNC.         
006500 77  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
006600 77  MAX-IX                      PIC S9(4)   VALUE +11 COMP SYNC.         
006700 77  DAGENS-DATUM-Y2K            PIC 9(8)   VALUE ZERO.                   
006800 77  WS-ANTAL-X                  PIC 9(3)   VALUE ZERO COMP-3.            
006900 77  W-TID                       PIC 9(8)   VALUE ZERO.                   
007000 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
007100 01  TEST-IDINK-GRP              PIC X(4)    VALUE ZERO.                  
007200 01  FILLER  REDEFINES TEST-IDINK-GRP.                                    
007300    03  TEST-IDINK               PIC 9(3).                                
007301    03  FILLER                   PIC X.                                   
007302 77  WS-IDLOGLOP                 PIC 9(01) COMP-3 VALUE ZERO.             
007303 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
007400                                                                          
007500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007600     88  INDATA-OK                           VALUE 'J'.                   
007700     88  INDATA-FEL                          VALUE 'N'.                   
007800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007900     88  NYCKLAR-OK                          VALUE 'J'.                   
008000     88  NYCKLAR-FEL                         VALUE 'N'.                   
008100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008200     88  EGEN-MID                            VALUE '6322'.                
008300     88  GODK-MID                            VALUE '6322' '6325'.         
008700 77  WS-CMD                      PIC X       VALUE SPACE.                 
008800 77  SW-R23                      PIC X       VALUE 'N'.                   
008810 77  SW-2223-TEST                PIC X       VALUE 'N'.                   
008900 77  WS-FL6326                   PIC X       VALUE 'N'.                   
008910 77  R22-FINNS                   PIC X       VALUE 'N'.                   
009000                                                                          
009300 77  IDARTNR-SOEKNING            PIC X       VALUE 'N'.                   
009400 77  DATUM-SOEKNING              PIC X       VALUE 'N'.                   
009500 77  DATUM-IDARTNR-SOEKNING      PIC X       VALUE 'N'.                   
009600 77  IDDC-SOEKNING               PIC X       VALUE 'N'.                   
009800 77  WS-DASKROT9                 PIC 9(8)    VALUE ZERO.                  
009900 77  WS-SUARTSTD                 PIC 9(7)V9(2) VALUE ZERO.                
010000 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
010100 77  WS-DASKROT9-BEORD           PIC 9(8)    VALUE ZERO.                  
010500 77  W-KVSKROT-REST              PIC S9(7)  VALUE ZERO.                   
010600 77  W-ADBUFFPL                  PIC 9(5)  VALUE ZERO.                    
010700 77  TEST-NYCKEL-IDARTNR         PIC X(9)    VALUE SPACE.                 
010800 77  TEST-NYCKEL-TIDATUM         PIC X(6)    VALUE SPACE.                 
010900 77  TEST-NYCKEL-IDANSK          PIC X(3)    VALUE SPACE.                 
011000 77  WS-FLHOGRE                  PIC X       VALUE 'N'.                   
011100 77  WS-FLURVAL                  PIC X       VALUE 'N'.                   
011200 77  WS-HIGHLEV                  PIC X       VALUE 'N'.                   
011400 77  W-ANNUL-IDUSER              PIC X(8)    VALUE SPACE.                 
011500 77  W-ANNUL-IDMAIL              PIC X(60)   VALUE SPACE.                 
011600 01  ANUL-BEANST-GODK            PIC X(25) VALUE SPACE.                   
011700 01  ANUL-IDMAIL                 PIC X(60) VALUE SPACE.                   
011800 01  WSM-IDARTNR                 PIC Z(8)9 VALUE ZERO.                    
011900 77  WS-KDERS-UTG                PIC Z(2)    VALUE ZERO.                  
012000 77  WS-SUTPO-TOT                PIC Z(6)9   VALUE ZERO.                  
012100 77  WS-KVSKROT-BEORD            PIC Z(6)9   VALUE ZERO.                  
012200 77  WS-KVSKROT-KVAR             PIC Z(6)9   VALUE ZERO.                  
012300 77  WS-KVTILLG-CDC              PIC Z(6)9   VALUE ZERO.                  
012400 77  WS-KVTILLG-SDC              PIC Z(6)9   VALUE ZERO.                  
012500 77  WS-KVAKS-CDC                PIC Z(6)9   VALUE ZERO.                  
012600 77  WS-KVAKS-SDC                PIC Z(6)9   VALUE ZERO.                  
012700 77  WS-IDLEVNR-NUM              PIC 9(5)    VALUE ZERO.                  
012800 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
012900 77  WS-IDARTNR-8                PIC 9(08)   VALUE ZERO.                  
013000 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
013010     88  END-OF-W01160                       VALUE 'J'.                   
013100 77  WS-IDUSER                   PIC X(8)    VALUE 'W2616800'.            
013300     SKIP3                                                                
013400 01  FELTEXT.                                                             
013500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013700 01  CHKP-VAR.                                                            
013800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
013900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
014000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
014100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
014200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
014300     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
014400*    NÄSTA KLOCKSLAG                                                      
014401 01  NASTA-KLOCKSLAG.                                                     
014402   03  WS-NXT-KL             PIC 9(6).                                    
014403   03  FILLER                REDEFINES WS-NXT-KL.                         
014404     05  WS-NXT-KL-TT        PIC 9(2).                                    
014405     05  WS-NXT-KL-MM        PIC 9(2).                                    
014406     05  WS-NXT-KL-SS        PIC 9(2).                                    
014407     EJECT                                                                
014500 01  TABELL-SKROT-VARDE.                                                  
014600     03  W-VARDE-PER-RAD         OCCURS 12.                               
014700         05 W-SUARTSTD           PIC 9(7)V9(2) VALUE ZERO.                
014800                                                                          
014900 01  TABELL-TEMEMO.                                                       
015000     03  W-TEMEMO-RAD            OCCURS 11.                               
015100         05 W-TEMEMO             PIC X(66) VALUE SPACE.                   
015200*      --- VALID IDDC CODES                                               
015300*                                                                         
015400*01    -COPY WWDCKONS                                                     
015401*                                                                         
015402*01    -COPY WWDC99                                                       
015500*01    -COPY WWDC99 -PRE SW-                                              
015600       EJECT                                                              
017300 01  WS-TIDATETIME               PIC X(14).                               
017400 01  FILLER REDEFINES WS-TIDATETIME.                                      
017500     03  WS-DATUM                PIC 9(8).                                
017600     03  WS-TIDHHMMSS            PIC 9(6).                                
017700                                                                          
017800 01  WS-TID                      PIC 9(8) VALUE ZERO.                     
017810 01  WS-DAREGDAT                 PIC 9(8).                                
017900                                                                          
018000 01  WS-DASKROT.                                                          
018100     03  WS-DASKROT-SS                PIC 9(2).                           
018200     03  WS-DASKROT-AAMMDD            PIC 9(6).                           
018300 01  WS-AAAAMMDD REDEFINES WS-DASKROT PIC 9(8).                           
018400                                                                          
018500 01  W-IDAVTAL-RED               PIC 9(13).                               
018600 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
018700     03  FILLER                  PIC X.                                   
018800     03  W-PREFIX                PIC X(3).                                
018900     03  W-AVTALSNR              PIC X(6).                                
019000     03  W-SUFFIX                PIC X(3).                                
019100     SKIP2                                                                
019200                                                                          
020200 01  BAS-R22-REGPOST.                                                     
020300*    03      -COPY W212R22   -PRE BAS-R22-                                
020400     03 BAS-R22-REST            PIC X(41).                                
020500     EJECT                                                                
020600 01  BAS-R23-REGPOST.                                                     
020700*    03      -COPY W212R23   -PRE BAS-R23-                                
020800     03 BAS-R23-REST            PIC X(41).                                
021000     EJECT                                                                
021400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
021500 01  GENERELLA-SUBPROGRAM.                                                
021600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
021900     EJECT                                                                
022000*    --- PARAMETRAR TILL POSTSUM                                          
022100*                                                                         
022200*01  -COPY W0005   -PRE  POSTSUM-                                         
022300     EJECT                                                                
022400 01  PROG-TO-PROG-SW.                                                     
022500*    03  -COPY WMSGSOP                                                    
022600     EJECT                                                                
022700 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
022800 01  P-TO-P-SW.                                                           
022900     03  P-TO-P-KVLL                PIC S9(4)           COMP SYNC.        
023000     03  P-TO-P-KDZ1                PIC X(1)  VALUE LOW-VALUE.            
023100     03  P-TO-P-KDZ2                PIC X(1)  VALUE LOW-VALUE.            
023200     03  P-TO-P-KDTRANS             PIC X(8).                             
023300     03  P-TO-P-IDTRANS             PIC X(4).                             
023400     03  P-TO-P-KDMFSFOR            PIC X(1).                             
023500     03  P-TO-P-DATA.                                                     
023600        05 FILLER                  PIC X(1000).                           
023700                                                                          
023800**********************************************************                
023900***   I N K Ö P S - P O S T   P V                                         
024000**********************************************************                
024100*                                                                         
024200*01  -COPY A310TB65                -PRE A310-                             
024300     EJECT                                                                
024400*01  AREA   -COPY W092W001     -PRE W092-.                                
024500     EJECT                                                                
024900                                                                          
025100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025200     SKIP3                                                                
025300*01  -COPY WMSGAREA                                                       
025400     EJECT                                                                
025500 01  FILLER              PIC X(16)  VALUE 'PROG-TO-PROG-SW'.              
025600                                                                          
025700 01  W-PROG-TO-PROG-SW.                                                   
025800     05  P-WS-LL         PIC S9(4)  VALUE +469 COMP SYNC.                 
025900     05  P-WS-Z1-Z2      PIC  X(2)  VALUE LOW-VALUE.                      
026000     05  KDTRANS-WS      PIC  X(8)  VALUE 'W1T113X '.                     
026100     05  FILLER          PIC  X(5)  VALUE '21293'.                        
026200     05  MID    -COPY W1I11301     -PRE PROGSW-                           
026300     EJECT                                                                
026400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
026500     SKIP3                                                                
026600*01  -COPY WMFSAREA                                                       
026700     EJECT                                                                
026800                                                                          
026900*01  AREA -COPY W01160     -PRE IN-                                       
027000*                                                                         
027100     EJECT                                                                
027200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027300*                                                                         
027400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027500     SKIP3                                                                
027600 01  NYCKLAR-TILL-DLI.                                                    
027700*                                                                         
027800     03  W-IDARTNR-MIN-X.                                                 
027900         05  W-IDARTNR-MIN   PIC S9(9)  VALUE ZERO COMP-3.                
028000     03  W-IDARTNR-MAX-X.                                                 
028100         05  W-IDARTNR-MAX   PIC S9(9)  VALUE +999999999 COMP-3.          
028600     03  W-IDDC-MIN-X.                                                    
028700         05  W-IDDC-MIN      PIC X(2)   VALUE LOW-VALUE.                  
028800     03  W-IDDC-MAX-X.                                                    
028900         05  W-IDDC-MAX      PIC X(2)   VALUE HIGH-VALUE.                 
029400     03  W-IDARTNR-X.                                                     
029500         05  W-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.                
029600     03  W-IDDC-X.                                                        
029700         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
029800     03  W-IDLAND-X.                                                      
029900         05  W-IDLAND        PIC X(2)   VALUE SPACE.                      
031500     03  W-IDUSER-GODK-X.                                                 
031600         05  W-IDUSER-GODK    PIC X(8)   VALUE SPACE.                     
033300     03  W-IDLEVNR-X.                                                     
033400         05 W-IDLEVNR      PIC X(5)   VALUE SPACE.                        
033500     03  W-IDBEST-X.                                                      
033501         05 W-IDBEST       PIC S9(13) COMP-3.                             
033502     03  W-IDAVTAL-CL-X.                                                  
033503         05 W-IDAVTAL-CL   PIC S9(13) COMP-3.                             
033504     03  W-WDG901KY-X.                                                    
033600         05  W-TIREGDAT          PIC S9(07)   VALUE ZERO COMP-3.          
033700         05  W-TIKLOCK           PIC S9(09)   VALUE ZERO COMP-3.          
033800                                                                          
033900     03  W-WDF201KY-X.                                                    
034000         05  W-IDLEVNR-WDF2      PIC  X(5)   VALUE SPACE.                 
034100         05  W-IDDIRGRP          PIC X(10)   VALUE SPACE.                 
034200     03  W-IDARTNR-WDF2-X.                                                
034201         05  W-IDARTNR-WDF2      PIC S9(9)  VALUE ZERO COMP-3.            
034202                                                                          
034203     03  W-WDGXKEY-2223-X.                                                
034204         05  W-IDHTYP-2223       PIC X(4)    VALUE '2223'.                
034205         05  W-IDANSK-2223       PIC S9(3)   COMP-3 VALUE ZERO.           
034206         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
034207     03  W-WDGXKEY-2224-X.                                                
034208         05  W-TISENBEK-DAG-2224 PIC S9(7)   COMP-3 VALUE ZERO.           
034209         05  W-TISENBEK-KL-2224  PIC S9(7)   COMP-3 VALUE ZERO.           
034210         05  W-KDLARM-2224       PIC S9(3)   COMP-3 VALUE 240.            
034220     03  W-IDARTNR-2224-X.                                                
034230         05  W-IDARTNR-2224      PIC S9(9)   COMP-3 VALUE ZERO.           
034300                                                                          
034310     03  W-WDGX2231-X.                                                    
034320         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
034330         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
034340                                                                          
034350     03  W-WDGX2232-X.                                                    
034360         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
034370         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
034600                                                                          
034700     EJECT                                                                
034800*    --- STATUS-KOD FRÅN IMS                                              
034900 01  STATUS-WS                   PIC XX.                                  
035000     88  SEGMENT-FINNS                       VALUE '  '.                  
035100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
035200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
035300     88  IMS-EJ-OK                           VALUE 'XD'.                  
035400     SKIP2                                                                
035500 01  GODK-STATUSKODER.                                                    
035600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035700     SKIP3                                                                
035800 01  SSA1                        PIC X(128).                              
035900 01  SSA2                        PIC X(256).                              
036000 01  SSA3                        PIC X(256).                              
036100 01  SSA4                        PIC X(256).                              
036101 01  SSA1-2                      PIC X(128).                              
036102 01  SSA2-2                      PIC X(128).                              
036200     EJECT                                                                
036300*    --- IMS FUNKTIONSKODER                                               
036400*01  -COPY W0003                                                          
036500     EJECT                                                                
036600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
036700 01  DLI-IO-WLARTC01.                                                     
036800*    03  -COPY WDK601                                                     
036900     EJECT                                                                
037000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
037100 01  DLI-IO-WLARTC11.                                                     
037200*    03  -COPY WDK611                                                     
037300     EJECT                                                                
037400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC22'.                    
037500 01  DLI-IO-WLARTC22.                                                     
037600*    03  -COPY WDK622                                                     
037601     EJECT                                                                
037602 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC23'.                    
037603 01  DLI-IO-WLARTC23.                                                     
037604*    03  -COPY WDK623                                                     
037700     EJECT                                                                
042800                                                                          
043800 01  DLI-IO-AREA5.                                                        
043900     03  IO-AREA5              PIC X(150) VALUE SPACE.                    
044000                                                                          
044100*    03  ZZAC -COPY WDGZ01     -PRE ZZAC-  -RED IO-AREA5.                 
044200     EJECT                                                                
044800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDG901'.                      
044900 01  DLI-IO-WDG901.                                                       
045000*    03  -COPY WDG901                                                     
045100                                                                          
045200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF201'.                      
045300 01  DLI-IO-WDF201.                                                       
045400*    03  -COPY WDF201 -PRE WDF2-                                          
045500     EJECT                                                                
045600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF212'.                      
045700 01  DLI-IO-WDF212.                                                       
045800*    03  -COPY WDF212  -PRE  WDF2-                                        
045900     EJECT                                                                
046000*    ---  DLI INPUT-OUTPUT AREA                                           
046100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2223'.                    
046200 01  DLI-IO-WDGX2223.                                                     
046300*    03  -COPY WDGX2223                                                   
046400     EJECT                                                                
046500                                                                          
046600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2224'.                    
046700 01  DLI-IO-WDGX2224.                                                     
046800*    03  -COPY WDGX2224                                                   
046901     EJECT                                                                
046902                                                                          
046903 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301  '.                    
046904 01  DLI-IO-WDT301.                                                       
046905*    03  -COPY WDT301                                                     
046906 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311  '.                    
046907 01  DLI-IO-WDT311.                                                       
046908*    03  -COPY WDT311                                                     
046909     EJECT                                                                
046910                                                                          
046911 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2232'.                    
046912 01  DLI-IO-WDGX2232.                                                     
046913*    03  -COPY WDGX2232                                                   
046914     EJECT                                                                
046916                                                                          
046917 LINKAGE SECTION.                                                         
046918*01  -COPY W0009   -PRE MSG-                                              
046919     EJECT                                                                
047500*01  -COPY W0008   -PRE ARTC-                                             
047600     05  FILLER                  PIC X.                                   
047700     EJECT                                                                
050200*01  -COPY W0008     -PRE ZZAC-                                           
050300         05  FILLER           PIC X.                                      
050400     EJECT                                                                
050800*01  -COPY W0008  -PRE ZZAD-                                              
050900     05  FILLER                  PIC X.                                   
051000     EJECT                                                                
051100*01  -COPY W0008  -PRE WDF2-                                              
051200     05  FILLER                  PIC X.                                   
051201     EJECT                                                                
051202*01  -COPY W0008  -PRE ARTC2-                                             
051203     05  FILLER                  PIC X.                                   
051300     EJECT                                                                
051301*01  -COPY W0008  -PRE 2223-                                              
051302     05  FILLER                  PIC X.                                   
051303     EJECT                                                                
051304*01  -COPY W0008  -PRE WDT3-                                              
051305     05  FILLER                  PIC X.                                   
051306     EJECT                                                                
051307*01  -COPY W0008  -PRE WDR2-                                              
051308     05  FILLER                  PIC X.                                   
051309     EJECT                                                                
051310 PROCEDURE DIVISION  USING MSG-PCB                                        
051311                           ARTC-PCB                                       
051312                           ZZAC-PCB ZZAD-PCB                              
051320                           WDF2-PCB ARTC2-PCB 2223-PCB WDT3-PCB           
051330                           WDR2-PCB.                                      
052000 MAIN SECTION.                                                            
052100     ENTRY 'DLITCBL' USING MSG-PCB                                        
052200                           ARTC-PCB                                       
052300                           ZZAC-PCB ZZAD-PCB                              
052400                           WDF2-PCB ARTC2-PCB 2223-PCB WDT3-PCB           
052500                           WDR2-PCB.                                      
052700                                                                          
052800     PERFORM A-INIT                                                       
052900     PERFORM S00-LAES-W01160                                              
053000     PERFORM UNTIL END-OF-W01160                                          
053100       IF CHKP-ANT > CHKP-MAX                                             
053200         PERFORM X-TAG-CHECKPOINT                                         
053300       END-IF                                                             
053400                                                                          
053500       PERFORM H-KOLL-CLASSIC                                             
053600                                                                          
053700       PERFORM S00-LAES-W01160                                            
053704                                                                          
053800     END-PERFORM                                                          
053900                                                                          
054000                                                                          
054100     PERFORM Z-FINIT                                                      
054200                                                                          
054300     MOVE ZERO TO RETURN-CODE                                             
054400     GOBACK                                                               
054500     .                                                                    
054600     EJECT                                                                
054700 A-INIT SECTION.                                                          
054800                                                                          
054900                                                                          
055000     PERFORM IMS-RESTART                                                  
055100                                                                          
055200     OPEN INPUT W01160                                                    
055300                                                                          
055400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
055500                                                                          
055600                                                                          
055700     ACCEPT DAGENS-DATUM FROM DATE                                        
056400     ACCEPT POST-TIKLOCK FROM TIME                                        
056500     MOVE POST-TIKLOCK TO W-TIKLOCK                                       
056600     .                                                                    
056700     EJECT                                                                
056800                                                                          
056900 H-KOLL-CLASSIC SECTION.                                                  
057200                                                                          
057400                                                                          
057500     IF IN-CLAG-IDLEVNR   = 'BQ8VA'    AND                                
057600        IN-CLAG-KVLS      = ZERO       AND                                
057700        IN-CLAG-KDERS     = ZERO       AND                                
057800        IN-CLAG-KDERS-UTG = ZERO       AND                                
057900***     IN-CLAG-KDERS-UTG = ZERO                                          
058000        IN-CLAG-REDIRLEV  NOT = 1                                         
058100****            REDIRLEV LÄGGS TILL I URVALKRITERIERNA                    
058200                                                                          
058300        IF IN-CLAG-PRARTSTD    = ZERO  OR                                 
058400           IN-CLAG-VKART       = ZERO  OR                                 
058500           IN-CLAG-VLARTNTO    = ZERO  OR                                 
058600           IN-CLAG-KDARTURS    = SPACE OR                                 
058700           IN-CLAG-IDSTATNR(3) = ZERO                                     
058800           PERFORM HB-SKAPA-LARM                                          
058900        ELSE                                                              
059000           PERFORM HA-UPPDAT-DIR                                          
059100        END-IF                                                            
059101     END-IF                                                               
059104     .                                                                    
059105     EJECT                                                                
059106 HA-UPPDAT-DIR SECTION.                                                   
059107                                                                          
059108     PERFORM S14-SKAPA-CLASSIC-TRANS                                      
059109     PERFORM HAB-UPPDAT-MAN                                               
059110     .                                                                    
059111     EJECT                                                                
059112 HAB-UPPDAT-MAN SECTION.                                                  
059120                                                                          
059130     IF IN-CLAG-ADLAGOMR   = 49     AND                                   
059140        IN-CLAG-ADGANG     = 20     AND                                   
059150        IN-CLAG-ADPLATS    = 01     AND                                   
059160        IN-CLAG-REDIRLEV   = 1      AND                                   
059170        IN-CLAG-BEFT       = 02                                           
059180        CONTINUE                                                          
059190     ELSE                                                                 
059200          MOVE IN-CLAG-IDARTNR TO W-IDARTNR                               
059300          MOVE W-IDARTNR       TO IDARTNR-WS                              
061200          PERFORM IMS-GHU-ARTC01                                          
061210          PERFORM IMS-GHU-ARTC11                                          
061300          IF SEGMENT-FINNS                                                
062100                                                                          
062200             MOVE 49     TO CLAG-ADLAGOMR                                 
062300             MOVE 20     TO CLAG-ADGANG                                   
062400             MOVE 01     TO CLAG-ADPLATS                                  
062500             MOVE 1      TO CLAG-REDIRLEV                                 
062600             MOVE 02     TO CLAG-BEFT                                     
062700                                                                          
063300             PERFORM IMS-REPL-ARTC11                                      
063500          END-IF                                                          
063501     END-IF                                                               
063502     .                                                                    
063503     EJECT                                                                
063504 HB-SKAPA-LARM SECTION.                                                   
063506                                                                          
063507***  SKAPA LARM  SOM I W37173   XXBU-2224  WDR550                         
063508*    FÖRST TEST OM LARM REDAN LIGGER                                      
063509*    ANNARS 'DDGS 1.0 KUNDE EJ UPPDAT'                                    
063510*  UPPLÄGG AV NYTT LARM ALT. UPDATERING AV BEFINTLIGT LARM                
063513     MOVE IN-CLAG-IDARTNR   TO W-IDARTNR W-IDARTNR-2224                   
063514     MOVE IN-CLAG-IDANSK    TO W-IDANSK-2232                              
063515     PERFORM IMS-GU-WDR220                                                
063516     IF SEGMENT-FINNS                                                     
063517       MOVE 2232-IDANSK-LARM TO W-IDANSK-2223                             
063520     ELSE                                                                 
063521       MOVE ZERO            TO W-IDANSK-2223                              
063523     END-IF                                                               
063530     PERFORM IMS-GHU-WDGX2223                                             
063540     IF SEGMENT-SAKNAS                                                    
063550       MOVE '2223'          TO 2223-IDHTYP                                
063560       MOVE W-IDANSK-2223   TO 2223-IDANSK                                
063570       MOVE LOW-VALUE       TO 2223-LOW-VALUE                             
063580       PERFORM IMS-ISRT-WDGX2223                                          
063590       ADD +1               TO CHKP-ANT                                   
063600     END-IF                                                               
063610                                                                          
063800     PERFORM IMS-GHU-WDGX2223                                             
063900     MOVE IN-CLAG-IDARTNR   TO W-IDARTNR-2224                             
063910     MOVE WC-CDC-SE         TO W-IDDC                                     
064000     PERFORM IMS-GHNP-WDGX2224                                            
064100     IF SEGMENT-SAKNAS                                                    
064200       PERFORM IMS-GHU-WDGX2223                                           
064300       PERFORM HBA-BUILD-WDGX2224                                         
064400       PERFORM IMS-ISRT-WDGX2224                                          
064500***II                                                                     
064600       PERFORM UNTIL (NOT SEGMENT-FINNS-REDAN)                            
064700         MOVE 2224-TISENBEK-KL TO WS-NXT-KL                               
064800         PERFORM HBB-NXT-SEKUND                                           
064900         MOVE WS-NXT-KL        TO 2224-TISENBEK-KL                        
065000         PERFORM IMS-ISRT-WDGX2224                                        
065100       END-PERFORM                                                        
065200***II                                                                     
065300     ELSE                                                                 
065400***    MOVE NEJ             TO 2224-FLNYLARM                              
065500       MOVE DAGENS-DATUM    TO 2224-TIREGDAT                              
065600       PERFORM IMS-REPL-WDGX2224                                          
065700     END-IF                                                               
065800     ADD +1                 TO CHKP-ANT                                   
065900     .                                                                    
066000     EJECT                                                                
066100                                                                          
066200 HBA-BUILD-WDGX2224 SECTION.                                              
066300     MOVE FUNCTION CURRENT-DATE(3:6)    TO  2224-TISENBEK-DAG             
066400     MOVE FUNCTION CURRENT-DATE(11:6)   TO  2224-TISENBEK-KL              
066500     MOVE W-KDLARM-2224       TO 2224-KDLARM                              
066600     MOVE W-IDARTNR-2224      TO 2224-IDARTNR                             
066700     MOVE WC-CDC-SE           TO 2224-IDDC                                
066800     MOVE JA                  TO 2224-FLNYLARM                            
066900     MOVE ZERO                TO 2224-IDDISTR                             
067000                                 2224-IDKUNDNR                            
067100     MOVE '0000000   '        TO 2224-IDKUNDRF                            
067200     MOVE 1                   TO 2224-IDLOPNR                             
067300     MOVE DAGENS-DATUM        TO 2224-TIREGDAT                            
067500     MOVE SPACE               TO 2224-IDTRANS                             
067600                                 2224-KDMFSFOR                            
067601     MOVE ZERO                TO 2224-IDKR                                
067602     MOVE SPACE               TO 2224-IDLEVNR                             
067603     .                                                                    
067604     EJECT                                                                
067605 HBB-NXT-SEKUND SECTION.                                                  
117900*                                                                         
118000*    RÄKNAR UPP TILL NÄSTA SEKUND.                                        
118100*    GÅR ALDRIG ÖVER DYGNS-GRÄNS.                                         
118200*    NÄSTA SEKUND EFTER 23.59.59 GER 00.00.00 INOM SAMMA DYGN.            
118300*                                                                         
118400     ADD 1                   TO WS-NXT-KL-SS                              
118500     IF  WS-NXT-KL-SS > 59                                                
118600       MOVE ZERO             TO WS-NXT-KL-SS                              
118700       ADD 1                 TO WS-NXT-KL-MM                              
118800       IF  WS-NXT-KL-MM > 59                                              
118900         MOVE ZERO           TO WS-NXT-KL-MM                              
119000         ADD 1               TO WS-NXT-KL-TT                              
119100         IF  WS-NXT-KL-TT > 23                                            
119200           MOVE ZERO         TO WS-NXT-KL-TT                              
119300         END-IF                                                           
119400       END-IF                                                             
119500     END-IF                                                               
119600     .                                                                    
119601     EJECT                                                                
119602                                                                          
119603 Z-FINIT SECTION.                                                         
119604                                                                          
119605                                                                          
119606     CLOSE W01160                                                         
119607                                                                          
119608     .                                                                    
119609     EJECT                                                                
119610 S00-LAES-W01160  SECTION.                                                
119611     SKIP2                                                                
119612     READ W01160 INTO IN-AREA                                             
119613     AT END                                                               
119614        SET END-OF-W01160 TO TRUE                                         
119615                                                                          
119616     NOT AT END                                                           
119617        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
119618        MOVE 'W26178D1' TO POSTSUM-DDNAMN2                                
119619        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
119620        CALL POSTSUM USING POSTSUM-PARM                                   
119621                                                                          
119622     END-READ                                                             
119623     .                                                                    
119624     EJECT                                                                
119625 S14-SKAPA-CLASSIC-TRANS SECTION.                                         
119626                                                                          
119627     MOVE IN-CLAG-IDARTNR     TO W-IDARTNR                                
119628     MOVE W-IDARTNR           TO IDARTNR-WS                               
119629                                                                          
119630     IF IN-CLAG-IDPROJ      = 'GCP' AND                                   
119640        IN-CLAG-IDINK       = '987' AND                                   
119650        IN-CLAG-IDANSK      = 495   AND                                   
119660        IN-CLAG-IDBERED     = 049   AND                                   
119670        IN-CLAG-BEFT        = 02    AND                                   
119680        IN-CLAG-IDPLANGR-AG = 9     AND                                   
119690        IN-CLAG-KDFORPPL    = ZERO  AND                                   
119700        IN-CLAG-KDFORPGP    = ZERO  AND                                   
119800        IN-CLAG-KDFORPUF    = ZERO                                        
119900        CONTINUE                                                          
120000     ELSE                                                                 
120100        PERFORM IMS-GHU-ARTC01                                            
120200        IF SEGMENT-FINNS                                                  
120300           MOVE W-IDARTNR      TO IDARTNR-WS                              
120400           PERFORM IMS-GHU-ARTC11                                         
120500           IF SEGMENT-FINNS                                               
120600              MOVE 495         TO CLAG-IDANSK                             
120700              MOVE 049         TO CLAG-IDBERED                            
120800              MOVE 02          TO CLAG-BEFT                               
120900              MOVE 9           TO CLAG-IDPLANGR-AG                        
121000              MOVE 'GCP'       TO CLAG-IDPROJ                             
121100              MOVE ZERO        TO CLAG-KDFORPPL                           
121200              MOVE ZERO        TO CLAG-KDFORPGP                           
121300              MOVE ZERO        TO CLAG-KDFORPUF                           
121400              MOVE '987'       TO CLAG-IDINK                              
121500              PERFORM IMS-REPL-ARTC11                                     
121600           END-IF                                                         
121700        END-IF                                                            
121800     END-IF                                                               
121900                                                                          
122000                                                                          
122100     PERFORM IMS-GHU-ARTC01                                               
122200     IF SEGMENT-FINNS                                                     
122300        MOVE W-IDARTNR      TO IDARTNR-WS                                 
122400        PERFORM IMS-GHU-ARTC11                                            
122500        IF SEGMENT-FINNS                                                  
122600****          KOLLA ÄVEN AVTAL (ENDAST CLASSIC SKALL FINNAS)              
122601           MOVE IN-CLAG-IDINK TO TEST-IDINK-GRP                           
122602           PERFORM IMS-GNP-ARTC23                                         
122603           PERFORM UNTIL SEGMENT-SAKNAS                                   
122604             MOVE AVT-IDAVTAL TO W-IDAVTAL-RED                            
122605***             TAR ÄVEN MED NAP-AVTAL, PREFIX = 004                      
122606             IF (TEST-IDINK > 99 AND                                      
122607                 TEST-IDINK < 790) OR                                     
122608                (TEST-IDINK > 799 AND                                     
122609                 TEST-IDINK < 987) OR                                     
122610                (TEST-IDINK > 987 AND                                     
122611                 TEST-IDINK <= 999) OR                                    
122612                 (W-PREFIX = '004')                                       
122613                PERFORM S16-SKAPA-B65                                     
122614             ELSE                                                         
122615                PERFORM S18-SKAPA-R22POST                                 
122616             END-IF                                                       
122617             PERFORM IMS-GNP-ARTC23                                       
122618           END-PERFORM                                                    
122619           PERFORM S14A-SKAPA-FORP-HIST                                   
122620           PERFORM S19-SKAPA-R23POST                                      
122621                                                                          
122622           ACCEPT W-TID FROM TIME                                         
122623                                                                          
122624****    KOLLA  -IDDIRGRP = 'CLASSIC'  (WDF2)                              
122625           MOVE 'BQ8VA'         TO W-IDLEVNR-WDF2                         
122626           MOVE 'CLASSIC'       TO W-IDDIRGRP                             
122627           MOVE IN-CLAG-IDARTNR TO W-IDARTNR-WDF2                         
122628           PERFORM IMS-GU-WDF212-KVAL                                     
122629           IF SEGMENT-FINNS                                               
122630              CONTINUE                                                    
122640           ELSE                                                           
122650              MOVE 'BQ8VA'         TO W-IDLEVNR-WDF2                      
122660              MOVE 'CLASSIC'       TO W-IDDIRGRP                          
122670              MOVE IN-CLAG-IDARTNR TO WDF2-ART-IDARTNR                    
122680              MOVE ZERO            TO WDF2-ART-DASTADAT                   
122681              MOVE -99             TO WDF2-ART-KVLS-DLEV                  
122682              MOVE ZERO            TO WDF2-ART-TIINLMOT                   
122683                                      WDF2-ART-TIREGDAT                   
122684*      -99 BETYDER ATT SALDOT INTE UPPDATERAS AV LEVERANTÖR               
122685*      DET BETYDER ALLTSÅ INTE ATT VI HAR ETT NEGATIVT SALDO :-)          
122686*      FÖR LEVERANTÖRER SOM SKICKAR SALDOUPPGIFTER ÄR VÄRDET >= 0         
122687*      VID NYUPPLÄGG AV ARTIKLAR PÅ LEVERANTÖRER SOM REDOVISAR            
122688*      SALDO, KOMMER ARTIKELN SÅLEDES HA VÄRDET -99 TILL FÖRSTA           
122689*      UPPDATERINGEN AV SALDOT                                            
122690                                                                          
122700              PERFORM IMS-ISRT-WDF212                                     
122701           END-IF                                                         
122702        END-IF                                                            
122703     END-IF                                                               
122704     .                                                                    
122705     EJECT                                                                
122706 S14A-SKAPA-FORP-HIST SECTION.                                            
122707                                                                          
122708     PERFORM IMS-GU-WDT301                                                
122709     IF SEGMENT-SAKNAS                                                    
122710        MOVE W-IDARTNR     TO FART-IDARTNR                                
122711        PERFORM IMS-ISRT-WDT301                                           
122712     END-IF                                                               
122713     PERFORM IMS-GU-WDT301                                                
122714     MOVE 'SE'        TO W-IDLAND                                         
122715     PERFORM IMS-GNP-WDT311                                               
122716     IF SEGMENT-FINNS AND                                                 
122717        FPCK-IDUSER    = 'W26178'     AND                                 
122720        FPCK-BEFT      = 02           AND                                 
122730        FPCK-KDFORPPL  = ZERO         AND                                 
122740        FPCK-KDFORPGP  = ZERO         AND                                 
122750        FPCK-KDFORPUF  = ZERO         AND                                 
122760        FPCK-TEBEFT(1) = 'ÖVERGÅTT TILL CLASSIC DIREKTLEVERANS'           
122770        CONTINUE                                                          
122771     ELSE                                                                 
122772**** INSERT WDT311                                                        
122778        MOVE 'W26178' TO FPCK-IDUSER                                      
122779        MOVE 02       TO FPCK-BEFT                                        
122780        MOVE ZERO     TO FPCK-KDFORPPL                                    
122790        MOVE ZERO     TO FPCK-KDFORPGP                                    
122800        MOVE ZERO     TO FPCK-KDFORPUF                                    
122900        MOVE 'ÖVERGÅTT TILL CLASSIC DIREKTLEVERANS'                       
123000                      TO FPCK-TEBEFT(1)                                   
123100        MOVE SPACE    TO FPCK-TEBEFT(2)                                   
123200        MOVE SPACE    TO FPCK-TEBEFT(3)                                   
123210        MOVE SPACE    TO FPCK-TEBEFT(4)                                   
123220        MOVE SPACE    TO FPCK-TEBEFT(5)                                   
123300        MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                    
123400        COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WS-DAREGDAT            
123500        ACCEPT WS-TID FROM TIME                                           
123600        COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WS-TID                  
123700        MOVE 'SE'     TO FPCK-IDLANDX2                                    
123800        PERFORM IMS-ISRT-WDT311                                           
123801     END-IF                                                               
123802     .                                                                    
123803     EJECT                                                                
123804  S16-SKAPA-B65 SECTION.                                                  
123805                                                                          
123806***  KAN MAN SE OM B65 REDAN SKICKAD ?                                    
123807                                                                          
123808     IF AVT-IDAVTAL =  987910987094   AND                                 
123809        AVT-IDLEVNR-AVT = 'BQ8VA'                                         
123810        CONTINUE                                                          
123820     ELSE                                                                 
123830        ACCEPT ZZAC-TIKLOCK             FROM  TIME                        
123840        ACCEPT ZZAC-TIAAMMDD FROM             DATE                        
123850        ADD 1                          TO WS-IDLOGLOP                     
123860        MOVE WS-IDLOGLOP               TO ZZAC-IDLOGLOP                   
123870        MOVE SPACE              TO A310-LEVNUM-GODSM                      
123880                                 A310-ANT-BESTANN                         
123890        MOVE 'RY2'              TO A310-KT                                
123900        MOVE DAGENS-DATUM       TO A310-DATUM-UTSKR                       
124000        MOVE ART-IDLEVNR        TO W-IDLEVNR                              
124100        IF W-IDLEVNR (5:1) = SPACE                                        
124200*         LEVNUM SKALL TILLS VIDARE VARA NUMERISKT I X(5)                 
124300          MOVE ZERO             TO TALLY                                  
124400          INSPECT W-IDLEVNR TALLYING TALLY                                
124500                             FOR CHARACTERS BEFORE INITIAL SPACE          
124600          IF TALLY = ZERO                                                 
124700             MOVE ZERO          TO WS-IDLEVNR-NUM                         
124800          ELSE                                                            
124900             MOVE W-IDLEVNR (1:TALLY)                                     
125000                                TO WS-IDLEVNR-NUM                         
125100          END-IF                                                          
125200          MOVE WS-IDLEVNR-NUM TO A310-LEVNUM                              
125300        ELSE                                                              
125400          MOVE W-IDLEVNR        TO A310-LEVNUM                            
125500        END-IF                                                            
125600        MOVE W-IDARTNR          TO WS-IDARTNR                             
125700        MOVE WS-IDARTNR         TO WS-IDARTNR-8                           
125800        MOVE WS-IDARTNR-8       TO A310-ARTNR                             
125900                                   W092-SORTBGP                           
126000                                                                          
126100        MOVE AVT-IDAVTAL        TO W-IDAVTAL-RED                          
126200        MOVE W-PREFIX           TO A310-BESTPREF                          
126300        MOVE W-AVTALSNR         TO A310-BESTLNR                           
126400        MOVE W-SUFFIX           TO A310-BESTSUFF                          
126500        MOVE A310-A310B65       TO ZZAC-LOGGPOST                          
126600        MOVE W092-AREA          TO ZZAC-SORTPOST                          
126700        PERFORM IMS-ISRT-ZZAC                                             
126701        IF SEGMENT-FINNS-REDAN                                            
126702          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
126703             ADD +1             TO WS-IDLOGLOP                            
126704             IF WS-IDLOGLOP = 9                                           
126705                ADD +1          TO ZZAC-TIKLOCK                           
126706             END-IF                                                       
126707             MOVE WS-IDLOGLOP   TO ZZAC-IDLOGLOP                          
126708             PERFORM IMS-ISRT-ZZAC                                        
126709          END-PERFORM                                                     
126710        END-IF                                                            
126711     END-IF                                                               
126712     .                                                                    
126713     EJECT                                                                
126714*                                                                         
126715 S18-SKAPA-R22POST SECTION.                                               
126716                                                                          
126717     IF AVT-IDAVTAL =  987910987094   AND                                 
126718        AVT-IDLEVNR-AVT = 'BQ8VA'                                         
126719        CONTINUE                                                          
126720     ELSE                                                                 
126721        MOVE AVT-IDAVTAL TO W-IDBEST                                      
126730        MOVE NEJ         TO R22-FINNS                                     
126740        PERFORM IMS-GHU-ARTC11-2                                          
126750        IF SEGMENT-FINNS                                                  
126760          PERFORM IMS-GNP-ARTC22-2                                        
126770          PERFORM UNTIL SEGMENT-SAKNAS OR R22-FINNS = JA                  
126780             IF BEST-IDBEST       = AVT-IDAVTAL     AND                   
126790                BEST-IDLEVNR-BEST = AVT-IDLEVNR-AVT AND                   
126800                BEST-KDBEH-BEST   = 5                                     
126900                MOVE JA TO R22-FINNS                                      
127000             END-IF                                                       
127100             PERFORM IMS-GNP-ARTC22-2                                     
127200          END-PERFORM                                                     
127300                                                                          
127400          IF R22-FINNS = JA                                               
127500             CONTINUE                                                     
127600          ELSE                                                            
127700             MOVE 'R22'                TO BAS-R22-IDPTYP                  
127800             MOVE W-IDARTNR            TO BAS-R22-IDARTNR                 
127900             MOVE AVT-IDAVTAL          TO BAS-R22-IDBEST                  
128000             MOVE AVT-IDLEVNR-AVT      TO BAS-R22-IDLEVNR-BEST            
128100             MOVE SPACE                TO BAS-R22-IDLEVNR-SHIP            
128200             MOVE DAGENS-DATUM         TO BAS-R22-TIBEST                  
128300             MOVE AVT-KVAVTANT         TO BAS-R22-KVBEST                  
128400             MOVE 5                    TO BAS-R22-KDBEH-BEST              
128500             MOVE SPACE                TO BAS-R22-TENOT-BESTPRIS          
128600             MOVE SPACE                TO BAS-R22-REST                    
128700                                                                          
128800                                                                          
128900             MOVE DAGENS-DATUM         TO POST-TIREGDAT W-TIREGDAT        
129000             ADD +1                    TO POST-TIKLOCK W-TIKLOCK          
129100             MOVE WS-IDUSER            TO POST-IDUSER                     
129200             MOVE 'W26178'             TO POST-IDLTERM                    
129300             MOVE ZERO                 TO POST-TIBORT                     
129400                                                                          
129500             MOVE BAS-R22-REGPOST      TO POST-REGPOST                    
129600             PERFORM IMS-ISRT-WDG901                                      
129700             IF SEGMENT-FINNS-REDAN                                       
129800               PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                      
129900                  ADD +1 TO POST-TIKLOCK W-TIKLOCK                        
130000                  PERFORM IMS-ISRT-WDG901                                 
130100               END-PERFORM                                                
130200             END-IF                                                       
130300          END-IF                                                          
130301        END-IF                                                            
130302     END-IF                                                               
130303     .                                                                    
130304     EJECT                                                                
130305*                                                                         
130306 S19-SKAPA-R23POST SECTION.                                               
130307                                                                          
130308     MOVE 987910987094 TO W-IDAVTAL-CL                                    
130309     PERFORM IMS-GU-ARTC23-BQ8VA                                          
130310     IF SEGMENT-FINNS AND AVT-IDAVTAL =  987910987094                     
130320                      AND AVT-IDLEVNR-AVT = 'BQ8VA'                       
130330        CONTINUE                                                          
130340     ELSE                                                                 
130350        MOVE 'R23'                TO BAS-R23-IDPTYP                       
130360        MOVE W-IDARTNR            TO BAS-R23-IDARTNR                      
130370        MOVE 987910987094         TO BAS-R23-IDAVTAL                      
130380        MOVE 'BQ8VA'              TO BAS-R23-IDLEVNR-AVT                  
130390                                     BAS-R23-IDLEVNR-SHIP                 
130400        MOVE DAGENS-DATUM         TO BAS-R23-TIAVTAL                      
130500        MOVE ZERO                 TO BAS-R23-KVAVTANT                     
130600        MOVE +1                   TO BAS-R23-KDBEH-AVT                    
130700        MOVE SPACE                TO BAS-R23-TENOT-AVTPRIS                
130800        MOVE SPACE                TO BAS-R23-REST                         
130900                                                                          
131000                                                                          
131100        MOVE DAGENS-DATUM         TO POST-TIREGDAT W-TIREGDAT             
131200        ADD +1                    TO POST-TIKLOCK W-TIKLOCK               
131300        ACCEPT POST-TIKLOCK FROM TIME                                     
131400        MOVE POST-TIKLOCK TO W-TIKLOCK                                    
131500        MOVE WS-IDUSER            TO POST-IDUSER                          
131600        MOVE 'W26178'             TO POST-IDLTERM                         
131700        MOVE ZERO                 TO POST-TIBORT                          
131800                                                                          
131900        MOVE BAS-R23-REGPOST      TO POST-REGPOST                         
132000        PERFORM IMS-ISRT-WDG901                                           
132100        IF SEGMENT-FINNS-REDAN                                            
132200          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
132300             ADD +1 TO POST-TIKLOCK W-TIKLOCK                             
132400             PERFORM IMS-ISRT-WDG901                                      
132500          END-PERFORM                                                     
132600        END-IF                                                            
132601     END-IF                                                               
132602     .                                                                    
132603     EJECT                                                                
132604                                                                          
132605 X-TAG-CHECKPOINT   SECTION.                                              
132606                                                                          
132607     PERFORM IMS-CHECKPOINT                                               
132608     MOVE ZERO TO CHKP-ANT                                                
132609     .                                                                    
132610     EJECT                                                                
132611* --- IMS SEKTIONER ---                                                   
132612     SKIP3                                                                
132613 IMS-GHU-ARTC01 SECTION.                                                  
132614     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
132615          DELIMITED BY SIZE INTO SSA1                                     
132616     MOVE '  GE' TO GODK-STATUSKODER                                      
132617     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC01 SSA1                 
132618     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132619     PERFORM IMS-STATUSKONTROLL                                           
132620     .                                                                    
132621     SKIP3                                                                
132622 IMS-GHU-ARTC11 SECTION.                                                  
132623     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
132624          DELIMITED BY SIZE INTO SSA1                                     
132625     MOVE 'WDK611   ' TO SSA2                                             
132626     MOVE '  GE' TO GODK-STATUSKODER                                      
132627     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2            
132628     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132629     PERFORM IMS-STATUSKONTROLL                                           
132630     .                                                                    
132631     SKIP3                                                                
132632 IMS-GNP-ARTC23 SECTION.                                                  
132633     MOVE  'WDK623   ' TO  SSA1                                           
132634     MOVE '  GE' TO GODK-STATUSKODER                                      
132635     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC23 SSA1                 
132636     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132637     PERFORM IMS-STATUSKONTROLL                                           
132638     .                                                                    
132639     SKIP2                                                                
132640 IMS-GU-ARTC23-BQ8VA SECTION.                                             
132641     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
132642          DELIMITED BY SIZE INTO SSA1                                     
132643     MOVE 'WDK611   ' TO SSA2                                             
132644     STRING 'WDK623  (IDAVTAL  =' W-IDAVTAL-CL-X ')'                      
132645          DELIMITED BY SIZE INTO SSA3                                     
132646     MOVE '  GE' TO GODK-STATUSKODER                                      
132647     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC23 SSA1 SSA2             
132648                                                    SSA3                  
132649     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132650     PERFORM IMS-STATUSKONTROLL                                           
132651     .                                                                    
132652     SKIP2                                                                
132653 IMS-REPL-ARTC11 SECTION.                                                 
132654     MOVE '  ' TO GODK-STATUSKODER                                        
132655     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
132656     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132657     PERFORM IMS-STATUSKONTROLL                                           
132658     ADD +1  TO CHKP-ANT                                                  
132659     .                                                                    
132660     EJECT                                                                
132661 IMS-GU-WDT301 SECTION.                                                   
132662     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
132663          DELIMITED BY SIZE INTO SSA1                                     
132664     MOVE '  GE' TO GODK-STATUSKODER                                      
132665     CALL CBLTDLI USING GU   WDT3-PCB DLI-IO-WDT301 SSA1                  
132666     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
132667     PERFORM IMS-STATUSKONTROLL                                           
132668     .                                                                    
132669     SKIP3                                                                
132670 IMS-ISRT-WDT301 SECTION.                                                 
132673     MOVE 'WDT301   ' TO SSA1                                             
132674     MOVE '    ' TO GODK-STATUSKODER                                      
132675     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
132676     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
132677     PERFORM IMS-STATUSKONTROLL                                           
132678     ADD +1  TO CHKP-ANT                                                  
132679     .                                                                    
132680     SKIP3                                                                
132681 IMS-GNP-WDT311 SECTION.                                                  
132682     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
132683          DELIMITED BY SIZE INTO SSA1                                     
132684     STRING 'WDT311  (IDLAND   =' W-IDLAND-X ')'                          
132685          DELIMITED BY SIZE INTO SSA2                                     
132686     MOVE '  GE' TO GODK-STATUSKODER                                      
132687     CALL CBLTDLI USING GNP  WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
132688     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
132689     PERFORM IMS-STATUSKONTROLL                                           
132690     .                                                                    
132691     SKIP3                                                                
132692 IMS-ISRT-WDT311 SECTION.                                                 
132693     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
132694          DELIMITED BY SIZE INTO SSA1                                     
132695     MOVE 'WDT311   ' TO SSA2                                             
132696     MOVE '  II' TO GODK-STATUSKODER                                      
132697     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
132698     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
132699     PERFORM IMS-STATUSKONTROLL                                           
132700     ADD +1  TO CHKP-ANT                                                  
132701     .                                                                    
132702     SKIP3                                                                
132703 IMS-ISRT-ZZAC SECTION.                                                   
132704                                                                          
132705     MOVE 'WLZZAC01 '        TO SSA1                                      
132706     MOVE '  II'             TO GODK-STATUSKODER                          
132707     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA5 SSA1                   
132708     MOVE ZZAC-STATUS-CODE     TO STATUS-WS                               
132709     PERFORM IMS-STATUSKONTROLL                                           
132710     ADD +1  TO CHKP-ANT                                                  
132711     .                                                                    
132712     SKIP3                                                                
132713 IMS-ISRT-WDG901 SECTION.                                                 
132714                                                                          
132715     MOVE 'WLZZAD01 ' TO SSA1                                             
132716     MOVE '  II' TO GODK-STATUSKODER                                      
132717     CALL CBLTDLI USING ISRT ZZAD-PCB DLI-IO-WDG901 SSA1                  
132718     MOVE ZZAD-STATUS-CODE TO STATUS-WS                                   
132719     PERFORM IMS-STATUSKONTROLL                                           
132720     ADD +1  TO CHKP-ANT                                                  
132721     .                                                                    
132730     EJECT                                                                
132800                                                                          
132801 IMS-GU-WDF212-KVAL SECTION.                                              
132802                                                                          
132803     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
132804          DELIMITED BY SIZE INTO SSA1                                     
132805     STRING 'WDF212  (IDARTNR  =' W-IDARTNR-WDF2-X ')'                    
132806          DELIMITED BY SIZE INTO SSA2                                     
132807     MOVE 'GE    '     TO GODK-STATUSKODER                                
132808     CALL CBLTDLI USING GU WDF2-PCB DLI-IO-WDF212 SSA1 SSA2               
132809     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
132810     PERFORM IMS-STATUSKONTROLL                                           
132811     .                                                                    
132812     EJECT                                                                
132813 IMS-ISRT-WDF212 SECTION.                                                 
132814                                                                          
132815     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
132816          DELIMITED BY SIZE INTO SSA1                                     
132817     MOVE 'WDF212         ' TO SSA2                                       
132818     MOVE '  IINI' TO GODK-STATUSKODER                                    
132819     CALL CBLTDLI USING ISRT WDF2-PCB DLI-IO-WDF212 SSA1 SSA2             
132820     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
132821     PERFORM IMS-STATUSKONTROLL                                           
132822     ADD +1  TO CHKP-ANT                                                  
132823     .                                                                    
132824     EJECT                                                                
132825 IMS-GHU-ARTC11-2 SECTION.                                                
132826     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
132827          DELIMITED BY SIZE INTO SSA1-2                                   
132828     MOVE 'WDK611   ' TO SSA2-2                                           
132829     MOVE '  GE' TO GODK-STATUSKODER                                      
132830     CALL CBLTDLI USING GHU ARTC2-PCB DLI-IO-WLARTC11                     
132840                        SSA1-2 SSA2-2                                     
132841     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
132842     PERFORM IMS-STATUSKONTROLL                                           
132843     .                                                                    
132844     SKIP3                                                                
132845 IMS-GNP-ARTC22-2 SECTION.                                                
132846     MOVE  'WDK622   ' TO  SSA1-2                                         
132847     MOVE '  GE' TO GODK-STATUSKODER                                      
132848     CALL CBLTDLI USING GNP ARTC2-PCB DLI-IO-WLARTC22 SSA1-2              
132849     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
132850     PERFORM IMS-STATUSKONTROLL                                           
132851     .                                                                    
132852     SKIP2                                                                
132853 IMS-GU-WDR220 SECTION.                                                   
132854                                                                          
132855     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
132856          DELIMITED BY SIZE INTO SSA1                                     
132857     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
132858          DELIMITED BY SIZE INTO SSA2                                     
132859     MOVE '  GE' TO GODK-STATUSKODER                                      
132860     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2232 SSA1 SSA2             
132861     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
132862     PERFORM IMS-STATUSKONTROLL                                           
132863     .                                                                    
132864 IMS-GHU-WDGX2223 SECTION.                                                
132865     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
132866          DELIMITED BY SIZE INTO SSA1                                     
132867     MOVE '  GE'           TO GODK-STATUSKODER                            
132868     CALL CBLTDLI USING GHU 2223-PCB DLI-IO-WDGX2223 SSA1                 
132869     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
132870     PERFORM IMS-STATUSKONTROLL                                           
132871     .                                                                    
132872                                                                          
132880 IMS-ISRT-WDGX2223 SECTION.                                               
132890     MOVE 'WDR501   '      TO SSA1                                        
132900     MOVE '  II'           TO GODK-STATUSKODER                            
133000     CALL CBLTDLI USING ISRT 2223-PCB DLI-IO-WDGX2223 SSA1                
133100     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
133101     PERFORM IMS-STATUSKONTROLL                                           
133102     ADD +1  TO CHKP-ANT                                                  
133103     .                                                                    
133104                                                                          
133105 IMS-GHNP-WDGX2224 SECTION.                                               
133106     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
133107          DELIMITED BY SIZE INTO SSA1                                     
133108     STRING 'WDR550  (IDARTNR  =' W-IDARTNR-2224-X                        
133109                    '&IDDC     =' W-IDDC-X ')'                            
133111          DELIMITED BY SIZE INTO SSA2                                     
133112     MOVE '  GE'           TO GODK-STATUSKODER                            
133120     CALL CBLTDLI USING GHNP 2223-PCB DLI-IO-WDGX2224 SSA1 SSA2           
133130     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
133140     PERFORM IMS-STATUSKONTROLL                                           
133150     .                                                                    
133160                                                                          
133170 IMS-ISRT-WDGX2224 SECTION.                                               
133180     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
133190          DELIMITED BY SIZE INTO SSA1                                     
133200     MOVE 'WDR550   '      TO SSA2                                        
133300     MOVE '  II'           TO GODK-STATUSKODER                            
133400     CALL CBLTDLI USING ISRT 2223-PCB DLI-IO-WDGX2224 SSA1 SSA2           
133500     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
133501     PERFORM IMS-STATUSKONTROLL                                           
133502     ADD +1  TO CHKP-ANT                                                  
133503     .                                                                    
133504                                                                          
133505 IMS-REPL-WDGX2224 SECTION.                                               
133506     MOVE '  '             TO GODK-STATUSKODER                            
133507     CALL CBLTDLI USING REPL 2223-PCB DLI-IO-WDGX2224                     
133508     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
133509     PERFORM IMS-STATUSKONTROLL                                           
133510     ADD +1  TO CHKP-ANT                                                  
133520     .                                                                    
133530                                                                          
133531     EJECT                                                                
133532 IMS-RESTART SECTION.                                                     
133533     SKIP2                                                                
133534     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
133535     MOVE '  ' TO GODK-STATUSKODER                                        
133536     CALL CBLTDLI USING XRST MSG-PCB                                      
133537                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
133538                        CHKP-AREA-LENGTH CHKP-AREA                        
133539     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133540     PERFORM IMS-STATUSKONTROLL                                           
133541     .                                                                    
133542     SKIP3                                                                
133543 IMS-CHECKPOINT SECTION.                                                  
133544     SKIP2                                                                
133545     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
133546     MOVE '  XD' TO GODK-STATUSKODER                                      
133547     CALL CBLTDLI USING CHKP MSG-PCB                                      
133548                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
133600                        CHKP-AREA-LENGTH CHKP-AREA                        
133700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133800     PERFORM IMS-STATUSKONTROLL                                           
133900                                                                          
134000     IF IMS-EJ-OK                                                         
134100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
134200       DISPLAY FELTEXT                                                    
134300       CALL FELLOG                                                        
134400     END-IF                                                               
134500     .                                                                    
134600     EJECT                                                                
134700                                                                          
134800 IMS-STATUSKONTROLL SECTION.                                              
134900     SET STATUS-IX TO 1                                                   
135000     SEARCH GODK-STATUS                                                   
135100       AT END                                                             
135200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
135300         DELIMITED BY SIZE INTO FELTEXT                                   
135400         CALL FELLOG                                                      
135500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
135600         CONTINUE                                                         
135700     END-SEARCH                                                           
135800     .                                                                    
