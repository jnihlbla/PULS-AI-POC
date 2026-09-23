000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2617900.                                                
000300 AUTHOR.         P-A HELGEGREN, KOPIA W26178,  REDIRLEV !                 
000400 DATE-WRITTEN.   13/09/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERING AV DDGS-PARAMETRAR                                   
000900*        UPPDATERA VISS DIREKTLEVERANSINFO FÖR CLASSIC                    
000901*        LARM I VISSA FALL TILL 2171                                      
000902*                                                                         
001000*        PROGRAMMET UPPDATERAR WLARTC (WDK6)  (11)                        
001540*                                ZZAC (WDG6)  (B65)                       
001560*                                ZZAD (WDG9)  (K623)                      
001570*                                WDF2         (DIRLEV)                    
001571*                                2223 (WDR5)  (LARM)                      
001572*                                      WDT3   T311                        
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
003500     SELECT W01160                     ASSIGN TO W26179D1.                
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
005010 77  IDPGM                       PIC X(08)   VALUE 'W2617900'.            
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
037710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301  '.                    
037720 01  DLI-IO-WDT301.                                                       
037730*    03  -COPY WDT301                                                     
037740     EJECT                                                                
037750 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311  '.                    
037760 01  DLI-IO-WDT311.                                                       
037770*    03  -COPY WDT311                                                     
037780     EJECT                                                                
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
046903 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2232'.                    
046904 01  DLI-IO-WDGX2232.                                                     
046905*    03  -COPY WDGX2232                                                   
046906     EJECT                                                                
046907                                                                          
046908 LINKAGE SECTION.                                                         
046909*01  -COPY W0009   -PRE MSG-                                              
046910     EJECT                                                                
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
051313                           WDF2-PCB ARTC2-PCB 2223-PCB WDT3-PCB           
051320                           WDR2-PCB.                                      
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
057300                                                                          
057510     IF IN-CLAG-IDLEVNR   = 'BQ8VA'    AND                                
057600        IN-CLAG-KVLS      = ZERO       AND                                
057700        IN-CLAG-KDERS     = ZERO       AND                                
057800        IN-CLAG-KDERS-UTG = ZERO                                          
057900****      ALLA  REDIRLEV FINNS MED  I URVALKRITERIERNA                    
058000                                                                          
058100        IF IN-CLAG-PRARTSTD    = ZERO  OR                                 
058200           IN-CLAG-VKART       = ZERO  OR                                 
058300           IN-CLAG-VLARTNTO    = ZERO  OR                                 
058400           IN-CLAG-KDARTURS    = SPACE OR                                 
058500           IN-CLAG-IDSTATNR(3) = ZERO                                     
058600           PERFORM HB-SKAPA-LARM                                          
058700        ELSE                                                              
058800           PERFORM HA-UPPDAT-DIR                                          
058900        END-IF                                                            
058901     END-IF                                                               
058902     .                                                                    
058903     EJECT                                                                
058904 HA-UPPDAT-DIR SECTION.                                                   
058905                                                                          
058906     PERFORM S14-SKAPA-CLASSIC-TRANS                                      
058907     PERFORM HAB-UPPDAT-MAN                                               
058908     .                                                                    
058909     EJECT                                                                
058910 HAB-UPPDAT-MAN SECTION.                                                  
058920                                                                          
058930     IF IN-CLAG-ADLAGOMR   = 49     AND                                   
058940        IN-CLAG-ADGANG     = 20     AND                                   
058950        IN-CLAG-ADPLATS    = 01     AND                                   
058960        IN-CLAG-REDIRLEV   = 1      AND                                   
058970        IN-CLAG-BEFT       = 02                                           
058980        CONTINUE                                                          
058990     ELSE                                                                 
059000          MOVE IN-CLAG-IDARTNR TO W-IDARTNR                               
059100          MOVE W-IDARTNR       TO IDARTNR-WS                              
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
063505                                                                          
063506***  SKAPA LARM  SOM I W37173   XXBU-2224  WDR550                         
063507*    FÖRST TEST OM LARM REDAN LIGGER                                      
063508*    ANNARS 'DDGS 1.0 KUNDE EJ UPPDAT'                                    
063509*  UPPLÄGG AV NYTT LARM ALT. UPDATERING AV BEFINTLIGT LARM                
063514     MOVE IN-CLAG-IDARTNR   TO W-IDARTNR W-IDARTNR-2224                   
063515     MOVE IN-CLAG-IDANSK    TO W-IDANSK-2232                              
063518     PERFORM IMS-GU-WDR220                                                
063519     IF SEGMENT-FINNS                                                     
063520       MOVE 2232-IDANSK-LARM TO W-IDANSK-2223                             
063521     ELSE                                                                 
063522       MOVE ZERO            TO W-IDANSK-2223                              
063523     END-IF                                                               
063530     PERFORM IMS-GHU-WDGX2223                                             
063540     IF SEGMENT-SAKNAS                                                    
063550       MOVE '2223'          TO 2223-IDHTYP                                
063560       MOVE W-IDANSK-2223   TO 2223-IDANSK                                
063570       MOVE LOW-VALUE       TO 2223-LOW-VALUE                             
063580       PERFORM IMS-ISRT-WDGX2223                                          
063590       ADD +1               TO CHKP-ANT                                   
063600     END-IF                                                               
063700                                                                          
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
066300      MOVE FUNCTION CURRENT-DATE(3:6)    TO  2224-TISENBEK-DAG            
066400      MOVE FUNCTION CURRENT-DATE(11:6)   TO  2224-TISENBEK-KL             
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
119618        MOVE 'W26179D1' TO POSTSUM-DDNAMN2                                
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
122708     PERFORM IMS-GU-WDT301-GE                                             
122709     IF SEGMENT-SAKNAS                                                    
122710        MOVE W-IDARTNR TO FART-IDARTNR                                    
122711        PERFORM IMS-ISRT-WDT301                                           
122712        PERFORM IMS-GU-WDT301                                             
122713     END-IF                                                               
122714     MOVE 'SE'         TO W-IDLAND                                        
122715     PERFORM IMS-GNP-WDT311                                               
122716     IF SEGMENT-FINNS AND                                                 
122717        FPCK-IDUSER    = 'W26179'     AND                                 
122720        FPCK-BEFT      = 02           AND                                 
122730        FPCK-KDFORPPL  = ZERO         AND                                 
122740        FPCK-KDFORPGP  = ZERO         AND                                 
122750        FPCK-KDFORPUF  = ZERO         AND                                 
122760        FPCK-TEBEFT(1) = 'ÖVERGÅTT TILL CLASSIC DIREKTLEVERANS'           
122770        CONTINUE                                                          
122771     ELSE                                                                 
122772**** INSERT WDK612                                                        
122773        MOVE 'W26179' TO FPCK-IDUSER                                      
122774        MOVE 02       TO FPCK-BEFT                                        
122775        MOVE ZERO     TO FPCK-KDFORPPL                                    
122776        MOVE ZERO     TO FPCK-KDFORPGP                                    
122777        MOVE ZERO     TO FPCK-KDFORPUF                                    
122778        MOVE 'ÖVERGÅTT TILL CLASSIC DIREKTLEVERANS'                       
122779                      TO FPCK-TEBEFT(1)                                   
122780        MOVE SPACE    TO FPCK-TEBEFT(2)                                   
122790        MOVE SPACE    TO FPCK-TEBEFT(3)                                   
122791        MOVE SPACE    TO FPCK-TEBEFT(4)                                   
122792        MOVE SPACE    TO FPCK-TEBEFT(5)                                   
122800        MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                    
122900        COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WS-DAREGDAT            
123000        ACCEPT WS-TID FROM TIME                                           
123100        COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WS-TID                  
123110        MOVE 'SE'     TO FPCK-IDLANDX2                                    
123200        PERFORM IMS-ISRT-WDT311                                           
123201     END-IF                                                               
123202     .                                                                    
123203     EJECT                                                                
123204  S16-SKAPA-B65 SECTION.                                                  
123205                                                                          
123206***  KAN MAN SE OM B65 REDAN SKICKAD ?                                    
123207                                                                          
123208     IF AVT-IDAVTAL =  987910987094   AND                                 
123209        AVT-IDLEVNR-AVT = 'BQ8VA'                                         
123210        CONTINUE                                                          
123220     ELSE                                                                 
123230        ACCEPT ZZAC-TIKLOCK             FROM  TIME                        
123240        ACCEPT ZZAC-TIAAMMDD FROM             DATE                        
123250        ADD 1                          TO WS-IDLOGLOP                     
123260        MOVE WS-IDLOGLOP               TO ZZAC-IDLOGLOP                   
123270        MOVE SPACE              TO A310-LEVNUM-GODSM                      
123280                                 A310-ANT-BESTANN                         
123290        MOVE 'RY2'              TO A310-KT                                
123300        MOVE DAGENS-DATUM       TO A310-DATUM-UTSKR                       
123400        MOVE ART-IDLEVNR        TO W-IDLEVNR                              
123500        IF W-IDLEVNR (5:1) = SPACE                                        
123600*         LEVNUM SKALL TILLS VIDARE VARA NUMERISKT I X(5)                 
123700          MOVE ZERO             TO TALLY                                  
123800          INSPECT W-IDLEVNR TALLYING TALLY                                
123900                             FOR CHARACTERS BEFORE INITIAL SPACE          
124000          IF TALLY = ZERO                                                 
124100             MOVE ZERO          TO WS-IDLEVNR-NUM                         
124200          ELSE                                                            
124300             MOVE W-IDLEVNR (1:TALLY)                                     
124400                                TO WS-IDLEVNR-NUM                         
124500          END-IF                                                          
124600          MOVE WS-IDLEVNR-NUM TO A310-LEVNUM                              
124700        ELSE                                                              
124800          MOVE W-IDLEVNR        TO A310-LEVNUM                            
124900        END-IF                                                            
125000        MOVE W-IDARTNR          TO WS-IDARTNR                             
125100        MOVE WS-IDARTNR         TO WS-IDARTNR-8                           
125200        MOVE WS-IDARTNR-8       TO A310-ARTNR                             
125300                                   W092-SORTBGP                           
125400                                                                          
125500        MOVE AVT-IDAVTAL        TO W-IDAVTAL-RED                          
125600        MOVE W-PREFIX           TO A310-BESTPREF                          
125700        MOVE W-AVTALSNR         TO A310-BESTLNR                           
125800        MOVE W-SUFFIX           TO A310-BESTSUFF                          
125900        MOVE A310-A310B65       TO ZZAC-LOGGPOST                          
126000        MOVE W092-AREA          TO ZZAC-SORTPOST                          
126100        PERFORM IMS-ISRT-ZZAC                                             
126101        IF SEGMENT-FINNS-REDAN                                            
126102          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
126103             ADD +1             TO WS-IDLOGLOP                            
126104             IF WS-IDLOGLOP = 9                                           
126105                ADD +1          TO ZZAC-TIKLOCK                           
126106             END-IF                                                       
126107             MOVE WS-IDLOGLOP   TO ZZAC-IDLOGLOP                          
126109             PERFORM IMS-ISRT-ZZAC                                        
126110          END-PERFORM                                                     
126111        END-IF                                                            
126112     END-IF                                                               
126113     .                                                                    
126114     EJECT                                                                
126115*                                                                         
126116 S18-SKAPA-R22POST SECTION.                                               
126117                                                                          
126118     IF (AVT-IDAVTAL =  987910987094   AND                                
126119         AVT-IDLEVNR-AVT = 'BQ8VA')    OR                                 
126120         TEST-IDINK = '987 '                                              
126121        CONTINUE                                                          
126122     ELSE                                                                 
126130        MOVE AVT-IDAVTAL TO W-IDBEST                                      
126140        MOVE NEJ         TO R22-FINNS                                     
126150        PERFORM IMS-GHU-ARTC11-2                                          
126160        IF SEGMENT-FINNS                                                  
126170          PERFORM IMS-GNP-ARTC22-2                                        
126180          PERFORM UNTIL SEGMENT-SAKNAS OR R22-FINNS = JA                  
126190             IF BEST-IDBEST       = AVT-IDAVTAL     AND                   
126200                BEST-IDLEVNR-BEST = AVT-IDLEVNR-AVT AND                   
126300                BEST-KDBEH-BEST   = 5                                     
126400                MOVE JA TO R22-FINNS                                      
126500             END-IF                                                       
126600             PERFORM IMS-GNP-ARTC22-2                                     
126700          END-PERFORM                                                     
126800                                                                          
126900          IF R22-FINNS = JA                                               
127000             CONTINUE                                                     
127100          ELSE                                                            
127200             MOVE 'R22'                TO BAS-R22-IDPTYP                  
127300             MOVE W-IDARTNR            TO BAS-R22-IDARTNR                 
127400             MOVE AVT-IDAVTAL          TO BAS-R22-IDBEST                  
127500             MOVE AVT-IDLEVNR-AVT      TO BAS-R22-IDLEVNR-BEST            
127600             MOVE SPACE                TO BAS-R22-IDLEVNR-SHIP            
127700             MOVE DAGENS-DATUM         TO BAS-R22-TIBEST                  
127800             MOVE AVT-KVAVTANT         TO BAS-R22-KVBEST                  
127900             MOVE 5                    TO BAS-R22-KDBEH-BEST              
128000             MOVE SPACE                TO BAS-R22-TENOT-BESTPRIS          
128100             MOVE SPACE                TO BAS-R22-REST                    
128200                                                                          
128300                                                                          
128400             MOVE DAGENS-DATUM         TO POST-TIREGDAT W-TIREGDAT        
128500             ADD +1                    TO POST-TIKLOCK W-TIKLOCK          
128600             MOVE WS-IDUSER            TO POST-IDUSER                     
128700             MOVE 'W26179'             TO POST-IDLTERM                    
128800             MOVE ZERO                 TO POST-TIBORT                     
128900                                                                          
129000             MOVE BAS-R22-REGPOST      TO POST-REGPOST                    
129100             PERFORM IMS-ISRT-WDG901                                      
129200             IF SEGMENT-FINNS-REDAN                                       
129300               PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                      
129400                  ADD +1 TO POST-TIKLOCK W-TIKLOCK                        
129500                  PERFORM IMS-ISRT-WDG901                                 
129600               END-PERFORM                                                
129700             END-IF                                                       
129800          END-IF                                                          
129801        END-IF                                                            
129802     END-IF                                                               
129803     .                                                                    
129804     EJECT                                                                
129805*                                                                         
129806 S19-SKAPA-R23POST SECTION.                                               
129807                                                                          
129808     MOVE 987910987094 TO W-IDAVTAL-CL                                    
129809     PERFORM IMS-GU-ARTC23-BQ8VA                                          
129810     IF SEGMENT-FINNS AND AVT-IDAVTAL =  987910987094                     
129820                      AND AVT-IDLEVNR-AVT = 'BQ8VA'                       
129830        CONTINUE                                                          
129840     ELSE                                                                 
129850        MOVE 'R23'                TO BAS-R23-IDPTYP                       
129860        MOVE W-IDARTNR            TO BAS-R23-IDARTNR                      
129870        MOVE 987910987094         TO BAS-R23-IDAVTAL                      
129880        MOVE 'BQ8VA'              TO BAS-R23-IDLEVNR-AVT                  
129890                                     BAS-R23-IDLEVNR-SHIP                 
129900        MOVE DAGENS-DATUM         TO BAS-R23-TIAVTAL                      
130000        MOVE ZERO                 TO BAS-R23-KVAVTANT                     
130100        MOVE +1                   TO BAS-R23-KDBEH-AVT                    
130200        MOVE SPACE                TO BAS-R23-TENOT-AVTPRIS                
130300        MOVE SPACE                TO BAS-R23-REST                         
130400                                                                          
130500                                                                          
130600        MOVE DAGENS-DATUM         TO POST-TIREGDAT W-TIREGDAT             
130700        ADD +1                    TO POST-TIKLOCK W-TIKLOCK               
130800        ACCEPT POST-TIKLOCK FROM TIME                                     
130900        MOVE POST-TIKLOCK TO W-TIKLOCK                                    
131000        MOVE WS-IDUSER            TO POST-IDUSER                          
131100        MOVE 'W26179'             TO POST-IDLTERM                         
131200        MOVE ZERO                 TO POST-TIBORT                          
131300                                                                          
131400        MOVE BAS-R23-REGPOST      TO POST-REGPOST                         
131500        PERFORM IMS-ISRT-WDG901                                           
131600        IF SEGMENT-FINNS-REDAN                                            
131700          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
131800             ADD +1 TO POST-TIKLOCK W-TIKLOCK                             
131900             PERFORM IMS-ISRT-WDG901                                      
132000          END-PERFORM                                                     
132100        END-IF                                                            
132101     END-IF                                                               
132102     .                                                                    
132103     EJECT                                                                
132104                                                                          
132105 X-TAG-CHECKPOINT   SECTION.                                              
132106                                                                          
132107     PERFORM IMS-CHECKPOINT                                               
132108     MOVE ZERO TO CHKP-ANT                                                
132109     .                                                                    
132110     EJECT                                                                
132111* --- IMS SEKTIONER ---                                                   
132112     SKIP3                                                                
132113 IMS-GHU-ARTC01 SECTION.                                                  
132114     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
132115          DELIMITED BY SIZE INTO SSA1                                     
132116     MOVE '  GE' TO GODK-STATUSKODER                                      
132117     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC01 SSA1                 
132118     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132119     PERFORM IMS-STATUSKONTROLL                                           
132120     .                                                                    
132121     SKIP3                                                                
132122 IMS-GHU-ARTC11 SECTION.                                                  
132123     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
132124          DELIMITED BY SIZE INTO SSA1                                     
132125     MOVE 'WDK611   ' TO SSA2                                             
132126     MOVE '  GE' TO GODK-STATUSKODER                                      
132127     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2            
132128     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132129     PERFORM IMS-STATUSKONTROLL                                           
132130     .                                                                    
132131     SKIP3                                                                
132132 IMS-GNP-ARTC23 SECTION.                                                  
132133     MOVE  'WDK623   ' TO  SSA1                                           
132134     MOVE '  GE' TO GODK-STATUSKODER                                      
132135     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC23 SSA1                 
132136     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132137     PERFORM IMS-STATUSKONTROLL                                           
132138     .                                                                    
132139     SKIP2                                                                
132140 IMS-GU-ARTC23-BQ8VA SECTION.                                             
132141     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
132142          DELIMITED BY SIZE INTO SSA1                                     
132143     MOVE 'WDK611   ' TO SSA2                                             
132144     STRING 'WDK623  (IDAVTAL  =' W-IDAVTAL-CL-X ')'                      
132145          DELIMITED BY SIZE INTO SSA3                                     
132146     MOVE '  GE' TO GODK-STATUSKODER                                      
132147     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC23 SSA1 SSA2             
132148                                                    SSA3                  
132149     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132150     PERFORM IMS-STATUSKONTROLL                                           
132151     .                                                                    
132152     SKIP2                                                                
132153 IMS-REPL-ARTC11 SECTION.                                                 
132154     MOVE '  ' TO GODK-STATUSKODER                                        
132155     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
132156     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
132157     PERFORM IMS-STATUSKONTROLL                                           
132158     ADD +1  TO CHKP-ANT                                                  
132159     .                                                                    
132160     EJECT                                                                
132161 IMS-GU-WDT301-GE SECTION.                                                
132162     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
132163          DELIMITED BY SIZE INTO SSA1                                     
132165     MOVE '  GE' TO GODK-STATUSKODER                                      
132166     CALL CBLTDLI USING GU   WDT3-PCB DLI-IO-WDT301  SSA1                 
132167     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
132168     PERFORM IMS-STATUSKONTROLL                                           
132169     .                                                                    
132170     SKIP3                                                                
132171 IMS-GU-WDT301 SECTION.                                                   
132172     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
132173          DELIMITED BY SIZE INTO SSA1                                     
132174     MOVE '    ' TO GODK-STATUSKODER                                      
132175     CALL CBLTDLI USING GU   WDT3-PCB DLI-IO-WDT301  SSA1                 
132176     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
132177     PERFORM IMS-STATUSKONTROLL                                           
132178     .                                                                    
132179     SKIP3                                                                
132180 IMS-ISRT-WDT301 SECTION.                                                 
132181     MOVE 'WDT301   ' TO SSA1                                             
132182     MOVE '  II' TO GODK-STATUSKODER                                      
132183     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
132184     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
132185     PERFORM IMS-STATUSKONTROLL                                           
132186     ADD +1  TO CHKP-ANT                                                  
132187     .                                                                    
132188     SKIP3                                                                
132189 IMS-GNP-WDT311 SECTION.                                                  
132190     STRING 'WDT311  (IDLAND   =' W-IDLAND-X ')'                          
132191          DELIMITED BY SIZE INTO SSA1                                     
132192     MOVE '  GE' TO GODK-STATUSKODER                                      
132193     CALL CBLTDLI USING GNP  WDT3-PCB DLI-IO-WDT311 SSA1                  
132194     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
132195     PERFORM IMS-STATUSKONTROLL                                           
132196     .                                                                    
132197     SKIP3                                                                
132198 IMS-ISRT-WDT311 SECTION.                                                 
132199     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
132200          DELIMITED BY SIZE INTO SSA1                                     
132201     MOVE 'WDT311   ' TO SSA2                                             
132202     MOVE '    ' TO GODK-STATUSKODER                                      
132203     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
132204     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
132205     PERFORM IMS-STATUSKONTROLL                                           
132206     ADD +1  TO CHKP-ANT                                                  
132207     .                                                                    
132208     SKIP3                                                                
132209 IMS-ISRT-ZZAC SECTION.                                                   
132210                                                                          
132211     MOVE 'WLZZAC01 '        TO SSA1                                      
132212     MOVE '  II'             TO GODK-STATUSKODER                          
132213     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA5 SSA1                   
132214     MOVE ZZAC-STATUS-CODE     TO STATUS-WS                               
132215     PERFORM IMS-STATUSKONTROLL                                           
132216     ADD +1  TO CHKP-ANT                                                  
132217     .                                                                    
132218     SKIP3                                                                
132219 IMS-ISRT-WDG901 SECTION.                                                 
132220                                                                          
132221     MOVE 'WLZZAD01 ' TO SSA1                                             
132222     MOVE '  II' TO GODK-STATUSKODER                                      
132223     CALL CBLTDLI USING ISRT ZZAD-PCB DLI-IO-WDG901 SSA1                  
132224     MOVE ZZAD-STATUS-CODE TO STATUS-WS                                   
132225     PERFORM IMS-STATUSKONTROLL                                           
132226     ADD +1  TO CHKP-ANT                                                  
132227     .                                                                    
132228     EJECT                                                                
132229                                                                          
132230 IMS-GU-WDF212-KVAL SECTION.                                              
132231                                                                          
132232     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
132233          DELIMITED BY SIZE INTO SSA1                                     
132234     STRING 'WDF212  (IDARTNR  =' W-IDARTNR-WDF2-X ')'                    
132235          DELIMITED BY SIZE INTO SSA2                                     
132236     MOVE 'GE    '     TO GODK-STATUSKODER                                
132237     CALL CBLTDLI USING GU WDF2-PCB DLI-IO-WDF212 SSA1 SSA2               
132238     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
132239     PERFORM IMS-STATUSKONTROLL                                           
132240     .                                                                    
132241     EJECT                                                                
132242 IMS-ISRT-WDF212 SECTION.                                                 
132243                                                                          
132244     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
132245          DELIMITED BY SIZE INTO SSA1                                     
132246     MOVE 'WDF212         ' TO SSA2                                       
132247     MOVE '  IINI' TO GODK-STATUSKODER                                    
132248     CALL CBLTDLI USING ISRT WDF2-PCB DLI-IO-WDF212 SSA1 SSA2             
132249     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
132250     PERFORM IMS-STATUSKONTROLL                                           
132251     ADD +1  TO CHKP-ANT                                                  
132252     .                                                                    
132253     EJECT                                                                
132254 IMS-GHU-ARTC11-2 SECTION.                                                
132255     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
132256          DELIMITED BY SIZE INTO SSA1-2                                   
132257     MOVE 'WDK611   ' TO SSA2-2                                           
132258     MOVE '  GE' TO GODK-STATUSKODER                                      
132259     CALL CBLTDLI USING GHU ARTC2-PCB DLI-IO-WLARTC11                     
132260                        SSA1-2 SSA2-2                                     
132261     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
132262     PERFORM IMS-STATUSKONTROLL                                           
132263     .                                                                    
132264     SKIP3                                                                
132265 IMS-GNP-ARTC22-2 SECTION.                                                
132266     MOVE  'WDK622   ' TO  SSA1-2                                         
132267     MOVE '  GE' TO GODK-STATUSKODER                                      
132268     CALL CBLTDLI USING GNP ARTC2-PCB DLI-IO-WLARTC22 SSA1-2              
132269     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
132270     PERFORM IMS-STATUSKONTROLL                                           
132271     .                                                                    
132272     SKIP2                                                                
132273 IMS-GHU-WDGX2223 SECTION.                                                
132274     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
132275          DELIMITED BY SIZE INTO SSA1                                     
132276     MOVE '  GE'           TO GODK-STATUSKODER                            
132277     CALL CBLTDLI USING GHU 2223-PCB DLI-IO-WDGX2223 SSA1                 
132278     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
132279     PERFORM IMS-STATUSKONTROLL                                           
132280     .                                                                    
132290                                                                          
132300 IMS-ISRT-WDGX2223 SECTION.                                               
132400     MOVE 'WDR501   '      TO SSA1                                        
132500     MOVE '  II'           TO GODK-STATUSKODER                            
132600     CALL CBLTDLI USING ISRT 2223-PCB DLI-IO-WDGX2223 SSA1                
132700     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
132701     PERFORM IMS-STATUSKONTROLL                                           
132702     ADD +1  TO CHKP-ANT                                                  
132703     .                                                                    
132704                                                                          
132705 IMS-GHNP-WDGX2224 SECTION.                                               
132706     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
132707          DELIMITED BY SIZE INTO SSA1                                     
132708     STRING 'WDR550  (IDARTNR  =' W-IDARTNR-2224-X                        
132709                    '&IDDC     =' W-IDDC-X ')'                            
132710          DELIMITED BY SIZE INTO SSA2                                     
132711     MOVE '  GE'           TO GODK-STATUSKODER                            
132720     CALL CBLTDLI USING GHNP 2223-PCB DLI-IO-WDGX2224 SSA1 SSA2           
132730     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
132740     PERFORM IMS-STATUSKONTROLL                                           
132750     .                                                                    
132760                                                                          
132770 IMS-ISRT-WDGX2224 SECTION.                                               
132780     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
132790          DELIMITED BY SIZE INTO SSA1                                     
132800     MOVE 'WDR550   '      TO SSA2                                        
132900     MOVE '  II'           TO GODK-STATUSKODER                            
133000     CALL CBLTDLI USING ISRT 2223-PCB DLI-IO-WDGX2224 SSA1 SSA2           
133100     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
133101     PERFORM IMS-STATUSKONTROLL                                           
133102     ADD +1  TO CHKP-ANT                                                  
133103     .                                                                    
133104                                                                          
133105 IMS-REPL-WDGX2224 SECTION.                                               
133106     MOVE '  '             TO GODK-STATUSKODER                            
133107     CALL CBLTDLI USING REPL 2223-PCB DLI-IO-WDGX2224                     
133108     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
133109     PERFORM IMS-STATUSKONTROLL                                           
133110     ADD +1  TO CHKP-ANT                                                  
133120     .                                                                    
133130                                                                          
133131 IMS-GU-WDR220 SECTION.                                                   
133132                                                                          
133133     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
133134          DELIMITED BY SIZE INTO SSA1                                     
133135     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
133136          DELIMITED BY SIZE INTO SSA2                                     
133137     MOVE '  GE' TO GODK-STATUSKODER                                      
133138     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2232 SSA1 SSA2             
133139     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
133140     PERFORM IMS-STATUSKONTROLL                                           
133141     .                                                                    
133142     EJECT                                                                
133143 IMS-RESTART SECTION.                                                     
133144     SKIP2                                                                
133145     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
133146     MOVE '  ' TO GODK-STATUSKODER                                        
133147     CALL CBLTDLI USING XRST MSG-PCB                                      
133148                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
133149                        CHKP-AREA-LENGTH CHKP-AREA                        
133150     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
133151     PERFORM IMS-STATUSKONTROLL                                           
133152     .                                                                    
133153     SKIP3                                                                
133154 IMS-CHECKPOINT SECTION.                                                  
133160     SKIP2                                                                
133200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
133300     MOVE '  XD' TO GODK-STATUSKODER                                      
133400     CALL CBLTDLI USING CHKP MSG-PCB                                      
133500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
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
