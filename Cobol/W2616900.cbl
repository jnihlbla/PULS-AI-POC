000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2616900.                                                
000300 AUTHOR.         P-A HELGEGREN, KOPIA W60322.                             
000400 DATE-WRITTEN.   07/08/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        RELEASE SCRAP ORDERS                                             
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001200*                                6321 (WDR5)                              
001300*                                6327 (WDR5)                              
001400*                                2402 (WDR5)                              
001510*                                FILC (WDR3)                              
001520*                                WDD8                                     
001530*                                WDJ9                                     
001540*                                ZZAC (WDG6)                              
001550*                                XXAV (WDG2)                              
001560*                                ZZAD (WDG9)                              
001570*                                WDF2                                     
001600*                   STARTAR RUTIN W216S1 I SOP                            
001700*                   (SKAPAR SKROTORDER).                                  
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T322                                              
002100*        INFIL        W26169                                              
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
003400*          --- ARTIKLAR ATT SKROTA AUTO                                   
003500     SELECT W26169                     ASSIGN TO W26169D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W26169                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY W26169   -L.                                                   
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  IDPGM                       PIC X(08)   VALUE 'W2616900'.            
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
006200 77  URV-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
006300 77  URV-IX-MAX                  PIC S9(3)  VALUE +12   COMP-3.           
006400 77  IX-BEEMB                    PIC S9(4)   VALUE +0  COMP SYNC.         
006500 77  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
006600 77  MAX-IX                      PIC S9(4)   VALUE +11 COMP SYNC.         
006700 77  DAGENS-DATUM-Y2K            PIC 9(8)   VALUE ZERO.                   
006800 77  WS-ANTAL-X                  PIC 9(3)   VALUE ZERO COMP-3.            
006900 77  W-TID                       PIC 9(8)   VALUE ZERO.                   
007000 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
007100 77  TEST-IDINK                  PIC 9(3)    VALUE ZERO.                  
007200 77  WS-IDLOGLOP                 PIC 9(01) COMP-3 VALUE ZERO.             
007300 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
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
008900 77  WS-FL6326                   PIC X       VALUE 'N'.                   
009000                                                                          
009100 77  KDARBTYP-SOEKNING           PIC X       VALUE 'N'.                   
009200 77  KDARBTYP-PERSON-SOEKNING    PIC X       VALUE 'N'.                   
009300 77  IDARTNR-SOEKNING            PIC X       VALUE 'N'.                   
009400 77  DATUM-SOEKNING              PIC X       VALUE 'N'.                   
009500 77  DATUM-IDARTNR-SOEKNING      PIC X       VALUE 'N'.                   
009600 77  IDDC-SOEKNING               PIC X       VALUE 'N'.                   
009700 77  IDPERSON-SOEKNING           PIC X       VALUE 'N'.                   
009800 77  WS-DASKROT9                 PIC 9(8)    VALUE ZERO.                  
009900 77  WS-SUARTSTD                 PIC 9(7)V9(2) VALUE ZERO.                
010000 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
010100 77  WS-DASKROT9-BEORD           PIC 9(8)    VALUE ZERO.                  
010200 77  WS-6322-DASKROT9            PIC 9(8)    VALUE ZERO.                  
010300 77  WS-IDPERSON-FOM             PIC X(3)    VALUE ZERO.                  
010400 77  WS-IDPERSON-TOM             PIC X(3)    VALUE ZERO.                  
010500 77  W-KVSKROT-REST              PIC S9(7)  VALUE ZERO.                   
010600 77  W-ADBUFFPL                  PIC 9(5)  VALUE ZERO.                    
010700 77  TEST-NYCKEL-IDARTNR         PIC X(9)    VALUE SPACE.                 
010800 77  TEST-NYCKEL-TIDATUM         PIC X(6)    VALUE SPACE.                 
010900 77  TEST-NYCKEL-IDANSK          PIC X(3)    VALUE SPACE.                 
011000 77  WS-FLHOGRE                  PIC X       VALUE 'N'.                   
011100 77  WS-FLURVAL                  PIC X       VALUE 'N'.                   
011200 77  WS-HIGHLEV                  PIC X       VALUE 'N'.                   
011300 77  WS-SUBEL                    PIC 9(7)    VALUE ZERO.                  
011400 77  W-ANNUL-IDUSER              PIC X(8)    VALUE SPACE.                 
011500 77  W-ANNUL-IDMAIL              PIC X(60)   VALUE SPACE.                 
011600 01  ANUL-BEANST-GODK            PIC X(25) VALUE SPACE.                   
011700 01  ANUL-IDMAIL                 PIC X(60) VALUE SPACE.                   
011800 01  WSM-IDARTNR                 PIC Z(8)9 VALUE ZERO.                    
011810 01  SPAR-IDKUNDNR               PIC S9(9) VALUE ZERO COMP-3.             
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
013000 77  W26169-EOF-SW               PIC X       VALUE 'N'.                   
013010     88  END-OF-W26169                       VALUE 'J'.                   
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
014400     EJECT                                                                
014500 01  TABELL-SKROT-VARDE.                                                  
014600     03  W-VARDE-PER-RAD         OCCURS 12.                               
014700         05 W-SUARTSTD           PIC 9(7)V9(2) VALUE ZERO.                
014800                                                                          
014900 01  TABELL-TEMEMO.                                                       
015000     03  W-TEMEMO-RAD            OCCURS 11.                               
015100         05 W-TEMEMO             PIC X(66) VALUE SPACE.                   
015200*      --- VALID IDDC CODES                                               
015300*                                                                         
015400*01    -COPY WWDC99                                                       
015500*01    -COPY WWDC99 -PRE SW-                                              
015600       EJECT                                                              
016400                                                                          
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
019300 01  WS-BC-PARAMETRAR.                                                    
019400     03  WS-URVAL.                                                        
019500         05  URV-FLKLAR       PIC X     VALUE SPACE.                      
019600         05  URV-KDARBTYP     PIC X(8)  VALUE SPACE.                      
019700     03  URV-TABELL.                                                      
019800         05 URV-TAB-RAD OCCURS 12.                                        
019900            07  URV-IDDC             PIC X(2).                            
020000            07  URV-IDARTNR          PIC 9(9).                            
020100            07  URV-DASKROT9-BEORD   PIC 9(8).                            
020200 01  BAS-R22-REGPOST.                                                     
020300*    03      -COPY W212R22   -PRE BAS-R22-                                
020400     03 BAS-R22-REST            PIC X(41).                                
020500     EJECT                                                                
020600 01  BAS-R23-REGPOST.                                                     
020700*    03      -COPY W212R23   -PRE BAS-R23-                                
020800     03 BAS-R23-REST            PIC X(41).                                
020900     EJECT                                                                
021000     EJECT                                                                
021100*    --- CLASSIC TRANS                                                    
021200*01 -COPY W21632            -PRE FILC-                                    
021300     EJECT                                                                
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
026900*01  AREA -COPY W26169     -PRE IN-                                       
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
028200     03  W-IDPERSON-MIN-X.                                                
028300         05  W-IDPERSON-MIN    PIC S9(3)  VALUE ZERO COMP-3.              
028400     03  W-IDPERSON-MAX-X.                                                
028500         05  W-IDPERSON-MAX    PIC S9(3)  VALUE +999 COMP-3.              
028600     03  W-IDDC-MIN-X.                                                    
028700         05  W-IDDC-MIN      PIC X(2)   VALUE LOW-VALUE.                  
028800     03  W-IDDC-MAX-X.                                                    
028900         05  W-IDDC-MAX      PIC X(2)   VALUE HIGH-VALUE.                 
029000     03  W-DASKROT9-MIN-X.                                                
029100         05  W-DASKROT9-MIN  PIC 9(8)   VALUE ZERO.                       
029200     03  W-DASKROT9-MAX-X.                                                
029300         05  W-DASKROT9-MAX  PIC 9(8)   VALUE 99999999.                   
029400     03  W-IDARTNR-X.                                                     
029500         05  W-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.                
029600     03  W-IDDC-X.                                                        
029700         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
029800     03  W-KDARBTYP-X.                                                    
029900         05  W-KDARBTYP      PIC X(8)   VALUE 'ANSK'.                     
030000     03  W-DASKROT9-X.                                                    
030100         05  W-DASKROT9      PIC 9(8)   VALUE ZERO.                       
030200     03  W-IDDC-6324-X.                                                   
030300         05  W-IDDC-6324      PIC X(2)   VALUE SPACE.                     
030400     03  W-KDSTASKR-X.                                                    
030500         05  W-KDSTASKR      PIC S9     VALUE 1 COMP-3.                   
030600     03  W-WDGXKEY-6321.                                                  
030700         05  W-6321-IDHTYP    PIC X(4)   VALUE '6321'.                    
030800         05  W-6321-KDARBTYP  PIC X(8)   VALUE SPACE.                     
030900         05  W-6321-LOWVALUE  PIC X(18)  VALUE LOW-VALUE.                 
031000     03  W-WDGXKEY-6327.                                                  
031100         05  W-6327-IDHTYP    PIC X(4)   VALUE '6327'.                    
031200         05  W-6327-KDARBTYP  PIC X(8)   VALUE SPACE.                     
031300         05  W-6327-IDDC      PIC X(2)   VALUE SPACE.                     
031400         05  W-6327-LOWVALUE  PIC X(16)  VALUE LOW-VALUE.                 
031500     03  W-IDUSER-GODK-X.                                                 
031600         05  W-IDUSER-GODK    PIC X(8)   VALUE SPACE.                     
031700     03  W-SUBEL-MIN-X.                                                   
031800         05  W-SUBEL-MIN      PIC 9(7)  VALUE ZERO.                       
031900     03  W-SUBEL-MAX-X.                                                   
032000         05  W-SUBEL-MAX      PIC 9(7)  VALUE 9999999.                    
032100     03 W-2401-KEY-X.                                                     
032200         05 FILLER               PIC X(4)    VALUE '2401'.                
032300         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
032400     03  W-KY6324-KVAL-X.                                                 
032500         05  W-IDARTNR-KVAL  PIC S9(9)  VALUE ZERO COMP-3.                
032600         05  W-IDDC-KVAL     PIC X(2)   VALUE SPACE.                      
032700         05  W-KDSTASKR-KVAL PIC S9     VALUE ZERO COMP-3.                
032800     03  W-IDRADNR-X.                                                     
032900         05  W-IDRADNR       PIC S9(5)  VALUE ZERO COMP-3.                
033000     03  W-1141-KEY-X.                                                    
033100         05 FILLER         PIC X(04)  VALUE '1141'.                       
033200         05 FILLER         PIC X(26)  VALUE LOW-VALUE.                    
033300     03  W-IDLEVNR-X.                                                     
033400         05 W-IDLEVNR      PIC X(5)   VALUE SPACE.                        
033500     03  W-WDG901KY-X.                                                    
033600         05  W-TIREGDAT          PIC S9(07)   VALUE ZERO COMP-3.          
033700         05  W-TIKLOCK           PIC S9(09)   VALUE ZERO COMP-3.          
033800                                                                          
033900     03  W-WDF201KY-X.                                                    
034000         05  W-IDLEVNR-WDF2      PIC  X(5)   VALUE SPACE.                 
034100         05  W-IDDIRGRP          PIC X(10)   VALUE SPACE.                 
034200                                                                          
034300                                                                          
034400     EJECT                                                                
034500                                                                          
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
037400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC23'.                    
037500 01  DLI-IO-WLARTC23.                                                     
037600*    03  -COPY WDK623                                                     
037700     EJECT                                                                
037710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301  '.                    
037720 01  DLI-IO-WDT301.                                                       
037730*    03  -COPY WDT301                                                     
037731 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311  '.                    
037732 01  DLI-IO-WDT311.                                                       
037733*    03  -COPY WDT311                                                     
037740     EJECT                                                                
038200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
038300 01  DLI-IO-WDR501-6321.                                                  
038400*    03  -COPY WDGX6321                                                   
038500     EJECT                                                                
038600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
038700 01  DLI-IO-WDGX6322.                                                     
038800*    03  -COPY WDGX6322                                                   
038900     EJECT                                                                
039000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
039100 01  DLI-IO-WDGX6324.                                                     
039200*    03  -COPY WDGX6324                                                   
039300     EJECT                                                                
039400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6326'.                    
039500 01  DLI-IO-WDGX6326.                                                     
039600*    03  -COPY WDGX6326                                                   
039700     EJECT                                                                
040200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6327'.                    
040300 01  DLI-IO-WDGX6327.                                                     
040400*    03  -COPY WDGX6327                                                   
040500     EJECT                                                                
040600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
040700 01  DLI-IO-WDGX6328.                                                     
040800*    03  -COPY WDGX6328                                                   
040900     EJECT                                                                
041000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-2401'.                 
041100 01  DLI-IO-WDR501-2401.                                                  
041200*    03  -COPY WDGX2402                                                   
041300     EJECT                                                                
041400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLFILC'.                      
041500 01  DLI-IO-AREA-FILC.                                                    
041600*    03  -COPY WDR301       -PRE FILC-                                    
041700     EJECT                                                                
041800                                                                          
041900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD801'.                      
042000 01  DLI-IO-WDD801.                                                       
042100*    03  -COPY WDD801                                                     
042200     EJECT                                                                
042300                                                                          
042400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD811'.                      
042500 01  DLI-IO-WDD811.                                                       
042600*    03  -COPY WDD811                                                     
042700     EJECT                                                                
042800                                                                          
042900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDJ901'.                      
043000 01  DLI-IO-WDJ901.                                                       
043100*    03  -COPY WDJ901                                                     
043200     EJECT                                                                
043300                                                                          
043400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDJ911'.                      
043500 01  DLI-IO-WDJ911.                                                       
043600*    03  -COPY WDJ911                                                     
043700     EJECT                                                                
043800 01  DLI-IO-AREA5.                                                        
043900     03  IO-AREA5              PIC X(150) VALUE SPACE.                    
044000                                                                          
044100*    03  ZZAC -COPY WDGZ01     -PRE ZZAC-  -RED IO-AREA5.                 
044200     EJECT                                                                
044300 01  DLI-IO-AREA6.                                                        
044400     03  IO-AREA6              PIC X(50) VALUE SPACE.                     
044500                                                                          
044600*    03  XXAV -COPY WDGX1142   -PRE XXAV-  -RED IO-AREA6.                 
044700     EJECT                                                                
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
046000                                                                          
046100 LINKAGE SECTION.                                                         
046200*01  -COPY W0009   -PRE MSG-                                              
046300     EJECT                                                                
046400*01  -COPY W0009   -PRE ALT-                                              
046500     EJECT                                                                
047000*01  -COPY W0009   -PRE ALT4-                                             
047100     EJECT                                                                
047500*01  -COPY W0008   -PRE ARTC-                                             
047600     05  FILLER                  PIC X.                                   
047700     EJECT                                                                
048100*01  -COPY W0008   -PRE 6321-                                             
048200     05  FILLER                  PIC X.                                   
048300     EJECT                                                                
048400*01  -COPY W0008   -PRE 6327-                                             
048500     05  FILLER                  PIC X.                                   
048600     EJECT                                                                
048700*01  -COPY W0008   -PRE 2401-                                             
048800     05  FILLER                  PIC X.                                   
048900     EJECT                                                                
049000*01  -COPY W0008   -PRE FILC-                                             
049100     05  FILLER                  PIC X.                                   
049200     EJECT                                                                
049300*01  -COPY W0008  -PRE WDD8-                                              
049400     05  FILLER                  PIC X.                                   
049500     EJECT                                                                
049600*01  -COPY W0008  -PRE WDJ9-                                              
049700     05  FILLER                  PIC X.                                   
049800     EJECT                                                                
050200*01  -COPY W0008     -PRE ZZAC-                                           
050300         05  FILLER           PIC X.                                      
050400     EJECT                                                                
050500*01  -COPY W0008     -PRE XXAV-                                           
050600         05  FILLER           PIC X.                                      
050700     EJECT                                                                
050800*01  -COPY W0008  -PRE ZZAD-                                              
050900     05  FILLER                  PIC X.                                   
051000     EJECT                                                                
051100*01  -COPY W0008  -PRE WDF2-                                              
051200     05  FILLER                  PIC X.                                   
051300     EJECT                                                                
051310*01  -COPY W0008  -PRE WDT3-                                              
051320     05  FILLER                  PIC X.                                   
051330     EJECT                                                                
051400 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
051500                           ALT4-PCB                                       
051600                           ARTC-PCB 6321-PCB 6327-PCB                     
051700                           2401-PCB FILC-PCB WDD8-PCB WDJ9-PCB            
051800                           ZZAC-PCB XXAV-PCB ZZAD-PCB                     
051900                           WDF2-PCB WDT3-PCB.                             
052000 MAIN SECTION.                                                            
052100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
052200                           ALT4-PCB                                       
052300                           ARTC-PCB 6321-PCB 6327-PCB                     
052400                           2401-PCB FILC-PCB WDD8-PCB WDJ9-PCB            
052500                           ZZAC-PCB XXAV-PCB ZZAD-PCB                     
052600                           WDF2-PCB WDT3-PCB.                             
052700                                                                          
052800     PERFORM A-INIT                                                       
052900     PERFORM S00-LAES-W26169                                              
052910     IF NOT END-OF-W26169                                                 
052920        MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR                                 
052930     END-IF                                                               
053000     PERFORM UNTIL END-OF-W26169                                          
053100       IF CHKP-ANT > CHKP-MAX                                             
053200         PERFORM X-TAG-CHECKPOINT                                         
053300       END-IF                                                             
053400                                                                          
053500       PERFORM H-UPPDATERA                                                
053600                                                                          
053700       PERFORM S00-LAES-W26169                                            
053704                                                                          
053705       ADD +1           TO URV-IX                                         
053710       IF URV-IX > 12 OR                                                  
053711          END-OF-W26169 OR                                                
053712          IN-IDKUNDNR NOT = SPAR-IDKUNDNR                                 
053720          PERFORM UNTIL URV-IX > 12                                       
053730             MOVE SPACE TO URV-IDDC           (URV-IX)                    
053740             MOVE ZERO  TO URV-IDARTNR        (URV-IX)                    
053750             MOVE ZERO  TO URV-DASKROT9-BEORD (URV-IX)                    
053760             ADD +1     TO URV-IX                                         
053770          END-PERFORM                                                     
053780          PERFORM S02-STARTA-URV-TRANS                                    
053781          MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR                               
053782          MOVE +1       TO URV-IX                                         
053790       END-IF                                                             
053791                                                                          
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
055200     OPEN INPUT W26169                                                    
055300                                                                          
055400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
055500                                                                          
055600                                                                          
055700     ACCEPT DAGENS-DATUM FROM DATE                                        
055800     MOVE 'W2616900'      TO FILC-FIL-IDPGM                               
055900     MOVE DAGENS-DATUM    TO FILC-FIL-TIREGDAT                            
056000     MOVE 'W21632  '      TO FILC-FIL-IDCPYTXT                            
056100     MOVE ZERO            TO FILC-FIL-TIKLOCK                             
056300     MOVE 1               TO W-KDSTASKR-KVAL                              
056400     ACCEPT POST-TIKLOCK FROM TIME                                        
056500     MOVE POST-TIKLOCK TO W-TIKLOCK                                       
056510                                                                          
056520     MOVE +1 TO URV-IX                                                    
056600     .                                                                    
056700     EJECT                                                                
056800                                                                          
056900 H-UPPDATERA SECTION.                                                     
057200                                                                          
057300     PERFORM HC-SKAPA-EN-SKROTORDER                                       
057400                                                                          
057500     .                                                                    
057600     EJECT                                                                
057700 HC-SKAPA-EN-SKROTORDER SECTION.                                          
057800                                                                          
057900                                                                          
058000     MOVE IN-IDARTNR          TO W-IDARTNR                                
058100     MOVE W-IDARTNR           TO IDARTNR-WS                               
058200     MOVE IN-IDDC             TO W-IDDC                                   
058300                                 W-IDDC-6324                              
058400                                 W-6327-IDDC                              
058500                                 WS-IDDC                                  
058600     MOVE W-KDARBTYP          TO W-6321-KDARBTYP                          
058700     COMPUTE WS-DASKROT9-BEORD = 99999999 - IN-DADATUM                    
058800     MOVE WS-DASKROT9-BEORD TO W-DASKROT9                                 
058900                               WS-6322-DASKROT9                           
059000     MOVE ZERO                TO 6324-KVSKROT-BEORD                       
059100                                                                          
059200       PERFORM IMS-GHU-WDR501-6321                                        
059300       IF SEGMENT-FINNS                                                   
059400          PERFORM IMS-GHNP-WDGX6324                                       
059500          IF SEGMENT-FINNS                                                
059600             MOVE 'J' TO 6324-FLSKROT-GODK                                
059700             PERFORM IMS-REPL-WDGX6324                                    
059800             IF 6324-IDKUNDNR = 111                                       
059900                PERFORM S14-SKAPA-CLASSIC-TRANS                           
060000             END-IF                                                       
060100          END-IF                                                          
060200       END-IF                                                             
060300       PERFORM HE-UPDATERA-6326                                           
060400       MOVE 'J' TO URV-FLKLAR                                             
060500       PERFORM S01-SKAPA-URV-TRANS                                        
060600       PERFORM S15-SKAPA-WDGX2402                                         
060800                                                                          
060900**************                                                            
061000       MOVE W-IDDC  TO SW-WS-IDDC                                         
061100       IF SW-CDC-SE                                                       
061200          PERFORM IMS-GHU-ARTC01                                          
061210          PERFORM IMS-GHU-ARTC11                                          
061300          IF SEGMENT-FINNS                                                
061400             COMPUTE CLAG-KVSPARR-KVAL =                                  
061500                     CLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD               
061600             IF CLAG-KVSPARR-KVAL < ZERO                                  
061700                MOVE ZERO    TO CLAG-KVSPARR-KVAL                         
061800             END-IF                                                       
061900             MOVE WS-IDUSER    TO CLAG-IDUSER-SPKVAL                      
062000             MOVE DAGENS-DATUM TO CLAG-TISPARR-KVAL                       
062100                                                                          
062200             MOVE 'N' TO CLAG-FLSKROT-BEORD                               
062300             MOVE 'J' TO CLAG-FLSKROT-AUTO                                
062400             MOVE DAGENS-DATUM                                            
062500                         TO CLAG-TISKROT                                  
062600             IF 6324-IDKUNDNR = 11         AND                            
062700                6324-IDUSER   = 'W2616800' AND                            
062800                CLAG-KDERS    = 00         AND                            
062810               (ART-IDLEVNR NOT = 'BQ8VA')                                
062900                MOVE 09 TO PROGSW-MID-KDERS                               
063000                MOVE 1  TO PROGSW-MID-DIERS-ERS                           
063100                PERFORM S10-SKAPA-TRANS-TILL-1113                         
063200             END-IF                                                       
063300             PERFORM IMS-REPL-ARTC11                                      
063400             PERFORM S04-BOKA-NER-BUFFERT                                 
063500          END-IF                                                          
063600       END-IF                                                             
063700     .                                                                    
063800     EJECT                                                                
063900                                                                          
064000 HE-UPDATERA-6326 SECTION.                                                
064100                                                                          
064200     ACCEPT WS-TID FROM TIME                                              
064300     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATUM                          
064400     MOVE WS-TID (1:6)               TO WS-TIDHHMMSS                      
064500     MOVE WS-TIDATETIME              TO 6326-TIDATETIME                   
064600     MOVE WS-IDUSER                  TO 6326-IDUSER-GODK                  
064700     MOVE WS-IDUSER                  TO W-IDUSER-GODK-X                   
064800     PERFORM IMS-GU-WDGX6328                                              
064900     MOVE 6328-BEANST-GODK           TO 6326-BEANST-GODK                  
065000     PERFORM IMS-ISRT-WDGX6326                                            
065100     .                                                                    
065200     EJECT                                                                
065300 Z-FINIT SECTION.                                                         
065400                                                                          
065500                                                                          
065600     CLOSE W26169                                                         
065700                                                                          
065800     .                                                                    
065900     EJECT                                                                
066000 S00-LAES-W26169  SECTION.                                                
066100     SKIP2                                                                
066200     READ W26169 INTO IN-AREA                                             
066300     AT END                                                               
066400        SET END-OF-W26169 TO TRUE                                         
066500                                                                          
066600     NOT AT END                                                           
066700        MOVE 'W26169'   TO POSTSUM-FDNAMN                                 
066800        MOVE 'W26169D1' TO POSTSUM-DDNAMN2                                
066900        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
067000        CALL POSTSUM USING POSTSUM-PARM                                   
067100                                                                          
067200     END-READ                                                             
067300     .                                                                    
067400     EJECT                                                                
067500 S01-SKAPA-URV-TRANS SECTION.                                             
067600                                                                          
067700     MOVE W-KDARBTYP           TO URV-KDARBTYP                            
067800                                                                          
067900     MOVE IN-IDARTNR           TO URV-IDARTNR(URV-IX)                     
068000     MOVE IN-IDDC              TO URV-IDDC(URV-IX)                        
068100                                                                          
068200     COMPUTE WS-DASKROT9-BEORD = 999999999 - IN-DADATUM                   
068300     MOVE WS-DASKROT9-BEORD    TO URV-DASKROT9-BEORD(URV-IX)              
068400     .                                                                    
068500     EJECT                                                                
068600 S02-STARTA-URV-TRANS SECTION.                                            
068700                                                                          
068800     MOVE '6322'   TO MSGSOP-IDTRANS                                      
068900     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
069000     MOVE 'W216S1' TO MSGSOP-IDPROCESS                                    
069100     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
069200                                                                          
069300     STRING 'URVAL1(' WS-URVAL ') '                                       
069400            'URVAL2(' URV-TAB-RAD (1) ') '                                
069500            'URVAL3(' URV-TAB-RAD (2) ') '                                
069600            'URVAL4(' URV-TAB-RAD (3) ') '                                
069700            'URVAL5(' URV-TAB-RAD (4) ') '                                
069800            'URVAL6(' URV-TAB-RAD (5) ') '                                
069900            'URVAL7(' URV-TAB-RAD (6) ') '                                
070000            'URVAL8(' URV-TAB-RAD (7) ') '                                
070100            'URVAL9(' URV-TAB-RAD (8) ') '                                
070200            'URVAL10(' URV-TAB-RAD (9) ') '                               
070300            'URVAL11(' URV-TAB-RAD (10) ') '                              
070400            'URVAL12(' URV-TAB-RAD (11) ') '                              
070500            'URVAL13(' URV-TAB-RAD (12) ')'                               
070600              DELIMITED BY SIZE INTO MSGSOP-TESYMBV                       
070700                                                                          
070800     PERFORM IMS-INSERT-ALTMSG                                            
070900     .                                                                    
071000     EJECT                                                                
071100 S04-BOKA-NER-BUFFERT SECTION.                                            
071200*      --- BOKAR NER WDD8 VID SKROTNING                                   
071300     MOVE 6324-KVSKROT-BEORD TO W-KVSKROT-REST                            
071400     PERFORM IMS-GU-WDD801                                                
071500     IF SEGMENT-FINNS                                                     
071600       PERFORM IMS-GHNP-WDD811                                            
071700       PERFORM UNTIL SEGMENT-SAKNAS                                       
071800                  OR W-KVSKROT-REST <= +0                                 
071900         MOVE SALDO-ADBUFFPL TO W-ADBUFFPL                                
072000         IF (6324-FLJUSTBUFF = 'J' OR 'Y')                                
072100            AND SALDO-ADBUFFOMR = 59                                      
072200            AND W-ADBUFFPL (1:2) = 97                                     
072300           IF W-KVSKROT-REST > SALDO-KVBUFF-F                             
072400             COMPUTE W-KVSKROT-REST = W-KVSKROT-REST -                    
072500                                      SALDO-KVBUFF-F                      
072600                     END-COMPUTE                                          
072700             MOVE ZERO TO SALDO-KVBUFF-F                                  
072800           ELSE                                                           
072900             COMPUTE SALDO-KVBUFF-F = SALDO-KVBUFF-F -                    
073000                                      W-KVSKROT-REST                      
073100                     END-COMPUTE                                          
073200             MOVE ZERO TO W-KVSKROT-REST                                  
073300           END-IF                                                         
073400           IF SALDO-KVBUFF-F <= ZERO                                      
073500             PERFORM IMS-DLET-WDD811                                      
073600             PERFORM IMS-GU-WDJ901                                        
073700             PERFORM UNTIL SEGMENT-SAKNAS OR (HIST-KDLOC = 'B'            
073800                              AND HIST-IDDC = 11                          
073900                              AND SALDO-ADBUFFOMR  = HIST-ADLAGOMR        
074000                              AND SALDO-ADBUFFGANG = HIST-ADGANG          
074100                              AND SALDO-ADBUFFPL   = HIST-ADPLATS)        
074200               PERFORM IMS-GHNP-WDJ911                                    
074300               IF SEGMENT-FINNS AND HIST-KDLOC = 'B'                      
074400                              AND HIST-IDDC = 11                          
074500                              AND SALDO-ADBUFFOMR  = HIST-ADLAGOMR        
074600                              AND SALDO-ADBUFFGANG = HIST-ADGANG          
074700                              AND SALDO-ADBUFFPL   = HIST-ADPLATS         
074800                 MOVE FUNCTION CURRENT-DATE(1:8) TO                       
074900                                       HIST-DASTODAT                      
075000                 MOVE WS-IDUSER TO HIST-IDUSER-STO                        
075100                 PERFORM IMS-REPL-WDJ911                                  
075200               END-IF                                                     
075300             END-PERFORM                                                  
075400           ELSE                                                           
075500             PERFORM IMS-REPL-WDD811                                      
075600           END-IF                                                         
075700         END-IF                                                           
075800         PERFORM IMS-GHNP-WDD811                                          
075900       END-PERFORM                                                        
076000     END-IF                                                               
076100                                                                          
076200                                                                          
076300     .                                                                    
076400     EJECT                                                                
076500 S10-SKAPA-TRANS-TILL-1113 SECTION.                                       
076600                                                                          
076700     MOVE IDARTNR-WS     TO PROGSW-MID-IDARTNR-UT                         
076800*    MOVE 1              TO PROGSW-MID-DIERS-ERS                          
076900*    MOVE 09             TO PROGSW-MID-KDERS                              
077000     MOVE JA             TO PROGSW-MID-FLKLAR                             
077100                                                                          
077200     MOVE ALL '+'        TO PROGSW-MID-IDARTNR-IN                         
077300                            PROGSW-MID-IDAO                               
077400                            PROGSW-MID-TIERSDAT-PREL                      
077500                            PROGSW-MID-TEARTNOT                           
077600                                                                          
077700     MOVE  1                TO PROGSW-IX                                  
077800     PERFORM UNTIL PROGSW-IX > 09                                         
077900        MOVE ALL '+'        TO PROGSW-MID-IDKORTNR                        
078000                                            (PROGSW-IX)                   
078100                               PROGSW-MID-IDARTNR-TILLK                   
078200                                            (PROGSW-IX)                   
078300                               PROGSW-MID-DIERS-TILLK                     
078400                                            (PROGSW-IX)                   
078500                               PROGSW-MID-BEERS                           
078600                                            (PROGSW-IX)                   
078700        ADD 1               TO PROGSW-IX                                  
078800     END-PERFORM                                                          
078900                                                                          
079000     PERFORM IMS-ISRT-ALT-PCB                                             
079100     .                                                                    
079200     EJECT                                                                
079300 S14-SKAPA-CLASSIC-TRANS SECTION.                                         
079400                                                                          
079500     PERFORM IMS-GHU-ARTC01                                               
079600     IF SEGMENT-FINNS                                                     
079700        MOVE ART-IDLEVNR    TO FILC-IDLEVNR                               
079800        MOVE W-IDARTNR      TO IDARTNR-WS                                 
079900        PERFORM IMS-GHU-ARTC11                                            
080000        IF SEGMENT-FINNS                                                  
080100           MOVE 495         TO CLAG-IDANSK                                
080200           MOVE 049         TO CLAG-IDBERED                               
080300           MOVE 02          TO CLAG-BEFT                                  
080400           MOVE 9           TO CLAG-IDPLANGR-AG                           
080500           MOVE 'GCP'       TO CLAG-IDPROJ                                
080510           MOVE ZERO        TO CLAG-KDFORPPL                              
080520           MOVE ZERO        TO CLAG-KDFORPGP                              
080530           MOVE ZERO        TO CLAG-KDFORPUF                              
080600*****                                                                     
080700           IF CLAG-IDINK (1:3) NUMERIC                                    
080800              MOVE CLAG-IDINK (1:3) TO FILC-IDINK                         
080900                                       TEST-IDINK                         
081000           ELSE                                                           
081100              IF CLAG-IDINK (2:3) NUMERIC                                 
081200                 MOVE CLAG-IDINK (2:3) TO FILC-IDINK                      
081300              ELSE                                                        
081400                 MOVE ZERO TO FILC-IDINK                                  
081500              END-IF                                                      
081600           END-IF                                                         
081700           MOVE '987'       TO CLAG-IDINK                                 
081800*****                                                                     
081900           IF CLAG-KDERS = 09    AND                                      
081910              (ART-IDLEVNR NOT = 'BQ8VA')                                 
082000              MOVE 00       TO PROGSW-MID-KDERS                           
082100              MOVE ALL '+'  TO PROGSW-MID-DIERS-ERS                       
082200              PERFORM S10-SKAPA-TRANS-TILL-1113                           
082300           END-IF                                                         
082400                                                                          
082500           PERFORM IMS-REPL-ARTC11                                        
082600           PERFORM IMS-GNP-ARTC23                                         
082700           PERFORM UNTIL SEGMENT-SAKNAS                                   
082800             MOVE AVT-IDAVTAL TO W-IDAVTAL-RED                            
082900***             TAR ÄVEN MED NAP-AVTAL, PREFIX = 004                      
083000             IF (TEST-IDINK > 99 AND                                      
083100                 TEST-IDINK < 790) OR                                     
083200                (TEST-IDINK > 799 AND                                     
083300                 TEST-IDINK < 987) OR                                     
083301                (TEST-IDINK > 987 AND                                     
083302                 TEST-IDINK < 1000) OR                                    
083600                 (W-PREFIX = '004')                                       
083700                PERFORM S16-SKAPA-B65                                     
083800             ELSE                                                         
084300                  PERFORM S18-SKAPA-R22POST                               
084500             END-IF                                                       
084600             PERFORM IMS-GNP-ARTC23                                       
084700           END-PERFORM                                                    
084710           PERFORM S14A-SKAPA-FORP-HIST                                   
084800           PERFORM S19-SKAPA-R23POST                                      
084900                                                                          
085000           MOVE W-IDARTNR   TO FILC-IDARTNR                               
085100           MOVE SPACE       TO FILC-BEART                                 
085200           MOVE FILC-W21632 TO FILC-FIL-WDR301-DATA                       
085300           ACCEPT W-TID FROM TIME                                         
085400           IF W-TID = FILC-FIL-TIKLOCK                                    
085500              ADD +1        TO FILC-FIL-IDSEKVNR                          
085600           ELSE                                                           
085700              MOVE W-TID    TO FILC-FIL-TIKLOCK                           
085800              MOVE +1       TO FILC-FIL-IDSEKVNR                          
085900           END-IF                                                         
086000           PERFORM IMS-ISRT-WLFILC                                        
086100                                                                          
086200           MOVE 'BQ8VA'    TO W-IDLEVNR-WDF2                              
086300           MOVE 'CLASSIC'  TO W-IDDIRGRP                                  
086400           MOVE IN-IDARTNR TO WDF2-ART-IDARTNR                            
086500           MOVE ZERO       TO WDF2-ART-DASTADAT                           
086600***********MOVE DAGENS-DATUM-Y2K TO WD2-ART-DASTADAT                      
086610           MOVE -99        TO WDF2-ART-KVLS-DLEV                          
086611           MOVE ZERO       TO WDF2-ART-TIINLMOT                           
086612                              WDF2-ART-TIREGDAT                           
086620*      -99 BETYDER ATT SALDOT INTE UPPDATERAS AV LEVERANTÖR               
086630*      DET BETYDER ALLTSÅ INTE ATT VI HAR ETT NEGATIVT SALDO :-)          
086640*      FÖR LEVERANTÖRER SOM SKICKAR SALDOUPPGIFTER ÄR VÄRDET >= 0         
086650*      VID NYUPPLÄGG AV ARTIKLAR PÅ LEVERANTÖRER SOM REDOVISAR            
086660*      SALDO, KOMMER ARTIKELN SÅLEDES HA VÄRDET -99 TILL FÖRSTA           
086670*      UPPDATERINGEN AV SALDOT                                            
086700                                                                          
086800           PERFORM IMS-ISRT-WDF212                                        
086900        END-IF                                                            
087000     END-IF                                                               
087100     .                                                                    
087200     EJECT                                                                
087210 S14A-SKAPA-FORP-HIST SECTION.                                            
087220                                                                          
087221     PERFORM IMS-GU-WDT301                                                
087222     IF SEGMENT-SAKNAS                                                    
087223        MOVE W-IDARTNR   TO FART-IDARTNR                                  
087224        PERFORM IMS-ISRT-WDT301                                           
087225     END-IF                                                               
087230**** INSERT WDT311                                                        
087240     MOVE 'W26169' TO FPCK-IDUSER                                         
087250     MOVE 02       TO FPCK-BEFT                                           
087260     MOVE ZERO     TO FPCK-KDFORPPL                                       
087270     MOVE ZERO     TO FPCK-KDFORPGP                                       
087280     MOVE ZERO     TO FPCK-KDFORPUF                                       
087290     MOVE 'ÖVERGÅTT TILL CLASSIC DIREKTLEVERANS'                          
087291                   TO FPCK-TEBEFT(1)                                      
087292     MOVE SPACE    TO FPCK-TEBEFT(2)                                      
087293     MOVE SPACE    TO FPCK-TEBEFT(3)                                      
087294     MOVE SPACE    TO FPCK-TEBEFT(4)                                      
087295     MOVE SPACE    TO FPCK-TEBEFT(5)                                      
087296     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                       
087297     COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WS-DAREGDAT               
087298     ACCEPT WS-TID FROM TIME                                              
087299     COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WS-TID                     
087300     MOVE 'SE'     TO FPCK-IDLANDX2                                       
087301     PERFORM IMS-ISRT-WDT311                                              
087302     .                                                                    
087303     EJECT                                                                
087310 S15-SKAPA-WDGX2402 SECTION.                                              
087400                                                                          
087500     IF NOT NDC-NA                                                        
087600       MOVE 6324-IDARTNR      TO 2402-IDARTNR                             
087700       MOVE 6324-IDANALYS     TO 2402-IDANALYS                            
087800       MOVE 6324-IDDC         TO 2402-IDDC                                
087900       MOVE 6324-IDDISTR      TO 2402-IDDISTR                             
088000       MOVE 6324-IDKONTO      TO 2402-IDKONTO                             
088100       MOVE 6324-IDKST        TO 2402-IDKST                               
088200       MOVE 6324-IDPERSON     TO 2402-IDPERSON                            
088300       MOVE 6321-KDARBTYP     TO 2402-KDARBTYP                            
088400       MOVE 6324-KVSKROT-BEORD TO 2402-KVSKROT-BEORD                      
088500       MOVE 6324-KVSKROT-ONDEM TO 2402-KVSKROT-KVAR                       
088600       MOVE 6324-IDUSER       TO 2402-IDUSER                              
088700       MOVE 6324-BEANST       TO 2402-BEANST                              
088800       MOVE WS-6322-DASKROT9  TO 2402-DASKROT9-BEORD                      
088900       MOVE 6326-TIDATETIME   TO 2402-TIDATETIME(1)                       
089000       MOVE 6326-IDUSER-GODK  TO 2402-IDUSER-GODK(1)                      
089100       MOVE 6326-BEANST-GODK  TO 2402-BEANST-GODK(1)                      
089200       MOVE 6324-IDKUNDNR     TO 2402-IDKUNDNR                            
089300       MOVE 6324-KDERS-UTG    TO 2402-KDERS-UTG                           
089400       MOVE 6324-KVTILLG-CDC  TO 2402-KVTILLG-CDC                         
089500       MOVE 6324-KVAKS-CDC    TO 2402-KVAKS-CDC                           
089510*FIX FIX                                                                  
089600       MOVE ZERO              TO 2402-KVTILLG-SDC                         
089800       MOVE ZERO              TO 2402-KVAKS-SDC                           
089810*      MOVE 6324-KVTILLG-SDC  TO 2402-KVTILLG-SDC                         
089820*      MOVE 6324-KVAKS-SDC    TO 2402-KVAKS-SDC                           
089900       MOVE +1     TO IX-BEEMB                                            
090000       PERFORM UNTIL IX-BEEMB > 20                                        
090100         MOVE 6324-BEEMBLEM(IX-BEEMB) TO 2402-BEEMBLEM(IX-BEEMB)          
090200         ADD +1 TO IX-BEEMB                                               
090300       END-PERFORM                                                        
090400       MOVE 6324-SUTPO-TOT    TO 2402-SUTPO-TOT                           
090500       PERFORM IMS-ISRT-WDGX2402                                          
090600     END-IF                                                               
090700     .                                                                    
090800     EJECT                                                                
090900  S16-SKAPA-B65 SECTION.                                                  
091000                                                                          
091100     ACCEPT ZZAC-TIKLOCK             FROM  TIME                           
091200     ACCEPT ZZAC-TIAAMMDD FROM             DATE                           
091300     ADD 1                          TO WS-IDLOGLOP                        
091400     MOVE WS-IDLOGLOP               TO ZZAC-IDLOGLOP                      
091500     MOVE SPACE              TO A310-LEVNUM-GODSM                         
091600                              A310-ANT-BESTANN                            
091700     MOVE 'RY2'              TO A310-KT                                   
091800     MOVE DAGENS-DATUM       TO A310-DATUM-UTSKR                          
091900     MOVE ART-IDLEVNR        TO W-IDLEVNR                                 
092000     IF W-IDLEVNR (5:1) = SPACE                                           
092100*      LEVNUM SKALL TILLS VIDARE VARA NUMERISKT I X(5)                    
092200       MOVE ZERO             TO TALLY                                     
092300       INSPECT W-IDLEVNR TALLYING TALLY                                   
092400                          FOR CHARACTERS BEFORE INITIAL SPACE             
092500       IF TALLY = ZERO                                                    
092600          MOVE ZERO          TO WS-IDLEVNR-NUM                            
092700       ELSE                                                               
092800          MOVE W-IDLEVNR (1:TALLY)                                        
092900                             TO WS-IDLEVNR-NUM                            
093000       END-IF                                                             
093100       MOVE WS-IDLEVNR-NUM TO A310-LEVNUM                                 
093200     ELSE                                                                 
093300       MOVE W-IDLEVNR        TO A310-LEVNUM                               
093400     END-IF                                                               
093500     MOVE W-IDARTNR          TO WS-IDARTNR                                
093600     MOVE WS-IDARTNR         TO WS-IDARTNR-8                              
093700     MOVE WS-IDARTNR-8       TO A310-ARTNR                                
093800                              W092-SORTBGP                                
093900                                                                          
094000     MOVE AVT-IDAVTAL        TO W-IDAVTAL-RED                             
094100     MOVE W-PREFIX           TO A310-BESTPREF                             
094200     MOVE W-AVTALSNR         TO A310-BESTLNR                              
094300     MOVE W-SUFFIX           TO A310-BESTSUFF                             
094400     MOVE A310-A310B65       TO ZZAC-LOGGPOST                             
094500     MOVE W092-AREA          TO ZZAC-SORTPOST                             
094600     PERFORM IMS-ISRT-ZZAC                                                
094610     IF SEGMENT-FINNS-REDAN                                               
094620       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
094630          ADD +1             TO WS-IDLOGLOP                               
094640          IF WS-IDLOGLOP = 9                                              
094650             ADD +1          TO ZZAC-TIKLOCK                              
094660          END-IF                                                          
094670          MOVE WS-IDLOGLOP   TO ZZAC-IDLOGLOP                             
094680          PERFORM IMS-ISRT-ZZAC                                           
094690       END-PERFORM                                                        
094691     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900*                                                                         
095000  S17-SKAPA-1142 SECTION.                                                 
095100                                                                          
095200     MOVE SPACE       TO XXAV-1142-WDGX1142                               
095300     MOVE W-IDARTNR   TO XXAV-1142-IDARTNR                                
095400     MOVE +2          TO XXAV-1142-KDSEGKEY                               
095500     MOVE 'A'         TO XXAV-1142-KDSVAR                                 
095600                                                                          
095700     PERFORM IMS-ISRT-XXAV                                                
095800     .                                                                    
095900     EJECT                                                                
096000 S18-SKAPA-R22POST SECTION.                                               
096100                                                                          
096200     MOVE 'R22'                TO BAS-R22-IDPTYP                          
096300     MOVE W-IDARTNR            TO BAS-R22-IDARTNR                         
096400     MOVE AVT-IDAVTAL          TO BAS-R22-IDBEST                          
096500     MOVE AVT-IDLEVNR-AVT      TO BAS-R22-IDLEVNR-BEST                    
096510     MOVE SPACE                TO BAS-R22-IDLEVNR-SHIP                    
096600*    MOVE AVT-TIAVTAL          TO BAS-R22-TIBEST                          
096700     MOVE DAGENS-DATUM         TO BAS-R22-TIBEST                          
096800     MOVE AVT-KVAVTANT         TO BAS-R22-KVBEST                          
096900     MOVE 5                    TO BAS-R22-KDBEH-BEST                      
097000     MOVE SPACE                TO BAS-R22-TENOT-BESTPRIS                  
097100     MOVE SPACE                TO BAS-R22-REST                            
097200                                                                          
097300                                                                          
097400     MOVE DAGENS-DATUM         TO POST-TIREGDAT W-TIREGDAT                
097500     ADD +1                    TO POST-TIKLOCK W-TIKLOCK                  
097600     MOVE WS-IDUSER            TO POST-IDUSER                             
097700     MOVE 'W261V2'             TO POST-IDLTERM                            
097800     MOVE ZERO                 TO POST-TIBORT                             
097900                                                                          
098000     MOVE BAS-R22-REGPOST      TO POST-REGPOST                            
098100     PERFORM IMS-ISRT-WDG901                                              
098200     IF SEGMENT-FINNS-REDAN                                               
098300       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
098400          ADD +1 TO POST-TIKLOCK W-TIKLOCK                                
098500          PERFORM IMS-ISRT-WDG901                                         
098600       END-PERFORM                                                        
098700     END-IF                                                               
098800     .                                                                    
098900     EJECT                                                                
099000*                                                                         
099100 S19-SKAPA-R23POST SECTION.                                               
099200                                                                          
099300     MOVE 'R23'                TO BAS-R23-IDPTYP                          
099400     MOVE W-IDARTNR            TO BAS-R23-IDARTNR                         
099500     MOVE 987910987094         TO BAS-R23-IDAVTAL                         
099600     MOVE 'BQ8VA'              TO BAS-R23-IDLEVNR-AVT                     
099610                                  BAS-R23-IDLEVNR-SHIP                    
099700     MOVE DAGENS-DATUM         TO BAS-R23-TIAVTAL                         
099800     MOVE ZERO                 TO BAS-R23-KVAVTANT                        
099900     MOVE +1                   TO BAS-R23-KDBEH-AVT                       
100000     MOVE SPACE                TO BAS-R23-TENOT-AVTPRIS                   
100100     MOVE SPACE                TO BAS-R23-REST                            
100200                                                                          
100300                                                                          
100400     MOVE DAGENS-DATUM         TO POST-TIREGDAT W-TIREGDAT                
100500     ADD +1                    TO POST-TIKLOCK W-TIKLOCK                  
100600     ACCEPT POST-TIKLOCK FROM TIME                                        
100700     MOVE POST-TIKLOCK TO W-TIKLOCK                                       
100800     MOVE WS-IDUSER            TO POST-IDUSER                             
100900     MOVE 'W261V2'             TO POST-IDLTERM                            
101000     MOVE ZERO                 TO POST-TIBORT                             
101100                                                                          
101200     MOVE BAS-R23-REGPOST      TO POST-REGPOST                            
101300     PERFORM IMS-ISRT-WDG901                                              
101400     IF SEGMENT-FINNS-REDAN                                               
101500       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
101600          ADD +1 TO POST-TIKLOCK W-TIKLOCK                                
101700          PERFORM IMS-ISRT-WDG901                                         
101800       END-PERFORM                                                        
101900     END-IF                                                               
102000     .                                                                    
102100     EJECT                                                                
102200                                                                          
102300 X-TAG-CHECKPOINT   SECTION.                                              
102400                                                                          
102500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
102600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
102700     PERFORM IMS-CHECKPOINT                                               
102800     MOVE ZERO TO CHKP-ANT                                                
102900* --- LÄS OM DATABAS OM DET BEHÖVS                                        
103000     .                                                                    
103100     EJECT                                                                
103200* --- IMS SEKTIONER ---                                                   
103300     SKIP3                                                                
103400 IMS-INSERT-ALTMSG SECTION.                                               
103500     MOVE SPACE TO GODK-STATUSKODER                                       
103600     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
103700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
103800     PERFORM IMS-STATUSKONTROLL                                           
103810     ADD +1  TO CHKP-ANT                                                  
103900     .                                                                    
104000     EJECT                                                                
104100 IMS-GHU-ARTC01 SECTION.                                                  
104200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
104300          DELIMITED BY SIZE INTO SSA1                                     
104400     MOVE '  GE' TO GODK-STATUSKODER                                      
104500     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC01 SSA1                 
104600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
104700     PERFORM IMS-STATUSKONTROLL                                           
104800     .                                                                    
104900     SKIP3                                                                
105000 IMS-GHU-ARTC11 SECTION.                                                  
105100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
105200          DELIMITED BY SIZE INTO SSA1                                     
105300     MOVE 'WDK611   ' TO SSA2                                             
105400     MOVE '  GE' TO GODK-STATUSKODER                                      
105500     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2            
105600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
105700     PERFORM IMS-STATUSKONTROLL                                           
105800     .                                                                    
105900     SKIP3                                                                
106000 IMS-GNP-ARTC23 SECTION.                                                  
106100     MOVE  'WDK623   ' TO  SSA1                                           
106200     MOVE '  GE' TO GODK-STATUSKODER                                      
106300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC23 SSA1                 
106400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     .                                                                    
106700     SKIP2                                                                
106800 IMS-REPL-ARTC11 SECTION.                                                 
106900     MOVE '  ' TO GODK-STATUSKODER                                        
107300     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
107400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
107500     PERFORM IMS-STATUSKONTROLL                                           
107600     ADD +1  TO CHKP-ANT                                                  
107700     .                                                                    
107800     EJECT                                                                
107801 IMS-GU-WDT301 SECTION.                                                   
107802     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
107803          DELIMITED BY SIZE INTO SSA1                                     
107804     MOVE '  GE' TO GODK-STATUSKODER                                      
107805     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT301 SSA1                    
107806     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
107807     PERFORM IMS-STATUSKONTROLL                                           
107808     .                                                                    
107809     SKIP3                                                                
107810 IMS-ISRT-WDT301 SECTION.                                                 
107840     MOVE 'WDT301   ' TO SSA1                                             
107850     MOVE '  II' TO GODK-STATUSKODER                                      
107860     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
107870     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
107880     PERFORM IMS-STATUSKONTROLL                                           
107881     ADD +1  TO CHKP-ANT                                                  
107890     .                                                                    
107891 IMS-ISRT-WDT311 SECTION.                                                 
107892     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
107893          DELIMITED BY SIZE INTO SSA1                                     
107894     MOVE 'WDT311   ' TO SSA2                                             
107895     MOVE '  II' TO GODK-STATUSKODER                                      
107896     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
107897     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
107898     PERFORM IMS-STATUSKONTROLL                                           
107899     ADD +1  TO CHKP-ANT                                                  
107900     .                                                                    
107901     SKIP3                                                                
107910 IMS-REPL-WDGX6324 SECTION.                                               
108000     MOVE '  ' TO GODK-STATUSKODER                                        
108400     CALL CBLTDLI USING REPL 6321-PCB DLI-IO-WDGX6324                     
108500     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
108600     PERFORM IMS-STATUSKONTROLL                                           
108700     ADD +1  TO CHKP-ANT                                                  
108800     .                                                                    
108900     EJECT                                                                
109000 IMS-GHU-WDR501-6321 SECTION.                                             
109100     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
109200          DELIMITED BY SIZE INTO SSA1                                     
109300     MOVE 'GE  ' TO GODK-STATUSKODER                                      
109400     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDR501-6321 SSA1              
109500     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
109600     PERFORM IMS-STATUSKONTROLL                                           
109700     .                                                                    
109800     SKIP3                                                                
109900 IMS-GHNP-WDGX6324 SECTION.                                               
110000     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
110100          DELIMITED BY SIZE INTO SSA1                                     
110200     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
110300                    '&IDDC     =' W-IDDC-6324-X                           
110400                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
110500          DELIMITED BY SIZE INTO SSA2                                     
110600     MOVE '  GE' TO GODK-STATUSKODER                                      
110700     CALL CBLTDLI USING GHNP 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2           
110800     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
110900     PERFORM IMS-STATUSKONTROLL                                           
111000     .                                                                    
111100     SKIP2                                                                
111200 IMS-ISRT-WDGX6326 SECTION.                                               
111300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
111400            DELIMITED BY SIZE INTO SSA1                                   
111500     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
111600            DELIMITED BY SIZE INTO SSA2                                   
111700     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
111800                    '&IDDC     =' W-IDDC-6324-X                           
111900                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
112000          DELIMITED BY SIZE INTO SSA3                                     
112100     MOVE 'WDGX6326'            TO SSA4                                   
112200     MOVE '  '                  TO GODK-STATUSKODER                       
112600     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6326                     
112700                                      SSA1 SSA2 SSA3 SSA4                 
112800     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     ADD +1  TO CHKP-ANT                                                  
113100     .                                                                    
113200     EJECT                                                                
113300 IMS-ISRT-WLFILC SECTION.                                                 
113400     STRING 'WLFILC01    '                                                
113500          DELIMITED BY SIZE INTO SSA1                                     
113600     MOVE '   ' TO GODK-STATUSKODER                                       
114000     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
114100     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
114200     PERFORM IMS-STATUSKONTROLL                                           
114300     ADD +1  TO CHKP-ANT                                                  
114400     .                                                                    
114500     EJECT                                                                
114600 IMS-GU-WDGX6328 SECTION.                                                 
114700     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6327 ')'                      
114800          DELIMITED BY SIZE INTO SSA1                                     
114900     STRING 'WDGX6328(SUBEL   =>' W-SUBEL-MIN-X                           
115000                    '&SUBEL   =<' W-SUBEL-MAX-X                           
115100                    '&IDUSERGK= ' W-IDUSER-GODK-X ')'                     
115200          DELIMITED BY SIZE INTO SSA2                                     
115300     MOVE '  GE' TO GODK-STATUSKODER                                      
115400     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6328 SSA1 SSA2             
115500     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
115600     PERFORM IMS-STATUSKONTROLL                                           
115700     .                                                                    
115800     SKIP2                                                                
115900 IMS-ISRT-WDGX2402 SECTION.                                               
116000     STRING 'WDR501  (WDGXKEY  =' W-2401-KEY-X ')'                        
116100            DELIMITED BY SIZE INTO SSA1                                   
116200     MOVE 'WDGX2402'            TO SSA2                                   
116300     MOVE '  '                  TO GODK-STATUSKODER                       
116700     CALL CBLTDLI USING ISRT 2401-PCB DLI-IO-WDR501-2401 SSA1 SSA2        
116800     MOVE 2401-STATUS-CODE      TO STATUS-WS                              
116900     PERFORM IMS-STATUSKONTROLL                                           
117000     ADD +1  TO CHKP-ANT                                                  
117100     SKIP3                                                                
117200     .                                                                    
117300     EJECT                                                                
117400 IMS-GU-WDD801 SECTION.                                                   
117500                                                                          
117600     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
117700          DELIMITED BY SIZE INTO SSA1                                     
117800     MOVE '  GE' TO GODK-STATUSKODER                                      
117900     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
118000     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
118100     PERFORM IMS-STATUSKONTROLL                                           
118200     .                                                                    
118300     EJECT                                                                
118400 IMS-GHNP-WDD811 SECTION.                                                 
118500                                                                          
118600     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
118700          DELIMITED BY SIZE INTO SSA1                                     
118800     STRING 'WDD811   '                                                   
118900          DELIMITED BY SIZE INTO SSA2                                     
119000     MOVE '  GE' TO GODK-STATUSKODER                                      
119100     CALL CBLTDLI USING GHNP WDD8-PCB DLI-IO-WDD811 SSA1 SSA2             
119200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
119300     PERFORM IMS-STATUSKONTROLL                                           
119400     .                                                                    
119500     SKIP3                                                                
119600 IMS-REPL-WDD811 SECTION.                                                 
119700                                                                          
119800     MOVE '  ' TO GODK-STATUSKODER                                        
120200     CALL CBLTDLI USING REPL WDD8-PCB DLI-IO-WDD811                       
120300     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
120400     PERFORM IMS-STATUSKONTROLL                                           
120500     ADD +1  TO CHKP-ANT                                                  
120600     .                                                                    
120700     EJECT                                                                
120800 IMS-DLET-WDD811 SECTION.                                                 
120900                                                                          
121000     MOVE '  ' TO GODK-STATUSKODER                                        
121400     CALL CBLTDLI USING DLET WDD8-PCB DLI-IO-WDD811                       
121500     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
121600     PERFORM IMS-STATUSKONTROLL                                           
121700     ADD +1  TO CHKP-ANT                                                  
121800     .                                                                    
121900     EJECT                                                                
122000 IMS-GU-WDJ901 SECTION.                                                   
122100                                                                          
122200     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
122300          DELIMITED BY SIZE INTO SSA1                                     
122400     MOVE '  GE' TO GODK-STATUSKODER                                      
122500     CALL CBLTDLI USING GU WDJ9-PCB DLI-IO-WDJ901 SSA1                    
122600     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
122700     PERFORM IMS-STATUSKONTROLL                                           
122800     .                                                                    
122900     EJECT                                                                
123000 IMS-GHNP-WDJ911 SECTION.                                                 
123100                                                                          
123200     MOVE '  GE' TO GODK-STATUSKODER                                      
123300     CALL CBLTDLI USING GHNP WDJ9-PCB DLI-IO-WDJ911                       
123400     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
123500     PERFORM IMS-STATUSKONTROLL                                           
123600                                                                          
123700     EJECT                                                                
123800     .                                                                    
123900                                                                          
124000 IMS-REPL-WDJ911 SECTION.                                                 
124100                                                                          
124200     MOVE '  ' TO GODK-STATUSKODER                                        
124600     CALL CBLTDLI USING REPL WDJ9-PCB DLI-IO-WDJ911                       
124700     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
124800     PERFORM IMS-STATUSKONTROLL                                           
124900     ADD +1  TO CHKP-ANT                                                  
125000     .                                                                    
125100     EJECT                                                                
125200 IMS-ISRT-ZZAC SECTION.                                                   
125300                                                                          
125400     MOVE 'WLZZAC01 '        TO SSA1                                      
125500     MOVE '  II'             TO GODK-STATUSKODER                          
125900     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA5 SSA1                   
126000     MOVE ZZAC-STATUS-CODE     TO STATUS-WS                               
126100     PERFORM IMS-STATUSKONTROLL                                           
126200     ADD +1  TO CHKP-ANT                                                  
126300     .                                                                    
126400     SKIP3                                                                
126500 IMS-ISRT-XXAV SECTION.                                                   
126600     STRING 'WLXXAV01(WDGXKEY  =' W-1141-KEY-X ')'                        
126700              DELIMITED BY SIZE INTO SSA1                                 
126800     MOVE 'WLXXAV11 ' TO SSA2                                             
126900     MOVE '  '   TO GODK-STATUSKODER                                      
127300     CALL CBLTDLI USING ISRT XXAV-PCB DLI-IO-AREA6 SSA1 SSA2              
127400     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
127500     PERFORM IMS-STATUSKONTROLL                                           
127600     ADD +1  TO CHKP-ANT                                                  
127700     .                                                                    
127800     EJECT                                                                
127900 IMS-ISRT-WDG901 SECTION.                                                 
128000                                                                          
128100     MOVE 'WLZZAD01 ' TO SSA1                                             
128200     MOVE '  II' TO GODK-STATUSKODER                                      
128600     CALL CBLTDLI USING ISRT ZZAD-PCB DLI-IO-WDG901 SSA1                  
128700     MOVE ZZAD-STATUS-CODE TO STATUS-WS                                   
128800     PERFORM IMS-STATUSKONTROLL                                           
128900     ADD +1  TO CHKP-ANT                                                  
129000     .                                                                    
129100     EJECT                                                                
129200                                                                          
129300 IMS-ISRT-ALT-PCB SECTION.                                                
129400     MOVE '  ' TO GODK-STATUSKODER                                        
129800     CALL CBLTDLI USING PURG ALT4-PCB W-PROG-TO-PROG-SW                   
129900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     ADD +1  TO CHKP-ANT                                                  
130200     SKIP3                                                                
130300     .                                                                    
130400 IMS-ISRT-WDF212 SECTION.                                                 
130500                                                                          
130600     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
130700          DELIMITED BY SIZE INTO SSA1                                     
130800     MOVE 'WDF212    ' TO SSA2                                            
130900     MOVE '  IINI' TO GODK-STATUSKODER                                    
131300     CALL CBLTDLI USING ISRT WDF2-PCB DLI-IO-WDF212 SSA1 SSA2             
131400     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
131500     PERFORM IMS-STATUSKONTROLL                                           
131600     ADD +1  TO CHKP-ANT                                                  
131700     .                                                                    
131800     SKIP3                                                                
131900 IMS-RESTART SECTION.                                                     
132000     SKIP2                                                                
132100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
132200     MOVE '  ' TO GODK-STATUSKODER                                        
132300     CALL CBLTDLI USING XRST MSG-PCB                                      
132400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
132500                        CHKP-AREA-LENGTH CHKP-AREA                        
132600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
132700     PERFORM IMS-STATUSKONTROLL                                           
132800     .                                                                    
132900     SKIP3                                                                
133000 IMS-CHECKPOINT SECTION.                                                  
133100     SKIP2                                                                
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
