000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2619400.                                                
000300 AUTHOR.         INGER STENING.                                           
000400 DATE-WRITTEN.   19/02/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        RELEASE SCRAP ORDERS                                             
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR        (WDK7)                              
001100*                                6321 (WDR5)                              
001200*                                6327 (WDR5)                              
001300*                                2402 (WDR5)                              
002100*                   STARTAR RUTIN W216S1 I SOP                            
002200*                   (SKAPAR SKROTORDER).                                  
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W6T322                                              
002600*        INFIL        W26193                                              
002700*                                                                         
002800*    UTDATA.                                                              
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     SKIP2                                                                
003900*          --- ARTIKLAR ATT SKROTA AUTO                                   
004000     SELECT W26193                     ASSIGN TO W26194D1.                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004500     SKIP3                                                                
004600 FD  W26193                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  -COPY W26169   -L.                                                   
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400*    -- CHECKED BY WY2000                                                 
005500 77  IDPGM                       PIC X(08)   VALUE 'W2619400'.            
005600 77  LAES-SW                     PIC X     VALUE SPACE.                   
005700 77  WS-SKROTDATUM-SLUT          PIC X     VALUE SPACE.                   
005800 77  WS-DATUM-HITTAD             PIC X     VALUE SPACE.                   
005900 77  WS-ART-SLUT                 PIC X     VALUE SPACE.                   
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  PROGSW-IX                   PIC S9(3)  VALUE ZERO  COMP-3.           
006300 77  RAD-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
006400 77  RAD-IX-MAX                  PIC S9(3)  VALUE +12   COMP-3.           
006500 77  TAB-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
006600 77  TAB-IX-MAX                  PIC S9(3)  VALUE +30   COMP-3.           
006700 77  URV-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
006800 77  URV-IX-MAX                  PIC S9(3)  VALUE +12   COMP-3.           
006900 77  IX-BEEMB                    PIC S9(4)   VALUE +0  COMP SYNC.         
007000 77  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
007100 77  MAX-IX                      PIC S9(4)   VALUE +11 COMP SYNC.         
007200 77  DAGENS-DATUM-Y2K            PIC 9(8)   VALUE ZERO.                   
007300 77  WS-ANTAL-X                  PIC 9(3)   VALUE ZERO COMP-3.            
007400 77  W-TID                       PIC 9(8)   VALUE ZERO.                   
007500 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
007600 77  TEST-IDINK                  PIC 9(3)    VALUE ZERO.                  
007700 77  WS-IDLOGLOP                 PIC 9(01) COMP-3 VALUE ZERO.             
007800 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
007900                                                                          
008000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008100     88  INDATA-OK                           VALUE 'J'.                   
008200     88  INDATA-FEL                          VALUE 'N'.                   
008300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008400     88  NYCKLAR-OK                          VALUE 'J'.                   
008500     88  NYCKLAR-FEL                         VALUE 'N'.                   
008600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008700     88  EGEN-MID                            VALUE '6322'.                
008800     88  GODK-MID                            VALUE '6322' '6325'.         
008900 77  WS-CMD                      PIC X       VALUE SPACE.                 
009000 77  SW-R23                      PIC X       VALUE 'N'.                   
009100 77  WS-FL6326                   PIC X       VALUE 'N'.                   
009200                                                                          
009300 77  KDARBTYP-SOEKNING           PIC X       VALUE 'N'.                   
009400 77  KDARBTYP-PERSON-SOEKNING    PIC X       VALUE 'N'.                   
009500 77  IDARTNR-SOEKNING            PIC X       VALUE 'N'.                   
009600 77  DATUM-SOEKNING              PIC X       VALUE 'N'.                   
009700 77  DATUM-IDARTNR-SOEKNING      PIC X       VALUE 'N'.                   
009800 77  IDDC-SOEKNING               PIC X       VALUE 'N'.                   
009900 77  IDPERSON-SOEKNING           PIC X       VALUE 'N'.                   
010000 77  WS-DASKROT9                 PIC 9(8)    VALUE ZERO.                  
010100 77  WS-SUARTSTD                 PIC 9(7)V9(2) VALUE ZERO.                
010200 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
010300 77  WS-DASKROT9-BEORD           PIC 9(8)    VALUE ZERO.                  
010400 77  WS-6322-DASKROT9            PIC 9(8)    VALUE ZERO.                  
010500 77  WS-IDPERSON-FOM             PIC X(3)    VALUE ZERO.                  
010600 77  WS-IDPERSON-TOM             PIC X(3)    VALUE ZERO.                  
010700 77  W-KVSKROT-REST              PIC S9(7)  VALUE ZERO.                   
010800 77  W-ADBUFFPL                  PIC 9(5)  VALUE ZERO.                    
010900 77  TEST-NYCKEL-IDARTNR         PIC X(9)    VALUE SPACE.                 
011000 77  TEST-NYCKEL-TIDATUM         PIC X(6)    VALUE SPACE.                 
011100 77  TEST-NYCKEL-IDANSK          PIC X(3)    VALUE SPACE.                 
011200 77  WS-FLHOGRE                  PIC X       VALUE 'N'.                   
011300 77  WS-FLURVAL                  PIC X       VALUE 'N'.                   
011400 77  WS-HIGHLEV                  PIC X       VALUE 'N'.                   
011500 77  WS-SUBEL                    PIC 9(7)    VALUE ZERO.                  
011600 77  W-ANNUL-IDUSER              PIC X(8)    VALUE SPACE.                 
011700 77  W-ANNUL-IDMAIL              PIC X(60)   VALUE SPACE.                 
011800 01  ANUL-BEANST-GODK            PIC X(25) VALUE SPACE.                   
011900 01  ANUL-IDMAIL                 PIC X(60) VALUE SPACE.                   
012000 01  WSM-IDARTNR                 PIC Z(8)9 VALUE ZERO.                    
012100 01  SPAR-IDKUNDNR               PIC S9(9) VALUE ZERO COMP-3.             
012200 77  WS-KDERS-UTG                PIC Z(2)    VALUE ZERO.                  
012300 77  WS-SUTPO-TOT                PIC Z(6)9   VALUE ZERO.                  
012400 77  WS-KVSKROT-BEORD            PIC Z(6)9   VALUE ZERO.                  
012500 77  WS-KVSKROT-KVAR             PIC Z(6)9   VALUE ZERO.                  
012600 77  WS-KVTILLG-CDC              PIC Z(6)9   VALUE ZERO.                  
012700 77  WS-KVTILLG-SDC              PIC Z(6)9   VALUE ZERO.                  
012800 77  WS-KVAKS-CDC                PIC Z(6)9   VALUE ZERO.                  
012900 77  WS-KVAKS-SDC                PIC Z(6)9   VALUE ZERO.                  
013000 77  WS-IDLEVNR-NUM              PIC 9(5)    VALUE ZERO.                  
013100 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
013200 77  WS-IDARTNR-8                PIC 9(08)   VALUE ZERO.                  
013300 77  W26193-EOF-SW               PIC X       VALUE 'N'.                   
013400     88  END-OF-W26193                       VALUE 'J'.                   
013500 77  WS-IDUSER                   PIC X(8)    VALUE 'W2619400'.            
013600     SKIP3                                                                
013700 01  FELTEXT.                                                             
013800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014000 01  CHKP-VAR.                                                            
014100     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
014200     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
014300     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
014400     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
014500     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
014600     03 CHKP-MAX                 PIC S9(3)   VALUE +200 COMP-3.           
014700     EJECT                                                                
014800 01  TABELL-SKROT-VARDE.                                                  
014900     03  W-VARDE-PER-RAD         OCCURS 12.                               
015000         05 W-SUARTSTD           PIC 9(7)V9(2) VALUE ZERO.                
015100                                                                          
015200 01  TABELL-TEMEMO.                                                       
015300     03  W-TEMEMO-RAD            OCCURS 11.                               
015400         05 W-TEMEMO             PIC X(66) VALUE SPACE.                   
015500*      --- VALID IDDC CODES                                               
015600*                                                                         
015700*01    -COPY WWDC99                                                       
015800*01    -COPY WWDC99 -PRE SW-                                              
015900       EJECT                                                              
016000                                                                          
016100 01  WS-TIDATETIME               PIC X(14).                               
016200 01  FILLER REDEFINES WS-TIDATETIME.                                      
016300     03  WS-DATUM                PIC 9(8).                                
016400     03  WS-TIDHHMMSS            PIC 9(6).                                
016500                                                                          
016600 01  WS-TID                      PIC 9(8) VALUE ZERO.                     
016700 01  WS-DAREGDAT                 PIC 9(8).                                
016800                                                                          
016900 01  WS-DASKROT.                                                          
017000     03  WS-DASKROT-SS                PIC 9(2).                           
017100     03  WS-DASKROT-AAMMDD            PIC 9(6).                           
017200 01  WS-AAAAMMDD REDEFINES WS-DASKROT PIC 9(8).                           
017300                                                                          
017400 01  W-IDAVTAL-RED               PIC 9(13).                               
017500 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
017600     03  FILLER                  PIC X.                                   
017700     03  W-PREFIX                PIC X(3).                                
017800     03  W-AVTALSNR              PIC X(6).                                
017900     03  W-SUFFIX                PIC X(3).                                
018000     SKIP2                                                                
018100                                                                          
018200 01  WS-BC-PARAMETRAR.                                                    
018300     03  WS-URVAL.                                                        
018400         05  URV-FLKLAR       PIC X     VALUE SPACE.                      
018500         05  URV-KDARBTYP     PIC X(8)  VALUE SPACE.                      
018600     03  URV-TABELL.                                                      
018700         05 URV-TAB-RAD OCCURS 12.                                        
018800            07  URV-IDDC             PIC X(2).                            
018900            07  URV-IDARTNR          PIC 9(9).                            
019000            07  URV-DASKROT9-BEORD   PIC 9(8).                            
019100 01  BAS-R22-REGPOST.                                                     
019200*    03      -COPY W212R22   -PRE BAS-R22-                                
019300     03 BAS-R22-REST            PIC X(41).                                
019400     EJECT                                                                
019500 01  BAS-R23-REGPOST.                                                     
019600*    03      -COPY W212R23   -PRE BAS-R23-                                
019700     03 BAS-R23-REST            PIC X(41).                                
019900     EJECT                                                                
020300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
020400 01  GENERELLA-SUBPROGRAM.                                                
020500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
020800     EJECT                                                                
020900*    --- PARAMETRAR TILL POSTSUM                                          
021000*                                                                         
021100*01  -COPY W0005   -PRE  POSTSUM-                                         
021200     EJECT                                                                
021300 01  PROG-TO-PROG-SW.                                                     
021400*    03  -COPY WMSGSOP                                                    
021500     EJECT                                                                
021600 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
021700 01  P-TO-P-SW.                                                           
021800     03  P-TO-P-KVLL                PIC S9(4)           COMP SYNC.        
021900     03  P-TO-P-KDZ1                PIC X(1)  VALUE LOW-VALUE.            
022000     03  P-TO-P-KDZ2                PIC X(1)  VALUE LOW-VALUE.            
022100     03  P-TO-P-KDTRANS             PIC X(8).                             
022200     03  P-TO-P-IDTRANS             PIC X(4).                             
022300     03  P-TO-P-KDMFSFOR            PIC X(1).                             
022400     03  P-TO-P-DATA.                                                     
022500        05 FILLER                  PIC X(1000).                           
022600                                                                          
022700**********************************************************                
022800***   I N K Ö P S - P O S T   P V                                         
022900**********************************************************                
023000*                                                                         
023100*01  -COPY A310TB65                -PRE A310-                             
023200     EJECT                                                                
023300*01  AREA   -COPY W092W001     -PRE W092-.                                
023400     EJECT                                                                
023500                                                                          
023600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023700     SKIP3                                                                
023800*01  -COPY WMSGAREA                                                       
023900     EJECT                                                                
024000 01  FILLER              PIC X(16)  VALUE 'PROG-TO-PROG-SW'.              
024100                                                                          
024800     EJECT                                                                
024900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025000     SKIP3                                                                
025100*01  -COPY WMFSAREA                                                       
025200     EJECT                                                                
025300                                                                          
025400*01  AREA -COPY W26169     -PRE IN-                                       
025500*                                                                         
025600     EJECT                                                                
025700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025800*                                                                         
025900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026000     SKIP3                                                                
026100 01  NYCKLAR-TILL-DLI.                                                    
026200*                                                                         
026300     03  W-IDARTNR-MIN-X.                                                 
026400         05  W-IDARTNR-MIN   PIC S9(9)  VALUE ZERO COMP-3.                
026500     03  W-IDARTNR-MAX-X.                                                 
026600         05  W-IDARTNR-MAX   PIC S9(9)  VALUE +999999999 COMP-3.          
026700     03  W-IDPERSON-MIN-X.                                                
026800         05  W-IDPERSON-MIN    PIC S9(3)  VALUE ZERO COMP-3.              
026900     03  W-IDPERSON-MAX-X.                                                
027000         05  W-IDPERSON-MAX    PIC S9(3)  VALUE +999 COMP-3.              
027100     03  W-IDDC-MIN-X.                                                    
027200         05  W-IDDC-MIN      PIC X(2)   VALUE LOW-VALUE.                  
027300     03  W-IDDC-MAX-X.                                                    
027400         05  W-IDDC-MAX      PIC X(2)   VALUE HIGH-VALUE.                 
027500     03  W-DASKROT9-MIN-X.                                                
027600         05  W-DASKROT9-MIN  PIC 9(8)   VALUE ZERO.                       
027700     03  W-DASKROT9-MAX-X.                                                
027800         05  W-DASKROT9-MAX  PIC 9(8)   VALUE 99999999.                   
027900     03  W-IDARTNR-X.                                                     
028000         05  W-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.                
028100     03  W-IDDC-X.                                                        
028200         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
028300     03  W-KDARBTYP-X.                                                    
028400         05  W-KDARBTYP      PIC X(8)   VALUE 'ANSK'.                     
028500     03  W-DASKROT9-X.                                                    
028600         05  W-DASKROT9      PIC 9(8)   VALUE ZERO.                       
028700     03  W-IDDC-6324-X.                                                   
028800         05  W-IDDC-6324      PIC X(2)   VALUE SPACE.                     
028900     03  W-KDSTASKR-X.                                                    
029000         05  W-KDSTASKR      PIC S9     VALUE 1 COMP-3.                   
029100     03  W-WDGXKEY-6321.                                                  
029200         05  W-6321-IDHTYP    PIC X(4)   VALUE '6321'.                    
029300         05  W-6321-KDARBTYP  PIC X(8)   VALUE SPACE.                     
029400         05  W-6321-LOWVALUE  PIC X(18)  VALUE LOW-VALUE.                 
029500     03  W-WDGXKEY-6327.                                                  
029600         05  W-6327-IDHTYP    PIC X(4)   VALUE '6327'.                    
029700         05  W-6327-KDARBTYP  PIC X(8)   VALUE SPACE.                     
029800         05  W-6327-IDDC      PIC X(2)   VALUE SPACE.                     
029900         05  W-6327-LOWVALUE  PIC X(16)  VALUE LOW-VALUE.                 
030000     03  W-IDUSER-GODK-X.                                                 
030100         05  W-IDUSER-GODK    PIC X(8)   VALUE SPACE.                     
030200     03  W-SUBEL-MIN-X.                                                   
030300         05  W-SUBEL-MIN      PIC 9(7)  VALUE ZERO.                       
030400     03  W-SUBEL-MAX-X.                                                   
030500         05  W-SUBEL-MAX      PIC 9(7)  VALUE 9999999.                    
030600     03 W-2401-KEY-X.                                                     
030700         05 FILLER               PIC X(4)    VALUE '2401'.                
030800         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
031300     03  W-IDRADNR-X.                                                     
031400         05  W-IDRADNR       PIC S9(5)  VALUE ZERO COMP-3.                
031500     03  W-1141-KEY-X.                                                    
031600         05 FILLER         PIC X(04)  VALUE '1141'.                       
031700         05 FILLER         PIC X(26)  VALUE LOW-VALUE.                    
031800     03  W-IDLEVNR-X.                                                     
031900         05 W-IDLEVNR      PIC X(5)   VALUE SPACE.                        
032000     03  W-WDG901KY-X.                                                    
032100         05  W-TIREGDAT          PIC S9(07)   VALUE ZERO COMP-3.          
032200         05  W-TIKLOCK           PIC S9(09)   VALUE ZERO COMP-3.          
033200     EJECT                                                                
033300*    --- STATUS-KOD FRÅN IMS                                              
033400 01  STATUS-WS                   PIC XX.                                  
033500     88  SEGMENT-FINNS                       VALUE '  '.                  
033600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033800     88  IMS-EJ-OK                           VALUE 'XD'.                  
033900     SKIP2                                                                
034000 01  GODK-STATUSKODER.                                                    
034100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034200     SKIP3                                                                
034300 01  SSA1                        PIC X(128).                              
034400 01  SSA2                        PIC X(256).                              
034500 01  SSA3                        PIC X(256).                              
034600 01  SSA4                        PIC X(256).                              
034700     EJECT                                                                
034800*    --- IMS FUNKTIONSKODER                                               
034900*01  -COPY W0003                                                          
035000     EJECT                                                                
035100 01  FILLER         PIC X(16) VALUE 'DLI-IO-EDK701'.                      
035200 01  DLI-IO-WDK701.                                                       
035300*    03  -COPY WDK701                                                     
035400     EJECT                                                                
035500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
035600 01  DLI-IO-WDK711.                                                       
035700*    03  -COPY WDK711                                                     
035800     EJECT                                                                
035900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK723'.                      
036000 01  DLI-IO-WDK723.                                                       
036100*    03  -COPY WDK723                                                     
036200     EJECT                                                                
037000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
037100 01  DLI-IO-WDR501-6321.                                                  
037200*    03  -COPY WDGX6321                                                   
037300     EJECT                                                                
037400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
037500 01  DLI-IO-WDGX6322.                                                     
037600*    03  -COPY WDGX6322                                                   
037700     EJECT                                                                
037800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
037900 01  DLI-IO-WDGX6324.                                                     
038000*    03  -COPY WDGX6324                                                   
038100     EJECT                                                                
038200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6326'.                    
038300 01  DLI-IO-WDGX6326.                                                     
038400*    03  -COPY WDGX6326                                                   
038500     EJECT                                                                
038600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6327'.                    
038700 01  DLI-IO-WDGX6327.                                                     
038800*    03  -COPY WDGX6327                                                   
038900     EJECT                                                                
039000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
039100 01  DLI-IO-WDGX6328.                                                     
039200*    03  -COPY WDGX6328                                                   
039300     EJECT                                                                
039400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-2401'.                 
039500 01  DLI-IO-WDR501-2401.                                                  
039600*    03  -COPY WDGX2402                                                   
039700     EJECT                                                                
040200                                                                          
043200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDG901'.                      
043300 01  DLI-IO-WDG901.                                                       
043400*    03  -COPY WDG901                                                     
043500     EJECT                                                                
044400                                                                          
044500 LINKAGE SECTION.                                                         
044600*01  -COPY W0009   -PRE MSG-                                              
044700     EJECT                                                                
044800*01  -COPY W0009   -PRE ALT-                                              
044900     EJECT                                                                
045200*01  -COPY W0008   -PRE WDK7-                                             
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008   -PRE 6321-                                             
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800*01  -COPY W0008   -PRE 6327-                                             
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100*01  -COPY W0008   -PRE 2401-                                             
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
048800 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB                               
049000                           WDK7-PCB 6321-PCB 6327-PCB                     
049100                           2401-PCB.                                      
049400 MAIN SECTION.                                                            
049500     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB                               
049700                           WDK7-PCB 6321-PCB 6327-PCB                     
049800                           2401-PCB.                                      
050100                                                                          
050200     PERFORM A-INIT                                                       
050300     PERFORM S00-LAES-W26193                                              
050400     IF NOT END-OF-W26193                                                 
050500        MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR                                 
050600     END-IF                                                               
050700     PERFORM UNTIL END-OF-W26193                                          
050800       IF CHKP-ANT > CHKP-MAX                                             
050900         PERFORM X-TAG-CHECKPOINT                                         
051000       END-IF                                                             
051100                                                                          
051200       PERFORM H-UPPDATERA                                                
051300                                                                          
051400       PERFORM S00-LAES-W26193                                            
051500                                                                          
051600       ADD +1           TO URV-IX                                         
051700       IF URV-IX > 12 OR                                                  
051800          END-OF-W26193 OR                                                
051900          IN-IDKUNDNR NOT = SPAR-IDKUNDNR                                 
052000          PERFORM UNTIL URV-IX > 12                                       
052100             MOVE SPACE TO URV-IDDC           (URV-IX)                    
052200             MOVE ZERO  TO URV-IDARTNR        (URV-IX)                    
052300             MOVE ZERO  TO URV-DASKROT9-BEORD (URV-IX)                    
052400             ADD +1     TO URV-IX                                         
052500          END-PERFORM                                                     
052600          PERFORM S02-STARTA-URV-TRANS                                    
052700          MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR                               
052800          MOVE +1       TO URV-IX                                         
052900       END-IF                                                             
053000                                                                          
053100     END-PERFORM                                                          
053200                                                                          
053300                                                                          
053400     PERFORM Z-FINIT                                                      
053500                                                                          
053600     MOVE ZERO TO RETURN-CODE                                             
053700     GOBACK                                                               
053800     .                                                                    
053900     EJECT                                                                
054000 A-INIT SECTION.                                                          
054100                                                                          
054200                                                                          
054300     PERFORM IMS-RESTART                                                  
054400                                                                          
054500     OPEN INPUT W26193                                                    
054600                                                                          
054700     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM-Y2K                 
054800                                                                          
054900                                                                          
055000     ACCEPT DAGENS-DATUM FROM DATE                                        
055600     ACCEPT POST-TIKLOCK FROM TIME                                        
055700     MOVE POST-TIKLOCK TO W-TIKLOCK                                       
055800                                                                          
055900     MOVE +1 TO URV-IX                                                    
056000     .                                                                    
056100     EJECT                                                                
056200                                                                          
056300 H-UPPDATERA SECTION.                                                     
056400                                                                          
056500     PERFORM HC-SKAPA-EN-SKROTORDER                                       
056600                                                                          
056700     .                                                                    
056800     EJECT                                                                
056900 HC-SKAPA-EN-SKROTORDER SECTION.                                          
057000                                                                          
057200     MOVE IN-IDARTNR         TO W-IDARTNR                                 
057300     MOVE W-IDARTNR          TO IDARTNR-WS                                
057400     MOVE IN-IDDC            TO W-IDDC                                    
057500                                W-IDDC-6324                               
057600                                W-6327-IDDC                               
057700                                WS-IDDC                                   
057800     MOVE W-KDARBTYP         TO W-6321-KDARBTYP                           
057900     COMPUTE WS-DASKROT9-BEORD = 99999999 - IN-DADATUM                    
058000     MOVE WS-DASKROT9-BEORD  TO W-DASKROT9                                
058100                                WS-6322-DASKROT9                          
058200     MOVE ZERO               TO 6324-KVSKROT-BEORD                        
058300                                                                          
058400     PERFORM IMS-GHU-WDR501-6321                                          
058500     IF SEGMENT-FINNS                                                     
058600        PERFORM IMS-GHNP-WDGX6324                                         
058700        IF SEGMENT-FINNS                                                  
058800           MOVE 'J'          TO 6324-FLSKROT-GODK                         
058900           PERFORM IMS-REPL-WDGX6324                                      
059300        END-IF                                                            
059400     END-IF                                                               
059500     PERFORM HE-UPDATERA-6326                                             
059600     MOVE 'J'                TO URV-FLKLAR                                
059700     PERFORM S01-SKAPA-URV-TRANS                                          
059800     PERFORM S15-SKAPA-WDGX2402                                           
059900                                                                          
060300     PERFORM IMS-GHU-WDK711                                               
060400     IF SEGMENT-FINNS                                                     
060410       IF SLAG-IDDC-REF = SPACE                                           
060500          COMPUTE SLAG-KVSPARR-KVAL =                                     
060600                  SLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD                  
060700          IF SLAG-KVSPARR-KVAL < ZERO                                     
060800             MOVE ZERO      TO SLAG-KVSPARR-KVAL                          
060900          END-IF                                                          
061000          MOVE WS-IDUSER    TO SLAG-IDUSER-SPKVAL                         
061100          MOVE DAGENS-DATUM TO SLAG-TISPARR-KVAL                          
061200                                                                          
061300          MOVE 'N'          TO SLAG-FLSKROT-BEORD                         
061400          MOVE 'J'          TO SLAG-FLSKROT-AUTO                          
061500          MOVE DAGENS-DATUM TO SLAG-TISKROT                               
062400          PERFORM IMS-REPL-WDK711                                         
062600       END-IF                                                             
062700     END-IF                                                               
062800     .                                                                    
062900     EJECT                                                                
063000                                                                          
063100 HE-UPDATERA-6326 SECTION.                                                
063200                                                                          
063300     ACCEPT WS-TID FROM TIME                                              
063400     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATUM                          
063500     MOVE WS-TID (1:6)               TO WS-TIDHHMMSS                      
063600     MOVE WS-TIDATETIME              TO 6326-TIDATETIME                   
063700     MOVE WS-IDUSER                  TO 6326-IDUSER-GODK                  
063800     MOVE WS-IDUSER                  TO W-IDUSER-GODK-X                   
063900     PERFORM IMS-GU-WDGX6328                                              
064000     MOVE 6328-BEANST-GODK           TO 6326-BEANST-GODK                  
064100     PERFORM IMS-ISRT-WDGX6326                                            
064200     .                                                                    
064300     EJECT                                                                
064400 Z-FINIT SECTION.                                                         
064500                                                                          
064600                                                                          
064700     CLOSE W26193                                                         
064800                                                                          
064900     .                                                                    
065000     EJECT                                                                
065100 S00-LAES-W26193  SECTION.                                                
065200                                                                          
065300     READ W26193 INTO IN-AREA                                             
065400     AT END                                                               
065500        SET END-OF-W26193 TO TRUE                                         
065600                                                                          
065700     NOT AT END                                                           
065800        MOVE 'W26193'   TO POSTSUM-FDNAMN                                 
065900        MOVE 'W26194D1' TO POSTSUM-DDNAMN2                                
066000        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
066100        CALL POSTSUM USING POSTSUM-PARM                                   
066200                                                                          
066300     END-READ                                                             
066400     .                                                                    
066500     EJECT                                                                
066600 S01-SKAPA-URV-TRANS SECTION.                                             
066700                                                                          
066800     MOVE W-KDARBTYP           TO URV-KDARBTYP                            
066900                                                                          
067000     MOVE IN-IDARTNR           TO URV-IDARTNR(URV-IX)                     
067100     MOVE IN-IDDC              TO URV-IDDC(URV-IX)                        
067200                                                                          
067300     COMPUTE WS-DASKROT9-BEORD = 999999999 - IN-DADATUM                   
067400     MOVE WS-DASKROT9-BEORD    TO URV-DASKROT9-BEORD(URV-IX)              
067500     .                                                                    
067600     EJECT                                                                
067700 S02-STARTA-URV-TRANS SECTION.                                            
067800                                                                          
067900     MOVE '6322'   TO MSGSOP-IDTRANS                                      
068000     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
068100     MOVE 'W216S1' TO MSGSOP-IDPROCESS                                    
068200     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
068300                                                                          
068400     STRING 'URVAL1(' WS-URVAL ') '                                       
068500            'URVAL2(' URV-TAB-RAD (1) ') '                                
068600            'URVAL3(' URV-TAB-RAD (2) ') '                                
068700            'URVAL4(' URV-TAB-RAD (3) ') '                                
068800            'URVAL5(' URV-TAB-RAD (4) ') '                                
068900            'URVAL6(' URV-TAB-RAD (5) ') '                                
069000            'URVAL7(' URV-TAB-RAD (6) ') '                                
069100            'URVAL8(' URV-TAB-RAD (7) ') '                                
069200            'URVAL9(' URV-TAB-RAD (8) ') '                                
069300            'URVAL10(' URV-TAB-RAD (9) ') '                               
069400            'URVAL11(' URV-TAB-RAD (10) ') '                              
069500            'URVAL12(' URV-TAB-RAD (11) ') '                              
069600            'URVAL13(' URV-TAB-RAD (12) ')'                               
069700              DELIMITED BY SIZE INTO MSGSOP-TESYMBV                       
069800                                                                          
069900     PERFORM IMS-INSERT-ALTMSG                                            
070000     .                                                                    
070100     EJECT                                                                
089900 S15-SKAPA-WDGX2402 SECTION.                                              
089910                                                                          
090200     MOVE 6324-IDARTNR      TO 2402-IDARTNR                               
090300     MOVE 6324-IDANALYS     TO 2402-IDANALYS                              
090400     MOVE 6324-IDDC         TO 2402-IDDC                                  
090500     MOVE 6324-IDDISTR      TO 2402-IDDISTR                               
090600     MOVE 6324-IDKONTO      TO 2402-IDKONTO                               
090700     MOVE 6324-IDKST        TO 2402-IDKST                                 
090800     MOVE 6324-IDPERSON     TO 2402-IDPERSON                              
090900     MOVE 6321-KDARBTYP     TO 2402-KDARBTYP                              
091000     MOVE 6324-KVSKROT-BEORD TO 2402-KVSKROT-BEORD                        
091100     MOVE 6324-KVSKROT-ONDEM TO 2402-KVSKROT-KVAR                         
091200     MOVE 6324-IDUSER       TO 2402-IDUSER                                
091300     MOVE 6324-BEANST       TO 2402-BEANST                                
091400     MOVE WS-6322-DASKROT9  TO 2402-DASKROT9-BEORD                        
091500     MOVE 6326-TIDATETIME   TO 2402-TIDATETIME(1)                         
091600     MOVE 6326-IDUSER-GODK  TO 2402-IDUSER-GODK(1)                        
091700     MOVE 6326-BEANST-GODK  TO 2402-BEANST-GODK(1)                        
091800     MOVE 6324-IDKUNDNR     TO 2402-IDKUNDNR                              
091900     MOVE 6324-KDERS-UTG    TO 2402-KDERS-UTG                             
092000     MOVE 6324-KVTILLG-CDC  TO 2402-KVTILLG-CDC                           
092100     MOVE 6324-KVAKS-CDC    TO 2402-KVAKS-CDC                             
092500     MOVE 6324-KVTILLG-SDC  TO 2402-KVTILLG-SDC                           
092600     MOVE 6324-KVAKS-SDC    TO 2402-KVAKS-SDC                             
092700     MOVE +1     TO IX-BEEMB                                              
092800     PERFORM UNTIL IX-BEEMB > 20                                          
092900       MOVE 6324-BEEMBLEM(IX-BEEMB) TO 2402-BEEMBLEM(IX-BEEMB)            
093000       ADD +1 TO IX-BEEMB                                                 
093100     END-PERFORM                                                          
093200     MOVE 6324-SUTPO-TOT    TO 2402-SUTPO-TOT                             
093300     PERFORM IMS-ISRT-WDGX2402                                            
093500     .                                                                    
093600     EJECT                                                                
105200                                                                          
105300 X-TAG-CHECKPOINT   SECTION.                                              
105400                                                                          
105500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
105600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
105700     PERFORM IMS-CHECKPOINT                                               
105800     MOVE ZERO TO CHKP-ANT                                                
105900* --- LÄS OM DATABAS OM DET BEHÖVS                                        
106000     .                                                                    
106100     EJECT                                                                
106200* --- IMS SEKTIONER ---                                                   
106300     SKIP3                                                                
106400 IMS-INSERT-ALTMSG SECTION.                                               
106500     MOVE SPACE TO GODK-STATUSKODER                                       
106600     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
106700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
106800     PERFORM IMS-STATUSKONTROLL                                           
106900     ADD +1  TO CHKP-ANT                                                  
107000     .                                                                    
107100     EJECT                                                                
108100 IMS-GHU-WDK711 SECTION.                                                  
108200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
108300          DELIMITED BY SIZE INTO SSA1                                     
108400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
108500          DELIMITED BY SIZE INTO SSA2                                     
108600     MOVE '  GE' TO GODK-STATUSKODER                                      
108700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
108800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
108900     PERFORM IMS-STATUSKONTROLL                                           
109000     .                                                                    
109100     SKIP3                                                                
110000 IMS-REPL-WDK711 SECTION.                                                 
110100     MOVE '  ' TO GODK-STATUSKODER                                        
110200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
110300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
110400     PERFORM IMS-STATUSKONTROLL                                           
110500     ADD +1  TO CHKP-ANT                                                  
110600     .                                                                    
110700     EJECT                                                                
113600 IMS-REPL-WDGX6324 SECTION.                                               
113700     MOVE '  ' TO GODK-STATUSKODER                                        
113800     CALL CBLTDLI USING REPL 6321-PCB DLI-IO-WDGX6324                     
113900     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
114000     PERFORM IMS-STATUSKONTROLL                                           
114100     ADD +1  TO CHKP-ANT                                                  
114200     .                                                                    
114300     EJECT                                                                
114400 IMS-GHU-WDR501-6321 SECTION.                                             
114500     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
114600          DELIMITED BY SIZE INTO SSA1                                     
114700     MOVE 'GE  ' TO GODK-STATUSKODER                                      
114800     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDR501-6321 SSA1              
114900     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
115000     PERFORM IMS-STATUSKONTROLL                                           
115100     .                                                                    
115200     SKIP3                                                                
115300 IMS-GHNP-WDGX6324 SECTION.                                               
115400     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
115500          DELIMITED BY SIZE INTO SSA1                                     
115600     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
115700                    '&IDDC     =' W-IDDC-6324-X                           
115800                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
115900          DELIMITED BY SIZE INTO SSA2                                     
116000     MOVE '  GE' TO GODK-STATUSKODER                                      
116100     CALL CBLTDLI USING GHNP 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2           
116200     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
116300     PERFORM IMS-STATUSKONTROLL                                           
116400     .                                                                    
116500     SKIP2                                                                
116600 IMS-ISRT-WDGX6326 SECTION.                                               
116700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
116800            DELIMITED BY SIZE INTO SSA1                                   
116900     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
117000            DELIMITED BY SIZE INTO SSA2                                   
117100     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
117200                    '&IDDC     =' W-IDDC-6324-X                           
117300                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
117400          DELIMITED BY SIZE INTO SSA3                                     
117500     MOVE 'WDGX6326'            TO SSA4                                   
117600     MOVE '  '                  TO GODK-STATUSKODER                       
117700     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6326                     
117800                                      SSA1 SSA2 SSA3 SSA4                 
117900     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
118000     PERFORM IMS-STATUSKONTROLL                                           
118100     ADD +1  TO CHKP-ANT                                                  
118200     .                                                                    
118300     EJECT                                                                
119400 IMS-GU-WDGX6328 SECTION.                                                 
119500     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6327 ')'                      
119600          DELIMITED BY SIZE INTO SSA1                                     
119700     STRING 'WDGX6328(SUBEL   =>' W-SUBEL-MIN-X                           
119800                    '&SUBEL   =<' W-SUBEL-MAX-X                           
119900                    '&IDUSERGK= ' W-IDUSER-GODK-X ')'                     
120000          DELIMITED BY SIZE INTO SSA2                                     
120100     MOVE '  GE' TO GODK-STATUSKODER                                      
120200     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6328 SSA1 SSA2             
120300     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
120400     PERFORM IMS-STATUSKONTROLL                                           
120500     .                                                                    
120600     SKIP2                                                                
120700 IMS-ISRT-WDGX2402 SECTION.                                               
120800     STRING 'WDR501  (WDGXKEY  =' W-2401-KEY-X ')'                        
120900            DELIMITED BY SIZE INTO SSA1                                   
121000     MOVE 'WDGX2402'            TO SSA2                                   
121100     MOVE '  '                  TO GODK-STATUSKODER                       
121200     CALL CBLTDLI USING ISRT 2401-PCB DLI-IO-WDR501-2401 SSA1 SSA2        
121300     MOVE 2401-STATUS-CODE      TO STATUS-WS                              
121400     PERFORM IMS-STATUSKONTROLL                                           
121500     ADD +1  TO CHKP-ANT                                                  
121600     SKIP3                                                                
121700     .                                                                    
121800     EJECT                                                                
127800                                                                          
134000 IMS-RESTART SECTION.                                                     
134100     SKIP2                                                                
134200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
134300     MOVE '  ' TO GODK-STATUSKODER                                        
134400     CALL CBLTDLI USING XRST MSG-PCB                                      
134500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
134600                        CHKP-AREA-LENGTH CHKP-AREA                        
134700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
134800     PERFORM IMS-STATUSKONTROLL                                           
134900     .                                                                    
135000     SKIP3                                                                
135100 IMS-CHECKPOINT SECTION.                                                  
135200     SKIP2                                                                
135300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
135400     MOVE '  XD' TO GODK-STATUSKODER                                      
135500     CALL CBLTDLI USING CHKP MSG-PCB                                      
135600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
135700                        CHKP-AREA-LENGTH CHKP-AREA                        
135800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
135900     PERFORM IMS-STATUSKONTROLL                                           
136000                                                                          
136100     IF IMS-EJ-OK                                                         
136200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
136300       DISPLAY FELTEXT                                                    
136400       CALL FELLOG                                                        
136500     END-IF                                                               
136600     .                                                                    
136700     EJECT                                                                
136800                                                                          
136900 IMS-STATUSKONTROLL SECTION.                                              
137000     SET STATUS-IX TO 1                                                   
137100     SEARCH GODK-STATUS                                                   
137200       AT END                                                             
137300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
137400         DELIMITED BY SIZE INTO FELTEXT                                   
137500         CALL FELLOG                                                      
137600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
137700         CONTINUE                                                         
137800     END-SEARCH                                                           
137900     .                                                                    
