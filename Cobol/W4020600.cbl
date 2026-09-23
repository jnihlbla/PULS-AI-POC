000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4020600.                                                
000300 AUTHOR.         GÖRAN KJELLSON  GUIDE                                    
000400 DATE-WRITTEN.   JANUARI   2007                                           
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        PROGRAMMET HANTERAR TVINGANDE TILLÄGG AV ORDERRADER.             
001000*        REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA HÄMTAS FRÅN          
001100*        ARTIKELREGISTRET.                                                
001200*                                                                         
001300*        EFTER UPPLÄGGNING AV GODKÄNDA RADER SKER UTHOPP TILL             
001400*        ORDERAVSLUT 4213/4243.                                           
001500                                                                          
001600*                                                                         
001700*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
001800*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKRÄFTELSER           
001900*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD                  
002000*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)  ORDERRADER                  
002100*        PROGRAMMET LÄSER              WDK6   ARTIKELREGISTER             
002200*        PROGRAMMET LÄSER              WDK9   ARTIKELREGISTER             
002300*        PROGRAMMET LÄSER      WLXXKM (WDR1)  RANSFAKTOR.TAB              
002400*        PROGRAMMET LÄSER      WLXXKN (WDR1)  LEDTIDS.TAB                 
002500*        PROGRAMMET LÄSER      WLXXKB (WDR1)  TRANSPORTAVGÅNGAR           
002600*        PROGRAMMET LÄSER      WLLEVF (WDF2)  STYRREG DIRLEV              
002700*        PROGRAMMET LÄSER      WLLEVG (WDF2)  STYRREG DIRLEV ASEQ         
002800*        PROGRAMMET LÄSER      WLLEVA (WDF1)  LEVERANTÖRSREG              
002900*        PROGRAMMET LÄSER      WLERSA (WDD7)  ERSÄTTNINGSREG.             
003000*        PROGRAMMET LÄSER              WDM2   KAMPANJREGISTER             
003100*        PROGRAMMET LÄSER      WLZZAC (WDG6)  TRANSAKTIONSBAS             
003200*        PROGRAMMET LÄSER      WLORDP (WDA5)  RESTORDERREG.               
003300*        PROGRAMMET LÄSER      WLXXBU (WDR5)  LARMKÖ                      
003400*        PROGRAMMET LÄSER      WL4437 (WDR1)  ARBETSTIDKALENDER           
003500*                                                                         
003600*                                                                         
003700*    INDATA.                                                              
003800*        TRANSAKTION: W4T206                                              
003900*        MID:         W4I20601                                            
004000*    UTDATA.                                                              
004100*        MOD:         W4O20601                                            
004200*                                                                         
004300*    E'TRACKER: 5444132 DATED 2007-08-21                                  
004400*    E'TRACKER: 7450328 DATED 2008 HÖST   VOHF                            
004500*    E'TRACKER: 8081720  DATED 2009-04-09  ORDER STEERING                 
004600*    E'TRACKER: 10254592      2015        DECOMISSION VOHF                
004700                                                                          
004800                                                                          
004900 ENVIRONMENT DIVISION.                                                    
005000 DATA DIVISION.                                                           
005100 WORKING-STORAGE SECTION.                                                 
005200                                                                          
005300 77  IDPGM                       PIC X(08)  VALUE 'W4020600'.             
005400 77  CURRENT-SECTION             PIC X(16)  VALUE 'MAIN'.                 
005500 77  CURRENT-IMS-SECTION         PIC X(16)  VALUE SPACE.                  
005600 77  HOPP-TILL-4213-4243         PIC X(1)   VALUE 'N'.                    
005700 77  HOPP-TILL-0504              PIC X(1)   VALUE 'N'.                    
005800 77  YES                         PIC X(1)   VALUE 'Y'.                    
005900 77  NEJ                         PIC X(1)   VALUE 'N'.                    
006000 77  JA                          PIC X(1)   VALUE 'J'.                    
006100 77  SW-KDORDSTA-ALL-E-FLAG      PIC X(1)   VALUE 'J'.                    
006200 77  SW-KDORDSTA-O-ALL-SPACE-FLAG PIC X(1)  VALUE 'J'.                    
006300 77  SW-KDORDSTA-O-STATUS-FLAG   PIC X(1)   VALUE 'J'.                    
006400 77  SW-KDORDSTA-STATUS-FLAG     PIC X(1)   VALUE 'J'.                    
006500*                                                                         
006600*    ---FOR MOD0504-IDTRANS                                               
006700 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
006800                                                                          
006900 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
007000 77  WS-IDTRANS-SPAR             PIC X(4)   VALUE SPACE.                  
007100*01  -COPY WWDCKONS                                                       
007200                                                                          
007300*01  -COPY WWPRODSL                                                       
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
007600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
007700 77  FELTEXT                     PIC  X(64) VALUE SPACE.                  
007800 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
007900 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
008000 77  HFAK-TAB-IX                 PIC S9(4)  VALUE +0    COMP SYNC.        
008100 77  IX-DCCLEAR-MAX              PIC S9(9)   COMP SYNC VALUE +99.         
008200 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
008300 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
008400 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +14.         
008500 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
008600 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
008700 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
008800 77  WS-INDEX-WOPS-MAX           PIC S9(9)   COMP SYNC VALUE +100.        
008900 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
009000 77  WS-HFAK-REF-X10             PIC X(10)   VALUE SPACE.                 
009100 77  W-KDORDBEK                  PIC S9(2)   VALUE +0.                    
009200 77  WX-KDORDBEK                 PIC S9(2)   VALUE +0.                    
009300 77  W-KDTPOTYP                  PIC S9      VALUE +0.                    
009400 77  WS-KVSLASK                  PIC S9(7)   VALUE +0  COMP-3.            
009500 77  WS-CLDC-IX                  PIC S9(5)   VALUE +1   COMP-3.           
009600 77  WS-SAVE-INDEX               PIC S9(9)   COMP SYNC VALUE ZERO.        
009700 77  IDDC-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
009800 77  W-TILLK-DC                  PIC X(2)    VALUE SPACE.                 
009900                                                                          
010000 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
010100 01  FILLER REDEFINES WS-TIHHMMSS.                                        
010200     03 WS-TIHHMM                PIC 9(4).                                
010300     03 FILLER                   PIC 9(2).                                
010400                                                                          
010500 01  FILLER PIC X(16)   VALUE 'SPARAREA'.                                 
010600 01  SPAR-AREA.                                                           
010700     03  SPAR-IDTRANS            PIC X(4).                                
010800     03  SPAR-IDDC-MSGI          PIC X(2).                                
010900                                                                          
011000 01  WS-IDKUNDRF.                                                         
011100     03  FILLER                  PIC X(2)    VALUE '00'.                  
011200     03  WS-IDORDNR5-X.                                                   
011300         05 WS-IDORDNR5          PIC 9(5).                                
011400     03  FILLER                  PIC X(3)    VALUE SPACE.                 
011500                                                                          
011600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011700     88  ALLT-OK                             VALUE 'J'.                   
011800                                                                          
011900 77  FIRST-TIME-SW               PIC X       VALUE 'N'.                   
012000     88  FIRST-TIME                          VALUE 'J'.                   
012100                                                                          
012200 77  TILLK-SW                    PIC X       VALUE 'N'.                   
012300     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
012400     88  EJ-TILLKOMMANDE-RAD                 VALUE 'N'.                   
012500                                                                          
012600 77  KOLLA-ERS-SW                PIC X       VALUE 'N'.                   
012700     88  KOLLA-ERS                           VALUE 'J'.                   
012800                                                                          
012900 77  SVARSBILD-SW                PIC X       VALUE 'N'.                   
013000     88  SVARSBILD                           VALUE 'J'.                   
013100                                                                          
013200 77  OBKR-SW                     PIC X       VALUE 'N'.                   
013300     88  SKRIV-OBKR                          VALUE 'J'.                   
013400     88  OBKR-SKRIVEN                        VALUE 'S'.                   
013500                                                                          
013600 77  EGET-CL-RAD-SW              PIC X       VALUE 'N'.                   
013700     88  EGET-CL-RAD                         VALUE 'J'.                   
013800                                                                          
013900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
014000     88  EGEN-MID                            VALUE '4206'.                
014100*                                                                         
014200 77  BEVARREF-I-HFAK-TAB-SW      PIC X       VALUE 'N'.                   
014300     88  BEVARREF-I-HFAK-TAB                 VALUE 'J'.                   
014400                                                                          
014500 77  KOLLA-ARBTAB-C1-SW          PIC X       VALUE 'N'.                   
014600     88  KOLLA-ARBTAB-C1                     VALUE 'J'.                   
014700     88  KOLLA-ARBTAB-C1-EJ                  VALUE 'N'.                   
014800                                                                          
014900 77  RAD-GODKAND-SW              PIC X       VALUE 'N'.                   
015000     88  RAD-GODKAND                         VALUE 'J'.                   
015100                                                                          
015200 77  RAD-LO-60-SW                PIC X       VALUE 'N'.                   
015300     88  RAD-LO-60                           VALUE 'J'.                   
015400                                                                          
015500 77  RAD-LO-61-SW                PIC X       VALUE 'N'.                   
015600     88  RAD-LO-61                           VALUE 'J'.                   
015700                                                                          
015800 77  BAL-DC-FND-SW               PIC X       VALUE 'N'.                   
015900     88  BAL-DC-FND                          VALUE 'J'.                   
016000                                                                          
016100 77  TILLK-BAL-DC-FND-SW         PIC X       VALUE 'N'.                   
016200     88  TILLK-BAL-DC-FND                    VALUE 'J'.                   
016300                                                                          
016400 77  CDC-MOVE-SW                 PIC X       VALUE 'N'.                   
016500     88  CDC-MOVE                            VALUE 'J'.                   
016600                                                                          
016700 77  KDERS-CHAIN-SW              PIC X       VALUE 'N'.                   
016800     88  KDERS-CHAIN                         VALUE 'J'.                   
016900                                                                          
017000 01  WS-ALFA-1.                                                           
017100     03  WS-NUM-1                PIC 9(1).                                
017200 01  WS-ALFA-2.                                                           
017300     03  WS-NUM-2                PIC 9(2).                                
017400 01  WS-ALFA-6.                                                           
017500     03  WS-NUM-6                PIC 9(6).                                
017600 01  WS-ALFA-7.                                                           
017700     03  WS-NUM-7                PIC 9(7).                                
017800 01  WS-ALFA-8.                                                           
017900     03  WS-NUM-8                PIC 9(8).                                
018000     03  FILLER REDEFINES WS-NUM-8.                                       
018100         05  WS-NUM-1--4         PIC 9(4).                                
018200         05  WS-NUM-5--8         PIC 9(4).                                
018300 01  WS-ALFA-12.                                                          
018400     03  WS-NUM-12               PIC 9(12).                               
018500 01  WS-ALFA-2V3.                                                         
018600     03  WS-ALFA-1--2            PIC X(2).                                
018700     03  WS-NUM-PUNKT            PIC X(1).                                
018800     03  WS-ALFA-3--5            PIC X(3).                                
018900 01  WS-NUM-2V3                  PIC 9(2)V9(3).                           
019000 01  FILLER REDEFINES WS-NUM-2V3.                                         
019100     03  WS-NUM-1--2             PIC 9(2).                                
019200     03  WS-NUM-3--5             PIC 9(3).                                
019300                                                                          
019400                                                                          
019500 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
019600 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
019700     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
019800     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
019900                                                                          
020000 01  W-WORK-VAR.                                                          
020100     03 W-FLREFILL-MAIN          PIC X       VALUE SPACE.                 
020200     03 W-KDPRODSL-MAIN          PIC S9(3)   COMP-3 VALUE 0.              
020300     03 W-KDSORT-MAIN            PIC X(2)    VALUE SPACE.                 
020400     03 W-KVQPACK-1-MAIN         PIC S9(5)   COMP-3 VALUE 0.              
020500     03 W-REDIRLEV-MAIN          PIC S9V9(2) COMP-3 VALUE 0.              
020600     03 W-FLREFILL-REPL          PIC X       VALUE SPACE.                 
020700     03 W-KDPRODSL-REPL          PIC S9(3)   COMP-3 VALUE 0.              
020800     03 W-KDSORT-REPL            PIC X(2)    VALUE SPACE.                 
020900     03 W-KVQPACK-1-REPL         PIC S9(5)   COMP-3 VALUE 0.              
021000     03 W-REDIRLEV-REPL          PIC S9V9(2) COMP-3 VALUE 0.              
021100     03 W-IDARTNR-SDCA           PIC S9(9)   COMP-3 VALUE 0.              
021200     03 W-GMT-IDDC-CLEAR-GRP.                                             
021300*                                 GRUPP AV IDDC-CLEAR                     
021400        05 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                             
021500                                 PIC X(2)    VALUE SPACE.                 
021600                                                                          
021700 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
021800*01  FILLER -COPY WWDIST07 -RED TEST-IDDISTR.                             
021900                                                                          
022000*01  FILLER -COPY WWDIST18 -RED TEST-IDDISTR.                             
022100                                                                          
022200                                                                          
022300*    ----DIST79-DEALER-PRICE----                                          
022400*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
022500*                                                                         
022600 01 FILLER                       PIC X(8) VALUE 'W411TILK'.               
022700*    -COPY W411TILK                                                       
022800                                                                          
022900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
023000 01  GENERELLA-SUBPROGRAM.                                                
023100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
023200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
023300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
023500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
023600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
023700                                                                          
023800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
023900*01 -COPY WMSGINIT                                                        
024000*                                                                         
024100*                                                                         
024200 01  GEMENSAMMA-SUBPROGRAM.                                               
024300     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
024400*        PRISTILLÄMPNING                                                  
024500     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
024600*        HÄMTA PRISFRÅGENR                                                
024700     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
024800*        DEALER PRISFRÅGABEHANDLING                                       
024900     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
025000*        LÄSNING ARTIKELREGISTER                                          
025100     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
025200*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
025300     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
025400*        KONTROLL DIREKTLEVERANS                                          
025500     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
025600*        KONTROLL DIREKTLEVERANS                                          
025700     03  W411KAMP                PIC X(8)    VALUE 'W411KAMP'.            
025800*        KONTROLL TPO4 - KAMPANJ                                          
025900     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
026000*        KONTROLL ERSÄTTNINGAR                                            
026100     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
026200*        KONTROLL KVANTANPASSNING                                         
026300     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
026400*        FORMELLA KONTROLLER AV INDATA                                    
026500                                                                          
026600     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
026700*        KONTROLL PRELIMINÄRAVBOKNING-CDC                                 
026800     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
026900*        BERÄKNA RANSONERING                                              
027000     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
027100*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
027200     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
027300*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
027400     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
027500*        KONTROLL PRELIMINÄRAVBOKNING-SDC                                 
027600     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
027700*        KONTROLL SPÄRRAR                                                 
027800     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
027900*        KONTROLL STORA UTTAG                                             
028000     03  W411TPO1                PIC X(8)    VALUE 'W411TPO1'.            
028100*        KONTROLL TPO1                                                    
028200     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
028300*        KONTROLL TPO2                                                    
028400     03  W411RELS                PIC X(8)    VALUE 'W411RELS'.            
028500*        KONTROLL RELS                                                    
028600     03  W411CLDC                PIC X(8)    VALUE 'W411CLDC'.            
028700*        WDB601-SEGMENT FÖR CLARING-DC                                    
028800     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
028900*        WOPS RADBEHANDLING                                               
029000     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
029100*        OMVANDLING AV LAGOMR + PLATS                                     
029200*                                                                         
029300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
029400*   -COPY WMEDAREA                                                        
029500     SKIP3                                                                
029600 01  FILLER.                                                              
029700   03  FELMEDD-AREA.                                                      
029800     05  FELMEDD-ENGLISH.                                                 
029900       10  FILLER                PIC X(50)                                
030000     VALUE '622 4206 NOT AVAILABLE ONLY FOR CONSOLIDATE ORDERS'.          
030100*                                                                         
030200 01  MESSAGE-CODES.                                                       
030300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
030400     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
030500     03  ERR-ORDER-EJ-AVSLUT     PIC X(3)    VALUE '053'.                 
030600     03  ERR-STORT-UTTAG         PIC X(3)    VALUE '725'.                 
030700     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
030800     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
030900     03  ERR-EJ-TILLAEGG         PIC X(3)    VALUE '077'.                 
031000                                                                          
031100*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
031200*                                                                         
031300 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
031400*   -COPY W335PRIS                                                        
031500                                                                          
031600 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
031700*   -COPY W335PRNO                                                        
031800                                                                          
031900 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
032000*   -COPY W335PRQU                                                        
032100                                                                          
032200 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
032300*   -COPY W411AREG                                                        
032400                                                                          
032500 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
032600*   -COPY W411ARTM                                                        
032700                                                                          
032800 01 FILLER                       PIC X(8) VALUE 'W411DLEV'.               
032900*   -COPY W411DLEV                                                        
033000                                                                          
033100 01 FILLER                       PIC X(8) VALUE 'W411DNOT'.               
033200*   -COPY W411DNOT                                                        
033300                                                                          
033400 01 FILLER                       PIC X(8) VALUE 'W411KAMP'.               
033500*   -COPY W411KAMP                                                        
033600                                                                          
033700 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
033800*   -COPY W411KERS                                                        
033900                                                                          
034000 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
034100*   -COPY W411KVAN                                                        
034200                                                                          
034300 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
034400*   -COPY W411ORFK                                                        
034500                                                                          
034600 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
034700*   -COPY W411CDCA                                                        
034800                                                                          
034900 01 FILLER                       PIC X(8) VALUE 'W411RANS'.               
035000*   -COPY W411RANS                                                        
035100                                                                          
035200 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
035300*   -COPY W411NDCA                                                        
035400                                                                          
035500 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
035600*   -COPY W411XDCA                                                        
035700                                                                          
035800 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
035900*   -COPY W411XDK7 -PRE NDCA-                                             
036000                                                                          
036100 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
036200*   -COPY W411SDCA                                                        
036300                                                                          
036400 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
036500*   -COPY W411SPAR                                                        
036600                                                                          
036700 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
036800*   -COPY W411STOR                                                        
036900                                                                          
037000 01 FILLER                       PIC X(8) VALUE 'W411TPO1'.               
037100*   -COPY W411TPO1                                                        
037200                                                                          
037300 01 FILLER                       PIC X(8) VALUE 'W411TPO2'.               
037400*   -COPY W411TPO2                                                        
037500                                                                          
037600 01 FILLER                       PIC X(8) VALUE 'W411RELS'.               
037700*   -COPY W411RELS                                                        
037800                                                                          
037900 01 FILLER                       PIC X(8) VALUE 'W411CLDC'.               
038000*   -COPY W411CLDC                                                        
038100                                                                          
038200 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
038300*   -COPY W413AVSR                                                        
038400                                                                          
038500 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
038600*   -COPY W413ADRS                                                        
038700                                                                          
038800*----> TABELL FÖR ATT ÖVERSÄTTA HF-AK-PLOCK                               
038900                                                                          
039000*   -COPY W413WHFA                                                        
039100                                                                          
039200                                                                          
039300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
039400*                                                                         
039500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
039600*01  MID -COPY W4I20601                                                   
039700                                                                          
039800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
039900*01  -COPY WMSGAREA                                                       
040000                                                                          
040100*    03  MOD -COPY W4O20601   -RED MSG-AREA.                              
040200*   TO RETURN TO MAIN MENU                                                
040300*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
040400     EJECT                                                                
040500                                                                          
040600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
040700                                                                          
040800*01  -COPY WMFSAREA                                                       
040900                                                                          
041000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
041100*                                                                         
041200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
041300 01  NYCKLAR-TILL-DLI.                                                    
041400                                                                          
041500     03  W-IDGMTREF-X.                                                    
041600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
041700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
041800         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
041900                                                                          
042000     03  W-IDDC-X.                                                        
042100         05  W-IDDC-WDQ212       PIC X(2)    VALUE SPACE.                 
042200                                                                          
042300     03  W-WDQ211KY-X.                                                    
042400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
042500         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
042600                                                                          
042700     03  W-IDARTNR-X.                                                     
042800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
042900                                                                          
043000     03  W-WDQ101KY-MIN-X.                                                
043100         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
043200         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
043300         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
043400         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
043500         05  FILLER              PIC  X(04)  VALUE LOW-VALUE.             
043600                                                                          
043700     03  W-WDQ101KY-MAX-X.                                                
043800         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
043900         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
044000         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
044100         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
044200         05  FILLER              PIC  X(04)  VALUE HIGH-VALUE.            
044300     03  W-IDGMT-X.                                                       
044400         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
044500         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
044600*                                                                         
044700     03  W-WDB101KY-X.                                                    
044800       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
044900       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
045000                                                                          
045100     03  W-IDDC-B6-X.                                                     
045200       05 W-IDDC-B6              PIC X(2).                                
045300                                                                          
045400     03  W-IDORDER-X.                                                     
045500         05  W-IDORDER           PIC S9(7)   VALUE +0 COMP-3.             
045600                                                                          
045700*    --- STATUS-KOD FRÅN IMS                                              
045800 01  STATUS-WS                   PIC XX.                                  
045900     88  SEGMENT-FINNS                       VALUE '  '.                  
046000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
046100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
046200     88  BASEN-SLUT                          VALUE 'GB'.                  
046300                                                                          
046400 01  GODK-STATUSKODER.                                                    
046500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
046600                                                                          
046700*                            DB2 FUNKTIONSKODER                           
046800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
046900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
047000                                                                          
047100                                                                          
047200 01  SSA1                        PIC X(96).                               
047300 01  SSA2                        PIC X(64).                               
047400                                                                          
047500*    --- IMS FUNKTIONSKODER                                               
047600*01  -COPY W0003                                                          
047700                                                                          
047800*    ---  DLI INPUT-OUTPUT AREA                                           
047900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
048000                                                                          
048100 01  DLI-IO-AREA-OBKR.                                                    
048200*    03  -COPY WDQ101                                                     
048300                                                                          
048400 01  DLI-IO-AREA-OHUV.                                                    
048500*    03  -COPY WDQ201                                                     
048600                                                                          
048700 01  DLI-IO-AREA-ARB.                                                     
048800*    03  -COPY WDQ212                                                     
048900                                                                          
049000 01  DLI-IO-AREA-DLEV.                                                    
049100*    03  -COPY WDQ211                                                     
049200                                                                          
049300 01  DLI-IO-AREA-ORAD.                                                    
049400*    03  -COPY WDQ401                                                     
049500                                                                          
049600 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
049700 01  DLI-IO-AREA-WDK901.                                                  
049800*    03  -COPY WDK901                                                     
049900                                                                          
050000 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
050100 01  DLI-IO-AREA-WDB201.                                                  
050200*    03  -COPY WDB201                                                     
050300                                                                          
050400 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
050500 01  DLI-IO-AREA-WDB101.                                                  
050600*    03  -COPY WDB101                                                     
050700                                                                          
050800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
050900 01   DLI-IO-AREA-B601.                                                   
051000*     03  -COPY WDB601                                                    
051100                                                                          
051200 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
051300 01   DLI-IO-AREA-R601.                                                   
051400*     03  -COPY WDR601                                                    
051500*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
051600     EJECT                                                                
051700                                                                          
051800*---MSG-AREA FÖR HOPP TILL 4213-SVARSBILDEN                               
051900                                                                          
052000 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
052100 01  4213-MSG-IO-AREA.                                                    
052200     03  4213-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
052300     03  4213-Z1               PIC X.                                     
052400     03  4213-Z2               PIC X.                                     
052500     03  4213-TRANSKOD         PIC X(8)   VALUE 'W4T213  '.               
052600     03  4213-IDTRANS          PIC X(4)   VALUE '4206'.                   
052700     03  4213-SPRAK            PIC X.                                     
052800     03  4213-IDDISTR-IN       PIC X(4).                                  
052900     03  4213-IDKUNDNR-IN      PIC X(6).                                  
053000     03  4213-IDORDNR-IN       PIC X(5).                                  
053100     03  4213-IDDISTR-UT       PIC X(4).                                  
053200     03  4213-IDKUNDNR-UT      PIC X(6).                                  
053300     03  4213-IDORDNR-UT       PIC X(5).                                  
053400                                                                          
053500 01  4243-MSG-IO-AREA.                                                    
053600     03  4243-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
053700     03  4243-Z1               PIC X.                                     
053800     03  4243-Z2               PIC X.                                     
053900     03  4243-TRANSKOD         PIC X(8)   VALUE 'W4T243  '.               
054000     03  4243-IDTRANS          PIC X(4)   VALUE '4206'.                   
054100     03  4243-SPRAK            PIC X.                                     
054200     03  4243-IDDISTR-IN       PIC X(4).                                  
054300     03  4243-IDKUNDNR-IN      PIC X(6).                                  
054400     03  4243-IDORDNR-IN       PIC X(5).                                  
054500     03  4243-IDDISTR-UT       PIC X(4).                                  
054600     03  4243-IDKUNDNR-UT      PIC X(6).                                  
054700     03  4243-IDORDNR-UT       PIC X(5).                                  
054800                                                                          
054900 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
055000 01  -COPY WZ01SEND                                                       
055100                                                                          
055200 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
055300 01  SEND-AREA.                                                           
055400*    03  -COPY WZ01REQU  -PRE 3039-                                       
055500*    03  -COPY W30391I1  -PRE 3039-                                       
055600                                                                          
055700                                                                          
055800 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
055900                                                                          
056000*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
056100                                                                          
056200     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
056300                                                                          
056400                                                                          
056500 LINKAGE SECTION.                                                         
056600                                                                          
056700*01  -COPY W0009      -PRE MSG-                                           
056800*01  -COPY W0009      -PRE 4213-                                          
056900*01  -COPY W0009      -PRE 4243-                                          
057000                                                                          
057100 01  AVSR-LIMS-PCB               PIC X.                                   
057200                                                                          
057300 01  2109-PCB                    PIC X.                                   
057400                                                                          
057500 01  PRQRY-PCB                   PIC X.                                   
057600*01  -COPY W0008      -PRE USEA-                                          
057700     05  FILLER                  PIC X.                                   
057800                                                                          
057900*01  -COPY W0008      -PRE WDQ1-                                          
058000     05  FILLER                  PIC X.                                   
058100                                                                          
058200*01  -COPY W0008      -PRE WDQ2-                                          
058300     05  FILLER                  PIC X.                                   
058400                                                                          
058500*01  -COPY W0008      -PRE WDQ2-UPD-                                      
058600     05  FILLER                  PIC X.                                   
058700                                                                          
058800*01  -COPY W0008      -PRE WDQ4-                                          
058900     05  FILLER                  PIC X.                                   
059000                                                                          
059100*01  -COPY W0008      -PRE WDK9-                                          
059200     05  FILLER                  PIC X.                                   
059300                                                                          
059400*01  -COPY W0008      -PRE WDB2-                                          
059500     05  FILLER                  PIC X.                                   
059600                                                                          
059700*01  -COPY W0008      -PRE WDB1-                                          
059800     05  FILLER                  PIC X.                                   
059900                                                                          
060000*01  -COPY W0008      -PRE WDB6-                                          
060100     05  FILLER                  PIC X.                                   
060200                                                                          
060300*01  -COPY W0008      -PRE WDR6-                                          
060400     05  FILLER                  PIC X.                                   
060500                                                                          
060600 01  PRIS-ARTC-PCB               PIC X.                                   
060700 01  PRIS-WDK7-PCB               PIC X.                                   
060800 01  PRIS-GMTA-PCB               PIC X.                                   
060900 01  PRIS-BETA-PCB               PIC X.                                   
061000 01  PRIS-GPRIA-PCB              PIC X.                                   
061100 01  PRIS-GPRIB-PCB              PIC X.                                   
061200 01  PRIS-COST-WDK6-PCB          PIC X.                                   
061300 01  PRIS-COST-WDK7-PCB          PIC X.                                   
061400 01  PRIS-COST-WDF1-PCB          PIC X.                                   
061500 01  PRIS-COST-9305-PCB          PIC X.                                   
061600 01  PRIS-COST-WDK72-PCB         PIC X.                                   
061700 01  PRIS-COST-WDB6-PCB          PIC X.                                   
061800 01  PRNO-3107-PCB               PIC X.                                   
061900 01  PRQU-WDG2-PCB               PIC X.                                   
062000 01  PRQU-WDC7-PCB               PIC X.                                   
062100 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
062200 01  AREG-WDK6-PCB               PIC X.                                   
062300 01  AREG-WDK7-PCB               PIC X.                                   
062400 01  ARTM-ARTM-PCB               PIC X.                                   
062500 01  DLEV-LEVF-PCB               PIC X.                                   
062600 01  DLEV-LEVG-PCB               PIC X.                                   
062700 01  DLEV-LEVA-PCB               PIC X.                                   
062800 01  DLEV-ARTS-PCB               PIC X.                                   
062900 01  DLEV-WDB6-PCB               PIC X.                                   
063000 01  SPAR-WDF8-PCB               PIC X.                                   
063100 01  SPAR-WDF8A-PCB              PIC X.                                   
063200 01  SPAR-WDK6-PCB               PIC X.                                   
063300 01  DNOT-ORQP-PCB               PIC X.                                   
063400 01  DNOT-ORQP2-PCB              PIC X.                                   
063500 01  DNOT-ORQP3-PCB              PIC X.                                   
063600 01  DNOT-4013-PCB               PIC X.                                   
063700 01  DNOT-BENA-PCB               PIC X.                                   
063800 01  KAMP-ORDP-PCB               PIC X.                                   
063900 01  KAMP-ZZAC-PCB               PIC X.                                   
064000 01  KAMP-WDM2-PCB               PIC X.                                   
064100 01  KERS-ARTC-PCB               PIC X.                                   
064200 01  KERS-ERSA-PCB               PIC X.                                   
064300 01  NDCA-USEA-PCB               PIC X.                                   
064400 01  NDCA-WDK7-PCB               PIC X.                                   
064500 01  NDCA-WDL6-PCB               PIC X.                                   
064600 01  NDCA-WDB6-PCB               PIC X.                                   
064700 01  SDCA-ARTS-PCB               PIC X.                                   
064800 01  SDCA-WDB6-PCB               PIC X.                                   
064900 01  SDCA-WDK9-PCB               PIC X.                                   
065000 01  SDCA-WDR6-PCB               PIC X.                                   
065100 01  SDCA-WDK6-PCB               PIC X.                                   
065200 01  SDCA-WDQ4B-PCB              PIC X.                                   
065300 01  SDCA-WDQ2-PCB               PIC X.                                   
065400 01  SDCA-WDQ4-PCB               PIC X.                                   
065500 01  SDCA-WDB6-2-PCB             PIC X.                                   
065600 01  SDCA-WDK6-2-PCB             PIC X.                                   
065700 01  SDCA-WDK7-2-PCB             PIC X.                                   
065800 01  SDCA-WDK7-3-PCB             PIC X.                                   
065900 01  CDCA-ARTM-PCB               PIC X.                                   
066000 01  CDCA-INLB-PCB               PIC X.                                   
066100 01  CDCA-WDB2-PCB               PIC X.                                   
066200 01  CDCA-WDC1-PCB               PIC X.                                   
066300 01  RANS-XXKM-PCB               PIC X.                                   
066400 01  RANS-ARTM-PCB               PIC X.                                   
066500 01  RANS-ARTS-PCB               PIC X.                                   
066600                                                                          
066700 01  TPO1-ORDP-PCB               PIC X.                                   
066800 01  TPO1-ARTM-PCB               PIC X.                                   
066900 01  TPO1-ZZAC-PCB               PIC X.                                   
067000 01  TPO2-ORDP-PCB               PIC X.                                   
067100 01  TPO2-XXBU-PCB               PIC X.                                   
067200 01  TPO2-XXBV-PCB               PIC X.                                   
067300 01  TPO2-ARTM-PCB               PIC X.                                   
067400 01  TPO2-FILA-PCB               PIC X.                                   
067500 01  TPO2-XXBX-PCB               PIC X.                                   
067600 01  RELS-ORDP-PCB               PIC X.                                   
067700 01  RELS-FILA-PCB               PIC X.                                   
067800 01  RELS-ARTM-PCB               PIC X.                                   
067900 01  TIME-4437-PCB               PIC X.                                   
068000 01  AVSR-ORQI-PCB               PIC X.                                   
068100 01  AVSR-GMTB-PCB               PIC X.                                   
068200 01  AVSR-GMTC-PCB               PIC X.                                   
068300 01  AVSR-WDB2-PCB               PIC X.                                   
068400 01  AVSR-WDB6-PCB               PIC X.                                   
068500 01  TRAN-XXKB-PCB               PIC X.                                   
068600 01  KVAN-WDB2-PCB               PIC X.                                   
068700 01  KVAN-WDC1-PCB               PIC X.                                   
068800 01  XDCA-USEA-PCB               PIC X.                                   
068900 01  XDCA-WDB6-PCB               PIC X.                                   
069000 01  XDCA-WDK6-PCB               PIC X.                                   
069100 01  XDCA-WDK7-PCB               PIC X.                                   
069200 01  XDCA-WDK9-PCB               PIC X.                                   
069300 01  XDCA-WDL6-PCB               PIC X.                                   
069400 01  XDCA-WDQ4B-PCB              PIC X.                                   
069500 01  XDCA-WDQ2-PCB               PIC X.                                   
069600 01  XDCA-WDQ4-PCB               PIC X.                                   
069700 01  XDCA-WDR6-PCB               PIC X.                                   
069800 01  XDCA-WDB6-2-PCB             PIC X.                                   
069900 01  XDCA-WDK6-2-PCB             PIC X.                                   
070000 01  XDCA-WDK7-2-PCB             PIC X.                                   
070100 01  XDCA-WDK7-3-PCB             PIC X.                                   
070200                                                                          
070300                                                                          
070400 PROCEDURE DIVISION  USING                                                
070500        MSG-PCB        4213-PCB       4243-PCB                            
070600        AVSR-LIMS-PCB  2109-PCB       PRQRY-PCB     USEA-PCB              
070700        WDQ1-PCB       WDQ2-PCB       WDQ2-UPD-PCB  WDQ4-PCB              
070800        WDK9-PCB       WDB2-PCB       WDB1-PCB      WDB6-PCB              
070900        WDR6-PCB                                                          
071000        PRIS-ARTC-PCB  PRIS-WDK7-PCB                                      
071100        PRIS-GMTA-PCB  PRIS-BETA-PCB                                      
071200        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
071300        PRIS-COST-WDK6-PCB                                                
071400        PRIS-COST-WDK7-PCB                                                
071500        PRIS-COST-WDF1-PCB                                                
071600        PRIS-COST-9305-PCB                                                
071700        PRIS-COST-WDK72-PCB                                               
071800        PRIS-COST-WDB6-PCB                                                
071900        PRNO-3107-PCB                                                     
072000        PRQU-WDG2-PCB                                                     
072100        PRQU-WDC7-PCB                                                     
072200        PRQU-SJKO-WDK6-PCB                                                
072300        AREG-WDK6-PCB                                                     
072400        AREG-WDK7-PCB                                                     
072500        ARTM-ARTM-PCB                                                     
072600        DLEV-LEVF-PCB                                                     
072700        DLEV-LEVG-PCB                                                     
072800        DLEV-LEVA-PCB                                                     
072900        DLEV-ARTS-PCB                                                     
073000        DLEV-WDB6-PCB                                                     
073100        SPAR-WDF8-PCB                                                     
073200        SPAR-WDF8A-PCB                                                    
073300        SPAR-WDK6-PCB                                                     
073400        DNOT-ORQP-PCB                                                     
073500        DNOT-ORQP2-PCB                                                    
073600        DNOT-ORQP3-PCB                                                    
073700        DNOT-4013-PCB                                                     
073800        DNOT-BENA-PCB                                                     
073900        KAMP-ORDP-PCB KAMP-ZZAC-PCB  KAMP-WDM2-PCB                        
074000        KERS-ARTC-PCB  KERS-ERSA-PCB                                      
074100        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
074200        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
074300        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
074400        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
074500        SDCA-WDK7-3-PCB                                                   
074600        CDCA-ARTM-PCB  CDCA-INLB-PCB  CDCA-WDB2-PCB  CDCA-WDC1-PCB        
074700        RANS-XXKM-PCB  RANS-ARTM-PCB  RANS-ARTS-PCB                       
074800        TPO1-ORDP-PCB  TPO1-ARTM-PCB  TPO1-ZZAC-PCB                       
074900        TPO2-ORDP-PCB  TPO2-XXBU-PCB  TPO2-XXBV-PCB                       
075000        TPO2-ARTM-PCB  TPO2-FILA-PCB  TPO2-XXBX-PCB                       
075100        RELS-ORDP-PCB  RELS-FILA-PCB  RELS-ARTM-PCB                       
075200        TIME-4437-PCB                                                     
075300        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
075400        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
075500        TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                         
075600        XDCA-USEA-PCB                                                     
075700        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
075800        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
075900        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
076000        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
076100        XDCA-WDK7-3-PCB.                                                  
076200                                                                          
076300                                                                          
076400     ENTRY 'DLITCBL' USING                                                
076500        MSG-PCB        4213-PCB       4243-PCB                            
076600        AVSR-LIMS-PCB  2109-PCB       PRQRY-PCB     USEA-PCB              
076700        WDQ1-PCB       WDQ2-PCB       WDQ2-UPD-PCB  WDQ4-PCB              
076800        WDK9-PCB       WDB2-PCB       WDB1-PCB      WDB6-PCB              
076900        WDR6-PCB                                                          
077000        PRIS-ARTC-PCB  PRIS-WDK7-PCB                                      
077100        PRIS-GMTA-PCB  PRIS-BETA-PCB                                      
077200        PRIS-GPRIA-PCB PRIS-GPRIB-PCB                                     
077300        PRIS-COST-WDK6-PCB                                                
077400        PRIS-COST-WDK7-PCB                                                
077500        PRIS-COST-WDF1-PCB                                                
077600        PRIS-COST-9305-PCB                                                
077700        PRIS-COST-WDK72-PCB                                               
077800        PRIS-COST-WDB6-PCB                                                
077900        PRNO-3107-PCB                                                     
078000        PRQU-WDG2-PCB                                                     
078100        PRQU-WDC7-PCB                                                     
078200        PRQU-SJKO-WDK6-PCB                                                
078300        AREG-WDK6-PCB                                                     
078400        AREG-WDK7-PCB                                                     
078500        ARTM-ARTM-PCB                                                     
078600        DLEV-LEVF-PCB                                                     
078700        DLEV-LEVG-PCB                                                     
078800        DLEV-LEVA-PCB                                                     
078900        DLEV-ARTS-PCB                                                     
079000        DLEV-WDB6-PCB                                                     
079100        SPAR-WDF8-PCB                                                     
079200        SPAR-WDF8A-PCB                                                    
079300        SPAR-WDK6-PCB                                                     
079400        DNOT-ORQP-PCB                                                     
079500        DNOT-ORQP2-PCB                                                    
079600        DNOT-ORQP3-PCB                                                    
079700        DNOT-4013-PCB                                                     
079800        DNOT-BENA-PCB                                                     
079900        KAMP-ORDP-PCB KAMP-ZZAC-PCB  KAMP-WDM2-PCB                        
080000        KERS-ARTC-PCB  KERS-ERSA-PCB                                      
080100        NDCA-USEA-PCB NDCA-WDK7-PCB NDCA-WDL6-PCB NDCA-WDB6-PCB           
080200        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
080300        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
080400        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
080500        SDCA-WDK7-3-PCB                                                   
080600        CDCA-ARTM-PCB  CDCA-INLB-PCB  CDCA-WDB2-PCB  CDCA-WDC1-PCB        
080700        RANS-XXKM-PCB  RANS-ARTM-PCB  RANS-ARTS-PCB                       
080800        TPO1-ORDP-PCB  TPO1-ARTM-PCB  TPO1-ZZAC-PCB                       
080900        TPO2-ORDP-PCB  TPO2-XXBU-PCB  TPO2-XXBV-PCB                       
081000        TPO2-ARTM-PCB  TPO2-FILA-PCB  TPO2-XXBX-PCB                       
081100        RELS-ORDP-PCB  RELS-FILA-PCB  RELS-ARTM-PCB                       
081200        TIME-4437-PCB                                                     
081300        AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                         
081400        AVSR-WDB2-PCB AVSR-WDB6-PCB                                       
081500        TRAN-XXKB-PCB KVAN-WDB2-PCB KVAN-WDC1-PCB                         
081600        XDCA-USEA-PCB                                                     
081700        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
081800        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
081900        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
082000        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
082100        XDCA-WDK7-3-PCB.                                                  
082200                                                                          
082300 MAIN SECTION.                                                            
082400     PERFORM IMS-GET-MSG                                                  
082500     IF SEGMENT-FINNS                                                     
082600        PERFORM A-INIT                                                    
082700        IF ALLT-OK                                                        
082800          PERFORM B-KOLLA-NYCKLAR                                         
082900        END-IF                                                            
083000        IF ALLT-OK                                                        
083100           PERFORM C-GET-ORDER                                            
083200           IF ALLT-OK                                                     
083300              PERFORM D-FORMELL-KONTROLL                                  
083400              IF ALLT-OK                                                  
083500                 PERFORM E-BEHANDLA-RADER                                 
083600              END-IF                                                      
083700           END-IF                                                         
083800        END-IF                                                            
083900        IF ALLT-OK                                                        
084000           IF MID-IDARTNR(WS-INDEX-MID-MAX) =                             
084100              ALL '+' OR SVARSBILD                                        
084200              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
084300                PERFORM I-SKICKA-PRISFRAGA                                
084400              END-IF                                                      
084500              PERFORM F-HOPPA-TILL-SVARSBILD                              
084600           ELSE                                                           
084700              IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0             
084800                PERFORM I-SKICKA-PRISFRAGA                                
084900              END-IF                                                      
085000              PERFORM G-VISA-TOM-SIDA                                     
085100           END-IF                                                         
085200        END-IF                                                            
085300        IF HOPP-TILL-4213-4243 = NEJ                                      
085400        AND HOPP-TILL-0504     = NEJ                                      
085500          PERFORM Z-FINIT-INSERT-MSG                                      
085600        END-IF                                                            
085700     END-IF                                                               
085800     MOVE +0 TO RETURN-CODE                                               
085900     GOBACK                                                               
086000     .                                                                    
086100                                                                          
086200 A-INIT SECTION.                                                          
086300     MOVE 'A-INIT          '   TO CURRENT-SECTION                         
086400                                                                          
086500     MOVE JA                   TO ALLT-SW                                 
086600     MOVE SPACE                TO MED-IDMFSFEL                            
086700     IF MSG-DUBBLA-TRANSKODER                                             
086800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I20601                 
086900       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
087000       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
087100     ELSE                                                                 
087200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I20601                  
087300       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
087400       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
087500     END-IF                                                               
087600                                                                          
087700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
087800     MOVE MSG-IDPFK            TO MFS-IDPFK                               
087900     MOVE MFS-IDTRANS          TO W-IDTRANS                               
088000     IF (W-IDTRANS = '4213' AND MSG-KDTRANS-1  = 'W4T206U ')              
088100     OR (W-IDTRANS = '4243' AND MSG-KDTRANS-1  = 'W4T206U ')              
088200        MOVE JA TO SVARSBILD-SW                                           
088300     END-IF                                                               
088400     MOVE LOW-VALUE            TO MSG-AREA                                
088500     MOVE 'W4O206N1'           TO MFS-IDMOD                               
088600     MOVE '4206'               TO MOD-IDTRANS                             
088700     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
088800                                                                          
088900     IF NOT EGEN-MID AND NOT SVARSBILD                                    
089000        MOVE SPACE              TO MFS-KDTRTYP                            
089100        MOVE '7'                TO MFS-IDPFK                              
089200     END-IF                                                               
089300                                                                          
089400     IF ENGLISH-TEXT                                                      
089500        MOVE 'GB '              TO MED-IDSKYLT                            
089600     ELSE                                                                 
089700        MOVE 'S  '              TO MED-IDSKYLT                            
089800     END-IF                                                               
089900                                                                          
090000     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O20601 + 4                  
090100     PERFORM AA-NOLLA-WOPS-TABELL                                         
090200                                                                          
090300     IF NOT EGEN-MID                                                      
090400       MOVE NEJ  TO ALLT-SW                                               
090500*                   INDATA-SW                                             
090600       PERFORM S10-WRONG-PICTURE-MESSAGE                                  
090700     END-IF                                                               
090800                                                                          
090900     IF ALLT-OK                                                           
091000        MOVE ALL '+'           TO MSGI-WMSGINIT                           
091100        MOVE '001'             TO MSGI-KDCALL                             
091200        MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                       
091300        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
091400        MOVE '4206'            TO MSGI-IDTRANS                            
091500        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
091600        MOVE MSGI-SPAR-AREA    TO SPAR-AREA                               
091700        MOVE SPAR-IDTRANS       TO WS-IDTRANS-SPAR                        
091800                                                                          
091900        MOVE ALL '+'            TO MSGI-WMSGINIT                          
092000        MOVE '001'              TO MSGI-KDCALL                            
092100        MOVE 'WIDDC   '         TO MSGI-IDUSER                            
092200        MOVE SPAR-IDDC-MSGI     TO MSGI-IDUSER(6:2)                       
092300        MOVE '4206'             TO MSGI-IDTRANS                           
092400        MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                      
092500        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
092600     END-IF                                                               
092700     .                                                                    
092800                                                                          
092900 AA-NOLLA-WOPS-TABELL SECTION.                                            
093000     MOVE 'AA-NOLLA-WOPS   '   TO CURRENT-SECTION                         
093100                                                                          
093200     MOVE +1                   TO WS-INDEX-WOPS                           
093300     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
093400        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
093500        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
093600        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
093700        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
093800        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
093900        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
094000        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
094100        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
094200        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
094300        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
094400        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
094500        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
094600        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
094700        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
094800        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
094900        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
095000                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
095100        ADD +1                 TO WS-INDEX-WOPS                           
095200     END-PERFORM                                                          
095300                                                                          
095400     MOVE +1                   TO WS-INDEX-WOPS                           
095500     .                                                                    
095600                                                                          
095700 B-KOLLA-NYCKLAR SECTION.                                                 
095800     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
095900                                                                          
096000     MOVE MID-IDDISTR             TO TEST-IDDISTR                         
096100     IF DIST79-DEALER-PRICE                                               
096200        MOVE 'DEALERPRICE'        TO MOD-TEDDI                            
096300     ELSE                                                                 
096400        MOVE SPACE                TO MOD-TEDDI                            
096500     END-IF                                                               
096600                                                                          
096700     MOVE MID-IDDISTR           TO MOD-IDDISTR                            
096800     INSPECT MOD-IDDISTR  REPLACING LEADING ZERO BY SPACE                 
096900                                                                          
097000     MOVE MID-IDKUNDNR          TO MOD-IDKUNDNR                           
097100     INSPECT MOD-IDKUNDNR REPLACING LEADING ZERO BY SPACE                 
097200                                                                          
097300     MOVE MID-IDORDNR5          TO MOD-IDORDNR5                           
097400     INSPECT MOD-IDORDNR5 REPLACING LEADING ZERO BY SPACE                 
097500                                                                          
097600     MOVE MID-KDORDKL           TO MOD-KDORDKL                            
097700     MOVE MID-KDFRAKT           TO MOD-KDFRAKT                            
097800     MOVE SPACE                 TO MOD-KDVALISO                           
097900                                                                          
098000     .                                                                    
098100                                                                          
098200 C-GET-ORDER        SECTION.                                              
098300     MOVE 'C-GET-ORDER     '         TO CURRENT-SECTION                   
098400                                                                          
098500     MOVE MID-IDDISTR           TO W-IDDISTR                              
098600     MOVE MID-IDKUNDNR          TO W-IDKUNDNR                             
098700     MOVE MID-IDORDNR5          TO WS-IDORDNR5                            
098800     INSPECT WS-IDORDNR5-X REPLACING LEADING SPACE BY ZERO                
098900     MOVE WS-IDKUNDRF           TO W-IDKUNDRF                             
099000     PERFORM IMS-01-GU-WDQ201                                             
099100                                                                          
099200     PERFORM CA-KDORDSTA-VALIDATE                                         
099300     PERFORM CB-HAMTA-KUND                                                
099400     PERFORM CC-HAMTA-WDB6-INFO                                           
099500                                                                          
099600     IF SW-KDORDSTA-O-ALL-SPACE-FLAG = NEJ                                
099700                                                                          
099800        IF SW-KDORDSTA-O-STATUS-FLAG = JA                                 
099900                                                                          
100000           MOVE OHUV-IDDC-PRIM        TO W-IDDC-WDQ212                    
100100           PERFORM IMS-02-GNP-WDQ212                                      
100200        ELSE                                                              
100300           MOVE NEJ             TO ALLT-SW                                
100400           MOVE ERR-EJ-TILLAEGG TO MED-IDMFSFEL                           
100500        END-IF                                                            
100600     ELSE                                                                 
100700        IF SW-KDORDSTA-STATUS-FLAG = JA                                   
100800                                                                          
100900           MOVE OHUV-IDDC-PRIM        TO W-IDDC-WDQ212                    
101000           PERFORM IMS-02-GNP-WDQ212                                      
101100        ELSE                                                              
101200           MOVE NEJ             TO ALLT-SW                                
101300           MOVE ERR-EJ-TILLAEGG TO MED-IDMFSFEL                           
101400        END-IF                                                            
101500     END-IF                                                               
101600     .                                                                    
101700                                                                          
101800 CA-KDORDSTA-VALIDATE  SECTION.                                           
101900                                                                          
102000                                                                          
102100     MOVE JA TO SW-KDORDSTA-ALL-E-FLAG                                    
102200     MOVE JA TO SW-KDORDSTA-O-ALL-SPACE-FLAG                              
102300     MOVE JA TO SW-KDORDSTA-O-STATUS-FLAG                                 
102400     MOVE JA TO SW-KDORDSTA-STATUS-FLAG                                   
102500                                                                          
102600     PERFORM IMS-GNP-WDQ212-OKVAL                                         
102700                                                                          
102800     PERFORM UNTIL SEGMENT-SAKNAS                                         
102900        IF ARB-KDORDSTA NOT = 'E'                                         
103000           MOVE NEJ TO SW-KDORDSTA-ALL-E-FLAG                             
103100        END-IF                                                            
103200        IF ARB-KDORDSTA-O NOT = SPACE                                     
103300           MOVE NEJ TO SW-KDORDSTA-O-ALL-SPACE-FLAG                       
103400        END-IF                                                            
103500        IF ARB-KDORDSTA-O NOT = ' ' AND                                   
103600           ARB-KDORDSTA-O NOT = 'B' AND                                   
103700           ARB-KDORDSTA-O NOT = 'C' AND                                   
103800           ARB-KDORDSTA-O NOT = 'R'                                       
103900           MOVE NEJ TO SW-KDORDSTA-O-STATUS-FLAG                          
104000        END-IF                                                            
104100        IF ARB-KDORDSTA   NOT = ' ' AND                                   
104200           ARB-KDORDSTA   NOT = 'B' AND                                   
104300           ARB-KDORDSTA   NOT = 'C' AND                                   
104400           ARB-KDORDSTA   NOT = 'R'                                       
104500           MOVE NEJ TO SW-KDORDSTA-STATUS-FLAG                            
104600        END-IF                                                            
104700                                                                          
104800        PERFORM IMS-GNP-WDQ212-OKVAL                                      
104900     END-PERFORM                                                          
105000     .                                                                    
105100     EJECT                                                                
105200 CB-HAMTA-KUND      SECTION.                                              
105300     MOVE 'CB-HAMTA-KUND   ' TO CURRENT-SECTION                           
105400                                                                          
105500     MOVE MID-IDDISTR               TO W-IDDISTR-WDB2                     
105600     MOVE MID-IDKUNDNR              TO W-IDKUNDNR-WDB2                    
105700     PERFORM IMS-07-GU-WDB201                                             
105800                                                                          
105900     IF OHUV-KDORDKL > 1                                                  
106000                                                                          
106100        MOVE +1 TO WS-INDEX                                               
106200        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
106300           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
106400                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
106500           ADD +1 TO WS-INDEX                                             
106600        END-PERFORM                                                       
106700                                                                          
106800     ELSE                                                                 
106900       IF OHUV-KDORDKL = 1                                                
107000                                                                          
107100          MOVE +1 TO WS-INDEX                                             
107200          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
107300             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
107400                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
107500             ADD +1 TO WS-INDEX                                           
107600          END-PERFORM                                                     
107700                                                                          
107800       ELSE                                                               
107900         IF OHUV-KDORDKL = 0                                              
108000                                                                          
108100            MOVE +1 TO WS-INDEX                                           
108200            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
108300               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
108400                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
108500               ADD +1 TO WS-INDEX                                         
108600            END-PERFORM                                                   
108700                                                                          
108800         END-IF                                                           
108900       END-IF                                                             
109000     END-IF                                                               
109100     .                                                                    
109200     EJECT                                                                
109300                                                                          
109400 CC-HAMTA-WDB6-INFO SECTION.                                              
109500                                                                          
109600     MOVE SPACE                TO CLDC-W411CLDC                           
109700     MOVE W-GMT-IDDC-CLEAR-GRP TO CLDC-IDDC-CLEAR-GRP                     
109800                                                                          
109900     CALL W411CLDC USING CLDC-W411CLDC WDB6-PCB                           
110000     .                                                                    
110100     EJECT                                                                
110200 D-FORMELL-KONTROLL SECTION.                                              
110300     MOVE 'D-FORMELL-KONTRO'         TO CURRENT-SECTION                   
110400                                                                          
110500     MOVE 'IMS '                     TO ORFK-IDSYSTEM                     
110600     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
110700     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
110800     MOVE OHUV-IDUSER                TO ORFK-IDUSER                       
110900     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
111000     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
111100     MOVE OHUV-KDTPOTYP              TO ORFK-KDTPOTYP                     
111200     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
111300     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
111400     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
111500     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
111600     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
111700     MOVE +1                   TO WS-INDEX-MID                            
111800     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
111900        MOVE NEJ               TO ORFK-FLINVEST(WS-INDEX-MID)             
112000        IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                            
112100           MOVE OHUV-FLRESTN   TO ORFK-FLRESTN(WS-INDEX-MID)              
112200        ELSE                                                              
112300           IF MID-FLRESTN(WS-INDEX-MID) = 'Y'                             
112400              MOVE JA          TO MID-FLRESTN(WS-INDEX-MID)               
112500           END-IF                                                         
112600           MOVE MID-FLRESTN(WS-INDEX-MID)                                 
112700                               TO ORFK-FLRESTN(WS-INDEX-MID)              
112800                                MID-FLRESTN(WS-INDEX-MID)                 
112900        END-IF                                                            
113000        IF MID-FLSLATT(WS-INDEX-MID) = ALL '+'                            
113100           MOVE JA             TO ORFK-FLSLATT(WS-INDEX-MID)              
113200        ELSE                                                              
113300           IF MID-FLSLATT(WS-INDEX-MID) = 'Y'                             
113400              MOVE JA          TO MID-FLSLATT(WS-INDEX-MID)               
113500           END-IF                                                         
113600           MOVE MID-FLSLATT(WS-INDEX-MID)                                 
113700                               TO ORFK-FLSLATT(WS-INDEX-MID)              
113800        END-IF                                                            
113900                                                                          
114000        MOVE MID-IDARTNR(WS-INDEX-MID)                                    
114100                               TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
114200                                                                          
114300        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
114400                               TO ORFK-KDKVBRYT(WS-INDEX-MID)             
114500                                                                          
114600        IF MID-KDVRINFO(WS-INDEX-MID) = ALL '+'                           
114700           MOVE OHUV-KDVRINFO  TO ORFK-KDVRINFO(WS-INDEX-MID)             
114800        ELSE                                                              
114900           MOVE MID-KDVRINFO(WS-INDEX-MID)                                
115000                               TO ORFK-KDVRINFO(WS-INDEX-MID)             
115100        END-IF                                                            
115200                                                                          
115300        MOVE MID-KVBEART(WS-INDEX-MID)                                    
115400                               TO ORFK-KVBEART(WS-INDEX-MID)              
115500                                                                          
115600        IF DIST79-DEALER-PRICE OR                                         
115800           DIST79-ECOM-PRICE                                              
115900           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
116000                               TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
116100           MOVE ALL '+'        TO ORFK-PRARTNTO(WS-INDEX-MID)             
116200           MOVE ALL '+'        TO                                         
116300                          ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)             
116400        ELSE                                                              
116500           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
116600                               TO ORFK-PRARTNTO(WS-INDEX-MID)             
116700           MOVE ALL '+'        TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
116800           MOVE ALL '+'        TO                                         
116900                          ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)             
117000        END-IF                                                            
117100                                                                          
117200        MOVE MID-TITPO(WS-INDEX-MID)                                      
117300                               TO ORFK-TITPO-RAD(WS-INDEX-MID)            
117400        MOVE ALL '+'           TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)         
117500        ADD +1                 TO WS-INDEX-MID                            
117600     END-PERFORM                                                          
117700                                                                          
117800     CALL W411ORFK USING ORFK-W411ORFK                                    
117900                         AREG-WDK6-PCB                                    
118000                         AREG-WDK7-PCB                                    
118100                                                                          
118200     MOVE +1                   TO WS-INDEX-MID                            
118300     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
118400        PERFORM DA-KOLLA-FEL-FK                                           
118500        ADD +1                 TO WS-INDEX-MID                            
118600     END-PERFORM                                                          
118700                                                                          
118800     IF SVARSBILD AND NOT ALLT-OK                                         
118900        MOVE 'FORMELLA FEL FÅR EJ INTRÄFFA VID START FRÅN 4213'           
119000                               TO FELTEXT                                 
119100        CALL ABEND USING RKOD-ABEND                                       
119200     END-IF                                                               
119300     .                                                                    
119400                                                                          
119500 DA-KOLLA-FEL-FK SECTION.                                                 
119600     MOVE 'DA-KOLLA-FEL-FK '    TO CURRENT-SECTION                        
119700                                                                          
119800     IF ORFK-FLINVEST-OK(WS-INDEX-MID) = NEJ                              
119900        MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                           
120000        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLINVEST-ATTR(WS-INDEX-MID)        
120100        MOVE NEJ                TO ALLT-SW                                
120200     END-IF                                                               
120300                                                                          
120400     IF ORFK-FLRESTN-OK(WS-INDEX-MID) = NEJ                               
120500        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
120600        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLRESTN-ATTR(WS-INDEX-MID)        
120700        MOVE NEJ                 TO ALLT-SW                               
120800     END-IF                                                               
120900                                                                          
121000     IF ORFK-FLSLATT-OK(WS-INDEX-MID) = NEJ                               
121100        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
121200        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLSLATT-ATTR(WS-INDEX-MID)        
121300        MOVE NEJ                 TO ALLT-SW                               
121400     END-IF                                                               
121500                                                                          
121600     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
121700        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
121800        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
121900        MOVE NEJ                 TO ALLT-SW                               
122000     ELSE                                                                 
122100        IF (ORFK-KDORDBEK(WS-INDEX-MID) = 58 OR 59)                       
122200                  AND NOT MFS-UPDATE                                      
122300          MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                         
122400          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
122500          MOVE NEJ               TO ALLT-SW                               
122600        END-IF                                                            
122700     END-IF                                                               
122800                                                                          
122900     IF ORFK-KDKVBRYT-OK(WS-INDEX-MID) = NEJ                              
123000        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
123100        MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVBRYT-ATTR(WS-INDEX-MID)         
123200        MOVE NEJ                 TO ALLT-SW                               
123300     END-IF                                                               
123400                                                                          
123500     IF ORFK-KDVRINFO-OK(WS-INDEX-MID) = NEJ                              
123600        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
123700        MOVE MFS-NUM-FAELT-FEL TO MOD-KDVRINFO-ATTR(WS-INDEX-MID)         
123800        MOVE NEJ                 TO ALLT-SW                               
123900     END-IF                                                               
124000     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
124100        IF MED-IDMFSFEL = SPACE                                           
124200           IF ORFK-KVBEART(WS-INDEX-MID) NUMERIC  AND                     
124300                    ORFK-KVBEART(WS-INDEX-MID) > ZERO                     
124400              IF NOT MFS-UPDATE                                           
124500                 MOVE ERR-STORT-UTTAG TO MED-IDMFSFEL                     
124600                 MOVE MFS-NUM-FAELT-FEL   TO                              
124700                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
124800                 MOVE NEJ             TO ALLT-SW                          
124900              END-IF                                                      
125000           ELSE                                                           
125100            MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                         
125200            MOVE MFS-NUM-FAELT-FEL   TO                                   
125300                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
125400            MOVE NEJ                 TO ALLT-SW                           
125500           END-IF                                                         
125600        ELSE                                                              
125700           MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                          
125800           MOVE MFS-NUM-FAELT-FEL   TO                                    
125900                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
126000           MOVE NEJ                 TO ALLT-SW                            
126100        END-IF                                                            
126200     END-IF                                                               
126300                                                                          
126400     IF ORFK-PRARTNTO-OK(WS-INDEX-MID) = NEJ                              
126500        MOVE ERR-UPPLYSTA-FEL  TO MED-IDMFSFEL                            
126600        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
126700        MOVE NEJ               TO ALLT-SW                                 
126800     END-IF                                                               
126900                                                                          
127000     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-MID) = NEJ                          
127100        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
127200        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
127300        MOVE NEJ                 TO ALLT-SW                               
127400     END-IF                                                               
127500                                                                          
127600     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-MID) = NEJ                      
127700        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
127800        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
127900        MOVE NEJ                 TO ALLT-SW                               
128000     END-IF                                                               
128100                                                                          
128200     IF ORFK-TITPO-OK(WS-INDEX-MID) = NEJ                                 
128300        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
128400        MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-ATTR(WS-INDEX-MID)          
128500        MOVE NEJ                 TO ALLT-SW                               
128600     END-IF                                                               
128700     .                                                                    
128800                                                                          
128900 E-BEHANDLA-RADER SECTION.                                                
129000     MOVE 'E-BEHANDLA-RADER'    TO CURRENT-SECTION                        
129100                                                                          
129200     MOVE +1 TO WS-INDEX-MID                                              
129300     MOVE NEJ                     TO TILLK-SW                             
129400                                     OBKR-SW                              
129500                                     CDC-MOVE-SW                          
129600     MOVE +0                      TO WS-IDPRQUES                          
129700     MOVE JA                      TO FIRST-TIME-SW                        
129800     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
129900        IF MID-IDARTNR(WS-INDEX-MID) NOT = ALL '+'                        
130000           IF FIRST-TIME                                                  
130100                                                                          
130200              MOVE OHUV-IDORDER     TO W-IDORDER                          
130300              PERFORM IMS-10-GHU-WDQ201                                   
130400              MOVE NEJ              TO OHUV-FLKLAR                        
130500              MOVE WS-IDTRANS-SPAR  TO OHUV-IDSYSTEM                      
130600              PERFORM IMS-11-REPL-WDQ201                                  
130700              IF SW-KDORDSTA-O-ALL-SPACE-FLAG = JA                        
130800                                                                          
130900                 PERFORM IMS-GHNP-WDQ212                                  
131000                                                                          
131100                 PERFORM UNTIL SEGMENT-SAKNAS                             
131200                                                                          
131300                    MOVE ARB-KDORDSTA     TO ARB-KDORDSTA-O               
131400                    MOVE 'E '             TO ARB-KDORDSTA                 
131500                    PERFORM IMS-REPL-WDQ212                               
131600                                                                          
131700                    PERFORM IMS-GHNP-WDQ212                               
131800                 END-PERFORM                                              
131900              END-IF                                                      
132000                                                                          
132100              MOVE NEJ              TO FIRST-TIME-SW                      
132200           END-IF                                                         
132300           PERFORM S02-RENSA-TILLK-TAB                                    
132400           MOVE ORFK-W411AREG-001(WS-INDEX-MID)                           
132500                                  TO AREG-W411AREG-001                    
132600           PERFORM EC-BEHANDLA-RAD                                        
132700                                                                          
132800       PERFORM S20-HAMTA-WDB6-INFO                                        
132900                                                                          
133000           IF DCS-NDC-NA                                                  
133100              PERFORM S03-DATA-TILL-DEL-NOTE                              
133200           END-IF                                                         
133300           MOVE JA                TO TILLK-SW                             
133400           MOVE NEJ               TO CDC-MOVE-SW                          
133500           MOVE +1                TO WS-INDEX-TILLK                       
133600           PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR           
133700              TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                         
133800              IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                      
133900                 PERFORM ED-LAES-TILLK-DATA                               
134000                 PERFORM EC-BEHANDLA-RAD                                  
134100                    IF DCS-NDC-NA                                         
134200                       PERFORM S03-DATA-TILL-DEL-NOTE                     
134300                    END-IF                                                
134400              END-IF                                                      
134500              ADD +1              TO WS-INDEX-TILLK                       
134600           END-PERFORM                                                    
134700        END-IF                                                            
134800        MOVE NEJ                  TO TILLK-SW                             
134900                                     OBKR-SW                              
135000                                     CDC-MOVE-SW                          
135100        ADD +1 TO WS-INDEX-MID                                            
135200     END-PERFORM                                                          
135300                                                                          
135400     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
135500       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
135600       MOVE +3                      TO PRNO-KDCALL                        
135700                                                                          
135800       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
135900     END-IF                                                               
136000                                                                          
136100     IF AVSR-IDDC(1) NOT = SPACE                                          
136200        CALL W413AVSR USING AVSR-W413AVSR AVSR-LIMS-PCB                   
136300              AVSR-ORQI-PCB AVSR-GMTB-PCB AVSR-GMTC-PCB                   
136400              AVSR-WDB2-PCB AVSR-WDB6-PCB                                 
136500              TRAN-XXKB-PCB                                               
136600     END-IF                                                               
136700     MOVE JA                      TO ALLT-SW                              
136800     .                                                                    
136900                                                                          
137000                                                                          
137100 EC-BEHANDLA-RAD SECTION.                                                 
137200     MOVE 'EC-BEHANDLA-RAD '    TO CURRENT-SECTION                        
137300                                                                          
137400                                                                          
137500     PERFORM ECA-NOLLSTALL-OBKR                                           
137600     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
137700     PERFORM ECC-LAS-NYA-ARTIKELREG                                       
137800     PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                                 
137900     PERFORM ECI-KOMPLETTERA-DIREKTLEVERANS                               
138000                                                                          
138100     IF NOT TILLKOMMANDE-RAD                                              
138200        PERFORM ECK-KOMPLETTERA-ERSATTNING                                
138300     END-IF                                                               
138400                                                                          
138500     PERFORM ECL-KOMPLETTERA-SPARRAR                                      
138600     PERFORM ECJ-KOMPLETTERA-PRIS                                         
138700     PERFORM ECM-KOMPLETTERA-TPO1                                         
138800     PERFORM ECN-KOMPLETTERA-TPO2                                         
138900     PERFORM ECO-KOMPLETTERA-KAMPANJER                                    
139000     PERFORM ECT-KOMPLETTERA-RELEASESPARR                                 
139100     IF KOLLA-ERS AND (DCS-CDC OR DCS-SDC)                                
139200        PERFORM ECZ-CHECK-KDERS-IN-DC                                     
139300     END-IF                                                               
139400     PERFORM ECW-PREL-AVBOKNING-SDC1                                      
139500     PERFORM ECH-PREL-AVBOKNING-SDC2                                      
139600     PERFORM ECX-PREL-AVBOKNING-SDC3                                      
139700     PERFORM ECG-PREL-AVBOKNING-XDC                                       
139800     PERFORM ECP-KOMPLETTERA-RANSONERING                                  
139900     PERFORM ECQ-KOMPLETTERA-STORA-UTTAG                                  
140000                                                                          
140100     PERFORM ECR-PREL-AVBOKNING-CDC                                       
140200                                                                          
140300     IF NOT TILLKOMMANDE-RAD                                              
140400        IF SKRIV-OBKR                                                     
140500           PERFORM ECS-SKRIV-OBKR                                         
140600           IF NOT OBKR-SKRIVEN OR                                         
140700              EGET-CL-RAD                                                 
140800              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
140900           END-IF                                                         
141000        ELSE                                                              
141100           IF TPO1-FLKLAR = JA OR TPO2-FLKLAR = JA                        
141200                               OR KAMP-FLKLAR = JA                        
141300                               OR RELS-FLKLAR = JA                        
141400              CONTINUE                                                    
141500           ELSE                                                           
141600              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
141700           END-IF                                                         
141800        END-IF                                                            
141900     ELSE                                                                 
142000        IF SKRIV-OBKR                                                     
142100           PERFORM ECS-SKRIV-OBKR                                         
142200        END-IF                                                            
142300     END-IF                                                               
142400     .                                                                    
142500                                                                          
142600 ECA-NOLLSTALL-OBKR SECTION.                                              
142700     MOVE 'ECA-NOLLSTALL-OB'   TO CURRENT-SECTION                         
142800                                                                          
142900     MOVE +0                   TO KVAN-KDORDBEK-UT                        
143000     MOVE +0                   TO KVAN-KVQPACK-UT                         
143100     MOVE +0                   TO DLEV-KDORDBEK-UT                        
143200     MOVE +0                   TO KERS-KDERS                              
143300     IF NOT TILLKOMMANDE-RAD                                              
143400        MOVE +0                TO KERS-KDORDBEK                           
143500     ELSE                                                                 
143600        MOVE JA                TO OBKR-SW                                 
143700     END-IF                                                               
143800     MOVE +0                   TO TPO1-KDORDBEK                           
143900     MOVE +0                   TO TPO2-KDORDBEK                           
144000     MOVE +0                   TO RELS-KDORDBEK                           
144100     MOVE +0                   TO KAMP-KDORDBEK                           
144200     MOVE +0                   TO STOR-KDORDBEK                           
144300     MOVE +0                   TO XDCA-KDORDBEK                           
144400*    MOVE +0                   TO NDCA-KDORDBEK                           
144500     MOVE +0                   TO SDCA-KDORDBEK                           
144600     MOVE +0                   TO SDCA-KDORDBEK-FIRST-SDC                 
144700     MOVE +0                   TO SDCA-KDORDBEK-SECOND-SDC                
144800     MOVE +0                   TO CDCA-KDORDBEK-UT                        
144900     MOVE +1                   TO WS-INDEX                                
145000     MOVE ZERO                 TO SPAR-KDORDBEK                           
145100                                                                          
145200     MOVE JA                   TO ALLT-SW                                 
145300     MOVE NEJ                  TO EGET-CL-RAD-SW                          
145400                                                                          
145500     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
145600       MOVE ORFK-KDORDBEK(WS-INDEX-MID) TO W-KDORDBEK                     
145700       MOVE +7                          TO W-KDTPOTYP                     
145800     ELSE                                                                 
145900       IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                 
146000         MOVE NEJ               TO ALLT-SW                                
146100                                   KOLLA-ERS-SW                           
146200         MOVE JA                TO OBKR-SW                                
146300       END-IF                                                             
146400     END-IF                                                               
146500     .                                                                    
146600                                                                          
146700 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
146800     MOVE 'ECB-BYGG-UPP-ORD'    TO CURRENT-SECTION                        
146900                                                                          
147000     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
147100     MOVE OHUV-IDDC-PRIM       TO ORAD-IDDC                               
147200     MOVE ORAD-IDDC            TO WS-IDDC                                 
147300     MOVE +0                   TO ORAD-ADLAGOMR                           
147400     MOVE +0                   TO ORAD-ADGANG                             
147500     MOVE +0                   TO ORAD-ADPLATS                            
147600     IF TILLKOMMANDE-RAD                                                  
147700        MOVE AREG-IDARTNR      TO ORAD-IDARTNR                            
147800     ELSE                                                                 
147900        MOVE ORFK-IDARTNR(WS-INDEX-MID)                                   
148000                               TO ORAD-IDARTNR                            
148100     END-IF                                                               
148200     MOVE +1                   TO ORAD-IDLOPNR                            
148300                                                                          
148400     IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                              
148500        MOVE SPACE             TO ORAD-BERADREF                           
148600*       IF MID-IDORDNR5-REG NOT = SPACE                                   
148700*          MOVE MID-IDORDNR5-REG TO ORAD-BERADREF                         
148800*       END-IF                                                            
148900     ELSE                                                                 
149000        MOVE MID-BERADREF(WS-INDEX-MID)                                   
149100                               TO ORAD-BERADREF                           
149200     END-IF                                                               
149300     MOVE OHUV-BEKUNDRF        TO ORAD-BEVOLREF                           
149400     MOVE SPACE                TO ORAD-FLAKPLOC                           
149500     MOVE 'N'                  TO ORAD-FLSDCLEV                           
149600                                                                          
149700     MOVE NEJ                  TO ORAD-FLINVEST                           
149800     MOVE JA                   TO ORAD-FLOBTRAN                           
149900     IF TILLKOMMANDE-RAD                                                  
150000        IF DIST79-DEALER-PRICE                                            
150100          MOVE NEJ             TO ORAD-FLPRTILL                           
150200        ELSE                                                              
150300         IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                            
150400           MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                             
150500                               TO ORAD-FLPRTILL                           
150600         ELSE                                                             
150700           MOVE NEJ            TO ORAD-FLPRTILL                           
150800         END-IF                                                           
150900        END-IF                                                            
151000     ELSE                                                                 
151100        MOVE NEJ               TO ORAD-FLPRTILL                           
151200     END-IF                                                               
151300     IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                               
151400        MOVE OHUV-FLRESTN      TO ORAD-FLRESTN                            
151500     ELSE                                                                 
151600        MOVE MID-FLRESTN(WS-INDEX-MID)                                    
151700                               TO ORAD-FLRESTN                            
151800     END-IF                                                               
151900     IF TILLKOMMANDE-RAD                                                  
152000        MOVE JA                TO ORAD-FLTILLK                            
152100     ELSE                                                                 
152200        MOVE NEJ               TO ORAD-FLTILLK                            
152300     END-IF                                                               
152400     MOVE WC-CDC-SE            TO ORAD-IDDC-RO                            
152500     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
152600     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
152700     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
152800     IF OHUV-IDKAMPRF > +0                                                
152900        MOVE WC-CDC-SE         TO ORAD-IDDC                               
153000        MOVE ORAD-IDDC         TO WS-IDDC                                 
153100     END-IF                                                               
153200     MOVE SPACE                TO ORAD-IDLEVNR                            
153300     MOVE +0                   TO ORAD-IDLOPNR-RO                         
153400     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
153500     MOVE OHUV-IDKUNDRF        TO ORAD-IDKUNDRF                           
153600     MOVE ZERO                 TO ORAD-IDSPECEMB                          
153700     MOVE 'IMS '               TO ORAD-IDSYSTEM                           
153800     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
153900     IF MID-KDVRINFO(WS-INDEX-MID) = ALL '+'                              
154000        MOVE OHUV-KDVRINFO     TO ORAD-KDDSP                              
154100     ELSE                                                                 
154200        MOVE MID-KDVRINFO(WS-INDEX-MID)                                   
154300                               TO WS-ALFA-1                               
154400        MOVE WS-NUM-1          TO ORAD-KDDSP                              
154500     END-IF                                                               
154600     IF ORAD-KDDSP = +0                                                   
154700        MOVE +1                TO ORAD-KDDSP                              
154800     END-IF                                                               
154900     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
155000     IF MID-KDKVBRYT(WS-INDEX-MID) = ALL '+'                              
155100        MOVE +0                TO ORAD-KDKVBRYT                           
155200     ELSE                                                                 
155300        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
155400                               TO ORAD-KDKVBRYT                           
155500     END-IF                                                               
155600     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
155700     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
155800     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
155900                                  TEST-KDPRODSL                           
156000     IF ORAD-KDORDING = +3                                                
156100       MOVE SPACE              TO ORAD-KDOI                               
156200     ELSE                                                                 
156300       IF KDPRODSL-VOLVO-EMB OR ORAD-KDORDING = +1                        
156400         MOVE 'CD'             TO ORAD-KDOI                               
156500       ELSE                                                               
156600         MOVE 'DT'             TO ORAD-KDOI                               
156700       END-IF                                                             
156800     END-IF                                                               
156900     MOVE SPACE                TO ORAD-CLEARGROUP                         
157000                                                                          
157100     IF TILLKOMMANDE-RAD                                                  
157200       IF DIST79-DEALER-PRICE                                             
157300         MOVE SPACE            TO ORAD-KDPRTYP                            
157400       ELSE                                                               
157500        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
157600           MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                              
157700                               TO ORAD-KDPRTYP                            
157800        ELSE                                                              
157900           MOVE SPACE          TO ORAD-KDPRTYP                            
158000        END-IF                                                            
158100       END-IF                                                             
158200     ELSE                                                                 
158300        MOVE SPACE             TO ORAD-KDPRTYP                            
158400     END-IF                                                               
158500     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
158600     IF OHUV-KDTPOTYP = 1 OR 2 OR 3 OR 4                                  
158700        MOVE OHUV-KDTPOTYP     TO ORAD-KDTPOTYP                           
158800     ELSE                                                                 
158900        IF OHUV-KDTPOTYP = +0 AND                                         
159000                   MID-TITPO(WS-INDEX-MID) NOT = ALL '+'                  
159100           IF OHUV-IDKAMPRF > +0                                          
159200              MOVE +4          TO ORAD-KDTPOTYP                           
159300           ELSE                                                           
159400              MOVE +2          TO ORAD-KDTPOTYP                           
159500              IF ORAD-KDORDING = +3                                       
159600                 CONTINUE                                                 
159700              ELSE                                                        
159800                 MOVE +2       TO ORAD-KDORDING                           
159900              END-IF                                                      
160000           END-IF                                                         
160100        ELSE                                                              
160200           MOVE +0             TO ORAD-KDTPOTYP                           
160300        END-IF                                                            
160400     END-IF                                                               
160500     MOVE JA                   TO ORAD-FLORDING                           
160600     MOVE ORFK-KDVRINFO(WS-INDEX-MID) TO ORAD-KDVRINFO                    
160700                                                                          
160800     IF TILLKOMMANDE-RAD                                                  
160900       MOVE TILK-KVBEART(WS-INDEX-TILLK)                                  
161000                              TO ORAD-KVBEART                             
161100     ELSE                                                                 
161200       MOVE ORFK-KVBEART(WS-INDEX-MID) TO WS-ALFA-6                       
161300       MOVE WS-NUM-6           TO ORAD-KVBEART                            
161400     END-IF                                                               
161500                                                                          
161600     MOVE +0                   TO ORAD-KVBEART-Q                          
161700     MOVE +0                   TO ORAD-KVPREAVB                           
161800     MOVE +0                   TO ORAD-KVPRERO                            
161900     MOVE +0                   TO ORAD-KVOKS-PREL                         
162000     MOVE +0                   TO ORAD-IDPRQUES                           
162100     MOVE +0                   TO ORAD-PRARTBTO-LOC                       
162200     MOVE +0                   TO ORAD-RERAB                              
162300     MOVE SPACE                TO ORAD-KDVALISO                           
162400     MOVE SPACE                TO ORAD-KDVAT                              
162500     MOVE SPACE                TO ORAD-KDRAB                              
162600     MOVE SPACE                TO ORAD-BEART-VIPS                         
162700                                                                          
162800     IF TILLKOMMANDE-RAD                                                  
162900       IF DIST79-DEALER-PRICE                                             
163000           MOVE +0             TO ORAD-PRARTNTO                           
163100           MOVE TILK-PRARTNTO-LOC(WS-INDEX-TILLK)                         
163200                                  TO ORAD-PRARTNTO-LOC                    
163300           MOVE TILK-PRARTNTO-LOCPREL(WS-INDEX-TILLK)                     
163400                                  TO ORAD-PRARTNTO-LOCPREL                
163500       ELSE                                                               
163600        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
163700           MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                             
163800                                 TO ORAD-PRARTNTO                         
163900           MOVE ZERO             TO ORAD-PRARTNTO-LOC                     
164000        ELSE                                                              
164100           MOVE +0             TO ORAD-PRARTNTO                           
164200                                  ORAD-PRARTNTO-LOC                       
164300                                  ORAD-PRARTNTO-LOCPREL                   
164400        END-IF                                                            
164500       END-IF                                                             
164600     ELSE                                                                 
164700        IF MID-PRARTNTO(WS-INDEX-MID) = ALL '+'                           
164800           MOVE +0             TO ORAD-PRARTNTO                           
164900                                  ORAD-PRARTNTO-LOC                       
165000                                  ORAD-PRARTNTO-LOCPREL                   
165100        ELSE                                                              
165200          IF DIST79-DEALER-PRICE OR                                       
165400             DIST79-ECOM-PRICE                                            
165500            MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)                       
165600                               TO ORAD-PRARTNTO-LOC                       
165710           IF DIST79-ECOM-PRICE                                           
165800             MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)                      
165900                               TO ORAD-PRARTBTO-LOC                       
166000           END-IF                                                         
166100            MOVE +0            TO ORAD-PRARTNTO                           
166200            MOVE +0            TO ORAD-PRARTNTO-LOCPREL                   
166300          ELSE                                                            
166400            MOVE ORFK-PRARTNTO-UT(WS-INDEX-MID)                           
166500                               TO ORAD-PRARTNTO                           
166600            MOVE +0            TO ORAD-PRARTNTO-LOC                       
166700            MOVE +0            TO ORAD-PRARTNTO-LOCPREL                   
166800          END-IF                                                          
166900        END-IF                                                            
167000        MOVE +0                TO ORAD-PRBPRIS                            
167100     END-IF                                                               
167200     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
167300        MOVE MID-IDARTNR(WS-INDEX-MID) (11:1)                             
167400                               TO ORAD-REKSIFFR                           
167500     ELSE                                                                 
167600        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
167700     END-IF                                                               
167800     MOVE +0                   TO ORAD-RERF-RAD                           
167900     MOVE +0                   TO ORAD-KVSLATT                            
168000                                                                          
168100     IF TILLKOMMANDE-RAD                                                  
168200       IF DIST79-DEALER-PRICE                                             
168300          MOVE +0              TO ORAD-TIPRIS                             
168400       ELSE                                                               
168500        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
168600           MOVE TILK-TIPRIS(WS-INDEX-TILLK)                               
168700                               TO ORAD-TIPRIS                             
168800        ELSE                                                              
168900           MOVE +0             TO ORAD-TIPRIS                             
169000        END-IF                                                            
169100       END-IF                                                             
169200     ELSE                                                                 
169300        MOVE +0                TO ORAD-TIPRIS                             
169400     END-IF                                                               
169500                                                                          
169600     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
169700     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
169800     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
169900     MOVE +0                   TO ORAD-TIRODAT                            
170000     IF MID-TITPO(WS-INDEX-MID) = ALL '+'                                 
170100        MOVE OHUV-TITPO        TO ORAD-TITPO                              
170200     ELSE                                                                 
170300        MOVE MID-TITPO(WS-INDEX-MID) TO WS-ALFA-6                         
170400        MOVE WS-NUM-6          TO ORAD-TITPO                              
170500     END-IF                                                               
170600     MOVE AREG-VKART           TO ORAD-VKART                              
170700     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
170800     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
170900     MOVE SPACE                TO ORAD-IDBIL                              
171000                                  ORAD-IDKLIENT                           
171100                                  ORAD-IDARBREF                           
171200                                  ORAD-IDVIN                              
171300                                                                          
171400     IF ORAD-KDORDKL = 1 AND                                              
171500        GMT-FLLDCKND = JA                                                 
171600        MOVE ORAD-BERADREF     TO ORAD-IDKUNDRF-WIP                       
171700     ELSE                                                                 
171800        MOVE SPACE             TO ORAD-IDKUNDRF-WIP                       
171900     END-IF                                                               
172000     MOVE +0                   TO ORAD-PRAVCOST                           
172100     .                                                                    
172200                                                                          
172300 ECC-LAS-NYA-ARTIKELREG SECTION.                                          
172400     MOVE 'ECC-LAS-NYA-ARTI'   TO CURRENT-SECTION                         
172500                                                                          
172600     IF ALLT-OK                                                           
172700     MOVE ORAD-IDARTNR         TO W-IDARTNR                               
172800     PERFORM IMS-06-GU-WDK901                                             
172900                                                                          
173000     IF SEGMENT-SAKNAS                                                    
173100        MOVE W-IDARTNR         TO ARTM-IDARTNR-IN                         
173200        CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                   
173300     END-IF                                                               
173400                                                                          
173500     END-IF                                                               
173600     .                                                                    
173700                                                                          
173800 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
173900     MOVE 'ECF-KOMPLETTERA-'   TO CURRENT-SECTION                         
174000                                                                          
174100     IF ALLT-OK                                                           
174200        MOVE ORAD-KDKVBRYT     TO KVAN-KDKVBRYT-IN                        
174300        MOVE ORAD-IDSYSTEM     TO KVAN-IDSYSTEM-IN                        
174400        MOVE ORAD-KVBEART      TO KVAN-KVBEART-IN                         
174500        MOVE AREG-KVQPACK-0    TO KVAN-KVQPACK-0-IN                       
174600        MOVE AREG-KVQPACK-1    TO KVAN-KVQPACK-1-IN                       
174700        MOVE AREG-KDPRODSL     TO KVAN-KDPRODSL-IN                        
174800        MOVE AREG-KDSORT       TO KVAN-KDSORT-IN                          
174900        MOVE AREG-IDFKNGRP     TO KVAN-IDFKNGRP-IN                        
175000        MOVE OHUV-KDORDKL      TO KVAN-KDORDKL-IN                         
175100        MOVE OHUV-FLEMBORD     TO KVAN-FLEMBORD-IN                        
175200        MOVE OHUV-FLOVRLEV     TO KVAN-FLOVRLEV-IN                        
175300        MOVE OHUV-FLORDSPE     TO KVAN-FLORDSPE-IN                        
175400        MOVE OHUV-FLFORBI      TO KVAN-FLFORBI-IN                         
175500        MOVE OHUV-IDKAMPRF     TO KVAN-IDKAMPRF-IN                        
175600        MOVE ORAD-IDDC         TO KVAN-IDDC-IN                            
175700        MOVE ORAD-IDDISTR      TO KVAN-IDDISTR-IN                         
175800        MOVE ORAD-IDKUNDNR     TO KVAN-IDKUNDNR-IN                        
175900        MOVE ORAD-BERADREF     TO KVAN-BERADREF-IN                        
176000        MOVE ORAD-IDARTNR      TO KVAN-IDARTNR-IN                         
176100                                                                          
176200        CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB                   
176300                                          KVAN-WDC1-PCB                   
176400                                                                          
176500        MOVE KVAN-KDKVBRYT-UT  TO ORAD-KDKVBRYT                           
176600        MOVE KVAN-KVBEART-Q-UT TO ORAD-KVBEART-Q                          
176700                                                                          
176800        IF KVAN-KDORDBEK-UT > +0                                          
176900           MOVE JA             TO OBKR-SW                                 
177000        END-IF                                                            
177100     END-IF                                                               
177200     .                                                                    
177300                                                                          
177400 ECI-KOMPLETTERA-DIREKTLEVERANS SECTION.                                  
177500     MOVE 'ECI-KOMPLETTERA-'   TO CURRENT-SECTION                         
177600                                                                          
177700     PERFORM S20-HAMTA-WDB6-INFO                                          
177800                                                                          
177900     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC)                    
178000        MOVE ORAD-IDDISTR      TO DLEV-IDDISTR-IN                         
178100        MOVE ORAD-IDKUNDNR     TO DLEV-IDKUNDNR-IN                        
178200        MOVE OHUV-KDORDKL      TO DLEV-KDORDKL-IN                         
178300        MOVE ORAD-IDARTNR      TO DLEV-IDARTNR-IN                         
178400        MOVE AREG-IDLEVNR      TO DLEV-IDLEVNR-IN                         
178500        MOVE ORAD-KVBEART-Q    TO DLEV-KVBEART-Q-IN                       
178600        MOVE AREG-REDIRLEV     TO DLEV-REDIRLEV-IN                        
178700        MOVE SPACE             TO DLEV-IDDC-IN                            
178800        MOVE ORAD-IDDC         TO DLEV-IDDC-ORD-IN                        
178900        MOVE ORAD-IDKAMPRF     TO DLEV-IDKAMPRF-IN                        
179000        MOVE ORAD-KDTPOTYP     TO DLEV-KDTPOTYP-IN                        
179100        MOVE AREG-KDUART       TO DLEV-KDUART-IN                          
179200        MOVE OHUV-FLFORBI      TO DLEV-FLFORBI-IN                         
179300        MOVE ORAD-FLRESTN      TO DLEV-FLRESTN-IN                         
179400        MOVE AREG-FLREFILL     TO DLEV-FLREFILL-IN                        
179500        MOVE ORAD-KDORDING     TO DLEV-KDORDING-IN                        
179600        MOVE OHUV-IDKUNDRF     TO DLEV-IDKUNDRF-IN                        
179700                                                                          
179800        MOVE +1 TO WS-INDEX                                               
179900        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
180000           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
180100                               TO DLEV-IDDC-CLEAR-IN(WS-INDEX)            
180200           ADD +1 TO WS-INDEX                                             
180300        END-PERFORM                                                       
180400                                                                          
180500        MOVE 1                 TO DLEV-KDCALL                             
180600        MOVE SPACE             TO DLEV-CLEARGROUP                         
180700        MOVE ORAD-KDOI         TO DLEV-KDOI-UT                            
180800                                                                          
180900        CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                   
181000                                          DLEV-LEVG-PCB                   
181100                                          DLEV-LEVA-PCB                   
181200                                          DLEV-ARTS-PCB                   
181300                                          DLEV-WDB6-PCB                   
181400                                          TPO2-FILA-PCB                   
181500        IF DLEV-KDORDBEK-UT = 21 OR 53                                    
181600           MOVE JA               TO OBKR-SW                               
181700           MOVE NEJ              TO ALLT-SW                               
181800           MOVE ZERO             TO KVAN-KDORDBEK-UT                      
181900           MOVE DLEV-IDDC-UT     TO ORAD-IDDC                             
182000        ELSE                                                              
182100           IF DLEV-KDORDBEK-UT = 95                                       
182200              MOVE JA            TO OBKR-SW                               
182300           END-IF                                                         
182400           IF DLEV-IDLEVNR-UT NOT = SPACE                                 
182500              IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                          
182600                MOVE DLEV-IDDC-UT   TO ORAD-IDDC                          
182700                                          WS-IDDC                         
182800              END-IF                                                      
182900                                                                          
183000              MOVE AREG-ADLAGOMR    TO ORAD-ADLAGOMR                      
183100              MOVE AREG-ADGANG      TO ORAD-ADGANG                        
183200              MOVE AREG-ADPLATS     TO ORAD-ADPLATS                       
183300           END-IF                                                         
183400           MOVE DLEV-KDORDSTA-UT  TO AVSR-KDORDSTA (WS-INDEX-WOPS)        
183500           MOVE DLEV-KDVIA-UT     TO AVSR-KDVIA    (WS-INDEX-WOPS)        
183600           MOVE DLEV-KVDAGAR-DIFF-UT TO                                   
183700                                  AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
183800           MOVE DLEV-TISKEPPN-DDC-UT TO                                   
183900                                  AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
184000*        DC FRÅN WDF211                                                   
184100           IF DLEV-FLSDCLEV-UT = JA                                       
184200             MOVE DLEV-IDDC-UT       TO ORAD-IDDC                         
184300                                        WS-IDDC                           
184400           ELSE                                                           
184500             MOVE DLEV-KDOI-UT       TO ORAD-KDOI                         
184600             MOVE DLEV-CLEARGROUP    TO ORAD-CLEARGROUP                   
184700           END-IF                                                         
184800                                                                          
184900           MOVE DLEV-FLRESTN-UT   TO ORAD-FLRESTN                         
185000           MOVE DLEV-FLSDCLEV-UT  TO ORAD-FLSDCLEV                        
185100           MOVE DLEV-IDLEVNR-UT   TO ORAD-IDLEVNR                         
185200        END-IF                                                            
185300        IF DLEV-KDORDBEK-UT = 0 AND AREG-ADLAGOMR = 79                    
185400           AND ORAD-IDDC = WC-CDC-SE                                      
185500           MOVE OHUV-IDDISTR  TO TEST-IDDISTR                             
185600           IF DIST18-SKROT                                                
185700              CONTINUE                                                    
185800           ELSE                                                           
185900              MOVE 26         TO DLEV-KDORDBEK-UT                         
186000              MOVE JA         TO OBKR-SW                                  
186100           END-IF                                                         
186200        END-IF                                                            
186300     END-IF                                                               
186400     .                                                                    
186500                                                                          
186600 ECK-KOMPLETTERA-ERSATTNING SECTION.                                      
186700     MOVE 'ECK-KOMPLETTERA-'   TO CURRENT-SECTION                         
186800                                                                          
186900     IF ALLT-OK                                                           
187000        MOVE ORAD-IDARTNR      TO KERS-IDARTNR                            
187100        MOVE ORAD-IDDC         TO KERS-IDDC                               
187200        MOVE OHUV-FLPRERS      TO KERS-FLPRERS                            
187300        MOVE ORAD-FLPRTILL     TO KERS-FLPRTILL                           
187400        MOVE OHUV-FLFORBI      TO KERS-FLFORBI                            
187500        MOVE OHUV-FLORDSPE     TO KERS-FLORDSPE                           
187600        MOVE OHUV-FLOVRLEV     TO KERS-FLOVRLEV                           
187700        MOVE ORAD-IDKAMPRF     TO KERS-IDKAMPRF                           
187800        MOVE AREG-KDERS        TO KERS-KDERS                              
187900        MOVE AREG-KDERS-UTG    TO KERS-KDERS-UTG                          
188000        MOVE ORAD-KDPRTYP      TO KERS-KDPRTYP                            
188100        MOVE ORAD-KDTPOTYP     TO KERS-KDTPOTYP                           
188200        MOVE AREG-KDUART       TO KERS-KDUART                             
188300        MOVE ORAD-KVBEART      TO KERS-KVBEART                            
188400        MOVE ORAD-PRARTNTO     TO KERS-PRARTNTO                           
188500        MOVE ORAD-DEAL-PR-LINE TO KERS-DEAL-PR-LINE                       
188600        MOVE ORAD-TIPRIS       TO KERS-TIPRIS                             
188700                                                                          
188800        CALL W411KERS USING KERS-W411KERS TILK-W411TILK                   
188900                            KERS-ARTC-PCB KERS-ERSA-PCB                   
189000                            SDCA-ARTS-PCB CDCA-ARTM-PCB                   
189100                                                                          
189200        IF KERS-KDORDBEK > ZERO AND                                       
189300           ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0) OR                 
189400             KERS-KDERS > 10 )                                            
189500           MOVE JA               TO OBKR-SW                               
189600           MOVE NEJ              TO ALLT-SW                               
189700        END-IF                                                            
189800                                                                          
189900        PERFORM S20-HAMTA-WDB6-INFO                                       
190000                                                                          
190100        IF KERS-KDORDBEK > ZERO                                           
190200           IF DCS-SDC OR DCS-CDC                                          
190300             IF AREG-KDERS = 11 OR 12 OR 17 OR                            
190400                             21 OR 22 OR 27                               
190500                MOVE JA            TO KOLLA-ERS-SW                        
190600             ELSE                                                         
190700                IF AREG-KDERS = 14 OR 15 OR 18 OR 19 OR                   
190800                                24 OR 25 OR 28 OR 29                      
190900                  MOVE JA          TO ALLT-SW                             
191000                END-IF                                                    
191100             END-IF                                                       
191200           END-IF                                                         
191300           IF DCS-NDC                                                     
191400             IF AREG-KDERS > 18                                           
191500               MOVE JA              TO KOLLA-ERS-SW                       
191600             ELSE                                                         
191700               MOVE ZERO            TO KERS-KDORDBEK                      
191800               PERFORM S02-RENSA-TILLK-TAB                                
191900               MOVE JA              TO ALLT-SW                            
192000             END-IF                                                       
192100           END-IF                                                         
192200        END-IF                                                            
192300        IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                   
192400          IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')               
192500***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
192600***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
192700***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
192800***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
192900           CONTINUE                                                       
193000          ELSE                                                            
193100           MOVE +0               TO KVAN-KDORDBEK-UT                      
193200           MOVE ORAD-KVBEART     TO ORAD-KVBEART-Q                        
193300          END-IF                                                          
193400        END-IF                                                            
193500        IF KERS-KDORDBEK > +0 AND DLEV-KDORDBEK-UT > +0                   
193600           MOVE +0               TO DLEV-KDORDBEK-UT                      
193700        END-IF                                                            
193800                                                                          
193900     ELSE                                                                 
194000        MOVE +0                  TO KERS-KDERS                            
194100     END-IF                                                               
194200     .                                                                    
194300                                                                          
194400 ECL-KOMPLETTERA-SPARRAR SECTION.                                         
194500     MOVE 'ECL-KOMPLETTERA-'   TO CURRENT-SECTION                         
194600                                                                          
194700     IF ALLT-OK OR KOLLA-ERS                                              
194800        MOVE ORAD-BERADREF     TO SPAR-BERADREF                           
194900        MOVE OHUV-BEKUNDRF     TO SPAR-BEKUNDRF                           
195000        MOVE AREG-FLAVRART     TO SPAR-FLAVRART                           
195100        MOVE OHUV-FLEMBORD     TO SPAR-FLEMBORD                           
195200        MOVE OHUV-FLFORBI      TO SPAR-FLFORBI                            
195300        MOVE AREG-FLLSRDEL     TO SPAR-FLLSRDEL                           
195400        MOVE OHUV-FLORDSPE     TO SPAR-FLORDSPE                           
195500        MOVE OHUV-FLOVRLEV     TO SPAR-FLOVRLEV                           
195600        MOVE AREG-FLRADREF     TO SPAR-FLRADREF                           
195700        MOVE ORAD-FLRESTN      TO SPAR-FLRESTN                            
195800        MOVE ORAD-IDARTNR      TO SPAR-IDARTNR                            
195900        MOVE AREG-FLIART       TO SPAR-FLIART                             
196000        MOVE AREG-FLMARKSP     TO SPAR-FLMARKSP                           
196100        MOVE ORAD-IDDISTR      TO SPAR-IDDISTR                            
196200        MOVE ORAD-IDKUNDNR     TO SPAR-IDKUNDNR                           
196300        MOVE ORAD-IDKUNDRF-RO  TO SPAR-IDKUNDRF-RO                        
196400        MOVE ORAD-IDDC         TO SPAR-IDDC                               
196500        MOVE ORAD-IDSYSTEM     TO SPAR-IDSYSTEM                           
196600        MOVE AREG-KDERS-UTG    TO SPAR-KDERS-UTG                          
196700        MOVE OHUV-KDFAKTYP     TO SPAR-KDFAKTYP                           
196800        MOVE AREG-KDLEVSP      TO SPAR-KDLEVSP                            
196900        MOVE +1                TO SPAR-KDORDBEH                           
197000        MOVE OHUV-KDORDKL      TO SPAR-KDORDKL                            
197100        MOVE AREG-KDPRODSL     TO SPAR-KDPRODSL                           
197200        MOVE AREG-KDSORT       TO SPAR-KDSORT                             
197300        MOVE ORAD-KDPRTYP      TO SPAR-KDPRTYP                            
197400        MOVE ORAD-KDTPOTYP     TO SPAR-KDTPOTYP                           
197500        MOVE AREG-KDUART       TO SPAR-KDUART                             
197600        MOVE AREG-PRARTSTD     TO SPAR-PRARTSTD                           
197700        MOVE AREG-TIFINLV      TO SPAR-TIFINLV                            
197800        MOVE ORAD-TIRODAT      TO SPAR-TIRODAT                            
197900        MOVE ORAD-TITPO        TO SPAR-TITPO                              
198000        MOVE ORAD-FLSDCLEV     TO SPAR-FLSDCLEV                           
198100        MOVE AREG-KDERS        TO SPAR-KDERS                              
198200        MOVE OHUV-TIREPDAT     TO SPAR-TIREPDAT                           
198300                                                                          
198400        CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                   
198500                                          SPAR-WDF8A-PCB                  
198600                                          SPAR-WDK6-PCB                   
198700                                                                          
198800        IF SPAR-KDORDBEK     > ZERO                                       
198900          MOVE JA             TO OBKR-SW                                  
199000          MOVE NEJ            TO ALLT-SW                                  
199100          IF SPAR-KDORDBEK = 51 OR 67 OR 58                               
199200            MOVE NEJ          TO KOLLA-ERS-SW                             
199300            MOVE ZERO            TO KERS-KDORDBEK                         
199400            PERFORM S02-RENSA-TILLK-TAB                                   
199500          END-IF                                                          
199800          MOVE ZERO         TO XDCA-DAPUBL                                
199600*FOR CODE 67 THERE IS A SPECIAL CHECK IN W411XDCA                         
199700        IF  SPAR-KDORDBEK =67 AND  SPAR-FLPUBCDC = YES                    
199800          MOVE 99999999       TO XDCA-DAPUBL                              
199900*         MOVE 99999999       TO NDCA-DAPUBL                              
200000        END-IF                                                            
200100          IF KVAN-KDORDBEK-UT > +0                                        
200200             MOVE +0          TO KVAN-KDORDBEK-UT                         
200300             MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                          
200400          END-IF                                                          
200500*FOR KDERS 19/29,SPAR GIVES OCC54.BUT IF STOCKS ARE PRESENT IN A          
200600*DC,ORDERLINE WITH 19/29 CAN BE REGISTERED.                               
200700          IF SPAR-KDORDBEK = 54 AND NOT DCS-DDC                           
200800             MOVE JA                TO ALLT-SW                            
200900          END-IF                                                          
201000          IF DLEV-KDORDBEK-UT > +0                                        
201100             MOVE +0          TO DLEV-KDORDBEK-UT                         
201200          END-IF                                                          
201300        END-IF                                                            
201400        IF AREG-KDSORT = 'SW'                                             
201500           MOVE 67                  TO SPAR-KDORDBEK                      
201600           MOVE JA                  TO OBKR-SW                            
201700           MOVE NEJ                 TO ALLT-SW                            
201800           IF KOLLA-ERS-SW = JA                                           
201900             MOVE NEJ             TO KOLLA-ERS-SW                         
202000             MOVE ZERO            TO KERS-KDORDBEK                        
202100             PERFORM S02-RENSA-TILLK-TAB                                  
202200           END-IF                                                         
202300        END-IF                                                            
202400     END-IF                                                               
202500     .                                                                    
202600                                                                          
202700 ECJ-KOMPLETTERA-PRIS SECTION.                                            
202800     MOVE 'ECJ-KOMPLETTERA-'         TO CURRENT-SECTION                   
202900                                                                          
203000     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
203100        IF DIST79-DEALER-PRICE                                            
203200          IF WS-IDPRQUES             = +0                                 
203300             MOVE WS-IDPRQUES        TO PRNO-IDPRQUES-IN                  
203400             MOVE +1                 TO PRNO-KDCALL                       
203500                                                                          
203600             CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB              
203700                                                                          
203800             MOVE PRNO-IDPRQUES-UT   TO PRQU-IDPRQUES                     
203900                                           WS-IDPRQUES                    
204000             MOVE +1                 TO PRQU-KDCALL                       
204100          ELSE                                                            
204200             MOVE WS-IDPRQUES        TO PRNO-IDPRQUES-IN                  
204300             MOVE +2                 TO PRNO-KDCALL                       
204400                                                                          
204500             CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB              
204600                                                                          
204700             MOVE PRNO-IDPRQUES-UT   TO PRQU-IDPRQUES                     
204800                                           WS-IDPRQUES                    
204900             MOVE +2                 TO PRQU-KDCALL                       
205000          END-IF                                                          
205100                                                                          
205200          MOVE W-IDDISTR             TO PRQU-IDDISTR                      
205300          MOVE W-IDKUNDNR            TO PRQU-IDKUNDNR                     
205400          MOVE ORAD-IDKUNDRF         TO PRQU-IDKUNDRF                     
205500          MOVE ORAD-IDORDER          TO PRQU-IDORDER                      
205600          MOVE ORAD-KDORDKL          TO PRQU-KDORDKL                      
205700          MOVE 'N'                   TO PRQU-KDPRSTA                      
205800          MOVE ORAD-IDARTNR          TO PRQU-IDARTNR                      
205900          MOVE ORAD-KVBEART-Q        TO PRQU-KVBEART-Q                    
206000                                                                          
206100          MOVE GMT-IDFTG             TO W-WDB1-IDFTG                      
206200          MOVE GMT-IDPARTNR          TO W-WDB1-IDPARTNR                   
206300          PERFORM IMS-08-GU-WDB101                                        
206400          MOVE BET-KDVALISO          TO PRQU-KDVALISO                     
206500                                        ORAD-KDVALISO                     
206600                                                                          
206700          MOVE ORAD-PRARTNTO-LOC     TO PRQU-PRARTNTO-LOC                 
206800          MOVE +0                    TO PRQU-PRARTNTO-LOCPREL             
206900          MOVE ORAD-IDSYSTEM         TO PRQU-IDSYSTEM                     
207000                                                                          
207100          CALL W335PRQU USING PRQU-W335PRQU PRQU-WDG2-PCB                 
207200                                             PRQU-WDC7-PCB                
207300                                             PRQU-SJKO-WDK6-PCB           
207400                                                                          
207500          MOVE PRQU-IDPRQUES         TO  ORAD-IDPRQUES                    
207600                                            WS-IDPRQUES                   
207700          MOVE PRQU-FLPRTILL         TO  ORAD-FLPRTILL                    
207800                                                                          
207900          IF ORAD-PRARTNTO-LOC = +0                                       
208000             MOVE PRQU-PRARTNTO-LOCPREL TO ORAD-PRARTNTO-LOCPREL          
208100          END-IF                                                          
208200                                                                          
208300          IF ORAD-PRARTNTO-LOC NOT = +0                                   
208400            IF ORAD-KDPRTYP = SPACE                                       
208500              MOVE 'P'         TO ORAD-KDPRTYP                            
208600              MOVE ORAD-TIREGDAT TO ORAD-TIPRIS                           
208700            END-IF                                                        
208800          END-IF                                                          
208900                                                                          
209000        ELSE                                                              
209100*           *IF NOT DIST79-DEALER-PRICE                                   
209200                                                                          
209300            PERFORM S20-HAMTA-WDB6-INFO                                   
209400                                                                          
209500            IF DCS-NDC OR DCS-DDC OR DCS-SDC OR                           
209600              (ORAD-KDTPOTYP = +0 AND AREG-KDUART = SPACE)                
209700                                                                          
210800              IF DIST79-ECOM-PRICE                                        
210900                CONTINUE                                                  
211000              ELSE                                                        
211100                 IF ORAD-PRARTNTO NOT = +0                                
211200*                  *FETCH ONLY KDVALISO FROM W335PRIS                     
211300                   MOVE 2            TO PRIS-KDCALL                       
211400                 ELSE                                                     
211500*                  *FETCH PRARTNTO/PRAVCOST AND KDVALISO FRON W3          
211600                   MOVE 1            TO PRIS-KDCALL                       
211700                 END-IF                                                   
211800              END-IF                                                      
212000            ELSE                                                          
212100              MOVE 2               TO PRIS-KDCALL                         
212200            END-IF                                                        
212300                                                                          
212400            MOVE IDPGM             TO PRIS-IDPGM                          
212500            MOVE ORAD-IDARTNR      TO PRIS-IDARTNR                        
212600            MOVE ORAD-IDDISTR      TO PRIS-IDDISTR                        
212700            MOVE ORAD-IDKUNDNR     TO PRIS-IDKUNDNR                       
212800            MOVE ORAD-IDDC         TO PRIS-IDDC                           
212900            MOVE OHUV-KDORDKL      TO PRIS-KDORDKL                        
213000            MOVE ORAD-KVBEART-Q    TO PRIS-KVBEART                        
213100            MOVE ORAD-FLINVEST     TO PRIS-FLINVEST                       
213200                                                                          
213300            CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB               
213400                                PRIS-WDK7-PCB                             
213500                                PRIS-GMTA-PCB PRIS-BETA-PCB               
213600                                PRIS-GPRIA-PCB PRIS-GPRIB-PCB             
213700                                PRIS-COST-WDK6-PCB                        
213800                                PRIS-COST-WDK7-PCB                        
213900                                PRIS-COST-WDF1-PCB                        
214000                                PRIS-COST-9305-PCB                        
214100                                PRIS-COST-WDK72-PCB                       
214200                                PRIS-COST-WDB6-PCB                        
214300                                                                          
214400            IF PRIS-KDSVAR = '2'                                          
214500              MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'          
214600                                   TO FELTEXT                             
214700              CALL ABEND USING RKOD-ABEND                                 
214800            END-IF                                                        
214900                                                                          
215000            IF PRIS-KDCALL = 2                                            
215100               MOVE PRIS-KDVALISO TO ORAD-KDVALISO                        
215200               IF ORAD-KDPRTYP = SPACE                                    
215300                 MOVE 'P'           TO ORAD-KDPRTYP                       
215400                 MOVE ORAD-TIREGDAT TO ORAD-TIPRIS                        
215500               END-IF                                                     
215600            ELSE                                                          
216200               MOVE PRIS-PRARTNTO TO ORAD-PRARTNTO                        
216400               MOVE PRIS-FLPRTILL TO ORAD-FLPRTILL                        
216500               MOVE PRIS-KDPRTYP  TO ORAD-KDPRTYP                         
216600               MOVE PRIS-PRBPRIS  TO ORAD-PRBPRIS                         
216700               MOVE ORAD-TIREGDAT TO ORAD-TIPRIS                          
216800               MOVE PRIS-KDVALISO TO ORAD-KDVALISO                        
216900               MOVE PRIS-PRAVCOST TO ORAD-PRAVCOST                        
217000            END-IF                                                        
217100        END-IF                                                            
217200     END-IF                                                               
217300     .                                                                    
217400                                                                          
217500 ECM-KOMPLETTERA-TPO1 SECTION.                                            
217600     MOVE 'ECM-KOMPLETTERA-'   TO CURRENT-SECTION                         
217700                                                                          
217800     PERFORM S20-HAMTA-WDB6-INFO                                          
217900                                                                          
218000     IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC)                    
218100        MOVE ORAD-IDDISTR      TO TPO1-IDDISTR                            
218200        MOVE ORAD-IDKUNDNR     TO TPO1-IDKUNDNR                           
218300        MOVE ORAD-IDKUNDRF     TO TPO1-IDKUNDRF                           
218400        MOVE ORAD-IDARTNR      TO TPO1-IDARTNR                            
218500        MOVE ORAD-BERADREF     TO TPO1-BERADREF                           
218600        MOVE AREG-IDANSK       TO TPO1-IDANSK                             
218700        MOVE OHUV-IDKONTO      TO TPO1-IDKONTO                            
218800        MOVE OHUV-IDKST        TO TPO1-IDKST                              
218900        MOVE OHUV-IDANALYS     TO TPO1-IDANALYS                           
219000        MOVE ORAD-KDDSP        TO TPO1-KDDSP                              
219100        MOVE OHUV-KDFAKTYP     TO TPO1-KDFAKTYP                           
219200        MOVE ARB-KDFRAKT       TO TPO1-KDFRAKT                            
219300        MOVE ORAD-KDKVBRYT     TO TPO1-KDKVBRYT                           
219400        MOVE ORAD-KDORDING     TO TPO1-KDORDING                           
219500        MOVE OHUV-KDORDKL      TO TPO1-KDORDKL                            
219600        MOVE AREG-KDPRODSL     TO TPO1-KDPRODSL                           
219700        MOVE ORAD-KDVRINFO     TO TPO1-KDVRINFO                           
219800        MOVE ORAD-KVBEART-Q    TO TPO1-KVBEART-Q                          
219900        MOVE AREG-REKSIFFR     TO TPO1-REKSIFFR                           
220000        MOVE ORAD-TITPO        TO TPO1-TITPO                              
220100        IF ORAD-KDPRTYP = 'P'                                             
220200           MOVE ORAD-PRARTNTO  TO TPO1-PRARTNTO                           
220300           MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                    
220400           MOVE ORAD-KDPRTYP   TO TPO1-KDPRTYP                            
220500           MOVE ORAD-FLPRTILL  TO TPO1-FLPRTILL                           
220600        ELSE                                                              
220700           MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                    
220800           MOVE ZERO           TO TPO1-PRARTNTO                           
220900           MOVE SPACE          TO TPO1-KDPRTYP                            
221000           MOVE NEJ            TO TPO1-FLPRTILL                           
221100        END-IF                                                            
221200        MOVE ORAD-BEVOLREF     TO TPO1-BEVOLREF                           
221300        MOVE ORAD-IDKAMPRF     TO TPO1-IDKAMPRF                           
221400        MOVE ORAD-IDSYSTEM     TO TPO1-IDSYSTEM                           
221500        MOVE ORAD-FLINVEST     TO TPO1-FLINVEST                           
221600        MOVE ORAD-IDLEVNR      TO TPO1-IDLEVNR                            
221700        MOVE AREG-FLTPO1       TO TPO1-FLTPO1                             
221800        MOVE AREG-KVFRYSTI     TO TPO1-KVFRYSTI                           
221900        MOVE +1                TO TPO1-KDORDBEH                           
222000        MOVE OHUV-FLFORBI      TO TPO1-FLFORBI                            
222100        MOVE OHUV-FLORDSPE     TO TPO1-FLORDSPE                           
222200        MOVE OHUV-FLOVRLEV     TO TPO1-FLOVRLEV                           
222300        MOVE ORAD-KDTPOTYP     TO TPO1-KDTPOTYP                           
222400        MOVE OHUV-BEKUNDRF     TO TPO1-BEKUNDRF                           
222500                                                                          
222600        MOVE +0                TO TPO1-KDORDBEK                           
222700        MOVE SPACE             TO TPO1-FLKLAR                             
222800                                                                          
222900        MOVE OHUV-KDORDTYP-LDC TO TPO1-KDORDTYP-LDC                       
223000        MOVE OHUV-TIREPDAT     TO TPO1-TIREPDAT                           
223100        MOVE ORAD-IDKUNDRF-WIP TO TPO1-IDKUNDRF-WIP                       
223200                                                                          
223300        CALL W411TPO1 USING TPO1-W411TPO1 TPO1-ORDP-PCB                   
223400                            TPO1-ARTM-PCB TPO1-ZZAC-PCB                   
223500                                                                          
223600        IF TPO1-KDORDBEK > +0                                             
223700           MOVE JA             TO OBKR-SW                                 
223800           MOVE NEJ            TO ALLT-SW                                 
223900           MOVE WC-CDC-SE      TO ORAD-IDDC                               
224000           MOVE ORAD-IDDC      TO WS-IDDC                                 
224100           IF KVAN-KDORDBEK-UT > +0                                       
224200              MOVE +0          TO KVAN-KDORDBEK-UT                        
224300              MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                         
224400           END-IF                                                         
224500           IF DLEV-KDORDBEK-UT > +0                                       
224600              MOVE +0          TO DLEV-KDORDBEK-UT                        
224700           END-IF                                                         
224800        ELSE                                                              
224900           IF TPO1-FLKLAR = JA                                            
225000              MOVE NEJ         TO ALLT-SW                                 
225100              IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD             
225200                 MOVE ZERO     TO KERS-KDORDBEK                           
225300                 PERFORM S02-RENSA-TILLK-TAB                              
225400              END-IF                                                      
225500           END-IF                                                         
225600        END-IF                                                            
225700     END-IF                                                               
225800     .                                                                    
225900                                                                          
226000 ECN-KOMPLETTERA-TPO2 SECTION.                                            
226100     MOVE 'ECN-KOMPLETTERA-'     TO CURRENT-SECTION                       
226200                                                                          
226300     PERFORM S20-HAMTA-WDB6-INFO                                          
226400                                                                          
226500     IF DCS-SDC             AND                                           
226600        ORAD-KDORDKL  = 1   AND                                           
226700        ORAD-IDKAMPRF = 0   AND                                           
226800       (AREG-KDUART   = 'L'  OR  AREG-KDUART = 'P')                       
226900                                                                          
227000        CONTINUE                                                          
227100     ELSE                                                                 
227200       IF ALLT-OK AND (DCS-CDC OR DCS-CDC-TR OR DCS-SDC)                  
227300         MOVE ORAD-IDDISTR       TO TPO2-IDDISTR                          
227400         MOVE ORAD-IDKUNDNR      TO TPO2-IDKUNDNR                         
227500         MOVE ORAD-IDKUNDRF      TO TPO2-IDKUNDRF                         
227600         MOVE ORAD-IDARTNR       TO TPO2-IDARTNR                          
227700         MOVE ORAD-BERADREF      TO TPO2-BERADREF                         
227800         MOVE AREG-IDANSK        TO TPO2-IDANSK                           
227900         MOVE OHUV-IDKONTO       TO TPO2-IDKONTO                          
228000         MOVE OHUV-IDKST         TO TPO2-IDKST                            
228100         MOVE OHUV-IDANALYS      TO TPO2-IDANALYS                         
228200         MOVE ORAD-KDDSP         TO TPO2-KDDSP                            
228300         MOVE OHUV-KDFAKTYP      TO TPO2-KDFAKTYP                         
228400         MOVE ARB-KDFRAKT        TO TPO2-KDFRAKT                          
228500         MOVE ORAD-KDKVBRYT      TO TPO2-KDKVBRYT                         
228600         MOVE ORAD-KDORDING      TO TPO2-KDORDING                         
228700         MOVE OHUV-KDORDKL       TO TPO2-KDORDKL                          
228800         MOVE AREG-KDPRODSL      TO TPO2-KDPRODSL                         
228900         MOVE ORAD-KDVRINFO      TO TPO2-KDVRINFO                         
229000         MOVE ORAD-KVBEART-Q     TO TPO2-KVBEART-Q                        
229100         MOVE AREG-REKSIFFR      TO TPO2-REKSIFFR                         
229200         MOVE ORAD-TITPO         TO TPO2-TITPO                            
229300         MOVE ORAD-PRARTNTO      TO TPO2-PRARTNTO                         
229400         MOVE ORAD-DEAL-PR-LINE  TO TPO2-DEAL-PR-LINE                     
229500         MOVE ORAD-KDPRTYP       TO TPO2-KDPRTYP                          
229600         MOVE ORAD-FLPRTILL      TO TPO2-FLPRTILL                         
229700         MOVE ORAD-BEVOLREF      TO TPO2-BEVOLREF                         
229800         MOVE ORAD-IDKAMPRF      TO TPO2-IDKAMPRF                         
229900         MOVE ORAD-IDSYSTEM      TO TPO2-IDSYSTEM                         
230000         MOVE ORAD-FLINVEST      TO TPO2-FLINVEST                         
230100         MOVE OHUV-FLORDSPE      TO TPO2-FLORDSPE                         
230200         MOVE OHUV-FLOVRLEV      TO TPO2-FLOVRLEV                         
230300         MOVE OHUV-FLFORBI       TO TPO2-FLFORBI                          
230400         MOVE ORAD-IDLEVNR       TO TPO2-IDLEVNR                          
230500         MOVE AREG-KDUART        TO TPO2-KDUART                           
230600         MOVE AREG-KVFRYSTI      TO TPO2-KVFRYSTI                         
230700         MOVE +1                 TO TPO2-KDORDBEH                         
230800         MOVE ORAD-KDTPOTYP      TO TPO2-KDTPOTYP                         
230900         MOVE OHUV-BEKUNDRF      TO TPO2-BEKUNDRF                         
231000         MOVE ORAD-FLTILLK       TO TPO2-FLTILLK                          
231100         MOVE AREG-TIDISPIN      TO TPO2-TIDISPIN                         
231200         MOVE OHUV-BEVARREF      TO TPO2-BEVARREF                         
231300         MOVE KVAN-KVQPACK-UT    TO TPO2-KVQPACK-1                        
231400         MOVE ORAD-KVBEART       TO TPO2-KVBEART                          
231500                                                                          
231600         MOVE +0                 TO TPO2-KDORDBEK                         
231700         MOVE SPACE              TO TPO2-FLKLAR                           
231800                                                                          
231900         MOVE OHUV-KDORDTYP-LDC  TO TPO2-KDORDTYP-LDC                     
232000         MOVE OHUV-TIREPDAT      TO TPO2-TIREPDAT                         
232100         MOVE ORAD-IDKUNDRF-WIP  TO TPO2-IDKUNDRF-WIP                     
232200                                                                          
232300         CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                  
232400                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
232500                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
232600                           TIME-4437-PCB                                  
232700                                                                          
232800         IF TPO2-KDORDBEK > +0                                            
232900            MOVE JA              TO OBKR-SW                               
233000            MOVE NEJ             TO ALLT-SW                               
233100            MOVE WC-CDC-SE       TO ORAD-IDDC                             
233200            MOVE ORAD-IDDC       TO WS-IDDC                               
233300            MOVE TPO2-KDTPOTYP   TO ORAD-KDTPOTYP                         
233400            IF ORAD-KDTPOTYP = 6                                          
233500               IF ORAD-KDPRTYP NOT = 'P'                                  
233600                  MOVE ZERO      TO ORAD-PRARTNTO                         
233700                  MOVE SPACE     TO ORAD-KDPRTYP                          
233800****   OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                    
233900                  IF NOT DIST79-DEALER-PRICE AND                          
234100                     NOT DIST79-ECOM-PRICE                                
234200                    MOVE ZERO      TO ORAD-PRARTNTO-LOC                   
234300                    MOVE NEJ       TO ORAD-FLPRTILL                       
234400                  END-IF                                                  
234500*************                                                             
234600               END-IF                                                     
234700            END-IF                                                        
234800         ELSE                                                             
234900            IF TPO2-FLKLAR = JA                                           
235000               MOVE NEJ          TO ALLT-SW                               
235100               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
235200                  MOVE ZERO      TO KERS-KDORDBEK                         
235300                  PERFORM S02-RENSA-TILLK-TAB                             
235400               END-IF                                                     
235500            END-IF                                                        
235600         END-IF                                                           
235700       END-IF                                                             
235800     END-IF                                                               
235900     .                                                                    
236000                                                                          
236100 ECO-KOMPLETTERA-KAMPANJER SECTION.                                       
236200     MOVE 'ECO-KOMPLETTERA-'   TO CURRENT-SECTION                         
236300                                                                          
236400     PERFORM S20-HAMTA-WDB6-INFO                                          
236500                                                                          
236600     IF ALLT-OK AND (DCS-CDC OR  DCS-CDC-TR OR DCS-SDC)                   
236700       MOVE ORAD-IDDISTR       TO KAMP-IDDISTR                            
236800       MOVE ORAD-IDKUNDNR      TO KAMP-IDKUNDNR                           
236900       MOVE ORAD-IDKUNDRF      TO KAMP-IDKUNDRF                           
237000       MOVE ORAD-IDARTNR       TO KAMP-IDARTNR                            
237100       MOVE ORAD-BERADREF      TO KAMP-BERADREF                           
237200       MOVE AREG-IDANSK        TO KAMP-IDANSK                             
237300       MOVE OHUV-IDKONTO       TO KAMP-IDKONTO                            
237400       MOVE OHUV-IDANALYS      TO KAMP-IDANALYS                           
237500       MOVE OHUV-IDKST         TO KAMP-IDKST                              
237600       MOVE ORAD-KDDSP         TO KAMP-KDDSP                              
237700       MOVE OHUV-KDFAKTYP      TO KAMP-KDFAKTYP                           
237800       MOVE ARB-KDFRAKT        TO KAMP-KDFRAKT                            
237900       MOVE ORAD-KDKVBRYT      TO KAMP-KDKVBRYT                           
238000       MOVE ORAD-KDORDING      TO KAMP-KDORDING                           
238100       MOVE OHUV-KDORDKL       TO KAMP-KDORDKL                            
238200       MOVE AREG-KDPRODSL      TO KAMP-KDPRODSL                           
238300       MOVE ORAD-KDVRINFO      TO KAMP-KDVRINFO                           
238400       MOVE ORAD-KVBEART-Q     TO KAMP-KVBEART-Q                          
238500       MOVE AREG-REKSIFFR      TO KAMP-REKSIFFR                           
238600       MOVE ORAD-TITPO         TO KAMP-TITPO                              
238700       IF ORAD-KDPRTYP = 'P' OR ORAD-KDTPOTYP = +0                        
238800          MOVE ORAD-PRARTNTO   TO KAMP-PRARTNTO                           
238900          MOVE ORAD-DEAL-PR-LINE                                          
239000                               TO KAMP-DEAL-PR-LINE                       
239100          MOVE ORAD-KDPRTYP    TO KAMP-KDPRTYP                            
239200          MOVE ORAD-FLPRTILL   TO KAMP-FLPRTILL                           
239300       ELSE                                                               
239400          MOVE ORAD-DEAL-PR-LINE                                          
239500                               TO KAMP-DEAL-PR-LINE                       
239600          MOVE ZERO            TO KAMP-PRARTNTO                           
239700          MOVE SPACE           TO KAMP-KDPRTYP                            
239800          MOVE NEJ             TO KAMP-FLPRTILL                           
239900       END-IF                                                             
240000       MOVE ORAD-BEVOLREF      TO KAMP-BEVOLREF                           
240100       MOVE ORAD-FLINVEST      TO KAMP-FLINVEST                           
240200       MOVE OHUV-BEKUNDRF      TO KAMP-BEKUNDRF                           
240300       MOVE ORAD-IDKAMPRF      TO KAMP-IDKAMPRF                           
240400       MOVE ORAD-IDDC          TO KAMP-IDDC                               
240500       MOVE ORAD-IDLEVNR       TO KAMP-IDLEVNR                            
240600       MOVE ORAD-IDSYSTEM      TO KAMP-IDSYSTEM                           
240700       MOVE AREG-KVFRYSTI      TO KAMP-KVFRYSTI                           
240800       MOVE ORAD-KDTPOTYP      TO KAMP-KDTPOTYP                           
240900       MOVE OHUV-FLFORBI       TO KAMP-FLFORBI                            
241000       MOVE OHUV-FLORDSPE      TO KAMP-FLORDSPE                           
241100       MOVE OHUV-FLOVRLEV      TO KAMP-FLOVRLEV                           
241200                                                                          
241300       MOVE +0                 TO KAMP-KDORDBEK                           
241400       MOVE SPACE              TO KAMP-FLKLAR                             
241500                                                                          
241600       CALL W411KAMP USING KAMP-W411KAMP KAMP-ORDP-PCB                    
241700                           KAMP-ZZAC-PCB KAMP-WDM2-PCB                    
241800                                                                          
241900       IF KAMP-KDORDBEK > +0                                              
242000          MOVE JA              TO OBKR-SW                                 
242100          MOVE NEJ             TO ALLT-SW                                 
242200          MOVE WC-CDC-SE       TO ORAD-IDDC                               
242300          MOVE ORAD-IDDC       TO WS-IDDC                                 
242400          IF KVAN-KDORDBEK-UT > +0                                        
242500             MOVE +0           TO KVAN-KDORDBEK-UT                        
242600             MOVE ORAD-KVBEART TO ORAD-KVBEART-Q                          
242700          END-IF                                                          
242800          IF DLEV-KDORDBEK-UT > +0                                        
242900             MOVE +0           TO DLEV-KDORDBEK-UT                        
243000          END-IF                                                          
243100       ELSE                                                               
243200          IF KAMP-FLKLAR = JA                                             
243300             MOVE NEJ          TO ALLT-SW                                 
243400             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
243500                MOVE ZERO      TO KERS-KDORDBEK                           
243600                PERFORM S02-RENSA-TILLK-TAB                               
243700             END-IF                                                       
243800          END-IF                                                          
243900       END-IF                                                             
244000                                                                          
244100     END-IF                                                               
244200     .                                                                    
244300                                                                          
244400 ECT-KOMPLETTERA-RELEASESPARR SECTION.                                    
244500     MOVE 'ECT-KOMPLETTERA-'       TO CURRENT-SECTION                     
244600                                                                          
244700       IF ALLT-OK AND W-KDORDBEK = 56                                     
244800         MOVE ORAD-IDDISTR         TO RELS-IDDISTR                        
244900         MOVE ORAD-IDKUNDNR        TO RELS-IDKUNDNR                       
245000         MOVE ORAD-IDKUNDRF        TO RELS-IDKUNDRF                       
245100         MOVE ORAD-IDARTNR         TO RELS-IDARTNR                        
245200         MOVE ORAD-BERADREF        TO RELS-BERADREF                       
245300         MOVE AREG-IDANSK          TO RELS-IDANSK                         
245400         MOVE OHUV-IDKONTO         TO RELS-IDKONTO                        
245500         MOVE OHUV-IDKST           TO RELS-IDKST                          
245600         MOVE OHUV-IDANALYS        TO RELS-IDANALYS                       
245700         MOVE ORAD-KDDSP           TO RELS-KDDSP                          
245800         MOVE OHUV-KDFAKTYP        TO RELS-KDFAKTYP                       
245900         MOVE ARB-KDFRAKT          TO RELS-KDFRAKT                        
246000         MOVE ORAD-KDKVBRYT        TO RELS-KDKVBRYT                       
246100         MOVE ORAD-KDORDING        TO RELS-KDORDING                       
246200         MOVE OHUV-KDORDKL         TO RELS-KDORDKL                        
246300         MOVE AREG-KDPRODSL        TO RELS-KDPRODSL                       
246400         MOVE ORAD-KDVRINFO        TO RELS-KDVRINFO                       
246500         MOVE ORAD-KVBEART-Q       TO RELS-KVBEART-Q                      
246600         MOVE AREG-REKSIFFR        TO RELS-REKSIFFR                       
246700         MOVE MSGI-TILOKDAT        TO RELS-TITPO                          
246800         MOVE ORAD-PRARTNTO        TO RELS-PRARTNTO                       
246900         MOVE ORAD-PRAVCOST        TO RELS-PRAVCOST                       
247000         MOVE ORAD-DEAL-PR-LINE    TO RELS-DEAL-PR-LINE                   
247100         MOVE ORAD-KDPRTYP         TO RELS-KDPRTYP                        
247200         MOVE ORAD-FLPRTILL        TO RELS-FLPRTILL                       
247300                                                                          
247400         MOVE ORAD-BEVOLREF        TO RELS-BEVOLREF                       
247500         MOVE ORAD-IDKAMPRF        TO RELS-IDKAMPRF                       
247600         MOVE ORAD-IDSYSTEM        TO RELS-IDSYSTEM                       
247700         MOVE ORAD-FLINVEST        TO RELS-FLINVEST                       
247800         MOVE OHUV-FLORDSPE        TO RELS-FLORDSPE                       
247900         MOVE OHUV-FLOVRLEV        TO RELS-FLOVRLEV                       
248000         MOVE OHUV-FLFORBI         TO RELS-FLFORBI                        
248100         MOVE ORAD-IDLEVNR         TO RELS-IDLEVNR                        
248200         MOVE AREG-KDUART          TO RELS-KDUART                         
248300         MOVE AREG-KVFRYSTI        TO RELS-KVFRYSTI                       
248400         MOVE +1                   TO RELS-KDORDBEH                       
248500         MOVE W-KDTPOTYP           TO RELS-KDTPOTYP                       
248600         MOVE OHUV-BEKUNDRF        TO RELS-BEKUNDRF                       
248700         MOVE ORAD-FLTILLK         TO RELS-FLTILLK                        
248800         MOVE AREG-TIDISPIN        TO RELS-TIDISPIN                       
248900         MOVE OHUV-BEVARREF        TO RELS-BEVARREF                       
249000         MOVE 0                    TO RELS-KVQPACK-1                      
249100         MOVE ORAD-KVBEART         TO RELS-KVBEART                        
249200         MOVE W-KDORDBEK           TO RELS-KDORDBEK                       
249300                                                                          
249400         MOVE SPACE                TO RELS-FLKLAR                         
249500                                                                          
249600         MOVE OHUV-KDORDTYP-LDC    TO RELS-KDORDTYP-LDC                   
249700         MOVE OHUV-TIREPDAT        TO RELS-TIREPDAT                       
249800         MOVE ORAD-IDKUNDRF-WIP    TO RELS-IDKUNDRF-WIP                   
249900                                                                          
250000                                                                          
250100         CALL W411RELS USING RELS-W411RELS RELS-ORDP-PCB                  
250200                             RELS-FILA-PCB RELS-ARTM-PCB 2109-PCB         
250300                                                                          
250400         PERFORM ECTA-ANDRA-WDC711                                        
250500         IF RELS-KDORDBEK > +0                                            
250600            MOVE JA                TO OBKR-SW                             
250700            MOVE NEJ               TO ALLT-SW                             
250800            MOVE WC-CDC-SE         TO ORAD-IDDC                           
250900            MOVE ORAD-IDDC         TO WS-IDDC                             
251000         ELSE                                                             
251100            IF RELS-FLKLAR = JA                                           
251200               MOVE NEJ            TO ALLT-SW                             
251300               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
251400                  MOVE ZERO        TO KERS-KDORDBEK                       
251500                  PERFORM S02-RENSA-TILLK-TAB                             
251600               END-IF                                                     
251700            END-IF                                                        
251800         END-IF                                                           
251900                                                                          
252000       MOVE +0                   TO W-KDORDBEK                            
252100       MOVE SPACE                TO RELS-FLKLAR                           
252200                                                                          
252300       END-IF                                                             
252400     .                                                                    
252500                                                                          
252600 ECTA-ANDRA-WDC711 SECTION.                                               
252700     MOVE 'ECT-A-ANDRA-WDC7'       TO CURRENT-SECTION                     
252800                                                                          
252900     IF W-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                         
253000       IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                        
253100         INITIALIZE PRQU-W335PRQU                                         
253200         MOVE ORAD-IDDISTR       TO PRQU-IDDISTR                          
253300         MOVE ORAD-IDKUNDNR      TO PRQU-IDKUNDNR                         
253400         MOVE ORAD-IDKUNDRF      TO PRQU-IDKUNDRF                         
253500         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
253600         MOVE 6                  TO PRQU-KDCALL                           
253700         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
253800                                            PRQU-WDC7-PCB                 
253900                                            PRQU-SJKO-WDK6-PCB            
254000         MOVE 'N'                TO ORAD-FLPRTILL                         
254100       END-IF                                                             
254200     END-IF                                                               
254300     .                                                                    
254400                                                                          
254500 ECG-PREL-AVBOKNING-XDC SECTION.                                          
254600     MOVE 'ECG-PREL-AVBOKN7'       TO CURRENT-SECTION                     
254700                                                                          
254800     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
254900                                                                          
255000       PERFORM S20-HAMTA-WDB6-INFO                                        
255100                                                                          
255200       IF DCS-NDC                                                         
255300         MOVE JA TO ALLT-SW                                               
255400         PERFORM ECGX-PREL-AVBOKNING-XDC                                  
255500*        MOVE OHUV-FLFORBI         TO NDCA-FLFORBI                        
255600*        MOVE GMT-FLLDCKND         TO NDCA-FLLDCKND                       
255700*        MOVE OHUV-FLPRELRO        TO NDCA-FLPRELRO                       
255800*        MOVE OHUV-FLRESTN         TO NDCA-FLRESTN                        
255900*        MOVE OHUV-FLORDSPE        TO NDCA-FLORDSPE                       
256000*        MOVE ORAD-IDARTNR         TO NDCA-IDARTNR                        
256100*        MOVE ORAD-IDDC            TO NDCA-IDDC                           
256200*        MOVE OHUV-IDDC-CLEAR-GRP  TO NDCA-IDDC-CLEAR-GRP                 
256300*        MOVE ORAD-CLEARGROUP      TO NDCA-CLEARGROUP                     
256400*        MOVE OHUV-IDDC-PRIM       TO NDCA-IDDC-TVS                       
256500*        MOVE ORAD-IDDISTR         TO NDCA-IDDISTR                        
256600*        MOVE WS-IXDCCLEAR         TO NDCA-IXDCCLEAR                      
256700*        MOVE ORAD-KDARTURS        TO NDCA-KDARTURS                       
256800*        MOVE AREG-KDERS           TO NDCA-KDERS                          
256900*        MOVE ORAD-KDORDING        TO NDCA-KDORDING                       
257000*        MOVE ORAD-KDORDKL         TO NDCA-KDORDKL                        
257100*        MOVE AREG-KDSORT          TO NDCA-KDSORT                         
257200*        MOVE OHUV-KVDAGAR-DOW     TO NDCA-KVDAGAR-DOW                    
257300*        MOVE ORAD-KVBEART-Q       TO NDCA-KVBEART-Q                      
257400*        MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                      
257500*        MOVE ORAD-TIREGDAT        TO NDCA-TIREGDAT                       
257600*        MOVE ORAD-TIREGTID        TO NDCA-TIREGTID                       
257700*        MOVE ORAD-VKART           TO NDCA-VKART                          
257800*        MOVE ORAD-VKART-NTO       TO NDCA-VKART-NTO                      
257900*        MOVE ORAD-VLARTNTO        TO NDCA-VLARTNTO                       
258000*        MOVE +2                   TO NDCA-KDCALL                         
258100*        MOVE SPACE                TO NDCA-XDK7-IDDC                      
258200*        MOVE ZERO                 TO NDCA-XDK7-KDIDDC                    
258300*                                     NDCA-XDK7-KVOKS-DAG                 
258400*                                     NDCA-XDK7-KVOKS-BULK                
258500*                                                                         
258600*        CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                  
258700*                                          NDCA-USEA-PCB                  
258800*                                          NDCA-WDK7-PCB                  
258900*                                          NDCA-WDL6-PCB                  
259000*                                          NDCA-WDB6-PCB                  
259100*                                          NDCA-XDK7-W411XDK7             
259200*                                                                         
259300*        PERFORM ECGX-CHECK-DIFF                                          
259400         IF XDCA-KDORDBEK > ZERO                                          
259500           IF SPAR-FLPUBCDC = YES                                         
259600*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR CDC PUBL.DATE.           
259700*** IF XDCA-KDORDBEK = 15 MEANS LINE IS MOVED TO ANOTHER DC               
259800*** THEN DAPUBL IS TESTED OK IN W411XDCA                                  
259900              IF (XDCA-KDORDBEK = 15 OR 92 OR 98 OR 99)                   
260000                 AND XDCA-DAPUBL > ZERO                                   
260100                 MOVE ZERO TO SPAR-KDORDBEK                               
260200              ELSE                                                        
260300                 MOVE ZERO TO XDCA-KDORDBEK                               
260400              END-IF                                                      
260500           END-IF                                                         
260600           IF KOLLA-ERS                                                   
260700              IF XDCA-KVPREAVB > 0                                        
260800                MOVE ZERO            TO KERS-KDORDBEK                     
260900                PERFORM S02-RENSA-TILLK-TAB                               
261000                MOVE ZERO            TO SPAR-KDORDBEK                     
261100              ELSE                                                        
261200                MOVE ZERO            TO XDCA-KDORDBEK                     
261300              END-IF                                                      
261400           ELSE                                                           
261500             IF XDCA-KDORDBEK = 15                                        
261600                IF SDCA-KDORDBEK-FIRST-SDC = 15                           
261700                   MOVE ZERO         TO SDCA-KDORDBEK-FIRST-SDC           
261800                END-IF                                                    
261900                IF SDCA-KDORDBEK-SECOND-SDC = 15                          
262000                   MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC          
262100                END-IF                                                    
262200                IF SDCA-KDORDBEK = 15                                     
262300                   MOVE ZERO         TO SDCA-KDORDBEK                     
262400                END-IF                                                    
262500             END-IF                                                       
262600             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
262700                 MOVE ZERO           TO XDCA-KDORDBEK                     
262800             END-IF                                                       
262900           END-IF                                                         
263000           MOVE JA                   TO OBKR-SW                           
263100         ELSE                                                             
263200           IF KOLLA-ERS    OR                                             
263300             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
263400             IF XDCA-KVPREAVB > 0                                         
263500               MOVE ZERO             TO KERS-KDORDBEK                     
263600               PERFORM S02-RENSA-TILLK-TAB                                
263700               MOVE ZERO             TO SPAR-KDORDBEK                     
263800             ELSE                                                         
263900               MOVE JA               TO OBKR-SW                           
264000             END-IF                                                       
264100           ELSE                                                           
264110            IF XDCA-KVPREAVB > 0 AND SPAR-KDORDBEK ='54'                  
264120               MOVE ZERO             TO SPAR-KDORDBEK                     
264130            END-IF                                                        
264140           END-IF                                                         
264200           IF XDCA-DAPUBL > 0 AND SPAR-FLPUBCDC = YES                     
264300*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR PUBL.DATE ON CDC         
264400*** THAT BLOCK SHALL BE REMOVED IF CHECK ON DAPUBL FOR NDC IS OK.         
264500              MOVE 0  TO SPAR-KDORDBEK                                    
264600              MOVE JA  TO ALLT-SW                                         
264700              MOVE NEJ TO OBKR-SW                                         
264800           END-IF                                                         
264900         END-IF                                                           
265000         MOVE XDCA-ADLAGOMR          TO ORAD-ADLAGOMR                     
265100         MOVE XDCA-ADGANG            TO ORAD-ADGANG                       
265200         MOVE XDCA-ADPLATS           TO ORAD-ADPLATS                      
265300         MOVE XDCA-IDDC-OUT          TO ORAD-IDDC                         
265400         MOVE XDCA-IDDC-RO           TO ORAD-IDDC-RO                      
265500         MOVE XDCA-KDARTURS          TO ORAD-KDARTURS                     
265600         MOVE XDCA-KDOI              TO ORAD-KDOI                         
265700         MOVE XDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
265800         MOVE XDCA-KVPREAVB          TO ORAD-KVPREAVB                     
265900         MOVE XDCA-KVPRERO           TO ORAD-KVPRERO                      
266000         MOVE XDCA-TIREGDAT-OUT      TO ORAD-TIREGDAT                     
266100         MOVE XDCA-TIREGTID-OUT      TO ORAD-TIREGTID                     
266200         MOVE XDCA-VKART-OUT         TO ORAD-VKART                        
266300         MOVE XDCA-VKART-NTO         TO ORAD-VKART-NTO                    
266400         MOVE XDCA-VLARTNTO          TO ORAD-VLARTNTO                     
266500         MOVE NEJ                    TO ALLT-SW                           
266600       END-IF                                                             
266700                                                                          
266800     END-IF                                                               
266900     .                                                                    
267000                                                                          
267100 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
267200                                                                          
267300     MOVE 'STA ECGX-XDC      '       TO   CURRENT-SECTION                 
267400     IF ALLT-OK OR KOLLA-ERS                                              
267500                                                                          
267600       PERFORM S20-HAMTA-WDB6-INFO                                        
267700                                                                          
267800       IF DCS-NDC                                                         
267900                                                                          
268000* XDCA-INPUT                                                              
268100         PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                          
268200           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
268300                                   TO XDCA-IDDC-CLEAR-IN(WS-INDEX)        
268400           ADD +1 TO WS-INDEX                                             
268500         END-PERFORM                                                      
268600         MOVE OHUV-IDDC-PRIM       TO XDCA-IDDC-TVS                       
268700         MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                       
268800         MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                        
268900         MOVE ORAD-IDDC            TO XDCA-IDDC                           
269000         MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                        
269100         MOVE AREG-KDERS           TO XDCA-KDERS                          
269200         MOVE AREG-KDSORT          TO XDCA-KDSORT                         
269300         MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                      
269400         MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                      
269500         MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                       
269600         MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                       
269700         MOVE ORAD-VKART           TO XDCA-VKART                          
269800         MOVE +1                   TO XDCA-KDCALL                         
269900                                                                          
270000* XDCA-OUTPUT                                                             
270100         MOVE SPACE                TO XDCA-IDDC-OUT                       
270200                                      XDCA-IDDC-RO                        
270300                                      XDCA-KDARTURS                       
270400                                      XDCA-KDOI                           
270500                                      XDCA-CLEARGROUP                     
270600         MOVE ZERO                 TO XDCA-ADLAGOMR                       
270700                                      XDCA-ADGANG                         
270800                                      XDCA-ADPLATS                        
270900                                      XDCA-KDORDBEK                       
271000                                      XDCA-KVPREAVB                       
271100                                      XDCA-KVPRERO                        
271200                                      XDCA-TIREGDAT-OUT                   
271300                                      XDCA-TIREGTID-OUT                   
271400                                      XDCA-VKART-OUT                      
271500                                      XDCA-VKART-NTO                      
271600                                      XDCA-VLARTNTO                       
271700         MOVE ZERO                 TO                                     
271800                                      XDCA-KVOKS-DAG                      
271900                                      XDCA-KVOKS-BULK                     
272000                                                                          
272100         IF XDCA-DAPUBL NOT = 99999999                                    
272200            MOVE ZERO              TO XDCA-DAPUBL                         
272300         END-IF                                                           
272400                                                                          
272500         CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                    
272600         XDCA-USEA-PCB                                                    
272700         XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                        
272800         XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                       
272900         XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                        
273000         XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                  
273100         XDCA-WDK7-3-PCB                                                  
273200                                                                          
273300* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
273400* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
273500         IF XDCA-KDARTURS = SPACE                                         
273600           MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                           
273700         END-IF                                                           
273800         IF XDCA-VKART-NTO = ZERO                                         
273900           MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                          
274000         END-IF                                                           
274100         IF XDCA-VLARTNTO = ZERO                                          
274200           MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                           
274300         END-IF                                                           
274400       END-IF                                                             
274500     END-IF                                                               
274600     .                                                                    
274700     EJECT                                                                
274800 ECGX-CHECK-DIFF SECTION.                                                 
274900                                                                          
275000     MOVE 'CHECK-DIFF       '              TO   CURRENT-SECTION           
275100     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
275200     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
275300     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
275400     AND NDCA-ADGANG     = XDCA-ADGANG                                    
275500     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
275600     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
275700     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
275800     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
275900     AND NDCA-KDOI       = XDCA-KDOI                                      
276000     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
276100     AND NDCA-VKART      = XDCA-VKART-OUT                                 
276200     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
276300     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
276400     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
276500     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
276600     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
276700     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
276800         MOVE NEJ TO DIFF-FLSVAR                                          
276900     ELSE                                                                 
277000        MOVE JA           TO DIFF-FLSVAR                                  
277100     END-IF                                                               
277200                                                                          
277300* ORDER LOG INFO                                                          
277400     IF DIFF-FLSVAR = JA                                                  
277500       MOVE IDPGM         TO FIL-IDPGM                                    
277600       ACCEPT FIL-TIREGDAT FROM DATE                                      
277700       ACCEPT FIL-TIKLOCK FROM TIME                                       
277800       MOVE 1             TO FIL-IDSEKVNR                                 
277900       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
278000       MOVE 'A'           TO FIL-CT-IDVTYP                                
278100       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
278200                                                                          
278300*   ORDER LINE INFO                                                       
278400       MOVE OHUV-IDDISTR   TO DIFF-IDDISTR                                
278500       MOVE OHUV-IDKUNDNR  TO DIFF-IDKUNDNR                               
278600       MOVE OHUV-IDORDNR7  TO DIFF-IDORDNR5                               
278700       MOVE OHUV-KDORDKL   TO DIFF-KDORDKL                                
278800       MOVE ORAD-IDARTNR   TO DIFF-IDARTNR                                
278900       MOVE ORAD-KVBEART-Q TO DIFF-KVBEART-Q                              
279000       MOVE ORAD-IDDC      TO DIFF-IDDC                                   
279100       MOVE '4206'         TO DIFF-IDSYSTEM                               
279200                                                                          
279300*   NDCA INFO                                                             
279400       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
279500       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
279600       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
279700       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
279800       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
279900       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
280000       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
280100       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
280200       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
280300       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
280400       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
280500       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
280600       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
280700       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
280800       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
280900       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
281000                                                                          
281100*   XDCA INFO                                                             
281200       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
281300       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
281400       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
281500       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
281600       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
281700       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
281800       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
281900       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
282000       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
282100       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
282200       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
282300       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
282400       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
282500       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
282600       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
282700       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
282800                                                                          
282900       PERFORM IMS-ISRT-WDR601                                            
283000       IF SEGMENT-FINNS-REDAN                                             
283100          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
283200             ADD 1 TO FIL-IDSEKVNR                                        
283300             PERFORM IMS-ISRT-WDR601                                      
283400          END-PERFORM                                                     
283500       END-IF                                                             
283600     END-IF                                                               
283700     .                                                                    
283800     EJECT                                                                
283900 ECW-PREL-AVBOKNING-SDC1 SECTION.                                         
284000     MOVE 'ECW-PREL-AVBOKNI'       TO CURRENT-SECTION                     
284100                                                                          
284200     IF ALLT-OK OR KOLLA-ERS                                              
284300                                                                          
284400       PERFORM S20-HAMTA-WDB6-INFO                                        
284500                                                                          
284600       IF DCS-SDC                                                         
284700         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
284800         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
284900         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
285000         MOVE NEJ                  TO SDCA-FLORDSPE                       
285100         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
285200         MOVE ORAD-IDDC            TO SDCA-IDDC                           
285300         MOVE SPACE                TO SDCA-IDDC-TVS                       
285400         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
285500         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
285600         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
285700         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
285800         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
285900         MOVE AREG-KDSORT          TO SDCA-KDSORT                         
286000         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
286100         MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                      
286200         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
286300         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
286400         MOVE +0                   TO SDCA-TIREPDAT                       
286500         MOVE +0                   TO SDCA-KVOKS-PREL                     
286600         MOVE +1                   TO SDCA-KDCALL                         
286700         MOVE +1                   TO SDCA-IXDCCLEAR                      
286800                                                                          
286900         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
287000                                           SDCA-WDB6-PCB                  
287100                                           SDCA-WDK9-PCB                  
287200                                           SDCA-WDR6-PCB                  
287300                                           SDCA-WDK6-PCB                  
287400                                           SDCA-WDQ4B-PCB                 
287500                                           SDCA-WDQ2-PCB                  
287600                                           SDCA-WDQ4-PCB                  
287700                                           SDCA-WDB6-2-PCB                
287800                                           SDCA-WDK6-2-PCB                
287900                                           SDCA-WDK7-2-PCB                
288000                                           SDCA-WDK7-3-PCB                
288100                                                                          
288200         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-FIRST-SDC                    
288300         MOVE ZERO          TO SDCA-KDORDBEK                              
288400                                                                          
288500         IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                
288600           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-FIRST-SDC = 80         
288700*            IF OHUV-IDDC-CLEAR(2) > '19'                                 
288800*              MOVE ZERO               TO SDCA-KDORDBEK-FIRST-SDC         
288900*              MOVE OHUV-IDDC-CLEAR(2) TO ORAD-IDDC                       
289000*              MOVE ORAD-IDDC          TO WS-IDDC                         
289100*              IF SDCA-KDOI = 'XX'                                        
289200*                 MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)             
289300*              END-IF                                                     
289400*            ELSE                                                         
289500               MOVE JA        TO OBKR-SW                                  
289600               MOVE NEJ       TO ALLT-SW                                  
289700*            END-IF                                                       
289800           ELSE                                                           
289900             IF ORAD-KDORDKL > 0                                          
290000             AND ORAD-IDSYSTEM NOT = 'OREL'                               
290100             AND (AREG-KDUART = 'L'                                       
290200             OR AREG-KDUART = 'P')                                        
290300             AND OHUV-FLORDSPE NOT = JA                                   
290400             AND OHUV-FLOVRLEV NOT = JA                                   
290500             AND OHUV-FLFORBI = NEJ                                       
290600                MOVE ZERO    TO SDCA-KDORDBEK-FIRST-SDC                   
290700                MOVE 70      TO TPO2-KDORDBEK                             
290800                MOVE JA      TO OBKR-SW                                   
290900                MOVE NEJ     TO ALLT-SW                                   
291000                MOVE WC-CDC-SE TO ORAD-IDDC                               
291100                MOVE ORAD-IDDC TO WS-IDDC                                 
291200                MOVE 6       TO ORAD-KDTPOTYP                             
291300                IF ORAD-KDPRTYP NOT = 'P'                                 
291400                   MOVE ZERO  TO ORAD-PRARTNTO                            
291500                   MOVE SPACE TO ORAD-KDPRTYP                             
291600**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
291700                   IF NOT DIST79-DEALER-PRICE AND                         
291900                      NOT DIST79-ECOM-PRICE                               
292000                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
292100                     MOVE NEJ         TO ORAD-FLPRTILL                    
292200                   END-IF                                                 
292300*************                                                             
292400                END-IF                                                    
292500             ELSE                                                         
292600               IF KOLLA-ERS                                               
292700*                 MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC         
292800                  IF SPAR-KDORDBEK = ZERO                                 
292900                   IF ORAD-IDDC = W-TILLK-DC                              
293000                      MOVE JA            TO OBKR-SW                       
293100                      MOVE NEJ           TO ALLT-SW                       
293200                                            KOLLA-ERS-SW                  
293300                      MOVE ZERO      TO SDCA-KDORDBEK-FIRST-SDC           
293400                   ELSE                                                   
293500                    MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                 
293600                    MOVE ORAD-IDDC     TO WS-IDDC                         
293700                    IF WS-IDDC = WC-CDC-SE                                
293800                       MOVE JA              TO CDC-MOVE-SW                
293900                    END-IF                                                
294000                   END-IF                                                 
294100                  ELSE                                                    
294200                    MOVE NEJ           TO KOLLA-ERS-SW                    
294300                  END-IF                                                  
294400               ELSE                                                       
294500                 IF SDCA-KDORDBEK-FIRST-SDC = 15                          
294600                    MOVE JA            TO OBKR-SW                         
294700                    MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                 
294800                    MOVE ORAD-IDDC     TO WS-IDDC                         
294900                 ELSE                                                     
295000                    MOVE JA            TO OBKR-SW                         
295100                    MOVE NEJ           TO ALLT-SW                         
295200                 END-IF                                                   
295300               END-IF                                                     
295400             END-IF                                                       
295500           END-IF                                                         
295600         ELSE                                                             
295700           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
295800           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
295900           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
296000           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
296100           MOVE NEJ                  TO ALLT-SW                           
296200           IF KVAN-KDORDBEK-UT = ZERO                                     
296300             MOVE JA                 TO EGET-CL-RAD-SW                    
296400           END-IF                                                         
296500           IF KOLLA-ERS OR                                                
296600             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
296700              MOVE NEJ               TO KOLLA-ERS-SW                      
296800              MOVE ZERO              TO KERS-KDORDBEK                     
296900              PERFORM S02-RENSA-TILLK-TAB                                 
297000              MOVE ZERO              TO SPAR-KDORDBEK                     
297100           END-IF                                                         
297200           IF KERS-KDERS = +19 OR +29                                     
297300              MOVE ZERO           TO SPAR-KDORDBEK                        
297400           END-IF                                                         
297500         END-IF                                                           
297600         MOVE SDCA-KDOI              TO ORAD-KDOI                         
297700         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
297800       END-IF                                                             
297900                                                                          
298000     END-IF                                                               
298100     .                                                                    
298200                                                                          
298300 ECH-PREL-AVBOKNING-SDC2 SECTION.                                         
298400     MOVE 'ECH-PREL-AVBOKNI'       TO CURRENT-SECTION                     
298500                                                                          
298600     IF ALLT-OK OR KOLLA-ERS                                              
298700                                                                          
298800       PERFORM S20-HAMTA-WDB6-INFO                                        
298900                                                                          
299000       IF DCS-SDC                                                         
299100         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
299200         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
299300         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
299400         MOVE NEJ                  TO SDCA-FLORDSPE                       
299500         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
299600         MOVE ORAD-IDDC            TO SDCA-IDDC                           
299700         MOVE SPACE                TO SDCA-IDDC-TVS                       
299800         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
299900         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
300000         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
300100         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
300200         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
300300         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
300400         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
300500         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
300600         MOVE +1                   TO SDCA-KDCALL                         
300700         MOVE +2                   TO SDCA-IXDCCLEAR                      
300800                                                                          
300900         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
301000                                           SDCA-WDB6-PCB                  
301100                                           SDCA-WDK9-PCB                  
301200                                           SDCA-WDR6-PCB                  
301300                                           SDCA-WDK6-PCB                  
301400                                           SDCA-WDQ4B-PCB                 
301500                                           SDCA-WDQ2-PCB                  
301600                                           SDCA-WDQ4-PCB                  
301700                                           SDCA-WDB6-2-PCB                
301800                                           SDCA-WDK6-2-PCB                
301900                                           SDCA-WDK7-2-PCB                
302000                                           SDCA-WDK7-3-PCB                
302100                                                                          
302200         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-SECOND-SDC                   
302300         MOVE ZERO          TO SDCA-KDORDBEK                              
302400                                                                          
302500         IF SDCA-KDORDBEK-SECOND-SDC > ZERO                               
302600           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-SECOND-SDC = 80        
302700*            IF OHUV-IDDC-CLEAR(3) > '19'                                 
302800*              MOVE ZERO               TO SDCA-KDORDBEK-SECOND-SDC        
302900*              MOVE OHUV-IDDC-CLEAR(3) TO ORAD-IDDC                       
303000*              MOVE ORAD-IDDC          TO WS-IDDC                         
303100*              IF SDCA-KDOI = 'XX'                                        
303200*                 MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)             
303300*              END-IF                                                     
303400*            ELSE                                                         
303500               MOVE JA        TO OBKR-SW                                  
303600               MOVE NEJ       TO ALLT-SW                                  
303700*            END-IF                                                       
303800           ELSE                                                           
303900             IF ORAD-KDORDKL > 0                                          
304000             AND ORAD-IDSYSTEM NOT = 'OREL'                               
304100             AND (AREG-KDUART = 'L'                                       
304200             OR AREG-KDUART = 'P')                                        
304300             AND OHUV-FLORDSPE NOT = JA                                   
304400             AND OHUV-FLOVRLEV NOT = JA                                   
304500             AND OHUV-FLFORBI = NEJ                                       
304600                MOVE ZERO    TO SDCA-KDORDBEK-SECOND-SDC                  
304700                MOVE 70      TO TPO2-KDORDBEK                             
304800                MOVE JA      TO OBKR-SW                                   
304900                MOVE NEJ     TO ALLT-SW                                   
305000                MOVE WC-CDC-SE TO ORAD-IDDC                               
305100                MOVE ORAD-IDDC TO WS-IDDC                                 
305200                MOVE 6       TO ORAD-KDTPOTYP                             
305300                IF ORAD-KDPRTYP NOT = 'P'                                 
305400                   MOVE ZERO  TO ORAD-PRARTNTO                            
305500                   MOVE SPACE TO ORAD-KDPRTYP                             
305600**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
305700                   IF NOT DIST79-DEALER-PRICE AND                         
305900                      NOT DIST79-ECOM-PRICE                               
306000                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
306100                     MOVE NEJ         TO ORAD-FLPRTILL                    
306200                   END-IF                                                 
306300*************                                                             
306400                END-IF                                                    
306500             ELSE                                                         
306600               IF KOLLA-ERS                                               
306700                  MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC         
306800*                 MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC        
306900                  IF SPAR-KDORDBEK = ZERO                                 
307000                   IF ORAD-IDDC = W-TILLK-DC                              
307100                      MOVE JA            TO OBKR-SW                       
307200                      MOVE NEJ           TO ALLT-SW                       
307300                                            KOLLA-ERS-SW                  
307400                     MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC        
307500                   ELSE                                                   
307600                    MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                 
307700                    MOVE ORAD-IDDC     TO WS-IDDC                         
307800                    IF WS-IDDC = WC-CDC-SE                                
307900                       MOVE JA              TO CDC-MOVE-SW                
308000                    END-IF                                                
308100                   END-IF                                                 
308200                  ELSE                                                    
308300                    MOVE NEJ           TO KOLLA-ERS-SW                    
308400                  END-IF                                                  
308500               ELSE                                                       
308600                 IF SDCA-KDORDBEK-SECOND-SDC = 15                         
308700                    IF SDCA-KDORDBEK-FIRST-SDC = 15                       
308800                       MOVE ZERO TO SDCA-KDORDBEK-FIRST-SDC               
308900                    END-IF                                                
309000                    MOVE JA            TO OBKR-SW                         
309100                    MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                 
309200                    MOVE ORAD-IDDC     TO WS-IDDC                         
309300                 ELSE                                                     
309400                    MOVE JA            TO OBKR-SW                         
309500                    MOVE NEJ           TO ALLT-SW                         
309600                 END-IF                                                   
309700               END-IF                                                     
309800             END-IF                                                       
309900           END-IF                                                         
310000         ELSE                                                             
310100           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
310200           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
310300           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
310400           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
310500           MOVE NEJ                  TO ALLT-SW                           
310600           IF KVAN-KDORDBEK-UT = ZERO                                     
310700           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
310800             MOVE JA                 TO EGET-CL-RAD-SW                    
310900           END-IF                                                         
311000           IF KOLLA-ERS OR                                                
311100             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
311200              MOVE NEJ               TO KOLLA-ERS-SW                      
311300              MOVE ZERO              TO KERS-KDORDBEK                     
311400              PERFORM S02-RENSA-TILLK-TAB                                 
311500              MOVE ZERO              TO SPAR-KDORDBEK                     
311600           END-IF                                                         
311700           IF KERS-KDERS = +19 OR +29                                     
311800              MOVE ZERO           TO SPAR-KDORDBEK                        
311900           END-IF                                                         
312000         END-IF                                                           
312100         MOVE SDCA-KDOI              TO ORAD-KDOI                         
312200         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
312300       END-IF                                                             
312400                                                                          
312500     END-IF                                                               
312600     .                                                                    
312700                                                                          
312800 ECX-PREL-AVBOKNING-SDC3 SECTION.                                         
312900     MOVE 'ECX-PREL-AVBOKNI'       TO CURRENT-SECTION                     
313000                                                                          
313100     IF ALLT-OK OR KOLLA-ERS                                              
313200                                                                          
313300       PERFORM S20-HAMTA-WDB6-INFO                                        
313400                                                                          
313500       IF DCS-SDC                                                         
313600         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
313700         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
313800         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
313900         MOVE NEJ                  TO SDCA-FLORDSPE                       
314000         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
314100         MOVE ORAD-IDDC            TO SDCA-IDDC                           
314200         MOVE SPACE                TO SDCA-IDDC-TVS                       
314300         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
314400         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
314500         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
314600         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
314700         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
314800         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
314900         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
315000         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
315100         MOVE +1                   TO SDCA-KDCALL                         
315200         MOVE +3                   TO SDCA-IXDCCLEAR                      
315300                                                                          
315400         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
315500                                           SDCA-WDB6-PCB                  
315600                                           SDCA-WDK9-PCB                  
315700                                           SDCA-WDR6-PCB                  
315800                                           SDCA-WDK6-PCB                  
315900                                           SDCA-WDQ4B-PCB                 
316000                                           SDCA-WDQ2-PCB                  
316100                                           SDCA-WDQ4-PCB                  
316200                                           SDCA-WDB6-2-PCB                
316300                                           SDCA-WDK6-2-PCB                
316400                                           SDCA-WDK7-2-PCB                
316500                                           SDCA-WDK7-3-PCB                
316600                                                                          
316700         IF SDCA-KDORDBEK > ZERO                                          
316800           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK = 80                   
316900              MOVE JA        TO OBKR-SW                                   
317000              MOVE NEJ       TO ALLT-SW                                   
317100           ELSE                                                           
317200             IF ORAD-KDORDKL > 0                                          
317300             AND ORAD-IDSYSTEM NOT = 'OREL'                               
317400             AND (AREG-KDUART = 'L'                                       
317500             OR AREG-KDUART = 'P')                                        
317600             AND OHUV-FLORDSPE NOT = JA                                   
317700             AND OHUV-FLOVRLEV NOT = JA                                   
317800             AND OHUV-FLFORBI = NEJ                                       
317900                MOVE ZERO    TO SDCA-KDORDBEK                             
318000                MOVE 70      TO TPO2-KDORDBEK                             
318100                MOVE JA      TO OBKR-SW                                   
318200                MOVE NEJ     TO ALLT-SW                                   
318300                MOVE WC-CDC-SE TO ORAD-IDDC                               
318400                MOVE ORAD-IDDC TO WS-IDDC                                 
318500                MOVE 6       TO ORAD-KDTPOTYP                             
318600                IF ORAD-KDPRTYP NOT = 'P'                                 
318700                   MOVE ZERO  TO ORAD-PRARTNTO                            
318800                   MOVE SPACE TO ORAD-KDPRTYP                             
318900**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
319000                   IF NOT DIST79-DEALER-PRICE AND                         
319200                      NOT DIST79-ECOM-PRICE                               
319300                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
319400                     MOVE NEJ         TO ORAD-FLPRTILL                    
319500                   END-IF                                                 
319600*************                                                             
319700                END-IF                                                    
319800             ELSE                                                         
319900               IF KOLLA-ERS                                               
320000*                 MOVE NEJ             TO KOLLA-ERS-SW                    
320100*                 MOVE ZERO            TO SDCA-KDORDBEK                   
320200                  MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC           
320300                  IF SPAR-KDORDBEK      = ZERO                            
320400                    IF ORAD-IDDC = W-TILLK-DC                             
320500                       MOVE JA            TO OBKR-SW                      
320600                       MOVE NEJ           TO ALLT-SW                      
320700                                             KOLLA-ERS-SW                 
320800                       MOVE ZERO            TO SDCA-KDORDBEK              
320900                    ELSE                                                  
321000                        MOVE WC-CDC-SE  TO ORAD-IDDC                      
321100                     MOVE ORAD-IDDC     TO WS-IDDC                        
321200                     MOVE JA              TO CDC-MOVE-SW                  
321300                    END-IF                                                
321400                  END-IF                                                  
321500               ELSE                                                       
321600                 IF SDCA-KDORDBEK = 15                                    
321700                    IF SDCA-KDORDBEK-SECOND-SDC = 15                      
321800                       MOVE ZERO TO SDCA-KDORDBEK-SECOND-SDC              
321900                    END-IF                                                
322000                    MOVE JA            TO OBKR-SW                         
322100                    MOVE WC-CDC-SE     TO ORAD-IDDC                       
322200                    MOVE ORAD-IDDC     TO WS-IDDC                         
322300                 ELSE                                                     
322400                    MOVE JA            TO OBKR-SW                         
322500                    MOVE NEJ           TO ALLT-SW                         
322600                 END-IF                                                   
322700               END-IF                                                     
322800             END-IF                                                       
322900           END-IF                                                         
323000         ELSE                                                             
323100           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
323200           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
323300           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
323400           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
323500           MOVE NEJ                  TO ALLT-SW                           
323600           IF KVAN-KDORDBEK-UT   = ZERO                                   
323700           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
323800           AND SDCA-KDORDBEK-SECOND-SDC = ZERO                            
323900             MOVE JA                 TO EGET-CL-RAD-SW                    
324000           END-IF                                                         
324100           IF KOLLA-ERS OR                                                
324200             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
324300              MOVE NEJ               TO KOLLA-ERS-SW                      
324400              MOVE ZERO              TO KERS-KDORDBEK                     
324500              PERFORM S02-RENSA-TILLK-TAB                                 
324600              MOVE ZERO              TO SPAR-KDORDBEK                     
324700           END-IF                                                         
324800           IF KERS-KDERS = +19 OR +29                                     
324900              MOVE ZERO           TO SPAR-KDORDBEK                        
325000           END-IF                                                         
325100         END-IF                                                           
325200                                                                          
325300         MOVE SDCA-KDOI              TO ORAD-KDOI                         
325400         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
325500       END-IF                                                             
325600                                                                          
325700     END-IF                                                               
325800     .                                                                    
325900                                                                          
326000 ECP-KOMPLETTERA-RANSONERING SECTION.                                     
326100     MOVE 'ECP-KOMPLETTERA-'   TO CURRENT-SECTION                         
326200                                                                          
326300     IF ALLT-OK OR CDC-MOVE                                               
326400        MOVE ORAD-BERADREF     TO RANS-BERADREF                           
326500        MOVE OHUV-FLEMBORD     TO RANS-FLEMBORD                           
326600        MOVE OHUV-FLFORBI      TO RANS-FLFORBI                            
326700        MOVE OHUV-FLORDSPE     TO RANS-FLORDSPE                           
326800        MOVE OHUV-FLOVRLEV     TO RANS-FLOVRLEV                           
326900        MOVE ORAD-IDKAMPRF     TO RANS-IDKAMPRF                           
327000        MOVE ORAD-IDARTNR      TO RANS-IDARTNR                            
327100        MOVE ORAD-IDLEVNR      TO RANS-IDLEVNR                            
327200        MOVE OHUV-IDRFTAB      TO RANS-IDRFTAB                            
327300        MOVE ORAD-TIRODAT      TO RANS-TIRODAT                            
327400        MOVE OHUV-KDORDKL      TO RANS-KDORDKL                            
327500        MOVE +1                TO RANS-KDORDBEH                           
327600        MOVE ORAD-KVBEART-Q    TO RANS-KVBEART-Q                          
327700        MOVE ORAD-KDTPOTYP     TO RANS-KDTPOTYP                           
327800        MOVE AREG-KDERS        TO RANS-KDERS                              
327900        MOVE AREG-KVLS         TO RANS-KVLS                               
328000        MOVE AREG-KVPB-SATS    TO RANS-KVPB-SATS                          
328100        MOVE AREG-KVPB-SEP     TO RANS-KVPB-SEP                           
328200        MOVE AREG-REDIRLEV     TO RANS-REDIRLEV                           
328300        MOVE AREG-KVRESS       TO RANS-KVRESS                             
328400        MOVE AREG-KVSPANT      TO RANS-KVSPANT                            
328500        MOVE AREG-KVUTRS       TO RANS-KVUTRS                             
328600        MOVE AREG-TIDISPIN     TO RANS-TIDISPIN                           
328700                                                                          
328800        MOVE AREG-KDPRODSL     TO TEST-KDPRODSL                           
328900        IF KDPRODSL-BIMA                                                  
329000          MOVE 1               TO ORAD-RERF-RAD                           
329100                                  RANS-RERF-RAD-UT                        
329200          MOVE ZERO            TO RANS-SUTPO-PB-UT                        
329300                                  RANS-SUTPO-EJPB-UT                      
329400                                  RANS-RERF-ART-UT                        
329500        ELSE                                                              
329600          CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                 
329700                              RANS-ARTM-PCB RANS-ARTS-PCB                 
329800                                                                          
329900          MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                          
330000        END-IF                                                            
330100     END-IF                                                               
330200     .                                                                    
330300                                                                          
330400 ECQ-KOMPLETTERA-STORA-UTTAG SECTION.                                     
330500     MOVE 'ECQ-KOMPLETTERA-'   TO CURRENT-SECTION                         
330600                                                                          
330700     IF ALLT-OK                                                           
330800        MOVE ORAD-IDSYSTEM     TO STOR-IDSYSTEM                           
330900        MOVE ORAD-IDLEVNR      TO STOR-IDLEVNR                            
331000        MOVE ORAD-IDKUNDRF-RO  TO STOR-IDKUNDRF-RO                        
331100        MOVE ORAD-BERADREF     TO STOR-BERADREF                           
331200        MOVE OHUV-FLFORBI      TO STOR-FLFORBI                            
331300        MOVE OHUV-FLORDSPE     TO STOR-FLORDSPE                           
331400        MOVE OHUV-FLOVRLEV     TO STOR-FLOVRLEV                           
331500        MOVE OHUV-KDORDKL      TO STOR-KDORDKL                            
331600        MOVE AREG-KDERS        TO STOR-KDERS                              
331700        MOVE AREG-KDVVKL       TO STOR-KDVVKL                             
331800        MOVE ORAD-KVBEART-Q    TO STOR-KVBEART-Q                          
331900        MOVE AREG-KVPB-SEP     TO STOR-KVPB-SEP                           
332000        MOVE AREG-KVSLUTKP     TO STOR-KVSLUTKP                           
332100        MOVE RANS-RERF-ART-UT  TO STOR-RERF-ART                           
332200        MOVE OHUV-IDKAMPRF     TO STOR-IDKAMPRF                           
332300        MOVE ORAD-IDDISTR      TO STOR-IDDISTR                            
332400        MOVE SPACE             TO STOR-KDPROTYP                           
332500        MOVE AREG-KDPRODSL     TO STOR-KDPRODSL                           
332600                                                                          
332700        CALL W411STOR USING STOR-W411STOR                                 
332800                                                                          
332900        IF STOR-KDORDBEK > +0                                             
333000           MOVE +6                TO ORAD-KDTPOTYP                        
333100           IF ORAD-KDPRTYP NOT = 'P'                                      
333200              MOVE +0          TO ORAD-PRARTNTO                           
333300              MOVE SPACE       TO ORAD-KDPRTYP                            
333400****    OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                   
333500              IF NOT DIST79-DEALER-PRICE AND                              
333700                 NOT DIST79-ECOM-PRICE                                    
333800                 MOVE ZERO     TO ORAD-PRARTNTO-LOC                       
333900                 MOVE NEJ      TO ORAD-FLPRTILL                           
334000              END-IF                                                      
334100*************                                                             
334200           END-IF                                                         
334300           MOVE JA                TO OBKR-SW                              
334400           MOVE NEJ               TO ALLT-SW                              
334500        END-IF                                                            
334600     END-IF                                                               
334700     .                                                                    
334800                                                                          
334900 ECR-PREL-AVBOKNING-CDC SECTION.                                          
335000     MOVE 'ECR-PREL-AVBOKNI'     TO CURRENT-SECTION                       
335100                                                                          
335200     IF ALLT-OK OR CDC-MOVE                                               
335300       MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                  
335400                                 TO CDCA-FLFINLV-IN                       
335500       MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                       
335600       MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                      
335700       MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                      
335800       MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                      
335900       MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                       
336000       MOVE ORFK-FLSLATT(WS-INDEX-MID)                                    
336100                                 TO CDCA-FLSLATT-IN                       
336200       MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                       
336300       MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                       
336400       MOVE ORAD-IDDC            TO CDCA-IDDC-IN                          
336500       MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                      
336600       MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                   
336700       MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                      
336800       MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                       
336900       MOVE AREG-KDERS           TO CDCA-KDERS-IN                         
337000       MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                      
337100       MOVE SPACE                TO CDCA-KDPROTYP-IN                      
337200       MOVE OHUV-KDORDKL         TO CDCA-KDORDKL-IN                       
337300       MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                      
337400       MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                        
337500       MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                      
337600       MOVE AREG-KDUART          TO CDCA-KDUART-IN                        
337700       MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                       
337800       MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                     
337900       MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                     
338000       MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                     
338100       MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                      
338200       MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                      
338300       MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                      
338400       MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                       
338500       MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                     
338600       MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                     
338700       MOVE AREG-KVLS            TO CDCA-KVLS-IN                          
338800       MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                        
338900       MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                       
339000       MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                        
339100       MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                  
339200       MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                      
339300       MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                      
339400       MOVE +1                   TO CDCA-KDCALL                           
339500                                                                          
339600                                                                          
339700       IF ALLT-OK                                                         
339800         CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                  
339900                                           CDCA-INLB-PCB                  
340000                                           CDCA-WDB2-PCB                  
340100                                           CDCA-WDC1-PCB                  
340200                                                                          
340300         IF KERS-KDERS = 0                                                
340400            CONTINUE                                                      
340500         ELSE                                                             
340600            IF KERS-KDERS > 0 AND < 10                                    
340700               IF CDCA-KVPREAVB-UT = +0 AND CDCA-KVPRERO-UT = +0          
340800                  MOVE CDCA-KVBEART-Q-UT TO CDCA-KVPRERO-UT               
340900               END-IF                                                     
341000               PERFORM S02-RENSA-TILLK-TAB                                
341100               MOVE ZERO           TO KERS-KDORDBEK                       
341200               MOVE NEJ            TO TILLK-SW                            
341300            ELSE                                                          
341400***FOR KDERS>10,DONT SWITCH IF A HAS STOCKS IN CDC                        
341500              IF CDCA-KVPREAVB-UT > 0                                     
341600                IF KOLLA-ERS    OR                                        
341700                  (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)          
341800                   MOVE NEJ               TO KOLLA-ERS-SW                 
341900                   MOVE ZERO              TO KERS-KDORDBEK                
342000                   PERFORM S02-RENSA-TILLK-TAB                            
342100                   MOVE ZERO              TO SPAR-KDORDBEK                
342200                ELSE                                                      
342300                   IF KERS-KDERS = +19 OR +29                             
342400                      MOVE ZERO           TO SPAR-KDORDBEK                
342500                   END-IF                                                 
342600                END-IF                                                    
342700              ELSE                                                        
342800                 IF (CDCA-KVPREAVB-UT <= 0) AND                           
342900                   (CDCA-KDORDBEK-UT = 92 OR 99)                          
343000                     MOVE ZEROES        TO CDCA-KDORDBEK-UT               
343100                 END-IF                                                   
343200                 IF (KOLLA-ERS AND KERS-KDORDBEK > ZERO)                  
343300                 OR (KERS-KDORDBEK = 61 AND EJ-TILLKOMMANDE-RAD)          
343400                 OR SPAR-KDORDBEK = 54                                    
343500                    PERFORM S07-SPACE-SDCA-KDORDBEK                       
343600                 END-IF                                                   
343700              END-IF                                                      
343800            END-IF                                                        
343900         END-IF                                                           
344000************                                                              
344100         MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                       
344200                                                                          
344300         IF ORAD-IDLEVNR NOT = SPACE                                      
344400              CONTINUE                                                    
344500         ELSE                                                             
344600            MOVE CDCA-KVBEART-UT      TO ORAD-KVBEART                     
344700            MOVE CDCA-KVBEART-Q-UT    TO ORAD-KVBEART-Q                   
344800         END-IF                                                           
344900         MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                       
345000         MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                        
345100         MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                        
345200         MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                       
345300         IF CDCA-KDORDBEK-UT > ZERO                                       
345400            MOVE JA                TO OBKR-SW                             
345500         END-IF                                                           
345600                                                                          
345700         IF KVAN-KDORDBEK-UT > +0                                         
345800            IF CDCA-KVBEART-UT = CDCA-KVBEART-Q-UT                        
345900               MOVE +0             TO KVAN-KDORDBEK-UT                    
346000            END-IF                                                        
346100         END-IF                                                           
346200                                                                          
346300         IF (CDCA-KVPREAVB-UT > +0 OR CDCA-KVPRERO-UT > +0) AND           
346400             CDCA-KDORDBEK-UT  = +0 AND                                   
346500             KVAN-KDORDBEK-UT  = +0 AND                                   
346600             DLEV-KDORDBEK-UT  = +0 AND                                   
346700             KERS-KDORDBEK     = +0 AND                                   
346800             TPO1-KDORDBEK     = +0 AND                                   
346900             TPO2-KDORDBEK     = +0 AND                                   
347000             KAMP-KDORDBEK     = +0 AND                                   
347100             STOR-KDORDBEK     = +0 AND                                   
347200             SDCA-KDORDBEK     = +0 AND                                   
347300             SDCA-KDORDBEK-FIRST-SDC = +0 AND                             
347400             SDCA-KDORDBEK-SECOND-SDC = +0 AND                            
347500             SPAR-KDORDBEK     = ZERO                                     
347600           MOVE JA                  TO EGET-CL-RAD-SW                     
347700         END-IF                                                           
347800         MOVE AREG-ADLAGOMR       TO ORAD-ADLAGOMR                        
347900         MOVE AREG-ADGANG         TO ORAD-ADGANG                          
348000         MOVE AREG-ADPLATS        TO ORAD-ADPLATS                         
348100       END-IF                                                             
348200     END-IF                                                               
348300     .                                                                    
348400                                                                          
348500 ECZ-CHECK-KDERS-IN-DC SECTION.                                           
348600                                                                          
348700     MOVE WS-INDEX-TILLK    TO WS-SAVE-INDEX                              
348800     MOVE +1                TO WS-INDEX-TILLK                             
348900                               IDDC-IX                                    
349000     MOVE NEJ               TO BAL-DC-FND-SW                              
349100                               TILLK-BAL-DC-FND-SW                        
349200                               KDERS-CHAIN-SW                             
349300     MOVE AREG-W411AREG-001 TO ORFK-W411AREG-001(WS-INDEX-MID)            
349400                                                                          
349500     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
349600                   TILK-IDARTNR(WS-INDEX-TILLK) = ZERO OR                 
349700                   BAL-DC-FND                          OR                 
349800                   TILLK-BAL-DC-FND                    OR                 
349900                   KDERS-CHAIN                                            
350000        PERFORM ECZD-CHECK-KDERS-CHAIN                                    
350100        IF KDERS-CHAIN-SW = NEJ                                           
350200          IF TILK-IDARTNR(WS-INDEX-TILLK) > ZERO AND                      
350300             TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND                
350400             TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                          
350500              PERFORM UNTIL                                               
350600                       W-GMT-IDDC-CLEAR(IDDC-IX) = SPACES OR              
350700                       IDDC-IX > IX-DCCLEAR-MAX           OR              
350800                       BAL-DC-FND OR TILLK-BAL-DC-FND                     
350900                 MOVE TILK-IDARTNR(WS-INDEX-TILLK)                        
351000                                          TO W-IDARTNR-SDCA               
351100                 PERFORM ECZB-CALL-SDCA                                   
351200                 IF SDCA-KDORDBEK > 0                                     
351300                    PERFORM ECZA-GET-TILLK-DATA                           
351400                    MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)               
351500                                          TO W-IDARTNR-SDCA               
351600                    PERFORM ECZB-CALL-SDCA                                
351700                    IF SDCA-KDORDBEK = 0                                  
351800                      MOVE JA             TO TILLK-BAL-DC-FND-SW          
351900                      MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                      
352000                                          TO W-TILLK-DC                   
352100                    END-IF                                                
352200                 ELSE                                                     
352300                    MOVE JA                TO BAL-DC-FND-SW               
352400                 END-IF                                                   
352500                 ADD +1       TO IDDC-IX                                  
352600              END-PERFORM                                                 
352700          END-IF                                                          
352800        END-IF                                                            
352900        ADD +1              TO WS-INDEX-TILLK                             
353000     END-PERFORM                                                          
353100                                                                          
353200     MOVE WS-SAVE-INDEX     TO WS-INDEX-TILLK                             
353300     MOVE ORFK-W411AREG-001(WS-INDEX-MID) TO AREG-W411AREG-001            
353400     MOVE ZEROES            TO SDCA-KDORDBEK                              
353500                                                                          
353600     IF KDERS-CHAIN-SW = NEJ                                              
353700        IF BAL-DC-FND-SW = NEJ AND TILLK-BAL-DC-FND-SW = NEJ              
353800           MOVE '11'           TO W-TILLK-DC                              
353900        END-IF                                                            
354000     END-IF                                                               
354100     .                                                                    
354200     EJECT                                                                
354300*****************************************************************         
354400*IF A(KDERS 22) SUPERSEEDED BY B(KDERS-25) AND IS SUPERSEEDED             
354500*BY C1(KDERS 00) AND C2(KDERS 00),C1,C2 WILL BE SKIPPED AND               
354600*A WILL BE CHECKED FOR STOCKS, IF NOT OCC61                               
354700*****************************************************************         
354800 ECZD-CHECK-KDERS-CHAIN SECTION.                                          
354900                                                                          
355000     IF TILK-IDARTNR(WS-INDEX-TILLK) > ZERO AND                           
355100        TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND                     
355200        TILK-FLTILLK-X(WS-INDEX-TILLK) = NEJ AND                          
355300       (TILK-KDERS(WS-INDEX-TILLK) = 14 OR 15 OR 18 OR                    
355400                                     24 OR 25 OR 28)                      
355500          MOVE JA              TO KDERS-CHAIN-SW                          
355600     END-IF                                                               
355700     .                                                                    
355800     EJECT                                                                
355900 ECZA-GET-TILLK-DATA SECTION.                                             
356000                                                                          
356100     MOVE AREG-FLREFILL        TO W-FLREFILL-MAIN                         
356200     MOVE AREG-KDPRODSL        TO W-KDPRODSL-MAIN                         
356300     MOVE AREG-KDSORT          TO W-KDSORT-MAIN                           
356400     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-MAIN                        
356500     MOVE AREG-REDIRLEV        TO W-REDIRLEV-MAIN                         
356600                                                                          
356700     PERFORM ED-LAES-TILLK-DATA                                           
356800                                                                          
356900     MOVE AREG-FLREFILL        TO W-FLREFILL-REPL                         
357000     MOVE AREG-KDPRODSL        TO W-KDPRODSL-REPL                         
357100     MOVE AREG-KDSORT          TO W-KDSORT-REPL                           
357200     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-REPL                        
357300     MOVE AREG-REDIRLEV        TO W-REDIRLEV-REPL                         
357400                                                                          
357500     MOVE W-FLREFILL-MAIN      TO AREG-FLREFILL                           
357600     MOVE W-KDPRODSL-MAIN      TO AREG-KDPRODSL                           
357700     MOVE W-KDSORT-MAIN        TO AREG-KDSORT                             
357800     MOVE W-KVQPACK-1-MAIN     TO AREG-KVQPACK-1                          
357900     MOVE W-REDIRLEV-MAIN      TO AREG-REDIRLEV                           
358000     .                                                                    
358100     EJECT                                                                
358200 ECZB-CALL-SDCA   SECTION.                                                
358300                                                                          
358400     IF ORAD-IDARTNR = W-IDARTNR-SDCA                                     
358500        MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                         
358600        MOVE AREG-FLREFILL        TO SDCA-FLREFILL                        
358700        MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                        
358800        MOVE AREG-KDSORT          TO SDCA-KDSORT                          
358900        MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                       
359000        MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                        
359100     ELSE                                                                 
359200        MOVE W-IDARTNR-SDCA       TO SDCA-IDARTNR                         
359300        MOVE W-FLREFILL-REPL      TO SDCA-FLREFILL                        
359400        MOVE W-KDPRODSL-REPL      TO SDCA-KDPRODSL                        
359500        MOVE W-KDSORT-REPL        TO SDCA-KDSORT                          
359600        MOVE W-KVQPACK-1-REPL     TO SDCA-KVQPACK-1                       
359700        MOVE W-REDIRLEV-REPL      TO SDCA-REDIRLEV                        
359800     END-IF                                                               
359900                                                                          
360000     MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                            
360100     MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                            
360200     MOVE NEJ                  TO SDCA-FLORDSPE                           
360300     MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                                       
360400                               TO SDCA-IDDC                               
360500     MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                           
360600     MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                            
360700     MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                           
360800     MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                            
360900     MOVE ORAD-KDORDING        TO SDCA-KDORDING                           
361000     MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                          
361100     MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                           
361200     MOVE +0                   TO SDCA-TIREPDAT                           
361300     MOVE +0                   TO SDCA-KVOKS-PREL                         
361400     MOVE +2                   TO SDCA-KDCALL                             
361500     MOVE +1                   TO SDCA-IXDCCLEAR                          
361600                                                                          
361700     CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                      
361800                                       SDCA-WDB6-PCB                      
361900                                       SDCA-WDK9-PCB                      
362000                                       SDCA-WDR6-PCB                      
362100                                       SDCA-WDK6-PCB                      
362200                                       SDCA-WDQ4B-PCB                     
362300                                       SDCA-WDQ2-PCB                      
362400                                       SDCA-WDQ4-PCB                      
362500                                       SDCA-WDB6-2-PCB                    
362600                                       SDCA-WDK6-2-PCB                    
362700                                       SDCA-WDK7-2-PCB                    
362800                                       SDCA-WDK7-3-PCB                    
362900     .                                                                    
363000     EJECT                                                                
363100 S07-SPACE-SDCA-KDORDBEK  SECTION.                                        
363200                                                                          
363300     IF SDCA-KDORDBEK-FIRST-SDC > 0                                       
363400         MOVE ZEROES  TO SDCA-KDORDBEK-FIRST-SDC                          
363500     ELSE                                                                 
363600        IF SDCA-KDORDBEK-SECOND-SDC > 0                                   
363700           MOVE ZEROES  TO SDCA-KDORDBEK-SECOND-SDC                       
363800        ELSE                                                              
363900           IF SDCA-KDORDBEK > 0                                           
364000              MOVE ZEROES  TO SDCA-KDORDBEK                               
364100           END-IF                                                         
364200        END-IF                                                            
364300     END-IF                                                               
364400     .                                                                    
364500                                                                          
364600     EJECT                                                                
364700 ECS-SKRIV-OBKR SECTION.                                                  
364800     MOVE 'ECS-SKRIV-OBKR  '      TO CURRENT-SECTION                      
364900                                                                          
365000*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
365100*    DEN SISTA ORDERBEKRÄFTELSERADEN FÖR ETT LAGER 'SLÄPAR'               
365200*    ISRT AV RADEN.                                                       
365300*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSERAD SKALL                  
365400*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
365500*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
365600*---                                                                      
365700     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
365800                                                                          
365900     IF TILLKOMMANDE-RAD                                                  
366000        IF KERS-KDORDBEK = 41                                             
366100           MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                        
366200           MOVE '4206KER1'        TO OBKR-IDPGM                           
366300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
366400                                  TO OBKR-KVBEART-TILLK                   
366500           COMPUTE OBKR-DIERS-KVOT =                                      
366600                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
366700                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
366800           MOVE 'S'               TO OBKR-SW                              
366900        END-IF                                                            
367000     END-IF                                                               
367100                                                                          
367200     IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                   
367300       AND ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                           
367400*----(KOD 58, 59, 98)                                                     
367500        IF OBKR-SKRIVEN                                                   
367600           PERFORM IMS-04-ISRT-WDQ101                                     
367700           ADD +1              TO OBKR-IDSEKVNR                           
367800        END-IF                                                            
367900        MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                  
368000                               TO OBKR-KDORDBEK                           
368100        MOVE '4206ORFK'        TO OBKR-IDPGM                              
368200        MOVE 'S'               TO OBKR-SW                                 
368300     END-IF                                                               
368400                                                                          
368500     IF KVAN-KDORDBEK-UT > +0                                             
368600*----(KOD 43, 44)                                                         
368700        IF OBKR-SKRIVEN                                                   
368800           PERFORM IMS-04-ISRT-WDQ101                                     
368900           ADD +1              TO OBKR-IDSEKVNR                           
369000        END-IF                                                            
369100        IF TILLKOMMANDE-RAD                                               
369200           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
369300                               TO OBKR-KVBEART-TILLK                      
369400           COMPUTE OBKR-DIERS-KVOT =                                      
369500                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
369600                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
369700        END-IF                                                            
369800        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
369900        MOVE '4206KVAN'        TO OBKR-IDPGM                              
370000        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
370100        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
370200        MOVE 'S'               TO OBKR-SW                                 
370300     END-IF                                                               
370400                                                                          
370500                                                                          
370600     IF DLEV-KDORDBEK-UT = 21 OR 53 OR 82 OR 95 OR 26                     
370700*----(KOD 21, 53, 82, 95) , 26                                            
370800        IF OBKR-SKRIVEN                                                   
370900           PERFORM IMS-04-ISRT-WDQ101                                     
371000           ADD +1              TO OBKR-IDSEKVNR                           
371100        END-IF                                                            
371200        IF TILLKOMMANDE-RAD                                               
371300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
371400                               TO OBKR-KVBEART-TILLK                      
371500           COMPUTE OBKR-DIERS-KVOT =                                      
371600                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
371700                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
371800        END-IF                                                            
371900        MOVE DLEV-KDORDBEK-UT  TO OBKR-KDORDBEK                           
372000        MOVE '4206DLEV'        TO OBKR-IDPGM                              
372100        MOVE 'S'               TO OBKR-SW                                 
372200     END-IF                                                               
372300                                                                          
372400     IF KERS-KDORDBEK > +0                                                
372500*----(KOD 41, 61)                                                         
372600                                                                          
372700        IF KERS-KDORDBEK = 61                                             
372800*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
372900*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
373000*----RADBEHANDLINGEN                                                      
373100           IF OBKR-SKRIVEN                                                
373200              PERFORM IMS-04-ISRT-WDQ101                                  
373300              ADD +1           TO OBKR-IDSEKVNR                           
373400           END-IF                                                         
373500           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
373600           MOVE '4206KER2'     TO OBKR-IDPGM                              
373700           MOVE 'S'            TO OBKR-SW                                 
373800           PERFORM ECSC-OBKR-FRAN-TILLK-TAB                               
373900           PERFORM S02-RENSA-TILLK-TAB                                    
374000        ELSE                                                              
374100           IF NOT TILLKOMMANDE-RAD                                        
374200              IF OBKR-SKRIVEN                                             
374300                 PERFORM IMS-04-ISRT-WDQ101                               
374400                 ADD +1        TO OBKR-IDSEKVNR                           
374500              END-IF                                                      
374600              MOVE 'S'            TO OBKR-SW                              
374700              MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                        
374800              MOVE '4206KER3'  TO OBKR-IDPGM                              
374900           END-IF                                                         
375000        END-IF                                                            
375100     END-IF                                                               
375200                                                                          
375300     IF SPAR-KDORDBEK > ZERO                                              
375400*----(KOD 51, 52, 53, 54, 55, 57, 58, 66, 67, 80, 90, 92)                 
375500        IF OBKR-SKRIVEN                                                   
375600           PERFORM IMS-04-ISRT-WDQ101                                     
375700           ADD +1              TO OBKR-IDSEKVNR                           
375800        END-IF                                                            
375900        IF TILLKOMMANDE-RAD                                               
376000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
376100                               TO OBKR-KVBEART-TILLK                      
376200           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
376300              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
376400             MOVE +0              TO OBKR-DIERS-KVOT                      
376500           ELSE                                                           
376600             COMPUTE OBKR-DIERS-KVOT =                                    
376700                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
376800                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
376900           END-IF                                                         
377000        END-IF                                                            
377100        MOVE SPAR-KDORDBEK     TO OBKR-KDORDBEK                           
377200        MOVE '4206SPAR'        TO OBKR-IDPGM                              
377300        MOVE 'S'               TO OBKR-SW                                 
377400     END-IF                                                               
377500                                                                          
377600     IF TPO1-KDORDBEK > +0                                                
377700*----(KOD 72, 73, 74, 85)                                                 
377800        IF OBKR-SKRIVEN                                                   
377900           PERFORM IMS-04-ISRT-WDQ101                                     
378000           ADD +1              TO OBKR-IDSEKVNR                           
378100        END-IF                                                            
378200        IF TILLKOMMANDE-RAD                                               
378300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
378400                               TO OBKR-KVBEART-TILLK                      
378500           COMPUTE OBKR-DIERS-KVOT =                                      
378600                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
378700                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
378800        END-IF                                                            
378900        MOVE TPO1-KDORDBEK     TO OBKR-KDORDBEK                           
379000        MOVE '4206TPO1'        TO OBKR-IDPGM                              
379100        IF TPO1-KDORDBEK = 85                                             
379200           MOVE TPO1-KVANNANT  TO OBKR-KVANNANT                           
379300        END-IF                                                            
379400        MOVE 'S'               TO OBKR-SW                                 
379500     END-IF                                                               
379600                                                                          
379700     IF TPO2-KDORDBEK > +0                                                
379800*----(KOD 70)                                                             
379900        IF OBKR-SKRIVEN                                                   
380000           PERFORM IMS-04-ISRT-WDQ101                                     
380100           ADD +1              TO OBKR-IDSEKVNR                           
380200        END-IF                                                            
380300        IF TILLKOMMANDE-RAD                                               
380400           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
380500                               TO OBKR-KVBEART-TILLK                      
380600           COMPUTE OBKR-DIERS-KVOT =                                      
380700                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
380800                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
380900        END-IF                                                            
381000        MOVE TPO2-KDORDBEK     TO OBKR-KDORDBEK                           
381100        MOVE '4206TPO2'        TO OBKR-IDPGM                              
381200        MOVE 'S'               TO OBKR-SW                                 
381300     END-IF                                                               
381400                                                                          
381500                                                                          
381600     IF KAMP-KDORDBEK > +0                                                
381700*----(KOD 72, 75, 76)                                                     
381800        IF OBKR-SKRIVEN                                                   
381900           PERFORM IMS-04-ISRT-WDQ101                                     
382000           ADD +1              TO OBKR-IDSEKVNR                           
382100        END-IF                                                            
382200        IF TILLKOMMANDE-RAD                                               
382300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
382400                               TO OBKR-KVBEART-TILLK                      
382500           COMPUTE OBKR-DIERS-KVOT =                                      
382600                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
382700                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
382800        END-IF                                                            
382900        MOVE KAMP-KDORDBEK     TO OBKR-KDORDBEK                           
383000        MOVE '4206KAMP'        TO OBKR-IDPGM                              
383100        MOVE 'S'               TO OBKR-SW                                 
383200     END-IF                                                               
383300                                                                          
383400                                                                          
383500     IF RELS-KDORDBEK > 0                                                 
383600*----(KOD 56)                                                             
383700        IF OBKR-SKRIVEN                                                   
383800           PERFORM IMS-04-ISRT-WDQ101                                     
383900           ADD +1              TO OBKR-IDSEKVNR                           
384000        END-IF                                                            
384100        MOVE RELS-KDORDBEK     TO OBKR-KDORDBEK                           
384200        MOVE '4206ORFK'        TO OBKR-IDPGM                              
384300        MOVE 'S'               TO OBKR-SW                                 
384400     END-IF                                                               
384500                                                                          
384600     IF XDCA-KDORDBEK > ZERO                                              
384700*----(KOD 15, 53, 55, 80, 92, 98, 99)                                     
384800        IF OBKR-SKRIVEN                                                   
384900           PERFORM IMS-04-ISRT-WDQ101                                     
385000           ADD +1              TO OBKR-IDSEKVNR                           
385100        END-IF                                                            
385200        IF TILLKOMMANDE-RAD                                               
385300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
385400                               TO OBKR-KVBEART-TILLK                      
385500           COMPUTE OBKR-DIERS-KVOT =                                      
385600                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
385700                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
385800        END-IF                                                            
385900                                                                          
386000        IF XDCA-KDORDBEK NOT = 15                                         
386100           IF OHUV-IDDC-PRIM     NOT = XDCA-IDDC-OUT                      
386200              MOVE 15          TO OBKR-KDORDBEK                           
386300              MOVE IDPGM       TO OBKR-IDPGM                              
386400              MOVE 'S'         TO OBKR-SW                                 
386500                                                                          
386600*  FÖR ATT SLIPPA DUBBLA 15-BEKRÄFTELSER NOLLAR VI EV. SDC                
386700              IF SDCA-KDORDBEK-FIRST-SDC = 15                             
386800                 MOVE ZERO     TO SDCA-KDORDBEK-FIRST-SDC                 
386900              END-IF                                                      
387000              IF SDCA-KDORDBEK-SECOND-SDC = 15                            
387100                 MOVE ZERO     TO SDCA-KDORDBEK-SECOND-SDC                
387200              END-IF                                                      
387300              IF SDCA-KDORDBEK = 15                                       
387400                 MOVE ZERO     TO SDCA-KDORDBEK                           
387500              END-IF                                                      
387600           END-IF                                                         
387700           IF OBKR-SKRIVEN                                                
387800              PERFORM IMS-04-ISRT-WDQ101                                  
387900              ADD +1           TO OBKR-IDSEKVNR                           
388000           END-IF                                                         
388100        END-IF                                                            
388200        IF XDCA-KDORDBEK = 80                                             
388300           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
388400        END-IF                                                            
388500        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
388600        MOVE '4206XDCA'        TO OBKR-IDPGM                              
388700        MOVE 'S'               TO OBKR-SW                                 
388800     END-IF                                                               
388900                                                                          
389000                                                                          
389100     IF SDCA-KDORDBEK-SECOND-SDC > ZERO                                   
389200*----(KOD 15, 53, 80, 92)                                                 
389300        IF OBKR-SKRIVEN                                                   
389400           PERFORM IMS-04-ISRT-WDQ101                                     
389500           ADD +1              TO OBKR-IDSEKVNR                           
389600        END-IF                                                            
389700        IF TILLKOMMANDE-RAD                                               
389800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
389900                               TO OBKR-KVBEART-TILLK                      
390000           COMPUTE OBKR-DIERS-KVOT =                                      
390100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
390200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
390300        END-IF                                                            
390400        IF SDCA-KDORDBEK-SECOND-SDC = 80                                  
390500           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
390600        END-IF                                                            
390700        IF SDCA-KDORDBEK-SECOND-SDC = 80 OR 92                            
390800           MOVE 0              TO OBKR-IDARTNR-TILLK                      
390900        END-IF                                                            
391000        MOVE SDCA-KDORDBEK-SECOND-SDC TO OBKR-KDORDBEK                    
391100        MOVE '4206SDCA'        TO OBKR-IDPGM                              
391200        MOVE 'S'               TO OBKR-SW                                 
391300     END-IF                                                               
391400                                                                          
391500                                                                          
391600     IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                    
391700*----(KOD 15, 53, 80, 92)                                                 
391800        IF OBKR-SKRIVEN                                                   
391900           PERFORM IMS-04-ISRT-WDQ101                                     
392000           ADD +1              TO OBKR-IDSEKVNR                           
392100        END-IF                                                            
392200        IF TILLKOMMANDE-RAD                                               
392300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
392400                               TO OBKR-KVBEART-TILLK                      
392500           COMPUTE OBKR-DIERS-KVOT =                                      
392600                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
392700                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
392800        END-IF                                                            
392900        IF SDCA-KDORDBEK-FIRST-SDC = 80                                   
393000           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
393100        END-IF                                                            
393200        IF SDCA-KDORDBEK-FIRST-SDC = 80 OR 92                             
393300           MOVE 0              TO OBKR-IDARTNR-TILLK                      
393400        END-IF                                                            
393500        MOVE SDCA-KDORDBEK-FIRST-SDC TO OBKR-KDORDBEK                     
393600        MOVE '4206SDCA'        TO OBKR-IDPGM                              
393700        MOVE 'S'               TO OBKR-SW                                 
393800     END-IF                                                               
393900                                                                          
394000                                                                          
394100     IF SDCA-KDORDBEK > ZERO                                              
394200*----(KOD 15, 53, 80, 92)                                                 
394300        IF OBKR-SKRIVEN                                                   
394400           PERFORM IMS-04-ISRT-WDQ101                                     
394500           ADD +1              TO OBKR-IDSEKVNR                           
394600        END-IF                                                            
394700        IF TILLKOMMANDE-RAD                                               
394800           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
394900                               TO OBKR-KVBEART-TILLK                      
395000           COMPUTE OBKR-DIERS-KVOT =                                      
395100                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
395200                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
395300        END-IF                                                            
395400        IF SDCA-KDORDBEK = 80                                             
395500           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
395600        END-IF                                                            
395700        IF SDCA-KDORDBEK = 80 OR 92                                       
395800           MOVE 0              TO OBKR-IDARTNR-TILLK                      
395900        END-IF                                                            
396000        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
396100        MOVE '4206SDCA'        TO OBKR-IDPGM                              
396200        MOVE 'S'               TO OBKR-SW                                 
396300     END-IF                                                               
396400                                                                          
396500     IF STOR-KDORDBEK > +0                                                
396600*----(KOD 70)                                                             
396700        IF OBKR-SKRIVEN                                                   
396800           PERFORM IMS-04-ISRT-WDQ101                                     
396900           ADD +1              TO OBKR-IDSEKVNR                           
397000        END-IF                                                            
397100        IF TILLKOMMANDE-RAD                                               
397200           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
397300                               TO OBKR-KVBEART-TILLK                      
397400           COMPUTE OBKR-DIERS-KVOT =                                      
397500                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
397600                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
397700        END-IF                                                            
397800        MOVE STOR-KDORDBEK     TO OBKR-KDORDBEK                           
397900        MOVE '4206STOR'        TO OBKR-IDPGM                              
398000        MOVE 'S'               TO OBKR-SW                                 
398100     END-IF                                                               
398200                                                                          
398300     IF CDCA-KDORDBEK-UT > +0                                             
398400*----(KOD 80, 92, 99)                                                     
398500        IF OBKR-SKRIVEN                                                   
398600           PERFORM IMS-04-ISRT-WDQ101                                     
398700           ADD +1              TO OBKR-IDSEKVNR                           
398800        END-IF                                                            
398900        IF TILLKOMMANDE-RAD                                               
399000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
399100                               TO OBKR-KVBEART-TILLK                      
399200           COMPUTE OBKR-DIERS-KVOT =                                      
399300                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
399400                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
399500        END-IF                                                            
399600        IF CDCA-KDORDBEK-UT = +80                                         
399700           MOVE CDCA-KVANNANT-UT  TO OBKR-KVANNANT                        
399800        END-IF                                                            
399900        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
400000        MOVE '4206CDCA'        TO OBKR-IDPGM                              
400100        MOVE 'S'               TO OBKR-SW                                 
400200     END-IF                                                               
400300*                                                                         
400400* PÅ SISTA RADEN FÖR KUNDENS NORMALA LAGER LÄGGS DE AVBOKADE              
400500* ANTALEN!                                                                
400600*                                                                         
400700     IF OBKR-SKRIVEN                                                      
400800        MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                           
400900        MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                            
401000*THE IF CONDITION (OCC 61 AND KVPREAVB,KVPRERO = 0) IS CODED,             
401100*BECAUSE IF A HAS NO STOCKS IN LDC,CDC, THEN KVPRERO 1 IS                 
401200*INSERTED FOR THE LAST SUPERSEEDING PART THEREBY CREATING                 
401300*ORDERLINE FOR THAT PART ALONG WITH OCC61.                                
401400        IF OBKR-KDORDBEK = 61 AND                                         
401500         (OBKR-KVPREAVB = 0 AND OBKR-KVPRERO = 0)                         
401600           CONTINUE                                                       
401700        ELSE                                                              
401800           MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                        
401900           MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                         
402000        END-IF                                                            
402100        IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                     
402200           PERFORM S05-DELETE-PRICE-Q-LINE                                
402300           INITIALIZE OBKR-DEAL-PR-LINE                                   
402400        END-IF                                                            
402500        PERFORM IMS-04-ISRT-WDQ101                                        
402600        ADD +1                 TO OBKR-IDSEKVNR                           
402700     END-IF                                                               
402800                                                                          
402900     .                                                                    
403000                                                                          
403100 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
403200     MOVE 'ECSA-REDIGERA-OB'   TO CURRENT-SECTION                         
403300                                                                          
403400     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
403500     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
403600                               TO OBKR-IDARTNR                            
403700     IF NOT TILLKOMMANDE-RAD                                              
403800        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
403900                                  W-IDORDER-Q1-MAX                        
404000        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
404100                                  W-IDARTNR-Q1-MAX                        
404200        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
404300                                  W-IDLOPNR-Q1-MAX                        
404400                                  W-IDSEKVNR-Q1-MIN                       
404500                                  W-IDSEKVNR-Q1-MAX                       
404600        PERFORM IMS-03-GU-WDQ101                                          
404700        PERFORM UNTIL SEGMENT-SAKNAS                                      
404800           ADD +1              TO W-IDLOPNR-Q1-MIN                        
404900                                  W-IDLOPNR-Q1-MAX                        
405000           PERFORM IMS-03-GU-WDQ101                                       
405100        END-PERFORM                                                       
405200        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
405300        MOVE W-IDSEKVNR-Q1-MIN TO OBKR-IDSEKVNR                           
405400     END-IF                                                               
405500     MOVE ORAD-IDDC            TO OBKR-IDDC                               
405600     MOVE +0                   TO OBKR-KDORDBEK                           
405700     MOVE SPACE                TO OBKR-BEERS                              
405800     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
405900     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
406000     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
406100     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
406200     MOVE +0                   TO OBKR-DIERS-KVOT                         
406300     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
406400     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
406500     MOVE NEJ                  TO OBKR-FLOBOK                             
406600     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
406700     MOVE NEJ                  TO OBKR-FLOBPRT                            
406800     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
406900     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
407000     MOVE ORFK-FLSLATT(WS-INDEX-MID)                                      
407100                               TO OBKR-FLSLATT                            
407200     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
407300     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
407400        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
407500     ELSE                                                                 
407600        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
407700                               TO OBKR-REKSIFFR                           
407800     END-IF                                                               
407900     IF TILLKOMMANDE-RAD                                                  
408000        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
408100                               TO OBKR-IDARTNR-TILLK                      
408200        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
408300                               TO OBKR-REKSIFFR-TILLK                     
408400     ELSE                                                                 
408500        MOVE +0                TO OBKR-IDARTNR-TILLK                      
408600        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
408700     END-IF                                                               
408800     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
408900     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
409000     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
409100     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
409200     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
409300     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
409400     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
409500     IF NOT TILLKOMMANDE-RAD                                              
409600        MOVE AREG-KDERS        TO OBKR-KDERS                              
409700     END-IF                                                               
409800     MOVE ORAD-KDOI            TO OBKR-KDOI                               
409900     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
410000     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
410100     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
410200     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
410300     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
410400     MOVE +0                   TO OBKR-KVANNANT                           
410500     MOVE +0                   TO OBKR-KVAVBART                           
410600     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
410700     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
410800     MOVE +0                   TO OBKR-KVBEART-TILLK                      
410900     MOVE +0                   TO OBKR-KVPREAVB                           
411000     MOVE +0                   TO OBKR-KVPRERO                            
411100     IF ALLT-OK                                                           
411200        MOVE KVAN-KVQPACK-UT   TO OBKR-KVQPACK                            
411300     ELSE                                                                 
411400        MOVE +0                TO OBKR-KVQPACK                            
411500     END-IF                                                               
411600     MOVE +0                   TO OBKR-KVRO                               
411700     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
411800     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
411900     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
412000     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
412100     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
412200     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
412300     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
412400     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
412500     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
412600     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
412700     MOVE +0                   TO OBKR-TIRODAT                            
412800     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
412900     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
413000       MOVE 20                 TO WS-SEKEL-9KOMPL                         
413100     ELSE                                                                 
413200       MOVE 19                 TO WS-SEKEL-9KOMPL                         
413300     END-IF                                                               
413400     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
413500     MOVE ORAD-TITPO           TO OBKR-TITPO                              
413600     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
413700     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
413800     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
413900     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
414000     MOVE SPACE                TO OBKR-IDBIL                              
414100                                                                          
414200     MOVE OHUV-KDORDTYP-LDC    TO OBKR-KDORDTYP-LDC                       
414300     MOVE OHUV-TIREPDAT        TO OBKR-TIREPDAT                           
414400     MOVE ORAD-IDKUNDRF-WIP    TO OBKR-IDKUNDRF-WIP                       
414500     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
414600     MOVE ORAD-PRAVCOST        TO OBKR-PRAVCOST                           
414700     MOVE ORAD-KDVALISO        TO OBKR-KDVALISO                           
414800*    *GLOBAL EXPORT PROJEKTET KRÄVER IFYLLD VALUTA                        
414900     IF OBKR-KDVALISO = SPACE                                             
415000        MOVE 'N/A'             TO OBKR-KDVALISO                           
415100     END-IF                                                               
415200                                                                          
415300     .                                                                    
415400     EJECT                                                                
415500                                                                          
415600 ECSC-OBKR-FRAN-TILLK-TAB SECTION.                                        
415700     MOVE 'ECSC-OBKR-FRAN-TIL' TO CURRENT-SECTION                         
415800                                                                          
415900     MOVE +1                   TO WS-INDEX-TILLK                          
416000     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
416100                TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                       
416200        IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
416300           IF OBKR-SKRIVEN                                                
416400              PERFORM IMS-04-ISRT-WDQ101                                  
416500              ADD +1              TO OBKR-IDSEKVNR                        
416600           END-IF                                                         
416700           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
416800           MOVE '4206KER4'     TO OBKR-IDPGM                              
416900           MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                        
417000                               TO OBKR-IDARTNR-TILLK                      
417100           MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                       
417200                               TO OBKR-REKSIFFR-TILLK                     
417300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
417400                               TO OBKR-KVBEART-TILLK                      
417500           COMPUTE OBKR-DIERS-KVOT =                                      
417600                   TILK-DIERS-TILLK(WS-INDEX-TILLK) /                     
417700                   TILK-DIERS-ERS(WS-INDEX-TILLK)                         
417800           MOVE TILK-BEERS(WS-INDEX-TILLK)                                
417900                               TO OBKR-BEERS                              
418000           MOVE ZEROES         TO OBKR-KVPREAVB                           
418100                                  OBKR-KVPRERO                            
418200                                                                          
418300           MOVE 'S'            TO OBKR-SW                                 
418400        END-IF                                                            
418500        ADD +1                 TO WS-INDEX-TILLK                          
418600     END-PERFORM                                                          
418700                                                                          
418800     IF WS-INDEX-TILLK = +1                                               
418900        MOVE +0                TO OBKR-KDERS                              
419000     END-IF                                                               
419100     .                                                                    
419200                                                                          
419300 ECV-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
419400     MOVE 'ECV-BERAKNA-WOPS'    TO CURRENT-SECTION                        
419500                                                                          
419600*------------------------------------------------------------*            
419700*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
419800*------------------------------------------------------------*            
419900     PERFORM ECVA-FIXA-LAGEROMR-PLATS                                     
420000     PERFORM ECVB-REDIGERA-WOPS-AREA                                      
420100     PERFORM IMS-05-ISRT-WDQ401                                           
420200     PERFORM UNTIL SEGMENT-FINNS                                          
420300        ADD +1                    TO ORAD-IDLOPNR                         
420400        PERFORM IMS-05-ISRT-WDQ401                                        
420500     END-PERFORM                                                          
420600     .                                                                    
420700                                                                          
420800 ECVA-FIXA-LAGEROMR-PLATS SECTION.                                        
420900     MOVE 'ECVA-FIXA-LAGERO'   TO CURRENT-SECTION                         
421000                                                                          
421100     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
421200     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
421300                                                                          
421400     IF OHUV-BEVARREF = SPACE                                             
421500        MOVE ORAD-BERADREF     TO ADRS-BEVARREF-IN                        
421600     ELSE                                                                 
421700        MOVE OHUV-BEVARREF     TO WS-HFAK-REF-X10                         
421800        PERFORM ECVAA-KOLLA-I-HFAK-TAB                                    
421900        IF BEVARREF-I-HFAK-TAB                                            
422000           MOVE OHUV-BEVARREF  TO ADRS-BEVARREF-IN                        
422100        ELSE                                                              
422200           MOVE ORAD-BERADREF  TO ADRS-BEVARREF-IN                        
422300        END-IF                                                            
422400     END-IF                                                               
422500                                                                          
422600     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
422700     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
422800     MOVE 1                    TO ADRS-KDCALL-IN                          
422900     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
423000     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
423100     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
423200     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
423300                                                                          
423400     CALL W413ADRS USING ADRS-W413ADRS                                    
423500                                                                          
423600*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
423700                                                                          
423800     PERFORM S20-HAMTA-WDB6-INFO                                          
423900                                                                          
424000     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
424100     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
424200                                                                          
424300     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
424400     .                                                                    
424500                                                                          
424600 ECVAA-KOLLA-I-HFAK-TAB   SECTION.                                        
424700     MOVE 'ECVAA-KOLLA-I-HFAK' TO CURRENT-SECTION                         
424800                                                                          
424900     IF WS-HFAK-REF-X10 NOT = SPACE                                       
425000        MOVE 1 TO HFAK-TAB-IX                                             
425100        PERFORM UNTIL HFAK-TAB-IX > MAX-HFAK-IX                           
425200           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (HFAK-TAB-IX)         
425300           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
425400           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
425500           AND HFAK-BERADREF-1 (HFAK-TAB-IX) = '#')                       
425600              MOVE JA TO BEVARREF-I-HFAK-TAB-SW                           
425700              MOVE 99 TO HFAK-TAB-IX                                      
425800           END-IF                                                         
425900           ADD 1 TO HFAK-TAB-IX                                           
426000        END-PERFORM                                                       
426100     END-IF                                                               
426200     .                                                                    
426300                                                                          
426400 ECVB-REDIGERA-WOPS-AREA SECTION.                                         
426500     MOVE 'ECVB-REDIGERA-WO'   TO CURRENT-SECTION                         
426600                                                                          
426700*    MOVE +1                   TO AVSR-KDCALL                             
426800     MOVE +8                   TO AVSR-KDCALL                             
426900     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
427000     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
427100     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
427200     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
427300     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
427400     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
427500                                                                          
427600     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
427700     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
427800     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
427900     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
428000     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
428100     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
428200     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
428300     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
428400     MOVE ORAD-DEAL-PR-LINE                                               
428500                               TO AVSR-DEAL-PR-LINE                       
428600                                               (WS-INDEX-WOPS)            
428700     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
428800     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
428900     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
429000     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
429100                                                                          
429200     ADD +1                    TO WS-INDEX-WOPS                           
429300     .                                                                    
429400 ED-LAES-TILLK-DATA SECTION.                                              
429500     MOVE 'ED-LAES-TILLK-DA'   TO CURRENT-SECTION                         
429600                                                                          
429700     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
429800                               TO AREG-IDARTNR                            
429900                                                                          
430000     CALL W411AREG USING AREG-W411AREG                                    
430100                         AREG-WDK6-PCB                                    
430200                         AREG-WDK7-PCB                                    
430300     .                                                                    
430400                                                                          
430500 F-HOPPA-TILL-SVARSBILD SECTION.                                          
430600     MOVE 'F-HOPPA-TILL-SVA'     TO CURRENT-SECTION                       
430700                                                                          
430800     INSPECT MID-IDDISTR  REPLACING LEADING SPACE BY ZERO                 
430900     INSPECT MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO                 
431000     INSPECT MID-IDORDNR5 REPLACING LEADING SPACE BY ZERO                 
431100                                                                          
431200     IF WS-IDTRANS-SPAR = '4211'                                          
431300        MOVE MFS-KDMFSFOR        TO 4213-SPRAK                            
431400        MOVE MID-IDDISTR         TO 4213-IDDISTR-IN                       
431500        MOVE MID-IDKUNDNR        TO 4213-IDKUNDNR-IN                      
431600        MOVE MID-IDORDNR5        TO 4213-IDORDNR-IN                       
431700        MOVE MFS-RENSA-FAELT     TO 4213-IDDISTR-UT                       
431800                                    4213-IDKUNDNR-UT                      
431900                                    4213-IDORDNR-UT                       
432000        PERFORM IMS-INSERT-4213-MSG                                       
432100     ELSE                                                                 
432200        MOVE MFS-KDMFSFOR        TO 4243-SPRAK                            
432300        MOVE MID-IDDISTR         TO 4243-IDDISTR-IN                       
432400        MOVE MID-IDKUNDNR        TO 4243-IDKUNDNR-IN                      
432500        MOVE MID-IDORDNR5        TO 4243-IDORDNR-IN                       
432600        MOVE MFS-RENSA-FAELT     TO 4243-IDDISTR-UT                       
432700                                    4243-IDKUNDNR-UT                      
432800                                    4243-IDORDNR-UT                       
432900        PERFORM IMS-INSERT-4243-MSG                                       
433000     END-IF                                                               
433100                                                                          
433200                                                                          
433300     MOVE JA                     TO HOPP-TILL-4213-4243                   
433400     .                                                                    
433500                                                                          
433600 G-VISA-TOM-SIDA SECTION.                                                 
433700     MOVE 'G-VISA-TOM-SIDA '     TO CURRENT-SECTION                       
433800                                                                          
433900     MOVE +1 TO WS-INDEX                                                  
434000     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
434100       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR(WS-INDEX)                    
434200                                 MOD-KVBEART(WS-INDEX)                    
434300                                 MOD-PRARTNTO(WS-INDEX)                   
434400                                 MOD-TITPO(WS-INDEX)                      
434500                                 MOD-FLRESTN(WS-INDEX)                    
434600                                 MOD-FLSLATT(WS-INDEX)                    
434700                                 MOD-KDKVBRYT(WS-INDEX)                   
434800                                 MOD-FLINVEST(WS-INDEX)                   
434900                                 MOD-KDVRINFO(WS-INDEX)                   
435000                                 MOD-BERADREF(WS-INDEX)                   
435100       ADD  +1 TO WS-INDEX                                                
435200     END-PERFORM                                                          
435300     MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                   
435400     .                                                                    
435500                                                                          
435600 I-SKICKA-PRISFRAGA SECTION.                                              
435700     MOVE 'I-SKICKA-PRISFRA'     TO CURRENT-SECTION                       
435800                                                                          
435900     MOVE 1                      TO 3039-REQU-IDMSGVER                    
436000     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
436100     MOVE 'W4020600'             TO 3039-REQU-IDUSER                      
436200                                                                          
436300     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
436400     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
436500     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
436600     MOVE ORAD-IDORDNR7          TO 3039-MID-IDBUNDLE                     
436700     IF MID-IDARTNR(WS-INDEX-MID-MAX) = ALL '+'                           
436800       MOVE W-IDDISTR            TO 3039-MID-IDDISTR                      
436900       MOVE W-IDKUNDNR           TO 3039-MID-IDKUNDNR                     
437000       MOVE ORAD-IDKUNDRF        TO 3039-MID-IDBUNDLE                     
437100     END-IF                                                               
437200     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
437300                                                                          
437400     PERFORM S04-SKICKA-OPEN                                              
437500     PERFORM S04-SKICKA-MEDDELANDE                                        
437600     PERFORM S04-SKICKA-CLOSE                                             
437700     .                                                                    
437800                                                                          
437900 Z-FINIT-INSERT-MSG SECTION.                                              
438000     MOVE 'Z-FINIT-INSERT-M'     TO CURRENT-SECTION                       
438100                                                                          
438200     IF MED-IDMFSFEL NOT = SPACE                                          
438300         CALL WMEDKONV USING MED-WMEDAREA                                 
438400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
438500     END-IF                                                               
438600                                                                          
438700     IF NOT ALLT-OK                                                       
438800        PERFORM MFS-ROER-EJ-BILD                                          
438900     END-IF                                                               
439000                                                                          
439100     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
439200     PERFORM IMS-INSERT-MSG                                               
439300     .                                                                    
439400                                                                          
439500     EJECT                                                                
439600 S02-RENSA-TILLK-TAB SECTION.                                             
439700     MOVE 'S02-RENSA-TILLK-'     TO CURRENT-SECTION                       
439800                                                                          
439900     MOVE +1              TO WS-INDEX-TILLK                               
440000     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
440100        MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                 
440200        MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)           
440300        MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                   
440400        MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)               
440500        MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)             
440600        MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                 
440700        MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                
440800        MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)               
440900        MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                   
441000        MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                 
441100        MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                 
441200        MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                
441300        INITIALIZE        TILK-DEAL-PR-LINE(WS-INDEX-TILLK)               
441400        MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                  
441500        MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)          
441600        ADD +1            TO WS-INDEX-TILLK                               
441700     END-PERFORM                                                          
441800     MOVE +1              TO WS-INDEX-TILLK                               
441900     .                                                                    
442000                                                                          
442100                                                                          
442200 S03-DATA-TILL-DEL-NOTE SECTION.                                          
442300     MOVE 'S03-DATA-TILL-DE'     TO CURRENT-SECTION                       
442400                                                                          
442500     MOVE OHUV-IDDISTR             TO TEST-IDDISTR                        
442600     IF DIST07-USA-RETAILER-DNOTE                                         
442700     OR DIST07-CAN-RETAILER                                               
442800                                                                          
442900        INITIALIZE DNOT-ORDER-INFO                                        
443000                                                                          
443100        MOVE IDPGM                    TO DNOT-IDPGM                       
443200        MOVE OHUV-IDORDER             TO DNOT-IDORDER                     
443300        MOVE ORAD-IDARTNR             TO DNOT-IDARTNR                     
443400        MOVE ORAD-IDDC                TO DNOT-IDDC                        
443500        MOVE OHUV-ADGMT-GATA          TO DNOT-ADGMT-GATA                  
443600        MOVE OHUV-ADGMT-PADR          TO DNOT-ADGMT-PADR                  
443700        MOVE OHUV-ADGMT-LAND          TO DNOT-ADGMT-LAND                  
443800        MOVE OHUV-BEGMT-RAD1          TO DNOT-BEGMT-RAD1                  
443900        MOVE OHUV-BEGMT-RAD2          TO DNOT-BEGMT-RAD2                  
444000        MOVE OHUV-BEKUNDRF            TO DNOT-BEKUNDRF                    
444100        MOVE ORAD-BERADREF            TO DNOT-BERADREF                    
444200        MOVE OHUV-IDGMTREF            TO DNOT-IDGMTREF                    
444300        MOVE OHUV-IDDC-PRIM           TO DNOT-IDDC-PRIM                   
444400        MOVE ORAD-IDKUNDRF-RO         TO DNOT-IDKUNDRF-RO                 
444500        MOVE ORAD-FLTILLK             TO DNOT-FLTILLK                     
444600        MOVE OHUV-KDORDKL             TO DNOT-KDORDKL                     
444700        MOVE ARB-KDFRAKT              TO DNOT-KDFRAKT                     
444800        MOVE ORAD-KVBEART             TO DNOT-KVBEART                     
444900        MOVE ORAD-KVBEART-Q           TO DNOT-KVBEART-Q                   
445000        MOVE ORAD-REKSIFFR            TO DNOT-REKSIFFR                    
445100        MOVE ORAD-TIREGDAT            TO DNOT-TIREGDAT                    
445200        MOVE ORAD-TIREGTID            TO DNOT-TIREGTID                    
445300        MOVE NEJ                      TO DNOT-FLDIRLEV                    
445400                                                                          
445500                                                                          
445600        IF WS-INDEX-MID > WS-INDEX-MID-MAX                                
445700           MOVE JA              TO DNOT-FL-ORAD-LAST                      
445800        END-IF                                                            
445900                                                                          
446000        CALL W411DNOT USING DNOT-W411DNOT                                 
446100                            DNOT-ORQP-PCB                                 
446200                            DNOT-ORQP2-PCB                                
446300                            DNOT-ORQP3-PCB                                
446400                            DNOT-4013-PCB                                 
446500                            DNOT-BENA-PCB                                 
446600     END-IF                                                               
446700     .                                                                    
446800                                                                          
446900 S04-SKICKA-OPEN SECTION.                                                 
447000     MOVE 'S04-SKICKA-OPEN '     TO CURRENT-SECTION                       
447100                                                                          
447200     MOVE 'OPEN'                     TO SEND-KDFUNC                       
447300     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
447400     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
447500                                                                          
447600     IF SEND-KDRC > 0                                                     
447700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
447800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
447900       DELIMITED BY SIZE INTO FELTEXT                                     
448000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
448100     END-IF                                                               
448200     .                                                                    
448300                                                                          
448400 S04-SKICKA-MEDDELANDE SECTION.                                           
448500     MOVE 'S04-SKICKA-MEDDE'     TO CURRENT-SECTION                       
448600                                                                          
448700     MOVE 'PUT'                      TO SEND-KDFUNC                       
448800     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
448900     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
449000                                                                          
449100     IF SEND-KDRC > 0                                                     
449200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
449300       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
449400       DELIMITED BY SIZE INTO FELTEXT                                     
449500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
449600     END-IF                                                               
449700     .                                                                    
449800                                                                          
449900 S04-SKICKA-CLOSE SECTION.                                                
450000     MOVE 'S04-SKICKA-CLOSE'     TO CURRENT-SECTION                       
450100                                                                          
450200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
450300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
450400                                                                          
450500     IF SEND-KDRC > 0                                                     
450600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
450700       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
450800       DELIMITED BY SIZE INTO FELTEXT                                     
450900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
451000     END-IF                                                               
451100     .                                                                    
451200                                                                          
451300 S05-DELETE-PRICE-Q-LINE SECTION.                                         
451400     MOVE 'S05-DELETE-PRICE'     TO CURRENT-SECTION                       
451500                                                                          
451600     IF DIST79-DEALER-PRICE                                               
451700       IF OBKR-IDPRQUES > ZERO                                            
451800         INITIALIZE PRQU-W335PRQU                                         
451900         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
452000         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
452100         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
452200         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
452300         MOVE 4                  TO PRQU-KDCALL                           
452400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
452500                                            PRQU-WDC7-PCB                 
452600                                            PRQU-SJKO-WDK6-PCB            
452700       END-IF                                                             
452800     END-IF                                                               
452900     .                                                                    
453000 S10-WRONG-PICTURE-MESSAGE SECTION.                                       
453100     SKIP2                                                                
453200* *****************************************************                   
453300*                                                     *                   
453400* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
453500*                                                     *                   
453600* *****************************************************                   
453700     SKIP2                                                                
453800     MOVE JA                  TO HOPP-TILL-0504                           
453900     MOVE 'W0O50401'          TO MFS-IDMOD                                
454000     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
454100     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
454200     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
454300     PERFORM IMS-INSERT-MSG                                               
454400     .                                                                    
454500     EJECT                                                                
454600                                                                          
454700 S20-HAMTA-WDB6-INFO      SECTION.                                        
454800                                                                          
454900     IF WS-CLDC-IX > IX-DCCLEAR-MAX                                       
455000       CONTINUE                                                           
455100     ELSE                                                                 
455200     IF CLDC-IDDC (WS-CLDC-IX) NOT = WS-IDDC                              
455300                                                                          
455400        MOVE 1 TO WS-CLDC-IX                                              
455500        PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX OR                      
455600                      CLDC-IDDC (WS-CLDC-IX) = WS-IDDC OR                 
455700                      CLDC-IDDC (WS-CLDC-IX) = SPACE                      
455800           ADD 1 TO WS-CLDC-IX                                            
455900        END-PERFORM                                                       
456000                                                                          
456100     END-IF                                                               
456200     END-IF                                                               
456300     IF WS-CLDC-IX > IX-DCCLEAR-MAX    OR                                 
456400        CLDC-IDDC (WS-CLDC-IX) = SPACE                                    
456500        MOVE WS-IDDC TO W-IDDC-B6                                         
456600        PERFORM IMS-09-GU-WDB601                                          
456700     ELSE                                                                 
456800        MOVE CLDC-WDB601 (WS-CLDC-IX)  TO DLI-IO-AREA-B601                
456900     END-IF                                                               
457000     .                                                                    
457100     EJECT                                                                
457200 MFS-ROER-EJ-BILD SECTION.                                                
457300                                                                          
457400     MOVE +1 TO WS-INDEX                                                  
457500     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
457600       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(WS-INDEX)                    
457700                                 MOD-KVBEART(WS-INDEX)                    
457800                                 MOD-PRARTNTO(WS-INDEX)                   
457900                                 MOD-TITPO(WS-INDEX)                      
458000                                 MOD-FLRESTN(WS-INDEX)                    
458100                                 MOD-FLSLATT(WS-INDEX)                    
458200                                 MOD-KDKVBRYT(WS-INDEX)                   
458300                                 MOD-FLINVEST(WS-INDEX)                   
458400                                 MOD-KDVRINFO(WS-INDEX)                   
458500                                 MOD-BERADREF(WS-INDEX)                   
458600       ADD  +1 TO WS-INDEX                                                
458700     END-PERFORM                                                          
458800     .                                                                    
458900                                                                          
459000* --- IMS SEKTIONER ---                                                   
459100                                                                          
459200 IMS-GET-MSG SECTION.                                                     
459300                                                                          
459400     MOVE '  QC' TO GODK-STATUSKODER                                      
459500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
459600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
459700     PERFORM IMS-STATUSKONTROLL                                           
459800     .                                                                    
459900                                                                          
460000 IMS-INSERT-MSG SECTION.                                                  
460100                                                                          
460200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
460300     MOVE SPACE TO GODK-STATUSKODER                                       
460400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
460500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
460600     PERFORM IMS-STATUSKONTROLL                                           
460700     .                                                                    
460800                                                                          
460900 IMS-INSERT-4213-MSG SECTION.                                             
461000                                                                          
461100     IF NOT ENGLISH-TEXT                                                  
461200       MOVE '0' TO MFS-KDHUVOMR                                           
461300     END-IF                                                               
461400     MOVE LOW-VALUE TO 4213-Z1 4213-Z2                                    
461500     MOVE SPACE TO GODK-STATUSKODER                                       
461600     CALL CBLTDLI USING ISRT 4213-PCB 4213-MSG-IO-AREA                    
461700     MOVE 4213-STATUS-CODE TO STATUS-WS                                   
461800     PERFORM IMS-STATUSKONTROLL                                           
461900     .                                                                    
462000                                                                          
462100 IMS-INSERT-4243-MSG SECTION.                                             
462200                                                                          
462300     IF NOT ENGLISH-TEXT                                                  
462400       MOVE '0' TO MFS-KDHUVOMR                                           
462500     END-IF                                                               
462600     MOVE LOW-VALUE TO 4243-Z1 4243-Z2                                    
462700     MOVE SPACE TO GODK-STATUSKODER                                       
462800     CALL CBLTDLI USING ISRT 4243-PCB 4243-MSG-IO-AREA                    
462900     MOVE 4243-STATUS-CODE TO STATUS-WS                                   
463000     PERFORM IMS-STATUSKONTROLL                                           
463100     .                                                                    
463200                                                                          
463300 IMS-01-GU-WDQ201 SECTION.                                                
463400     MOVE 'IMS-01' TO CURRENT-IMS-SECTION                                 
463500                                                                          
463600     STRING 'WDQ201  (WDQ2CSEQ =' W-IDGMTREF-X ')'                        
463700          DELIMITED BY SIZE INTO SSA1                                     
463800     MOVE '    '               TO GODK-STATUSKODER                        
463900     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-OHUV SSA1                 
464000     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
464100     PERFORM IMS-STATUSKONTROLL                                           
464200     .                                                                    
464300                                                                          
464400 IMS-GNP-WDQ212-OKVAL SECTION.                                            
464500     MOVE 'IMS-GNP-WDQ212-OKVAL' TO CURRENT-IMS-SECTION                   
464600                                                                          
464700     MOVE 'WDQ212  '       TO SSA1                                        
464800     MOVE '  GE' TO GODK-STATUSKODER                                      
464900     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                 
465000     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
465100     PERFORM IMS-STATUSKONTROLL                                           
465200     .                                                                    
465300                                                                          
465400 IMS-02-GNP-WDQ212 SECTION.                                               
465500     MOVE 'IMS-02' TO CURRENT-IMS-SECTION                                 
465600                                                                          
465700     STRING 'WDQ212  *F(IDDC     =' W-IDDC-X ')'                          
465800          DELIMITED BY SIZE INTO SSA1                                     
465900     MOVE '    ' TO GODK-STATUSKODER                                      
466000     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-ARB SSA1                 
466100     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
466200     PERFORM IMS-STATUSKONTROLL                                           
466300     .                                                                    
466400                                                                          
466500 IMS-03-GU-WDQ101 SECTION.                                                
466600     MOVE 'IMS-03' TO CURRENT-IMS-SECTION                                 
466700                                                                          
466800     STRING 'WDQ101  (WDQ101KY >' W-WDQ101KY-MIN-X                        
466900                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
467000          DELIMITED BY SIZE INTO SSA1                                     
467100     MOVE '  GE'             TO GODK-STATUSKODER                          
467200     CALL CBLTDLI USING GU   WDQ1-PCB DLI-IO-AREA-OBKR SSA1               
467300     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
467400     PERFORM IMS-STATUSKONTROLL                                           
467500     .                                                                    
467600                                                                          
467700 IMS-04-ISRT-WDQ101 SECTION.                                              
467800     MOVE 'IMS-04' TO CURRENT-IMS-SECTION                                 
467900                                                                          
468000     MOVE 'WDQ101   '          TO SSA1                                    
468100     MOVE '    '               TO GODK-STATUSKODER                        
468200     CALL CBLTDLI USING ISRT WDQ1-PCB DLI-IO-AREA-OBKR SSA1               
468300     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
468400     PERFORM IMS-STATUSKONTROLL                                           
468500     .                                                                    
468600                                                                          
468700 IMS-05-ISRT-WDQ401 SECTION.                                              
468800     MOVE 'IMS-05' TO CURRENT-IMS-SECTION                                 
468900                                                                          
469000     MOVE 'WDQ401   '          TO SSA1                                    
469100     MOVE '  II'               TO GODK-STATUSKODER                        
469200     CALL CBLTDLI USING ISRT WDQ4-PCB DLI-IO-AREA-ORAD SSA1               
469300     MOVE WDQ4-STATUS-CODE     TO STATUS-WS                               
469400     PERFORM IMS-STATUSKONTROLL                                           
469500     .                                                                    
469600                                                                          
469700 IMS-06-GU-WDK901 SECTION.                                                
469800     MOVE 'IMS-06' TO CURRENT-IMS-SECTION                                 
469900                                                                          
470000     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
470100          DELIMITED BY SIZE INTO SSA1                                     
470200     MOVE '  GE'               TO GODK-STATUSKODER                        
470300     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-AREA-WDK901 SSA1               
470400     MOVE WDK9-STATUS-CODE     TO STATUS-WS                               
470500     PERFORM IMS-STATUSKONTROLL                                           
470600     .                                                                    
470700 IMS-10-GHU-WLARTM-WDK901 SECTION.                                        
470800                                                                          
470900     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
471000          DELIMITED BY SIZE INTO SSA1                                     
471100     MOVE '    '               TO GODK-STATUSKODER                        
471200     CALL CBLTDLI USING GHU WDK9-PCB DLI-IO-AREA-WDK901 SSA1              
471300     MOVE WDK9-STATUS-CODE     TO STATUS-WS                               
471400     PERFORM IMS-STATUSKONTROLL                                           
471500     .                                                                    
471600                                                                          
471700 IMS-11-REPL-ARTM-WDK901 SECTION.                                         
471800                                                                          
471900     MOVE '    '               TO GODK-STATUSKODER                        
472000     CALL CBLTDLI USING REPL WDK9-PCB DLI-IO-AREA-WDK901                  
472100     MOVE WDK9-STATUS-CODE     TO STATUS-WS                               
472200     PERFORM IMS-STATUSKONTROLL                                           
472300     .                                                                    
472400     EJECT                                                                
472500                                                                          
472600 IMS-07-GU-WDB201 SECTION.                                                
472700     MOVE 'IMS-07' TO CURRENT-IMS-SECTION                                 
472800                                                                          
472900     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
473000          DELIMITED BY SIZE INTO SSA1                                     
473100     MOVE '  '                 TO GODK-STATUSKODER                        
473200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
473300     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
473400     PERFORM IMS-STATUSKONTROLL                                           
473500     .                                                                    
473600                                                                          
473700 IMS-08-GU-WDB101 SECTION.                                                
473800     MOVE 'IMS-08' TO CURRENT-IMS-SECTION                                 
473900                                                                          
474000     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
474100          DELIMITED BY SIZE INTO SSA1                                     
474200     MOVE '  '                 TO GODK-STATUSKODER                        
474300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
474400     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
474500     PERFORM IMS-STATUSKONTROLL                                           
474600     .                                                                    
474700                                                                          
474800 IMS-09-GU-WDB601 SECTION.                                                
474900     MOVE 'IMS-09' TO CURRENT-IMS-SECTION                                 
475000                                                                          
475100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
475200          DELIMITED BY SIZE INTO SSA1                                     
475300     MOVE '  GE' TO GODK-STATUSKODER                                      
475400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
475500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
475600     PERFORM IMS-STATUSKONTROLL                                           
475700     IF SEGMENT-SAKNAS                                                    
475800        MOVE SPACE TO DCS-KDDC                                            
475900     END-IF                                                               
476000     .                                                                    
476100                                                                          
476200 IMS-10-GHU-WDQ201 SECTION.                                               
476300     MOVE 'IMS-10'               TO CURRENT-IMS-SECTION                   
476400                                                                          
476500     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
476600             DELIMITED BY SIZE INTO SSA1                                  
476700     MOVE '    '                 TO GODK-STATUSKODER                      
476800     CALL CBLTDLI USING GHU WDQ2-UPD-PCB DLI-IO-AREA-OHUV SSA1            
476900     MOVE WDQ2-UPD-STATUS-CODE   TO STATUS-WS                             
477000     PERFORM IMS-STATUSKONTROLL                                           
477100     .                                                                    
477200                                                                          
477300 IMS-11-REPL-WDQ201 SECTION.                                              
477400     MOVE 'IMS-11'               TO CURRENT-IMS-SECTION                   
477500                                                                          
477600     MOVE '    '                 TO GODK-STATUSKODER                      
477700     CALL CBLTDLI USING REPL WDQ2-UPD-PCB DLI-IO-AREA-OHUV                
477800     MOVE WDQ2-UPD-STATUS-CODE   TO STATUS-WS                             
477900     PERFORM IMS-STATUSKONTROLL                                           
478000     .                                                                    
478100 IMS-GHNP-WDQ212   SECTION.                                               
478200     MOVE 'IMS-GHNP-WDQ212 ' TO CURRENT-IMS-SECTION                       
478300                                                                          
478400     MOVE 'WDQ212  '       TO SSA1                                        
478500     MOVE '  GE' TO GODK-STATUSKODER                                      
478600     CALL CBLTDLI USING GHNP WDQ2-UPD-PCB DLI-IO-AREA-ARB SSA1            
478700     MOVE WDQ2-UPD-STATUS-CODE TO STATUS-WS                               
478800     PERFORM IMS-STATUSKONTROLL                                           
478900     .                                                                    
479000 IMS-REPL-WDQ212   SECTION.                                               
479100     MOVE 'IMS-REPL-WDQ212 ' TO CURRENT-IMS-SECTION                       
479200                                                                          
479300     MOVE '    '                 TO GODK-STATUSKODER                      
479400     CALL CBLTDLI USING REPL WDQ2-UPD-PCB DLI-IO-AREA-ARB                 
479500     MOVE WDQ2-UPD-STATUS-CODE TO STATUS-WS                               
479600     PERFORM IMS-STATUSKONTROLL                                           
479700     .                                                                    
479800 IMS-ISRT-WDR601 SECTION.                                                 
479900                                                                          
480000     MOVE 'WDR601' TO SSA1                                                
480100     MOVE '  II' TO GODK-STATUSKODER                                      
480200     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
480300     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
480400     PERFORM IMS-STATUSKONTROLL                                           
480500     .                                                                    
480600                                                                          
480700                                                                          
480800 IMS-STATUSKONTROLL SECTION.                                              
480900                                                                          
481000     SET STATUS-IX TO 1                                                   
481100     SEARCH GODK-STATUS                                                   
481200       AT END CALL FELLOG                                                 
481300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
481400     END-SEARCH                                                           
481500     .                                                                    
481600                                                                          
